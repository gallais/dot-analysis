module Data.DependencyGraph where

import Data.Function (on)

import Data.Functor ((<&>))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (fromJust)
import Data.Semigroup (getMax)
import Data.Word (Word8)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T (readFile, writeFile)

import Data.Traversable (for)

import Data.GraphViz.Attributes.Colors (toWC, Color(RGB))
import Data.GraphViz.Attributes.Complete
  ( Attribute(Label, FillColor, Style)
  , Label(..)
  , StyleItem(SItem), StyleName(Filled))
import Data.GraphViz.Types (parseDotGraph, printDotGraph)
import Data.GraphViz.Types.Canonical

import Data.GraphViz.Types.Generalised (fromGeneralised)
import qualified Data.GraphViz.Types.Generalised as Generalised

import System.Exit (die)

------------------------------------------------------------------------
-- A node's neighbours

type Node = String
type Label = String

data Neighbours n v = Neighbours
  { value    :: v
  , parents  :: Set n
  , children :: Set n
  } deriving (Show, Functor, Foldable, Traversable)

instance (Ord n, Semigroup v) => Semigroup (Neighbours n v) where
  Neighbours v1 p1 c1 <> Neighbours v2 p2 c2
    = Neighbours (v1 <> v2) (p1 <> p2) (c1 <> c2)

degreeWith :: (Int -> Int -> Int) -> Neighbours n v -> Int
degreeWith f ngh = (f `on` Set.size) (parents ngh) (children ngh)

degree :: Neighbours n v -> Int
degree = degreeWith (+)

type Labels n = Map n Text

------------------------------------------------------------------------
-- Dependency graph

data DependencyGraph n v = DependencyGraph
  { labels   :: Labels n
  , contexts :: Map n (Neighbours n v)
  } deriving (Functor, Foldable, Traversable)


mapWithKey :: Ord n
           => (n -> Neighbours n v -> Neighbours n w)
           -> (DependencyGraph n v -> DependencyGraph n w)
mapWithKey f (DependencyGraph labels contexts)
  = DependencyGraph labels (Map.mapWithKey f contexts)

------------------------------------------------------------------------
-- Computing a node's transitive dependencies

transitively :: forall n a v. (Ord n, Monoid a)
             => (n -> a -> a)
             -> DependencyGraph n v
             -> Map n a
transitively f grph = go (contexts (mempty <$ grph)) mempty where

  go :: Map n (Neighbours n a) -> Map n a -> Map n a
  go acc res | null acc = res
  go acc res
    = let (next, acc') = Map.partition (null . parents) acc in
      let res' = Map.union res (value <$> next) in
      let update ngh =
            let (hits, miss) = Set.partition (`Map.member` next) (parents ngh) in
            let diff = foldMap (\ n -> f n (fromJust $ Map.lookup n res')) hits in
            ngh { parents = miss, value = value ngh <> diff }
      in go (update <$> acc') res'

dependencyNames :: Ord n => DependencyGraph n v -> Map n (Set n)
dependencyNames = transitively Set.insert

dependencyWeights :: Ord n => DependencyGraph n v -> Map n Int
dependencyWeights = fmap Set.size . dependencyNames

newtype SmallNat = SmallNat { getSmallNat :: Int }
  deriving (Eq, Ord, Num)

instance Bounded SmallNat where
  minBound = SmallNat 0
  maxBound = SmallNat maxBound

dependencyDepth :: Ord n => DependencyGraph n v -> Map n Int
dependencyDepth = fmap (getSmallNat . getMax) . transitively (const (1+))

------------------------------------------------------------------------
-- Converting back and forth to a DOT graph

fromDotGraph :: Ord n => DotGraph n -> Maybe (DependencyGraph n ())
fromDotGraph (DotGraph False True _ (DotStmts [] [] nodes edges))
  = do labels <- for nodes $ \case
         DotNode id [Label (StrLabel txt)] -> pure (id, txt)
         _ -> Nothing
       nghbrs <- fmap concat $ for edges $ \case
         DotEdge from to [] -> pure [ (to, Neighbours () mempty (Set.singleton from))
                                    , (from, Neighbours () (Set.singleton to) mempty)
                                    ]
         _ -> Nothing
       pure $ DependencyGraph (Map.fromList labels) (Map.fromListWith (<>) nghbrs)
isDependencyGraph _ = Nothing

toDotGraph :: forall n. Ord n => DependencyGraph n [Attribute] -> DotGraph n
toDotGraph (DependencyGraph labels nghbrs)
  = DotGraph False True Nothing (DotStmts [] [] nodes edges)

  where

    nodes :: [DotNode n]
    nodes = Map.toList labels <&> \ (lbl, nm) ->
      DotNode lbl $ Label (StrLabel nm)
                  : maybe [] value (Map.lookup lbl nghbrs)

    edges :: [DotEdge n]
    edges = flip concatMap (Map.toList nghbrs) $ \ (lbl, nghbr) ->
      flip (DotEdge lbl) [] <$> Set.toList (parents nghbr)

------------------------------------------------------------------------
-- Loading a graph from a file

fromFile :: FilePath -> IO (DependencyGraph String ())
fromFile fp = do
  file <- T.readFile fp
  let grph = fromGeneralised (parseDotGraph file :: Generalised.DotGraph String)
  case fromDotGraph grph of
    Nothing -> die "Invalid dependency graph"
    Just deps -> pure deps

toFile :: FilePath -> DependencyGraph String [Attribute] -> IO ()
toFile fp grph = T.writeFile fp (printDotGraph (toDotGraph grph))

------------------------------------------------------------------------
-- Scoring systems for graphs

data Scoring f = Scoring
  { degreeScore :: f
  , inTheFringe :: Bool
  }

arityScoring :: Floating f => Neighbours n v -> Scoring f
arityScoring ngh
  = Scoring (sqrt $ fromIntegral $ degreeWith (\ parents children -> parents + children * children) ngh)
            (null (parents ngh) || null (children ngh))

weightScoring :: Floating f => Neighbours n Int -> Scoring f
weightScoring ngh
  = let score = value ngh * Set.size (children ngh) in
    Scoring (fromIntegral score) (score == 0)

------------------------------------------------------------------------
-- Loading a graph from a file

shading :: Ord n
        => DependencyGraph n (Scoring Double)
        -> DependencyGraph n [Attribute]
shading grph
   = let scale = floor $ maximum (fmap degreeScore grph) :: Integer in
     grph <&> \ score ->
       let upper = fromIntegral (maxBound :: Word8)
           scaling = (floor (degreeScore score) * upper) `quot` scale
           weight = if inTheFringe score then maxBound
                    else maxBound - fromIntegral scaling
       in
       [ FillColor [toWC (RGB maxBound weight weight)]
       , Style [SItem Filled []]
       ]
