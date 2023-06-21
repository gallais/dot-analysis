module Main where

import Data.Function (on)

import qualified Data.List as List
import Data.Maybe (fromJust)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Foldable (for_)
import Data.Traversable (for)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Data.GraphViz.Attributes.Complete (Attribute(Label), Label(..))
import Data.GraphViz.Types (parseDotGraph)
import Data.GraphViz.Types.Canonical

import System.Environment (getArgs)
import System.Exit (die)

type Node = String
type Label = String

data Neighbours n v = Neighbours
  { value    :: v
  , parents  :: Set n
  , children :: Set n
  } deriving (Show, Functor, Foldable, Traversable)

degreeWith :: (Int -> Int -> Int) -> Neighbours n v -> Int
degreeWith f ngh = (f `on` Set.size) (parents ngh) (children ngh)

degree :: Neighbours n v -> Int
degree = degreeWith (+)

data Scoring = Scoring
  { degreeScore :: Float
  , inTheFringe :: Bool
  }

scoring :: Neighbours n v -> Scoring
scoring ngh
  = Scoring (sqrt $ fromIntegral $ degreeWith (\ parents children -> parents + children * children) ngh)
            (null (parents ngh) || null (children ngh))

type Labels n = Map n Text

data DependencyGraph n v = DependencyGraph
  { labels   :: Labels n
  , contexts :: Map n (Neighbours n v)
  } deriving (Functor, Foldable, Traversable)

onContexts :: Ord n
           => (Neighbours n v -> Neighbours n w)
           -> (DependencyGraph n v -> DependencyGraph n w)
onContexts f (DependencyGraph labels contexts)
  = DependencyGraph labels (f <$> contexts)

instance (Ord n, Semigroup v) => Semigroup (Neighbours n v) where
  Neighbours v1 p1 c1 <> Neighbours v2 p2 c2
    = Neighbours (v1 <> v2) (p1 <> p2) (c1 <> c2)

isDependencyGraph :: Ord n => DotGraph n -> Maybe (DependencyGraph n ())
isDependencyGraph (DotGraph False True _ (DotStmts [] [] nodes edges))
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

colour :: Ord n
       => (Neighbours n v -> w)
       -> DependencyGraph n v -> DependencyGraph n w
colour f = onContexts $ \ ngh -> f ngh <$ ngh

display :: (Ord n, Show v) => Labels n -> (n, Neighbours n v) -> Text
display lbls (nm, ngh@(Neighbours v parents children))
  = T.unlines
  $ unsafeLookup nm <> T.pack (" (" ++ show v ++ ")")
  :  prefixedSet '↑' parents
  ++ prefixedSet '↓' children

  where
    prefixedSet = \ c -> map ((T.pack ("  " ++ [c]) <>) . unsafeLookup) . Set.toList
    unsafeLookup = fromJust . flip Map.lookup lbls

fromFile :: FilePath -> IO (DependencyGraph String ())
fromFile fp = do
  file <- T.readFile fp
  let grph = parseDotGraph file
  case isDependencyGraph grph of
    Nothing -> die "Invalid dependency graph"
    Just deps -> pure deps

top5 :: Ord n => DependencyGraph n Scoring -> [(n, Neighbours n Float)]
top5 = fmap (fmap (fmap degreeScore))
     . take 5
     . List.sortBy (flip (compare `on` (degreeScore . value . snd)))
     . Map.toList
     . Map.filter (not . inTheFringe . value)
     . contexts


main :: IO ()
main = do
  (fp : _) <- getArgs
  deps <- fromFile fp
  let heavies = top5 $ colour scoring deps
  for_ heavies $ T.putStrLn . display (labels deps)
