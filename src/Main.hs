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

type Node = String
type Label = String

data Neighbours n =  Neighbours
  { parents  :: Set n
  , children :: Set n
  } deriving (Show)

degreeWith :: (Int -> Int -> Int) -> Neighbours n -> Int
degreeWith f ngh = (f `on` Set.size) (parents ngh) (children ngh)

degree :: Neighbours n -> Int
degree = degreeWith (+)

score :: Neighbours n -> Float
score = sqrt . fromIntegral . degreeWith (\ parents children ->
  parents + children * children)

type Labels n = Map n Text

data DependencyGraph n = DependencyGraph
  { labels :: Labels n
  , edges  :: Map n (Neighbours n)
  }

instance Ord n => Semigroup (Neighbours n) where
  Neighbours p1 c1 <> Neighbours p2 c2
    = Neighbours (p1 <> p2) (c1 <> c2)

isDependencyGraph :: Ord n => DotGraph n -> Maybe (DependencyGraph n)
isDependencyGraph (DotGraph False True _ (DotStmts [] [] nodes edges))
  = do labels <- for nodes $ \case
         DotNode id [Label (StrLabel txt)] -> pure (id, txt)
         _ -> Nothing
       nghbrs <- fmap concat $ for edges $ \case
         DotEdge from to [] -> pure [ (to, Neighbours mempty (Set.singleton from))
                                    , (from, Neighbours (Set.singleton to) mempty)
                                    ]
         _ -> Nothing
       pure $ DependencyGraph (Map.fromList labels) (Map.fromListWith (<>) nghbrs)
isDependencyGraph _ = Nothing

display :: Ord n => Labels n -> (n, Neighbours n) -> Text
display lbls (nm, Neighbours parents children)
  = T.unlines
  $ unsafeLookup nm
  :  prefixedSet '↑' parents
  ++ prefixedSet '↓' children

  where
    prefixedSet = \ c -> map ((T.pack ("  " ++ [c]) <>) . unsafeLookup) . Set.toList
    unsafeLookup = fromJust . flip Map.lookup lbls

main :: IO ()
main = do
  (fp : _) <- getArgs
  file <- T.readFile fp
  let grph = (parseDotGraph file :: DotGraph String)
  case isDependencyGraph grph of
    Nothing -> putStrLn "oops"
    Just deps -> do
      let heavies = List.sortBy (flip (compare `on` (score . snd)))
                  $ filter (\ (_, ngh) -> degreeWith (*) ngh /= 0)
                  $ Map.toList (edges deps)
      for_ (take 5 heavies) $ T.putStrLn . display (labels deps)
