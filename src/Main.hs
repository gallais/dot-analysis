module Main where

import Data.Functor ((<&>))
import Data.Function (on)

import qualified Data.List as List
import Data.Maybe (fromJust)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Foldable (for_)


import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Data.Word (Word8)

import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName, (</>))

import Data.DependencyGraph

display :: (Ord n, Show v) => Labels n -> (n, Neighbours n v) -> Text
display lbls (nm, ngh@(Neighbours v parents children))
  = T.unlines
  $ unsafeLookup nm <> T.pack (" (" ++ show v ++ ")")
  :  prefixedSet '↑' parents
  ++ prefixedSet '↓' children

  where
    prefixedSet c = map ((T.pack ("  " ++ [c]) <>) . unsafeLookup) . Set.toList
    unsafeLookup = fromJust . flip Map.lookup lbls

top5 :: (Ord n, Ord f) => DependencyGraph n (Scoring f) -> [(n, Neighbours n f)]
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
--  let coloured = colour (const arityScoring) deps
  let weights  = dependencyWeights deps
  let coloured = mapWithKey (\ n ngh -> weightScoring (fromJust (Map.lookup n weights) <$ ngh) <$ ngh) deps

{-
  let depNames = Map.fromList
               $ zipWith3 (\ (k, v1) (_, v2) (_, v3) -> (k, (v1, v2, v3)))
                 (Map.toList $ dependencyDepth coloured)
                 (Map.toList $ dependencyWeights coloured)
                 (Map.toList $ dependencyNames coloured)
  let heavies = top5 coloured
  for_ heavies $ \ heavy -> do
    let name = fst heavy
    let (depth, weight, dependencies) = fromJust $ Map.lookup name depNames
    T.putStrLn $ display (labels deps) heavy
    T.putStrLn $ T.unlines
      $ fromJust (Map.lookup name $ labels deps) <> T.pack (concat [ " (", show depth, ", ", show weight, ", ", show (Set.size dependencies), ")"])
      : map ((T.pack "  " <>) . fromJust . flip Map.lookup (labels deps)) (Set.toList dependencies)
-}
  toFile (takeDirectory fp </> "updated-" <> takeFileName fp) (shading coloured)
