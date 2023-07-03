module Main where

import Control.Monad (unless, when)

import Data.Foldable (for_)
import Data.Function (on)

import qualified Data.List as List
import Data.Maybe (fromJust)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Options.Applicative
  (Parser, execParser, (<**>)
  , info, helper
  , fullDesc, progDesc, header
  , str
  , flag, argument
  , long, metavar, help
  , completer, bashCompleter)

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

data Options = Options
  { arities :: Bool
  , weights :: Bool
  , withTop :: Bool
  , graph   :: FilePath
  }

poptions :: Parser Options
poptions = Options
  <$> flag False True (long "arity"  <> help "Highlight the graph using arities")
  <*> flag False True (long "weight" <> help "Highlight the graph using weights")
  <*> flag False True (long "top"    <> help "List top 5 modules on the command line")
  <*> argument str (metavar "FILE" <> completer (bashCompleter "file") <> help "Input file")

listTop5 :: (Ord n, Ord f, Show f) => DependencyGraph n (Scoring f) -> IO ()
listTop5 grph = do
  for_ (top5 grph) $ T.putStrLn . display (labels grph)

main :: IO ()
main = do
  -- parsing and sanity checking options
  opts <- execParser (info (poptions <**> helper)
                     (fullDesc <> progDesc "run requested analysis on graph"
                               <> header "dot-analysis - analysing dependency graphs produced by Agda"))
  opts <- pure $ if arities opts || weights opts
                 then opts else opts { weights = True }

  -- filename business
  let fpath = graph opts
  let dir   = takeDirectory fpath
  let fname = takeFileName fpath
  let mkFP prefix = dir </> prefix <> fname

  -- load graph
  deps <- fromFile fpath

  -- perform analyses
  when (arities opts) $ do
    let coloured = mapWithKey (\ n ngh -> arityScoring ngh <$ ngh) deps
    toFile (mkFP "arities-") (shading coloured)
    when (withTop opts) $ listTop5 coloured

  when (weights opts) $ do
    let weights  = dependencyWeights deps
    let coloured = mapWithKey (\ n ngh -> weightScoring (fromJust (Map.lookup n weights) <$ ngh) <$ ngh) deps
    toFile (mkFP "weights-") (shading coloured)
    when (withTop opts) $ listTop5 coloured
