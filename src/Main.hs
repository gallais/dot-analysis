module Main where

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Data.GraphViz.Types (parseDotGraph)
import Data.GraphViz.Types.Canonical
import System.Environment (getArgs)

type Node = String
type Label = String

data DependencyGraph = DependencyGraph
  { nodes :: Map Node Label
  , edges :: Map Node [Node]
  }

processSts :: DotStatements n -> DotStatements n
processSts sts@(DotStmts [] [] nodes edges) = sts
processSts sts = error "Invalid dot statements"

process :: DotGraph n -> DotGraph n
process (DotGraph strict True mid sts) = DotGraph strict True mid (processSts sts)
process grph = error "Invalid dot graph"

main :: IO ()
main = do
  (fp : _) <- getArgs
  file <- T.readFile fp
  let grph = (parseDotGraph file :: DotGraph String)
  putStrLn "Hi, I'm dot-annot"
  print grph
