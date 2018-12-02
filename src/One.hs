module Main where

import           Control.Monad ((>=>))
import           Data.List (foldl1', groupBy)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import           Prelude hiding (sum)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

main :: IO ()
main = do
  filename <- head <$> getArgs
  -- part 1
  -- fileLines filename >>= print . sum . parseLines

  -- part 2
  fileLines filename >>= maybe exitFailure print . firstDuplicate . deviceFrequencies . cycle . parseLines


fileLines = TIO.readFile >=> return . T.lines

parseLines = map (parse . T.unpack)

sum = foldl1' (+)

deviceFrequencies = scanl1 (+)

firstDuplicate xs = go Set.empty xs
  where go seen (x:xs') = if x `Set.member` seen then Just x else go (Set.insert x seen) xs'
        go _ [] = Nothing

-- adding a type annotation here to specialize `read`
parse :: String -> Int
parse ('+':s) = read s
parse s = read s
