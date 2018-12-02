module Main where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import           System.Environment (getArgs)

main :: IO ()
main = do
  filename <- head <$> getArgs

  -- part one
  TIO.readFile filename >>= print . checksum partOneCounters . T.lines

checksum :: [Counter] -> [Text] -> Int
checksum counters = L.foldl1' (*) . count counters

partOneCounters = [twos, threes]

type Counter = Text -> Int

newtype Counters = Counters [Counter]

newtype Counts = Counts [Int]

instance Semigroup Counts where
  (Counts xs) <> (Counts ys) = Counts $ zipWith (+) xs ys

instance Monoid Counts where
  mempty = Counts $ repeat 0

count :: [Counter] -> [Text] -> [Int]
count counters ids =
  let (Counts cs) = foldMap (runCounters counters) ids
  in cs

runCounters counters t = Counts $ counters <*> pure t

countIf pred = \t -> if pred t then 1 else 0

twos :: Counter
twos = countIf $ hasFrequency 2 . frequencies

threes :: Counter
threes = countIf $ hasFrequency 3 . frequencies

hasFrequency x = any (== x) . Map.elems

frequencies t = T.foldl' count Map.empty t
  where count m c = Map.alter (maybe one inc) c m
        one = Just 1
        inc = Just . succ
