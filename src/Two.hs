module Main where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import           System.Environment (getArgs)

main :: IO ()
main = do
  filename <- head <$> getArgs

  -- part one
  TIO.readFile filename >>= print . checksum partOneCounters . T.lines

  -- part two
  -- the set conversions here are super hacky because you cannot, in fact, return
  -- the "characters in common", you have to do that but maintain the original order.
  -- So, i have this set conversion stuff to whittle down my matches to 2 so that the
  -- fold behaves in a way that works appropriately.
  -- this is massively hacky but it's late 🤷‍♂️!
  TIO.readFile filename >>= print . L.foldl1' removeDifference . Set.toList . Set.fromList . fabricBoxIds . map T.unpack . T.lines

{- PART ONE: "checksumming" lists of ids -}

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

frequencies = T.foldl' count Map.empty
  where count m c = Map.alter (maybe one inc) c m
        one = Just 1
        inc = Just . succ

{- PART TWO: finding the "correct" box ids: box ids that differ by one character
             in the same position -}

fabricBoxIds ids = L.concatMap fst . filter snd $ recordMatch <$> ids <*> ids
  where recordMatch id1 id2 = ([id1, id2], id1 `matches` id2)

t1 `matches` t2 = if differences == 1 then True else False
  where differences = L.foldl1' (+) $ zipWith countDifference t1 t2
        countDifference c1 c2 = if c1 == c2 then 0 else 1

removeDifference s1 s2 = reverse $ go [] s1 s2
  where go out [] [] = out
        go out [] s2 = out ++ s2
        go out s1 [] = out ++ s1
        go out (c1:s1) (c2:s2) = if c1 /= c2 then go out s1 s2 else go (c1:out) s1 s2

{- This was my original approach to getting the "letters in common"
   by kind of inverting the normal set monoid.

   in the end i couldn't use it since the challenge actualy wants the order of the letters
   to be preserved.

   i really like it so i'm leaving it here anyway.

newtype LetterSet = LS (Set Char)

instance Semigroup LetterSet where
  (LS s1) <> (LS s2) = LS $ s1 `Set.intersection` s2

instance Monoid LetterSet where
  mempty = LS $ Set.fromList [minBound..maxBound]

letterSet = LS . Set.fromList

letters (LS s) = Set.toList s
-}
