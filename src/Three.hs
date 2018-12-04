{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.List (foldl', foldl1')
import           Data.Maybe (fromJust)

import           Data.IntSet (IntSet)
import qualified Data.IntSet as Set

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import qualified Text.Parsec as P

import           System.Environment (getArgs)
import           System.Exit (exitFailure)

main :: IO ()
main = do
  fname <- head <$> getArgs
  partOne fname
  partTwo fname

parseClaims = mapM parseClaim

parseClaim :: Text -> Either P.ParseError Claim
parseClaim = P.parse claimParser ""
  where claimParser = Claim <$> id <*> point <*> dimensions
        id = P.char '#' >> digits <* P.space
        point = P.string "@ " >> ((:@) <$> digits <* P.char ',' <*> digits)
        dimensions = P.string ": " >> ((:*) <$> digits <* P.char 'x' <*> digits)
        digits = P.many P.digit >>= pure . read

data Claim = Claim
  { claimId :: Int
  , claimPoint :: Point
  , claimDimensions :: Dimensions
  }
  deriving Show

data Point = Int :@ Int
  deriving (Show, Ord, Eq)

data Dimensions = Int :* Int
  deriving (Show, Ord, Eq)

newtype Presences = S (Map Point IntSet)
  deriving Show

instance Semigroup Presences where
  (S m1) <> (S m2) = S $ Map.unionWith Set.union m1 m2

instance Monoid Presences where
  mempty = S Map.empty

{- PART ONE -}

partOne fname = do
  claims <- parseClaims . T.lines <$> TIO.readFile fname
  either (const exitFailure) (print . presenceArea . presenceOf (>1) . foldMap claimPresence) claims

presenceArea (S m) = Map.size m

presenceOf p (S m) = S $ Map.filter (p . Set.size) m

(S m2) `inPresence` (S m) = Map.size (m Map.\\ m2) > 0

claimPresence (Claim i (x :@ y) (w :* h)) = S . Map.fromList $ zipWith (,) points ids
  where ids = repeat $ Set.fromList [i]
        points = (:@) <$> [x..x+w-1] <*> [y..y+h-1]

{- PART TWO -}

partTwo fname = do
  claims <- parseClaims . T.lines <$> TIO.readFile fname
  either (const exitFailure) (print . head . Set.toList . freeClaim) claims

freeClaim claims =
  let superPresence = foldMap claimPresence claims
      (S overlaps) = presenceOf (>1) superPresence
      (S independents) = presenceOf (== 1) superPresence
  in (Map.foldl' (<>) mempty independents) Set.\\ (Map.foldl' (<>) mempty overlaps)
