{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec as P

import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  fname <- head <$> getArgs
  partOne fname

partOne fname = do
  claims <- parseClaims . T.lines <$> TIO.readFile fname
  either (const exitFailure) (print . presenceArea . presenceAbove 1 . foldMap claimPresence) claims

parseClaims = mapM parseClaim

parseClaim :: Text -> Either P.ParseError Claim
parseClaim = P.parse claimParser ""
  where claimParser = Claim <$> id <*> point <*> dimensions
        id = P.char '#' >> digits <* P.space
        point = P.string "@ " >> ((:@) <$> digits <* P.char ',' <*> digits)
        dimensions = P.string ": " >> ((:*) <$> digits <* P.char 'x' <*> digits)
        digits = P.many P.digit >>= pure . read

{- PART ONE -}
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

newtype Presences = S (Map Point Int)
  deriving Show

instance Semigroup Presences where
  (S m1) <> (S m2) = S $ Map.unionWith (+) m1 m2

instance Monoid Presences where
  mempty = S Map.empty

presenceArea (S m) = Map.size m

presenceAbove n (S m) = S $ Map.filter (>n) m

claimPresence (Claim _ (x :@ y) (w :* h)) = S . Map.fromList $ zipWith (,) points ones
  where ones = repeat 1
        points = (:@) <$> [x..x+w-1] <*> [y..y+h-1]
