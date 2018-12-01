module Main where

import           Control.Monad ((>=>))
import           Data.List (foldl1')
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import           System.Environment (getArgs)

main :: IO ()
main = do
  filename <- head <$> getArgs
  result <- sumContents filename
  putStrLn $ show result

sumContents :: String -> IO Int
sumContents = TIO.readFile >=> return . sumLines

sumLines :: T.Text -> Int
sumLines = foldl1' (+) . map (parse . T.unpack) . T.lines

parse :: String -> Int
parse ('+':s) = read s
parse s = read s
