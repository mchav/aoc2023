{-# LANGUAGE StrictData #-}
{-# LANGUAGE StrictData #-}
module Fertilizer (main) where

import Control.Monad
import Data.List (sort)
import System.IO
import Text.Regex.TDFA

main :: IO ()
main = withFile "./data/fertilizer" ReadMode $ \handle -> do
  contents <- hGetContents handle
  let allNumbers = getAllTextMatches (contents =~ "[0-9 ]+") :: [String]
  let seeds = expandSeeds $ map (read :: String -> Int) $ words (head allNumbers)
  let allMappings = readAllMappings allNumbers
  let seedLocations = map (getLocations allMappings) seeds
  print (minimum seedLocations)

readAllMappings :: [String] -> [[(Int, Int, Int)]]
readAllMappings [] = []
readAllMappings s = readMappings processed : readAllMappings processed
  where processed = if null (dropWhile (/= " ") s) then [] else tail $ dropWhile (/= " ") s

readMappings :: [String] -> [(Int, Int, Int)]
readMappings xs = sort $ map (toTuple . map (read :: String -> Int) . words) $ takeWhile (/= " ") xs

data Range = Range {
  start :: Int,
  end :: Int,
  offset :: Int,
  image :: Int
}

withinRange :: Int -> Range -> Bool
withinRange n r = n >= start r && n <= end r

toTuple :: [a] -> (a, a, a)
toTuple [t, s, n] = (s, t, n)
toTuple _ = error "Tuple doesn't match"

first, second, third :: (a, a, a) -> a
first (a, _, _)  = a
second (_, a, _) = a
third (_, _, a)  = a

toRanges :: [(Int, Int, Int)] -> [Range]
toRanges = map toRange

expandSeeds :: [Int] -> [Int]
expandSeeds [] = []
expandSeeds xs = [(head xs)..(head xs + (xs !! 1) - 1)] ++ expandSeeds (drop 2 xs)

toRange :: (Int, Int, Int) -> Range
toRange (s, t, n) = Range { start = s,
                            end = s + n - 1,
                            offset = t - s,
                            image = t}

calculateMapping :: Int -> [Range] -> Int
calculateMapping n = foldr (\r acc -> if withinRange n r then n + offset r else acc) n

getLocations :: [[(Int, Int, Int)]] -> Int -> Int
getLocations mappings s = foldl calculateMapping s (map toRanges mappings)
