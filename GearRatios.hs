module GearRatios where

import Control.Monad
import Data.Char
import Data.List
import Data.List.GroupBy as G
import System.IO


main :: IO ()
main = withFile "./data/gear_ratios" ReadMode $ \handle -> do
  schematic <- liftM readSchematic (hGetContents handle)
  let parts = filter (adjacent (symbols schematic)) (engineParts schematic)
  print (sum $ map partNumber parts)
  print (sum $ gearRatios schematic)

-- Types
data Point = Point {
  x :: Int,
  y :: Int
} deriving (Ord, Show, Eq, Read)

type Range = (Point, Point)

data EnginePart = Part {
  partNumber :: Int,
  range :: Range
} deriving (Show, Read)

data Schematic = Schematic {
  engineParts :: [EnginePart],
  symbols :: [Point]
} deriving (Show, Read)

-- Utility.

gearRatios :: Schematic -> [Int]
gearRatios schematic = map (product' . gears) (symbols schematic)
  where gears s = map partNumber $ filter (adjacent [s]) (engineParts schematic)


product' :: [Int] -> Int
product' [x] = 0
product' xs  = product xs


adjacent :: [Point] -> EnginePart -> Bool
adjacent p ep = any (withinRange (range ep)) (concatMap expandPoint p)


withinRange :: Range -> Point -> Bool
withinRange r p = let minPoint = fst r
                      maxPoint = snd r
                  in (x p) >= (x minPoint) &&
                     (x p) <= (x maxPoint) &&
                     (y p) == (y minPoint)


expandPoint :: Point -> [Point]
expandPoint p = map (+p) adjacentPoints 


adjacentPoints :: [Point]
adjacentPoints =  map toPoint [(i, j) | i <- directions, j <- directions]
  where directions = [-1, 0, 1]

readSchematic :: String -> Schematic
readSchematic input = let rows = lines input
                          w = length (head rows)
                          h = length rows
                          grid = toGrid w h (concat rows)
                      in Schematic { engineParts = readEngineParts grid
                                   , symbols     = readSymbols grid
                                   }

type Square = ((Int, Int), Char)
type Grid = [Square]

toGrid :: Int -> Int -> String -> Grid
toGrid w h = zip [(j, i) | i <- [0..(w - 1)], j <- [0..(h - 1)]]

readEngineParts :: Grid -> [EnginePart]
readEngineParts grid = map toPart (G.groupBy nextTo onlyParts)
  where nextTo i j = (fst (fst j)) == ((fst (fst i)) + 1)
        onlyParts = filter (isNumber . snd) grid
        toPart s = Part { partNumber = read (map snd s) :: Int
                        , range = toRange $ map (toPoint . fst) s
                        }
                        
readSymbols :: Grid -> [Point]
readSymbols grid = map (toPoint . fst) symbols
  where symbols = filter ((=='*') . snd) grid
        -- to read all symbols for part 1 comment this out.
        -- filter (\c -> not (isAlphaNum (snd c)) && (snd c) /= '.') grid

toPoint :: (Int, Int) -> Point
toPoint (p, q) = Point { x = p, y = q }

toRange :: [Point] -> Range
toRange ps = (minimum ps, maximum ps)

instance Num Point where
  (+) p q = Point { x = (x p) + (x q)
                  , y = (y p) + (y q)
                  }
