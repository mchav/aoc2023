module Cube where

import Control.Monad
import Data.Char
import Data.List.Split
import qualified Data.Map as M
import System.IO

data Colour = Red | Green | Blue deriving (Ord, Eq)

data Draw = Draw {
  red   :: Int,
  green :: Int,
  blue  :: Int
} deriving (Show)

data Game = Game {
  gameId  :: Int,
  draws   :: [Draw]
} deriving (Show)


main = withFile "./data/cube" ReadMode $ \handle -> do
  games <- liftM (map readGame . lines) (hGetContents handle)
  let draw = Draw { red = 12, green = 13, blue = 14 }
  let feasibleGameIds = map gameId $ filter (feasible draw) games
  let powerSets = sum $ map (powerSet . maxPossible . draws) games
  print (sum feasibleGameIds)
  print powerSets

feasible :: Draw -> Game -> Bool
feasible draw game = all isWithinBounds (draws game)
  where isWithinBounds d = and [ (red d) <= (red draw)
                               , (green d) <= (green draw)
                               , (blue d) <= (blue draw)
                               ]

powerSet :: Draw -> Int
powerSet d = (red d) * (blue d) * (green d)

maxPossible :: [Draw] -> Draw
maxPossible ds = Draw { red = maximum (map red ds)
                      , green = maximum (map green ds)
                      , blue = maximum (map blue ds)
                      }

readColour :: String -> Colour
readColour "red"   = Red
readColour "green" = Green
readColour "blue"  = Blue
readColour _       = error "Unknown colour"



splitDraws :: String -> [String]
splitDraws = splitOn ";"

splitCubes :: String -> [String]
splitCubes = splitOn ","

parseCube :: String -> (Colour, Int)
parseCube input = (readColour colour, read n :: Int) 
  where (n:colour:_) = words input
  
countCubes :: String -> M.Map Colour Int
countCubes input = M.fromList $ map parseCube (splitCubes input)

readDraw :: String -> Draw
readDraw []    = Draw { red = 0, blue = 0, green = 0 }
readDraw input = let cubeCount = countCubes input 
                 in Draw { red   = M.findWithDefault 0 Red cubeCount
                           , blue  = M.findWithDefault 0 Blue cubeCount
                           , green = M.findWithDefault 0 Green cubeCount
                           }

readGame :: String -> Game
readGame []    = error "Empty game"
readGame input = let (g:d:[]) = splitOn ":" input
                 in Game { gameId = read (dropWhile isAlpha g) :: Int
                         , draws  = map readDraw (splitDraws d)
                         }


