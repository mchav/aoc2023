module Wait (main) where

import Control.Monad
import Data.Char
import System.IO

main :: IO ()
main = withFile "./data/wait" ReadMode $ \handle -> do
  contents <- fmap lines (hGetContents handle)
  let timesStr = dropWhile (not . isNumber) $ head contents
  let distancesStr = dropWhile (not . isNumber) $ last contents
  let times = map (read :: String -> Int) $ words $ filter (not . isSpace) timesStr
  let distances = map (read :: String -> Int) $ words $ filter (not . isSpace) distancesStr
  let timeWithDistance = zip times distances
  print (product $ map solutions timeWithDistance)


solutions :: (Int, Int) -> Int
solutions (t, d) = 2 * foldr (\i acc -> if (t - i) * i > d then acc + 1 else acc) 0 [0..(t `div` 2)]
