module ScratchCards where

import Control.Arrow
import Control.Monad
import Data.List.Split
import qualified Data.Set as S
import System.IO

main :: IO ()
main = withFile "./data/scratchcards" ReadMode $ \handle -> do
  cards <- liftM (readCards . lines) (hGetContents handle)
  let wins = map countWins cards
  let points = map pointsOnCard (filter (/= 0) wins)
  print (sum points)
  print (totalNumberOfCards wins)

pointsOnCard :: Int -> Int
pointsOnCard n = 2 ^ (n - 1)

countWins :: Cards -> Int
countWins c = intersectionSize (winningCards c) (playerCards c)

intersectionSize :: (Ord a) => S.Set a -> S.Set a -> Int
intersectionSize a b = S.size $ S.intersection a b

totalNumberOfCards :: [Int] -> Int
totalNumberOfCards points = countBonusCards (zip (repeat 1) points)

countBonusCards :: [(Int, Int)] -> Int
countBonusCards []     = 0
countBonusCards (x:xs) = error "UNIMPLEMENTED" -- n + go (applyN w acc xs)
  where n = fst x  -- number of copies (including original) of current card
        w = snd x  -- number of winning matchs on current card.
        acc = (+ (fst x)) *** id -- add bonus copies onto first value of tuple
      
applyN :: Int -> (a -> a) -> [a] -> [a]
applyN _ _ []     = []
applyN 0 _ xs     = xs
applyN n f (x:xs) = f x : (applyN (n - 1) f xs)  

data Cards = Cards {
  winningCards :: S.Set Int,
  playerCards  :: S.Set Int
}

readCards :: [String] -> [Cards]
readCards xs = map readSingleDeck xs
  where
    splitCards s = (map words . splitOn "|" . tail . dropWhile (/=':')) s
    readSingleDeck x = Cards { winningCards =  S.fromList (map read $ head (splitCards x))
                             , playerCards  = S.fromList (map read $ last (splitCards x))
                             }

