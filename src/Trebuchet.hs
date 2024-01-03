{-# LANGUAGE OverloadedStrings #-}
module Trebuchet where

import Control.Monad
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO

main = withFile "./data/trebuchet" ReadMode $ \handle -> do
  xs <- liftM (filter (/= T.empty) . T.lines) (TIO.hGetContents handle)
  let numbers = map calibrationValue xs
  print (sum numbers)

calibrationValue :: T.Text -> Int
calibrationValue v = (head nums * 10) + (last nums)
  where nums = filter (>0) $ convertToNumbers v

convertToNumbers :: T.Text -> [Int]
convertToNumbers txt
  | T.empty == txt = []
  | otherwise      = textToNumber txt : (convertToNumbers (T.tail txt))

textToNumber :: T.Text -> Int
textToNumber txt
  | T.empty == txt           = 0
  | isNumber (T.head txt)    = digitToInt (T.head txt)
  | T.isPrefixOf "one"   txt = 1
  | T.isPrefixOf "two"   txt = 2
  | T.isPrefixOf "three" txt = 3
  | T.isPrefixOf "four"  txt = 4
  | T.isPrefixOf "five"  txt = 5
  | T.isPrefixOf "six"   txt = 6
  | T.isPrefixOf "seven" txt = 7
  | T.isPrefixOf "eight" txt = 8
  | T.isPrefixOf "nine"  txt = 9
  | otherwise                = 0

