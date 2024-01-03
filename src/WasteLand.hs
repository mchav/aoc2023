module WasteLand where

import qualified Data.Map as M
import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )
import System.IO ( hGetContents, withFile, IOMode(ReadMode) )

main :: IO ()
main =  withFile "./data/wasteland" ReadMode $ \handle -> do
    contents <- fmap lines (hGetContents handle)
    let moves = cycle $ map toDirection (head contents)
    let mappings = M.fromList $ map (toMapping . (\s -> getAllTextMatches (s =~ "[0-9A-Z]+") :: [String])) ((filter (not . null) . tail) contents)
    print (minSteps moves mappings)

minSteps :: [Direction] -> M.Map String Mapping -> Int
minSteps moves maps = let steps = map (\m -> minSteps' m 0 moves maps) (filter ((== 'A') . last) (M.keys maps))
                      in foldr lcm (head steps) steps

minSteps' :: String -> Int -> [Direction] -> M.Map String Mapping -> Int
minSteps' curr n moves mappings
    | last curr == 'Z' = n
    | otherwise     = minSteps' (apply (head moves) $ M.lookup curr mappings) (n + 1) (tail moves) mappings

apply :: Direction -> Maybe Mapping -> String
apply R (Just (Mapping _ r)) = r
apply L (Just (Mapping l _)) = l
apply _ _ = error "Could not apply"

data Direction = R | L deriving (Show, Read)

toDirection :: Char -> Direction
toDirection 'R' = R
toDirection 'L' = L
toDirection c   = error ("Invalid position: " ++ show c)

data Mapping = Mapping !String !String deriving (Show)

toMapping :: [String] -> (String, Mapping)
toMapping (a:x:y:_) = (a, Mapping x y)
toMapping xs = error ("Invalid row: " ++ show xs)
