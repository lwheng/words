module Lib (
    formatGrid
  , outputGrid
  , findWord
  , findWords
  , findWordInLine
  , skew
) where

import           Data
import qualified Data.List as List
import qualified Data.Maybe as Maybe

formatGrid :: Grid -> String
formatGrid = unlines

outputGrid :: Grid -> IO ()
outputGrid = putStrLn . formatGrid

getLines :: Grid -> [String]
getLines grid = lines ++ (map reverse lines)
  where
    horizontal = grid
    vertical   = List.transpose grid
    diagonal1  = diagonalize grid
    diagonal2  = diagonalize (map reverse grid)
    lines      = horizontal ++ vertical ++ diagonal1 ++ diagonal2

diagonalize :: Grid -> Grid
diagonalize = List.transpose . skew

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where
    indent l = '_' : l

findWord :: Grid -> String -> Maybe String
findWord grid word =
  case or $ map (findWordInLine word) lines of
    False -> Nothing
    True  -> Just word
  where
    lines = getLines grid

findWords :: Grid -> [String] -> [String]
findWords grid words = Maybe.catMaybes $ map (findWord grid) words

findWordInLine :: String -> String -> Bool
findWordInLine = List.isInfixOf
