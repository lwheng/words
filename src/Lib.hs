module Lib (
    cell2char
  , coordsGrid
  , findWord
  , completed
  , findWordInCellLinePrefix
  , playGame
  , formatGame
  , findWordInLine
  , outputGame
  , findWords
  , formatGrid
  , gridWithCoords
  , makeGame
  , outputGrid
  , score
  , skew
  , totalWords
  , zipOverGrid
  , zipOverGridWith

  , Cell (..)
  , Game (..)
) where

import           Data
import qualified Data.List as List
import qualified Data.Map as M
import qualified Data.Maybe as Maybe

data Game = Game
              { gameGrid :: Grid Cell
              , gameWords :: M.Map String (Maybe [Cell])
              }
          deriving (Show)

data Cell = Cell (Integer, Integer) Char
          | Indent
          deriving (Eq, Ord, Show)

makeGame :: Grid Char -> [String] -> Game
makeGame mGrid mWords =
  let
    gwc = gridWithCoords mGrid
    list = map (\w -> (w, Nothing)) mWords
    dict = M.fromList list
  in
    Game gwc dict

totalWords :: Game -> Int
totalWords = length . M.keys . gameWords

score :: Game -> Int
score = length . Maybe.catMaybes . M.elems . gameWords

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word =
  case findWord (gameGrid game) word of
    Nothing -> game
    Just cs -> game { gameWords = M.insert word (Just cs) $ gameWords game }

formatGame :: Game -> String
formatGame game = formatGrid (gameGrid game)
               ++ "\n\n"
               ++ (show $ score game)
               ++ "/"
               ++ (show $ totalWords game)

completed :: Game -> Bool
completed game = totalWords game == score game

outputGame :: Game -> IO ()
outputGame = putStrLn . formatGame

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
  let
    rows = map repeat [0..]
    cols = repeat [0..]
  in
    zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords mGrid = zipOverGridWith Cell coordsGrid mGrid


formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent     = '_'

outputGrid :: Grid Cell -> IO ()
outputGrid = putStrLn . formatGrid

getLines :: Grid Cell -> [[Cell]]
getLines mGrid = mLines ++ (map reverse mLines)
  where
    horizontal = mGrid
    vertical   = List.transpose mGrid
    diagonal1  = diagonalize mGrid
    diagonal2  = diagonalize (map reverse mGrid)
    mLines     = horizontal ++ vertical ++ diagonal1 ++ diagonal2

diagonalize :: Grid Cell -> Grid Cell
diagonalize = List.transpose . skew

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where
    indent line = Indent : line

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord mGrid word = Maybe.listToMaybe . Maybe.catMaybes $ foundWords
  where
    mLines = getLines mGrid
    foundWords = map (findWordInLine word) mLines

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords mGrid mWords = Maybe.catMaybes $ map (findWord mGrid) mWords

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _     []   = Nothing
findWordInLine mWord line =
  case findWordInCellLinePrefix [] mWord line of
    Nothing -> findWordInLine mWord (tail line)
    Just cs -> Just cs


findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x : xs) (c : cs)
    | x == cell2char c = findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing
