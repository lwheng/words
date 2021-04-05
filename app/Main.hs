module Main where

import Data
import Lib
import System.IO
import qualified System.Random as R

main :: IO ()
main = do
  gen <- R.newStdGen
  let
    filledInGrid = fillInBlanks gen grid
  hSetBuffering stdout NoBuffering
  playTurn $ makeGame filledInGrid languages

playTurn :: Game -> IO ()
playTurn game = do
  outputGame game
  putStr "Please enter a word> "
  word <- getLine
  let
    newGame = playGame game word
  if completed newGame then
    putStrLn "Congratulations!"
  else
    playTurn newGame
