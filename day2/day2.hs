{-# LANGUAGE DerivingStrategies #-}

import Helpers
import System.Directory (getCurrentDirectory)

main = do
  dir <- getCurrentDirectory
  content <- readFile (dir ++ "/day2/input.txt")
  let inputArr = (getInputArr content)
  print (scores inputArr)

data InputType = Rock | Paper | Scissors
  deriving (Enum)
  deriving (Show)

-- There should be a better way to not repeat each input type
input "A" = Rock
input "B" = Paper
input "C" = Scissors
input "X" = Rock
input "Y" = Paper
input "Z" = Scissors

-- Unsure how key-value works in haskell. running low on time to find out
inputScore :: InputType -> Int
inputScore Rock = 1
inputScore Paper = 2
inputScore Scissors = 3

data Winner = Opp | Player1 | Tie
  deriving (Enum)
  deriving (Show)

winner oppInput player1Input
  | show oppInput == show player1Input = Tie
  | (inputScore player1Input - inputScore oppInput) == 1 || (inputScore player1Input - inputScore oppInput) == -2 = Player1
  | otherwise = Opp

getRoundScore round = inputScore (input (round !! 1)) + matchScore (input (round !! 0)) (input (round !! 1))
  where
    matchScore o p
      | show (winner o p) == show Player1 = 6
      | show (winner o p) == show Tie = 3
      | otherwise = 0

scores [] = 0
scores (a : xs) = getRoundScore a + scores xs

getInputArr fileContent = [splitBy ' ' line | line <- lines fileContent]