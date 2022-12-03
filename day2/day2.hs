{-# LANGUAGE DerivingStrategies #-}

import Helpers
import System.Directory (getCurrentDirectory)

main = do
  dir <- getCurrentDirectory
  content <- readFile (dir ++ "/day2/input.txt")
  let inputArr = (getInputArr content)
  print [getRoundScore x | x <- inputArr]
  print (scores inputArr)

data InputType = Rock | Paper | Scissors
  deriving (Enum)
  deriving (Show)

-- There should be a better way to not repeat each input type
input "A" = Rock
input "B" = Paper
input "C" = Scissors

desiredOutcome "X" = Opp
desiredOutcome "Y" = Tie
desiredOutcome "Z" = Player1

getPlayer1Choice :: Winner -> InputType -> InputType
getPlayer1Choice outcome oppChoice = getChoice [Rock, Paper, Scissors]
  where
    getChoice (a : xs)
      | show (winner oppChoice a) == show outcome = a
      | otherwise = getChoice xs

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

getRoundScore round = inputScore (getPlayer1Choice outcome (input (round !! 0))) + matchScore outcome
  where
    outcome = desiredOutcome (round !! 1)
    matchScore r
      | show r == show Player1 = 6
      | show r == show Tie = 3
      | otherwise = 0

scores [] = 0
scores (a : xs) = getRoundScore a + scores xs

getInputArr fileContent = [splitBy ' ' line | line <- lines fileContent]