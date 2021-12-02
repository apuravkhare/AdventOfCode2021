module Day2 where

import Data.List
import Debug.Trace

parseInstructions :: [String] -> Int -> Int -> Int
parseInstructions [] h v = h * v
parseInstructions (x:xs) h v
  | isPrefixOf "forward" x = parseInstructions xs ((read (drop 8 x)::Int) + h) v
  | isPrefixOf "down" x = parseInstructions xs h ((read (drop 5 x)::Int) + v)
  | isPrefixOf "up" x = parseInstructions xs h (v - (read (drop 3 x)::Int))

solveA :: IO()
solveA =
  do input <- readFile "./inputs/day2.txt"
     line <- return (lines input)
     putStrLn (show (parseInstructions line 0 0))

parseInstructionsB :: [String] -> Int -> Int -> Int -> Int
parseInstructionsB _ h v a | trace (show h ++ " " ++ show v ++ " " ++ show a) False = undefined
parseInstructionsB [] h v a = h * v
parseInstructionsB (x:xs) h v a
  | isPrefixOf "forward" x =
    let val = (read (drop 8 x)::Int)
    in parseInstructionsB xs (val + h) (v + (a * val)) a
  | isPrefixOf "down" x = 
    let val = (read (drop 5 x)::Int)
    in parseInstructionsB xs h v (a + val)
  | isPrefixOf "up" x =
    let val = (read (drop 3 x)::Int)
    in parseInstructionsB xs h v (a - val)

solveB :: IO()
solveB =
  do input <- readFile "./inputs/day2.txt"
     line <- return (lines input)
     putStrLn (show (parseInstructionsB line 0 0 0))