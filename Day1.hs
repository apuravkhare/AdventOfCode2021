module Day1 where

count :: (Ord a) => [a] -> Int -> Int
count [] c  = c
count [x] c = c
count (x:y:xs) c
  | x < y     = count (y:xs) (c + 1)
  | otherwise = count (y:xs) c

solveA :: IO ()
solveA = 
  do input <- readFile "./inputs/day1.txt"
     numbers <- return (map (\s -> read s::Int) (lines input))
     putStrLn (show (count numbers 0))

countWindow :: [Int] -> Int -> Int -> Int
countWindow (x:y:z:xs) (-1) c = countWindow (y:z:xs) (x+y+z) 0
countWindow (x:y:z:xs) s c
  | (x+y+z) > s = countWindow (y:z:xs) (x+y+z) (c+1)
  | otherwise   = countWindow (y:z:xs) (x+y+z) c
countWindow _ _ c = c

solveB :: IO ()
solveB =
  do input <- readFile "./inputs/day1.txt"
     numbers <- return (map (\s -> read s::Int) (lines input))
     putStrLn (show (countWindow numbers (-1) 0))