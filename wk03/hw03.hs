{-# OPTIONS_GHC -Wall #-}

module Golf where
import Data.List

skips :: [a] -> [[a]]
skips xs = map lol [1..(length xs)]
  where truth n = cycle $ reverse $ take n $ True:repeat False
        lol     = skipHelper xs . truth

skipHelper :: [a] -> [Bool] -> [a]
skipHelper (x:xs) (t:ts)
  | t = x: skipHelper xs ts
  |otherwise = skipHelper xs ts
skipHelper _ _ = []

-----------------------------------------

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > x && y > z = y:localMaxima (z:xs)
  | otherwise = localMaxima (y:z:xs)
localMaxima _ = []

-----------------------------------------
 
histogram :: [Integer] -> String
histogram = show . group . sort
