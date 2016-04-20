{-# OPTIONS_GHC -Wall #-}

toDigitsRev ::  Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = (mod n 10) : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleHelper :: [Integer] -> [Integer]
doubleHelper (x:y:xs) = x:(y*2):doubleHelper xs
doubleHelper xs = xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleHelper . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . digiter

digiter :: [Integer] -> [Integer]
digiter [] = []
digiter (x:xs)
  | x >= 10 = (div x 10) : (mod x 10) : digiter xs
  | otherwise = x : digiter xs

validate :: Integer -> Bool
validate n
  | mod x 10 == 0 = True
  | otherwise     = False
  where x = sumDigits $ doubleEveryOther $ toDigits n
  --    x = sumDigits $ doubleHelper $ toDigitsRev n

----------------------------------------------------------------

type Peg  = String
type Move = (Peg,Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

