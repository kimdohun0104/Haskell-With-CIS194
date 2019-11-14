-- chcp 65001 for Permission Denied error

module Main where

-- Card Number vaildator
toDigits :: Integer -> [Integer]
toDigits x
    | x > 0 = toDigits (x `div` 10) ++ [x `mod` 10]
    | x == 0 = []

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x1:x2:xs) = x1: x2*2 : doubleEveryOtherFromLeft xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOtherFromLeft (reverse x))

sumEachDigit :: Integer -> Integer
sumEachDigit x = 1 + x `mod` 10

sumTenDigit :: Integer -> Integer
sumTenDigit x
    | x >= 10 = sumEachDigit x
    | otherwise = x

sumDigits :: [Integer] -> Integer
sumDigits [] = -1
sumDigits [x] = if x >= 10 then sumEachDigit x else x
sumDigits (x:xs) = sumTenDigit x + sumDigits xs

validateCardNum :: Integer -> Bool
validateCardNum x
    | x `mod` 10 == 0 = True
    | otherwise = False

main :: IO ()
main = print (validateCardNum $ sumDigits $ doubleEveryOther $ toDigits 513)
