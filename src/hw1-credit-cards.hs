{-# OPTIONS_GHC -Wall #-}

module Homework1 where

toDigitsReverse :: Integer -> [Integer]
toDigitsReverse n
  | n <= 0    = []
  | otherwise = n `mod` 10 : toDigitsReverse (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsReverse n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x:[])   = [x]
doubleEveryOther (x:y:zs) = x : (y * 2) : doubleEveryOther zs

doubleEveryOtherReverse :: [Integer] -> [Integer]
doubleEveryOtherReverse xs = reverse $ doubleEveryOther $ reverse xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ fmap (sum . toDigitsReverse) xs

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigitsReverse) x `mod` 10 == 0
