module Pangram (isPangram) where
import Data.Char (toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram sentence =
  let
    letters :: String
    letters = extractLetters sentence

    uniqueLetters :: String
    uniqueLetters = nub letters
    
  in
    length uniqueLetters == 26

extractLetters :: String -> String
extractLetters [] = []
extractLetters (c:cs)
  | isAsciiLetter c = toLower c : extractLetters(cs)
  | otherwise = extractLetters(cs)

isAsciiLetter :: Char -> Bool
isAsciiLetter c =
  let x = toLower c
  in 'a' <= x && x <= 'z'