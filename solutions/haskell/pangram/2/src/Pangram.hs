module Pangram (isPangram) where
import Data.Char (toLower, isAsciiLower, isAsciiUpper)
import Data.List (nub)

isPangram :: String -> Bool
isPangram sentence =
  length (nub normalizedLetters) == 26
  where
    normalizedLetters :: String
    normalizedLetters =
      map toLower (filter isAsciiAlphabet sentence)

isAsciiAlphabet :: Char -> Bool
isAsciiAlphabet c = isAsciiLower c || isAsciiUpper c
