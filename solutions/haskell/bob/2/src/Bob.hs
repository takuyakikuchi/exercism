module Bob (responseFor) where
import Data.Char (toUpper, isSpace, isAsciiUpper, isAlpha)

responseFor :: String -> String
responseFor input
  | isSilence message = "Fine. Be that way!"
  | isQuestion message && isYell message = "Calm down, I know what I'm doing!"
  | isQuestion message = "Sure."
  | isYell message = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    message = trim input

isSilence :: String -> Bool
isSilence = null

isQuestion :: String -> Bool
isQuestion message = not (null message) && last message == '?'

isYell :: String -> Bool
isYell message = not (null letters) && all isAsciiUpper letters
  where
    letters = filter isAlpha message

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse