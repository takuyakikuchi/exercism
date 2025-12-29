module Bob (responseFor) where
import Data.Char (toUpper, isSpace, isAsciiUpper, isAlpha)

responseFor :: String -> String
responseFor xs
  | isQuestion && isYell = "Calm down, I know what I'm doing!"
  | isQuestion = "Sure."
  | isYell = "Whoa, chill out!"
  | isSilence = "Fine. Be that way!"
  | otherwise = "Whatever."
  where
    trimmed = trim xs
    letters = filter isAlpha trimmed
    
    isQuestion = not (null trimmed) && last trimmed == '?'
    isYell = not (null letters) && all isAsciiUpper letters
    isSilence = null trimmed

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse