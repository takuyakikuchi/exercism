module Darts (score) where

score :: Float -> Float -> Int
score x y
  | distanceSquared <= 1^2 = 10
  | distanceSquared <= 5^2 = 5
  | distanceSquared <= 10^2 = 1
  | otherwise = 0
  where
    distanceSquared = (x^2 + y^2)