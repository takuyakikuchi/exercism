module Darts (score) where

score :: Float -> Float -> Int
score x y = scoreFromDistanceSquared (x*x + y*y)

scoreFromDistanceSquared :: Float -> Int
scoreFromDistanceSquared distanceSquared
  | distanceSquared <= bullRadiusSquared = 10
  | distanceSquared <= innerCircleRadiusSquared = 5
  | distanceSquared <= outerCircleRadiusSquared = 1
  | otherwise = 0
  where
    bullRadiusSquared = 1
    innerCircleRadiusSquared = 25
    outerCircleRadiusSquared = 100