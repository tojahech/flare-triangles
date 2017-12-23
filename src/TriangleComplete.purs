module TriangleComplete (Triangle, PartialTriangle, test, complete)  where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr)

import Math (sqrt)
import DegreeTrig (sin, cos, acos, asin)

-- Datatypes for triangles

type Triangle =
  { sideA :: Number
  , sideB :: Number
  , sideC :: Number
  , angleA :: Number
  , angleB :: Number
  , angleC :: Number
  }

type PartialTriangle =
  { sideA :: Maybe Number
  , sideB :: Maybe Number
  , sideC :: Maybe Number
  , angleA :: Maybe Number
  , angleB :: Maybe Number
  , angleC :: Maybe Number
  , option :: Boolean
  }

-- Test if the partial triangle is valid and can be completed. 
-- There is a bunch of code repetition here, maybe I can work out a more elegant way to do this?

type TriangleTest = PartialTriangle -> Boolean

bound_side_a :: TriangleTest
bound_side_a { sideA : Just x } = x > 0.0
bound_side_a _ = true

bound_side_b :: TriangleTest
bound_side_b { sideB : Just x } = x > 0.0
bound_side_b _ = true

bound_side_c :: TriangleTest
bound_side_c { sideC : Just x } = x > 0.0
bound_side_c _ = true

bound_angle_a :: TriangleTest
bound_angle_a { angleA : Just x } = x > 0.0 && x < 180.0
bound_angle_a _ = true

bound_angle_b :: TriangleTest
bound_angle_b { angleB : Just x } = x > 0.0 && x < 180.0
bound_angle_b _ = true

bound_angle_c :: TriangleTest
bound_angle_c { angleC : Just x } = x > 0.0 && x < 180.0
bound_angle_c _ = true

triangleInequality :: TriangleTest
triangleInequality { sideA : Just a , sideB : Just b , sideC : Just c } = a + b > c && a + c > b && b + c > a
triangleInequality _ = true

angleSum :: TriangleTest
angleSum { angleA : Just alph , angleB : Just bet, angleC : Just gam } = alph + bet + gam == 180.0
angleSum _ = true

partialAngleSum1 :: TriangleTest
partialAngleSum1 { angleA : Just alph , angleB : Just bet } = alph + bet < 180.0
partialAngleSum1 _ = true

partialAngleSum2 :: TriangleTest
partialAngleSum2 { angleA : Just alph , angleB : Just gam } = alph + gam < 180.0
partialAngleSum2 _ = true

partialAngleSum3 :: TriangleTest
partialAngleSum3 { angleB : Just bet , angleC : Just gam } = bet + gam < 180.0
partialAngleSum3 _ = true

sideLength1 :: TriangleTest
sideLength1 { sideA : Just a , angleA : Just alph , sideB : Just b } = b * sin alph <= a
sideLength1 _ = true

sideLength2 :: TriangleTest
sideLength2 { sideA : Just a , angleA : Just alph , sideC : Just c } = c * sin alph <= a
sideLength2 _ = true

sideLength3 :: TriangleTest
sideLength3 { sideB : Just b , angleB : Just bet , sideA : Just a } = a * sin bet <= b
sideLength3 _ = true

sideLength4 :: TriangleTest
sideLength4 { sideB : Just b , angleB : Just bet , sideC : Just c } = c * sin bet <= b
sideLength4 _ = true

sideLength5 :: TriangleTest
sideLength5 { sideC : Just c , angleC : Just gam , sideA : Just a } = a * sin gam <= c
sideLength5 _ = true

sideLength6 :: TriangleTest
sideLength6 { sideC : Just c , angleC : Just gam , sideB : Just b } = b * sin gam <= c
sideLength6 _ = true

validityTest :: Array TriangleTest
validityTest =
  [ bound_side_a
  , bound_side_b
  , bound_side_c
  , bound_angle_a
  , bound_angle_b
  , bound_angle_c
  , triangleInequality
  , angleSum
  , partialAngleSum1
  , partialAngleSum2
  , partialAngleSum3
  , sideLength1
  , sideLength2
  , sideLength3
  , sideLength4
  , sideLength5
  , sideLength6
  ]

test :: TriangleTest
test tri = foldr (&&) true testResults
  where testResults = (_ $ tri) <$> validityTest


-- Complete a partial triangle (typically three good bits of information)
-- Use pattern matching to determine which value to complete next

complete :: PartialTriangle -> Triangle
complete { sideA : Just a , sideB : Just b , sideC : Just c , angleA : Just alph , angleB : Just bet, angleC : Just gam } = { sideA:a , sideB:b , sideC:c , angleA:alph , angleB:bet , angleC:gam }
-- Missing angles
complete partial = complete $ case partial of
  -- Add missing angles
  { angleA : Just alph , angleB : Just bet , angleC : Nothing  } -> partial { angleC = Just (180.0-alph-bet) }
  { angleA : Just alph , angleB : Nothing  , angleC : Just gam } -> partial { angleB = Just (180.0-alph-gam) }
  { angleA : Nothing   , angleB : Just bet , angleC : Just gam } -> partial { angleA = Just (180.0-bet-gam) }
  -- Three sides to angle
  { sideA : Just a , sideB : Just b , sideC : Just c , angleA : Nothing } -> partial { angleA = Just (acos ((a*a - b*b - c*c) / (-2.0 * b * c )) ) }
  { sideA : Just a , sideB : Just b , sideC : Just c , angleB : Nothing } -> partial { angleB = Just (acos ((b*b - a*a - c*c) / (-2.0 * a * c )) ) }
  { sideA : Just a , sideB : Just b , sideC : Just c , angleC : Nothing } -> partial { angleC = Just (acos ((c*c - a*a - b*b) / (-2.0 * a * b )) ) }
  -- Two angles and side to another side (can avoid half the permutations because third angle always known)
  { sideA : Just a , sideB : Nothing , angleA : Just alph , angleB : Just bet  } -> partial { sideB = Just ( a * sin bet / sin alph ) }
  { sideB : Just b , sideC : Nothing , angleB : Just bet  , angleC : Just gam  } -> partial { sideC = Just ( b * sin gam / sin bet ) }
  { sideC : Just c , sideA : Nothing , angleC : Just gam ,  angleA : Just alph } -> partial { sideA = Just ( c * sin alph / sin gam ) }
  -- Two sides with angle in between to third side
  { sideA : Just a  , sideB : Just b  , sideC : Nothing , angleC : Just gam  } -> partial { sideC = Just (sqrt ( a*a + b*b - 2.0*a*b*cos gam ) ) }
  { sideA : Nothing , sideB : Just b  , sideC : Just c  , angleA : Just alph } -> partial { sideA = Just (sqrt ( b*b + c*c - 2.0*b*c*cos alph ) ) }
  { sideA : Just a  , sideB : Nothing , sideC : Just c  , angleB : Just bet  } -> partial { sideB = Just (sqrt ( a*a + c*c - 2.0*a*c*cos bet ) ) }
  -- Two sides and non-shared angle
  { sideA : Just a , sideB : Just b , angleA : Just alph , angleB : Nothing, option: o } -> partial { angleB = if a < b && alph < 90.0 && o
                                        then Just (180.0 - asin ( b * sin alph / a ))
                                        else Just (asin ( b * sin alph / a ))
                                }
  { sideA : Just a , sideC : Just c , angleA : Just alph , angleC : Nothing, option: o } -> partial { angleC = if a < c && alph < 90.0 && o
                                        then Just (180.0 - asin ( c * sin alph / a ))
                                        else Just (asin ( c * sin alph / a ))
                                }
  { sideB : Just b , sideA : Just a , angleB : Just bet , angleA : Nothing, option: o } -> partial { angleA = if b < a && bet < 90.0 && o
                                        then Just (180.0 - asin ( a * sin bet / b ))
                                        else Just (asin ( a * sin bet / b ))
                                }
  { sideB : Just b , sideC : Just c , angleB : Just bet , angleC : Nothing, option: o } -> partial { angleC = if b < c && bet < 90.0 && o
                                        then Just (180.0 - asin ( c * sin bet / b ))
                                        else Just (asin ( c * sin bet / b ))
                                }
  { sideC : Just c , sideA : Just a , angleC : Just gam , angleA : Nothing, option: o } -> partial { angleA = if c < a && gam < 90.0 && o
                                        then Just (180.0 - asin ( a * sin gam / c ))
                                        else Just (asin ( a * sin gam / c ))
                                }
  { sideC : Just c , sideB : Just b , angleC : Just gam , angleB : Nothing, option: o } -> partial { angleB  = if c < b && gam < 90.0 && o
                                        then Just (180.0 - asin ( b * sin gam / c ))
                                        else Just (asin ( b * sin gam / c ))
                                }
  -- Catchall
  _ -> { sideA :Just 0.0 , sideB :Just 0.0 , sideC :Just 0.0 , angleA :Just 0.0 , angleB :Just 0.0 , angleC :Just 0.0 , option: true }
