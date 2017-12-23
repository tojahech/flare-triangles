module DegreeTrig (sin, cos, tan, asin, acos, atan) where

import Prelude
import Math (pi)
import Math (sin, cos, tan, asin, acos, atan) as M

-- Redefine trig functions to use degrees. Obviously it doesn't matter behind the scenes, but if dealing with user input most people will prefer degrees to radians. 

toRadian :: Number -> Number
toRadian x = (pi/180.0) * x

toDegree :: Number -> Number
toDegree x = (180.0/pi) * x

sin :: Number -> Number
sin = M.sin <<< toRadian

cos :: Number -> Number
cos = M.cos <<< toRadian

tan :: Number -> Number
tan = M.tan <<< toRadian

asin :: Number -> Number
asin = toDegree <<< M.asin

acos :: Number -> Number
acos = toDegree <<< M.acos

atan :: Number -> Number
atan = toDegree <<< M.atan

