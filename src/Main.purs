module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)

import Data.Foldable (foldr)
import Data.Array (filter,length)

import Partial.Unsafe (unsafePartial)

import Flare (boolean, string)
import Flare.Drawing (Drawing, Point, Font, Color, closed, fillColor, filled, fromInt, runFlareDrawing, text, translate)

import Graphics.Drawing.Font (font, sansSerif)
import Graphics.Canvas (getCanvasElementById, getContext2D, setTextAlign, TextAlign(..))

import Data.Number (fromString)
import Data.Int (round, toNumber)
import Math (pow)
import DegreeTrig (cos, sin)

import TriangleComplete (PartialTriangle, Triangle, complete, test)


main = do
  void $ unsafePartial do
    Just canvas <- getCanvasElementById "output-canvas"
    ctx <- getContext2D canvas
    setTextAlign ctx AlignCenter
  runFlareDrawing "controls" "output-canvas" uiDraw


-- Improvised Vector Arithmetic
-- Purescript doesn't seem to have a vector space class. It does have classes for abelian groups and fields, so it should be easy to build a vector space class, but that's overkill for this little project. The only bonus would be writing + instead of .+ for vector addition. 

-- Build upon the Point class from Graphics.Drawing
-- type Point = { x :: Number, y :: Number }

vecAddition :: Point -> Point -> Point
vecAddition p1 p2 = { x: p1.x + p2.x , y: p1.y + p2.y } 
infixl 6 vecAddition as .+

vecScale :: Number -> Point -> Point
vecScale a p = { x: a * p.x , y: a * p.y }
infixl 7 vecScale as .*

midPoint :: Point -> Point -> Point
midPoint p1 p2 = 0.5 .* ( p1 .+ p2 )


-- Triangle Rendering and Annotation

annotationFont = font sansSerif 16 mempty :: Font
triangleColor = fromInt 0xb3e099 :: Color
annotationColor = fromInt 0x1c2130 :: Color

prettyPrint :: Number -> String
prettyPrint num = show $ (toNumber $ round $ shift * num) / shift
  where n = 3
        shift = pow 10.0 (toNumber n)

canvasSize = 400.0 :: Number
drawingSize = 300.0 :: Number

plotTriangle :: Triangle -> Drawing
plotTriangle tri = translate offset.x offset.y $
  filled ( fillColor triangleColor ) ( closed [ v1' , v2' , v3' ] )
  <> annotate (v1' .+ textOffset) ("γ : " <> prettyPrint tri.angleC <> "°")
  <> annotate (v2' .+ textOffset) ("α : " <> prettyPrint tri.angleA <> "°")
  <> annotate (v3' .+ textOffset) ("β : " <> prettyPrint tri.angleB <> "°")
  <> annotate (midPoint v1' v3' .+ textOffset) ("A : " <> prettyPrint tri.sideA)
  <> annotate (midPoint v1' v2' .+ textOffset) ("B : " <> prettyPrint tri.sideB)
  <> annotate (midPoint v2' v3' .+ textOffset) ("C : " <> prettyPrint tri.sideC)
  where 
        v1 = { x : 0.0 , y : 0.0 }
        v2 = { x : tri.sideB * cos tri.angleC , y : - tri.sideB * sin tri.angleA }
        v3 = { x : tri.sideA , y : 0.0 }
        width = foldr max 0.0 [ v3.x , v2.x - v1.x , v3.x - v2.x ]
        height = -v2.y
        s = drawingSize / max width height
        v1' = s .* v1
        v2' = s .* v2
        v3' = s .* v3
        offset = { x: (canvasSize - s * width) / 2.0 + if v1'.x > v2'.x then v1'.x - v2'.x else 0.0 , y: (canvasSize + s * height) / 2.0 }
        textOffset = { x: 0.0 , y: 6.0 }
        annotate p annotation = text annotationFont p.x p.y (fillColor annotationColor) annotation


-- Input validation and combination

readPartial :: String -> String -> String -> String -> String -> String -> Boolean -> PartialTriangle
readPartial a b c aa bb cc opt = { sideA : fromString a
                                 , sideB : fromString b
                                 , sideC : fromString c
                                 , angleA : fromString aa
                                 , angleB : fromString bb
                                 , angleC : fromString cc
                                 , option: opt
                                 }

countEmptyInputs :: String -> String -> String -> String -> String -> String -> Int
countEmptyInputs a b c aa bb cc = length $ filter (_ == "") [a,b,c,aa,bb,cc]

infoFont = font sansSerif 16 mempty :: Font
infoColor = fromInt 0x1c2130 :: Color

errorMessage :: String -> Drawing
errorMessage = text infoFont center center (fillColor infoColor)
  where center = canvasSize / 2.0

draw :: String -> String -> String -> String -> String -> String -> Boolean -> Drawing
draw a b c aa bb cc opt
  | countEmptyInputs a b c aa bb cc > 3
    = errorMessage "Not enough inputs."
  | countEmptyInputs a b c aa bb cc < 3
    = errorMessage "Too many inputs."
  | not $ test $ readPartial a b c aa bb cc opt
    = errorMessage "Values don't work together."
  | otherwise
    = plotTriangle $ complete $ readPartial a b c aa bb cc opt

-- Flare inputs

uiDraw = draw <$> string "Side A" "1.0"
              <*> string "Side B" "1.0"
              <*> string "Side C" "1.0"
              <*> string "Angle α" ""
              <*> string "Angle β" ""
              <*> string "Angle γ" ""
              <*> boolean "Swap triangles" false
