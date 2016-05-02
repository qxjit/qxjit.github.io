module Main where

import Codec.Picture
import Data.List
import Data.Word
import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  savePngImage filename $ ImageRGB16 image

image :: Image PixelRGB16
image = generateImage pixel 2160 1800

pixel :: Int -> Int -> PixelRGB16
pixel px py = runSamples px py 1 sample

runSamples :: Int -> Int -> Int -> (Double -> Double -> (Double, Double, Double)) -> PixelRGB16
runSamples px py count sampler =
  let xmin = (realToFrac px) - 2
      ymin = (realToFrac py) - 2
      ds   = 1 / sqrt (realToFrac count)

      xmax = xmin + 4
      ymax = ymin + 4

      values = [ sampler sx sy
               | sx <- [xmin, xmin + ds .. xmax]
               , sy <- [ymin, ymin + ds .. ymax]
               ]

      actualCount = realToFrac $ length values

      sumUp (r, g, b) (r', g', b') = (r + r', g + g', b + b')

      (rTot, gTot, bTot) = foldl' sumUp (0,0,0) values

      pr = round (rTot / actualCount)
      pg = round (gTot / actualCount)
      pb = round (bTot / actualCount)

  in PixelRGB16 pr pg pb

sample :: Double -> Double -> (Double, Double, Double)
sample x y =
  let i1 = circularWave 71 (linearDamp 1200) (960, 450) (x, y)
      i2 = circularWave 83 (linearDamp 1200) (125, 600) (x, y)
      r  = unitScale 57000 61000 ((i1 + i2) / 2)
      g  = r
      b  = r

  in (r, g, b)

unitScale :: Double -> Double -> Double -> Double
unitScale mn mx s
  | s < -1 = mn
  | s >  1 = mx
  | otherwise = mn + (mx - mn) * (s + 1) / 2


type Damping = Double -> Double -> Double

linearDamp :: Double -> Damping
linearDamp maxD d v
  | d >= maxD = 0
  | otherwise = v * (maxD - d) / maxD

circularWave :: Double -> Damping -> (Double, Double) -> (Double, Double) -> Double
circularWave wavelength damp (x1, y1) (x2, y2) =
  let x = x2 - x1
      y = y2 - y1
      d = sqrt (x*x + y*y)

  in damp d (sin (2 * pi * d / wavelength))

