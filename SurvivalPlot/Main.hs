{- Copyright (C) 2014 Calvin Beck

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

import SurvivalPlot.Parser

import Data.Attoparsec.Text hiding (take)
import qualified Data.Text.IO as T
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import System.Environment


main :: IO ()
main = do [intervalFile, distributionFile] <- getArgs

          intervalData <- T.readFile intervalFile
          distributionData <- T.readFile distributionFile
          
          let intervals = takeRight intervalFile $ parseOnly parseIntervals intervalData
          let (curves, info) = takeRight distributionFile $ parseOnly parseDistributions distributionData

          plotCurves intervals (take 10 curves) "survival-plot.pdf"
  where takeRight _ (Right a) = a
        takeRight file _ = error ("Could not parse file: " ++ file)

plotCurves intervals curves fileName = toFile (fo_format .~ PDF $ def) fileName $ 
                                       do layout_title .= "Survival Curves"
                                          mapM_ (\curve -> plot (line "" [(zip intervals (map constrain . curvePoints $ curve))])) curves
  where constrain n = if n > 1
                         then 1
                         else if n < 0
                                 then 0
                                 else n
