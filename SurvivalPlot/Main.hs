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
import qualified Data.Foldable as F

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import System.Directory
import System.Directory.Tree
import System.Environment
import System.FilePath


-- | Plots for one set of iterations.
-- | SurvivalPlot model-dir distribution-dir
-- | distribution directory contains iteration 1, 2, and 3 (as does the model directory)
-- | We assume that the distribution directory has the same structure as the model directory
-- | and that the file extension of the distributions is ".out"
main :: IO ()
main = do (plotDir:errorName:intervalDir:args) <- getArgs

          let distributionDirs = map snd . filter (\(n,_) -> n `mod` 2 == 0) $ zip [0..] args
          let names = map snd . filter (\(n,_) -> n `mod` 2 == 1) $ zip [0..] args

          putStrLn "Getting averages..."
          tests <- getTestAvgs distributionDirs
          
          putStrLn "Plotting errors..."
          createDirectoryIfMissing True plotDir
          plotErrors names tests (joinPath [plotDir, errorName])

          putStrLn "Plotting all curves..."
          plotAllCurves intervalDir distributionDirs plotDir
          
          putStrLn "Done!"


plotAllCurves intervalDir dataDirs plotDir = 
  do readDir <- readDirectoryWith return intervalDir
     let intervalFiles = F.toList $ dirTree readDir
     sequence_ [plotIntervalFile inter dir plotDir | inter <- intervalFiles, dir <- dataDirs]


plotIntervalFile intervalFile dataDir plotDir = 
  do intervalData <- T.readFile intervalFile
     let intervals = takeRight intervalFile $ parseOnly parseIntervals intervalData
     
     dataData <- T.readFile dataFile
     let (curves, _) = takeRight dataFile $ parseOnly parseDistributions dataData

     plotCurves intervals curves plotFile
  where dataFile = joinPath (dataDir : dirs ++ [(replaceExtension (takeFileName intervalFile) "out")])
        plotFile = joinPath (plotDir : dirs ++ [(replaceExtension (takeFileName intervalFile) "pdf")])
        (_:_:dirs) = splitDirectories (dropFileName intervalFile)


takeRight _ (Right a) = a
takeRight file _ = error ("Could not parse file: " ++ file)


getTestAvgs :: [FilePath] -> IO [TestInfo]
getTestAvgs allDirs = mapM getTestAvg allDirs

getTestAvg :: FilePath -> IO TestInfo
getTestAvg dir = do readDir <- readDirectoryWith return dir
                    let files = F.toList $ dirTree readDir

                    distData <- mapM T.readFile files
                    let infos = map (\(f, d) -> snd . takeRight f $ parseOnly parseDistributions d) (zip files distData)

                    return (avgTestInfo infos)


plotCurves intervals curves fileName =
  do createDirectoryIfMissing True (takeDirectory fileName)
     toFile (fo_format .~ PDF $ def) fileName $ 
       do layout_title .= "Survival Curves"
          mapM_ (\curve -> plot (line "" [(zip intervals (map constrain . curvePoints $ curve))])) (take 10 curves)
  where constrain n = if n > 1
                         then 1
                         else if n < 0
                                 then 0
                                 else n


plotErrors names tests fileName = 
  do createDirectoryIfMissing True (takeDirectory fileName)
     toFile (fo_format .~ PDF $ def) fileName $
       do layout_title .= "Average Losses"
          layout_x_axis . laxis_generate .= autoIndexAxis names
          plot $ fmap plotBars $ bars ["L2-log"] (addIndexes indexes) 
  where indexes = map ((: []) . (!! 4) . testLosses) tests


testLosses :: TestInfo -> [Double]
testLosses (TestInfo _ l1 l2 rae l1Log l2Log logLikely) = [l1, l2, rae, l1Log, l2Log, logLikely]


lossesToTest :: Double -> [Double] -> TestInfo
lossesToTest c [l1, l2, rae, l1Log, l2Log, logLikely] = TestInfo c l1 l2 rae l1Log l2Log logLikely


-- | From a list of TestInfo data create a new one with the average of all losses.
avgTestInfo :: [TestInfo] -> TestInfo
avgTestInfo tests = (foldl infoSum (TestInfo 0 0 0 0 0 0 0) tests) `infoDiv` (fromIntegral $ length tests)
  where infoSum t1 t2 = lossesToTest (concordance t1) (zipWith (+) (testLosses t1) (testLosses t2))
        infoDiv t1 s = lossesToTest (concordance t1) (zipWith (/) (testLosses t1) (repeat s))
