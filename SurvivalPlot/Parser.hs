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

{-# LANGUAGE OverloadedStrings #-}

module SurvivalPlot.Parser (parseDistributions, parseIntervals, Curve (..), TestInfo (..)) where

import Control.Applicative
import Data.Attoparsec.Text


data Curve = Curve { curvePoints :: [Double]
                   , tValue :: (Double,Double)  -- Not really sure what to do for OUT_OF_RANGE
                   }
                   deriving (Show)


data TestInfo = TestInfo { concordance :: Double
                         , l1Loss :: Double
                         , l2Loss :: Double
                         , raeLoss :: Double
                         , l1LogLoss :: Double
                         , l2LogLoss :: Double
                         , logLikelihoodLoss :: Double
                         }


-- | Parse a time intervals file.
parseIntervals :: Parser [Double]
parseIntervals = many1 (double <* endOfLine)


-- | Parse an MTLR test distribution.
parseDistributions :: Parser ([Curve], TestInfo)
parseDistributions = do curves <- parseCurves
                        info <- parseTestInfo
                        endOfInput
                        return (curves, info)


-- | Parse MTLR test error information.
parseTestInfo :: Parser TestInfo
parseTestInfo = do string "#concordance index: "
                   conc <- double
                   endOfLine
                   
                   string "#avg l1-loss: "
                   l1 <- double
                   endOfLine
                   
                   string "#avg l2-loss: "
                   l2 <- double
                   endOfLine
                   
                   string "#avg rae-loss: "
                   rae <- double
                   endOfLine
                   
                   string "#avg l1-log-loss: "
                   l1Log <- double
                   endOfLine
                   
                   string "#avg l2-log-loss: "
                   l2Log <- double
                   endOfLine
                   
                   string "#avg log-likelihood loss: "
                   logLikelihood <- double
                   endOfLine
                   
                   return (TestInfo conc l1 l2 rae l1Log l2Log logLikelihood)


-- | Parse all curves in a file.
parseCurves :: Parser [Curve]
parseCurves = many1 parseCurveLine


-- | Parse a single curve.
parseCurveLine :: Parser Curve
parseCurveLine = do points <- parsePoints
                    t <- parseTValue
                    return (Curve (drop 2 points) t)


-- | Get all points for a curve.
parsePoints :: Parser [Double]
parsePoints = do point <- double
                 otherPoints <- (char ',' *> skipSpace *> parsePoints) <|> return []
                 skipSpace
                 return (point:otherPoints)
                 

-- | Parse the weird scary "t" values.
parseTValue :: Parser (Double,Double)
parseTValue = do string ", t:"
                 t1 <- double <|> (string "OUT_OF_RANGE" *> return 0)
                 char ':'
                 t2 <- double <|> (string "OUT_OF_RANGE" *> return 0)
                 endOfLine
                 return (t1, t2)
