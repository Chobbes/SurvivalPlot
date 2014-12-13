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

-- | MTLR Model data type
data Model = Model { numTimePoints :: Integer -- ^ Number of time points in the interval.
                   , timePoints :: [Double] -- ^ Time points in the interval.
                   , rForSomeReason :: Integer -- ^ I have no idea what is this even?
                   , dimension :: Integer -- ^ Number of features in the model.
                   , features :: [(Integer, Double)]  -- ^ Feature, value pairs
                   }

-- | Parse a time intervals file.
parseIntervals :: Parser [Double]
parseIntervals = (many1 (double <* endOfLine)) <|> (do model <- parseModel; return (timePoints model))


-- | Parse an MTLR test distribution.
parseDistributions :: Parser ([Curve], TestInfo)
parseDistributions = do curves <- parseCurves
                        info <- parseTestInfo
                        endOfInput
                        return (curves, info)

-- | Parse PSSP model.
parseModel :: Parser Model
parseModel = do string "m:"
                tps <- decimal
                endOfLine'

                timeInterval <- parseTimeInterval
                endOfLine'

                string "r:"
                r <- decimal
                endOfLine'

                string "DIM:"
                dim <- decimal
                endOfLine'

                feats <-  many parseKeyWeight
                many endOfLine'
                endOfInput <?> "End of input not reached. Probably some bad padding at the end, or bad features."

                return (Model tps timeInterval r dim feats)

-- | Parse the comma separated time points for the interval.
parseTimeInterval :: Parser [Double]
parseTimeInterval = 
  do point <- double
     others <- (do char ',' *> parseTimeInterval) <|> (return [])
     return (point : others)

-- | Parse a key:double pair for model weights.
parseKeyWeight :: Parser (Integer, Double)
parseKeyWeight = do key <- decimal
                    char ':'
                    value <- double
                    endOfLine'
                    return (key, value)

-- | Newline parser to handle all sorts of horrible.
endOfLine' :: Parser ()
endOfLine' = endOfLine <|> (char '\r' *> return ())

-- | Parse MTLR test error information.
parseTestInfo :: Parser TestInfo
parseTestInfo = do string "#concordance index: "
                   conc <- doubleInf
                   endOfLine
                   
                   string "#avg l1-loss: "
                   l1 <- doubleInf
                   endOfLine
                   
                   string "#avg l2-loss: "
                   l2 <- doubleInf
                   endOfLine
                   
                   string "#avg rae-loss: "
                   rae <- doubleInf
                   endOfLine
                   
                   string "#avg l1-log-loss: "
                   l1Log <- doubleInf
                   endOfLine
                   
                   string "#avg l2-log-loss: "
                   l2Log <- doubleInf
                   endOfLine
                   
                   string "#avg log-likelihood loss: "
                   logLikelihood <- doubleInf
                   endOfLine
                   
                   return (TestInfo conc l1 l2 rae l1Log l2Log logLikelihood)

doubleInf :: Parser Double
doubleInf = double <|> (string "inf" *> (return 0))


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
