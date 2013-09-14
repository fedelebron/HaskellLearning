{- |
Module      :  Perceptron
Description :  A simple perceptron algorithm for R^n samples.
License     :  BSD

Maintainer  :  flebron@dc.uba.ar
Stability   :  experimental
Portability :  portable


This implementats the Perceptron learning algorithm for points in R^n. The bias
is encoded as the first component of the training's resulting vector.
-}

module Perceptron (StoppingCriterion (..),
                   vectorToClassifier,
                   train) where

import Control.Arrow (first)
import Foreign.Marshal.Utils (fromBool)

import Learning (ClassifiedSample,
                 BinaryClassifier,
                 Sample)
import Utils (dot,
              normalize)

-- | The condition we need to satisfy to stop the training algorithm.
data StoppingCriterion = Iterations Int -- ^ An upper bound on the number of
                                        -- iterations.
                       | InSampleError Double -- ^ An upper bound on the
                                              -- in-sample error.

-- | Given a vector w obtained from training the a perceptron neuron, we
-- construct a BinaryClassifier which tells us which side of the hyperplane
-- defined by w a given point falls in.
vectorToClassifier :: [Double] -- ^ The vector resulting from the
                               -- perceptron learning algorithm.
                      -> BinaryClassifier -- ^ A perceptron classifier.
vectorToClassifier w = (>= 0) . dot w

-- | Given a list of classified samples, we construct a vector w such that a
-- point x in R^n is accepted if w `dot` x > 0. This defines a hyperplane that
-- separates the sample space in two.
-- The learning rate should be in the range (0, 1].
train :: Double -- ^ The learning rate.
         -> StoppingCriterion -- ^ When to stop training.
         -> [ClassifiedSample] -- ^ The classified samples
                               -- to be used as training data.
         -> [Double] -- ^ The resulting vector for a perceptron.
train alpha criterion samples = go 0 w_1
  where
    samples'@(s : _) = prepareTrainingData samples
    w_1 = map (const 0) (fst s)
    go k w = let w' = trainingRound alpha samples' w
             in  case criterion of
                    Iterations t | t == k -> w'
                    InSampleError e | err <= e -> w'
                      where
                        err = sampleError (vectorToClassifier w') samples'
                    _ -> go (k + 1) w'

prepareTrainingData :: [ClassifiedSample] -> [ClassifiedSample]
prepareTrainingData = map $ first (1 :)

-- | Computes the sample error, using the euclidean 1-norm.
sampleError :: BinaryClassifier -> [ClassifiedSample] -> Double
sampleError f samples = sum $ map g samples
  where
    g (s, b) = abs $ fromBool (f s) - fromBool b

-- | Updates the current separating vector using the samples and at a given
-- learning rate.
trainingRound :: Double -- ^ The learning rate.
             -> [ClassifiedSample] -- ^ The classified samples
                                   -- to be used as training data.
             -> [Double] -- ^ The current separating vector.
             -> [Double] -- ^ The new separating vector.
trainingRound alpha samples w = let update w' (x, o) = zipWith (+) w' x'
                                      where
                                        d  = if o then 1 else (-1)
                                        y  = signum $ w' `dot` x
                                        x' = map (* (alpha * (d - y))) x
                                in  foldl update w samples
