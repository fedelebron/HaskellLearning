{- |
Module      :  Learning
Description :  Basic definitions useful for machine learning algorithms.
License     :  BSD

Maintainer  :  flebron@dc.uba.ar
Stability   :  experimental
Portability :  portable
-}

module Learning where

type Sample = [Double]
type ClassifiedSample = (Sample, Bool)
type BinaryClassifier = Sample -> Bool
