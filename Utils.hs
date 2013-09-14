{- |
Module      :  Util
Description :  Basic linear algebra utilities.
License     :  BSD

Maintainer  :  flebron@dc.uba.ar
Stability   :  experimental
Portability :  portable
-}

module Utils (dot,
              normalize) where

-- | Compute the dot product between two vectors.
dot :: Num a => [a] -> [a] -> a
dot = (sum .) . zipWith (*)

-- | Normalize a vector.
normalize :: Floating a => [a] -> [a]
normalize xs = map (/ norm) xs
  where
    norm = sqrt . sum $ map (^ 2) xs
