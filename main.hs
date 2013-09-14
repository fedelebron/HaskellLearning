import Text.Printf

import Plotting
import Perceptron
import Learning
import Utils

red = Color 0.9 0.05 0.05
blue = Color 0.2 0.1 0.85
green = Color 0.01 0.8 0.2

sampleToCircle :: ClassifiedSample -> Shape
sampleToCircle ([x, y], b) = let c = if b then blue else red
                             in Circle (Position x y) 0.05 c

vectorToLine :: [Double] -> Shape
vectorToLine [w0, w1, w2] = Line p0 p1 green
  where
    p0 = Position (-1) (( w1 - w0) / w2)
    p1 = Position   1  ((-w1 - w0) / w2)

main = let samples = [([0.25, 0.85], True), ([0.5, 0.8], True), ([-0.3, 0.3], True),
                      ([0.1, 0.1], False), ([0.5, -0.25], False)] :: [ClassifiedSample]
           samples2 = [([sqrt 3 / 4, sqrt 3 / 2], True),
                      ([- sqrt 3 / 4, sqrt 3 / 2], False),
                      ([0, -0.5], False)]
           w = train 0.2 (InSampleError 0) samples2
           perceptron = vectorToClassifier w
       in  do
         putStrLn $ printf "Classifications: %s" (show $ map (signum . dot w . (1:) . fst) samples2)
         putStrLn $ printf "w: %s" (show w)
         display "Classified samples" $ vectorToLine w : map sampleToCircle samples2