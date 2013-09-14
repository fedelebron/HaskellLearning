import Text.Printf

import Plotting
import Perceptron
import Learning

red, green, blue :: Color
red = Color 0.9 0.05 0.05
green = Color 0.01 0.8 0.2
blue = Color 0.2 0.1 0.85

sampleToCircle :: ClassifiedSample -> Shape
sampleToCircle ([x, y], b) = let c = if b then blue else red
                             in Circle (Position x y) 0.05 c
sampleToCircle _ = error "Samples should be two dimensional."

vectorToLine :: [Double] -> Shape
vectorToLine [w0, w1, w2] = Line p0 p1 green
  where
    p0 = Position (-1) (( w1 - w0) / w2)
    p1 = Position   1  ((-w1 - w0) / w2)
vectorToLine _ = error "Classification vectors should be 3 dimensional, with the bias as the first component."

main :: IO ()
main = let samples = [([sqrt 3 / 4, sqrt 3 / 2], True),
                      ([- sqrt 3 / 4, sqrt 3 / 2], False),
                      ([0, -0.5], False)]
           w = train 0.2 (InSampleError 0) samples
           perceptron = vectorToClassifier w
       in  do
         putStrLn $ printf "Classifications: %s" (show $ map (perceptron . fst) samples)
         putStrLn $ printf "w: %s" (show w)
         display "Classified samples" $ vectorToLine w : map sampleToCircle samples