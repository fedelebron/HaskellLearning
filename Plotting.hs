{- |
Module      :  Plotting
Description :  Plotting of 2D shapes on screen.
License     :  BSD

Maintainer  :  flebron@dc.uba.ar
Stability   :  experimental
Portability :  portable

Utilities for plotting circles and lines in 2 dimensions,
and displaying them on the screen.
-}

module Plotting (Color (Color),
                 Plot,
                 Position (Position),
                 Shape (..),
                 display) where
import Foreign.C.Types (CDouble (CDouble))

import Graphics.Rendering.OpenGL (ClearBuffer (ColorBuffer),
                                  GLdouble,
                                  PrimitiveMode (Lines, TriangleFan, Quads),
                                  clear,
                                  color,
                                  loadIdentity,
                                  preservingMatrix,
                                  renderPrimitive,
                                  vertex)
import Graphics.Rendering.OpenGL.GL.Tensor (Vertex2 (Vertex2))
import Graphics.Rendering.OpenGL.GL.VertexAttributes (Color3 (Color3))
import Graphics.UI.GLUT (DisplayMode (DoubleBuffered),
                         ($=),
                         createWindow,
                         displayCallback,
                         getArgsAndInitialize,
                         initialDisplayMode,
                         mainLoop,
                         swapBuffers)
-- | A pair of coordinates, both Doubles in the range [-1, 1].
data Position = Position { _x, _y :: Double }
-- | A color with red, green and blue components, each a Double from 0 to 1.
data Color = Color { _r, _g, _b :: Double }
-- | Shapes that one can draw on the screen.
data Shape = Circle Position Double Color -- ^ A circle with a position, radius
                                           -- and a color.

             | Line Position Position Color -- ^ A line with starting and
                                            -- ending points, and a color.

data Renderable = Renderable PrimitiveMode (Color3 GLdouble) [Vertex2 GLdouble]
type Plot = [Shape]

colorToGLColor :: Color -> Color3 GLdouble
colorToGLColor c = Color3 (CDouble $ _r c) (CDouble $ _g c) (CDouble $ _b c)

makeRenderable :: Shape -> Renderable
makeRenderable (Circle p r c) = Renderable TriangleFan (colorToGLColor c) vs
  where
    x = CDouble $ _x p
    y = CDouble $ _y p
    r' = CDouble r
    vertexCount = 300 :: CDouble
    coordinates theta = (x + r' * cos theta, y + r' * sin theta)
    angles = map (\ i -> 2 * pi * i / vertexCount) [0.0 .. vertexCount]
    vs = map (uncurry Vertex2) $ (x, y) : map coordinates angles
makeRenderable (Line start end c) = Renderable Lines (colorToGLColor c) vs
  where
    vs = [Vertex2 (CDouble $ _x start) (CDouble $ _y start),
          Vertex2 (CDouble $ _x end) (CDouble $ _y end)]

render :: Renderable -> IO ()
render (Renderable p c vs) = do
  color c
  renderPrimitive p $ mapM_ vertex vs

-- | Display the given plot in a window with a given title.
display :: String -- ^ The desired window title.
           -> Plot -- ^ A Plot to draw in the window.
           -> IO ()
display title plot = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _ <- createWindow title
  displayCallback $= displayFunction
  mainLoop
  where
    renderables = whiteBackground : map makeRenderable plot
    displayFunction :: IO ()
    displayFunction = do
      clear [ColorBuffer]
      loadIdentity
      preservingMatrix (mapM_ render renderables)
      swapBuffers

whiteBackground :: Renderable
whiteBackground = Renderable Quads c vertices
  where
    one         = 1 :: CDouble
    c           = Color3 one one one
    coordinates = [(one, -one), (-one, -one), (-one, one), (one, one)]
    vertices    = map (uncurry Vertex2) coordinates
