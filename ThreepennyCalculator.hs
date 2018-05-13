-----------------------------------------------------------------------------------------------------------------------
-- Setting up the modules for the script


import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Data.IORef

import Expr

import Data.Maybe
import Text.Read hiding (get)
-------------------------------------------------------------------------------------------------------------------------
-- Setting up the helper functions for the plotter

canvasWidth,canvasHeight :: Num a => a                                           -- size for canvas elements
canvasWidth  = 300
canvasHeight = 300


pixToReal :: Double -> Double
pixToReal x = (x - fromIntegral canvasWidth / 2) * 0.04

realToPix :: Double -> Double
realToPix x = x / 0.04 + fromIntegral canvasWidth / 2

points :: Expr -> Double -> (Int,Int) -> [UI.Point]
points expr scale (width, height) = map point [0..width]
    where
        point :: Int -> (Double, Double)
        point x = (fromIntegral x, pointY x)
        pointY = realToPix
            . negate
            . (*scale)
            . (eval expr)
            . (/scale)
            . pixToReal
            . fromIntegral

lineStep :: [a] -> [(a, a)]
lineStep (x:y:[]) = [(x, y)]
lineStep (x:y:ys) = (x, y) : lineStep (y:ys)

linez :: Expr -> Double -> (Int,Int) -> [(UI.Point, UI.Point)]
linez expr scale canSize = lineStep $ points expr scale canSize

readDouble :: String -> Maybe Double
readDouble = readMaybe

convertInput :: String -> String -> Maybe (Expr, Double)
convertInput expText expScale = (,) <$> readExpr expText <*> readDouble expScale
-----------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Elements
     canvas     <- mkCanvas canvasWidth canvasHeight   -- The drawing area
     fx         <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input      <- mkInput 20 "x"                -- The formula input
     draw       <- mkButton "Draw Graph"         -- The draw button
     diff       <- mkButton "Differentiate"      -- The differentiate button
     clr        <- mkButton "Clear Graph"        -- Clears Graph
     scaleUp    <- mkButton "Scale up"           -- Scale Up Graph
     scaleDown  <- mkButton "Scale Down"         -- Scale Down Graph
     normScale  <- mkButton "Normal Scale"       -- Scale to normal
     top        <- UI.div                        -- Page Heading
                   #+  [ UI.h1     # set UI.text "Graph Generator"]
     return window # set UI.title "Functional Programming - Lab 4"
     let scaler = 1.0    
     -- Layout
     formula <- row [pure fx,pure input]
     getBody window #+ [pure top,column [pure canvas, pure formula, pure draw, row[pure scaleUp,pure scaleDown,pure normScale],pure diff,pure clr]]  

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure canvas    # set style [("textAlign","center")]
     pure input     # set style [("fontSize","14pt")]
     pure clr       # set style [("backgroundColor","grey")]

     -- Interaction
     on UI.click     draw  $ \ _ -> readAndDraw input 1.0 canvas               --draws the plot for the given function
     on UI.click     diff  $ \ _ -> do                                         --draws the plot of differentiated input function
      ddt input
      readAndDraw input 1.0 canvas

     on UI.click     clr $ \_    -> do                                         --clears the canvas
                                      UI.clearCanvas canvas
                                      set value ("x") (pure input)

     on UI.click     scaleUp   $ \_ -> readAndDraw input (scaler/0.5) canvas   --zooms in the plot by a factor of 2
     on UI.click     scaleDown $ \_ -> readAndDraw input (scaler*0.5) canvas   --zooms out of the plot by a factor of 2
     on UI.click     normScale $ \_ -> readAndDraw input (scaler) canvas       --normalizes the zoomed plots
     on valueChange' input     $ \ _ -> readAndDraw input 1.0 canvas           

---------------------------------------------------------------------------------------------------------------------------
-- | creates the plot from input and scale | default set to 1 if not zoomed in or out
readAndDraw :: Element -> Double -> Canvas -> UI ()
readAndDraw input scale canvas =
  do s <- get value input

     set UI.fillStyle (UI.solidColor (UI.RGB 255 255 255)) (pure canvas)
     UI.fillRect (0,0) canvasWidth canvasHeight canvas
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)

     case readExpr s of
      Just expr -> do
        plotExpr expr

      Nothing -> do
        return()

      where
        plotExpr expr1 = (id
          . drawLines
          . linez expr1 scale) (canvasWidth, canvasHeight)

        --drawLines 
        drawLines [] = return ()
        drawLines ((p1, p2):xs) = do
            UI.beginPath canvas

            UI.moveTo p1 canvas
            UI.lineTo p2 canvas

            UI.closePath canvas
            UI.stroke canvas

            drawLines xs

-- | generates the differentiated function
ddt :: Element -> UI ()
ddt input = do
               fx <- get value input
               case readExpr fx of
                   Just expr -> do
                                  let dfxdt = differentiate  expr
                                  set value (show dfxdt) (pure input)
                                  return ()
                   Nothing -> do
                                return ()

