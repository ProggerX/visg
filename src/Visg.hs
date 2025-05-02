module Visg where

import Control.Monad.State (State, evalState, get, put)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Graphics.Gloss
import Visg.Parser qualified as P

type CNC = State (Float, Float, Float)

(!?) :: (Eq a) => [(a, b)] -> a -> Maybe b
(!?) = flip lookup

drawCode :: P.Action -> CNC Picture
drawCode act = case P.code act of
  "T1" -> pure blank
  "G17" -> pure blank
  "G90" -> pure blank
  "M30" -> pure blank
  "G0" -> do
    let as = P.assocs act
    (cx, cy, cz) <- get
    let nx = fromMaybe cx $ as !? 'X'
    let ny = fromMaybe cy $ as !? 'Y'
    let nz = fromMaybe cz $ as !? 'Z'
    put (nx, ny, nz)
    pure $ color (makeColorI 0 (round nz `mod` 255) 255 255) $ pictures [translate nx ny $ circle 0.2, line [(cx, cy), (nx, ny)]]
  "G1" -> do
    let as = P.assocs act
    (cx, cy, cz) <- get
    let nx = fromMaybe cx $ as !? 'X'
    let ny = fromMaybe cy $ as !? 'Y'
    let nz = fromMaybe cz $ as !? 'Z'
    put (nx, ny, nz)
    pure $ color (makeColorI 255 (negate $ round nz `mod` 255) 0 255) $ pictures [translate nx ny $ circle 0.5, line [(cx, cy), (nx, ny)]]
  _ -> trace (show (P.code act) ++ " is not yet supported") (pure blank)

drawGCode :: [P.Action] -> Picture
drawGCode s = evalState (pictures <$> mapM drawCode s) (0, 0, 0)

drawFile :: FilePath -> IO Picture
drawFile f = do
  r <- P.parseFile f
  case r of
    Right acts -> pure $ drawGCode acts
    Left err -> error $ show err

grid :: Int -> Float -> Picture
grid gridSize cellSize =
  color (greyN 0.2) $
    pictures $
      [line [(x, -half), (x, half)] | i <- [0 .. gridSize], let x = fromIntegral i * cellSize - half]
        ++ [line [(-half, y), (half, y)] | i <- [0 .. gridSize], let y = fromIntegral i * cellSize - half]
 where
  half = (fromIntegral gridSize * cellSize) / 2

showDrawing :: Picture -> IO ()
showDrawing p = display (InWindow "GCode" (800, 800) (0, 0)) black $ scale 10 10 $ pictures [grid 500 1, p]
