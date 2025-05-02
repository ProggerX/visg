module Visg where

import Control.Monad.State (State, get, modify, runState)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Graphics.Gloss
import Visg.Parser qualified as P

type CNC = State (Float, Float, Float, Float, Float)

(!?) :: (Eq a) => [(a, b)] -> a -> Maybe b
(!?) = flip lookup

move :: Float -> Float -> Float -> CNC ()
move x y z = modify (\(_, _, _, px, py) -> (x, y, z, max x px, max y py))

drawCode :: P.Action -> CNC Picture
drawCode act = case P.code act of
  "T1" -> pure blank
  "G17" -> pure blank
  "G90" -> pure blank
  "M30" -> pure blank
  "G0" -> do
    let as = P.assocs act
    (cx, cy, cz, _, _) <- get
    let nx = fromMaybe cx $ as !? 'X'
    let ny = fromMaybe cy $ as !? 'Y'
    let nz = fromMaybe cz $ as !? 'Z'
    move nx ny nz
    pure $ color (makeColorI 0 (round nz `mod` 255) 255 255) $ pictures [translate nx ny $ circle 0.2, line [(cx, cy), (nx, ny)]]
  "G1" -> do
    let as = P.assocs act
    (cx, cy, cz, _, _) <- get
    let nx = fromMaybe cx $ as !? 'X'
    let ny = fromMaybe cy $ as !? 'Y'
    let nz = fromMaybe cz $ as !? 'Z'
    move nx ny nz
    pure $ color (makeColorI 255 (negate $ round nz `mod` 255) 0 255) $ pictures [translate nx ny $ circle 0.5, line [(cx, cy), (nx, ny)]]
  _ -> trace (show (P.code act) ++ " is not yet supported") (pure blank)

grid :: Int -> Float -> Picture
grid gridSize cellSize =
  color (greyN 0.2) $
    pictures $
      [line [(x, -half), (x, half)] | i <- [0 .. gridSize], let x = fromIntegral i * cellSize - half]
        ++ [line [(-half, y), (half, y)] | i <- [0 .. gridSize], let y = fromIntegral i * cellSize - half]
 where
  half = (fromIntegral gridSize * cellSize) / 2

drawGCode :: [P.Action] -> (Picture, Float, Float)
drawGCode s =
  let (pic, f) = runState (pictures <$> mapM drawCode s) (0, 0, 0, 0, 0)
      (_, _, _, px, py) = f
   in (pic, px, py)

drawFile :: FilePath -> IO (Picture, Float, Float)
drawFile f = do
  r <- P.parseFile f
  case r of
    Right acts -> pure $ drawGCode acts
    Left err -> error $ show err

txt :: [String] -> Picture
txt = scale 0.3 0.3 . color white . text . concat

showDrawing :: (Picture, Float, Float) -> IO ()
showDrawing (p, px, py) =
  let sz :: Int = max (round px + 1) (round py + 1) + 5
      fac = fromIntegral ((sz + 1) `div` 2) - 2
   in display (InWindow "GCode" (800, 800) (0, 0)) black $
        pictures
          [ scale 10 10 $
              pictures
                [ translate fac fac $ grid sz 1
                , p
                ]
          , translate (-100) (-100) $ txt ["size: ", show @Int $ round px, "X", show @Int $ round py]
          ]
