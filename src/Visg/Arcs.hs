module Visg.Arcs where

import Control.Monad.State (State, get, put)
import Data.Fixed (mod')
import Data.Maybe (fromJust)
import Graphics.Gloss (Picture, color, line, makeColorI)
import Visg.Parser (Action (..))

type CNC = State (Float, Float, Float, Float, Float)

drawG02Arc' :: Float -> Float -> Float -> Action -> (Picture, Float, Float)
drawG02Arc' cx cy cz action = (color (makeColorI 255 (round cz `mod` 255) 255 255) $ line points, x, y)
 where
  assocs' = assocs action
  x = fromJust $ lookup 'X' assocs'
  y = fromJust $ lookup 'Y' assocs'
  iMaybe = lookup 'I' assocs'
  jMaybe = lookup 'J' assocs'
  rMaybe = lookup 'R' assocs'

  (centerX, centerY, radius) =
    case (iMaybe, jMaybe, rMaybe) of
      (Just i, Just j, _) ->
        let cx' = cx + i
            cy' = cy + j
            r = sqrt $ i * i + j * j
         in (cx', cy', r)
      (Nothing, Nothing, Just r) ->
        let dx = x - cx
            dy = y - cy
            d = sqrt $ dx * dx + dy * dy
            midX = (cx + x) / 2
            midY = (cy + y) / 2
            perpX = dy
            perpY = -dx
            unitPerpX = perpX / d
            unitPerpY = perpY / d
            h = sqrt (r * r - (d / 2) ^ (2 :: Int))
         in (midX + h * unitPerpX, midY + h * unitPerpY, abs r)
      _ -> error "Invalid arc parameters: must specify either I and J or R"

  dxStart = cx - centerX
  dyStart = cy - centerY
  dxEnd = x - centerX
  dyEnd = y - centerY

  start_rad = atan2 dyStart dxStart
  end_rad = atan2 dyEnd dxEnd

  delta_rad = (start_rad - end_rad) `mod'` (2 * pi)
  step = delta_rad / 35

  indices = [0 .. 35]
  angles = map (\t -> start_rad - t * step) indices

  points =
    map
      ( \theta ->
          ( centerX + radius * cos theta
          , centerY + radius * sin theta
          )
      )
      angles

drawG03Arc' :: Float -> Float -> Float -> Action -> (Picture, Float, Float)
drawG03Arc' cx cy cz action = (color (makeColorI 255 (round cz `mod` 255) 255 255) $ line points, x, y)
 where
  assocs' = assocs action
  x = fromJust $ lookup 'X' assocs'
  y = fromJust $ lookup 'Y' assocs'
  iMaybe = lookup 'I' assocs'
  jMaybe = lookup 'J' assocs'
  rMaybe = lookup 'R' assocs'

  (centerX, centerY, radius) =
    case (iMaybe, jMaybe, rMaybe) of
      (Just i, Just j, _) ->
        let cx' = cx + i
            cy' = cy + j
            r = sqrt (i * i + j * j)
         in (cx', cy', r)
      (Nothing, Nothing, Just r) ->
        let dx = x - cx
            dy = y - cy
            dSquared = dx * dx + dy * dy
            d = sqrt dSquared
            midX = (cx + x) / 2
            midY = (cy + y) / 2
            perpX = -dy
            perpY = dx
            unitPerpX = perpX / d
            unitPerpY = perpY / d
            h = sqrt (r * r - (d / 2) ^ (2 :: Int))
         in (midX + h * unitPerpX, midY + h * unitPerpY, abs r)
      _ -> error "Invalid arc parameters: must specify either I and J or R"

  dxStart = cx - centerX
  dyStart = cy - centerY
  dxEnd = x - centerX
  dyEnd = y - centerY

  start_rad = atan2 dyStart dxStart
  end_rad = atan2 dyEnd dxEnd

  delta_rad = (end_rad - start_rad) `mod'` (2 * pi)
  step = delta_rad / 35

  indices = [0 .. 35]
  angles = map (\t -> start_rad + t * step) indices

  points =
    map
      ( \theta ->
          ( centerX + radius * cos theta
          , centerY + radius * sin theta
          )
      )
      angles

drawG02Arc :: Action -> CNC Picture
drawG02Arc act = do
  (cx, cy, cz, a, b) <- get
  let (pic, nx, ny) = drawG02Arc' cx cy cz act
  put (nx, ny, cz, a, b)
  pure pic

drawG03Arc :: Action -> CNC Picture
drawG03Arc act = do
  (cx, cy, cz, a, b) <- get
  let (pic, nx, ny) = drawG03Arc' cx cy cz act
  put (nx, ny, cz, a, b)
  pure pic
