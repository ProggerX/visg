module Main where

import System.Environment (getArgs)
import Visg (drawFile, showDrawing)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    1 -> showDrawing =<< drawFile (head args)
    _ -> error "Expected one argument -- filename"
