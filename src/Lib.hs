{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( testZ3
    ) where

import Control.Monad
import Grisette

testZ3 :: IO ()
testZ3 = do
  putStrLn "Testing Grisette installation."
  let x = "x" :: SymInteger
  let y = "y" :: SymInteger
  let constraint = x + y .== 7
  m <- solve (precise z3) constraint
  case m of
    Left _ -> do
      error "The solver is malfunctioning."
    Right model -> do
      when (evaluateSymToCon model (x + y) /= (7 :: Integer)) $ error "The solver is malfunctioning"
      putStrLn "Congratulations! You have a working Grisette and solver installation."
