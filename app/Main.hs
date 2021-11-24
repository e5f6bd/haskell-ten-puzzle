{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lib (solve, exprToStr)
import Data.Ratio ((%))
import Data.Maybe (listToMaybe)

main :: IO ()
main = do
  xs :: [Int] <- map read . words <$> getLine
  case listToMaybe $ solve (% 1) 10 xs of
    Just res -> do
      putStrLn "Found"
      putStrLn $ exprToStr res
    Nothing -> putStrLn "Not found"
