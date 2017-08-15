-- Author: Noah Halford
-- Main.hs
module Main where

import Taxi
import Learn

main :: IO ()
main = trainNN 50000

{-
 - uncomment to play game
main :: IO ()
main = playGame
-}
