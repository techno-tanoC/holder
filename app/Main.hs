module Main where

import Control.Monad
import Control.Concurrent.STM
import Lib

main :: IO ()
main = do
  holder <- atomically newHolder
  atomically $ forM_ [1..10] $ \i ->
    insert i 0 holder
  atomically $ forM_ [1..100000000] $ \i ->
    modify succ (i `mod` 10) holder
  Just n <- atomically $ get 1 holder
  print n
