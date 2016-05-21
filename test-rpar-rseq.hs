{-# LANGUAGE DeriveGeneric #-}
-- module Test where

import           Control.Parallel.Strategies
import           Control.DeepSeq
import GHC.Conc (pseq, par)

import           Debug.Trace
import           GHC.Generics (Generic)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment

-------------------
-- first experiment

data A a = A a
  deriving (Show, Generic)

instance NFData a => NFData (A a)

z :: [A Int]
z = runEval $ do
  a <- rseq $ force $ A $ trace "fib 31" (fib 31)
  b <- rpar $ force $ A $ trace "fib 33" (fib 33)
  -- rseq a rseq b
  return [a, b]

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

--------------------
-- second experiment

-- helper function: evaluates the given argument and measures time needed
evalWithTimer f = do
  putStrLn "starting..."
  start <- getCurrentTime
  putStrLn $ "Result: " ++ (show f)
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."


nfib :: Integer -> Integer
nfib n | n < 2 = 1
nfib n = nfib (n - 1) + nfib (n - 2)

-- Eval Monad
qfib :: Integer -> Integer
qfib n | n < 2 = 1
qfib n = runEval $ do
  nf1 <- rpar (qfib (n - 1))
  nf2 <- rpar (qfib (n - 2))
  return $ nf1 + nf2

-- Strategy
sfib :: Integer -> Integer
sfib n | n < 2 = 1
-- sfib n = (withStrategy rpar nfib (n - 1)) + (withStrategy rpar nfib (n - 2)) -- where
sfib n =
  runEval strat where
  nf1 = nfib (n - 1)
  nf2 = nfib (n - 2)
  strat = do
    x <- rpar nf1
    y <- rpar nf2
    return $ x + y

parfibStrat :: Integer -> Integer
parfibStrat n | n < 2 = 1
parfibStrat n = x `par` y `pseq` (x+y)
  where
     x = parfibStrat (n-1)
     y = parfibStrat (n-2)

main = do
  -- (Reasonable setting for Intel Core 2 Duo laptop)
  (n:args) <- getArgs
  let size = read n

  putStrLn "\n nfib"
  evalWithTimer $ nfib size

  putStrLn "\n sfib"
  evalWithTimer $ sfib size

  putStrLn "\n parFibStrat"
  evalWithTimer $ parfibStrat size
