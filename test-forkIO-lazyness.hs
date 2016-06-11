import System.IO
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (evaluate)
import Control.Concurrent.Async (async, wait, cancel)
import Control.Concurrent.MVar
import Control.DeepSeq

-- Intentionally slow fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

blip :: IO ()
blip = do
  putStrLn "BLIP"
  threadDelay 500000
  blip


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  v1 <- newEmptyMVar

  forkIO $ do
    let r1 = map fib [20, 29, 30]
    putMVar v1 (force r1)

  blips <- async blip

  l1 <- takeMVar v1
  print l1

  cancel blips
  return ()
