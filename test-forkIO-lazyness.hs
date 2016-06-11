import System.IO
import Control.Concurrent (threadDelay, forkIO, myThreadId)
import Control.Exception (evaluate, bracket_)
import Control.Concurrent.Async (async, wait, cancel)
import Control.Concurrent.MVar
import Control.DeepSeq

import Debug.Trace (traceEventIO)
import GHC.Conc (labelThread)


event :: String -> IO a -> IO a
event label =
  bracket_ (traceEventIO $ "START " ++ label)
           (traceEventIO $ "STOP "  ++ label)



-- Intentionally slow fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

blip :: IO ()
blip = do
  tid <- myThreadId
  labelThread tid "blip-thread"
  doBlips where
    doBlips = do
      putStrLn "BLIP"
      threadDelay 500000
      doBlips


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  tid <- myThreadId
  labelThread tid "main-thread"

  v1 <- newEmptyMVar

  forkIO $ do
    tid <- myThreadId
    labelThread tid "fib-thread"
    let r1 = map fib [20, 29, 30, 34]
    print r1
    putMVar v1 r1

  blips <- async blip

  l1 <- takeMVar v1
  print l1

  cancel blips
  return ()
