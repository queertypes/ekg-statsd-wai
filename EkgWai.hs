{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as M
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import System.Remote.Monitoring
import System.Remote.Monitoring.Statsd
import System.Metrics
import qualified System.Metrics.Counter as Counter

type CounterMap = M.Map T.Text Counter.Counter
type Routes = [T.Text]

middleware :: CounterMap -> Application -> Request -> IO Response
middleware counters app req = do
  path <- return $ T.append "path." $ (decodeUtf8 . rawPathInfo) req

  _ <- case M.lookup path counters of
        Just c -> Counter.inc c
        Nothing -> return ()

  app req

application :: Request -> IO Response
application _ = return $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, world"

routes :: Routes
routes = map (T.append "path.") [
    "/serversCollection",
    "/servers",
    "/ipaddresses",
    "/allIpsBatch",
    "/primaryIpsBatch",
    "/privateIpsBatch",
    "/publicIpsBatch",
    "/ipv6IpsBatch",
    "/blockBatch"
  ]

counterMap :: Routes -> Store -> IO CounterMap
counterMap routes' store = do
  counters <- mapM (`createCounter` store) routes'
  return $ M.fromList $ zip routes' counters

main :: IO ()
main = do
  serv <- forkServer "localhost" 8000
  store <- return $ serverMetricStore serv
  counters <- counterMap routes store
  _ <- forkStatsd defaultStatsdOptions store
  run 3000 $ middleware counters application
