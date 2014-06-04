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

type CounterMap = M.Map T.Text (IO Counter.Counter)

middleware :: CounterMap -> Application -> Request -> IO Response
middleware counters app req = do
  path <- return $ T.append "path." $ (decodeUtf8 . rawPathInfo) req
  _ <- case (M.lookup path counters) of 
    (Just c) -> fmap Counter.inc c -- this line triggers a crash, but it may be because of laziness (late counter registration)
    Nothing -> (return . return) ()
  app req

application :: Request -> IO Response 
application _ = return $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, world"

routes :: [T.Text]
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

counterMap :: Store -> CounterMap
counterMap s = M.fromList $ zip routes counters
  where 
    counters :: [IO Counter.Counter]
    counters = map (\k -> createCounter k s) routes

main :: IO ()
main = do
  serv <- forkServer "localhost" 8000
  store <- return $ serverMetricStore serv
  counters <- return $ counterMap store
  _ <- forkStatsd defaultStatsdOptions store
  run 3000 $ middleware counters application
