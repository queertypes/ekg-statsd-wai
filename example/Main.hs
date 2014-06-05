{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Remote.Monitoring
import System.Remote.Monitoring.Statsd

import qualified Middleware.EkgWai as EKG

application :: Request -> IO Response
application _ = return $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, world"

main :: IO ()
main = do
  serv <- forkServer "localhost" 8000
  store <- return $ serverMetricStore serv
  counters <- EKG.counterMap routes store
  _ <- forkStatsd defaultStatsdOptions store
  run 3000 $ EKG.middleware counters application
  where
    routes = ["/", "/hello"]