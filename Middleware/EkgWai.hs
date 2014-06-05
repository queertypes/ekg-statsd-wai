{-# LANGUAGE OverloadedStrings #-}
module Middleware.EkgWai (middleware, counterMap) where

import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as M
import Network.Wai
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

counterMap :: Routes -> Store -> IO CounterMap
counterMap routes' store = do
  counters <- mapM (`createCounter` store) routes'
  return $ M.fromList $ zip routes' counters
