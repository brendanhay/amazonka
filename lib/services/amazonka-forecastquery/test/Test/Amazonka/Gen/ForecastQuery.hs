{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ForecastQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ForecastQuery where

import Amazonka.ForecastQuery
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.ForecastQuery.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestQueryForecast $
--             newQueryForecast
--
--         , requestQueryWhatIfForecast $
--             newQueryWhatIfForecast
--
--           ]

--     , testGroup "response"
--         [ responseQueryForecast $
--             newQueryForecastResponse
--
--         , responseQueryWhatIfForecast $
--             newQueryWhatIfForecastResponse
--
--           ]
--     ]

-- Requests

requestQueryForecast :: QueryForecast -> TestTree
requestQueryForecast =
  req
    "QueryForecast"
    "fixture/QueryForecast.yaml"

requestQueryWhatIfForecast :: QueryWhatIfForecast -> TestTree
requestQueryWhatIfForecast =
  req
    "QueryWhatIfForecast"
    "fixture/QueryWhatIfForecast.yaml"

-- Responses

responseQueryForecast :: QueryForecastResponse -> TestTree
responseQueryForecast =
  res
    "QueryForecastResponse"
    "fixture/QueryForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy QueryForecast)

responseQueryWhatIfForecast :: QueryWhatIfForecastResponse -> TestTree
responseQueryWhatIfForecast =
  res
    "QueryWhatIfForecastResponse"
    "fixture/QueryWhatIfForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy QueryWhatIfForecast)
