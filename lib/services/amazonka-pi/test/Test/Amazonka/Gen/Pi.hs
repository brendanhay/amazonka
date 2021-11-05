{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Pi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Pi where

import Amazonka.Pi
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Pi.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetDimensionKeyDetails $
--             newGetDimensionKeyDetails
--
--         , requestGetResourceMetrics $
--             newGetResourceMetrics
--
--         , requestDescribeDimensionKeys $
--             newDescribeDimensionKeys
--
--           ]

--     , testGroup "response"
--         [ responseGetDimensionKeyDetails $
--             newGetDimensionKeyDetailsResponse
--
--         , responseGetResourceMetrics $
--             newGetResourceMetricsResponse
--
--         , responseDescribeDimensionKeys $
--             newDescribeDimensionKeysResponse
--
--           ]
--     ]

-- Requests

requestGetDimensionKeyDetails :: GetDimensionKeyDetails -> TestTree
requestGetDimensionKeyDetails =
  req
    "GetDimensionKeyDetails"
    "fixture/GetDimensionKeyDetails.yaml"

requestGetResourceMetrics :: GetResourceMetrics -> TestTree
requestGetResourceMetrics =
  req
    "GetResourceMetrics"
    "fixture/GetResourceMetrics.yaml"

requestDescribeDimensionKeys :: DescribeDimensionKeys -> TestTree
requestDescribeDimensionKeys =
  req
    "DescribeDimensionKeys"
    "fixture/DescribeDimensionKeys.yaml"

-- Responses

responseGetDimensionKeyDetails :: GetDimensionKeyDetailsResponse -> TestTree
responseGetDimensionKeyDetails =
  res
    "GetDimensionKeyDetailsResponse"
    "fixture/GetDimensionKeyDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDimensionKeyDetails)

responseGetResourceMetrics :: GetResourceMetricsResponse -> TestTree
responseGetResourceMetrics =
  res
    "GetResourceMetricsResponse"
    "fixture/GetResourceMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceMetrics)

responseDescribeDimensionKeys :: DescribeDimensionKeysResponse -> TestTree
responseDescribeDimensionKeys =
  res
    "DescribeDimensionKeysResponse"
    "fixture/DescribeDimensionKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDimensionKeys)
