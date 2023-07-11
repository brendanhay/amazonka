{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Pi
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestDescribeDimensionKeys $
--             newDescribeDimensionKeys
--
--         , requestGetDimensionKeyDetails $
--             newGetDimensionKeyDetails
--
--         , requestGetResourceMetadata $
--             newGetResourceMetadata
--
--         , requestGetResourceMetrics $
--             newGetResourceMetrics
--
--         , requestListAvailableResourceDimensions $
--             newListAvailableResourceDimensions
--
--         , requestListAvailableResourceMetrics $
--             newListAvailableResourceMetrics
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDimensionKeys $
--             newDescribeDimensionKeysResponse
--
--         , responseGetDimensionKeyDetails $
--             newGetDimensionKeyDetailsResponse
--
--         , responseGetResourceMetadata $
--             newGetResourceMetadataResponse
--
--         , responseGetResourceMetrics $
--             newGetResourceMetricsResponse
--
--         , responseListAvailableResourceDimensions $
--             newListAvailableResourceDimensionsResponse
--
--         , responseListAvailableResourceMetrics $
--             newListAvailableResourceMetricsResponse
--
--           ]
--     ]

-- Requests

requestDescribeDimensionKeys :: DescribeDimensionKeys -> TestTree
requestDescribeDimensionKeys =
  req
    "DescribeDimensionKeys"
    "fixture/DescribeDimensionKeys.yaml"

requestGetDimensionKeyDetails :: GetDimensionKeyDetails -> TestTree
requestGetDimensionKeyDetails =
  req
    "GetDimensionKeyDetails"
    "fixture/GetDimensionKeyDetails.yaml"

requestGetResourceMetadata :: GetResourceMetadata -> TestTree
requestGetResourceMetadata =
  req
    "GetResourceMetadata"
    "fixture/GetResourceMetadata.yaml"

requestGetResourceMetrics :: GetResourceMetrics -> TestTree
requestGetResourceMetrics =
  req
    "GetResourceMetrics"
    "fixture/GetResourceMetrics.yaml"

requestListAvailableResourceDimensions :: ListAvailableResourceDimensions -> TestTree
requestListAvailableResourceDimensions =
  req
    "ListAvailableResourceDimensions"
    "fixture/ListAvailableResourceDimensions.yaml"

requestListAvailableResourceMetrics :: ListAvailableResourceMetrics -> TestTree
requestListAvailableResourceMetrics =
  req
    "ListAvailableResourceMetrics"
    "fixture/ListAvailableResourceMetrics.yaml"

-- Responses

responseDescribeDimensionKeys :: DescribeDimensionKeysResponse -> TestTree
responseDescribeDimensionKeys =
  res
    "DescribeDimensionKeysResponse"
    "fixture/DescribeDimensionKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDimensionKeys)

responseGetDimensionKeyDetails :: GetDimensionKeyDetailsResponse -> TestTree
responseGetDimensionKeyDetails =
  res
    "GetDimensionKeyDetailsResponse"
    "fixture/GetDimensionKeyDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDimensionKeyDetails)

responseGetResourceMetadata :: GetResourceMetadataResponse -> TestTree
responseGetResourceMetadata =
  res
    "GetResourceMetadataResponse"
    "fixture/GetResourceMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceMetadata)

responseGetResourceMetrics :: GetResourceMetricsResponse -> TestTree
responseGetResourceMetrics =
  res
    "GetResourceMetricsResponse"
    "fixture/GetResourceMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceMetrics)

responseListAvailableResourceDimensions :: ListAvailableResourceDimensionsResponse -> TestTree
responseListAvailableResourceDimensions =
  res
    "ListAvailableResourceDimensionsResponse"
    "fixture/ListAvailableResourceDimensionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAvailableResourceDimensions)

responseListAvailableResourceMetrics :: ListAvailableResourceMetricsResponse -> TestTree
responseListAvailableResourceMetrics =
  res
    "ListAvailableResourceMetricsResponse"
    "fixture/ListAvailableResourceMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAvailableResourceMetrics)
