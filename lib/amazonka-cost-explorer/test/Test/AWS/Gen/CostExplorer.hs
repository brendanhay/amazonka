{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CostExplorer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CostExplorer where

import Data.Proxy
import Network.AWS.CostExplorer
import Test.AWS.CostExplorer.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetReservationUtilization $
--             getReservationUtilization
--
--         , requestGetTags $
--             getTags
--
--         , requestGetReservationCoverage $
--             getReservationCoverage
--
--         , requestGetDimensionValues $
--             getDimensionValues
--
--         , requestGetReservationPurchaseRecommendation $
--             getReservationPurchaseRecommendation
--
--         , requestGetCostAndUsage $
--             getCostAndUsage
--
--           ]

--     , testGroup "response"
--         [ responseGetReservationUtilization $
--             getReservationUtilizationResponse
--
--         , responseGetTags $
--             getTagsResponse
--
--         , responseGetReservationCoverage $
--             getReservationCoverageResponse
--
--         , responseGetDimensionValues $
--             getDimensionValuesResponse
--
--         , responseGetReservationPurchaseRecommendation $
--             getReservationPurchaseRecommendationResponse
--
--         , responseGetCostAndUsage $
--             getCostAndUsageResponse
--
--           ]
--     ]

-- Requests

requestGetReservationUtilization :: GetReservationUtilization -> TestTree
requestGetReservationUtilization = req
    "GetReservationUtilization"
    "fixture/GetReservationUtilization.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags = req
    "GetTags"
    "fixture/GetTags.yaml"

requestGetReservationCoverage :: GetReservationCoverage -> TestTree
requestGetReservationCoverage = req
    "GetReservationCoverage"
    "fixture/GetReservationCoverage.yaml"

requestGetDimensionValues :: GetDimensionValues -> TestTree
requestGetDimensionValues = req
    "GetDimensionValues"
    "fixture/GetDimensionValues.yaml"

requestGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendation -> TestTree
requestGetReservationPurchaseRecommendation = req
    "GetReservationPurchaseRecommendation"
    "fixture/GetReservationPurchaseRecommendation.yaml"

requestGetCostAndUsage :: GetCostAndUsage -> TestTree
requestGetCostAndUsage = req
    "GetCostAndUsage"
    "fixture/GetCostAndUsage.yaml"

-- Responses

responseGetReservationUtilization :: GetReservationUtilizationResponse -> TestTree
responseGetReservationUtilization = res
    "GetReservationUtilizationResponse"
    "fixture/GetReservationUtilizationResponse.proto"
    costExplorer
    (Proxy :: Proxy GetReservationUtilization)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags = res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    costExplorer
    (Proxy :: Proxy GetTags)

responseGetReservationCoverage :: GetReservationCoverageResponse -> TestTree
responseGetReservationCoverage = res
    "GetReservationCoverageResponse"
    "fixture/GetReservationCoverageResponse.proto"
    costExplorer
    (Proxy :: Proxy GetReservationCoverage)

responseGetDimensionValues :: GetDimensionValuesResponse -> TestTree
responseGetDimensionValues = res
    "GetDimensionValuesResponse"
    "fixture/GetDimensionValuesResponse.proto"
    costExplorer
    (Proxy :: Proxy GetDimensionValues)

responseGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendationResponse -> TestTree
responseGetReservationPurchaseRecommendation = res
    "GetReservationPurchaseRecommendationResponse"
    "fixture/GetReservationPurchaseRecommendationResponse.proto"
    costExplorer
    (Proxy :: Proxy GetReservationPurchaseRecommendation)

responseGetCostAndUsage :: GetCostAndUsageResponse -> TestTree
responseGetCostAndUsage = res
    "GetCostAndUsageResponse"
    "fixture/GetCostAndUsageResponse.proto"
    costExplorer
    (Proxy :: Proxy GetCostAndUsage)
