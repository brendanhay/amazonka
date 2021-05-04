{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AutoScalingPlans
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.AutoScalingPlans where

import Data.Proxy
import Network.AWS.AutoScalingPlans
import Test.AWS.AutoScalingPlans.Internal
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
--         [ requestUpdateScalingPlan $
--             newUpdateScalingPlan
--
--         , requestDeleteScalingPlan $
--             newDeleteScalingPlan
--
--         , requestDescribeScalingPlanResources $
--             newDescribeScalingPlanResources
--
--         , requestGetScalingPlanResourceForecastData $
--             newGetScalingPlanResourceForecastData
--
--         , requestDescribeScalingPlans $
--             newDescribeScalingPlans
--
--         , requestCreateScalingPlan $
--             newCreateScalingPlan
--
--           ]

--     , testGroup "response"
--         [ responseUpdateScalingPlan $
--             newUpdateScalingPlanResponse
--
--         , responseDeleteScalingPlan $
--             newDeleteScalingPlanResponse
--
--         , responseDescribeScalingPlanResources $
--             newDescribeScalingPlanResourcesResponse
--
--         , responseGetScalingPlanResourceForecastData $
--             newGetScalingPlanResourceForecastDataResponse
--
--         , responseDescribeScalingPlans $
--             newDescribeScalingPlansResponse
--
--         , responseCreateScalingPlan $
--             newCreateScalingPlanResponse
--
--           ]
--     ]

-- Requests

requestUpdateScalingPlan :: UpdateScalingPlan -> TestTree
requestUpdateScalingPlan =
  req
    "UpdateScalingPlan"
    "fixture/UpdateScalingPlan.yaml"

requestDeleteScalingPlan :: DeleteScalingPlan -> TestTree
requestDeleteScalingPlan =
  req
    "DeleteScalingPlan"
    "fixture/DeleteScalingPlan.yaml"

requestDescribeScalingPlanResources :: DescribeScalingPlanResources -> TestTree
requestDescribeScalingPlanResources =
  req
    "DescribeScalingPlanResources"
    "fixture/DescribeScalingPlanResources.yaml"

requestGetScalingPlanResourceForecastData :: GetScalingPlanResourceForecastData -> TestTree
requestGetScalingPlanResourceForecastData =
  req
    "GetScalingPlanResourceForecastData"
    "fixture/GetScalingPlanResourceForecastData.yaml"

requestDescribeScalingPlans :: DescribeScalingPlans -> TestTree
requestDescribeScalingPlans =
  req
    "DescribeScalingPlans"
    "fixture/DescribeScalingPlans.yaml"

requestCreateScalingPlan :: CreateScalingPlan -> TestTree
requestCreateScalingPlan =
  req
    "CreateScalingPlan"
    "fixture/CreateScalingPlan.yaml"

-- Responses

responseUpdateScalingPlan :: UpdateScalingPlanResponse -> TestTree
responseUpdateScalingPlan =
  res
    "UpdateScalingPlanResponse"
    "fixture/UpdateScalingPlanResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateScalingPlan)

responseDeleteScalingPlan :: DeleteScalingPlanResponse -> TestTree
responseDeleteScalingPlan =
  res
    "DeleteScalingPlanResponse"
    "fixture/DeleteScalingPlanResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteScalingPlan)

responseDescribeScalingPlanResources :: DescribeScalingPlanResourcesResponse -> TestTree
responseDescribeScalingPlanResources =
  res
    "DescribeScalingPlanResourcesResponse"
    "fixture/DescribeScalingPlanResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScalingPlanResources)

responseGetScalingPlanResourceForecastData :: GetScalingPlanResourceForecastDataResponse -> TestTree
responseGetScalingPlanResourceForecastData =
  res
    "GetScalingPlanResourceForecastDataResponse"
    "fixture/GetScalingPlanResourceForecastDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetScalingPlanResourceForecastData)

responseDescribeScalingPlans :: DescribeScalingPlansResponse -> TestTree
responseDescribeScalingPlans =
  res
    "DescribeScalingPlansResponse"
    "fixture/DescribeScalingPlansResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScalingPlans)

responseCreateScalingPlan :: CreateScalingPlanResponse -> TestTree
responseCreateScalingPlan =
  res
    "CreateScalingPlanResponse"
    "fixture/CreateScalingPlanResponse.proto"
    defaultService
    (Proxy :: Proxy CreateScalingPlan)
