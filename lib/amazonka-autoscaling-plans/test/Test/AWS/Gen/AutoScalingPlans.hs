{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AutoScalingPlans
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestDescribeScalingPlanResources $
--             describeScalingPlanResources
--
--         , requestCreateScalingPlan $
--             createScalingPlan
--
--         , requestDeleteScalingPlan $
--             deleteScalingPlan
--
--         , requestUpdateScalingPlan $
--             updateScalingPlan
--
--         , requestDescribeScalingPlans $
--             describeScalingPlans
--
--           ]

--     , testGroup "response"
--         [ responseDescribeScalingPlanResources $
--             describeScalingPlanResourcesResponse
--
--         , responseCreateScalingPlan $
--             createScalingPlanResponse
--
--         , responseDeleteScalingPlan $
--             deleteScalingPlanResponse
--
--         , responseUpdateScalingPlan $
--             updateScalingPlanResponse
--
--         , responseDescribeScalingPlans $
--             describeScalingPlansResponse
--
--           ]
--     ]

-- Requests

requestDescribeScalingPlanResources :: DescribeScalingPlanResources -> TestTree
requestDescribeScalingPlanResources = req
    "DescribeScalingPlanResources"
    "fixture/DescribeScalingPlanResources.yaml"

requestCreateScalingPlan :: CreateScalingPlan -> TestTree
requestCreateScalingPlan = req
    "CreateScalingPlan"
    "fixture/CreateScalingPlan.yaml"

requestDeleteScalingPlan :: DeleteScalingPlan -> TestTree
requestDeleteScalingPlan = req
    "DeleteScalingPlan"
    "fixture/DeleteScalingPlan.yaml"

requestUpdateScalingPlan :: UpdateScalingPlan -> TestTree
requestUpdateScalingPlan = req
    "UpdateScalingPlan"
    "fixture/UpdateScalingPlan.yaml"

requestDescribeScalingPlans :: DescribeScalingPlans -> TestTree
requestDescribeScalingPlans = req
    "DescribeScalingPlans"
    "fixture/DescribeScalingPlans.yaml"

-- Responses

responseDescribeScalingPlanResources :: DescribeScalingPlanResourcesResponse -> TestTree
responseDescribeScalingPlanResources = res
    "DescribeScalingPlanResourcesResponse"
    "fixture/DescribeScalingPlanResourcesResponse.proto"
    autoScalingPlans
    (Proxy :: Proxy DescribeScalingPlanResources)

responseCreateScalingPlan :: CreateScalingPlanResponse -> TestTree
responseCreateScalingPlan = res
    "CreateScalingPlanResponse"
    "fixture/CreateScalingPlanResponse.proto"
    autoScalingPlans
    (Proxy :: Proxy CreateScalingPlan)

responseDeleteScalingPlan :: DeleteScalingPlanResponse -> TestTree
responseDeleteScalingPlan = res
    "DeleteScalingPlanResponse"
    "fixture/DeleteScalingPlanResponse.proto"
    autoScalingPlans
    (Proxy :: Proxy DeleteScalingPlan)

responseUpdateScalingPlan :: UpdateScalingPlanResponse -> TestTree
responseUpdateScalingPlan = res
    "UpdateScalingPlanResponse"
    "fixture/UpdateScalingPlanResponse.proto"
    autoScalingPlans
    (Proxy :: Proxy UpdateScalingPlan)

responseDescribeScalingPlans :: DescribeScalingPlansResponse -> TestTree
responseDescribeScalingPlans = res
    "DescribeScalingPlansResponse"
    "fixture/DescribeScalingPlansResponse.proto"
    autoScalingPlans
    (Proxy :: Proxy DescribeScalingPlans)
