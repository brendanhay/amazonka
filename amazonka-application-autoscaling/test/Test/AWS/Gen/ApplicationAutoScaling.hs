{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ApplicationAutoScaling
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ApplicationAutoScaling where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ApplicationAutoScaling
import Test.AWS.ApplicationAutoScaling.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteScalingPolicy $
--             deleteScalingPolicy
--
--         , requestPutScalingPolicy $
--             putScalingPolicy
--
--         , requestRegisterScalableTarget $
--             registerScalableTarget
--
--         , requestDescribeScalingPolicies $
--             describeScalingPolicies
--
--         , requestDescribeScalableTargets $
--             describeScalableTargets
--
--         , requestDescribeScalingActivities $
--             describeScalingActivities
--
--         , requestDeregisterScalableTarget $
--             deregisterScalableTarget
--
--           ]

--     , testGroup "response"
--         [ responseDeleteScalingPolicy $
--             deleteScalingPolicyResponse
--
--         , responsePutScalingPolicy $
--             putScalingPolicyResponse
--
--         , responseRegisterScalableTarget $
--             registerScalableTargetResponse
--
--         , responseDescribeScalingPolicies $
--             describeScalingPoliciesResponse
--
--         , responseDescribeScalableTargets $
--             describeScalableTargetsResponse
--
--         , responseDescribeScalingActivities $
--             describeScalingActivitiesResponse
--
--         , responseDeregisterScalableTarget $
--             deregisterScalableTargetResponse
--
--           ]
--     ]

-- Requests

requestDeleteScalingPolicy :: DeleteScalingPolicy -> TestTree
requestDeleteScalingPolicy = req
    "DeleteScalingPolicy"
    "fixture/DeleteScalingPolicy.yaml"

requestPutScalingPolicy :: PutScalingPolicy -> TestTree
requestPutScalingPolicy = req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

requestRegisterScalableTarget :: RegisterScalableTarget -> TestTree
requestRegisterScalableTarget = req
    "RegisterScalableTarget"
    "fixture/RegisterScalableTarget.yaml"

requestDescribeScalingPolicies :: DescribeScalingPolicies -> TestTree
requestDescribeScalingPolicies = req
    "DescribeScalingPolicies"
    "fixture/DescribeScalingPolicies.yaml"

requestDescribeScalableTargets :: DescribeScalableTargets -> TestTree
requestDescribeScalableTargets = req
    "DescribeScalableTargets"
    "fixture/DescribeScalableTargets.yaml"

requestDescribeScalingActivities :: DescribeScalingActivities -> TestTree
requestDescribeScalingActivities = req
    "DescribeScalingActivities"
    "fixture/DescribeScalingActivities.yaml"

requestDeregisterScalableTarget :: DeregisterScalableTarget -> TestTree
requestDeregisterScalableTarget = req
    "DeregisterScalableTarget"
    "fixture/DeregisterScalableTarget.yaml"

-- Responses

responseDeleteScalingPolicy :: DeleteScalingPolicyResponse -> TestTree
responseDeleteScalingPolicy = res
    "DeleteScalingPolicyResponse"
    "fixture/DeleteScalingPolicyResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy DeleteScalingPolicy)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy = res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy PutScalingPolicy)

responseRegisterScalableTarget :: RegisterScalableTargetResponse -> TestTree
responseRegisterScalableTarget = res
    "RegisterScalableTargetResponse"
    "fixture/RegisterScalableTargetResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy RegisterScalableTarget)

responseDescribeScalingPolicies :: DescribeScalingPoliciesResponse -> TestTree
responseDescribeScalingPolicies = res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy DescribeScalingPolicies)

responseDescribeScalableTargets :: DescribeScalableTargetsResponse -> TestTree
responseDescribeScalableTargets = res
    "DescribeScalableTargetsResponse"
    "fixture/DescribeScalableTargetsResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy DescribeScalableTargets)

responseDescribeScalingActivities :: DescribeScalingActivitiesResponse -> TestTree
responseDescribeScalingActivities = res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy DescribeScalingActivities)

responseDeregisterScalableTarget :: DeregisterScalableTargetResponse -> TestTree
responseDeregisterScalableTarget = res
    "DeregisterScalableTargetResponse"
    "fixture/DeregisterScalableTargetResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy DeregisterScalableTarget)
