{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ApplicationAutoScaling
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
--         [ testDeleteScalingPolicy $
--             deleteScalingPolicy
--
--         , testPutScalingPolicy $
--             putScalingPolicy
--
--         , testRegisterScalableTarget $
--             registerScalableTarget
--
--         , testDescribeScalingPolicies $
--             describeScalingPolicies
--
--         , testDescribeScalableTargets $
--             describeScalableTargets
--
--         , testDescribeScalingActivities $
--             describeScalingActivities
--
--         , testDeregisterScalableTarget $
--             deregisterScalableTarget
--
--           ]

--     , testGroup "response"
--         [ testDeleteScalingPolicyResponse $
--             deleteScalingPolicyResponse
--
--         , testPutScalingPolicyResponse $
--             putScalingPolicyResponse
--
--         , testRegisterScalableTargetResponse $
--             registerScalableTargetResponse
--
--         , testDescribeScalingPoliciesResponse $
--             describeScalingPoliciesResponse
--
--         , testDescribeScalableTargetsResponse $
--             describeScalableTargetsResponse
--
--         , testDescribeScalingActivitiesResponse $
--             describeScalingActivitiesResponse
--
--         , testDeregisterScalableTargetResponse $
--             deregisterScalableTargetResponse
--
--           ]
--     ]

-- Requests

testDeleteScalingPolicy :: DeleteScalingPolicy -> TestTree
testDeleteScalingPolicy = req
    "DeleteScalingPolicy"
    "fixture/DeleteScalingPolicy.yaml"

testPutScalingPolicy :: PutScalingPolicy -> TestTree
testPutScalingPolicy = req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

testRegisterScalableTarget :: RegisterScalableTarget -> TestTree
testRegisterScalableTarget = req
    "RegisterScalableTarget"
    "fixture/RegisterScalableTarget.yaml"

testDescribeScalingPolicies :: DescribeScalingPolicies -> TestTree
testDescribeScalingPolicies = req
    "DescribeScalingPolicies"
    "fixture/DescribeScalingPolicies.yaml"

testDescribeScalableTargets :: DescribeScalableTargets -> TestTree
testDescribeScalableTargets = req
    "DescribeScalableTargets"
    "fixture/DescribeScalableTargets.yaml"

testDescribeScalingActivities :: DescribeScalingActivities -> TestTree
testDescribeScalingActivities = req
    "DescribeScalingActivities"
    "fixture/DescribeScalingActivities.yaml"

testDeregisterScalableTarget :: DeregisterScalableTarget -> TestTree
testDeregisterScalableTarget = req
    "DeregisterScalableTarget"
    "fixture/DeregisterScalableTarget.yaml"

-- Responses

testDeleteScalingPolicyResponse :: DeleteScalingPolicyResponse -> TestTree
testDeleteScalingPolicyResponse = res
    "DeleteScalingPolicyResponse"
    "fixture/DeleteScalingPolicyResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy DeleteScalingPolicy)

testPutScalingPolicyResponse :: PutScalingPolicyResponse -> TestTree
testPutScalingPolicyResponse = res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy PutScalingPolicy)

testRegisterScalableTargetResponse :: RegisterScalableTargetResponse -> TestTree
testRegisterScalableTargetResponse = res
    "RegisterScalableTargetResponse"
    "fixture/RegisterScalableTargetResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy RegisterScalableTarget)

testDescribeScalingPoliciesResponse :: DescribeScalingPoliciesResponse -> TestTree
testDescribeScalingPoliciesResponse = res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy DescribeScalingPolicies)

testDescribeScalableTargetsResponse :: DescribeScalableTargetsResponse -> TestTree
testDescribeScalableTargetsResponse = res
    "DescribeScalableTargetsResponse"
    "fixture/DescribeScalableTargetsResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy DescribeScalableTargets)

testDescribeScalingActivitiesResponse :: DescribeScalingActivitiesResponse -> TestTree
testDescribeScalingActivitiesResponse = res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy DescribeScalingActivities)

testDeregisterScalableTargetResponse :: DeregisterScalableTargetResponse -> TestTree
testDeregisterScalableTargetResponse = res
    "DeregisterScalableTargetResponse"
    "fixture/DeregisterScalableTargetResponse.proto"
    applicationAutoScaling
    (Proxy :: Proxy DeregisterScalableTarget)
