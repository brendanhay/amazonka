{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ApplicationAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ApplicationAutoScaling where

import Data.Proxy
import Network.AWS.ApplicationAutoScaling
import Test.AWS.ApplicationAutoScaling.Internal
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
--         [ requestDeleteScalingPolicy $
--             mkDeleteScalingPolicy
--
--         , requestPutScalingPolicy $
--             mkPutScalingPolicy
--
--         , requestRegisterScalableTarget $
--             mkRegisterScalableTarget
--
--         , requestDescribeScalingPolicies $
--             mkDescribeScalingPolicies
--
--         , requestPutScheduledAction $
--             mkPutScheduledAction
--
--         , requestDeleteScheduledAction $
--             mkDeleteScheduledAction
--
--         , requestDescribeScheduledActions $
--             mkDescribeScheduledActions
--
--         , requestDescribeScalableTargets $
--             mkDescribeScalableTargets
--
--         , requestDescribeScalingActivities $
--             mkDescribeScalingActivities
--
--         , requestDeregisterScalableTarget $
--             mkDeregisterScalableTarget
--
--           ]

--     , testGroup "response"
--         [ responseDeleteScalingPolicy $
--             mkDeleteScalingPolicyResponse
--
--         , responsePutScalingPolicy $
--             mkPutScalingPolicyResponse
--
--         , responseRegisterScalableTarget $
--             mkRegisterScalableTargetResponse
--
--         , responseDescribeScalingPolicies $
--             mkDescribeScalingPoliciesResponse
--
--         , responsePutScheduledAction $
--             mkPutScheduledActionResponse
--
--         , responseDeleteScheduledAction $
--             mkDeleteScheduledActionResponse
--
--         , responseDescribeScheduledActions $
--             mkDescribeScheduledActionsResponse
--
--         , responseDescribeScalableTargets $
--             mkDescribeScalableTargetsResponse
--
--         , responseDescribeScalingActivities $
--             mkDescribeScalingActivitiesResponse
--
--         , responseDeregisterScalableTarget $
--             mkDeregisterScalableTargetResponse
--
--           ]
--     ]

-- Requests

requestDeleteScalingPolicy :: DeleteScalingPolicy -> TestTree
requestDeleteScalingPolicy =
  req
    "DeleteScalingPolicy"
    "fixture/DeleteScalingPolicy.yaml"

requestPutScalingPolicy :: PutScalingPolicy -> TestTree
requestPutScalingPolicy =
  req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

requestRegisterScalableTarget :: RegisterScalableTarget -> TestTree
requestRegisterScalableTarget =
  req
    "RegisterScalableTarget"
    "fixture/RegisterScalableTarget.yaml"

requestDescribeScalingPolicies :: DescribeScalingPolicies -> TestTree
requestDescribeScalingPolicies =
  req
    "DescribeScalingPolicies"
    "fixture/DescribeScalingPolicies.yaml"

requestPutScheduledAction :: PutScheduledAction -> TestTree
requestPutScheduledAction =
  req
    "PutScheduledAction"
    "fixture/PutScheduledAction.yaml"

requestDeleteScheduledAction :: DeleteScheduledAction -> TestTree
requestDeleteScheduledAction =
  req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

requestDescribeScheduledActions :: DescribeScheduledActions -> TestTree
requestDescribeScheduledActions =
  req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions.yaml"

requestDescribeScalableTargets :: DescribeScalableTargets -> TestTree
requestDescribeScalableTargets =
  req
    "DescribeScalableTargets"
    "fixture/DescribeScalableTargets.yaml"

requestDescribeScalingActivities :: DescribeScalingActivities -> TestTree
requestDescribeScalingActivities =
  req
    "DescribeScalingActivities"
    "fixture/DescribeScalingActivities.yaml"

requestDeregisterScalableTarget :: DeregisterScalableTarget -> TestTree
requestDeregisterScalableTarget =
  req
    "DeregisterScalableTarget"
    "fixture/DeregisterScalableTarget.yaml"

-- Responses

responseDeleteScalingPolicy :: DeleteScalingPolicyResponse -> TestTree
responseDeleteScalingPolicy =
  res
    "DeleteScalingPolicyResponse"
    "fixture/DeleteScalingPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteScalingPolicy)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy =
  res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutScalingPolicy)

responseRegisterScalableTarget :: RegisterScalableTargetResponse -> TestTree
responseRegisterScalableTarget =
  res
    "RegisterScalableTargetResponse"
    "fixture/RegisterScalableTargetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterScalableTarget)

responseDescribeScalingPolicies :: DescribeScalingPoliciesResponse -> TestTree
responseDescribeScalingPolicies =
  res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeScalingPolicies)

responsePutScheduledAction :: PutScheduledActionResponse -> TestTree
responsePutScheduledAction =
  res
    "PutScheduledActionResponse"
    "fixture/PutScheduledActionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutScheduledAction)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteScheduledAction)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeScheduledActions)

responseDescribeScalableTargets :: DescribeScalableTargetsResponse -> TestTree
responseDescribeScalableTargets =
  res
    "DescribeScalableTargetsResponse"
    "fixture/DescribeScalableTargetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeScalableTargets)

responseDescribeScalingActivities :: DescribeScalingActivitiesResponse -> TestTree
responseDescribeScalingActivities =
  res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeScalingActivities)

responseDeregisterScalableTarget :: DeregisterScalableTargetResponse -> TestTree
responseDeregisterScalableTarget =
  res
    "DeregisterScalableTargetResponse"
    "fixture/DeregisterScalableTargetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterScalableTarget)
