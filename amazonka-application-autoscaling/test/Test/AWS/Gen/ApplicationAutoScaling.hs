{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ApplicationAutoScaling
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestRegisterScalableTarget $
--             newRegisterScalableTarget
--
--         , requestPutScalingPolicy $
--             newPutScalingPolicy
--
--         , requestDescribeScheduledActions $
--             newDescribeScheduledActions
--
--         , requestDescribeScalingPolicies $
--             newDescribeScalingPolicies
--
--         , requestDeregisterScalableTarget $
--             newDeregisterScalableTarget
--
--         , requestDeleteScheduledAction $
--             newDeleteScheduledAction
--
--         , requestDescribeScalingActivities $
--             newDescribeScalingActivities
--
--         , requestDescribeScalableTargets $
--             newDescribeScalableTargets
--
--         , requestDeleteScalingPolicy $
--             newDeleteScalingPolicy
--
--         , requestPutScheduledAction $
--             newPutScheduledAction
--
--           ]

--     , testGroup "response"
--         [ responseRegisterScalableTarget $
--             newRegisterScalableTargetResponse
--
--         , responsePutScalingPolicy $
--             newPutScalingPolicyResponse
--
--         , responseDescribeScheduledActions $
--             newDescribeScheduledActionsResponse
--
--         , responseDescribeScalingPolicies $
--             newDescribeScalingPoliciesResponse
--
--         , responseDeregisterScalableTarget $
--             newDeregisterScalableTargetResponse
--
--         , responseDeleteScheduledAction $
--             newDeleteScheduledActionResponse
--
--         , responseDescribeScalingActivities $
--             newDescribeScalingActivitiesResponse
--
--         , responseDescribeScalableTargets $
--             newDescribeScalableTargetsResponse
--
--         , responseDeleteScalingPolicy $
--             newDeleteScalingPolicyResponse
--
--         , responsePutScheduledAction $
--             newPutScheduledActionResponse
--
--           ]
--     ]

-- Requests

requestRegisterScalableTarget :: RegisterScalableTarget -> TestTree
requestRegisterScalableTarget =
  req
    "RegisterScalableTarget"
    "fixture/RegisterScalableTarget.yaml"

requestPutScalingPolicy :: PutScalingPolicy -> TestTree
requestPutScalingPolicy =
  req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

requestDescribeScheduledActions :: DescribeScheduledActions -> TestTree
requestDescribeScheduledActions =
  req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions.yaml"

requestDescribeScalingPolicies :: DescribeScalingPolicies -> TestTree
requestDescribeScalingPolicies =
  req
    "DescribeScalingPolicies"
    "fixture/DescribeScalingPolicies.yaml"

requestDeregisterScalableTarget :: DeregisterScalableTarget -> TestTree
requestDeregisterScalableTarget =
  req
    "DeregisterScalableTarget"
    "fixture/DeregisterScalableTarget.yaml"

requestDeleteScheduledAction :: DeleteScheduledAction -> TestTree
requestDeleteScheduledAction =
  req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

requestDescribeScalingActivities :: DescribeScalingActivities -> TestTree
requestDescribeScalingActivities =
  req
    "DescribeScalingActivities"
    "fixture/DescribeScalingActivities.yaml"

requestDescribeScalableTargets :: DescribeScalableTargets -> TestTree
requestDescribeScalableTargets =
  req
    "DescribeScalableTargets"
    "fixture/DescribeScalableTargets.yaml"

requestDeleteScalingPolicy :: DeleteScalingPolicy -> TestTree
requestDeleteScalingPolicy =
  req
    "DeleteScalingPolicy"
    "fixture/DeleteScalingPolicy.yaml"

requestPutScheduledAction :: PutScheduledAction -> TestTree
requestPutScheduledAction =
  req
    "PutScheduledAction"
    "fixture/PutScheduledAction.yaml"

-- Responses

responseRegisterScalableTarget :: RegisterScalableTargetResponse -> TestTree
responseRegisterScalableTarget =
  res
    "RegisterScalableTargetResponse"
    "fixture/RegisterScalableTargetResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterScalableTarget)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy =
  res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutScalingPolicy)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScheduledActions)

responseDescribeScalingPolicies :: DescribeScalingPoliciesResponse -> TestTree
responseDescribeScalingPolicies =
  res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScalingPolicies)

responseDeregisterScalableTarget :: DeregisterScalableTargetResponse -> TestTree
responseDeregisterScalableTarget =
  res
    "DeregisterScalableTargetResponse"
    "fixture/DeregisterScalableTargetResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterScalableTarget)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteScheduledAction)

responseDescribeScalingActivities :: DescribeScalingActivitiesResponse -> TestTree
responseDescribeScalingActivities =
  res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScalingActivities)

responseDescribeScalableTargets :: DescribeScalableTargetsResponse -> TestTree
responseDescribeScalableTargets =
  res
    "DescribeScalableTargetsResponse"
    "fixture/DescribeScalableTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScalableTargets)

responseDeleteScalingPolicy :: DeleteScalingPolicyResponse -> TestTree
responseDeleteScalingPolicy =
  res
    "DeleteScalingPolicyResponse"
    "fixture/DeleteScalingPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteScalingPolicy)

responsePutScheduledAction :: PutScheduledActionResponse -> TestTree
responsePutScheduledAction =
  res
    "PutScheduledActionResponse"
    "fixture/PutScheduledActionResponse.proto"
    defaultService
    (Proxy :: Proxy PutScheduledAction)
