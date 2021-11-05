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

import qualified Data.Proxy as Proxy
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
--             newDeleteScalingPolicy
--
--         , requestPutScalingPolicy $
--             newPutScalingPolicy
--
--         , requestRegisterScalableTarget $
--             newRegisterScalableTarget
--
--         , requestDescribeScalingPolicies $
--             newDescribeScalingPolicies
--
--         , requestPutScheduledAction $
--             newPutScheduledAction
--
--         , requestDeleteScheduledAction $
--             newDeleteScheduledAction
--
--         , requestDescribeScheduledActions $
--             newDescribeScheduledActions
--
--         , requestDescribeScalableTargets $
--             newDescribeScalableTargets
--
--         , requestDescribeScalingActivities $
--             newDescribeScalingActivities
--
--         , requestDeregisterScalableTarget $
--             newDeregisterScalableTarget
--
--           ]

--     , testGroup "response"
--         [ responseDeleteScalingPolicy $
--             newDeleteScalingPolicyResponse
--
--         , responsePutScalingPolicy $
--             newPutScalingPolicyResponse
--
--         , responseRegisterScalableTarget $
--             newRegisterScalableTargetResponse
--
--         , responseDescribeScalingPolicies $
--             newDescribeScalingPoliciesResponse
--
--         , responsePutScheduledAction $
--             newPutScheduledActionResponse
--
--         , responseDeleteScheduledAction $
--             newDeleteScheduledActionResponse
--
--         , responseDescribeScheduledActions $
--             newDescribeScheduledActionsResponse
--
--         , responseDescribeScalableTargets $
--             newDescribeScalableTargetsResponse
--
--         , responseDescribeScalingActivities $
--             newDescribeScalingActivitiesResponse
--
--         , responseDeregisterScalableTarget $
--             newDeregisterScalableTargetResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScalingPolicy)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy =
  res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutScalingPolicy)

responseRegisterScalableTarget :: RegisterScalableTargetResponse -> TestTree
responseRegisterScalableTarget =
  res
    "RegisterScalableTargetResponse"
    "fixture/RegisterScalableTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterScalableTarget)

responseDescribeScalingPolicies :: DescribeScalingPoliciesResponse -> TestTree
responseDescribeScalingPolicies =
  res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScalingPolicies)

responsePutScheduledAction :: PutScheduledActionResponse -> TestTree
responsePutScheduledAction =
  res
    "PutScheduledActionResponse"
    "fixture/PutScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutScheduledAction)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScheduledAction)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledActions)

responseDescribeScalableTargets :: DescribeScalableTargetsResponse -> TestTree
responseDescribeScalableTargets =
  res
    "DescribeScalableTargetsResponse"
    "fixture/DescribeScalableTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScalableTargets)

responseDescribeScalingActivities :: DescribeScalingActivitiesResponse -> TestTree
responseDescribeScalingActivities =
  res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScalingActivities)

responseDeregisterScalableTarget :: DeregisterScalableTargetResponse -> TestTree
responseDeregisterScalableTarget =
  res
    "DeregisterScalableTargetResponse"
    "fixture/DeregisterScalableTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterScalableTarget)
