{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ApplicationAutoScaling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ApplicationAutoScaling where

import Amazonka.ApplicationAutoScaling
import qualified Data.Proxy as Proxy
import Test.Amazonka.ApplicationAutoScaling.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
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
--         , requestDeleteScheduledAction $
--             newDeleteScheduledAction
--
--         , requestDeregisterScalableTarget $
--             newDeregisterScalableTarget
--
--         , requestDescribeScalableTargets $
--             newDescribeScalableTargets
--
--         , requestDescribeScalingActivities $
--             newDescribeScalingActivities
--
--         , requestDescribeScalingPolicies $
--             newDescribeScalingPolicies
--
--         , requestDescribeScheduledActions $
--             newDescribeScheduledActions
--
--         , requestPutScalingPolicy $
--             newPutScalingPolicy
--
--         , requestPutScheduledAction $
--             newPutScheduledAction
--
--         , requestRegisterScalableTarget $
--             newRegisterScalableTarget
--
--           ]

--     , testGroup "response"
--         [ responseDeleteScalingPolicy $
--             newDeleteScalingPolicyResponse
--
--         , responseDeleteScheduledAction $
--             newDeleteScheduledActionResponse
--
--         , responseDeregisterScalableTarget $
--             newDeregisterScalableTargetResponse
--
--         , responseDescribeScalableTargets $
--             newDescribeScalableTargetsResponse
--
--         , responseDescribeScalingActivities $
--             newDescribeScalingActivitiesResponse
--
--         , responseDescribeScalingPolicies $
--             newDescribeScalingPoliciesResponse
--
--         , responseDescribeScheduledActions $
--             newDescribeScheduledActionsResponse
--
--         , responsePutScalingPolicy $
--             newPutScalingPolicyResponse
--
--         , responsePutScheduledAction $
--             newPutScheduledActionResponse
--
--         , responseRegisterScalableTarget $
--             newRegisterScalableTargetResponse
--
--           ]
--     ]

-- Requests

requestDeleteScalingPolicy :: DeleteScalingPolicy -> TestTree
requestDeleteScalingPolicy =
  req
    "DeleteScalingPolicy"
    "fixture/DeleteScalingPolicy.yaml"

requestDeleteScheduledAction :: DeleteScheduledAction -> TestTree
requestDeleteScheduledAction =
  req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

requestDeregisterScalableTarget :: DeregisterScalableTarget -> TestTree
requestDeregisterScalableTarget =
  req
    "DeregisterScalableTarget"
    "fixture/DeregisterScalableTarget.yaml"

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

requestDescribeScalingPolicies :: DescribeScalingPolicies -> TestTree
requestDescribeScalingPolicies =
  req
    "DescribeScalingPolicies"
    "fixture/DescribeScalingPolicies.yaml"

requestDescribeScheduledActions :: DescribeScheduledActions -> TestTree
requestDescribeScheduledActions =
  req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions.yaml"

requestPutScalingPolicy :: PutScalingPolicy -> TestTree
requestPutScalingPolicy =
  req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

requestPutScheduledAction :: PutScheduledAction -> TestTree
requestPutScheduledAction =
  req
    "PutScheduledAction"
    "fixture/PutScheduledAction.yaml"

requestRegisterScalableTarget :: RegisterScalableTarget -> TestTree
requestRegisterScalableTarget =
  req
    "RegisterScalableTarget"
    "fixture/RegisterScalableTarget.yaml"

-- Responses

responseDeleteScalingPolicy :: DeleteScalingPolicyResponse -> TestTree
responseDeleteScalingPolicy =
  res
    "DeleteScalingPolicyResponse"
    "fixture/DeleteScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScalingPolicy)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScheduledAction)

responseDeregisterScalableTarget :: DeregisterScalableTargetResponse -> TestTree
responseDeregisterScalableTarget =
  res
    "DeregisterScalableTargetResponse"
    "fixture/DeregisterScalableTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterScalableTarget)

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

responseDescribeScalingPolicies :: DescribeScalingPoliciesResponse -> TestTree
responseDescribeScalingPolicies =
  res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScalingPolicies)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledActions)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy =
  res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutScalingPolicy)

responsePutScheduledAction :: PutScheduledActionResponse -> TestTree
responsePutScheduledAction =
  res
    "PutScheduledActionResponse"
    "fixture/PutScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutScheduledAction)

responseRegisterScalableTarget :: RegisterScalableTargetResponse -> TestTree
responseRegisterScalableTarget =
  res
    "RegisterScalableTargetResponse"
    "fixture/RegisterScalableTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterScalableTarget)
