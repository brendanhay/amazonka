{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EventBridge
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.EventBridge where

import Data.Proxy
import Network.AWS.EventBridge
import Test.AWS.EventBridge.Internal
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
--         [ requestRemoveTargets $
--             removeTargets
--
--         , requestDeleteRule $
--             deleteRule
--
--         , requestListPartnerEventSourceAccounts $
--             listPartnerEventSourceAccounts
--
--         , requestListRules $
--             listRules
--
--         , requestPutRule $
--             putRule
--
--         , requestDisableRule $
--             disableRule
--
--         , requestPutPermission $
--             putPermission
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestListTargetsByRule $
--             listTargetsByRule
--
--         , requestRemovePermission $
--             removePermission
--
--         , requestActivateEventSource $
--             activateEventSource
--
--         , requestPutPartnerEvents $
--             putPartnerEvents
--
--         , requestDescribeRule $
--             describeRule
--
--         , requestDeletePartnerEventSource $
--             deletePartnerEventSource
--
--         , requestListEventBuses $
--             listEventBuses
--
--         , requestCreateEventBus $
--             createEventBus
--
--         , requestDescribeEventSource $
--             describeEventSource
--
--         , requestEnableRule $
--             enableRule
--
--         , requestListRuleNamesByTarget $
--             listRuleNamesByTarget
--
--         , requestTestEventPattern $
--             testEventPattern
--
--         , requestDescribePartnerEventSource $
--             describePartnerEventSource
--
--         , requestDescribeEventBus $
--             describeEventBus
--
--         , requestListEventSources $
--             listEventSources
--
--         , requestTagResource $
--             tagResource
--
--         , requestCreatePartnerEventSource $
--             createPartnerEventSource
--
--         , requestPutTargets $
--             putTargets
--
--         , requestUntagResource $
--             untagResource
--
--         , requestPutEvents $
--             putEvents
--
--         , requestListPartnerEventSources $
--             listPartnerEventSources
--
--         , requestDeactivateEventSource $
--             deactivateEventSource
--
--         , requestDeleteEventBus $
--             deleteEventBus
--
--           ]

--     , testGroup "response"
--         [ responseRemoveTargets $
--             removeTargetsResponse
--
--         , responseDeleteRule $
--             deleteRuleResponse
--
--         , responseListPartnerEventSourceAccounts $
--             listPartnerEventSourceAccountsResponse
--
--         , responseListRules $
--             listRulesResponse
--
--         , responsePutRule $
--             putRuleResponse
--
--         , responseDisableRule $
--             disableRuleResponse
--
--         , responsePutPermission $
--             putPermissionResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseListTargetsByRule $
--             listTargetsByRuleResponse
--
--         , responseRemovePermission $
--             removePermissionResponse
--
--         , responseActivateEventSource $
--             activateEventSourceResponse
--
--         , responsePutPartnerEvents $
--             putPartnerEventsResponse
--
--         , responseDescribeRule $
--             describeRuleResponse
--
--         , responseDeletePartnerEventSource $
--             deletePartnerEventSourceResponse
--
--         , responseListEventBuses $
--             listEventBusesResponse
--
--         , responseCreateEventBus $
--             createEventBusResponse
--
--         , responseDescribeEventSource $
--             describeEventSourceResponse
--
--         , responseEnableRule $
--             enableRuleResponse
--
--         , responseListRuleNamesByTarget $
--             listRuleNamesByTargetResponse
--
--         , responseTestEventPattern $
--             testEventPatternResponse
--
--         , responseDescribePartnerEventSource $
--             describePartnerEventSourceResponse
--
--         , responseDescribeEventBus $
--             describeEventBusResponse
--
--         , responseListEventSources $
--             listEventSourcesResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseCreatePartnerEventSource $
--             createPartnerEventSourceResponse
--
--         , responsePutTargets $
--             putTargetsResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responsePutEvents $
--             putEventsResponse
--
--         , responseListPartnerEventSources $
--             listPartnerEventSourcesResponse
--
--         , responseDeactivateEventSource $
--             deactivateEventSourceResponse
--
--         , responseDeleteEventBus $
--             deleteEventBusResponse
--
--           ]
--     ]

-- Requests

requestRemoveTargets :: RemoveTargets -> TestTree
requestRemoveTargets = req
    "RemoveTargets"
    "fixture/RemoveTargets.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule = req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestListPartnerEventSourceAccounts :: ListPartnerEventSourceAccounts -> TestTree
requestListPartnerEventSourceAccounts = req
    "ListPartnerEventSourceAccounts"
    "fixture/ListPartnerEventSourceAccounts.yaml"

requestListRules :: ListRules -> TestTree
requestListRules = req
    "ListRules"
    "fixture/ListRules.yaml"

requestPutRule :: PutRule -> TestTree
requestPutRule = req
    "PutRule"
    "fixture/PutRule.yaml"

requestDisableRule :: DisableRule -> TestTree
requestDisableRule = req
    "DisableRule"
    "fixture/DisableRule.yaml"

requestPutPermission :: PutPermission -> TestTree
requestPutPermission = req
    "PutPermission"
    "fixture/PutPermission.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTargetsByRule :: ListTargetsByRule -> TestTree
requestListTargetsByRule = req
    "ListTargetsByRule"
    "fixture/ListTargetsByRule.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission = req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestActivateEventSource :: ActivateEventSource -> TestTree
requestActivateEventSource = req
    "ActivateEventSource"
    "fixture/ActivateEventSource.yaml"

requestPutPartnerEvents :: PutPartnerEvents -> TestTree
requestPutPartnerEvents = req
    "PutPartnerEvents"
    "fixture/PutPartnerEvents.yaml"

requestDescribeRule :: DescribeRule -> TestTree
requestDescribeRule = req
    "DescribeRule"
    "fixture/DescribeRule.yaml"

requestDeletePartnerEventSource :: DeletePartnerEventSource -> TestTree
requestDeletePartnerEventSource = req
    "DeletePartnerEventSource"
    "fixture/DeletePartnerEventSource.yaml"

requestListEventBuses :: ListEventBuses -> TestTree
requestListEventBuses = req
    "ListEventBuses"
    "fixture/ListEventBuses.yaml"

requestCreateEventBus :: CreateEventBus -> TestTree
requestCreateEventBus = req
    "CreateEventBus"
    "fixture/CreateEventBus.yaml"

requestDescribeEventSource :: DescribeEventSource -> TestTree
requestDescribeEventSource = req
    "DescribeEventSource"
    "fixture/DescribeEventSource.yaml"

requestEnableRule :: EnableRule -> TestTree
requestEnableRule = req
    "EnableRule"
    "fixture/EnableRule.yaml"

requestListRuleNamesByTarget :: ListRuleNamesByTarget -> TestTree
requestListRuleNamesByTarget = req
    "ListRuleNamesByTarget"
    "fixture/ListRuleNamesByTarget.yaml"

requestTestEventPattern :: TestEventPattern -> TestTree
requestTestEventPattern = req
    "TestEventPattern"
    "fixture/TestEventPattern.yaml"

requestDescribePartnerEventSource :: DescribePartnerEventSource -> TestTree
requestDescribePartnerEventSource = req
    "DescribePartnerEventSource"
    "fixture/DescribePartnerEventSource.yaml"

requestDescribeEventBus :: DescribeEventBus -> TestTree
requestDescribeEventBus = req
    "DescribeEventBus"
    "fixture/DescribeEventBus.yaml"

requestListEventSources :: ListEventSources -> TestTree
requestListEventSources = req
    "ListEventSources"
    "fixture/ListEventSources.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreatePartnerEventSource :: CreatePartnerEventSource -> TestTree
requestCreatePartnerEventSource = req
    "CreatePartnerEventSource"
    "fixture/CreatePartnerEventSource.yaml"

requestPutTargets :: PutTargets -> TestTree
requestPutTargets = req
    "PutTargets"
    "fixture/PutTargets.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents = req
    "PutEvents"
    "fixture/PutEvents.yaml"

requestListPartnerEventSources :: ListPartnerEventSources -> TestTree
requestListPartnerEventSources = req
    "ListPartnerEventSources"
    "fixture/ListPartnerEventSources.yaml"

requestDeactivateEventSource :: DeactivateEventSource -> TestTree
requestDeactivateEventSource = req
    "DeactivateEventSource"
    "fixture/DeactivateEventSource.yaml"

requestDeleteEventBus :: DeleteEventBus -> TestTree
requestDeleteEventBus = req
    "DeleteEventBus"
    "fixture/DeleteEventBus.yaml"

-- Responses

responseRemoveTargets :: RemoveTargetsResponse -> TestTree
responseRemoveTargets = res
    "RemoveTargetsResponse"
    "fixture/RemoveTargetsResponse.proto"
    eventBridge
    (Proxy :: Proxy RemoveTargets)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule = res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    eventBridge
    (Proxy :: Proxy DeleteRule)

responseListPartnerEventSourceAccounts :: ListPartnerEventSourceAccountsResponse -> TestTree
responseListPartnerEventSourceAccounts = res
    "ListPartnerEventSourceAccountsResponse"
    "fixture/ListPartnerEventSourceAccountsResponse.proto"
    eventBridge
    (Proxy :: Proxy ListPartnerEventSourceAccounts)

responseListRules :: ListRulesResponse -> TestTree
responseListRules = res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    eventBridge
    (Proxy :: Proxy ListRules)

responsePutRule :: PutRuleResponse -> TestTree
responsePutRule = res
    "PutRuleResponse"
    "fixture/PutRuleResponse.proto"
    eventBridge
    (Proxy :: Proxy PutRule)

responseDisableRule :: DisableRuleResponse -> TestTree
responseDisableRule = res
    "DisableRuleResponse"
    "fixture/DisableRuleResponse.proto"
    eventBridge
    (Proxy :: Proxy DisableRule)

responsePutPermission :: PutPermissionResponse -> TestTree
responsePutPermission = res
    "PutPermissionResponse"
    "fixture/PutPermissionResponse.proto"
    eventBridge
    (Proxy :: Proxy PutPermission)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    eventBridge
    (Proxy :: Proxy ListTagsForResource)

responseListTargetsByRule :: ListTargetsByRuleResponse -> TestTree
responseListTargetsByRule = res
    "ListTargetsByRuleResponse"
    "fixture/ListTargetsByRuleResponse.proto"
    eventBridge
    (Proxy :: Proxy ListTargetsByRule)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission = res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    eventBridge
    (Proxy :: Proxy RemovePermission)

responseActivateEventSource :: ActivateEventSourceResponse -> TestTree
responseActivateEventSource = res
    "ActivateEventSourceResponse"
    "fixture/ActivateEventSourceResponse.proto"
    eventBridge
    (Proxy :: Proxy ActivateEventSource)

responsePutPartnerEvents :: PutPartnerEventsResponse -> TestTree
responsePutPartnerEvents = res
    "PutPartnerEventsResponse"
    "fixture/PutPartnerEventsResponse.proto"
    eventBridge
    (Proxy :: Proxy PutPartnerEvents)

responseDescribeRule :: DescribeRuleResponse -> TestTree
responseDescribeRule = res
    "DescribeRuleResponse"
    "fixture/DescribeRuleResponse.proto"
    eventBridge
    (Proxy :: Proxy DescribeRule)

responseDeletePartnerEventSource :: DeletePartnerEventSourceResponse -> TestTree
responseDeletePartnerEventSource = res
    "DeletePartnerEventSourceResponse"
    "fixture/DeletePartnerEventSourceResponse.proto"
    eventBridge
    (Proxy :: Proxy DeletePartnerEventSource)

responseListEventBuses :: ListEventBusesResponse -> TestTree
responseListEventBuses = res
    "ListEventBusesResponse"
    "fixture/ListEventBusesResponse.proto"
    eventBridge
    (Proxy :: Proxy ListEventBuses)

responseCreateEventBus :: CreateEventBusResponse -> TestTree
responseCreateEventBus = res
    "CreateEventBusResponse"
    "fixture/CreateEventBusResponse.proto"
    eventBridge
    (Proxy :: Proxy CreateEventBus)

responseDescribeEventSource :: DescribeEventSourceResponse -> TestTree
responseDescribeEventSource = res
    "DescribeEventSourceResponse"
    "fixture/DescribeEventSourceResponse.proto"
    eventBridge
    (Proxy :: Proxy DescribeEventSource)

responseEnableRule :: EnableRuleResponse -> TestTree
responseEnableRule = res
    "EnableRuleResponse"
    "fixture/EnableRuleResponse.proto"
    eventBridge
    (Proxy :: Proxy EnableRule)

responseListRuleNamesByTarget :: ListRuleNamesByTargetResponse -> TestTree
responseListRuleNamesByTarget = res
    "ListRuleNamesByTargetResponse"
    "fixture/ListRuleNamesByTargetResponse.proto"
    eventBridge
    (Proxy :: Proxy ListRuleNamesByTarget)

responseTestEventPattern :: TestEventPatternResponse -> TestTree
responseTestEventPattern = res
    "TestEventPatternResponse"
    "fixture/TestEventPatternResponse.proto"
    eventBridge
    (Proxy :: Proxy TestEventPattern)

responseDescribePartnerEventSource :: DescribePartnerEventSourceResponse -> TestTree
responseDescribePartnerEventSource = res
    "DescribePartnerEventSourceResponse"
    "fixture/DescribePartnerEventSourceResponse.proto"
    eventBridge
    (Proxy :: Proxy DescribePartnerEventSource)

responseDescribeEventBus :: DescribeEventBusResponse -> TestTree
responseDescribeEventBus = res
    "DescribeEventBusResponse"
    "fixture/DescribeEventBusResponse.proto"
    eventBridge
    (Proxy :: Proxy DescribeEventBus)

responseListEventSources :: ListEventSourcesResponse -> TestTree
responseListEventSources = res
    "ListEventSourcesResponse"
    "fixture/ListEventSourcesResponse.proto"
    eventBridge
    (Proxy :: Proxy ListEventSources)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    eventBridge
    (Proxy :: Proxy TagResource)

responseCreatePartnerEventSource :: CreatePartnerEventSourceResponse -> TestTree
responseCreatePartnerEventSource = res
    "CreatePartnerEventSourceResponse"
    "fixture/CreatePartnerEventSourceResponse.proto"
    eventBridge
    (Proxy :: Proxy CreatePartnerEventSource)

responsePutTargets :: PutTargetsResponse -> TestTree
responsePutTargets = res
    "PutTargetsResponse"
    "fixture/PutTargetsResponse.proto"
    eventBridge
    (Proxy :: Proxy PutTargets)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    eventBridge
    (Proxy :: Proxy UntagResource)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents = res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    eventBridge
    (Proxy :: Proxy PutEvents)

responseListPartnerEventSources :: ListPartnerEventSourcesResponse -> TestTree
responseListPartnerEventSources = res
    "ListPartnerEventSourcesResponse"
    "fixture/ListPartnerEventSourcesResponse.proto"
    eventBridge
    (Proxy :: Proxy ListPartnerEventSources)

responseDeactivateEventSource :: DeactivateEventSourceResponse -> TestTree
responseDeactivateEventSource = res
    "DeactivateEventSourceResponse"
    "fixture/DeactivateEventSourceResponse.proto"
    eventBridge
    (Proxy :: Proxy DeactivateEventSource)

responseDeleteEventBus :: DeleteEventBusResponse -> TestTree
responseDeleteEventBus = res
    "DeleteEventBusResponse"
    "fixture/DeleteEventBusResponse.proto"
    eventBridge
    (Proxy :: Proxy DeleteEventBus)
