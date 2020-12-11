{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatchEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CloudWatchEvents where

import Data.Proxy
import Network.AWS.CloudWatchEvents
import Test.AWS.CloudWatchEvents.Internal
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
--             mkRemoveTargets
--
--         , requestDeleteRule $
--             mkDeleteRule
--
--         , requestListPartnerEventSourceAccounts $
--             mkListPartnerEventSourceAccounts
--
--         , requestListRules $
--             mkListRules
--
--         , requestPutRule $
--             mkPutRule
--
--         , requestDisableRule $
--             mkDisableRule
--
--         , requestPutPermission $
--             mkPutPermission
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestListReplays $
--             mkListReplays
--
--         , requestCancelReplay $
--             mkCancelReplay
--
--         , requestListTargetsByRule $
--             mkListTargetsByRule
--
--         , requestRemovePermission $
--             mkRemovePermission
--
--         , requestActivateEventSource $
--             mkActivateEventSource
--
--         , requestPutPartnerEvents $
--             mkPutPartnerEvents
--
--         , requestDescribeRule $
--             mkDescribeRule
--
--         , requestListArchives $
--             mkListArchives
--
--         , requestStartReplay $
--             mkStartReplay
--
--         , requestDeletePartnerEventSource $
--             mkDeletePartnerEventSource
--
--         , requestDescribeReplay $
--             mkDescribeReplay
--
--         , requestListEventBuses $
--             mkListEventBuses
--
--         , requestCreateEventBus $
--             mkCreateEventBus
--
--         , requestDescribeEventSource $
--             mkDescribeEventSource
--
--         , requestDescribeArchive $
--             mkDescribeArchive
--
--         , requestEnableRule $
--             mkEnableRule
--
--         , requestListRuleNamesByTarget $
--             mkListRuleNamesByTarget
--
--         , requestTestEventPattern $
--             mkTestEventPattern
--
--         , requestDescribePartnerEventSource $
--             mkDescribePartnerEventSource
--
--         , requestDescribeEventBus $
--             mkDescribeEventBus
--
--         , requestListEventSources $
--             mkListEventSources
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestCreatePartnerEventSource $
--             mkCreatePartnerEventSource
--
--         , requestPutTargets $
--             mkPutTargets
--
--         , requestUpdateArchive $
--             mkUpdateArchive
--
--         , requestDeleteArchive $
--             mkDeleteArchive
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestPutEvents $
--             mkPutEvents
--
--         , requestListPartnerEventSources $
--             mkListPartnerEventSources
--
--         , requestCreateArchive $
--             mkCreateArchive
--
--         , requestDeactivateEventSource $
--             mkDeactivateEventSource
--
--         , requestDeleteEventBus $
--             mkDeleteEventBus
--
--           ]

--     , testGroup "response"
--         [ responseRemoveTargets $
--             mkRemoveTargetsResponse
--
--         , responseDeleteRule $
--             mkDeleteRuleResponse
--
--         , responseListPartnerEventSourceAccounts $
--             mkListPartnerEventSourceAccountsResponse
--
--         , responseListRules $
--             mkListRulesResponse
--
--         , responsePutRule $
--             mkPutRuleResponse
--
--         , responseDisableRule $
--             mkDisableRuleResponse
--
--         , responsePutPermission $
--             mkPutPermissionResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseListReplays $
--             mkListReplaysResponse
--
--         , responseCancelReplay $
--             mkCancelReplayResponse
--
--         , responseListTargetsByRule $
--             mkListTargetsByRuleResponse
--
--         , responseRemovePermission $
--             mkRemovePermissionResponse
--
--         , responseActivateEventSource $
--             mkActivateEventSourceResponse
--
--         , responsePutPartnerEvents $
--             mkPutPartnerEventsResponse
--
--         , responseDescribeRule $
--             mkDescribeRuleResponse
--
--         , responseListArchives $
--             mkListArchivesResponse
--
--         , responseStartReplay $
--             mkStartReplayResponse
--
--         , responseDeletePartnerEventSource $
--             mkDeletePartnerEventSourceResponse
--
--         , responseDescribeReplay $
--             mkDescribeReplayResponse
--
--         , responseListEventBuses $
--             mkListEventBusesResponse
--
--         , responseCreateEventBus $
--             mkCreateEventBusResponse
--
--         , responseDescribeEventSource $
--             mkDescribeEventSourceResponse
--
--         , responseDescribeArchive $
--             mkDescribeArchiveResponse
--
--         , responseEnableRule $
--             mkEnableRuleResponse
--
--         , responseListRuleNamesByTarget $
--             mkListRuleNamesByTargetResponse
--
--         , responseTestEventPattern $
--             mkTestEventPatternResponse
--
--         , responseDescribePartnerEventSource $
--             mkDescribePartnerEventSourceResponse
--
--         , responseDescribeEventBus $
--             mkDescribeEventBusResponse
--
--         , responseListEventSources $
--             mkListEventSourcesResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseCreatePartnerEventSource $
--             mkCreatePartnerEventSourceResponse
--
--         , responsePutTargets $
--             mkPutTargetsResponse
--
--         , responseUpdateArchive $
--             mkUpdateArchiveResponse
--
--         , responseDeleteArchive $
--             mkDeleteArchiveResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responsePutEvents $
--             mkPutEventsResponse
--
--         , responseListPartnerEventSources $
--             mkListPartnerEventSourcesResponse
--
--         , responseCreateArchive $
--             mkCreateArchiveResponse
--
--         , responseDeactivateEventSource $
--             mkDeactivateEventSourceResponse
--
--         , responseDeleteEventBus $
--             mkDeleteEventBusResponse
--
--           ]
--     ]

-- Requests

requestRemoveTargets :: RemoveTargets -> TestTree
requestRemoveTargets =
  req
    "RemoveTargets"
    "fixture/RemoveTargets.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestListPartnerEventSourceAccounts :: ListPartnerEventSourceAccounts -> TestTree
requestListPartnerEventSourceAccounts =
  req
    "ListPartnerEventSourceAccounts"
    "fixture/ListPartnerEventSourceAccounts.yaml"

requestListRules :: ListRules -> TestTree
requestListRules =
  req
    "ListRules"
    "fixture/ListRules.yaml"

requestPutRule :: PutRule -> TestTree
requestPutRule =
  req
    "PutRule"
    "fixture/PutRule.yaml"

requestDisableRule :: DisableRule -> TestTree
requestDisableRule =
  req
    "DisableRule"
    "fixture/DisableRule.yaml"

requestPutPermission :: PutPermission -> TestTree
requestPutPermission =
  req
    "PutPermission"
    "fixture/PutPermission.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListReplays :: ListReplays -> TestTree
requestListReplays =
  req
    "ListReplays"
    "fixture/ListReplays.yaml"

requestCancelReplay :: CancelReplay -> TestTree
requestCancelReplay =
  req
    "CancelReplay"
    "fixture/CancelReplay.yaml"

requestListTargetsByRule :: ListTargetsByRule -> TestTree
requestListTargetsByRule =
  req
    "ListTargetsByRule"
    "fixture/ListTargetsByRule.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestActivateEventSource :: ActivateEventSource -> TestTree
requestActivateEventSource =
  req
    "ActivateEventSource"
    "fixture/ActivateEventSource.yaml"

requestPutPartnerEvents :: PutPartnerEvents -> TestTree
requestPutPartnerEvents =
  req
    "PutPartnerEvents"
    "fixture/PutPartnerEvents.yaml"

requestDescribeRule :: DescribeRule -> TestTree
requestDescribeRule =
  req
    "DescribeRule"
    "fixture/DescribeRule.yaml"

requestListArchives :: ListArchives -> TestTree
requestListArchives =
  req
    "ListArchives"
    "fixture/ListArchives.yaml"

requestStartReplay :: StartReplay -> TestTree
requestStartReplay =
  req
    "StartReplay"
    "fixture/StartReplay.yaml"

requestDeletePartnerEventSource :: DeletePartnerEventSource -> TestTree
requestDeletePartnerEventSource =
  req
    "DeletePartnerEventSource"
    "fixture/DeletePartnerEventSource.yaml"

requestDescribeReplay :: DescribeReplay -> TestTree
requestDescribeReplay =
  req
    "DescribeReplay"
    "fixture/DescribeReplay.yaml"

requestListEventBuses :: ListEventBuses -> TestTree
requestListEventBuses =
  req
    "ListEventBuses"
    "fixture/ListEventBuses.yaml"

requestCreateEventBus :: CreateEventBus -> TestTree
requestCreateEventBus =
  req
    "CreateEventBus"
    "fixture/CreateEventBus.yaml"

requestDescribeEventSource :: DescribeEventSource -> TestTree
requestDescribeEventSource =
  req
    "DescribeEventSource"
    "fixture/DescribeEventSource.yaml"

requestDescribeArchive :: DescribeArchive -> TestTree
requestDescribeArchive =
  req
    "DescribeArchive"
    "fixture/DescribeArchive.yaml"

requestEnableRule :: EnableRule -> TestTree
requestEnableRule =
  req
    "EnableRule"
    "fixture/EnableRule.yaml"

requestListRuleNamesByTarget :: ListRuleNamesByTarget -> TestTree
requestListRuleNamesByTarget =
  req
    "ListRuleNamesByTarget"
    "fixture/ListRuleNamesByTarget.yaml"

requestTestEventPattern :: TestEventPattern -> TestTree
requestTestEventPattern =
  req
    "TestEventPattern"
    "fixture/TestEventPattern.yaml"

requestDescribePartnerEventSource :: DescribePartnerEventSource -> TestTree
requestDescribePartnerEventSource =
  req
    "DescribePartnerEventSource"
    "fixture/DescribePartnerEventSource.yaml"

requestDescribeEventBus :: DescribeEventBus -> TestTree
requestDescribeEventBus =
  req
    "DescribeEventBus"
    "fixture/DescribeEventBus.yaml"

requestListEventSources :: ListEventSources -> TestTree
requestListEventSources =
  req
    "ListEventSources"
    "fixture/ListEventSources.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreatePartnerEventSource :: CreatePartnerEventSource -> TestTree
requestCreatePartnerEventSource =
  req
    "CreatePartnerEventSource"
    "fixture/CreatePartnerEventSource.yaml"

requestPutTargets :: PutTargets -> TestTree
requestPutTargets =
  req
    "PutTargets"
    "fixture/PutTargets.yaml"

requestUpdateArchive :: UpdateArchive -> TestTree
requestUpdateArchive =
  req
    "UpdateArchive"
    "fixture/UpdateArchive.yaml"

requestDeleteArchive :: DeleteArchive -> TestTree
requestDeleteArchive =
  req
    "DeleteArchive"
    "fixture/DeleteArchive.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents =
  req
    "PutEvents"
    "fixture/PutEvents.yaml"

requestListPartnerEventSources :: ListPartnerEventSources -> TestTree
requestListPartnerEventSources =
  req
    "ListPartnerEventSources"
    "fixture/ListPartnerEventSources.yaml"

requestCreateArchive :: CreateArchive -> TestTree
requestCreateArchive =
  req
    "CreateArchive"
    "fixture/CreateArchive.yaml"

requestDeactivateEventSource :: DeactivateEventSource -> TestTree
requestDeactivateEventSource =
  req
    "DeactivateEventSource"
    "fixture/DeactivateEventSource.yaml"

requestDeleteEventBus :: DeleteEventBus -> TestTree
requestDeleteEventBus =
  req
    "DeleteEventBus"
    "fixture/DeleteEventBus.yaml"

-- Responses

responseRemoveTargets :: RemoveTargetsResponse -> TestTree
responseRemoveTargets =
  res
    "RemoveTargetsResponse"
    "fixture/RemoveTargetsResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy RemoveTargets)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy DeleteRule)

responseListPartnerEventSourceAccounts :: ListPartnerEventSourceAccountsResponse -> TestTree
responseListPartnerEventSourceAccounts =
  res
    "ListPartnerEventSourceAccountsResponse"
    "fixture/ListPartnerEventSourceAccountsResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy ListPartnerEventSourceAccounts)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy ListRules)

responsePutRule :: PutRuleResponse -> TestTree
responsePutRule =
  res
    "PutRuleResponse"
    "fixture/PutRuleResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy PutRule)

responseDisableRule :: DisableRuleResponse -> TestTree
responseDisableRule =
  res
    "DisableRuleResponse"
    "fixture/DisableRuleResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy DisableRule)

responsePutPermission :: PutPermissionResponse -> TestTree
responsePutPermission =
  res
    "PutPermissionResponse"
    "fixture/PutPermissionResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy PutPermission)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy ListTagsForResource)

responseListReplays :: ListReplaysResponse -> TestTree
responseListReplays =
  res
    "ListReplaysResponse"
    "fixture/ListReplaysResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy ListReplays)

responseCancelReplay :: CancelReplayResponse -> TestTree
responseCancelReplay =
  res
    "CancelReplayResponse"
    "fixture/CancelReplayResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy CancelReplay)

responseListTargetsByRule :: ListTargetsByRuleResponse -> TestTree
responseListTargetsByRule =
  res
    "ListTargetsByRuleResponse"
    "fixture/ListTargetsByRuleResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy ListTargetsByRule)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy RemovePermission)

responseActivateEventSource :: ActivateEventSourceResponse -> TestTree
responseActivateEventSource =
  res
    "ActivateEventSourceResponse"
    "fixture/ActivateEventSourceResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy ActivateEventSource)

responsePutPartnerEvents :: PutPartnerEventsResponse -> TestTree
responsePutPartnerEvents =
  res
    "PutPartnerEventsResponse"
    "fixture/PutPartnerEventsResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy PutPartnerEvents)

responseDescribeRule :: DescribeRuleResponse -> TestTree
responseDescribeRule =
  res
    "DescribeRuleResponse"
    "fixture/DescribeRuleResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy DescribeRule)

responseListArchives :: ListArchivesResponse -> TestTree
responseListArchives =
  res
    "ListArchivesResponse"
    "fixture/ListArchivesResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy ListArchives)

responseStartReplay :: StartReplayResponse -> TestTree
responseStartReplay =
  res
    "StartReplayResponse"
    "fixture/StartReplayResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy StartReplay)

responseDeletePartnerEventSource :: DeletePartnerEventSourceResponse -> TestTree
responseDeletePartnerEventSource =
  res
    "DeletePartnerEventSourceResponse"
    "fixture/DeletePartnerEventSourceResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy DeletePartnerEventSource)

responseDescribeReplay :: DescribeReplayResponse -> TestTree
responseDescribeReplay =
  res
    "DescribeReplayResponse"
    "fixture/DescribeReplayResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy DescribeReplay)

responseListEventBuses :: ListEventBusesResponse -> TestTree
responseListEventBuses =
  res
    "ListEventBusesResponse"
    "fixture/ListEventBusesResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy ListEventBuses)

responseCreateEventBus :: CreateEventBusResponse -> TestTree
responseCreateEventBus =
  res
    "CreateEventBusResponse"
    "fixture/CreateEventBusResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy CreateEventBus)

responseDescribeEventSource :: DescribeEventSourceResponse -> TestTree
responseDescribeEventSource =
  res
    "DescribeEventSourceResponse"
    "fixture/DescribeEventSourceResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy DescribeEventSource)

responseDescribeArchive :: DescribeArchiveResponse -> TestTree
responseDescribeArchive =
  res
    "DescribeArchiveResponse"
    "fixture/DescribeArchiveResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy DescribeArchive)

responseEnableRule :: EnableRuleResponse -> TestTree
responseEnableRule =
  res
    "EnableRuleResponse"
    "fixture/EnableRuleResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy EnableRule)

responseListRuleNamesByTarget :: ListRuleNamesByTargetResponse -> TestTree
responseListRuleNamesByTarget =
  res
    "ListRuleNamesByTargetResponse"
    "fixture/ListRuleNamesByTargetResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy ListRuleNamesByTarget)

responseTestEventPattern :: TestEventPatternResponse -> TestTree
responseTestEventPattern =
  res
    "TestEventPatternResponse"
    "fixture/TestEventPatternResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy TestEventPattern)

responseDescribePartnerEventSource :: DescribePartnerEventSourceResponse -> TestTree
responseDescribePartnerEventSource =
  res
    "DescribePartnerEventSourceResponse"
    "fixture/DescribePartnerEventSourceResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy DescribePartnerEventSource)

responseDescribeEventBus :: DescribeEventBusResponse -> TestTree
responseDescribeEventBus =
  res
    "DescribeEventBusResponse"
    "fixture/DescribeEventBusResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy DescribeEventBus)

responseListEventSources :: ListEventSourcesResponse -> TestTree
responseListEventSources =
  res
    "ListEventSourcesResponse"
    "fixture/ListEventSourcesResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy ListEventSources)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy TagResource)

responseCreatePartnerEventSource :: CreatePartnerEventSourceResponse -> TestTree
responseCreatePartnerEventSource =
  res
    "CreatePartnerEventSourceResponse"
    "fixture/CreatePartnerEventSourceResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy CreatePartnerEventSource)

responsePutTargets :: PutTargetsResponse -> TestTree
responsePutTargets =
  res
    "PutTargetsResponse"
    "fixture/PutTargetsResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy PutTargets)

responseUpdateArchive :: UpdateArchiveResponse -> TestTree
responseUpdateArchive =
  res
    "UpdateArchiveResponse"
    "fixture/UpdateArchiveResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy UpdateArchive)

responseDeleteArchive :: DeleteArchiveResponse -> TestTree
responseDeleteArchive =
  res
    "DeleteArchiveResponse"
    "fixture/DeleteArchiveResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy DeleteArchive)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy UntagResource)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy PutEvents)

responseListPartnerEventSources :: ListPartnerEventSourcesResponse -> TestTree
responseListPartnerEventSources =
  res
    "ListPartnerEventSourcesResponse"
    "fixture/ListPartnerEventSourcesResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy ListPartnerEventSources)

responseCreateArchive :: CreateArchiveResponse -> TestTree
responseCreateArchive =
  res
    "CreateArchiveResponse"
    "fixture/CreateArchiveResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy CreateArchive)

responseDeactivateEventSource :: DeactivateEventSourceResponse -> TestTree
responseDeactivateEventSource =
  res
    "DeactivateEventSourceResponse"
    "fixture/DeactivateEventSourceResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy DeactivateEventSource)

responseDeleteEventBus :: DeleteEventBusResponse -> TestTree
responseDeleteEventBus =
  res
    "DeleteEventBusResponse"
    "fixture/DeleteEventBusResponse.proto"
    cloudWatchEventsService
    (Proxy :: Proxy DeleteEventBus)
