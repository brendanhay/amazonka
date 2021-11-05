{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatchEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CloudWatchEvents where

import Amazonka.CloudWatchEvents
import qualified Data.Proxy as Proxy
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
--         [ requestDeauthorizeConnection $
--             newDeauthorizeConnection
--
--         , requestRemoveTargets $
--             newRemoveTargets
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestListPartnerEventSourceAccounts $
--             newListPartnerEventSourceAccounts
--
--         , requestListConnections $
--             newListConnections
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestUpdateConnection $
--             newUpdateConnection
--
--         , requestListRules $
--             newListRules
--
--         , requestPutRule $
--             newPutRule
--
--         , requestDisableRule $
--             newDisableRule
--
--         , requestPutPermission $
--             newPutPermission
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListReplays $
--             newListReplays
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestCancelReplay $
--             newCancelReplay
--
--         , requestListTargetsByRule $
--             newListTargetsByRule
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestListApiDestinations $
--             newListApiDestinations
--
--         , requestUpdateApiDestination $
--             newUpdateApiDestination
--
--         , requestDeleteApiDestination $
--             newDeleteApiDestination
--
--         , requestActivateEventSource $
--             newActivateEventSource
--
--         , requestCreateApiDestination $
--             newCreateApiDestination
--
--         , requestPutPartnerEvents $
--             newPutPartnerEvents
--
--         , requestDescribeConnection $
--             newDescribeConnection
--
--         , requestDescribeRule $
--             newDescribeRule
--
--         , requestListArchives $
--             newListArchives
--
--         , requestStartReplay $
--             newStartReplay
--
--         , requestDeletePartnerEventSource $
--             newDeletePartnerEventSource
--
--         , requestDescribeReplay $
--             newDescribeReplay
--
--         , requestDescribeApiDestination $
--             newDescribeApiDestination
--
--         , requestListEventBuses $
--             newListEventBuses
--
--         , requestCreateEventBus $
--             newCreateEventBus
--
--         , requestDescribeEventSource $
--             newDescribeEventSource
--
--         , requestDescribeArchive $
--             newDescribeArchive
--
--         , requestEnableRule $
--             newEnableRule
--
--         , requestListRuleNamesByTarget $
--             newListRuleNamesByTarget
--
--         , requestTestEventPattern $
--             newTestEventPattern
--
--         , requestDescribePartnerEventSource $
--             newDescribePartnerEventSource
--
--         , requestDescribeEventBus $
--             newDescribeEventBus
--
--         , requestListEventSources $
--             newListEventSources
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreatePartnerEventSource $
--             newCreatePartnerEventSource
--
--         , requestPutTargets $
--             newPutTargets
--
--         , requestUpdateArchive $
--             newUpdateArchive
--
--         , requestDeleteArchive $
--             newDeleteArchive
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestPutEvents $
--             newPutEvents
--
--         , requestListPartnerEventSources $
--             newListPartnerEventSources
--
--         , requestCreateArchive $
--             newCreateArchive
--
--         , requestDeactivateEventSource $
--             newDeactivateEventSource
--
--         , requestDeleteEventBus $
--             newDeleteEventBus
--
--           ]

--     , testGroup "response"
--         [ responseDeauthorizeConnection $
--             newDeauthorizeConnectionResponse
--
--         , responseRemoveTargets $
--             newRemoveTargetsResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseListPartnerEventSourceAccounts $
--             newListPartnerEventSourceAccountsResponse
--
--         , responseListConnections $
--             newListConnectionsResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseUpdateConnection $
--             newUpdateConnectionResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responsePutRule $
--             newPutRuleResponse
--
--         , responseDisableRule $
--             newDisableRuleResponse
--
--         , responsePutPermission $
--             newPutPermissionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListReplays $
--             newListReplaysResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseCancelReplay $
--             newCancelReplayResponse
--
--         , responseListTargetsByRule $
--             newListTargetsByRuleResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseListApiDestinations $
--             newListApiDestinationsResponse
--
--         , responseUpdateApiDestination $
--             newUpdateApiDestinationResponse
--
--         , responseDeleteApiDestination $
--             newDeleteApiDestinationResponse
--
--         , responseActivateEventSource $
--             newActivateEventSourceResponse
--
--         , responseCreateApiDestination $
--             newCreateApiDestinationResponse
--
--         , responsePutPartnerEvents $
--             newPutPartnerEventsResponse
--
--         , responseDescribeConnection $
--             newDescribeConnectionResponse
--
--         , responseDescribeRule $
--             newDescribeRuleResponse
--
--         , responseListArchives $
--             newListArchivesResponse
--
--         , responseStartReplay $
--             newStartReplayResponse
--
--         , responseDeletePartnerEventSource $
--             newDeletePartnerEventSourceResponse
--
--         , responseDescribeReplay $
--             newDescribeReplayResponse
--
--         , responseDescribeApiDestination $
--             newDescribeApiDestinationResponse
--
--         , responseListEventBuses $
--             newListEventBusesResponse
--
--         , responseCreateEventBus $
--             newCreateEventBusResponse
--
--         , responseDescribeEventSource $
--             newDescribeEventSourceResponse
--
--         , responseDescribeArchive $
--             newDescribeArchiveResponse
--
--         , responseEnableRule $
--             newEnableRuleResponse
--
--         , responseListRuleNamesByTarget $
--             newListRuleNamesByTargetResponse
--
--         , responseTestEventPattern $
--             newTestEventPatternResponse
--
--         , responseDescribePartnerEventSource $
--             newDescribePartnerEventSourceResponse
--
--         , responseDescribeEventBus $
--             newDescribeEventBusResponse
--
--         , responseListEventSources $
--             newListEventSourcesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreatePartnerEventSource $
--             newCreatePartnerEventSourceResponse
--
--         , responsePutTargets $
--             newPutTargetsResponse
--
--         , responseUpdateArchive $
--             newUpdateArchiveResponse
--
--         , responseDeleteArchive $
--             newDeleteArchiveResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responsePutEvents $
--             newPutEventsResponse
--
--         , responseListPartnerEventSources $
--             newListPartnerEventSourcesResponse
--
--         , responseCreateArchive $
--             newCreateArchiveResponse
--
--         , responseDeactivateEventSource $
--             newDeactivateEventSourceResponse
--
--         , responseDeleteEventBus $
--             newDeleteEventBusResponse
--
--           ]
--     ]

-- Requests

requestDeauthorizeConnection :: DeauthorizeConnection -> TestTree
requestDeauthorizeConnection =
  req
    "DeauthorizeConnection"
    "fixture/DeauthorizeConnection.yaml"

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

requestListConnections :: ListConnections -> TestTree
requestListConnections =
  req
    "ListConnections"
    "fixture/ListConnections.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestUpdateConnection :: UpdateConnection -> TestTree
requestUpdateConnection =
  req
    "UpdateConnection"
    "fixture/UpdateConnection.yaml"

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

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

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

requestListApiDestinations :: ListApiDestinations -> TestTree
requestListApiDestinations =
  req
    "ListApiDestinations"
    "fixture/ListApiDestinations.yaml"

requestUpdateApiDestination :: UpdateApiDestination -> TestTree
requestUpdateApiDestination =
  req
    "UpdateApiDestination"
    "fixture/UpdateApiDestination.yaml"

requestDeleteApiDestination :: DeleteApiDestination -> TestTree
requestDeleteApiDestination =
  req
    "DeleteApiDestination"
    "fixture/DeleteApiDestination.yaml"

requestActivateEventSource :: ActivateEventSource -> TestTree
requestActivateEventSource =
  req
    "ActivateEventSource"
    "fixture/ActivateEventSource.yaml"

requestCreateApiDestination :: CreateApiDestination -> TestTree
requestCreateApiDestination =
  req
    "CreateApiDestination"
    "fixture/CreateApiDestination.yaml"

requestPutPartnerEvents :: PutPartnerEvents -> TestTree
requestPutPartnerEvents =
  req
    "PutPartnerEvents"
    "fixture/PutPartnerEvents.yaml"

requestDescribeConnection :: DescribeConnection -> TestTree
requestDescribeConnection =
  req
    "DescribeConnection"
    "fixture/DescribeConnection.yaml"

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

requestDescribeApiDestination :: DescribeApiDestination -> TestTree
requestDescribeApiDestination =
  req
    "DescribeApiDestination"
    "fixture/DescribeApiDestination.yaml"

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

responseDeauthorizeConnection :: DeauthorizeConnectionResponse -> TestTree
responseDeauthorizeConnection =
  res
    "DeauthorizeConnectionResponse"
    "fixture/DeauthorizeConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeauthorizeConnection)

responseRemoveTargets :: RemoveTargetsResponse -> TestTree
responseRemoveTargets =
  res
    "RemoveTargetsResponse"
    "fixture/RemoveTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTargets)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRule)

responseListPartnerEventSourceAccounts :: ListPartnerEventSourceAccountsResponse -> TestTree
responseListPartnerEventSourceAccounts =
  res
    "ListPartnerEventSourceAccountsResponse"
    "fixture/ListPartnerEventSourceAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPartnerEventSourceAccounts)

responseListConnections :: ListConnectionsResponse -> TestTree
responseListConnections =
  res
    "ListConnectionsResponse"
    "fixture/ListConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnections)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnection)

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnection)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRules)

responsePutRule :: PutRuleResponse -> TestTree
responsePutRule =
  res
    "PutRuleResponse"
    "fixture/PutRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRule)

responseDisableRule :: DisableRuleResponse -> TestTree
responseDisableRule =
  res
    "DisableRuleResponse"
    "fixture/DisableRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableRule)

responsePutPermission :: PutPermissionResponse -> TestTree
responsePutPermission =
  res
    "PutPermissionResponse"
    "fixture/PutPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPermission)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListReplays :: ListReplaysResponse -> TestTree
responseListReplays =
  res
    "ListReplaysResponse"
    "fixture/ListReplaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReplays)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnection)

responseCancelReplay :: CancelReplayResponse -> TestTree
responseCancelReplay =
  res
    "CancelReplayResponse"
    "fixture/CancelReplayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelReplay)

responseListTargetsByRule :: ListTargetsByRuleResponse -> TestTree
responseListTargetsByRule =
  res
    "ListTargetsByRuleResponse"
    "fixture/ListTargetsByRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargetsByRule)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemovePermission)

responseListApiDestinations :: ListApiDestinationsResponse -> TestTree
responseListApiDestinations =
  res
    "ListApiDestinationsResponse"
    "fixture/ListApiDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApiDestinations)

responseUpdateApiDestination :: UpdateApiDestinationResponse -> TestTree
responseUpdateApiDestination =
  res
    "UpdateApiDestinationResponse"
    "fixture/UpdateApiDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApiDestination)

responseDeleteApiDestination :: DeleteApiDestinationResponse -> TestTree
responseDeleteApiDestination =
  res
    "DeleteApiDestinationResponse"
    "fixture/DeleteApiDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApiDestination)

responseActivateEventSource :: ActivateEventSourceResponse -> TestTree
responseActivateEventSource =
  res
    "ActivateEventSourceResponse"
    "fixture/ActivateEventSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateEventSource)

responseCreateApiDestination :: CreateApiDestinationResponse -> TestTree
responseCreateApiDestination =
  res
    "CreateApiDestinationResponse"
    "fixture/CreateApiDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApiDestination)

responsePutPartnerEvents :: PutPartnerEventsResponse -> TestTree
responsePutPartnerEvents =
  res
    "PutPartnerEventsResponse"
    "fixture/PutPartnerEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPartnerEvents)

responseDescribeConnection :: DescribeConnectionResponse -> TestTree
responseDescribeConnection =
  res
    "DescribeConnectionResponse"
    "fixture/DescribeConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnection)

responseDescribeRule :: DescribeRuleResponse -> TestTree
responseDescribeRule =
  res
    "DescribeRuleResponse"
    "fixture/DescribeRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRule)

responseListArchives :: ListArchivesResponse -> TestTree
responseListArchives =
  res
    "ListArchivesResponse"
    "fixture/ListArchivesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListArchives)

responseStartReplay :: StartReplayResponse -> TestTree
responseStartReplay =
  res
    "StartReplayResponse"
    "fixture/StartReplayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReplay)

responseDeletePartnerEventSource :: DeletePartnerEventSourceResponse -> TestTree
responseDeletePartnerEventSource =
  res
    "DeletePartnerEventSourceResponse"
    "fixture/DeletePartnerEventSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePartnerEventSource)

responseDescribeReplay :: DescribeReplayResponse -> TestTree
responseDescribeReplay =
  res
    "DescribeReplayResponse"
    "fixture/DescribeReplayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplay)

responseDescribeApiDestination :: DescribeApiDestinationResponse -> TestTree
responseDescribeApiDestination =
  res
    "DescribeApiDestinationResponse"
    "fixture/DescribeApiDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApiDestination)

responseListEventBuses :: ListEventBusesResponse -> TestTree
responseListEventBuses =
  res
    "ListEventBusesResponse"
    "fixture/ListEventBusesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventBuses)

responseCreateEventBus :: CreateEventBusResponse -> TestTree
responseCreateEventBus =
  res
    "CreateEventBusResponse"
    "fixture/CreateEventBusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventBus)

responseDescribeEventSource :: DescribeEventSourceResponse -> TestTree
responseDescribeEventSource =
  res
    "DescribeEventSourceResponse"
    "fixture/DescribeEventSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventSource)

responseDescribeArchive :: DescribeArchiveResponse -> TestTree
responseDescribeArchive =
  res
    "DescribeArchiveResponse"
    "fixture/DescribeArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeArchive)

responseEnableRule :: EnableRuleResponse -> TestTree
responseEnableRule =
  res
    "EnableRuleResponse"
    "fixture/EnableRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableRule)

responseListRuleNamesByTarget :: ListRuleNamesByTargetResponse -> TestTree
responseListRuleNamesByTarget =
  res
    "ListRuleNamesByTargetResponse"
    "fixture/ListRuleNamesByTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRuleNamesByTarget)

responseTestEventPattern :: TestEventPatternResponse -> TestTree
responseTestEventPattern =
  res
    "TestEventPatternResponse"
    "fixture/TestEventPatternResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestEventPattern)

responseDescribePartnerEventSource :: DescribePartnerEventSourceResponse -> TestTree
responseDescribePartnerEventSource =
  res
    "DescribePartnerEventSourceResponse"
    "fixture/DescribePartnerEventSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePartnerEventSource)

responseDescribeEventBus :: DescribeEventBusResponse -> TestTree
responseDescribeEventBus =
  res
    "DescribeEventBusResponse"
    "fixture/DescribeEventBusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventBus)

responseListEventSources :: ListEventSourcesResponse -> TestTree
responseListEventSources =
  res
    "ListEventSourcesResponse"
    "fixture/ListEventSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventSources)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCreatePartnerEventSource :: CreatePartnerEventSourceResponse -> TestTree
responseCreatePartnerEventSource =
  res
    "CreatePartnerEventSourceResponse"
    "fixture/CreatePartnerEventSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePartnerEventSource)

responsePutTargets :: PutTargetsResponse -> TestTree
responsePutTargets =
  res
    "PutTargetsResponse"
    "fixture/PutTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutTargets)

responseUpdateArchive :: UpdateArchiveResponse -> TestTree
responseUpdateArchive =
  res
    "UpdateArchiveResponse"
    "fixture/UpdateArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateArchive)

responseDeleteArchive :: DeleteArchiveResponse -> TestTree
responseDeleteArchive =
  res
    "DeleteArchiveResponse"
    "fixture/DeleteArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteArchive)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEvents)

responseListPartnerEventSources :: ListPartnerEventSourcesResponse -> TestTree
responseListPartnerEventSources =
  res
    "ListPartnerEventSourcesResponse"
    "fixture/ListPartnerEventSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPartnerEventSources)

responseCreateArchive :: CreateArchiveResponse -> TestTree
responseCreateArchive =
  res
    "CreateArchiveResponse"
    "fixture/CreateArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateArchive)

responseDeactivateEventSource :: DeactivateEventSourceResponse -> TestTree
responseDeactivateEventSource =
  res
    "DeactivateEventSourceResponse"
    "fixture/DeactivateEventSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateEventSource)

responseDeleteEventBus :: DeleteEventBusResponse -> TestTree
responseDeleteEventBus =
  res
    "DeleteEventBusResponse"
    "fixture/DeleteEventBusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventBus)
