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
--         [ requestListPartnerEventSourceAccounts $
--             newListPartnerEventSourceAccounts
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestUpdateConnection $
--             newUpdateConnection
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestDescribeArchive $
--             newDescribeArchive
--
--         , requestDescribeEventSource $
--             newDescribeEventSource
--
--         , requestDescribeApiDestination $
--             newDescribeApiDestination
--
--         , requestDeactivateEventSource $
--             newDeactivateEventSource
--
--         , requestUpdateArchive $
--             newUpdateArchive
--
--         , requestDescribeConnection $
--             newDescribeConnection
--
--         , requestDeleteArchive $
--             newDeleteArchive
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeRule $
--             newDescribeRule
--
--         , requestListArchives $
--             newListArchives
--
--         , requestPutPartnerEvents $
--             newPutPartnerEvents
--
--         , requestCreateApiDestination $
--             newCreateApiDestination
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListApiDestinations $
--             newListApiDestinations
--
--         , requestDescribeEventBus $
--             newDescribeEventBus
--
--         , requestListTargetsByRule $
--             newListTargetsByRule
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestListRuleNamesByTarget $
--             newListRuleNamesByTarget
--
--         , requestListRules $
--             newListRules
--
--         , requestPutRule $
--             newPutRule
--
--         , requestEnableRule $
--             newEnableRule
--
--         , requestListConnections $
--             newListConnections
--
--         , requestDeauthorizeConnection $
--             newDeauthorizeConnection
--
--         , requestCreateEventBus $
--             newCreateEventBus
--
--         , requestRemoveTargets $
--             newRemoveTargets
--
--         , requestListEventBuses $
--             newListEventBuses
--
--         , requestDeleteEventBus $
--             newDeleteEventBus
--
--         , requestPutEvents $
--             newPutEvents
--
--         , requestCreateArchive $
--             newCreateArchive
--
--         , requestListPartnerEventSources $
--             newListPartnerEventSources
--
--         , requestDescribeReplay $
--             newDescribeReplay
--
--         , requestDeletePartnerEventSource $
--             newDeletePartnerEventSource
--
--         , requestCreatePartnerEventSource $
--             newCreatePartnerEventSource
--
--         , requestStartReplay $
--             newStartReplay
--
--         , requestPutTargets $
--             newPutTargets
--
--         , requestListEventSources $
--             newListEventSources
--
--         , requestActivateEventSource $
--             newActivateEventSource
--
--         , requestDeleteApiDestination $
--             newDeleteApiDestination
--
--         , requestCancelReplay $
--             newCancelReplay
--
--         , requestUpdateApiDestination $
--             newUpdateApiDestination
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestTestEventPattern $
--             newTestEventPattern
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDisableRule $
--             newDisableRule
--
--         , requestListReplays $
--             newListReplays
--
--         , requestDescribePartnerEventSource $
--             newDescribePartnerEventSource
--
--         , requestPutPermission $
--             newPutPermission
--
--           ]

--     , testGroup "response"
--         [ responseListPartnerEventSourceAccounts $
--             newListPartnerEventSourceAccountsResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseUpdateConnection $
--             newUpdateConnectionResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseDescribeArchive $
--             newDescribeArchiveResponse
--
--         , responseDescribeEventSource $
--             newDescribeEventSourceResponse
--
--         , responseDescribeApiDestination $
--             newDescribeApiDestinationResponse
--
--         , responseDeactivateEventSource $
--             newDeactivateEventSourceResponse
--
--         , responseUpdateArchive $
--             newUpdateArchiveResponse
--
--         , responseDescribeConnection $
--             newDescribeConnectionResponse
--
--         , responseDeleteArchive $
--             newDeleteArchiveResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeRule $
--             newDescribeRuleResponse
--
--         , responseListArchives $
--             newListArchivesResponse
--
--         , responsePutPartnerEvents $
--             newPutPartnerEventsResponse
--
--         , responseCreateApiDestination $
--             newCreateApiDestinationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListApiDestinations $
--             newListApiDestinationsResponse
--
--         , responseDescribeEventBus $
--             newDescribeEventBusResponse
--
--         , responseListTargetsByRule $
--             newListTargetsByRuleResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseListRuleNamesByTarget $
--             newListRuleNamesByTargetResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responsePutRule $
--             newPutRuleResponse
--
--         , responseEnableRule $
--             newEnableRuleResponse
--
--         , responseListConnections $
--             newListConnectionsResponse
--
--         , responseDeauthorizeConnection $
--             newDeauthorizeConnectionResponse
--
--         , responseCreateEventBus $
--             newCreateEventBusResponse
--
--         , responseRemoveTargets $
--             newRemoveTargetsResponse
--
--         , responseListEventBuses $
--             newListEventBusesResponse
--
--         , responseDeleteEventBus $
--             newDeleteEventBusResponse
--
--         , responsePutEvents $
--             newPutEventsResponse
--
--         , responseCreateArchive $
--             newCreateArchiveResponse
--
--         , responseListPartnerEventSources $
--             newListPartnerEventSourcesResponse
--
--         , responseDescribeReplay $
--             newDescribeReplayResponse
--
--         , responseDeletePartnerEventSource $
--             newDeletePartnerEventSourceResponse
--
--         , responseCreatePartnerEventSource $
--             newCreatePartnerEventSourceResponse
--
--         , responseStartReplay $
--             newStartReplayResponse
--
--         , responsePutTargets $
--             newPutTargetsResponse
--
--         , responseListEventSources $
--             newListEventSourcesResponse
--
--         , responseActivateEventSource $
--             newActivateEventSourceResponse
--
--         , responseDeleteApiDestination $
--             newDeleteApiDestinationResponse
--
--         , responseCancelReplay $
--             newCancelReplayResponse
--
--         , responseUpdateApiDestination $
--             newUpdateApiDestinationResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseTestEventPattern $
--             newTestEventPatternResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDisableRule $
--             newDisableRuleResponse
--
--         , responseListReplays $
--             newListReplaysResponse
--
--         , responseDescribePartnerEventSource $
--             newDescribePartnerEventSourceResponse
--
--         , responsePutPermission $
--             newPutPermissionResponse
--
--           ]
--     ]

-- Requests

requestListPartnerEventSourceAccounts :: ListPartnerEventSourceAccounts -> TestTree
requestListPartnerEventSourceAccounts =
  req
    "ListPartnerEventSourceAccounts"
    "fixture/ListPartnerEventSourceAccounts.yaml"

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

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestDescribeArchive :: DescribeArchive -> TestTree
requestDescribeArchive =
  req
    "DescribeArchive"
    "fixture/DescribeArchive.yaml"

requestDescribeEventSource :: DescribeEventSource -> TestTree
requestDescribeEventSource =
  req
    "DescribeEventSource"
    "fixture/DescribeEventSource.yaml"

requestDescribeApiDestination :: DescribeApiDestination -> TestTree
requestDescribeApiDestination =
  req
    "DescribeApiDestination"
    "fixture/DescribeApiDestination.yaml"

requestDeactivateEventSource :: DeactivateEventSource -> TestTree
requestDeactivateEventSource =
  req
    "DeactivateEventSource"
    "fixture/DeactivateEventSource.yaml"

requestUpdateArchive :: UpdateArchive -> TestTree
requestUpdateArchive =
  req
    "UpdateArchive"
    "fixture/UpdateArchive.yaml"

requestDescribeConnection :: DescribeConnection -> TestTree
requestDescribeConnection =
  req
    "DescribeConnection"
    "fixture/DescribeConnection.yaml"

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

requestPutPartnerEvents :: PutPartnerEvents -> TestTree
requestPutPartnerEvents =
  req
    "PutPartnerEvents"
    "fixture/PutPartnerEvents.yaml"

requestCreateApiDestination :: CreateApiDestination -> TestTree
requestCreateApiDestination =
  req
    "CreateApiDestination"
    "fixture/CreateApiDestination.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListApiDestinations :: ListApiDestinations -> TestTree
requestListApiDestinations =
  req
    "ListApiDestinations"
    "fixture/ListApiDestinations.yaml"

requestDescribeEventBus :: DescribeEventBus -> TestTree
requestDescribeEventBus =
  req
    "DescribeEventBus"
    "fixture/DescribeEventBus.yaml"

requestListTargetsByRule :: ListTargetsByRule -> TestTree
requestListTargetsByRule =
  req
    "ListTargetsByRule"
    "fixture/ListTargetsByRule.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestListRuleNamesByTarget :: ListRuleNamesByTarget -> TestTree
requestListRuleNamesByTarget =
  req
    "ListRuleNamesByTarget"
    "fixture/ListRuleNamesByTarget.yaml"

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

requestEnableRule :: EnableRule -> TestTree
requestEnableRule =
  req
    "EnableRule"
    "fixture/EnableRule.yaml"

requestListConnections :: ListConnections -> TestTree
requestListConnections =
  req
    "ListConnections"
    "fixture/ListConnections.yaml"

requestDeauthorizeConnection :: DeauthorizeConnection -> TestTree
requestDeauthorizeConnection =
  req
    "DeauthorizeConnection"
    "fixture/DeauthorizeConnection.yaml"

requestCreateEventBus :: CreateEventBus -> TestTree
requestCreateEventBus =
  req
    "CreateEventBus"
    "fixture/CreateEventBus.yaml"

requestRemoveTargets :: RemoveTargets -> TestTree
requestRemoveTargets =
  req
    "RemoveTargets"
    "fixture/RemoveTargets.yaml"

requestListEventBuses :: ListEventBuses -> TestTree
requestListEventBuses =
  req
    "ListEventBuses"
    "fixture/ListEventBuses.yaml"

requestDeleteEventBus :: DeleteEventBus -> TestTree
requestDeleteEventBus =
  req
    "DeleteEventBus"
    "fixture/DeleteEventBus.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents =
  req
    "PutEvents"
    "fixture/PutEvents.yaml"

requestCreateArchive :: CreateArchive -> TestTree
requestCreateArchive =
  req
    "CreateArchive"
    "fixture/CreateArchive.yaml"

requestListPartnerEventSources :: ListPartnerEventSources -> TestTree
requestListPartnerEventSources =
  req
    "ListPartnerEventSources"
    "fixture/ListPartnerEventSources.yaml"

requestDescribeReplay :: DescribeReplay -> TestTree
requestDescribeReplay =
  req
    "DescribeReplay"
    "fixture/DescribeReplay.yaml"

requestDeletePartnerEventSource :: DeletePartnerEventSource -> TestTree
requestDeletePartnerEventSource =
  req
    "DeletePartnerEventSource"
    "fixture/DeletePartnerEventSource.yaml"

requestCreatePartnerEventSource :: CreatePartnerEventSource -> TestTree
requestCreatePartnerEventSource =
  req
    "CreatePartnerEventSource"
    "fixture/CreatePartnerEventSource.yaml"

requestStartReplay :: StartReplay -> TestTree
requestStartReplay =
  req
    "StartReplay"
    "fixture/StartReplay.yaml"

requestPutTargets :: PutTargets -> TestTree
requestPutTargets =
  req
    "PutTargets"
    "fixture/PutTargets.yaml"

requestListEventSources :: ListEventSources -> TestTree
requestListEventSources =
  req
    "ListEventSources"
    "fixture/ListEventSources.yaml"

requestActivateEventSource :: ActivateEventSource -> TestTree
requestActivateEventSource =
  req
    "ActivateEventSource"
    "fixture/ActivateEventSource.yaml"

requestDeleteApiDestination :: DeleteApiDestination -> TestTree
requestDeleteApiDestination =
  req
    "DeleteApiDestination"
    "fixture/DeleteApiDestination.yaml"

requestCancelReplay :: CancelReplay -> TestTree
requestCancelReplay =
  req
    "CancelReplay"
    "fixture/CancelReplay.yaml"

requestUpdateApiDestination :: UpdateApiDestination -> TestTree
requestUpdateApiDestination =
  req
    "UpdateApiDestination"
    "fixture/UpdateApiDestination.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestTestEventPattern :: TestEventPattern -> TestTree
requestTestEventPattern =
  req
    "TestEventPattern"
    "fixture/TestEventPattern.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDisableRule :: DisableRule -> TestTree
requestDisableRule =
  req
    "DisableRule"
    "fixture/DisableRule.yaml"

requestListReplays :: ListReplays -> TestTree
requestListReplays =
  req
    "ListReplays"
    "fixture/ListReplays.yaml"

requestDescribePartnerEventSource :: DescribePartnerEventSource -> TestTree
requestDescribePartnerEventSource =
  req
    "DescribePartnerEventSource"
    "fixture/DescribePartnerEventSource.yaml"

requestPutPermission :: PutPermission -> TestTree
requestPutPermission =
  req
    "PutPermission"
    "fixture/PutPermission.yaml"

-- Responses

responseListPartnerEventSourceAccounts :: ListPartnerEventSourceAccountsResponse -> TestTree
responseListPartnerEventSourceAccounts =
  res
    "ListPartnerEventSourceAccountsResponse"
    "fixture/ListPartnerEventSourceAccountsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPartnerEventSourceAccounts)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnection)

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConnection)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRule)

responseDescribeArchive :: DescribeArchiveResponse -> TestTree
responseDescribeArchive =
  res
    "DescribeArchiveResponse"
    "fixture/DescribeArchiveResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeArchive)

responseDescribeEventSource :: DescribeEventSourceResponse -> TestTree
responseDescribeEventSource =
  res
    "DescribeEventSourceResponse"
    "fixture/DescribeEventSourceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventSource)

responseDescribeApiDestination :: DescribeApiDestinationResponse -> TestTree
responseDescribeApiDestination =
  res
    "DescribeApiDestinationResponse"
    "fixture/DescribeApiDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApiDestination)

responseDeactivateEventSource :: DeactivateEventSourceResponse -> TestTree
responseDeactivateEventSource =
  res
    "DeactivateEventSourceResponse"
    "fixture/DeactivateEventSourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeactivateEventSource)

responseUpdateArchive :: UpdateArchiveResponse -> TestTree
responseUpdateArchive =
  res
    "UpdateArchiveResponse"
    "fixture/UpdateArchiveResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateArchive)

responseDescribeConnection :: DescribeConnectionResponse -> TestTree
responseDescribeConnection =
  res
    "DescribeConnectionResponse"
    "fixture/DescribeConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConnection)

responseDeleteArchive :: DeleteArchiveResponse -> TestTree
responseDeleteArchive =
  res
    "DeleteArchiveResponse"
    "fixture/DeleteArchiveResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteArchive)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDescribeRule :: DescribeRuleResponse -> TestTree
responseDescribeRule =
  res
    "DescribeRuleResponse"
    "fixture/DescribeRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRule)

responseListArchives :: ListArchivesResponse -> TestTree
responseListArchives =
  res
    "ListArchivesResponse"
    "fixture/ListArchivesResponse.proto"
    defaultService
    (Proxy :: Proxy ListArchives)

responsePutPartnerEvents :: PutPartnerEventsResponse -> TestTree
responsePutPartnerEvents =
  res
    "PutPartnerEventsResponse"
    "fixture/PutPartnerEventsResponse.proto"
    defaultService
    (Proxy :: Proxy PutPartnerEvents)

responseCreateApiDestination :: CreateApiDestinationResponse -> TestTree
responseCreateApiDestination =
  res
    "CreateApiDestinationResponse"
    "fixture/CreateApiDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApiDestination)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListApiDestinations :: ListApiDestinationsResponse -> TestTree
responseListApiDestinations =
  res
    "ListApiDestinationsResponse"
    "fixture/ListApiDestinationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApiDestinations)

responseDescribeEventBus :: DescribeEventBusResponse -> TestTree
responseDescribeEventBus =
  res
    "DescribeEventBusResponse"
    "fixture/DescribeEventBusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventBus)

responseListTargetsByRule :: ListTargetsByRuleResponse -> TestTree
responseListTargetsByRule =
  res
    "ListTargetsByRuleResponse"
    "fixture/ListTargetsByRuleResponse.proto"
    defaultService
    (Proxy :: Proxy ListTargetsByRule)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnection)

responseListRuleNamesByTarget :: ListRuleNamesByTargetResponse -> TestTree
responseListRuleNamesByTarget =
  res
    "ListRuleNamesByTargetResponse"
    "fixture/ListRuleNamesByTargetResponse.proto"
    defaultService
    (Proxy :: Proxy ListRuleNamesByTarget)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRules)

responsePutRule :: PutRuleResponse -> TestTree
responsePutRule =
  res
    "PutRuleResponse"
    "fixture/PutRuleResponse.proto"
    defaultService
    (Proxy :: Proxy PutRule)

responseEnableRule :: EnableRuleResponse -> TestTree
responseEnableRule =
  res
    "EnableRuleResponse"
    "fixture/EnableRuleResponse.proto"
    defaultService
    (Proxy :: Proxy EnableRule)

responseListConnections :: ListConnectionsResponse -> TestTree
responseListConnections =
  res
    "ListConnectionsResponse"
    "fixture/ListConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConnections)

responseDeauthorizeConnection :: DeauthorizeConnectionResponse -> TestTree
responseDeauthorizeConnection =
  res
    "DeauthorizeConnectionResponse"
    "fixture/DeauthorizeConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeauthorizeConnection)

responseCreateEventBus :: CreateEventBusResponse -> TestTree
responseCreateEventBus =
  res
    "CreateEventBusResponse"
    "fixture/CreateEventBusResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEventBus)

responseRemoveTargets :: RemoveTargetsResponse -> TestTree
responseRemoveTargets =
  res
    "RemoveTargetsResponse"
    "fixture/RemoveTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTargets)

responseListEventBuses :: ListEventBusesResponse -> TestTree
responseListEventBuses =
  res
    "ListEventBusesResponse"
    "fixture/ListEventBusesResponse.proto"
    defaultService
    (Proxy :: Proxy ListEventBuses)

responseDeleteEventBus :: DeleteEventBusResponse -> TestTree
responseDeleteEventBus =
  res
    "DeleteEventBusResponse"
    "fixture/DeleteEventBusResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEventBus)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    defaultService
    (Proxy :: Proxy PutEvents)

responseCreateArchive :: CreateArchiveResponse -> TestTree
responseCreateArchive =
  res
    "CreateArchiveResponse"
    "fixture/CreateArchiveResponse.proto"
    defaultService
    (Proxy :: Proxy CreateArchive)

responseListPartnerEventSources :: ListPartnerEventSourcesResponse -> TestTree
responseListPartnerEventSources =
  res
    "ListPartnerEventSourcesResponse"
    "fixture/ListPartnerEventSourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPartnerEventSources)

responseDescribeReplay :: DescribeReplayResponse -> TestTree
responseDescribeReplay =
  res
    "DescribeReplayResponse"
    "fixture/DescribeReplayResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReplay)

responseDeletePartnerEventSource :: DeletePartnerEventSourceResponse -> TestTree
responseDeletePartnerEventSource =
  res
    "DeletePartnerEventSourceResponse"
    "fixture/DeletePartnerEventSourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePartnerEventSource)

responseCreatePartnerEventSource :: CreatePartnerEventSourceResponse -> TestTree
responseCreatePartnerEventSource =
  res
    "CreatePartnerEventSourceResponse"
    "fixture/CreatePartnerEventSourceResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePartnerEventSource)

responseStartReplay :: StartReplayResponse -> TestTree
responseStartReplay =
  res
    "StartReplayResponse"
    "fixture/StartReplayResponse.proto"
    defaultService
    (Proxy :: Proxy StartReplay)

responsePutTargets :: PutTargetsResponse -> TestTree
responsePutTargets =
  res
    "PutTargetsResponse"
    "fixture/PutTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy PutTargets)

responseListEventSources :: ListEventSourcesResponse -> TestTree
responseListEventSources =
  res
    "ListEventSourcesResponse"
    "fixture/ListEventSourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListEventSources)

responseActivateEventSource :: ActivateEventSourceResponse -> TestTree
responseActivateEventSource =
  res
    "ActivateEventSourceResponse"
    "fixture/ActivateEventSourceResponse.proto"
    defaultService
    (Proxy :: Proxy ActivateEventSource)

responseDeleteApiDestination :: DeleteApiDestinationResponse -> TestTree
responseDeleteApiDestination =
  res
    "DeleteApiDestinationResponse"
    "fixture/DeleteApiDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApiDestination)

responseCancelReplay :: CancelReplayResponse -> TestTree
responseCancelReplay =
  res
    "CancelReplayResponse"
    "fixture/CancelReplayResponse.proto"
    defaultService
    (Proxy :: Proxy CancelReplay)

responseUpdateApiDestination :: UpdateApiDestinationResponse -> TestTree
responseUpdateApiDestination =
  res
    "UpdateApiDestinationResponse"
    "fixture/UpdateApiDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApiDestination)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy RemovePermission)

responseTestEventPattern :: TestEventPatternResponse -> TestTree
responseTestEventPattern =
  res
    "TestEventPatternResponse"
    "fixture/TestEventPatternResponse.proto"
    defaultService
    (Proxy :: Proxy TestEventPattern)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDisableRule :: DisableRuleResponse -> TestTree
responseDisableRule =
  res
    "DisableRuleResponse"
    "fixture/DisableRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DisableRule)

responseListReplays :: ListReplaysResponse -> TestTree
responseListReplays =
  res
    "ListReplaysResponse"
    "fixture/ListReplaysResponse.proto"
    defaultService
    (Proxy :: Proxy ListReplays)

responseDescribePartnerEventSource :: DescribePartnerEventSourceResponse -> TestTree
responseDescribePartnerEventSource =
  res
    "DescribePartnerEventSourceResponse"
    "fixture/DescribePartnerEventSourceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePartnerEventSource)

responsePutPermission :: PutPermissionResponse -> TestTree
responsePutPermission =
  res
    "PutPermissionResponse"
    "fixture/PutPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy PutPermission)
