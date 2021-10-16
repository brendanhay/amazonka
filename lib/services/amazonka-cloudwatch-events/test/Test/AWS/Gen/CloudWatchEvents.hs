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
--         , requestUpdateConnection $
--             newUpdateConnection
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestDescribeArchive $
--             newDescribeArchive
--
--         , requestDeleteConnection $
--             newDeleteConnection
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
--         , requestListArchives $
--             newListArchives
--
--         , requestDeleteArchive $
--             newDeleteArchive
--
--         , requestDescribeRule $
--             newDescribeRule
--
--         , requestUpdateArchive $
--             newUpdateArchive
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeConnection $
--             newDescribeConnection
--
--         , requestPutPartnerEvents $
--             newPutPartnerEvents
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateApiDestination $
--             newCreateApiDestination
--
--         , requestDescribeEventBus $
--             newDescribeEventBus
--
--         , requestListTargetsByRule $
--             newListTargetsByRule
--
--         , requestListApiDestinations $
--             newListApiDestinations
--
--         , requestListRuleNamesByTarget $
--             newListRuleNamesByTarget
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestEnableRule $
--             newEnableRule
--
--         , requestPutRule $
--             newPutRule
--
--         , requestListRules $
--             newListRules
--
--         , requestListConnections $
--             newListConnections
--
--         , requestCreateEventBus $
--             newCreateEventBus
--
--         , requestDeauthorizeConnection $
--             newDeauthorizeConnection
--
--         , requestListEventBuses $
--             newListEventBuses
--
--         , requestRemoveTargets $
--             newRemoveTargets
--
--         , requestDeleteEventBus $
--             newDeleteEventBus
--
--         , requestDeletePartnerEventSource $
--             newDeletePartnerEventSource
--
--         , requestPutEvents $
--             newPutEvents
--
--         , requestDescribeReplay $
--             newDescribeReplay
--
--         , requestListPartnerEventSources $
--             newListPartnerEventSources
--
--         , requestCreateArchive $
--             newCreateArchive
--
--         , requestPutTargets $
--             newPutTargets
--
--         , requestCreatePartnerEventSource $
--             newCreatePartnerEventSource
--
--         , requestStartReplay $
--             newStartReplay
--
--         , requestActivateEventSource $
--             newActivateEventSource
--
--         , requestListEventSources $
--             newListEventSources
--
--         , requestUpdateApiDestination $
--             newUpdateApiDestination
--
--         , requestCancelReplay $
--             newCancelReplay
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestDeleteApiDestination $
--             newDeleteApiDestination
--
--         , requestTestEventPattern $
--             newTestEventPattern
--
--         , requestDisableRule $
--             newDisableRule
--
--         , requestDescribePartnerEventSource $
--             newDescribePartnerEventSource
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutPermission $
--             newPutPermission
--
--         , requestListReplays $
--             newListReplays
--
--           ]

--     , testGroup "response"
--         [ responseListPartnerEventSourceAccounts $
--             newListPartnerEventSourceAccountsResponse
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
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
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
--         , responseListArchives $
--             newListArchivesResponse
--
--         , responseDeleteArchive $
--             newDeleteArchiveResponse
--
--         , responseDescribeRule $
--             newDescribeRuleResponse
--
--         , responseUpdateArchive $
--             newUpdateArchiveResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeConnection $
--             newDescribeConnectionResponse
--
--         , responsePutPartnerEvents $
--             newPutPartnerEventsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateApiDestination $
--             newCreateApiDestinationResponse
--
--         , responseDescribeEventBus $
--             newDescribeEventBusResponse
--
--         , responseListTargetsByRule $
--             newListTargetsByRuleResponse
--
--         , responseListApiDestinations $
--             newListApiDestinationsResponse
--
--         , responseListRuleNamesByTarget $
--             newListRuleNamesByTargetResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseEnableRule $
--             newEnableRuleResponse
--
--         , responsePutRule $
--             newPutRuleResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responseListConnections $
--             newListConnectionsResponse
--
--         , responseCreateEventBus $
--             newCreateEventBusResponse
--
--         , responseDeauthorizeConnection $
--             newDeauthorizeConnectionResponse
--
--         , responseListEventBuses $
--             newListEventBusesResponse
--
--         , responseRemoveTargets $
--             newRemoveTargetsResponse
--
--         , responseDeleteEventBus $
--             newDeleteEventBusResponse
--
--         , responseDeletePartnerEventSource $
--             newDeletePartnerEventSourceResponse
--
--         , responsePutEvents $
--             newPutEventsResponse
--
--         , responseDescribeReplay $
--             newDescribeReplayResponse
--
--         , responseListPartnerEventSources $
--             newListPartnerEventSourcesResponse
--
--         , responseCreateArchive $
--             newCreateArchiveResponse
--
--         , responsePutTargets $
--             newPutTargetsResponse
--
--         , responseCreatePartnerEventSource $
--             newCreatePartnerEventSourceResponse
--
--         , responseStartReplay $
--             newStartReplayResponse
--
--         , responseActivateEventSource $
--             newActivateEventSourceResponse
--
--         , responseListEventSources $
--             newListEventSourcesResponse
--
--         , responseUpdateApiDestination $
--             newUpdateApiDestinationResponse
--
--         , responseCancelReplay $
--             newCancelReplayResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseDeleteApiDestination $
--             newDeleteApiDestinationResponse
--
--         , responseTestEventPattern $
--             newTestEventPatternResponse
--
--         , responseDisableRule $
--             newDisableRuleResponse
--
--         , responseDescribePartnerEventSource $
--             newDescribePartnerEventSourceResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutPermission $
--             newPutPermissionResponse
--
--         , responseListReplays $
--             newListReplaysResponse
--
--           ]
--     ]

-- Requests

requestListPartnerEventSourceAccounts :: ListPartnerEventSourceAccounts -> TestTree
requestListPartnerEventSourceAccounts =
  req
    "ListPartnerEventSourceAccounts"
    "fixture/ListPartnerEventSourceAccounts.yaml"

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

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

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

requestListArchives :: ListArchives -> TestTree
requestListArchives =
  req
    "ListArchives"
    "fixture/ListArchives.yaml"

requestDeleteArchive :: DeleteArchive -> TestTree
requestDeleteArchive =
  req
    "DeleteArchive"
    "fixture/DeleteArchive.yaml"

requestDescribeRule :: DescribeRule -> TestTree
requestDescribeRule =
  req
    "DescribeRule"
    "fixture/DescribeRule.yaml"

requestUpdateArchive :: UpdateArchive -> TestTree
requestUpdateArchive =
  req
    "UpdateArchive"
    "fixture/UpdateArchive.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeConnection :: DescribeConnection -> TestTree
requestDescribeConnection =
  req
    "DescribeConnection"
    "fixture/DescribeConnection.yaml"

requestPutPartnerEvents :: PutPartnerEvents -> TestTree
requestPutPartnerEvents =
  req
    "PutPartnerEvents"
    "fixture/PutPartnerEvents.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateApiDestination :: CreateApiDestination -> TestTree
requestCreateApiDestination =
  req
    "CreateApiDestination"
    "fixture/CreateApiDestination.yaml"

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

requestListApiDestinations :: ListApiDestinations -> TestTree
requestListApiDestinations =
  req
    "ListApiDestinations"
    "fixture/ListApiDestinations.yaml"

requestListRuleNamesByTarget :: ListRuleNamesByTarget -> TestTree
requestListRuleNamesByTarget =
  req
    "ListRuleNamesByTarget"
    "fixture/ListRuleNamesByTarget.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestEnableRule :: EnableRule -> TestTree
requestEnableRule =
  req
    "EnableRule"
    "fixture/EnableRule.yaml"

requestPutRule :: PutRule -> TestTree
requestPutRule =
  req
    "PutRule"
    "fixture/PutRule.yaml"

requestListRules :: ListRules -> TestTree
requestListRules =
  req
    "ListRules"
    "fixture/ListRules.yaml"

requestListConnections :: ListConnections -> TestTree
requestListConnections =
  req
    "ListConnections"
    "fixture/ListConnections.yaml"

requestCreateEventBus :: CreateEventBus -> TestTree
requestCreateEventBus =
  req
    "CreateEventBus"
    "fixture/CreateEventBus.yaml"

requestDeauthorizeConnection :: DeauthorizeConnection -> TestTree
requestDeauthorizeConnection =
  req
    "DeauthorizeConnection"
    "fixture/DeauthorizeConnection.yaml"

requestListEventBuses :: ListEventBuses -> TestTree
requestListEventBuses =
  req
    "ListEventBuses"
    "fixture/ListEventBuses.yaml"

requestRemoveTargets :: RemoveTargets -> TestTree
requestRemoveTargets =
  req
    "RemoveTargets"
    "fixture/RemoveTargets.yaml"

requestDeleteEventBus :: DeleteEventBus -> TestTree
requestDeleteEventBus =
  req
    "DeleteEventBus"
    "fixture/DeleteEventBus.yaml"

requestDeletePartnerEventSource :: DeletePartnerEventSource -> TestTree
requestDeletePartnerEventSource =
  req
    "DeletePartnerEventSource"
    "fixture/DeletePartnerEventSource.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents =
  req
    "PutEvents"
    "fixture/PutEvents.yaml"

requestDescribeReplay :: DescribeReplay -> TestTree
requestDescribeReplay =
  req
    "DescribeReplay"
    "fixture/DescribeReplay.yaml"

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

requestPutTargets :: PutTargets -> TestTree
requestPutTargets =
  req
    "PutTargets"
    "fixture/PutTargets.yaml"

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

requestActivateEventSource :: ActivateEventSource -> TestTree
requestActivateEventSource =
  req
    "ActivateEventSource"
    "fixture/ActivateEventSource.yaml"

requestListEventSources :: ListEventSources -> TestTree
requestListEventSources =
  req
    "ListEventSources"
    "fixture/ListEventSources.yaml"

requestUpdateApiDestination :: UpdateApiDestination -> TestTree
requestUpdateApiDestination =
  req
    "UpdateApiDestination"
    "fixture/UpdateApiDestination.yaml"

requestCancelReplay :: CancelReplay -> TestTree
requestCancelReplay =
  req
    "CancelReplay"
    "fixture/CancelReplay.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestDeleteApiDestination :: DeleteApiDestination -> TestTree
requestDeleteApiDestination =
  req
    "DeleteApiDestination"
    "fixture/DeleteApiDestination.yaml"

requestTestEventPattern :: TestEventPattern -> TestTree
requestTestEventPattern =
  req
    "TestEventPattern"
    "fixture/TestEventPattern.yaml"

requestDisableRule :: DisableRule -> TestTree
requestDisableRule =
  req
    "DisableRule"
    "fixture/DisableRule.yaml"

requestDescribePartnerEventSource :: DescribePartnerEventSource -> TestTree
requestDescribePartnerEventSource =
  req
    "DescribePartnerEventSource"
    "fixture/DescribePartnerEventSource.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutPermission :: PutPermission -> TestTree
requestPutPermission =
  req
    "PutPermission"
    "fixture/PutPermission.yaml"

requestListReplays :: ListReplays -> TestTree
requestListReplays =
  req
    "ListReplays"
    "fixture/ListReplays.yaml"

-- Responses

responseListPartnerEventSourceAccounts :: ListPartnerEventSourceAccountsResponse -> TestTree
responseListPartnerEventSourceAccounts =
  res
    "ListPartnerEventSourceAccountsResponse"
    "fixture/ListPartnerEventSourceAccountsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPartnerEventSourceAccounts)

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

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnection)

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

responseListArchives :: ListArchivesResponse -> TestTree
responseListArchives =
  res
    "ListArchivesResponse"
    "fixture/ListArchivesResponse.proto"
    defaultService
    (Proxy :: Proxy ListArchives)

responseDeleteArchive :: DeleteArchiveResponse -> TestTree
responseDeleteArchive =
  res
    "DeleteArchiveResponse"
    "fixture/DeleteArchiveResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteArchive)

responseDescribeRule :: DescribeRuleResponse -> TestTree
responseDescribeRule =
  res
    "DescribeRuleResponse"
    "fixture/DescribeRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRule)

responseUpdateArchive :: UpdateArchiveResponse -> TestTree
responseUpdateArchive =
  res
    "UpdateArchiveResponse"
    "fixture/UpdateArchiveResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateArchive)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDescribeConnection :: DescribeConnectionResponse -> TestTree
responseDescribeConnection =
  res
    "DescribeConnectionResponse"
    "fixture/DescribeConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConnection)

responsePutPartnerEvents :: PutPartnerEventsResponse -> TestTree
responsePutPartnerEvents =
  res
    "PutPartnerEventsResponse"
    "fixture/PutPartnerEventsResponse.proto"
    defaultService
    (Proxy :: Proxy PutPartnerEvents)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseCreateApiDestination :: CreateApiDestinationResponse -> TestTree
responseCreateApiDestination =
  res
    "CreateApiDestinationResponse"
    "fixture/CreateApiDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApiDestination)

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

responseListApiDestinations :: ListApiDestinationsResponse -> TestTree
responseListApiDestinations =
  res
    "ListApiDestinationsResponse"
    "fixture/ListApiDestinationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApiDestinations)

responseListRuleNamesByTarget :: ListRuleNamesByTargetResponse -> TestTree
responseListRuleNamesByTarget =
  res
    "ListRuleNamesByTargetResponse"
    "fixture/ListRuleNamesByTargetResponse.proto"
    defaultService
    (Proxy :: Proxy ListRuleNamesByTarget)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnection)

responseEnableRule :: EnableRuleResponse -> TestTree
responseEnableRule =
  res
    "EnableRuleResponse"
    "fixture/EnableRuleResponse.proto"
    defaultService
    (Proxy :: Proxy EnableRule)

responsePutRule :: PutRuleResponse -> TestTree
responsePutRule =
  res
    "PutRuleResponse"
    "fixture/PutRuleResponse.proto"
    defaultService
    (Proxy :: Proxy PutRule)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRules)

responseListConnections :: ListConnectionsResponse -> TestTree
responseListConnections =
  res
    "ListConnectionsResponse"
    "fixture/ListConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConnections)

responseCreateEventBus :: CreateEventBusResponse -> TestTree
responseCreateEventBus =
  res
    "CreateEventBusResponse"
    "fixture/CreateEventBusResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEventBus)

responseDeauthorizeConnection :: DeauthorizeConnectionResponse -> TestTree
responseDeauthorizeConnection =
  res
    "DeauthorizeConnectionResponse"
    "fixture/DeauthorizeConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeauthorizeConnection)

responseListEventBuses :: ListEventBusesResponse -> TestTree
responseListEventBuses =
  res
    "ListEventBusesResponse"
    "fixture/ListEventBusesResponse.proto"
    defaultService
    (Proxy :: Proxy ListEventBuses)

responseRemoveTargets :: RemoveTargetsResponse -> TestTree
responseRemoveTargets =
  res
    "RemoveTargetsResponse"
    "fixture/RemoveTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTargets)

responseDeleteEventBus :: DeleteEventBusResponse -> TestTree
responseDeleteEventBus =
  res
    "DeleteEventBusResponse"
    "fixture/DeleteEventBusResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEventBus)

responseDeletePartnerEventSource :: DeletePartnerEventSourceResponse -> TestTree
responseDeletePartnerEventSource =
  res
    "DeletePartnerEventSourceResponse"
    "fixture/DeletePartnerEventSourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePartnerEventSource)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    defaultService
    (Proxy :: Proxy PutEvents)

responseDescribeReplay :: DescribeReplayResponse -> TestTree
responseDescribeReplay =
  res
    "DescribeReplayResponse"
    "fixture/DescribeReplayResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReplay)

responseListPartnerEventSources :: ListPartnerEventSourcesResponse -> TestTree
responseListPartnerEventSources =
  res
    "ListPartnerEventSourcesResponse"
    "fixture/ListPartnerEventSourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPartnerEventSources)

responseCreateArchive :: CreateArchiveResponse -> TestTree
responseCreateArchive =
  res
    "CreateArchiveResponse"
    "fixture/CreateArchiveResponse.proto"
    defaultService
    (Proxy :: Proxy CreateArchive)

responsePutTargets :: PutTargetsResponse -> TestTree
responsePutTargets =
  res
    "PutTargetsResponse"
    "fixture/PutTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy PutTargets)

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

responseActivateEventSource :: ActivateEventSourceResponse -> TestTree
responseActivateEventSource =
  res
    "ActivateEventSourceResponse"
    "fixture/ActivateEventSourceResponse.proto"
    defaultService
    (Proxy :: Proxy ActivateEventSource)

responseListEventSources :: ListEventSourcesResponse -> TestTree
responseListEventSources =
  res
    "ListEventSourcesResponse"
    "fixture/ListEventSourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListEventSources)

responseUpdateApiDestination :: UpdateApiDestinationResponse -> TestTree
responseUpdateApiDestination =
  res
    "UpdateApiDestinationResponse"
    "fixture/UpdateApiDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApiDestination)

responseCancelReplay :: CancelReplayResponse -> TestTree
responseCancelReplay =
  res
    "CancelReplayResponse"
    "fixture/CancelReplayResponse.proto"
    defaultService
    (Proxy :: Proxy CancelReplay)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy RemovePermission)

responseDeleteApiDestination :: DeleteApiDestinationResponse -> TestTree
responseDeleteApiDestination =
  res
    "DeleteApiDestinationResponse"
    "fixture/DeleteApiDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApiDestination)

responseTestEventPattern :: TestEventPatternResponse -> TestTree
responseTestEventPattern =
  res
    "TestEventPatternResponse"
    "fixture/TestEventPatternResponse.proto"
    defaultService
    (Proxy :: Proxy TestEventPattern)

responseDisableRule :: DisableRuleResponse -> TestTree
responseDisableRule =
  res
    "DisableRuleResponse"
    "fixture/DisableRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DisableRule)

responseDescribePartnerEventSource :: DescribePartnerEventSourceResponse -> TestTree
responseDescribePartnerEventSource =
  res
    "DescribePartnerEventSourceResponse"
    "fixture/DescribePartnerEventSourceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePartnerEventSource)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responsePutPermission :: PutPermissionResponse -> TestTree
responsePutPermission =
  res
    "PutPermissionResponse"
    "fixture/PutPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy PutPermission)

responseListReplays :: ListReplaysResponse -> TestTree
responseListReplays =
  res
    "ListReplaysResponse"
    "fixture/ListReplaysResponse.proto"
    defaultService
    (Proxy :: Proxy ListReplays)
