{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudWatchEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CloudWatchEvents where

import Amazonka.CloudWatchEvents
import qualified Data.Proxy as Proxy
import Test.Amazonka.CloudWatchEvents.Internal
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
--         [ requestActivateEventSource $
--             newActivateEventSource
--
--         , requestCancelReplay $
--             newCancelReplay
--
--         , requestCreateApiDestination $
--             newCreateApiDestination
--
--         , requestCreateArchive $
--             newCreateArchive
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestCreateEndpoint $
--             newCreateEndpoint
--
--         , requestCreateEventBus $
--             newCreateEventBus
--
--         , requestCreatePartnerEventSource $
--             newCreatePartnerEventSource
--
--         , requestDeactivateEventSource $
--             newDeactivateEventSource
--
--         , requestDeauthorizeConnection $
--             newDeauthorizeConnection
--
--         , requestDeleteApiDestination $
--             newDeleteApiDestination
--
--         , requestDeleteArchive $
--             newDeleteArchive
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestDeleteEventBus $
--             newDeleteEventBus
--
--         , requestDeletePartnerEventSource $
--             newDeletePartnerEventSource
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestDescribeApiDestination $
--             newDescribeApiDestination
--
--         , requestDescribeArchive $
--             newDescribeArchive
--
--         , requestDescribeConnection $
--             newDescribeConnection
--
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestDescribeEventBus $
--             newDescribeEventBus
--
--         , requestDescribeEventSource $
--             newDescribeEventSource
--
--         , requestDescribePartnerEventSource $
--             newDescribePartnerEventSource
--
--         , requestDescribeReplay $
--             newDescribeReplay
--
--         , requestDescribeRule $
--             newDescribeRule
--
--         , requestDisableRule $
--             newDisableRule
--
--         , requestEnableRule $
--             newEnableRule
--
--         , requestListApiDestinations $
--             newListApiDestinations
--
--         , requestListArchives $
--             newListArchives
--
--         , requestListConnections $
--             newListConnections
--
--         , requestListEndpoints $
--             newListEndpoints
--
--         , requestListEventBuses $
--             newListEventBuses
--
--         , requestListEventSources $
--             newListEventSources
--
--         , requestListPartnerEventSourceAccounts $
--             newListPartnerEventSourceAccounts
--
--         , requestListPartnerEventSources $
--             newListPartnerEventSources
--
--         , requestListReplays $
--             newListReplays
--
--         , requestListRuleNamesByTarget $
--             newListRuleNamesByTarget
--
--         , requestListRules $
--             newListRules
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTargetsByRule $
--             newListTargetsByRule
--
--         , requestPutEvents $
--             newPutEvents
--
--         , requestPutPartnerEvents $
--             newPutPartnerEvents
--
--         , requestPutPermission $
--             newPutPermission
--
--         , requestPutRule $
--             newPutRule
--
--         , requestPutTargets $
--             newPutTargets
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestRemoveTargets $
--             newRemoveTargets
--
--         , requestStartReplay $
--             newStartReplay
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestEventPattern $
--             newTestEventPattern
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApiDestination $
--             newUpdateApiDestination
--
--         , requestUpdateArchive $
--             newUpdateArchive
--
--         , requestUpdateConnection $
--             newUpdateConnection
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--           ]

--     , testGroup "response"
--         [ responseActivateEventSource $
--             newActivateEventSourceResponse
--
--         , responseCancelReplay $
--             newCancelReplayResponse
--
--         , responseCreateApiDestination $
--             newCreateApiDestinationResponse
--
--         , responseCreateArchive $
--             newCreateArchiveResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseCreateEndpoint $
--             newCreateEndpointResponse
--
--         , responseCreateEventBus $
--             newCreateEventBusResponse
--
--         , responseCreatePartnerEventSource $
--             newCreatePartnerEventSourceResponse
--
--         , responseDeactivateEventSource $
--             newDeactivateEventSourceResponse
--
--         , responseDeauthorizeConnection $
--             newDeauthorizeConnectionResponse
--
--         , responseDeleteApiDestination $
--             newDeleteApiDestinationResponse
--
--         , responseDeleteArchive $
--             newDeleteArchiveResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseDeleteEventBus $
--             newDeleteEventBusResponse
--
--         , responseDeletePartnerEventSource $
--             newDeletePartnerEventSourceResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseDescribeApiDestination $
--             newDescribeApiDestinationResponse
--
--         , responseDescribeArchive $
--             newDescribeArchiveResponse
--
--         , responseDescribeConnection $
--             newDescribeConnectionResponse
--
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseDescribeEventBus $
--             newDescribeEventBusResponse
--
--         , responseDescribeEventSource $
--             newDescribeEventSourceResponse
--
--         , responseDescribePartnerEventSource $
--             newDescribePartnerEventSourceResponse
--
--         , responseDescribeReplay $
--             newDescribeReplayResponse
--
--         , responseDescribeRule $
--             newDescribeRuleResponse
--
--         , responseDisableRule $
--             newDisableRuleResponse
--
--         , responseEnableRule $
--             newEnableRuleResponse
--
--         , responseListApiDestinations $
--             newListApiDestinationsResponse
--
--         , responseListArchives $
--             newListArchivesResponse
--
--         , responseListConnections $
--             newListConnectionsResponse
--
--         , responseListEndpoints $
--             newListEndpointsResponse
--
--         , responseListEventBuses $
--             newListEventBusesResponse
--
--         , responseListEventSources $
--             newListEventSourcesResponse
--
--         , responseListPartnerEventSourceAccounts $
--             newListPartnerEventSourceAccountsResponse
--
--         , responseListPartnerEventSources $
--             newListPartnerEventSourcesResponse
--
--         , responseListReplays $
--             newListReplaysResponse
--
--         , responseListRuleNamesByTarget $
--             newListRuleNamesByTargetResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTargetsByRule $
--             newListTargetsByRuleResponse
--
--         , responsePutEvents $
--             newPutEventsResponse
--
--         , responsePutPartnerEvents $
--             newPutPartnerEventsResponse
--
--         , responsePutPermission $
--             newPutPermissionResponse
--
--         , responsePutRule $
--             newPutRuleResponse
--
--         , responsePutTargets $
--             newPutTargetsResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseRemoveTargets $
--             newRemoveTargetsResponse
--
--         , responseStartReplay $
--             newStartReplayResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestEventPattern $
--             newTestEventPatternResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApiDestination $
--             newUpdateApiDestinationResponse
--
--         , responseUpdateArchive $
--             newUpdateArchiveResponse
--
--         , responseUpdateConnection $
--             newUpdateConnectionResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--           ]
--     ]

-- Requests

requestActivateEventSource :: ActivateEventSource -> TestTree
requestActivateEventSource =
  req
    "ActivateEventSource"
    "fixture/ActivateEventSource.yaml"

requestCancelReplay :: CancelReplay -> TestTree
requestCancelReplay =
  req
    "CancelReplay"
    "fixture/CancelReplay.yaml"

requestCreateApiDestination :: CreateApiDestination -> TestTree
requestCreateApiDestination =
  req
    "CreateApiDestination"
    "fixture/CreateApiDestination.yaml"

requestCreateArchive :: CreateArchive -> TestTree
requestCreateArchive =
  req
    "CreateArchive"
    "fixture/CreateArchive.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint =
  req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestCreateEventBus :: CreateEventBus -> TestTree
requestCreateEventBus =
  req
    "CreateEventBus"
    "fixture/CreateEventBus.yaml"

requestCreatePartnerEventSource :: CreatePartnerEventSource -> TestTree
requestCreatePartnerEventSource =
  req
    "CreatePartnerEventSource"
    "fixture/CreatePartnerEventSource.yaml"

requestDeactivateEventSource :: DeactivateEventSource -> TestTree
requestDeactivateEventSource =
  req
    "DeactivateEventSource"
    "fixture/DeactivateEventSource.yaml"

requestDeauthorizeConnection :: DeauthorizeConnection -> TestTree
requestDeauthorizeConnection =
  req
    "DeauthorizeConnection"
    "fixture/DeauthorizeConnection.yaml"

requestDeleteApiDestination :: DeleteApiDestination -> TestTree
requestDeleteApiDestination =
  req
    "DeleteApiDestination"
    "fixture/DeleteApiDestination.yaml"

requestDeleteArchive :: DeleteArchive -> TestTree
requestDeleteArchive =
  req
    "DeleteArchive"
    "fixture/DeleteArchive.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

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

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestDescribeApiDestination :: DescribeApiDestination -> TestTree
requestDescribeApiDestination =
  req
    "DescribeApiDestination"
    "fixture/DescribeApiDestination.yaml"

requestDescribeArchive :: DescribeArchive -> TestTree
requestDescribeArchive =
  req
    "DescribeArchive"
    "fixture/DescribeArchive.yaml"

requestDescribeConnection :: DescribeConnection -> TestTree
requestDescribeConnection =
  req
    "DescribeConnection"
    "fixture/DescribeConnection.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint =
  req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestDescribeEventBus :: DescribeEventBus -> TestTree
requestDescribeEventBus =
  req
    "DescribeEventBus"
    "fixture/DescribeEventBus.yaml"

requestDescribeEventSource :: DescribeEventSource -> TestTree
requestDescribeEventSource =
  req
    "DescribeEventSource"
    "fixture/DescribeEventSource.yaml"

requestDescribePartnerEventSource :: DescribePartnerEventSource -> TestTree
requestDescribePartnerEventSource =
  req
    "DescribePartnerEventSource"
    "fixture/DescribePartnerEventSource.yaml"

requestDescribeReplay :: DescribeReplay -> TestTree
requestDescribeReplay =
  req
    "DescribeReplay"
    "fixture/DescribeReplay.yaml"

requestDescribeRule :: DescribeRule -> TestTree
requestDescribeRule =
  req
    "DescribeRule"
    "fixture/DescribeRule.yaml"

requestDisableRule :: DisableRule -> TestTree
requestDisableRule =
  req
    "DisableRule"
    "fixture/DisableRule.yaml"

requestEnableRule :: EnableRule -> TestTree
requestEnableRule =
  req
    "EnableRule"
    "fixture/EnableRule.yaml"

requestListApiDestinations :: ListApiDestinations -> TestTree
requestListApiDestinations =
  req
    "ListApiDestinations"
    "fixture/ListApiDestinations.yaml"

requestListArchives :: ListArchives -> TestTree
requestListArchives =
  req
    "ListArchives"
    "fixture/ListArchives.yaml"

requestListConnections :: ListConnections -> TestTree
requestListConnections =
  req
    "ListConnections"
    "fixture/ListConnections.yaml"

requestListEndpoints :: ListEndpoints -> TestTree
requestListEndpoints =
  req
    "ListEndpoints"
    "fixture/ListEndpoints.yaml"

requestListEventBuses :: ListEventBuses -> TestTree
requestListEventBuses =
  req
    "ListEventBuses"
    "fixture/ListEventBuses.yaml"

requestListEventSources :: ListEventSources -> TestTree
requestListEventSources =
  req
    "ListEventSources"
    "fixture/ListEventSources.yaml"

requestListPartnerEventSourceAccounts :: ListPartnerEventSourceAccounts -> TestTree
requestListPartnerEventSourceAccounts =
  req
    "ListPartnerEventSourceAccounts"
    "fixture/ListPartnerEventSourceAccounts.yaml"

requestListPartnerEventSources :: ListPartnerEventSources -> TestTree
requestListPartnerEventSources =
  req
    "ListPartnerEventSources"
    "fixture/ListPartnerEventSources.yaml"

requestListReplays :: ListReplays -> TestTree
requestListReplays =
  req
    "ListReplays"
    "fixture/ListReplays.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTargetsByRule :: ListTargetsByRule -> TestTree
requestListTargetsByRule =
  req
    "ListTargetsByRule"
    "fixture/ListTargetsByRule.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents =
  req
    "PutEvents"
    "fixture/PutEvents.yaml"

requestPutPartnerEvents :: PutPartnerEvents -> TestTree
requestPutPartnerEvents =
  req
    "PutPartnerEvents"
    "fixture/PutPartnerEvents.yaml"

requestPutPermission :: PutPermission -> TestTree
requestPutPermission =
  req
    "PutPermission"
    "fixture/PutPermission.yaml"

requestPutRule :: PutRule -> TestTree
requestPutRule =
  req
    "PutRule"
    "fixture/PutRule.yaml"

requestPutTargets :: PutTargets -> TestTree
requestPutTargets =
  req
    "PutTargets"
    "fixture/PutTargets.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestRemoveTargets :: RemoveTargets -> TestTree
requestRemoveTargets =
  req
    "RemoveTargets"
    "fixture/RemoveTargets.yaml"

requestStartReplay :: StartReplay -> TestTree
requestStartReplay =
  req
    "StartReplay"
    "fixture/StartReplay.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestEventPattern :: TestEventPattern -> TestTree
requestTestEventPattern =
  req
    "TestEventPattern"
    "fixture/TestEventPattern.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateApiDestination :: UpdateApiDestination -> TestTree
requestUpdateApiDestination =
  req
    "UpdateApiDestination"
    "fixture/UpdateApiDestination.yaml"

requestUpdateArchive :: UpdateArchive -> TestTree
requestUpdateArchive =
  req
    "UpdateArchive"
    "fixture/UpdateArchive.yaml"

requestUpdateConnection :: UpdateConnection -> TestTree
requestUpdateConnection =
  req
    "UpdateConnection"
    "fixture/UpdateConnection.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint =
  req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

-- Responses

responseActivateEventSource :: ActivateEventSourceResponse -> TestTree
responseActivateEventSource =
  res
    "ActivateEventSourceResponse"
    "fixture/ActivateEventSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateEventSource)

responseCancelReplay :: CancelReplayResponse -> TestTree
responseCancelReplay =
  res
    "CancelReplayResponse"
    "fixture/CancelReplayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelReplay)

responseCreateApiDestination :: CreateApiDestinationResponse -> TestTree
responseCreateApiDestination =
  res
    "CreateApiDestinationResponse"
    "fixture/CreateApiDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApiDestination)

responseCreateArchive :: CreateArchiveResponse -> TestTree
responseCreateArchive =
  res
    "CreateArchiveResponse"
    "fixture/CreateArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateArchive)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnection)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpoint)

responseCreateEventBus :: CreateEventBusResponse -> TestTree
responseCreateEventBus =
  res
    "CreateEventBusResponse"
    "fixture/CreateEventBusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventBus)

responseCreatePartnerEventSource :: CreatePartnerEventSourceResponse -> TestTree
responseCreatePartnerEventSource =
  res
    "CreatePartnerEventSourceResponse"
    "fixture/CreatePartnerEventSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePartnerEventSource)

responseDeactivateEventSource :: DeactivateEventSourceResponse -> TestTree
responseDeactivateEventSource =
  res
    "DeactivateEventSourceResponse"
    "fixture/DeactivateEventSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateEventSource)

responseDeauthorizeConnection :: DeauthorizeConnectionResponse -> TestTree
responseDeauthorizeConnection =
  res
    "DeauthorizeConnectionResponse"
    "fixture/DeauthorizeConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeauthorizeConnection)

responseDeleteApiDestination :: DeleteApiDestinationResponse -> TestTree
responseDeleteApiDestination =
  res
    "DeleteApiDestinationResponse"
    "fixture/DeleteApiDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApiDestination)

responseDeleteArchive :: DeleteArchiveResponse -> TestTree
responseDeleteArchive =
  res
    "DeleteArchiveResponse"
    "fixture/DeleteArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteArchive)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnection)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpoint)

responseDeleteEventBus :: DeleteEventBusResponse -> TestTree
responseDeleteEventBus =
  res
    "DeleteEventBusResponse"
    "fixture/DeleteEventBusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventBus)

responseDeletePartnerEventSource :: DeletePartnerEventSourceResponse -> TestTree
responseDeletePartnerEventSource =
  res
    "DeletePartnerEventSourceResponse"
    "fixture/DeletePartnerEventSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePartnerEventSource)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRule)

responseDescribeApiDestination :: DescribeApiDestinationResponse -> TestTree
responseDescribeApiDestination =
  res
    "DescribeApiDestinationResponse"
    "fixture/DescribeApiDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApiDestination)

responseDescribeArchive :: DescribeArchiveResponse -> TestTree
responseDescribeArchive =
  res
    "DescribeArchiveResponse"
    "fixture/DescribeArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeArchive)

responseDescribeConnection :: DescribeConnectionResponse -> TestTree
responseDescribeConnection =
  res
    "DescribeConnectionResponse"
    "fixture/DescribeConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnection)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoint)

responseDescribeEventBus :: DescribeEventBusResponse -> TestTree
responseDescribeEventBus =
  res
    "DescribeEventBusResponse"
    "fixture/DescribeEventBusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventBus)

responseDescribeEventSource :: DescribeEventSourceResponse -> TestTree
responseDescribeEventSource =
  res
    "DescribeEventSourceResponse"
    "fixture/DescribeEventSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventSource)

responseDescribePartnerEventSource :: DescribePartnerEventSourceResponse -> TestTree
responseDescribePartnerEventSource =
  res
    "DescribePartnerEventSourceResponse"
    "fixture/DescribePartnerEventSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePartnerEventSource)

responseDescribeReplay :: DescribeReplayResponse -> TestTree
responseDescribeReplay =
  res
    "DescribeReplayResponse"
    "fixture/DescribeReplayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplay)

responseDescribeRule :: DescribeRuleResponse -> TestTree
responseDescribeRule =
  res
    "DescribeRuleResponse"
    "fixture/DescribeRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRule)

responseDisableRule :: DisableRuleResponse -> TestTree
responseDisableRule =
  res
    "DisableRuleResponse"
    "fixture/DisableRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableRule)

responseEnableRule :: EnableRuleResponse -> TestTree
responseEnableRule =
  res
    "EnableRuleResponse"
    "fixture/EnableRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableRule)

responseListApiDestinations :: ListApiDestinationsResponse -> TestTree
responseListApiDestinations =
  res
    "ListApiDestinationsResponse"
    "fixture/ListApiDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApiDestinations)

responseListArchives :: ListArchivesResponse -> TestTree
responseListArchives =
  res
    "ListArchivesResponse"
    "fixture/ListArchivesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListArchives)

responseListConnections :: ListConnectionsResponse -> TestTree
responseListConnections =
  res
    "ListConnectionsResponse"
    "fixture/ListConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnections)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpoints)

responseListEventBuses :: ListEventBusesResponse -> TestTree
responseListEventBuses =
  res
    "ListEventBusesResponse"
    "fixture/ListEventBusesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventBuses)

responseListEventSources :: ListEventSourcesResponse -> TestTree
responseListEventSources =
  res
    "ListEventSourcesResponse"
    "fixture/ListEventSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventSources)

responseListPartnerEventSourceAccounts :: ListPartnerEventSourceAccountsResponse -> TestTree
responseListPartnerEventSourceAccounts =
  res
    "ListPartnerEventSourceAccountsResponse"
    "fixture/ListPartnerEventSourceAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPartnerEventSourceAccounts)

responseListPartnerEventSources :: ListPartnerEventSourcesResponse -> TestTree
responseListPartnerEventSources =
  res
    "ListPartnerEventSourcesResponse"
    "fixture/ListPartnerEventSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPartnerEventSources)

responseListReplays :: ListReplaysResponse -> TestTree
responseListReplays =
  res
    "ListReplaysResponse"
    "fixture/ListReplaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReplays)

responseListRuleNamesByTarget :: ListRuleNamesByTargetResponse -> TestTree
responseListRuleNamesByTarget =
  res
    "ListRuleNamesByTargetResponse"
    "fixture/ListRuleNamesByTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRuleNamesByTarget)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRules)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTargetsByRule :: ListTargetsByRuleResponse -> TestTree
responseListTargetsByRule =
  res
    "ListTargetsByRuleResponse"
    "fixture/ListTargetsByRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargetsByRule)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEvents)

responsePutPartnerEvents :: PutPartnerEventsResponse -> TestTree
responsePutPartnerEvents =
  res
    "PutPartnerEventsResponse"
    "fixture/PutPartnerEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPartnerEvents)

responsePutPermission :: PutPermissionResponse -> TestTree
responsePutPermission =
  res
    "PutPermissionResponse"
    "fixture/PutPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPermission)

responsePutRule :: PutRuleResponse -> TestTree
responsePutRule =
  res
    "PutRuleResponse"
    "fixture/PutRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRule)

responsePutTargets :: PutTargetsResponse -> TestTree
responsePutTargets =
  res
    "PutTargetsResponse"
    "fixture/PutTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutTargets)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemovePermission)

responseRemoveTargets :: RemoveTargetsResponse -> TestTree
responseRemoveTargets =
  res
    "RemoveTargetsResponse"
    "fixture/RemoveTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTargets)

responseStartReplay :: StartReplayResponse -> TestTree
responseStartReplay =
  res
    "StartReplayResponse"
    "fixture/StartReplayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReplay)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestEventPattern :: TestEventPatternResponse -> TestTree
responseTestEventPattern =
  res
    "TestEventPatternResponse"
    "fixture/TestEventPatternResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestEventPattern)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateApiDestination :: UpdateApiDestinationResponse -> TestTree
responseUpdateApiDestination =
  res
    "UpdateApiDestinationResponse"
    "fixture/UpdateApiDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApiDestination)

responseUpdateArchive :: UpdateArchiveResponse -> TestTree
responseUpdateArchive =
  res
    "UpdateArchiveResponse"
    "fixture/UpdateArchiveResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateArchive)

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnection)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpoint)
