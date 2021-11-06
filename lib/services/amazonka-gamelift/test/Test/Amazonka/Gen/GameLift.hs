{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.GameLift
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.GameLift where

import Amazonka.GameLift
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.GameLift.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStopMatchmaking $
--             newStopMatchmaking
--
--         , requestDescribeGameServerInstances $
--             newDescribeGameServerInstances
--
--         , requestCreateGameSession $
--             newCreateGameSession
--
--         , requestDeleteScalingPolicy $
--             newDeleteScalingPolicy
--
--         , requestPutScalingPolicy $
--             newPutScalingPolicy
--
--         , requestListBuilds $
--             newListBuilds
--
--         , requestDeleteFleet $
--             newDeleteFleet
--
--         , requestCreateBuild $
--             newCreateBuild
--
--         , requestRequestUploadCredentials $
--             newRequestUploadCredentials
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestListGameServers $
--             newListGameServers
--
--         , requestResolveAlias $
--             newResolveAlias
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRegisterGameServer $
--             newRegisterGameServer
--
--         , requestListAliases $
--             newListAliases
--
--         , requestUpdateRuntimeConfiguration $
--             newUpdateRuntimeConfiguration
--
--         , requestCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnection
--
--         , requestListGameServerGroups $
--             newListGameServerGroups
--
--         , requestCreateGameSessionQueue $
--             newCreateGameSessionQueue
--
--         , requestSearchGameSessions $
--             newSearchGameSessions
--
--         , requestCreateVpcPeeringAuthorization $
--             newCreateVpcPeeringAuthorization
--
--         , requestUpdateGameSessionQueue $
--             newUpdateGameSessionQueue
--
--         , requestDeleteGameSessionQueue $
--             newDeleteGameSessionQueue
--
--         , requestCreateGameServerGroup $
--             newCreateGameServerGroup
--
--         , requestDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnection
--
--         , requestStartFleetActions $
--             newStartFleetActions
--
--         , requestDeregisterGameServer $
--             newDeregisterGameServer
--
--         , requestGetInstanceAccess $
--             newGetInstanceAccess
--
--         , requestDescribeScalingPolicies $
--             newDescribeScalingPolicies
--
--         , requestDescribeMatchmakingRuleSets $
--             newDescribeMatchmakingRuleSets
--
--         , requestDescribeGameSessions $
--             newDescribeGameSessions
--
--         , requestDescribeGameServer $
--             newDescribeGameServer
--
--         , requestUpdateScript $
--             newUpdateScript
--
--         , requestDeleteScript $
--             newDeleteScript
--
--         , requestStartGameSessionPlacement $
--             newStartGameSessionPlacement
--
--         , requestDescribeFleetUtilization $
--             newDescribeFleetUtilization
--
--         , requestDescribeRuntimeConfiguration $
--             newDescribeRuntimeConfiguration
--
--         , requestGetGameSessionLogUrl $
--             newGetGameSessionLogUrl
--
--         , requestDescribeFleetAttributes $
--             newDescribeFleetAttributes
--
--         , requestDescribeGameSessionPlacement $
--             newDescribeGameSessionPlacement
--
--         , requestDescribeFleetEvents $
--             newDescribeFleetEvents
--
--         , requestCreateFleetLocations $
--             newCreateFleetLocations
--
--         , requestStartMatchmaking $
--             newStartMatchmaking
--
--         , requestCreateMatchmakingRuleSet $
--             newCreateMatchmakingRuleSet
--
--         , requestDescribeFleetLocationUtilization $
--             newDescribeFleetLocationUtilization
--
--         , requestDescribeFleetCapacity $
--             newDescribeFleetCapacity
--
--         , requestDeleteBuild $
--             newDeleteBuild
--
--         , requestUpdateBuild $
--             newUpdateBuild
--
--         , requestListFleets $
--             newListFleets
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestUpdateAlias $
--             newUpdateAlias
--
--         , requestStartMatchBackfill $
--             newStartMatchBackfill
--
--         , requestDescribeInstances $
--             newDescribeInstances
--
--         , requestDescribeGameSessionDetails $
--             newDescribeGameSessionDetails
--
--         , requestDescribeFleetPortSettings $
--             newDescribeFleetPortSettings
--
--         , requestDescribeGameSessionQueues $
--             newDescribeGameSessionQueues
--
--         , requestDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnections
--
--         , requestDescribeScript $
--             newDescribeScript
--
--         , requestCreatePlayerSessions $
--             newCreatePlayerSessions
--
--         , requestDescribeMatchmakingConfigurations $
--             newDescribeMatchmakingConfigurations
--
--         , requestDescribeVpcPeeringAuthorizations $
--             newDescribeVpcPeeringAuthorizations
--
--         , requestUpdateGameServer $
--             newUpdateGameServer
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestDescribeFleetLocationCapacity $
--             newDescribeFleetLocationCapacity
--
--         , requestDeleteMatchmakingConfiguration $
--             newDeleteMatchmakingConfiguration
--
--         , requestUpdateMatchmakingConfiguration $
--             newUpdateMatchmakingConfiguration
--
--         , requestDeleteGameServerGroup $
--             newDeleteGameServerGroup
--
--         , requestUpdateGameServerGroup $
--             newUpdateGameServerGroup
--
--         , requestResumeGameServerGroup $
--             newResumeGameServerGroup
--
--         , requestDeleteVpcPeeringAuthorization $
--             newDeleteVpcPeeringAuthorization
--
--         , requestUpdateFleetAttributes $
--             newUpdateFleetAttributes
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateMatchmakingConfiguration $
--             newCreateMatchmakingConfiguration
--
--         , requestDescribePlayerSessions $
--             newDescribePlayerSessions
--
--         , requestStopFleetActions $
--             newStopFleetActions
--
--         , requestDescribeBuild $
--             newDescribeBuild
--
--         , requestUpdateFleetPortSettings $
--             newUpdateFleetPortSettings
--
--         , requestUpdateFleetCapacity $
--             newUpdateFleetCapacity
--
--         , requestCreateScript $
--             newCreateScript
--
--         , requestAcceptMatch $
--             newAcceptMatch
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeAlias $
--             newDescribeAlias
--
--         , requestValidateMatchmakingRuleSet $
--             newValidateMatchmakingRuleSet
--
--         , requestListScripts $
--             newListScripts
--
--         , requestDescribeEC2InstanceLimits $
--             newDescribeEC2InstanceLimits
--
--         , requestSuspendGameServerGroup $
--             newSuspendGameServerGroup
--
--         , requestDeleteFleetLocations $
--             newDeleteFleetLocations
--
--         , requestDeleteMatchmakingRuleSet $
--             newDeleteMatchmakingRuleSet
--
--         , requestStopGameSessionPlacement $
--             newStopGameSessionPlacement
--
--         , requestClaimGameServer $
--             newClaimGameServer
--
--         , requestUpdateGameSession $
--             newUpdateGameSession
--
--         , requestDescribeFleetLocationAttributes $
--             newDescribeFleetLocationAttributes
--
--         , requestDescribeMatchmaking $
--             newDescribeMatchmaking
--
--         , requestCreatePlayerSession $
--             newCreatePlayerSession
--
--         , requestDescribeGameServerGroup $
--             newDescribeGameServerGroup
--
--           ]

--     , testGroup "response"
--         [ responseStopMatchmaking $
--             newStopMatchmakingResponse
--
--         , responseDescribeGameServerInstances $
--             newDescribeGameServerInstancesResponse
--
--         , responseCreateGameSession $
--             newCreateGameSessionResponse
--
--         , responseDeleteScalingPolicy $
--             newDeleteScalingPolicyResponse
--
--         , responsePutScalingPolicy $
--             newPutScalingPolicyResponse
--
--         , responseListBuilds $
--             newListBuildsResponse
--
--         , responseDeleteFleet $
--             newDeleteFleetResponse
--
--         , responseCreateBuild $
--             newCreateBuildResponse
--
--         , responseRequestUploadCredentials $
--             newRequestUploadCredentialsResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseListGameServers $
--             newListGameServersResponse
--
--         , responseResolveAlias $
--             newResolveAliasResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRegisterGameServer $
--             newRegisterGameServerResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseUpdateRuntimeConfiguration $
--             newUpdateRuntimeConfigurationResponse
--
--         , responseCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnectionResponse
--
--         , responseListGameServerGroups $
--             newListGameServerGroupsResponse
--
--         , responseCreateGameSessionQueue $
--             newCreateGameSessionQueueResponse
--
--         , responseSearchGameSessions $
--             newSearchGameSessionsResponse
--
--         , responseCreateVpcPeeringAuthorization $
--             newCreateVpcPeeringAuthorizationResponse
--
--         , responseUpdateGameSessionQueue $
--             newUpdateGameSessionQueueResponse
--
--         , responseDeleteGameSessionQueue $
--             newDeleteGameSessionQueueResponse
--
--         , responseCreateGameServerGroup $
--             newCreateGameServerGroupResponse
--
--         , responseDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnectionResponse
--
--         , responseStartFleetActions $
--             newStartFleetActionsResponse
--
--         , responseDeregisterGameServer $
--             newDeregisterGameServerResponse
--
--         , responseGetInstanceAccess $
--             newGetInstanceAccessResponse
--
--         , responseDescribeScalingPolicies $
--             newDescribeScalingPoliciesResponse
--
--         , responseDescribeMatchmakingRuleSets $
--             newDescribeMatchmakingRuleSetsResponse
--
--         , responseDescribeGameSessions $
--             newDescribeGameSessionsResponse
--
--         , responseDescribeGameServer $
--             newDescribeGameServerResponse
--
--         , responseUpdateScript $
--             newUpdateScriptResponse
--
--         , responseDeleteScript $
--             newDeleteScriptResponse
--
--         , responseStartGameSessionPlacement $
--             newStartGameSessionPlacementResponse
--
--         , responseDescribeFleetUtilization $
--             newDescribeFleetUtilizationResponse
--
--         , responseDescribeRuntimeConfiguration $
--             newDescribeRuntimeConfigurationResponse
--
--         , responseGetGameSessionLogUrl $
--             newGetGameSessionLogUrlResponse
--
--         , responseDescribeFleetAttributes $
--             newDescribeFleetAttributesResponse
--
--         , responseDescribeGameSessionPlacement $
--             newDescribeGameSessionPlacementResponse
--
--         , responseDescribeFleetEvents $
--             newDescribeFleetEventsResponse
--
--         , responseCreateFleetLocations $
--             newCreateFleetLocationsResponse
--
--         , responseStartMatchmaking $
--             newStartMatchmakingResponse
--
--         , responseCreateMatchmakingRuleSet $
--             newCreateMatchmakingRuleSetResponse
--
--         , responseDescribeFleetLocationUtilization $
--             newDescribeFleetLocationUtilizationResponse
--
--         , responseDescribeFleetCapacity $
--             newDescribeFleetCapacityResponse
--
--         , responseDeleteBuild $
--             newDeleteBuildResponse
--
--         , responseUpdateBuild $
--             newUpdateBuildResponse
--
--         , responseListFleets $
--             newListFleetsResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseUpdateAlias $
--             newUpdateAliasResponse
--
--         , responseStartMatchBackfill $
--             newStartMatchBackfillResponse
--
--         , responseDescribeInstances $
--             newDescribeInstancesResponse
--
--         , responseDescribeGameSessionDetails $
--             newDescribeGameSessionDetailsResponse
--
--         , responseDescribeFleetPortSettings $
--             newDescribeFleetPortSettingsResponse
--
--         , responseDescribeGameSessionQueues $
--             newDescribeGameSessionQueuesResponse
--
--         , responseDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnectionsResponse
--
--         , responseDescribeScript $
--             newDescribeScriptResponse
--
--         , responseCreatePlayerSessions $
--             newCreatePlayerSessionsResponse
--
--         , responseDescribeMatchmakingConfigurations $
--             newDescribeMatchmakingConfigurationsResponse
--
--         , responseDescribeVpcPeeringAuthorizations $
--             newDescribeVpcPeeringAuthorizationsResponse
--
--         , responseUpdateGameServer $
--             newUpdateGameServerResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseDescribeFleetLocationCapacity $
--             newDescribeFleetLocationCapacityResponse
--
--         , responseDeleteMatchmakingConfiguration $
--             newDeleteMatchmakingConfigurationResponse
--
--         , responseUpdateMatchmakingConfiguration $
--             newUpdateMatchmakingConfigurationResponse
--
--         , responseDeleteGameServerGroup $
--             newDeleteGameServerGroupResponse
--
--         , responseUpdateGameServerGroup $
--             newUpdateGameServerGroupResponse
--
--         , responseResumeGameServerGroup $
--             newResumeGameServerGroupResponse
--
--         , responseDeleteVpcPeeringAuthorization $
--             newDeleteVpcPeeringAuthorizationResponse
--
--         , responseUpdateFleetAttributes $
--             newUpdateFleetAttributesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateMatchmakingConfiguration $
--             newCreateMatchmakingConfigurationResponse
--
--         , responseDescribePlayerSessions $
--             newDescribePlayerSessionsResponse
--
--         , responseStopFleetActions $
--             newStopFleetActionsResponse
--
--         , responseDescribeBuild $
--             newDescribeBuildResponse
--
--         , responseUpdateFleetPortSettings $
--             newUpdateFleetPortSettingsResponse
--
--         , responseUpdateFleetCapacity $
--             newUpdateFleetCapacityResponse
--
--         , responseCreateScript $
--             newCreateScriptResponse
--
--         , responseAcceptMatch $
--             newAcceptMatchResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeAlias $
--             newDescribeAliasResponse
--
--         , responseValidateMatchmakingRuleSet $
--             newValidateMatchmakingRuleSetResponse
--
--         , responseListScripts $
--             newListScriptsResponse
--
--         , responseDescribeEC2InstanceLimits $
--             newDescribeEC2InstanceLimitsResponse
--
--         , responseSuspendGameServerGroup $
--             newSuspendGameServerGroupResponse
--
--         , responseDeleteFleetLocations $
--             newDeleteFleetLocationsResponse
--
--         , responseDeleteMatchmakingRuleSet $
--             newDeleteMatchmakingRuleSetResponse
--
--         , responseStopGameSessionPlacement $
--             newStopGameSessionPlacementResponse
--
--         , responseClaimGameServer $
--             newClaimGameServerResponse
--
--         , responseUpdateGameSession $
--             newUpdateGameSessionResponse
--
--         , responseDescribeFleetLocationAttributes $
--             newDescribeFleetLocationAttributesResponse
--
--         , responseDescribeMatchmaking $
--             newDescribeMatchmakingResponse
--
--         , responseCreatePlayerSession $
--             newCreatePlayerSessionResponse
--
--         , responseDescribeGameServerGroup $
--             newDescribeGameServerGroupResponse
--
--           ]
--     ]

-- Requests

requestStopMatchmaking :: StopMatchmaking -> TestTree
requestStopMatchmaking =
  req
    "StopMatchmaking"
    "fixture/StopMatchmaking.yaml"

requestDescribeGameServerInstances :: DescribeGameServerInstances -> TestTree
requestDescribeGameServerInstances =
  req
    "DescribeGameServerInstances"
    "fixture/DescribeGameServerInstances.yaml"

requestCreateGameSession :: CreateGameSession -> TestTree
requestCreateGameSession =
  req
    "CreateGameSession"
    "fixture/CreateGameSession.yaml"

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

requestListBuilds :: ListBuilds -> TestTree
requestListBuilds =
  req
    "ListBuilds"
    "fixture/ListBuilds.yaml"

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet =
  req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestCreateBuild :: CreateBuild -> TestTree
requestCreateBuild =
  req
    "CreateBuild"
    "fixture/CreateBuild.yaml"

requestRequestUploadCredentials :: RequestUploadCredentials -> TestTree
requestRequestUploadCredentials =
  req
    "RequestUploadCredentials"
    "fixture/RequestUploadCredentials.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestListGameServers :: ListGameServers -> TestTree
requestListGameServers =
  req
    "ListGameServers"
    "fixture/ListGameServers.yaml"

requestResolveAlias :: ResolveAlias -> TestTree
requestResolveAlias =
  req
    "ResolveAlias"
    "fixture/ResolveAlias.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRegisterGameServer :: RegisterGameServer -> TestTree
requestRegisterGameServer =
  req
    "RegisterGameServer"
    "fixture/RegisterGameServer.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestUpdateRuntimeConfiguration :: UpdateRuntimeConfiguration -> TestTree
requestUpdateRuntimeConfiguration =
  req
    "UpdateRuntimeConfiguration"
    "fixture/UpdateRuntimeConfiguration.yaml"

requestCreateVpcPeeringConnection :: CreateVpcPeeringConnection -> TestTree
requestCreateVpcPeeringConnection =
  req
    "CreateVpcPeeringConnection"
    "fixture/CreateVpcPeeringConnection.yaml"

requestListGameServerGroups :: ListGameServerGroups -> TestTree
requestListGameServerGroups =
  req
    "ListGameServerGroups"
    "fixture/ListGameServerGroups.yaml"

requestCreateGameSessionQueue :: CreateGameSessionQueue -> TestTree
requestCreateGameSessionQueue =
  req
    "CreateGameSessionQueue"
    "fixture/CreateGameSessionQueue.yaml"

requestSearchGameSessions :: SearchGameSessions -> TestTree
requestSearchGameSessions =
  req
    "SearchGameSessions"
    "fixture/SearchGameSessions.yaml"

requestCreateVpcPeeringAuthorization :: CreateVpcPeeringAuthorization -> TestTree
requestCreateVpcPeeringAuthorization =
  req
    "CreateVpcPeeringAuthorization"
    "fixture/CreateVpcPeeringAuthorization.yaml"

requestUpdateGameSessionQueue :: UpdateGameSessionQueue -> TestTree
requestUpdateGameSessionQueue =
  req
    "UpdateGameSessionQueue"
    "fixture/UpdateGameSessionQueue.yaml"

requestDeleteGameSessionQueue :: DeleteGameSessionQueue -> TestTree
requestDeleteGameSessionQueue =
  req
    "DeleteGameSessionQueue"
    "fixture/DeleteGameSessionQueue.yaml"

requestCreateGameServerGroup :: CreateGameServerGroup -> TestTree
requestCreateGameServerGroup =
  req
    "CreateGameServerGroup"
    "fixture/CreateGameServerGroup.yaml"

requestDeleteVpcPeeringConnection :: DeleteVpcPeeringConnection -> TestTree
requestDeleteVpcPeeringConnection =
  req
    "DeleteVpcPeeringConnection"
    "fixture/DeleteVpcPeeringConnection.yaml"

requestStartFleetActions :: StartFleetActions -> TestTree
requestStartFleetActions =
  req
    "StartFleetActions"
    "fixture/StartFleetActions.yaml"

requestDeregisterGameServer :: DeregisterGameServer -> TestTree
requestDeregisterGameServer =
  req
    "DeregisterGameServer"
    "fixture/DeregisterGameServer.yaml"

requestGetInstanceAccess :: GetInstanceAccess -> TestTree
requestGetInstanceAccess =
  req
    "GetInstanceAccess"
    "fixture/GetInstanceAccess.yaml"

requestDescribeScalingPolicies :: DescribeScalingPolicies -> TestTree
requestDescribeScalingPolicies =
  req
    "DescribeScalingPolicies"
    "fixture/DescribeScalingPolicies.yaml"

requestDescribeMatchmakingRuleSets :: DescribeMatchmakingRuleSets -> TestTree
requestDescribeMatchmakingRuleSets =
  req
    "DescribeMatchmakingRuleSets"
    "fixture/DescribeMatchmakingRuleSets.yaml"

requestDescribeGameSessions :: DescribeGameSessions -> TestTree
requestDescribeGameSessions =
  req
    "DescribeGameSessions"
    "fixture/DescribeGameSessions.yaml"

requestDescribeGameServer :: DescribeGameServer -> TestTree
requestDescribeGameServer =
  req
    "DescribeGameServer"
    "fixture/DescribeGameServer.yaml"

requestUpdateScript :: UpdateScript -> TestTree
requestUpdateScript =
  req
    "UpdateScript"
    "fixture/UpdateScript.yaml"

requestDeleteScript :: DeleteScript -> TestTree
requestDeleteScript =
  req
    "DeleteScript"
    "fixture/DeleteScript.yaml"

requestStartGameSessionPlacement :: StartGameSessionPlacement -> TestTree
requestStartGameSessionPlacement =
  req
    "StartGameSessionPlacement"
    "fixture/StartGameSessionPlacement.yaml"

requestDescribeFleetUtilization :: DescribeFleetUtilization -> TestTree
requestDescribeFleetUtilization =
  req
    "DescribeFleetUtilization"
    "fixture/DescribeFleetUtilization.yaml"

requestDescribeRuntimeConfiguration :: DescribeRuntimeConfiguration -> TestTree
requestDescribeRuntimeConfiguration =
  req
    "DescribeRuntimeConfiguration"
    "fixture/DescribeRuntimeConfiguration.yaml"

requestGetGameSessionLogUrl :: GetGameSessionLogUrl -> TestTree
requestGetGameSessionLogUrl =
  req
    "GetGameSessionLogUrl"
    "fixture/GetGameSessionLogUrl.yaml"

requestDescribeFleetAttributes :: DescribeFleetAttributes -> TestTree
requestDescribeFleetAttributes =
  req
    "DescribeFleetAttributes"
    "fixture/DescribeFleetAttributes.yaml"

requestDescribeGameSessionPlacement :: DescribeGameSessionPlacement -> TestTree
requestDescribeGameSessionPlacement =
  req
    "DescribeGameSessionPlacement"
    "fixture/DescribeGameSessionPlacement.yaml"

requestDescribeFleetEvents :: DescribeFleetEvents -> TestTree
requestDescribeFleetEvents =
  req
    "DescribeFleetEvents"
    "fixture/DescribeFleetEvents.yaml"

requestCreateFleetLocations :: CreateFleetLocations -> TestTree
requestCreateFleetLocations =
  req
    "CreateFleetLocations"
    "fixture/CreateFleetLocations.yaml"

requestStartMatchmaking :: StartMatchmaking -> TestTree
requestStartMatchmaking =
  req
    "StartMatchmaking"
    "fixture/StartMatchmaking.yaml"

requestCreateMatchmakingRuleSet :: CreateMatchmakingRuleSet -> TestTree
requestCreateMatchmakingRuleSet =
  req
    "CreateMatchmakingRuleSet"
    "fixture/CreateMatchmakingRuleSet.yaml"

requestDescribeFleetLocationUtilization :: DescribeFleetLocationUtilization -> TestTree
requestDescribeFleetLocationUtilization =
  req
    "DescribeFleetLocationUtilization"
    "fixture/DescribeFleetLocationUtilization.yaml"

requestDescribeFleetCapacity :: DescribeFleetCapacity -> TestTree
requestDescribeFleetCapacity =
  req
    "DescribeFleetCapacity"
    "fixture/DescribeFleetCapacity.yaml"

requestDeleteBuild :: DeleteBuild -> TestTree
requestDeleteBuild =
  req
    "DeleteBuild"
    "fixture/DeleteBuild.yaml"

requestUpdateBuild :: UpdateBuild -> TestTree
requestUpdateBuild =
  req
    "UpdateBuild"
    "fixture/UpdateBuild.yaml"

requestListFleets :: ListFleets -> TestTree
requestListFleets =
  req
    "ListFleets"
    "fixture/ListFleets.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestUpdateAlias :: UpdateAlias -> TestTree
requestUpdateAlias =
  req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

requestStartMatchBackfill :: StartMatchBackfill -> TestTree
requestStartMatchBackfill =
  req
    "StartMatchBackfill"
    "fixture/StartMatchBackfill.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances =
  req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestDescribeGameSessionDetails :: DescribeGameSessionDetails -> TestTree
requestDescribeGameSessionDetails =
  req
    "DescribeGameSessionDetails"
    "fixture/DescribeGameSessionDetails.yaml"

requestDescribeFleetPortSettings :: DescribeFleetPortSettings -> TestTree
requestDescribeFleetPortSettings =
  req
    "DescribeFleetPortSettings"
    "fixture/DescribeFleetPortSettings.yaml"

requestDescribeGameSessionQueues :: DescribeGameSessionQueues -> TestTree
requestDescribeGameSessionQueues =
  req
    "DescribeGameSessionQueues"
    "fixture/DescribeGameSessionQueues.yaml"

requestDescribeVpcPeeringConnections :: DescribeVpcPeeringConnections -> TestTree
requestDescribeVpcPeeringConnections =
  req
    "DescribeVpcPeeringConnections"
    "fixture/DescribeVpcPeeringConnections.yaml"

requestDescribeScript :: DescribeScript -> TestTree
requestDescribeScript =
  req
    "DescribeScript"
    "fixture/DescribeScript.yaml"

requestCreatePlayerSessions :: CreatePlayerSessions -> TestTree
requestCreatePlayerSessions =
  req
    "CreatePlayerSessions"
    "fixture/CreatePlayerSessions.yaml"

requestDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurations -> TestTree
requestDescribeMatchmakingConfigurations =
  req
    "DescribeMatchmakingConfigurations"
    "fixture/DescribeMatchmakingConfigurations.yaml"

requestDescribeVpcPeeringAuthorizations :: DescribeVpcPeeringAuthorizations -> TestTree
requestDescribeVpcPeeringAuthorizations =
  req
    "DescribeVpcPeeringAuthorizations"
    "fixture/DescribeVpcPeeringAuthorizations.yaml"

requestUpdateGameServer :: UpdateGameServer -> TestTree
requestUpdateGameServer =
  req
    "UpdateGameServer"
    "fixture/UpdateGameServer.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestDescribeFleetLocationCapacity :: DescribeFleetLocationCapacity -> TestTree
requestDescribeFleetLocationCapacity =
  req
    "DescribeFleetLocationCapacity"
    "fixture/DescribeFleetLocationCapacity.yaml"

requestDeleteMatchmakingConfiguration :: DeleteMatchmakingConfiguration -> TestTree
requestDeleteMatchmakingConfiguration =
  req
    "DeleteMatchmakingConfiguration"
    "fixture/DeleteMatchmakingConfiguration.yaml"

requestUpdateMatchmakingConfiguration :: UpdateMatchmakingConfiguration -> TestTree
requestUpdateMatchmakingConfiguration =
  req
    "UpdateMatchmakingConfiguration"
    "fixture/UpdateMatchmakingConfiguration.yaml"

requestDeleteGameServerGroup :: DeleteGameServerGroup -> TestTree
requestDeleteGameServerGroup =
  req
    "DeleteGameServerGroup"
    "fixture/DeleteGameServerGroup.yaml"

requestUpdateGameServerGroup :: UpdateGameServerGroup -> TestTree
requestUpdateGameServerGroup =
  req
    "UpdateGameServerGroup"
    "fixture/UpdateGameServerGroup.yaml"

requestResumeGameServerGroup :: ResumeGameServerGroup -> TestTree
requestResumeGameServerGroup =
  req
    "ResumeGameServerGroup"
    "fixture/ResumeGameServerGroup.yaml"

requestDeleteVpcPeeringAuthorization :: DeleteVpcPeeringAuthorization -> TestTree
requestDeleteVpcPeeringAuthorization =
  req
    "DeleteVpcPeeringAuthorization"
    "fixture/DeleteVpcPeeringAuthorization.yaml"

requestUpdateFleetAttributes :: UpdateFleetAttributes -> TestTree
requestUpdateFleetAttributes =
  req
    "UpdateFleetAttributes"
    "fixture/UpdateFleetAttributes.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateMatchmakingConfiguration :: CreateMatchmakingConfiguration -> TestTree
requestCreateMatchmakingConfiguration =
  req
    "CreateMatchmakingConfiguration"
    "fixture/CreateMatchmakingConfiguration.yaml"

requestDescribePlayerSessions :: DescribePlayerSessions -> TestTree
requestDescribePlayerSessions =
  req
    "DescribePlayerSessions"
    "fixture/DescribePlayerSessions.yaml"

requestStopFleetActions :: StopFleetActions -> TestTree
requestStopFleetActions =
  req
    "StopFleetActions"
    "fixture/StopFleetActions.yaml"

requestDescribeBuild :: DescribeBuild -> TestTree
requestDescribeBuild =
  req
    "DescribeBuild"
    "fixture/DescribeBuild.yaml"

requestUpdateFleetPortSettings :: UpdateFleetPortSettings -> TestTree
requestUpdateFleetPortSettings =
  req
    "UpdateFleetPortSettings"
    "fixture/UpdateFleetPortSettings.yaml"

requestUpdateFleetCapacity :: UpdateFleetCapacity -> TestTree
requestUpdateFleetCapacity =
  req
    "UpdateFleetCapacity"
    "fixture/UpdateFleetCapacity.yaml"

requestCreateScript :: CreateScript -> TestTree
requestCreateScript =
  req
    "CreateScript"
    "fixture/CreateScript.yaml"

requestAcceptMatch :: AcceptMatch -> TestTree
requestAcceptMatch =
  req
    "AcceptMatch"
    "fixture/AcceptMatch.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeAlias :: DescribeAlias -> TestTree
requestDescribeAlias =
  req
    "DescribeAlias"
    "fixture/DescribeAlias.yaml"

requestValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSet -> TestTree
requestValidateMatchmakingRuleSet =
  req
    "ValidateMatchmakingRuleSet"
    "fixture/ValidateMatchmakingRuleSet.yaml"

requestListScripts :: ListScripts -> TestTree
requestListScripts =
  req
    "ListScripts"
    "fixture/ListScripts.yaml"

requestDescribeEC2InstanceLimits :: DescribeEC2InstanceLimits -> TestTree
requestDescribeEC2InstanceLimits =
  req
    "DescribeEC2InstanceLimits"
    "fixture/DescribeEC2InstanceLimits.yaml"

requestSuspendGameServerGroup :: SuspendGameServerGroup -> TestTree
requestSuspendGameServerGroup =
  req
    "SuspendGameServerGroup"
    "fixture/SuspendGameServerGroup.yaml"

requestDeleteFleetLocations :: DeleteFleetLocations -> TestTree
requestDeleteFleetLocations =
  req
    "DeleteFleetLocations"
    "fixture/DeleteFleetLocations.yaml"

requestDeleteMatchmakingRuleSet :: DeleteMatchmakingRuleSet -> TestTree
requestDeleteMatchmakingRuleSet =
  req
    "DeleteMatchmakingRuleSet"
    "fixture/DeleteMatchmakingRuleSet.yaml"

requestStopGameSessionPlacement :: StopGameSessionPlacement -> TestTree
requestStopGameSessionPlacement =
  req
    "StopGameSessionPlacement"
    "fixture/StopGameSessionPlacement.yaml"

requestClaimGameServer :: ClaimGameServer -> TestTree
requestClaimGameServer =
  req
    "ClaimGameServer"
    "fixture/ClaimGameServer.yaml"

requestUpdateGameSession :: UpdateGameSession -> TestTree
requestUpdateGameSession =
  req
    "UpdateGameSession"
    "fixture/UpdateGameSession.yaml"

requestDescribeFleetLocationAttributes :: DescribeFleetLocationAttributes -> TestTree
requestDescribeFleetLocationAttributes =
  req
    "DescribeFleetLocationAttributes"
    "fixture/DescribeFleetLocationAttributes.yaml"

requestDescribeMatchmaking :: DescribeMatchmaking -> TestTree
requestDescribeMatchmaking =
  req
    "DescribeMatchmaking"
    "fixture/DescribeMatchmaking.yaml"

requestCreatePlayerSession :: CreatePlayerSession -> TestTree
requestCreatePlayerSession =
  req
    "CreatePlayerSession"
    "fixture/CreatePlayerSession.yaml"

requestDescribeGameServerGroup :: DescribeGameServerGroup -> TestTree
requestDescribeGameServerGroup =
  req
    "DescribeGameServerGroup"
    "fixture/DescribeGameServerGroup.yaml"

-- Responses

responseStopMatchmaking :: StopMatchmakingResponse -> TestTree
responseStopMatchmaking =
  res
    "StopMatchmakingResponse"
    "fixture/StopMatchmakingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopMatchmaking)

responseDescribeGameServerInstances :: DescribeGameServerInstancesResponse -> TestTree
responseDescribeGameServerInstances =
  res
    "DescribeGameServerInstancesResponse"
    "fixture/DescribeGameServerInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameServerInstances)

responseCreateGameSession :: CreateGameSessionResponse -> TestTree
responseCreateGameSession =
  res
    "CreateGameSessionResponse"
    "fixture/CreateGameSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGameSession)

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

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds =
  res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuilds)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet =
  res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleet)

responseCreateBuild :: CreateBuildResponse -> TestTree
responseCreateBuild =
  res
    "CreateBuildResponse"
    "fixture/CreateBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBuild)

responseRequestUploadCredentials :: RequestUploadCredentialsResponse -> TestTree
responseRequestUploadCredentials =
  res
    "RequestUploadCredentialsResponse"
    "fixture/RequestUploadCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestUploadCredentials)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlias)

responseListGameServers :: ListGameServersResponse -> TestTree
responseListGameServers =
  res
    "ListGameServersResponse"
    "fixture/ListGameServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGameServers)

responseResolveAlias :: ResolveAliasResponse -> TestTree
responseResolveAlias =
  res
    "ResolveAliasResponse"
    "fixture/ResolveAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResolveAlias)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRegisterGameServer :: RegisterGameServerResponse -> TestTree
responseRegisterGameServer =
  res
    "RegisterGameServerResponse"
    "fixture/RegisterGameServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterGameServer)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAliases)

responseUpdateRuntimeConfiguration :: UpdateRuntimeConfigurationResponse -> TestTree
responseUpdateRuntimeConfiguration =
  res
    "UpdateRuntimeConfigurationResponse"
    "fixture/UpdateRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRuntimeConfiguration)

responseCreateVpcPeeringConnection :: CreateVpcPeeringConnectionResponse -> TestTree
responseCreateVpcPeeringConnection =
  res
    "CreateVpcPeeringConnectionResponse"
    "fixture/CreateVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcPeeringConnection)

responseListGameServerGroups :: ListGameServerGroupsResponse -> TestTree
responseListGameServerGroups =
  res
    "ListGameServerGroupsResponse"
    "fixture/ListGameServerGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGameServerGroups)

responseCreateGameSessionQueue :: CreateGameSessionQueueResponse -> TestTree
responseCreateGameSessionQueue =
  res
    "CreateGameSessionQueueResponse"
    "fixture/CreateGameSessionQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGameSessionQueue)

responseSearchGameSessions :: SearchGameSessionsResponse -> TestTree
responseSearchGameSessions =
  res
    "SearchGameSessionsResponse"
    "fixture/SearchGameSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchGameSessions)

responseCreateVpcPeeringAuthorization :: CreateVpcPeeringAuthorizationResponse -> TestTree
responseCreateVpcPeeringAuthorization =
  res
    "CreateVpcPeeringAuthorizationResponse"
    "fixture/CreateVpcPeeringAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcPeeringAuthorization)

responseUpdateGameSessionQueue :: UpdateGameSessionQueueResponse -> TestTree
responseUpdateGameSessionQueue =
  res
    "UpdateGameSessionQueueResponse"
    "fixture/UpdateGameSessionQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGameSessionQueue)

responseDeleteGameSessionQueue :: DeleteGameSessionQueueResponse -> TestTree
responseDeleteGameSessionQueue =
  res
    "DeleteGameSessionQueueResponse"
    "fixture/DeleteGameSessionQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGameSessionQueue)

responseCreateGameServerGroup :: CreateGameServerGroupResponse -> TestTree
responseCreateGameServerGroup =
  res
    "CreateGameServerGroupResponse"
    "fixture/CreateGameServerGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGameServerGroup)

responseDeleteVpcPeeringConnection :: DeleteVpcPeeringConnectionResponse -> TestTree
responseDeleteVpcPeeringConnection =
  res
    "DeleteVpcPeeringConnectionResponse"
    "fixture/DeleteVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcPeeringConnection)

responseStartFleetActions :: StartFleetActionsResponse -> TestTree
responseStartFleetActions =
  res
    "StartFleetActionsResponse"
    "fixture/StartFleetActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFleetActions)

responseDeregisterGameServer :: DeregisterGameServerResponse -> TestTree
responseDeregisterGameServer =
  res
    "DeregisterGameServerResponse"
    "fixture/DeregisterGameServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterGameServer)

responseGetInstanceAccess :: GetInstanceAccessResponse -> TestTree
responseGetInstanceAccess =
  res
    "GetInstanceAccessResponse"
    "fixture/GetInstanceAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceAccess)

responseDescribeScalingPolicies :: DescribeScalingPoliciesResponse -> TestTree
responseDescribeScalingPolicies =
  res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScalingPolicies)

responseDescribeMatchmakingRuleSets :: DescribeMatchmakingRuleSetsResponse -> TestTree
responseDescribeMatchmakingRuleSets =
  res
    "DescribeMatchmakingRuleSetsResponse"
    "fixture/DescribeMatchmakingRuleSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMatchmakingRuleSets)

responseDescribeGameSessions :: DescribeGameSessionsResponse -> TestTree
responseDescribeGameSessions =
  res
    "DescribeGameSessionsResponse"
    "fixture/DescribeGameSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameSessions)

responseDescribeGameServer :: DescribeGameServerResponse -> TestTree
responseDescribeGameServer =
  res
    "DescribeGameServerResponse"
    "fixture/DescribeGameServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameServer)

responseUpdateScript :: UpdateScriptResponse -> TestTree
responseUpdateScript =
  res
    "UpdateScriptResponse"
    "fixture/UpdateScriptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateScript)

responseDeleteScript :: DeleteScriptResponse -> TestTree
responseDeleteScript =
  res
    "DeleteScriptResponse"
    "fixture/DeleteScriptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScript)

responseStartGameSessionPlacement :: StartGameSessionPlacementResponse -> TestTree
responseStartGameSessionPlacement =
  res
    "StartGameSessionPlacementResponse"
    "fixture/StartGameSessionPlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartGameSessionPlacement)

responseDescribeFleetUtilization :: DescribeFleetUtilizationResponse -> TestTree
responseDescribeFleetUtilization =
  res
    "DescribeFleetUtilizationResponse"
    "fixture/DescribeFleetUtilizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetUtilization)

responseDescribeRuntimeConfiguration :: DescribeRuntimeConfigurationResponse -> TestTree
responseDescribeRuntimeConfiguration =
  res
    "DescribeRuntimeConfigurationResponse"
    "fixture/DescribeRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRuntimeConfiguration)

responseGetGameSessionLogUrl :: GetGameSessionLogUrlResponse -> TestTree
responseGetGameSessionLogUrl =
  res
    "GetGameSessionLogUrlResponse"
    "fixture/GetGameSessionLogUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGameSessionLogUrl)

responseDescribeFleetAttributes :: DescribeFleetAttributesResponse -> TestTree
responseDescribeFleetAttributes =
  res
    "DescribeFleetAttributesResponse"
    "fixture/DescribeFleetAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetAttributes)

responseDescribeGameSessionPlacement :: DescribeGameSessionPlacementResponse -> TestTree
responseDescribeGameSessionPlacement =
  res
    "DescribeGameSessionPlacementResponse"
    "fixture/DescribeGameSessionPlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameSessionPlacement)

responseDescribeFleetEvents :: DescribeFleetEventsResponse -> TestTree
responseDescribeFleetEvents =
  res
    "DescribeFleetEventsResponse"
    "fixture/DescribeFleetEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetEvents)

responseCreateFleetLocations :: CreateFleetLocationsResponse -> TestTree
responseCreateFleetLocations =
  res
    "CreateFleetLocationsResponse"
    "fixture/CreateFleetLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleetLocations)

responseStartMatchmaking :: StartMatchmakingResponse -> TestTree
responseStartMatchmaking =
  res
    "StartMatchmakingResponse"
    "fixture/StartMatchmakingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMatchmaking)

responseCreateMatchmakingRuleSet :: CreateMatchmakingRuleSetResponse -> TestTree
responseCreateMatchmakingRuleSet =
  res
    "CreateMatchmakingRuleSetResponse"
    "fixture/CreateMatchmakingRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMatchmakingRuleSet)

responseDescribeFleetLocationUtilization :: DescribeFleetLocationUtilizationResponse -> TestTree
responseDescribeFleetLocationUtilization =
  res
    "DescribeFleetLocationUtilizationResponse"
    "fixture/DescribeFleetLocationUtilizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetLocationUtilization)

responseDescribeFleetCapacity :: DescribeFleetCapacityResponse -> TestTree
responseDescribeFleetCapacity =
  res
    "DescribeFleetCapacityResponse"
    "fixture/DescribeFleetCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetCapacity)

responseDeleteBuild :: DeleteBuildResponse -> TestTree
responseDeleteBuild =
  res
    "DeleteBuildResponse"
    "fixture/DeleteBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBuild)

responseUpdateBuild :: UpdateBuildResponse -> TestTree
responseUpdateBuild =
  res
    "UpdateBuildResponse"
    "fixture/UpdateBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBuild)

responseListFleets :: ListFleetsResponse -> TestTree
responseListFleets =
  res
    "ListFleetsResponse"
    "fixture/ListFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFleets)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlias)

responseUpdateAlias :: UpdateAliasResponse -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAlias)

responseStartMatchBackfill :: StartMatchBackfillResponse -> TestTree
responseStartMatchBackfill =
  res
    "StartMatchBackfillResponse"
    "fixture/StartMatchBackfillResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMatchBackfill)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstances)

responseDescribeGameSessionDetails :: DescribeGameSessionDetailsResponse -> TestTree
responseDescribeGameSessionDetails =
  res
    "DescribeGameSessionDetailsResponse"
    "fixture/DescribeGameSessionDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameSessionDetails)

responseDescribeFleetPortSettings :: DescribeFleetPortSettingsResponse -> TestTree
responseDescribeFleetPortSettings =
  res
    "DescribeFleetPortSettingsResponse"
    "fixture/DescribeFleetPortSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetPortSettings)

responseDescribeGameSessionQueues :: DescribeGameSessionQueuesResponse -> TestTree
responseDescribeGameSessionQueues =
  res
    "DescribeGameSessionQueuesResponse"
    "fixture/DescribeGameSessionQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameSessionQueues)

responseDescribeVpcPeeringConnections :: DescribeVpcPeeringConnectionsResponse -> TestTree
responseDescribeVpcPeeringConnections =
  res
    "DescribeVpcPeeringConnectionsResponse"
    "fixture/DescribeVpcPeeringConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcPeeringConnections)

responseDescribeScript :: DescribeScriptResponse -> TestTree
responseDescribeScript =
  res
    "DescribeScriptResponse"
    "fixture/DescribeScriptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScript)

responseCreatePlayerSessions :: CreatePlayerSessionsResponse -> TestTree
responseCreatePlayerSessions =
  res
    "CreatePlayerSessionsResponse"
    "fixture/CreatePlayerSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlayerSessions)

responseDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurationsResponse -> TestTree
responseDescribeMatchmakingConfigurations =
  res
    "DescribeMatchmakingConfigurationsResponse"
    "fixture/DescribeMatchmakingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMatchmakingConfigurations)

responseDescribeVpcPeeringAuthorizations :: DescribeVpcPeeringAuthorizationsResponse -> TestTree
responseDescribeVpcPeeringAuthorizations =
  res
    "DescribeVpcPeeringAuthorizationsResponse"
    "fixture/DescribeVpcPeeringAuthorizationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcPeeringAuthorizations)

responseUpdateGameServer :: UpdateGameServerResponse -> TestTree
responseUpdateGameServer =
  res
    "UpdateGameServerResponse"
    "fixture/UpdateGameServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGameServer)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleet)

responseDescribeFleetLocationCapacity :: DescribeFleetLocationCapacityResponse -> TestTree
responseDescribeFleetLocationCapacity =
  res
    "DescribeFleetLocationCapacityResponse"
    "fixture/DescribeFleetLocationCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetLocationCapacity)

responseDeleteMatchmakingConfiguration :: DeleteMatchmakingConfigurationResponse -> TestTree
responseDeleteMatchmakingConfiguration =
  res
    "DeleteMatchmakingConfigurationResponse"
    "fixture/DeleteMatchmakingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMatchmakingConfiguration)

responseUpdateMatchmakingConfiguration :: UpdateMatchmakingConfigurationResponse -> TestTree
responseUpdateMatchmakingConfiguration =
  res
    "UpdateMatchmakingConfigurationResponse"
    "fixture/UpdateMatchmakingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMatchmakingConfiguration)

responseDeleteGameServerGroup :: DeleteGameServerGroupResponse -> TestTree
responseDeleteGameServerGroup =
  res
    "DeleteGameServerGroupResponse"
    "fixture/DeleteGameServerGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGameServerGroup)

responseUpdateGameServerGroup :: UpdateGameServerGroupResponse -> TestTree
responseUpdateGameServerGroup =
  res
    "UpdateGameServerGroupResponse"
    "fixture/UpdateGameServerGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGameServerGroup)

responseResumeGameServerGroup :: ResumeGameServerGroupResponse -> TestTree
responseResumeGameServerGroup =
  res
    "ResumeGameServerGroupResponse"
    "fixture/ResumeGameServerGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeGameServerGroup)

responseDeleteVpcPeeringAuthorization :: DeleteVpcPeeringAuthorizationResponse -> TestTree
responseDeleteVpcPeeringAuthorization =
  res
    "DeleteVpcPeeringAuthorizationResponse"
    "fixture/DeleteVpcPeeringAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcPeeringAuthorization)

responseUpdateFleetAttributes :: UpdateFleetAttributesResponse -> TestTree
responseUpdateFleetAttributes =
  res
    "UpdateFleetAttributesResponse"
    "fixture/UpdateFleetAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFleetAttributes)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCreateMatchmakingConfiguration :: CreateMatchmakingConfigurationResponse -> TestTree
responseCreateMatchmakingConfiguration =
  res
    "CreateMatchmakingConfigurationResponse"
    "fixture/CreateMatchmakingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMatchmakingConfiguration)

responseDescribePlayerSessions :: DescribePlayerSessionsResponse -> TestTree
responseDescribePlayerSessions =
  res
    "DescribePlayerSessionsResponse"
    "fixture/DescribePlayerSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePlayerSessions)

responseStopFleetActions :: StopFleetActionsResponse -> TestTree
responseStopFleetActions =
  res
    "StopFleetActionsResponse"
    "fixture/StopFleetActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopFleetActions)

responseDescribeBuild :: DescribeBuildResponse -> TestTree
responseDescribeBuild =
  res
    "DescribeBuildResponse"
    "fixture/DescribeBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBuild)

responseUpdateFleetPortSettings :: UpdateFleetPortSettingsResponse -> TestTree
responseUpdateFleetPortSettings =
  res
    "UpdateFleetPortSettingsResponse"
    "fixture/UpdateFleetPortSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFleetPortSettings)

responseUpdateFleetCapacity :: UpdateFleetCapacityResponse -> TestTree
responseUpdateFleetCapacity =
  res
    "UpdateFleetCapacityResponse"
    "fixture/UpdateFleetCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFleetCapacity)

responseCreateScript :: CreateScriptResponse -> TestTree
responseCreateScript =
  res
    "CreateScriptResponse"
    "fixture/CreateScriptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateScript)

responseAcceptMatch :: AcceptMatchResponse -> TestTree
responseAcceptMatch =
  res
    "AcceptMatchResponse"
    "fixture/AcceptMatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptMatch)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDescribeAlias :: DescribeAliasResponse -> TestTree
responseDescribeAlias =
  res
    "DescribeAliasResponse"
    "fixture/DescribeAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlias)

responseValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSetResponse -> TestTree
responseValidateMatchmakingRuleSet =
  res
    "ValidateMatchmakingRuleSetResponse"
    "fixture/ValidateMatchmakingRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateMatchmakingRuleSet)

responseListScripts :: ListScriptsResponse -> TestTree
responseListScripts =
  res
    "ListScriptsResponse"
    "fixture/ListScriptsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListScripts)

responseDescribeEC2InstanceLimits :: DescribeEC2InstanceLimitsResponse -> TestTree
responseDescribeEC2InstanceLimits =
  res
    "DescribeEC2InstanceLimitsResponse"
    "fixture/DescribeEC2InstanceLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEC2InstanceLimits)

responseSuspendGameServerGroup :: SuspendGameServerGroupResponse -> TestTree
responseSuspendGameServerGroup =
  res
    "SuspendGameServerGroupResponse"
    "fixture/SuspendGameServerGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SuspendGameServerGroup)

responseDeleteFleetLocations :: DeleteFleetLocationsResponse -> TestTree
responseDeleteFleetLocations =
  res
    "DeleteFleetLocationsResponse"
    "fixture/DeleteFleetLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleetLocations)

responseDeleteMatchmakingRuleSet :: DeleteMatchmakingRuleSetResponse -> TestTree
responseDeleteMatchmakingRuleSet =
  res
    "DeleteMatchmakingRuleSetResponse"
    "fixture/DeleteMatchmakingRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMatchmakingRuleSet)

responseStopGameSessionPlacement :: StopGameSessionPlacementResponse -> TestTree
responseStopGameSessionPlacement =
  res
    "StopGameSessionPlacementResponse"
    "fixture/StopGameSessionPlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopGameSessionPlacement)

responseClaimGameServer :: ClaimGameServerResponse -> TestTree
responseClaimGameServer =
  res
    "ClaimGameServerResponse"
    "fixture/ClaimGameServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClaimGameServer)

responseUpdateGameSession :: UpdateGameSessionResponse -> TestTree
responseUpdateGameSession =
  res
    "UpdateGameSessionResponse"
    "fixture/UpdateGameSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGameSession)

responseDescribeFleetLocationAttributes :: DescribeFleetLocationAttributesResponse -> TestTree
responseDescribeFleetLocationAttributes =
  res
    "DescribeFleetLocationAttributesResponse"
    "fixture/DescribeFleetLocationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetLocationAttributes)

responseDescribeMatchmaking :: DescribeMatchmakingResponse -> TestTree
responseDescribeMatchmaking =
  res
    "DescribeMatchmakingResponse"
    "fixture/DescribeMatchmakingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMatchmaking)

responseCreatePlayerSession :: CreatePlayerSessionResponse -> TestTree
responseCreatePlayerSession =
  res
    "CreatePlayerSessionResponse"
    "fixture/CreatePlayerSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlayerSession)

responseDescribeGameServerGroup :: DescribeGameServerGroupResponse -> TestTree
responseDescribeGameServerGroup =
  res
    "DescribeGameServerGroupResponse"
    "fixture/DescribeGameServerGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameServerGroup)
