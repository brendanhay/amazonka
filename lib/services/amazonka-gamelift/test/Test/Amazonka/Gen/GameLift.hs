{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.GameLift
-- Copyright   : (c) 2013-2022 Brendan Hay
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
--         [ requestAcceptMatch $
--             newAcceptMatch
--
--         , requestClaimGameServer $
--             newClaimGameServer
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestCreateBuild $
--             newCreateBuild
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestCreateFleetLocations $
--             newCreateFleetLocations
--
--         , requestCreateGameServerGroup $
--             newCreateGameServerGroup
--
--         , requestCreateGameSession $
--             newCreateGameSession
--
--         , requestCreateGameSessionQueue $
--             newCreateGameSessionQueue
--
--         , requestCreateLocation $
--             newCreateLocation
--
--         , requestCreateMatchmakingConfiguration $
--             newCreateMatchmakingConfiguration
--
--         , requestCreateMatchmakingRuleSet $
--             newCreateMatchmakingRuleSet
--
--         , requestCreatePlayerSession $
--             newCreatePlayerSession
--
--         , requestCreatePlayerSessions $
--             newCreatePlayerSessions
--
--         , requestCreateScript $
--             newCreateScript
--
--         , requestCreateVpcPeeringAuthorization $
--             newCreateVpcPeeringAuthorization
--
--         , requestCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnection
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestDeleteBuild $
--             newDeleteBuild
--
--         , requestDeleteFleet $
--             newDeleteFleet
--
--         , requestDeleteFleetLocations $
--             newDeleteFleetLocations
--
--         , requestDeleteGameServerGroup $
--             newDeleteGameServerGroup
--
--         , requestDeleteGameSessionQueue $
--             newDeleteGameSessionQueue
--
--         , requestDeleteLocation $
--             newDeleteLocation
--
--         , requestDeleteMatchmakingConfiguration $
--             newDeleteMatchmakingConfiguration
--
--         , requestDeleteMatchmakingRuleSet $
--             newDeleteMatchmakingRuleSet
--
--         , requestDeleteScalingPolicy $
--             newDeleteScalingPolicy
--
--         , requestDeleteScript $
--             newDeleteScript
--
--         , requestDeleteVpcPeeringAuthorization $
--             newDeleteVpcPeeringAuthorization
--
--         , requestDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnection
--
--         , requestDeregisterCompute $
--             newDeregisterCompute
--
--         , requestDeregisterGameServer $
--             newDeregisterGameServer
--
--         , requestDescribeAlias $
--             newDescribeAlias
--
--         , requestDescribeBuild $
--             newDescribeBuild
--
--         , requestDescribeCompute $
--             newDescribeCompute
--
--         , requestDescribeEC2InstanceLimits $
--             newDescribeEC2InstanceLimits
--
--         , requestDescribeFleetAttributes $
--             newDescribeFleetAttributes
--
--         , requestDescribeFleetCapacity $
--             newDescribeFleetCapacity
--
--         , requestDescribeFleetEvents $
--             newDescribeFleetEvents
--
--         , requestDescribeFleetLocationAttributes $
--             newDescribeFleetLocationAttributes
--
--         , requestDescribeFleetLocationCapacity $
--             newDescribeFleetLocationCapacity
--
--         , requestDescribeFleetLocationUtilization $
--             newDescribeFleetLocationUtilization
--
--         , requestDescribeFleetPortSettings $
--             newDescribeFleetPortSettings
--
--         , requestDescribeFleetUtilization $
--             newDescribeFleetUtilization
--
--         , requestDescribeGameServer $
--             newDescribeGameServer
--
--         , requestDescribeGameServerGroup $
--             newDescribeGameServerGroup
--
--         , requestDescribeGameServerInstances $
--             newDescribeGameServerInstances
--
--         , requestDescribeGameSessionDetails $
--             newDescribeGameSessionDetails
--
--         , requestDescribeGameSessionPlacement $
--             newDescribeGameSessionPlacement
--
--         , requestDescribeGameSessionQueues $
--             newDescribeGameSessionQueues
--
--         , requestDescribeGameSessions $
--             newDescribeGameSessions
--
--         , requestDescribeInstances $
--             newDescribeInstances
--
--         , requestDescribeMatchmaking $
--             newDescribeMatchmaking
--
--         , requestDescribeMatchmakingConfigurations $
--             newDescribeMatchmakingConfigurations
--
--         , requestDescribeMatchmakingRuleSets $
--             newDescribeMatchmakingRuleSets
--
--         , requestDescribePlayerSessions $
--             newDescribePlayerSessions
--
--         , requestDescribeRuntimeConfiguration $
--             newDescribeRuntimeConfiguration
--
--         , requestDescribeScalingPolicies $
--             newDescribeScalingPolicies
--
--         , requestDescribeScript $
--             newDescribeScript
--
--         , requestDescribeVpcPeeringAuthorizations $
--             newDescribeVpcPeeringAuthorizations
--
--         , requestDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnections
--
--         , requestGetComputeAccess $
--             newGetComputeAccess
--
--         , requestGetComputeAuthToken $
--             newGetComputeAuthToken
--
--         , requestGetGameSessionLogUrl $
--             newGetGameSessionLogUrl
--
--         , requestGetInstanceAccess $
--             newGetInstanceAccess
--
--         , requestListAliases $
--             newListAliases
--
--         , requestListBuilds $
--             newListBuilds
--
--         , requestListCompute $
--             newListCompute
--
--         , requestListFleets $
--             newListFleets
--
--         , requestListGameServerGroups $
--             newListGameServerGroups
--
--         , requestListGameServers $
--             newListGameServers
--
--         , requestListLocations $
--             newListLocations
--
--         , requestListScripts $
--             newListScripts
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutScalingPolicy $
--             newPutScalingPolicy
--
--         , requestRegisterCompute $
--             newRegisterCompute
--
--         , requestRegisterGameServer $
--             newRegisterGameServer
--
--         , requestRequestUploadCredentials $
--             newRequestUploadCredentials
--
--         , requestResolveAlias $
--             newResolveAlias
--
--         , requestResumeGameServerGroup $
--             newResumeGameServerGroup
--
--         , requestSearchGameSessions $
--             newSearchGameSessions
--
--         , requestStartFleetActions $
--             newStartFleetActions
--
--         , requestStartGameSessionPlacement $
--             newStartGameSessionPlacement
--
--         , requestStartMatchBackfill $
--             newStartMatchBackfill
--
--         , requestStartMatchmaking $
--             newStartMatchmaking
--
--         , requestStopFleetActions $
--             newStopFleetActions
--
--         , requestStopGameSessionPlacement $
--             newStopGameSessionPlacement
--
--         , requestStopMatchmaking $
--             newStopMatchmaking
--
--         , requestSuspendGameServerGroup $
--             newSuspendGameServerGroup
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAlias $
--             newUpdateAlias
--
--         , requestUpdateBuild $
--             newUpdateBuild
--
--         , requestUpdateFleetAttributes $
--             newUpdateFleetAttributes
--
--         , requestUpdateFleetCapacity $
--             newUpdateFleetCapacity
--
--         , requestUpdateFleetPortSettings $
--             newUpdateFleetPortSettings
--
--         , requestUpdateGameServer $
--             newUpdateGameServer
--
--         , requestUpdateGameServerGroup $
--             newUpdateGameServerGroup
--
--         , requestUpdateGameSession $
--             newUpdateGameSession
--
--         , requestUpdateGameSessionQueue $
--             newUpdateGameSessionQueue
--
--         , requestUpdateMatchmakingConfiguration $
--             newUpdateMatchmakingConfiguration
--
--         , requestUpdateRuntimeConfiguration $
--             newUpdateRuntimeConfiguration
--
--         , requestUpdateScript $
--             newUpdateScript
--
--         , requestValidateMatchmakingRuleSet $
--             newValidateMatchmakingRuleSet
--
--           ]

--     , testGroup "response"
--         [ responseAcceptMatch $
--             newAcceptMatchResponse
--
--         , responseClaimGameServer $
--             newClaimGameServerResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseCreateBuild $
--             newCreateBuildResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseCreateFleetLocations $
--             newCreateFleetLocationsResponse
--
--         , responseCreateGameServerGroup $
--             newCreateGameServerGroupResponse
--
--         , responseCreateGameSession $
--             newCreateGameSessionResponse
--
--         , responseCreateGameSessionQueue $
--             newCreateGameSessionQueueResponse
--
--         , responseCreateLocation $
--             newCreateLocationResponse
--
--         , responseCreateMatchmakingConfiguration $
--             newCreateMatchmakingConfigurationResponse
--
--         , responseCreateMatchmakingRuleSet $
--             newCreateMatchmakingRuleSetResponse
--
--         , responseCreatePlayerSession $
--             newCreatePlayerSessionResponse
--
--         , responseCreatePlayerSessions $
--             newCreatePlayerSessionsResponse
--
--         , responseCreateScript $
--             newCreateScriptResponse
--
--         , responseCreateVpcPeeringAuthorization $
--             newCreateVpcPeeringAuthorizationResponse
--
--         , responseCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnectionResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseDeleteBuild $
--             newDeleteBuildResponse
--
--         , responseDeleteFleet $
--             newDeleteFleetResponse
--
--         , responseDeleteFleetLocations $
--             newDeleteFleetLocationsResponse
--
--         , responseDeleteGameServerGroup $
--             newDeleteGameServerGroupResponse
--
--         , responseDeleteGameSessionQueue $
--             newDeleteGameSessionQueueResponse
--
--         , responseDeleteLocation $
--             newDeleteLocationResponse
--
--         , responseDeleteMatchmakingConfiguration $
--             newDeleteMatchmakingConfigurationResponse
--
--         , responseDeleteMatchmakingRuleSet $
--             newDeleteMatchmakingRuleSetResponse
--
--         , responseDeleteScalingPolicy $
--             newDeleteScalingPolicyResponse
--
--         , responseDeleteScript $
--             newDeleteScriptResponse
--
--         , responseDeleteVpcPeeringAuthorization $
--             newDeleteVpcPeeringAuthorizationResponse
--
--         , responseDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnectionResponse
--
--         , responseDeregisterCompute $
--             newDeregisterComputeResponse
--
--         , responseDeregisterGameServer $
--             newDeregisterGameServerResponse
--
--         , responseDescribeAlias $
--             newDescribeAliasResponse
--
--         , responseDescribeBuild $
--             newDescribeBuildResponse
--
--         , responseDescribeCompute $
--             newDescribeComputeResponse
--
--         , responseDescribeEC2InstanceLimits $
--             newDescribeEC2InstanceLimitsResponse
--
--         , responseDescribeFleetAttributes $
--             newDescribeFleetAttributesResponse
--
--         , responseDescribeFleetCapacity $
--             newDescribeFleetCapacityResponse
--
--         , responseDescribeFleetEvents $
--             newDescribeFleetEventsResponse
--
--         , responseDescribeFleetLocationAttributes $
--             newDescribeFleetLocationAttributesResponse
--
--         , responseDescribeFleetLocationCapacity $
--             newDescribeFleetLocationCapacityResponse
--
--         , responseDescribeFleetLocationUtilization $
--             newDescribeFleetLocationUtilizationResponse
--
--         , responseDescribeFleetPortSettings $
--             newDescribeFleetPortSettingsResponse
--
--         , responseDescribeFleetUtilization $
--             newDescribeFleetUtilizationResponse
--
--         , responseDescribeGameServer $
--             newDescribeGameServerResponse
--
--         , responseDescribeGameServerGroup $
--             newDescribeGameServerGroupResponse
--
--         , responseDescribeGameServerInstances $
--             newDescribeGameServerInstancesResponse
--
--         , responseDescribeGameSessionDetails $
--             newDescribeGameSessionDetailsResponse
--
--         , responseDescribeGameSessionPlacement $
--             newDescribeGameSessionPlacementResponse
--
--         , responseDescribeGameSessionQueues $
--             newDescribeGameSessionQueuesResponse
--
--         , responseDescribeGameSessions $
--             newDescribeGameSessionsResponse
--
--         , responseDescribeInstances $
--             newDescribeInstancesResponse
--
--         , responseDescribeMatchmaking $
--             newDescribeMatchmakingResponse
--
--         , responseDescribeMatchmakingConfigurations $
--             newDescribeMatchmakingConfigurationsResponse
--
--         , responseDescribeMatchmakingRuleSets $
--             newDescribeMatchmakingRuleSetsResponse
--
--         , responseDescribePlayerSessions $
--             newDescribePlayerSessionsResponse
--
--         , responseDescribeRuntimeConfiguration $
--             newDescribeRuntimeConfigurationResponse
--
--         , responseDescribeScalingPolicies $
--             newDescribeScalingPoliciesResponse
--
--         , responseDescribeScript $
--             newDescribeScriptResponse
--
--         , responseDescribeVpcPeeringAuthorizations $
--             newDescribeVpcPeeringAuthorizationsResponse
--
--         , responseDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnectionsResponse
--
--         , responseGetComputeAccess $
--             newGetComputeAccessResponse
--
--         , responseGetComputeAuthToken $
--             newGetComputeAuthTokenResponse
--
--         , responseGetGameSessionLogUrl $
--             newGetGameSessionLogUrlResponse
--
--         , responseGetInstanceAccess $
--             newGetInstanceAccessResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseListBuilds $
--             newListBuildsResponse
--
--         , responseListCompute $
--             newListComputeResponse
--
--         , responseListFleets $
--             newListFleetsResponse
--
--         , responseListGameServerGroups $
--             newListGameServerGroupsResponse
--
--         , responseListGameServers $
--             newListGameServersResponse
--
--         , responseListLocations $
--             newListLocationsResponse
--
--         , responseListScripts $
--             newListScriptsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutScalingPolicy $
--             newPutScalingPolicyResponse
--
--         , responseRegisterCompute $
--             newRegisterComputeResponse
--
--         , responseRegisterGameServer $
--             newRegisterGameServerResponse
--
--         , responseRequestUploadCredentials $
--             newRequestUploadCredentialsResponse
--
--         , responseResolveAlias $
--             newResolveAliasResponse
--
--         , responseResumeGameServerGroup $
--             newResumeGameServerGroupResponse
--
--         , responseSearchGameSessions $
--             newSearchGameSessionsResponse
--
--         , responseStartFleetActions $
--             newStartFleetActionsResponse
--
--         , responseStartGameSessionPlacement $
--             newStartGameSessionPlacementResponse
--
--         , responseStartMatchBackfill $
--             newStartMatchBackfillResponse
--
--         , responseStartMatchmaking $
--             newStartMatchmakingResponse
--
--         , responseStopFleetActions $
--             newStopFleetActionsResponse
--
--         , responseStopGameSessionPlacement $
--             newStopGameSessionPlacementResponse
--
--         , responseStopMatchmaking $
--             newStopMatchmakingResponse
--
--         , responseSuspendGameServerGroup $
--             newSuspendGameServerGroupResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAlias $
--             newUpdateAliasResponse
--
--         , responseUpdateBuild $
--             newUpdateBuildResponse
--
--         , responseUpdateFleetAttributes $
--             newUpdateFleetAttributesResponse
--
--         , responseUpdateFleetCapacity $
--             newUpdateFleetCapacityResponse
--
--         , responseUpdateFleetPortSettings $
--             newUpdateFleetPortSettingsResponse
--
--         , responseUpdateGameServer $
--             newUpdateGameServerResponse
--
--         , responseUpdateGameServerGroup $
--             newUpdateGameServerGroupResponse
--
--         , responseUpdateGameSession $
--             newUpdateGameSessionResponse
--
--         , responseUpdateGameSessionQueue $
--             newUpdateGameSessionQueueResponse
--
--         , responseUpdateMatchmakingConfiguration $
--             newUpdateMatchmakingConfigurationResponse
--
--         , responseUpdateRuntimeConfiguration $
--             newUpdateRuntimeConfigurationResponse
--
--         , responseUpdateScript $
--             newUpdateScriptResponse
--
--         , responseValidateMatchmakingRuleSet $
--             newValidateMatchmakingRuleSetResponse
--
--           ]
--     ]

-- Requests

requestAcceptMatch :: AcceptMatch -> TestTree
requestAcceptMatch =
  req
    "AcceptMatch"
    "fixture/AcceptMatch.yaml"

requestClaimGameServer :: ClaimGameServer -> TestTree
requestClaimGameServer =
  req
    "ClaimGameServer"
    "fixture/ClaimGameServer.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestCreateBuild :: CreateBuild -> TestTree
requestCreateBuild =
  req
    "CreateBuild"
    "fixture/CreateBuild.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestCreateFleetLocations :: CreateFleetLocations -> TestTree
requestCreateFleetLocations =
  req
    "CreateFleetLocations"
    "fixture/CreateFleetLocations.yaml"

requestCreateGameServerGroup :: CreateGameServerGroup -> TestTree
requestCreateGameServerGroup =
  req
    "CreateGameServerGroup"
    "fixture/CreateGameServerGroup.yaml"

requestCreateGameSession :: CreateGameSession -> TestTree
requestCreateGameSession =
  req
    "CreateGameSession"
    "fixture/CreateGameSession.yaml"

requestCreateGameSessionQueue :: CreateGameSessionQueue -> TestTree
requestCreateGameSessionQueue =
  req
    "CreateGameSessionQueue"
    "fixture/CreateGameSessionQueue.yaml"

requestCreateLocation :: CreateLocation -> TestTree
requestCreateLocation =
  req
    "CreateLocation"
    "fixture/CreateLocation.yaml"

requestCreateMatchmakingConfiguration :: CreateMatchmakingConfiguration -> TestTree
requestCreateMatchmakingConfiguration =
  req
    "CreateMatchmakingConfiguration"
    "fixture/CreateMatchmakingConfiguration.yaml"

requestCreateMatchmakingRuleSet :: CreateMatchmakingRuleSet -> TestTree
requestCreateMatchmakingRuleSet =
  req
    "CreateMatchmakingRuleSet"
    "fixture/CreateMatchmakingRuleSet.yaml"

requestCreatePlayerSession :: CreatePlayerSession -> TestTree
requestCreatePlayerSession =
  req
    "CreatePlayerSession"
    "fixture/CreatePlayerSession.yaml"

requestCreatePlayerSessions :: CreatePlayerSessions -> TestTree
requestCreatePlayerSessions =
  req
    "CreatePlayerSessions"
    "fixture/CreatePlayerSessions.yaml"

requestCreateScript :: CreateScript -> TestTree
requestCreateScript =
  req
    "CreateScript"
    "fixture/CreateScript.yaml"

requestCreateVpcPeeringAuthorization :: CreateVpcPeeringAuthorization -> TestTree
requestCreateVpcPeeringAuthorization =
  req
    "CreateVpcPeeringAuthorization"
    "fixture/CreateVpcPeeringAuthorization.yaml"

requestCreateVpcPeeringConnection :: CreateVpcPeeringConnection -> TestTree
requestCreateVpcPeeringConnection =
  req
    "CreateVpcPeeringConnection"
    "fixture/CreateVpcPeeringConnection.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestDeleteBuild :: DeleteBuild -> TestTree
requestDeleteBuild =
  req
    "DeleteBuild"
    "fixture/DeleteBuild.yaml"

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet =
  req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestDeleteFleetLocations :: DeleteFleetLocations -> TestTree
requestDeleteFleetLocations =
  req
    "DeleteFleetLocations"
    "fixture/DeleteFleetLocations.yaml"

requestDeleteGameServerGroup :: DeleteGameServerGroup -> TestTree
requestDeleteGameServerGroup =
  req
    "DeleteGameServerGroup"
    "fixture/DeleteGameServerGroup.yaml"

requestDeleteGameSessionQueue :: DeleteGameSessionQueue -> TestTree
requestDeleteGameSessionQueue =
  req
    "DeleteGameSessionQueue"
    "fixture/DeleteGameSessionQueue.yaml"

requestDeleteLocation :: DeleteLocation -> TestTree
requestDeleteLocation =
  req
    "DeleteLocation"
    "fixture/DeleteLocation.yaml"

requestDeleteMatchmakingConfiguration :: DeleteMatchmakingConfiguration -> TestTree
requestDeleteMatchmakingConfiguration =
  req
    "DeleteMatchmakingConfiguration"
    "fixture/DeleteMatchmakingConfiguration.yaml"

requestDeleteMatchmakingRuleSet :: DeleteMatchmakingRuleSet -> TestTree
requestDeleteMatchmakingRuleSet =
  req
    "DeleteMatchmakingRuleSet"
    "fixture/DeleteMatchmakingRuleSet.yaml"

requestDeleteScalingPolicy :: DeleteScalingPolicy -> TestTree
requestDeleteScalingPolicy =
  req
    "DeleteScalingPolicy"
    "fixture/DeleteScalingPolicy.yaml"

requestDeleteScript :: DeleteScript -> TestTree
requestDeleteScript =
  req
    "DeleteScript"
    "fixture/DeleteScript.yaml"

requestDeleteVpcPeeringAuthorization :: DeleteVpcPeeringAuthorization -> TestTree
requestDeleteVpcPeeringAuthorization =
  req
    "DeleteVpcPeeringAuthorization"
    "fixture/DeleteVpcPeeringAuthorization.yaml"

requestDeleteVpcPeeringConnection :: DeleteVpcPeeringConnection -> TestTree
requestDeleteVpcPeeringConnection =
  req
    "DeleteVpcPeeringConnection"
    "fixture/DeleteVpcPeeringConnection.yaml"

requestDeregisterCompute :: DeregisterCompute -> TestTree
requestDeregisterCompute =
  req
    "DeregisterCompute"
    "fixture/DeregisterCompute.yaml"

requestDeregisterGameServer :: DeregisterGameServer -> TestTree
requestDeregisterGameServer =
  req
    "DeregisterGameServer"
    "fixture/DeregisterGameServer.yaml"

requestDescribeAlias :: DescribeAlias -> TestTree
requestDescribeAlias =
  req
    "DescribeAlias"
    "fixture/DescribeAlias.yaml"

requestDescribeBuild :: DescribeBuild -> TestTree
requestDescribeBuild =
  req
    "DescribeBuild"
    "fixture/DescribeBuild.yaml"

requestDescribeCompute :: DescribeCompute -> TestTree
requestDescribeCompute =
  req
    "DescribeCompute"
    "fixture/DescribeCompute.yaml"

requestDescribeEC2InstanceLimits :: DescribeEC2InstanceLimits -> TestTree
requestDescribeEC2InstanceLimits =
  req
    "DescribeEC2InstanceLimits"
    "fixture/DescribeEC2InstanceLimits.yaml"

requestDescribeFleetAttributes :: DescribeFleetAttributes -> TestTree
requestDescribeFleetAttributes =
  req
    "DescribeFleetAttributes"
    "fixture/DescribeFleetAttributes.yaml"

requestDescribeFleetCapacity :: DescribeFleetCapacity -> TestTree
requestDescribeFleetCapacity =
  req
    "DescribeFleetCapacity"
    "fixture/DescribeFleetCapacity.yaml"

requestDescribeFleetEvents :: DescribeFleetEvents -> TestTree
requestDescribeFleetEvents =
  req
    "DescribeFleetEvents"
    "fixture/DescribeFleetEvents.yaml"

requestDescribeFleetLocationAttributes :: DescribeFleetLocationAttributes -> TestTree
requestDescribeFleetLocationAttributes =
  req
    "DescribeFleetLocationAttributes"
    "fixture/DescribeFleetLocationAttributes.yaml"

requestDescribeFleetLocationCapacity :: DescribeFleetLocationCapacity -> TestTree
requestDescribeFleetLocationCapacity =
  req
    "DescribeFleetLocationCapacity"
    "fixture/DescribeFleetLocationCapacity.yaml"

requestDescribeFleetLocationUtilization :: DescribeFleetLocationUtilization -> TestTree
requestDescribeFleetLocationUtilization =
  req
    "DescribeFleetLocationUtilization"
    "fixture/DescribeFleetLocationUtilization.yaml"

requestDescribeFleetPortSettings :: DescribeFleetPortSettings -> TestTree
requestDescribeFleetPortSettings =
  req
    "DescribeFleetPortSettings"
    "fixture/DescribeFleetPortSettings.yaml"

requestDescribeFleetUtilization :: DescribeFleetUtilization -> TestTree
requestDescribeFleetUtilization =
  req
    "DescribeFleetUtilization"
    "fixture/DescribeFleetUtilization.yaml"

requestDescribeGameServer :: DescribeGameServer -> TestTree
requestDescribeGameServer =
  req
    "DescribeGameServer"
    "fixture/DescribeGameServer.yaml"

requestDescribeGameServerGroup :: DescribeGameServerGroup -> TestTree
requestDescribeGameServerGroup =
  req
    "DescribeGameServerGroup"
    "fixture/DescribeGameServerGroup.yaml"

requestDescribeGameServerInstances :: DescribeGameServerInstances -> TestTree
requestDescribeGameServerInstances =
  req
    "DescribeGameServerInstances"
    "fixture/DescribeGameServerInstances.yaml"

requestDescribeGameSessionDetails :: DescribeGameSessionDetails -> TestTree
requestDescribeGameSessionDetails =
  req
    "DescribeGameSessionDetails"
    "fixture/DescribeGameSessionDetails.yaml"

requestDescribeGameSessionPlacement :: DescribeGameSessionPlacement -> TestTree
requestDescribeGameSessionPlacement =
  req
    "DescribeGameSessionPlacement"
    "fixture/DescribeGameSessionPlacement.yaml"

requestDescribeGameSessionQueues :: DescribeGameSessionQueues -> TestTree
requestDescribeGameSessionQueues =
  req
    "DescribeGameSessionQueues"
    "fixture/DescribeGameSessionQueues.yaml"

requestDescribeGameSessions :: DescribeGameSessions -> TestTree
requestDescribeGameSessions =
  req
    "DescribeGameSessions"
    "fixture/DescribeGameSessions.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances =
  req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestDescribeMatchmaking :: DescribeMatchmaking -> TestTree
requestDescribeMatchmaking =
  req
    "DescribeMatchmaking"
    "fixture/DescribeMatchmaking.yaml"

requestDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurations -> TestTree
requestDescribeMatchmakingConfigurations =
  req
    "DescribeMatchmakingConfigurations"
    "fixture/DescribeMatchmakingConfigurations.yaml"

requestDescribeMatchmakingRuleSets :: DescribeMatchmakingRuleSets -> TestTree
requestDescribeMatchmakingRuleSets =
  req
    "DescribeMatchmakingRuleSets"
    "fixture/DescribeMatchmakingRuleSets.yaml"

requestDescribePlayerSessions :: DescribePlayerSessions -> TestTree
requestDescribePlayerSessions =
  req
    "DescribePlayerSessions"
    "fixture/DescribePlayerSessions.yaml"

requestDescribeRuntimeConfiguration :: DescribeRuntimeConfiguration -> TestTree
requestDescribeRuntimeConfiguration =
  req
    "DescribeRuntimeConfiguration"
    "fixture/DescribeRuntimeConfiguration.yaml"

requestDescribeScalingPolicies :: DescribeScalingPolicies -> TestTree
requestDescribeScalingPolicies =
  req
    "DescribeScalingPolicies"
    "fixture/DescribeScalingPolicies.yaml"

requestDescribeScript :: DescribeScript -> TestTree
requestDescribeScript =
  req
    "DescribeScript"
    "fixture/DescribeScript.yaml"

requestDescribeVpcPeeringAuthorizations :: DescribeVpcPeeringAuthorizations -> TestTree
requestDescribeVpcPeeringAuthorizations =
  req
    "DescribeVpcPeeringAuthorizations"
    "fixture/DescribeVpcPeeringAuthorizations.yaml"

requestDescribeVpcPeeringConnections :: DescribeVpcPeeringConnections -> TestTree
requestDescribeVpcPeeringConnections =
  req
    "DescribeVpcPeeringConnections"
    "fixture/DescribeVpcPeeringConnections.yaml"

requestGetComputeAccess :: GetComputeAccess -> TestTree
requestGetComputeAccess =
  req
    "GetComputeAccess"
    "fixture/GetComputeAccess.yaml"

requestGetComputeAuthToken :: GetComputeAuthToken -> TestTree
requestGetComputeAuthToken =
  req
    "GetComputeAuthToken"
    "fixture/GetComputeAuthToken.yaml"

requestGetGameSessionLogUrl :: GetGameSessionLogUrl -> TestTree
requestGetGameSessionLogUrl =
  req
    "GetGameSessionLogUrl"
    "fixture/GetGameSessionLogUrl.yaml"

requestGetInstanceAccess :: GetInstanceAccess -> TestTree
requestGetInstanceAccess =
  req
    "GetInstanceAccess"
    "fixture/GetInstanceAccess.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestListBuilds :: ListBuilds -> TestTree
requestListBuilds =
  req
    "ListBuilds"
    "fixture/ListBuilds.yaml"

requestListCompute :: ListCompute -> TestTree
requestListCompute =
  req
    "ListCompute"
    "fixture/ListCompute.yaml"

requestListFleets :: ListFleets -> TestTree
requestListFleets =
  req
    "ListFleets"
    "fixture/ListFleets.yaml"

requestListGameServerGroups :: ListGameServerGroups -> TestTree
requestListGameServerGroups =
  req
    "ListGameServerGroups"
    "fixture/ListGameServerGroups.yaml"

requestListGameServers :: ListGameServers -> TestTree
requestListGameServers =
  req
    "ListGameServers"
    "fixture/ListGameServers.yaml"

requestListLocations :: ListLocations -> TestTree
requestListLocations =
  req
    "ListLocations"
    "fixture/ListLocations.yaml"

requestListScripts :: ListScripts -> TestTree
requestListScripts =
  req
    "ListScripts"
    "fixture/ListScripts.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutScalingPolicy :: PutScalingPolicy -> TestTree
requestPutScalingPolicy =
  req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

requestRegisterCompute :: RegisterCompute -> TestTree
requestRegisterCompute =
  req
    "RegisterCompute"
    "fixture/RegisterCompute.yaml"

requestRegisterGameServer :: RegisterGameServer -> TestTree
requestRegisterGameServer =
  req
    "RegisterGameServer"
    "fixture/RegisterGameServer.yaml"

requestRequestUploadCredentials :: RequestUploadCredentials -> TestTree
requestRequestUploadCredentials =
  req
    "RequestUploadCredentials"
    "fixture/RequestUploadCredentials.yaml"

requestResolveAlias :: ResolveAlias -> TestTree
requestResolveAlias =
  req
    "ResolveAlias"
    "fixture/ResolveAlias.yaml"

requestResumeGameServerGroup :: ResumeGameServerGroup -> TestTree
requestResumeGameServerGroup =
  req
    "ResumeGameServerGroup"
    "fixture/ResumeGameServerGroup.yaml"

requestSearchGameSessions :: SearchGameSessions -> TestTree
requestSearchGameSessions =
  req
    "SearchGameSessions"
    "fixture/SearchGameSessions.yaml"

requestStartFleetActions :: StartFleetActions -> TestTree
requestStartFleetActions =
  req
    "StartFleetActions"
    "fixture/StartFleetActions.yaml"

requestStartGameSessionPlacement :: StartGameSessionPlacement -> TestTree
requestStartGameSessionPlacement =
  req
    "StartGameSessionPlacement"
    "fixture/StartGameSessionPlacement.yaml"

requestStartMatchBackfill :: StartMatchBackfill -> TestTree
requestStartMatchBackfill =
  req
    "StartMatchBackfill"
    "fixture/StartMatchBackfill.yaml"

requestStartMatchmaking :: StartMatchmaking -> TestTree
requestStartMatchmaking =
  req
    "StartMatchmaking"
    "fixture/StartMatchmaking.yaml"

requestStopFleetActions :: StopFleetActions -> TestTree
requestStopFleetActions =
  req
    "StopFleetActions"
    "fixture/StopFleetActions.yaml"

requestStopGameSessionPlacement :: StopGameSessionPlacement -> TestTree
requestStopGameSessionPlacement =
  req
    "StopGameSessionPlacement"
    "fixture/StopGameSessionPlacement.yaml"

requestStopMatchmaking :: StopMatchmaking -> TestTree
requestStopMatchmaking =
  req
    "StopMatchmaking"
    "fixture/StopMatchmaking.yaml"

requestSuspendGameServerGroup :: SuspendGameServerGroup -> TestTree
requestSuspendGameServerGroup =
  req
    "SuspendGameServerGroup"
    "fixture/SuspendGameServerGroup.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAlias :: UpdateAlias -> TestTree
requestUpdateAlias =
  req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

requestUpdateBuild :: UpdateBuild -> TestTree
requestUpdateBuild =
  req
    "UpdateBuild"
    "fixture/UpdateBuild.yaml"

requestUpdateFleetAttributes :: UpdateFleetAttributes -> TestTree
requestUpdateFleetAttributes =
  req
    "UpdateFleetAttributes"
    "fixture/UpdateFleetAttributes.yaml"

requestUpdateFleetCapacity :: UpdateFleetCapacity -> TestTree
requestUpdateFleetCapacity =
  req
    "UpdateFleetCapacity"
    "fixture/UpdateFleetCapacity.yaml"

requestUpdateFleetPortSettings :: UpdateFleetPortSettings -> TestTree
requestUpdateFleetPortSettings =
  req
    "UpdateFleetPortSettings"
    "fixture/UpdateFleetPortSettings.yaml"

requestUpdateGameServer :: UpdateGameServer -> TestTree
requestUpdateGameServer =
  req
    "UpdateGameServer"
    "fixture/UpdateGameServer.yaml"

requestUpdateGameServerGroup :: UpdateGameServerGroup -> TestTree
requestUpdateGameServerGroup =
  req
    "UpdateGameServerGroup"
    "fixture/UpdateGameServerGroup.yaml"

requestUpdateGameSession :: UpdateGameSession -> TestTree
requestUpdateGameSession =
  req
    "UpdateGameSession"
    "fixture/UpdateGameSession.yaml"

requestUpdateGameSessionQueue :: UpdateGameSessionQueue -> TestTree
requestUpdateGameSessionQueue =
  req
    "UpdateGameSessionQueue"
    "fixture/UpdateGameSessionQueue.yaml"

requestUpdateMatchmakingConfiguration :: UpdateMatchmakingConfiguration -> TestTree
requestUpdateMatchmakingConfiguration =
  req
    "UpdateMatchmakingConfiguration"
    "fixture/UpdateMatchmakingConfiguration.yaml"

requestUpdateRuntimeConfiguration :: UpdateRuntimeConfiguration -> TestTree
requestUpdateRuntimeConfiguration =
  req
    "UpdateRuntimeConfiguration"
    "fixture/UpdateRuntimeConfiguration.yaml"

requestUpdateScript :: UpdateScript -> TestTree
requestUpdateScript =
  req
    "UpdateScript"
    "fixture/UpdateScript.yaml"

requestValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSet -> TestTree
requestValidateMatchmakingRuleSet =
  req
    "ValidateMatchmakingRuleSet"
    "fixture/ValidateMatchmakingRuleSet.yaml"

-- Responses

responseAcceptMatch :: AcceptMatchResponse -> TestTree
responseAcceptMatch =
  res
    "AcceptMatchResponse"
    "fixture/AcceptMatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptMatch)

responseClaimGameServer :: ClaimGameServerResponse -> TestTree
responseClaimGameServer =
  res
    "ClaimGameServerResponse"
    "fixture/ClaimGameServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClaimGameServer)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlias)

responseCreateBuild :: CreateBuildResponse -> TestTree
responseCreateBuild =
  res
    "CreateBuildResponse"
    "fixture/CreateBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBuild)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleet)

responseCreateFleetLocations :: CreateFleetLocationsResponse -> TestTree
responseCreateFleetLocations =
  res
    "CreateFleetLocationsResponse"
    "fixture/CreateFleetLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleetLocations)

responseCreateGameServerGroup :: CreateGameServerGroupResponse -> TestTree
responseCreateGameServerGroup =
  res
    "CreateGameServerGroupResponse"
    "fixture/CreateGameServerGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGameServerGroup)

responseCreateGameSession :: CreateGameSessionResponse -> TestTree
responseCreateGameSession =
  res
    "CreateGameSessionResponse"
    "fixture/CreateGameSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGameSession)

responseCreateGameSessionQueue :: CreateGameSessionQueueResponse -> TestTree
responseCreateGameSessionQueue =
  res
    "CreateGameSessionQueueResponse"
    "fixture/CreateGameSessionQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGameSessionQueue)

responseCreateLocation :: CreateLocationResponse -> TestTree
responseCreateLocation =
  res
    "CreateLocationResponse"
    "fixture/CreateLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocation)

responseCreateMatchmakingConfiguration :: CreateMatchmakingConfigurationResponse -> TestTree
responseCreateMatchmakingConfiguration =
  res
    "CreateMatchmakingConfigurationResponse"
    "fixture/CreateMatchmakingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMatchmakingConfiguration)

responseCreateMatchmakingRuleSet :: CreateMatchmakingRuleSetResponse -> TestTree
responseCreateMatchmakingRuleSet =
  res
    "CreateMatchmakingRuleSetResponse"
    "fixture/CreateMatchmakingRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMatchmakingRuleSet)

responseCreatePlayerSession :: CreatePlayerSessionResponse -> TestTree
responseCreatePlayerSession =
  res
    "CreatePlayerSessionResponse"
    "fixture/CreatePlayerSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlayerSession)

responseCreatePlayerSessions :: CreatePlayerSessionsResponse -> TestTree
responseCreatePlayerSessions =
  res
    "CreatePlayerSessionsResponse"
    "fixture/CreatePlayerSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlayerSessions)

responseCreateScript :: CreateScriptResponse -> TestTree
responseCreateScript =
  res
    "CreateScriptResponse"
    "fixture/CreateScriptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateScript)

responseCreateVpcPeeringAuthorization :: CreateVpcPeeringAuthorizationResponse -> TestTree
responseCreateVpcPeeringAuthorization =
  res
    "CreateVpcPeeringAuthorizationResponse"
    "fixture/CreateVpcPeeringAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcPeeringAuthorization)

responseCreateVpcPeeringConnection :: CreateVpcPeeringConnectionResponse -> TestTree
responseCreateVpcPeeringConnection =
  res
    "CreateVpcPeeringConnectionResponse"
    "fixture/CreateVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcPeeringConnection)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlias)

responseDeleteBuild :: DeleteBuildResponse -> TestTree
responseDeleteBuild =
  res
    "DeleteBuildResponse"
    "fixture/DeleteBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBuild)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet =
  res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleet)

responseDeleteFleetLocations :: DeleteFleetLocationsResponse -> TestTree
responseDeleteFleetLocations =
  res
    "DeleteFleetLocationsResponse"
    "fixture/DeleteFleetLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleetLocations)

responseDeleteGameServerGroup :: DeleteGameServerGroupResponse -> TestTree
responseDeleteGameServerGroup =
  res
    "DeleteGameServerGroupResponse"
    "fixture/DeleteGameServerGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGameServerGroup)

responseDeleteGameSessionQueue :: DeleteGameSessionQueueResponse -> TestTree
responseDeleteGameSessionQueue =
  res
    "DeleteGameSessionQueueResponse"
    "fixture/DeleteGameSessionQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGameSessionQueue)

responseDeleteLocation :: DeleteLocationResponse -> TestTree
responseDeleteLocation =
  res
    "DeleteLocationResponse"
    "fixture/DeleteLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLocation)

responseDeleteMatchmakingConfiguration :: DeleteMatchmakingConfigurationResponse -> TestTree
responseDeleteMatchmakingConfiguration =
  res
    "DeleteMatchmakingConfigurationResponse"
    "fixture/DeleteMatchmakingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMatchmakingConfiguration)

responseDeleteMatchmakingRuleSet :: DeleteMatchmakingRuleSetResponse -> TestTree
responseDeleteMatchmakingRuleSet =
  res
    "DeleteMatchmakingRuleSetResponse"
    "fixture/DeleteMatchmakingRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMatchmakingRuleSet)

responseDeleteScalingPolicy :: DeleteScalingPolicyResponse -> TestTree
responseDeleteScalingPolicy =
  res
    "DeleteScalingPolicyResponse"
    "fixture/DeleteScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScalingPolicy)

responseDeleteScript :: DeleteScriptResponse -> TestTree
responseDeleteScript =
  res
    "DeleteScriptResponse"
    "fixture/DeleteScriptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScript)

responseDeleteVpcPeeringAuthorization :: DeleteVpcPeeringAuthorizationResponse -> TestTree
responseDeleteVpcPeeringAuthorization =
  res
    "DeleteVpcPeeringAuthorizationResponse"
    "fixture/DeleteVpcPeeringAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcPeeringAuthorization)

responseDeleteVpcPeeringConnection :: DeleteVpcPeeringConnectionResponse -> TestTree
responseDeleteVpcPeeringConnection =
  res
    "DeleteVpcPeeringConnectionResponse"
    "fixture/DeleteVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcPeeringConnection)

responseDeregisterCompute :: DeregisterComputeResponse -> TestTree
responseDeregisterCompute =
  res
    "DeregisterComputeResponse"
    "fixture/DeregisterComputeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterCompute)

responseDeregisterGameServer :: DeregisterGameServerResponse -> TestTree
responseDeregisterGameServer =
  res
    "DeregisterGameServerResponse"
    "fixture/DeregisterGameServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterGameServer)

responseDescribeAlias :: DescribeAliasResponse -> TestTree
responseDescribeAlias =
  res
    "DescribeAliasResponse"
    "fixture/DescribeAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlias)

responseDescribeBuild :: DescribeBuildResponse -> TestTree
responseDescribeBuild =
  res
    "DescribeBuildResponse"
    "fixture/DescribeBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBuild)

responseDescribeCompute :: DescribeComputeResponse -> TestTree
responseDescribeCompute =
  res
    "DescribeComputeResponse"
    "fixture/DescribeComputeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCompute)

responseDescribeEC2InstanceLimits :: DescribeEC2InstanceLimitsResponse -> TestTree
responseDescribeEC2InstanceLimits =
  res
    "DescribeEC2InstanceLimitsResponse"
    "fixture/DescribeEC2InstanceLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEC2InstanceLimits)

responseDescribeFleetAttributes :: DescribeFleetAttributesResponse -> TestTree
responseDescribeFleetAttributes =
  res
    "DescribeFleetAttributesResponse"
    "fixture/DescribeFleetAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetAttributes)

responseDescribeFleetCapacity :: DescribeFleetCapacityResponse -> TestTree
responseDescribeFleetCapacity =
  res
    "DescribeFleetCapacityResponse"
    "fixture/DescribeFleetCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetCapacity)

responseDescribeFleetEvents :: DescribeFleetEventsResponse -> TestTree
responseDescribeFleetEvents =
  res
    "DescribeFleetEventsResponse"
    "fixture/DescribeFleetEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetEvents)

responseDescribeFleetLocationAttributes :: DescribeFleetLocationAttributesResponse -> TestTree
responseDescribeFleetLocationAttributes =
  res
    "DescribeFleetLocationAttributesResponse"
    "fixture/DescribeFleetLocationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetLocationAttributes)

responseDescribeFleetLocationCapacity :: DescribeFleetLocationCapacityResponse -> TestTree
responseDescribeFleetLocationCapacity =
  res
    "DescribeFleetLocationCapacityResponse"
    "fixture/DescribeFleetLocationCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetLocationCapacity)

responseDescribeFleetLocationUtilization :: DescribeFleetLocationUtilizationResponse -> TestTree
responseDescribeFleetLocationUtilization =
  res
    "DescribeFleetLocationUtilizationResponse"
    "fixture/DescribeFleetLocationUtilizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetLocationUtilization)

responseDescribeFleetPortSettings :: DescribeFleetPortSettingsResponse -> TestTree
responseDescribeFleetPortSettings =
  res
    "DescribeFleetPortSettingsResponse"
    "fixture/DescribeFleetPortSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetPortSettings)

responseDescribeFleetUtilization :: DescribeFleetUtilizationResponse -> TestTree
responseDescribeFleetUtilization =
  res
    "DescribeFleetUtilizationResponse"
    "fixture/DescribeFleetUtilizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleetUtilization)

responseDescribeGameServer :: DescribeGameServerResponse -> TestTree
responseDescribeGameServer =
  res
    "DescribeGameServerResponse"
    "fixture/DescribeGameServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameServer)

responseDescribeGameServerGroup :: DescribeGameServerGroupResponse -> TestTree
responseDescribeGameServerGroup =
  res
    "DescribeGameServerGroupResponse"
    "fixture/DescribeGameServerGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameServerGroup)

responseDescribeGameServerInstances :: DescribeGameServerInstancesResponse -> TestTree
responseDescribeGameServerInstances =
  res
    "DescribeGameServerInstancesResponse"
    "fixture/DescribeGameServerInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameServerInstances)

responseDescribeGameSessionDetails :: DescribeGameSessionDetailsResponse -> TestTree
responseDescribeGameSessionDetails =
  res
    "DescribeGameSessionDetailsResponse"
    "fixture/DescribeGameSessionDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameSessionDetails)

responseDescribeGameSessionPlacement :: DescribeGameSessionPlacementResponse -> TestTree
responseDescribeGameSessionPlacement =
  res
    "DescribeGameSessionPlacementResponse"
    "fixture/DescribeGameSessionPlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameSessionPlacement)

responseDescribeGameSessionQueues :: DescribeGameSessionQueuesResponse -> TestTree
responseDescribeGameSessionQueues =
  res
    "DescribeGameSessionQueuesResponse"
    "fixture/DescribeGameSessionQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameSessionQueues)

responseDescribeGameSessions :: DescribeGameSessionsResponse -> TestTree
responseDescribeGameSessions =
  res
    "DescribeGameSessionsResponse"
    "fixture/DescribeGameSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGameSessions)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstances)

responseDescribeMatchmaking :: DescribeMatchmakingResponse -> TestTree
responseDescribeMatchmaking =
  res
    "DescribeMatchmakingResponse"
    "fixture/DescribeMatchmakingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMatchmaking)

responseDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurationsResponse -> TestTree
responseDescribeMatchmakingConfigurations =
  res
    "DescribeMatchmakingConfigurationsResponse"
    "fixture/DescribeMatchmakingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMatchmakingConfigurations)

responseDescribeMatchmakingRuleSets :: DescribeMatchmakingRuleSetsResponse -> TestTree
responseDescribeMatchmakingRuleSets =
  res
    "DescribeMatchmakingRuleSetsResponse"
    "fixture/DescribeMatchmakingRuleSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMatchmakingRuleSets)

responseDescribePlayerSessions :: DescribePlayerSessionsResponse -> TestTree
responseDescribePlayerSessions =
  res
    "DescribePlayerSessionsResponse"
    "fixture/DescribePlayerSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePlayerSessions)

responseDescribeRuntimeConfiguration :: DescribeRuntimeConfigurationResponse -> TestTree
responseDescribeRuntimeConfiguration =
  res
    "DescribeRuntimeConfigurationResponse"
    "fixture/DescribeRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRuntimeConfiguration)

responseDescribeScalingPolicies :: DescribeScalingPoliciesResponse -> TestTree
responseDescribeScalingPolicies =
  res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScalingPolicies)

responseDescribeScript :: DescribeScriptResponse -> TestTree
responseDescribeScript =
  res
    "DescribeScriptResponse"
    "fixture/DescribeScriptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScript)

responseDescribeVpcPeeringAuthorizations :: DescribeVpcPeeringAuthorizationsResponse -> TestTree
responseDescribeVpcPeeringAuthorizations =
  res
    "DescribeVpcPeeringAuthorizationsResponse"
    "fixture/DescribeVpcPeeringAuthorizationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcPeeringAuthorizations)

responseDescribeVpcPeeringConnections :: DescribeVpcPeeringConnectionsResponse -> TestTree
responseDescribeVpcPeeringConnections =
  res
    "DescribeVpcPeeringConnectionsResponse"
    "fixture/DescribeVpcPeeringConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcPeeringConnections)

responseGetComputeAccess :: GetComputeAccessResponse -> TestTree
responseGetComputeAccess =
  res
    "GetComputeAccessResponse"
    "fixture/GetComputeAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComputeAccess)

responseGetComputeAuthToken :: GetComputeAuthTokenResponse -> TestTree
responseGetComputeAuthToken =
  res
    "GetComputeAuthTokenResponse"
    "fixture/GetComputeAuthTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComputeAuthToken)

responseGetGameSessionLogUrl :: GetGameSessionLogUrlResponse -> TestTree
responseGetGameSessionLogUrl =
  res
    "GetGameSessionLogUrlResponse"
    "fixture/GetGameSessionLogUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGameSessionLogUrl)

responseGetInstanceAccess :: GetInstanceAccessResponse -> TestTree
responseGetInstanceAccess =
  res
    "GetInstanceAccessResponse"
    "fixture/GetInstanceAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceAccess)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAliases)

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds =
  res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuilds)

responseListCompute :: ListComputeResponse -> TestTree
responseListCompute =
  res
    "ListComputeResponse"
    "fixture/ListComputeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCompute)

responseListFleets :: ListFleetsResponse -> TestTree
responseListFleets =
  res
    "ListFleetsResponse"
    "fixture/ListFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFleets)

responseListGameServerGroups :: ListGameServerGroupsResponse -> TestTree
responseListGameServerGroups =
  res
    "ListGameServerGroupsResponse"
    "fixture/ListGameServerGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGameServerGroups)

responseListGameServers :: ListGameServersResponse -> TestTree
responseListGameServers =
  res
    "ListGameServersResponse"
    "fixture/ListGameServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGameServers)

responseListLocations :: ListLocationsResponse -> TestTree
responseListLocations =
  res
    "ListLocationsResponse"
    "fixture/ListLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLocations)

responseListScripts :: ListScriptsResponse -> TestTree
responseListScripts =
  res
    "ListScriptsResponse"
    "fixture/ListScriptsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListScripts)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy =
  res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutScalingPolicy)

responseRegisterCompute :: RegisterComputeResponse -> TestTree
responseRegisterCompute =
  res
    "RegisterComputeResponse"
    "fixture/RegisterComputeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterCompute)

responseRegisterGameServer :: RegisterGameServerResponse -> TestTree
responseRegisterGameServer =
  res
    "RegisterGameServerResponse"
    "fixture/RegisterGameServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterGameServer)

responseRequestUploadCredentials :: RequestUploadCredentialsResponse -> TestTree
responseRequestUploadCredentials =
  res
    "RequestUploadCredentialsResponse"
    "fixture/RequestUploadCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestUploadCredentials)

responseResolveAlias :: ResolveAliasResponse -> TestTree
responseResolveAlias =
  res
    "ResolveAliasResponse"
    "fixture/ResolveAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResolveAlias)

responseResumeGameServerGroup :: ResumeGameServerGroupResponse -> TestTree
responseResumeGameServerGroup =
  res
    "ResumeGameServerGroupResponse"
    "fixture/ResumeGameServerGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeGameServerGroup)

responseSearchGameSessions :: SearchGameSessionsResponse -> TestTree
responseSearchGameSessions =
  res
    "SearchGameSessionsResponse"
    "fixture/SearchGameSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchGameSessions)

responseStartFleetActions :: StartFleetActionsResponse -> TestTree
responseStartFleetActions =
  res
    "StartFleetActionsResponse"
    "fixture/StartFleetActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFleetActions)

responseStartGameSessionPlacement :: StartGameSessionPlacementResponse -> TestTree
responseStartGameSessionPlacement =
  res
    "StartGameSessionPlacementResponse"
    "fixture/StartGameSessionPlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartGameSessionPlacement)

responseStartMatchBackfill :: StartMatchBackfillResponse -> TestTree
responseStartMatchBackfill =
  res
    "StartMatchBackfillResponse"
    "fixture/StartMatchBackfillResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMatchBackfill)

responseStartMatchmaking :: StartMatchmakingResponse -> TestTree
responseStartMatchmaking =
  res
    "StartMatchmakingResponse"
    "fixture/StartMatchmakingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMatchmaking)

responseStopFleetActions :: StopFleetActionsResponse -> TestTree
responseStopFleetActions =
  res
    "StopFleetActionsResponse"
    "fixture/StopFleetActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopFleetActions)

responseStopGameSessionPlacement :: StopGameSessionPlacementResponse -> TestTree
responseStopGameSessionPlacement =
  res
    "StopGameSessionPlacementResponse"
    "fixture/StopGameSessionPlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopGameSessionPlacement)

responseStopMatchmaking :: StopMatchmakingResponse -> TestTree
responseStopMatchmaking =
  res
    "StopMatchmakingResponse"
    "fixture/StopMatchmakingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopMatchmaking)

responseSuspendGameServerGroup :: SuspendGameServerGroupResponse -> TestTree
responseSuspendGameServerGroup =
  res
    "SuspendGameServerGroupResponse"
    "fixture/SuspendGameServerGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SuspendGameServerGroup)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAlias :: UpdateAliasResponse -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAlias)

responseUpdateBuild :: UpdateBuildResponse -> TestTree
responseUpdateBuild =
  res
    "UpdateBuildResponse"
    "fixture/UpdateBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBuild)

responseUpdateFleetAttributes :: UpdateFleetAttributesResponse -> TestTree
responseUpdateFleetAttributes =
  res
    "UpdateFleetAttributesResponse"
    "fixture/UpdateFleetAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFleetAttributes)

responseUpdateFleetCapacity :: UpdateFleetCapacityResponse -> TestTree
responseUpdateFleetCapacity =
  res
    "UpdateFleetCapacityResponse"
    "fixture/UpdateFleetCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFleetCapacity)

responseUpdateFleetPortSettings :: UpdateFleetPortSettingsResponse -> TestTree
responseUpdateFleetPortSettings =
  res
    "UpdateFleetPortSettingsResponse"
    "fixture/UpdateFleetPortSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFleetPortSettings)

responseUpdateGameServer :: UpdateGameServerResponse -> TestTree
responseUpdateGameServer =
  res
    "UpdateGameServerResponse"
    "fixture/UpdateGameServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGameServer)

responseUpdateGameServerGroup :: UpdateGameServerGroupResponse -> TestTree
responseUpdateGameServerGroup =
  res
    "UpdateGameServerGroupResponse"
    "fixture/UpdateGameServerGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGameServerGroup)

responseUpdateGameSession :: UpdateGameSessionResponse -> TestTree
responseUpdateGameSession =
  res
    "UpdateGameSessionResponse"
    "fixture/UpdateGameSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGameSession)

responseUpdateGameSessionQueue :: UpdateGameSessionQueueResponse -> TestTree
responseUpdateGameSessionQueue =
  res
    "UpdateGameSessionQueueResponse"
    "fixture/UpdateGameSessionQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGameSessionQueue)

responseUpdateMatchmakingConfiguration :: UpdateMatchmakingConfigurationResponse -> TestTree
responseUpdateMatchmakingConfiguration =
  res
    "UpdateMatchmakingConfigurationResponse"
    "fixture/UpdateMatchmakingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMatchmakingConfiguration)

responseUpdateRuntimeConfiguration :: UpdateRuntimeConfigurationResponse -> TestTree
responseUpdateRuntimeConfiguration =
  res
    "UpdateRuntimeConfigurationResponse"
    "fixture/UpdateRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRuntimeConfiguration)

responseUpdateScript :: UpdateScriptResponse -> TestTree
responseUpdateScript =
  res
    "UpdateScriptResponse"
    "fixture/UpdateScriptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateScript)

responseValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSetResponse -> TestTree
responseValidateMatchmakingRuleSet =
  res
    "ValidateMatchmakingRuleSetResponse"
    "fixture/ValidateMatchmakingRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateMatchmakingRuleSet)
