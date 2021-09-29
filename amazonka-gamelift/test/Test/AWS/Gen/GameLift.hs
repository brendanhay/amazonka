{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.GameLift
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.GameLift where

import Data.Proxy
import Network.AWS.GameLift
import Test.AWS.Fixture
import Test.AWS.GameLift.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnections
--
--         , requestDescribeFleetCapacity $
--             newDescribeFleetCapacity
--
--         , requestListBuilds $
--             newListBuilds
--
--         , requestDeleteBuild $
--             newDeleteBuild
--
--         , requestDescribeGameSessionQueues $
--             newDescribeGameSessionQueues
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestDescribeFleetPortSettings $
--             newDescribeFleetPortSettings
--
--         , requestUpdateBuild $
--             newUpdateBuild
--
--         , requestDescribeInstances $
--             newDescribeInstances
--
--         , requestUpdateAlias $
--             newUpdateAlias
--
--         , requestDescribeFleetAttributes $
--             newDescribeFleetAttributes
--
--         , requestDescribeFleetEvents $
--             newDescribeFleetEvents
--
--         , requestPutScalingPolicy $
--             newPutScalingPolicy
--
--         , requestDescribeFleetUtilization $
--             newDescribeFleetUtilization
--
--         , requestDescribeRuntimeConfiguration $
--             newDescribeRuntimeConfiguration
--
--         , requestClaimGameServer $
--             newClaimGameServer
--
--         , requestUpdateGameSession $
--             newUpdateGameSession
--
--         , requestDescribeGameServerGroup $
--             newDescribeGameServerGroup
--
--         , requestCreatePlayerSession $
--             newCreatePlayerSession
--
--         , requestDescribeFleetLocationAttributes $
--             newDescribeFleetLocationAttributes
--
--         , requestDescribeMatchmaking $
--             newDescribeMatchmaking
--
--         , requestGetGameSessionLogUrl $
--             newGetGameSessionLogUrl
--
--         , requestDescribeMatchmakingRuleSets $
--             newDescribeMatchmakingRuleSets
--
--         , requestSuspendGameServerGroup $
--             newSuspendGameServerGroup
--
--         , requestDescribeScalingPolicies $
--             newDescribeScalingPolicies
--
--         , requestValidateMatchmakingRuleSet $
--             newValidateMatchmakingRuleSet
--
--         , requestDescribeBuild $
--             newDescribeBuild
--
--         , requestDeregisterGameServer $
--             newDeregisterGameServer
--
--         , requestUpdateFleetPortSettings $
--             newUpdateFleetPortSettings
--
--         , requestAcceptMatch $
--             newAcceptMatch
--
--         , requestUpdateFleetCapacity $
--             newUpdateFleetCapacity
--
--         , requestDescribeAlias $
--             newDescribeAlias
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateFleetAttributes $
--             newUpdateFleetAttributes
--
--         , requestUpdateGameSessionQueue $
--             newUpdateGameSessionQueue
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnection
--
--         , requestDeleteGameSessionQueue $
--             newDeleteGameSessionQueue
--
--         , requestDeleteMatchmakingConfiguration $
--             newDeleteMatchmakingConfiguration
--
--         , requestUpdateMatchmakingConfiguration $
--             newUpdateMatchmakingConfiguration
--
--         , requestRequestUploadCredentials $
--             newRequestUploadCredentials
--
--         , requestDescribeFleetLocationCapacity $
--             newDescribeFleetLocationCapacity
--
--         , requestRegisterGameServer $
--             newRegisterGameServer
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestDescribeMatchmakingConfigurations $
--             newDescribeMatchmakingConfigurations
--
--         , requestResolveAlias $
--             newResolveAlias
--
--         , requestDeleteFleet $
--             newDeleteFleet
--
--         , requestDescribeGameSessionDetails $
--             newDescribeGameSessionDetails
--
--         , requestListFleets $
--             newListFleets
--
--         , requestStartMatchBackfill $
--             newStartMatchBackfill
--
--         , requestStopMatchmaking $
--             newStopMatchmaking
--
--         , requestCreateFleetLocations $
--             newCreateFleetLocations
--
--         , requestCreateMatchmakingRuleSet $
--             newCreateMatchmakingRuleSet
--
--         , requestDescribeGameSessionPlacement $
--             newDescribeGameSessionPlacement
--
--         , requestCreateGameSession $
--             newCreateGameSession
--
--         , requestStartMatchmaking $
--             newStartMatchmaking
--
--         , requestDeleteScalingPolicy $
--             newDeleteScalingPolicy
--
--         , requestDescribeFleetLocationUtilization $
--             newDescribeFleetLocationUtilization
--
--         , requestDescribeGameServerInstances $
--             newDescribeGameServerInstances
--
--         , requestStartGameSessionPlacement $
--             newStartGameSessionPlacement
--
--         , requestDeleteMatchmakingRuleSet $
--             newDeleteMatchmakingRuleSet
--
--         , requestDeleteFleetLocations $
--             newDeleteFleetLocations
--
--         , requestStopGameSessionPlacement $
--             newStopGameSessionPlacement
--
--         , requestUpdateScript $
--             newUpdateScript
--
--         , requestDescribeGameSessions $
--             newDescribeGameSessions
--
--         , requestDeleteScript $
--             newDeleteScript
--
--         , requestDescribeGameServer $
--             newDescribeGameServer
--
--         , requestListScripts $
--             newListScripts
--
--         , requestDescribeEC2InstanceLimits $
--             newDescribeEC2InstanceLimits
--
--         , requestCreateScript $
--             newCreateScript
--
--         , requestStartFleetActions $
--             newStartFleetActions
--
--         , requestGetInstanceAccess $
--             newGetInstanceAccess
--
--         , requestDescribePlayerSessions $
--             newDescribePlayerSessions
--
--         , requestStopFleetActions $
--             newStopFleetActions
--
--         , requestCreateMatchmakingConfiguration $
--             newCreateMatchmakingConfiguration
--
--         , requestCreateGameServerGroup $
--             newCreateGameServerGroup
--
--         , requestCreateVpcPeeringAuthorization $
--             newCreateVpcPeeringAuthorization
--
--         , requestCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnection
--
--         , requestResumeGameServerGroup $
--             newResumeGameServerGroup
--
--         , requestDeleteGameServerGroup $
--             newDeleteGameServerGroup
--
--         , requestUpdateGameServerGroup $
--             newUpdateGameServerGroup
--
--         , requestUpdateRuntimeConfiguration $
--             newUpdateRuntimeConfiguration
--
--         , requestDeleteVpcPeeringAuthorization $
--             newDeleteVpcPeeringAuthorization
--
--         , requestCreateGameSessionQueue $
--             newCreateGameSessionQueue
--
--         , requestSearchGameSessions $
--             newSearchGameSessions
--
--         , requestListGameServerGroups $
--             newListGameServerGroups
--
--         , requestCreatePlayerSessions $
--             newCreatePlayerSessions
--
--         , requestCreateBuild $
--             newCreateBuild
--
--         , requestListAliases $
--             newListAliases
--
--         , requestDescribeScript $
--             newDescribeScript
--
--         , requestDescribeVpcPeeringAuthorizations $
--             newDescribeVpcPeeringAuthorizations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestListGameServers $
--             newListGameServers
--
--         , requestUpdateGameServer $
--             newUpdateGameServer
--
--           ]

--     , testGroup "response"
--         [ responseDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnectionsResponse
--
--         , responseDescribeFleetCapacity $
--             newDescribeFleetCapacityResponse
--
--         , responseListBuilds $
--             newListBuildsResponse
--
--         , responseDeleteBuild $
--             newDeleteBuildResponse
--
--         , responseDescribeGameSessionQueues $
--             newDescribeGameSessionQueuesResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseDescribeFleetPortSettings $
--             newDescribeFleetPortSettingsResponse
--
--         , responseUpdateBuild $
--             newUpdateBuildResponse
--
--         , responseDescribeInstances $
--             newDescribeInstancesResponse
--
--         , responseUpdateAlias $
--             newUpdateAliasResponse
--
--         , responseDescribeFleetAttributes $
--             newDescribeFleetAttributesResponse
--
--         , responseDescribeFleetEvents $
--             newDescribeFleetEventsResponse
--
--         , responsePutScalingPolicy $
--             newPutScalingPolicyResponse
--
--         , responseDescribeFleetUtilization $
--             newDescribeFleetUtilizationResponse
--
--         , responseDescribeRuntimeConfiguration $
--             newDescribeRuntimeConfigurationResponse
--
--         , responseClaimGameServer $
--             newClaimGameServerResponse
--
--         , responseUpdateGameSession $
--             newUpdateGameSessionResponse
--
--         , responseDescribeGameServerGroup $
--             newDescribeGameServerGroupResponse
--
--         , responseCreatePlayerSession $
--             newCreatePlayerSessionResponse
--
--         , responseDescribeFleetLocationAttributes $
--             newDescribeFleetLocationAttributesResponse
--
--         , responseDescribeMatchmaking $
--             newDescribeMatchmakingResponse
--
--         , responseGetGameSessionLogUrl $
--             newGetGameSessionLogUrlResponse
--
--         , responseDescribeMatchmakingRuleSets $
--             newDescribeMatchmakingRuleSetsResponse
--
--         , responseSuspendGameServerGroup $
--             newSuspendGameServerGroupResponse
--
--         , responseDescribeScalingPolicies $
--             newDescribeScalingPoliciesResponse
--
--         , responseValidateMatchmakingRuleSet $
--             newValidateMatchmakingRuleSetResponse
--
--         , responseDescribeBuild $
--             newDescribeBuildResponse
--
--         , responseDeregisterGameServer $
--             newDeregisterGameServerResponse
--
--         , responseUpdateFleetPortSettings $
--             newUpdateFleetPortSettingsResponse
--
--         , responseAcceptMatch $
--             newAcceptMatchResponse
--
--         , responseUpdateFleetCapacity $
--             newUpdateFleetCapacityResponse
--
--         , responseDescribeAlias $
--             newDescribeAliasResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateFleetAttributes $
--             newUpdateFleetAttributesResponse
--
--         , responseUpdateGameSessionQueue $
--             newUpdateGameSessionQueueResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnectionResponse
--
--         , responseDeleteGameSessionQueue $
--             newDeleteGameSessionQueueResponse
--
--         , responseDeleteMatchmakingConfiguration $
--             newDeleteMatchmakingConfigurationResponse
--
--         , responseUpdateMatchmakingConfiguration $
--             newUpdateMatchmakingConfigurationResponse
--
--         , responseRequestUploadCredentials $
--             newRequestUploadCredentialsResponse
--
--         , responseDescribeFleetLocationCapacity $
--             newDescribeFleetLocationCapacityResponse
--
--         , responseRegisterGameServer $
--             newRegisterGameServerResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseDescribeMatchmakingConfigurations $
--             newDescribeMatchmakingConfigurationsResponse
--
--         , responseResolveAlias $
--             newResolveAliasResponse
--
--         , responseDeleteFleet $
--             newDeleteFleetResponse
--
--         , responseDescribeGameSessionDetails $
--             newDescribeGameSessionDetailsResponse
--
--         , responseListFleets $
--             newListFleetsResponse
--
--         , responseStartMatchBackfill $
--             newStartMatchBackfillResponse
--
--         , responseStopMatchmaking $
--             newStopMatchmakingResponse
--
--         , responseCreateFleetLocations $
--             newCreateFleetLocationsResponse
--
--         , responseCreateMatchmakingRuleSet $
--             newCreateMatchmakingRuleSetResponse
--
--         , responseDescribeGameSessionPlacement $
--             newDescribeGameSessionPlacementResponse
--
--         , responseCreateGameSession $
--             newCreateGameSessionResponse
--
--         , responseStartMatchmaking $
--             newStartMatchmakingResponse
--
--         , responseDeleteScalingPolicy $
--             newDeleteScalingPolicyResponse
--
--         , responseDescribeFleetLocationUtilization $
--             newDescribeFleetLocationUtilizationResponse
--
--         , responseDescribeGameServerInstances $
--             newDescribeGameServerInstancesResponse
--
--         , responseStartGameSessionPlacement $
--             newStartGameSessionPlacementResponse
--
--         , responseDeleteMatchmakingRuleSet $
--             newDeleteMatchmakingRuleSetResponse
--
--         , responseDeleteFleetLocations $
--             newDeleteFleetLocationsResponse
--
--         , responseStopGameSessionPlacement $
--             newStopGameSessionPlacementResponse
--
--         , responseUpdateScript $
--             newUpdateScriptResponse
--
--         , responseDescribeGameSessions $
--             newDescribeGameSessionsResponse
--
--         , responseDeleteScript $
--             newDeleteScriptResponse
--
--         , responseDescribeGameServer $
--             newDescribeGameServerResponse
--
--         , responseListScripts $
--             newListScriptsResponse
--
--         , responseDescribeEC2InstanceLimits $
--             newDescribeEC2InstanceLimitsResponse
--
--         , responseCreateScript $
--             newCreateScriptResponse
--
--         , responseStartFleetActions $
--             newStartFleetActionsResponse
--
--         , responseGetInstanceAccess $
--             newGetInstanceAccessResponse
--
--         , responseDescribePlayerSessions $
--             newDescribePlayerSessionsResponse
--
--         , responseStopFleetActions $
--             newStopFleetActionsResponse
--
--         , responseCreateMatchmakingConfiguration $
--             newCreateMatchmakingConfigurationResponse
--
--         , responseCreateGameServerGroup $
--             newCreateGameServerGroupResponse
--
--         , responseCreateVpcPeeringAuthorization $
--             newCreateVpcPeeringAuthorizationResponse
--
--         , responseCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnectionResponse
--
--         , responseResumeGameServerGroup $
--             newResumeGameServerGroupResponse
--
--         , responseDeleteGameServerGroup $
--             newDeleteGameServerGroupResponse
--
--         , responseUpdateGameServerGroup $
--             newUpdateGameServerGroupResponse
--
--         , responseUpdateRuntimeConfiguration $
--             newUpdateRuntimeConfigurationResponse
--
--         , responseDeleteVpcPeeringAuthorization $
--             newDeleteVpcPeeringAuthorizationResponse
--
--         , responseCreateGameSessionQueue $
--             newCreateGameSessionQueueResponse
--
--         , responseSearchGameSessions $
--             newSearchGameSessionsResponse
--
--         , responseListGameServerGroups $
--             newListGameServerGroupsResponse
--
--         , responseCreatePlayerSessions $
--             newCreatePlayerSessionsResponse
--
--         , responseCreateBuild $
--             newCreateBuildResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseDescribeScript $
--             newDescribeScriptResponse
--
--         , responseDescribeVpcPeeringAuthorizations $
--             newDescribeVpcPeeringAuthorizationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseListGameServers $
--             newListGameServersResponse
--
--         , responseUpdateGameServer $
--             newUpdateGameServerResponse
--
--           ]
--     ]

-- Requests

requestDescribeVpcPeeringConnections :: DescribeVpcPeeringConnections -> TestTree
requestDescribeVpcPeeringConnections =
  req
    "DescribeVpcPeeringConnections"
    "fixture/DescribeVpcPeeringConnections.yaml"

requestDescribeFleetCapacity :: DescribeFleetCapacity -> TestTree
requestDescribeFleetCapacity =
  req
    "DescribeFleetCapacity"
    "fixture/DescribeFleetCapacity.yaml"

requestListBuilds :: ListBuilds -> TestTree
requestListBuilds =
  req
    "ListBuilds"
    "fixture/ListBuilds.yaml"

requestDeleteBuild :: DeleteBuild -> TestTree
requestDeleteBuild =
  req
    "DeleteBuild"
    "fixture/DeleteBuild.yaml"

requestDescribeGameSessionQueues :: DescribeGameSessionQueues -> TestTree
requestDescribeGameSessionQueues =
  req
    "DescribeGameSessionQueues"
    "fixture/DescribeGameSessionQueues.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestDescribeFleetPortSettings :: DescribeFleetPortSettings -> TestTree
requestDescribeFleetPortSettings =
  req
    "DescribeFleetPortSettings"
    "fixture/DescribeFleetPortSettings.yaml"

requestUpdateBuild :: UpdateBuild -> TestTree
requestUpdateBuild =
  req
    "UpdateBuild"
    "fixture/UpdateBuild.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances =
  req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestUpdateAlias :: UpdateAlias -> TestTree
requestUpdateAlias =
  req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

requestDescribeFleetAttributes :: DescribeFleetAttributes -> TestTree
requestDescribeFleetAttributes =
  req
    "DescribeFleetAttributes"
    "fixture/DescribeFleetAttributes.yaml"

requestDescribeFleetEvents :: DescribeFleetEvents -> TestTree
requestDescribeFleetEvents =
  req
    "DescribeFleetEvents"
    "fixture/DescribeFleetEvents.yaml"

requestPutScalingPolicy :: PutScalingPolicy -> TestTree
requestPutScalingPolicy =
  req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

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

requestDescribeGameServerGroup :: DescribeGameServerGroup -> TestTree
requestDescribeGameServerGroup =
  req
    "DescribeGameServerGroup"
    "fixture/DescribeGameServerGroup.yaml"

requestCreatePlayerSession :: CreatePlayerSession -> TestTree
requestCreatePlayerSession =
  req
    "CreatePlayerSession"
    "fixture/CreatePlayerSession.yaml"

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

requestGetGameSessionLogUrl :: GetGameSessionLogUrl -> TestTree
requestGetGameSessionLogUrl =
  req
    "GetGameSessionLogUrl"
    "fixture/GetGameSessionLogUrl.yaml"

requestDescribeMatchmakingRuleSets :: DescribeMatchmakingRuleSets -> TestTree
requestDescribeMatchmakingRuleSets =
  req
    "DescribeMatchmakingRuleSets"
    "fixture/DescribeMatchmakingRuleSets.yaml"

requestSuspendGameServerGroup :: SuspendGameServerGroup -> TestTree
requestSuspendGameServerGroup =
  req
    "SuspendGameServerGroup"
    "fixture/SuspendGameServerGroup.yaml"

requestDescribeScalingPolicies :: DescribeScalingPolicies -> TestTree
requestDescribeScalingPolicies =
  req
    "DescribeScalingPolicies"
    "fixture/DescribeScalingPolicies.yaml"

requestValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSet -> TestTree
requestValidateMatchmakingRuleSet =
  req
    "ValidateMatchmakingRuleSet"
    "fixture/ValidateMatchmakingRuleSet.yaml"

requestDescribeBuild :: DescribeBuild -> TestTree
requestDescribeBuild =
  req
    "DescribeBuild"
    "fixture/DescribeBuild.yaml"

requestDeregisterGameServer :: DeregisterGameServer -> TestTree
requestDeregisterGameServer =
  req
    "DeregisterGameServer"
    "fixture/DeregisterGameServer.yaml"

requestUpdateFleetPortSettings :: UpdateFleetPortSettings -> TestTree
requestUpdateFleetPortSettings =
  req
    "UpdateFleetPortSettings"
    "fixture/UpdateFleetPortSettings.yaml"

requestAcceptMatch :: AcceptMatch -> TestTree
requestAcceptMatch =
  req
    "AcceptMatch"
    "fixture/AcceptMatch.yaml"

requestUpdateFleetCapacity :: UpdateFleetCapacity -> TestTree
requestUpdateFleetCapacity =
  req
    "UpdateFleetCapacity"
    "fixture/UpdateFleetCapacity.yaml"

requestDescribeAlias :: DescribeAlias -> TestTree
requestDescribeAlias =
  req
    "DescribeAlias"
    "fixture/DescribeAlias.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateFleetAttributes :: UpdateFleetAttributes -> TestTree
requestUpdateFleetAttributes =
  req
    "UpdateFleetAttributes"
    "fixture/UpdateFleetAttributes.yaml"

requestUpdateGameSessionQueue :: UpdateGameSessionQueue -> TestTree
requestUpdateGameSessionQueue =
  req
    "UpdateGameSessionQueue"
    "fixture/UpdateGameSessionQueue.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteVpcPeeringConnection :: DeleteVpcPeeringConnection -> TestTree
requestDeleteVpcPeeringConnection =
  req
    "DeleteVpcPeeringConnection"
    "fixture/DeleteVpcPeeringConnection.yaml"

requestDeleteGameSessionQueue :: DeleteGameSessionQueue -> TestTree
requestDeleteGameSessionQueue =
  req
    "DeleteGameSessionQueue"
    "fixture/DeleteGameSessionQueue.yaml"

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

requestRequestUploadCredentials :: RequestUploadCredentials -> TestTree
requestRequestUploadCredentials =
  req
    "RequestUploadCredentials"
    "fixture/RequestUploadCredentials.yaml"

requestDescribeFleetLocationCapacity :: DescribeFleetLocationCapacity -> TestTree
requestDescribeFleetLocationCapacity =
  req
    "DescribeFleetLocationCapacity"
    "fixture/DescribeFleetLocationCapacity.yaml"

requestRegisterGameServer :: RegisterGameServer -> TestTree
requestRegisterGameServer =
  req
    "RegisterGameServer"
    "fixture/RegisterGameServer.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurations -> TestTree
requestDescribeMatchmakingConfigurations =
  req
    "DescribeMatchmakingConfigurations"
    "fixture/DescribeMatchmakingConfigurations.yaml"

requestResolveAlias :: ResolveAlias -> TestTree
requestResolveAlias =
  req
    "ResolveAlias"
    "fixture/ResolveAlias.yaml"

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet =
  req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestDescribeGameSessionDetails :: DescribeGameSessionDetails -> TestTree
requestDescribeGameSessionDetails =
  req
    "DescribeGameSessionDetails"
    "fixture/DescribeGameSessionDetails.yaml"

requestListFleets :: ListFleets -> TestTree
requestListFleets =
  req
    "ListFleets"
    "fixture/ListFleets.yaml"

requestStartMatchBackfill :: StartMatchBackfill -> TestTree
requestStartMatchBackfill =
  req
    "StartMatchBackfill"
    "fixture/StartMatchBackfill.yaml"

requestStopMatchmaking :: StopMatchmaking -> TestTree
requestStopMatchmaking =
  req
    "StopMatchmaking"
    "fixture/StopMatchmaking.yaml"

requestCreateFleetLocations :: CreateFleetLocations -> TestTree
requestCreateFleetLocations =
  req
    "CreateFleetLocations"
    "fixture/CreateFleetLocations.yaml"

requestCreateMatchmakingRuleSet :: CreateMatchmakingRuleSet -> TestTree
requestCreateMatchmakingRuleSet =
  req
    "CreateMatchmakingRuleSet"
    "fixture/CreateMatchmakingRuleSet.yaml"

requestDescribeGameSessionPlacement :: DescribeGameSessionPlacement -> TestTree
requestDescribeGameSessionPlacement =
  req
    "DescribeGameSessionPlacement"
    "fixture/DescribeGameSessionPlacement.yaml"

requestCreateGameSession :: CreateGameSession -> TestTree
requestCreateGameSession =
  req
    "CreateGameSession"
    "fixture/CreateGameSession.yaml"

requestStartMatchmaking :: StartMatchmaking -> TestTree
requestStartMatchmaking =
  req
    "StartMatchmaking"
    "fixture/StartMatchmaking.yaml"

requestDeleteScalingPolicy :: DeleteScalingPolicy -> TestTree
requestDeleteScalingPolicy =
  req
    "DeleteScalingPolicy"
    "fixture/DeleteScalingPolicy.yaml"

requestDescribeFleetLocationUtilization :: DescribeFleetLocationUtilization -> TestTree
requestDescribeFleetLocationUtilization =
  req
    "DescribeFleetLocationUtilization"
    "fixture/DescribeFleetLocationUtilization.yaml"

requestDescribeGameServerInstances :: DescribeGameServerInstances -> TestTree
requestDescribeGameServerInstances =
  req
    "DescribeGameServerInstances"
    "fixture/DescribeGameServerInstances.yaml"

requestStartGameSessionPlacement :: StartGameSessionPlacement -> TestTree
requestStartGameSessionPlacement =
  req
    "StartGameSessionPlacement"
    "fixture/StartGameSessionPlacement.yaml"

requestDeleteMatchmakingRuleSet :: DeleteMatchmakingRuleSet -> TestTree
requestDeleteMatchmakingRuleSet =
  req
    "DeleteMatchmakingRuleSet"
    "fixture/DeleteMatchmakingRuleSet.yaml"

requestDeleteFleetLocations :: DeleteFleetLocations -> TestTree
requestDeleteFleetLocations =
  req
    "DeleteFleetLocations"
    "fixture/DeleteFleetLocations.yaml"

requestStopGameSessionPlacement :: StopGameSessionPlacement -> TestTree
requestStopGameSessionPlacement =
  req
    "StopGameSessionPlacement"
    "fixture/StopGameSessionPlacement.yaml"

requestUpdateScript :: UpdateScript -> TestTree
requestUpdateScript =
  req
    "UpdateScript"
    "fixture/UpdateScript.yaml"

requestDescribeGameSessions :: DescribeGameSessions -> TestTree
requestDescribeGameSessions =
  req
    "DescribeGameSessions"
    "fixture/DescribeGameSessions.yaml"

requestDeleteScript :: DeleteScript -> TestTree
requestDeleteScript =
  req
    "DeleteScript"
    "fixture/DeleteScript.yaml"

requestDescribeGameServer :: DescribeGameServer -> TestTree
requestDescribeGameServer =
  req
    "DescribeGameServer"
    "fixture/DescribeGameServer.yaml"

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

requestCreateScript :: CreateScript -> TestTree
requestCreateScript =
  req
    "CreateScript"
    "fixture/CreateScript.yaml"

requestStartFleetActions :: StartFleetActions -> TestTree
requestStartFleetActions =
  req
    "StartFleetActions"
    "fixture/StartFleetActions.yaml"

requestGetInstanceAccess :: GetInstanceAccess -> TestTree
requestGetInstanceAccess =
  req
    "GetInstanceAccess"
    "fixture/GetInstanceAccess.yaml"

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

requestCreateMatchmakingConfiguration :: CreateMatchmakingConfiguration -> TestTree
requestCreateMatchmakingConfiguration =
  req
    "CreateMatchmakingConfiguration"
    "fixture/CreateMatchmakingConfiguration.yaml"

requestCreateGameServerGroup :: CreateGameServerGroup -> TestTree
requestCreateGameServerGroup =
  req
    "CreateGameServerGroup"
    "fixture/CreateGameServerGroup.yaml"

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

requestResumeGameServerGroup :: ResumeGameServerGroup -> TestTree
requestResumeGameServerGroup =
  req
    "ResumeGameServerGroup"
    "fixture/ResumeGameServerGroup.yaml"

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

requestUpdateRuntimeConfiguration :: UpdateRuntimeConfiguration -> TestTree
requestUpdateRuntimeConfiguration =
  req
    "UpdateRuntimeConfiguration"
    "fixture/UpdateRuntimeConfiguration.yaml"

requestDeleteVpcPeeringAuthorization :: DeleteVpcPeeringAuthorization -> TestTree
requestDeleteVpcPeeringAuthorization =
  req
    "DeleteVpcPeeringAuthorization"
    "fixture/DeleteVpcPeeringAuthorization.yaml"

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

requestListGameServerGroups :: ListGameServerGroups -> TestTree
requestListGameServerGroups =
  req
    "ListGameServerGroups"
    "fixture/ListGameServerGroups.yaml"

requestCreatePlayerSessions :: CreatePlayerSessions -> TestTree
requestCreatePlayerSessions =
  req
    "CreatePlayerSessions"
    "fixture/CreatePlayerSessions.yaml"

requestCreateBuild :: CreateBuild -> TestTree
requestCreateBuild =
  req
    "CreateBuild"
    "fixture/CreateBuild.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestUpdateGameServer :: UpdateGameServer -> TestTree
requestUpdateGameServer =
  req
    "UpdateGameServer"
    "fixture/UpdateGameServer.yaml"

-- Responses

responseDescribeVpcPeeringConnections :: DescribeVpcPeeringConnectionsResponse -> TestTree
responseDescribeVpcPeeringConnections =
  res
    "DescribeVpcPeeringConnectionsResponse"
    "fixture/DescribeVpcPeeringConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcPeeringConnections)

responseDescribeFleetCapacity :: DescribeFleetCapacityResponse -> TestTree
responseDescribeFleetCapacity =
  res
    "DescribeFleetCapacityResponse"
    "fixture/DescribeFleetCapacityResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetCapacity)

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds =
  res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBuilds)

responseDeleteBuild :: DeleteBuildResponse -> TestTree
responseDeleteBuild =
  res
    "DeleteBuildResponse"
    "fixture/DeleteBuildResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBuild)

responseDescribeGameSessionQueues :: DescribeGameSessionQueuesResponse -> TestTree
responseDescribeGameSessionQueues =
  res
    "DescribeGameSessionQueuesResponse"
    "fixture/DescribeGameSessionQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGameSessionQueues)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAlias)

responseDescribeFleetPortSettings :: DescribeFleetPortSettingsResponse -> TestTree
responseDescribeFleetPortSettings =
  res
    "DescribeFleetPortSettingsResponse"
    "fixture/DescribeFleetPortSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetPortSettings)

responseUpdateBuild :: UpdateBuildResponse -> TestTree
responseUpdateBuild =
  res
    "UpdateBuildResponse"
    "fixture/UpdateBuildResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBuild)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstances)

responseUpdateAlias :: UpdateAliasResponse -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAlias)

responseDescribeFleetAttributes :: DescribeFleetAttributesResponse -> TestTree
responseDescribeFleetAttributes =
  res
    "DescribeFleetAttributesResponse"
    "fixture/DescribeFleetAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetAttributes)

responseDescribeFleetEvents :: DescribeFleetEventsResponse -> TestTree
responseDescribeFleetEvents =
  res
    "DescribeFleetEventsResponse"
    "fixture/DescribeFleetEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetEvents)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy =
  res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutScalingPolicy)

responseDescribeFleetUtilization :: DescribeFleetUtilizationResponse -> TestTree
responseDescribeFleetUtilization =
  res
    "DescribeFleetUtilizationResponse"
    "fixture/DescribeFleetUtilizationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetUtilization)

responseDescribeRuntimeConfiguration :: DescribeRuntimeConfigurationResponse -> TestTree
responseDescribeRuntimeConfiguration =
  res
    "DescribeRuntimeConfigurationResponse"
    "fixture/DescribeRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRuntimeConfiguration)

responseClaimGameServer :: ClaimGameServerResponse -> TestTree
responseClaimGameServer =
  res
    "ClaimGameServerResponse"
    "fixture/ClaimGameServerResponse.proto"
    defaultService
    (Proxy :: Proxy ClaimGameServer)

responseUpdateGameSession :: UpdateGameSessionResponse -> TestTree
responseUpdateGameSession =
  res
    "UpdateGameSessionResponse"
    "fixture/UpdateGameSessionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGameSession)

responseDescribeGameServerGroup :: DescribeGameServerGroupResponse -> TestTree
responseDescribeGameServerGroup =
  res
    "DescribeGameServerGroupResponse"
    "fixture/DescribeGameServerGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGameServerGroup)

responseCreatePlayerSession :: CreatePlayerSessionResponse -> TestTree
responseCreatePlayerSession =
  res
    "CreatePlayerSessionResponse"
    "fixture/CreatePlayerSessionResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePlayerSession)

responseDescribeFleetLocationAttributes :: DescribeFleetLocationAttributesResponse -> TestTree
responseDescribeFleetLocationAttributes =
  res
    "DescribeFleetLocationAttributesResponse"
    "fixture/DescribeFleetLocationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetLocationAttributes)

responseDescribeMatchmaking :: DescribeMatchmakingResponse -> TestTree
responseDescribeMatchmaking =
  res
    "DescribeMatchmakingResponse"
    "fixture/DescribeMatchmakingResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMatchmaking)

responseGetGameSessionLogUrl :: GetGameSessionLogUrlResponse -> TestTree
responseGetGameSessionLogUrl =
  res
    "GetGameSessionLogUrlResponse"
    "fixture/GetGameSessionLogUrlResponse.proto"
    defaultService
    (Proxy :: Proxy GetGameSessionLogUrl)

responseDescribeMatchmakingRuleSets :: DescribeMatchmakingRuleSetsResponse -> TestTree
responseDescribeMatchmakingRuleSets =
  res
    "DescribeMatchmakingRuleSetsResponse"
    "fixture/DescribeMatchmakingRuleSetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMatchmakingRuleSets)

responseSuspendGameServerGroup :: SuspendGameServerGroupResponse -> TestTree
responseSuspendGameServerGroup =
  res
    "SuspendGameServerGroupResponse"
    "fixture/SuspendGameServerGroupResponse.proto"
    defaultService
    (Proxy :: Proxy SuspendGameServerGroup)

responseDescribeScalingPolicies :: DescribeScalingPoliciesResponse -> TestTree
responseDescribeScalingPolicies =
  res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScalingPolicies)

responseValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSetResponse -> TestTree
responseValidateMatchmakingRuleSet =
  res
    "ValidateMatchmakingRuleSetResponse"
    "fixture/ValidateMatchmakingRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy ValidateMatchmakingRuleSet)

responseDescribeBuild :: DescribeBuildResponse -> TestTree
responseDescribeBuild =
  res
    "DescribeBuildResponse"
    "fixture/DescribeBuildResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBuild)

responseDeregisterGameServer :: DeregisterGameServerResponse -> TestTree
responseDeregisterGameServer =
  res
    "DeregisterGameServerResponse"
    "fixture/DeregisterGameServerResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterGameServer)

responseUpdateFleetPortSettings :: UpdateFleetPortSettingsResponse -> TestTree
responseUpdateFleetPortSettings =
  res
    "UpdateFleetPortSettingsResponse"
    "fixture/UpdateFleetPortSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFleetPortSettings)

responseAcceptMatch :: AcceptMatchResponse -> TestTree
responseAcceptMatch =
  res
    "AcceptMatchResponse"
    "fixture/AcceptMatchResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptMatch)

responseUpdateFleetCapacity :: UpdateFleetCapacityResponse -> TestTree
responseUpdateFleetCapacity =
  res
    "UpdateFleetCapacityResponse"
    "fixture/UpdateFleetCapacityResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFleetCapacity)

responseDescribeAlias :: DescribeAliasResponse -> TestTree
responseDescribeAlias =
  res
    "DescribeAliasResponse"
    "fixture/DescribeAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAlias)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseUpdateFleetAttributes :: UpdateFleetAttributesResponse -> TestTree
responseUpdateFleetAttributes =
  res
    "UpdateFleetAttributesResponse"
    "fixture/UpdateFleetAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFleetAttributes)

responseUpdateGameSessionQueue :: UpdateGameSessionQueueResponse -> TestTree
responseUpdateGameSessionQueue =
  res
    "UpdateGameSessionQueueResponse"
    "fixture/UpdateGameSessionQueueResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGameSessionQueue)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDeleteVpcPeeringConnection :: DeleteVpcPeeringConnectionResponse -> TestTree
responseDeleteVpcPeeringConnection =
  res
    "DeleteVpcPeeringConnectionResponse"
    "fixture/DeleteVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcPeeringConnection)

responseDeleteGameSessionQueue :: DeleteGameSessionQueueResponse -> TestTree
responseDeleteGameSessionQueue =
  res
    "DeleteGameSessionQueueResponse"
    "fixture/DeleteGameSessionQueueResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGameSessionQueue)

responseDeleteMatchmakingConfiguration :: DeleteMatchmakingConfigurationResponse -> TestTree
responseDeleteMatchmakingConfiguration =
  res
    "DeleteMatchmakingConfigurationResponse"
    "fixture/DeleteMatchmakingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMatchmakingConfiguration)

responseUpdateMatchmakingConfiguration :: UpdateMatchmakingConfigurationResponse -> TestTree
responseUpdateMatchmakingConfiguration =
  res
    "UpdateMatchmakingConfigurationResponse"
    "fixture/UpdateMatchmakingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMatchmakingConfiguration)

responseRequestUploadCredentials :: RequestUploadCredentialsResponse -> TestTree
responseRequestUploadCredentials =
  res
    "RequestUploadCredentialsResponse"
    "fixture/RequestUploadCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy RequestUploadCredentials)

responseDescribeFleetLocationCapacity :: DescribeFleetLocationCapacityResponse -> TestTree
responseDescribeFleetLocationCapacity =
  res
    "DescribeFleetLocationCapacityResponse"
    "fixture/DescribeFleetLocationCapacityResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetLocationCapacity)

responseRegisterGameServer :: RegisterGameServerResponse -> TestTree
responseRegisterGameServer =
  res
    "RegisterGameServerResponse"
    "fixture/RegisterGameServerResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterGameServer)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFleet)

responseDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurationsResponse -> TestTree
responseDescribeMatchmakingConfigurations =
  res
    "DescribeMatchmakingConfigurationsResponse"
    "fixture/DescribeMatchmakingConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMatchmakingConfigurations)

responseResolveAlias :: ResolveAliasResponse -> TestTree
responseResolveAlias =
  res
    "ResolveAliasResponse"
    "fixture/ResolveAliasResponse.proto"
    defaultService
    (Proxy :: Proxy ResolveAlias)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet =
  res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFleet)

responseDescribeGameSessionDetails :: DescribeGameSessionDetailsResponse -> TestTree
responseDescribeGameSessionDetails =
  res
    "DescribeGameSessionDetailsResponse"
    "fixture/DescribeGameSessionDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGameSessionDetails)

responseListFleets :: ListFleetsResponse -> TestTree
responseListFleets =
  res
    "ListFleetsResponse"
    "fixture/ListFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFleets)

responseStartMatchBackfill :: StartMatchBackfillResponse -> TestTree
responseStartMatchBackfill =
  res
    "StartMatchBackfillResponse"
    "fixture/StartMatchBackfillResponse.proto"
    defaultService
    (Proxy :: Proxy StartMatchBackfill)

responseStopMatchmaking :: StopMatchmakingResponse -> TestTree
responseStopMatchmaking =
  res
    "StopMatchmakingResponse"
    "fixture/StopMatchmakingResponse.proto"
    defaultService
    (Proxy :: Proxy StopMatchmaking)

responseCreateFleetLocations :: CreateFleetLocationsResponse -> TestTree
responseCreateFleetLocations =
  res
    "CreateFleetLocationsResponse"
    "fixture/CreateFleetLocationsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFleetLocations)

responseCreateMatchmakingRuleSet :: CreateMatchmakingRuleSetResponse -> TestTree
responseCreateMatchmakingRuleSet =
  res
    "CreateMatchmakingRuleSetResponse"
    "fixture/CreateMatchmakingRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMatchmakingRuleSet)

responseDescribeGameSessionPlacement :: DescribeGameSessionPlacementResponse -> TestTree
responseDescribeGameSessionPlacement =
  res
    "DescribeGameSessionPlacementResponse"
    "fixture/DescribeGameSessionPlacementResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGameSessionPlacement)

responseCreateGameSession :: CreateGameSessionResponse -> TestTree
responseCreateGameSession =
  res
    "CreateGameSessionResponse"
    "fixture/CreateGameSessionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGameSession)

responseStartMatchmaking :: StartMatchmakingResponse -> TestTree
responseStartMatchmaking =
  res
    "StartMatchmakingResponse"
    "fixture/StartMatchmakingResponse.proto"
    defaultService
    (Proxy :: Proxy StartMatchmaking)

responseDeleteScalingPolicy :: DeleteScalingPolicyResponse -> TestTree
responseDeleteScalingPolicy =
  res
    "DeleteScalingPolicyResponse"
    "fixture/DeleteScalingPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteScalingPolicy)

responseDescribeFleetLocationUtilization :: DescribeFleetLocationUtilizationResponse -> TestTree
responseDescribeFleetLocationUtilization =
  res
    "DescribeFleetLocationUtilizationResponse"
    "fixture/DescribeFleetLocationUtilizationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetLocationUtilization)

responseDescribeGameServerInstances :: DescribeGameServerInstancesResponse -> TestTree
responseDescribeGameServerInstances =
  res
    "DescribeGameServerInstancesResponse"
    "fixture/DescribeGameServerInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGameServerInstances)

responseStartGameSessionPlacement :: StartGameSessionPlacementResponse -> TestTree
responseStartGameSessionPlacement =
  res
    "StartGameSessionPlacementResponse"
    "fixture/StartGameSessionPlacementResponse.proto"
    defaultService
    (Proxy :: Proxy StartGameSessionPlacement)

responseDeleteMatchmakingRuleSet :: DeleteMatchmakingRuleSetResponse -> TestTree
responseDeleteMatchmakingRuleSet =
  res
    "DeleteMatchmakingRuleSetResponse"
    "fixture/DeleteMatchmakingRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMatchmakingRuleSet)

responseDeleteFleetLocations :: DeleteFleetLocationsResponse -> TestTree
responseDeleteFleetLocations =
  res
    "DeleteFleetLocationsResponse"
    "fixture/DeleteFleetLocationsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFleetLocations)

responseStopGameSessionPlacement :: StopGameSessionPlacementResponse -> TestTree
responseStopGameSessionPlacement =
  res
    "StopGameSessionPlacementResponse"
    "fixture/StopGameSessionPlacementResponse.proto"
    defaultService
    (Proxy :: Proxy StopGameSessionPlacement)

responseUpdateScript :: UpdateScriptResponse -> TestTree
responseUpdateScript =
  res
    "UpdateScriptResponse"
    "fixture/UpdateScriptResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateScript)

responseDescribeGameSessions :: DescribeGameSessionsResponse -> TestTree
responseDescribeGameSessions =
  res
    "DescribeGameSessionsResponse"
    "fixture/DescribeGameSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGameSessions)

responseDeleteScript :: DeleteScriptResponse -> TestTree
responseDeleteScript =
  res
    "DeleteScriptResponse"
    "fixture/DeleteScriptResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteScript)

responseDescribeGameServer :: DescribeGameServerResponse -> TestTree
responseDescribeGameServer =
  res
    "DescribeGameServerResponse"
    "fixture/DescribeGameServerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGameServer)

responseListScripts :: ListScriptsResponse -> TestTree
responseListScripts =
  res
    "ListScriptsResponse"
    "fixture/ListScriptsResponse.proto"
    defaultService
    (Proxy :: Proxy ListScripts)

responseDescribeEC2InstanceLimits :: DescribeEC2InstanceLimitsResponse -> TestTree
responseDescribeEC2InstanceLimits =
  res
    "DescribeEC2InstanceLimitsResponse"
    "fixture/DescribeEC2InstanceLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEC2InstanceLimits)

responseCreateScript :: CreateScriptResponse -> TestTree
responseCreateScript =
  res
    "CreateScriptResponse"
    "fixture/CreateScriptResponse.proto"
    defaultService
    (Proxy :: Proxy CreateScript)

responseStartFleetActions :: StartFleetActionsResponse -> TestTree
responseStartFleetActions =
  res
    "StartFleetActionsResponse"
    "fixture/StartFleetActionsResponse.proto"
    defaultService
    (Proxy :: Proxy StartFleetActions)

responseGetInstanceAccess :: GetInstanceAccessResponse -> TestTree
responseGetInstanceAccess =
  res
    "GetInstanceAccessResponse"
    "fixture/GetInstanceAccessResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceAccess)

responseDescribePlayerSessions :: DescribePlayerSessionsResponse -> TestTree
responseDescribePlayerSessions =
  res
    "DescribePlayerSessionsResponse"
    "fixture/DescribePlayerSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePlayerSessions)

responseStopFleetActions :: StopFleetActionsResponse -> TestTree
responseStopFleetActions =
  res
    "StopFleetActionsResponse"
    "fixture/StopFleetActionsResponse.proto"
    defaultService
    (Proxy :: Proxy StopFleetActions)

responseCreateMatchmakingConfiguration :: CreateMatchmakingConfigurationResponse -> TestTree
responseCreateMatchmakingConfiguration =
  res
    "CreateMatchmakingConfigurationResponse"
    "fixture/CreateMatchmakingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMatchmakingConfiguration)

responseCreateGameServerGroup :: CreateGameServerGroupResponse -> TestTree
responseCreateGameServerGroup =
  res
    "CreateGameServerGroupResponse"
    "fixture/CreateGameServerGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGameServerGroup)

responseCreateVpcPeeringAuthorization :: CreateVpcPeeringAuthorizationResponse -> TestTree
responseCreateVpcPeeringAuthorization =
  res
    "CreateVpcPeeringAuthorizationResponse"
    "fixture/CreateVpcPeeringAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcPeeringAuthorization)

responseCreateVpcPeeringConnection :: CreateVpcPeeringConnectionResponse -> TestTree
responseCreateVpcPeeringConnection =
  res
    "CreateVpcPeeringConnectionResponse"
    "fixture/CreateVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcPeeringConnection)

responseResumeGameServerGroup :: ResumeGameServerGroupResponse -> TestTree
responseResumeGameServerGroup =
  res
    "ResumeGameServerGroupResponse"
    "fixture/ResumeGameServerGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ResumeGameServerGroup)

responseDeleteGameServerGroup :: DeleteGameServerGroupResponse -> TestTree
responseDeleteGameServerGroup =
  res
    "DeleteGameServerGroupResponse"
    "fixture/DeleteGameServerGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGameServerGroup)

responseUpdateGameServerGroup :: UpdateGameServerGroupResponse -> TestTree
responseUpdateGameServerGroup =
  res
    "UpdateGameServerGroupResponse"
    "fixture/UpdateGameServerGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGameServerGroup)

responseUpdateRuntimeConfiguration :: UpdateRuntimeConfigurationResponse -> TestTree
responseUpdateRuntimeConfiguration =
  res
    "UpdateRuntimeConfigurationResponse"
    "fixture/UpdateRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRuntimeConfiguration)

responseDeleteVpcPeeringAuthorization :: DeleteVpcPeeringAuthorizationResponse -> TestTree
responseDeleteVpcPeeringAuthorization =
  res
    "DeleteVpcPeeringAuthorizationResponse"
    "fixture/DeleteVpcPeeringAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcPeeringAuthorization)

responseCreateGameSessionQueue :: CreateGameSessionQueueResponse -> TestTree
responseCreateGameSessionQueue =
  res
    "CreateGameSessionQueueResponse"
    "fixture/CreateGameSessionQueueResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGameSessionQueue)

responseSearchGameSessions :: SearchGameSessionsResponse -> TestTree
responseSearchGameSessions =
  res
    "SearchGameSessionsResponse"
    "fixture/SearchGameSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy SearchGameSessions)

responseListGameServerGroups :: ListGameServerGroupsResponse -> TestTree
responseListGameServerGroups =
  res
    "ListGameServerGroupsResponse"
    "fixture/ListGameServerGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGameServerGroups)

responseCreatePlayerSessions :: CreatePlayerSessionsResponse -> TestTree
responseCreatePlayerSessions =
  res
    "CreatePlayerSessionsResponse"
    "fixture/CreatePlayerSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePlayerSessions)

responseCreateBuild :: CreateBuildResponse -> TestTree
responseCreateBuild =
  res
    "CreateBuildResponse"
    "fixture/CreateBuildResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBuild)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAliases)

responseDescribeScript :: DescribeScriptResponse -> TestTree
responseDescribeScript =
  res
    "DescribeScriptResponse"
    "fixture/DescribeScriptResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScript)

responseDescribeVpcPeeringAuthorizations :: DescribeVpcPeeringAuthorizationsResponse -> TestTree
responseDescribeVpcPeeringAuthorizations =
  res
    "DescribeVpcPeeringAuthorizationsResponse"
    "fixture/DescribeVpcPeeringAuthorizationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcPeeringAuthorizations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAlias)

responseListGameServers :: ListGameServersResponse -> TestTree
responseListGameServers =
  res
    "ListGameServersResponse"
    "fixture/ListGameServersResponse.proto"
    defaultService
    (Proxy :: Proxy ListGameServers)

responseUpdateGameServer :: UpdateGameServerResponse -> TestTree
responseUpdateGameServer =
  res
    "UpdateGameServerResponse"
    "fixture/UpdateGameServerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGameServer)
