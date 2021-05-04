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
--         [ requestDescribeGameSessionQueues $
--             newDescribeGameSessionQueues
--
--         , requestDeleteBuild $
--             newDeleteBuild
--
--         , requestDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnections
--
--         , requestDescribeFleetPortSettings $
--             newDescribeFleetPortSettings
--
--         , requestDescribeInstances $
--             newDescribeInstances
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestDescribeFleetCapacity $
--             newDescribeFleetCapacity
--
--         , requestListBuilds $
--             newListBuilds
--
--         , requestUpdateBuild $
--             newUpdateBuild
--
--         , requestUpdateAlias $
--             newUpdateAlias
--
--         , requestDescribeFleetAttributes $
--             newDescribeFleetAttributes
--
--         , requestPutScalingPolicy $
--             newPutScalingPolicy
--
--         , requestDescribeFleetEvents $
--             newDescribeFleetEvents
--
--         , requestDescribeFleetUtilization $
--             newDescribeFleetUtilization
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
--         , requestDescribeMatchmaking $
--             newDescribeMatchmaking
--
--         , requestGetGameSessionLogUrl $
--             newGetGameSessionLogUrl
--
--         , requestCreatePlayerSession $
--             newCreatePlayerSession
--
--         , requestDescribeRuntimeConfiguration $
--             newDescribeRuntimeConfiguration
--
--         , requestDescribeScalingPolicies $
--             newDescribeScalingPolicies
--
--         , requestSuspendGameServerGroup $
--             newSuspendGameServerGroup
--
--         , requestDescribeMatchmakingRuleSets $
--             newDescribeMatchmakingRuleSets
--
--         , requestValidateMatchmakingRuleSet $
--             newValidateMatchmakingRuleSet
--
--         , requestUpdateFleetPortSettings $
--             newUpdateFleetPortSettings
--
--         , requestDescribeBuild $
--             newDescribeBuild
--
--         , requestAcceptMatch $
--             newAcceptMatch
--
--         , requestDeregisterGameServer $
--             newDeregisterGameServer
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateFleetCapacity $
--             newUpdateFleetCapacity
--
--         , requestDescribeAlias $
--             newDescribeAlias
--
--         , requestDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnection
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
--         , requestDeleteGameSessionQueue $
--             newDeleteGameSessionQueue
--
--         , requestUpdateMatchmakingConfiguration $
--             newUpdateMatchmakingConfiguration
--
--         , requestDeleteMatchmakingConfiguration $
--             newDeleteMatchmakingConfiguration
--
--         , requestDescribeMatchmakingConfigurations $
--             newDescribeMatchmakingConfigurations
--
--         , requestCreateFleet $
--             newCreateFleet
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
--         , requestStartMatchBackfill $
--             newStartMatchBackfill
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
--         , requestStopMatchmaking $
--             newStopMatchmaking
--
--         , requestDescribeGameServerInstances $
--             newDescribeGameServerInstances
--
--         , requestCreateGameSession $
--             newCreateGameSession
--
--         , requestCreateMatchmakingRuleSet $
--             newCreateMatchmakingRuleSet
--
--         , requestStartMatchmaking $
--             newStartMatchmaking
--
--         , requestDescribeGameSessionPlacement $
--             newDescribeGameSessionPlacement
--
--         , requestDeleteScalingPolicy $
--             newDeleteScalingPolicy
--
--         , requestStopGameSessionPlacement $
--             newStopGameSessionPlacement
--
--         , requestStartGameSessionPlacement $
--             newStartGameSessionPlacement
--
--         , requestDeleteMatchmakingRuleSet $
--             newDeleteMatchmakingRuleSet
--
--         , requestDescribeGameServer $
--             newDescribeGameServer
--
--         , requestDeleteScript $
--             newDeleteScript
--
--         , requestListScripts $
--             newListScripts
--
--         , requestDescribeGameSessions $
--             newDescribeGameSessions
--
--         , requestUpdateScript $
--             newUpdateScript
--
--         , requestDescribeEC2InstanceLimits $
--             newDescribeEC2InstanceLimits
--
--         , requestStopFleetActions $
--             newStopFleetActions
--
--         , requestGetInstanceAccess $
--             newGetInstanceAccess
--
--         , requestStartFleetActions $
--             newStartFleetActions
--
--         , requestDescribePlayerSessions $
--             newDescribePlayerSessions
--
--         , requestCreateScript $
--             newCreateScript
--
--         , requestCreateMatchmakingConfiguration $
--             newCreateMatchmakingConfiguration
--
--         , requestCreateVpcPeeringAuthorization $
--             newCreateVpcPeeringAuthorization
--
--         , requestCreateGameServerGroup $
--             newCreateGameServerGroup
--
--         , requestUpdateGameServerGroup $
--             newUpdateGameServerGroup
--
--         , requestSearchGameSessions $
--             newSearchGameSessions
--
--         , requestDeleteGameServerGroup $
--             newDeleteGameServerGroup
--
--         , requestListGameServerGroups $
--             newListGameServerGroups
--
--         , requestUpdateRuntimeConfiguration $
--             newUpdateRuntimeConfiguration
--
--         , requestCreateGameSessionQueue $
--             newCreateGameSessionQueue
--
--         , requestResumeGameServerGroup $
--             newResumeGameServerGroup
--
--         , requestDeleteVpcPeeringAuthorization $
--             newDeleteVpcPeeringAuthorization
--
--         , requestCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnection
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeScript $
--             newDescribeScript
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestCreatePlayerSessions $
--             newCreatePlayerSessions
--
--         , requestUpdateGameServer $
--             newUpdateGameServer
--
--         , requestDescribeVpcPeeringAuthorizations $
--             newDescribeVpcPeeringAuthorizations
--
--         , requestListGameServers $
--             newListGameServers
--
--         , requestCreateBuild $
--             newCreateBuild
--
--         , requestListAliases $
--             newListAliases
--
--           ]

--     , testGroup "response"
--         [ responseDescribeGameSessionQueues $
--             newDescribeGameSessionQueuesResponse
--
--         , responseDeleteBuild $
--             newDeleteBuildResponse
--
--         , responseDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnectionsResponse
--
--         , responseDescribeFleetPortSettings $
--             newDescribeFleetPortSettingsResponse
--
--         , responseDescribeInstances $
--             newDescribeInstancesResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseDescribeFleetCapacity $
--             newDescribeFleetCapacityResponse
--
--         , responseListBuilds $
--             newListBuildsResponse
--
--         , responseUpdateBuild $
--             newUpdateBuildResponse
--
--         , responseUpdateAlias $
--             newUpdateAliasResponse
--
--         , responseDescribeFleetAttributes $
--             newDescribeFleetAttributesResponse
--
--         , responsePutScalingPolicy $
--             newPutScalingPolicyResponse
--
--         , responseDescribeFleetEvents $
--             newDescribeFleetEventsResponse
--
--         , responseDescribeFleetUtilization $
--             newDescribeFleetUtilizationResponse
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
--         , responseDescribeMatchmaking $
--             newDescribeMatchmakingResponse
--
--         , responseGetGameSessionLogUrl $
--             newGetGameSessionLogUrlResponse
--
--         , responseCreatePlayerSession $
--             newCreatePlayerSessionResponse
--
--         , responseDescribeRuntimeConfiguration $
--             newDescribeRuntimeConfigurationResponse
--
--         , responseDescribeScalingPolicies $
--             newDescribeScalingPoliciesResponse
--
--         , responseSuspendGameServerGroup $
--             newSuspendGameServerGroupResponse
--
--         , responseDescribeMatchmakingRuleSets $
--             newDescribeMatchmakingRuleSetsResponse
--
--         , responseValidateMatchmakingRuleSet $
--             newValidateMatchmakingRuleSetResponse
--
--         , responseUpdateFleetPortSettings $
--             newUpdateFleetPortSettingsResponse
--
--         , responseDescribeBuild $
--             newDescribeBuildResponse
--
--         , responseAcceptMatch $
--             newAcceptMatchResponse
--
--         , responseDeregisterGameServer $
--             newDeregisterGameServerResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateFleetCapacity $
--             newUpdateFleetCapacityResponse
--
--         , responseDescribeAlias $
--             newDescribeAliasResponse
--
--         , responseDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnectionResponse
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
--         , responseDeleteGameSessionQueue $
--             newDeleteGameSessionQueueResponse
--
--         , responseUpdateMatchmakingConfiguration $
--             newUpdateMatchmakingConfigurationResponse
--
--         , responseDeleteMatchmakingConfiguration $
--             newDeleteMatchmakingConfigurationResponse
--
--         , responseDescribeMatchmakingConfigurations $
--             newDescribeMatchmakingConfigurationsResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
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
--         , responseStartMatchBackfill $
--             newStartMatchBackfillResponse
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
--         , responseStopMatchmaking $
--             newStopMatchmakingResponse
--
--         , responseDescribeGameServerInstances $
--             newDescribeGameServerInstancesResponse
--
--         , responseCreateGameSession $
--             newCreateGameSessionResponse
--
--         , responseCreateMatchmakingRuleSet $
--             newCreateMatchmakingRuleSetResponse
--
--         , responseStartMatchmaking $
--             newStartMatchmakingResponse
--
--         , responseDescribeGameSessionPlacement $
--             newDescribeGameSessionPlacementResponse
--
--         , responseDeleteScalingPolicy $
--             newDeleteScalingPolicyResponse
--
--         , responseStopGameSessionPlacement $
--             newStopGameSessionPlacementResponse
--
--         , responseStartGameSessionPlacement $
--             newStartGameSessionPlacementResponse
--
--         , responseDeleteMatchmakingRuleSet $
--             newDeleteMatchmakingRuleSetResponse
--
--         , responseDescribeGameServer $
--             newDescribeGameServerResponse
--
--         , responseDeleteScript $
--             newDeleteScriptResponse
--
--         , responseListScripts $
--             newListScriptsResponse
--
--         , responseDescribeGameSessions $
--             newDescribeGameSessionsResponse
--
--         , responseUpdateScript $
--             newUpdateScriptResponse
--
--         , responseDescribeEC2InstanceLimits $
--             newDescribeEC2InstanceLimitsResponse
--
--         , responseStopFleetActions $
--             newStopFleetActionsResponse
--
--         , responseGetInstanceAccess $
--             newGetInstanceAccessResponse
--
--         , responseStartFleetActions $
--             newStartFleetActionsResponse
--
--         , responseDescribePlayerSessions $
--             newDescribePlayerSessionsResponse
--
--         , responseCreateScript $
--             newCreateScriptResponse
--
--         , responseCreateMatchmakingConfiguration $
--             newCreateMatchmakingConfigurationResponse
--
--         , responseCreateVpcPeeringAuthorization $
--             newCreateVpcPeeringAuthorizationResponse
--
--         , responseCreateGameServerGroup $
--             newCreateGameServerGroupResponse
--
--         , responseUpdateGameServerGroup $
--             newUpdateGameServerGroupResponse
--
--         , responseSearchGameSessions $
--             newSearchGameSessionsResponse
--
--         , responseDeleteGameServerGroup $
--             newDeleteGameServerGroupResponse
--
--         , responseListGameServerGroups $
--             newListGameServerGroupsResponse
--
--         , responseUpdateRuntimeConfiguration $
--             newUpdateRuntimeConfigurationResponse
--
--         , responseCreateGameSessionQueue $
--             newCreateGameSessionQueueResponse
--
--         , responseResumeGameServerGroup $
--             newResumeGameServerGroupResponse
--
--         , responseDeleteVpcPeeringAuthorization $
--             newDeleteVpcPeeringAuthorizationResponse
--
--         , responseCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnectionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeScript $
--             newDescribeScriptResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseCreatePlayerSessions $
--             newCreatePlayerSessionsResponse
--
--         , responseUpdateGameServer $
--             newUpdateGameServerResponse
--
--         , responseDescribeVpcPeeringAuthorizations $
--             newDescribeVpcPeeringAuthorizationsResponse
--
--         , responseListGameServers $
--             newListGameServersResponse
--
--         , responseCreateBuild $
--             newCreateBuildResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--           ]
--     ]

-- Requests

requestDescribeGameSessionQueues :: DescribeGameSessionQueues -> TestTree
requestDescribeGameSessionQueues =
  req
    "DescribeGameSessionQueues"
    "fixture/DescribeGameSessionQueues.yaml"

requestDeleteBuild :: DeleteBuild -> TestTree
requestDeleteBuild =
  req
    "DeleteBuild"
    "fixture/DeleteBuild.yaml"

requestDescribeVpcPeeringConnections :: DescribeVpcPeeringConnections -> TestTree
requestDescribeVpcPeeringConnections =
  req
    "DescribeVpcPeeringConnections"
    "fixture/DescribeVpcPeeringConnections.yaml"

requestDescribeFleetPortSettings :: DescribeFleetPortSettings -> TestTree
requestDescribeFleetPortSettings =
  req
    "DescribeFleetPortSettings"
    "fixture/DescribeFleetPortSettings.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances =
  req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

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

requestUpdateBuild :: UpdateBuild -> TestTree
requestUpdateBuild =
  req
    "UpdateBuild"
    "fixture/UpdateBuild.yaml"

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

requestPutScalingPolicy :: PutScalingPolicy -> TestTree
requestPutScalingPolicy =
  req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

requestDescribeFleetEvents :: DescribeFleetEvents -> TestTree
requestDescribeFleetEvents =
  req
    "DescribeFleetEvents"
    "fixture/DescribeFleetEvents.yaml"

requestDescribeFleetUtilization :: DescribeFleetUtilization -> TestTree
requestDescribeFleetUtilization =
  req
    "DescribeFleetUtilization"
    "fixture/DescribeFleetUtilization.yaml"

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

requestCreatePlayerSession :: CreatePlayerSession -> TestTree
requestCreatePlayerSession =
  req
    "CreatePlayerSession"
    "fixture/CreatePlayerSession.yaml"

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

requestSuspendGameServerGroup :: SuspendGameServerGroup -> TestTree
requestSuspendGameServerGroup =
  req
    "SuspendGameServerGroup"
    "fixture/SuspendGameServerGroup.yaml"

requestDescribeMatchmakingRuleSets :: DescribeMatchmakingRuleSets -> TestTree
requestDescribeMatchmakingRuleSets =
  req
    "DescribeMatchmakingRuleSets"
    "fixture/DescribeMatchmakingRuleSets.yaml"

requestValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSet -> TestTree
requestValidateMatchmakingRuleSet =
  req
    "ValidateMatchmakingRuleSet"
    "fixture/ValidateMatchmakingRuleSet.yaml"

requestUpdateFleetPortSettings :: UpdateFleetPortSettings -> TestTree
requestUpdateFleetPortSettings =
  req
    "UpdateFleetPortSettings"
    "fixture/UpdateFleetPortSettings.yaml"

requestDescribeBuild :: DescribeBuild -> TestTree
requestDescribeBuild =
  req
    "DescribeBuild"
    "fixture/DescribeBuild.yaml"

requestAcceptMatch :: AcceptMatch -> TestTree
requestAcceptMatch =
  req
    "AcceptMatch"
    "fixture/AcceptMatch.yaml"

requestDeregisterGameServer :: DeregisterGameServer -> TestTree
requestDeregisterGameServer =
  req
    "DeregisterGameServer"
    "fixture/DeregisterGameServer.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

requestDeleteVpcPeeringConnection :: DeleteVpcPeeringConnection -> TestTree
requestDeleteVpcPeeringConnection =
  req
    "DeleteVpcPeeringConnection"
    "fixture/DeleteVpcPeeringConnection.yaml"

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

requestDeleteGameSessionQueue :: DeleteGameSessionQueue -> TestTree
requestDeleteGameSessionQueue =
  req
    "DeleteGameSessionQueue"
    "fixture/DeleteGameSessionQueue.yaml"

requestUpdateMatchmakingConfiguration :: UpdateMatchmakingConfiguration -> TestTree
requestUpdateMatchmakingConfiguration =
  req
    "UpdateMatchmakingConfiguration"
    "fixture/UpdateMatchmakingConfiguration.yaml"

requestDeleteMatchmakingConfiguration :: DeleteMatchmakingConfiguration -> TestTree
requestDeleteMatchmakingConfiguration =
  req
    "DeleteMatchmakingConfiguration"
    "fixture/DeleteMatchmakingConfiguration.yaml"

requestDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurations -> TestTree
requestDescribeMatchmakingConfigurations =
  req
    "DescribeMatchmakingConfigurations"
    "fixture/DescribeMatchmakingConfigurations.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

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

requestStartMatchBackfill :: StartMatchBackfill -> TestTree
requestStartMatchBackfill =
  req
    "StartMatchBackfill"
    "fixture/StartMatchBackfill.yaml"

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

requestCreateMatchmakingRuleSet :: CreateMatchmakingRuleSet -> TestTree
requestCreateMatchmakingRuleSet =
  req
    "CreateMatchmakingRuleSet"
    "fixture/CreateMatchmakingRuleSet.yaml"

requestStartMatchmaking :: StartMatchmaking -> TestTree
requestStartMatchmaking =
  req
    "StartMatchmaking"
    "fixture/StartMatchmaking.yaml"

requestDescribeGameSessionPlacement :: DescribeGameSessionPlacement -> TestTree
requestDescribeGameSessionPlacement =
  req
    "DescribeGameSessionPlacement"
    "fixture/DescribeGameSessionPlacement.yaml"

requestDeleteScalingPolicy :: DeleteScalingPolicy -> TestTree
requestDeleteScalingPolicy =
  req
    "DeleteScalingPolicy"
    "fixture/DeleteScalingPolicy.yaml"

requestStopGameSessionPlacement :: StopGameSessionPlacement -> TestTree
requestStopGameSessionPlacement =
  req
    "StopGameSessionPlacement"
    "fixture/StopGameSessionPlacement.yaml"

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

requestDescribeGameServer :: DescribeGameServer -> TestTree
requestDescribeGameServer =
  req
    "DescribeGameServer"
    "fixture/DescribeGameServer.yaml"

requestDeleteScript :: DeleteScript -> TestTree
requestDeleteScript =
  req
    "DeleteScript"
    "fixture/DeleteScript.yaml"

requestListScripts :: ListScripts -> TestTree
requestListScripts =
  req
    "ListScripts"
    "fixture/ListScripts.yaml"

requestDescribeGameSessions :: DescribeGameSessions -> TestTree
requestDescribeGameSessions =
  req
    "DescribeGameSessions"
    "fixture/DescribeGameSessions.yaml"

requestUpdateScript :: UpdateScript -> TestTree
requestUpdateScript =
  req
    "UpdateScript"
    "fixture/UpdateScript.yaml"

requestDescribeEC2InstanceLimits :: DescribeEC2InstanceLimits -> TestTree
requestDescribeEC2InstanceLimits =
  req
    "DescribeEC2InstanceLimits"
    "fixture/DescribeEC2InstanceLimits.yaml"

requestStopFleetActions :: StopFleetActions -> TestTree
requestStopFleetActions =
  req
    "StopFleetActions"
    "fixture/StopFleetActions.yaml"

requestGetInstanceAccess :: GetInstanceAccess -> TestTree
requestGetInstanceAccess =
  req
    "GetInstanceAccess"
    "fixture/GetInstanceAccess.yaml"

requestStartFleetActions :: StartFleetActions -> TestTree
requestStartFleetActions =
  req
    "StartFleetActions"
    "fixture/StartFleetActions.yaml"

requestDescribePlayerSessions :: DescribePlayerSessions -> TestTree
requestDescribePlayerSessions =
  req
    "DescribePlayerSessions"
    "fixture/DescribePlayerSessions.yaml"

requestCreateScript :: CreateScript -> TestTree
requestCreateScript =
  req
    "CreateScript"
    "fixture/CreateScript.yaml"

requestCreateMatchmakingConfiguration :: CreateMatchmakingConfiguration -> TestTree
requestCreateMatchmakingConfiguration =
  req
    "CreateMatchmakingConfiguration"
    "fixture/CreateMatchmakingConfiguration.yaml"

requestCreateVpcPeeringAuthorization :: CreateVpcPeeringAuthorization -> TestTree
requestCreateVpcPeeringAuthorization =
  req
    "CreateVpcPeeringAuthorization"
    "fixture/CreateVpcPeeringAuthorization.yaml"

requestCreateGameServerGroup :: CreateGameServerGroup -> TestTree
requestCreateGameServerGroup =
  req
    "CreateGameServerGroup"
    "fixture/CreateGameServerGroup.yaml"

requestUpdateGameServerGroup :: UpdateGameServerGroup -> TestTree
requestUpdateGameServerGroup =
  req
    "UpdateGameServerGroup"
    "fixture/UpdateGameServerGroup.yaml"

requestSearchGameSessions :: SearchGameSessions -> TestTree
requestSearchGameSessions =
  req
    "SearchGameSessions"
    "fixture/SearchGameSessions.yaml"

requestDeleteGameServerGroup :: DeleteGameServerGroup -> TestTree
requestDeleteGameServerGroup =
  req
    "DeleteGameServerGroup"
    "fixture/DeleteGameServerGroup.yaml"

requestListGameServerGroups :: ListGameServerGroups -> TestTree
requestListGameServerGroups =
  req
    "ListGameServerGroups"
    "fixture/ListGameServerGroups.yaml"

requestUpdateRuntimeConfiguration :: UpdateRuntimeConfiguration -> TestTree
requestUpdateRuntimeConfiguration =
  req
    "UpdateRuntimeConfiguration"
    "fixture/UpdateRuntimeConfiguration.yaml"

requestCreateGameSessionQueue :: CreateGameSessionQueue -> TestTree
requestCreateGameSessionQueue =
  req
    "CreateGameSessionQueue"
    "fixture/CreateGameSessionQueue.yaml"

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

requestCreateVpcPeeringConnection :: CreateVpcPeeringConnection -> TestTree
requestCreateVpcPeeringConnection =
  req
    "CreateVpcPeeringConnection"
    "fixture/CreateVpcPeeringConnection.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeScript :: DescribeScript -> TestTree
requestDescribeScript =
  req
    "DescribeScript"
    "fixture/DescribeScript.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestCreatePlayerSessions :: CreatePlayerSessions -> TestTree
requestCreatePlayerSessions =
  req
    "CreatePlayerSessions"
    "fixture/CreatePlayerSessions.yaml"

requestUpdateGameServer :: UpdateGameServer -> TestTree
requestUpdateGameServer =
  req
    "UpdateGameServer"
    "fixture/UpdateGameServer.yaml"

requestDescribeVpcPeeringAuthorizations :: DescribeVpcPeeringAuthorizations -> TestTree
requestDescribeVpcPeeringAuthorizations =
  req
    "DescribeVpcPeeringAuthorizations"
    "fixture/DescribeVpcPeeringAuthorizations.yaml"

requestListGameServers :: ListGameServers -> TestTree
requestListGameServers =
  req
    "ListGameServers"
    "fixture/ListGameServers.yaml"

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

-- Responses

responseDescribeGameSessionQueues :: DescribeGameSessionQueuesResponse -> TestTree
responseDescribeGameSessionQueues =
  res
    "DescribeGameSessionQueuesResponse"
    "fixture/DescribeGameSessionQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGameSessionQueues)

responseDeleteBuild :: DeleteBuildResponse -> TestTree
responseDeleteBuild =
  res
    "DeleteBuildResponse"
    "fixture/DeleteBuildResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBuild)

responseDescribeVpcPeeringConnections :: DescribeVpcPeeringConnectionsResponse -> TestTree
responseDescribeVpcPeeringConnections =
  res
    "DescribeVpcPeeringConnectionsResponse"
    "fixture/DescribeVpcPeeringConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcPeeringConnections)

responseDescribeFleetPortSettings :: DescribeFleetPortSettingsResponse -> TestTree
responseDescribeFleetPortSettings =
  res
    "DescribeFleetPortSettingsResponse"
    "fixture/DescribeFleetPortSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetPortSettings)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstances)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAlias)

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

responseUpdateBuild :: UpdateBuildResponse -> TestTree
responseUpdateBuild =
  res
    "UpdateBuildResponse"
    "fixture/UpdateBuildResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBuild)

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

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy =
  res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutScalingPolicy)

responseDescribeFleetEvents :: DescribeFleetEventsResponse -> TestTree
responseDescribeFleetEvents =
  res
    "DescribeFleetEventsResponse"
    "fixture/DescribeFleetEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetEvents)

responseDescribeFleetUtilization :: DescribeFleetUtilizationResponse -> TestTree
responseDescribeFleetUtilization =
  res
    "DescribeFleetUtilizationResponse"
    "fixture/DescribeFleetUtilizationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetUtilization)

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

responseCreatePlayerSession :: CreatePlayerSessionResponse -> TestTree
responseCreatePlayerSession =
  res
    "CreatePlayerSessionResponse"
    "fixture/CreatePlayerSessionResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePlayerSession)

responseDescribeRuntimeConfiguration :: DescribeRuntimeConfigurationResponse -> TestTree
responseDescribeRuntimeConfiguration =
  res
    "DescribeRuntimeConfigurationResponse"
    "fixture/DescribeRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRuntimeConfiguration)

responseDescribeScalingPolicies :: DescribeScalingPoliciesResponse -> TestTree
responseDescribeScalingPolicies =
  res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScalingPolicies)

responseSuspendGameServerGroup :: SuspendGameServerGroupResponse -> TestTree
responseSuspendGameServerGroup =
  res
    "SuspendGameServerGroupResponse"
    "fixture/SuspendGameServerGroupResponse.proto"
    defaultService
    (Proxy :: Proxy SuspendGameServerGroup)

responseDescribeMatchmakingRuleSets :: DescribeMatchmakingRuleSetsResponse -> TestTree
responseDescribeMatchmakingRuleSets =
  res
    "DescribeMatchmakingRuleSetsResponse"
    "fixture/DescribeMatchmakingRuleSetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMatchmakingRuleSets)

responseValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSetResponse -> TestTree
responseValidateMatchmakingRuleSet =
  res
    "ValidateMatchmakingRuleSetResponse"
    "fixture/ValidateMatchmakingRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy ValidateMatchmakingRuleSet)

responseUpdateFleetPortSettings :: UpdateFleetPortSettingsResponse -> TestTree
responseUpdateFleetPortSettings =
  res
    "UpdateFleetPortSettingsResponse"
    "fixture/UpdateFleetPortSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFleetPortSettings)

responseDescribeBuild :: DescribeBuildResponse -> TestTree
responseDescribeBuild =
  res
    "DescribeBuildResponse"
    "fixture/DescribeBuildResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBuild)

responseAcceptMatch :: AcceptMatchResponse -> TestTree
responseAcceptMatch =
  res
    "AcceptMatchResponse"
    "fixture/AcceptMatchResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptMatch)

responseDeregisterGameServer :: DeregisterGameServerResponse -> TestTree
responseDeregisterGameServer =
  res
    "DeregisterGameServerResponse"
    "fixture/DeregisterGameServerResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterGameServer)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

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

responseDeleteVpcPeeringConnection :: DeleteVpcPeeringConnectionResponse -> TestTree
responseDeleteVpcPeeringConnection =
  res
    "DeleteVpcPeeringConnectionResponse"
    "fixture/DeleteVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcPeeringConnection)

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

responseDeleteGameSessionQueue :: DeleteGameSessionQueueResponse -> TestTree
responseDeleteGameSessionQueue =
  res
    "DeleteGameSessionQueueResponse"
    "fixture/DeleteGameSessionQueueResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGameSessionQueue)

responseUpdateMatchmakingConfiguration :: UpdateMatchmakingConfigurationResponse -> TestTree
responseUpdateMatchmakingConfiguration =
  res
    "UpdateMatchmakingConfigurationResponse"
    "fixture/UpdateMatchmakingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMatchmakingConfiguration)

responseDeleteMatchmakingConfiguration :: DeleteMatchmakingConfigurationResponse -> TestTree
responseDeleteMatchmakingConfiguration =
  res
    "DeleteMatchmakingConfigurationResponse"
    "fixture/DeleteMatchmakingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMatchmakingConfiguration)

responseDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurationsResponse -> TestTree
responseDescribeMatchmakingConfigurations =
  res
    "DescribeMatchmakingConfigurationsResponse"
    "fixture/DescribeMatchmakingConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMatchmakingConfigurations)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFleet)

responseRegisterGameServer :: RegisterGameServerResponse -> TestTree
responseRegisterGameServer =
  res
    "RegisterGameServerResponse"
    "fixture/RegisterGameServerResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterGameServer)

responseRequestUploadCredentials :: RequestUploadCredentialsResponse -> TestTree
responseRequestUploadCredentials =
  res
    "RequestUploadCredentialsResponse"
    "fixture/RequestUploadCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy RequestUploadCredentials)

responseResolveAlias :: ResolveAliasResponse -> TestTree
responseResolveAlias =
  res
    "ResolveAliasResponse"
    "fixture/ResolveAliasResponse.proto"
    defaultService
    (Proxy :: Proxy ResolveAlias)

responseStartMatchBackfill :: StartMatchBackfillResponse -> TestTree
responseStartMatchBackfill =
  res
    "StartMatchBackfillResponse"
    "fixture/StartMatchBackfillResponse.proto"
    defaultService
    (Proxy :: Proxy StartMatchBackfill)

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

responseStopMatchmaking :: StopMatchmakingResponse -> TestTree
responseStopMatchmaking =
  res
    "StopMatchmakingResponse"
    "fixture/StopMatchmakingResponse.proto"
    defaultService
    (Proxy :: Proxy StopMatchmaking)

responseDescribeGameServerInstances :: DescribeGameServerInstancesResponse -> TestTree
responseDescribeGameServerInstances =
  res
    "DescribeGameServerInstancesResponse"
    "fixture/DescribeGameServerInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGameServerInstances)

responseCreateGameSession :: CreateGameSessionResponse -> TestTree
responseCreateGameSession =
  res
    "CreateGameSessionResponse"
    "fixture/CreateGameSessionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGameSession)

responseCreateMatchmakingRuleSet :: CreateMatchmakingRuleSetResponse -> TestTree
responseCreateMatchmakingRuleSet =
  res
    "CreateMatchmakingRuleSetResponse"
    "fixture/CreateMatchmakingRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMatchmakingRuleSet)

responseStartMatchmaking :: StartMatchmakingResponse -> TestTree
responseStartMatchmaking =
  res
    "StartMatchmakingResponse"
    "fixture/StartMatchmakingResponse.proto"
    defaultService
    (Proxy :: Proxy StartMatchmaking)

responseDescribeGameSessionPlacement :: DescribeGameSessionPlacementResponse -> TestTree
responseDescribeGameSessionPlacement =
  res
    "DescribeGameSessionPlacementResponse"
    "fixture/DescribeGameSessionPlacementResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGameSessionPlacement)

responseDeleteScalingPolicy :: DeleteScalingPolicyResponse -> TestTree
responseDeleteScalingPolicy =
  res
    "DeleteScalingPolicyResponse"
    "fixture/DeleteScalingPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteScalingPolicy)

responseStopGameSessionPlacement :: StopGameSessionPlacementResponse -> TestTree
responseStopGameSessionPlacement =
  res
    "StopGameSessionPlacementResponse"
    "fixture/StopGameSessionPlacementResponse.proto"
    defaultService
    (Proxy :: Proxy StopGameSessionPlacement)

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

responseDescribeGameServer :: DescribeGameServerResponse -> TestTree
responseDescribeGameServer =
  res
    "DescribeGameServerResponse"
    "fixture/DescribeGameServerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGameServer)

responseDeleteScript :: DeleteScriptResponse -> TestTree
responseDeleteScript =
  res
    "DeleteScriptResponse"
    "fixture/DeleteScriptResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteScript)

responseListScripts :: ListScriptsResponse -> TestTree
responseListScripts =
  res
    "ListScriptsResponse"
    "fixture/ListScriptsResponse.proto"
    defaultService
    (Proxy :: Proxy ListScripts)

responseDescribeGameSessions :: DescribeGameSessionsResponse -> TestTree
responseDescribeGameSessions =
  res
    "DescribeGameSessionsResponse"
    "fixture/DescribeGameSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGameSessions)

responseUpdateScript :: UpdateScriptResponse -> TestTree
responseUpdateScript =
  res
    "UpdateScriptResponse"
    "fixture/UpdateScriptResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateScript)

responseDescribeEC2InstanceLimits :: DescribeEC2InstanceLimitsResponse -> TestTree
responseDescribeEC2InstanceLimits =
  res
    "DescribeEC2InstanceLimitsResponse"
    "fixture/DescribeEC2InstanceLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEC2InstanceLimits)

responseStopFleetActions :: StopFleetActionsResponse -> TestTree
responseStopFleetActions =
  res
    "StopFleetActionsResponse"
    "fixture/StopFleetActionsResponse.proto"
    defaultService
    (Proxy :: Proxy StopFleetActions)

responseGetInstanceAccess :: GetInstanceAccessResponse -> TestTree
responseGetInstanceAccess =
  res
    "GetInstanceAccessResponse"
    "fixture/GetInstanceAccessResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceAccess)

responseStartFleetActions :: StartFleetActionsResponse -> TestTree
responseStartFleetActions =
  res
    "StartFleetActionsResponse"
    "fixture/StartFleetActionsResponse.proto"
    defaultService
    (Proxy :: Proxy StartFleetActions)

responseDescribePlayerSessions :: DescribePlayerSessionsResponse -> TestTree
responseDescribePlayerSessions =
  res
    "DescribePlayerSessionsResponse"
    "fixture/DescribePlayerSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePlayerSessions)

responseCreateScript :: CreateScriptResponse -> TestTree
responseCreateScript =
  res
    "CreateScriptResponse"
    "fixture/CreateScriptResponse.proto"
    defaultService
    (Proxy :: Proxy CreateScript)

responseCreateMatchmakingConfiguration :: CreateMatchmakingConfigurationResponse -> TestTree
responseCreateMatchmakingConfiguration =
  res
    "CreateMatchmakingConfigurationResponse"
    "fixture/CreateMatchmakingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMatchmakingConfiguration)

responseCreateVpcPeeringAuthorization :: CreateVpcPeeringAuthorizationResponse -> TestTree
responseCreateVpcPeeringAuthorization =
  res
    "CreateVpcPeeringAuthorizationResponse"
    "fixture/CreateVpcPeeringAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcPeeringAuthorization)

responseCreateGameServerGroup :: CreateGameServerGroupResponse -> TestTree
responseCreateGameServerGroup =
  res
    "CreateGameServerGroupResponse"
    "fixture/CreateGameServerGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGameServerGroup)

responseUpdateGameServerGroup :: UpdateGameServerGroupResponse -> TestTree
responseUpdateGameServerGroup =
  res
    "UpdateGameServerGroupResponse"
    "fixture/UpdateGameServerGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGameServerGroup)

responseSearchGameSessions :: SearchGameSessionsResponse -> TestTree
responseSearchGameSessions =
  res
    "SearchGameSessionsResponse"
    "fixture/SearchGameSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy SearchGameSessions)

responseDeleteGameServerGroup :: DeleteGameServerGroupResponse -> TestTree
responseDeleteGameServerGroup =
  res
    "DeleteGameServerGroupResponse"
    "fixture/DeleteGameServerGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGameServerGroup)

responseListGameServerGroups :: ListGameServerGroupsResponse -> TestTree
responseListGameServerGroups =
  res
    "ListGameServerGroupsResponse"
    "fixture/ListGameServerGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGameServerGroups)

responseUpdateRuntimeConfiguration :: UpdateRuntimeConfigurationResponse -> TestTree
responseUpdateRuntimeConfiguration =
  res
    "UpdateRuntimeConfigurationResponse"
    "fixture/UpdateRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRuntimeConfiguration)

responseCreateGameSessionQueue :: CreateGameSessionQueueResponse -> TestTree
responseCreateGameSessionQueue =
  res
    "CreateGameSessionQueueResponse"
    "fixture/CreateGameSessionQueueResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGameSessionQueue)

responseResumeGameServerGroup :: ResumeGameServerGroupResponse -> TestTree
responseResumeGameServerGroup =
  res
    "ResumeGameServerGroupResponse"
    "fixture/ResumeGameServerGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ResumeGameServerGroup)

responseDeleteVpcPeeringAuthorization :: DeleteVpcPeeringAuthorizationResponse -> TestTree
responseDeleteVpcPeeringAuthorization =
  res
    "DeleteVpcPeeringAuthorizationResponse"
    "fixture/DeleteVpcPeeringAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcPeeringAuthorization)

responseCreateVpcPeeringConnection :: CreateVpcPeeringConnectionResponse -> TestTree
responseCreateVpcPeeringConnection =
  res
    "CreateVpcPeeringConnectionResponse"
    "fixture/CreateVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcPeeringConnection)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeScript :: DescribeScriptResponse -> TestTree
responseDescribeScript =
  res
    "DescribeScriptResponse"
    "fixture/DescribeScriptResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScript)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAlias)

responseCreatePlayerSessions :: CreatePlayerSessionsResponse -> TestTree
responseCreatePlayerSessions =
  res
    "CreatePlayerSessionsResponse"
    "fixture/CreatePlayerSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePlayerSessions)

responseUpdateGameServer :: UpdateGameServerResponse -> TestTree
responseUpdateGameServer =
  res
    "UpdateGameServerResponse"
    "fixture/UpdateGameServerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGameServer)

responseDescribeVpcPeeringAuthorizations :: DescribeVpcPeeringAuthorizationsResponse -> TestTree
responseDescribeVpcPeeringAuthorizations =
  res
    "DescribeVpcPeeringAuthorizationsResponse"
    "fixture/DescribeVpcPeeringAuthorizationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcPeeringAuthorizations)

responseListGameServers :: ListGameServersResponse -> TestTree
responseListGameServers =
  res
    "ListGameServersResponse"
    "fixture/ListGameServersResponse.proto"
    defaultService
    (Proxy :: Proxy ListGameServers)

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
