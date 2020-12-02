{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.GameLift
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestStopMatchmaking $
--             stopMatchmaking
--
--         , requestDescribeGameServerInstances $
--             describeGameServerInstances
--
--         , requestCreateGameSession $
--             createGameSession
--
--         , requestDeleteScalingPolicy $
--             deleteScalingPolicy
--
--         , requestPutScalingPolicy $
--             putScalingPolicy
--
--         , requestListBuilds $
--             listBuilds
--
--         , requestDeleteFleet $
--             deleteFleet
--
--         , requestCreateBuild $
--             createBuild
--
--         , requestRequestUploadCredentials $
--             requestUploadCredentials
--
--         , requestCreateAlias $
--             createAlias
--
--         , requestListGameServers $
--             listGameServers
--
--         , requestResolveAlias $
--             resolveAlias
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestRegisterGameServer $
--             registerGameServer
--
--         , requestListAliases $
--             listAliases
--
--         , requestUpdateRuntimeConfiguration $
--             updateRuntimeConfiguration
--
--         , requestCreateVPCPeeringConnection $
--             createVPCPeeringConnection
--
--         , requestListGameServerGroups $
--             listGameServerGroups
--
--         , requestCreateGameSessionQueue $
--             createGameSessionQueue
--
--         , requestSearchGameSessions $
--             searchGameSessions
--
--         , requestCreateVPCPeeringAuthorization $
--             createVPCPeeringAuthorization
--
--         , requestUpdateGameSessionQueue $
--             updateGameSessionQueue
--
--         , requestDeleteGameSessionQueue $
--             deleteGameSessionQueue
--
--         , requestCreateGameServerGroup $
--             createGameServerGroup
--
--         , requestDeleteVPCPeeringConnection $
--             deleteVPCPeeringConnection
--
--         , requestStartFleetActions $
--             startFleetActions
--
--         , requestDeregisterGameServer $
--             deregisterGameServer
--
--         , requestGetInstanceAccess $
--             getInstanceAccess
--
--         , requestDescribeScalingPolicies $
--             describeScalingPolicies
--
--         , requestDescribeMatchmakingRuleSets $
--             describeMatchmakingRuleSets
--
--         , requestDescribeGameSessions $
--             describeGameSessions
--
--         , requestDescribeGameServer $
--             describeGameServer
--
--         , requestUpdateScript $
--             updateScript
--
--         , requestDeleteScript $
--             deleteScript
--
--         , requestStartGameSessionPlacement $
--             startGameSessionPlacement
--
--         , requestDescribeFleetUtilization $
--             describeFleetUtilization
--
--         , requestDescribeRuntimeConfiguration $
--             describeRuntimeConfiguration
--
--         , requestGetGameSessionLogURL $
--             getGameSessionLogURL
--
--         , requestDescribeFleetAttributes $
--             describeFleetAttributes
--
--         , requestDescribeGameSessionPlacement $
--             describeGameSessionPlacement
--
--         , requestDescribeFleetEvents $
--             describeFleetEvents
--
--         , requestStartMatchmaking $
--             startMatchmaking
--
--         , requestCreateMatchmakingRuleSet $
--             createMatchmakingRuleSet
--
--         , requestDescribeFleetCapacity $
--             describeFleetCapacity
--
--         , requestDeleteBuild $
--             deleteBuild
--
--         , requestUpdateBuild $
--             updateBuild
--
--         , requestListFleets $
--             listFleets
--
--         , requestDeleteAlias $
--             deleteAlias
--
--         , requestUpdateAlias $
--             updateAlias
--
--         , requestStartMatchBackfill $
--             startMatchBackfill
--
--         , requestDescribeInstances $
--             describeInstances
--
--         , requestDescribeGameSessionDetails $
--             describeGameSessionDetails
--
--         , requestDescribeFleetPortSettings $
--             describeFleetPortSettings
--
--         , requestDescribeGameSessionQueues $
--             describeGameSessionQueues
--
--         , requestDescribeVPCPeeringConnections $
--             describeVPCPeeringConnections
--
--         , requestDescribeScript $
--             describeScript
--
--         , requestCreatePlayerSessions $
--             createPlayerSessions
--
--         , requestDescribeMatchmakingConfigurations $
--             describeMatchmakingConfigurations
--
--         , requestDescribeVPCPeeringAuthorizations $
--             describeVPCPeeringAuthorizations
--
--         , requestUpdateGameServer $
--             updateGameServer
--
--         , requestCreateFleet $
--             createFleet
--
--         , requestDeleteMatchmakingConfiguration $
--             deleteMatchmakingConfiguration
--
--         , requestUpdateMatchmakingConfiguration $
--             updateMatchmakingConfiguration
--
--         , requestDeleteGameServerGroup $
--             deleteGameServerGroup
--
--         , requestUpdateGameServerGroup $
--             updateGameServerGroup
--
--         , requestResumeGameServerGroup $
--             resumeGameServerGroup
--
--         , requestDeleteVPCPeeringAuthorization $
--             deleteVPCPeeringAuthorization
--
--         , requestUpdateFleetAttributes $
--             updateFleetAttributes
--
--         , requestTagResource $
--             tagResource
--
--         , requestCreateMatchmakingConfiguration $
--             createMatchmakingConfiguration
--
--         , requestDescribePlayerSessions $
--             describePlayerSessions
--
--         , requestStopFleetActions $
--             stopFleetActions
--
--         , requestDescribeBuild $
--             describeBuild
--
--         , requestUpdateFleetPortSettings $
--             updateFleetPortSettings
--
--         , requestUpdateFleetCapacity $
--             updateFleetCapacity
--
--         , requestCreateScript $
--             createScript
--
--         , requestAcceptMatch $
--             acceptMatch
--
--         , requestUntagResource $
--             untagResource
--
--         , requestDescribeAlias $
--             describeAlias
--
--         , requestValidateMatchmakingRuleSet $
--             validateMatchmakingRuleSet
--
--         , requestListScripts $
--             listScripts
--
--         , requestDescribeEC2InstanceLimits $
--             describeEC2InstanceLimits
--
--         , requestSuspendGameServerGroup $
--             suspendGameServerGroup
--
--         , requestDeleteMatchmakingRuleSet $
--             deleteMatchmakingRuleSet
--
--         , requestStopGameSessionPlacement $
--             stopGameSessionPlacement
--
--         , requestClaimGameServer $
--             claimGameServer
--
--         , requestUpdateGameSession $
--             updateGameSession
--
--         , requestDescribeMatchmaking $
--             describeMatchmaking
--
--         , requestCreatePlayerSession $
--             createPlayerSession
--
--         , requestDescribeGameServerGroup $
--             describeGameServerGroup
--
--           ]

--     , testGroup "response"
--         [ responseStopMatchmaking $
--             stopMatchmakingResponse
--
--         , responseDescribeGameServerInstances $
--             describeGameServerInstancesResponse
--
--         , responseCreateGameSession $
--             createGameSessionResponse
--
--         , responseDeleteScalingPolicy $
--             deleteScalingPolicyResponse
--
--         , responsePutScalingPolicy $
--             putScalingPolicyResponse
--
--         , responseListBuilds $
--             listBuildsResponse
--
--         , responseDeleteFleet $
--             deleteFleetResponse
--
--         , responseCreateBuild $
--             createBuildResponse
--
--         , responseRequestUploadCredentials $
--             requestUploadCredentialsResponse
--
--         , responseCreateAlias $
--             createAliasResponse
--
--         , responseListGameServers $
--             listGameServersResponse
--
--         , responseResolveAlias $
--             resolveAliasResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseRegisterGameServer $
--             registerGameServerResponse
--
--         , responseListAliases $
--             listAliasesResponse
--
--         , responseUpdateRuntimeConfiguration $
--             updateRuntimeConfigurationResponse
--
--         , responseCreateVPCPeeringConnection $
--             createVPCPeeringConnectionResponse
--
--         , responseListGameServerGroups $
--             listGameServerGroupsResponse
--
--         , responseCreateGameSessionQueue $
--             createGameSessionQueueResponse
--
--         , responseSearchGameSessions $
--             searchGameSessionsResponse
--
--         , responseCreateVPCPeeringAuthorization $
--             createVPCPeeringAuthorizationResponse
--
--         , responseUpdateGameSessionQueue $
--             updateGameSessionQueueResponse
--
--         , responseDeleteGameSessionQueue $
--             deleteGameSessionQueueResponse
--
--         , responseCreateGameServerGroup $
--             createGameServerGroupResponse
--
--         , responseDeleteVPCPeeringConnection $
--             deleteVPCPeeringConnectionResponse
--
--         , responseStartFleetActions $
--             startFleetActionsResponse
--
--         , responseDeregisterGameServer $
--             deregisterGameServerResponse
--
--         , responseGetInstanceAccess $
--             getInstanceAccessResponse
--
--         , responseDescribeScalingPolicies $
--             describeScalingPoliciesResponse
--
--         , responseDescribeMatchmakingRuleSets $
--             describeMatchmakingRuleSetsResponse
--
--         , responseDescribeGameSessions $
--             describeGameSessionsResponse
--
--         , responseDescribeGameServer $
--             describeGameServerResponse
--
--         , responseUpdateScript $
--             updateScriptResponse
--
--         , responseDeleteScript $
--             deleteScriptResponse
--
--         , responseStartGameSessionPlacement $
--             startGameSessionPlacementResponse
--
--         , responseDescribeFleetUtilization $
--             describeFleetUtilizationResponse
--
--         , responseDescribeRuntimeConfiguration $
--             describeRuntimeConfigurationResponse
--
--         , responseGetGameSessionLogURL $
--             getGameSessionLogURLResponse
--
--         , responseDescribeFleetAttributes $
--             describeFleetAttributesResponse
--
--         , responseDescribeGameSessionPlacement $
--             describeGameSessionPlacementResponse
--
--         , responseDescribeFleetEvents $
--             describeFleetEventsResponse
--
--         , responseStartMatchmaking $
--             startMatchmakingResponse
--
--         , responseCreateMatchmakingRuleSet $
--             createMatchmakingRuleSetResponse
--
--         , responseDescribeFleetCapacity $
--             describeFleetCapacityResponse
--
--         , responseDeleteBuild $
--             deleteBuildResponse
--
--         , responseUpdateBuild $
--             updateBuildResponse
--
--         , responseListFleets $
--             listFleetsResponse
--
--         , responseDeleteAlias $
--             deleteAliasResponse
--
--         , responseUpdateAlias $
--             updateAliasResponse
--
--         , responseStartMatchBackfill $
--             startMatchBackfillResponse
--
--         , responseDescribeInstances $
--             describeInstancesResponse
--
--         , responseDescribeGameSessionDetails $
--             describeGameSessionDetailsResponse
--
--         , responseDescribeFleetPortSettings $
--             describeFleetPortSettingsResponse
--
--         , responseDescribeGameSessionQueues $
--             describeGameSessionQueuesResponse
--
--         , responseDescribeVPCPeeringConnections $
--             describeVPCPeeringConnectionsResponse
--
--         , responseDescribeScript $
--             describeScriptResponse
--
--         , responseCreatePlayerSessions $
--             createPlayerSessionsResponse
--
--         , responseDescribeMatchmakingConfigurations $
--             describeMatchmakingConfigurationsResponse
--
--         , responseDescribeVPCPeeringAuthorizations $
--             describeVPCPeeringAuthorizationsResponse
--
--         , responseUpdateGameServer $
--             updateGameServerResponse
--
--         , responseCreateFleet $
--             createFleetResponse
--
--         , responseDeleteMatchmakingConfiguration $
--             deleteMatchmakingConfigurationResponse
--
--         , responseUpdateMatchmakingConfiguration $
--             updateMatchmakingConfigurationResponse
--
--         , responseDeleteGameServerGroup $
--             deleteGameServerGroupResponse
--
--         , responseUpdateGameServerGroup $
--             updateGameServerGroupResponse
--
--         , responseResumeGameServerGroup $
--             resumeGameServerGroupResponse
--
--         , responseDeleteVPCPeeringAuthorization $
--             deleteVPCPeeringAuthorizationResponse
--
--         , responseUpdateFleetAttributes $
--             updateFleetAttributesResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseCreateMatchmakingConfiguration $
--             createMatchmakingConfigurationResponse
--
--         , responseDescribePlayerSessions $
--             describePlayerSessionsResponse
--
--         , responseStopFleetActions $
--             stopFleetActionsResponse
--
--         , responseDescribeBuild $
--             describeBuildResponse
--
--         , responseUpdateFleetPortSettings $
--             updateFleetPortSettingsResponse
--
--         , responseUpdateFleetCapacity $
--             updateFleetCapacityResponse
--
--         , responseCreateScript $
--             createScriptResponse
--
--         , responseAcceptMatch $
--             acceptMatchResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseDescribeAlias $
--             describeAliasResponse
--
--         , responseValidateMatchmakingRuleSet $
--             validateMatchmakingRuleSetResponse
--
--         , responseListScripts $
--             listScriptsResponse
--
--         , responseDescribeEC2InstanceLimits $
--             describeEC2InstanceLimitsResponse
--
--         , responseSuspendGameServerGroup $
--             suspendGameServerGroupResponse
--
--         , responseDeleteMatchmakingRuleSet $
--             deleteMatchmakingRuleSetResponse
--
--         , responseStopGameSessionPlacement $
--             stopGameSessionPlacementResponse
--
--         , responseClaimGameServer $
--             claimGameServerResponse
--
--         , responseUpdateGameSession $
--             updateGameSessionResponse
--
--         , responseDescribeMatchmaking $
--             describeMatchmakingResponse
--
--         , responseCreatePlayerSession $
--             createPlayerSessionResponse
--
--         , responseDescribeGameServerGroup $
--             describeGameServerGroupResponse
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

requestCreateVPCPeeringConnection :: CreateVPCPeeringConnection -> TestTree
requestCreateVPCPeeringConnection =
  req
    "CreateVPCPeeringConnection"
    "fixture/CreateVPCPeeringConnection.yaml"

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

requestCreateVPCPeeringAuthorization :: CreateVPCPeeringAuthorization -> TestTree
requestCreateVPCPeeringAuthorization =
  req
    "CreateVPCPeeringAuthorization"
    "fixture/CreateVPCPeeringAuthorization.yaml"

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

requestDeleteVPCPeeringConnection :: DeleteVPCPeeringConnection -> TestTree
requestDeleteVPCPeeringConnection =
  req
    "DeleteVPCPeeringConnection"
    "fixture/DeleteVPCPeeringConnection.yaml"

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

requestGetGameSessionLogURL :: GetGameSessionLogURL -> TestTree
requestGetGameSessionLogURL =
  req
    "GetGameSessionLogURL"
    "fixture/GetGameSessionLogURL.yaml"

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

requestDescribeVPCPeeringConnections :: DescribeVPCPeeringConnections -> TestTree
requestDescribeVPCPeeringConnections =
  req
    "DescribeVPCPeeringConnections"
    "fixture/DescribeVPCPeeringConnections.yaml"

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

requestDescribeVPCPeeringAuthorizations :: DescribeVPCPeeringAuthorizations -> TestTree
requestDescribeVPCPeeringAuthorizations =
  req
    "DescribeVPCPeeringAuthorizations"
    "fixture/DescribeVPCPeeringAuthorizations.yaml"

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

requestDeleteVPCPeeringAuthorization :: DeleteVPCPeeringAuthorization -> TestTree
requestDeleteVPCPeeringAuthorization =
  req
    "DeleteVPCPeeringAuthorization"
    "fixture/DeleteVPCPeeringAuthorization.yaml"

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
    gameLift
    (Proxy :: Proxy StopMatchmaking)

responseDescribeGameServerInstances :: DescribeGameServerInstancesResponse -> TestTree
responseDescribeGameServerInstances =
  res
    "DescribeGameServerInstancesResponse"
    "fixture/DescribeGameServerInstancesResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameServerInstances)

responseCreateGameSession :: CreateGameSessionResponse -> TestTree
responseCreateGameSession =
  res
    "CreateGameSessionResponse"
    "fixture/CreateGameSessionResponse.proto"
    gameLift
    (Proxy :: Proxy CreateGameSession)

responseDeleteScalingPolicy :: DeleteScalingPolicyResponse -> TestTree
responseDeleteScalingPolicy =
  res
    "DeleteScalingPolicyResponse"
    "fixture/DeleteScalingPolicyResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteScalingPolicy)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy =
  res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    gameLift
    (Proxy :: Proxy PutScalingPolicy)

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds =
  res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    gameLift
    (Proxy :: Proxy ListBuilds)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet =
  res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteFleet)

responseCreateBuild :: CreateBuildResponse -> TestTree
responseCreateBuild =
  res
    "CreateBuildResponse"
    "fixture/CreateBuildResponse.proto"
    gameLift
    (Proxy :: Proxy CreateBuild)

responseRequestUploadCredentials :: RequestUploadCredentialsResponse -> TestTree
responseRequestUploadCredentials =
  res
    "RequestUploadCredentialsResponse"
    "fixture/RequestUploadCredentialsResponse.proto"
    gameLift
    (Proxy :: Proxy RequestUploadCredentials)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    gameLift
    (Proxy :: Proxy CreateAlias)

responseListGameServers :: ListGameServersResponse -> TestTree
responseListGameServers =
  res
    "ListGameServersResponse"
    "fixture/ListGameServersResponse.proto"
    gameLift
    (Proxy :: Proxy ListGameServers)

responseResolveAlias :: ResolveAliasResponse -> TestTree
responseResolveAlias =
  res
    "ResolveAliasResponse"
    "fixture/ResolveAliasResponse.proto"
    gameLift
    (Proxy :: Proxy ResolveAlias)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    gameLift
    (Proxy :: Proxy ListTagsForResource)

responseRegisterGameServer :: RegisterGameServerResponse -> TestTree
responseRegisterGameServer =
  res
    "RegisterGameServerResponse"
    "fixture/RegisterGameServerResponse.proto"
    gameLift
    (Proxy :: Proxy RegisterGameServer)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    gameLift
    (Proxy :: Proxy ListAliases)

responseUpdateRuntimeConfiguration :: UpdateRuntimeConfigurationResponse -> TestTree
responseUpdateRuntimeConfiguration =
  res
    "UpdateRuntimeConfigurationResponse"
    "fixture/UpdateRuntimeConfigurationResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateRuntimeConfiguration)

responseCreateVPCPeeringConnection :: CreateVPCPeeringConnectionResponse -> TestTree
responseCreateVPCPeeringConnection =
  res
    "CreateVPCPeeringConnectionResponse"
    "fixture/CreateVPCPeeringConnectionResponse.proto"
    gameLift
    (Proxy :: Proxy CreateVPCPeeringConnection)

responseListGameServerGroups :: ListGameServerGroupsResponse -> TestTree
responseListGameServerGroups =
  res
    "ListGameServerGroupsResponse"
    "fixture/ListGameServerGroupsResponse.proto"
    gameLift
    (Proxy :: Proxy ListGameServerGroups)

responseCreateGameSessionQueue :: CreateGameSessionQueueResponse -> TestTree
responseCreateGameSessionQueue =
  res
    "CreateGameSessionQueueResponse"
    "fixture/CreateGameSessionQueueResponse.proto"
    gameLift
    (Proxy :: Proxy CreateGameSessionQueue)

responseSearchGameSessions :: SearchGameSessionsResponse -> TestTree
responseSearchGameSessions =
  res
    "SearchGameSessionsResponse"
    "fixture/SearchGameSessionsResponse.proto"
    gameLift
    (Proxy :: Proxy SearchGameSessions)

responseCreateVPCPeeringAuthorization :: CreateVPCPeeringAuthorizationResponse -> TestTree
responseCreateVPCPeeringAuthorization =
  res
    "CreateVPCPeeringAuthorizationResponse"
    "fixture/CreateVPCPeeringAuthorizationResponse.proto"
    gameLift
    (Proxy :: Proxy CreateVPCPeeringAuthorization)

responseUpdateGameSessionQueue :: UpdateGameSessionQueueResponse -> TestTree
responseUpdateGameSessionQueue =
  res
    "UpdateGameSessionQueueResponse"
    "fixture/UpdateGameSessionQueueResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateGameSessionQueue)

responseDeleteGameSessionQueue :: DeleteGameSessionQueueResponse -> TestTree
responseDeleteGameSessionQueue =
  res
    "DeleteGameSessionQueueResponse"
    "fixture/DeleteGameSessionQueueResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteGameSessionQueue)

responseCreateGameServerGroup :: CreateGameServerGroupResponse -> TestTree
responseCreateGameServerGroup =
  res
    "CreateGameServerGroupResponse"
    "fixture/CreateGameServerGroupResponse.proto"
    gameLift
    (Proxy :: Proxy CreateGameServerGroup)

responseDeleteVPCPeeringConnection :: DeleteVPCPeeringConnectionResponse -> TestTree
responseDeleteVPCPeeringConnection =
  res
    "DeleteVPCPeeringConnectionResponse"
    "fixture/DeleteVPCPeeringConnectionResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteVPCPeeringConnection)

responseStartFleetActions :: StartFleetActionsResponse -> TestTree
responseStartFleetActions =
  res
    "StartFleetActionsResponse"
    "fixture/StartFleetActionsResponse.proto"
    gameLift
    (Proxy :: Proxy StartFleetActions)

responseDeregisterGameServer :: DeregisterGameServerResponse -> TestTree
responseDeregisterGameServer =
  res
    "DeregisterGameServerResponse"
    "fixture/DeregisterGameServerResponse.proto"
    gameLift
    (Proxy :: Proxy DeregisterGameServer)

responseGetInstanceAccess :: GetInstanceAccessResponse -> TestTree
responseGetInstanceAccess =
  res
    "GetInstanceAccessResponse"
    "fixture/GetInstanceAccessResponse.proto"
    gameLift
    (Proxy :: Proxy GetInstanceAccess)

responseDescribeScalingPolicies :: DescribeScalingPoliciesResponse -> TestTree
responseDescribeScalingPolicies =
  res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeScalingPolicies)

responseDescribeMatchmakingRuleSets :: DescribeMatchmakingRuleSetsResponse -> TestTree
responseDescribeMatchmakingRuleSets =
  res
    "DescribeMatchmakingRuleSetsResponse"
    "fixture/DescribeMatchmakingRuleSetsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeMatchmakingRuleSets)

responseDescribeGameSessions :: DescribeGameSessionsResponse -> TestTree
responseDescribeGameSessions =
  res
    "DescribeGameSessionsResponse"
    "fixture/DescribeGameSessionsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameSessions)

responseDescribeGameServer :: DescribeGameServerResponse -> TestTree
responseDescribeGameServer =
  res
    "DescribeGameServerResponse"
    "fixture/DescribeGameServerResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameServer)

responseUpdateScript :: UpdateScriptResponse -> TestTree
responseUpdateScript =
  res
    "UpdateScriptResponse"
    "fixture/UpdateScriptResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateScript)

responseDeleteScript :: DeleteScriptResponse -> TestTree
responseDeleteScript =
  res
    "DeleteScriptResponse"
    "fixture/DeleteScriptResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteScript)

responseStartGameSessionPlacement :: StartGameSessionPlacementResponse -> TestTree
responseStartGameSessionPlacement =
  res
    "StartGameSessionPlacementResponse"
    "fixture/StartGameSessionPlacementResponse.proto"
    gameLift
    (Proxy :: Proxy StartGameSessionPlacement)

responseDescribeFleetUtilization :: DescribeFleetUtilizationResponse -> TestTree
responseDescribeFleetUtilization =
  res
    "DescribeFleetUtilizationResponse"
    "fixture/DescribeFleetUtilizationResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetUtilization)

responseDescribeRuntimeConfiguration :: DescribeRuntimeConfigurationResponse -> TestTree
responseDescribeRuntimeConfiguration =
  res
    "DescribeRuntimeConfigurationResponse"
    "fixture/DescribeRuntimeConfigurationResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeRuntimeConfiguration)

responseGetGameSessionLogURL :: GetGameSessionLogURLResponse -> TestTree
responseGetGameSessionLogURL =
  res
    "GetGameSessionLogURLResponse"
    "fixture/GetGameSessionLogURLResponse.proto"
    gameLift
    (Proxy :: Proxy GetGameSessionLogURL)

responseDescribeFleetAttributes :: DescribeFleetAttributesResponse -> TestTree
responseDescribeFleetAttributes =
  res
    "DescribeFleetAttributesResponse"
    "fixture/DescribeFleetAttributesResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetAttributes)

responseDescribeGameSessionPlacement :: DescribeGameSessionPlacementResponse -> TestTree
responseDescribeGameSessionPlacement =
  res
    "DescribeGameSessionPlacementResponse"
    "fixture/DescribeGameSessionPlacementResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameSessionPlacement)

responseDescribeFleetEvents :: DescribeFleetEventsResponse -> TestTree
responseDescribeFleetEvents =
  res
    "DescribeFleetEventsResponse"
    "fixture/DescribeFleetEventsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetEvents)

responseStartMatchmaking :: StartMatchmakingResponse -> TestTree
responseStartMatchmaking =
  res
    "StartMatchmakingResponse"
    "fixture/StartMatchmakingResponse.proto"
    gameLift
    (Proxy :: Proxy StartMatchmaking)

responseCreateMatchmakingRuleSet :: CreateMatchmakingRuleSetResponse -> TestTree
responseCreateMatchmakingRuleSet =
  res
    "CreateMatchmakingRuleSetResponse"
    "fixture/CreateMatchmakingRuleSetResponse.proto"
    gameLift
    (Proxy :: Proxy CreateMatchmakingRuleSet)

responseDescribeFleetCapacity :: DescribeFleetCapacityResponse -> TestTree
responseDescribeFleetCapacity =
  res
    "DescribeFleetCapacityResponse"
    "fixture/DescribeFleetCapacityResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetCapacity)

responseDeleteBuild :: DeleteBuildResponse -> TestTree
responseDeleteBuild =
  res
    "DeleteBuildResponse"
    "fixture/DeleteBuildResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteBuild)

responseUpdateBuild :: UpdateBuildResponse -> TestTree
responseUpdateBuild =
  res
    "UpdateBuildResponse"
    "fixture/UpdateBuildResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateBuild)

responseListFleets :: ListFleetsResponse -> TestTree
responseListFleets =
  res
    "ListFleetsResponse"
    "fixture/ListFleetsResponse.proto"
    gameLift
    (Proxy :: Proxy ListFleets)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteAlias)

responseUpdateAlias :: UpdateAliasResponse -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateAlias)

responseStartMatchBackfill :: StartMatchBackfillResponse -> TestTree
responseStartMatchBackfill =
  res
    "StartMatchBackfillResponse"
    "fixture/StartMatchBackfillResponse.proto"
    gameLift
    (Proxy :: Proxy StartMatchBackfill)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeInstances)

responseDescribeGameSessionDetails :: DescribeGameSessionDetailsResponse -> TestTree
responseDescribeGameSessionDetails =
  res
    "DescribeGameSessionDetailsResponse"
    "fixture/DescribeGameSessionDetailsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameSessionDetails)

responseDescribeFleetPortSettings :: DescribeFleetPortSettingsResponse -> TestTree
responseDescribeFleetPortSettings =
  res
    "DescribeFleetPortSettingsResponse"
    "fixture/DescribeFleetPortSettingsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetPortSettings)

responseDescribeGameSessionQueues :: DescribeGameSessionQueuesResponse -> TestTree
responseDescribeGameSessionQueues =
  res
    "DescribeGameSessionQueuesResponse"
    "fixture/DescribeGameSessionQueuesResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameSessionQueues)

responseDescribeVPCPeeringConnections :: DescribeVPCPeeringConnectionsResponse -> TestTree
responseDescribeVPCPeeringConnections =
  res
    "DescribeVPCPeeringConnectionsResponse"
    "fixture/DescribeVPCPeeringConnectionsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeVPCPeeringConnections)

responseDescribeScript :: DescribeScriptResponse -> TestTree
responseDescribeScript =
  res
    "DescribeScriptResponse"
    "fixture/DescribeScriptResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeScript)

responseCreatePlayerSessions :: CreatePlayerSessionsResponse -> TestTree
responseCreatePlayerSessions =
  res
    "CreatePlayerSessionsResponse"
    "fixture/CreatePlayerSessionsResponse.proto"
    gameLift
    (Proxy :: Proxy CreatePlayerSessions)

responseDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurationsResponse -> TestTree
responseDescribeMatchmakingConfigurations =
  res
    "DescribeMatchmakingConfigurationsResponse"
    "fixture/DescribeMatchmakingConfigurationsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeMatchmakingConfigurations)

responseDescribeVPCPeeringAuthorizations :: DescribeVPCPeeringAuthorizationsResponse -> TestTree
responseDescribeVPCPeeringAuthorizations =
  res
    "DescribeVPCPeeringAuthorizationsResponse"
    "fixture/DescribeVPCPeeringAuthorizationsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeVPCPeeringAuthorizations)

responseUpdateGameServer :: UpdateGameServerResponse -> TestTree
responseUpdateGameServer =
  res
    "UpdateGameServerResponse"
    "fixture/UpdateGameServerResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateGameServer)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    gameLift
    (Proxy :: Proxy CreateFleet)

responseDeleteMatchmakingConfiguration :: DeleteMatchmakingConfigurationResponse -> TestTree
responseDeleteMatchmakingConfiguration =
  res
    "DeleteMatchmakingConfigurationResponse"
    "fixture/DeleteMatchmakingConfigurationResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteMatchmakingConfiguration)

responseUpdateMatchmakingConfiguration :: UpdateMatchmakingConfigurationResponse -> TestTree
responseUpdateMatchmakingConfiguration =
  res
    "UpdateMatchmakingConfigurationResponse"
    "fixture/UpdateMatchmakingConfigurationResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateMatchmakingConfiguration)

responseDeleteGameServerGroup :: DeleteGameServerGroupResponse -> TestTree
responseDeleteGameServerGroup =
  res
    "DeleteGameServerGroupResponse"
    "fixture/DeleteGameServerGroupResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteGameServerGroup)

responseUpdateGameServerGroup :: UpdateGameServerGroupResponse -> TestTree
responseUpdateGameServerGroup =
  res
    "UpdateGameServerGroupResponse"
    "fixture/UpdateGameServerGroupResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateGameServerGroup)

responseResumeGameServerGroup :: ResumeGameServerGroupResponse -> TestTree
responseResumeGameServerGroup =
  res
    "ResumeGameServerGroupResponse"
    "fixture/ResumeGameServerGroupResponse.proto"
    gameLift
    (Proxy :: Proxy ResumeGameServerGroup)

responseDeleteVPCPeeringAuthorization :: DeleteVPCPeeringAuthorizationResponse -> TestTree
responseDeleteVPCPeeringAuthorization =
  res
    "DeleteVPCPeeringAuthorizationResponse"
    "fixture/DeleteVPCPeeringAuthorizationResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteVPCPeeringAuthorization)

responseUpdateFleetAttributes :: UpdateFleetAttributesResponse -> TestTree
responseUpdateFleetAttributes =
  res
    "UpdateFleetAttributesResponse"
    "fixture/UpdateFleetAttributesResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateFleetAttributes)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    gameLift
    (Proxy :: Proxy TagResource)

responseCreateMatchmakingConfiguration :: CreateMatchmakingConfigurationResponse -> TestTree
responseCreateMatchmakingConfiguration =
  res
    "CreateMatchmakingConfigurationResponse"
    "fixture/CreateMatchmakingConfigurationResponse.proto"
    gameLift
    (Proxy :: Proxy CreateMatchmakingConfiguration)

responseDescribePlayerSessions :: DescribePlayerSessionsResponse -> TestTree
responseDescribePlayerSessions =
  res
    "DescribePlayerSessionsResponse"
    "fixture/DescribePlayerSessionsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribePlayerSessions)

responseStopFleetActions :: StopFleetActionsResponse -> TestTree
responseStopFleetActions =
  res
    "StopFleetActionsResponse"
    "fixture/StopFleetActionsResponse.proto"
    gameLift
    (Proxy :: Proxy StopFleetActions)

responseDescribeBuild :: DescribeBuildResponse -> TestTree
responseDescribeBuild =
  res
    "DescribeBuildResponse"
    "fixture/DescribeBuildResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeBuild)

responseUpdateFleetPortSettings :: UpdateFleetPortSettingsResponse -> TestTree
responseUpdateFleetPortSettings =
  res
    "UpdateFleetPortSettingsResponse"
    "fixture/UpdateFleetPortSettingsResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateFleetPortSettings)

responseUpdateFleetCapacity :: UpdateFleetCapacityResponse -> TestTree
responseUpdateFleetCapacity =
  res
    "UpdateFleetCapacityResponse"
    "fixture/UpdateFleetCapacityResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateFleetCapacity)

responseCreateScript :: CreateScriptResponse -> TestTree
responseCreateScript =
  res
    "CreateScriptResponse"
    "fixture/CreateScriptResponse.proto"
    gameLift
    (Proxy :: Proxy CreateScript)

responseAcceptMatch :: AcceptMatchResponse -> TestTree
responseAcceptMatch =
  res
    "AcceptMatchResponse"
    "fixture/AcceptMatchResponse.proto"
    gameLift
    (Proxy :: Proxy AcceptMatch)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    gameLift
    (Proxy :: Proxy UntagResource)

responseDescribeAlias :: DescribeAliasResponse -> TestTree
responseDescribeAlias =
  res
    "DescribeAliasResponse"
    "fixture/DescribeAliasResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeAlias)

responseValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSetResponse -> TestTree
responseValidateMatchmakingRuleSet =
  res
    "ValidateMatchmakingRuleSetResponse"
    "fixture/ValidateMatchmakingRuleSetResponse.proto"
    gameLift
    (Proxy :: Proxy ValidateMatchmakingRuleSet)

responseListScripts :: ListScriptsResponse -> TestTree
responseListScripts =
  res
    "ListScriptsResponse"
    "fixture/ListScriptsResponse.proto"
    gameLift
    (Proxy :: Proxy ListScripts)

responseDescribeEC2InstanceLimits :: DescribeEC2InstanceLimitsResponse -> TestTree
responseDescribeEC2InstanceLimits =
  res
    "DescribeEC2InstanceLimitsResponse"
    "fixture/DescribeEC2InstanceLimitsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeEC2InstanceLimits)

responseSuspendGameServerGroup :: SuspendGameServerGroupResponse -> TestTree
responseSuspendGameServerGroup =
  res
    "SuspendGameServerGroupResponse"
    "fixture/SuspendGameServerGroupResponse.proto"
    gameLift
    (Proxy :: Proxy SuspendGameServerGroup)

responseDeleteMatchmakingRuleSet :: DeleteMatchmakingRuleSetResponse -> TestTree
responseDeleteMatchmakingRuleSet =
  res
    "DeleteMatchmakingRuleSetResponse"
    "fixture/DeleteMatchmakingRuleSetResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteMatchmakingRuleSet)

responseStopGameSessionPlacement :: StopGameSessionPlacementResponse -> TestTree
responseStopGameSessionPlacement =
  res
    "StopGameSessionPlacementResponse"
    "fixture/StopGameSessionPlacementResponse.proto"
    gameLift
    (Proxy :: Proxy StopGameSessionPlacement)

responseClaimGameServer :: ClaimGameServerResponse -> TestTree
responseClaimGameServer =
  res
    "ClaimGameServerResponse"
    "fixture/ClaimGameServerResponse.proto"
    gameLift
    (Proxy :: Proxy ClaimGameServer)

responseUpdateGameSession :: UpdateGameSessionResponse -> TestTree
responseUpdateGameSession =
  res
    "UpdateGameSessionResponse"
    "fixture/UpdateGameSessionResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateGameSession)

responseDescribeMatchmaking :: DescribeMatchmakingResponse -> TestTree
responseDescribeMatchmaking =
  res
    "DescribeMatchmakingResponse"
    "fixture/DescribeMatchmakingResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeMatchmaking)

responseCreatePlayerSession :: CreatePlayerSessionResponse -> TestTree
responseCreatePlayerSession =
  res
    "CreatePlayerSessionResponse"
    "fixture/CreatePlayerSessionResponse.proto"
    gameLift
    (Proxy :: Proxy CreatePlayerSession)

responseDescribeGameServerGroup :: DescribeGameServerGroupResponse -> TestTree
responseDescribeGameServerGroup =
  res
    "DescribeGameServerGroupResponse"
    "fixture/DescribeGameServerGroupResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameServerGroup)
