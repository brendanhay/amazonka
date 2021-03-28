{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.GameLift
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.GameLift where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.GameLift
import Test.AWS.GameLift.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStopMatchmaking $
--             mkStopMatchmaking
--
--         , requestDescribeGameServerInstances $
--             mkDescribeGameServerInstances
--
--         , requestCreateGameSession $
--             mkCreateGameSession
--
--         , requestDeleteScalingPolicy $
--             mkDeleteScalingPolicy
--
--         , requestPutScalingPolicy $
--             mkPutScalingPolicy
--
--         , requestListBuilds $
--             mkListBuilds
--
--         , requestDeleteFleet $
--             mkDeleteFleet
--
--         , requestCreateBuild $
--             mkCreateBuild
--
--         , requestRequestUploadCredentials $
--             mkRequestUploadCredentials
--
--         , requestCreateAlias $
--             mkCreateAlias
--
--         , requestListGameServers $
--             mkListGameServers
--
--         , requestResolveAlias $
--             mkResolveAlias
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestRegisterGameServer $
--             mkRegisterGameServer
--
--         , requestListAliases $
--             mkListAliases
--
--         , requestUpdateRuntimeConfiguration $
--             mkUpdateRuntimeConfiguration
--
--         , requestCreateVpcPeeringConnection $
--             mkCreateVpcPeeringConnection
--
--         , requestListGameServerGroups $
--             mkListGameServerGroups
--
--         , requestCreateGameSessionQueue $
--             mkCreateGameSessionQueue
--
--         , requestSearchGameSessions $
--             mkSearchGameSessions
--
--         , requestCreateVpcPeeringAuthorization $
--             mkCreateVpcPeeringAuthorization
--
--         , requestUpdateGameSessionQueue $
--             mkUpdateGameSessionQueue
--
--         , requestDeleteGameSessionQueue $
--             mkDeleteGameSessionQueue
--
--         , requestCreateGameServerGroup $
--             mkCreateGameServerGroup
--
--         , requestDeleteVpcPeeringConnection $
--             mkDeleteVpcPeeringConnection
--
--         , requestStartFleetActions $
--             mkStartFleetActions
--
--         , requestDeregisterGameServer $
--             mkDeregisterGameServer
--
--         , requestGetInstanceAccess $
--             mkGetInstanceAccess
--
--         , requestDescribeScalingPolicies $
--             mkDescribeScalingPolicies
--
--         , requestDescribeMatchmakingRuleSets $
--             mkDescribeMatchmakingRuleSets
--
--         , requestDescribeGameSessions $
--             mkDescribeGameSessions
--
--         , requestDescribeGameServer $
--             mkDescribeGameServer
--
--         , requestUpdateScript $
--             mkUpdateScript
--
--         , requestDeleteScript $
--             mkDeleteScript
--
--         , requestStartGameSessionPlacement $
--             mkStartGameSessionPlacement
--
--         , requestDescribeFleetUtilization $
--             mkDescribeFleetUtilization
--
--         , requestDescribeRuntimeConfiguration $
--             mkDescribeRuntimeConfiguration
--
--         , requestGetGameSessionLogUrl $
--             mkGetGameSessionLogUrl
--
--         , requestDescribeFleetAttributes $
--             mkDescribeFleetAttributes
--
--         , requestDescribeGameSessionPlacement $
--             mkDescribeGameSessionPlacement
--
--         , requestDescribeFleetEvents $
--             mkDescribeFleetEvents
--
--         , requestStartMatchmaking $
--             mkStartMatchmaking
--
--         , requestCreateMatchmakingRuleSet $
--             mkCreateMatchmakingRuleSet
--
--         , requestDescribeFleetCapacity $
--             mkDescribeFleetCapacity
--
--         , requestDeleteBuild $
--             mkDeleteBuild
--
--         , requestUpdateBuild $
--             mkUpdateBuild
--
--         , requestListFleets $
--             mkListFleets
--
--         , requestDeleteAlias $
--             mkDeleteAlias
--
--         , requestUpdateAlias $
--             mkUpdateAlias
--
--         , requestStartMatchBackfill $
--             mkStartMatchBackfill
--
--         , requestDescribeInstances $
--             mkDescribeInstances
--
--         , requestDescribeGameSessionDetails $
--             mkDescribeGameSessionDetails
--
--         , requestDescribeFleetPortSettings $
--             mkDescribeFleetPortSettings
--
--         , requestDescribeGameSessionQueues $
--             mkDescribeGameSessionQueues
--
--         , requestDescribeVpcPeeringConnections $
--             mkDescribeVpcPeeringConnections
--
--         , requestDescribeScript $
--             mkDescribeScript
--
--         , requestCreatePlayerSessions $
--             mkCreatePlayerSessions
--
--         , requestDescribeMatchmakingConfigurations $
--             mkDescribeMatchmakingConfigurations
--
--         , requestDescribeVpcPeeringAuthorizations $
--             mkDescribeVpcPeeringAuthorizations
--
--         , requestUpdateGameServer $
--             mkUpdateGameServer
--
--         , requestCreateFleet $
--             mkCreateFleet
--
--         , requestDeleteMatchmakingConfiguration $
--             mkDeleteMatchmakingConfiguration
--
--         , requestUpdateMatchmakingConfiguration $
--             mkUpdateMatchmakingConfiguration
--
--         , requestDeleteGameServerGroup $
--             mkDeleteGameServerGroup
--
--         , requestUpdateGameServerGroup $
--             mkUpdateGameServerGroup
--
--         , requestResumeGameServerGroup $
--             mkResumeGameServerGroup
--
--         , requestDeleteVpcPeeringAuthorization $
--             mkDeleteVpcPeeringAuthorization
--
--         , requestUpdateFleetAttributes $
--             mkUpdateFleetAttributes
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestCreateMatchmakingConfiguration $
--             mkCreateMatchmakingConfiguration
--
--         , requestDescribePlayerSessions $
--             mkDescribePlayerSessions
--
--         , requestStopFleetActions $
--             mkStopFleetActions
--
--         , requestDescribeBuild $
--             mkDescribeBuild
--
--         , requestUpdateFleetPortSettings $
--             mkUpdateFleetPortSettings
--
--         , requestUpdateFleetCapacity $
--             mkUpdateFleetCapacity
--
--         , requestCreateScript $
--             mkCreateScript
--
--         , requestAcceptMatch $
--             mkAcceptMatch
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDescribeAlias $
--             mkDescribeAlias
--
--         , requestValidateMatchmakingRuleSet $
--             mkValidateMatchmakingRuleSet
--
--         , requestListScripts $
--             mkListScripts
--
--         , requestDescribeEC2InstanceLimits $
--             mkDescribeEC2InstanceLimits
--
--         , requestSuspendGameServerGroup $
--             mkSuspendGameServerGroup
--
--         , requestDeleteMatchmakingRuleSet $
--             mkDeleteMatchmakingRuleSet
--
--         , requestStopGameSessionPlacement $
--             mkStopGameSessionPlacement
--
--         , requestClaimGameServer $
--             mkClaimGameServer
--
--         , requestUpdateGameSession $
--             mkUpdateGameSession
--
--         , requestDescribeMatchmaking $
--             mkDescribeMatchmaking
--
--         , requestCreatePlayerSession $
--             mkCreatePlayerSession
--
--         , requestDescribeGameServerGroup $
--             mkDescribeGameServerGroup
--
--           ]

--     , testGroup "response"
--         [ responseStopMatchmaking $
--             mkStopMatchmakingResponse
--
--         , responseDescribeGameServerInstances $
--             mkDescribeGameServerInstancesResponse
--
--         , responseCreateGameSession $
--             mkCreateGameSessionResponse
--
--         , responseDeleteScalingPolicy $
--             mkDeleteScalingPolicyResponse
--
--         , responsePutScalingPolicy $
--             mkPutScalingPolicyResponse
--
--         , responseListBuilds $
--             mkListBuildsResponse
--
--         , responseDeleteFleet $
--             mkDeleteFleetResponse
--
--         , responseCreateBuild $
--             mkCreateBuildResponse
--
--         , responseRequestUploadCredentials $
--             mkRequestUploadCredentialsResponse
--
--         , responseCreateAlias $
--             mkCreateAliasResponse
--
--         , responseListGameServers $
--             mkListGameServersResponse
--
--         , responseResolveAlias $
--             mkResolveAliasResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseRegisterGameServer $
--             mkRegisterGameServerResponse
--
--         , responseListAliases $
--             mkListAliasesResponse
--
--         , responseUpdateRuntimeConfiguration $
--             mkUpdateRuntimeConfigurationResponse
--
--         , responseCreateVpcPeeringConnection $
--             mkCreateVpcPeeringConnectionResponse
--
--         , responseListGameServerGroups $
--             mkListGameServerGroupsResponse
--
--         , responseCreateGameSessionQueue $
--             mkCreateGameSessionQueueResponse
--
--         , responseSearchGameSessions $
--             mkSearchGameSessionsResponse
--
--         , responseCreateVpcPeeringAuthorization $
--             mkCreateVpcPeeringAuthorizationResponse
--
--         , responseUpdateGameSessionQueue $
--             mkUpdateGameSessionQueueResponse
--
--         , responseDeleteGameSessionQueue $
--             mkDeleteGameSessionQueueResponse
--
--         , responseCreateGameServerGroup $
--             mkCreateGameServerGroupResponse
--
--         , responseDeleteVpcPeeringConnection $
--             mkDeleteVpcPeeringConnectionResponse
--
--         , responseStartFleetActions $
--             mkStartFleetActionsResponse
--
--         , responseDeregisterGameServer $
--             mkDeregisterGameServerResponse
--
--         , responseGetInstanceAccess $
--             mkGetInstanceAccessResponse
--
--         , responseDescribeScalingPolicies $
--             mkDescribeScalingPoliciesResponse
--
--         , responseDescribeMatchmakingRuleSets $
--             mkDescribeMatchmakingRuleSetsResponse
--
--         , responseDescribeGameSessions $
--             mkDescribeGameSessionsResponse
--
--         , responseDescribeGameServer $
--             mkDescribeGameServerResponse
--
--         , responseUpdateScript $
--             mkUpdateScriptResponse
--
--         , responseDeleteScript $
--             mkDeleteScriptResponse
--
--         , responseStartGameSessionPlacement $
--             mkStartGameSessionPlacementResponse
--
--         , responseDescribeFleetUtilization $
--             mkDescribeFleetUtilizationResponse
--
--         , responseDescribeRuntimeConfiguration $
--             mkDescribeRuntimeConfigurationResponse
--
--         , responseGetGameSessionLogUrl $
--             mkGetGameSessionLogUrlResponse
--
--         , responseDescribeFleetAttributes $
--             mkDescribeFleetAttributesResponse
--
--         , responseDescribeGameSessionPlacement $
--             mkDescribeGameSessionPlacementResponse
--
--         , responseDescribeFleetEvents $
--             mkDescribeFleetEventsResponse
--
--         , responseStartMatchmaking $
--             mkStartMatchmakingResponse
--
--         , responseCreateMatchmakingRuleSet $
--             mkCreateMatchmakingRuleSetResponse
--
--         , responseDescribeFleetCapacity $
--             mkDescribeFleetCapacityResponse
--
--         , responseDeleteBuild $
--             mkDeleteBuildResponse
--
--         , responseUpdateBuild $
--             mkUpdateBuildResponse
--
--         , responseListFleets $
--             mkListFleetsResponse
--
--         , responseDeleteAlias $
--             mkDeleteAliasResponse
--
--         , responseUpdateAlias $
--             mkUpdateAliasResponse
--
--         , responseStartMatchBackfill $
--             mkStartMatchBackfillResponse
--
--         , responseDescribeInstances $
--             mkDescribeInstancesResponse
--
--         , responseDescribeGameSessionDetails $
--             mkDescribeGameSessionDetailsResponse
--
--         , responseDescribeFleetPortSettings $
--             mkDescribeFleetPortSettingsResponse
--
--         , responseDescribeGameSessionQueues $
--             mkDescribeGameSessionQueuesResponse
--
--         , responseDescribeVpcPeeringConnections $
--             mkDescribeVpcPeeringConnectionsResponse
--
--         , responseDescribeScript $
--             mkDescribeScriptResponse
--
--         , responseCreatePlayerSessions $
--             mkCreatePlayerSessionsResponse
--
--         , responseDescribeMatchmakingConfigurations $
--             mkDescribeMatchmakingConfigurationsResponse
--
--         , responseDescribeVpcPeeringAuthorizations $
--             mkDescribeVpcPeeringAuthorizationsResponse
--
--         , responseUpdateGameServer $
--             mkUpdateGameServerResponse
--
--         , responseCreateFleet $
--             mkCreateFleetResponse
--
--         , responseDeleteMatchmakingConfiguration $
--             mkDeleteMatchmakingConfigurationResponse
--
--         , responseUpdateMatchmakingConfiguration $
--             mkUpdateMatchmakingConfigurationResponse
--
--         , responseDeleteGameServerGroup $
--             mkDeleteGameServerGroupResponse
--
--         , responseUpdateGameServerGroup $
--             mkUpdateGameServerGroupResponse
--
--         , responseResumeGameServerGroup $
--             mkResumeGameServerGroupResponse
--
--         , responseDeleteVpcPeeringAuthorization $
--             mkDeleteVpcPeeringAuthorizationResponse
--
--         , responseUpdateFleetAttributes $
--             mkUpdateFleetAttributesResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseCreateMatchmakingConfiguration $
--             mkCreateMatchmakingConfigurationResponse
--
--         , responseDescribePlayerSessions $
--             mkDescribePlayerSessionsResponse
--
--         , responseStopFleetActions $
--             mkStopFleetActionsResponse
--
--         , responseDescribeBuild $
--             mkDescribeBuildResponse
--
--         , responseUpdateFleetPortSettings $
--             mkUpdateFleetPortSettingsResponse
--
--         , responseUpdateFleetCapacity $
--             mkUpdateFleetCapacityResponse
--
--         , responseCreateScript $
--             mkCreateScriptResponse
--
--         , responseAcceptMatch $
--             mkAcceptMatchResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDescribeAlias $
--             mkDescribeAliasResponse
--
--         , responseValidateMatchmakingRuleSet $
--             mkValidateMatchmakingRuleSetResponse
--
--         , responseListScripts $
--             mkListScriptsResponse
--
--         , responseDescribeEC2InstanceLimits $
--             mkDescribeEC2InstanceLimitsResponse
--
--         , responseSuspendGameServerGroup $
--             mkSuspendGameServerGroupResponse
--
--         , responseDeleteMatchmakingRuleSet $
--             mkDeleteMatchmakingRuleSetResponse
--
--         , responseStopGameSessionPlacement $
--             mkStopGameSessionPlacementResponse
--
--         , responseClaimGameServer $
--             mkClaimGameServerResponse
--
--         , responseUpdateGameSession $
--             mkUpdateGameSessionResponse
--
--         , responseDescribeMatchmaking $
--             mkDescribeMatchmakingResponse
--
--         , responseCreatePlayerSession $
--             mkCreatePlayerSessionResponse
--
--         , responseDescribeGameServerGroup $
--             mkDescribeGameServerGroupResponse
--
--           ]
--     ]

-- Requests

requestStopMatchmaking :: StopMatchmaking -> TestTree
requestStopMatchmaking = req
    "StopMatchmaking"
    "fixture/StopMatchmaking.yaml"

requestDescribeGameServerInstances :: DescribeGameServerInstances -> TestTree
requestDescribeGameServerInstances = req
    "DescribeGameServerInstances"
    "fixture/DescribeGameServerInstances.yaml"

requestCreateGameSession :: CreateGameSession -> TestTree
requestCreateGameSession = req
    "CreateGameSession"
    "fixture/CreateGameSession.yaml"

requestDeleteScalingPolicy :: DeleteScalingPolicy -> TestTree
requestDeleteScalingPolicy = req
    "DeleteScalingPolicy"
    "fixture/DeleteScalingPolicy.yaml"

requestPutScalingPolicy :: PutScalingPolicy -> TestTree
requestPutScalingPolicy = req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

requestListBuilds :: ListBuilds -> TestTree
requestListBuilds = req
    "ListBuilds"
    "fixture/ListBuilds.yaml"

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet = req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestCreateBuild :: CreateBuild -> TestTree
requestCreateBuild = req
    "CreateBuild"
    "fixture/CreateBuild.yaml"

requestRequestUploadCredentials :: RequestUploadCredentials -> TestTree
requestRequestUploadCredentials = req
    "RequestUploadCredentials"
    "fixture/RequestUploadCredentials.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias = req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestListGameServers :: ListGameServers -> TestTree
requestListGameServers = req
    "ListGameServers"
    "fixture/ListGameServers.yaml"

requestResolveAlias :: ResolveAlias -> TestTree
requestResolveAlias = req
    "ResolveAlias"
    "fixture/ResolveAlias.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRegisterGameServer :: RegisterGameServer -> TestTree
requestRegisterGameServer = req
    "RegisterGameServer"
    "fixture/RegisterGameServer.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases = req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestUpdateRuntimeConfiguration :: UpdateRuntimeConfiguration -> TestTree
requestUpdateRuntimeConfiguration = req
    "UpdateRuntimeConfiguration"
    "fixture/UpdateRuntimeConfiguration.yaml"

requestCreateVpcPeeringConnection :: CreateVpcPeeringConnection -> TestTree
requestCreateVpcPeeringConnection = req
    "CreateVpcPeeringConnection"
    "fixture/CreateVpcPeeringConnection.yaml"

requestListGameServerGroups :: ListGameServerGroups -> TestTree
requestListGameServerGroups = req
    "ListGameServerGroups"
    "fixture/ListGameServerGroups.yaml"

requestCreateGameSessionQueue :: CreateGameSessionQueue -> TestTree
requestCreateGameSessionQueue = req
    "CreateGameSessionQueue"
    "fixture/CreateGameSessionQueue.yaml"

requestSearchGameSessions :: SearchGameSessions -> TestTree
requestSearchGameSessions = req
    "SearchGameSessions"
    "fixture/SearchGameSessions.yaml"

requestCreateVpcPeeringAuthorization :: CreateVpcPeeringAuthorization -> TestTree
requestCreateVpcPeeringAuthorization = req
    "CreateVpcPeeringAuthorization"
    "fixture/CreateVpcPeeringAuthorization.yaml"

requestUpdateGameSessionQueue :: UpdateGameSessionQueue -> TestTree
requestUpdateGameSessionQueue = req
    "UpdateGameSessionQueue"
    "fixture/UpdateGameSessionQueue.yaml"

requestDeleteGameSessionQueue :: DeleteGameSessionQueue -> TestTree
requestDeleteGameSessionQueue = req
    "DeleteGameSessionQueue"
    "fixture/DeleteGameSessionQueue.yaml"

requestCreateGameServerGroup :: CreateGameServerGroup -> TestTree
requestCreateGameServerGroup = req
    "CreateGameServerGroup"
    "fixture/CreateGameServerGroup.yaml"

requestDeleteVpcPeeringConnection :: DeleteVpcPeeringConnection -> TestTree
requestDeleteVpcPeeringConnection = req
    "DeleteVpcPeeringConnection"
    "fixture/DeleteVpcPeeringConnection.yaml"

requestStartFleetActions :: StartFleetActions -> TestTree
requestStartFleetActions = req
    "StartFleetActions"
    "fixture/StartFleetActions.yaml"

requestDeregisterGameServer :: DeregisterGameServer -> TestTree
requestDeregisterGameServer = req
    "DeregisterGameServer"
    "fixture/DeregisterGameServer.yaml"

requestGetInstanceAccess :: GetInstanceAccess -> TestTree
requestGetInstanceAccess = req
    "GetInstanceAccess"
    "fixture/GetInstanceAccess.yaml"

requestDescribeScalingPolicies :: DescribeScalingPolicies -> TestTree
requestDescribeScalingPolicies = req
    "DescribeScalingPolicies"
    "fixture/DescribeScalingPolicies.yaml"

requestDescribeMatchmakingRuleSets :: DescribeMatchmakingRuleSets -> TestTree
requestDescribeMatchmakingRuleSets = req
    "DescribeMatchmakingRuleSets"
    "fixture/DescribeMatchmakingRuleSets.yaml"

requestDescribeGameSessions :: DescribeGameSessions -> TestTree
requestDescribeGameSessions = req
    "DescribeGameSessions"
    "fixture/DescribeGameSessions.yaml"

requestDescribeGameServer :: DescribeGameServer -> TestTree
requestDescribeGameServer = req
    "DescribeGameServer"
    "fixture/DescribeGameServer.yaml"

requestUpdateScript :: UpdateScript -> TestTree
requestUpdateScript = req
    "UpdateScript"
    "fixture/UpdateScript.yaml"

requestDeleteScript :: DeleteScript -> TestTree
requestDeleteScript = req
    "DeleteScript"
    "fixture/DeleteScript.yaml"

requestStartGameSessionPlacement :: StartGameSessionPlacement -> TestTree
requestStartGameSessionPlacement = req
    "StartGameSessionPlacement"
    "fixture/StartGameSessionPlacement.yaml"

requestDescribeFleetUtilization :: DescribeFleetUtilization -> TestTree
requestDescribeFleetUtilization = req
    "DescribeFleetUtilization"
    "fixture/DescribeFleetUtilization.yaml"

requestDescribeRuntimeConfiguration :: DescribeRuntimeConfiguration -> TestTree
requestDescribeRuntimeConfiguration = req
    "DescribeRuntimeConfiguration"
    "fixture/DescribeRuntimeConfiguration.yaml"

requestGetGameSessionLogUrl :: GetGameSessionLogUrl -> TestTree
requestGetGameSessionLogUrl = req
    "GetGameSessionLogUrl"
    "fixture/GetGameSessionLogUrl.yaml"

requestDescribeFleetAttributes :: DescribeFleetAttributes -> TestTree
requestDescribeFleetAttributes = req
    "DescribeFleetAttributes"
    "fixture/DescribeFleetAttributes.yaml"

requestDescribeGameSessionPlacement :: DescribeGameSessionPlacement -> TestTree
requestDescribeGameSessionPlacement = req
    "DescribeGameSessionPlacement"
    "fixture/DescribeGameSessionPlacement.yaml"

requestDescribeFleetEvents :: DescribeFleetEvents -> TestTree
requestDescribeFleetEvents = req
    "DescribeFleetEvents"
    "fixture/DescribeFleetEvents.yaml"

requestStartMatchmaking :: StartMatchmaking -> TestTree
requestStartMatchmaking = req
    "StartMatchmaking"
    "fixture/StartMatchmaking.yaml"

requestCreateMatchmakingRuleSet :: CreateMatchmakingRuleSet -> TestTree
requestCreateMatchmakingRuleSet = req
    "CreateMatchmakingRuleSet"
    "fixture/CreateMatchmakingRuleSet.yaml"

requestDescribeFleetCapacity :: DescribeFleetCapacity -> TestTree
requestDescribeFleetCapacity = req
    "DescribeFleetCapacity"
    "fixture/DescribeFleetCapacity.yaml"

requestDeleteBuild :: DeleteBuild -> TestTree
requestDeleteBuild = req
    "DeleteBuild"
    "fixture/DeleteBuild.yaml"

requestUpdateBuild :: UpdateBuild -> TestTree
requestUpdateBuild = req
    "UpdateBuild"
    "fixture/UpdateBuild.yaml"

requestListFleets :: ListFleets -> TestTree
requestListFleets = req
    "ListFleets"
    "fixture/ListFleets.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias = req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestUpdateAlias :: UpdateAlias -> TestTree
requestUpdateAlias = req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

requestStartMatchBackfill :: StartMatchBackfill -> TestTree
requestStartMatchBackfill = req
    "StartMatchBackfill"
    "fixture/StartMatchBackfill.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances = req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestDescribeGameSessionDetails :: DescribeGameSessionDetails -> TestTree
requestDescribeGameSessionDetails = req
    "DescribeGameSessionDetails"
    "fixture/DescribeGameSessionDetails.yaml"

requestDescribeFleetPortSettings :: DescribeFleetPortSettings -> TestTree
requestDescribeFleetPortSettings = req
    "DescribeFleetPortSettings"
    "fixture/DescribeFleetPortSettings.yaml"

requestDescribeGameSessionQueues :: DescribeGameSessionQueues -> TestTree
requestDescribeGameSessionQueues = req
    "DescribeGameSessionQueues"
    "fixture/DescribeGameSessionQueues.yaml"

requestDescribeVpcPeeringConnections :: DescribeVpcPeeringConnections -> TestTree
requestDescribeVpcPeeringConnections = req
    "DescribeVpcPeeringConnections"
    "fixture/DescribeVpcPeeringConnections.yaml"

requestDescribeScript :: DescribeScript -> TestTree
requestDescribeScript = req
    "DescribeScript"
    "fixture/DescribeScript.yaml"

requestCreatePlayerSessions :: CreatePlayerSessions -> TestTree
requestCreatePlayerSessions = req
    "CreatePlayerSessions"
    "fixture/CreatePlayerSessions.yaml"

requestDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurations -> TestTree
requestDescribeMatchmakingConfigurations = req
    "DescribeMatchmakingConfigurations"
    "fixture/DescribeMatchmakingConfigurations.yaml"

requestDescribeVpcPeeringAuthorizations :: DescribeVpcPeeringAuthorizations -> TestTree
requestDescribeVpcPeeringAuthorizations = req
    "DescribeVpcPeeringAuthorizations"
    "fixture/DescribeVpcPeeringAuthorizations.yaml"

requestUpdateGameServer :: UpdateGameServer -> TestTree
requestUpdateGameServer = req
    "UpdateGameServer"
    "fixture/UpdateGameServer.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet = req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestDeleteMatchmakingConfiguration :: DeleteMatchmakingConfiguration -> TestTree
requestDeleteMatchmakingConfiguration = req
    "DeleteMatchmakingConfiguration"
    "fixture/DeleteMatchmakingConfiguration.yaml"

requestUpdateMatchmakingConfiguration :: UpdateMatchmakingConfiguration -> TestTree
requestUpdateMatchmakingConfiguration = req
    "UpdateMatchmakingConfiguration"
    "fixture/UpdateMatchmakingConfiguration.yaml"

requestDeleteGameServerGroup :: DeleteGameServerGroup -> TestTree
requestDeleteGameServerGroup = req
    "DeleteGameServerGroup"
    "fixture/DeleteGameServerGroup.yaml"

requestUpdateGameServerGroup :: UpdateGameServerGroup -> TestTree
requestUpdateGameServerGroup = req
    "UpdateGameServerGroup"
    "fixture/UpdateGameServerGroup.yaml"

requestResumeGameServerGroup :: ResumeGameServerGroup -> TestTree
requestResumeGameServerGroup = req
    "ResumeGameServerGroup"
    "fixture/ResumeGameServerGroup.yaml"

requestDeleteVpcPeeringAuthorization :: DeleteVpcPeeringAuthorization -> TestTree
requestDeleteVpcPeeringAuthorization = req
    "DeleteVpcPeeringAuthorization"
    "fixture/DeleteVpcPeeringAuthorization.yaml"

requestUpdateFleetAttributes :: UpdateFleetAttributes -> TestTree
requestUpdateFleetAttributes = req
    "UpdateFleetAttributes"
    "fixture/UpdateFleetAttributes.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateMatchmakingConfiguration :: CreateMatchmakingConfiguration -> TestTree
requestCreateMatchmakingConfiguration = req
    "CreateMatchmakingConfiguration"
    "fixture/CreateMatchmakingConfiguration.yaml"

requestDescribePlayerSessions :: DescribePlayerSessions -> TestTree
requestDescribePlayerSessions = req
    "DescribePlayerSessions"
    "fixture/DescribePlayerSessions.yaml"

requestStopFleetActions :: StopFleetActions -> TestTree
requestStopFleetActions = req
    "StopFleetActions"
    "fixture/StopFleetActions.yaml"

requestDescribeBuild :: DescribeBuild -> TestTree
requestDescribeBuild = req
    "DescribeBuild"
    "fixture/DescribeBuild.yaml"

requestUpdateFleetPortSettings :: UpdateFleetPortSettings -> TestTree
requestUpdateFleetPortSettings = req
    "UpdateFleetPortSettings"
    "fixture/UpdateFleetPortSettings.yaml"

requestUpdateFleetCapacity :: UpdateFleetCapacity -> TestTree
requestUpdateFleetCapacity = req
    "UpdateFleetCapacity"
    "fixture/UpdateFleetCapacity.yaml"

requestCreateScript :: CreateScript -> TestTree
requestCreateScript = req
    "CreateScript"
    "fixture/CreateScript.yaml"

requestAcceptMatch :: AcceptMatch -> TestTree
requestAcceptMatch = req
    "AcceptMatch"
    "fixture/AcceptMatch.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeAlias :: DescribeAlias -> TestTree
requestDescribeAlias = req
    "DescribeAlias"
    "fixture/DescribeAlias.yaml"

requestValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSet -> TestTree
requestValidateMatchmakingRuleSet = req
    "ValidateMatchmakingRuleSet"
    "fixture/ValidateMatchmakingRuleSet.yaml"

requestListScripts :: ListScripts -> TestTree
requestListScripts = req
    "ListScripts"
    "fixture/ListScripts.yaml"

requestDescribeEC2InstanceLimits :: DescribeEC2InstanceLimits -> TestTree
requestDescribeEC2InstanceLimits = req
    "DescribeEC2InstanceLimits"
    "fixture/DescribeEC2InstanceLimits.yaml"

requestSuspendGameServerGroup :: SuspendGameServerGroup -> TestTree
requestSuspendGameServerGroup = req
    "SuspendGameServerGroup"
    "fixture/SuspendGameServerGroup.yaml"

requestDeleteMatchmakingRuleSet :: DeleteMatchmakingRuleSet -> TestTree
requestDeleteMatchmakingRuleSet = req
    "DeleteMatchmakingRuleSet"
    "fixture/DeleteMatchmakingRuleSet.yaml"

requestStopGameSessionPlacement :: StopGameSessionPlacement -> TestTree
requestStopGameSessionPlacement = req
    "StopGameSessionPlacement"
    "fixture/StopGameSessionPlacement.yaml"

requestClaimGameServer :: ClaimGameServer -> TestTree
requestClaimGameServer = req
    "ClaimGameServer"
    "fixture/ClaimGameServer.yaml"

requestUpdateGameSession :: UpdateGameSession -> TestTree
requestUpdateGameSession = req
    "UpdateGameSession"
    "fixture/UpdateGameSession.yaml"

requestDescribeMatchmaking :: DescribeMatchmaking -> TestTree
requestDescribeMatchmaking = req
    "DescribeMatchmaking"
    "fixture/DescribeMatchmaking.yaml"

requestCreatePlayerSession :: CreatePlayerSession -> TestTree
requestCreatePlayerSession = req
    "CreatePlayerSession"
    "fixture/CreatePlayerSession.yaml"

requestDescribeGameServerGroup :: DescribeGameServerGroup -> TestTree
requestDescribeGameServerGroup = req
    "DescribeGameServerGroup"
    "fixture/DescribeGameServerGroup.yaml"

-- Responses

responseStopMatchmaking :: StopMatchmakingResponse -> TestTree
responseStopMatchmaking = res
    "StopMatchmakingResponse"
    "fixture/StopMatchmakingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopMatchmaking)

responseDescribeGameServerInstances :: DescribeGameServerInstancesResponse -> TestTree
responseDescribeGameServerInstances = res
    "DescribeGameServerInstancesResponse"
    "fixture/DescribeGameServerInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeGameServerInstances)

responseCreateGameSession :: CreateGameSessionResponse -> TestTree
responseCreateGameSession = res
    "CreateGameSessionResponse"
    "fixture/CreateGameSessionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGameSession)

responseDeleteScalingPolicy :: DeleteScalingPolicyResponse -> TestTree
responseDeleteScalingPolicy = res
    "DeleteScalingPolicyResponse"
    "fixture/DeleteScalingPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteScalingPolicy)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy = res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutScalingPolicy)

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds = res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBuilds)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet = res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFleet)

responseCreateBuild :: CreateBuildResponse -> TestTree
responseCreateBuild = res
    "CreateBuildResponse"
    "fixture/CreateBuildResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateBuild)

responseRequestUploadCredentials :: RequestUploadCredentialsResponse -> TestTree
responseRequestUploadCredentials = res
    "RequestUploadCredentialsResponse"
    "fixture/RequestUploadCredentialsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RequestUploadCredentials)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAlias)

responseListGameServers :: ListGameServersResponse -> TestTree
responseListGameServers = res
    "ListGameServersResponse"
    "fixture/ListGameServersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGameServers)

responseResolveAlias :: ResolveAliasResponse -> TestTree
responseResolveAlias = res
    "ResolveAliasResponse"
    "fixture/ResolveAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResolveAlias)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseRegisterGameServer :: RegisterGameServerResponse -> TestTree
responseRegisterGameServer = res
    "RegisterGameServerResponse"
    "fixture/RegisterGameServerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterGameServer)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases = res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAliases)

responseUpdateRuntimeConfiguration :: UpdateRuntimeConfigurationResponse -> TestTree
responseUpdateRuntimeConfiguration = res
    "UpdateRuntimeConfigurationResponse"
    "fixture/UpdateRuntimeConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRuntimeConfiguration)

responseCreateVpcPeeringConnection :: CreateVpcPeeringConnectionResponse -> TestTree
responseCreateVpcPeeringConnection = res
    "CreateVpcPeeringConnectionResponse"
    "fixture/CreateVpcPeeringConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVpcPeeringConnection)

responseListGameServerGroups :: ListGameServerGroupsResponse -> TestTree
responseListGameServerGroups = res
    "ListGameServerGroupsResponse"
    "fixture/ListGameServerGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGameServerGroups)

responseCreateGameSessionQueue :: CreateGameSessionQueueResponse -> TestTree
responseCreateGameSessionQueue = res
    "CreateGameSessionQueueResponse"
    "fixture/CreateGameSessionQueueResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGameSessionQueue)

responseSearchGameSessions :: SearchGameSessionsResponse -> TestTree
responseSearchGameSessions = res
    "SearchGameSessionsResponse"
    "fixture/SearchGameSessionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchGameSessions)

responseCreateVpcPeeringAuthorization :: CreateVpcPeeringAuthorizationResponse -> TestTree
responseCreateVpcPeeringAuthorization = res
    "CreateVpcPeeringAuthorizationResponse"
    "fixture/CreateVpcPeeringAuthorizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVpcPeeringAuthorization)

responseUpdateGameSessionQueue :: UpdateGameSessionQueueResponse -> TestTree
responseUpdateGameSessionQueue = res
    "UpdateGameSessionQueueResponse"
    "fixture/UpdateGameSessionQueueResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGameSessionQueue)

responseDeleteGameSessionQueue :: DeleteGameSessionQueueResponse -> TestTree
responseDeleteGameSessionQueue = res
    "DeleteGameSessionQueueResponse"
    "fixture/DeleteGameSessionQueueResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteGameSessionQueue)

responseCreateGameServerGroup :: CreateGameServerGroupResponse -> TestTree
responseCreateGameServerGroup = res
    "CreateGameServerGroupResponse"
    "fixture/CreateGameServerGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGameServerGroup)

responseDeleteVpcPeeringConnection :: DeleteVpcPeeringConnectionResponse -> TestTree
responseDeleteVpcPeeringConnection = res
    "DeleteVpcPeeringConnectionResponse"
    "fixture/DeleteVpcPeeringConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVpcPeeringConnection)

responseStartFleetActions :: StartFleetActionsResponse -> TestTree
responseStartFleetActions = res
    "StartFleetActionsResponse"
    "fixture/StartFleetActionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartFleetActions)

responseDeregisterGameServer :: DeregisterGameServerResponse -> TestTree
responseDeregisterGameServer = res
    "DeregisterGameServerResponse"
    "fixture/DeregisterGameServerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterGameServer)

responseGetInstanceAccess :: GetInstanceAccessResponse -> TestTree
responseGetInstanceAccess = res
    "GetInstanceAccessResponse"
    "fixture/GetInstanceAccessResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetInstanceAccess)

responseDescribeScalingPolicies :: DescribeScalingPoliciesResponse -> TestTree
responseDescribeScalingPolicies = res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeScalingPolicies)

responseDescribeMatchmakingRuleSets :: DescribeMatchmakingRuleSetsResponse -> TestTree
responseDescribeMatchmakingRuleSets = res
    "DescribeMatchmakingRuleSetsResponse"
    "fixture/DescribeMatchmakingRuleSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMatchmakingRuleSets)

responseDescribeGameSessions :: DescribeGameSessionsResponse -> TestTree
responseDescribeGameSessions = res
    "DescribeGameSessionsResponse"
    "fixture/DescribeGameSessionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeGameSessions)

responseDescribeGameServer :: DescribeGameServerResponse -> TestTree
responseDescribeGameServer = res
    "DescribeGameServerResponse"
    "fixture/DescribeGameServerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeGameServer)

responseUpdateScript :: UpdateScriptResponse -> TestTree
responseUpdateScript = res
    "UpdateScriptResponse"
    "fixture/UpdateScriptResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateScript)

responseDeleteScript :: DeleteScriptResponse -> TestTree
responseDeleteScript = res
    "DeleteScriptResponse"
    "fixture/DeleteScriptResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteScript)

responseStartGameSessionPlacement :: StartGameSessionPlacementResponse -> TestTree
responseStartGameSessionPlacement = res
    "StartGameSessionPlacementResponse"
    "fixture/StartGameSessionPlacementResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartGameSessionPlacement)

responseDescribeFleetUtilization :: DescribeFleetUtilizationResponse -> TestTree
responseDescribeFleetUtilization = res
    "DescribeFleetUtilizationResponse"
    "fixture/DescribeFleetUtilizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFleetUtilization)

responseDescribeRuntimeConfiguration :: DescribeRuntimeConfigurationResponse -> TestTree
responseDescribeRuntimeConfiguration = res
    "DescribeRuntimeConfigurationResponse"
    "fixture/DescribeRuntimeConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeRuntimeConfiguration)

responseGetGameSessionLogUrl :: GetGameSessionLogUrlResponse -> TestTree
responseGetGameSessionLogUrl = res
    "GetGameSessionLogUrlResponse"
    "fixture/GetGameSessionLogUrlResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGameSessionLogUrl)

responseDescribeFleetAttributes :: DescribeFleetAttributesResponse -> TestTree
responseDescribeFleetAttributes = res
    "DescribeFleetAttributesResponse"
    "fixture/DescribeFleetAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFleetAttributes)

responseDescribeGameSessionPlacement :: DescribeGameSessionPlacementResponse -> TestTree
responseDescribeGameSessionPlacement = res
    "DescribeGameSessionPlacementResponse"
    "fixture/DescribeGameSessionPlacementResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeGameSessionPlacement)

responseDescribeFleetEvents :: DescribeFleetEventsResponse -> TestTree
responseDescribeFleetEvents = res
    "DescribeFleetEventsResponse"
    "fixture/DescribeFleetEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFleetEvents)

responseStartMatchmaking :: StartMatchmakingResponse -> TestTree
responseStartMatchmaking = res
    "StartMatchmakingResponse"
    "fixture/StartMatchmakingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartMatchmaking)

responseCreateMatchmakingRuleSet :: CreateMatchmakingRuleSetResponse -> TestTree
responseCreateMatchmakingRuleSet = res
    "CreateMatchmakingRuleSetResponse"
    "fixture/CreateMatchmakingRuleSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateMatchmakingRuleSet)

responseDescribeFleetCapacity :: DescribeFleetCapacityResponse -> TestTree
responseDescribeFleetCapacity = res
    "DescribeFleetCapacityResponse"
    "fixture/DescribeFleetCapacityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFleetCapacity)

responseDeleteBuild :: DeleteBuildResponse -> TestTree
responseDeleteBuild = res
    "DeleteBuildResponse"
    "fixture/DeleteBuildResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBuild)

responseUpdateBuild :: UpdateBuildResponse -> TestTree
responseUpdateBuild = res
    "UpdateBuildResponse"
    "fixture/UpdateBuildResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateBuild)

responseListFleets :: ListFleetsResponse -> TestTree
responseListFleets = res
    "ListFleetsResponse"
    "fixture/ListFleetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListFleets)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias = res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAlias)

responseUpdateAlias :: UpdateAliasResponse -> TestTree
responseUpdateAlias = res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAlias)

responseStartMatchBackfill :: StartMatchBackfillResponse -> TestTree
responseStartMatchBackfill = res
    "StartMatchBackfillResponse"
    "fixture/StartMatchBackfillResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartMatchBackfill)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances = res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInstances)

responseDescribeGameSessionDetails :: DescribeGameSessionDetailsResponse -> TestTree
responseDescribeGameSessionDetails = res
    "DescribeGameSessionDetailsResponse"
    "fixture/DescribeGameSessionDetailsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeGameSessionDetails)

responseDescribeFleetPortSettings :: DescribeFleetPortSettingsResponse -> TestTree
responseDescribeFleetPortSettings = res
    "DescribeFleetPortSettingsResponse"
    "fixture/DescribeFleetPortSettingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFleetPortSettings)

responseDescribeGameSessionQueues :: DescribeGameSessionQueuesResponse -> TestTree
responseDescribeGameSessionQueues = res
    "DescribeGameSessionQueuesResponse"
    "fixture/DescribeGameSessionQueuesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeGameSessionQueues)

responseDescribeVpcPeeringConnections :: DescribeVpcPeeringConnectionsResponse -> TestTree
responseDescribeVpcPeeringConnections = res
    "DescribeVpcPeeringConnectionsResponse"
    "fixture/DescribeVpcPeeringConnectionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcPeeringConnections)

responseDescribeScript :: DescribeScriptResponse -> TestTree
responseDescribeScript = res
    "DescribeScriptResponse"
    "fixture/DescribeScriptResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeScript)

responseCreatePlayerSessions :: CreatePlayerSessionsResponse -> TestTree
responseCreatePlayerSessions = res
    "CreatePlayerSessionsResponse"
    "fixture/CreatePlayerSessionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePlayerSessions)

responseDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurationsResponse -> TestTree
responseDescribeMatchmakingConfigurations = res
    "DescribeMatchmakingConfigurationsResponse"
    "fixture/DescribeMatchmakingConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMatchmakingConfigurations)

responseDescribeVpcPeeringAuthorizations :: DescribeVpcPeeringAuthorizationsResponse -> TestTree
responseDescribeVpcPeeringAuthorizations = res
    "DescribeVpcPeeringAuthorizationsResponse"
    "fixture/DescribeVpcPeeringAuthorizationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcPeeringAuthorizations)

responseUpdateGameServer :: UpdateGameServerResponse -> TestTree
responseUpdateGameServer = res
    "UpdateGameServerResponse"
    "fixture/UpdateGameServerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGameServer)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet = res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateFleet)

responseDeleteMatchmakingConfiguration :: DeleteMatchmakingConfigurationResponse -> TestTree
responseDeleteMatchmakingConfiguration = res
    "DeleteMatchmakingConfigurationResponse"
    "fixture/DeleteMatchmakingConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteMatchmakingConfiguration)

responseUpdateMatchmakingConfiguration :: UpdateMatchmakingConfigurationResponse -> TestTree
responseUpdateMatchmakingConfiguration = res
    "UpdateMatchmakingConfigurationResponse"
    "fixture/UpdateMatchmakingConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateMatchmakingConfiguration)

responseDeleteGameServerGroup :: DeleteGameServerGroupResponse -> TestTree
responseDeleteGameServerGroup = res
    "DeleteGameServerGroupResponse"
    "fixture/DeleteGameServerGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteGameServerGroup)

responseUpdateGameServerGroup :: UpdateGameServerGroupResponse -> TestTree
responseUpdateGameServerGroup = res
    "UpdateGameServerGroupResponse"
    "fixture/UpdateGameServerGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGameServerGroup)

responseResumeGameServerGroup :: ResumeGameServerGroupResponse -> TestTree
responseResumeGameServerGroup = res
    "ResumeGameServerGroupResponse"
    "fixture/ResumeGameServerGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResumeGameServerGroup)

responseDeleteVpcPeeringAuthorization :: DeleteVpcPeeringAuthorizationResponse -> TestTree
responseDeleteVpcPeeringAuthorization = res
    "DeleteVpcPeeringAuthorizationResponse"
    "fixture/DeleteVpcPeeringAuthorizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVpcPeeringAuthorization)

responseUpdateFleetAttributes :: UpdateFleetAttributesResponse -> TestTree
responseUpdateFleetAttributes = res
    "UpdateFleetAttributesResponse"
    "fixture/UpdateFleetAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateFleetAttributes)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseCreateMatchmakingConfiguration :: CreateMatchmakingConfigurationResponse -> TestTree
responseCreateMatchmakingConfiguration = res
    "CreateMatchmakingConfigurationResponse"
    "fixture/CreateMatchmakingConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateMatchmakingConfiguration)

responseDescribePlayerSessions :: DescribePlayerSessionsResponse -> TestTree
responseDescribePlayerSessions = res
    "DescribePlayerSessionsResponse"
    "fixture/DescribePlayerSessionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribePlayerSessions)

responseStopFleetActions :: StopFleetActionsResponse -> TestTree
responseStopFleetActions = res
    "StopFleetActionsResponse"
    "fixture/StopFleetActionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopFleetActions)

responseDescribeBuild :: DescribeBuildResponse -> TestTree
responseDescribeBuild = res
    "DescribeBuildResponse"
    "fixture/DescribeBuildResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeBuild)

responseUpdateFleetPortSettings :: UpdateFleetPortSettingsResponse -> TestTree
responseUpdateFleetPortSettings = res
    "UpdateFleetPortSettingsResponse"
    "fixture/UpdateFleetPortSettingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateFleetPortSettings)

responseUpdateFleetCapacity :: UpdateFleetCapacityResponse -> TestTree
responseUpdateFleetCapacity = res
    "UpdateFleetCapacityResponse"
    "fixture/UpdateFleetCapacityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateFleetCapacity)

responseCreateScript :: CreateScriptResponse -> TestTree
responseCreateScript = res
    "CreateScriptResponse"
    "fixture/CreateScriptResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateScript)

responseAcceptMatch :: AcceptMatchResponse -> TestTree
responseAcceptMatch = res
    "AcceptMatchResponse"
    "fixture/AcceptMatchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AcceptMatch)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseDescribeAlias :: DescribeAliasResponse -> TestTree
responseDescribeAlias = res
    "DescribeAliasResponse"
    "fixture/DescribeAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAlias)

responseValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSetResponse -> TestTree
responseValidateMatchmakingRuleSet = res
    "ValidateMatchmakingRuleSetResponse"
    "fixture/ValidateMatchmakingRuleSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ValidateMatchmakingRuleSet)

responseListScripts :: ListScriptsResponse -> TestTree
responseListScripts = res
    "ListScriptsResponse"
    "fixture/ListScriptsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListScripts)

responseDescribeEC2InstanceLimits :: DescribeEC2InstanceLimitsResponse -> TestTree
responseDescribeEC2InstanceLimits = res
    "DescribeEC2InstanceLimitsResponse"
    "fixture/DescribeEC2InstanceLimitsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEC2InstanceLimits)

responseSuspendGameServerGroup :: SuspendGameServerGroupResponse -> TestTree
responseSuspendGameServerGroup = res
    "SuspendGameServerGroupResponse"
    "fixture/SuspendGameServerGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SuspendGameServerGroup)

responseDeleteMatchmakingRuleSet :: DeleteMatchmakingRuleSetResponse -> TestTree
responseDeleteMatchmakingRuleSet = res
    "DeleteMatchmakingRuleSetResponse"
    "fixture/DeleteMatchmakingRuleSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteMatchmakingRuleSet)

responseStopGameSessionPlacement :: StopGameSessionPlacementResponse -> TestTree
responseStopGameSessionPlacement = res
    "StopGameSessionPlacementResponse"
    "fixture/StopGameSessionPlacementResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopGameSessionPlacement)

responseClaimGameServer :: ClaimGameServerResponse -> TestTree
responseClaimGameServer = res
    "ClaimGameServerResponse"
    "fixture/ClaimGameServerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ClaimGameServer)

responseUpdateGameSession :: UpdateGameSessionResponse -> TestTree
responseUpdateGameSession = res
    "UpdateGameSessionResponse"
    "fixture/UpdateGameSessionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGameSession)

responseDescribeMatchmaking :: DescribeMatchmakingResponse -> TestTree
responseDescribeMatchmaking = res
    "DescribeMatchmakingResponse"
    "fixture/DescribeMatchmakingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMatchmaking)

responseCreatePlayerSession :: CreatePlayerSessionResponse -> TestTree
responseCreatePlayerSession = res
    "CreatePlayerSessionResponse"
    "fixture/CreatePlayerSessionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePlayerSession)

responseDescribeGameServerGroup :: DescribeGameServerGroupResponse -> TestTree
responseDescribeGameServerGroup = res
    "DescribeGameServerGroupResponse"
    "fixture/DescribeGameServerGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeGameServerGroup)
