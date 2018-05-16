{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.GameLift
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         , requestResolveAlias $
--             resolveAlias
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
--         , requestDeleteVPCPeeringConnection $
--             deleteVPCPeeringConnection
--
--         , requestStartFleetActions $
--             startFleetActions
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
--         , requestCreatePlayerSessions $
--             createPlayerSessions
--
--         , requestDescribeMatchmakingConfigurations $
--             describeMatchmakingConfigurations
--
--         , requestDescribeVPCPeeringAuthorizations $
--             describeVPCPeeringAuthorizations
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
--         , requestDeleteVPCPeeringAuthorization $
--             deleteVPCPeeringAuthorization
--
--         , requestUpdateFleetAttributes $
--             updateFleetAttributes
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
--         , requestAcceptMatch $
--             acceptMatch
--
--         , requestDescribeAlias $
--             describeAlias
--
--         , requestValidateMatchmakingRuleSet $
--             validateMatchmakingRuleSet
--
--         , requestDescribeEC2InstanceLimits $
--             describeEC2InstanceLimits
--
--         , requestStopGameSessionPlacement $
--             stopGameSessionPlacement
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
--           ]

--     , testGroup "response"
--         [ responseStopMatchmaking $
--             stopMatchmakingResponse
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
--         , responseResolveAlias $
--             resolveAliasResponse
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
--         , responseDeleteVPCPeeringConnection $
--             deleteVPCPeeringConnectionResponse
--
--         , responseStartFleetActions $
--             startFleetActionsResponse
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
--         , responseCreatePlayerSessions $
--             createPlayerSessionsResponse
--
--         , responseDescribeMatchmakingConfigurations $
--             describeMatchmakingConfigurationsResponse
--
--         , responseDescribeVPCPeeringAuthorizations $
--             describeVPCPeeringAuthorizationsResponse
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
--         , responseDeleteVPCPeeringAuthorization $
--             deleteVPCPeeringAuthorizationResponse
--
--         , responseUpdateFleetAttributes $
--             updateFleetAttributesResponse
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
--         , responseAcceptMatch $
--             acceptMatchResponse
--
--         , responseDescribeAlias $
--             describeAliasResponse
--
--         , responseValidateMatchmakingRuleSet $
--             validateMatchmakingRuleSetResponse
--
--         , responseDescribeEC2InstanceLimits $
--             describeEC2InstanceLimitsResponse
--
--         , responseStopGameSessionPlacement $
--             stopGameSessionPlacementResponse
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
--           ]
--     ]

-- Requests

requestStopMatchmaking :: StopMatchmaking -> TestTree
requestStopMatchmaking = req
    "StopMatchmaking"
    "fixture/StopMatchmaking.yaml"

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

requestResolveAlias :: ResolveAlias -> TestTree
requestResolveAlias = req
    "ResolveAlias"
    "fixture/ResolveAlias.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases = req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestUpdateRuntimeConfiguration :: UpdateRuntimeConfiguration -> TestTree
requestUpdateRuntimeConfiguration = req
    "UpdateRuntimeConfiguration"
    "fixture/UpdateRuntimeConfiguration.yaml"

requestCreateVPCPeeringConnection :: CreateVPCPeeringConnection -> TestTree
requestCreateVPCPeeringConnection = req
    "CreateVPCPeeringConnection"
    "fixture/CreateVPCPeeringConnection.yaml"

requestCreateGameSessionQueue :: CreateGameSessionQueue -> TestTree
requestCreateGameSessionQueue = req
    "CreateGameSessionQueue"
    "fixture/CreateGameSessionQueue.yaml"

requestSearchGameSessions :: SearchGameSessions -> TestTree
requestSearchGameSessions = req
    "SearchGameSessions"
    "fixture/SearchGameSessions.yaml"

requestCreateVPCPeeringAuthorization :: CreateVPCPeeringAuthorization -> TestTree
requestCreateVPCPeeringAuthorization = req
    "CreateVPCPeeringAuthorization"
    "fixture/CreateVPCPeeringAuthorization.yaml"

requestUpdateGameSessionQueue :: UpdateGameSessionQueue -> TestTree
requestUpdateGameSessionQueue = req
    "UpdateGameSessionQueue"
    "fixture/UpdateGameSessionQueue.yaml"

requestDeleteGameSessionQueue :: DeleteGameSessionQueue -> TestTree
requestDeleteGameSessionQueue = req
    "DeleteGameSessionQueue"
    "fixture/DeleteGameSessionQueue.yaml"

requestDeleteVPCPeeringConnection :: DeleteVPCPeeringConnection -> TestTree
requestDeleteVPCPeeringConnection = req
    "DeleteVPCPeeringConnection"
    "fixture/DeleteVPCPeeringConnection.yaml"

requestStartFleetActions :: StartFleetActions -> TestTree
requestStartFleetActions = req
    "StartFleetActions"
    "fixture/StartFleetActions.yaml"

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

requestGetGameSessionLogURL :: GetGameSessionLogURL -> TestTree
requestGetGameSessionLogURL = req
    "GetGameSessionLogURL"
    "fixture/GetGameSessionLogURL.yaml"

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

requestDescribeVPCPeeringConnections :: DescribeVPCPeeringConnections -> TestTree
requestDescribeVPCPeeringConnections = req
    "DescribeVPCPeeringConnections"
    "fixture/DescribeVPCPeeringConnections.yaml"

requestCreatePlayerSessions :: CreatePlayerSessions -> TestTree
requestCreatePlayerSessions = req
    "CreatePlayerSessions"
    "fixture/CreatePlayerSessions.yaml"

requestDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurations -> TestTree
requestDescribeMatchmakingConfigurations = req
    "DescribeMatchmakingConfigurations"
    "fixture/DescribeMatchmakingConfigurations.yaml"

requestDescribeVPCPeeringAuthorizations :: DescribeVPCPeeringAuthorizations -> TestTree
requestDescribeVPCPeeringAuthorizations = req
    "DescribeVPCPeeringAuthorizations"
    "fixture/DescribeVPCPeeringAuthorizations.yaml"

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

requestDeleteVPCPeeringAuthorization :: DeleteVPCPeeringAuthorization -> TestTree
requestDeleteVPCPeeringAuthorization = req
    "DeleteVPCPeeringAuthorization"
    "fixture/DeleteVPCPeeringAuthorization.yaml"

requestUpdateFleetAttributes :: UpdateFleetAttributes -> TestTree
requestUpdateFleetAttributes = req
    "UpdateFleetAttributes"
    "fixture/UpdateFleetAttributes.yaml"

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

requestAcceptMatch :: AcceptMatch -> TestTree
requestAcceptMatch = req
    "AcceptMatch"
    "fixture/AcceptMatch.yaml"

requestDescribeAlias :: DescribeAlias -> TestTree
requestDescribeAlias = req
    "DescribeAlias"
    "fixture/DescribeAlias.yaml"

requestValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSet -> TestTree
requestValidateMatchmakingRuleSet = req
    "ValidateMatchmakingRuleSet"
    "fixture/ValidateMatchmakingRuleSet.yaml"

requestDescribeEC2InstanceLimits :: DescribeEC2InstanceLimits -> TestTree
requestDescribeEC2InstanceLimits = req
    "DescribeEC2InstanceLimits"
    "fixture/DescribeEC2InstanceLimits.yaml"

requestStopGameSessionPlacement :: StopGameSessionPlacement -> TestTree
requestStopGameSessionPlacement = req
    "StopGameSessionPlacement"
    "fixture/StopGameSessionPlacement.yaml"

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

-- Responses

responseStopMatchmaking :: StopMatchmakingResponse -> TestTree
responseStopMatchmaking = res
    "StopMatchmakingResponse"
    "fixture/StopMatchmakingResponse.proto"
    gameLift
    (Proxy :: Proxy StopMatchmaking)

responseCreateGameSession :: CreateGameSessionResponse -> TestTree
responseCreateGameSession = res
    "CreateGameSessionResponse"
    "fixture/CreateGameSessionResponse.proto"
    gameLift
    (Proxy :: Proxy CreateGameSession)

responseDeleteScalingPolicy :: DeleteScalingPolicyResponse -> TestTree
responseDeleteScalingPolicy = res
    "DeleteScalingPolicyResponse"
    "fixture/DeleteScalingPolicyResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteScalingPolicy)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy = res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    gameLift
    (Proxy :: Proxy PutScalingPolicy)

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds = res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    gameLift
    (Proxy :: Proxy ListBuilds)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet = res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteFleet)

responseCreateBuild :: CreateBuildResponse -> TestTree
responseCreateBuild = res
    "CreateBuildResponse"
    "fixture/CreateBuildResponse.proto"
    gameLift
    (Proxy :: Proxy CreateBuild)

responseRequestUploadCredentials :: RequestUploadCredentialsResponse -> TestTree
responseRequestUploadCredentials = res
    "RequestUploadCredentialsResponse"
    "fixture/RequestUploadCredentialsResponse.proto"
    gameLift
    (Proxy :: Proxy RequestUploadCredentials)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    gameLift
    (Proxy :: Proxy CreateAlias)

responseResolveAlias :: ResolveAliasResponse -> TestTree
responseResolveAlias = res
    "ResolveAliasResponse"
    "fixture/ResolveAliasResponse.proto"
    gameLift
    (Proxy :: Proxy ResolveAlias)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases = res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    gameLift
    (Proxy :: Proxy ListAliases)

responseUpdateRuntimeConfiguration :: UpdateRuntimeConfigurationResponse -> TestTree
responseUpdateRuntimeConfiguration = res
    "UpdateRuntimeConfigurationResponse"
    "fixture/UpdateRuntimeConfigurationResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateRuntimeConfiguration)

responseCreateVPCPeeringConnection :: CreateVPCPeeringConnectionResponse -> TestTree
responseCreateVPCPeeringConnection = res
    "CreateVPCPeeringConnectionResponse"
    "fixture/CreateVPCPeeringConnectionResponse.proto"
    gameLift
    (Proxy :: Proxy CreateVPCPeeringConnection)

responseCreateGameSessionQueue :: CreateGameSessionQueueResponse -> TestTree
responseCreateGameSessionQueue = res
    "CreateGameSessionQueueResponse"
    "fixture/CreateGameSessionQueueResponse.proto"
    gameLift
    (Proxy :: Proxy CreateGameSessionQueue)

responseSearchGameSessions :: SearchGameSessionsResponse -> TestTree
responseSearchGameSessions = res
    "SearchGameSessionsResponse"
    "fixture/SearchGameSessionsResponse.proto"
    gameLift
    (Proxy :: Proxy SearchGameSessions)

responseCreateVPCPeeringAuthorization :: CreateVPCPeeringAuthorizationResponse -> TestTree
responseCreateVPCPeeringAuthorization = res
    "CreateVPCPeeringAuthorizationResponse"
    "fixture/CreateVPCPeeringAuthorizationResponse.proto"
    gameLift
    (Proxy :: Proxy CreateVPCPeeringAuthorization)

responseUpdateGameSessionQueue :: UpdateGameSessionQueueResponse -> TestTree
responseUpdateGameSessionQueue = res
    "UpdateGameSessionQueueResponse"
    "fixture/UpdateGameSessionQueueResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateGameSessionQueue)

responseDeleteGameSessionQueue :: DeleteGameSessionQueueResponse -> TestTree
responseDeleteGameSessionQueue = res
    "DeleteGameSessionQueueResponse"
    "fixture/DeleteGameSessionQueueResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteGameSessionQueue)

responseDeleteVPCPeeringConnection :: DeleteVPCPeeringConnectionResponse -> TestTree
responseDeleteVPCPeeringConnection = res
    "DeleteVPCPeeringConnectionResponse"
    "fixture/DeleteVPCPeeringConnectionResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteVPCPeeringConnection)

responseStartFleetActions :: StartFleetActionsResponse -> TestTree
responseStartFleetActions = res
    "StartFleetActionsResponse"
    "fixture/StartFleetActionsResponse.proto"
    gameLift
    (Proxy :: Proxy StartFleetActions)

responseGetInstanceAccess :: GetInstanceAccessResponse -> TestTree
responseGetInstanceAccess = res
    "GetInstanceAccessResponse"
    "fixture/GetInstanceAccessResponse.proto"
    gameLift
    (Proxy :: Proxy GetInstanceAccess)

responseDescribeScalingPolicies :: DescribeScalingPoliciesResponse -> TestTree
responseDescribeScalingPolicies = res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeScalingPolicies)

responseDescribeMatchmakingRuleSets :: DescribeMatchmakingRuleSetsResponse -> TestTree
responseDescribeMatchmakingRuleSets = res
    "DescribeMatchmakingRuleSetsResponse"
    "fixture/DescribeMatchmakingRuleSetsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeMatchmakingRuleSets)

responseDescribeGameSessions :: DescribeGameSessionsResponse -> TestTree
responseDescribeGameSessions = res
    "DescribeGameSessionsResponse"
    "fixture/DescribeGameSessionsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameSessions)

responseStartGameSessionPlacement :: StartGameSessionPlacementResponse -> TestTree
responseStartGameSessionPlacement = res
    "StartGameSessionPlacementResponse"
    "fixture/StartGameSessionPlacementResponse.proto"
    gameLift
    (Proxy :: Proxy StartGameSessionPlacement)

responseDescribeFleetUtilization :: DescribeFleetUtilizationResponse -> TestTree
responseDescribeFleetUtilization = res
    "DescribeFleetUtilizationResponse"
    "fixture/DescribeFleetUtilizationResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetUtilization)

responseDescribeRuntimeConfiguration :: DescribeRuntimeConfigurationResponse -> TestTree
responseDescribeRuntimeConfiguration = res
    "DescribeRuntimeConfigurationResponse"
    "fixture/DescribeRuntimeConfigurationResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeRuntimeConfiguration)

responseGetGameSessionLogURL :: GetGameSessionLogURLResponse -> TestTree
responseGetGameSessionLogURL = res
    "GetGameSessionLogURLResponse"
    "fixture/GetGameSessionLogURLResponse.proto"
    gameLift
    (Proxy :: Proxy GetGameSessionLogURL)

responseDescribeFleetAttributes :: DescribeFleetAttributesResponse -> TestTree
responseDescribeFleetAttributes = res
    "DescribeFleetAttributesResponse"
    "fixture/DescribeFleetAttributesResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetAttributes)

responseDescribeGameSessionPlacement :: DescribeGameSessionPlacementResponse -> TestTree
responseDescribeGameSessionPlacement = res
    "DescribeGameSessionPlacementResponse"
    "fixture/DescribeGameSessionPlacementResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameSessionPlacement)

responseDescribeFleetEvents :: DescribeFleetEventsResponse -> TestTree
responseDescribeFleetEvents = res
    "DescribeFleetEventsResponse"
    "fixture/DescribeFleetEventsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetEvents)

responseStartMatchmaking :: StartMatchmakingResponse -> TestTree
responseStartMatchmaking = res
    "StartMatchmakingResponse"
    "fixture/StartMatchmakingResponse.proto"
    gameLift
    (Proxy :: Proxy StartMatchmaking)

responseCreateMatchmakingRuleSet :: CreateMatchmakingRuleSetResponse -> TestTree
responseCreateMatchmakingRuleSet = res
    "CreateMatchmakingRuleSetResponse"
    "fixture/CreateMatchmakingRuleSetResponse.proto"
    gameLift
    (Proxy :: Proxy CreateMatchmakingRuleSet)

responseDescribeFleetCapacity :: DescribeFleetCapacityResponse -> TestTree
responseDescribeFleetCapacity = res
    "DescribeFleetCapacityResponse"
    "fixture/DescribeFleetCapacityResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetCapacity)

responseDeleteBuild :: DeleteBuildResponse -> TestTree
responseDeleteBuild = res
    "DeleteBuildResponse"
    "fixture/DeleteBuildResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteBuild)

responseUpdateBuild :: UpdateBuildResponse -> TestTree
responseUpdateBuild = res
    "UpdateBuildResponse"
    "fixture/UpdateBuildResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateBuild)

responseListFleets :: ListFleetsResponse -> TestTree
responseListFleets = res
    "ListFleetsResponse"
    "fixture/ListFleetsResponse.proto"
    gameLift
    (Proxy :: Proxy ListFleets)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias = res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteAlias)

responseUpdateAlias :: UpdateAliasResponse -> TestTree
responseUpdateAlias = res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateAlias)

responseStartMatchBackfill :: StartMatchBackfillResponse -> TestTree
responseStartMatchBackfill = res
    "StartMatchBackfillResponse"
    "fixture/StartMatchBackfillResponse.proto"
    gameLift
    (Proxy :: Proxy StartMatchBackfill)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances = res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeInstances)

responseDescribeGameSessionDetails :: DescribeGameSessionDetailsResponse -> TestTree
responseDescribeGameSessionDetails = res
    "DescribeGameSessionDetailsResponse"
    "fixture/DescribeGameSessionDetailsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameSessionDetails)

responseDescribeFleetPortSettings :: DescribeFleetPortSettingsResponse -> TestTree
responseDescribeFleetPortSettings = res
    "DescribeFleetPortSettingsResponse"
    "fixture/DescribeFleetPortSettingsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetPortSettings)

responseDescribeGameSessionQueues :: DescribeGameSessionQueuesResponse -> TestTree
responseDescribeGameSessionQueues = res
    "DescribeGameSessionQueuesResponse"
    "fixture/DescribeGameSessionQueuesResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameSessionQueues)

responseDescribeVPCPeeringConnections :: DescribeVPCPeeringConnectionsResponse -> TestTree
responseDescribeVPCPeeringConnections = res
    "DescribeVPCPeeringConnectionsResponse"
    "fixture/DescribeVPCPeeringConnectionsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeVPCPeeringConnections)

responseCreatePlayerSessions :: CreatePlayerSessionsResponse -> TestTree
responseCreatePlayerSessions = res
    "CreatePlayerSessionsResponse"
    "fixture/CreatePlayerSessionsResponse.proto"
    gameLift
    (Proxy :: Proxy CreatePlayerSessions)

responseDescribeMatchmakingConfigurations :: DescribeMatchmakingConfigurationsResponse -> TestTree
responseDescribeMatchmakingConfigurations = res
    "DescribeMatchmakingConfigurationsResponse"
    "fixture/DescribeMatchmakingConfigurationsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeMatchmakingConfigurations)

responseDescribeVPCPeeringAuthorizations :: DescribeVPCPeeringAuthorizationsResponse -> TestTree
responseDescribeVPCPeeringAuthorizations = res
    "DescribeVPCPeeringAuthorizationsResponse"
    "fixture/DescribeVPCPeeringAuthorizationsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeVPCPeeringAuthorizations)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet = res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    gameLift
    (Proxy :: Proxy CreateFleet)

responseDeleteMatchmakingConfiguration :: DeleteMatchmakingConfigurationResponse -> TestTree
responseDeleteMatchmakingConfiguration = res
    "DeleteMatchmakingConfigurationResponse"
    "fixture/DeleteMatchmakingConfigurationResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteMatchmakingConfiguration)

responseUpdateMatchmakingConfiguration :: UpdateMatchmakingConfigurationResponse -> TestTree
responseUpdateMatchmakingConfiguration = res
    "UpdateMatchmakingConfigurationResponse"
    "fixture/UpdateMatchmakingConfigurationResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateMatchmakingConfiguration)

responseDeleteVPCPeeringAuthorization :: DeleteVPCPeeringAuthorizationResponse -> TestTree
responseDeleteVPCPeeringAuthorization = res
    "DeleteVPCPeeringAuthorizationResponse"
    "fixture/DeleteVPCPeeringAuthorizationResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteVPCPeeringAuthorization)

responseUpdateFleetAttributes :: UpdateFleetAttributesResponse -> TestTree
responseUpdateFleetAttributes = res
    "UpdateFleetAttributesResponse"
    "fixture/UpdateFleetAttributesResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateFleetAttributes)

responseCreateMatchmakingConfiguration :: CreateMatchmakingConfigurationResponse -> TestTree
responseCreateMatchmakingConfiguration = res
    "CreateMatchmakingConfigurationResponse"
    "fixture/CreateMatchmakingConfigurationResponse.proto"
    gameLift
    (Proxy :: Proxy CreateMatchmakingConfiguration)

responseDescribePlayerSessions :: DescribePlayerSessionsResponse -> TestTree
responseDescribePlayerSessions = res
    "DescribePlayerSessionsResponse"
    "fixture/DescribePlayerSessionsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribePlayerSessions)

responseStopFleetActions :: StopFleetActionsResponse -> TestTree
responseStopFleetActions = res
    "StopFleetActionsResponse"
    "fixture/StopFleetActionsResponse.proto"
    gameLift
    (Proxy :: Proxy StopFleetActions)

responseDescribeBuild :: DescribeBuildResponse -> TestTree
responseDescribeBuild = res
    "DescribeBuildResponse"
    "fixture/DescribeBuildResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeBuild)

responseUpdateFleetPortSettings :: UpdateFleetPortSettingsResponse -> TestTree
responseUpdateFleetPortSettings = res
    "UpdateFleetPortSettingsResponse"
    "fixture/UpdateFleetPortSettingsResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateFleetPortSettings)

responseUpdateFleetCapacity :: UpdateFleetCapacityResponse -> TestTree
responseUpdateFleetCapacity = res
    "UpdateFleetCapacityResponse"
    "fixture/UpdateFleetCapacityResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateFleetCapacity)

responseAcceptMatch :: AcceptMatchResponse -> TestTree
responseAcceptMatch = res
    "AcceptMatchResponse"
    "fixture/AcceptMatchResponse.proto"
    gameLift
    (Proxy :: Proxy AcceptMatch)

responseDescribeAlias :: DescribeAliasResponse -> TestTree
responseDescribeAlias = res
    "DescribeAliasResponse"
    "fixture/DescribeAliasResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeAlias)

responseValidateMatchmakingRuleSet :: ValidateMatchmakingRuleSetResponse -> TestTree
responseValidateMatchmakingRuleSet = res
    "ValidateMatchmakingRuleSetResponse"
    "fixture/ValidateMatchmakingRuleSetResponse.proto"
    gameLift
    (Proxy :: Proxy ValidateMatchmakingRuleSet)

responseDescribeEC2InstanceLimits :: DescribeEC2InstanceLimitsResponse -> TestTree
responseDescribeEC2InstanceLimits = res
    "DescribeEC2InstanceLimitsResponse"
    "fixture/DescribeEC2InstanceLimitsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeEC2InstanceLimits)

responseStopGameSessionPlacement :: StopGameSessionPlacementResponse -> TestTree
responseStopGameSessionPlacement = res
    "StopGameSessionPlacementResponse"
    "fixture/StopGameSessionPlacementResponse.proto"
    gameLift
    (Proxy :: Proxy StopGameSessionPlacement)

responseUpdateGameSession :: UpdateGameSessionResponse -> TestTree
responseUpdateGameSession = res
    "UpdateGameSessionResponse"
    "fixture/UpdateGameSessionResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateGameSession)

responseDescribeMatchmaking :: DescribeMatchmakingResponse -> TestTree
responseDescribeMatchmaking = res
    "DescribeMatchmakingResponse"
    "fixture/DescribeMatchmakingResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeMatchmaking)

responseCreatePlayerSession :: CreatePlayerSessionResponse -> TestTree
responseCreatePlayerSession = res
    "CreatePlayerSessionResponse"
    "fixture/CreatePlayerSessionResponse.proto"
    gameLift
    (Proxy :: Proxy CreatePlayerSession)
