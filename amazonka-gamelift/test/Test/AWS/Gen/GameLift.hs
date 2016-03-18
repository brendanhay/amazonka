{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.GameLift
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
--         [ testCreateGameSession $
--             createGameSession
--
--         , testDeleteScalingPolicy $
--             deleteScalingPolicy
--
--         , testPutScalingPolicy $
--             putScalingPolicy
--
--         , testListBuilds $
--             listBuilds
--
--         , testDeleteFleet $
--             deleteFleet
--
--         , testCreateBuild $
--             createBuild
--
--         , testRequestUploadCredentials $
--             requestUploadCredentials
--
--         , testCreateAlias $
--             createAlias
--
--         , testResolveAlias $
--             resolveAlias
--
--         , testListAliases $
--             listAliases
--
--         , testDescribeScalingPolicies $
--             describeScalingPolicies
--
--         , testDescribeGameSessions $
--             describeGameSessions
--
--         , testDescribeFleetUtilization $
--             describeFleetUtilization
--
--         , testGetGameSessionLogURL $
--             getGameSessionLogURL
--
--         , testDescribeFleetAttributes $
--             describeFleetAttributes
--
--         , testDescribeFleetEvents $
--             describeFleetEvents
--
--         , testDescribeFleetCapacity $
--             describeFleetCapacity
--
--         , testDeleteBuild $
--             deleteBuild
--
--         , testUpdateBuild $
--             updateBuild
--
--         , testListFleets $
--             listFleets
--
--         , testDeleteAlias $
--             deleteAlias
--
--         , testUpdateAlias $
--             updateAlias
--
--         , testDescribeGameSessionDetails $
--             describeGameSessionDetails
--
--         , testDescribeFleetPortSettings $
--             describeFleetPortSettings
--
--         , testCreatePlayerSessions $
--             createPlayerSessions
--
--         , testCreateFleet $
--             createFleet
--
--         , testUpdateFleetAttributes $
--             updateFleetAttributes
--
--         , testDescribePlayerSessions $
--             describePlayerSessions
--
--         , testDescribeBuild $
--             describeBuild
--
--         , testUpdateFleetPortSettings $
--             updateFleetPortSettings
--
--         , testUpdateFleetCapacity $
--             updateFleetCapacity
--
--         , testDescribeAlias $
--             describeAlias
--
--         , testDescribeEC2InstanceLimits $
--             describeEC2InstanceLimits
--
--         , testUpdateGameSession $
--             updateGameSession
--
--         , testCreatePlayerSession $
--             createPlayerSession
--
--           ]

--     , testGroup "response"
--         [ testCreateGameSessionResponse $
--             createGameSessionResponse
--
--         , testDeleteScalingPolicyResponse $
--             deleteScalingPolicyResponse
--
--         , testPutScalingPolicyResponse $
--             putScalingPolicyResponse
--
--         , testListBuildsResponse $
--             listBuildsResponse
--
--         , testDeleteFleetResponse $
--             deleteFleetResponse
--
--         , testCreateBuildResponse $
--             createBuildResponse
--
--         , testRequestUploadCredentialsResponse $
--             requestUploadCredentialsResponse
--
--         , testCreateAliasResponse $
--             createAliasResponse
--
--         , testResolveAliasResponse $
--             resolveAliasResponse
--
--         , testListAliasesResponse $
--             listAliasesResponse
--
--         , testDescribeScalingPoliciesResponse $
--             describeScalingPoliciesResponse
--
--         , testDescribeGameSessionsResponse $
--             describeGameSessionsResponse
--
--         , testDescribeFleetUtilizationResponse $
--             describeFleetUtilizationResponse
--
--         , testGetGameSessionLogURLResponse $
--             getGameSessionLogURLResponse
--
--         , testDescribeFleetAttributesResponse $
--             describeFleetAttributesResponse
--
--         , testDescribeFleetEventsResponse $
--             describeFleetEventsResponse
--
--         , testDescribeFleetCapacityResponse $
--             describeFleetCapacityResponse
--
--         , testDeleteBuildResponse $
--             deleteBuildResponse
--
--         , testUpdateBuildResponse $
--             updateBuildResponse
--
--         , testListFleetsResponse $
--             listFleetsResponse
--
--         , testDeleteAliasResponse $
--             deleteAliasResponse
--
--         , testUpdateAliasResponse $
--             updateAliasResponse
--
--         , testDescribeGameSessionDetailsResponse $
--             describeGameSessionDetailsResponse
--
--         , testDescribeFleetPortSettingsResponse $
--             describeFleetPortSettingsResponse
--
--         , testCreatePlayerSessionsResponse $
--             createPlayerSessionsResponse
--
--         , testCreateFleetResponse $
--             createFleetResponse
--
--         , testUpdateFleetAttributesResponse $
--             updateFleetAttributesResponse
--
--         , testDescribePlayerSessionsResponse $
--             describePlayerSessionsResponse
--
--         , testDescribeBuildResponse $
--             describeBuildResponse
--
--         , testUpdateFleetPortSettingsResponse $
--             updateFleetPortSettingsResponse
--
--         , testUpdateFleetCapacityResponse $
--             updateFleetCapacityResponse
--
--         , testDescribeAliasResponse $
--             describeAliasResponse
--
--         , testDescribeEC2InstanceLimitsResponse $
--             describeEC2InstanceLimitsResponse
--
--         , testUpdateGameSessionResponse $
--             updateGameSessionResponse
--
--         , testCreatePlayerSessionResponse $
--             createPlayerSessionResponse
--
--           ]
--     ]

-- Requests

testCreateGameSession :: CreateGameSession -> TestTree
testCreateGameSession = req
    "CreateGameSession"
    "fixture/CreateGameSession.yaml"

testDeleteScalingPolicy :: DeleteScalingPolicy -> TestTree
testDeleteScalingPolicy = req
    "DeleteScalingPolicy"
    "fixture/DeleteScalingPolicy.yaml"

testPutScalingPolicy :: PutScalingPolicy -> TestTree
testPutScalingPolicy = req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

testListBuilds :: ListBuilds -> TestTree
testListBuilds = req
    "ListBuilds"
    "fixture/ListBuilds.yaml"

testDeleteFleet :: DeleteFleet -> TestTree
testDeleteFleet = req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

testCreateBuild :: CreateBuild -> TestTree
testCreateBuild = req
    "CreateBuild"
    "fixture/CreateBuild.yaml"

testRequestUploadCredentials :: RequestUploadCredentials -> TestTree
testRequestUploadCredentials = req
    "RequestUploadCredentials"
    "fixture/RequestUploadCredentials.yaml"

testCreateAlias :: CreateAlias -> TestTree
testCreateAlias = req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

testResolveAlias :: ResolveAlias -> TestTree
testResolveAlias = req
    "ResolveAlias"
    "fixture/ResolveAlias.yaml"

testListAliases :: ListAliases -> TestTree
testListAliases = req
    "ListAliases"
    "fixture/ListAliases.yaml"

testDescribeScalingPolicies :: DescribeScalingPolicies -> TestTree
testDescribeScalingPolicies = req
    "DescribeScalingPolicies"
    "fixture/DescribeScalingPolicies.yaml"

testDescribeGameSessions :: DescribeGameSessions -> TestTree
testDescribeGameSessions = req
    "DescribeGameSessions"
    "fixture/DescribeGameSessions.yaml"

testDescribeFleetUtilization :: DescribeFleetUtilization -> TestTree
testDescribeFleetUtilization = req
    "DescribeFleetUtilization"
    "fixture/DescribeFleetUtilization.yaml"

testGetGameSessionLogURL :: GetGameSessionLogURL -> TestTree
testGetGameSessionLogURL = req
    "GetGameSessionLogURL"
    "fixture/GetGameSessionLogURL.yaml"

testDescribeFleetAttributes :: DescribeFleetAttributes -> TestTree
testDescribeFleetAttributes = req
    "DescribeFleetAttributes"
    "fixture/DescribeFleetAttributes.yaml"

testDescribeFleetEvents :: DescribeFleetEvents -> TestTree
testDescribeFleetEvents = req
    "DescribeFleetEvents"
    "fixture/DescribeFleetEvents.yaml"

testDescribeFleetCapacity :: DescribeFleetCapacity -> TestTree
testDescribeFleetCapacity = req
    "DescribeFleetCapacity"
    "fixture/DescribeFleetCapacity.yaml"

testDeleteBuild :: DeleteBuild -> TestTree
testDeleteBuild = req
    "DeleteBuild"
    "fixture/DeleteBuild.yaml"

testUpdateBuild :: UpdateBuild -> TestTree
testUpdateBuild = req
    "UpdateBuild"
    "fixture/UpdateBuild.yaml"

testListFleets :: ListFleets -> TestTree
testListFleets = req
    "ListFleets"
    "fixture/ListFleets.yaml"

testDeleteAlias :: DeleteAlias -> TestTree
testDeleteAlias = req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

testUpdateAlias :: UpdateAlias -> TestTree
testUpdateAlias = req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

testDescribeGameSessionDetails :: DescribeGameSessionDetails -> TestTree
testDescribeGameSessionDetails = req
    "DescribeGameSessionDetails"
    "fixture/DescribeGameSessionDetails.yaml"

testDescribeFleetPortSettings :: DescribeFleetPortSettings -> TestTree
testDescribeFleetPortSettings = req
    "DescribeFleetPortSettings"
    "fixture/DescribeFleetPortSettings.yaml"

testCreatePlayerSessions :: CreatePlayerSessions -> TestTree
testCreatePlayerSessions = req
    "CreatePlayerSessions"
    "fixture/CreatePlayerSessions.yaml"

testCreateFleet :: CreateFleet -> TestTree
testCreateFleet = req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

testUpdateFleetAttributes :: UpdateFleetAttributes -> TestTree
testUpdateFleetAttributes = req
    "UpdateFleetAttributes"
    "fixture/UpdateFleetAttributes.yaml"

testDescribePlayerSessions :: DescribePlayerSessions -> TestTree
testDescribePlayerSessions = req
    "DescribePlayerSessions"
    "fixture/DescribePlayerSessions.yaml"

testDescribeBuild :: DescribeBuild -> TestTree
testDescribeBuild = req
    "DescribeBuild"
    "fixture/DescribeBuild.yaml"

testUpdateFleetPortSettings :: UpdateFleetPortSettings -> TestTree
testUpdateFleetPortSettings = req
    "UpdateFleetPortSettings"
    "fixture/UpdateFleetPortSettings.yaml"

testUpdateFleetCapacity :: UpdateFleetCapacity -> TestTree
testUpdateFleetCapacity = req
    "UpdateFleetCapacity"
    "fixture/UpdateFleetCapacity.yaml"

testDescribeAlias :: DescribeAlias -> TestTree
testDescribeAlias = req
    "DescribeAlias"
    "fixture/DescribeAlias.yaml"

testDescribeEC2InstanceLimits :: DescribeEC2InstanceLimits -> TestTree
testDescribeEC2InstanceLimits = req
    "DescribeEC2InstanceLimits"
    "fixture/DescribeEC2InstanceLimits.yaml"

testUpdateGameSession :: UpdateGameSession -> TestTree
testUpdateGameSession = req
    "UpdateGameSession"
    "fixture/UpdateGameSession.yaml"

testCreatePlayerSession :: CreatePlayerSession -> TestTree
testCreatePlayerSession = req
    "CreatePlayerSession"
    "fixture/CreatePlayerSession.yaml"

-- Responses

testCreateGameSessionResponse :: CreateGameSessionResponse -> TestTree
testCreateGameSessionResponse = res
    "CreateGameSessionResponse"
    "fixture/CreateGameSessionResponse.proto"
    gameLift
    (Proxy :: Proxy CreateGameSession)

testDeleteScalingPolicyResponse :: DeleteScalingPolicyResponse -> TestTree
testDeleteScalingPolicyResponse = res
    "DeleteScalingPolicyResponse"
    "fixture/DeleteScalingPolicyResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteScalingPolicy)

testPutScalingPolicyResponse :: PutScalingPolicyResponse -> TestTree
testPutScalingPolicyResponse = res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    gameLift
    (Proxy :: Proxy PutScalingPolicy)

testListBuildsResponse :: ListBuildsResponse -> TestTree
testListBuildsResponse = res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    gameLift
    (Proxy :: Proxy ListBuilds)

testDeleteFleetResponse :: DeleteFleetResponse -> TestTree
testDeleteFleetResponse = res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteFleet)

testCreateBuildResponse :: CreateBuildResponse -> TestTree
testCreateBuildResponse = res
    "CreateBuildResponse"
    "fixture/CreateBuildResponse.proto"
    gameLift
    (Proxy :: Proxy CreateBuild)

testRequestUploadCredentialsResponse :: RequestUploadCredentialsResponse -> TestTree
testRequestUploadCredentialsResponse = res
    "RequestUploadCredentialsResponse"
    "fixture/RequestUploadCredentialsResponse.proto"
    gameLift
    (Proxy :: Proxy RequestUploadCredentials)

testCreateAliasResponse :: CreateAliasResponse -> TestTree
testCreateAliasResponse = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    gameLift
    (Proxy :: Proxy CreateAlias)

testResolveAliasResponse :: ResolveAliasResponse -> TestTree
testResolveAliasResponse = res
    "ResolveAliasResponse"
    "fixture/ResolveAliasResponse.proto"
    gameLift
    (Proxy :: Proxy ResolveAlias)

testListAliasesResponse :: ListAliasesResponse -> TestTree
testListAliasesResponse = res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    gameLift
    (Proxy :: Proxy ListAliases)

testDescribeScalingPoliciesResponse :: DescribeScalingPoliciesResponse -> TestTree
testDescribeScalingPoliciesResponse = res
    "DescribeScalingPoliciesResponse"
    "fixture/DescribeScalingPoliciesResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeScalingPolicies)

testDescribeGameSessionsResponse :: DescribeGameSessionsResponse -> TestTree
testDescribeGameSessionsResponse = res
    "DescribeGameSessionsResponse"
    "fixture/DescribeGameSessionsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameSessions)

testDescribeFleetUtilizationResponse :: DescribeFleetUtilizationResponse -> TestTree
testDescribeFleetUtilizationResponse = res
    "DescribeFleetUtilizationResponse"
    "fixture/DescribeFleetUtilizationResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetUtilization)

testGetGameSessionLogURLResponse :: GetGameSessionLogURLResponse -> TestTree
testGetGameSessionLogURLResponse = res
    "GetGameSessionLogURLResponse"
    "fixture/GetGameSessionLogURLResponse.proto"
    gameLift
    (Proxy :: Proxy GetGameSessionLogURL)

testDescribeFleetAttributesResponse :: DescribeFleetAttributesResponse -> TestTree
testDescribeFleetAttributesResponse = res
    "DescribeFleetAttributesResponse"
    "fixture/DescribeFleetAttributesResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetAttributes)

testDescribeFleetEventsResponse :: DescribeFleetEventsResponse -> TestTree
testDescribeFleetEventsResponse = res
    "DescribeFleetEventsResponse"
    "fixture/DescribeFleetEventsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetEvents)

testDescribeFleetCapacityResponse :: DescribeFleetCapacityResponse -> TestTree
testDescribeFleetCapacityResponse = res
    "DescribeFleetCapacityResponse"
    "fixture/DescribeFleetCapacityResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetCapacity)

testDeleteBuildResponse :: DeleteBuildResponse -> TestTree
testDeleteBuildResponse = res
    "DeleteBuildResponse"
    "fixture/DeleteBuildResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteBuild)

testUpdateBuildResponse :: UpdateBuildResponse -> TestTree
testUpdateBuildResponse = res
    "UpdateBuildResponse"
    "fixture/UpdateBuildResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateBuild)

testListFleetsResponse :: ListFleetsResponse -> TestTree
testListFleetsResponse = res
    "ListFleetsResponse"
    "fixture/ListFleetsResponse.proto"
    gameLift
    (Proxy :: Proxy ListFleets)

testDeleteAliasResponse :: DeleteAliasResponse -> TestTree
testDeleteAliasResponse = res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    gameLift
    (Proxy :: Proxy DeleteAlias)

testUpdateAliasResponse :: UpdateAliasResponse -> TestTree
testUpdateAliasResponse = res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateAlias)

testDescribeGameSessionDetailsResponse :: DescribeGameSessionDetailsResponse -> TestTree
testDescribeGameSessionDetailsResponse = res
    "DescribeGameSessionDetailsResponse"
    "fixture/DescribeGameSessionDetailsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeGameSessionDetails)

testDescribeFleetPortSettingsResponse :: DescribeFleetPortSettingsResponse -> TestTree
testDescribeFleetPortSettingsResponse = res
    "DescribeFleetPortSettingsResponse"
    "fixture/DescribeFleetPortSettingsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeFleetPortSettings)

testCreatePlayerSessionsResponse :: CreatePlayerSessionsResponse -> TestTree
testCreatePlayerSessionsResponse = res
    "CreatePlayerSessionsResponse"
    "fixture/CreatePlayerSessionsResponse.proto"
    gameLift
    (Proxy :: Proxy CreatePlayerSessions)

testCreateFleetResponse :: CreateFleetResponse -> TestTree
testCreateFleetResponse = res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    gameLift
    (Proxy :: Proxy CreateFleet)

testUpdateFleetAttributesResponse :: UpdateFleetAttributesResponse -> TestTree
testUpdateFleetAttributesResponse = res
    "UpdateFleetAttributesResponse"
    "fixture/UpdateFleetAttributesResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateFleetAttributes)

testDescribePlayerSessionsResponse :: DescribePlayerSessionsResponse -> TestTree
testDescribePlayerSessionsResponse = res
    "DescribePlayerSessionsResponse"
    "fixture/DescribePlayerSessionsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribePlayerSessions)

testDescribeBuildResponse :: DescribeBuildResponse -> TestTree
testDescribeBuildResponse = res
    "DescribeBuildResponse"
    "fixture/DescribeBuildResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeBuild)

testUpdateFleetPortSettingsResponse :: UpdateFleetPortSettingsResponse -> TestTree
testUpdateFleetPortSettingsResponse = res
    "UpdateFleetPortSettingsResponse"
    "fixture/UpdateFleetPortSettingsResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateFleetPortSettings)

testUpdateFleetCapacityResponse :: UpdateFleetCapacityResponse -> TestTree
testUpdateFleetCapacityResponse = res
    "UpdateFleetCapacityResponse"
    "fixture/UpdateFleetCapacityResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateFleetCapacity)

testDescribeAliasResponse :: DescribeAliasResponse -> TestTree
testDescribeAliasResponse = res
    "DescribeAliasResponse"
    "fixture/DescribeAliasResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeAlias)

testDescribeEC2InstanceLimitsResponse :: DescribeEC2InstanceLimitsResponse -> TestTree
testDescribeEC2InstanceLimitsResponse = res
    "DescribeEC2InstanceLimitsResponse"
    "fixture/DescribeEC2InstanceLimitsResponse.proto"
    gameLift
    (Proxy :: Proxy DescribeEC2InstanceLimits)

testUpdateGameSessionResponse :: UpdateGameSessionResponse -> TestTree
testUpdateGameSessionResponse = res
    "UpdateGameSessionResponse"
    "fixture/UpdateGameSessionResponse.proto"
    gameLift
    (Proxy :: Proxy UpdateGameSession)

testCreatePlayerSessionResponse :: CreatePlayerSessionResponse -> TestTree
testCreatePlayerSessionResponse = res
    "CreatePlayerSessionResponse"
    "fixture/CreatePlayerSessionResponse.proto"
    gameLift
    (Proxy :: Proxy CreatePlayerSession)
