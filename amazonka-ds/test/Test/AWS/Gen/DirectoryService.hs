{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DirectoryService
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DirectoryService where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.DirectoryService
import Test.AWS.DirectoryService.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetSnapshotLimits $
--             getSnapshotLimits
--
--         , testRegisterEventTopic $
--             registerEventTopic
--
--         , testConnectDirectory $
--             connectDirectory
--
--         , testCreateAlias $
--             createAlias
--
--         , testDescribeDirectories $
--             describeDirectories
--
--         , testDescribeTrusts $
--             describeTrusts
--
--         , testDeleteTrust $
--             deleteTrust
--
--         , testCreateMicrosoftAD $
--             createMicrosoftAD
--
--         , testDeregisterEventTopic $
--             deregisterEventTopic
--
--         , testCreateDirectory $
--             createDirectory
--
--         , testDescribeEventTopics $
--             describeEventTopics
--
--         , testEnableSSO $
--             enableSSO
--
--         , testEnableRadius $
--             enableRadius
--
--         , testDisableRadius $
--             disableRadius
--
--         , testRestoreFromSnapshot $
--             restoreFromSnapshot
--
--         , testDescribeSnapshots $
--             describeSnapshots
--
--         , testDeleteSnapshot $
--             deleteSnapshot
--
--         , testCreateTrust $
--             createTrust
--
--         , testDeleteDirectory $
--             deleteDirectory
--
--         , testCreateSnapshot $
--             createSnapshot
--
--         , testCreateComputer $
--             createComputer
--
--         , testDisableSSO $
--             disableSSO
--
--         , testVerifyTrust $
--             verifyTrust
--
--         , testGetDirectoryLimits $
--             getDirectoryLimits
--
--         , testUpdateRadius $
--             updateRadius
--
--           ]

--     , testGroup "response"
--         [ testGetSnapshotLimitsResponse $
--             getSnapshotLimitsResponse
--
--         , testRegisterEventTopicResponse $
--             registerEventTopicResponse
--
--         , testConnectDirectoryResponse $
--             connectDirectoryResponse
--
--         , testCreateAliasResponse $
--             createAliasResponse
--
--         , testDescribeDirectoriesResponse $
--             describeDirectoriesResponse
--
--         , testDescribeTrustsResponse $
--             describeTrustsResponse
--
--         , testDeleteTrustResponse $
--             deleteTrustResponse
--
--         , testCreateMicrosoftADResponse $
--             createMicrosoftADResponse
--
--         , testDeregisterEventTopicResponse $
--             deregisterEventTopicResponse
--
--         , testCreateDirectoryResponse $
--             createDirectoryResponse
--
--         , testDescribeEventTopicsResponse $
--             describeEventTopicsResponse
--
--         , testEnableSSOResponse $
--             enableSSOResponse
--
--         , testEnableRadiusResponse $
--             enableRadiusResponse
--
--         , testDisableRadiusResponse $
--             disableRadiusResponse
--
--         , testRestoreFromSnapshotResponse $
--             restoreFromSnapshotResponse
--
--         , testDescribeSnapshotsResponse $
--             describeSnapshotsResponse
--
--         , testDeleteSnapshotResponse $
--             deleteSnapshotResponse
--
--         , testCreateTrustResponse $
--             createTrustResponse
--
--         , testDeleteDirectoryResponse $
--             deleteDirectoryResponse
--
--         , testCreateSnapshotResponse $
--             createSnapshotResponse
--
--         , testCreateComputerResponse $
--             createComputerResponse
--
--         , testDisableSSOResponse $
--             disableSSOResponse
--
--         , testVerifyTrustResponse $
--             verifyTrustResponse
--
--         , testGetDirectoryLimitsResponse $
--             getDirectoryLimitsResponse
--
--         , testUpdateRadiusResponse $
--             updateRadiusResponse
--
--           ]
--     ]

-- Requests

testGetSnapshotLimits :: GetSnapshotLimits -> TestTree
testGetSnapshotLimits = req
    "GetSnapshotLimits"
    "fixture/GetSnapshotLimits.yaml"

testRegisterEventTopic :: RegisterEventTopic -> TestTree
testRegisterEventTopic = req
    "RegisterEventTopic"
    "fixture/RegisterEventTopic.yaml"

testConnectDirectory :: ConnectDirectory -> TestTree
testConnectDirectory = req
    "ConnectDirectory"
    "fixture/ConnectDirectory.yaml"

testCreateAlias :: CreateAlias -> TestTree
testCreateAlias = req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

testDescribeDirectories :: DescribeDirectories -> TestTree
testDescribeDirectories = req
    "DescribeDirectories"
    "fixture/DescribeDirectories.yaml"

testDescribeTrusts :: DescribeTrusts -> TestTree
testDescribeTrusts = req
    "DescribeTrusts"
    "fixture/DescribeTrusts.yaml"

testDeleteTrust :: DeleteTrust -> TestTree
testDeleteTrust = req
    "DeleteTrust"
    "fixture/DeleteTrust.yaml"

testCreateMicrosoftAD :: CreateMicrosoftAD -> TestTree
testCreateMicrosoftAD = req
    "CreateMicrosoftAD"
    "fixture/CreateMicrosoftAD.yaml"

testDeregisterEventTopic :: DeregisterEventTopic -> TestTree
testDeregisterEventTopic = req
    "DeregisterEventTopic"
    "fixture/DeregisterEventTopic.yaml"

testCreateDirectory :: CreateDirectory -> TestTree
testCreateDirectory = req
    "CreateDirectory"
    "fixture/CreateDirectory.yaml"

testDescribeEventTopics :: DescribeEventTopics -> TestTree
testDescribeEventTopics = req
    "DescribeEventTopics"
    "fixture/DescribeEventTopics.yaml"

testEnableSSO :: EnableSSO -> TestTree
testEnableSSO = req
    "EnableSSO"
    "fixture/EnableSSO.yaml"

testEnableRadius :: EnableRadius -> TestTree
testEnableRadius = req
    "EnableRadius"
    "fixture/EnableRadius.yaml"

testDisableRadius :: DisableRadius -> TestTree
testDisableRadius = req
    "DisableRadius"
    "fixture/DisableRadius.yaml"

testRestoreFromSnapshot :: RestoreFromSnapshot -> TestTree
testRestoreFromSnapshot = req
    "RestoreFromSnapshot"
    "fixture/RestoreFromSnapshot.yaml"

testDescribeSnapshots :: DescribeSnapshots -> TestTree
testDescribeSnapshots = req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

testDeleteSnapshot :: DeleteSnapshot -> TestTree
testDeleteSnapshot = req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

testCreateTrust :: CreateTrust -> TestTree
testCreateTrust = req
    "CreateTrust"
    "fixture/CreateTrust.yaml"

testDeleteDirectory :: DeleteDirectory -> TestTree
testDeleteDirectory = req
    "DeleteDirectory"
    "fixture/DeleteDirectory.yaml"

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

testCreateComputer :: CreateComputer -> TestTree
testCreateComputer = req
    "CreateComputer"
    "fixture/CreateComputer.yaml"

testDisableSSO :: DisableSSO -> TestTree
testDisableSSO = req
    "DisableSSO"
    "fixture/DisableSSO.yaml"

testVerifyTrust :: VerifyTrust -> TestTree
testVerifyTrust = req
    "VerifyTrust"
    "fixture/VerifyTrust.yaml"

testGetDirectoryLimits :: GetDirectoryLimits -> TestTree
testGetDirectoryLimits = req
    "GetDirectoryLimits"
    "fixture/GetDirectoryLimits.yaml"

testUpdateRadius :: UpdateRadius -> TestTree
testUpdateRadius = req
    "UpdateRadius"
    "fixture/UpdateRadius.yaml"

-- Responses

testGetSnapshotLimitsResponse :: GetSnapshotLimitsResponse -> TestTree
testGetSnapshotLimitsResponse = res
    "GetSnapshotLimitsResponse"
    "fixture/GetSnapshotLimitsResponse.proto"
    directoryService
    (Proxy :: Proxy GetSnapshotLimits)

testRegisterEventTopicResponse :: RegisterEventTopicResponse -> TestTree
testRegisterEventTopicResponse = res
    "RegisterEventTopicResponse"
    "fixture/RegisterEventTopicResponse.proto"
    directoryService
    (Proxy :: Proxy RegisterEventTopic)

testConnectDirectoryResponse :: ConnectDirectoryResponse -> TestTree
testConnectDirectoryResponse = res
    "ConnectDirectoryResponse"
    "fixture/ConnectDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy ConnectDirectory)

testCreateAliasResponse :: CreateAliasResponse -> TestTree
testCreateAliasResponse = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    directoryService
    (Proxy :: Proxy CreateAlias)

testDescribeDirectoriesResponse :: DescribeDirectoriesResponse -> TestTree
testDescribeDirectoriesResponse = res
    "DescribeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeDirectories)

testDescribeTrustsResponse :: DescribeTrustsResponse -> TestTree
testDescribeTrustsResponse = res
    "DescribeTrustsResponse"
    "fixture/DescribeTrustsResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeTrusts)

testDeleteTrustResponse :: DeleteTrustResponse -> TestTree
testDeleteTrustResponse = res
    "DeleteTrustResponse"
    "fixture/DeleteTrustResponse.proto"
    directoryService
    (Proxy :: Proxy DeleteTrust)

testCreateMicrosoftADResponse :: CreateMicrosoftADResponse -> TestTree
testCreateMicrosoftADResponse = res
    "CreateMicrosoftADResponse"
    "fixture/CreateMicrosoftADResponse.proto"
    directoryService
    (Proxy :: Proxy CreateMicrosoftAD)

testDeregisterEventTopicResponse :: DeregisterEventTopicResponse -> TestTree
testDeregisterEventTopicResponse = res
    "DeregisterEventTopicResponse"
    "fixture/DeregisterEventTopicResponse.proto"
    directoryService
    (Proxy :: Proxy DeregisterEventTopic)

testCreateDirectoryResponse :: CreateDirectoryResponse -> TestTree
testCreateDirectoryResponse = res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy CreateDirectory)

testDescribeEventTopicsResponse :: DescribeEventTopicsResponse -> TestTree
testDescribeEventTopicsResponse = res
    "DescribeEventTopicsResponse"
    "fixture/DescribeEventTopicsResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeEventTopics)

testEnableSSOResponse :: EnableSSOResponse -> TestTree
testEnableSSOResponse = res
    "EnableSSOResponse"
    "fixture/EnableSSOResponse.proto"
    directoryService
    (Proxy :: Proxy EnableSSO)

testEnableRadiusResponse :: EnableRadiusResponse -> TestTree
testEnableRadiusResponse = res
    "EnableRadiusResponse"
    "fixture/EnableRadiusResponse.proto"
    directoryService
    (Proxy :: Proxy EnableRadius)

testDisableRadiusResponse :: DisableRadiusResponse -> TestTree
testDisableRadiusResponse = res
    "DisableRadiusResponse"
    "fixture/DisableRadiusResponse.proto"
    directoryService
    (Proxy :: Proxy DisableRadius)

testRestoreFromSnapshotResponse :: RestoreFromSnapshotResponse -> TestTree
testRestoreFromSnapshotResponse = res
    "RestoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse.proto"
    directoryService
    (Proxy :: Proxy RestoreFromSnapshot)

testDescribeSnapshotsResponse :: DescribeSnapshotsResponse -> TestTree
testDescribeSnapshotsResponse = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    directoryService
    (Proxy :: Proxy DescribeSnapshots)

testDeleteSnapshotResponse :: DeleteSnapshotResponse -> TestTree
testDeleteSnapshotResponse = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    directoryService
    (Proxy :: Proxy DeleteSnapshot)

testCreateTrustResponse :: CreateTrustResponse -> TestTree
testCreateTrustResponse = res
    "CreateTrustResponse"
    "fixture/CreateTrustResponse.proto"
    directoryService
    (Proxy :: Proxy CreateTrust)

testDeleteDirectoryResponse :: DeleteDirectoryResponse -> TestTree
testDeleteDirectoryResponse = res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy DeleteDirectory)

testCreateSnapshotResponse :: CreateSnapshotResponse -> TestTree
testCreateSnapshotResponse = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    directoryService
    (Proxy :: Proxy CreateSnapshot)

testCreateComputerResponse :: CreateComputerResponse -> TestTree
testCreateComputerResponse = res
    "CreateComputerResponse"
    "fixture/CreateComputerResponse.proto"
    directoryService
    (Proxy :: Proxy CreateComputer)

testDisableSSOResponse :: DisableSSOResponse -> TestTree
testDisableSSOResponse = res
    "DisableSSOResponse"
    "fixture/DisableSSOResponse.proto"
    directoryService
    (Proxy :: Proxy DisableSSO)

testVerifyTrustResponse :: VerifyTrustResponse -> TestTree
testVerifyTrustResponse = res
    "VerifyTrustResponse"
    "fixture/VerifyTrustResponse.proto"
    directoryService
    (Proxy :: Proxy VerifyTrust)

testGetDirectoryLimitsResponse :: GetDirectoryLimitsResponse -> TestTree
testGetDirectoryLimitsResponse = res
    "GetDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse.proto"
    directoryService
    (Proxy :: Proxy GetDirectoryLimits)

testUpdateRadiusResponse :: UpdateRadiusResponse -> TestTree
testUpdateRadiusResponse = res
    "UpdateRadiusResponse"
    "fixture/UpdateRadiusResponse.proto"
    directoryService
    (Proxy :: Proxy UpdateRadius)
