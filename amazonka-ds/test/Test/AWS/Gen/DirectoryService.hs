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
--         , testConnectDirectory $
--             connectDirectory
--
--         , testCreateAlias $
--             createAlias
--
--         , testDescribeDirectories $
--             describeDirectories
--
--         , testCreateDirectory $
--             createDirectory
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
--         , testConnectDirectoryResponse $
--             connectDirectoryResponse
--
--         , testCreateAliasResponse $
--             createAliasResponse
--
--         , testDescribeDirectoriesResponse $
--             describeDirectoriesResponse
--
--         , testCreateDirectoryResponse $
--             createDirectoryResponse
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

testCreateDirectory :: CreateDirectory -> TestTree
testCreateDirectory = req
    "CreateDirectory"
    "fixture/CreateDirectory.yaml"

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

testCreateDirectoryResponse :: CreateDirectoryResponse -> TestTree
testCreateDirectoryResponse = res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    directoryService
    (Proxy :: Proxy CreateDirectory)

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
