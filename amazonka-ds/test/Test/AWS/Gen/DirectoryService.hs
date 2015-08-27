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
--         , testDescribeDirectories $
--             describeDirectories
--
--         , testCreateAlias $
--             createAlias
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
--         , testDescribeSnapshots $
--             describeSnapshots
--
--         , testRestoreFromSnapshot $
--             restoreFromSnapshot
--
--         , testDeleteSnapshot $
--             deleteSnapshot
--
--         , testCreateSnapshot $
--             createSnapshot
--
--         , testDeleteDirectory $
--             deleteDirectory
--
--         , testDisableSSO $
--             disableSSO
--
--         , testCreateComputer $
--             createComputer
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
--         , testDescribeDirectoriesResponse $
--             describeDirectoriesResponse
--
--         , testCreateAliasResponse $
--             createAliasResponse
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
--         , testDescribeSnapshotsResponse $
--             describeSnapshotsResponse
--
--         , testRestoreFromSnapshotResponse $
--             restoreFromSnapshotResponse
--
--         , testDeleteSnapshotResponse $
--             deleteSnapshotResponse
--
--         , testCreateSnapshotResponse $
--             createSnapshotResponse
--
--         , testDeleteDirectoryResponse $
--             deleteDirectoryResponse
--
--         , testDisableSSOResponse $
--             disableSSOResponse
--
--         , testCreateComputerResponse $
--             createComputerResponse
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
    "fixture/GetSnapshotLimits"

testConnectDirectory :: ConnectDirectory -> TestTree
testConnectDirectory = req
    "ConnectDirectory"
    "fixture/ConnectDirectory"

testDescribeDirectories :: DescribeDirectories -> TestTree
testDescribeDirectories = req
    "DescribeDirectories"
    "fixture/DescribeDirectories"

testCreateAlias :: CreateAlias -> TestTree
testCreateAlias = req
    "CreateAlias"
    "fixture/CreateAlias"

testCreateDirectory :: CreateDirectory -> TestTree
testCreateDirectory = req
    "CreateDirectory"
    "fixture/CreateDirectory"

testEnableSSO :: EnableSSO -> TestTree
testEnableSSO = req
    "EnableSSO"
    "fixture/EnableSSO"

testEnableRadius :: EnableRadius -> TestTree
testEnableRadius = req
    "EnableRadius"
    "fixture/EnableRadius"

testDisableRadius :: DisableRadius -> TestTree
testDisableRadius = req
    "DisableRadius"
    "fixture/DisableRadius"

testDescribeSnapshots :: DescribeSnapshots -> TestTree
testDescribeSnapshots = req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots"

testRestoreFromSnapshot :: RestoreFromSnapshot -> TestTree
testRestoreFromSnapshot = req
    "RestoreFromSnapshot"
    "fixture/RestoreFromSnapshot"

testDeleteSnapshot :: DeleteSnapshot -> TestTree
testDeleteSnapshot = req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot"

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot"

testDeleteDirectory :: DeleteDirectory -> TestTree
testDeleteDirectory = req
    "DeleteDirectory"
    "fixture/DeleteDirectory"

testDisableSSO :: DisableSSO -> TestTree
testDisableSSO = req
    "DisableSSO"
    "fixture/DisableSSO"

testCreateComputer :: CreateComputer -> TestTree
testCreateComputer = req
    "CreateComputer"
    "fixture/CreateComputer"

testGetDirectoryLimits :: GetDirectoryLimits -> TestTree
testGetDirectoryLimits = req
    "GetDirectoryLimits"
    "fixture/GetDirectoryLimits"

testUpdateRadius :: UpdateRadius -> TestTree
testUpdateRadius = req
    "UpdateRadius"
    "fixture/UpdateRadius"

-- Responses

testGetSnapshotLimitsResponse :: GetSnapshotLimitsResponse -> TestTree
testGetSnapshotLimitsResponse = res
    "GetSnapshotLimitsResponse"
    "fixture/GetSnapshotLimitsResponse"
    directoryService
    (Proxy :: Proxy GetSnapshotLimits)

testConnectDirectoryResponse :: ConnectDirectoryResponse -> TestTree
testConnectDirectoryResponse = res
    "ConnectDirectoryResponse"
    "fixture/ConnectDirectoryResponse"
    directoryService
    (Proxy :: Proxy ConnectDirectory)

testDescribeDirectoriesResponse :: DescribeDirectoriesResponse -> TestTree
testDescribeDirectoriesResponse = res
    "DescribeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse"
    directoryService
    (Proxy :: Proxy DescribeDirectories)

testCreateAliasResponse :: CreateAliasResponse -> TestTree
testCreateAliasResponse = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse"
    directoryService
    (Proxy :: Proxy CreateAlias)

testCreateDirectoryResponse :: CreateDirectoryResponse -> TestTree
testCreateDirectoryResponse = res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse"
    directoryService
    (Proxy :: Proxy CreateDirectory)

testEnableSSOResponse :: EnableSSOResponse -> TestTree
testEnableSSOResponse = res
    "EnableSSOResponse"
    "fixture/EnableSSOResponse"
    directoryService
    (Proxy :: Proxy EnableSSO)

testEnableRadiusResponse :: EnableRadiusResponse -> TestTree
testEnableRadiusResponse = res
    "EnableRadiusResponse"
    "fixture/EnableRadiusResponse"
    directoryService
    (Proxy :: Proxy EnableRadius)

testDisableRadiusResponse :: DisableRadiusResponse -> TestTree
testDisableRadiusResponse = res
    "DisableRadiusResponse"
    "fixture/DisableRadiusResponse"
    directoryService
    (Proxy :: Proxy DisableRadius)

testDescribeSnapshotsResponse :: DescribeSnapshotsResponse -> TestTree
testDescribeSnapshotsResponse = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse"
    directoryService
    (Proxy :: Proxy DescribeSnapshots)

testRestoreFromSnapshotResponse :: RestoreFromSnapshotResponse -> TestTree
testRestoreFromSnapshotResponse = res
    "RestoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse"
    directoryService
    (Proxy :: Proxy RestoreFromSnapshot)

testDeleteSnapshotResponse :: DeleteSnapshotResponse -> TestTree
testDeleteSnapshotResponse = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse"
    directoryService
    (Proxy :: Proxy DeleteSnapshot)

testCreateSnapshotResponse :: CreateSnapshotResponse -> TestTree
testCreateSnapshotResponse = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    directoryService
    (Proxy :: Proxy CreateSnapshot)

testDeleteDirectoryResponse :: DeleteDirectoryResponse -> TestTree
testDeleteDirectoryResponse = res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse"
    directoryService
    (Proxy :: Proxy DeleteDirectory)

testDisableSSOResponse :: DisableSSOResponse -> TestTree
testDisableSSOResponse = res
    "DisableSSOResponse"
    "fixture/DisableSSOResponse"
    directoryService
    (Proxy :: Proxy DisableSSO)

testCreateComputerResponse :: CreateComputerResponse -> TestTree
testCreateComputerResponse = res
    "CreateComputerResponse"
    "fixture/CreateComputerResponse"
    directoryService
    (Proxy :: Proxy CreateComputer)

testGetDirectoryLimitsResponse :: GetDirectoryLimitsResponse -> TestTree
testGetDirectoryLimitsResponse = res
    "GetDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse"
    directoryService
    (Proxy :: Proxy GetDirectoryLimits)

testUpdateRadiusResponse :: UpdateRadiusResponse -> TestTree
testUpdateRadiusResponse = res
    "UpdateRadiusResponse"
    "fixture/UpdateRadiusResponse"
    directoryService
    (Proxy :: Proxy UpdateRadius)
