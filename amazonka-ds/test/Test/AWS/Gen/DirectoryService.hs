{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DirectoryService
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
--         , testEnableSso $
--             enableSso
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
--         , testDisableSso $
--             disableSso
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
--         , testEnableSsoResponse $
--             enableSsoResponse
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
--         , testDisableSsoResponse $
--             disableSsoResponse
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

testEnableSso :: EnableSso -> TestTree
testEnableSso = req
    "EnableSso"
    "fixture/EnableSso"

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

testDisableSso :: DisableSso -> TestTree
testDisableSso = req
    "DisableSso"
    "fixture/DisableSso"

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
    (Proxy :: Proxy GetSnapshotLimits)

testConnectDirectoryResponse :: ConnectDirectoryResponse -> TestTree
testConnectDirectoryResponse = res
    "ConnectDirectoryResponse"
    "fixture/ConnectDirectoryResponse"
    (Proxy :: Proxy ConnectDirectory)

testDescribeDirectoriesResponse :: DescribeDirectoriesResponse -> TestTree
testDescribeDirectoriesResponse = res
    "DescribeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse"
    (Proxy :: Proxy DescribeDirectories)

testCreateAliasResponse :: CreateAliasResponse -> TestTree
testCreateAliasResponse = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse"
    (Proxy :: Proxy CreateAlias)

testCreateDirectoryResponse :: CreateDirectoryResponse -> TestTree
testCreateDirectoryResponse = res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse"
    (Proxy :: Proxy CreateDirectory)

testEnableSsoResponse :: EnableSsoResponse -> TestTree
testEnableSsoResponse = res
    "EnableSsoResponse"
    "fixture/EnableSsoResponse"
    (Proxy :: Proxy EnableSso)

testEnableRadiusResponse :: EnableRadiusResponse -> TestTree
testEnableRadiusResponse = res
    "EnableRadiusResponse"
    "fixture/EnableRadiusResponse"
    (Proxy :: Proxy EnableRadius)

testDisableRadiusResponse :: DisableRadiusResponse -> TestTree
testDisableRadiusResponse = res
    "DisableRadiusResponse"
    "fixture/DisableRadiusResponse"
    (Proxy :: Proxy DisableRadius)

testDescribeSnapshotsResponse :: DescribeSnapshotsResponse -> TestTree
testDescribeSnapshotsResponse = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse"
    (Proxy :: Proxy DescribeSnapshots)

testRestoreFromSnapshotResponse :: RestoreFromSnapshotResponse -> TestTree
testRestoreFromSnapshotResponse = res
    "RestoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse"
    (Proxy :: Proxy RestoreFromSnapshot)

testDeleteSnapshotResponse :: DeleteSnapshotResponse -> TestTree
testDeleteSnapshotResponse = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse"
    (Proxy :: Proxy DeleteSnapshot)

testCreateSnapshotResponse :: CreateSnapshotResponse -> TestTree
testCreateSnapshotResponse = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

testDeleteDirectoryResponse :: DeleteDirectoryResponse -> TestTree
testDeleteDirectoryResponse = res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse"
    (Proxy :: Proxy DeleteDirectory)

testDisableSsoResponse :: DisableSsoResponse -> TestTree
testDisableSsoResponse = res
    "DisableSsoResponse"
    "fixture/DisableSsoResponse"
    (Proxy :: Proxy DisableSso)

testCreateComputerResponse :: CreateComputerResponse -> TestTree
testCreateComputerResponse = res
    "CreateComputerResponse"
    "fixture/CreateComputerResponse"
    (Proxy :: Proxy CreateComputer)

testGetDirectoryLimitsResponse :: GetDirectoryLimitsResponse -> TestTree
testGetDirectoryLimitsResponse = res
    "GetDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse"
    (Proxy :: Proxy GetDirectoryLimits)

testUpdateRadiusResponse :: UpdateRadiusResponse -> TestTree
testUpdateRadiusResponse = res
    "UpdateRadiusResponse"
    "fixture/UpdateRadiusResponse"
    (Proxy :: Proxy UpdateRadius)
