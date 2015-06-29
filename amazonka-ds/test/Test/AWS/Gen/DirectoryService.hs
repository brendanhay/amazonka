-- Module      : Test.AWS.Gen.DirectoryService
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.DirectoryService where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.DirectoryService

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
testGetSnapshotLimits = undefined

testConnectDirectory :: ConnectDirectory -> TestTree
testConnectDirectory = undefined

testDescribeDirectories :: DescribeDirectories -> TestTree
testDescribeDirectories = undefined

testCreateAlias :: CreateAlias -> TestTree
testCreateAlias = undefined

testCreateDirectory :: CreateDirectory -> TestTree
testCreateDirectory = undefined

testEnableSso :: EnableSso -> TestTree
testEnableSso = undefined

testEnableRadius :: EnableRadius -> TestTree
testEnableRadius = undefined

testDisableRadius :: DisableRadius -> TestTree
testDisableRadius = undefined

testDescribeSnapshots :: DescribeSnapshots -> TestTree
testDescribeSnapshots = undefined

testRestoreFromSnapshot :: RestoreFromSnapshot -> TestTree
testRestoreFromSnapshot = undefined

testDeleteSnapshot :: DeleteSnapshot -> TestTree
testDeleteSnapshot = undefined

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = undefined

testDeleteDirectory :: DeleteDirectory -> TestTree
testDeleteDirectory = undefined

testDisableSso :: DisableSso -> TestTree
testDisableSso = undefined

testCreateComputer :: CreateComputer -> TestTree
testCreateComputer = undefined

testGetDirectoryLimits :: GetDirectoryLimits -> TestTree
testGetDirectoryLimits = undefined

testUpdateRadius :: UpdateRadius -> TestTree
testUpdateRadius = undefined

-- Responses

testGetSnapshotLimitsResponse :: GetSnapshotLimitsResponse -> TestTree
testGetSnapshotLimitsResponse = resp
    "GetSnapshotLimitsResponse"
    "fixture/GetSnapshotLimitsResponse"
    (Proxy :: Proxy GetSnapshotLimits)

testConnectDirectoryResponse :: ConnectDirectoryResponse -> TestTree
testConnectDirectoryResponse = resp
    "ConnectDirectoryResponse"
    "fixture/ConnectDirectoryResponse"
    (Proxy :: Proxy ConnectDirectory)

testDescribeDirectoriesResponse :: DescribeDirectoriesResponse -> TestTree
testDescribeDirectoriesResponse = resp
    "DescribeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse"
    (Proxy :: Proxy DescribeDirectories)

testCreateAliasResponse :: CreateAliasResponse -> TestTree
testCreateAliasResponse = resp
    "CreateAliasResponse"
    "fixture/CreateAliasResponse"
    (Proxy :: Proxy CreateAlias)

testCreateDirectoryResponse :: CreateDirectoryResponse -> TestTree
testCreateDirectoryResponse = resp
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse"
    (Proxy :: Proxy CreateDirectory)

testEnableSsoResponse :: EnableSsoResponse -> TestTree
testEnableSsoResponse = resp
    "EnableSsoResponse"
    "fixture/EnableSsoResponse"
    (Proxy :: Proxy EnableSso)

testEnableRadiusResponse :: EnableRadiusResponse -> TestTree
testEnableRadiusResponse = resp
    "EnableRadiusResponse"
    "fixture/EnableRadiusResponse"
    (Proxy :: Proxy EnableRadius)

testDisableRadiusResponse :: DisableRadiusResponse -> TestTree
testDisableRadiusResponse = resp
    "DisableRadiusResponse"
    "fixture/DisableRadiusResponse"
    (Proxy :: Proxy DisableRadius)

testDescribeSnapshotsResponse :: DescribeSnapshotsResponse -> TestTree
testDescribeSnapshotsResponse = resp
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse"
    (Proxy :: Proxy DescribeSnapshots)

testRestoreFromSnapshotResponse :: RestoreFromSnapshotResponse -> TestTree
testRestoreFromSnapshotResponse = resp
    "RestoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse"
    (Proxy :: Proxy RestoreFromSnapshot)

testDeleteSnapshotResponse :: DeleteSnapshotResponse -> TestTree
testDeleteSnapshotResponse = resp
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse"
    (Proxy :: Proxy DeleteSnapshot)

testCreateSnapshotResponse :: CreateSnapshotResponse -> TestTree
testCreateSnapshotResponse = resp
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

testDeleteDirectoryResponse :: DeleteDirectoryResponse -> TestTree
testDeleteDirectoryResponse = resp
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse"
    (Proxy :: Proxy DeleteDirectory)

testDisableSsoResponse :: DisableSsoResponse -> TestTree
testDisableSsoResponse = resp
    "DisableSsoResponse"
    "fixture/DisableSsoResponse"
    (Proxy :: Proxy DisableSso)

testCreateComputerResponse :: CreateComputerResponse -> TestTree
testCreateComputerResponse = resp
    "CreateComputerResponse"
    "fixture/CreateComputerResponse"
    (Proxy :: Proxy CreateComputer)

testGetDirectoryLimitsResponse :: GetDirectoryLimitsResponse -> TestTree
testGetDirectoryLimitsResponse = resp
    "GetDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse"
    (Proxy :: Proxy GetDirectoryLimits)

testUpdateRadiusResponse :: UpdateRadiusResponse -> TestTree
testUpdateRadiusResponse = resp
    "UpdateRadiusResponse"
    "fixture/UpdateRadiusResponse"
    (Proxy :: Proxy UpdateRadius)
