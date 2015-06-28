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

import           Data.Proxy
import           Network.AWS.DirectoryService
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ getSnapshotLimitsTest $
--             getSnapshotLimits
--
--         , connectDirectoryTest $
--             connectDirectory
--
--         , describeDirectoriesTest $
--             describeDirectories
--
--         , createAliasTest $
--             createAlias
--
--         , createDirectoryTest $
--             createDirectory
--
--         , enableSsoTest $
--             enableSso
--
--         , enableRadiusTest $
--             enableRadius
--
--         , disableRadiusTest $
--             disableRadius
--
--         , describeSnapshotsTest $
--             describeSnapshots
--
--         , restoreFromSnapshotTest $
--             restoreFromSnapshot
--
--         , deleteSnapshotTest $
--             deleteSnapshot
--
--         , createSnapshotTest $
--             createSnapshot
--
--         , deleteDirectoryTest $
--             deleteDirectory
--
--         , disableSsoTest $
--             disableSso
--
--         , createComputerTest $
--             createComputer
--
--         , getDirectoryLimitsTest $
--             getDirectoryLimits
--
--         , updateRadiusTest $
--             updateRadius
--
--           ]

--     , testGroup "response"
--         [ getSnapshotLimitsResponseTest $
--             getSnapshotLimitsResponse
--
--         , connectDirectoryResponseTest $
--             connectDirectoryResponse
--
--         , describeDirectoriesResponseTest $
--             describeDirectoriesResponse
--
--         , createAliasResponseTest $
--             createAliasResponse
--
--         , createDirectoryResponseTest $
--             createDirectoryResponse
--
--         , enableSsoResponseTest $
--             enableSsoResponse
--
--         , enableRadiusResponseTest $
--             enableRadiusResponse
--
--         , disableRadiusResponseTest $
--             disableRadiusResponse
--
--         , describeSnapshotsResponseTest $
--             describeSnapshotsResponse
--
--         , restoreFromSnapshotResponseTest $
--             restoreFromSnapshotResponse
--
--         , deleteSnapshotResponseTest $
--             deleteSnapshotResponse
--
--         , createSnapshotResponseTest $
--             createSnapshotResponse
--
--         , deleteDirectoryResponseTest $
--             deleteDirectoryResponse
--
--         , disableSsoResponseTest $
--             disableSsoResponse
--
--         , createComputerResponseTest $
--             createComputerResponse
--
--         , getDirectoryLimitsResponseTest $
--             getDirectoryLimitsResponse
--
--         , updateRadiusResponseTest $
--             updateRadiusResponse
--
--           ]
--     ]

-- Requests

getSnapshotLimitsTest :: GetSnapshotLimits -> TestTree
getSnapshotLimitsTest = undefined

connectDirectoryTest :: ConnectDirectory -> TestTree
connectDirectoryTest = undefined

describeDirectoriesTest :: DescribeDirectories -> TestTree
describeDirectoriesTest = undefined

createAliasTest :: CreateAlias -> TestTree
createAliasTest = undefined

createDirectoryTest :: CreateDirectory -> TestTree
createDirectoryTest = undefined

enableSsoTest :: EnableSso -> TestTree
enableSsoTest = undefined

enableRadiusTest :: EnableRadius -> TestTree
enableRadiusTest = undefined

disableRadiusTest :: DisableRadius -> TestTree
disableRadiusTest = undefined

describeSnapshotsTest :: DescribeSnapshots -> TestTree
describeSnapshotsTest = undefined

restoreFromSnapshotTest :: RestoreFromSnapshot -> TestTree
restoreFromSnapshotTest = undefined

deleteSnapshotTest :: DeleteSnapshot -> TestTree
deleteSnapshotTest = undefined

createSnapshotTest :: CreateSnapshot -> TestTree
createSnapshotTest = undefined

deleteDirectoryTest :: DeleteDirectory -> TestTree
deleteDirectoryTest = undefined

disableSsoTest :: DisableSso -> TestTree
disableSsoTest = undefined

createComputerTest :: CreateComputer -> TestTree
createComputerTest = undefined

getDirectoryLimitsTest :: GetDirectoryLimits -> TestTree
getDirectoryLimitsTest = undefined

updateRadiusTest :: UpdateRadius -> TestTree
updateRadiusTest = undefined

-- Responses

getSnapshotLimitsResponseTest :: GetSnapshotLimitsResponse -> TestTree
getSnapshotLimitsResponseTest = resp
    "GetSnapshotLimits"
    "fixture/DirectoryService/GetSnapshotLimitsResponse"
    (Proxy :: Proxy GetSnapshotLimits)

connectDirectoryResponseTest :: ConnectDirectoryResponse -> TestTree
connectDirectoryResponseTest = resp
    "ConnectDirectory"
    "fixture/DirectoryService/ConnectDirectoryResponse"
    (Proxy :: Proxy ConnectDirectory)

describeDirectoriesResponseTest :: DescribeDirectoriesResponse -> TestTree
describeDirectoriesResponseTest = resp
    "DescribeDirectories"
    "fixture/DirectoryService/DescribeDirectoriesResponse"
    (Proxy :: Proxy DescribeDirectories)

createAliasResponseTest :: CreateAliasResponse -> TestTree
createAliasResponseTest = resp
    "CreateAlias"
    "fixture/DirectoryService/CreateAliasResponse"
    (Proxy :: Proxy CreateAlias)

createDirectoryResponseTest :: CreateDirectoryResponse -> TestTree
createDirectoryResponseTest = resp
    "CreateDirectory"
    "fixture/DirectoryService/CreateDirectoryResponse"
    (Proxy :: Proxy CreateDirectory)

enableSsoResponseTest :: EnableSsoResponse -> TestTree
enableSsoResponseTest = resp
    "EnableSso"
    "fixture/DirectoryService/EnableSsoResponse"
    (Proxy :: Proxy EnableSso)

enableRadiusResponseTest :: EnableRadiusResponse -> TestTree
enableRadiusResponseTest = resp
    "EnableRadius"
    "fixture/DirectoryService/EnableRadiusResponse"
    (Proxy :: Proxy EnableRadius)

disableRadiusResponseTest :: DisableRadiusResponse -> TestTree
disableRadiusResponseTest = resp
    "DisableRadius"
    "fixture/DirectoryService/DisableRadiusResponse"
    (Proxy :: Proxy DisableRadius)

describeSnapshotsResponseTest :: DescribeSnapshotsResponse -> TestTree
describeSnapshotsResponseTest = resp
    "DescribeSnapshots"
    "fixture/DirectoryService/DescribeSnapshotsResponse"
    (Proxy :: Proxy DescribeSnapshots)

restoreFromSnapshotResponseTest :: RestoreFromSnapshotResponse -> TestTree
restoreFromSnapshotResponseTest = resp
    "RestoreFromSnapshot"
    "fixture/DirectoryService/RestoreFromSnapshotResponse"
    (Proxy :: Proxy RestoreFromSnapshot)

deleteSnapshotResponseTest :: DeleteSnapshotResponse -> TestTree
deleteSnapshotResponseTest = resp
    "DeleteSnapshot"
    "fixture/DirectoryService/DeleteSnapshotResponse"
    (Proxy :: Proxy DeleteSnapshot)

createSnapshotResponseTest :: CreateSnapshotResponse -> TestTree
createSnapshotResponseTest = resp
    "CreateSnapshot"
    "fixture/DirectoryService/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

deleteDirectoryResponseTest :: DeleteDirectoryResponse -> TestTree
deleteDirectoryResponseTest = resp
    "DeleteDirectory"
    "fixture/DirectoryService/DeleteDirectoryResponse"
    (Proxy :: Proxy DeleteDirectory)

disableSsoResponseTest :: DisableSsoResponse -> TestTree
disableSsoResponseTest = resp
    "DisableSso"
    "fixture/DirectoryService/DisableSsoResponse"
    (Proxy :: Proxy DisableSso)

createComputerResponseTest :: CreateComputerResponse -> TestTree
createComputerResponseTest = resp
    "CreateComputer"
    "fixture/DirectoryService/CreateComputerResponse"
    (Proxy :: Proxy CreateComputer)

getDirectoryLimitsResponseTest :: GetDirectoryLimitsResponse -> TestTree
getDirectoryLimitsResponseTest = resp
    "GetDirectoryLimits"
    "fixture/DirectoryService/GetDirectoryLimitsResponse"
    (Proxy :: Proxy GetDirectoryLimits)

updateRadiusResponseTest :: UpdateRadiusResponse -> TestTree
updateRadiusResponseTest = resp
    "UpdateRadius"
    "fixture/DirectoryService/UpdateRadiusResponse"
    (Proxy :: Proxy UpdateRadius)
