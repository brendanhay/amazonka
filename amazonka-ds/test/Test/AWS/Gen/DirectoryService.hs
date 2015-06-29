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
    "GetSnapshotLimitsResponse"
    "fixture/GetSnapshotLimitsResponse"
    (Proxy :: Proxy GetSnapshotLimits)

connectDirectoryResponseTest :: ConnectDirectoryResponse -> TestTree
connectDirectoryResponseTest = resp
    "ConnectDirectoryResponse"
    "fixture/ConnectDirectoryResponse"
    (Proxy :: Proxy ConnectDirectory)

describeDirectoriesResponseTest :: DescribeDirectoriesResponse -> TestTree
describeDirectoriesResponseTest = resp
    "DescribeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse"
    (Proxy :: Proxy DescribeDirectories)

createAliasResponseTest :: CreateAliasResponse -> TestTree
createAliasResponseTest = resp
    "CreateAliasResponse"
    "fixture/CreateAliasResponse"
    (Proxy :: Proxy CreateAlias)

createDirectoryResponseTest :: CreateDirectoryResponse -> TestTree
createDirectoryResponseTest = resp
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse"
    (Proxy :: Proxy CreateDirectory)

enableSsoResponseTest :: EnableSsoResponse -> TestTree
enableSsoResponseTest = resp
    "EnableSsoResponse"
    "fixture/EnableSsoResponse"
    (Proxy :: Proxy EnableSso)

enableRadiusResponseTest :: EnableRadiusResponse -> TestTree
enableRadiusResponseTest = resp
    "EnableRadiusResponse"
    "fixture/EnableRadiusResponse"
    (Proxy :: Proxy EnableRadius)

disableRadiusResponseTest :: DisableRadiusResponse -> TestTree
disableRadiusResponseTest = resp
    "DisableRadiusResponse"
    "fixture/DisableRadiusResponse"
    (Proxy :: Proxy DisableRadius)

describeSnapshotsResponseTest :: DescribeSnapshotsResponse -> TestTree
describeSnapshotsResponseTest = resp
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse"
    (Proxy :: Proxy DescribeSnapshots)

restoreFromSnapshotResponseTest :: RestoreFromSnapshotResponse -> TestTree
restoreFromSnapshotResponseTest = resp
    "RestoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse"
    (Proxy :: Proxy RestoreFromSnapshot)

deleteSnapshotResponseTest :: DeleteSnapshotResponse -> TestTree
deleteSnapshotResponseTest = resp
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse"
    (Proxy :: Proxy DeleteSnapshot)

createSnapshotResponseTest :: CreateSnapshotResponse -> TestTree
createSnapshotResponseTest = resp
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

deleteDirectoryResponseTest :: DeleteDirectoryResponse -> TestTree
deleteDirectoryResponseTest = resp
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse"
    (Proxy :: Proxy DeleteDirectory)

disableSsoResponseTest :: DisableSsoResponse -> TestTree
disableSsoResponseTest = resp
    "DisableSsoResponse"
    "fixture/DisableSsoResponse"
    (Proxy :: Proxy DisableSso)

createComputerResponseTest :: CreateComputerResponse -> TestTree
createComputerResponseTest = resp
    "CreateComputerResponse"
    "fixture/CreateComputerResponse"
    (Proxy :: Proxy CreateComputer)

getDirectoryLimitsResponseTest :: GetDirectoryLimitsResponse -> TestTree
getDirectoryLimitsResponseTest = resp
    "GetDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse"
    (Proxy :: Proxy GetDirectoryLimits)

updateRadiusResponseTest :: UpdateRadiusResponse -> TestTree
updateRadiusResponseTest = resp
    "UpdateRadiusResponse"
    "fixture/UpdateRadiusResponse"
    (Proxy :: Proxy UpdateRadius)
