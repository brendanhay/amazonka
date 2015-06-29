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
--         [ connectDirectoryTest $
--             connectDirectory
--
--         , createAliasTest $
--             createAlias
--
--         , createComputerTest $
--             createComputer
--
--         , createDirectoryTest $
--             createDirectory
--
--         , createSnapshotTest $
--             createSnapshot
--
--         , deleteDirectoryTest $
--             deleteDirectory
--
--         , deleteSnapshotTest $
--             deleteSnapshot
--
--         , describeDirectoriesTest $
--             describeDirectories
--
--         , describeSnapshotsTest $
--             describeSnapshots
--
--         , disableRadiusTest $
--             disableRadius
--
--         , disableSsoTest $
--             disableSso
--
--         , enableRadiusTest $
--             enableRadius
--
--         , enableSsoTest $
--             enableSso
--
--         , getDirectoryLimitsTest $
--             getDirectoryLimits
--
--         , getSnapshotLimitsTest $
--             getSnapshotLimits
--
--         , restoreFromSnapshotTest $
--             restoreFromSnapshot
--
--         , updateRadiusTest $
--             updateRadius
--
--           ]

--     , testGroup "response"
--         [ connectDirectoryResponseTest $
--             connectDirectoryResponse
--
--         , createAliasResponseTest $
--             createAliasResponse
--
--         , createComputerResponseTest $
--             createComputerResponse
--
--         , createDirectoryResponseTest $
--             createDirectoryResponse
--
--         , createSnapshotResponseTest $
--             createSnapshotResponse
--
--         , deleteDirectoryResponseTest $
--             deleteDirectoryResponse
--
--         , deleteSnapshotResponseTest $
--             deleteSnapshotResponse
--
--         , describeDirectoriesResponseTest $
--             describeDirectoriesResponse
--
--         , describeSnapshotsResponseTest $
--             describeSnapshotsResponse
--
--         , disableRadiusResponseTest $
--             disableRadiusResponse
--
--         , disableSsoResponseTest $
--             disableSsoResponse
--
--         , enableRadiusResponseTest $
--             enableRadiusResponse
--
--         , enableSsoResponseTest $
--             enableSsoResponse
--
--         , getDirectoryLimitsResponseTest $
--             getDirectoryLimitsResponse
--
--         , getSnapshotLimitsResponseTest $
--             getSnapshotLimitsResponse
--
--         , restoreFromSnapshotResponseTest $
--             restoreFromSnapshotResponse
--
--         , updateRadiusResponseTest $
--             updateRadiusResponse
--
--           ]
--     ]

-- Requests

connectDirectoryTest :: ConnectDirectory -> TestTree
connectDirectoryTest = undefined

createAliasTest :: CreateAlias -> TestTree
createAliasTest = undefined

createComputerTest :: CreateComputer -> TestTree
createComputerTest = undefined

createDirectoryTest :: CreateDirectory -> TestTree
createDirectoryTest = undefined

createSnapshotTest :: CreateSnapshot -> TestTree
createSnapshotTest = undefined

deleteDirectoryTest :: DeleteDirectory -> TestTree
deleteDirectoryTest = undefined

deleteSnapshotTest :: DeleteSnapshot -> TestTree
deleteSnapshotTest = undefined

describeDirectoriesTest :: DescribeDirectories -> TestTree
describeDirectoriesTest = undefined

describeSnapshotsTest :: DescribeSnapshots -> TestTree
describeSnapshotsTest = undefined

disableRadiusTest :: DisableRadius -> TestTree
disableRadiusTest = undefined

disableSsoTest :: DisableSso -> TestTree
disableSsoTest = undefined

enableRadiusTest :: EnableRadius -> TestTree
enableRadiusTest = undefined

enableSsoTest :: EnableSso -> TestTree
enableSsoTest = undefined

getDirectoryLimitsTest :: GetDirectoryLimits -> TestTree
getDirectoryLimitsTest = undefined

getSnapshotLimitsTest :: GetSnapshotLimits -> TestTree
getSnapshotLimitsTest = undefined

restoreFromSnapshotTest :: RestoreFromSnapshot -> TestTree
restoreFromSnapshotTest = undefined

updateRadiusTest :: UpdateRadius -> TestTree
updateRadiusTest = undefined

-- Responses

connectDirectoryResponseTest :: ConnectDirectoryResponse -> TestTree
connectDirectoryResponseTest = resp
    "connectDirectoryResponse"
    "fixture/ConnectDirectoryResponse"
    (Proxy :: Proxy ConnectDirectory)

createAliasResponseTest :: CreateAliasResponse -> TestTree
createAliasResponseTest = resp
    "createAliasResponse"
    "fixture/CreateAliasResponse"
    (Proxy :: Proxy CreateAlias)

createComputerResponseTest :: CreateComputerResponse -> TestTree
createComputerResponseTest = resp
    "createComputerResponse"
    "fixture/CreateComputerResponse"
    (Proxy :: Proxy CreateComputer)

createDirectoryResponseTest :: CreateDirectoryResponse -> TestTree
createDirectoryResponseTest = resp
    "createDirectoryResponse"
    "fixture/CreateDirectoryResponse"
    (Proxy :: Proxy CreateDirectory)

createSnapshotResponseTest :: CreateSnapshotResponse -> TestTree
createSnapshotResponseTest = resp
    "createSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

deleteDirectoryResponseTest :: DeleteDirectoryResponse -> TestTree
deleteDirectoryResponseTest = resp
    "deleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse"
    (Proxy :: Proxy DeleteDirectory)

deleteSnapshotResponseTest :: DeleteSnapshotResponse -> TestTree
deleteSnapshotResponseTest = resp
    "deleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse"
    (Proxy :: Proxy DeleteSnapshot)

describeDirectoriesResponseTest :: DescribeDirectoriesResponse -> TestTree
describeDirectoriesResponseTest = resp
    "describeDirectoriesResponse"
    "fixture/DescribeDirectoriesResponse"
    (Proxy :: Proxy DescribeDirectories)

describeSnapshotsResponseTest :: DescribeSnapshotsResponse -> TestTree
describeSnapshotsResponseTest = resp
    "describeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse"
    (Proxy :: Proxy DescribeSnapshots)

disableRadiusResponseTest :: DisableRadiusResponse -> TestTree
disableRadiusResponseTest = resp
    "disableRadiusResponse"
    "fixture/DisableRadiusResponse"
    (Proxy :: Proxy DisableRadius)

disableSsoResponseTest :: DisableSsoResponse -> TestTree
disableSsoResponseTest = resp
    "disableSsoResponse"
    "fixture/DisableSsoResponse"
    (Proxy :: Proxy DisableSso)

enableRadiusResponseTest :: EnableRadiusResponse -> TestTree
enableRadiusResponseTest = resp
    "enableRadiusResponse"
    "fixture/EnableRadiusResponse"
    (Proxy :: Proxy EnableRadius)

enableSsoResponseTest :: EnableSsoResponse -> TestTree
enableSsoResponseTest = resp
    "enableSsoResponse"
    "fixture/EnableSsoResponse"
    (Proxy :: Proxy EnableSso)

getDirectoryLimitsResponseTest :: GetDirectoryLimitsResponse -> TestTree
getDirectoryLimitsResponseTest = resp
    "getDirectoryLimitsResponse"
    "fixture/GetDirectoryLimitsResponse"
    (Proxy :: Proxy GetDirectoryLimits)

getSnapshotLimitsResponseTest :: GetSnapshotLimitsResponse -> TestTree
getSnapshotLimitsResponseTest = resp
    "getSnapshotLimitsResponse"
    "fixture/GetSnapshotLimitsResponse"
    (Proxy :: Proxy GetSnapshotLimits)

restoreFromSnapshotResponseTest :: RestoreFromSnapshotResponse -> TestTree
restoreFromSnapshotResponseTest = resp
    "restoreFromSnapshotResponse"
    "fixture/RestoreFromSnapshotResponse"
    (Proxy :: Proxy RestoreFromSnapshot)

updateRadiusResponseTest :: UpdateRadiusResponse -> TestTree
updateRadiusResponseTest = resp
    "updateRadiusResponse"
    "fixture/UpdateRadiusResponse"
    (Proxy :: Proxy UpdateRadius)
