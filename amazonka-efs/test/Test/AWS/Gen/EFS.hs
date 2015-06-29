-- Module      : Test.AWS.Gen.EFS
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

module Test.AWS.Gen.EFS where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.EFS

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ createFileSystemTest $
--             createFileSystem
--
--         , createMountTargetTest $
--             createMountTarget
--
--         , createTagsTest $
--             createTags
--
--         , deleteFileSystemTest $
--             deleteFileSystem
--
--         , deleteMountTargetTest $
--             deleteMountTarget
--
--         , deleteTagsTest $
--             deleteTags
--
--         , describeFileSystemsTest $
--             describeFileSystems
--
--         , describeMountTargetSecurityGroupsTest $
--             describeMountTargetSecurityGroups
--
--         , describeMountTargetsTest $
--             describeMountTargets
--
--         , describeTagsTest $
--             describeTags
--
--         , modifyMountTargetSecurityGroupsTest $
--             modifyMountTargetSecurityGroups
--
--           ]

--     , testGroup "response"
--         [ createFileSystemResponseTest $
--             fileSystemDescription
--
--         , createMountTargetResponseTest $
--             mountTargetDescription
--
--         , createTagsResponseTest $
--             createTagsResponse
--
--         , deleteFileSystemResponseTest $
--             deleteFileSystemResponse
--
--         , deleteMountTargetResponseTest $
--             deleteMountTargetResponse
--
--         , deleteTagsResponseTest $
--             deleteTagsResponse
--
--         , describeFileSystemsResponseTest $
--             describeFileSystemsResponse
--
--         , describeMountTargetSecurityGroupsResponseTest $
--             describeMountTargetSecurityGroupsResponse
--
--         , describeMountTargetsResponseTest $
--             describeMountTargetsResponse
--
--         , describeTagsResponseTest $
--             describeTagsResponse
--
--         , modifyMountTargetSecurityGroupsResponseTest $
--             modifyMountTargetSecurityGroupsResponse
--
--           ]
--     ]

-- Requests

createFileSystemTest :: CreateFileSystem -> TestTree
createFileSystemTest = undefined

createMountTargetTest :: CreateMountTarget -> TestTree
createMountTargetTest = undefined

createTagsTest :: CreateTags -> TestTree
createTagsTest = undefined

deleteFileSystemTest :: DeleteFileSystem -> TestTree
deleteFileSystemTest = undefined

deleteMountTargetTest :: DeleteMountTarget -> TestTree
deleteMountTargetTest = undefined

deleteTagsTest :: DeleteTags -> TestTree
deleteTagsTest = undefined

describeFileSystemsTest :: DescribeFileSystems -> TestTree
describeFileSystemsTest = undefined

describeMountTargetSecurityGroupsTest :: DescribeMountTargetSecurityGroups -> TestTree
describeMountTargetSecurityGroupsTest = undefined

describeMountTargetsTest :: DescribeMountTargets -> TestTree
describeMountTargetsTest = undefined

describeTagsTest :: DescribeTags -> TestTree
describeTagsTest = undefined

modifyMountTargetSecurityGroupsTest :: ModifyMountTargetSecurityGroups -> TestTree
modifyMountTargetSecurityGroupsTest = undefined

-- Responses

createFileSystemResponseTest :: FileSystemDescription -> TestTree
createFileSystemResponseTest = resp
    "createFileSystemResponse"
    "fixture/FileSystemDescription"
    (Proxy :: Proxy CreateFileSystem)

createMountTargetResponseTest :: MountTargetDescription -> TestTree
createMountTargetResponseTest = resp
    "createMountTargetResponse"
    "fixture/MountTargetDescription"
    (Proxy :: Proxy CreateMountTarget)

createTagsResponseTest :: CreateTagsResponse -> TestTree
createTagsResponseTest = resp
    "createTagsResponse"
    "fixture/CreateTagsResponse"
    (Proxy :: Proxy CreateTags)

deleteFileSystemResponseTest :: DeleteFileSystemResponse -> TestTree
deleteFileSystemResponseTest = resp
    "deleteFileSystemResponse"
    "fixture/DeleteFileSystemResponse"
    (Proxy :: Proxy DeleteFileSystem)

deleteMountTargetResponseTest :: DeleteMountTargetResponse -> TestTree
deleteMountTargetResponseTest = resp
    "deleteMountTargetResponse"
    "fixture/DeleteMountTargetResponse"
    (Proxy :: Proxy DeleteMountTarget)

deleteTagsResponseTest :: DeleteTagsResponse -> TestTree
deleteTagsResponseTest = resp
    "deleteTagsResponse"
    "fixture/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

describeFileSystemsResponseTest :: DescribeFileSystemsResponse -> TestTree
describeFileSystemsResponseTest = resp
    "describeFileSystemsResponse"
    "fixture/DescribeFileSystemsResponse"
    (Proxy :: Proxy DescribeFileSystems)

describeMountTargetSecurityGroupsResponseTest :: DescribeMountTargetSecurityGroupsResponse -> TestTree
describeMountTargetSecurityGroupsResponseTest = resp
    "describeMountTargetSecurityGroupsResponse"
    "fixture/DescribeMountTargetSecurityGroupsResponse"
    (Proxy :: Proxy DescribeMountTargetSecurityGroups)

describeMountTargetsResponseTest :: DescribeMountTargetsResponse -> TestTree
describeMountTargetsResponseTest = resp
    "describeMountTargetsResponse"
    "fixture/DescribeMountTargetsResponse"
    (Proxy :: Proxy DescribeMountTargets)

describeTagsResponseTest :: DescribeTagsResponse -> TestTree
describeTagsResponseTest = resp
    "describeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

modifyMountTargetSecurityGroupsResponseTest :: ModifyMountTargetSecurityGroupsResponse -> TestTree
modifyMountTargetSecurityGroupsResponseTest = resp
    "modifyMountTargetSecurityGroupsResponse"
    "fixture/ModifyMountTargetSecurityGroupsResponse"
    (Proxy :: Proxy ModifyMountTargetSecurityGroups)
