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

import           Data.Proxy
import           Network.AWS.EFS
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeTagsTest $
--             describeTags
--
--         , describeMountTargetsTest $
--             describeMountTargets
--
--         , deleteMountTargetTest $
--             deleteMountTarget
--
--         , createTagsTest $
--             createTags
--
--         , describeFileSystemsTest $
--             describeFileSystems
--
--         , deleteTagsTest $
--             deleteTags
--
--         , describeMountTargetSecurityGroupsTest $
--             describeMountTargetSecurityGroups
--
--         , modifyMountTargetSecurityGroupsTest $
--             modifyMountTargetSecurityGroups
--
--         , createFileSystemTest $
--             createFileSystem
--
--         , deleteFileSystemTest $
--             deleteFileSystem
--
--         , createMountTargetTest $
--             createMountTarget
--
--           ]

--     , testGroup "response"
--         [ describeTagsResponseTest $
--             describeTagsResponse
--
--         , describeMountTargetsResponseTest $
--             describeMountTargetsResponse
--
--         , deleteMountTargetResponseTest $
--             deleteMountTargetResponse
--
--         , createTagsResponseTest $
--             createTagsResponse
--
--         , describeFileSystemsResponseTest $
--             describeFileSystemsResponse
--
--         , deleteTagsResponseTest $
--             deleteTagsResponse
--
--         , describeMountTargetSecurityGroupsResponseTest $
--             describeMountTargetSecurityGroupsResponse
--
--         , modifyMountTargetSecurityGroupsResponseTest $
--             modifyMountTargetSecurityGroupsResponse
--
--         , fileSystemDescriptionTest $
--             fileSystemDescription
--
--         , deleteFileSystemResponseTest $
--             deleteFileSystemResponse
--
--         , mountTargetDescriptionTest $
--             mountTargetDescription
--
--           ]
--     ]

-- Requests

describeTagsTest :: DescribeTags -> TestTree
describeTagsTest = undefined

describeMountTargetsTest :: DescribeMountTargets -> TestTree
describeMountTargetsTest = undefined

deleteMountTargetTest :: DeleteMountTarget -> TestTree
deleteMountTargetTest = undefined

createTagsTest :: CreateTags -> TestTree
createTagsTest = undefined

describeFileSystemsTest :: DescribeFileSystems -> TestTree
describeFileSystemsTest = undefined

deleteTagsTest :: DeleteTags -> TestTree
deleteTagsTest = undefined

describeMountTargetSecurityGroupsTest :: DescribeMountTargetSecurityGroups -> TestTree
describeMountTargetSecurityGroupsTest = undefined

modifyMountTargetSecurityGroupsTest :: ModifyMountTargetSecurityGroups -> TestTree
modifyMountTargetSecurityGroupsTest = undefined

createFileSystemTest :: CreateFileSystem -> TestTree
createFileSystemTest = undefined

deleteFileSystemTest :: DeleteFileSystem -> TestTree
deleteFileSystemTest = undefined

createMountTargetTest :: CreateMountTarget -> TestTree
createMountTargetTest = undefined

-- Responses

describeTagsResponseTest :: DescribeTagsResponse -> TestTree
describeTagsResponseTest = resp
    "DescribeTags"
    "fixture/EFS/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

describeMountTargetsResponseTest :: DescribeMountTargetsResponse -> TestTree
describeMountTargetsResponseTest = resp
    "DescribeMountTargets"
    "fixture/EFS/DescribeMountTargetsResponse"
    (Proxy :: Proxy DescribeMountTargets)

deleteMountTargetResponseTest :: DeleteMountTargetResponse -> TestTree
deleteMountTargetResponseTest = resp
    "DeleteMountTarget"
    "fixture/EFS/DeleteMountTargetResponse"
    (Proxy :: Proxy DeleteMountTarget)

createTagsResponseTest :: CreateTagsResponse -> TestTree
createTagsResponseTest = resp
    "CreateTags"
    "fixture/EFS/CreateTagsResponse"
    (Proxy :: Proxy CreateTags)

describeFileSystemsResponseTest :: DescribeFileSystemsResponse -> TestTree
describeFileSystemsResponseTest = resp
    "DescribeFileSystems"
    "fixture/EFS/DescribeFileSystemsResponse"
    (Proxy :: Proxy DescribeFileSystems)

deleteTagsResponseTest :: DeleteTagsResponse -> TestTree
deleteTagsResponseTest = resp
    "DeleteTags"
    "fixture/EFS/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

describeMountTargetSecurityGroupsResponseTest :: DescribeMountTargetSecurityGroupsResponse -> TestTree
describeMountTargetSecurityGroupsResponseTest = resp
    "DescribeMountTargetSecurityGroups"
    "fixture/EFS/DescribeMountTargetSecurityGroupsResponse"
    (Proxy :: Proxy DescribeMountTargetSecurityGroups)

modifyMountTargetSecurityGroupsResponseTest :: ModifyMountTargetSecurityGroupsResponse -> TestTree
modifyMountTargetSecurityGroupsResponseTest = resp
    "ModifyMountTargetSecurityGroups"
    "fixture/EFS/ModifyMountTargetSecurityGroupsResponse"
    (Proxy :: Proxy ModifyMountTargetSecurityGroups)

fileSystemDescriptionTest :: FileSystemDescription -> TestTree
fileSystemDescriptionTest = resp
    "CreateFileSystem"
    "fixture/EFS/FileSystemDescription"
    (Proxy :: Proxy CreateFileSystem)

deleteFileSystemResponseTest :: DeleteFileSystemResponse -> TestTree
deleteFileSystemResponseTest = resp
    "DeleteFileSystem"
    "fixture/EFS/DeleteFileSystemResponse"
    (Proxy :: Proxy DeleteFileSystem)

mountTargetDescriptionTest :: MountTargetDescription -> TestTree
mountTargetDescriptionTest = resp
    "CreateMountTarget"
    "fixture/EFS/MountTargetDescription"
    (Proxy :: Proxy CreateMountTarget)
