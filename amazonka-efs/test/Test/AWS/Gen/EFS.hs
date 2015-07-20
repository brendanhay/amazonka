{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EFS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.EFS where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.EFS
import Test.AWS.EFS.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeTags $
--             describeTags
--
--         , testDescribeMountTargets $
--             describeMountTargets
--
--         , testDeleteMountTarget $
--             deleteMountTarget
--
--         , testCreateTags $
--             createTags
--
--         , testDescribeFileSystems $
--             describeFileSystems
--
--         , testDeleteTags $
--             deleteTags
--
--         , testDescribeMountTargetSecurityGroups $
--             describeMountTargetSecurityGroups
--
--         , testModifyMountTargetSecurityGroups $
--             modifyMountTargetSecurityGroups
--
--         , testCreateFileSystem $
--             createFileSystem
--
--         , testDeleteFileSystem $
--             deleteFileSystem
--
--         , testCreateMountTarget $
--             createMountTarget
--
--           ]

--     , testGroup "response"
--         [ testDescribeTagsResponse $
--             describeTagsResponse
--
--         , testDescribeMountTargetsResponse $
--             describeMountTargetsResponse
--
--         , testDeleteMountTargetResponse $
--             deleteMountTargetResponse
--
--         , testCreateTagsResponse $
--             createTagsResponse
--
--         , testDescribeFileSystemsResponse $
--             describeFileSystemsResponse
--
--         , testDeleteTagsResponse $
--             deleteTagsResponse
--
--         , testDescribeMountTargetSecurityGroupsResponse $
--             describeMountTargetSecurityGroupsResponse
--
--         , testModifyMountTargetSecurityGroupsResponse $
--             modifyMountTargetSecurityGroupsResponse
--
--         , testCreateFileSystemResponse $
--             fileSystemDescription
--
--         , testDeleteFileSystemResponse $
--             deleteFileSystemResponse
--
--         , testCreateMountTargetResponse $
--             mountTargetDescription
--
--           ]
--     ]

-- Requests

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags"

testDescribeMountTargets :: DescribeMountTargets -> TestTree
testDescribeMountTargets = req
    "DescribeMountTargets"
    "fixture/DescribeMountTargets"

testDeleteMountTarget :: DeleteMountTarget -> TestTree
testDeleteMountTarget = req
    "DeleteMountTarget"
    "fixture/DeleteMountTarget"

testCreateTags :: CreateTags -> TestTree
testCreateTags = req
    "CreateTags"
    "fixture/CreateTags"

testDescribeFileSystems :: DescribeFileSystems -> TestTree
testDescribeFileSystems = req
    "DescribeFileSystems"
    "fixture/DescribeFileSystems"

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags"

testDescribeMountTargetSecurityGroups :: DescribeMountTargetSecurityGroups -> TestTree
testDescribeMountTargetSecurityGroups = req
    "DescribeMountTargetSecurityGroups"
    "fixture/DescribeMountTargetSecurityGroups"

testModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroups -> TestTree
testModifyMountTargetSecurityGroups = req
    "ModifyMountTargetSecurityGroups"
    "fixture/ModifyMountTargetSecurityGroups"

testCreateFileSystem :: CreateFileSystem -> TestTree
testCreateFileSystem = req
    "CreateFileSystem"
    "fixture/CreateFileSystem"

testDeleteFileSystem :: DeleteFileSystem -> TestTree
testDeleteFileSystem = req
    "DeleteFileSystem"
    "fixture/DeleteFileSystem"

testCreateMountTarget :: CreateMountTarget -> TestTree
testCreateMountTarget = req
    "CreateMountTarget"
    "fixture/CreateMountTarget"

-- Responses

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

testDescribeMountTargetsResponse :: DescribeMountTargetsResponse -> TestTree
testDescribeMountTargetsResponse = res
    "DescribeMountTargetsResponse"
    "fixture/DescribeMountTargetsResponse"
    (Proxy :: Proxy DescribeMountTargets)

testDeleteMountTargetResponse :: DeleteMountTargetResponse -> TestTree
testDeleteMountTargetResponse = res
    "DeleteMountTargetResponse"
    "fixture/DeleteMountTargetResponse"
    (Proxy :: Proxy DeleteMountTarget)

testCreateTagsResponse :: CreateTagsResponse -> TestTree
testCreateTagsResponse = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse"
    (Proxy :: Proxy CreateTags)

testDescribeFileSystemsResponse :: DescribeFileSystemsResponse -> TestTree
testDescribeFileSystemsResponse = res
    "DescribeFileSystemsResponse"
    "fixture/DescribeFileSystemsResponse"
    (Proxy :: Proxy DescribeFileSystems)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

testDescribeMountTargetSecurityGroupsResponse :: DescribeMountTargetSecurityGroupsResponse -> TestTree
testDescribeMountTargetSecurityGroupsResponse = res
    "DescribeMountTargetSecurityGroupsResponse"
    "fixture/DescribeMountTargetSecurityGroupsResponse"
    (Proxy :: Proxy DescribeMountTargetSecurityGroups)

testModifyMountTargetSecurityGroupsResponse :: ModifyMountTargetSecurityGroupsResponse -> TestTree
testModifyMountTargetSecurityGroupsResponse = res
    "ModifyMountTargetSecurityGroupsResponse"
    "fixture/ModifyMountTargetSecurityGroupsResponse"
    (Proxy :: Proxy ModifyMountTargetSecurityGroups)

testCreateFileSystemResponse :: FileSystemDescription -> TestTree
testCreateFileSystemResponse = res
    "CreateFileSystemResponse"
    "fixture/CreateFileSystemResponse"
    (Proxy :: Proxy CreateFileSystem)

testDeleteFileSystemResponse :: DeleteFileSystemResponse -> TestTree
testDeleteFileSystemResponse = res
    "DeleteFileSystemResponse"
    "fixture/DeleteFileSystemResponse"
    (Proxy :: Proxy DeleteFileSystem)

testCreateMountTargetResponse :: MountTargetDescription -> TestTree
testCreateMountTargetResponse = res
    "CreateMountTargetResponse"
    "fixture/CreateMountTargetResponse"
    (Proxy :: Proxy CreateMountTarget)
