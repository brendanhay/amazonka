{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EFS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
--         , testDescribeFileSystems $
--             describeFileSystems
--
--         , testDeleteMountTarget $
--             deleteMountTarget
--
--         , testCreateTags $
--             createTags
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
--         , testDescribeFileSystemsResponse $
--             describeFileSystemsResponse
--
--         , testDeleteMountTargetResponse $
--             deleteMountTargetResponse
--
--         , testCreateTagsResponse $
--             createTagsResponse
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
    "fixture/DescribeTags.yaml"

testDescribeMountTargets :: DescribeMountTargets -> TestTree
testDescribeMountTargets = req
    "DescribeMountTargets"
    "fixture/DescribeMountTargets.yaml"

testDescribeFileSystems :: DescribeFileSystems -> TestTree
testDescribeFileSystems = req
    "DescribeFileSystems"
    "fixture/DescribeFileSystems.yaml"

testDeleteMountTarget :: DeleteMountTarget -> TestTree
testDeleteMountTarget = req
    "DeleteMountTarget"
    "fixture/DeleteMountTarget.yaml"

testCreateTags :: CreateTags -> TestTree
testCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

testDescribeMountTargetSecurityGroups :: DescribeMountTargetSecurityGroups -> TestTree
testDescribeMountTargetSecurityGroups = req
    "DescribeMountTargetSecurityGroups"
    "fixture/DescribeMountTargetSecurityGroups.yaml"

testModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroups -> TestTree
testModifyMountTargetSecurityGroups = req
    "ModifyMountTargetSecurityGroups"
    "fixture/ModifyMountTargetSecurityGroups.yaml"

testCreateFileSystem :: CreateFileSystem -> TestTree
testCreateFileSystem = req
    "CreateFileSystem"
    "fixture/CreateFileSystem.yaml"

testDeleteFileSystem :: DeleteFileSystem -> TestTree
testDeleteFileSystem = req
    "DeleteFileSystem"
    "fixture/DeleteFileSystem.yaml"

testCreateMountTarget :: CreateMountTarget -> TestTree
testCreateMountTarget = req
    "CreateMountTarget"
    "fixture/CreateMountTarget.yaml"

-- Responses

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    efs
    (Proxy :: Proxy DescribeTags)

testDescribeMountTargetsResponse :: DescribeMountTargetsResponse -> TestTree
testDescribeMountTargetsResponse = res
    "DescribeMountTargetsResponse"
    "fixture/DescribeMountTargetsResponse.proto"
    efs
    (Proxy :: Proxy DescribeMountTargets)

testDescribeFileSystemsResponse :: DescribeFileSystemsResponse -> TestTree
testDescribeFileSystemsResponse = res
    "DescribeFileSystemsResponse"
    "fixture/DescribeFileSystemsResponse.proto"
    efs
    (Proxy :: Proxy DescribeFileSystems)

testDeleteMountTargetResponse :: DeleteMountTargetResponse -> TestTree
testDeleteMountTargetResponse = res
    "DeleteMountTargetResponse"
    "fixture/DeleteMountTargetResponse.proto"
    efs
    (Proxy :: Proxy DeleteMountTarget)

testCreateTagsResponse :: CreateTagsResponse -> TestTree
testCreateTagsResponse = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    efs
    (Proxy :: Proxy CreateTags)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    efs
    (Proxy :: Proxy DeleteTags)

testDescribeMountTargetSecurityGroupsResponse :: DescribeMountTargetSecurityGroupsResponse -> TestTree
testDescribeMountTargetSecurityGroupsResponse = res
    "DescribeMountTargetSecurityGroupsResponse"
    "fixture/DescribeMountTargetSecurityGroupsResponse.proto"
    efs
    (Proxy :: Proxy DescribeMountTargetSecurityGroups)

testModifyMountTargetSecurityGroupsResponse :: ModifyMountTargetSecurityGroupsResponse -> TestTree
testModifyMountTargetSecurityGroupsResponse = res
    "ModifyMountTargetSecurityGroupsResponse"
    "fixture/ModifyMountTargetSecurityGroupsResponse.proto"
    efs
    (Proxy :: Proxy ModifyMountTargetSecurityGroups)

testCreateFileSystemResponse :: FileSystemDescription -> TestTree
testCreateFileSystemResponse = res
    "CreateFileSystemResponse"
    "fixture/CreateFileSystemResponse.proto"
    efs
    (Proxy :: Proxy CreateFileSystem)

testDeleteFileSystemResponse :: DeleteFileSystemResponse -> TestTree
testDeleteFileSystemResponse = res
    "DeleteFileSystemResponse"
    "fixture/DeleteFileSystemResponse.proto"
    efs
    (Proxy :: Proxy DeleteFileSystem)

testCreateMountTargetResponse :: MountTargetDescription -> TestTree
testCreateMountTargetResponse = res
    "CreateMountTargetResponse"
    "fixture/CreateMountTargetResponse.proto"
    efs
    (Proxy :: Proxy CreateMountTarget)
