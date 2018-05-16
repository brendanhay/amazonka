{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EFS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.EFS where

import Data.Proxy
import Network.AWS.EFS
import Test.AWS.EFS.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeTags $
--             describeTags
--
--         , requestDescribeMountTargets $
--             describeMountTargets
--
--         , requestDescribeFileSystems $
--             describeFileSystems
--
--         , requestDeleteMountTarget $
--             deleteMountTarget
--
--         , requestCreateTags $
--             createTags
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestDescribeMountTargetSecurityGroups $
--             describeMountTargetSecurityGroups
--
--         , requestModifyMountTargetSecurityGroups $
--             modifyMountTargetSecurityGroups
--
--         , requestCreateFileSystem $
--             createFileSystem
--
--         , requestDeleteFileSystem $
--             deleteFileSystem
--
--         , requestCreateMountTarget $
--             createMountTarget
--
--           ]

--     , testGroup "response"
--         [ responseDescribeTags $
--             describeTagsResponse
--
--         , responseDescribeMountTargets $
--             describeMountTargetsResponse
--
--         , responseDescribeFileSystems $
--             describeFileSystemsResponse
--
--         , responseDeleteMountTarget $
--             deleteMountTargetResponse
--
--         , responseCreateTags $
--             createTagsResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseDescribeMountTargetSecurityGroups $
--             describeMountTargetSecurityGroupsResponse
--
--         , responseModifyMountTargetSecurityGroups $
--             modifyMountTargetSecurityGroupsResponse
--
--         , responseCreateFileSystem $
--             fileSystemDescription
--
--         , responseDeleteFileSystem $
--             deleteFileSystemResponse
--
--         , responseCreateMountTarget $
--             mountTargetDescription
--
--           ]
--     ]

-- Requests

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeMountTargets :: DescribeMountTargets -> TestTree
requestDescribeMountTargets = req
    "DescribeMountTargets"
    "fixture/DescribeMountTargets.yaml"

requestDescribeFileSystems :: DescribeFileSystems -> TestTree
requestDescribeFileSystems = req
    "DescribeFileSystems"
    "fixture/DescribeFileSystems.yaml"

requestDeleteMountTarget :: DeleteMountTarget -> TestTree
requestDeleteMountTarget = req
    "DeleteMountTarget"
    "fixture/DeleteMountTarget.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDescribeMountTargetSecurityGroups :: DescribeMountTargetSecurityGroups -> TestTree
requestDescribeMountTargetSecurityGroups = req
    "DescribeMountTargetSecurityGroups"
    "fixture/DescribeMountTargetSecurityGroups.yaml"

requestModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroups -> TestTree
requestModifyMountTargetSecurityGroups = req
    "ModifyMountTargetSecurityGroups"
    "fixture/ModifyMountTargetSecurityGroups.yaml"

requestCreateFileSystem :: CreateFileSystem -> TestTree
requestCreateFileSystem = req
    "CreateFileSystem"
    "fixture/CreateFileSystem.yaml"

requestDeleteFileSystem :: DeleteFileSystem -> TestTree
requestDeleteFileSystem = req
    "DeleteFileSystem"
    "fixture/DeleteFileSystem.yaml"

requestCreateMountTarget :: CreateMountTarget -> TestTree
requestCreateMountTarget = req
    "CreateMountTarget"
    "fixture/CreateMountTarget.yaml"

-- Responses

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    efs
    (Proxy :: Proxy DescribeTags)

responseDescribeMountTargets :: DescribeMountTargetsResponse -> TestTree
responseDescribeMountTargets = res
    "DescribeMountTargetsResponse"
    "fixture/DescribeMountTargetsResponse.proto"
    efs
    (Proxy :: Proxy DescribeMountTargets)

responseDescribeFileSystems :: DescribeFileSystemsResponse -> TestTree
responseDescribeFileSystems = res
    "DescribeFileSystemsResponse"
    "fixture/DescribeFileSystemsResponse.proto"
    efs
    (Proxy :: Proxy DescribeFileSystems)

responseDeleteMountTarget :: DeleteMountTargetResponse -> TestTree
responseDeleteMountTarget = res
    "DeleteMountTargetResponse"
    "fixture/DeleteMountTargetResponse.proto"
    efs
    (Proxy :: Proxy DeleteMountTarget)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    efs
    (Proxy :: Proxy CreateTags)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    efs
    (Proxy :: Proxy DeleteTags)

responseDescribeMountTargetSecurityGroups :: DescribeMountTargetSecurityGroupsResponse -> TestTree
responseDescribeMountTargetSecurityGroups = res
    "DescribeMountTargetSecurityGroupsResponse"
    "fixture/DescribeMountTargetSecurityGroupsResponse.proto"
    efs
    (Proxy :: Proxy DescribeMountTargetSecurityGroups)

responseModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroupsResponse -> TestTree
responseModifyMountTargetSecurityGroups = res
    "ModifyMountTargetSecurityGroupsResponse"
    "fixture/ModifyMountTargetSecurityGroupsResponse.proto"
    efs
    (Proxy :: Proxy ModifyMountTargetSecurityGroups)

responseCreateFileSystem :: FileSystemDescription -> TestTree
responseCreateFileSystem = res
    "CreateFileSystemResponse"
    "fixture/CreateFileSystemResponse.proto"
    efs
    (Proxy :: Proxy CreateFileSystem)

responseDeleteFileSystem :: DeleteFileSystemResponse -> TestTree
responseDeleteFileSystem = res
    "DeleteFileSystemResponse"
    "fixture/DeleteFileSystemResponse.proto"
    efs
    (Proxy :: Proxy DeleteFileSystem)

responseCreateMountTarget :: MountTargetDescription -> TestTree
responseCreateMountTarget = res
    "CreateMountTargetResponse"
    "fixture/CreateMountTargetResponse.proto"
    efs
    (Proxy :: Proxy CreateMountTarget)
