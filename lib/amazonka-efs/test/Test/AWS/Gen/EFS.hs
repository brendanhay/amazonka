{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EFS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestCreateAccessPoint $
--             createAccessPoint
--
--         , requestDescribeMountTargets $
--             describeMountTargets
--
--         , requestDeleteFileSystemPolicy $
--             deleteFileSystemPolicy
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestPutFileSystemPolicy $
--             putFileSystemPolicy
--
--         , requestDescribeFileSystems $
--             describeFileSystems
--
--         , requestDeleteMountTarget $
--             deleteMountTarget
--
--         , requestDescribeMountTargetSecurityGroups $
--             describeMountTargetSecurityGroups
--
--         , requestDescribeAccessPoints $
--             describeAccessPoints
--
--         , requestModifyMountTargetSecurityGroups $
--             modifyMountTargetSecurityGroups
--
--         , requestCreateFileSystem $
--             createFileSystem
--
--         , requestPutLifecycleConfiguration $
--             putLifecycleConfiguration
--
--         , requestPutBackupPolicy $
--             putBackupPolicy
--
--         , requestDeleteFileSystem $
--             deleteFileSystem
--
--         , requestUpdateFileSystem $
--             updateFileSystem
--
--         , requestCreateMountTarget $
--             createMountTarget
--
--         , requestTagResource $
--             tagResource
--
--         , requestDescribeBackupPolicy $
--             describeBackupPolicy
--
--         , requestDescribeLifecycleConfiguration $
--             describeLifecycleConfiguration
--
--         , requestUntagResource $
--             untagResource
--
--         , requestDescribeFileSystemPolicy $
--             describeFileSystemPolicy
--
--         , requestDeleteAccessPoint $
--             deleteAccessPoint
--
--           ]

--     , testGroup "response"
--         [ responseCreateAccessPoint $
--             accessPointDescription
--
--         , responseDescribeMountTargets $
--             describeMountTargetsResponse
--
--         , responseDeleteFileSystemPolicy $
--             deleteFileSystemPolicyResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responsePutFileSystemPolicy $
--             fileSystemPolicyDescription
--
--         , responseDescribeFileSystems $
--             describeFileSystemsResponse
--
--         , responseDeleteMountTarget $
--             deleteMountTargetResponse
--
--         , responseDescribeMountTargetSecurityGroups $
--             describeMountTargetSecurityGroupsResponse
--
--         , responseDescribeAccessPoints $
--             describeAccessPointsResponse
--
--         , responseModifyMountTargetSecurityGroups $
--             modifyMountTargetSecurityGroupsResponse
--
--         , responseCreateFileSystem $
--             fileSystemDescription
--
--         , responsePutLifecycleConfiguration $
--             lifecycleConfigurationDescription
--
--         , responsePutBackupPolicy $
--             backupPolicyDescription
--
--         , responseDeleteFileSystem $
--             deleteFileSystemResponse
--
--         , responseUpdateFileSystem $
--             fileSystemDescription
--
--         , responseCreateMountTarget $
--             mountTargetDescription
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseDescribeBackupPolicy $
--             backupPolicyDescription
--
--         , responseDescribeLifecycleConfiguration $
--             lifecycleConfigurationDescription
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseDescribeFileSystemPolicy $
--             fileSystemPolicyDescription
--
--         , responseDeleteAccessPoint $
--             deleteAccessPointResponse
--
--           ]
--     ]

-- Requests

requestCreateAccessPoint :: CreateAccessPoint -> TestTree
requestCreateAccessPoint =
  req
    "CreateAccessPoint"
    "fixture/CreateAccessPoint.yaml"

requestDescribeMountTargets :: DescribeMountTargets -> TestTree
requestDescribeMountTargets =
  req
    "DescribeMountTargets"
    "fixture/DescribeMountTargets.yaml"

requestDeleteFileSystemPolicy :: DeleteFileSystemPolicy -> TestTree
requestDeleteFileSystemPolicy =
  req
    "DeleteFileSystemPolicy"
    "fixture/DeleteFileSystemPolicy.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutFileSystemPolicy :: PutFileSystemPolicy -> TestTree
requestPutFileSystemPolicy =
  req
    "PutFileSystemPolicy"
    "fixture/PutFileSystemPolicy.yaml"

requestDescribeFileSystems :: DescribeFileSystems -> TestTree
requestDescribeFileSystems =
  req
    "DescribeFileSystems"
    "fixture/DescribeFileSystems.yaml"

requestDeleteMountTarget :: DeleteMountTarget -> TestTree
requestDeleteMountTarget =
  req
    "DeleteMountTarget"
    "fixture/DeleteMountTarget.yaml"

requestDescribeMountTargetSecurityGroups :: DescribeMountTargetSecurityGroups -> TestTree
requestDescribeMountTargetSecurityGroups =
  req
    "DescribeMountTargetSecurityGroups"
    "fixture/DescribeMountTargetSecurityGroups.yaml"

requestDescribeAccessPoints :: DescribeAccessPoints -> TestTree
requestDescribeAccessPoints =
  req
    "DescribeAccessPoints"
    "fixture/DescribeAccessPoints.yaml"

requestModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroups -> TestTree
requestModifyMountTargetSecurityGroups =
  req
    "ModifyMountTargetSecurityGroups"
    "fixture/ModifyMountTargetSecurityGroups.yaml"

requestCreateFileSystem :: CreateFileSystem -> TestTree
requestCreateFileSystem =
  req
    "CreateFileSystem"
    "fixture/CreateFileSystem.yaml"

requestPutLifecycleConfiguration :: PutLifecycleConfiguration -> TestTree
requestPutLifecycleConfiguration =
  req
    "PutLifecycleConfiguration"
    "fixture/PutLifecycleConfiguration.yaml"

requestPutBackupPolicy :: PutBackupPolicy -> TestTree
requestPutBackupPolicy =
  req
    "PutBackupPolicy"
    "fixture/PutBackupPolicy.yaml"

requestDeleteFileSystem :: DeleteFileSystem -> TestTree
requestDeleteFileSystem =
  req
    "DeleteFileSystem"
    "fixture/DeleteFileSystem.yaml"

requestUpdateFileSystem :: UpdateFileSystem -> TestTree
requestUpdateFileSystem =
  req
    "UpdateFileSystem"
    "fixture/UpdateFileSystem.yaml"

requestCreateMountTarget :: CreateMountTarget -> TestTree
requestCreateMountTarget =
  req
    "CreateMountTarget"
    "fixture/CreateMountTarget.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeBackupPolicy :: DescribeBackupPolicy -> TestTree
requestDescribeBackupPolicy =
  req
    "DescribeBackupPolicy"
    "fixture/DescribeBackupPolicy.yaml"

requestDescribeLifecycleConfiguration :: DescribeLifecycleConfiguration -> TestTree
requestDescribeLifecycleConfiguration =
  req
    "DescribeLifecycleConfiguration"
    "fixture/DescribeLifecycleConfiguration.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeFileSystemPolicy :: DescribeFileSystemPolicy -> TestTree
requestDescribeFileSystemPolicy =
  req
    "DescribeFileSystemPolicy"
    "fixture/DescribeFileSystemPolicy.yaml"

requestDeleteAccessPoint :: DeleteAccessPoint -> TestTree
requestDeleteAccessPoint =
  req
    "DeleteAccessPoint"
    "fixture/DeleteAccessPoint.yaml"

-- Responses

responseCreateAccessPoint :: AccessPointDescription -> TestTree
responseCreateAccessPoint =
  res
    "CreateAccessPointResponse"
    "fixture/CreateAccessPointResponse.proto"
    efs
    (Proxy :: Proxy CreateAccessPoint)

responseDescribeMountTargets :: DescribeMountTargetsResponse -> TestTree
responseDescribeMountTargets =
  res
    "DescribeMountTargetsResponse"
    "fixture/DescribeMountTargetsResponse.proto"
    efs
    (Proxy :: Proxy DescribeMountTargets)

responseDeleteFileSystemPolicy :: DeleteFileSystemPolicyResponse -> TestTree
responseDeleteFileSystemPolicy =
  res
    "DeleteFileSystemPolicyResponse"
    "fixture/DeleteFileSystemPolicyResponse.proto"
    efs
    (Proxy :: Proxy DeleteFileSystemPolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    efs
    (Proxy :: Proxy ListTagsForResource)

responsePutFileSystemPolicy :: FileSystemPolicyDescription -> TestTree
responsePutFileSystemPolicy =
  res
    "PutFileSystemPolicyResponse"
    "fixture/PutFileSystemPolicyResponse.proto"
    efs
    (Proxy :: Proxy PutFileSystemPolicy)

responseDescribeFileSystems :: DescribeFileSystemsResponse -> TestTree
responseDescribeFileSystems =
  res
    "DescribeFileSystemsResponse"
    "fixture/DescribeFileSystemsResponse.proto"
    efs
    (Proxy :: Proxy DescribeFileSystems)

responseDeleteMountTarget :: DeleteMountTargetResponse -> TestTree
responseDeleteMountTarget =
  res
    "DeleteMountTargetResponse"
    "fixture/DeleteMountTargetResponse.proto"
    efs
    (Proxy :: Proxy DeleteMountTarget)

responseDescribeMountTargetSecurityGroups :: DescribeMountTargetSecurityGroupsResponse -> TestTree
responseDescribeMountTargetSecurityGroups =
  res
    "DescribeMountTargetSecurityGroupsResponse"
    "fixture/DescribeMountTargetSecurityGroupsResponse.proto"
    efs
    (Proxy :: Proxy DescribeMountTargetSecurityGroups)

responseDescribeAccessPoints :: DescribeAccessPointsResponse -> TestTree
responseDescribeAccessPoints =
  res
    "DescribeAccessPointsResponse"
    "fixture/DescribeAccessPointsResponse.proto"
    efs
    (Proxy :: Proxy DescribeAccessPoints)

responseModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroupsResponse -> TestTree
responseModifyMountTargetSecurityGroups =
  res
    "ModifyMountTargetSecurityGroupsResponse"
    "fixture/ModifyMountTargetSecurityGroupsResponse.proto"
    efs
    (Proxy :: Proxy ModifyMountTargetSecurityGroups)

responseCreateFileSystem :: FileSystemDescription -> TestTree
responseCreateFileSystem =
  res
    "CreateFileSystemResponse"
    "fixture/CreateFileSystemResponse.proto"
    efs
    (Proxy :: Proxy CreateFileSystem)

responsePutLifecycleConfiguration :: LifecycleConfigurationDescription -> TestTree
responsePutLifecycleConfiguration =
  res
    "PutLifecycleConfigurationResponse"
    "fixture/PutLifecycleConfigurationResponse.proto"
    efs
    (Proxy :: Proxy PutLifecycleConfiguration)

responsePutBackupPolicy :: BackupPolicyDescription -> TestTree
responsePutBackupPolicy =
  res
    "PutBackupPolicyResponse"
    "fixture/PutBackupPolicyResponse.proto"
    efs
    (Proxy :: Proxy PutBackupPolicy)

responseDeleteFileSystem :: DeleteFileSystemResponse -> TestTree
responseDeleteFileSystem =
  res
    "DeleteFileSystemResponse"
    "fixture/DeleteFileSystemResponse.proto"
    efs
    (Proxy :: Proxy DeleteFileSystem)

responseUpdateFileSystem :: FileSystemDescription -> TestTree
responseUpdateFileSystem =
  res
    "UpdateFileSystemResponse"
    "fixture/UpdateFileSystemResponse.proto"
    efs
    (Proxy :: Proxy UpdateFileSystem)

responseCreateMountTarget :: MountTargetDescription -> TestTree
responseCreateMountTarget =
  res
    "CreateMountTargetResponse"
    "fixture/CreateMountTargetResponse.proto"
    efs
    (Proxy :: Proxy CreateMountTarget)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    efs
    (Proxy :: Proxy TagResource)

responseDescribeBackupPolicy :: BackupPolicyDescription -> TestTree
responseDescribeBackupPolicy =
  res
    "DescribeBackupPolicyResponse"
    "fixture/DescribeBackupPolicyResponse.proto"
    efs
    (Proxy :: Proxy DescribeBackupPolicy)

responseDescribeLifecycleConfiguration :: LifecycleConfigurationDescription -> TestTree
responseDescribeLifecycleConfiguration =
  res
    "DescribeLifecycleConfigurationResponse"
    "fixture/DescribeLifecycleConfigurationResponse.proto"
    efs
    (Proxy :: Proxy DescribeLifecycleConfiguration)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    efs
    (Proxy :: Proxy UntagResource)

responseDescribeFileSystemPolicy :: FileSystemPolicyDescription -> TestTree
responseDescribeFileSystemPolicy =
  res
    "DescribeFileSystemPolicyResponse"
    "fixture/DescribeFileSystemPolicyResponse.proto"
    efs
    (Proxy :: Proxy DescribeFileSystemPolicy)

responseDeleteAccessPoint :: DeleteAccessPointResponse -> TestTree
responseDeleteAccessPoint =
  res
    "DeleteAccessPointResponse"
    "fixture/DeleteAccessPointResponse.proto"
    efs
    (Proxy :: Proxy DeleteAccessPoint)
