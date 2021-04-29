{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EFS
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestPutBackupPolicy $
--             newPutBackupPolicy
--
--         , requestPutLifecycleConfiguration $
--             newPutLifecycleConfiguration
--
--         , requestDeleteAccessPoint $
--             newDeleteAccessPoint
--
--         , requestModifyMountTargetSecurityGroups $
--             newModifyMountTargetSecurityGroups
--
--         , requestDescribeFileSystemPolicy $
--             newDescribeFileSystemPolicy
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateMountTarget $
--             newCreateMountTarget
--
--         , requestDeleteMountTarget $
--             newDeleteMountTarget
--
--         , requestDescribeFileSystems $
--             newDescribeFileSystems
--
--         , requestDescribeMountTargets $
--             newDescribeMountTargets
--
--         , requestDeleteFileSystemPolicy $
--             newDeleteFileSystemPolicy
--
--         , requestCreateFileSystem $
--             newCreateFileSystem
--
--         , requestCreateAccessPoint $
--             newCreateAccessPoint
--
--         , requestDescribeAccessPoints $
--             newDescribeAccessPoints
--
--         , requestDescribeLifecycleConfiguration $
--             newDescribeLifecycleConfiguration
--
--         , requestDescribeMountTargetSecurityGroups $
--             newDescribeMountTargetSecurityGroups
--
--         , requestDescribeBackupPolicy $
--             newDescribeBackupPolicy
--
--         , requestUpdateFileSystem $
--             newUpdateFileSystem
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteFileSystem $
--             newDeleteFileSystem
--
--         , requestPutFileSystemPolicy $
--             newPutFileSystemPolicy
--
--           ]

--     , testGroup "response"
--         [ responsePutBackupPolicy $
--             newBackupPolicyDescription
--
--         , responsePutLifecycleConfiguration $
--             newLifecycleConfigurationDescription
--
--         , responseDeleteAccessPoint $
--             newDeleteAccessPointResponse
--
--         , responseModifyMountTargetSecurityGroups $
--             newModifyMountTargetSecurityGroupsResponse
--
--         , responseDescribeFileSystemPolicy $
--             newFileSystemPolicyDescription
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateMountTarget $
--             newMountTargetDescription
--
--         , responseDeleteMountTarget $
--             newDeleteMountTargetResponse
--
--         , responseDescribeFileSystems $
--             newDescribeFileSystemsResponse
--
--         , responseDescribeMountTargets $
--             newDescribeMountTargetsResponse
--
--         , responseDeleteFileSystemPolicy $
--             newDeleteFileSystemPolicyResponse
--
--         , responseCreateFileSystem $
--             newFileSystemDescription
--
--         , responseCreateAccessPoint $
--             newAccessPointDescription
--
--         , responseDescribeAccessPoints $
--             newDescribeAccessPointsResponse
--
--         , responseDescribeLifecycleConfiguration $
--             newLifecycleConfigurationDescription
--
--         , responseDescribeMountTargetSecurityGroups $
--             newDescribeMountTargetSecurityGroupsResponse
--
--         , responseDescribeBackupPolicy $
--             newBackupPolicyDescription
--
--         , responseUpdateFileSystem $
--             newFileSystemDescription
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteFileSystem $
--             newDeleteFileSystemResponse
--
--         , responsePutFileSystemPolicy $
--             newFileSystemPolicyDescription
--
--           ]
--     ]

-- Requests

requestPutBackupPolicy :: PutBackupPolicy -> TestTree
requestPutBackupPolicy =
  req
    "PutBackupPolicy"
    "fixture/PutBackupPolicy.yaml"

requestPutLifecycleConfiguration :: PutLifecycleConfiguration -> TestTree
requestPutLifecycleConfiguration =
  req
    "PutLifecycleConfiguration"
    "fixture/PutLifecycleConfiguration.yaml"

requestDeleteAccessPoint :: DeleteAccessPoint -> TestTree
requestDeleteAccessPoint =
  req
    "DeleteAccessPoint"
    "fixture/DeleteAccessPoint.yaml"

requestModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroups -> TestTree
requestModifyMountTargetSecurityGroups =
  req
    "ModifyMountTargetSecurityGroups"
    "fixture/ModifyMountTargetSecurityGroups.yaml"

requestDescribeFileSystemPolicy :: DescribeFileSystemPolicy -> TestTree
requestDescribeFileSystemPolicy =
  req
    "DescribeFileSystemPolicy"
    "fixture/DescribeFileSystemPolicy.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateMountTarget :: CreateMountTarget -> TestTree
requestCreateMountTarget =
  req
    "CreateMountTarget"
    "fixture/CreateMountTarget.yaml"

requestDeleteMountTarget :: DeleteMountTarget -> TestTree
requestDeleteMountTarget =
  req
    "DeleteMountTarget"
    "fixture/DeleteMountTarget.yaml"

requestDescribeFileSystems :: DescribeFileSystems -> TestTree
requestDescribeFileSystems =
  req
    "DescribeFileSystems"
    "fixture/DescribeFileSystems.yaml"

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

requestCreateFileSystem :: CreateFileSystem -> TestTree
requestCreateFileSystem =
  req
    "CreateFileSystem"
    "fixture/CreateFileSystem.yaml"

requestCreateAccessPoint :: CreateAccessPoint -> TestTree
requestCreateAccessPoint =
  req
    "CreateAccessPoint"
    "fixture/CreateAccessPoint.yaml"

requestDescribeAccessPoints :: DescribeAccessPoints -> TestTree
requestDescribeAccessPoints =
  req
    "DescribeAccessPoints"
    "fixture/DescribeAccessPoints.yaml"

requestDescribeLifecycleConfiguration :: DescribeLifecycleConfiguration -> TestTree
requestDescribeLifecycleConfiguration =
  req
    "DescribeLifecycleConfiguration"
    "fixture/DescribeLifecycleConfiguration.yaml"

requestDescribeMountTargetSecurityGroups :: DescribeMountTargetSecurityGroups -> TestTree
requestDescribeMountTargetSecurityGroups =
  req
    "DescribeMountTargetSecurityGroups"
    "fixture/DescribeMountTargetSecurityGroups.yaml"

requestDescribeBackupPolicy :: DescribeBackupPolicy -> TestTree
requestDescribeBackupPolicy =
  req
    "DescribeBackupPolicy"
    "fixture/DescribeBackupPolicy.yaml"

requestUpdateFileSystem :: UpdateFileSystem -> TestTree
requestUpdateFileSystem =
  req
    "UpdateFileSystem"
    "fixture/UpdateFileSystem.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteFileSystem :: DeleteFileSystem -> TestTree
requestDeleteFileSystem =
  req
    "DeleteFileSystem"
    "fixture/DeleteFileSystem.yaml"

requestPutFileSystemPolicy :: PutFileSystemPolicy -> TestTree
requestPutFileSystemPolicy =
  req
    "PutFileSystemPolicy"
    "fixture/PutFileSystemPolicy.yaml"

-- Responses

responsePutBackupPolicy :: BackupPolicyDescription -> TestTree
responsePutBackupPolicy =
  res
    "PutBackupPolicyResponse"
    "fixture/PutBackupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutBackupPolicy)

responsePutLifecycleConfiguration :: LifecycleConfigurationDescription -> TestTree
responsePutLifecycleConfiguration =
  res
    "PutLifecycleConfigurationResponse"
    "fixture/PutLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutLifecycleConfiguration)

responseDeleteAccessPoint :: DeleteAccessPointResponse -> TestTree
responseDeleteAccessPoint =
  res
    "DeleteAccessPointResponse"
    "fixture/DeleteAccessPointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccessPoint)

responseModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroupsResponse -> TestTree
responseModifyMountTargetSecurityGroups =
  res
    "ModifyMountTargetSecurityGroupsResponse"
    "fixture/ModifyMountTargetSecurityGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyMountTargetSecurityGroups)

responseDescribeFileSystemPolicy :: FileSystemPolicyDescription -> TestTree
responseDescribeFileSystemPolicy =
  res
    "DescribeFileSystemPolicyResponse"
    "fixture/DescribeFileSystemPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFileSystemPolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseCreateMountTarget :: MountTargetDescription -> TestTree
responseCreateMountTarget =
  res
    "CreateMountTargetResponse"
    "fixture/CreateMountTargetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMountTarget)

responseDeleteMountTarget :: DeleteMountTargetResponse -> TestTree
responseDeleteMountTarget =
  res
    "DeleteMountTargetResponse"
    "fixture/DeleteMountTargetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMountTarget)

responseDescribeFileSystems :: DescribeFileSystemsResponse -> TestTree
responseDescribeFileSystems =
  res
    "DescribeFileSystemsResponse"
    "fixture/DescribeFileSystemsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFileSystems)

responseDescribeMountTargets :: DescribeMountTargetsResponse -> TestTree
responseDescribeMountTargets =
  res
    "DescribeMountTargetsResponse"
    "fixture/DescribeMountTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMountTargets)

responseDeleteFileSystemPolicy :: DeleteFileSystemPolicyResponse -> TestTree
responseDeleteFileSystemPolicy =
  res
    "DeleteFileSystemPolicyResponse"
    "fixture/DeleteFileSystemPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFileSystemPolicy)

responseCreateFileSystem :: FileSystemDescription -> TestTree
responseCreateFileSystem =
  res
    "CreateFileSystemResponse"
    "fixture/CreateFileSystemResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFileSystem)

responseCreateAccessPoint :: AccessPointDescription -> TestTree
responseCreateAccessPoint =
  res
    "CreateAccessPointResponse"
    "fixture/CreateAccessPointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAccessPoint)

responseDescribeAccessPoints :: DescribeAccessPointsResponse -> TestTree
responseDescribeAccessPoints =
  res
    "DescribeAccessPointsResponse"
    "fixture/DescribeAccessPointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccessPoints)

responseDescribeLifecycleConfiguration :: LifecycleConfigurationDescription -> TestTree
responseDescribeLifecycleConfiguration =
  res
    "DescribeLifecycleConfigurationResponse"
    "fixture/DescribeLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLifecycleConfiguration)

responseDescribeMountTargetSecurityGroups :: DescribeMountTargetSecurityGroupsResponse -> TestTree
responseDescribeMountTargetSecurityGroups =
  res
    "DescribeMountTargetSecurityGroupsResponse"
    "fixture/DescribeMountTargetSecurityGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMountTargetSecurityGroups)

responseDescribeBackupPolicy :: BackupPolicyDescription -> TestTree
responseDescribeBackupPolicy =
  res
    "DescribeBackupPolicyResponse"
    "fixture/DescribeBackupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBackupPolicy)

responseUpdateFileSystem :: FileSystemDescription -> TestTree
responseUpdateFileSystem =
  res
    "UpdateFileSystemResponse"
    "fixture/UpdateFileSystemResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFileSystem)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDeleteFileSystem :: DeleteFileSystemResponse -> TestTree
responseDeleteFileSystem =
  res
    "DeleteFileSystemResponse"
    "fixture/DeleteFileSystemResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFileSystem)

responsePutFileSystemPolicy :: FileSystemPolicyDescription -> TestTree
responsePutFileSystemPolicy =
  res
    "PutFileSystemPolicyResponse"
    "fixture/PutFileSystemPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutFileSystemPolicy)
