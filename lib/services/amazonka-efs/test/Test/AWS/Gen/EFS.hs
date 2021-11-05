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

import qualified Data.Proxy as Proxy
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
--             newCreateAccessPoint
--
--         , requestDescribeAccountPreferences $
--             newDescribeAccountPreferences
--
--         , requestDescribeMountTargets $
--             newDescribeMountTargets
--
--         , requestDeleteFileSystemPolicy $
--             newDeleteFileSystemPolicy
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutFileSystemPolicy $
--             newPutFileSystemPolicy
--
--         , requestDescribeFileSystems $
--             newDescribeFileSystems
--
--         , requestDeleteMountTarget $
--             newDeleteMountTarget
--
--         , requestPutAccountPreferences $
--             newPutAccountPreferences
--
--         , requestDescribeMountTargetSecurityGroups $
--             newDescribeMountTargetSecurityGroups
--
--         , requestDescribeAccessPoints $
--             newDescribeAccessPoints
--
--         , requestModifyMountTargetSecurityGroups $
--             newModifyMountTargetSecurityGroups
--
--         , requestCreateFileSystem $
--             newCreateFileSystem
--
--         , requestPutLifecycleConfiguration $
--             newPutLifecycleConfiguration
--
--         , requestPutBackupPolicy $
--             newPutBackupPolicy
--
--         , requestDeleteFileSystem $
--             newDeleteFileSystem
--
--         , requestUpdateFileSystem $
--             newUpdateFileSystem
--
--         , requestCreateMountTarget $
--             newCreateMountTarget
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeBackupPolicy $
--             newDescribeBackupPolicy
--
--         , requestDescribeLifecycleConfiguration $
--             newDescribeLifecycleConfiguration
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeFileSystemPolicy $
--             newDescribeFileSystemPolicy
--
--         , requestDeleteAccessPoint $
--             newDeleteAccessPoint
--
--           ]

--     , testGroup "response"
--         [ responseCreateAccessPoint $
--             newAccessPointDescription
--
--         , responseDescribeAccountPreferences $
--             newDescribeAccountPreferencesResponse
--
--         , responseDescribeMountTargets $
--             newDescribeMountTargetsResponse
--
--         , responseDeleteFileSystemPolicy $
--             newDeleteFileSystemPolicyResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutFileSystemPolicy $
--             newFileSystemPolicyDescription
--
--         , responseDescribeFileSystems $
--             newDescribeFileSystemsResponse
--
--         , responseDeleteMountTarget $
--             newDeleteMountTargetResponse
--
--         , responsePutAccountPreferences $
--             newPutAccountPreferencesResponse
--
--         , responseDescribeMountTargetSecurityGroups $
--             newDescribeMountTargetSecurityGroupsResponse
--
--         , responseDescribeAccessPoints $
--             newDescribeAccessPointsResponse
--
--         , responseModifyMountTargetSecurityGroups $
--             newModifyMountTargetSecurityGroupsResponse
--
--         , responseCreateFileSystem $
--             newFileSystemDescription
--
--         , responsePutLifecycleConfiguration $
--             newLifecycleConfigurationDescription
--
--         , responsePutBackupPolicy $
--             newBackupPolicyDescription
--
--         , responseDeleteFileSystem $
--             newDeleteFileSystemResponse
--
--         , responseUpdateFileSystem $
--             newFileSystemDescription
--
--         , responseCreateMountTarget $
--             newMountTargetDescription
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeBackupPolicy $
--             newBackupPolicyDescription
--
--         , responseDescribeLifecycleConfiguration $
--             newLifecycleConfigurationDescription
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeFileSystemPolicy $
--             newFileSystemPolicyDescription
--
--         , responseDeleteAccessPoint $
--             newDeleteAccessPointResponse
--
--           ]
--     ]

-- Requests

requestCreateAccessPoint :: CreateAccessPoint -> TestTree
requestCreateAccessPoint =
  req
    "CreateAccessPoint"
    "fixture/CreateAccessPoint.yaml"

requestDescribeAccountPreferences :: DescribeAccountPreferences -> TestTree
requestDescribeAccountPreferences =
  req
    "DescribeAccountPreferences"
    "fixture/DescribeAccountPreferences.yaml"

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

requestPutAccountPreferences :: PutAccountPreferences -> TestTree
requestPutAccountPreferences =
  req
    "PutAccountPreferences"
    "fixture/PutAccountPreferences.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccessPoint)

responseDescribeAccountPreferences :: DescribeAccountPreferencesResponse -> TestTree
responseDescribeAccountPreferences =
  res
    "DescribeAccountPreferencesResponse"
    "fixture/DescribeAccountPreferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountPreferences)

responseDescribeMountTargets :: DescribeMountTargetsResponse -> TestTree
responseDescribeMountTargets =
  res
    "DescribeMountTargetsResponse"
    "fixture/DescribeMountTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMountTargets)

responseDeleteFileSystemPolicy :: DeleteFileSystemPolicyResponse -> TestTree
responseDeleteFileSystemPolicy =
  res
    "DeleteFileSystemPolicyResponse"
    "fixture/DeleteFileSystemPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFileSystemPolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutFileSystemPolicy :: FileSystemPolicyDescription -> TestTree
responsePutFileSystemPolicy =
  res
    "PutFileSystemPolicyResponse"
    "fixture/PutFileSystemPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFileSystemPolicy)

responseDescribeFileSystems :: DescribeFileSystemsResponse -> TestTree
responseDescribeFileSystems =
  res
    "DescribeFileSystemsResponse"
    "fixture/DescribeFileSystemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFileSystems)

responseDeleteMountTarget :: DeleteMountTargetResponse -> TestTree
responseDeleteMountTarget =
  res
    "DeleteMountTargetResponse"
    "fixture/DeleteMountTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMountTarget)

responsePutAccountPreferences :: PutAccountPreferencesResponse -> TestTree
responsePutAccountPreferences =
  res
    "PutAccountPreferencesResponse"
    "fixture/PutAccountPreferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountPreferences)

responseDescribeMountTargetSecurityGroups :: DescribeMountTargetSecurityGroupsResponse -> TestTree
responseDescribeMountTargetSecurityGroups =
  res
    "DescribeMountTargetSecurityGroupsResponse"
    "fixture/DescribeMountTargetSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMountTargetSecurityGroups)

responseDescribeAccessPoints :: DescribeAccessPointsResponse -> TestTree
responseDescribeAccessPoints =
  res
    "DescribeAccessPointsResponse"
    "fixture/DescribeAccessPointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccessPoints)

responseModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroupsResponse -> TestTree
responseModifyMountTargetSecurityGroups =
  res
    "ModifyMountTargetSecurityGroupsResponse"
    "fixture/ModifyMountTargetSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyMountTargetSecurityGroups)

responseCreateFileSystem :: FileSystemDescription -> TestTree
responseCreateFileSystem =
  res
    "CreateFileSystemResponse"
    "fixture/CreateFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFileSystem)

responsePutLifecycleConfiguration :: LifecycleConfigurationDescription -> TestTree
responsePutLifecycleConfiguration =
  res
    "PutLifecycleConfigurationResponse"
    "fixture/PutLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLifecycleConfiguration)

responsePutBackupPolicy :: BackupPolicyDescription -> TestTree
responsePutBackupPolicy =
  res
    "PutBackupPolicyResponse"
    "fixture/PutBackupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBackupPolicy)

responseDeleteFileSystem :: DeleteFileSystemResponse -> TestTree
responseDeleteFileSystem =
  res
    "DeleteFileSystemResponse"
    "fixture/DeleteFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFileSystem)

responseUpdateFileSystem :: FileSystemDescription -> TestTree
responseUpdateFileSystem =
  res
    "UpdateFileSystemResponse"
    "fixture/UpdateFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFileSystem)

responseCreateMountTarget :: MountTargetDescription -> TestTree
responseCreateMountTarget =
  res
    "CreateMountTargetResponse"
    "fixture/CreateMountTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMountTarget)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDescribeBackupPolicy :: BackupPolicyDescription -> TestTree
responseDescribeBackupPolicy =
  res
    "DescribeBackupPolicyResponse"
    "fixture/DescribeBackupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBackupPolicy)

responseDescribeLifecycleConfiguration :: LifecycleConfigurationDescription -> TestTree
responseDescribeLifecycleConfiguration =
  res
    "DescribeLifecycleConfigurationResponse"
    "fixture/DescribeLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLifecycleConfiguration)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDescribeFileSystemPolicy :: FileSystemPolicyDescription -> TestTree
responseDescribeFileSystemPolicy =
  res
    "DescribeFileSystemPolicyResponse"
    "fixture/DescribeFileSystemPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFileSystemPolicy)

responseDeleteAccessPoint :: DeleteAccessPointResponse -> TestTree
responseDeleteAccessPoint =
  res
    "DeleteAccessPointResponse"
    "fixture/DeleteAccessPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessPoint)
