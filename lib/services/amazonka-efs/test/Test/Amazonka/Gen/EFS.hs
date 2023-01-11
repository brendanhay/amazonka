{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.EFS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.EFS where

import Amazonka.EFS
import qualified Data.Proxy as Proxy
import Test.Amazonka.EFS.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
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
--         , requestCreateFileSystem $
--             newCreateFileSystem
--
--         , requestCreateMountTarget $
--             newCreateMountTarget
--
--         , requestCreateReplicationConfiguration $
--             newCreateReplicationConfiguration
--
--         , requestDeleteAccessPoint $
--             newDeleteAccessPoint
--
--         , requestDeleteFileSystem $
--             newDeleteFileSystem
--
--         , requestDeleteFileSystemPolicy $
--             newDeleteFileSystemPolicy
--
--         , requestDeleteMountTarget $
--             newDeleteMountTarget
--
--         , requestDeleteReplicationConfiguration $
--             newDeleteReplicationConfiguration
--
--         , requestDescribeAccessPoints $
--             newDescribeAccessPoints
--
--         , requestDescribeAccountPreferences $
--             newDescribeAccountPreferences
--
--         , requestDescribeBackupPolicy $
--             newDescribeBackupPolicy
--
--         , requestDescribeFileSystemPolicy $
--             newDescribeFileSystemPolicy
--
--         , requestDescribeFileSystems $
--             newDescribeFileSystems
--
--         , requestDescribeLifecycleConfiguration $
--             newDescribeLifecycleConfiguration
--
--         , requestDescribeMountTargetSecurityGroups $
--             newDescribeMountTargetSecurityGroups
--
--         , requestDescribeMountTargets $
--             newDescribeMountTargets
--
--         , requestDescribeReplicationConfigurations $
--             newDescribeReplicationConfigurations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestModifyMountTargetSecurityGroups $
--             newModifyMountTargetSecurityGroups
--
--         , requestPutAccountPreferences $
--             newPutAccountPreferences
--
--         , requestPutBackupPolicy $
--             newPutBackupPolicy
--
--         , requestPutFileSystemPolicy $
--             newPutFileSystemPolicy
--
--         , requestPutLifecycleConfiguration $
--             newPutLifecycleConfiguration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateFileSystem $
--             newUpdateFileSystem
--
--           ]

--     , testGroup "response"
--         [ responseCreateAccessPoint $
--             newAccessPointDescription
--
--         , responseCreateFileSystem $
--             newFileSystemDescription
--
--         , responseCreateMountTarget $
--             newMountTargetDescription
--
--         , responseCreateReplicationConfiguration $
--             newReplicationConfigurationDescription
--
--         , responseDeleteAccessPoint $
--             newDeleteAccessPointResponse
--
--         , responseDeleteFileSystem $
--             newDeleteFileSystemResponse
--
--         , responseDeleteFileSystemPolicy $
--             newDeleteFileSystemPolicyResponse
--
--         , responseDeleteMountTarget $
--             newDeleteMountTargetResponse
--
--         , responseDeleteReplicationConfiguration $
--             newDeleteReplicationConfigurationResponse
--
--         , responseDescribeAccessPoints $
--             newDescribeAccessPointsResponse
--
--         , responseDescribeAccountPreferences $
--             newDescribeAccountPreferencesResponse
--
--         , responseDescribeBackupPolicy $
--             newBackupPolicyDescription
--
--         , responseDescribeFileSystemPolicy $
--             newFileSystemPolicyDescription
--
--         , responseDescribeFileSystems $
--             newDescribeFileSystemsResponse
--
--         , responseDescribeLifecycleConfiguration $
--             newLifecycleConfigurationDescription
--
--         , responseDescribeMountTargetSecurityGroups $
--             newDescribeMountTargetSecurityGroupsResponse
--
--         , responseDescribeMountTargets $
--             newDescribeMountTargetsResponse
--
--         , responseDescribeReplicationConfigurations $
--             newDescribeReplicationConfigurationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseModifyMountTargetSecurityGroups $
--             newModifyMountTargetSecurityGroupsResponse
--
--         , responsePutAccountPreferences $
--             newPutAccountPreferencesResponse
--
--         , responsePutBackupPolicy $
--             newBackupPolicyDescription
--
--         , responsePutFileSystemPolicy $
--             newFileSystemPolicyDescription
--
--         , responsePutLifecycleConfiguration $
--             newLifecycleConfigurationDescription
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateFileSystem $
--             newFileSystemDescription
--
--           ]
--     ]

-- Requests

requestCreateAccessPoint :: CreateAccessPoint -> TestTree
requestCreateAccessPoint =
  req
    "CreateAccessPoint"
    "fixture/CreateAccessPoint.yaml"

requestCreateFileSystem :: CreateFileSystem -> TestTree
requestCreateFileSystem =
  req
    "CreateFileSystem"
    "fixture/CreateFileSystem.yaml"

requestCreateMountTarget :: CreateMountTarget -> TestTree
requestCreateMountTarget =
  req
    "CreateMountTarget"
    "fixture/CreateMountTarget.yaml"

requestCreateReplicationConfiguration :: CreateReplicationConfiguration -> TestTree
requestCreateReplicationConfiguration =
  req
    "CreateReplicationConfiguration"
    "fixture/CreateReplicationConfiguration.yaml"

requestDeleteAccessPoint :: DeleteAccessPoint -> TestTree
requestDeleteAccessPoint =
  req
    "DeleteAccessPoint"
    "fixture/DeleteAccessPoint.yaml"

requestDeleteFileSystem :: DeleteFileSystem -> TestTree
requestDeleteFileSystem =
  req
    "DeleteFileSystem"
    "fixture/DeleteFileSystem.yaml"

requestDeleteFileSystemPolicy :: DeleteFileSystemPolicy -> TestTree
requestDeleteFileSystemPolicy =
  req
    "DeleteFileSystemPolicy"
    "fixture/DeleteFileSystemPolicy.yaml"

requestDeleteMountTarget :: DeleteMountTarget -> TestTree
requestDeleteMountTarget =
  req
    "DeleteMountTarget"
    "fixture/DeleteMountTarget.yaml"

requestDeleteReplicationConfiguration :: DeleteReplicationConfiguration -> TestTree
requestDeleteReplicationConfiguration =
  req
    "DeleteReplicationConfiguration"
    "fixture/DeleteReplicationConfiguration.yaml"

requestDescribeAccessPoints :: DescribeAccessPoints -> TestTree
requestDescribeAccessPoints =
  req
    "DescribeAccessPoints"
    "fixture/DescribeAccessPoints.yaml"

requestDescribeAccountPreferences :: DescribeAccountPreferences -> TestTree
requestDescribeAccountPreferences =
  req
    "DescribeAccountPreferences"
    "fixture/DescribeAccountPreferences.yaml"

requestDescribeBackupPolicy :: DescribeBackupPolicy -> TestTree
requestDescribeBackupPolicy =
  req
    "DescribeBackupPolicy"
    "fixture/DescribeBackupPolicy.yaml"

requestDescribeFileSystemPolicy :: DescribeFileSystemPolicy -> TestTree
requestDescribeFileSystemPolicy =
  req
    "DescribeFileSystemPolicy"
    "fixture/DescribeFileSystemPolicy.yaml"

requestDescribeFileSystems :: DescribeFileSystems -> TestTree
requestDescribeFileSystems =
  req
    "DescribeFileSystems"
    "fixture/DescribeFileSystems.yaml"

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

requestDescribeMountTargets :: DescribeMountTargets -> TestTree
requestDescribeMountTargets =
  req
    "DescribeMountTargets"
    "fixture/DescribeMountTargets.yaml"

requestDescribeReplicationConfigurations :: DescribeReplicationConfigurations -> TestTree
requestDescribeReplicationConfigurations =
  req
    "DescribeReplicationConfigurations"
    "fixture/DescribeReplicationConfigurations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroups -> TestTree
requestModifyMountTargetSecurityGroups =
  req
    "ModifyMountTargetSecurityGroups"
    "fixture/ModifyMountTargetSecurityGroups.yaml"

requestPutAccountPreferences :: PutAccountPreferences -> TestTree
requestPutAccountPreferences =
  req
    "PutAccountPreferences"
    "fixture/PutAccountPreferences.yaml"

requestPutBackupPolicy :: PutBackupPolicy -> TestTree
requestPutBackupPolicy =
  req
    "PutBackupPolicy"
    "fixture/PutBackupPolicy.yaml"

requestPutFileSystemPolicy :: PutFileSystemPolicy -> TestTree
requestPutFileSystemPolicy =
  req
    "PutFileSystemPolicy"
    "fixture/PutFileSystemPolicy.yaml"

requestPutLifecycleConfiguration :: PutLifecycleConfiguration -> TestTree
requestPutLifecycleConfiguration =
  req
    "PutLifecycleConfiguration"
    "fixture/PutLifecycleConfiguration.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateFileSystem :: UpdateFileSystem -> TestTree
requestUpdateFileSystem =
  req
    "UpdateFileSystem"
    "fixture/UpdateFileSystem.yaml"

-- Responses

responseCreateAccessPoint :: AccessPointDescription -> TestTree
responseCreateAccessPoint =
  res
    "CreateAccessPointResponse"
    "fixture/CreateAccessPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccessPoint)

responseCreateFileSystem :: FileSystemDescription -> TestTree
responseCreateFileSystem =
  res
    "CreateFileSystemResponse"
    "fixture/CreateFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFileSystem)

responseCreateMountTarget :: MountTargetDescription -> TestTree
responseCreateMountTarget =
  res
    "CreateMountTargetResponse"
    "fixture/CreateMountTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMountTarget)

responseCreateReplicationConfiguration :: ReplicationConfigurationDescription -> TestTree
responseCreateReplicationConfiguration =
  res
    "CreateReplicationConfigurationResponse"
    "fixture/CreateReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplicationConfiguration)

responseDeleteAccessPoint :: DeleteAccessPointResponse -> TestTree
responseDeleteAccessPoint =
  res
    "DeleteAccessPointResponse"
    "fixture/DeleteAccessPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessPoint)

responseDeleteFileSystem :: DeleteFileSystemResponse -> TestTree
responseDeleteFileSystem =
  res
    "DeleteFileSystemResponse"
    "fixture/DeleteFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFileSystem)

responseDeleteFileSystemPolicy :: DeleteFileSystemPolicyResponse -> TestTree
responseDeleteFileSystemPolicy =
  res
    "DeleteFileSystemPolicyResponse"
    "fixture/DeleteFileSystemPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFileSystemPolicy)

responseDeleteMountTarget :: DeleteMountTargetResponse -> TestTree
responseDeleteMountTarget =
  res
    "DeleteMountTargetResponse"
    "fixture/DeleteMountTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMountTarget)

responseDeleteReplicationConfiguration :: DeleteReplicationConfigurationResponse -> TestTree
responseDeleteReplicationConfiguration =
  res
    "DeleteReplicationConfigurationResponse"
    "fixture/DeleteReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationConfiguration)

responseDescribeAccessPoints :: DescribeAccessPointsResponse -> TestTree
responseDescribeAccessPoints =
  res
    "DescribeAccessPointsResponse"
    "fixture/DescribeAccessPointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccessPoints)

responseDescribeAccountPreferences :: DescribeAccountPreferencesResponse -> TestTree
responseDescribeAccountPreferences =
  res
    "DescribeAccountPreferencesResponse"
    "fixture/DescribeAccountPreferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountPreferences)

responseDescribeBackupPolicy :: BackupPolicyDescription -> TestTree
responseDescribeBackupPolicy =
  res
    "DescribeBackupPolicyResponse"
    "fixture/DescribeBackupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBackupPolicy)

responseDescribeFileSystemPolicy :: FileSystemPolicyDescription -> TestTree
responseDescribeFileSystemPolicy =
  res
    "DescribeFileSystemPolicyResponse"
    "fixture/DescribeFileSystemPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFileSystemPolicy)

responseDescribeFileSystems :: DescribeFileSystemsResponse -> TestTree
responseDescribeFileSystems =
  res
    "DescribeFileSystemsResponse"
    "fixture/DescribeFileSystemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFileSystems)

responseDescribeLifecycleConfiguration :: LifecycleConfigurationDescription -> TestTree
responseDescribeLifecycleConfiguration =
  res
    "DescribeLifecycleConfigurationResponse"
    "fixture/DescribeLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLifecycleConfiguration)

responseDescribeMountTargetSecurityGroups :: DescribeMountTargetSecurityGroupsResponse -> TestTree
responseDescribeMountTargetSecurityGroups =
  res
    "DescribeMountTargetSecurityGroupsResponse"
    "fixture/DescribeMountTargetSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMountTargetSecurityGroups)

responseDescribeMountTargets :: DescribeMountTargetsResponse -> TestTree
responseDescribeMountTargets =
  res
    "DescribeMountTargetsResponse"
    "fixture/DescribeMountTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMountTargets)

responseDescribeReplicationConfigurations :: DescribeReplicationConfigurationsResponse -> TestTree
responseDescribeReplicationConfigurations =
  res
    "DescribeReplicationConfigurationsResponse"
    "fixture/DescribeReplicationConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplicationConfigurations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroupsResponse -> TestTree
responseModifyMountTargetSecurityGroups =
  res
    "ModifyMountTargetSecurityGroupsResponse"
    "fixture/ModifyMountTargetSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyMountTargetSecurityGroups)

responsePutAccountPreferences :: PutAccountPreferencesResponse -> TestTree
responsePutAccountPreferences =
  res
    "PutAccountPreferencesResponse"
    "fixture/PutAccountPreferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountPreferences)

responsePutBackupPolicy :: BackupPolicyDescription -> TestTree
responsePutBackupPolicy =
  res
    "PutBackupPolicyResponse"
    "fixture/PutBackupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBackupPolicy)

responsePutFileSystemPolicy :: FileSystemPolicyDescription -> TestTree
responsePutFileSystemPolicy =
  res
    "PutFileSystemPolicyResponse"
    "fixture/PutFileSystemPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFileSystemPolicy)

responsePutLifecycleConfiguration :: LifecycleConfigurationDescription -> TestTree
responsePutLifecycleConfiguration =
  res
    "PutLifecycleConfigurationResponse"
    "fixture/PutLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLifecycleConfiguration)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateFileSystem :: FileSystemDescription -> TestTree
responseUpdateFileSystem =
  res
    "UpdateFileSystemResponse"
    "fixture/UpdateFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFileSystem)
