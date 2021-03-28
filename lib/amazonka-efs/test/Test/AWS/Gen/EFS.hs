{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EFS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestCreateAccessPoint $
--             mkCreateAccessPoint
--
--         , requestDescribeMountTargets $
--             mkDescribeMountTargets
--
--         , requestDeleteFileSystemPolicy $
--             mkDeleteFileSystemPolicy
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestPutFileSystemPolicy $
--             mkPutFileSystemPolicy
--
--         , requestDescribeFileSystems $
--             mkDescribeFileSystems
--
--         , requestDeleteMountTarget $
--             mkDeleteMountTarget
--
--         , requestDescribeMountTargetSecurityGroups $
--             mkDescribeMountTargetSecurityGroups
--
--         , requestDescribeAccessPoints $
--             mkDescribeAccessPoints
--
--         , requestModifyMountTargetSecurityGroups $
--             mkModifyMountTargetSecurityGroups
--
--         , requestCreateFileSystem $
--             mkCreateFileSystem
--
--         , requestPutLifecycleConfiguration $
--             mkPutLifecycleConfiguration
--
--         , requestPutBackupPolicy $
--             mkPutBackupPolicy
--
--         , requestDeleteFileSystem $
--             mkDeleteFileSystem
--
--         , requestUpdateFileSystem $
--             mkUpdateFileSystem
--
--         , requestCreateMountTarget $
--             mkCreateMountTarget
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestDescribeBackupPolicy $
--             mkDescribeBackupPolicy
--
--         , requestDescribeLifecycleConfiguration $
--             mkDescribeLifecycleConfiguration
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDescribeFileSystemPolicy $
--             mkDescribeFileSystemPolicy
--
--         , requestDeleteAccessPoint $
--             mkDeleteAccessPoint
--
--           ]

--     , testGroup "response"
--         [ responseCreateAccessPoint $
--             mkAccessPointDescription
--
--         , responseDescribeMountTargets $
--             mkDescribeMountTargetsResponse
--
--         , responseDeleteFileSystemPolicy $
--             mkDeleteFileSystemPolicyResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responsePutFileSystemPolicy $
--             mkFileSystemPolicyDescription
--
--         , responseDescribeFileSystems $
--             mkDescribeFileSystemsResponse
--
--         , responseDeleteMountTarget $
--             mkDeleteMountTargetResponse
--
--         , responseDescribeMountTargetSecurityGroups $
--             mkDescribeMountTargetSecurityGroupsResponse
--
--         , responseDescribeAccessPoints $
--             mkDescribeAccessPointsResponse
--
--         , responseModifyMountTargetSecurityGroups $
--             mkModifyMountTargetSecurityGroupsResponse
--
--         , responseCreateFileSystem $
--             mkFileSystemDescription
--
--         , responsePutLifecycleConfiguration $
--             mkLifecycleConfigurationDescription
--
--         , responsePutBackupPolicy $
--             mkBackupPolicyDescription
--
--         , responseDeleteFileSystem $
--             mkDeleteFileSystemResponse
--
--         , responseUpdateFileSystem $
--             mkFileSystemDescription
--
--         , responseCreateMountTarget $
--             mkMountTargetDescription
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseDescribeBackupPolicy $
--             mkBackupPolicyDescription
--
--         , responseDescribeLifecycleConfiguration $
--             mkLifecycleConfigurationDescription
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDescribeFileSystemPolicy $
--             mkFileSystemPolicyDescription
--
--         , responseDeleteAccessPoint $
--             mkDeleteAccessPointResponse
--
--           ]
--     ]

-- Requests

requestCreateAccessPoint :: CreateAccessPoint -> TestTree
requestCreateAccessPoint = req
    "CreateAccessPoint"
    "fixture/CreateAccessPoint.yaml"

requestDescribeMountTargets :: DescribeMountTargets -> TestTree
requestDescribeMountTargets = req
    "DescribeMountTargets"
    "fixture/DescribeMountTargets.yaml"

requestDeleteFileSystemPolicy :: DeleteFileSystemPolicy -> TestTree
requestDeleteFileSystemPolicy = req
    "DeleteFileSystemPolicy"
    "fixture/DeleteFileSystemPolicy.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutFileSystemPolicy :: PutFileSystemPolicy -> TestTree
requestPutFileSystemPolicy = req
    "PutFileSystemPolicy"
    "fixture/PutFileSystemPolicy.yaml"

requestDescribeFileSystems :: DescribeFileSystems -> TestTree
requestDescribeFileSystems = req
    "DescribeFileSystems"
    "fixture/DescribeFileSystems.yaml"

requestDeleteMountTarget :: DeleteMountTarget -> TestTree
requestDeleteMountTarget = req
    "DeleteMountTarget"
    "fixture/DeleteMountTarget.yaml"

requestDescribeMountTargetSecurityGroups :: DescribeMountTargetSecurityGroups -> TestTree
requestDescribeMountTargetSecurityGroups = req
    "DescribeMountTargetSecurityGroups"
    "fixture/DescribeMountTargetSecurityGroups.yaml"

requestDescribeAccessPoints :: DescribeAccessPoints -> TestTree
requestDescribeAccessPoints = req
    "DescribeAccessPoints"
    "fixture/DescribeAccessPoints.yaml"

requestModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroups -> TestTree
requestModifyMountTargetSecurityGroups = req
    "ModifyMountTargetSecurityGroups"
    "fixture/ModifyMountTargetSecurityGroups.yaml"

requestCreateFileSystem :: CreateFileSystem -> TestTree
requestCreateFileSystem = req
    "CreateFileSystem"
    "fixture/CreateFileSystem.yaml"

requestPutLifecycleConfiguration :: PutLifecycleConfiguration -> TestTree
requestPutLifecycleConfiguration = req
    "PutLifecycleConfiguration"
    "fixture/PutLifecycleConfiguration.yaml"

requestPutBackupPolicy :: PutBackupPolicy -> TestTree
requestPutBackupPolicy = req
    "PutBackupPolicy"
    "fixture/PutBackupPolicy.yaml"

requestDeleteFileSystem :: DeleteFileSystem -> TestTree
requestDeleteFileSystem = req
    "DeleteFileSystem"
    "fixture/DeleteFileSystem.yaml"

requestUpdateFileSystem :: UpdateFileSystem -> TestTree
requestUpdateFileSystem = req
    "UpdateFileSystem"
    "fixture/UpdateFileSystem.yaml"

requestCreateMountTarget :: CreateMountTarget -> TestTree
requestCreateMountTarget = req
    "CreateMountTarget"
    "fixture/CreateMountTarget.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeBackupPolicy :: DescribeBackupPolicy -> TestTree
requestDescribeBackupPolicy = req
    "DescribeBackupPolicy"
    "fixture/DescribeBackupPolicy.yaml"

requestDescribeLifecycleConfiguration :: DescribeLifecycleConfiguration -> TestTree
requestDescribeLifecycleConfiguration = req
    "DescribeLifecycleConfiguration"
    "fixture/DescribeLifecycleConfiguration.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeFileSystemPolicy :: DescribeFileSystemPolicy -> TestTree
requestDescribeFileSystemPolicy = req
    "DescribeFileSystemPolicy"
    "fixture/DescribeFileSystemPolicy.yaml"

requestDeleteAccessPoint :: DeleteAccessPoint -> TestTree
requestDeleteAccessPoint = req
    "DeleteAccessPoint"
    "fixture/DeleteAccessPoint.yaml"

-- Responses

responseCreateAccessPoint :: AccessPointDescription -> TestTree
responseCreateAccessPoint = res
    "CreateAccessPointResponse"
    "fixture/CreateAccessPointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAccessPoint)

responseDescribeMountTargets :: DescribeMountTargetsResponse -> TestTree
responseDescribeMountTargets = res
    "DescribeMountTargetsResponse"
    "fixture/DescribeMountTargetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMountTargets)

responseDeleteFileSystemPolicy :: DeleteFileSystemPolicyResponse -> TestTree
responseDeleteFileSystemPolicy = res
    "DeleteFileSystemPolicyResponse"
    "fixture/DeleteFileSystemPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFileSystemPolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responsePutFileSystemPolicy :: FileSystemPolicyDescription -> TestTree
responsePutFileSystemPolicy = res
    "PutFileSystemPolicyResponse"
    "fixture/PutFileSystemPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutFileSystemPolicy)

responseDescribeFileSystems :: DescribeFileSystemsResponse -> TestTree
responseDescribeFileSystems = res
    "DescribeFileSystemsResponse"
    "fixture/DescribeFileSystemsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFileSystems)

responseDeleteMountTarget :: DeleteMountTargetResponse -> TestTree
responseDeleteMountTarget = res
    "DeleteMountTargetResponse"
    "fixture/DeleteMountTargetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteMountTarget)

responseDescribeMountTargetSecurityGroups :: DescribeMountTargetSecurityGroupsResponse -> TestTree
responseDescribeMountTargetSecurityGroups = res
    "DescribeMountTargetSecurityGroupsResponse"
    "fixture/DescribeMountTargetSecurityGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMountTargetSecurityGroups)

responseDescribeAccessPoints :: DescribeAccessPointsResponse -> TestTree
responseDescribeAccessPoints = res
    "DescribeAccessPointsResponse"
    "fixture/DescribeAccessPointsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAccessPoints)

responseModifyMountTargetSecurityGroups :: ModifyMountTargetSecurityGroupsResponse -> TestTree
responseModifyMountTargetSecurityGroups = res
    "ModifyMountTargetSecurityGroupsResponse"
    "fixture/ModifyMountTargetSecurityGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyMountTargetSecurityGroups)

responseCreateFileSystem :: FileSystemDescription -> TestTree
responseCreateFileSystem = res
    "CreateFileSystemResponse"
    "fixture/CreateFileSystemResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateFileSystem)

responsePutLifecycleConfiguration :: LifecycleConfigurationDescription -> TestTree
responsePutLifecycleConfiguration = res
    "PutLifecycleConfigurationResponse"
    "fixture/PutLifecycleConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutLifecycleConfiguration)

responsePutBackupPolicy :: BackupPolicyDescription -> TestTree
responsePutBackupPolicy = res
    "PutBackupPolicyResponse"
    "fixture/PutBackupPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBackupPolicy)

responseDeleteFileSystem :: DeleteFileSystemResponse -> TestTree
responseDeleteFileSystem = res
    "DeleteFileSystemResponse"
    "fixture/DeleteFileSystemResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFileSystem)

responseUpdateFileSystem :: FileSystemDescription -> TestTree
responseUpdateFileSystem = res
    "UpdateFileSystemResponse"
    "fixture/UpdateFileSystemResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateFileSystem)

responseCreateMountTarget :: MountTargetDescription -> TestTree
responseCreateMountTarget = res
    "CreateMountTargetResponse"
    "fixture/CreateMountTargetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateMountTarget)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseDescribeBackupPolicy :: BackupPolicyDescription -> TestTree
responseDescribeBackupPolicy = res
    "DescribeBackupPolicyResponse"
    "fixture/DescribeBackupPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeBackupPolicy)

responseDescribeLifecycleConfiguration :: LifecycleConfigurationDescription -> TestTree
responseDescribeLifecycleConfiguration = res
    "DescribeLifecycleConfigurationResponse"
    "fixture/DescribeLifecycleConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLifecycleConfiguration)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseDescribeFileSystemPolicy :: FileSystemPolicyDescription -> TestTree
responseDescribeFileSystemPolicy = res
    "DescribeFileSystemPolicyResponse"
    "fixture/DescribeFileSystemPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFileSystemPolicy)

responseDeleteAccessPoint :: DeleteAccessPointResponse -> TestTree
responseDeleteAccessPoint = res
    "DeleteAccessPointResponse"
    "fixture/DeleteAccessPointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAccessPoint)
