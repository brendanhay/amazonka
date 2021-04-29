{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Lens
  ( -- * Operations

    -- ** PutBackupPolicy
    putBackupPolicy_fileSystemId,
    putBackupPolicy_backupPolicy,
    backupPolicyDescription_backupPolicy,

    -- ** PutLifecycleConfiguration
    putLifecycleConfiguration_fileSystemId,
    putLifecycleConfiguration_lifecyclePolicies,
    lifecycleConfigurationDescription_lifecyclePolicies,

    -- ** DeleteAccessPoint
    deleteAccessPoint_accessPointId,

    -- ** ModifyMountTargetSecurityGroups
    modifyMountTargetSecurityGroups_securityGroups,
    modifyMountTargetSecurityGroups_mountTargetId,

    -- ** DescribeFileSystemPolicy
    describeFileSystemPolicy_fileSystemId,
    fileSystemPolicyDescription_fileSystemId,
    fileSystemPolicyDescription_policy,

    -- ** UntagResource
    untagResource_resourceId,
    untagResource_tagKeys,

    -- ** TagResource
    tagResource_resourceId,
    tagResource_tags,

    -- ** CreateMountTarget
    createMountTarget_securityGroups,
    createMountTarget_ipAddress,
    createMountTarget_fileSystemId,
    createMountTarget_subnetId,
    mountTargetDescription_ownerId,
    mountTargetDescription_availabilityZoneName,
    mountTargetDescription_availabilityZoneId,
    mountTargetDescription_ipAddress,
    mountTargetDescription_networkInterfaceId,
    mountTargetDescription_vpcId,
    mountTargetDescription_mountTargetId,
    mountTargetDescription_fileSystemId,
    mountTargetDescription_subnetId,
    mountTargetDescription_lifeCycleState,

    -- ** DeleteMountTarget
    deleteMountTarget_mountTargetId,

    -- ** DescribeFileSystems
    describeFileSystems_creationToken,
    describeFileSystems_fileSystemId,
    describeFileSystems_maxItems,
    describeFileSystems_marker,
    describeFileSystemsResponse_nextMarker,
    describeFileSystemsResponse_fileSystems,
    describeFileSystemsResponse_marker,
    describeFileSystemsResponse_httpStatus,

    -- ** DescribeMountTargets
    describeMountTargets_mountTargetId,
    describeMountTargets_accessPointId,
    describeMountTargets_fileSystemId,
    describeMountTargets_maxItems,
    describeMountTargets_marker,
    describeMountTargetsResponse_nextMarker,
    describeMountTargetsResponse_mountTargets,
    describeMountTargetsResponse_marker,
    describeMountTargetsResponse_httpStatus,

    -- ** DeleteFileSystemPolicy
    deleteFileSystemPolicy_fileSystemId,

    -- ** CreateFileSystem
    createFileSystem_throughputMode,
    createFileSystem_encrypted,
    createFileSystem_provisionedThroughputInMibps,
    createFileSystem_kmsKeyId,
    createFileSystem_tags,
    createFileSystem_performanceMode,
    createFileSystem_creationToken,
    fileSystemDescription_throughputMode,
    fileSystemDescription_encrypted,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_kmsKeyId,
    fileSystemDescription_name,
    fileSystemDescription_ownerId,
    fileSystemDescription_creationToken,
    fileSystemDescription_fileSystemId,
    fileSystemDescription_creationTime,
    fileSystemDescription_lifeCycleState,
    fileSystemDescription_numberOfMountTargets,
    fileSystemDescription_sizeInBytes,
    fileSystemDescription_performanceMode,
    fileSystemDescription_tags,

    -- ** CreateAccessPoint
    createAccessPoint_rootDirectory,
    createAccessPoint_posixUser,
    createAccessPoint_tags,
    createAccessPoint_clientToken,
    createAccessPoint_fileSystemId,
    accessPointDescription_ownerId,
    accessPointDescription_accessPointArn,
    accessPointDescription_accessPointId,
    accessPointDescription_rootDirectory,
    accessPointDescription_name,
    accessPointDescription_posixUser,
    accessPointDescription_tags,
    accessPointDescription_lifeCycleState,
    accessPointDescription_fileSystemId,
    accessPointDescription_clientToken,

    -- ** DescribeAccessPoints
    describeAccessPoints_nextToken,
    describeAccessPoints_maxResults,
    describeAccessPoints_accessPointId,
    describeAccessPoints_fileSystemId,
    describeAccessPointsResponse_nextToken,
    describeAccessPointsResponse_accessPoints,
    describeAccessPointsResponse_httpStatus,

    -- ** DescribeLifecycleConfiguration
    describeLifecycleConfiguration_fileSystemId,
    lifecycleConfigurationDescription_lifecyclePolicies,

    -- ** DescribeMountTargetSecurityGroups
    describeMountTargetSecurityGroups_mountTargetId,
    describeMountTargetSecurityGroupsResponse_httpStatus,
    describeMountTargetSecurityGroupsResponse_securityGroups,

    -- ** DescribeBackupPolicy
    describeBackupPolicy_fileSystemId,
    backupPolicyDescription_backupPolicy,

    -- ** UpdateFileSystem
    updateFileSystem_throughputMode,
    updateFileSystem_provisionedThroughputInMibps,
    updateFileSystem_fileSystemId,
    fileSystemDescription_throughputMode,
    fileSystemDescription_encrypted,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_kmsKeyId,
    fileSystemDescription_name,
    fileSystemDescription_ownerId,
    fileSystemDescription_creationToken,
    fileSystemDescription_fileSystemId,
    fileSystemDescription_creationTime,
    fileSystemDescription_lifeCycleState,
    fileSystemDescription_numberOfMountTargets,
    fileSystemDescription_sizeInBytes,
    fileSystemDescription_performanceMode,
    fileSystemDescription_tags,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteFileSystem
    deleteFileSystem_fileSystemId,

    -- ** PutFileSystemPolicy
    putFileSystemPolicy_bypassPolicyLockoutSafetyCheck,
    putFileSystemPolicy_fileSystemId,
    putFileSystemPolicy_policy,
    fileSystemPolicyDescription_fileSystemId,
    fileSystemPolicyDescription_policy,

    -- * Types

    -- ** AccessPointDescription
    accessPointDescription_ownerId,
    accessPointDescription_accessPointArn,
    accessPointDescription_accessPointId,
    accessPointDescription_rootDirectory,
    accessPointDescription_name,
    accessPointDescription_posixUser,
    accessPointDescription_tags,
    accessPointDescription_lifeCycleState,
    accessPointDescription_fileSystemId,
    accessPointDescription_clientToken,

    -- ** BackupPolicy
    backupPolicy_status,

    -- ** BackupPolicyDescription
    backupPolicyDescription_backupPolicy,

    -- ** CreationInfo
    creationInfo_ownerUid,
    creationInfo_ownerGid,
    creationInfo_permissions,

    -- ** FileSystemDescription
    fileSystemDescription_throughputMode,
    fileSystemDescription_encrypted,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_kmsKeyId,
    fileSystemDescription_name,
    fileSystemDescription_ownerId,
    fileSystemDescription_creationToken,
    fileSystemDescription_fileSystemId,
    fileSystemDescription_creationTime,
    fileSystemDescription_lifeCycleState,
    fileSystemDescription_numberOfMountTargets,
    fileSystemDescription_sizeInBytes,
    fileSystemDescription_performanceMode,
    fileSystemDescription_tags,

    -- ** FileSystemPolicyDescription
    fileSystemPolicyDescription_fileSystemId,
    fileSystemPolicyDescription_policy,

    -- ** FileSystemSize
    fileSystemSize_valueInStandard,
    fileSystemSize_valueInIA,
    fileSystemSize_timestamp,
    fileSystemSize_value,

    -- ** LifecycleConfigurationDescription
    lifecycleConfigurationDescription_lifecyclePolicies,

    -- ** LifecyclePolicy
    lifecyclePolicy_transitionToIA,

    -- ** MountTargetDescription
    mountTargetDescription_ownerId,
    mountTargetDescription_availabilityZoneName,
    mountTargetDescription_availabilityZoneId,
    mountTargetDescription_ipAddress,
    mountTargetDescription_networkInterfaceId,
    mountTargetDescription_vpcId,
    mountTargetDescription_mountTargetId,
    mountTargetDescription_fileSystemId,
    mountTargetDescription_subnetId,
    mountTargetDescription_lifeCycleState,

    -- ** PosixUser
    posixUser_secondaryGids,
    posixUser_uid,
    posixUser_gid,

    -- ** RootDirectory
    rootDirectory_creationInfo,
    rootDirectory_path,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.EFS.CreateAccessPoint
import Network.AWS.EFS.CreateFileSystem
import Network.AWS.EFS.CreateMountTarget
import Network.AWS.EFS.DeleteAccessPoint
import Network.AWS.EFS.DeleteFileSystem
import Network.AWS.EFS.DeleteFileSystemPolicy
import Network.AWS.EFS.DeleteMountTarget
import Network.AWS.EFS.DescribeAccessPoints
import Network.AWS.EFS.DescribeBackupPolicy
import Network.AWS.EFS.DescribeFileSystemPolicy
import Network.AWS.EFS.DescribeFileSystems
import Network.AWS.EFS.DescribeLifecycleConfiguration
import Network.AWS.EFS.DescribeMountTargetSecurityGroups
import Network.AWS.EFS.DescribeMountTargets
import Network.AWS.EFS.ListTagsForResource
import Network.AWS.EFS.ModifyMountTargetSecurityGroups
import Network.AWS.EFS.PutBackupPolicy
import Network.AWS.EFS.PutFileSystemPolicy
import Network.AWS.EFS.PutLifecycleConfiguration
import Network.AWS.EFS.TagResource
import Network.AWS.EFS.Types.AccessPointDescription
import Network.AWS.EFS.Types.BackupPolicy
import Network.AWS.EFS.Types.BackupPolicyDescription
import Network.AWS.EFS.Types.CreationInfo
import Network.AWS.EFS.Types.FileSystemDescription
import Network.AWS.EFS.Types.FileSystemPolicyDescription
import Network.AWS.EFS.Types.FileSystemSize
import Network.AWS.EFS.Types.LifecycleConfigurationDescription
import Network.AWS.EFS.Types.LifecyclePolicy
import Network.AWS.EFS.Types.MountTargetDescription
import Network.AWS.EFS.Types.PosixUser
import Network.AWS.EFS.Types.RootDirectory
import Network.AWS.EFS.Types.Tag
import Network.AWS.EFS.UntagResource
import Network.AWS.EFS.UpdateFileSystem
