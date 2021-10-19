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

    -- ** CreateAccessPoint
    createAccessPoint_posixUser,
    createAccessPoint_rootDirectory,
    createAccessPoint_tags,
    createAccessPoint_clientToken,
    createAccessPoint_fileSystemId,
    accessPointDescription_posixUser,
    accessPointDescription_rootDirectory,
    accessPointDescription_clientToken,
    accessPointDescription_accessPointId,
    accessPointDescription_fileSystemId,
    accessPointDescription_ownerId,
    accessPointDescription_name,
    accessPointDescription_accessPointArn,
    accessPointDescription_lifeCycleState,
    accessPointDescription_tags,

    -- ** DescribeAccountPreferences
    describeAccountPreferences_nextToken,
    describeAccountPreferences_maxResults,
    describeAccountPreferencesResponse_resourceIdPreference,
    describeAccountPreferencesResponse_nextToken,
    describeAccountPreferencesResponse_httpStatus,

    -- ** DescribeMountTargets
    describeMountTargets_accessPointId,
    describeMountTargets_fileSystemId,
    describeMountTargets_marker,
    describeMountTargets_maxItems,
    describeMountTargets_mountTargetId,
    describeMountTargetsResponse_mountTargets,
    describeMountTargetsResponse_marker,
    describeMountTargetsResponse_nextMarker,
    describeMountTargetsResponse_httpStatus,

    -- ** DeleteFileSystemPolicy
    deleteFileSystemPolicy_fileSystemId,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutFileSystemPolicy
    putFileSystemPolicy_bypassPolicyLockoutSafetyCheck,
    putFileSystemPolicy_fileSystemId,
    putFileSystemPolicy_policy,
    fileSystemPolicyDescription_fileSystemId,
    fileSystemPolicyDescription_policy,

    -- ** DescribeFileSystems
    describeFileSystems_fileSystemId,
    describeFileSystems_creationToken,
    describeFileSystems_marker,
    describeFileSystems_maxItems,
    describeFileSystemsResponse_fileSystems,
    describeFileSystemsResponse_marker,
    describeFileSystemsResponse_nextMarker,
    describeFileSystemsResponse_httpStatus,

    -- ** DeleteMountTarget
    deleteMountTarget_mountTargetId,

    -- ** PutAccountPreferences
    putAccountPreferences_resourceIdType,
    putAccountPreferencesResponse_resourceIdPreference,
    putAccountPreferencesResponse_httpStatus,

    -- ** DescribeMountTargetSecurityGroups
    describeMountTargetSecurityGroups_mountTargetId,
    describeMountTargetSecurityGroupsResponse_httpStatus,
    describeMountTargetSecurityGroupsResponse_securityGroups,

    -- ** DescribeAccessPoints
    describeAccessPoints_accessPointId,
    describeAccessPoints_fileSystemId,
    describeAccessPoints_nextToken,
    describeAccessPoints_maxResults,
    describeAccessPointsResponse_accessPoints,
    describeAccessPointsResponse_nextToken,
    describeAccessPointsResponse_httpStatus,

    -- ** ModifyMountTargetSecurityGroups
    modifyMountTargetSecurityGroups_securityGroups,
    modifyMountTargetSecurityGroups_mountTargetId,

    -- ** CreateFileSystem
    createFileSystem_provisionedThroughputInMibps,
    createFileSystem_availabilityZoneName,
    createFileSystem_performanceMode,
    createFileSystem_backup,
    createFileSystem_encrypted,
    createFileSystem_throughputMode,
    createFileSystem_kmsKeyId,
    createFileSystem_tags,
    createFileSystem_creationToken,
    fileSystemDescription_availabilityZoneId,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_availabilityZoneName,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_encrypted,
    fileSystemDescription_throughputMode,
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

    -- ** PutLifecycleConfiguration
    putLifecycleConfiguration_fileSystemId,
    putLifecycleConfiguration_lifecyclePolicies,
    lifecycleConfigurationDescription_lifecyclePolicies,

    -- ** PutBackupPolicy
    putBackupPolicy_fileSystemId,
    putBackupPolicy_backupPolicy,
    backupPolicyDescription_backupPolicy,

    -- ** DeleteFileSystem
    deleteFileSystem_fileSystemId,

    -- ** UpdateFileSystem
    updateFileSystem_provisionedThroughputInMibps,
    updateFileSystem_throughputMode,
    updateFileSystem_fileSystemId,
    fileSystemDescription_availabilityZoneId,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_availabilityZoneName,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_encrypted,
    fileSystemDescription_throughputMode,
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

    -- ** CreateMountTarget
    createMountTarget_ipAddress,
    createMountTarget_securityGroups,
    createMountTarget_fileSystemId,
    createMountTarget_subnetId,
    mountTargetDescription_ipAddress,
    mountTargetDescription_availabilityZoneId,
    mountTargetDescription_vpcId,
    mountTargetDescription_availabilityZoneName,
    mountTargetDescription_networkInterfaceId,
    mountTargetDescription_ownerId,
    mountTargetDescription_mountTargetId,
    mountTargetDescription_fileSystemId,
    mountTargetDescription_subnetId,
    mountTargetDescription_lifeCycleState,

    -- ** TagResource
    tagResource_resourceId,
    tagResource_tags,

    -- ** DescribeBackupPolicy
    describeBackupPolicy_fileSystemId,
    backupPolicyDescription_backupPolicy,

    -- ** DescribeLifecycleConfiguration
    describeLifecycleConfiguration_fileSystemId,
    lifecycleConfigurationDescription_lifecyclePolicies,

    -- ** UntagResource
    untagResource_resourceId,
    untagResource_tagKeys,

    -- ** DescribeFileSystemPolicy
    describeFileSystemPolicy_fileSystemId,
    fileSystemPolicyDescription_fileSystemId,
    fileSystemPolicyDescription_policy,

    -- ** DeleteAccessPoint
    deleteAccessPoint_accessPointId,

    -- * Types

    -- ** AccessPointDescription
    accessPointDescription_posixUser,
    accessPointDescription_rootDirectory,
    accessPointDescription_clientToken,
    accessPointDescription_accessPointId,
    accessPointDescription_fileSystemId,
    accessPointDescription_ownerId,
    accessPointDescription_name,
    accessPointDescription_accessPointArn,
    accessPointDescription_lifeCycleState,
    accessPointDescription_tags,

    -- ** BackupPolicy
    backupPolicy_status,

    -- ** BackupPolicyDescription
    backupPolicyDescription_backupPolicy,

    -- ** CreationInfo
    creationInfo_ownerUid,
    creationInfo_ownerGid,
    creationInfo_permissions,

    -- ** FileSystemDescription
    fileSystemDescription_availabilityZoneId,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_availabilityZoneName,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_encrypted,
    fileSystemDescription_throughputMode,
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
    fileSystemSize_valueInIA,
    fileSystemSize_valueInStandard,
    fileSystemSize_timestamp,
    fileSystemSize_value,

    -- ** LifecycleConfigurationDescription
    lifecycleConfigurationDescription_lifecyclePolicies,

    -- ** LifecyclePolicy
    lifecyclePolicy_transitionToIA,
    lifecyclePolicy_transitionToPrimaryStorageClass,

    -- ** MountTargetDescription
    mountTargetDescription_ipAddress,
    mountTargetDescription_availabilityZoneId,
    mountTargetDescription_vpcId,
    mountTargetDescription_availabilityZoneName,
    mountTargetDescription_networkInterfaceId,
    mountTargetDescription_ownerId,
    mountTargetDescription_mountTargetId,
    mountTargetDescription_fileSystemId,
    mountTargetDescription_subnetId,
    mountTargetDescription_lifeCycleState,

    -- ** PosixUser
    posixUser_secondaryGids,
    posixUser_uid,
    posixUser_gid,

    -- ** ResourceIdPreference
    resourceIdPreference_resources,
    resourceIdPreference_resourceIdType,

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
import Network.AWS.EFS.DescribeAccountPreferences
import Network.AWS.EFS.DescribeBackupPolicy
import Network.AWS.EFS.DescribeFileSystemPolicy
import Network.AWS.EFS.DescribeFileSystems
import Network.AWS.EFS.DescribeLifecycleConfiguration
import Network.AWS.EFS.DescribeMountTargetSecurityGroups
import Network.AWS.EFS.DescribeMountTargets
import Network.AWS.EFS.ListTagsForResource
import Network.AWS.EFS.ModifyMountTargetSecurityGroups
import Network.AWS.EFS.PutAccountPreferences
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
import Network.AWS.EFS.Types.ResourceIdPreference
import Network.AWS.EFS.Types.RootDirectory
import Network.AWS.EFS.Types.Tag
import Network.AWS.EFS.UntagResource
import Network.AWS.EFS.UpdateFileSystem
