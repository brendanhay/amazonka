{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EFS.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Lens
  ( -- * Operations

    -- ** CreateAccessPoint
    createAccessPoint_posixUser,
    createAccessPoint_rootDirectory,
    createAccessPoint_tags,
    createAccessPoint_clientToken,
    createAccessPoint_fileSystemId,
    accessPointDescription_accessPointArn,
    accessPointDescription_accessPointId,
    accessPointDescription_clientToken,
    accessPointDescription_fileSystemId,
    accessPointDescription_lifeCycleState,
    accessPointDescription_name,
    accessPointDescription_ownerId,
    accessPointDescription_posixUser,
    accessPointDescription_rootDirectory,
    accessPointDescription_tags,

    -- ** CreateFileSystem
    createFileSystem_availabilityZoneName,
    createFileSystem_backup,
    createFileSystem_encrypted,
    createFileSystem_kmsKeyId,
    createFileSystem_performanceMode,
    createFileSystem_provisionedThroughputInMibps,
    createFileSystem_tags,
    createFileSystem_throughputMode,
    createFileSystem_creationToken,
    fileSystemDescription_availabilityZoneId,
    fileSystemDescription_availabilityZoneName,
    fileSystemDescription_encrypted,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_kmsKeyId,
    fileSystemDescription_name,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_throughputMode,
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
    mountTargetDescription_availabilityZoneId,
    mountTargetDescription_availabilityZoneName,
    mountTargetDescription_ipAddress,
    mountTargetDescription_networkInterfaceId,
    mountTargetDescription_ownerId,
    mountTargetDescription_vpcId,
    mountTargetDescription_mountTargetId,
    mountTargetDescription_fileSystemId,
    mountTargetDescription_subnetId,
    mountTargetDescription_lifeCycleState,

    -- ** CreateReplicationConfiguration
    createReplicationConfiguration_sourceFileSystemId,
    createReplicationConfiguration_destinations,
    replicationConfigurationDescription_sourceFileSystemId,
    replicationConfigurationDescription_sourceFileSystemRegion,
    replicationConfigurationDescription_sourceFileSystemArn,
    replicationConfigurationDescription_originalSourceFileSystemArn,
    replicationConfigurationDescription_creationTime,
    replicationConfigurationDescription_destinations,

    -- ** DeleteAccessPoint
    deleteAccessPoint_accessPointId,

    -- ** DeleteFileSystem
    deleteFileSystem_fileSystemId,

    -- ** DeleteFileSystemPolicy
    deleteFileSystemPolicy_fileSystemId,

    -- ** DeleteMountTarget
    deleteMountTarget_mountTargetId,

    -- ** DeleteReplicationConfiguration
    deleteReplicationConfiguration_sourceFileSystemId,

    -- ** DescribeAccessPoints
    describeAccessPoints_accessPointId,
    describeAccessPoints_fileSystemId,
    describeAccessPoints_maxResults,
    describeAccessPoints_nextToken,
    describeAccessPointsResponse_accessPoints,
    describeAccessPointsResponse_nextToken,
    describeAccessPointsResponse_httpStatus,

    -- ** DescribeAccountPreferences
    describeAccountPreferences_maxResults,
    describeAccountPreferences_nextToken,
    describeAccountPreferencesResponse_nextToken,
    describeAccountPreferencesResponse_resourceIdPreference,
    describeAccountPreferencesResponse_httpStatus,

    -- ** DescribeBackupPolicy
    describeBackupPolicy_fileSystemId,
    backupPolicyDescription_backupPolicy,

    -- ** DescribeFileSystemPolicy
    describeFileSystemPolicy_fileSystemId,
    fileSystemPolicyDescription_fileSystemId,
    fileSystemPolicyDescription_policy,

    -- ** DescribeFileSystems
    describeFileSystems_creationToken,
    describeFileSystems_fileSystemId,
    describeFileSystems_marker,
    describeFileSystems_maxItems,
    describeFileSystemsResponse_fileSystems,
    describeFileSystemsResponse_marker,
    describeFileSystemsResponse_nextMarker,
    describeFileSystemsResponse_httpStatus,

    -- ** DescribeLifecycleConfiguration
    describeLifecycleConfiguration_fileSystemId,
    lifecycleConfigurationDescription_lifecyclePolicies,

    -- ** DescribeMountTargetSecurityGroups
    describeMountTargetSecurityGroups_mountTargetId,
    describeMountTargetSecurityGroupsResponse_httpStatus,
    describeMountTargetSecurityGroupsResponse_securityGroups,

    -- ** DescribeMountTargets
    describeMountTargets_accessPointId,
    describeMountTargets_fileSystemId,
    describeMountTargets_marker,
    describeMountTargets_maxItems,
    describeMountTargets_mountTargetId,
    describeMountTargetsResponse_marker,
    describeMountTargetsResponse_mountTargets,
    describeMountTargetsResponse_nextMarker,
    describeMountTargetsResponse_httpStatus,

    -- ** DescribeReplicationConfigurations
    describeReplicationConfigurations_fileSystemId,
    describeReplicationConfigurations_maxResults,
    describeReplicationConfigurations_nextToken,
    describeReplicationConfigurationsResponse_nextToken,
    describeReplicationConfigurationsResponse_replications,
    describeReplicationConfigurationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_maxResults,
    listTagsForResource_nextToken,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ModifyMountTargetSecurityGroups
    modifyMountTargetSecurityGroups_securityGroups,
    modifyMountTargetSecurityGroups_mountTargetId,

    -- ** PutAccountPreferences
    putAccountPreferences_resourceIdType,
    putAccountPreferencesResponse_resourceIdPreference,
    putAccountPreferencesResponse_httpStatus,

    -- ** PutBackupPolicy
    putBackupPolicy_fileSystemId,
    putBackupPolicy_backupPolicy,
    backupPolicyDescription_backupPolicy,

    -- ** PutFileSystemPolicy
    putFileSystemPolicy_bypassPolicyLockoutSafetyCheck,
    putFileSystemPolicy_fileSystemId,
    putFileSystemPolicy_policy,
    fileSystemPolicyDescription_fileSystemId,
    fileSystemPolicyDescription_policy,

    -- ** PutLifecycleConfiguration
    putLifecycleConfiguration_fileSystemId,
    putLifecycleConfiguration_lifecyclePolicies,
    lifecycleConfigurationDescription_lifecyclePolicies,

    -- ** TagResource
    tagResource_resourceId,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceId,
    untagResource_tagKeys,

    -- ** UpdateFileSystem
    updateFileSystem_provisionedThroughputInMibps,
    updateFileSystem_throughputMode,
    updateFileSystem_fileSystemId,
    fileSystemDescription_availabilityZoneId,
    fileSystemDescription_availabilityZoneName,
    fileSystemDescription_encrypted,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_kmsKeyId,
    fileSystemDescription_name,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_throughputMode,
    fileSystemDescription_ownerId,
    fileSystemDescription_creationToken,
    fileSystemDescription_fileSystemId,
    fileSystemDescription_creationTime,
    fileSystemDescription_lifeCycleState,
    fileSystemDescription_numberOfMountTargets,
    fileSystemDescription_sizeInBytes,
    fileSystemDescription_performanceMode,
    fileSystemDescription_tags,

    -- * Types

    -- ** AccessPointDescription
    accessPointDescription_accessPointArn,
    accessPointDescription_accessPointId,
    accessPointDescription_clientToken,
    accessPointDescription_fileSystemId,
    accessPointDescription_lifeCycleState,
    accessPointDescription_name,
    accessPointDescription_ownerId,
    accessPointDescription_posixUser,
    accessPointDescription_rootDirectory,
    accessPointDescription_tags,

    -- ** BackupPolicy
    backupPolicy_status,

    -- ** BackupPolicyDescription
    backupPolicyDescription_backupPolicy,

    -- ** CreationInfo
    creationInfo_ownerUid,
    creationInfo_ownerGid,
    creationInfo_permissions,

    -- ** Destination
    destination_lastReplicatedTimestamp,
    destination_status,
    destination_fileSystemId,
    destination_region,

    -- ** DestinationToCreate
    destinationToCreate_availabilityZoneName,
    destinationToCreate_kmsKeyId,
    destinationToCreate_region,

    -- ** FileSystemDescription
    fileSystemDescription_availabilityZoneId,
    fileSystemDescription_availabilityZoneName,
    fileSystemDescription_encrypted,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_kmsKeyId,
    fileSystemDescription_name,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_throughputMode,
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
    fileSystemSize_timestamp,
    fileSystemSize_valueInIA,
    fileSystemSize_valueInStandard,
    fileSystemSize_value,

    -- ** LifecycleConfigurationDescription
    lifecycleConfigurationDescription_lifecyclePolicies,

    -- ** LifecyclePolicy
    lifecyclePolicy_transitionToIA,
    lifecyclePolicy_transitionToPrimaryStorageClass,

    -- ** MountTargetDescription
    mountTargetDescription_availabilityZoneId,
    mountTargetDescription_availabilityZoneName,
    mountTargetDescription_ipAddress,
    mountTargetDescription_networkInterfaceId,
    mountTargetDescription_ownerId,
    mountTargetDescription_vpcId,
    mountTargetDescription_mountTargetId,
    mountTargetDescription_fileSystemId,
    mountTargetDescription_subnetId,
    mountTargetDescription_lifeCycleState,

    -- ** PosixUser
    posixUser_secondaryGids,
    posixUser_uid,
    posixUser_gid,

    -- ** ReplicationConfigurationDescription
    replicationConfigurationDescription_sourceFileSystemId,
    replicationConfigurationDescription_sourceFileSystemRegion,
    replicationConfigurationDescription_sourceFileSystemArn,
    replicationConfigurationDescription_originalSourceFileSystemArn,
    replicationConfigurationDescription_creationTime,
    replicationConfigurationDescription_destinations,

    -- ** ResourceIdPreference
    resourceIdPreference_resourceIdType,
    resourceIdPreference_resources,

    -- ** RootDirectory
    rootDirectory_creationInfo,
    rootDirectory_path,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.EFS.CreateAccessPoint
import Amazonka.EFS.CreateFileSystem
import Amazonka.EFS.CreateMountTarget
import Amazonka.EFS.CreateReplicationConfiguration
import Amazonka.EFS.DeleteAccessPoint
import Amazonka.EFS.DeleteFileSystem
import Amazonka.EFS.DeleteFileSystemPolicy
import Amazonka.EFS.DeleteMountTarget
import Amazonka.EFS.DeleteReplicationConfiguration
import Amazonka.EFS.DescribeAccessPoints
import Amazonka.EFS.DescribeAccountPreferences
import Amazonka.EFS.DescribeBackupPolicy
import Amazonka.EFS.DescribeFileSystemPolicy
import Amazonka.EFS.DescribeFileSystems
import Amazonka.EFS.DescribeLifecycleConfiguration
import Amazonka.EFS.DescribeMountTargetSecurityGroups
import Amazonka.EFS.DescribeMountTargets
import Amazonka.EFS.DescribeReplicationConfigurations
import Amazonka.EFS.ListTagsForResource
import Amazonka.EFS.ModifyMountTargetSecurityGroups
import Amazonka.EFS.PutAccountPreferences
import Amazonka.EFS.PutBackupPolicy
import Amazonka.EFS.PutFileSystemPolicy
import Amazonka.EFS.PutLifecycleConfiguration
import Amazonka.EFS.TagResource
import Amazonka.EFS.Types.AccessPointDescription
import Amazonka.EFS.Types.BackupPolicy
import Amazonka.EFS.Types.BackupPolicyDescription
import Amazonka.EFS.Types.CreationInfo
import Amazonka.EFS.Types.Destination
import Amazonka.EFS.Types.DestinationToCreate
import Amazonka.EFS.Types.FileSystemDescription
import Amazonka.EFS.Types.FileSystemPolicyDescription
import Amazonka.EFS.Types.FileSystemSize
import Amazonka.EFS.Types.LifecycleConfigurationDescription
import Amazonka.EFS.Types.LifecyclePolicy
import Amazonka.EFS.Types.MountTargetDescription
import Amazonka.EFS.Types.PosixUser
import Amazonka.EFS.Types.ReplicationConfigurationDescription
import Amazonka.EFS.Types.ResourceIdPreference
import Amazonka.EFS.Types.RootDirectory
import Amazonka.EFS.Types.Tag
import Amazonka.EFS.UntagResource
import Amazonka.EFS.UpdateFileSystem
