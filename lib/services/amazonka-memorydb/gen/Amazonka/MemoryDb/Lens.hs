{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MemoryDb.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Lens
  ( -- * Operations

    -- ** BatchUpdateCluster
    batchUpdateCluster_serviceUpdate,
    batchUpdateCluster_clusterNames,
    batchUpdateClusterResponse_processedClusters,
    batchUpdateClusterResponse_unprocessedClusters,
    batchUpdateClusterResponse_httpStatus,

    -- ** CopySnapshot
    copySnapshot_tags,
    copySnapshot_targetBucket,
    copySnapshot_kmsKeyId,
    copySnapshot_sourceSnapshotName,
    copySnapshot_targetSnapshotName,
    copySnapshotResponse_snapshot,
    copySnapshotResponse_httpStatus,

    -- ** CreateACL
    createACL_tags,
    createACL_userNames,
    createACL_aCLName,
    createACLResponse_acl,
    createACLResponse_httpStatus,

    -- ** CreateCluster
    createCluster_tags,
    createCluster_port,
    createCluster_subnetGroupName,
    createCluster_parameterGroupName,
    createCluster_numReplicasPerShard,
    createCluster_snapshotName,
    createCluster_securityGroupIds,
    createCluster_autoMinorVersionUpgrade,
    createCluster_tLSEnabled,
    createCluster_description,
    createCluster_snapshotArns,
    createCluster_maintenanceWindow,
    createCluster_snapshotWindow,
    createCluster_snapshotRetentionLimit,
    createCluster_snsTopicArn,
    createCluster_kmsKeyId,
    createCluster_numShards,
    createCluster_dataTiering,
    createCluster_engineVersion,
    createCluster_clusterName,
    createCluster_nodeType,
    createCluster_aCLName,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CreateParameterGroup
    createParameterGroup_tags,
    createParameterGroup_description,
    createParameterGroup_parameterGroupName,
    createParameterGroup_family,
    createParameterGroupResponse_parameterGroup,
    createParameterGroupResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_tags,
    createSnapshot_kmsKeyId,
    createSnapshot_clusterName,
    createSnapshot_snapshotName,
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,

    -- ** CreateSubnetGroup
    createSubnetGroup_tags,
    createSubnetGroup_description,
    createSubnetGroup_subnetGroupName,
    createSubnetGroup_subnetIds,
    createSubnetGroupResponse_subnetGroup,
    createSubnetGroupResponse_httpStatus,

    -- ** CreateUser
    createUser_tags,
    createUser_userName,
    createUser_authenticationMode,
    createUser_accessString,
    createUserResponse_user,
    createUserResponse_httpStatus,

    -- ** DeleteACL
    deleteACL_aCLName,
    deleteACLResponse_acl,
    deleteACLResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_finalSnapshotName,
    deleteCluster_clusterName,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** DeleteParameterGroup
    deleteParameterGroup_parameterGroupName,
    deleteParameterGroupResponse_parameterGroup,
    deleteParameterGroupResponse_httpStatus,

    -- ** DeleteSnapshot
    deleteSnapshot_snapshotName,
    deleteSnapshotResponse_snapshot,
    deleteSnapshotResponse_httpStatus,

    -- ** DeleteSubnetGroup
    deleteSubnetGroup_subnetGroupName,
    deleteSubnetGroupResponse_subnetGroup,
    deleteSubnetGroupResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_userName,
    deleteUserResponse_user,
    deleteUserResponse_httpStatus,

    -- ** DescribeACLs
    describeACLs_nextToken,
    describeACLs_aCLName,
    describeACLs_maxResults,
    describeACLsResponse_nextToken,
    describeACLsResponse_aCLs,
    describeACLsResponse_httpStatus,

    -- ** DescribeClusters
    describeClusters_nextToken,
    describeClusters_maxResults,
    describeClusters_showShardDetails,
    describeClusters_clusterName,
    describeClustersResponse_nextToken,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,

    -- ** DescribeEngineVersions
    describeEngineVersions_nextToken,
    describeEngineVersions_parameterGroupFamily,
    describeEngineVersions_defaultOnly,
    describeEngineVersions_maxResults,
    describeEngineVersions_engineVersion,
    describeEngineVersionsResponse_nextToken,
    describeEngineVersionsResponse_engineVersions,
    describeEngineVersionsResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_nextToken,
    describeEvents_sourceName,
    describeEvents_sourceType,
    describeEvents_endTime,
    describeEvents_duration,
    describeEvents_maxResults,
    describeEvents_startTime,
    describeEventsResponse_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** DescribeParameterGroups
    describeParameterGroups_parameterGroupName,
    describeParameterGroups_nextToken,
    describeParameterGroups_maxResults,
    describeParameterGroupsResponse_nextToken,
    describeParameterGroupsResponse_parameterGroups,
    describeParameterGroupsResponse_httpStatus,

    -- ** DescribeParameters
    describeParameters_nextToken,
    describeParameters_maxResults,
    describeParameters_parameterGroupName,
    describeParametersResponse_nextToken,
    describeParametersResponse_parameters,
    describeParametersResponse_httpStatus,

    -- ** DescribeServiceUpdates
    describeServiceUpdates_nextToken,
    describeServiceUpdates_status,
    describeServiceUpdates_maxResults,
    describeServiceUpdates_serviceUpdateName,
    describeServiceUpdates_clusterNames,
    describeServiceUpdatesResponse_nextToken,
    describeServiceUpdatesResponse_serviceUpdates,
    describeServiceUpdatesResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_nextToken,
    describeSnapshots_snapshotName,
    describeSnapshots_showDetail,
    describeSnapshots_source,
    describeSnapshots_maxResults,
    describeSnapshots_clusterName,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,

    -- ** DescribeSubnetGroups
    describeSubnetGroups_subnetGroupName,
    describeSubnetGroups_nextToken,
    describeSubnetGroups_maxResults,
    describeSubnetGroupsResponse_nextToken,
    describeSubnetGroupsResponse_subnetGroups,
    describeSubnetGroupsResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_nextToken,
    describeUsers_userName,
    describeUsers_filters,
    describeUsers_maxResults,
    describeUsersResponse_nextToken,
    describeUsersResponse_users,
    describeUsersResponse_httpStatus,

    -- ** FailoverShard
    failoverShard_clusterName,
    failoverShard_shardName,
    failoverShardResponse_cluster,
    failoverShardResponse_httpStatus,

    -- ** ListAllowedNodeTypeUpdates
    listAllowedNodeTypeUpdates_clusterName,
    listAllowedNodeTypeUpdatesResponse_scaleUpNodeTypes,
    listAllowedNodeTypeUpdatesResponse_scaleDownNodeTypes,
    listAllowedNodeTypeUpdatesResponse_httpStatus,

    -- ** ListTags
    listTags_resourceArn,
    listTagsResponse_tagList,
    listTagsResponse_httpStatus,

    -- ** ResetParameterGroup
    resetParameterGroup_parameterNames,
    resetParameterGroup_allParameters,
    resetParameterGroup_parameterGroupName,
    resetParameterGroupResponse_parameterGroup,
    resetParameterGroupResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_tagList,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_tagList,
    untagResourceResponse_httpStatus,

    -- ** UpdateACL
    updateACL_userNamesToRemove,
    updateACL_userNamesToAdd,
    updateACL_aCLName,
    updateACLResponse_acl,
    updateACLResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_shardConfiguration,
    updateCluster_parameterGroupName,
    updateCluster_aCLName,
    updateCluster_securityGroupIds,
    updateCluster_snsTopicStatus,
    updateCluster_description,
    updateCluster_nodeType,
    updateCluster_maintenanceWindow,
    updateCluster_snapshotWindow,
    updateCluster_snapshotRetentionLimit,
    updateCluster_snsTopicArn,
    updateCluster_replicaConfiguration,
    updateCluster_engineVersion,
    updateCluster_clusterName,
    updateClusterResponse_cluster,
    updateClusterResponse_httpStatus,

    -- ** UpdateParameterGroup
    updateParameterGroup_parameterGroupName,
    updateParameterGroup_parameterNameValues,
    updateParameterGroupResponse_parameterGroup,
    updateParameterGroupResponse_httpStatus,

    -- ** UpdateSubnetGroup
    updateSubnetGroup_description,
    updateSubnetGroup_subnetIds,
    updateSubnetGroup_subnetGroupName,
    updateSubnetGroupResponse_subnetGroup,
    updateSubnetGroupResponse_httpStatus,

    -- ** UpdateUser
    updateUser_accessString,
    updateUser_authenticationMode,
    updateUser_userName,
    updateUserResponse_user,
    updateUserResponse_httpStatus,

    -- * Types

    -- ** ACL
    acl_name,
    acl_arn,
    acl_pendingChanges,
    acl_clusters,
    acl_userNames,
    acl_status,
    acl_minimumEngineVersion,

    -- ** ACLPendingChanges
    aCLPendingChanges_userNamesToRemove,
    aCLPendingChanges_userNamesToAdd,

    -- ** ACLsUpdateStatus
    aCLsUpdateStatus_aCLToApply,

    -- ** Authentication
    authentication_type,
    authentication_passwordCount,

    -- ** AuthenticationMode
    authenticationMode_passwords,
    authenticationMode_type,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** Cluster
    cluster_subnetGroupName,
    cluster_parameterGroupName,
    cluster_name,
    cluster_aCLName,
    cluster_autoMinorVersionUpgrade,
    cluster_enginePatchVersion,
    cluster_snsTopicStatus,
    cluster_arn,
    cluster_tLSEnabled,
    cluster_clusterEndpoint,
    cluster_availabilityMode,
    cluster_status,
    cluster_description,
    cluster_nodeType,
    cluster_maintenanceWindow,
    cluster_snapshotWindow,
    cluster_numberOfShards,
    cluster_snapshotRetentionLimit,
    cluster_snsTopicArn,
    cluster_pendingUpdates,
    cluster_securityGroups,
    cluster_shards,
    cluster_kmsKeyId,
    cluster_parameterGroupStatus,
    cluster_dataTiering,
    cluster_engineVersion,

    -- ** ClusterConfiguration
    clusterConfiguration_port,
    clusterConfiguration_subnetGroupName,
    clusterConfiguration_parameterGroupName,
    clusterConfiguration_name,
    clusterConfiguration_topicArn,
    clusterConfiguration_description,
    clusterConfiguration_nodeType,
    clusterConfiguration_maintenanceWindow,
    clusterConfiguration_snapshotWindow,
    clusterConfiguration_snapshotRetentionLimit,
    clusterConfiguration_shards,
    clusterConfiguration_numShards,
    clusterConfiguration_vpcId,
    clusterConfiguration_engineVersion,

    -- ** ClusterPendingUpdates
    clusterPendingUpdates_resharding,
    clusterPendingUpdates_aCLs,
    clusterPendingUpdates_serviceUpdates,

    -- ** Endpoint
    endpoint_port,
    endpoint_address,

    -- ** EngineVersionInfo
    engineVersionInfo_parameterGroupFamily,
    engineVersionInfo_enginePatchVersion,
    engineVersionInfo_engineVersion,

    -- ** Event
    event_message,
    event_sourceName,
    event_date,
    event_sourceType,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** Node
    node_name,
    node_status,
    node_availabilityZone,
    node_createTime,
    node_endpoint,

    -- ** Parameter
    parameter_name,
    parameter_description,
    parameter_minimumEngineVersion,
    parameter_allowedValues,
    parameter_dataType,
    parameter_value,

    -- ** ParameterGroup
    parameterGroup_name,
    parameterGroup_arn,
    parameterGroup_description,
    parameterGroup_family,

    -- ** ParameterNameValue
    parameterNameValue_parameterValue,
    parameterNameValue_parameterName,

    -- ** PendingModifiedServiceUpdate
    pendingModifiedServiceUpdate_status,
    pendingModifiedServiceUpdate_serviceUpdateName,

    -- ** ReplicaConfigurationRequest
    replicaConfigurationRequest_replicaCount,

    -- ** ReshardingStatus
    reshardingStatus_slotMigration,

    -- ** SecurityGroupMembership
    securityGroupMembership_securityGroupId,
    securityGroupMembership_status,

    -- ** ServiceUpdate
    serviceUpdate_type,
    serviceUpdate_releaseDate,
    serviceUpdate_status,
    serviceUpdate_description,
    serviceUpdate_serviceUpdateName,
    serviceUpdate_autoUpdateStartDate,
    serviceUpdate_clusterName,
    serviceUpdate_nodesUpdated,

    -- ** ServiceUpdateRequest
    serviceUpdateRequest_serviceUpdateNameToApply,

    -- ** Shard
    shard_name,
    shard_nodes,
    shard_status,
    shard_numberOfNodes,
    shard_slots,

    -- ** ShardConfiguration
    shardConfiguration_slots,
    shardConfiguration_replicaCount,

    -- ** ShardConfigurationRequest
    shardConfigurationRequest_shardCount,

    -- ** ShardDetail
    shardDetail_name,
    shardDetail_configuration,
    shardDetail_snapshotCreationTime,
    shardDetail_size,

    -- ** SlotMigration
    slotMigration_progressPercentage,

    -- ** Snapshot
    snapshot_clusterConfiguration,
    snapshot_name,
    snapshot_arn,
    snapshot_status,
    snapshot_source,
    snapshot_kmsKeyId,
    snapshot_dataTiering,

    -- ** Subnet
    subnet_availabilityZone,
    subnet_identifier,

    -- ** SubnetGroup
    subnetGroup_name,
    subnetGroup_subnets,
    subnetGroup_arn,
    subnetGroup_description,
    subnetGroup_vpcId,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UnprocessedCluster
    unprocessedCluster_errorMessage,
    unprocessedCluster_clusterName,
    unprocessedCluster_errorType,

    -- ** User
    user_accessString,
    user_authentication,
    user_name,
    user_arn,
    user_status,
    user_minimumEngineVersion,
    user_aCLNames,
  )
where

import Amazonka.MemoryDb.BatchUpdateCluster
import Amazonka.MemoryDb.CopySnapshot
import Amazonka.MemoryDb.CreateACL
import Amazonka.MemoryDb.CreateCluster
import Amazonka.MemoryDb.CreateParameterGroup
import Amazonka.MemoryDb.CreateSnapshot
import Amazonka.MemoryDb.CreateSubnetGroup
import Amazonka.MemoryDb.CreateUser
import Amazonka.MemoryDb.DeleteACL
import Amazonka.MemoryDb.DeleteCluster
import Amazonka.MemoryDb.DeleteParameterGroup
import Amazonka.MemoryDb.DeleteSnapshot
import Amazonka.MemoryDb.DeleteSubnetGroup
import Amazonka.MemoryDb.DeleteUser
import Amazonka.MemoryDb.DescribeACLs
import Amazonka.MemoryDb.DescribeClusters
import Amazonka.MemoryDb.DescribeEngineVersions
import Amazonka.MemoryDb.DescribeEvents
import Amazonka.MemoryDb.DescribeParameterGroups
import Amazonka.MemoryDb.DescribeParameters
import Amazonka.MemoryDb.DescribeServiceUpdates
import Amazonka.MemoryDb.DescribeSnapshots
import Amazonka.MemoryDb.DescribeSubnetGroups
import Amazonka.MemoryDb.DescribeUsers
import Amazonka.MemoryDb.FailoverShard
import Amazonka.MemoryDb.ListAllowedNodeTypeUpdates
import Amazonka.MemoryDb.ListTags
import Amazonka.MemoryDb.ResetParameterGroup
import Amazonka.MemoryDb.TagResource
import Amazonka.MemoryDb.Types.ACL
import Amazonka.MemoryDb.Types.ACLPendingChanges
import Amazonka.MemoryDb.Types.ACLsUpdateStatus
import Amazonka.MemoryDb.Types.Authentication
import Amazonka.MemoryDb.Types.AuthenticationMode
import Amazonka.MemoryDb.Types.AvailabilityZone
import Amazonka.MemoryDb.Types.Cluster
import Amazonka.MemoryDb.Types.ClusterConfiguration
import Amazonka.MemoryDb.Types.ClusterPendingUpdates
import Amazonka.MemoryDb.Types.Endpoint
import Amazonka.MemoryDb.Types.EngineVersionInfo
import Amazonka.MemoryDb.Types.Event
import Amazonka.MemoryDb.Types.Filter
import Amazonka.MemoryDb.Types.Node
import Amazonka.MemoryDb.Types.Parameter
import Amazonka.MemoryDb.Types.ParameterGroup
import Amazonka.MemoryDb.Types.ParameterNameValue
import Amazonka.MemoryDb.Types.PendingModifiedServiceUpdate
import Amazonka.MemoryDb.Types.ReplicaConfigurationRequest
import Amazonka.MemoryDb.Types.ReshardingStatus
import Amazonka.MemoryDb.Types.SecurityGroupMembership
import Amazonka.MemoryDb.Types.ServiceUpdate
import Amazonka.MemoryDb.Types.ServiceUpdateRequest
import Amazonka.MemoryDb.Types.Shard
import Amazonka.MemoryDb.Types.ShardConfiguration
import Amazonka.MemoryDb.Types.ShardConfigurationRequest
import Amazonka.MemoryDb.Types.ShardDetail
import Amazonka.MemoryDb.Types.SlotMigration
import Amazonka.MemoryDb.Types.Snapshot
import Amazonka.MemoryDb.Types.Subnet
import Amazonka.MemoryDb.Types.SubnetGroup
import Amazonka.MemoryDb.Types.Tag
import Amazonka.MemoryDb.Types.UnprocessedCluster
import Amazonka.MemoryDb.Types.User
import Amazonka.MemoryDb.UntagResource
import Amazonka.MemoryDb.UpdateACL
import Amazonka.MemoryDb.UpdateCluster
import Amazonka.MemoryDb.UpdateParameterGroup
import Amazonka.MemoryDb.UpdateSubnetGroup
import Amazonka.MemoryDb.UpdateUser
