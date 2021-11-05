{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MemoryDb.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MemoryDb.Lens
  ( -- * Operations

    -- ** DescribeClusters
    describeClusters_showShardDetails,
    describeClusters_nextToken,
    describeClusters_clusterName,
    describeClusters_maxResults,
    describeClustersResponse_nextToken,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,

    -- ** BatchUpdateCluster
    batchUpdateCluster_serviceUpdate,
    batchUpdateCluster_clusterNames,
    batchUpdateClusterResponse_unprocessedClusters,
    batchUpdateClusterResponse_processedClusters,
    batchUpdateClusterResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_filters,
    describeUsers_userName,
    describeUsers_nextToken,
    describeUsers_maxResults,
    describeUsersResponse_users,
    describeUsersResponse_nextToken,
    describeUsersResponse_httpStatus,

    -- ** DescribeParameters
    describeParameters_nextToken,
    describeParameters_maxResults,
    describeParameters_parameterGroupName,
    describeParametersResponse_nextToken,
    describeParametersResponse_parameters,
    describeParametersResponse_httpStatus,

    -- ** DeleteACL
    deleteACL_aCLName,
    deleteACLResponse_acl,
    deleteACLResponse_httpStatus,

    -- ** UpdateACL
    updateACL_userNamesToAdd,
    updateACL_userNamesToRemove,
    updateACL_aCLName,
    updateACLResponse_acl,
    updateACLResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_sourceName,
    describeEvents_startTime,
    describeEvents_sourceType,
    describeEvents_nextToken,
    describeEvents_endTime,
    describeEvents_duration,
    describeEvents_maxResults,
    describeEventsResponse_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** DescribeEngineVersions
    describeEngineVersions_engineVersion,
    describeEngineVersions_defaultOnly,
    describeEngineVersions_nextToken,
    describeEngineVersions_parameterGroupFamily,
    describeEngineVersions_maxResults,
    describeEngineVersionsResponse_nextToken,
    describeEngineVersionsResponse_engineVersions,
    describeEngineVersionsResponse_httpStatus,

    -- ** DescribeACLs
    describeACLs_nextToken,
    describeACLs_maxResults,
    describeACLs_aCLName,
    describeACLsResponse_nextToken,
    describeACLsResponse_aCLs,
    describeACLsResponse_httpStatus,

    -- ** CreateSubnetGroup
    createSubnetGroup_description,
    createSubnetGroup_tags,
    createSubnetGroup_subnetGroupName,
    createSubnetGroup_subnetIds,
    createSubnetGroupResponse_subnetGroup,
    createSubnetGroupResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_finalSnapshotName,
    deleteCluster_clusterName,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_engineVersion,
    updateCluster_securityGroupIds,
    updateCluster_snsTopicArn,
    updateCluster_snapshotWindow,
    updateCluster_maintenanceWindow,
    updateCluster_shardConfiguration,
    updateCluster_snapshotRetentionLimit,
    updateCluster_replicaConfiguration,
    updateCluster_nodeType,
    updateCluster_snsTopicStatus,
    updateCluster_description,
    updateCluster_aCLName,
    updateCluster_parameterGroupName,
    updateCluster_clusterName,
    updateClusterResponse_cluster,
    updateClusterResponse_httpStatus,

    -- ** CreateCluster
    createCluster_engineVersion,
    createCluster_securityGroupIds,
    createCluster_snapshotArns,
    createCluster_autoMinorVersionUpgrade,
    createCluster_snsTopicArn,
    createCluster_snapshotWindow,
    createCluster_subnetGroupName,
    createCluster_tLSEnabled,
    createCluster_numShards,
    createCluster_maintenanceWindow,
    createCluster_kmsKeyId,
    createCluster_snapshotRetentionLimit,
    createCluster_snapshotName,
    createCluster_description,
    createCluster_numReplicasPerShard,
    createCluster_tags,
    createCluster_port,
    createCluster_parameterGroupName,
    createCluster_clusterName,
    createCluster_nodeType,
    createCluster_aCLName,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CopySnapshot
    copySnapshot_targetBucket,
    copySnapshot_kmsKeyId,
    copySnapshot_tags,
    copySnapshot_sourceSnapshotName,
    copySnapshot_targetSnapshotName,
    copySnapshotResponse_snapshot,
    copySnapshotResponse_httpStatus,

    -- ** DeleteParameterGroup
    deleteParameterGroup_parameterGroupName,
    deleteParameterGroupResponse_parameterGroup,
    deleteParameterGroupResponse_httpStatus,

    -- ** UpdateParameterGroup
    updateParameterGroup_parameterGroupName,
    updateParameterGroup_parameterNameValues,
    updateParameterGroupResponse_parameterGroup,
    updateParameterGroupResponse_httpStatus,

    -- ** DescribeSubnetGroups
    describeSubnetGroups_subnetGroupName,
    describeSubnetGroups_nextToken,
    describeSubnetGroups_maxResults,
    describeSubnetGroupsResponse_subnetGroups,
    describeSubnetGroupsResponse_nextToken,
    describeSubnetGroupsResponse_httpStatus,

    -- ** DescribeServiceUpdates
    describeServiceUpdates_status,
    describeServiceUpdates_serviceUpdateName,
    describeServiceUpdates_clusterNames,
    describeServiceUpdates_nextToken,
    describeServiceUpdates_maxResults,
    describeServiceUpdatesResponse_serviceUpdates,
    describeServiceUpdatesResponse_nextToken,
    describeServiceUpdatesResponse_httpStatus,

    -- ** CreateParameterGroup
    createParameterGroup_description,
    createParameterGroup_tags,
    createParameterGroup_parameterGroupName,
    createParameterGroup_family,
    createParameterGroupResponse_parameterGroup,
    createParameterGroupResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_showDetail,
    describeSnapshots_nextToken,
    describeSnapshots_source,
    describeSnapshots_clusterName,
    describeSnapshots_snapshotName,
    describeSnapshots_maxResults,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,

    -- ** CreateACL
    createACL_userNames,
    createACL_tags,
    createACL_aCLName,
    createACLResponse_acl,
    createACLResponse_httpStatus,

    -- ** UpdateSubnetGroup
    updateSubnetGroup_subnetIds,
    updateSubnetGroup_description,
    updateSubnetGroup_subnetGroupName,
    updateSubnetGroupResponse_subnetGroup,
    updateSubnetGroupResponse_httpStatus,

    -- ** DeleteSubnetGroup
    deleteSubnetGroup_subnetGroupName,
    deleteSubnetGroupResponse_subnetGroup,
    deleteSubnetGroupResponse_httpStatus,

    -- ** CreateUser
    createUser_tags,
    createUser_userName,
    createUser_authenticationMode,
    createUser_accessString,
    createUserResponse_user,
    createUserResponse_httpStatus,

    -- ** DeleteSnapshot
    deleteSnapshot_snapshotName,
    deleteSnapshotResponse_snapshot,
    deleteSnapshotResponse_httpStatus,

    -- ** FailoverShard
    failoverShard_clusterName,
    failoverShard_shardName,
    failoverShardResponse_cluster,
    failoverShardResponse_httpStatus,

    -- ** UpdateUser
    updateUser_authenticationMode,
    updateUser_accessString,
    updateUser_userName,
    updateUserResponse_user,
    updateUserResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_userName,
    deleteUserResponse_user,
    deleteUserResponse_httpStatus,

    -- ** ListAllowedNodeTypeUpdates
    listAllowedNodeTypeUpdates_clusterName,
    listAllowedNodeTypeUpdatesResponse_scaleUpNodeTypes,
    listAllowedNodeTypeUpdatesResponse_scaleDownNodeTypes,
    listAllowedNodeTypeUpdatesResponse_httpStatus,

    -- ** DescribeParameterGroups
    describeParameterGroups_nextToken,
    describeParameterGroups_maxResults,
    describeParameterGroups_parameterGroupName,
    describeParameterGroupsResponse_nextToken,
    describeParameterGroupsResponse_parameterGroups,
    describeParameterGroupsResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_kmsKeyId,
    createSnapshot_tags,
    createSnapshot_clusterName,
    createSnapshot_snapshotName,
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_tagList,
    tagResourceResponse_httpStatus,

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

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_tagList,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** ACL
    acl_status,
    acl_userNames,
    acl_arn,
    acl_name,
    acl_pendingChanges,
    acl_minimumEngineVersion,
    acl_clusters,

    -- ** ACLPendingChanges
    aCLPendingChanges_userNamesToAdd,
    aCLPendingChanges_userNamesToRemove,

    -- ** ACLsUpdateStatus
    aCLsUpdateStatus_aCLToApply,

    -- ** Authentication
    authentication_passwordCount,
    authentication_type,

    -- ** AuthenticationMode
    authenticationMode_passwords,
    authenticationMode_type,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** Cluster
    cluster_engineVersion,
    cluster_status,
    cluster_autoMinorVersionUpgrade,
    cluster_snsTopicArn,
    cluster_securityGroups,
    cluster_availabilityMode,
    cluster_parameterGroupStatus,
    cluster_arn,
    cluster_pendingUpdates,
    cluster_numberOfShards,
    cluster_snapshotWindow,
    cluster_subnetGroupName,
    cluster_tLSEnabled,
    cluster_maintenanceWindow,
    cluster_kmsKeyId,
    cluster_shards,
    cluster_name,
    cluster_enginePatchVersion,
    cluster_snapshotRetentionLimit,
    cluster_nodeType,
    cluster_snsTopicStatus,
    cluster_description,
    cluster_aCLName,
    cluster_clusterEndpoint,
    cluster_parameterGroupName,

    -- ** ClusterConfiguration
    clusterConfiguration_engineVersion,
    clusterConfiguration_vpcId,
    clusterConfiguration_snapshotWindow,
    clusterConfiguration_subnetGroupName,
    clusterConfiguration_numShards,
    clusterConfiguration_maintenanceWindow,
    clusterConfiguration_topicArn,
    clusterConfiguration_shards,
    clusterConfiguration_name,
    clusterConfiguration_snapshotRetentionLimit,
    clusterConfiguration_nodeType,
    clusterConfiguration_description,
    clusterConfiguration_port,
    clusterConfiguration_parameterGroupName,

    -- ** ClusterPendingUpdates
    clusterPendingUpdates_serviceUpdates,
    clusterPendingUpdates_resharding,
    clusterPendingUpdates_aCLs,

    -- ** Endpoint
    endpoint_address,
    endpoint_port,

    -- ** EngineVersionInfo
    engineVersionInfo_engineVersion,
    engineVersionInfo_enginePatchVersion,
    engineVersionInfo_parameterGroupFamily,

    -- ** Event
    event_sourceName,
    event_sourceType,
    event_date,
    event_message,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** Node
    node_status,
    node_availabilityZone,
    node_name,
    node_endpoint,
    node_createTime,

    -- ** Parameter
    parameter_value,
    parameter_name,
    parameter_minimumEngineVersion,
    parameter_dataType,
    parameter_allowedValues,
    parameter_description,

    -- ** ParameterGroup
    parameterGroup_arn,
    parameterGroup_family,
    parameterGroup_name,
    parameterGroup_description,

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
    securityGroupMembership_status,
    securityGroupMembership_securityGroupId,

    -- ** ServiceUpdate
    serviceUpdate_status,
    serviceUpdate_autoUpdateStartDate,
    serviceUpdate_serviceUpdateName,
    serviceUpdate_nodesUpdated,
    serviceUpdate_releaseDate,
    serviceUpdate_clusterName,
    serviceUpdate_type,
    serviceUpdate_description,

    -- ** ServiceUpdateRequest
    serviceUpdateRequest_serviceUpdateNameToApply,

    -- ** Shard
    shard_status,
    shard_slots,
    shard_numberOfNodes,
    shard_name,
    shard_nodes,

    -- ** ShardConfiguration
    shardConfiguration_slots,
    shardConfiguration_replicaCount,

    -- ** ShardConfigurationRequest
    shardConfigurationRequest_shardCount,

    -- ** ShardDetail
    shardDetail_size,
    shardDetail_snapshotCreationTime,
    shardDetail_name,
    shardDetail_configuration,

    -- ** SlotMigration
    slotMigration_progressPercentage,

    -- ** Snapshot
    snapshot_status,
    snapshot_arn,
    snapshot_kmsKeyId,
    snapshot_name,
    snapshot_clusterConfiguration,
    snapshot_source,

    -- ** Subnet
    subnet_identifier,
    subnet_availabilityZone,

    -- ** SubnetGroup
    subnetGroup_arn,
    subnetGroup_vpcId,
    subnetGroup_subnets,
    subnetGroup_name,
    subnetGroup_description,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** UnprocessedCluster
    unprocessedCluster_clusterName,
    unprocessedCluster_errorType,
    unprocessedCluster_errorMessage,

    -- ** User
    user_status,
    user_arn,
    user_authentication,
    user_accessString,
    user_name,
    user_aCLNames,
    user_minimumEngineVersion,
  )
where

import Network.AWS.MemoryDb.BatchUpdateCluster
import Network.AWS.MemoryDb.CopySnapshot
import Network.AWS.MemoryDb.CreateACL
import Network.AWS.MemoryDb.CreateCluster
import Network.AWS.MemoryDb.CreateParameterGroup
import Network.AWS.MemoryDb.CreateSnapshot
import Network.AWS.MemoryDb.CreateSubnetGroup
import Network.AWS.MemoryDb.CreateUser
import Network.AWS.MemoryDb.DeleteACL
import Network.AWS.MemoryDb.DeleteCluster
import Network.AWS.MemoryDb.DeleteParameterGroup
import Network.AWS.MemoryDb.DeleteSnapshot
import Network.AWS.MemoryDb.DeleteSubnetGroup
import Network.AWS.MemoryDb.DeleteUser
import Network.AWS.MemoryDb.DescribeACLs
import Network.AWS.MemoryDb.DescribeClusters
import Network.AWS.MemoryDb.DescribeEngineVersions
import Network.AWS.MemoryDb.DescribeEvents
import Network.AWS.MemoryDb.DescribeParameterGroups
import Network.AWS.MemoryDb.DescribeParameters
import Network.AWS.MemoryDb.DescribeServiceUpdates
import Network.AWS.MemoryDb.DescribeSnapshots
import Network.AWS.MemoryDb.DescribeSubnetGroups
import Network.AWS.MemoryDb.DescribeUsers
import Network.AWS.MemoryDb.FailoverShard
import Network.AWS.MemoryDb.ListAllowedNodeTypeUpdates
import Network.AWS.MemoryDb.ListTags
import Network.AWS.MemoryDb.ResetParameterGroup
import Network.AWS.MemoryDb.TagResource
import Network.AWS.MemoryDb.Types.ACL
import Network.AWS.MemoryDb.Types.ACLPendingChanges
import Network.AWS.MemoryDb.Types.ACLsUpdateStatus
import Network.AWS.MemoryDb.Types.Authentication
import Network.AWS.MemoryDb.Types.AuthenticationMode
import Network.AWS.MemoryDb.Types.AvailabilityZone
import Network.AWS.MemoryDb.Types.Cluster
import Network.AWS.MemoryDb.Types.ClusterConfiguration
import Network.AWS.MemoryDb.Types.ClusterPendingUpdates
import Network.AWS.MemoryDb.Types.Endpoint
import Network.AWS.MemoryDb.Types.EngineVersionInfo
import Network.AWS.MemoryDb.Types.Event
import Network.AWS.MemoryDb.Types.Filter
import Network.AWS.MemoryDb.Types.Node
import Network.AWS.MemoryDb.Types.Parameter
import Network.AWS.MemoryDb.Types.ParameterGroup
import Network.AWS.MemoryDb.Types.ParameterNameValue
import Network.AWS.MemoryDb.Types.PendingModifiedServiceUpdate
import Network.AWS.MemoryDb.Types.ReplicaConfigurationRequest
import Network.AWS.MemoryDb.Types.ReshardingStatus
import Network.AWS.MemoryDb.Types.SecurityGroupMembership
import Network.AWS.MemoryDb.Types.ServiceUpdate
import Network.AWS.MemoryDb.Types.ServiceUpdateRequest
import Network.AWS.MemoryDb.Types.Shard
import Network.AWS.MemoryDb.Types.ShardConfiguration
import Network.AWS.MemoryDb.Types.ShardConfigurationRequest
import Network.AWS.MemoryDb.Types.ShardDetail
import Network.AWS.MemoryDb.Types.SlotMigration
import Network.AWS.MemoryDb.Types.Snapshot
import Network.AWS.MemoryDb.Types.Subnet
import Network.AWS.MemoryDb.Types.SubnetGroup
import Network.AWS.MemoryDb.Types.Tag
import Network.AWS.MemoryDb.Types.UnprocessedCluster
import Network.AWS.MemoryDb.Types.User
import Network.AWS.MemoryDb.UntagResource
import Network.AWS.MemoryDb.UpdateACL
import Network.AWS.MemoryDb.UpdateCluster
import Network.AWS.MemoryDb.UpdateParameterGroup
import Network.AWS.MemoryDb.UpdateSubnetGroup
import Network.AWS.MemoryDb.UpdateUser
