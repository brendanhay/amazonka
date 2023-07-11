{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MemoryDb.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    copySnapshot_kmsKeyId,
    copySnapshot_tags,
    copySnapshot_targetBucket,
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
    createCluster_autoMinorVersionUpgrade,
    createCluster_dataTiering,
    createCluster_description,
    createCluster_engineVersion,
    createCluster_kmsKeyId,
    createCluster_maintenanceWindow,
    createCluster_numReplicasPerShard,
    createCluster_numShards,
    createCluster_parameterGroupName,
    createCluster_port,
    createCluster_securityGroupIds,
    createCluster_snapshotArns,
    createCluster_snapshotName,
    createCluster_snapshotRetentionLimit,
    createCluster_snapshotWindow,
    createCluster_snsTopicArn,
    createCluster_subnetGroupName,
    createCluster_tLSEnabled,
    createCluster_tags,
    createCluster_clusterName,
    createCluster_nodeType,
    createCluster_aCLName,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CreateParameterGroup
    createParameterGroup_description,
    createParameterGroup_tags,
    createParameterGroup_parameterGroupName,
    createParameterGroup_family,
    createParameterGroupResponse_parameterGroup,
    createParameterGroupResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_kmsKeyId,
    createSnapshot_tags,
    createSnapshot_clusterName,
    createSnapshot_snapshotName,
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,

    -- ** CreateSubnetGroup
    createSubnetGroup_description,
    createSubnetGroup_tags,
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
    describeACLs_aCLName,
    describeACLs_maxResults,
    describeACLs_nextToken,
    describeACLsResponse_aCLs,
    describeACLsResponse_nextToken,
    describeACLsResponse_httpStatus,

    -- ** DescribeClusters
    describeClusters_clusterName,
    describeClusters_maxResults,
    describeClusters_nextToken,
    describeClusters_showShardDetails,
    describeClustersResponse_clusters,
    describeClustersResponse_nextToken,
    describeClustersResponse_httpStatus,

    -- ** DescribeEngineVersions
    describeEngineVersions_defaultOnly,
    describeEngineVersions_engineVersion,
    describeEngineVersions_maxResults,
    describeEngineVersions_nextToken,
    describeEngineVersions_parameterGroupFamily,
    describeEngineVersionsResponse_engineVersions,
    describeEngineVersionsResponse_nextToken,
    describeEngineVersionsResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_duration,
    describeEvents_endTime,
    describeEvents_maxResults,
    describeEvents_nextToken,
    describeEvents_sourceName,
    describeEvents_sourceType,
    describeEvents_startTime,
    describeEventsResponse_events,
    describeEventsResponse_nextToken,
    describeEventsResponse_httpStatus,

    -- ** DescribeParameterGroups
    describeParameterGroups_maxResults,
    describeParameterGroups_nextToken,
    describeParameterGroups_parameterGroupName,
    describeParameterGroupsResponse_nextToken,
    describeParameterGroupsResponse_parameterGroups,
    describeParameterGroupsResponse_httpStatus,

    -- ** DescribeParameters
    describeParameters_maxResults,
    describeParameters_nextToken,
    describeParameters_parameterGroupName,
    describeParametersResponse_nextToken,
    describeParametersResponse_parameters,
    describeParametersResponse_httpStatus,

    -- ** DescribeReservedNodes
    describeReservedNodes_duration,
    describeReservedNodes_maxResults,
    describeReservedNodes_nextToken,
    describeReservedNodes_nodeType,
    describeReservedNodes_offeringType,
    describeReservedNodes_reservationId,
    describeReservedNodes_reservedNodesOfferingId,
    describeReservedNodesResponse_nextToken,
    describeReservedNodesResponse_reservedNodes,
    describeReservedNodesResponse_httpStatus,

    -- ** DescribeReservedNodesOfferings
    describeReservedNodesOfferings_duration,
    describeReservedNodesOfferings_maxResults,
    describeReservedNodesOfferings_nextToken,
    describeReservedNodesOfferings_nodeType,
    describeReservedNodesOfferings_offeringType,
    describeReservedNodesOfferings_reservedNodesOfferingId,
    describeReservedNodesOfferingsResponse_nextToken,
    describeReservedNodesOfferingsResponse_reservedNodesOfferings,
    describeReservedNodesOfferingsResponse_httpStatus,

    -- ** DescribeServiceUpdates
    describeServiceUpdates_clusterNames,
    describeServiceUpdates_maxResults,
    describeServiceUpdates_nextToken,
    describeServiceUpdates_serviceUpdateName,
    describeServiceUpdates_status,
    describeServiceUpdatesResponse_nextToken,
    describeServiceUpdatesResponse_serviceUpdates,
    describeServiceUpdatesResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_clusterName,
    describeSnapshots_maxResults,
    describeSnapshots_nextToken,
    describeSnapshots_showDetail,
    describeSnapshots_snapshotName,
    describeSnapshots_source,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,

    -- ** DescribeSubnetGroups
    describeSubnetGroups_maxResults,
    describeSubnetGroups_nextToken,
    describeSubnetGroups_subnetGroupName,
    describeSubnetGroupsResponse_nextToken,
    describeSubnetGroupsResponse_subnetGroups,
    describeSubnetGroupsResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_filters,
    describeUsers_maxResults,
    describeUsers_nextToken,
    describeUsers_userName,
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
    listAllowedNodeTypeUpdatesResponse_scaleDownNodeTypes,
    listAllowedNodeTypeUpdatesResponse_scaleUpNodeTypes,
    listAllowedNodeTypeUpdatesResponse_httpStatus,

    -- ** ListTags
    listTags_resourceArn,
    listTagsResponse_tagList,
    listTagsResponse_httpStatus,

    -- ** PurchaseReservedNodesOffering
    purchaseReservedNodesOffering_nodeCount,
    purchaseReservedNodesOffering_reservationId,
    purchaseReservedNodesOffering_tags,
    purchaseReservedNodesOffering_reservedNodesOfferingId,
    purchaseReservedNodesOfferingResponse_reservedNode,
    purchaseReservedNodesOfferingResponse_httpStatus,

    -- ** ResetParameterGroup
    resetParameterGroup_allParameters,
    resetParameterGroup_parameterNames,
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
    updateACL_userNamesToAdd,
    updateACL_userNamesToRemove,
    updateACL_aCLName,
    updateACLResponse_acl,
    updateACLResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_aCLName,
    updateCluster_description,
    updateCluster_engineVersion,
    updateCluster_maintenanceWindow,
    updateCluster_nodeType,
    updateCluster_parameterGroupName,
    updateCluster_replicaConfiguration,
    updateCluster_securityGroupIds,
    updateCluster_shardConfiguration,
    updateCluster_snapshotRetentionLimit,
    updateCluster_snapshotWindow,
    updateCluster_snsTopicArn,
    updateCluster_snsTopicStatus,
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
    acl_arn,
    acl_clusters,
    acl_minimumEngineVersion,
    acl_name,
    acl_pendingChanges,
    acl_status,
    acl_userNames,

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
    cluster_aCLName,
    cluster_arn,
    cluster_autoMinorVersionUpgrade,
    cluster_availabilityMode,
    cluster_clusterEndpoint,
    cluster_dataTiering,
    cluster_description,
    cluster_enginePatchVersion,
    cluster_engineVersion,
    cluster_kmsKeyId,
    cluster_maintenanceWindow,
    cluster_name,
    cluster_nodeType,
    cluster_numberOfShards,
    cluster_parameterGroupName,
    cluster_parameterGroupStatus,
    cluster_pendingUpdates,
    cluster_securityGroups,
    cluster_shards,
    cluster_snapshotRetentionLimit,
    cluster_snapshotWindow,
    cluster_snsTopicArn,
    cluster_snsTopicStatus,
    cluster_status,
    cluster_subnetGroupName,
    cluster_tLSEnabled,

    -- ** ClusterConfiguration
    clusterConfiguration_description,
    clusterConfiguration_engineVersion,
    clusterConfiguration_maintenanceWindow,
    clusterConfiguration_name,
    clusterConfiguration_nodeType,
    clusterConfiguration_numShards,
    clusterConfiguration_parameterGroupName,
    clusterConfiguration_port,
    clusterConfiguration_shards,
    clusterConfiguration_snapshotRetentionLimit,
    clusterConfiguration_snapshotWindow,
    clusterConfiguration_subnetGroupName,
    clusterConfiguration_topicArn,
    clusterConfiguration_vpcId,

    -- ** ClusterPendingUpdates
    clusterPendingUpdates_aCLs,
    clusterPendingUpdates_resharding,
    clusterPendingUpdates_serviceUpdates,

    -- ** Endpoint
    endpoint_address,
    endpoint_port,

    -- ** EngineVersionInfo
    engineVersionInfo_enginePatchVersion,
    engineVersionInfo_engineVersion,
    engineVersionInfo_parameterGroupFamily,

    -- ** Event
    event_date,
    event_message,
    event_sourceName,
    event_sourceType,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** Node
    node_availabilityZone,
    node_createTime,
    node_endpoint,
    node_name,
    node_status,

    -- ** Parameter
    parameter_allowedValues,
    parameter_dataType,
    parameter_description,
    parameter_minimumEngineVersion,
    parameter_name,
    parameter_value,

    -- ** ParameterGroup
    parameterGroup_arn,
    parameterGroup_description,
    parameterGroup_family,
    parameterGroup_name,

    -- ** ParameterNameValue
    parameterNameValue_parameterName,
    parameterNameValue_parameterValue,

    -- ** PendingModifiedServiceUpdate
    pendingModifiedServiceUpdate_serviceUpdateName,
    pendingModifiedServiceUpdate_status,

    -- ** RecurringCharge
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- ** ReplicaConfigurationRequest
    replicaConfigurationRequest_replicaCount,

    -- ** ReservedNode
    reservedNode_arn,
    reservedNode_duration,
    reservedNode_fixedPrice,
    reservedNode_nodeCount,
    reservedNode_nodeType,
    reservedNode_offeringType,
    reservedNode_recurringCharges,
    reservedNode_reservationId,
    reservedNode_reservedNodesOfferingId,
    reservedNode_startTime,
    reservedNode_state,

    -- ** ReservedNodesOffering
    reservedNodesOffering_duration,
    reservedNodesOffering_fixedPrice,
    reservedNodesOffering_nodeType,
    reservedNodesOffering_offeringType,
    reservedNodesOffering_recurringCharges,
    reservedNodesOffering_reservedNodesOfferingId,

    -- ** ReshardingStatus
    reshardingStatus_slotMigration,

    -- ** SecurityGroupMembership
    securityGroupMembership_securityGroupId,
    securityGroupMembership_status,

    -- ** ServiceUpdate
    serviceUpdate_autoUpdateStartDate,
    serviceUpdate_clusterName,
    serviceUpdate_description,
    serviceUpdate_nodesUpdated,
    serviceUpdate_releaseDate,
    serviceUpdate_serviceUpdateName,
    serviceUpdate_status,
    serviceUpdate_type,

    -- ** ServiceUpdateRequest
    serviceUpdateRequest_serviceUpdateNameToApply,

    -- ** Shard
    shard_name,
    shard_nodes,
    shard_numberOfNodes,
    shard_slots,
    shard_status,

    -- ** ShardConfiguration
    shardConfiguration_replicaCount,
    shardConfiguration_slots,

    -- ** ShardConfigurationRequest
    shardConfigurationRequest_shardCount,

    -- ** ShardDetail
    shardDetail_configuration,
    shardDetail_name,
    shardDetail_size,
    shardDetail_snapshotCreationTime,

    -- ** SlotMigration
    slotMigration_progressPercentage,

    -- ** Snapshot
    snapshot_arn,
    snapshot_clusterConfiguration,
    snapshot_dataTiering,
    snapshot_kmsKeyId,
    snapshot_name,
    snapshot_source,
    snapshot_status,

    -- ** Subnet
    subnet_availabilityZone,
    subnet_identifier,

    -- ** SubnetGroup
    subnetGroup_arn,
    subnetGroup_description,
    subnetGroup_name,
    subnetGroup_subnets,
    subnetGroup_vpcId,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UnprocessedCluster
    unprocessedCluster_clusterName,
    unprocessedCluster_errorMessage,
    unprocessedCluster_errorType,

    -- ** User
    user_aCLNames,
    user_arn,
    user_accessString,
    user_authentication,
    user_minimumEngineVersion,
    user_name,
    user_status,
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
import Amazonka.MemoryDb.DescribeReservedNodes
import Amazonka.MemoryDb.DescribeReservedNodesOfferings
import Amazonka.MemoryDb.DescribeServiceUpdates
import Amazonka.MemoryDb.DescribeSnapshots
import Amazonka.MemoryDb.DescribeSubnetGroups
import Amazonka.MemoryDb.DescribeUsers
import Amazonka.MemoryDb.FailoverShard
import Amazonka.MemoryDb.ListAllowedNodeTypeUpdates
import Amazonka.MemoryDb.ListTags
import Amazonka.MemoryDb.PurchaseReservedNodesOffering
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
import Amazonka.MemoryDb.Types.RecurringCharge
import Amazonka.MemoryDb.Types.ReplicaConfigurationRequest
import Amazonka.MemoryDb.Types.ReservedNode
import Amazonka.MemoryDb.Types.ReservedNodesOffering
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
