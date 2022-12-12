{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElastiCache.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Lens
  ( -- * Operations

    -- ** AddTagsToResource
    addTagsToResource_resourceName,
    addTagsToResource_tags,
    tagListMessage_tagList,

    -- ** AuthorizeCacheSecurityGroupIngress
    authorizeCacheSecurityGroupIngress_cacheSecurityGroupName,
    authorizeCacheSecurityGroupIngress_eC2SecurityGroupName,
    authorizeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId,
    authorizeCacheSecurityGroupIngressResponse_cacheSecurityGroup,
    authorizeCacheSecurityGroupIngressResponse_httpStatus,

    -- ** BatchApplyUpdateAction
    batchApplyUpdateAction_cacheClusterIds,
    batchApplyUpdateAction_replicationGroupIds,
    batchApplyUpdateAction_serviceUpdateName,
    updateActionResultsMessage_processedUpdateActions,
    updateActionResultsMessage_unprocessedUpdateActions,

    -- ** BatchStopUpdateAction
    batchStopUpdateAction_cacheClusterIds,
    batchStopUpdateAction_replicationGroupIds,
    batchStopUpdateAction_serviceUpdateName,
    updateActionResultsMessage_processedUpdateActions,
    updateActionResultsMessage_unprocessedUpdateActions,

    -- ** CompleteMigration
    completeMigration_force,
    completeMigration_replicationGroupId,
    completeMigrationResponse_replicationGroup,
    completeMigrationResponse_httpStatus,

    -- ** CopySnapshot
    copySnapshot_kmsKeyId,
    copySnapshot_tags,
    copySnapshot_targetBucket,
    copySnapshot_sourceSnapshotName,
    copySnapshot_targetSnapshotName,
    copySnapshotResponse_snapshot,
    copySnapshotResponse_httpStatus,

    -- ** CreateCacheCluster
    createCacheCluster_aZMode,
    createCacheCluster_authToken,
    createCacheCluster_autoMinorVersionUpgrade,
    createCacheCluster_cacheNodeType,
    createCacheCluster_cacheParameterGroupName,
    createCacheCluster_cacheSecurityGroupNames,
    createCacheCluster_cacheSubnetGroupName,
    createCacheCluster_engine,
    createCacheCluster_engineVersion,
    createCacheCluster_ipDiscovery,
    createCacheCluster_logDeliveryConfigurations,
    createCacheCluster_networkType,
    createCacheCluster_notificationTopicArn,
    createCacheCluster_numCacheNodes,
    createCacheCluster_outpostMode,
    createCacheCluster_port,
    createCacheCluster_preferredAvailabilityZone,
    createCacheCluster_preferredAvailabilityZones,
    createCacheCluster_preferredMaintenanceWindow,
    createCacheCluster_preferredOutpostArn,
    createCacheCluster_preferredOutpostArns,
    createCacheCluster_replicationGroupId,
    createCacheCluster_securityGroupIds,
    createCacheCluster_snapshotArns,
    createCacheCluster_snapshotName,
    createCacheCluster_snapshotRetentionLimit,
    createCacheCluster_snapshotWindow,
    createCacheCluster_tags,
    createCacheCluster_transitEncryptionEnabled,
    createCacheCluster_cacheClusterId,
    createCacheClusterResponse_cacheCluster,
    createCacheClusterResponse_httpStatus,

    -- ** CreateCacheParameterGroup
    createCacheParameterGroup_tags,
    createCacheParameterGroup_cacheParameterGroupName,
    createCacheParameterGroup_cacheParameterGroupFamily,
    createCacheParameterGroup_description,
    createCacheParameterGroupResponse_cacheParameterGroup,
    createCacheParameterGroupResponse_httpStatus,

    -- ** CreateCacheSecurityGroup
    createCacheSecurityGroup_tags,
    createCacheSecurityGroup_cacheSecurityGroupName,
    createCacheSecurityGroup_description,
    createCacheSecurityGroupResponse_cacheSecurityGroup,
    createCacheSecurityGroupResponse_httpStatus,

    -- ** CreateCacheSubnetGroup
    createCacheSubnetGroup_tags,
    createCacheSubnetGroup_cacheSubnetGroupName,
    createCacheSubnetGroup_cacheSubnetGroupDescription,
    createCacheSubnetGroup_subnetIds,
    createCacheSubnetGroupResponse_cacheSubnetGroup,
    createCacheSubnetGroupResponse_httpStatus,

    -- ** CreateGlobalReplicationGroup
    createGlobalReplicationGroup_globalReplicationGroupDescription,
    createGlobalReplicationGroup_globalReplicationGroupIdSuffix,
    createGlobalReplicationGroup_primaryReplicationGroupId,
    createGlobalReplicationGroupResponse_globalReplicationGroup,
    createGlobalReplicationGroupResponse_httpStatus,

    -- ** CreateReplicationGroup
    createReplicationGroup_atRestEncryptionEnabled,
    createReplicationGroup_authToken,
    createReplicationGroup_autoMinorVersionUpgrade,
    createReplicationGroup_automaticFailoverEnabled,
    createReplicationGroup_cacheNodeType,
    createReplicationGroup_cacheParameterGroupName,
    createReplicationGroup_cacheSecurityGroupNames,
    createReplicationGroup_cacheSubnetGroupName,
    createReplicationGroup_dataTieringEnabled,
    createReplicationGroup_engine,
    createReplicationGroup_engineVersion,
    createReplicationGroup_globalReplicationGroupId,
    createReplicationGroup_ipDiscovery,
    createReplicationGroup_kmsKeyId,
    createReplicationGroup_logDeliveryConfigurations,
    createReplicationGroup_multiAZEnabled,
    createReplicationGroup_networkType,
    createReplicationGroup_nodeGroupConfiguration,
    createReplicationGroup_notificationTopicArn,
    createReplicationGroup_numCacheClusters,
    createReplicationGroup_numNodeGroups,
    createReplicationGroup_port,
    createReplicationGroup_preferredCacheClusterAZs,
    createReplicationGroup_preferredMaintenanceWindow,
    createReplicationGroup_primaryClusterId,
    createReplicationGroup_replicasPerNodeGroup,
    createReplicationGroup_securityGroupIds,
    createReplicationGroup_snapshotArns,
    createReplicationGroup_snapshotName,
    createReplicationGroup_snapshotRetentionLimit,
    createReplicationGroup_snapshotWindow,
    createReplicationGroup_tags,
    createReplicationGroup_transitEncryptionEnabled,
    createReplicationGroup_userGroupIds,
    createReplicationGroup_replicationGroupId,
    createReplicationGroup_replicationGroupDescription,
    createReplicationGroupResponse_replicationGroup,
    createReplicationGroupResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_cacheClusterId,
    createSnapshot_kmsKeyId,
    createSnapshot_replicationGroupId,
    createSnapshot_tags,
    createSnapshot_snapshotName,
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,

    -- ** CreateUser
    createUser_authenticationMode,
    createUser_noPasswordRequired,
    createUser_passwords,
    createUser_tags,
    createUser_userId,
    createUser_userName,
    createUser_engine,
    createUser_accessString,
    user_arn,
    user_accessString,
    user_authentication,
    user_engine,
    user_minimumEngineVersion,
    user_status,
    user_userGroupIds,
    user_userId,
    user_userName,

    -- ** CreateUserGroup
    createUserGroup_tags,
    createUserGroup_userIds,
    createUserGroup_userGroupId,
    createUserGroup_engine,
    userGroup_arn,
    userGroup_engine,
    userGroup_minimumEngineVersion,
    userGroup_pendingChanges,
    userGroup_replicationGroups,
    userGroup_status,
    userGroup_userGroupId,
    userGroup_userIds,

    -- ** DecreaseNodeGroupsInGlobalReplicationGroup
    decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRemove,
    decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRetain,
    decreaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId,
    decreaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount,
    decreaseNodeGroupsInGlobalReplicationGroup_applyImmediately,
    decreaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup,
    decreaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus,

    -- ** DecreaseReplicaCount
    decreaseReplicaCount_newReplicaCount,
    decreaseReplicaCount_replicaConfiguration,
    decreaseReplicaCount_replicasToRemove,
    decreaseReplicaCount_replicationGroupId,
    decreaseReplicaCount_applyImmediately,
    decreaseReplicaCountResponse_replicationGroup,
    decreaseReplicaCountResponse_httpStatus,

    -- ** DeleteCacheCluster
    deleteCacheCluster_finalSnapshotIdentifier,
    deleteCacheCluster_cacheClusterId,
    deleteCacheClusterResponse_cacheCluster,
    deleteCacheClusterResponse_httpStatus,

    -- ** DeleteCacheParameterGroup
    deleteCacheParameterGroup_cacheParameterGroupName,

    -- ** DeleteCacheSecurityGroup
    deleteCacheSecurityGroup_cacheSecurityGroupName,

    -- ** DeleteCacheSubnetGroup
    deleteCacheSubnetGroup_cacheSubnetGroupName,

    -- ** DeleteGlobalReplicationGroup
    deleteGlobalReplicationGroup_globalReplicationGroupId,
    deleteGlobalReplicationGroup_retainPrimaryReplicationGroup,
    deleteGlobalReplicationGroupResponse_globalReplicationGroup,
    deleteGlobalReplicationGroupResponse_httpStatus,

    -- ** DeleteReplicationGroup
    deleteReplicationGroup_finalSnapshotIdentifier,
    deleteReplicationGroup_retainPrimaryCluster,
    deleteReplicationGroup_replicationGroupId,
    deleteReplicationGroupResponse_replicationGroup,
    deleteReplicationGroupResponse_httpStatus,

    -- ** DeleteSnapshot
    deleteSnapshot_snapshotName,
    deleteSnapshotResponse_snapshot,
    deleteSnapshotResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_userId,
    user_arn,
    user_accessString,
    user_authentication,
    user_engine,
    user_minimumEngineVersion,
    user_status,
    user_userGroupIds,
    user_userId,
    user_userName,

    -- ** DeleteUserGroup
    deleteUserGroup_userGroupId,
    userGroup_arn,
    userGroup_engine,
    userGroup_minimumEngineVersion,
    userGroup_pendingChanges,
    userGroup_replicationGroups,
    userGroup_status,
    userGroup_userGroupId,
    userGroup_userIds,

    -- ** DescribeCacheClusters
    describeCacheClusters_cacheClusterId,
    describeCacheClusters_marker,
    describeCacheClusters_maxRecords,
    describeCacheClusters_showCacheClustersNotInReplicationGroups,
    describeCacheClusters_showCacheNodeInfo,
    describeCacheClustersResponse_cacheClusters,
    describeCacheClustersResponse_marker,
    describeCacheClustersResponse_httpStatus,

    -- ** DescribeCacheEngineVersions
    describeCacheEngineVersions_cacheParameterGroupFamily,
    describeCacheEngineVersions_defaultOnly,
    describeCacheEngineVersions_engine,
    describeCacheEngineVersions_engineVersion,
    describeCacheEngineVersions_marker,
    describeCacheEngineVersions_maxRecords,
    describeCacheEngineVersionsResponse_cacheEngineVersions,
    describeCacheEngineVersionsResponse_marker,
    describeCacheEngineVersionsResponse_httpStatus,

    -- ** DescribeCacheParameterGroups
    describeCacheParameterGroups_cacheParameterGroupName,
    describeCacheParameterGroups_marker,
    describeCacheParameterGroups_maxRecords,
    describeCacheParameterGroupsResponse_cacheParameterGroups,
    describeCacheParameterGroupsResponse_marker,
    describeCacheParameterGroupsResponse_httpStatus,

    -- ** DescribeCacheParameters
    describeCacheParameters_marker,
    describeCacheParameters_maxRecords,
    describeCacheParameters_source,
    describeCacheParameters_cacheParameterGroupName,
    describeCacheParametersResponse_cacheNodeTypeSpecificParameters,
    describeCacheParametersResponse_marker,
    describeCacheParametersResponse_parameters,
    describeCacheParametersResponse_httpStatus,

    -- ** DescribeCacheSecurityGroups
    describeCacheSecurityGroups_cacheSecurityGroupName,
    describeCacheSecurityGroups_marker,
    describeCacheSecurityGroups_maxRecords,
    describeCacheSecurityGroupsResponse_cacheSecurityGroups,
    describeCacheSecurityGroupsResponse_marker,
    describeCacheSecurityGroupsResponse_httpStatus,

    -- ** DescribeCacheSubnetGroups
    describeCacheSubnetGroups_cacheSubnetGroupName,
    describeCacheSubnetGroups_marker,
    describeCacheSubnetGroups_maxRecords,
    describeCacheSubnetGroupsResponse_cacheSubnetGroups,
    describeCacheSubnetGroupsResponse_marker,
    describeCacheSubnetGroupsResponse_httpStatus,

    -- ** DescribeEngineDefaultParameters
    describeEngineDefaultParameters_marker,
    describeEngineDefaultParameters_maxRecords,
    describeEngineDefaultParameters_cacheParameterGroupFamily,
    describeEngineDefaultParametersResponse_httpStatus,
    describeEngineDefaultParametersResponse_engineDefaults,

    -- ** DescribeEvents
    describeEvents_duration,
    describeEvents_endTime,
    describeEvents_marker,
    describeEvents_maxRecords,
    describeEvents_sourceIdentifier,
    describeEvents_sourceType,
    describeEvents_startTime,
    describeEventsResponse_events,
    describeEventsResponse_marker,
    describeEventsResponse_httpStatus,

    -- ** DescribeGlobalReplicationGroups
    describeGlobalReplicationGroups_globalReplicationGroupId,
    describeGlobalReplicationGroups_marker,
    describeGlobalReplicationGroups_maxRecords,
    describeGlobalReplicationGroups_showMemberInfo,
    describeGlobalReplicationGroupsResponse_globalReplicationGroups,
    describeGlobalReplicationGroupsResponse_marker,
    describeGlobalReplicationGroupsResponse_httpStatus,

    -- ** DescribeReplicationGroups
    describeReplicationGroups_marker,
    describeReplicationGroups_maxRecords,
    describeReplicationGroups_replicationGroupId,
    describeReplicationGroupsResponse_marker,
    describeReplicationGroupsResponse_replicationGroups,
    describeReplicationGroupsResponse_httpStatus,

    -- ** DescribeReservedCacheNodes
    describeReservedCacheNodes_cacheNodeType,
    describeReservedCacheNodes_duration,
    describeReservedCacheNodes_marker,
    describeReservedCacheNodes_maxRecords,
    describeReservedCacheNodes_offeringType,
    describeReservedCacheNodes_productDescription,
    describeReservedCacheNodes_reservedCacheNodeId,
    describeReservedCacheNodes_reservedCacheNodesOfferingId,
    describeReservedCacheNodesResponse_marker,
    describeReservedCacheNodesResponse_reservedCacheNodes,
    describeReservedCacheNodesResponse_httpStatus,

    -- ** DescribeReservedCacheNodesOfferings
    describeReservedCacheNodesOfferings_cacheNodeType,
    describeReservedCacheNodesOfferings_duration,
    describeReservedCacheNodesOfferings_marker,
    describeReservedCacheNodesOfferings_maxRecords,
    describeReservedCacheNodesOfferings_offeringType,
    describeReservedCacheNodesOfferings_productDescription,
    describeReservedCacheNodesOfferings_reservedCacheNodesOfferingId,
    describeReservedCacheNodesOfferingsResponse_marker,
    describeReservedCacheNodesOfferingsResponse_reservedCacheNodesOfferings,
    describeReservedCacheNodesOfferingsResponse_httpStatus,

    -- ** DescribeServiceUpdates
    describeServiceUpdates_marker,
    describeServiceUpdates_maxRecords,
    describeServiceUpdates_serviceUpdateName,
    describeServiceUpdates_serviceUpdateStatus,
    describeServiceUpdatesResponse_marker,
    describeServiceUpdatesResponse_serviceUpdates,
    describeServiceUpdatesResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_cacheClusterId,
    describeSnapshots_marker,
    describeSnapshots_maxRecords,
    describeSnapshots_replicationGroupId,
    describeSnapshots_showNodeGroupConfig,
    describeSnapshots_snapshotName,
    describeSnapshots_snapshotSource,
    describeSnapshotsResponse_marker,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,

    -- ** DescribeUpdateActions
    describeUpdateActions_cacheClusterIds,
    describeUpdateActions_engine,
    describeUpdateActions_marker,
    describeUpdateActions_maxRecords,
    describeUpdateActions_replicationGroupIds,
    describeUpdateActions_serviceUpdateName,
    describeUpdateActions_serviceUpdateStatus,
    describeUpdateActions_serviceUpdateTimeRange,
    describeUpdateActions_showNodeLevelUpdateStatus,
    describeUpdateActions_updateActionStatus,
    describeUpdateActionsResponse_marker,
    describeUpdateActionsResponse_updateActions,
    describeUpdateActionsResponse_httpStatus,

    -- ** DescribeUserGroups
    describeUserGroups_marker,
    describeUserGroups_maxRecords,
    describeUserGroups_userGroupId,
    describeUserGroupsResponse_marker,
    describeUserGroupsResponse_userGroups,
    describeUserGroupsResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_engine,
    describeUsers_filters,
    describeUsers_marker,
    describeUsers_maxRecords,
    describeUsers_userId,
    describeUsersResponse_marker,
    describeUsersResponse_users,
    describeUsersResponse_httpStatus,

    -- ** DisassociateGlobalReplicationGroup
    disassociateGlobalReplicationGroup_globalReplicationGroupId,
    disassociateGlobalReplicationGroup_replicationGroupId,
    disassociateGlobalReplicationGroup_replicationGroupRegion,
    disassociateGlobalReplicationGroupResponse_globalReplicationGroup,
    disassociateGlobalReplicationGroupResponse_httpStatus,

    -- ** FailoverGlobalReplicationGroup
    failoverGlobalReplicationGroup_globalReplicationGroupId,
    failoverGlobalReplicationGroup_primaryRegion,
    failoverGlobalReplicationGroup_primaryReplicationGroupId,
    failoverGlobalReplicationGroupResponse_globalReplicationGroup,
    failoverGlobalReplicationGroupResponse_httpStatus,

    -- ** IncreaseNodeGroupsInGlobalReplicationGroup
    increaseNodeGroupsInGlobalReplicationGroup_regionalConfigurations,
    increaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId,
    increaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount,
    increaseNodeGroupsInGlobalReplicationGroup_applyImmediately,
    increaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup,
    increaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus,

    -- ** IncreaseReplicaCount
    increaseReplicaCount_newReplicaCount,
    increaseReplicaCount_replicaConfiguration,
    increaseReplicaCount_replicationGroupId,
    increaseReplicaCount_applyImmediately,
    increaseReplicaCountResponse_replicationGroup,
    increaseReplicaCountResponse_httpStatus,

    -- ** ListAllowedNodeTypeModifications
    listAllowedNodeTypeModifications_cacheClusterId,
    listAllowedNodeTypeModifications_replicationGroupId,
    listAllowedNodeTypeModificationsResponse_scaleDownModifications,
    listAllowedNodeTypeModificationsResponse_scaleUpModifications,
    listAllowedNodeTypeModificationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceName,
    tagListMessage_tagList,

    -- ** ModifyCacheCluster
    modifyCacheCluster_aZMode,
    modifyCacheCluster_applyImmediately,
    modifyCacheCluster_authToken,
    modifyCacheCluster_authTokenUpdateStrategy,
    modifyCacheCluster_autoMinorVersionUpgrade,
    modifyCacheCluster_cacheNodeIdsToRemove,
    modifyCacheCluster_cacheNodeType,
    modifyCacheCluster_cacheParameterGroupName,
    modifyCacheCluster_cacheSecurityGroupNames,
    modifyCacheCluster_engineVersion,
    modifyCacheCluster_ipDiscovery,
    modifyCacheCluster_logDeliveryConfigurations,
    modifyCacheCluster_newAvailabilityZones,
    modifyCacheCluster_notificationTopicArn,
    modifyCacheCluster_notificationTopicStatus,
    modifyCacheCluster_numCacheNodes,
    modifyCacheCluster_preferredMaintenanceWindow,
    modifyCacheCluster_securityGroupIds,
    modifyCacheCluster_snapshotRetentionLimit,
    modifyCacheCluster_snapshotWindow,
    modifyCacheCluster_cacheClusterId,
    modifyCacheClusterResponse_cacheCluster,
    modifyCacheClusterResponse_httpStatus,

    -- ** ModifyCacheParameterGroup
    modifyCacheParameterGroup_cacheParameterGroupName,
    modifyCacheParameterGroup_parameterNameValues,
    cacheParameterGroupNameMessage_cacheParameterGroupName,

    -- ** ModifyCacheSubnetGroup
    modifyCacheSubnetGroup_cacheSubnetGroupDescription,
    modifyCacheSubnetGroup_subnetIds,
    modifyCacheSubnetGroup_cacheSubnetGroupName,
    modifyCacheSubnetGroupResponse_cacheSubnetGroup,
    modifyCacheSubnetGroupResponse_httpStatus,

    -- ** ModifyGlobalReplicationGroup
    modifyGlobalReplicationGroup_automaticFailoverEnabled,
    modifyGlobalReplicationGroup_cacheNodeType,
    modifyGlobalReplicationGroup_cacheParameterGroupName,
    modifyGlobalReplicationGroup_engineVersion,
    modifyGlobalReplicationGroup_globalReplicationGroupDescription,
    modifyGlobalReplicationGroup_globalReplicationGroupId,
    modifyGlobalReplicationGroup_applyImmediately,
    modifyGlobalReplicationGroupResponse_globalReplicationGroup,
    modifyGlobalReplicationGroupResponse_httpStatus,

    -- ** ModifyReplicationGroup
    modifyReplicationGroup_applyImmediately,
    modifyReplicationGroup_authToken,
    modifyReplicationGroup_authTokenUpdateStrategy,
    modifyReplicationGroup_autoMinorVersionUpgrade,
    modifyReplicationGroup_automaticFailoverEnabled,
    modifyReplicationGroup_cacheNodeType,
    modifyReplicationGroup_cacheParameterGroupName,
    modifyReplicationGroup_cacheSecurityGroupNames,
    modifyReplicationGroup_engineVersion,
    modifyReplicationGroup_ipDiscovery,
    modifyReplicationGroup_logDeliveryConfigurations,
    modifyReplicationGroup_multiAZEnabled,
    modifyReplicationGroup_nodeGroupId,
    modifyReplicationGroup_notificationTopicArn,
    modifyReplicationGroup_notificationTopicStatus,
    modifyReplicationGroup_preferredMaintenanceWindow,
    modifyReplicationGroup_primaryClusterId,
    modifyReplicationGroup_removeUserGroups,
    modifyReplicationGroup_replicationGroupDescription,
    modifyReplicationGroup_securityGroupIds,
    modifyReplicationGroup_snapshotRetentionLimit,
    modifyReplicationGroup_snapshotWindow,
    modifyReplicationGroup_snapshottingClusterId,
    modifyReplicationGroup_userGroupIdsToAdd,
    modifyReplicationGroup_userGroupIdsToRemove,
    modifyReplicationGroup_replicationGroupId,
    modifyReplicationGroupResponse_replicationGroup,
    modifyReplicationGroupResponse_httpStatus,

    -- ** ModifyReplicationGroupShardConfiguration
    modifyReplicationGroupShardConfiguration_nodeGroupsToRemove,
    modifyReplicationGroupShardConfiguration_nodeGroupsToRetain,
    modifyReplicationGroupShardConfiguration_reshardingConfiguration,
    modifyReplicationGroupShardConfiguration_replicationGroupId,
    modifyReplicationGroupShardConfiguration_nodeGroupCount,
    modifyReplicationGroupShardConfiguration_applyImmediately,
    modifyReplicationGroupShardConfigurationResponse_replicationGroup,
    modifyReplicationGroupShardConfigurationResponse_httpStatus,

    -- ** ModifyUser
    modifyUser_accessString,
    modifyUser_appendAccessString,
    modifyUser_authenticationMode,
    modifyUser_noPasswordRequired,
    modifyUser_passwords,
    modifyUser_userId,
    user_arn,
    user_accessString,
    user_authentication,
    user_engine,
    user_minimumEngineVersion,
    user_status,
    user_userGroupIds,
    user_userId,
    user_userName,

    -- ** ModifyUserGroup
    modifyUserGroup_userIdsToAdd,
    modifyUserGroup_userIdsToRemove,
    modifyUserGroup_userGroupId,
    userGroup_arn,
    userGroup_engine,
    userGroup_minimumEngineVersion,
    userGroup_pendingChanges,
    userGroup_replicationGroups,
    userGroup_status,
    userGroup_userGroupId,
    userGroup_userIds,

    -- ** PurchaseReservedCacheNodesOffering
    purchaseReservedCacheNodesOffering_cacheNodeCount,
    purchaseReservedCacheNodesOffering_reservedCacheNodeId,
    purchaseReservedCacheNodesOffering_tags,
    purchaseReservedCacheNodesOffering_reservedCacheNodesOfferingId,
    purchaseReservedCacheNodesOfferingResponse_reservedCacheNode,
    purchaseReservedCacheNodesOfferingResponse_httpStatus,

    -- ** RebalanceSlotsInGlobalReplicationGroup
    rebalanceSlotsInGlobalReplicationGroup_globalReplicationGroupId,
    rebalanceSlotsInGlobalReplicationGroup_applyImmediately,
    rebalanceSlotsInGlobalReplicationGroupResponse_globalReplicationGroup,
    rebalanceSlotsInGlobalReplicationGroupResponse_httpStatus,

    -- ** RebootCacheCluster
    rebootCacheCluster_cacheClusterId,
    rebootCacheCluster_cacheNodeIdsToReboot,
    rebootCacheClusterResponse_cacheCluster,
    rebootCacheClusterResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceName,
    removeTagsFromResource_tagKeys,
    tagListMessage_tagList,

    -- ** ResetCacheParameterGroup
    resetCacheParameterGroup_parameterNameValues,
    resetCacheParameterGroup_resetAllParameters,
    resetCacheParameterGroup_cacheParameterGroupName,
    cacheParameterGroupNameMessage_cacheParameterGroupName,

    -- ** RevokeCacheSecurityGroupIngress
    revokeCacheSecurityGroupIngress_cacheSecurityGroupName,
    revokeCacheSecurityGroupIngress_eC2SecurityGroupName,
    revokeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId,
    revokeCacheSecurityGroupIngressResponse_cacheSecurityGroup,
    revokeCacheSecurityGroupIngressResponse_httpStatus,

    -- ** StartMigration
    startMigration_replicationGroupId,
    startMigration_customerNodeEndpointList,
    startMigrationResponse_replicationGroup,
    startMigrationResponse_httpStatus,

    -- ** TestFailover
    testFailover_replicationGroupId,
    testFailover_nodeGroupId,
    testFailoverResponse_replicationGroup,
    testFailoverResponse_httpStatus,

    -- * Types

    -- ** Authentication
    authentication_passwordCount,
    authentication_type,

    -- ** AuthenticationMode
    authenticationMode_passwords,
    authenticationMode_type,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** CacheCluster
    cacheCluster_arn,
    cacheCluster_atRestEncryptionEnabled,
    cacheCluster_authTokenEnabled,
    cacheCluster_authTokenLastModifiedDate,
    cacheCluster_autoMinorVersionUpgrade,
    cacheCluster_cacheClusterCreateTime,
    cacheCluster_cacheClusterId,
    cacheCluster_cacheClusterStatus,
    cacheCluster_cacheNodeType,
    cacheCluster_cacheNodes,
    cacheCluster_cacheParameterGroup,
    cacheCluster_cacheSecurityGroups,
    cacheCluster_cacheSubnetGroupName,
    cacheCluster_clientDownloadLandingPage,
    cacheCluster_configurationEndpoint,
    cacheCluster_engine,
    cacheCluster_engineVersion,
    cacheCluster_ipDiscovery,
    cacheCluster_logDeliveryConfigurations,
    cacheCluster_networkType,
    cacheCluster_notificationConfiguration,
    cacheCluster_numCacheNodes,
    cacheCluster_pendingModifiedValues,
    cacheCluster_preferredAvailabilityZone,
    cacheCluster_preferredMaintenanceWindow,
    cacheCluster_preferredOutpostArn,
    cacheCluster_replicationGroupId,
    cacheCluster_replicationGroupLogDeliveryEnabled,
    cacheCluster_securityGroups,
    cacheCluster_snapshotRetentionLimit,
    cacheCluster_snapshotWindow,
    cacheCluster_transitEncryptionEnabled,

    -- ** CacheEngineVersion
    cacheEngineVersion_cacheEngineDescription,
    cacheEngineVersion_cacheEngineVersionDescription,
    cacheEngineVersion_cacheParameterGroupFamily,
    cacheEngineVersion_engine,
    cacheEngineVersion_engineVersion,

    -- ** CacheNode
    cacheNode_cacheNodeCreateTime,
    cacheNode_cacheNodeId,
    cacheNode_cacheNodeStatus,
    cacheNode_customerAvailabilityZone,
    cacheNode_customerOutpostArn,
    cacheNode_endpoint,
    cacheNode_parameterGroupStatus,
    cacheNode_sourceCacheNodeId,

    -- ** CacheNodeTypeSpecificParameter
    cacheNodeTypeSpecificParameter_allowedValues,
    cacheNodeTypeSpecificParameter_cacheNodeTypeSpecificValues,
    cacheNodeTypeSpecificParameter_changeType,
    cacheNodeTypeSpecificParameter_dataType,
    cacheNodeTypeSpecificParameter_description,
    cacheNodeTypeSpecificParameter_isModifiable,
    cacheNodeTypeSpecificParameter_minimumEngineVersion,
    cacheNodeTypeSpecificParameter_parameterName,
    cacheNodeTypeSpecificParameter_source,

    -- ** CacheNodeTypeSpecificValue
    cacheNodeTypeSpecificValue_cacheNodeType,
    cacheNodeTypeSpecificValue_value,

    -- ** CacheNodeUpdateStatus
    cacheNodeUpdateStatus_cacheNodeId,
    cacheNodeUpdateStatus_nodeDeletionDate,
    cacheNodeUpdateStatus_nodeUpdateEndDate,
    cacheNodeUpdateStatus_nodeUpdateInitiatedBy,
    cacheNodeUpdateStatus_nodeUpdateInitiatedDate,
    cacheNodeUpdateStatus_nodeUpdateStartDate,
    cacheNodeUpdateStatus_nodeUpdateStatus,
    cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate,

    -- ** CacheParameterGroup
    cacheParameterGroup_arn,
    cacheParameterGroup_cacheParameterGroupFamily,
    cacheParameterGroup_cacheParameterGroupName,
    cacheParameterGroup_description,
    cacheParameterGroup_isGlobal,

    -- ** CacheParameterGroupNameMessage
    cacheParameterGroupNameMessage_cacheParameterGroupName,

    -- ** CacheParameterGroupStatus
    cacheParameterGroupStatus_cacheNodeIdsToReboot,
    cacheParameterGroupStatus_cacheParameterGroupName,
    cacheParameterGroupStatus_parameterApplyStatus,

    -- ** CacheSecurityGroup
    cacheSecurityGroup_arn,
    cacheSecurityGroup_cacheSecurityGroupName,
    cacheSecurityGroup_description,
    cacheSecurityGroup_eC2SecurityGroups,
    cacheSecurityGroup_ownerId,

    -- ** CacheSecurityGroupMembership
    cacheSecurityGroupMembership_cacheSecurityGroupName,
    cacheSecurityGroupMembership_status,

    -- ** CacheSubnetGroup
    cacheSubnetGroup_arn,
    cacheSubnetGroup_cacheSubnetGroupDescription,
    cacheSubnetGroup_cacheSubnetGroupName,
    cacheSubnetGroup_subnets,
    cacheSubnetGroup_supportedNetworkTypes,
    cacheSubnetGroup_vpcId,

    -- ** CloudWatchLogsDestinationDetails
    cloudWatchLogsDestinationDetails_logGroup,

    -- ** ConfigureShard
    configureShard_preferredAvailabilityZones,
    configureShard_preferredOutpostArns,
    configureShard_nodeGroupId,
    configureShard_newReplicaCount,

    -- ** CustomerNodeEndpoint
    customerNodeEndpoint_address,
    customerNodeEndpoint_port,

    -- ** DestinationDetails
    destinationDetails_cloudWatchLogsDetails,
    destinationDetails_kinesisFirehoseDetails,

    -- ** EC2SecurityGroup
    eC2SecurityGroup_eC2SecurityGroupName,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_status,

    -- ** Endpoint
    endpoint_address,
    endpoint_port,

    -- ** EngineDefaults
    engineDefaults_cacheNodeTypeSpecificParameters,
    engineDefaults_cacheParameterGroupFamily,
    engineDefaults_marker,
    engineDefaults_parameters,

    -- ** Event
    event_date,
    event_message,
    event_sourceIdentifier,
    event_sourceType,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** GlobalNodeGroup
    globalNodeGroup_globalNodeGroupId,
    globalNodeGroup_slots,

    -- ** GlobalReplicationGroup
    globalReplicationGroup_arn,
    globalReplicationGroup_atRestEncryptionEnabled,
    globalReplicationGroup_authTokenEnabled,
    globalReplicationGroup_cacheNodeType,
    globalReplicationGroup_clusterEnabled,
    globalReplicationGroup_engine,
    globalReplicationGroup_engineVersion,
    globalReplicationGroup_globalNodeGroups,
    globalReplicationGroup_globalReplicationGroupDescription,
    globalReplicationGroup_globalReplicationGroupId,
    globalReplicationGroup_members,
    globalReplicationGroup_status,
    globalReplicationGroup_transitEncryptionEnabled,

    -- ** GlobalReplicationGroupInfo
    globalReplicationGroupInfo_globalReplicationGroupId,
    globalReplicationGroupInfo_globalReplicationGroupMemberRole,

    -- ** GlobalReplicationGroupMember
    globalReplicationGroupMember_automaticFailover,
    globalReplicationGroupMember_replicationGroupId,
    globalReplicationGroupMember_replicationGroupRegion,
    globalReplicationGroupMember_role,
    globalReplicationGroupMember_status,

    -- ** KinesisFirehoseDestinationDetails
    kinesisFirehoseDestinationDetails_deliveryStream,

    -- ** LogDeliveryConfiguration
    logDeliveryConfiguration_destinationDetails,
    logDeliveryConfiguration_destinationType,
    logDeliveryConfiguration_logFormat,
    logDeliveryConfiguration_logType,
    logDeliveryConfiguration_message,
    logDeliveryConfiguration_status,

    -- ** LogDeliveryConfigurationRequest
    logDeliveryConfigurationRequest_destinationDetails,
    logDeliveryConfigurationRequest_destinationType,
    logDeliveryConfigurationRequest_enabled,
    logDeliveryConfigurationRequest_logFormat,
    logDeliveryConfigurationRequest_logType,

    -- ** NodeGroup
    nodeGroup_nodeGroupId,
    nodeGroup_nodeGroupMembers,
    nodeGroup_primaryEndpoint,
    nodeGroup_readerEndpoint,
    nodeGroup_slots,
    nodeGroup_status,

    -- ** NodeGroupConfiguration
    nodeGroupConfiguration_nodeGroupId,
    nodeGroupConfiguration_primaryAvailabilityZone,
    nodeGroupConfiguration_primaryOutpostArn,
    nodeGroupConfiguration_replicaAvailabilityZones,
    nodeGroupConfiguration_replicaCount,
    nodeGroupConfiguration_replicaOutpostArns,
    nodeGroupConfiguration_slots,

    -- ** NodeGroupMember
    nodeGroupMember_cacheClusterId,
    nodeGroupMember_cacheNodeId,
    nodeGroupMember_currentRole,
    nodeGroupMember_preferredAvailabilityZone,
    nodeGroupMember_preferredOutpostArn,
    nodeGroupMember_readEndpoint,

    -- ** NodeGroupMemberUpdateStatus
    nodeGroupMemberUpdateStatus_cacheClusterId,
    nodeGroupMemberUpdateStatus_cacheNodeId,
    nodeGroupMemberUpdateStatus_nodeDeletionDate,
    nodeGroupMemberUpdateStatus_nodeUpdateEndDate,
    nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy,
    nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate,
    nodeGroupMemberUpdateStatus_nodeUpdateStartDate,
    nodeGroupMemberUpdateStatus_nodeUpdateStatus,
    nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate,

    -- ** NodeGroupUpdateStatus
    nodeGroupUpdateStatus_nodeGroupId,
    nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus,

    -- ** NodeSnapshot
    nodeSnapshot_cacheClusterId,
    nodeSnapshot_cacheNodeCreateTime,
    nodeSnapshot_cacheNodeId,
    nodeSnapshot_cacheSize,
    nodeSnapshot_nodeGroupConfiguration,
    nodeSnapshot_nodeGroupId,
    nodeSnapshot_snapshotCreateTime,

    -- ** NotificationConfiguration
    notificationConfiguration_topicArn,
    notificationConfiguration_topicStatus,

    -- ** Parameter
    parameter_allowedValues,
    parameter_changeType,
    parameter_dataType,
    parameter_description,
    parameter_isModifiable,
    parameter_minimumEngineVersion,
    parameter_parameterName,
    parameter_parameterValue,
    parameter_source,

    -- ** ParameterNameValue
    parameterNameValue_parameterName,
    parameterNameValue_parameterValue,

    -- ** PendingLogDeliveryConfiguration
    pendingLogDeliveryConfiguration_destinationDetails,
    pendingLogDeliveryConfiguration_destinationType,
    pendingLogDeliveryConfiguration_logFormat,
    pendingLogDeliveryConfiguration_logType,

    -- ** PendingModifiedValues
    pendingModifiedValues_authTokenStatus,
    pendingModifiedValues_cacheNodeIdsToRemove,
    pendingModifiedValues_cacheNodeType,
    pendingModifiedValues_engineVersion,
    pendingModifiedValues_logDeliveryConfigurations,
    pendingModifiedValues_numCacheNodes,

    -- ** ProcessedUpdateAction
    processedUpdateAction_cacheClusterId,
    processedUpdateAction_replicationGroupId,
    processedUpdateAction_serviceUpdateName,
    processedUpdateAction_updateActionStatus,

    -- ** RecurringCharge
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- ** RegionalConfiguration
    regionalConfiguration_replicationGroupId,
    regionalConfiguration_replicationGroupRegion,
    regionalConfiguration_reshardingConfiguration,

    -- ** ReplicationGroup
    replicationGroup_arn,
    replicationGroup_atRestEncryptionEnabled,
    replicationGroup_authTokenEnabled,
    replicationGroup_authTokenLastModifiedDate,
    replicationGroup_autoMinorVersionUpgrade,
    replicationGroup_automaticFailover,
    replicationGroup_cacheNodeType,
    replicationGroup_clusterEnabled,
    replicationGroup_configurationEndpoint,
    replicationGroup_dataTiering,
    replicationGroup_description,
    replicationGroup_globalReplicationGroupInfo,
    replicationGroup_ipDiscovery,
    replicationGroup_kmsKeyId,
    replicationGroup_logDeliveryConfigurations,
    replicationGroup_memberClusters,
    replicationGroup_memberClustersOutpostArns,
    replicationGroup_multiAZ,
    replicationGroup_networkType,
    replicationGroup_nodeGroups,
    replicationGroup_pendingModifiedValues,
    replicationGroup_replicationGroupCreateTime,
    replicationGroup_replicationGroupId,
    replicationGroup_snapshotRetentionLimit,
    replicationGroup_snapshotWindow,
    replicationGroup_snapshottingClusterId,
    replicationGroup_status,
    replicationGroup_transitEncryptionEnabled,
    replicationGroup_userGroupIds,

    -- ** ReplicationGroupPendingModifiedValues
    replicationGroupPendingModifiedValues_authTokenStatus,
    replicationGroupPendingModifiedValues_automaticFailoverStatus,
    replicationGroupPendingModifiedValues_logDeliveryConfigurations,
    replicationGroupPendingModifiedValues_primaryClusterId,
    replicationGroupPendingModifiedValues_resharding,
    replicationGroupPendingModifiedValues_userGroups,

    -- ** ReservedCacheNode
    reservedCacheNode_cacheNodeCount,
    reservedCacheNode_cacheNodeType,
    reservedCacheNode_duration,
    reservedCacheNode_fixedPrice,
    reservedCacheNode_offeringType,
    reservedCacheNode_productDescription,
    reservedCacheNode_recurringCharges,
    reservedCacheNode_reservationARN,
    reservedCacheNode_reservedCacheNodeId,
    reservedCacheNode_reservedCacheNodesOfferingId,
    reservedCacheNode_startTime,
    reservedCacheNode_state,
    reservedCacheNode_usagePrice,

    -- ** ReservedCacheNodesOffering
    reservedCacheNodesOffering_cacheNodeType,
    reservedCacheNodesOffering_duration,
    reservedCacheNodesOffering_fixedPrice,
    reservedCacheNodesOffering_offeringType,
    reservedCacheNodesOffering_productDescription,
    reservedCacheNodesOffering_recurringCharges,
    reservedCacheNodesOffering_reservedCacheNodesOfferingId,
    reservedCacheNodesOffering_usagePrice,

    -- ** ReshardingConfiguration
    reshardingConfiguration_nodeGroupId,
    reshardingConfiguration_preferredAvailabilityZones,

    -- ** ReshardingStatus
    reshardingStatus_slotMigration,

    -- ** SecurityGroupMembership
    securityGroupMembership_securityGroupId,
    securityGroupMembership_status,

    -- ** ServiceUpdate
    serviceUpdate_autoUpdateAfterRecommendedApplyByDate,
    serviceUpdate_engine,
    serviceUpdate_engineVersion,
    serviceUpdate_estimatedUpdateTime,
    serviceUpdate_serviceUpdateDescription,
    serviceUpdate_serviceUpdateEndDate,
    serviceUpdate_serviceUpdateName,
    serviceUpdate_serviceUpdateRecommendedApplyByDate,
    serviceUpdate_serviceUpdateReleaseDate,
    serviceUpdate_serviceUpdateSeverity,
    serviceUpdate_serviceUpdateStatus,
    serviceUpdate_serviceUpdateType,

    -- ** SlotMigration
    slotMigration_progressPercentage,

    -- ** Snapshot
    snapshot_arn,
    snapshot_autoMinorVersionUpgrade,
    snapshot_automaticFailover,
    snapshot_cacheClusterCreateTime,
    snapshot_cacheClusterId,
    snapshot_cacheNodeType,
    snapshot_cacheParameterGroupName,
    snapshot_cacheSubnetGroupName,
    snapshot_dataTiering,
    snapshot_engine,
    snapshot_engineVersion,
    snapshot_kmsKeyId,
    snapshot_nodeSnapshots,
    snapshot_numCacheNodes,
    snapshot_numNodeGroups,
    snapshot_port,
    snapshot_preferredAvailabilityZone,
    snapshot_preferredMaintenanceWindow,
    snapshot_preferredOutpostArn,
    snapshot_replicationGroupDescription,
    snapshot_replicationGroupId,
    snapshot_snapshotName,
    snapshot_snapshotRetentionLimit,
    snapshot_snapshotSource,
    snapshot_snapshotStatus,
    snapshot_snapshotWindow,
    snapshot_topicArn,
    snapshot_vpcId,

    -- ** Subnet
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,
    subnet_subnetOutpost,
    subnet_supportedNetworkTypes,

    -- ** SubnetOutpost
    subnetOutpost_subnetOutpostArn,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagListMessage
    tagListMessage_tagList,

    -- ** TimeRangeFilter
    timeRangeFilter_endTime,
    timeRangeFilter_startTime,

    -- ** UnprocessedUpdateAction
    unprocessedUpdateAction_cacheClusterId,
    unprocessedUpdateAction_errorMessage,
    unprocessedUpdateAction_errorType,
    unprocessedUpdateAction_replicationGroupId,
    unprocessedUpdateAction_serviceUpdateName,

    -- ** UpdateAction
    updateAction_cacheClusterId,
    updateAction_cacheNodeUpdateStatus,
    updateAction_engine,
    updateAction_estimatedUpdateTime,
    updateAction_nodeGroupUpdateStatus,
    updateAction_nodesUpdated,
    updateAction_replicationGroupId,
    updateAction_serviceUpdateName,
    updateAction_serviceUpdateRecommendedApplyByDate,
    updateAction_serviceUpdateReleaseDate,
    updateAction_serviceUpdateSeverity,
    updateAction_serviceUpdateStatus,
    updateAction_serviceUpdateType,
    updateAction_slaMet,
    updateAction_updateActionAvailableDate,
    updateAction_updateActionStatus,
    updateAction_updateActionStatusModifiedDate,

    -- ** UpdateActionResultsMessage
    updateActionResultsMessage_processedUpdateActions,
    updateActionResultsMessage_unprocessedUpdateActions,

    -- ** User
    user_arn,
    user_accessString,
    user_authentication,
    user_engine,
    user_minimumEngineVersion,
    user_status,
    user_userGroupIds,
    user_userId,
    user_userName,

    -- ** UserGroup
    userGroup_arn,
    userGroup_engine,
    userGroup_minimumEngineVersion,
    userGroup_pendingChanges,
    userGroup_replicationGroups,
    userGroup_status,
    userGroup_userGroupId,
    userGroup_userIds,

    -- ** UserGroupPendingChanges
    userGroupPendingChanges_userIdsToAdd,
    userGroupPendingChanges_userIdsToRemove,

    -- ** UserGroupsUpdateStatus
    userGroupsUpdateStatus_userGroupIdsToAdd,
    userGroupsUpdateStatus_userGroupIdsToRemove,
  )
where

import Amazonka.ElastiCache.AddTagsToResource
import Amazonka.ElastiCache.AuthorizeCacheSecurityGroupIngress
import Amazonka.ElastiCache.BatchApplyUpdateAction
import Amazonka.ElastiCache.BatchStopUpdateAction
import Amazonka.ElastiCache.CompleteMigration
import Amazonka.ElastiCache.CopySnapshot
import Amazonka.ElastiCache.CreateCacheCluster
import Amazonka.ElastiCache.CreateCacheParameterGroup
import Amazonka.ElastiCache.CreateCacheSecurityGroup
import Amazonka.ElastiCache.CreateCacheSubnetGroup
import Amazonka.ElastiCache.CreateGlobalReplicationGroup
import Amazonka.ElastiCache.CreateReplicationGroup
import Amazonka.ElastiCache.CreateSnapshot
import Amazonka.ElastiCache.CreateUser
import Amazonka.ElastiCache.CreateUserGroup
import Amazonka.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
import Amazonka.ElastiCache.DecreaseReplicaCount
import Amazonka.ElastiCache.DeleteCacheCluster
import Amazonka.ElastiCache.DeleteCacheParameterGroup
import Amazonka.ElastiCache.DeleteCacheSecurityGroup
import Amazonka.ElastiCache.DeleteCacheSubnetGroup
import Amazonka.ElastiCache.DeleteGlobalReplicationGroup
import Amazonka.ElastiCache.DeleteReplicationGroup
import Amazonka.ElastiCache.DeleteSnapshot
import Amazonka.ElastiCache.DeleteUser
import Amazonka.ElastiCache.DeleteUserGroup
import Amazonka.ElastiCache.DescribeCacheClusters
import Amazonka.ElastiCache.DescribeCacheEngineVersions
import Amazonka.ElastiCache.DescribeCacheParameterGroups
import Amazonka.ElastiCache.DescribeCacheParameters
import Amazonka.ElastiCache.DescribeCacheSecurityGroups
import Amazonka.ElastiCache.DescribeCacheSubnetGroups
import Amazonka.ElastiCache.DescribeEngineDefaultParameters
import Amazonka.ElastiCache.DescribeEvents
import Amazonka.ElastiCache.DescribeGlobalReplicationGroups
import Amazonka.ElastiCache.DescribeReplicationGroups
import Amazonka.ElastiCache.DescribeReservedCacheNodes
import Amazonka.ElastiCache.DescribeReservedCacheNodesOfferings
import Amazonka.ElastiCache.DescribeServiceUpdates
import Amazonka.ElastiCache.DescribeSnapshots
import Amazonka.ElastiCache.DescribeUpdateActions
import Amazonka.ElastiCache.DescribeUserGroups
import Amazonka.ElastiCache.DescribeUsers
import Amazonka.ElastiCache.DisassociateGlobalReplicationGroup
import Amazonka.ElastiCache.FailoverGlobalReplicationGroup
import Amazonka.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
import Amazonka.ElastiCache.IncreaseReplicaCount
import Amazonka.ElastiCache.ListAllowedNodeTypeModifications
import Amazonka.ElastiCache.ListTagsForResource
import Amazonka.ElastiCache.ModifyCacheCluster
import Amazonka.ElastiCache.ModifyCacheParameterGroup
import Amazonka.ElastiCache.ModifyCacheSubnetGroup
import Amazonka.ElastiCache.ModifyGlobalReplicationGroup
import Amazonka.ElastiCache.ModifyReplicationGroup
import Amazonka.ElastiCache.ModifyReplicationGroupShardConfiguration
import Amazonka.ElastiCache.ModifyUser
import Amazonka.ElastiCache.ModifyUserGroup
import Amazonka.ElastiCache.PurchaseReservedCacheNodesOffering
import Amazonka.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
import Amazonka.ElastiCache.RebootCacheCluster
import Amazonka.ElastiCache.RemoveTagsFromResource
import Amazonka.ElastiCache.ResetCacheParameterGroup
import Amazonka.ElastiCache.RevokeCacheSecurityGroupIngress
import Amazonka.ElastiCache.StartMigration
import Amazonka.ElastiCache.TestFailover
import Amazonka.ElastiCache.Types.Authentication
import Amazonka.ElastiCache.Types.AuthenticationMode
import Amazonka.ElastiCache.Types.AvailabilityZone
import Amazonka.ElastiCache.Types.CacheCluster
import Amazonka.ElastiCache.Types.CacheEngineVersion
import Amazonka.ElastiCache.Types.CacheNode
import Amazonka.ElastiCache.Types.CacheNodeTypeSpecificParameter
import Amazonka.ElastiCache.Types.CacheNodeTypeSpecificValue
import Amazonka.ElastiCache.Types.CacheNodeUpdateStatus
import Amazonka.ElastiCache.Types.CacheParameterGroup
import Amazonka.ElastiCache.Types.CacheParameterGroupNameMessage
import Amazonka.ElastiCache.Types.CacheParameterGroupStatus
import Amazonka.ElastiCache.Types.CacheSecurityGroup
import Amazonka.ElastiCache.Types.CacheSecurityGroupMembership
import Amazonka.ElastiCache.Types.CacheSubnetGroup
import Amazonka.ElastiCache.Types.CloudWatchLogsDestinationDetails
import Amazonka.ElastiCache.Types.ConfigureShard
import Amazonka.ElastiCache.Types.CustomerNodeEndpoint
import Amazonka.ElastiCache.Types.DestinationDetails
import Amazonka.ElastiCache.Types.EC2SecurityGroup
import Amazonka.ElastiCache.Types.Endpoint
import Amazonka.ElastiCache.Types.EngineDefaults
import Amazonka.ElastiCache.Types.Event
import Amazonka.ElastiCache.Types.Filter
import Amazonka.ElastiCache.Types.GlobalNodeGroup
import Amazonka.ElastiCache.Types.GlobalReplicationGroup
import Amazonka.ElastiCache.Types.GlobalReplicationGroupInfo
import Amazonka.ElastiCache.Types.GlobalReplicationGroupMember
import Amazonka.ElastiCache.Types.KinesisFirehoseDestinationDetails
import Amazonka.ElastiCache.Types.LogDeliveryConfiguration
import Amazonka.ElastiCache.Types.LogDeliveryConfigurationRequest
import Amazonka.ElastiCache.Types.NodeGroup
import Amazonka.ElastiCache.Types.NodeGroupConfiguration
import Amazonka.ElastiCache.Types.NodeGroupMember
import Amazonka.ElastiCache.Types.NodeGroupMemberUpdateStatus
import Amazonka.ElastiCache.Types.NodeGroupUpdateStatus
import Amazonka.ElastiCache.Types.NodeSnapshot
import Amazonka.ElastiCache.Types.NotificationConfiguration
import Amazonka.ElastiCache.Types.Parameter
import Amazonka.ElastiCache.Types.ParameterNameValue
import Amazonka.ElastiCache.Types.PendingLogDeliveryConfiguration
import Amazonka.ElastiCache.Types.PendingModifiedValues
import Amazonka.ElastiCache.Types.ProcessedUpdateAction
import Amazonka.ElastiCache.Types.RecurringCharge
import Amazonka.ElastiCache.Types.RegionalConfiguration
import Amazonka.ElastiCache.Types.ReplicationGroup
import Amazonka.ElastiCache.Types.ReplicationGroupPendingModifiedValues
import Amazonka.ElastiCache.Types.ReservedCacheNode
import Amazonka.ElastiCache.Types.ReservedCacheNodesOffering
import Amazonka.ElastiCache.Types.ReshardingConfiguration
import Amazonka.ElastiCache.Types.ReshardingStatus
import Amazonka.ElastiCache.Types.SecurityGroupMembership
import Amazonka.ElastiCache.Types.ServiceUpdate
import Amazonka.ElastiCache.Types.SlotMigration
import Amazonka.ElastiCache.Types.Snapshot
import Amazonka.ElastiCache.Types.Subnet
import Amazonka.ElastiCache.Types.SubnetOutpost
import Amazonka.ElastiCache.Types.Tag
import Amazonka.ElastiCache.Types.TagListMessage
import Amazonka.ElastiCache.Types.TimeRangeFilter
import Amazonka.ElastiCache.Types.UnprocessedUpdateAction
import Amazonka.ElastiCache.Types.UpdateAction
import Amazonka.ElastiCache.Types.UpdateActionResultsMessage
import Amazonka.ElastiCache.Types.User
import Amazonka.ElastiCache.Types.UserGroup
import Amazonka.ElastiCache.Types.UserGroupPendingChanges
import Amazonka.ElastiCache.Types.UserGroupsUpdateStatus
