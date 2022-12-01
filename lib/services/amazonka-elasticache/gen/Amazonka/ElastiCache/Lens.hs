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
    updateActionResultsMessage_unprocessedUpdateActions,
    updateActionResultsMessage_processedUpdateActions,

    -- ** BatchStopUpdateAction
    batchStopUpdateAction_cacheClusterIds,
    batchStopUpdateAction_replicationGroupIds,
    batchStopUpdateAction_serviceUpdateName,
    updateActionResultsMessage_unprocessedUpdateActions,
    updateActionResultsMessage_processedUpdateActions,

    -- ** CompleteMigration
    completeMigration_force,
    completeMigration_replicationGroupId,
    completeMigrationResponse_replicationGroup,
    completeMigrationResponse_httpStatus,

    -- ** CopySnapshot
    copySnapshot_tags,
    copySnapshot_targetBucket,
    copySnapshot_kmsKeyId,
    copySnapshot_sourceSnapshotName,
    copySnapshot_targetSnapshotName,
    copySnapshotResponse_snapshot,
    copySnapshotResponse_httpStatus,

    -- ** CreateCacheCluster
    createCacheCluster_transitEncryptionEnabled,
    createCacheCluster_tags,
    createCacheCluster_port,
    createCacheCluster_preferredAvailabilityZones,
    createCacheCluster_cacheSubnetGroupName,
    createCacheCluster_outpostMode,
    createCacheCluster_preferredOutpostArns,
    createCacheCluster_snapshotName,
    createCacheCluster_securityGroupIds,
    createCacheCluster_autoMinorVersionUpgrade,
    createCacheCluster_authToken,
    createCacheCluster_logDeliveryConfigurations,
    createCacheCluster_ipDiscovery,
    createCacheCluster_numCacheNodes,
    createCacheCluster_cacheNodeType,
    createCacheCluster_cacheParameterGroupName,
    createCacheCluster_preferredAvailabilityZone,
    createCacheCluster_notificationTopicArn,
    createCacheCluster_snapshotArns,
    createCacheCluster_snapshotWindow,
    createCacheCluster_aZMode,
    createCacheCluster_snapshotRetentionLimit,
    createCacheCluster_cacheSecurityGroupNames,
    createCacheCluster_preferredOutpostArn,
    createCacheCluster_engine,
    createCacheCluster_preferredMaintenanceWindow,
    createCacheCluster_replicationGroupId,
    createCacheCluster_engineVersion,
    createCacheCluster_networkType,
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
    createReplicationGroup_transitEncryptionEnabled,
    createReplicationGroup_tags,
    createReplicationGroup_dataTieringEnabled,
    createReplicationGroup_port,
    createReplicationGroup_cacheSubnetGroupName,
    createReplicationGroup_snapshotName,
    createReplicationGroup_securityGroupIds,
    createReplicationGroup_autoMinorVersionUpgrade,
    createReplicationGroup_globalReplicationGroupId,
    createReplicationGroup_preferredCacheClusterAZs,
    createReplicationGroup_authToken,
    createReplicationGroup_logDeliveryConfigurations,
    createReplicationGroup_atRestEncryptionEnabled,
    createReplicationGroup_ipDiscovery,
    createReplicationGroup_replicasPerNodeGroup,
    createReplicationGroup_cacheNodeType,
    createReplicationGroup_cacheParameterGroupName,
    createReplicationGroup_notificationTopicArn,
    createReplicationGroup_snapshotArns,
    createReplicationGroup_snapshotWindow,
    createReplicationGroup_snapshotRetentionLimit,
    createReplicationGroup_numCacheClusters,
    createReplicationGroup_cacheSecurityGroupNames,
    createReplicationGroup_userGroupIds,
    createReplicationGroup_automaticFailoverEnabled,
    createReplicationGroup_kmsKeyId,
    createReplicationGroup_engine,
    createReplicationGroup_preferredMaintenanceWindow,
    createReplicationGroup_primaryClusterId,
    createReplicationGroup_nodeGroupConfiguration,
    createReplicationGroup_numNodeGroups,
    createReplicationGroup_multiAZEnabled,
    createReplicationGroup_engineVersion,
    createReplicationGroup_networkType,
    createReplicationGroup_replicationGroupId,
    createReplicationGroup_replicationGroupDescription,
    createReplicationGroupResponse_replicationGroup,
    createReplicationGroupResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_tags,
    createSnapshot_cacheClusterId,
    createSnapshot_kmsKeyId,
    createSnapshot_replicationGroupId,
    createSnapshot_snapshotName,
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,

    -- ** CreateUser
    createUser_tags,
    createUser_passwords,
    createUser_authenticationMode,
    createUser_noPasswordRequired,
    createUser_userId,
    createUser_userName,
    createUser_engine,
    createUser_accessString,
    user_accessString,
    user_authentication,
    user_userName,
    user_arn,
    user_status,
    user_minimumEngineVersion,
    user_userGroupIds,
    user_userId,
    user_engine,

    -- ** CreateUserGroup
    createUserGroup_tags,
    createUserGroup_userIds,
    createUserGroup_userGroupId,
    createUserGroup_engine,
    userGroup_userGroupId,
    userGroup_replicationGroups,
    userGroup_arn,
    userGroup_pendingChanges,
    userGroup_status,
    userGroup_minimumEngineVersion,
    userGroup_engine,
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
    decreaseReplicaCount_replicasToRemove,
    decreaseReplicaCount_newReplicaCount,
    decreaseReplicaCount_replicaConfiguration,
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
    deleteReplicationGroup_retainPrimaryCluster,
    deleteReplicationGroup_finalSnapshotIdentifier,
    deleteReplicationGroup_replicationGroupId,
    deleteReplicationGroupResponse_replicationGroup,
    deleteReplicationGroupResponse_httpStatus,

    -- ** DeleteSnapshot
    deleteSnapshot_snapshotName,
    deleteSnapshotResponse_snapshot,
    deleteSnapshotResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_userId,
    user_accessString,
    user_authentication,
    user_userName,
    user_arn,
    user_status,
    user_minimumEngineVersion,
    user_userGroupIds,
    user_userId,
    user_engine,

    -- ** DeleteUserGroup
    deleteUserGroup_userGroupId,
    userGroup_userGroupId,
    userGroup_replicationGroups,
    userGroup_arn,
    userGroup_pendingChanges,
    userGroup_status,
    userGroup_minimumEngineVersion,
    userGroup_engine,
    userGroup_userIds,

    -- ** DescribeCacheClusters
    describeCacheClusters_marker,
    describeCacheClusters_showCacheClustersNotInReplicationGroups,
    describeCacheClusters_maxRecords,
    describeCacheClusters_cacheClusterId,
    describeCacheClusters_showCacheNodeInfo,
    describeCacheClustersResponse_cacheClusters,
    describeCacheClustersResponse_marker,
    describeCacheClustersResponse_httpStatus,

    -- ** DescribeCacheEngineVersions
    describeCacheEngineVersions_marker,
    describeCacheEngineVersions_maxRecords,
    describeCacheEngineVersions_defaultOnly,
    describeCacheEngineVersions_engine,
    describeCacheEngineVersions_cacheParameterGroupFamily,
    describeCacheEngineVersions_engineVersion,
    describeCacheEngineVersionsResponse_marker,
    describeCacheEngineVersionsResponse_cacheEngineVersions,
    describeCacheEngineVersionsResponse_httpStatus,

    -- ** DescribeCacheParameterGroups
    describeCacheParameterGroups_marker,
    describeCacheParameterGroups_maxRecords,
    describeCacheParameterGroups_cacheParameterGroupName,
    describeCacheParameterGroupsResponse_marker,
    describeCacheParameterGroupsResponse_cacheParameterGroups,
    describeCacheParameterGroupsResponse_httpStatus,

    -- ** DescribeCacheParameters
    describeCacheParameters_marker,
    describeCacheParameters_maxRecords,
    describeCacheParameters_source,
    describeCacheParameters_cacheParameterGroupName,
    describeCacheParametersResponse_marker,
    describeCacheParametersResponse_cacheNodeTypeSpecificParameters,
    describeCacheParametersResponse_parameters,
    describeCacheParametersResponse_httpStatus,

    -- ** DescribeCacheSecurityGroups
    describeCacheSecurityGroups_marker,
    describeCacheSecurityGroups_maxRecords,
    describeCacheSecurityGroups_cacheSecurityGroupName,
    describeCacheSecurityGroupsResponse_marker,
    describeCacheSecurityGroupsResponse_cacheSecurityGroups,
    describeCacheSecurityGroupsResponse_httpStatus,

    -- ** DescribeCacheSubnetGroups
    describeCacheSubnetGroups_cacheSubnetGroupName,
    describeCacheSubnetGroups_marker,
    describeCacheSubnetGroups_maxRecords,
    describeCacheSubnetGroupsResponse_marker,
    describeCacheSubnetGroupsResponse_cacheSubnetGroups,
    describeCacheSubnetGroupsResponse_httpStatus,

    -- ** DescribeEngineDefaultParameters
    describeEngineDefaultParameters_marker,
    describeEngineDefaultParameters_maxRecords,
    describeEngineDefaultParameters_cacheParameterGroupFamily,
    describeEngineDefaultParametersResponse_httpStatus,
    describeEngineDefaultParametersResponse_engineDefaults,

    -- ** DescribeEvents
    describeEvents_marker,
    describeEvents_sourceType,
    describeEvents_endTime,
    describeEvents_maxRecords,
    describeEvents_duration,
    describeEvents_sourceIdentifier,
    describeEvents_startTime,
    describeEventsResponse_marker,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** DescribeGlobalReplicationGroups
    describeGlobalReplicationGroups_marker,
    describeGlobalReplicationGroups_globalReplicationGroupId,
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
    describeReservedCacheNodes_marker,
    describeReservedCacheNodes_reservedCacheNodesOfferingId,
    describeReservedCacheNodes_offeringType,
    describeReservedCacheNodes_cacheNodeType,
    describeReservedCacheNodes_maxRecords,
    describeReservedCacheNodes_duration,
    describeReservedCacheNodes_reservedCacheNodeId,
    describeReservedCacheNodes_productDescription,
    describeReservedCacheNodesResponse_marker,
    describeReservedCacheNodesResponse_reservedCacheNodes,
    describeReservedCacheNodesResponse_httpStatus,

    -- ** DescribeReservedCacheNodesOfferings
    describeReservedCacheNodesOfferings_marker,
    describeReservedCacheNodesOfferings_reservedCacheNodesOfferingId,
    describeReservedCacheNodesOfferings_offeringType,
    describeReservedCacheNodesOfferings_cacheNodeType,
    describeReservedCacheNodesOfferings_maxRecords,
    describeReservedCacheNodesOfferings_duration,
    describeReservedCacheNodesOfferings_productDescription,
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
    describeSnapshots_marker,
    describeSnapshots_snapshotName,
    describeSnapshots_snapshotSource,
    describeSnapshots_showNodeGroupConfig,
    describeSnapshots_maxRecords,
    describeSnapshots_cacheClusterId,
    describeSnapshots_replicationGroupId,
    describeSnapshotsResponse_marker,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,

    -- ** DescribeUpdateActions
    describeUpdateActions_cacheClusterIds,
    describeUpdateActions_replicationGroupIds,
    describeUpdateActions_marker,
    describeUpdateActions_updateActionStatus,
    describeUpdateActions_showNodeLevelUpdateStatus,
    describeUpdateActions_maxRecords,
    describeUpdateActions_serviceUpdateName,
    describeUpdateActions_serviceUpdateStatus,
    describeUpdateActions_engine,
    describeUpdateActions_serviceUpdateTimeRange,
    describeUpdateActionsResponse_marker,
    describeUpdateActionsResponse_updateActions,
    describeUpdateActionsResponse_httpStatus,

    -- ** DescribeUserGroups
    describeUserGroups_marker,
    describeUserGroups_userGroupId,
    describeUserGroups_maxRecords,
    describeUserGroupsResponse_marker,
    describeUserGroupsResponse_userGroups,
    describeUserGroupsResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_marker,
    describeUsers_filters,
    describeUsers_maxRecords,
    describeUsers_userId,
    describeUsers_engine,
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
    listAllowedNodeTypeModificationsResponse_scaleUpModifications,
    listAllowedNodeTypeModificationsResponse_scaleDownModifications,
    listAllowedNodeTypeModificationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceName,
    tagListMessage_tagList,

    -- ** ModifyCacheCluster
    modifyCacheCluster_notificationTopicStatus,
    modifyCacheCluster_cacheNodeIdsToRemove,
    modifyCacheCluster_securityGroupIds,
    modifyCacheCluster_autoMinorVersionUpgrade,
    modifyCacheCluster_applyImmediately,
    modifyCacheCluster_authToken,
    modifyCacheCluster_logDeliveryConfigurations,
    modifyCacheCluster_ipDiscovery,
    modifyCacheCluster_numCacheNodes,
    modifyCacheCluster_cacheNodeType,
    modifyCacheCluster_cacheParameterGroupName,
    modifyCacheCluster_notificationTopicArn,
    modifyCacheCluster_snapshotWindow,
    modifyCacheCluster_aZMode,
    modifyCacheCluster_snapshotRetentionLimit,
    modifyCacheCluster_authTokenUpdateStrategy,
    modifyCacheCluster_cacheSecurityGroupNames,
    modifyCacheCluster_preferredMaintenanceWindow,
    modifyCacheCluster_newAvailabilityZones,
    modifyCacheCluster_engineVersion,
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
    modifyGlobalReplicationGroup_globalReplicationGroupDescription,
    modifyGlobalReplicationGroup_cacheNodeType,
    modifyGlobalReplicationGroup_cacheParameterGroupName,
    modifyGlobalReplicationGroup_automaticFailoverEnabled,
    modifyGlobalReplicationGroup_engineVersion,
    modifyGlobalReplicationGroup_globalReplicationGroupId,
    modifyGlobalReplicationGroup_applyImmediately,
    modifyGlobalReplicationGroupResponse_globalReplicationGroup,
    modifyGlobalReplicationGroupResponse_httpStatus,

    -- ** ModifyReplicationGroup
    modifyReplicationGroup_notificationTopicStatus,
    modifyReplicationGroup_removeUserGroups,
    modifyReplicationGroup_securityGroupIds,
    modifyReplicationGroup_autoMinorVersionUpgrade,
    modifyReplicationGroup_applyImmediately,
    modifyReplicationGroup_userGroupIdsToAdd,
    modifyReplicationGroup_authToken,
    modifyReplicationGroup_snapshottingClusterId,
    modifyReplicationGroup_logDeliveryConfigurations,
    modifyReplicationGroup_ipDiscovery,
    modifyReplicationGroup_cacheNodeType,
    modifyReplicationGroup_cacheParameterGroupName,
    modifyReplicationGroup_notificationTopicArn,
    modifyReplicationGroup_snapshotWindow,
    modifyReplicationGroup_snapshotRetentionLimit,
    modifyReplicationGroup_replicationGroupDescription,
    modifyReplicationGroup_authTokenUpdateStrategy,
    modifyReplicationGroup_cacheSecurityGroupNames,
    modifyReplicationGroup_automaticFailoverEnabled,
    modifyReplicationGroup_userGroupIdsToRemove,
    modifyReplicationGroup_preferredMaintenanceWindow,
    modifyReplicationGroup_nodeGroupId,
    modifyReplicationGroup_primaryClusterId,
    modifyReplicationGroup_multiAZEnabled,
    modifyReplicationGroup_engineVersion,
    modifyReplicationGroup_replicationGroupId,
    modifyReplicationGroupResponse_replicationGroup,
    modifyReplicationGroupResponse_httpStatus,

    -- ** ModifyReplicationGroupShardConfiguration
    modifyReplicationGroupShardConfiguration_nodeGroupsToRemove,
    modifyReplicationGroupShardConfiguration_reshardingConfiguration,
    modifyReplicationGroupShardConfiguration_nodeGroupsToRetain,
    modifyReplicationGroupShardConfiguration_replicationGroupId,
    modifyReplicationGroupShardConfiguration_nodeGroupCount,
    modifyReplicationGroupShardConfiguration_applyImmediately,
    modifyReplicationGroupShardConfigurationResponse_replicationGroup,
    modifyReplicationGroupShardConfigurationResponse_httpStatus,

    -- ** ModifyUser
    modifyUser_accessString,
    modifyUser_passwords,
    modifyUser_appendAccessString,
    modifyUser_authenticationMode,
    modifyUser_noPasswordRequired,
    modifyUser_userId,
    user_accessString,
    user_authentication,
    user_userName,
    user_arn,
    user_status,
    user_minimumEngineVersion,
    user_userGroupIds,
    user_userId,
    user_engine,

    -- ** ModifyUserGroup
    modifyUserGroup_userIdsToRemove,
    modifyUserGroup_userIdsToAdd,
    modifyUserGroup_userGroupId,
    userGroup_userGroupId,
    userGroup_replicationGroups,
    userGroup_arn,
    userGroup_pendingChanges,
    userGroup_status,
    userGroup_minimumEngineVersion,
    userGroup_engine,
    userGroup_userIds,

    -- ** PurchaseReservedCacheNodesOffering
    purchaseReservedCacheNodesOffering_tags,
    purchaseReservedCacheNodesOffering_cacheNodeCount,
    purchaseReservedCacheNodesOffering_reservedCacheNodeId,
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
    resetCacheParameterGroup_resetAllParameters,
    resetCacheParameterGroup_parameterNameValues,
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
    authentication_type,
    authentication_passwordCount,

    -- ** AuthenticationMode
    authenticationMode_passwords,
    authenticationMode_type,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** CacheCluster
    cacheCluster_transitEncryptionEnabled,
    cacheCluster_cacheSubnetGroupName,
    cacheCluster_replicationGroupLogDeliveryEnabled,
    cacheCluster_autoMinorVersionUpgrade,
    cacheCluster_cacheClusterStatus,
    cacheCluster_clientDownloadLandingPage,
    cacheCluster_arn,
    cacheCluster_logDeliveryConfigurations,
    cacheCluster_cacheClusterCreateTime,
    cacheCluster_atRestEncryptionEnabled,
    cacheCluster_ipDiscovery,
    cacheCluster_numCacheNodes,
    cacheCluster_notificationConfiguration,
    cacheCluster_cacheNodeType,
    cacheCluster_preferredAvailabilityZone,
    cacheCluster_cacheNodes,
    cacheCluster_cacheClusterId,
    cacheCluster_authTokenEnabled,
    cacheCluster_snapshotWindow,
    cacheCluster_snapshotRetentionLimit,
    cacheCluster_cacheParameterGroup,
    cacheCluster_securityGroups,
    cacheCluster_preferredOutpostArn,
    cacheCluster_engine,
    cacheCluster_pendingModifiedValues,
    cacheCluster_preferredMaintenanceWindow,
    cacheCluster_authTokenLastModifiedDate,
    cacheCluster_replicationGroupId,
    cacheCluster_engineVersion,
    cacheCluster_networkType,
    cacheCluster_cacheSecurityGroups,
    cacheCluster_configurationEndpoint,

    -- ** CacheEngineVersion
    cacheEngineVersion_cacheEngineVersionDescription,
    cacheEngineVersion_engine,
    cacheEngineVersion_cacheParameterGroupFamily,
    cacheEngineVersion_cacheEngineDescription,
    cacheEngineVersion_engineVersion,

    -- ** CacheNode
    cacheNode_customerOutpostArn,
    cacheNode_customerAvailabilityZone,
    cacheNode_sourceCacheNodeId,
    cacheNode_cacheNodeId,
    cacheNode_cacheNodeStatus,
    cacheNode_parameterGroupStatus,
    cacheNode_endpoint,
    cacheNode_cacheNodeCreateTime,

    -- ** CacheNodeTypeSpecificParameter
    cacheNodeTypeSpecificParameter_changeType,
    cacheNodeTypeSpecificParameter_isModifiable,
    cacheNodeTypeSpecificParameter_description,
    cacheNodeTypeSpecificParameter_parameterName,
    cacheNodeTypeSpecificParameter_cacheNodeTypeSpecificValues,
    cacheNodeTypeSpecificParameter_minimumEngineVersion,
    cacheNodeTypeSpecificParameter_source,
    cacheNodeTypeSpecificParameter_allowedValues,
    cacheNodeTypeSpecificParameter_dataType,

    -- ** CacheNodeTypeSpecificValue
    cacheNodeTypeSpecificValue_cacheNodeType,
    cacheNodeTypeSpecificValue_value,

    -- ** CacheNodeUpdateStatus
    cacheNodeUpdateStatus_nodeUpdateStatus,
    cacheNodeUpdateStatus_nodeUpdateEndDate,
    cacheNodeUpdateStatus_nodeUpdateStartDate,
    cacheNodeUpdateStatus_nodeDeletionDate,
    cacheNodeUpdateStatus_nodeUpdateInitiatedBy,
    cacheNodeUpdateStatus_nodeUpdateInitiatedDate,
    cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate,
    cacheNodeUpdateStatus_cacheNodeId,

    -- ** CacheParameterGroup
    cacheParameterGroup_isGlobal,
    cacheParameterGroup_arn,
    cacheParameterGroup_description,
    cacheParameterGroup_cacheParameterGroupName,
    cacheParameterGroup_cacheParameterGroupFamily,

    -- ** CacheParameterGroupNameMessage
    cacheParameterGroupNameMessage_cacheParameterGroupName,

    -- ** CacheParameterGroupStatus
    cacheParameterGroupStatus_cacheParameterGroupName,
    cacheParameterGroupStatus_parameterApplyStatus,
    cacheParameterGroupStatus_cacheNodeIdsToReboot,

    -- ** CacheSecurityGroup
    cacheSecurityGroup_ownerId,
    cacheSecurityGroup_arn,
    cacheSecurityGroup_description,
    cacheSecurityGroup_eC2SecurityGroups,
    cacheSecurityGroup_cacheSecurityGroupName,

    -- ** CacheSecurityGroupMembership
    cacheSecurityGroupMembership_status,
    cacheSecurityGroupMembership_cacheSecurityGroupName,

    -- ** CacheSubnetGroup
    cacheSubnetGroup_cacheSubnetGroupName,
    cacheSubnetGroup_subnets,
    cacheSubnetGroup_arn,
    cacheSubnetGroup_cacheSubnetGroupDescription,
    cacheSubnetGroup_vpcId,
    cacheSubnetGroup_supportedNetworkTypes,

    -- ** CloudWatchLogsDestinationDetails
    cloudWatchLogsDestinationDetails_logGroup,

    -- ** ConfigureShard
    configureShard_preferredAvailabilityZones,
    configureShard_preferredOutpostArns,
    configureShard_nodeGroupId,
    configureShard_newReplicaCount,

    -- ** CustomerNodeEndpoint
    customerNodeEndpoint_port,
    customerNodeEndpoint_address,

    -- ** DestinationDetails
    destinationDetails_kinesisFirehoseDetails,
    destinationDetails_cloudWatchLogsDetails,

    -- ** EC2SecurityGroup
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_status,
    eC2SecurityGroup_eC2SecurityGroupName,

    -- ** Endpoint
    endpoint_port,
    endpoint_address,

    -- ** EngineDefaults
    engineDefaults_marker,
    engineDefaults_cacheNodeTypeSpecificParameters,
    engineDefaults_cacheParameterGroupFamily,
    engineDefaults_parameters,

    -- ** Event
    event_message,
    event_date,
    event_sourceType,
    event_sourceIdentifier,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** GlobalNodeGroup
    globalNodeGroup_globalNodeGroupId,
    globalNodeGroup_slots,

    -- ** GlobalReplicationGroup
    globalReplicationGroup_transitEncryptionEnabled,
    globalReplicationGroup_globalReplicationGroupDescription,
    globalReplicationGroup_clusterEnabled,
    globalReplicationGroup_globalReplicationGroupId,
    globalReplicationGroup_members,
    globalReplicationGroup_arn,
    globalReplicationGroup_atRestEncryptionEnabled,
    globalReplicationGroup_status,
    globalReplicationGroup_cacheNodeType,
    globalReplicationGroup_authTokenEnabled,
    globalReplicationGroup_engine,
    globalReplicationGroup_engineVersion,
    globalReplicationGroup_globalNodeGroups,

    -- ** GlobalReplicationGroupInfo
    globalReplicationGroupInfo_globalReplicationGroupId,
    globalReplicationGroupInfo_globalReplicationGroupMemberRole,

    -- ** GlobalReplicationGroupMember
    globalReplicationGroupMember_replicationGroupRegion,
    globalReplicationGroupMember_automaticFailover,
    globalReplicationGroupMember_status,
    globalReplicationGroupMember_role,
    globalReplicationGroupMember_replicationGroupId,

    -- ** KinesisFirehoseDestinationDetails
    kinesisFirehoseDestinationDetails_deliveryStream,

    -- ** LogDeliveryConfiguration
    logDeliveryConfiguration_message,
    logDeliveryConfiguration_logType,
    logDeliveryConfiguration_destinationType,
    logDeliveryConfiguration_status,
    logDeliveryConfiguration_logFormat,
    logDeliveryConfiguration_destinationDetails,

    -- ** LogDeliveryConfigurationRequest
    logDeliveryConfigurationRequest_logType,
    logDeliveryConfigurationRequest_destinationType,
    logDeliveryConfigurationRequest_logFormat,
    logDeliveryConfigurationRequest_enabled,
    logDeliveryConfigurationRequest_destinationDetails,

    -- ** NodeGroup
    nodeGroup_primaryEndpoint,
    nodeGroup_status,
    nodeGroup_nodeGroupMembers,
    nodeGroup_readerEndpoint,
    nodeGroup_nodeGroupId,
    nodeGroup_slots,

    -- ** NodeGroupConfiguration
    nodeGroupConfiguration_replicaAvailabilityZones,
    nodeGroupConfiguration_replicaOutpostArns,
    nodeGroupConfiguration_primaryOutpostArn,
    nodeGroupConfiguration_primaryAvailabilityZone,
    nodeGroupConfiguration_nodeGroupId,
    nodeGroupConfiguration_slots,
    nodeGroupConfiguration_replicaCount,

    -- ** NodeGroupMember
    nodeGroupMember_preferredAvailabilityZone,
    nodeGroupMember_cacheClusterId,
    nodeGroupMember_preferredOutpostArn,
    nodeGroupMember_cacheNodeId,
    nodeGroupMember_readEndpoint,
    nodeGroupMember_currentRole,

    -- ** NodeGroupMemberUpdateStatus
    nodeGroupMemberUpdateStatus_nodeUpdateStatus,
    nodeGroupMemberUpdateStatus_nodeUpdateEndDate,
    nodeGroupMemberUpdateStatus_nodeUpdateStartDate,
    nodeGroupMemberUpdateStatus_nodeDeletionDate,
    nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy,
    nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate,
    nodeGroupMemberUpdateStatus_cacheClusterId,
    nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate,
    nodeGroupMemberUpdateStatus_cacheNodeId,

    -- ** NodeGroupUpdateStatus
    nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus,
    nodeGroupUpdateStatus_nodeGroupId,

    -- ** NodeSnapshot
    nodeSnapshot_snapshotCreateTime,
    nodeSnapshot_cacheClusterId,
    nodeSnapshot_cacheSize,
    nodeSnapshot_cacheNodeId,
    nodeSnapshot_nodeGroupId,
    nodeSnapshot_cacheNodeCreateTime,
    nodeSnapshot_nodeGroupConfiguration,

    -- ** NotificationConfiguration
    notificationConfiguration_topicStatus,
    notificationConfiguration_topicArn,

    -- ** Parameter
    parameter_changeType,
    parameter_parameterValue,
    parameter_isModifiable,
    parameter_description,
    parameter_parameterName,
    parameter_minimumEngineVersion,
    parameter_source,
    parameter_allowedValues,
    parameter_dataType,

    -- ** ParameterNameValue
    parameterNameValue_parameterValue,
    parameterNameValue_parameterName,

    -- ** PendingLogDeliveryConfiguration
    pendingLogDeliveryConfiguration_logType,
    pendingLogDeliveryConfiguration_destinationType,
    pendingLogDeliveryConfiguration_logFormat,
    pendingLogDeliveryConfiguration_destinationDetails,

    -- ** PendingModifiedValues
    pendingModifiedValues_cacheNodeIdsToRemove,
    pendingModifiedValues_logDeliveryConfigurations,
    pendingModifiedValues_numCacheNodes,
    pendingModifiedValues_cacheNodeType,
    pendingModifiedValues_authTokenStatus,
    pendingModifiedValues_engineVersion,

    -- ** ProcessedUpdateAction
    processedUpdateAction_updateActionStatus,
    processedUpdateAction_cacheClusterId,
    processedUpdateAction_serviceUpdateName,
    processedUpdateAction_replicationGroupId,

    -- ** RecurringCharge
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- ** RegionalConfiguration
    regionalConfiguration_replicationGroupId,
    regionalConfiguration_replicationGroupRegion,
    regionalConfiguration_reshardingConfiguration,

    -- ** ReplicationGroup
    replicationGroup_transitEncryptionEnabled,
    replicationGroup_memberClustersOutpostArns,
    replicationGroup_globalReplicationGroupInfo,
    replicationGroup_clusterEnabled,
    replicationGroup_autoMinorVersionUpgrade,
    replicationGroup_replicationGroupCreateTime,
    replicationGroup_automaticFailover,
    replicationGroup_arn,
    replicationGroup_snapshottingClusterId,
    replicationGroup_logDeliveryConfigurations,
    replicationGroup_atRestEncryptionEnabled,
    replicationGroup_ipDiscovery,
    replicationGroup_status,
    replicationGroup_cacheNodeType,
    replicationGroup_description,
    replicationGroup_authTokenEnabled,
    replicationGroup_nodeGroups,
    replicationGroup_snapshotWindow,
    replicationGroup_snapshotRetentionLimit,
    replicationGroup_userGroupIds,
    replicationGroup_kmsKeyId,
    replicationGroup_pendingModifiedValues,
    replicationGroup_dataTiering,
    replicationGroup_authTokenLastModifiedDate,
    replicationGroup_replicationGroupId,
    replicationGroup_memberClusters,
    replicationGroup_networkType,
    replicationGroup_multiAZ,
    replicationGroup_configurationEndpoint,

    -- ** ReplicationGroupPendingModifiedValues
    replicationGroupPendingModifiedValues_resharding,
    replicationGroupPendingModifiedValues_automaticFailoverStatus,
    replicationGroupPendingModifiedValues_logDeliveryConfigurations,
    replicationGroupPendingModifiedValues_userGroups,
    replicationGroupPendingModifiedValues_authTokenStatus,
    replicationGroupPendingModifiedValues_primaryClusterId,

    -- ** ReservedCacheNode
    reservedCacheNode_reservedCacheNodesOfferingId,
    reservedCacheNode_recurringCharges,
    reservedCacheNode_state,
    reservedCacheNode_offeringType,
    reservedCacheNode_cacheNodeCount,
    reservedCacheNode_cacheNodeType,
    reservedCacheNode_duration,
    reservedCacheNode_reservationARN,
    reservedCacheNode_reservedCacheNodeId,
    reservedCacheNode_productDescription,
    reservedCacheNode_fixedPrice,
    reservedCacheNode_startTime,
    reservedCacheNode_usagePrice,

    -- ** ReservedCacheNodesOffering
    reservedCacheNodesOffering_reservedCacheNodesOfferingId,
    reservedCacheNodesOffering_recurringCharges,
    reservedCacheNodesOffering_offeringType,
    reservedCacheNodesOffering_cacheNodeType,
    reservedCacheNodesOffering_duration,
    reservedCacheNodesOffering_productDescription,
    reservedCacheNodesOffering_fixedPrice,
    reservedCacheNodesOffering_usagePrice,

    -- ** ReshardingConfiguration
    reshardingConfiguration_preferredAvailabilityZones,
    reshardingConfiguration_nodeGroupId,

    -- ** ReshardingStatus
    reshardingStatus_slotMigration,

    -- ** SecurityGroupMembership
    securityGroupMembership_securityGroupId,
    securityGroupMembership_status,

    -- ** ServiceUpdate
    serviceUpdate_serviceUpdateType,
    serviceUpdate_serviceUpdateEndDate,
    serviceUpdate_serviceUpdateReleaseDate,
    serviceUpdate_estimatedUpdateTime,
    serviceUpdate_autoUpdateAfterRecommendedApplyByDate,
    serviceUpdate_serviceUpdateRecommendedApplyByDate,
    serviceUpdate_serviceUpdateSeverity,
    serviceUpdate_serviceUpdateName,
    serviceUpdate_serviceUpdateStatus,
    serviceUpdate_engine,
    serviceUpdate_serviceUpdateDescription,
    serviceUpdate_engineVersion,

    -- ** SlotMigration
    slotMigration_progressPercentage,

    -- ** Snapshot
    snapshot_port,
    snapshot_snapshotStatus,
    snapshot_cacheSubnetGroupName,
    snapshot_snapshotName,
    snapshot_snapshotSource,
    snapshot_autoMinorVersionUpgrade,
    snapshot_automaticFailover,
    snapshot_arn,
    snapshot_topicArn,
    snapshot_cacheClusterCreateTime,
    snapshot_numCacheNodes,
    snapshot_cacheNodeType,
    snapshot_cacheParameterGroupName,
    snapshot_preferredAvailabilityZone,
    snapshot_cacheClusterId,
    snapshot_snapshotWindow,
    snapshot_snapshotRetentionLimit,
    snapshot_replicationGroupDescription,
    snapshot_preferredOutpostArn,
    snapshot_kmsKeyId,
    snapshot_engine,
    snapshot_preferredMaintenanceWindow,
    snapshot_vpcId,
    snapshot_dataTiering,
    snapshot_replicationGroupId,
    snapshot_nodeSnapshots,
    snapshot_numNodeGroups,
    snapshot_engineVersion,

    -- ** Subnet
    subnet_subnetOutpost,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,
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
    unprocessedUpdateAction_errorMessage,
    unprocessedUpdateAction_cacheClusterId,
    unprocessedUpdateAction_serviceUpdateName,
    unprocessedUpdateAction_replicationGroupId,
    unprocessedUpdateAction_errorType,

    -- ** UpdateAction
    updateAction_serviceUpdateType,
    updateAction_nodeGroupUpdateStatus,
    updateAction_serviceUpdateReleaseDate,
    updateAction_estimatedUpdateTime,
    updateAction_updateActionStatus,
    updateAction_updateActionAvailableDate,
    updateAction_cacheClusterId,
    updateAction_serviceUpdateRecommendedApplyByDate,
    updateAction_serviceUpdateSeverity,
    updateAction_serviceUpdateName,
    updateAction_serviceUpdateStatus,
    updateAction_engine,
    updateAction_slaMet,
    updateAction_replicationGroupId,
    updateAction_cacheNodeUpdateStatus,
    updateAction_nodesUpdated,
    updateAction_updateActionStatusModifiedDate,

    -- ** UpdateActionResultsMessage
    updateActionResultsMessage_unprocessedUpdateActions,
    updateActionResultsMessage_processedUpdateActions,

    -- ** User
    user_accessString,
    user_authentication,
    user_userName,
    user_arn,
    user_status,
    user_minimumEngineVersion,
    user_userGroupIds,
    user_userId,
    user_engine,

    -- ** UserGroup
    userGroup_userGroupId,
    userGroup_replicationGroups,
    userGroup_arn,
    userGroup_pendingChanges,
    userGroup_status,
    userGroup_minimumEngineVersion,
    userGroup_engine,
    userGroup_userIds,

    -- ** UserGroupPendingChanges
    userGroupPendingChanges_userIdsToRemove,
    userGroupPendingChanges_userIdsToAdd,

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
