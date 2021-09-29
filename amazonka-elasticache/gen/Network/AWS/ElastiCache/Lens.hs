{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Lens
  ( -- * Operations

    -- ** CreateReplicationGroup
    createReplicationGroup_nodeGroupConfiguration,
    createReplicationGroup_securityGroupIds,
    createReplicationGroup_automaticFailoverEnabled,
    createReplicationGroup_cacheSecurityGroupNames,
    createReplicationGroup_primaryClusterId,
    createReplicationGroup_userGroupIds,
    createReplicationGroup_snapshotWindow,
    createReplicationGroup_cacheParameterGroupName,
    createReplicationGroup_snapshotRetentionLimit,
    createReplicationGroup_globalReplicationGroupId,
    createReplicationGroup_numNodeGroups,
    createReplicationGroup_snapshotArns,
    createReplicationGroup_kmsKeyId,
    createReplicationGroup_atRestEncryptionEnabled,
    createReplicationGroup_cacheSubnetGroupName,
    createReplicationGroup_multiAZEnabled,
    createReplicationGroup_engineVersion,
    createReplicationGroup_cacheNodeType,
    createReplicationGroup_preferredMaintenanceWindow,
    createReplicationGroup_tags,
    createReplicationGroup_notificationTopicArn,
    createReplicationGroup_port,
    createReplicationGroup_engine,
    createReplicationGroup_snapshotName,
    createReplicationGroup_replicasPerNodeGroup,
    createReplicationGroup_numCacheClusters,
    createReplicationGroup_authToken,
    createReplicationGroup_logDeliveryConfigurations,
    createReplicationGroup_preferredCacheClusterAZs,
    createReplicationGroup_transitEncryptionEnabled,
    createReplicationGroup_autoMinorVersionUpgrade,
    createReplicationGroup_replicationGroupId,
    createReplicationGroup_replicationGroupDescription,
    createReplicationGroupResponse_replicationGroup,
    createReplicationGroupResponse_httpStatus,

    -- ** DeleteCacheCluster
    deleteCacheCluster_finalSnapshotIdentifier,
    deleteCacheCluster_cacheClusterId,
    deleteCacheClusterResponse_cacheCluster,
    deleteCacheClusterResponse_httpStatus,

    -- ** DeleteUserGroup
    deleteUserGroup_userGroupId,
    userGroup_status,
    userGroup_replicationGroups,
    userGroup_arn,
    userGroup_userIds,
    userGroup_engine,
    userGroup_userGroupId,
    userGroup_pendingChanges,

    -- ** RebootCacheCluster
    rebootCacheCluster_cacheClusterId,
    rebootCacheCluster_cacheNodeIdsToReboot,
    rebootCacheClusterResponse_cacheCluster,
    rebootCacheClusterResponse_httpStatus,

    -- ** IncreaseNodeGroupsInGlobalReplicationGroup
    increaseNodeGroupsInGlobalReplicationGroup_regionalConfigurations,
    increaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId,
    increaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount,
    increaseNodeGroupsInGlobalReplicationGroup_applyImmediately,
    increaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup,
    increaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus,

    -- ** StartMigration
    startMigration_replicationGroupId,
    startMigration_customerNodeEndpointList,
    startMigrationResponse_replicationGroup,
    startMigrationResponse_httpStatus,

    -- ** DeleteCacheSecurityGroup
    deleteCacheSecurityGroup_cacheSecurityGroupName,

    -- ** AuthorizeCacheSecurityGroupIngress
    authorizeCacheSecurityGroupIngress_cacheSecurityGroupName,
    authorizeCacheSecurityGroupIngress_eC2SecurityGroupName,
    authorizeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId,
    authorizeCacheSecurityGroupIngressResponse_cacheSecurityGroup,
    authorizeCacheSecurityGroupIngressResponse_httpStatus,

    -- ** DecreaseReplicaCount
    decreaseReplicaCount_newReplicaCount,
    decreaseReplicaCount_replicasToRemove,
    decreaseReplicaCount_replicaConfiguration,
    decreaseReplicaCount_replicationGroupId,
    decreaseReplicaCount_applyImmediately,
    decreaseReplicaCountResponse_replicationGroup,
    decreaseReplicaCountResponse_httpStatus,

    -- ** CreateCacheSecurityGroup
    createCacheSecurityGroup_tags,
    createCacheSecurityGroup_cacheSecurityGroupName,
    createCacheSecurityGroup_description,
    createCacheSecurityGroupResponse_cacheSecurityGroup,
    createCacheSecurityGroupResponse_httpStatus,

    -- ** RebalanceSlotsInGlobalReplicationGroup
    rebalanceSlotsInGlobalReplicationGroup_globalReplicationGroupId,
    rebalanceSlotsInGlobalReplicationGroup_applyImmediately,
    rebalanceSlotsInGlobalReplicationGroupResponse_globalReplicationGroup,
    rebalanceSlotsInGlobalReplicationGroupResponse_httpStatus,

    -- ** CopySnapshot
    copySnapshot_targetBucket,
    copySnapshot_kmsKeyId,
    copySnapshot_tags,
    copySnapshot_sourceSnapshotName,
    copySnapshot_targetSnapshotName,
    copySnapshotResponse_snapshot,
    copySnapshotResponse_httpStatus,

    -- ** ModifyCacheCluster
    modifyCacheCluster_securityGroupIds,
    modifyCacheCluster_numCacheNodes,
    modifyCacheCluster_cacheSecurityGroupNames,
    modifyCacheCluster_authTokenUpdateStrategy,
    modifyCacheCluster_snapshotWindow,
    modifyCacheCluster_notificationTopicStatus,
    modifyCacheCluster_newAvailabilityZones,
    modifyCacheCluster_cacheParameterGroupName,
    modifyCacheCluster_snapshotRetentionLimit,
    modifyCacheCluster_cacheNodeIdsToRemove,
    modifyCacheCluster_engineVersion,
    modifyCacheCluster_cacheNodeType,
    modifyCacheCluster_preferredMaintenanceWindow,
    modifyCacheCluster_notificationTopicArn,
    modifyCacheCluster_authToken,
    modifyCacheCluster_logDeliveryConfigurations,
    modifyCacheCluster_aZMode,
    modifyCacheCluster_applyImmediately,
    modifyCacheCluster_autoMinorVersionUpgrade,
    modifyCacheCluster_cacheClusterId,
    modifyCacheClusterResponse_cacheCluster,
    modifyCacheClusterResponse_httpStatus,

    -- ** DescribeCacheSubnetGroups
    describeCacheSubnetGroups_cacheSubnetGroupName,
    describeCacheSubnetGroups_maxRecords,
    describeCacheSubnetGroups_marker,
    describeCacheSubnetGroupsResponse_cacheSubnetGroups,
    describeCacheSubnetGroupsResponse_marker,
    describeCacheSubnetGroupsResponse_httpStatus,

    -- ** DescribeGlobalReplicationGroups
    describeGlobalReplicationGroups_globalReplicationGroupId,
    describeGlobalReplicationGroups_showMemberInfo,
    describeGlobalReplicationGroups_maxRecords,
    describeGlobalReplicationGroups_marker,
    describeGlobalReplicationGroupsResponse_globalReplicationGroups,
    describeGlobalReplicationGroupsResponse_marker,
    describeGlobalReplicationGroupsResponse_httpStatus,

    -- ** DescribeReservedCacheNodes
    describeReservedCacheNodes_duration,
    describeReservedCacheNodes_reservedCacheNodesOfferingId,
    describeReservedCacheNodes_cacheNodeType,
    describeReservedCacheNodes_offeringType,
    describeReservedCacheNodes_reservedCacheNodeId,
    describeReservedCacheNodes_productDescription,
    describeReservedCacheNodes_maxRecords,
    describeReservedCacheNodes_marker,
    describeReservedCacheNodesResponse_reservedCacheNodes,
    describeReservedCacheNodesResponse_marker,
    describeReservedCacheNodesResponse_httpStatus,

    -- ** DeleteCacheParameterGroup
    deleteCacheParameterGroup_cacheParameterGroupName,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceName,
    removeTagsFromResource_tagKeys,
    tagListMessage_tagList,

    -- ** DescribeCacheSecurityGroups
    describeCacheSecurityGroups_cacheSecurityGroupName,
    describeCacheSecurityGroups_maxRecords,
    describeCacheSecurityGroups_marker,
    describeCacheSecurityGroupsResponse_cacheSecurityGroups,
    describeCacheSecurityGroupsResponse_marker,
    describeCacheSecurityGroupsResponse_httpStatus,

    -- ** ModifyReplicationGroup
    modifyReplicationGroup_securityGroupIds,
    modifyReplicationGroup_automaticFailoverEnabled,
    modifyReplicationGroup_cacheSecurityGroupNames,
    modifyReplicationGroup_authTokenUpdateStrategy,
    modifyReplicationGroup_primaryClusterId,
    modifyReplicationGroup_snapshotWindow,
    modifyReplicationGroup_notificationTopicStatus,
    modifyReplicationGroup_cacheParameterGroupName,
    modifyReplicationGroup_userGroupIdsToRemove,
    modifyReplicationGroup_replicationGroupDescription,
    modifyReplicationGroup_snapshotRetentionLimit,
    modifyReplicationGroup_userGroupIdsToAdd,
    modifyReplicationGroup_nodeGroupId,
    modifyReplicationGroup_snapshottingClusterId,
    modifyReplicationGroup_multiAZEnabled,
    modifyReplicationGroup_engineVersion,
    modifyReplicationGroup_cacheNodeType,
    modifyReplicationGroup_preferredMaintenanceWindow,
    modifyReplicationGroup_notificationTopicArn,
    modifyReplicationGroup_removeUserGroups,
    modifyReplicationGroup_authToken,
    modifyReplicationGroup_logDeliveryConfigurations,
    modifyReplicationGroup_applyImmediately,
    modifyReplicationGroup_autoMinorVersionUpgrade,
    modifyReplicationGroup_replicationGroupId,
    modifyReplicationGroupResponse_replicationGroup,
    modifyReplicationGroupResponse_httpStatus,

    -- ** BatchStopUpdateAction
    batchStopUpdateAction_cacheClusterIds,
    batchStopUpdateAction_replicationGroupIds,
    batchStopUpdateAction_serviceUpdateName,
    updateActionResultsMessage_processedUpdateActions,
    updateActionResultsMessage_unprocessedUpdateActions,

    -- ** PurchaseReservedCacheNodesOffering
    purchaseReservedCacheNodesOffering_cacheNodeCount,
    purchaseReservedCacheNodesOffering_tags,
    purchaseReservedCacheNodesOffering_reservedCacheNodeId,
    purchaseReservedCacheNodesOffering_reservedCacheNodesOfferingId,
    purchaseReservedCacheNodesOfferingResponse_reservedCacheNode,
    purchaseReservedCacheNodesOfferingResponse_httpStatus,

    -- ** DeleteSnapshot
    deleteSnapshot_snapshotName,
    deleteSnapshotResponse_snapshot,
    deleteSnapshotResponse_httpStatus,

    -- ** CreateUser
    createUser_passwords,
    createUser_noPasswordRequired,
    createUser_tags,
    createUser_userId,
    createUser_userName,
    createUser_engine,
    createUser_accessString,
    user_status,
    user_accessString,
    user_userGroupIds,
    user_authentication,
    user_arn,
    user_userId,
    user_engine,
    user_userName,

    -- ** CompleteMigration
    completeMigration_force,
    completeMigration_replicationGroupId,
    completeMigrationResponse_replicationGroup,
    completeMigrationResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_duration,
    describeEvents_startTime,
    describeEvents_endTime,
    describeEvents_sourceIdentifier,
    describeEvents_sourceType,
    describeEvents_maxRecords,
    describeEvents_marker,
    describeEventsResponse_events,
    describeEventsResponse_marker,
    describeEventsResponse_httpStatus,

    -- ** TestFailover
    testFailover_replicationGroupId,
    testFailover_nodeGroupId,
    testFailoverResponse_replicationGroup,
    testFailoverResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_replicationGroupId,
    describeSnapshots_cacheClusterId,
    describeSnapshots_snapshotSource,
    describeSnapshots_showNodeGroupConfig,
    describeSnapshots_snapshotName,
    describeSnapshots_maxRecords,
    describeSnapshots_marker,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_marker,
    describeSnapshotsResponse_httpStatus,

    -- ** DisassociateGlobalReplicationGroup
    disassociateGlobalReplicationGroup_globalReplicationGroupId,
    disassociateGlobalReplicationGroup_replicationGroupId,
    disassociateGlobalReplicationGroup_replicationGroupRegion,
    disassociateGlobalReplicationGroupResponse_globalReplicationGroup,
    disassociateGlobalReplicationGroupResponse_httpStatus,

    -- ** DeleteReplicationGroup
    deleteReplicationGroup_finalSnapshotIdentifier,
    deleteReplicationGroup_retainPrimaryCluster,
    deleteReplicationGroup_replicationGroupId,
    deleteReplicationGroupResponse_replicationGroup,
    deleteReplicationGroupResponse_httpStatus,

    -- ** CreateCacheCluster
    createCacheCluster_securityGroupIds,
    createCacheCluster_numCacheNodes,
    createCacheCluster_cacheSecurityGroupNames,
    createCacheCluster_preferredAvailabilityZones,
    createCacheCluster_replicationGroupId,
    createCacheCluster_snapshotWindow,
    createCacheCluster_cacheParameterGroupName,
    createCacheCluster_outpostMode,
    createCacheCluster_snapshotRetentionLimit,
    createCacheCluster_preferredOutpostArns,
    createCacheCluster_preferredAvailabilityZone,
    createCacheCluster_snapshotArns,
    createCacheCluster_cacheSubnetGroupName,
    createCacheCluster_engineVersion,
    createCacheCluster_cacheNodeType,
    createCacheCluster_preferredMaintenanceWindow,
    createCacheCluster_tags,
    createCacheCluster_notificationTopicArn,
    createCacheCluster_port,
    createCacheCluster_preferredOutpostArn,
    createCacheCluster_engine,
    createCacheCluster_snapshotName,
    createCacheCluster_authToken,
    createCacheCluster_logDeliveryConfigurations,
    createCacheCluster_aZMode,
    createCacheCluster_autoMinorVersionUpgrade,
    createCacheCluster_cacheClusterId,
    createCacheClusterResponse_cacheCluster,
    createCacheClusterResponse_httpStatus,

    -- ** BatchApplyUpdateAction
    batchApplyUpdateAction_cacheClusterIds,
    batchApplyUpdateAction_replicationGroupIds,
    batchApplyUpdateAction_serviceUpdateName,
    updateActionResultsMessage_processedUpdateActions,
    updateActionResultsMessage_unprocessedUpdateActions,

    -- ** ListAllowedNodeTypeModifications
    listAllowedNodeTypeModifications_replicationGroupId,
    listAllowedNodeTypeModifications_cacheClusterId,
    listAllowedNodeTypeModificationsResponse_scaleUpModifications,
    listAllowedNodeTypeModificationsResponse_scaleDownModifications,
    listAllowedNodeTypeModificationsResponse_httpStatus,

    -- ** IncreaseReplicaCount
    increaseReplicaCount_newReplicaCount,
    increaseReplicaCount_replicaConfiguration,
    increaseReplicaCount_replicationGroupId,
    increaseReplicaCount_applyImmediately,
    increaseReplicaCountResponse_replicationGroup,
    increaseReplicaCountResponse_httpStatus,

    -- ** ResetCacheParameterGroup
    resetCacheParameterGroup_resetAllParameters,
    resetCacheParameterGroup_parameterNameValues,
    resetCacheParameterGroup_cacheParameterGroupName,
    cacheParameterGroupNameMessage_cacheParameterGroupName,

    -- ** ModifyReplicationGroupShardConfiguration
    modifyReplicationGroupShardConfiguration_nodeGroupsToRetain,
    modifyReplicationGroupShardConfiguration_nodeGroupsToRemove,
    modifyReplicationGroupShardConfiguration_reshardingConfiguration,
    modifyReplicationGroupShardConfiguration_replicationGroupId,
    modifyReplicationGroupShardConfiguration_nodeGroupCount,
    modifyReplicationGroupShardConfiguration_applyImmediately,
    modifyReplicationGroupShardConfigurationResponse_replicationGroup,
    modifyReplicationGroupShardConfigurationResponse_httpStatus,

    -- ** DescribeUsers
    describeUsers_userId,
    describeUsers_engine,
    describeUsers_filters,
    describeUsers_maxRecords,
    describeUsers_marker,
    describeUsersResponse_users,
    describeUsersResponse_marker,
    describeUsersResponse_httpStatus,

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

    -- ** DescribeCacheParameterGroups
    describeCacheParameterGroups_cacheParameterGroupName,
    describeCacheParameterGroups_maxRecords,
    describeCacheParameterGroups_marker,
    describeCacheParameterGroupsResponse_cacheParameterGroups,
    describeCacheParameterGroupsResponse_marker,
    describeCacheParameterGroupsResponse_httpStatus,

    -- ** FailoverGlobalReplicationGroup
    failoverGlobalReplicationGroup_globalReplicationGroupId,
    failoverGlobalReplicationGroup_primaryRegion,
    failoverGlobalReplicationGroup_primaryReplicationGroupId,
    failoverGlobalReplicationGroupResponse_globalReplicationGroup,
    failoverGlobalReplicationGroupResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceName,
    addTagsToResource_tags,
    tagListMessage_tagList,

    -- ** DecreaseNodeGroupsInGlobalReplicationGroup
    decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRemove,
    decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRetain,
    decreaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId,
    decreaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount,
    decreaseNodeGroupsInGlobalReplicationGroup_applyImmediately,
    decreaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup,
    decreaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus,

    -- ** DeleteGlobalReplicationGroup
    deleteGlobalReplicationGroup_globalReplicationGroupId,
    deleteGlobalReplicationGroup_retainPrimaryReplicationGroup,
    deleteGlobalReplicationGroupResponse_globalReplicationGroup,
    deleteGlobalReplicationGroupResponse_httpStatus,

    -- ** ModifyUser
    modifyUser_appendAccessString,
    modifyUser_passwords,
    modifyUser_accessString,
    modifyUser_noPasswordRequired,
    modifyUser_userId,
    user_status,
    user_accessString,
    user_userGroupIds,
    user_authentication,
    user_arn,
    user_userId,
    user_engine,
    user_userName,

    -- ** DeleteCacheSubnetGroup
    deleteCacheSubnetGroup_cacheSubnetGroupName,

    -- ** DescribeUpdateActions
    describeUpdateActions_updateActionStatus,
    describeUpdateActions_serviceUpdateStatus,
    describeUpdateActions_showNodeLevelUpdateStatus,
    describeUpdateActions_engine,
    describeUpdateActions_serviceUpdateTimeRange,
    describeUpdateActions_serviceUpdateName,
    describeUpdateActions_cacheClusterIds,
    describeUpdateActions_replicationGroupIds,
    describeUpdateActions_maxRecords,
    describeUpdateActions_marker,
    describeUpdateActionsResponse_updateActions,
    describeUpdateActionsResponse_marker,
    describeUpdateActionsResponse_httpStatus,

    -- ** DescribeCacheEngineVersions
    describeCacheEngineVersions_defaultOnly,
    describeCacheEngineVersions_engineVersion,
    describeCacheEngineVersions_cacheParameterGroupFamily,
    describeCacheEngineVersions_engine,
    describeCacheEngineVersions_maxRecords,
    describeCacheEngineVersions_marker,
    describeCacheEngineVersionsResponse_cacheEngineVersions,
    describeCacheEngineVersionsResponse_marker,
    describeCacheEngineVersionsResponse_httpStatus,

    -- ** ModifyUserGroup
    modifyUserGroup_userIdsToRemove,
    modifyUserGroup_userIdsToAdd,
    modifyUserGroup_userGroupId,
    userGroup_status,
    userGroup_replicationGroups,
    userGroup_arn,
    userGroup_userIds,
    userGroup_engine,
    userGroup_userGroupId,
    userGroup_pendingChanges,

    -- ** CreateCacheParameterGroup
    createCacheParameterGroup_tags,
    createCacheParameterGroup_cacheParameterGroupName,
    createCacheParameterGroup_cacheParameterGroupFamily,
    createCacheParameterGroup_description,
    createCacheParameterGroupResponse_cacheParameterGroup,
    createCacheParameterGroupResponse_httpStatus,

    -- ** DescribeCacheParameters
    describeCacheParameters_source,
    describeCacheParameters_maxRecords,
    describeCacheParameters_marker,
    describeCacheParameters_cacheParameterGroupName,
    describeCacheParametersResponse_cacheNodeTypeSpecificParameters,
    describeCacheParametersResponse_parameters,
    describeCacheParametersResponse_marker,
    describeCacheParametersResponse_httpStatus,

    -- ** DescribeUserGroups
    describeUserGroups_userGroupId,
    describeUserGroups_maxRecords,
    describeUserGroups_marker,
    describeUserGroupsResponse_userGroups,
    describeUserGroupsResponse_marker,
    describeUserGroupsResponse_httpStatus,

    -- ** DescribeCacheClusters
    describeCacheClusters_showCacheNodeInfo,
    describeCacheClusters_cacheClusterId,
    describeCacheClusters_showCacheClustersNotInReplicationGroups,
    describeCacheClusters_maxRecords,
    describeCacheClusters_marker,
    describeCacheClustersResponse_cacheClusters,
    describeCacheClustersResponse_marker,
    describeCacheClustersResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_replicationGroupId,
    createSnapshot_cacheClusterId,
    createSnapshot_kmsKeyId,
    createSnapshot_tags,
    createSnapshot_snapshotName,
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,

    -- ** ModifyGlobalReplicationGroup
    modifyGlobalReplicationGroup_automaticFailoverEnabled,
    modifyGlobalReplicationGroup_cacheParameterGroupName,
    modifyGlobalReplicationGroup_engineVersion,
    modifyGlobalReplicationGroup_cacheNodeType,
    modifyGlobalReplicationGroup_globalReplicationGroupDescription,
    modifyGlobalReplicationGroup_globalReplicationGroupId,
    modifyGlobalReplicationGroup_applyImmediately,
    modifyGlobalReplicationGroupResponse_globalReplicationGroup,
    modifyGlobalReplicationGroupResponse_httpStatus,

    -- ** ModifyCacheSubnetGroup
    modifyCacheSubnetGroup_subnetIds,
    modifyCacheSubnetGroup_cacheSubnetGroupDescription,
    modifyCacheSubnetGroup_cacheSubnetGroupName,
    modifyCacheSubnetGroupResponse_cacheSubnetGroup,
    modifyCacheSubnetGroupResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_userId,
    user_status,
    user_accessString,
    user_userGroupIds,
    user_authentication,
    user_arn,
    user_userId,
    user_engine,
    user_userName,

    -- ** DescribeReplicationGroups
    describeReplicationGroups_replicationGroupId,
    describeReplicationGroups_maxRecords,
    describeReplicationGroups_marker,
    describeReplicationGroupsResponse_replicationGroups,
    describeReplicationGroupsResponse_marker,
    describeReplicationGroupsResponse_httpStatus,

    -- ** DescribeReservedCacheNodesOfferings
    describeReservedCacheNodesOfferings_duration,
    describeReservedCacheNodesOfferings_reservedCacheNodesOfferingId,
    describeReservedCacheNodesOfferings_cacheNodeType,
    describeReservedCacheNodesOfferings_offeringType,
    describeReservedCacheNodesOfferings_productDescription,
    describeReservedCacheNodesOfferings_maxRecords,
    describeReservedCacheNodesOfferings_marker,
    describeReservedCacheNodesOfferingsResponse_reservedCacheNodesOfferings,
    describeReservedCacheNodesOfferingsResponse_marker,
    describeReservedCacheNodesOfferingsResponse_httpStatus,

    -- ** DescribeEngineDefaultParameters
    describeEngineDefaultParameters_maxRecords,
    describeEngineDefaultParameters_marker,
    describeEngineDefaultParameters_cacheParameterGroupFamily,
    describeEngineDefaultParametersResponse_httpStatus,
    describeEngineDefaultParametersResponse_engineDefaults,

    -- ** ListTagsForResource
    listTagsForResource_resourceName,
    tagListMessage_tagList,

    -- ** RevokeCacheSecurityGroupIngress
    revokeCacheSecurityGroupIngress_cacheSecurityGroupName,
    revokeCacheSecurityGroupIngress_eC2SecurityGroupName,
    revokeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId,
    revokeCacheSecurityGroupIngressResponse_cacheSecurityGroup,
    revokeCacheSecurityGroupIngressResponse_httpStatus,

    -- ** CreateUserGroup
    createUserGroup_userIds,
    createUserGroup_tags,
    createUserGroup_userGroupId,
    createUserGroup_engine,
    userGroup_status,
    userGroup_replicationGroups,
    userGroup_arn,
    userGroup_userIds,
    userGroup_engine,
    userGroup_userGroupId,
    userGroup_pendingChanges,

    -- ** ModifyCacheParameterGroup
    modifyCacheParameterGroup_cacheParameterGroupName,
    modifyCacheParameterGroup_parameterNameValues,
    cacheParameterGroupNameMessage_cacheParameterGroupName,

    -- ** DescribeServiceUpdates
    describeServiceUpdates_serviceUpdateStatus,
    describeServiceUpdates_serviceUpdateName,
    describeServiceUpdates_maxRecords,
    describeServiceUpdates_marker,
    describeServiceUpdatesResponse_serviceUpdates,
    describeServiceUpdatesResponse_marker,
    describeServiceUpdatesResponse_httpStatus,

    -- * Types

    -- ** Authentication
    authentication_passwordCount,
    authentication_type,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** CacheCluster
    cacheCluster_cacheNodes,
    cacheCluster_cacheClusterCreateTime,
    cacheCluster_numCacheNodes,
    cacheCluster_replicationGroupId,
    cacheCluster_cacheSecurityGroups,
    cacheCluster_cacheClusterId,
    cacheCluster_snapshotWindow,
    cacheCluster_notificationConfiguration,
    cacheCluster_snapshotRetentionLimit,
    cacheCluster_arn,
    cacheCluster_preferredAvailabilityZone,
    cacheCluster_cacheParameterGroup,
    cacheCluster_securityGroups,
    cacheCluster_atRestEncryptionEnabled,
    cacheCluster_cacheSubnetGroupName,
    cacheCluster_engineVersion,
    cacheCluster_authTokenLastModifiedDate,
    cacheCluster_cacheNodeType,
    cacheCluster_preferredMaintenanceWindow,
    cacheCluster_clientDownloadLandingPage,
    cacheCluster_preferredOutpostArn,
    cacheCluster_engine,
    cacheCluster_replicationGroupLogDeliveryEnabled,
    cacheCluster_pendingModifiedValues,
    cacheCluster_authTokenEnabled,
    cacheCluster_configurationEndpoint,
    cacheCluster_logDeliveryConfigurations,
    cacheCluster_transitEncryptionEnabled,
    cacheCluster_cacheClusterStatus,
    cacheCluster_autoMinorVersionUpgrade,

    -- ** CacheEngineVersion
    cacheEngineVersion_cacheEngineDescription,
    cacheEngineVersion_cacheEngineVersionDescription,
    cacheEngineVersion_engineVersion,
    cacheEngineVersion_cacheParameterGroupFamily,
    cacheEngineVersion_engine,

    -- ** CacheNode
    cacheNode_customerAvailabilityZone,
    cacheNode_cacheNodeStatus,
    cacheNode_cacheNodeCreateTime,
    cacheNode_parameterGroupStatus,
    cacheNode_customerOutpostArn,
    cacheNode_sourceCacheNodeId,
    cacheNode_cacheNodeId,
    cacheNode_endpoint,

    -- ** CacheNodeTypeSpecificParameter
    cacheNodeTypeSpecificParameter_changeType,
    cacheNodeTypeSpecificParameter_allowedValues,
    cacheNodeTypeSpecificParameter_source,
    cacheNodeTypeSpecificParameter_cacheNodeTypeSpecificValues,
    cacheNodeTypeSpecificParameter_parameterName,
    cacheNodeTypeSpecificParameter_description,
    cacheNodeTypeSpecificParameter_dataType,
    cacheNodeTypeSpecificParameter_isModifiable,
    cacheNodeTypeSpecificParameter_minimumEngineVersion,

    -- ** CacheNodeTypeSpecificValue
    cacheNodeTypeSpecificValue_cacheNodeType,
    cacheNodeTypeSpecificValue_value,

    -- ** CacheNodeUpdateStatus
    cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate,
    cacheNodeUpdateStatus_nodeUpdateInitiatedBy,
    cacheNodeUpdateStatus_nodeUpdateStatus,
    cacheNodeUpdateStatus_nodeUpdateInitiatedDate,
    cacheNodeUpdateStatus_cacheNodeId,
    cacheNodeUpdateStatus_nodeDeletionDate,
    cacheNodeUpdateStatus_nodeUpdateStartDate,
    cacheNodeUpdateStatus_nodeUpdateEndDate,

    -- ** CacheParameterGroup
    cacheParameterGroup_isGlobal,
    cacheParameterGroup_cacheParameterGroupName,
    cacheParameterGroup_arn,
    cacheParameterGroup_cacheParameterGroupFamily,
    cacheParameterGroup_description,

    -- ** CacheParameterGroupNameMessage
    cacheParameterGroupNameMessage_cacheParameterGroupName,

    -- ** CacheParameterGroupStatus
    cacheParameterGroupStatus_cacheParameterGroupName,
    cacheParameterGroupStatus_parameterApplyStatus,
    cacheParameterGroupStatus_cacheNodeIdsToReboot,

    -- ** CacheSecurityGroup
    cacheSecurityGroup_ownerId,
    cacheSecurityGroup_arn,
    cacheSecurityGroup_cacheSecurityGroupName,
    cacheSecurityGroup_eC2SecurityGroups,
    cacheSecurityGroup_description,

    -- ** CacheSecurityGroupMembership
    cacheSecurityGroupMembership_status,
    cacheSecurityGroupMembership_cacheSecurityGroupName,

    -- ** CacheSubnetGroup
    cacheSubnetGroup_arn,
    cacheSubnetGroup_cacheSubnetGroupName,
    cacheSubnetGroup_cacheSubnetGroupDescription,
    cacheSubnetGroup_vpcId,
    cacheSubnetGroup_subnets,

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
    eC2SecurityGroup_status,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_eC2SecurityGroupName,

    -- ** Endpoint
    endpoint_address,
    endpoint_port,

    -- ** EngineDefaults
    engineDefaults_cacheNodeTypeSpecificParameters,
    engineDefaults_cacheParameterGroupFamily,
    engineDefaults_parameters,
    engineDefaults_marker,

    -- ** Event
    event_message,
    event_date,
    event_sourceIdentifier,
    event_sourceType,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** GlobalNodeGroup
    globalNodeGroup_globalNodeGroupId,
    globalNodeGroup_slots,

    -- ** GlobalReplicationGroup
    globalReplicationGroup_clusterEnabled,
    globalReplicationGroup_status,
    globalReplicationGroup_globalReplicationGroupId,
    globalReplicationGroup_arn,
    globalReplicationGroup_globalNodeGroups,
    globalReplicationGroup_atRestEncryptionEnabled,
    globalReplicationGroup_engineVersion,
    globalReplicationGroup_cacheNodeType,
    globalReplicationGroup_globalReplicationGroupDescription,
    globalReplicationGroup_engine,
    globalReplicationGroup_authTokenEnabled,
    globalReplicationGroup_members,
    globalReplicationGroup_transitEncryptionEnabled,

    -- ** GlobalReplicationGroupInfo
    globalReplicationGroupInfo_globalReplicationGroupMemberRole,
    globalReplicationGroupInfo_globalReplicationGroupId,

    -- ** GlobalReplicationGroupMember
    globalReplicationGroupMember_status,
    globalReplicationGroupMember_automaticFailover,
    globalReplicationGroupMember_replicationGroupId,
    globalReplicationGroupMember_replicationGroupRegion,
    globalReplicationGroupMember_role,

    -- ** KinesisFirehoseDestinationDetails
    kinesisFirehoseDestinationDetails_deliveryStream,

    -- ** LogDeliveryConfiguration
    logDeliveryConfiguration_status,
    logDeliveryConfiguration_destinationType,
    logDeliveryConfiguration_logType,
    logDeliveryConfiguration_message,
    logDeliveryConfiguration_logFormat,
    logDeliveryConfiguration_destinationDetails,

    -- ** LogDeliveryConfigurationRequest
    logDeliveryConfigurationRequest_destinationType,
    logDeliveryConfigurationRequest_logType,
    logDeliveryConfigurationRequest_enabled,
    logDeliveryConfigurationRequest_logFormat,
    logDeliveryConfigurationRequest_destinationDetails,

    -- ** NodeGroup
    nodeGroup_status,
    nodeGroup_readerEndpoint,
    nodeGroup_nodeGroupId,
    nodeGroup_slots,
    nodeGroup_primaryEndpoint,
    nodeGroup_nodeGroupMembers,

    -- ** NodeGroupConfiguration
    nodeGroupConfiguration_primaryOutpostArn,
    nodeGroupConfiguration_replicaCount,
    nodeGroupConfiguration_replicaOutpostArns,
    nodeGroupConfiguration_nodeGroupId,
    nodeGroupConfiguration_slots,
    nodeGroupConfiguration_replicaAvailabilityZones,
    nodeGroupConfiguration_primaryAvailabilityZone,

    -- ** NodeGroupMember
    nodeGroupMember_cacheClusterId,
    nodeGroupMember_preferredAvailabilityZone,
    nodeGroupMember_readEndpoint,
    nodeGroupMember_cacheNodeId,
    nodeGroupMember_preferredOutpostArn,
    nodeGroupMember_currentRole,

    -- ** NodeGroupMemberUpdateStatus
    nodeGroupMemberUpdateStatus_cacheClusterId,
    nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate,
    nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy,
    nodeGroupMemberUpdateStatus_nodeUpdateStatus,
    nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate,
    nodeGroupMemberUpdateStatus_cacheNodeId,
    nodeGroupMemberUpdateStatus_nodeDeletionDate,
    nodeGroupMemberUpdateStatus_nodeUpdateStartDate,
    nodeGroupMemberUpdateStatus_nodeUpdateEndDate,

    -- ** NodeGroupUpdateStatus
    nodeGroupUpdateStatus_nodeGroupId,
    nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus,

    -- ** NodeSnapshot
    nodeSnapshot_nodeGroupConfiguration,
    nodeSnapshot_cacheSize,
    nodeSnapshot_cacheClusterId,
    nodeSnapshot_cacheNodeCreateTime,
    nodeSnapshot_snapshotCreateTime,
    nodeSnapshot_nodeGroupId,
    nodeSnapshot_cacheNodeId,

    -- ** NotificationConfiguration
    notificationConfiguration_topicStatus,
    notificationConfiguration_topicArn,

    -- ** Parameter
    parameter_changeType,
    parameter_allowedValues,
    parameter_source,
    parameter_parameterValue,
    parameter_parameterName,
    parameter_description,
    parameter_dataType,
    parameter_isModifiable,
    parameter_minimumEngineVersion,

    -- ** ParameterNameValue
    parameterNameValue_parameterValue,
    parameterNameValue_parameterName,

    -- ** PendingLogDeliveryConfiguration
    pendingLogDeliveryConfiguration_destinationType,
    pendingLogDeliveryConfiguration_logType,
    pendingLogDeliveryConfiguration_logFormat,
    pendingLogDeliveryConfiguration_destinationDetails,

    -- ** PendingModifiedValues
    pendingModifiedValues_numCacheNodes,
    pendingModifiedValues_cacheNodeIdsToRemove,
    pendingModifiedValues_authTokenStatus,
    pendingModifiedValues_engineVersion,
    pendingModifiedValues_cacheNodeType,
    pendingModifiedValues_logDeliveryConfigurations,

    -- ** ProcessedUpdateAction
    processedUpdateAction_replicationGroupId,
    processedUpdateAction_updateActionStatus,
    processedUpdateAction_cacheClusterId,
    processedUpdateAction_serviceUpdateName,

    -- ** RecurringCharge
    recurringCharge_recurringChargeFrequency,
    recurringCharge_recurringChargeAmount,

    -- ** RegionalConfiguration
    regionalConfiguration_replicationGroupId,
    regionalConfiguration_replicationGroupRegion,
    regionalConfiguration_reshardingConfiguration,

    -- ** ReplicationGroup
    replicationGroup_clusterEnabled,
    replicationGroup_status,
    replicationGroup_nodeGroups,
    replicationGroup_automaticFailover,
    replicationGroup_memberClustersOutpostArns,
    replicationGroup_memberClusters,
    replicationGroup_replicationGroupId,
    replicationGroup_globalReplicationGroupInfo,
    replicationGroup_userGroupIds,
    replicationGroup_snapshotWindow,
    replicationGroup_snapshotRetentionLimit,
    replicationGroup_arn,
    replicationGroup_multiAZ,
    replicationGroup_replicationGroupCreateTime,
    replicationGroup_kmsKeyId,
    replicationGroup_snapshottingClusterId,
    replicationGroup_atRestEncryptionEnabled,
    replicationGroup_authTokenLastModifiedDate,
    replicationGroup_cacheNodeType,
    replicationGroup_description,
    replicationGroup_pendingModifiedValues,
    replicationGroup_authTokenEnabled,
    replicationGroup_configurationEndpoint,
    replicationGroup_logDeliveryConfigurations,
    replicationGroup_transitEncryptionEnabled,

    -- ** ReplicationGroupPendingModifiedValues
    replicationGroupPendingModifiedValues_resharding,
    replicationGroupPendingModifiedValues_primaryClusterId,
    replicationGroupPendingModifiedValues_authTokenStatus,
    replicationGroupPendingModifiedValues_logDeliveryConfigurations,
    replicationGroupPendingModifiedValues_automaticFailoverStatus,
    replicationGroupPendingModifiedValues_userGroups,

    -- ** ReservedCacheNode
    reservedCacheNode_duration,
    reservedCacheNode_reservedCacheNodesOfferingId,
    reservedCacheNode_startTime,
    reservedCacheNode_cacheNodeCount,
    reservedCacheNode_state,
    reservedCacheNode_cacheNodeType,
    reservedCacheNode_fixedPrice,
    reservedCacheNode_usagePrice,
    reservedCacheNode_offeringType,
    reservedCacheNode_recurringCharges,
    reservedCacheNode_reservedCacheNodeId,
    reservedCacheNode_productDescription,
    reservedCacheNode_reservationARN,

    -- ** ReservedCacheNodesOffering
    reservedCacheNodesOffering_duration,
    reservedCacheNodesOffering_reservedCacheNodesOfferingId,
    reservedCacheNodesOffering_cacheNodeType,
    reservedCacheNodesOffering_fixedPrice,
    reservedCacheNodesOffering_usagePrice,
    reservedCacheNodesOffering_offeringType,
    reservedCacheNodesOffering_recurringCharges,
    reservedCacheNodesOffering_productDescription,

    -- ** ReshardingConfiguration
    reshardingConfiguration_preferredAvailabilityZones,
    reshardingConfiguration_nodeGroupId,

    -- ** ReshardingStatus
    reshardingStatus_slotMigration,

    -- ** SecurityGroupMembership
    securityGroupMembership_status,
    securityGroupMembership_securityGroupId,

    -- ** ServiceUpdate
    serviceUpdate_serviceUpdateSeverity,
    serviceUpdate_autoUpdateAfterRecommendedApplyByDate,
    serviceUpdate_serviceUpdateReleaseDate,
    serviceUpdate_serviceUpdateStatus,
    serviceUpdate_serviceUpdateRecommendedApplyByDate,
    serviceUpdate_serviceUpdateEndDate,
    serviceUpdate_engineVersion,
    serviceUpdate_serviceUpdateType,
    serviceUpdate_engine,
    serviceUpdate_estimatedUpdateTime,
    serviceUpdate_serviceUpdateName,
    serviceUpdate_serviceUpdateDescription,

    -- ** SlotMigration
    slotMigration_progressPercentage,

    -- ** Snapshot
    snapshot_cacheClusterCreateTime,
    snapshot_nodeSnapshots,
    snapshot_automaticFailover,
    snapshot_numCacheNodes,
    snapshot_replicationGroupId,
    snapshot_cacheClusterId,
    snapshot_snapshotStatus,
    snapshot_snapshotWindow,
    snapshot_cacheParameterGroupName,
    snapshot_replicationGroupDescription,
    snapshot_snapshotRetentionLimit,
    snapshot_arn,
    snapshot_preferredAvailabilityZone,
    snapshot_numNodeGroups,
    snapshot_kmsKeyId,
    snapshot_cacheSubnetGroupName,
    snapshot_engineVersion,
    snapshot_cacheNodeType,
    snapshot_preferredMaintenanceWindow,
    snapshot_topicArn,
    snapshot_snapshotSource,
    snapshot_port,
    snapshot_preferredOutpostArn,
    snapshot_engine,
    snapshot_snapshotName,
    snapshot_vpcId,
    snapshot_autoMinorVersionUpgrade,

    -- ** Subnet
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,
    subnet_subnetOutpost,

    -- ** SubnetOutpost
    subnetOutpost_subnetOutpostArn,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagListMessage
    tagListMessage_tagList,

    -- ** TimeRangeFilter
    timeRangeFilter_startTime,
    timeRangeFilter_endTime,

    -- ** UnprocessedUpdateAction
    unprocessedUpdateAction_replicationGroupId,
    unprocessedUpdateAction_cacheClusterId,
    unprocessedUpdateAction_errorType,
    unprocessedUpdateAction_errorMessage,
    unprocessedUpdateAction_serviceUpdateName,

    -- ** UpdateAction
    updateAction_serviceUpdateSeverity,
    updateAction_serviceUpdateReleaseDate,
    updateAction_replicationGroupId,
    updateAction_updateActionStatus,
    updateAction_cacheClusterId,
    updateAction_serviceUpdateStatus,
    updateAction_slaMet,
    updateAction_serviceUpdateRecommendedApplyByDate,
    updateAction_updateActionAvailableDate,
    updateAction_nodeGroupUpdateStatus,
    updateAction_serviceUpdateType,
    updateAction_cacheNodeUpdateStatus,
    updateAction_nodesUpdated,
    updateAction_engine,
    updateAction_estimatedUpdateTime,
    updateAction_updateActionStatusModifiedDate,
    updateAction_serviceUpdateName,

    -- ** UpdateActionResultsMessage
    updateActionResultsMessage_processedUpdateActions,
    updateActionResultsMessage_unprocessedUpdateActions,

    -- ** User
    user_status,
    user_accessString,
    user_userGroupIds,
    user_authentication,
    user_arn,
    user_userId,
    user_engine,
    user_userName,

    -- ** UserGroup
    userGroup_status,
    userGroup_replicationGroups,
    userGroup_arn,
    userGroup_userIds,
    userGroup_engine,
    userGroup_userGroupId,
    userGroup_pendingChanges,

    -- ** UserGroupPendingChanges
    userGroupPendingChanges_userIdsToRemove,
    userGroupPendingChanges_userIdsToAdd,

    -- ** UserGroupsUpdateStatus
    userGroupsUpdateStatus_userGroupIdsToRemove,
    userGroupsUpdateStatus_userGroupIdsToAdd,
  )
where

import Network.AWS.ElastiCache.AddTagsToResource
import Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.BatchApplyUpdateAction
import Network.AWS.ElastiCache.BatchStopUpdateAction
import Network.AWS.ElastiCache.CompleteMigration
import Network.AWS.ElastiCache.CopySnapshot
import Network.AWS.ElastiCache.CreateCacheCluster
import Network.AWS.ElastiCache.CreateCacheParameterGroup
import Network.AWS.ElastiCache.CreateCacheSecurityGroup
import Network.AWS.ElastiCache.CreateCacheSubnetGroup
import Network.AWS.ElastiCache.CreateGlobalReplicationGroup
import Network.AWS.ElastiCache.CreateReplicationGroup
import Network.AWS.ElastiCache.CreateSnapshot
import Network.AWS.ElastiCache.CreateUser
import Network.AWS.ElastiCache.CreateUserGroup
import Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
import Network.AWS.ElastiCache.DecreaseReplicaCount
import Network.AWS.ElastiCache.DeleteCacheCluster
import Network.AWS.ElastiCache.DeleteCacheParameterGroup
import Network.AWS.ElastiCache.DeleteCacheSecurityGroup
import Network.AWS.ElastiCache.DeleteCacheSubnetGroup
import Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
import Network.AWS.ElastiCache.DeleteReplicationGroup
import Network.AWS.ElastiCache.DeleteSnapshot
import Network.AWS.ElastiCache.DeleteUser
import Network.AWS.ElastiCache.DeleteUserGroup
import Network.AWS.ElastiCache.DescribeCacheClusters
import Network.AWS.ElastiCache.DescribeCacheEngineVersions
import Network.AWS.ElastiCache.DescribeCacheParameterGroups
import Network.AWS.ElastiCache.DescribeCacheParameters
import Network.AWS.ElastiCache.DescribeCacheSecurityGroups
import Network.AWS.ElastiCache.DescribeCacheSubnetGroups
import Network.AWS.ElastiCache.DescribeEngineDefaultParameters
import Network.AWS.ElastiCache.DescribeEvents
import Network.AWS.ElastiCache.DescribeGlobalReplicationGroups
import Network.AWS.ElastiCache.DescribeReplicationGroups
import Network.AWS.ElastiCache.DescribeReservedCacheNodes
import Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
import Network.AWS.ElastiCache.DescribeServiceUpdates
import Network.AWS.ElastiCache.DescribeSnapshots
import Network.AWS.ElastiCache.DescribeUpdateActions
import Network.AWS.ElastiCache.DescribeUserGroups
import Network.AWS.ElastiCache.DescribeUsers
import Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup
import Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
import Network.AWS.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
import Network.AWS.ElastiCache.IncreaseReplicaCount
import Network.AWS.ElastiCache.ListAllowedNodeTypeModifications
import Network.AWS.ElastiCache.ListTagsForResource
import Network.AWS.ElastiCache.ModifyCacheCluster
import Network.AWS.ElastiCache.ModifyCacheParameterGroup
import Network.AWS.ElastiCache.ModifyCacheSubnetGroup
import Network.AWS.ElastiCache.ModifyGlobalReplicationGroup
import Network.AWS.ElastiCache.ModifyReplicationGroup
import Network.AWS.ElastiCache.ModifyReplicationGroupShardConfiguration
import Network.AWS.ElastiCache.ModifyUser
import Network.AWS.ElastiCache.ModifyUserGroup
import Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
import Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
import Network.AWS.ElastiCache.RebootCacheCluster
import Network.AWS.ElastiCache.RemoveTagsFromResource
import Network.AWS.ElastiCache.ResetCacheParameterGroup
import Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.StartMigration
import Network.AWS.ElastiCache.TestFailover
import Network.AWS.ElastiCache.Types.Authentication
import Network.AWS.ElastiCache.Types.AvailabilityZone
import Network.AWS.ElastiCache.Types.CacheCluster
import Network.AWS.ElastiCache.Types.CacheEngineVersion
import Network.AWS.ElastiCache.Types.CacheNode
import Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
import Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue
import Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus
import Network.AWS.ElastiCache.Types.CacheParameterGroup
import Network.AWS.ElastiCache.Types.CacheParameterGroupNameMessage
import Network.AWS.ElastiCache.Types.CacheParameterGroupStatus
import Network.AWS.ElastiCache.Types.CacheSecurityGroup
import Network.AWS.ElastiCache.Types.CacheSecurityGroupMembership
import Network.AWS.ElastiCache.Types.CacheSubnetGroup
import Network.AWS.ElastiCache.Types.CloudWatchLogsDestinationDetails
import Network.AWS.ElastiCache.Types.ConfigureShard
import Network.AWS.ElastiCache.Types.CustomerNodeEndpoint
import Network.AWS.ElastiCache.Types.DestinationDetails
import Network.AWS.ElastiCache.Types.EC2SecurityGroup
import Network.AWS.ElastiCache.Types.Endpoint
import Network.AWS.ElastiCache.Types.EngineDefaults
import Network.AWS.ElastiCache.Types.Event
import Network.AWS.ElastiCache.Types.Filter
import Network.AWS.ElastiCache.Types.GlobalNodeGroup
import Network.AWS.ElastiCache.Types.GlobalReplicationGroup
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember
import Network.AWS.ElastiCache.Types.KinesisFirehoseDestinationDetails
import Network.AWS.ElastiCache.Types.LogDeliveryConfiguration
import Network.AWS.ElastiCache.Types.LogDeliveryConfigurationRequest
import Network.AWS.ElastiCache.Types.NodeGroup
import Network.AWS.ElastiCache.Types.NodeGroupConfiguration
import Network.AWS.ElastiCache.Types.NodeGroupMember
import Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus
import Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus
import Network.AWS.ElastiCache.Types.NodeSnapshot
import Network.AWS.ElastiCache.Types.NotificationConfiguration
import Network.AWS.ElastiCache.Types.Parameter
import Network.AWS.ElastiCache.Types.ParameterNameValue
import Network.AWS.ElastiCache.Types.PendingLogDeliveryConfiguration
import Network.AWS.ElastiCache.Types.PendingModifiedValues
import Network.AWS.ElastiCache.Types.ProcessedUpdateAction
import Network.AWS.ElastiCache.Types.RecurringCharge
import Network.AWS.ElastiCache.Types.RegionalConfiguration
import Network.AWS.ElastiCache.Types.ReplicationGroup
import Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
import Network.AWS.ElastiCache.Types.ReservedCacheNode
import Network.AWS.ElastiCache.Types.ReservedCacheNodesOffering
import Network.AWS.ElastiCache.Types.ReshardingConfiguration
import Network.AWS.ElastiCache.Types.ReshardingStatus
import Network.AWS.ElastiCache.Types.SecurityGroupMembership
import Network.AWS.ElastiCache.Types.ServiceUpdate
import Network.AWS.ElastiCache.Types.SlotMigration
import Network.AWS.ElastiCache.Types.Snapshot
import Network.AWS.ElastiCache.Types.Subnet
import Network.AWS.ElastiCache.Types.SubnetOutpost
import Network.AWS.ElastiCache.Types.Tag
import Network.AWS.ElastiCache.Types.TagListMessage
import Network.AWS.ElastiCache.Types.TimeRangeFilter
import Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
import Network.AWS.ElastiCache.Types.UpdateAction
import Network.AWS.ElastiCache.Types.UpdateActionResultsMessage
import Network.AWS.ElastiCache.Types.User
import Network.AWS.ElastiCache.Types.UserGroup
import Network.AWS.ElastiCache.Types.UserGroupPendingChanges
import Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus
