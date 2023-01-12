{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Neptune.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Lens
  ( -- * Operations

    -- ** AddRoleToDBCluster
    addRoleToDBCluster_featureName,
    addRoleToDBCluster_dbClusterIdentifier,
    addRoleToDBCluster_roleArn,

    -- ** AddSourceIdentifierToSubscription
    addSourceIdentifierToSubscription_subscriptionName,
    addSourceIdentifierToSubscription_sourceIdentifier,
    addSourceIdentifierToSubscriptionResponse_eventSubscription,
    addSourceIdentifierToSubscriptionResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceName,
    addTagsToResource_tags,

    -- ** ApplyPendingMaintenanceAction
    applyPendingMaintenanceAction_resourceIdentifier,
    applyPendingMaintenanceAction_applyAction,
    applyPendingMaintenanceAction_optInType,
    applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions,
    applyPendingMaintenanceActionResponse_httpStatus,

    -- ** CopyDBClusterParameterGroup
    copyDBClusterParameterGroup_tags,
    copyDBClusterParameterGroup_sourceDBClusterParameterGroupIdentifier,
    copyDBClusterParameterGroup_targetDBClusterParameterGroupIdentifier,
    copyDBClusterParameterGroup_targetDBClusterParameterGroupDescription,
    copyDBClusterParameterGroupResponse_dbClusterParameterGroup,
    copyDBClusterParameterGroupResponse_httpStatus,

    -- ** CopyDBClusterSnapshot
    copyDBClusterSnapshot_copyTags,
    copyDBClusterSnapshot_kmsKeyId,
    copyDBClusterSnapshot_preSignedUrl,
    copyDBClusterSnapshot_tags,
    copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier,
    copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier,
    copyDBClusterSnapshotResponse_dbClusterSnapshot,
    copyDBClusterSnapshotResponse_httpStatus,

    -- ** CopyDBParameterGroup
    copyDBParameterGroup_tags,
    copyDBParameterGroup_sourceDBParameterGroupIdentifier,
    copyDBParameterGroup_targetDBParameterGroupIdentifier,
    copyDBParameterGroup_targetDBParameterGroupDescription,
    copyDBParameterGroupResponse_dbParameterGroup,
    copyDBParameterGroupResponse_httpStatus,

    -- ** CreateDBCluster
    createDBCluster_availabilityZones,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_characterSetName,
    createDBCluster_copyTagsToSnapshot,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_databaseName,
    createDBCluster_deletionProtection,
    createDBCluster_enableCloudwatchLogsExports,
    createDBCluster_enableIAMDatabaseAuthentication,
    createDBCluster_engineVersion,
    createDBCluster_globalClusterIdentifier,
    createDBCluster_kmsKeyId,
    createDBCluster_masterUserPassword,
    createDBCluster_masterUsername,
    createDBCluster_optionGroupName,
    createDBCluster_port,
    createDBCluster_preSignedUrl,
    createDBCluster_preferredBackupWindow,
    createDBCluster_preferredMaintenanceWindow,
    createDBCluster_replicationSourceIdentifier,
    createDBCluster_serverlessV2ScalingConfiguration,
    createDBCluster_storageEncrypted,
    createDBCluster_tags,
    createDBCluster_vpcSecurityGroupIds,
    createDBCluster_dbClusterIdentifier,
    createDBCluster_engine,
    createDBClusterResponse_dbCluster,
    createDBClusterResponse_httpStatus,

    -- ** CreateDBClusterEndpoint
    createDBClusterEndpoint_excludedMembers,
    createDBClusterEndpoint_staticMembers,
    createDBClusterEndpoint_tags,
    createDBClusterEndpoint_dbClusterIdentifier,
    createDBClusterEndpoint_dbClusterEndpointIdentifier,
    createDBClusterEndpoint_endpointType,
    createDBClusterEndpointResponse_customEndpointType,
    createDBClusterEndpointResponse_dbClusterEndpointArn,
    createDBClusterEndpointResponse_dbClusterEndpointIdentifier,
    createDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier,
    createDBClusterEndpointResponse_dbClusterIdentifier,
    createDBClusterEndpointResponse_endpoint,
    createDBClusterEndpointResponse_endpointType,
    createDBClusterEndpointResponse_excludedMembers,
    createDBClusterEndpointResponse_staticMembers,
    createDBClusterEndpointResponse_status,
    createDBClusterEndpointResponse_httpStatus,

    -- ** CreateDBClusterParameterGroup
    createDBClusterParameterGroup_tags,
    createDBClusterParameterGroup_dbClusterParameterGroupName,
    createDBClusterParameterGroup_dbParameterGroupFamily,
    createDBClusterParameterGroup_description,
    createDBClusterParameterGroupResponse_dbClusterParameterGroup,
    createDBClusterParameterGroupResponse_httpStatus,

    -- ** CreateDBClusterSnapshot
    createDBClusterSnapshot_tags,
    createDBClusterSnapshot_dbClusterSnapshotIdentifier,
    createDBClusterSnapshot_dbClusterIdentifier,
    createDBClusterSnapshotResponse_dbClusterSnapshot,
    createDBClusterSnapshotResponse_httpStatus,

    -- ** CreateDBInstance
    createDBInstance_allocatedStorage,
    createDBInstance_autoMinorVersionUpgrade,
    createDBInstance_availabilityZone,
    createDBInstance_backupRetentionPeriod,
    createDBInstance_characterSetName,
    createDBInstance_copyTagsToSnapshot,
    createDBInstance_dbClusterIdentifier,
    createDBInstance_dbName,
    createDBInstance_dbParameterGroupName,
    createDBInstance_dbSecurityGroups,
    createDBInstance_dbSubnetGroupName,
    createDBInstance_deletionProtection,
    createDBInstance_domain,
    createDBInstance_domainIAMRoleName,
    createDBInstance_enableCloudwatchLogsExports,
    createDBInstance_enableIAMDatabaseAuthentication,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_engineVersion,
    createDBInstance_iops,
    createDBInstance_kmsKeyId,
    createDBInstance_licenseModel,
    createDBInstance_masterUserPassword,
    createDBInstance_masterUsername,
    createDBInstance_monitoringInterval,
    createDBInstance_monitoringRoleArn,
    createDBInstance_multiAZ,
    createDBInstance_optionGroupName,
    createDBInstance_performanceInsightsKMSKeyId,
    createDBInstance_port,
    createDBInstance_preferredBackupWindow,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_promotionTier,
    createDBInstance_publiclyAccessible,
    createDBInstance_storageEncrypted,
    createDBInstance_storageType,
    createDBInstance_tags,
    createDBInstance_tdeCredentialArn,
    createDBInstance_tdeCredentialPassword,
    createDBInstance_timezone,
    createDBInstance_vpcSecurityGroupIds,
    createDBInstance_dbInstanceIdentifier,
    createDBInstance_dbInstanceClass,
    createDBInstance_engine,
    createDBInstanceResponse_dbInstance,
    createDBInstanceResponse_httpStatus,

    -- ** CreateDBParameterGroup
    createDBParameterGroup_tags,
    createDBParameterGroup_dbParameterGroupName,
    createDBParameterGroup_dbParameterGroupFamily,
    createDBParameterGroup_description,
    createDBParameterGroupResponse_dbParameterGroup,
    createDBParameterGroupResponse_httpStatus,

    -- ** CreateDBSubnetGroup
    createDBSubnetGroup_tags,
    createDBSubnetGroup_dbSubnetGroupName,
    createDBSubnetGroup_dbSubnetGroupDescription,
    createDBSubnetGroup_subnetIds,
    createDBSubnetGroupResponse_dbSubnetGroup,
    createDBSubnetGroupResponse_httpStatus,

    -- ** CreateEventSubscription
    createEventSubscription_enabled,
    createEventSubscription_eventCategories,
    createEventSubscription_sourceIds,
    createEventSubscription_sourceType,
    createEventSubscription_tags,
    createEventSubscription_subscriptionName,
    createEventSubscription_snsTopicArn,
    createEventSubscriptionResponse_eventSubscription,
    createEventSubscriptionResponse_httpStatus,

    -- ** CreateGlobalCluster
    createGlobalCluster_deletionProtection,
    createGlobalCluster_engine,
    createGlobalCluster_engineVersion,
    createGlobalCluster_sourceDBClusterIdentifier,
    createGlobalCluster_storageEncrypted,
    createGlobalCluster_globalClusterIdentifier,
    createGlobalClusterResponse_globalCluster,
    createGlobalClusterResponse_httpStatus,

    -- ** DeleteDBCluster
    deleteDBCluster_finalDBSnapshotIdentifier,
    deleteDBCluster_skipFinalSnapshot,
    deleteDBCluster_dbClusterIdentifier,
    deleteDBClusterResponse_dbCluster,
    deleteDBClusterResponse_httpStatus,

    -- ** DeleteDBClusterEndpoint
    deleteDBClusterEndpoint_dbClusterEndpointIdentifier,
    deleteDBClusterEndpointResponse_customEndpointType,
    deleteDBClusterEndpointResponse_dbClusterEndpointArn,
    deleteDBClusterEndpointResponse_dbClusterEndpointIdentifier,
    deleteDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier,
    deleteDBClusterEndpointResponse_dbClusterIdentifier,
    deleteDBClusterEndpointResponse_endpoint,
    deleteDBClusterEndpointResponse_endpointType,
    deleteDBClusterEndpointResponse_excludedMembers,
    deleteDBClusterEndpointResponse_staticMembers,
    deleteDBClusterEndpointResponse_status,
    deleteDBClusterEndpointResponse_httpStatus,

    -- ** DeleteDBClusterParameterGroup
    deleteDBClusterParameterGroup_dbClusterParameterGroupName,

    -- ** DeleteDBClusterSnapshot
    deleteDBClusterSnapshot_dbClusterSnapshotIdentifier,
    deleteDBClusterSnapshotResponse_dbClusterSnapshot,
    deleteDBClusterSnapshotResponse_httpStatus,

    -- ** DeleteDBInstance
    deleteDBInstance_finalDBSnapshotIdentifier,
    deleteDBInstance_skipFinalSnapshot,
    deleteDBInstance_dbInstanceIdentifier,
    deleteDBInstanceResponse_dbInstance,
    deleteDBInstanceResponse_httpStatus,

    -- ** DeleteDBParameterGroup
    deleteDBParameterGroup_dbParameterGroupName,

    -- ** DeleteDBSubnetGroup
    deleteDBSubnetGroup_dbSubnetGroupName,

    -- ** DeleteEventSubscription
    deleteEventSubscription_subscriptionName,
    deleteEventSubscriptionResponse_eventSubscription,
    deleteEventSubscriptionResponse_httpStatus,

    -- ** DeleteGlobalCluster
    deleteGlobalCluster_globalClusterIdentifier,
    deleteGlobalClusterResponse_globalCluster,
    deleteGlobalClusterResponse_httpStatus,

    -- ** DescribeDBClusterEndpoints
    describeDBClusterEndpoints_dbClusterEndpointIdentifier,
    describeDBClusterEndpoints_dbClusterIdentifier,
    describeDBClusterEndpoints_filters,
    describeDBClusterEndpoints_marker,
    describeDBClusterEndpoints_maxRecords,
    describeDBClusterEndpointsResponse_dbClusterEndpoints,
    describeDBClusterEndpointsResponse_marker,
    describeDBClusterEndpointsResponse_httpStatus,

    -- ** DescribeDBClusterParameterGroups
    describeDBClusterParameterGroups_dbClusterParameterGroupName,
    describeDBClusterParameterGroups_filters,
    describeDBClusterParameterGroups_marker,
    describeDBClusterParameterGroups_maxRecords,
    describeDBClusterParameterGroupsResponse_dbClusterParameterGroups,
    describeDBClusterParameterGroupsResponse_marker,
    describeDBClusterParameterGroupsResponse_httpStatus,

    -- ** DescribeDBClusterParameters
    describeDBClusterParameters_filters,
    describeDBClusterParameters_marker,
    describeDBClusterParameters_maxRecords,
    describeDBClusterParameters_source,
    describeDBClusterParameters_dbClusterParameterGroupName,
    describeDBClusterParametersResponse_marker,
    describeDBClusterParametersResponse_parameters,
    describeDBClusterParametersResponse_httpStatus,

    -- ** DescribeDBClusterSnapshotAttributes
    describeDBClusterSnapshotAttributes_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshotAttributesResponse_dbClusterSnapshotAttributesResult,
    describeDBClusterSnapshotAttributesResponse_httpStatus,

    -- ** DescribeDBClusterSnapshots
    describeDBClusterSnapshots_dbClusterIdentifier,
    describeDBClusterSnapshots_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshots_filters,
    describeDBClusterSnapshots_includePublic,
    describeDBClusterSnapshots_includeShared,
    describeDBClusterSnapshots_marker,
    describeDBClusterSnapshots_maxRecords,
    describeDBClusterSnapshots_snapshotType,
    describeDBClusterSnapshotsResponse_dbClusterSnapshots,
    describeDBClusterSnapshotsResponse_marker,
    describeDBClusterSnapshotsResponse_httpStatus,

    -- ** DescribeDBClusters
    describeDBClusters_dbClusterIdentifier,
    describeDBClusters_filters,
    describeDBClusters_marker,
    describeDBClusters_maxRecords,
    describeDBClustersResponse_dbClusters,
    describeDBClustersResponse_marker,
    describeDBClustersResponse_httpStatus,

    -- ** DescribeDBEngineVersions
    describeDBEngineVersions_dbParameterGroupFamily,
    describeDBEngineVersions_defaultOnly,
    describeDBEngineVersions_engine,
    describeDBEngineVersions_engineVersion,
    describeDBEngineVersions_filters,
    describeDBEngineVersions_listSupportedCharacterSets,
    describeDBEngineVersions_listSupportedTimezones,
    describeDBEngineVersions_marker,
    describeDBEngineVersions_maxRecords,
    describeDBEngineVersionsResponse_dbEngineVersions,
    describeDBEngineVersionsResponse_marker,
    describeDBEngineVersionsResponse_httpStatus,

    -- ** DescribeDBInstances
    describeDBInstances_dbInstanceIdentifier,
    describeDBInstances_filters,
    describeDBInstances_marker,
    describeDBInstances_maxRecords,
    describeDBInstancesResponse_dbInstances,
    describeDBInstancesResponse_marker,
    describeDBInstancesResponse_httpStatus,

    -- ** DescribeDBParameterGroups
    describeDBParameterGroups_dbParameterGroupName,
    describeDBParameterGroups_filters,
    describeDBParameterGroups_marker,
    describeDBParameterGroups_maxRecords,
    describeDBParameterGroupsResponse_dbParameterGroups,
    describeDBParameterGroupsResponse_marker,
    describeDBParameterGroupsResponse_httpStatus,

    -- ** DescribeDBParameters
    describeDBParameters_filters,
    describeDBParameters_marker,
    describeDBParameters_maxRecords,
    describeDBParameters_source,
    describeDBParameters_dbParameterGroupName,
    describeDBParametersResponse_marker,
    describeDBParametersResponse_parameters,
    describeDBParametersResponse_httpStatus,

    -- ** DescribeDBSubnetGroups
    describeDBSubnetGroups_dbSubnetGroupName,
    describeDBSubnetGroups_filters,
    describeDBSubnetGroups_marker,
    describeDBSubnetGroups_maxRecords,
    describeDBSubnetGroupsResponse_dbSubnetGroups,
    describeDBSubnetGroupsResponse_marker,
    describeDBSubnetGroupsResponse_httpStatus,

    -- ** DescribeEngineDefaultClusterParameters
    describeEngineDefaultClusterParameters_filters,
    describeEngineDefaultClusterParameters_marker,
    describeEngineDefaultClusterParameters_maxRecords,
    describeEngineDefaultClusterParameters_dbParameterGroupFamily,
    describeEngineDefaultClusterParametersResponse_engineDefaults,
    describeEngineDefaultClusterParametersResponse_httpStatus,

    -- ** DescribeEngineDefaultParameters
    describeEngineDefaultParameters_filters,
    describeEngineDefaultParameters_marker,
    describeEngineDefaultParameters_maxRecords,
    describeEngineDefaultParameters_dbParameterGroupFamily,
    describeEngineDefaultParametersResponse_engineDefaults,
    describeEngineDefaultParametersResponse_httpStatus,

    -- ** DescribeEventCategories
    describeEventCategories_filters,
    describeEventCategories_sourceType,
    describeEventCategoriesResponse_eventCategoriesMapList,
    describeEventCategoriesResponse_httpStatus,

    -- ** DescribeEventSubscriptions
    describeEventSubscriptions_filters,
    describeEventSubscriptions_marker,
    describeEventSubscriptions_maxRecords,
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_duration,
    describeEvents_endTime,
    describeEvents_eventCategories,
    describeEvents_filters,
    describeEvents_marker,
    describeEvents_maxRecords,
    describeEvents_sourceIdentifier,
    describeEvents_sourceType,
    describeEvents_startTime,
    describeEventsResponse_events,
    describeEventsResponse_marker,
    describeEventsResponse_httpStatus,

    -- ** DescribeGlobalClusters
    describeGlobalClusters_globalClusterIdentifier,
    describeGlobalClusters_marker,
    describeGlobalClusters_maxRecords,
    describeGlobalClustersResponse_globalClusters,
    describeGlobalClustersResponse_marker,
    describeGlobalClustersResponse_httpStatus,

    -- ** DescribeOrderableDBInstanceOptions
    describeOrderableDBInstanceOptions_dbInstanceClass,
    describeOrderableDBInstanceOptions_engineVersion,
    describeOrderableDBInstanceOptions_filters,
    describeOrderableDBInstanceOptions_licenseModel,
    describeOrderableDBInstanceOptions_marker,
    describeOrderableDBInstanceOptions_maxRecords,
    describeOrderableDBInstanceOptions_vpc,
    describeOrderableDBInstanceOptions_engine,
    describeOrderableDBInstanceOptionsResponse_marker,
    describeOrderableDBInstanceOptionsResponse_orderableDBInstanceOptions,
    describeOrderableDBInstanceOptionsResponse_httpStatus,

    -- ** DescribePendingMaintenanceActions
    describePendingMaintenanceActions_filters,
    describePendingMaintenanceActions_marker,
    describePendingMaintenanceActions_maxRecords,
    describePendingMaintenanceActions_resourceIdentifier,
    describePendingMaintenanceActionsResponse_marker,
    describePendingMaintenanceActionsResponse_pendingMaintenanceActions,
    describePendingMaintenanceActionsResponse_httpStatus,

    -- ** DescribeValidDBInstanceModifications
    describeValidDBInstanceModifications_dbInstanceIdentifier,
    describeValidDBInstanceModificationsResponse_validDBInstanceModificationsMessage,
    describeValidDBInstanceModificationsResponse_httpStatus,

    -- ** FailoverDBCluster
    failoverDBCluster_dbClusterIdentifier,
    failoverDBCluster_targetDBInstanceIdentifier,
    failoverDBClusterResponse_dbCluster,
    failoverDBClusterResponse_httpStatus,

    -- ** FailoverGlobalCluster
    failoverGlobalCluster_globalClusterIdentifier,
    failoverGlobalCluster_targetDbClusterIdentifier,
    failoverGlobalClusterResponse_globalCluster,
    failoverGlobalClusterResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_filters,
    listTagsForResource_resourceName,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** ModifyDBCluster
    modifyDBCluster_allowMajorVersionUpgrade,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_copyTagsToSnapshot,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_dbInstanceParameterGroupName,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_enableIAMDatabaseAuthentication,
    modifyDBCluster_engineVersion,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_optionGroupName,
    modifyDBCluster_port,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_serverlessV2ScalingConfiguration,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_dbClusterIdentifier,
    modifyDBClusterResponse_dbCluster,
    modifyDBClusterResponse_httpStatus,

    -- ** ModifyDBClusterEndpoint
    modifyDBClusterEndpoint_endpointType,
    modifyDBClusterEndpoint_excludedMembers,
    modifyDBClusterEndpoint_staticMembers,
    modifyDBClusterEndpoint_dbClusterEndpointIdentifier,
    modifyDBClusterEndpointResponse_customEndpointType,
    modifyDBClusterEndpointResponse_dbClusterEndpointArn,
    modifyDBClusterEndpointResponse_dbClusterEndpointIdentifier,
    modifyDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier,
    modifyDBClusterEndpointResponse_dbClusterIdentifier,
    modifyDBClusterEndpointResponse_endpoint,
    modifyDBClusterEndpointResponse_endpointType,
    modifyDBClusterEndpointResponse_excludedMembers,
    modifyDBClusterEndpointResponse_staticMembers,
    modifyDBClusterEndpointResponse_status,
    modifyDBClusterEndpointResponse_httpStatus,

    -- ** ModifyDBClusterParameterGroup
    modifyDBClusterParameterGroup_dbClusterParameterGroupName,
    modifyDBClusterParameterGroup_parameters,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** ModifyDBClusterSnapshotAttribute
    modifyDBClusterSnapshotAttribute_valuesToAdd,
    modifyDBClusterSnapshotAttribute_valuesToRemove,
    modifyDBClusterSnapshotAttribute_dbClusterSnapshotIdentifier,
    modifyDBClusterSnapshotAttribute_attributeName,
    modifyDBClusterSnapshotAttributeResponse_dbClusterSnapshotAttributesResult,
    modifyDBClusterSnapshotAttributeResponse_httpStatus,

    -- ** ModifyDBInstance
    modifyDBInstance_allocatedStorage,
    modifyDBInstance_allowMajorVersionUpgrade,
    modifyDBInstance_applyImmediately,
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_backupRetentionPeriod,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_cloudwatchLogsExportConfiguration,
    modifyDBInstance_copyTagsToSnapshot,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_dbParameterGroupName,
    modifyDBInstance_dbPortNumber,
    modifyDBInstance_dbSecurityGroups,
    modifyDBInstance_dbSubnetGroupName,
    modifyDBInstance_deletionProtection,
    modifyDBInstance_domain,
    modifyDBInstance_domainIAMRoleName,
    modifyDBInstance_enableIAMDatabaseAuthentication,
    modifyDBInstance_enablePerformanceInsights,
    modifyDBInstance_engineVersion,
    modifyDBInstance_iops,
    modifyDBInstance_licenseModel,
    modifyDBInstance_masterUserPassword,
    modifyDBInstance_monitoringInterval,
    modifyDBInstance_monitoringRoleArn,
    modifyDBInstance_multiAZ,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_optionGroupName,
    modifyDBInstance_performanceInsightsKMSKeyId,
    modifyDBInstance_preferredBackupWindow,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_promotionTier,
    modifyDBInstance_publiclyAccessible,
    modifyDBInstance_storageType,
    modifyDBInstance_tdeCredentialArn,
    modifyDBInstance_tdeCredentialPassword,
    modifyDBInstance_vpcSecurityGroupIds,
    modifyDBInstance_dbInstanceIdentifier,
    modifyDBInstanceResponse_dbInstance,
    modifyDBInstanceResponse_httpStatus,

    -- ** ModifyDBParameterGroup
    modifyDBParameterGroup_dbParameterGroupName,
    modifyDBParameterGroup_parameters,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** ModifyDBSubnetGroup
    modifyDBSubnetGroup_dbSubnetGroupDescription,
    modifyDBSubnetGroup_dbSubnetGroupName,
    modifyDBSubnetGroup_subnetIds,
    modifyDBSubnetGroupResponse_dbSubnetGroup,
    modifyDBSubnetGroupResponse_httpStatus,

    -- ** ModifyEventSubscription
    modifyEventSubscription_enabled,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_sourceType,
    modifyEventSubscription_subscriptionName,
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,

    -- ** ModifyGlobalCluster
    modifyGlobalCluster_allowMajorVersionUpgrade,
    modifyGlobalCluster_deletionProtection,
    modifyGlobalCluster_engineVersion,
    modifyGlobalCluster_newGlobalClusterIdentifier,
    modifyGlobalCluster_globalClusterIdentifier,
    modifyGlobalClusterResponse_globalCluster,
    modifyGlobalClusterResponse_httpStatus,

    -- ** PromoteReadReplicaDBCluster
    promoteReadReplicaDBCluster_dbClusterIdentifier,
    promoteReadReplicaDBClusterResponse_dbCluster,
    promoteReadReplicaDBClusterResponse_httpStatus,

    -- ** RebootDBInstance
    rebootDBInstance_forceFailover,
    rebootDBInstance_dbInstanceIdentifier,
    rebootDBInstanceResponse_dbInstance,
    rebootDBInstanceResponse_httpStatus,

    -- ** RemoveFromGlobalCluster
    removeFromGlobalCluster_globalClusterIdentifier,
    removeFromGlobalCluster_dbClusterIdentifier,
    removeFromGlobalClusterResponse_globalCluster,
    removeFromGlobalClusterResponse_httpStatus,

    -- ** RemoveRoleFromDBCluster
    removeRoleFromDBCluster_featureName,
    removeRoleFromDBCluster_dbClusterIdentifier,
    removeRoleFromDBCluster_roleArn,

    -- ** RemoveSourceIdentifierFromSubscription
    removeSourceIdentifierFromSubscription_subscriptionName,
    removeSourceIdentifierFromSubscription_sourceIdentifier,
    removeSourceIdentifierFromSubscriptionResponse_eventSubscription,
    removeSourceIdentifierFromSubscriptionResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceName,
    removeTagsFromResource_tagKeys,

    -- ** ResetDBClusterParameterGroup
    resetDBClusterParameterGroup_parameters,
    resetDBClusterParameterGroup_resetAllParameters,
    resetDBClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** ResetDBParameterGroup
    resetDBParameterGroup_parameters,
    resetDBParameterGroup_resetAllParameters,
    resetDBParameterGroup_dbParameterGroupName,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** RestoreDBClusterFromSnapshot
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_copyTagsToSnapshot,
    restoreDBClusterFromSnapshot_dbClusterParameterGroupName,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_databaseName,
    restoreDBClusterFromSnapshot_deletionProtection,
    restoreDBClusterFromSnapshot_enableCloudwatchLogsExports,
    restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication,
    restoreDBClusterFromSnapshot_engineVersion,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_optionGroupName,
    restoreDBClusterFromSnapshot_port,
    restoreDBClusterFromSnapshot_serverlessV2ScalingConfiguration,
    restoreDBClusterFromSnapshot_tags,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_dbClusterIdentifier,
    restoreDBClusterFromSnapshot_snapshotIdentifier,
    restoreDBClusterFromSnapshot_engine,
    restoreDBClusterFromSnapshotResponse_dbCluster,
    restoreDBClusterFromSnapshotResponse_httpStatus,

    -- ** RestoreDBClusterToPointInTime
    restoreDBClusterToPointInTime_dbClusterParameterGroupName,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_optionGroupName,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_serverlessV2ScalingConfiguration,
    restoreDBClusterToPointInTime_tags,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
    restoreDBClusterToPointInTime_dbClusterIdentifier,
    restoreDBClusterToPointInTime_sourceDBClusterIdentifier,
    restoreDBClusterToPointInTimeResponse_dbCluster,
    restoreDBClusterToPointInTimeResponse_httpStatus,

    -- ** StartDBCluster
    startDBCluster_dbClusterIdentifier,
    startDBClusterResponse_dbCluster,
    startDBClusterResponse_httpStatus,

    -- ** StopDBCluster
    stopDBCluster_dbClusterIdentifier,
    stopDBClusterResponse_dbCluster,
    stopDBClusterResponse_httpStatus,

    -- * Types

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** CharacterSet
    characterSet_characterSetDescription,
    characterSet_characterSetName,

    -- ** CloudwatchLogsExportConfiguration
    cloudwatchLogsExportConfiguration_disableLogTypes,
    cloudwatchLogsExportConfiguration_enableLogTypes,

    -- ** DBCluster
    dbCluster_allocatedStorage,
    dbCluster_associatedRoles,
    dbCluster_automaticRestartTime,
    dbCluster_availabilityZones,
    dbCluster_backupRetentionPeriod,
    dbCluster_characterSetName,
    dbCluster_cloneGroupId,
    dbCluster_clusterCreateTime,
    dbCluster_copyTagsToSnapshot,
    dbCluster_crossAccountClone,
    dbCluster_dbClusterArn,
    dbCluster_dbClusterIdentifier,
    dbCluster_dbClusterMembers,
    dbCluster_dbClusterOptionGroupMemberships,
    dbCluster_dbClusterParameterGroup,
    dbCluster_dbSubnetGroup,
    dbCluster_databaseName,
    dbCluster_dbClusterResourceId,
    dbCluster_deletionProtection,
    dbCluster_earliestRestorableTime,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_endpoint,
    dbCluster_engine,
    dbCluster_engineVersion,
    dbCluster_hostedZoneId,
    dbCluster_iAMDatabaseAuthenticationEnabled,
    dbCluster_kmsKeyId,
    dbCluster_latestRestorableTime,
    dbCluster_masterUsername,
    dbCluster_multiAZ,
    dbCluster_percentProgress,
    dbCluster_port,
    dbCluster_preferredBackupWindow,
    dbCluster_preferredMaintenanceWindow,
    dbCluster_readReplicaIdentifiers,
    dbCluster_readerEndpoint,
    dbCluster_replicationSourceIdentifier,
    dbCluster_serverlessV2ScalingConfiguration,
    dbCluster_status,
    dbCluster_storageEncrypted,
    dbCluster_vpcSecurityGroups,

    -- ** DBClusterEndpoint
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_endpoint,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_excludedMembers,
    dbClusterEndpoint_staticMembers,
    dbClusterEndpoint_status,

    -- ** DBClusterMember
    dbClusterMember_dbClusterParameterGroupStatus,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_isClusterWriter,
    dbClusterMember_promotionTier,

    -- ** DBClusterOptionGroupStatus
    dbClusterOptionGroupStatus_dbClusterOptionGroupName,
    dbClusterOptionGroupStatus_status,

    -- ** DBClusterParameterGroup
    dbClusterParameterGroup_dbClusterParameterGroupArn,
    dbClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroup_dbParameterGroupFamily,
    dbClusterParameterGroup_description,

    -- ** DBClusterParameterGroupNameMessage
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DBClusterRole
    dbClusterRole_featureName,
    dbClusterRole_roleArn,
    dbClusterRole_status,

    -- ** DBClusterSnapshot
    dbClusterSnapshot_allocatedStorage,
    dbClusterSnapshot_availabilityZones,
    dbClusterSnapshot_clusterCreateTime,
    dbClusterSnapshot_dbClusterIdentifier,
    dbClusterSnapshot_dbClusterSnapshotArn,
    dbClusterSnapshot_dbClusterSnapshotIdentifier,
    dbClusterSnapshot_engine,
    dbClusterSnapshot_engineVersion,
    dbClusterSnapshot_iAMDatabaseAuthenticationEnabled,
    dbClusterSnapshot_kmsKeyId,
    dbClusterSnapshot_licenseModel,
    dbClusterSnapshot_masterUsername,
    dbClusterSnapshot_percentProgress,
    dbClusterSnapshot_port,
    dbClusterSnapshot_snapshotCreateTime,
    dbClusterSnapshot_snapshotType,
    dbClusterSnapshot_sourceDBClusterSnapshotArn,
    dbClusterSnapshot_status,
    dbClusterSnapshot_storageEncrypted,
    dbClusterSnapshot_vpcId,

    -- ** DBClusterSnapshotAttribute
    dbClusterSnapshotAttribute_attributeName,
    dbClusterSnapshotAttribute_attributeValues,

    -- ** DBClusterSnapshotAttributesResult
    dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier,

    -- ** DBEngineVersion
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_engine,
    dbEngineVersion_engineVersion,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_validUpgradeTarget,

    -- ** DBInstance
    dbInstance_allocatedStorage,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_availabilityZone,
    dbInstance_backupRetentionPeriod,
    dbInstance_cACertificateIdentifier,
    dbInstance_characterSetName,
    dbInstance_copyTagsToSnapshot,
    dbInstance_dbClusterIdentifier,
    dbInstance_dbInstanceArn,
    dbInstance_dbInstanceClass,
    dbInstance_dbInstanceIdentifier,
    dbInstance_dbInstanceStatus,
    dbInstance_dbName,
    dbInstance_dbParameterGroups,
    dbInstance_dbSecurityGroups,
    dbInstance_dbSubnetGroup,
    dbInstance_dbInstancePort,
    dbInstance_dbiResourceId,
    dbInstance_deletionProtection,
    dbInstance_domainMemberships,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_endpoint,
    dbInstance_engine,
    dbInstance_engineVersion,
    dbInstance_enhancedMonitoringResourceArn,
    dbInstance_iAMDatabaseAuthenticationEnabled,
    dbInstance_instanceCreateTime,
    dbInstance_iops,
    dbInstance_kmsKeyId,
    dbInstance_latestRestorableTime,
    dbInstance_licenseModel,
    dbInstance_masterUsername,
    dbInstance_monitoringInterval,
    dbInstance_monitoringRoleArn,
    dbInstance_multiAZ,
    dbInstance_optionGroupMemberships,
    dbInstance_pendingModifiedValues,
    dbInstance_performanceInsightsEnabled,
    dbInstance_performanceInsightsKMSKeyId,
    dbInstance_preferredBackupWindow,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_promotionTier,
    dbInstance_publiclyAccessible,
    dbInstance_readReplicaDBClusterIdentifiers,
    dbInstance_readReplicaDBInstanceIdentifiers,
    dbInstance_readReplicaSourceDBInstanceIdentifier,
    dbInstance_secondaryAvailabilityZone,
    dbInstance_statusInfos,
    dbInstance_storageEncrypted,
    dbInstance_storageType,
    dbInstance_tdeCredentialArn,
    dbInstance_timezone,
    dbInstance_vpcSecurityGroups,

    -- ** DBInstanceStatusInfo
    dbInstanceStatusInfo_message,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_statusType,

    -- ** DBParameterGroup
    dbParameterGroup_dbParameterGroupArn,
    dbParameterGroup_dbParameterGroupFamily,
    dbParameterGroup_dbParameterGroupName,
    dbParameterGroup_description,

    -- ** DBParameterGroupNameMessage
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** DBParameterGroupStatus
    dbParameterGroupStatus_dbParameterGroupName,
    dbParameterGroupStatus_parameterApplyStatus,

    -- ** DBSecurityGroupMembership
    dbSecurityGroupMembership_dbSecurityGroupName,
    dbSecurityGroupMembership_status,

    -- ** DBSubnetGroup
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_subnetGroupStatus,
    dbSubnetGroup_subnets,
    dbSubnetGroup_vpcId,

    -- ** DomainMembership
    domainMembership_domain,
    domainMembership_fqdn,
    domainMembership_iAMRoleName,
    domainMembership_status,

    -- ** DoubleRange
    doubleRange_from,
    doubleRange_to,

    -- ** Endpoint
    endpoint_address,
    endpoint_hostedZoneId,
    endpoint_port,

    -- ** EngineDefaults
    engineDefaults_dbParameterGroupFamily,
    engineDefaults_marker,
    engineDefaults_parameters,

    -- ** Event
    event_date,
    event_eventCategories,
    event_message,
    event_sourceArn,
    event_sourceIdentifier,
    event_sourceType,

    -- ** EventCategoriesMap
    eventCategoriesMap_eventCategories,
    eventCategoriesMap_sourceType,

    -- ** EventSubscription
    eventSubscription_custSubscriptionId,
    eventSubscription_customerAwsId,
    eventSubscription_enabled,
    eventSubscription_eventCategoriesList,
    eventSubscription_eventSubscriptionArn,
    eventSubscription_snsTopicArn,
    eventSubscription_sourceIdsList,
    eventSubscription_sourceType,
    eventSubscription_status,
    eventSubscription_subscriptionCreationTime,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** GlobalCluster
    globalCluster_deletionProtection,
    globalCluster_engine,
    globalCluster_engineVersion,
    globalCluster_globalClusterArn,
    globalCluster_globalClusterIdentifier,
    globalCluster_globalClusterMembers,
    globalCluster_globalClusterResourceId,
    globalCluster_status,
    globalCluster_storageEncrypted,

    -- ** GlobalClusterMember
    globalClusterMember_dbClusterArn,
    globalClusterMember_isWriter,
    globalClusterMember_readers,

    -- ** OptionGroupMembership
    optionGroupMembership_optionGroupName,
    optionGroupMembership_status,

    -- ** OrderableDBInstanceOption
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_licenseModel,
    orderableDBInstanceOption_maxIopsPerDbInstance,
    orderableDBInstanceOption_maxIopsPerGib,
    orderableDBInstanceOption_maxStorageSize,
    orderableDBInstanceOption_minIopsPerDbInstance,
    orderableDBInstanceOption_minIopsPerGib,
    orderableDBInstanceOption_minStorageSize,
    orderableDBInstanceOption_multiAZCapable,
    orderableDBInstanceOption_readReplicaCapable,
    orderableDBInstanceOption_storageType,
    orderableDBInstanceOption_supportsEnhancedMonitoring,
    orderableDBInstanceOption_supportsGlobalDatabases,
    orderableDBInstanceOption_supportsIAMDatabaseAuthentication,
    orderableDBInstanceOption_supportsIops,
    orderableDBInstanceOption_supportsPerformanceInsights,
    orderableDBInstanceOption_supportsStorageEncryption,
    orderableDBInstanceOption_vpc,

    -- ** Parameter
    parameter_allowedValues,
    parameter_applyMethod,
    parameter_applyType,
    parameter_dataType,
    parameter_description,
    parameter_isModifiable,
    parameter_minimumEngineVersion,
    parameter_parameterName,
    parameter_parameterValue,
    parameter_source,

    -- ** PendingCloudwatchLogsExports
    pendingCloudwatchLogsExports_logTypesToDisable,
    pendingCloudwatchLogsExports_logTypesToEnable,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_optInStatus,

    -- ** PendingModifiedValues
    pendingModifiedValues_allocatedStorage,
    pendingModifiedValues_backupRetentionPeriod,
    pendingModifiedValues_cACertificateIdentifier,
    pendingModifiedValues_dbInstanceClass,
    pendingModifiedValues_dbInstanceIdentifier,
    pendingModifiedValues_dbSubnetGroupName,
    pendingModifiedValues_engineVersion,
    pendingModifiedValues_iops,
    pendingModifiedValues_licenseModel,
    pendingModifiedValues_masterUserPassword,
    pendingModifiedValues_multiAZ,
    pendingModifiedValues_pendingCloudwatchLogsExports,
    pendingModifiedValues_port,
    pendingModifiedValues_storageType,

    -- ** Range
    range_from,
    range_step,
    range_to,

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- ** ServerlessV2ScalingConfiguration
    serverlessV2ScalingConfiguration_maxCapacity,
    serverlessV2ScalingConfiguration_minCapacity,

    -- ** ServerlessV2ScalingConfigurationInfo
    serverlessV2ScalingConfigurationInfo_maxCapacity,
    serverlessV2ScalingConfigurationInfo_minCapacity,

    -- ** Subnet
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,
    subnet_subnetStatus,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Timezone
    timezone_timezoneName,

    -- ** UpgradeTarget
    upgradeTarget_autoUpgrade,
    upgradeTarget_description,
    upgradeTarget_engine,
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,
    upgradeTarget_supportsGlobalDatabases,

    -- ** ValidDBInstanceModificationsMessage
    validDBInstanceModificationsMessage_storage,

    -- ** ValidStorageOptions
    validStorageOptions_iopsToStorageRatio,
    validStorageOptions_provisionedIops,
    validStorageOptions_storageSize,
    validStorageOptions_storageType,

    -- ** VpcSecurityGroupMembership
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import Amazonka.Neptune.AddRoleToDBCluster
import Amazonka.Neptune.AddSourceIdentifierToSubscription
import Amazonka.Neptune.AddTagsToResource
import Amazonka.Neptune.ApplyPendingMaintenanceAction
import Amazonka.Neptune.CopyDBClusterParameterGroup
import Amazonka.Neptune.CopyDBClusterSnapshot
import Amazonka.Neptune.CopyDBParameterGroup
import Amazonka.Neptune.CreateDBCluster
import Amazonka.Neptune.CreateDBClusterEndpoint
import Amazonka.Neptune.CreateDBClusterParameterGroup
import Amazonka.Neptune.CreateDBClusterSnapshot
import Amazonka.Neptune.CreateDBInstance
import Amazonka.Neptune.CreateDBParameterGroup
import Amazonka.Neptune.CreateDBSubnetGroup
import Amazonka.Neptune.CreateEventSubscription
import Amazonka.Neptune.CreateGlobalCluster
import Amazonka.Neptune.DeleteDBCluster
import Amazonka.Neptune.DeleteDBClusterEndpoint
import Amazonka.Neptune.DeleteDBClusterParameterGroup
import Amazonka.Neptune.DeleteDBClusterSnapshot
import Amazonka.Neptune.DeleteDBInstance
import Amazonka.Neptune.DeleteDBParameterGroup
import Amazonka.Neptune.DeleteDBSubnetGroup
import Amazonka.Neptune.DeleteEventSubscription
import Amazonka.Neptune.DeleteGlobalCluster
import Amazonka.Neptune.DescribeDBClusterEndpoints
import Amazonka.Neptune.DescribeDBClusterParameterGroups
import Amazonka.Neptune.DescribeDBClusterParameters
import Amazonka.Neptune.DescribeDBClusterSnapshotAttributes
import Amazonka.Neptune.DescribeDBClusterSnapshots
import Amazonka.Neptune.DescribeDBClusters
import Amazonka.Neptune.DescribeDBEngineVersions
import Amazonka.Neptune.DescribeDBInstances
import Amazonka.Neptune.DescribeDBParameterGroups
import Amazonka.Neptune.DescribeDBParameters
import Amazonka.Neptune.DescribeDBSubnetGroups
import Amazonka.Neptune.DescribeEngineDefaultClusterParameters
import Amazonka.Neptune.DescribeEngineDefaultParameters
import Amazonka.Neptune.DescribeEventCategories
import Amazonka.Neptune.DescribeEventSubscriptions
import Amazonka.Neptune.DescribeEvents
import Amazonka.Neptune.DescribeGlobalClusters
import Amazonka.Neptune.DescribeOrderableDBInstanceOptions
import Amazonka.Neptune.DescribePendingMaintenanceActions
import Amazonka.Neptune.DescribeValidDBInstanceModifications
import Amazonka.Neptune.FailoverDBCluster
import Amazonka.Neptune.FailoverGlobalCluster
import Amazonka.Neptune.ListTagsForResource
import Amazonka.Neptune.ModifyDBCluster
import Amazonka.Neptune.ModifyDBClusterEndpoint
import Amazonka.Neptune.ModifyDBClusterParameterGroup
import Amazonka.Neptune.ModifyDBClusterSnapshotAttribute
import Amazonka.Neptune.ModifyDBInstance
import Amazonka.Neptune.ModifyDBParameterGroup
import Amazonka.Neptune.ModifyDBSubnetGroup
import Amazonka.Neptune.ModifyEventSubscription
import Amazonka.Neptune.ModifyGlobalCluster
import Amazonka.Neptune.PromoteReadReplicaDBCluster
import Amazonka.Neptune.RebootDBInstance
import Amazonka.Neptune.RemoveFromGlobalCluster
import Amazonka.Neptune.RemoveRoleFromDBCluster
import Amazonka.Neptune.RemoveSourceIdentifierFromSubscription
import Amazonka.Neptune.RemoveTagsFromResource
import Amazonka.Neptune.ResetDBClusterParameterGroup
import Amazonka.Neptune.ResetDBParameterGroup
import Amazonka.Neptune.RestoreDBClusterFromSnapshot
import Amazonka.Neptune.RestoreDBClusterToPointInTime
import Amazonka.Neptune.StartDBCluster
import Amazonka.Neptune.StopDBCluster
import Amazonka.Neptune.Types.AvailabilityZone
import Amazonka.Neptune.Types.CharacterSet
import Amazonka.Neptune.Types.CloudwatchLogsExportConfiguration
import Amazonka.Neptune.Types.DBCluster
import Amazonka.Neptune.Types.DBClusterEndpoint
import Amazonka.Neptune.Types.DBClusterMember
import Amazonka.Neptune.Types.DBClusterOptionGroupStatus
import Amazonka.Neptune.Types.DBClusterParameterGroup
import Amazonka.Neptune.Types.DBClusterParameterGroupNameMessage
import Amazonka.Neptune.Types.DBClusterRole
import Amazonka.Neptune.Types.DBClusterSnapshot
import Amazonka.Neptune.Types.DBClusterSnapshotAttribute
import Amazonka.Neptune.Types.DBClusterSnapshotAttributesResult
import Amazonka.Neptune.Types.DBEngineVersion
import Amazonka.Neptune.Types.DBInstance
import Amazonka.Neptune.Types.DBInstanceStatusInfo
import Amazonka.Neptune.Types.DBParameterGroup
import Amazonka.Neptune.Types.DBParameterGroupNameMessage
import Amazonka.Neptune.Types.DBParameterGroupStatus
import Amazonka.Neptune.Types.DBSecurityGroupMembership
import Amazonka.Neptune.Types.DBSubnetGroup
import Amazonka.Neptune.Types.DomainMembership
import Amazonka.Neptune.Types.DoubleRange
import Amazonka.Neptune.Types.Endpoint
import Amazonka.Neptune.Types.EngineDefaults
import Amazonka.Neptune.Types.Event
import Amazonka.Neptune.Types.EventCategoriesMap
import Amazonka.Neptune.Types.EventSubscription
import Amazonka.Neptune.Types.Filter
import Amazonka.Neptune.Types.GlobalCluster
import Amazonka.Neptune.Types.GlobalClusterMember
import Amazonka.Neptune.Types.OptionGroupMembership
import Amazonka.Neptune.Types.OrderableDBInstanceOption
import Amazonka.Neptune.Types.Parameter
import Amazonka.Neptune.Types.PendingCloudwatchLogsExports
import Amazonka.Neptune.Types.PendingMaintenanceAction
import Amazonka.Neptune.Types.PendingModifiedValues
import Amazonka.Neptune.Types.Range
import Amazonka.Neptune.Types.ResourcePendingMaintenanceActions
import Amazonka.Neptune.Types.ServerlessV2ScalingConfiguration
import Amazonka.Neptune.Types.ServerlessV2ScalingConfigurationInfo
import Amazonka.Neptune.Types.Subnet
import Amazonka.Neptune.Types.Tag
import Amazonka.Neptune.Types.Timezone
import Amazonka.Neptune.Types.UpgradeTarget
import Amazonka.Neptune.Types.ValidDBInstanceModificationsMessage
import Amazonka.Neptune.Types.ValidStorageOptions
import Amazonka.Neptune.Types.VpcSecurityGroupMembership
