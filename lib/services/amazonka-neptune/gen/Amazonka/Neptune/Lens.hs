{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Neptune.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    copyDBClusterSnapshot_tags,
    copyDBClusterSnapshot_copyTags,
    copyDBClusterSnapshot_kmsKeyId,
    copyDBClusterSnapshot_preSignedUrl,
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
    createDBCluster_tags,
    createDBCluster_port,
    createDBCluster_vpcSecurityGroupIds,
    createDBCluster_preferredBackupWindow,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_characterSetName,
    createDBCluster_masterUsername,
    createDBCluster_copyTagsToSnapshot,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_databaseName,
    createDBCluster_optionGroupName,
    createDBCluster_availabilityZones,
    createDBCluster_enableIAMDatabaseAuthentication,
    createDBCluster_masterUserPassword,
    createDBCluster_enableCloudwatchLogsExports,
    createDBCluster_replicationSourceIdentifier,
    createDBCluster_storageEncrypted,
    createDBCluster_kmsKeyId,
    createDBCluster_globalClusterIdentifier,
    createDBCluster_deletionProtection,
    createDBCluster_preferredMaintenanceWindow,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_preSignedUrl,
    createDBCluster_engineVersion,
    createDBCluster_dbClusterIdentifier,
    createDBCluster_engine,
    createDBClusterResponse_dbCluster,
    createDBClusterResponse_httpStatus,

    -- ** CreateDBClusterEndpoint
    createDBClusterEndpoint_tags,
    createDBClusterEndpoint_staticMembers,
    createDBClusterEndpoint_excludedMembers,
    createDBClusterEndpoint_dbClusterIdentifier,
    createDBClusterEndpoint_dbClusterEndpointIdentifier,
    createDBClusterEndpoint_endpointType,
    createDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier,
    createDBClusterEndpointResponse_staticMembers,
    createDBClusterEndpointResponse_dbClusterIdentifier,
    createDBClusterEndpointResponse_excludedMembers,
    createDBClusterEndpointResponse_customEndpointType,
    createDBClusterEndpointResponse_status,
    createDBClusterEndpointResponse_endpointType,
    createDBClusterEndpointResponse_dbClusterEndpointIdentifier,
    createDBClusterEndpointResponse_dbClusterEndpointArn,
    createDBClusterEndpointResponse_endpoint,
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
    createDBInstance_tags,
    createDBInstance_port,
    createDBInstance_vpcSecurityGroupIds,
    createDBInstance_dbParameterGroupName,
    createDBInstance_preferredBackupWindow,
    createDBInstance_backupRetentionPeriod,
    createDBInstance_characterSetName,
    createDBInstance_masterUsername,
    createDBInstance_copyTagsToSnapshot,
    createDBInstance_domainIAMRoleName,
    createDBInstance_promotionTier,
    createDBInstance_dbSubnetGroupName,
    createDBInstance_autoMinorVersionUpgrade,
    createDBInstance_domain,
    createDBInstance_optionGroupName,
    createDBInstance_dbClusterIdentifier,
    createDBInstance_timezone,
    createDBInstance_performanceInsightsKMSKeyId,
    createDBInstance_enableIAMDatabaseAuthentication,
    createDBInstance_dbSecurityGroups,
    createDBInstance_monitoringInterval,
    createDBInstance_tdeCredentialPassword,
    createDBInstance_availabilityZone,
    createDBInstance_masterUserPassword,
    createDBInstance_publiclyAccessible,
    createDBInstance_storageType,
    createDBInstance_enableCloudwatchLogsExports,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_tdeCredentialArn,
    createDBInstance_monitoringRoleArn,
    createDBInstance_storageEncrypted,
    createDBInstance_kmsKeyId,
    createDBInstance_allocatedStorage,
    createDBInstance_deletionProtection,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_iops,
    createDBInstance_engineVersion,
    createDBInstance_dbName,
    createDBInstance_multiAZ,
    createDBInstance_licenseModel,
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
    createEventSubscription_tags,
    createEventSubscription_sourceIds,
    createEventSubscription_sourceType,
    createEventSubscription_enabled,
    createEventSubscription_eventCategories,
    createEventSubscription_subscriptionName,
    createEventSubscription_snsTopicArn,
    createEventSubscriptionResponse_eventSubscription,
    createEventSubscriptionResponse_httpStatus,

    -- ** CreateGlobalCluster
    createGlobalCluster_sourceDBClusterIdentifier,
    createGlobalCluster_storageEncrypted,
    createGlobalCluster_engine,
    createGlobalCluster_deletionProtection,
    createGlobalCluster_engineVersion,
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
    deleteDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier,
    deleteDBClusterEndpointResponse_staticMembers,
    deleteDBClusterEndpointResponse_dbClusterIdentifier,
    deleteDBClusterEndpointResponse_excludedMembers,
    deleteDBClusterEndpointResponse_customEndpointType,
    deleteDBClusterEndpointResponse_status,
    deleteDBClusterEndpointResponse_endpointType,
    deleteDBClusterEndpointResponse_dbClusterEndpointIdentifier,
    deleteDBClusterEndpointResponse_dbClusterEndpointArn,
    deleteDBClusterEndpointResponse_endpoint,
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
    describeDBClusterEndpoints_marker,
    describeDBClusterEndpoints_filters,
    describeDBClusterEndpoints_dbClusterIdentifier,
    describeDBClusterEndpoints_maxRecords,
    describeDBClusterEndpoints_dbClusterEndpointIdentifier,
    describeDBClusterEndpointsResponse_dbClusterEndpoints,
    describeDBClusterEndpointsResponse_marker,
    describeDBClusterEndpointsResponse_httpStatus,

    -- ** DescribeDBClusterParameterGroups
    describeDBClusterParameterGroups_marker,
    describeDBClusterParameterGroups_filters,
    describeDBClusterParameterGroups_maxRecords,
    describeDBClusterParameterGroups_dbClusterParameterGroupName,
    describeDBClusterParameterGroupsResponse_marker,
    describeDBClusterParameterGroupsResponse_dbClusterParameterGroups,
    describeDBClusterParameterGroupsResponse_httpStatus,

    -- ** DescribeDBClusterParameters
    describeDBClusterParameters_marker,
    describeDBClusterParameters_filters,
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
    describeDBClusterSnapshots_includeShared,
    describeDBClusterSnapshots_marker,
    describeDBClusterSnapshots_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshots_filters,
    describeDBClusterSnapshots_dbClusterIdentifier,
    describeDBClusterSnapshots_maxRecords,
    describeDBClusterSnapshots_includePublic,
    describeDBClusterSnapshots_snapshotType,
    describeDBClusterSnapshotsResponse_marker,
    describeDBClusterSnapshotsResponse_dbClusterSnapshots,
    describeDBClusterSnapshotsResponse_httpStatus,

    -- ** DescribeDBClusters
    describeDBClusters_marker,
    describeDBClusters_filters,
    describeDBClusters_dbClusterIdentifier,
    describeDBClusters_maxRecords,
    describeDBClustersResponse_marker,
    describeDBClustersResponse_dbClusters,
    describeDBClustersResponse_httpStatus,

    -- ** DescribeDBEngineVersions
    describeDBEngineVersions_marker,
    describeDBEngineVersions_filters,
    describeDBEngineVersions_maxRecords,
    describeDBEngineVersions_defaultOnly,
    describeDBEngineVersions_engine,
    describeDBEngineVersions_dbParameterGroupFamily,
    describeDBEngineVersions_listSupportedCharacterSets,
    describeDBEngineVersions_listSupportedTimezones,
    describeDBEngineVersions_engineVersion,
    describeDBEngineVersionsResponse_marker,
    describeDBEngineVersionsResponse_dbEngineVersions,
    describeDBEngineVersionsResponse_httpStatus,

    -- ** DescribeDBInstances
    describeDBInstances_marker,
    describeDBInstances_dbInstanceIdentifier,
    describeDBInstances_filters,
    describeDBInstances_maxRecords,
    describeDBInstancesResponse_marker,
    describeDBInstancesResponse_dbInstances,
    describeDBInstancesResponse_httpStatus,

    -- ** DescribeDBParameterGroups
    describeDBParameterGroups_dbParameterGroupName,
    describeDBParameterGroups_marker,
    describeDBParameterGroups_filters,
    describeDBParameterGroups_maxRecords,
    describeDBParameterGroupsResponse_marker,
    describeDBParameterGroupsResponse_dbParameterGroups,
    describeDBParameterGroupsResponse_httpStatus,

    -- ** DescribeDBParameters
    describeDBParameters_marker,
    describeDBParameters_filters,
    describeDBParameters_maxRecords,
    describeDBParameters_source,
    describeDBParameters_dbParameterGroupName,
    describeDBParametersResponse_marker,
    describeDBParametersResponse_parameters,
    describeDBParametersResponse_httpStatus,

    -- ** DescribeDBSubnetGroups
    describeDBSubnetGroups_marker,
    describeDBSubnetGroups_dbSubnetGroupName,
    describeDBSubnetGroups_filters,
    describeDBSubnetGroups_maxRecords,
    describeDBSubnetGroupsResponse_marker,
    describeDBSubnetGroupsResponse_dbSubnetGroups,
    describeDBSubnetGroupsResponse_httpStatus,

    -- ** DescribeEngineDefaultClusterParameters
    describeEngineDefaultClusterParameters_marker,
    describeEngineDefaultClusterParameters_filters,
    describeEngineDefaultClusterParameters_maxRecords,
    describeEngineDefaultClusterParameters_dbParameterGroupFamily,
    describeEngineDefaultClusterParametersResponse_engineDefaults,
    describeEngineDefaultClusterParametersResponse_httpStatus,

    -- ** DescribeEngineDefaultParameters
    describeEngineDefaultParameters_marker,
    describeEngineDefaultParameters_filters,
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
    describeEventSubscriptions_marker,
    describeEventSubscriptions_filters,
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptions_maxRecords,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_marker,
    describeEvents_filters,
    describeEvents_sourceType,
    describeEvents_endTime,
    describeEvents_maxRecords,
    describeEvents_duration,
    describeEvents_sourceIdentifier,
    describeEvents_eventCategories,
    describeEvents_startTime,
    describeEventsResponse_marker,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** DescribeGlobalClusters
    describeGlobalClusters_marker,
    describeGlobalClusters_maxRecords,
    describeGlobalClusters_globalClusterIdentifier,
    describeGlobalClustersResponse_marker,
    describeGlobalClustersResponse_globalClusters,
    describeGlobalClustersResponse_httpStatus,

    -- ** DescribeOrderableDBInstanceOptions
    describeOrderableDBInstanceOptions_dbInstanceClass,
    describeOrderableDBInstanceOptions_marker,
    describeOrderableDBInstanceOptions_vpc,
    describeOrderableDBInstanceOptions_filters,
    describeOrderableDBInstanceOptions_maxRecords,
    describeOrderableDBInstanceOptions_engineVersion,
    describeOrderableDBInstanceOptions_licenseModel,
    describeOrderableDBInstanceOptions_engine,
    describeOrderableDBInstanceOptionsResponse_marker,
    describeOrderableDBInstanceOptionsResponse_orderableDBInstanceOptions,
    describeOrderableDBInstanceOptionsResponse_httpStatus,

    -- ** DescribePendingMaintenanceActions
    describePendingMaintenanceActions_marker,
    describePendingMaintenanceActions_filters,
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
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_port,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_copyTagsToSnapshot,
    modifyDBCluster_dbInstanceParameterGroupName,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_allowMajorVersionUpgrade,
    modifyDBCluster_optionGroupName,
    modifyDBCluster_enableIAMDatabaseAuthentication,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_engineVersion,
    modifyDBCluster_dbClusterIdentifier,
    modifyDBClusterResponse_dbCluster,
    modifyDBClusterResponse_httpStatus,

    -- ** ModifyDBClusterEndpoint
    modifyDBClusterEndpoint_staticMembers,
    modifyDBClusterEndpoint_excludedMembers,
    modifyDBClusterEndpoint_endpointType,
    modifyDBClusterEndpoint_dbClusterEndpointIdentifier,
    modifyDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier,
    modifyDBClusterEndpointResponse_staticMembers,
    modifyDBClusterEndpointResponse_dbClusterIdentifier,
    modifyDBClusterEndpointResponse_excludedMembers,
    modifyDBClusterEndpointResponse_customEndpointType,
    modifyDBClusterEndpointResponse_status,
    modifyDBClusterEndpointResponse_endpointType,
    modifyDBClusterEndpointResponse_dbClusterEndpointIdentifier,
    modifyDBClusterEndpointResponse_dbClusterEndpointArn,
    modifyDBClusterEndpointResponse_endpoint,
    modifyDBClusterEndpointResponse_httpStatus,

    -- ** ModifyDBClusterParameterGroup
    modifyDBClusterParameterGroup_dbClusterParameterGroupName,
    modifyDBClusterParameterGroup_parameters,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** ModifyDBClusterSnapshotAttribute
    modifyDBClusterSnapshotAttribute_valuesToRemove,
    modifyDBClusterSnapshotAttribute_valuesToAdd,
    modifyDBClusterSnapshotAttribute_dbClusterSnapshotIdentifier,
    modifyDBClusterSnapshotAttribute_attributeName,
    modifyDBClusterSnapshotAttributeResponse_dbClusterSnapshotAttributesResult,
    modifyDBClusterSnapshotAttributeResponse_httpStatus,

    -- ** ModifyDBInstance
    modifyDBInstance_vpcSecurityGroupIds,
    modifyDBInstance_dbParameterGroupName,
    modifyDBInstance_preferredBackupWindow,
    modifyDBInstance_backupRetentionPeriod,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_copyTagsToSnapshot,
    modifyDBInstance_domainIAMRoleName,
    modifyDBInstance_promotionTier,
    modifyDBInstance_dbSubnetGroupName,
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_applyImmediately,
    modifyDBInstance_allowMajorVersionUpgrade,
    modifyDBInstance_dbPortNumber,
    modifyDBInstance_domain,
    modifyDBInstance_optionGroupName,
    modifyDBInstance_performanceInsightsKMSKeyId,
    modifyDBInstance_enableIAMDatabaseAuthentication,
    modifyDBInstance_dbSecurityGroups,
    modifyDBInstance_monitoringInterval,
    modifyDBInstance_tdeCredentialPassword,
    modifyDBInstance_masterUserPassword,
    modifyDBInstance_publiclyAccessible,
    modifyDBInstance_storageType,
    modifyDBInstance_enablePerformanceInsights,
    modifyDBInstance_tdeCredentialArn,
    modifyDBInstance_cloudwatchLogsExportConfiguration,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_monitoringRoleArn,
    modifyDBInstance_allocatedStorage,
    modifyDBInstance_deletionProtection,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_iops,
    modifyDBInstance_engineVersion,
    modifyDBInstance_multiAZ,
    modifyDBInstance_licenseModel,
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
    modifyEventSubscription_sourceType,
    modifyEventSubscription_enabled,
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_subscriptionName,
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,

    -- ** ModifyGlobalCluster
    modifyGlobalCluster_allowMajorVersionUpgrade,
    modifyGlobalCluster_deletionProtection,
    modifyGlobalCluster_newGlobalClusterIdentifier,
    modifyGlobalCluster_engineVersion,
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
    resetDBClusterParameterGroup_resetAllParameters,
    resetDBClusterParameterGroup_parameters,
    resetDBClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** ResetDBParameterGroup
    resetDBParameterGroup_resetAllParameters,
    resetDBParameterGroup_parameters,
    resetDBParameterGroup_dbParameterGroupName,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** RestoreDBClusterFromSnapshot
    restoreDBClusterFromSnapshot_tags,
    restoreDBClusterFromSnapshot_port,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_copyTagsToSnapshot,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_databaseName,
    restoreDBClusterFromSnapshot_optionGroupName,
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication,
    restoreDBClusterFromSnapshot_enableCloudwatchLogsExports,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_deletionProtection,
    restoreDBClusterFromSnapshot_dbClusterParameterGroupName,
    restoreDBClusterFromSnapshot_engineVersion,
    restoreDBClusterFromSnapshot_dbClusterIdentifier,
    restoreDBClusterFromSnapshot_snapshotIdentifier,
    restoreDBClusterFromSnapshot_engine,
    restoreDBClusterFromSnapshotResponse_dbCluster,
    restoreDBClusterFromSnapshotResponse_httpStatus,

    -- ** RestoreDBClusterToPointInTime
    restoreDBClusterToPointInTime_tags,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_optionGroupName,
    restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_dbClusterParameterGroupName,
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
    characterSet_characterSetName,
    characterSet_characterSetDescription,

    -- ** CloudwatchLogsExportConfiguration
    cloudwatchLogsExportConfiguration_enableLogTypes,
    cloudwatchLogsExportConfiguration_disableLogTypes,

    -- ** DBCluster
    dbCluster_port,
    dbCluster_cloneGroupId,
    dbCluster_dbClusterArn,
    dbCluster_hostedZoneId,
    dbCluster_percentProgress,
    dbCluster_preferredBackupWindow,
    dbCluster_backupRetentionPeriod,
    dbCluster_characterSetName,
    dbCluster_masterUsername,
    dbCluster_copyTagsToSnapshot,
    dbCluster_dbClusterMembers,
    dbCluster_dbClusterParameterGroup,
    dbCluster_latestRestorableTime,
    dbCluster_databaseName,
    dbCluster_dbClusterIdentifier,
    dbCluster_availabilityZones,
    dbCluster_automaticRestartTime,
    dbCluster_crossAccountClone,
    dbCluster_dbClusterOptionGroupMemberships,
    dbCluster_dbSubnetGroup,
    dbCluster_status,
    dbCluster_replicationSourceIdentifier,
    dbCluster_storageEncrypted,
    dbCluster_kmsKeyId,
    dbCluster_engine,
    dbCluster_allocatedStorage,
    dbCluster_readerEndpoint,
    dbCluster_earliestRestorableTime,
    dbCluster_iAMDatabaseAuthenticationEnabled,
    dbCluster_deletionProtection,
    dbCluster_preferredMaintenanceWindow,
    dbCluster_endpoint,
    dbCluster_clusterCreateTime,
    dbCluster_readReplicaIdentifiers,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_dbClusterResourceId,
    dbCluster_associatedRoles,
    dbCluster_engineVersion,
    dbCluster_multiAZ,
    dbCluster_vpcSecurityGroups,

    -- ** DBClusterEndpoint
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_staticMembers,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_excludedMembers,
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_status,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_endpoint,

    -- ** DBClusterMember
    dbClusterMember_promotionTier,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_dbClusterParameterGroupStatus,
    dbClusterMember_isClusterWriter,

    -- ** DBClusterOptionGroupStatus
    dbClusterOptionGroupStatus_status,
    dbClusterOptionGroupStatus_dbClusterOptionGroupName,

    -- ** DBClusterParameterGroup
    dbClusterParameterGroup_description,
    dbClusterParameterGroup_dbClusterParameterGroupArn,
    dbClusterParameterGroup_dbParameterGroupFamily,
    dbClusterParameterGroup_dbClusterParameterGroupName,

    -- ** DBClusterParameterGroupNameMessage
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DBClusterRole
    dbClusterRole_roleArn,
    dbClusterRole_featureName,
    dbClusterRole_status,

    -- ** DBClusterSnapshot
    dbClusterSnapshot_port,
    dbClusterSnapshot_percentProgress,
    dbClusterSnapshot_masterUsername,
    dbClusterSnapshot_dbClusterSnapshotIdentifier,
    dbClusterSnapshot_dbClusterIdentifier,
    dbClusterSnapshot_availabilityZones,
    dbClusterSnapshot_sourceDBClusterSnapshotArn,
    dbClusterSnapshot_status,
    dbClusterSnapshot_snapshotCreateTime,
    dbClusterSnapshot_storageEncrypted,
    dbClusterSnapshot_kmsKeyId,
    dbClusterSnapshot_engine,
    dbClusterSnapshot_allocatedStorage,
    dbClusterSnapshot_iAMDatabaseAuthenticationEnabled,
    dbClusterSnapshot_vpcId,
    dbClusterSnapshot_clusterCreateTime,
    dbClusterSnapshot_dbClusterSnapshotArn,
    dbClusterSnapshot_engineVersion,
    dbClusterSnapshot_licenseModel,
    dbClusterSnapshot_snapshotType,

    -- ** DBClusterSnapshotAttribute
    dbClusterSnapshotAttribute_attributeValues,
    dbClusterSnapshotAttribute_attributeName,

    -- ** DBClusterSnapshotAttributesResult
    dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes,

    -- ** DBEngineVersion
    dbEngineVersion_validUpgradeTarget,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_engineVersion,
    dbEngineVersion_dbEngineDescription,

    -- ** DBInstance
    dbInstance_dbInstanceStatus,
    dbInstance_optionGroupMemberships,
    dbInstance_preferredBackupWindow,
    dbInstance_backupRetentionPeriod,
    dbInstance_dbInstanceClass,
    dbInstance_characterSetName,
    dbInstance_masterUsername,
    dbInstance_copyTagsToSnapshot,
    dbInstance_promotionTier,
    dbInstance_secondaryAvailabilityZone,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_dbInstanceIdentifier,
    dbInstance_dbInstancePort,
    dbInstance_latestRestorableTime,
    dbInstance_readReplicaDBInstanceIdentifiers,
    dbInstance_enhancedMonitoringResourceArn,
    dbInstance_dbClusterIdentifier,
    dbInstance_domainMemberships,
    dbInstance_timezone,
    dbInstance_performanceInsightsKMSKeyId,
    dbInstance_dbSecurityGroups,
    dbInstance_dbSubnetGroup,
    dbInstance_monitoringInterval,
    dbInstance_performanceInsightsEnabled,
    dbInstance_instanceCreateTime,
    dbInstance_availabilityZone,
    dbInstance_publiclyAccessible,
    dbInstance_storageType,
    dbInstance_readReplicaDBClusterIdentifiers,
    dbInstance_tdeCredentialArn,
    dbInstance_dbInstanceArn,
    dbInstance_cACertificateIdentifier,
    dbInstance_monitoringRoleArn,
    dbInstance_storageEncrypted,
    dbInstance_kmsKeyId,
    dbInstance_engine,
    dbInstance_allocatedStorage,
    dbInstance_iAMDatabaseAuthenticationEnabled,
    dbInstance_deletionProtection,
    dbInstance_pendingModifiedValues,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_endpoint,
    dbInstance_dbiResourceId,
    dbInstance_dbParameterGroups,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_iops,
    dbInstance_engineVersion,
    dbInstance_dbName,
    dbInstance_multiAZ,
    dbInstance_readReplicaSourceDBInstanceIdentifier,
    dbInstance_licenseModel,
    dbInstance_statusInfos,
    dbInstance_vpcSecurityGroups,

    -- ** DBInstanceStatusInfo
    dbInstanceStatusInfo_message,
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_statusType,

    -- ** DBParameterGroup
    dbParameterGroup_dbParameterGroupName,
    dbParameterGroup_description,
    dbParameterGroup_dbParameterGroupFamily,
    dbParameterGroup_dbParameterGroupArn,

    -- ** DBParameterGroupNameMessage
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** DBParameterGroupStatus
    dbParameterGroupStatus_dbParameterGroupName,
    dbParameterGroupStatus_parameterApplyStatus,

    -- ** DBSecurityGroupMembership
    dbSecurityGroupMembership_status,
    dbSecurityGroupMembership_dbSecurityGroupName,

    -- ** DBSubnetGroup
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_subnetGroupStatus,
    dbSubnetGroup_subnets,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_vpcId,

    -- ** DomainMembership
    domainMembership_domain,
    domainMembership_fqdn,
    domainMembership_status,
    domainMembership_iAMRoleName,

    -- ** DoubleRange
    doubleRange_from,
    doubleRange_to,

    -- ** Endpoint
    endpoint_port,
    endpoint_hostedZoneId,
    endpoint_address,

    -- ** EngineDefaults
    engineDefaults_marker,
    engineDefaults_dbParameterGroupFamily,
    engineDefaults_parameters,

    -- ** Event
    event_message,
    event_sourceArn,
    event_date,
    event_sourceType,
    event_sourceIdentifier,
    event_eventCategories,

    -- ** EventCategoriesMap
    eventCategoriesMap_sourceType,
    eventCategoriesMap_eventCategories,

    -- ** EventSubscription
    eventSubscription_subscriptionCreationTime,
    eventSubscription_custSubscriptionId,
    eventSubscription_sourceIdsList,
    eventSubscription_status,
    eventSubscription_sourceType,
    eventSubscription_enabled,
    eventSubscription_snsTopicArn,
    eventSubscription_eventCategoriesList,
    eventSubscription_eventSubscriptionArn,
    eventSubscription_customerAwsId,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** GlobalCluster
    globalCluster_globalClusterMembers,
    globalCluster_status,
    globalCluster_globalClusterArn,
    globalCluster_storageEncrypted,
    globalCluster_globalClusterIdentifier,
    globalCluster_engine,
    globalCluster_deletionProtection,
    globalCluster_globalClusterResourceId,
    globalCluster_engineVersion,

    -- ** GlobalClusterMember
    globalClusterMember_dbClusterArn,
    globalClusterMember_isWriter,
    globalClusterMember_readers,

    -- ** OptionGroupMembership
    optionGroupMembership_optionGroupName,
    optionGroupMembership_status,

    -- ** OrderableDBInstanceOption
    orderableDBInstanceOption_supportsStorageEncryption,
    orderableDBInstanceOption_maxStorageSize,
    orderableDBInstanceOption_multiAZCapable,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_vpc,
    orderableDBInstanceOption_supportsPerformanceInsights,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_minIopsPerDbInstance,
    orderableDBInstanceOption_minStorageSize,
    orderableDBInstanceOption_storageType,
    orderableDBInstanceOption_supportsIops,
    orderableDBInstanceOption_maxIopsPerDbInstance,
    orderableDBInstanceOption_supportsIAMDatabaseAuthentication,
    orderableDBInstanceOption_supportsEnhancedMonitoring,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_readReplicaCapable,
    orderableDBInstanceOption_supportsGlobalDatabases,
    orderableDBInstanceOption_maxIopsPerGib,
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_minIopsPerGib,
    orderableDBInstanceOption_licenseModel,

    -- ** Parameter
    parameter_parameterValue,
    parameter_applyMethod,
    parameter_applyType,
    parameter_isModifiable,
    parameter_description,
    parameter_parameterName,
    parameter_minimumEngineVersion,
    parameter_source,
    parameter_allowedValues,
    parameter_dataType,

    -- ** PendingCloudwatchLogsExports
    pendingCloudwatchLogsExports_logTypesToEnable,
    pendingCloudwatchLogsExports_logTypesToDisable,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_forcedApplyDate,

    -- ** PendingModifiedValues
    pendingModifiedValues_port,
    pendingModifiedValues_backupRetentionPeriod,
    pendingModifiedValues_dbInstanceClass,
    pendingModifiedValues_dbSubnetGroupName,
    pendingModifiedValues_dbInstanceIdentifier,
    pendingModifiedValues_pendingCloudwatchLogsExports,
    pendingModifiedValues_masterUserPassword,
    pendingModifiedValues_storageType,
    pendingModifiedValues_cACertificateIdentifier,
    pendingModifiedValues_allocatedStorage,
    pendingModifiedValues_iops,
    pendingModifiedValues_engineVersion,
    pendingModifiedValues_multiAZ,
    pendingModifiedValues_licenseModel,

    -- ** Range
    range_from,
    range_to,
    range_step,

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_resourceIdentifier,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,

    -- ** Subnet
    subnet_subnetIdentifier,
    subnet_subnetStatus,
    subnet_subnetAvailabilityZone,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Timezone
    timezone_timezoneName,

    -- ** UpgradeTarget
    upgradeTarget_autoUpgrade,
    upgradeTarget_description,
    upgradeTarget_engine,
    upgradeTarget_supportsGlobalDatabases,
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,

    -- ** ValidDBInstanceModificationsMessage
    validDBInstanceModificationsMessage_storage,

    -- ** ValidStorageOptions
    validStorageOptions_storageSize,
    validStorageOptions_iopsToStorageRatio,
    validStorageOptions_provisionedIops,
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
import Amazonka.Neptune.Types.Subnet
import Amazonka.Neptune.Types.Tag
import Amazonka.Neptune.Types.Timezone
import Amazonka.Neptune.Types.UpgradeTarget
import Amazonka.Neptune.Types.ValidDBInstanceModificationsMessage
import Amazonka.Neptune.Types.ValidStorageOptions
import Amazonka.Neptune.Types.VpcSecurityGroupMembership
