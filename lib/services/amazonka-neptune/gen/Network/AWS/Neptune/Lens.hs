{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Neptune.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Neptune.Lens
  ( -- * Operations

    -- ** StartDBCluster
    startDBCluster_dbClusterIdentifier,
    startDBClusterResponse_dbCluster,
    startDBClusterResponse_httpStatus,

    -- ** DescribeDBClusterParameterGroups
    describeDBClusterParameterGroups_filters,
    describeDBClusterParameterGroups_marker,
    describeDBClusterParameterGroups_maxRecords,
    describeDBClusterParameterGroups_dbClusterParameterGroupName,
    describeDBClusterParameterGroupsResponse_marker,
    describeDBClusterParameterGroupsResponse_dbClusterParameterGroups,
    describeDBClusterParameterGroupsResponse_httpStatus,

    -- ** DescribeDBEngineVersions
    describeDBEngineVersions_engineVersion,
    describeDBEngineVersions_listSupportedTimezones,
    describeDBEngineVersions_defaultOnly,
    describeDBEngineVersions_filters,
    describeDBEngineVersions_engine,
    describeDBEngineVersions_dbParameterGroupFamily,
    describeDBEngineVersions_listSupportedCharacterSets,
    describeDBEngineVersions_marker,
    describeDBEngineVersions_maxRecords,
    describeDBEngineVersionsResponse_marker,
    describeDBEngineVersionsResponse_dbEngineVersions,
    describeDBEngineVersionsResponse_httpStatus,

    -- ** ModifyDBClusterEndpoint
    modifyDBClusterEndpoint_staticMembers,
    modifyDBClusterEndpoint_endpointType,
    modifyDBClusterEndpoint_excludedMembers,
    modifyDBClusterEndpoint_dbClusterEndpointIdentifier,
    modifyDBClusterEndpointResponse_status,
    modifyDBClusterEndpointResponse_dbClusterIdentifier,
    modifyDBClusterEndpointResponse_dbClusterEndpointArn,
    modifyDBClusterEndpointResponse_customEndpointType,
    modifyDBClusterEndpointResponse_staticMembers,
    modifyDBClusterEndpointResponse_endpointType,
    modifyDBClusterEndpointResponse_dbClusterEndpointIdentifier,
    modifyDBClusterEndpointResponse_endpoint,
    modifyDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier,
    modifyDBClusterEndpointResponse_excludedMembers,
    modifyDBClusterEndpointResponse_httpStatus,

    -- ** AddSourceIdentifierToSubscription
    addSourceIdentifierToSubscription_subscriptionName,
    addSourceIdentifierToSubscription_sourceIdentifier,
    addSourceIdentifierToSubscriptionResponse_eventSubscription,
    addSourceIdentifierToSubscriptionResponse_httpStatus,

    -- ** ModifyDBInstance
    modifyDBInstance_engineVersion,
    modifyDBInstance_dbSecurityGroups,
    modifyDBInstance_dbPortNumber,
    modifyDBInstance_deletionProtection,
    modifyDBInstance_masterUserPassword,
    modifyDBInstance_publiclyAccessible,
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_dbSubnetGroupName,
    modifyDBInstance_monitoringRoleArn,
    modifyDBInstance_iops,
    modifyDBInstance_allowMajorVersionUpgrade,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_domain,
    modifyDBInstance_monitoringInterval,
    modifyDBInstance_cloudwatchLogsExportConfiguration,
    modifyDBInstance_tdeCredentialPassword,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_promotionTier,
    modifyDBInstance_licenseModel,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_enablePerformanceInsights,
    modifyDBInstance_dbParameterGroupName,
    modifyDBInstance_preferredBackupWindow,
    modifyDBInstance_backupRetentionPeriod,
    modifyDBInstance_performanceInsightsKMSKeyId,
    modifyDBInstance_vpcSecurityGroupIds,
    modifyDBInstance_multiAZ,
    modifyDBInstance_allocatedStorage,
    modifyDBInstance_applyImmediately,
    modifyDBInstance_optionGroupName,
    modifyDBInstance_copyTagsToSnapshot,
    modifyDBInstance_tdeCredentialArn,
    modifyDBInstance_domainIAMRoleName,
    modifyDBInstance_enableIAMDatabaseAuthentication,
    modifyDBInstance_storageType,
    modifyDBInstance_dbInstanceIdentifier,
    modifyDBInstanceResponse_dbInstance,
    modifyDBInstanceResponse_httpStatus,

    -- ** ModifyEventSubscription
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_enabled,
    modifyEventSubscription_sourceType,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_subscriptionName,
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,

    -- ** ResetDBClusterParameterGroup
    resetDBClusterParameterGroup_resetAllParameters,
    resetDBClusterParameterGroup_parameters,
    resetDBClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DescribeEvents
    describeEvents_startTime,
    describeEvents_sourceType,
    describeEvents_filters,
    describeEvents_sourceIdentifier,
    describeEvents_eventCategories,
    describeEvents_marker,
    describeEvents_maxRecords,
    describeEvents_endTime,
    describeEvents_duration,
    describeEventsResponse_events,
    describeEventsResponse_marker,
    describeEventsResponse_httpStatus,

    -- ** DescribeEngineDefaultParameters
    describeEngineDefaultParameters_filters,
    describeEngineDefaultParameters_marker,
    describeEngineDefaultParameters_maxRecords,
    describeEngineDefaultParameters_dbParameterGroupFamily,
    describeEngineDefaultParametersResponse_engineDefaults,
    describeEngineDefaultParametersResponse_httpStatus,

    -- ** DescribeDBClusters
    describeDBClusters_dbClusterIdentifier,
    describeDBClusters_filters,
    describeDBClusters_marker,
    describeDBClusters_maxRecords,
    describeDBClustersResponse_dbClusters,
    describeDBClustersResponse_marker,
    describeDBClustersResponse_httpStatus,

    -- ** ModifyDBSubnetGroup
    modifyDBSubnetGroup_dbSubnetGroupDescription,
    modifyDBSubnetGroup_dbSubnetGroupName,
    modifyDBSubnetGroup_subnetIds,
    modifyDBSubnetGroupResponse_dbSubnetGroup,
    modifyDBSubnetGroupResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_filters,
    listTagsForResource_resourceName,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteDBCluster
    deleteDBCluster_finalDBSnapshotIdentifier,
    deleteDBCluster_skipFinalSnapshot,
    deleteDBCluster_dbClusterIdentifier,
    deleteDBClusterResponse_dbCluster,
    deleteDBClusterResponse_httpStatus,

    -- ** CopyDBParameterGroup
    copyDBParameterGroup_tags,
    copyDBParameterGroup_sourceDBParameterGroupIdentifier,
    copyDBParameterGroup_targetDBParameterGroupIdentifier,
    copyDBParameterGroup_targetDBParameterGroupDescription,
    copyDBParameterGroupResponse_dbParameterGroup,
    copyDBParameterGroupResponse_httpStatus,

    -- ** RemoveSourceIdentifierFromSubscription
    removeSourceIdentifierFromSubscription_subscriptionName,
    removeSourceIdentifierFromSubscription_sourceIdentifier,
    removeSourceIdentifierFromSubscriptionResponse_eventSubscription,
    removeSourceIdentifierFromSubscriptionResponse_httpStatus,

    -- ** DescribeEngineDefaultClusterParameters
    describeEngineDefaultClusterParameters_filters,
    describeEngineDefaultClusterParameters_marker,
    describeEngineDefaultClusterParameters_maxRecords,
    describeEngineDefaultClusterParameters_dbParameterGroupFamily,
    describeEngineDefaultClusterParametersResponse_engineDefaults,
    describeEngineDefaultClusterParametersResponse_httpStatus,

    -- ** PromoteReadReplicaDBCluster
    promoteReadReplicaDBCluster_dbClusterIdentifier,
    promoteReadReplicaDBClusterResponse_dbCluster,
    promoteReadReplicaDBClusterResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceName,
    removeTagsFromResource_tagKeys,

    -- ** CreateEventSubscription
    createEventSubscription_enabled,
    createEventSubscription_sourceType,
    createEventSubscription_eventCategories,
    createEventSubscription_sourceIds,
    createEventSubscription_tags,
    createEventSubscription_subscriptionName,
    createEventSubscription_snsTopicArn,
    createEventSubscriptionResponse_eventSubscription,
    createEventSubscriptionResponse_httpStatus,

    -- ** CreateDBInstance
    createDBInstance_engineVersion,
    createDBInstance_dbSecurityGroups,
    createDBInstance_deletionProtection,
    createDBInstance_storageEncrypted,
    createDBInstance_dbClusterIdentifier,
    createDBInstance_masterUserPassword,
    createDBInstance_publiclyAccessible,
    createDBInstance_autoMinorVersionUpgrade,
    createDBInstance_masterUsername,
    createDBInstance_dbSubnetGroupName,
    createDBInstance_monitoringRoleArn,
    createDBInstance_iops,
    createDBInstance_domain,
    createDBInstance_monitoringInterval,
    createDBInstance_tdeCredentialPassword,
    createDBInstance_promotionTier,
    createDBInstance_licenseModel,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_characterSetName,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_kmsKeyId,
    createDBInstance_dbParameterGroupName,
    createDBInstance_preferredBackupWindow,
    createDBInstance_availabilityZone,
    createDBInstance_backupRetentionPeriod,
    createDBInstance_performanceInsightsKMSKeyId,
    createDBInstance_vpcSecurityGroupIds,
    createDBInstance_multiAZ,
    createDBInstance_allocatedStorage,
    createDBInstance_optionGroupName,
    createDBInstance_copyTagsToSnapshot,
    createDBInstance_timezone,
    createDBInstance_tdeCredentialArn,
    createDBInstance_domainIAMRoleName,
    createDBInstance_tags,
    createDBInstance_port,
    createDBInstance_enableIAMDatabaseAuthentication,
    createDBInstance_storageType,
    createDBInstance_enableCloudwatchLogsExports,
    createDBInstance_dbName,
    createDBInstance_dbInstanceIdentifier,
    createDBInstance_dbInstanceClass,
    createDBInstance_engine,
    createDBInstanceResponse_dbInstance,
    createDBInstanceResponse_httpStatus,

    -- ** DeleteDBClusterParameterGroup
    deleteDBClusterParameterGroup_dbClusterParameterGroupName,

    -- ** CreateDBClusterEndpoint
    createDBClusterEndpoint_staticMembers,
    createDBClusterEndpoint_excludedMembers,
    createDBClusterEndpoint_tags,
    createDBClusterEndpoint_dbClusterIdentifier,
    createDBClusterEndpoint_dbClusterEndpointIdentifier,
    createDBClusterEndpoint_endpointType,
    createDBClusterEndpointResponse_status,
    createDBClusterEndpointResponse_dbClusterIdentifier,
    createDBClusterEndpointResponse_dbClusterEndpointArn,
    createDBClusterEndpointResponse_customEndpointType,
    createDBClusterEndpointResponse_staticMembers,
    createDBClusterEndpointResponse_endpointType,
    createDBClusterEndpointResponse_dbClusterEndpointIdentifier,
    createDBClusterEndpointResponse_endpoint,
    createDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier,
    createDBClusterEndpointResponse_excludedMembers,
    createDBClusterEndpointResponse_httpStatus,

    -- ** RestoreDBClusterFromSnapshot
    restoreDBClusterFromSnapshot_engineVersion,
    restoreDBClusterFromSnapshot_deletionProtection,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_databaseName,
    restoreDBClusterFromSnapshot_dbClusterParameterGroupName,
    restoreDBClusterFromSnapshot_optionGroupName,
    restoreDBClusterFromSnapshot_copyTagsToSnapshot,
    restoreDBClusterFromSnapshot_tags,
    restoreDBClusterFromSnapshot_port,
    restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication,
    restoreDBClusterFromSnapshot_enableCloudwatchLogsExports,
    restoreDBClusterFromSnapshot_dbClusterIdentifier,
    restoreDBClusterFromSnapshot_snapshotIdentifier,
    restoreDBClusterFromSnapshot_engine,
    restoreDBClusterFromSnapshotResponse_dbCluster,
    restoreDBClusterFromSnapshotResponse_httpStatus,

    -- ** DescribeOrderableDBInstanceOptions
    describeOrderableDBInstanceOptions_engineVersion,
    describeOrderableDBInstanceOptions_filters,
    describeOrderableDBInstanceOptions_dbInstanceClass,
    describeOrderableDBInstanceOptions_licenseModel,
    describeOrderableDBInstanceOptions_marker,
    describeOrderableDBInstanceOptions_maxRecords,
    describeOrderableDBInstanceOptions_vpc,
    describeOrderableDBInstanceOptions_engine,
    describeOrderableDBInstanceOptionsResponse_orderableDBInstanceOptions,
    describeOrderableDBInstanceOptionsResponse_marker,
    describeOrderableDBInstanceOptionsResponse_httpStatus,

    -- ** DeleteDBClusterEndpoint
    deleteDBClusterEndpoint_dbClusterEndpointIdentifier,
    deleteDBClusterEndpointResponse_status,
    deleteDBClusterEndpointResponse_dbClusterIdentifier,
    deleteDBClusterEndpointResponse_dbClusterEndpointArn,
    deleteDBClusterEndpointResponse_customEndpointType,
    deleteDBClusterEndpointResponse_staticMembers,
    deleteDBClusterEndpointResponse_endpointType,
    deleteDBClusterEndpointResponse_dbClusterEndpointIdentifier,
    deleteDBClusterEndpointResponse_endpoint,
    deleteDBClusterEndpointResponse_dbClusterEndpointResourceIdentifier,
    deleteDBClusterEndpointResponse_excludedMembers,
    deleteDBClusterEndpointResponse_httpStatus,

    -- ** CreateDBClusterParameterGroup
    createDBClusterParameterGroup_tags,
    createDBClusterParameterGroup_dbClusterParameterGroupName,
    createDBClusterParameterGroup_dbParameterGroupFamily,
    createDBClusterParameterGroup_description,
    createDBClusterParameterGroupResponse_dbClusterParameterGroup,
    createDBClusterParameterGroupResponse_httpStatus,

    -- ** DeleteEventSubscription
    deleteEventSubscription_subscriptionName,
    deleteEventSubscriptionResponse_eventSubscription,
    deleteEventSubscriptionResponse_httpStatus,

    -- ** DescribeDBParameterGroups
    describeDBParameterGroups_filters,
    describeDBParameterGroups_dbParameterGroupName,
    describeDBParameterGroups_marker,
    describeDBParameterGroups_maxRecords,
    describeDBParameterGroupsResponse_marker,
    describeDBParameterGroupsResponse_dbParameterGroups,
    describeDBParameterGroupsResponse_httpStatus,

    -- ** DeleteDBClusterSnapshot
    deleteDBClusterSnapshot_dbClusterSnapshotIdentifier,
    deleteDBClusterSnapshotResponse_dbClusterSnapshot,
    deleteDBClusterSnapshotResponse_httpStatus,

    -- ** DescribeValidDBInstanceModifications
    describeValidDBInstanceModifications_dbInstanceIdentifier,
    describeValidDBInstanceModificationsResponse_validDBInstanceModificationsMessage,
    describeValidDBInstanceModificationsResponse_httpStatus,

    -- ** DescribeDBClusterEndpoints
    describeDBClusterEndpoints_dbClusterIdentifier,
    describeDBClusterEndpoints_filters,
    describeDBClusterEndpoints_dbClusterEndpointIdentifier,
    describeDBClusterEndpoints_marker,
    describeDBClusterEndpoints_maxRecords,
    describeDBClusterEndpointsResponse_dbClusterEndpoints,
    describeDBClusterEndpointsResponse_marker,
    describeDBClusterEndpointsResponse_httpStatus,

    -- ** DescribeEventSubscriptions
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptions_filters,
    describeEventSubscriptions_marker,
    describeEventSubscriptions_maxRecords,
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceName,
    addTagsToResource_tags,

    -- ** DescribeDBParameters
    describeDBParameters_filters,
    describeDBParameters_marker,
    describeDBParameters_maxRecords,
    describeDBParameters_source,
    describeDBParameters_dbParameterGroupName,
    describeDBParametersResponse_marker,
    describeDBParametersResponse_parameters,
    describeDBParametersResponse_httpStatus,

    -- ** CreateDBClusterSnapshot
    createDBClusterSnapshot_tags,
    createDBClusterSnapshot_dbClusterSnapshotIdentifier,
    createDBClusterSnapshot_dbClusterIdentifier,
    createDBClusterSnapshotResponse_dbClusterSnapshot,
    createDBClusterSnapshotResponse_httpStatus,

    -- ** DescribeDBSubnetGroups
    describeDBSubnetGroups_dbSubnetGroupName,
    describeDBSubnetGroups_filters,
    describeDBSubnetGroups_marker,
    describeDBSubnetGroups_maxRecords,
    describeDBSubnetGroupsResponse_dbSubnetGroups,
    describeDBSubnetGroupsResponse_marker,
    describeDBSubnetGroupsResponse_httpStatus,

    -- ** StopDBCluster
    stopDBCluster_dbClusterIdentifier,
    stopDBClusterResponse_dbCluster,
    stopDBClusterResponse_httpStatus,

    -- ** CreateDBParameterGroup
    createDBParameterGroup_tags,
    createDBParameterGroup_dbParameterGroupName,
    createDBParameterGroup_dbParameterGroupFamily,
    createDBParameterGroup_description,
    createDBParameterGroupResponse_dbParameterGroup,
    createDBParameterGroupResponse_httpStatus,

    -- ** ModifyDBClusterSnapshotAttribute
    modifyDBClusterSnapshotAttribute_valuesToAdd,
    modifyDBClusterSnapshotAttribute_valuesToRemove,
    modifyDBClusterSnapshotAttribute_dbClusterSnapshotIdentifier,
    modifyDBClusterSnapshotAttribute_attributeName,
    modifyDBClusterSnapshotAttributeResponse_dbClusterSnapshotAttributesResult,
    modifyDBClusterSnapshotAttributeResponse_httpStatus,

    -- ** ModifyDBCluster
    modifyDBCluster_engineVersion,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_optionGroupName,
    modifyDBCluster_copyTagsToSnapshot,
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_port,
    modifyDBCluster_enableIAMDatabaseAuthentication,
    modifyDBCluster_dbClusterIdentifier,
    modifyDBClusterResponse_dbCluster,
    modifyDBClusterResponse_httpStatus,

    -- ** CopyDBClusterParameterGroup
    copyDBClusterParameterGroup_tags,
    copyDBClusterParameterGroup_sourceDBClusterParameterGroupIdentifier,
    copyDBClusterParameterGroup_targetDBClusterParameterGroupIdentifier,
    copyDBClusterParameterGroup_targetDBClusterParameterGroupDescription,
    copyDBClusterParameterGroupResponse_dbClusterParameterGroup,
    copyDBClusterParameterGroupResponse_httpStatus,

    -- ** DescribeEventCategories
    describeEventCategories_sourceType,
    describeEventCategories_filters,
    describeEventCategoriesResponse_eventCategoriesMapList,
    describeEventCategoriesResponse_httpStatus,

    -- ** ModifyDBClusterParameterGroup
    modifyDBClusterParameterGroup_dbClusterParameterGroupName,
    modifyDBClusterParameterGroup_parameters,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DescribeDBClusterSnapshotAttributes
    describeDBClusterSnapshotAttributes_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshotAttributesResponse_dbClusterSnapshotAttributesResult,
    describeDBClusterSnapshotAttributesResponse_httpStatus,

    -- ** DescribePendingMaintenanceActions
    describePendingMaintenanceActions_filters,
    describePendingMaintenanceActions_marker,
    describePendingMaintenanceActions_maxRecords,
    describePendingMaintenanceActions_resourceIdentifier,
    describePendingMaintenanceActionsResponse_pendingMaintenanceActions,
    describePendingMaintenanceActionsResponse_marker,
    describePendingMaintenanceActionsResponse_httpStatus,

    -- ** AddRoleToDBCluster
    addRoleToDBCluster_featureName,
    addRoleToDBCluster_dbClusterIdentifier,
    addRoleToDBCluster_roleArn,

    -- ** CopyDBClusterSnapshot
    copyDBClusterSnapshot_preSignedUrl,
    copyDBClusterSnapshot_copyTags,
    copyDBClusterSnapshot_kmsKeyId,
    copyDBClusterSnapshot_tags,
    copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier,
    copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier,
    copyDBClusterSnapshotResponse_dbClusterSnapshot,
    copyDBClusterSnapshotResponse_httpStatus,

    -- ** ResetDBParameterGroup
    resetDBParameterGroup_resetAllParameters,
    resetDBParameterGroup_parameters,
    resetDBParameterGroup_dbParameterGroupName,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** CreateDBCluster
    createDBCluster_engineVersion,
    createDBCluster_deletionProtection,
    createDBCluster_storageEncrypted,
    createDBCluster_masterUserPassword,
    createDBCluster_replicationSourceIdentifier,
    createDBCluster_masterUsername,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_preSignedUrl,
    createDBCluster_preferredMaintenanceWindow,
    createDBCluster_availabilityZones,
    createDBCluster_characterSetName,
    createDBCluster_kmsKeyId,
    createDBCluster_preferredBackupWindow,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_vpcSecurityGroupIds,
    createDBCluster_databaseName,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_optionGroupName,
    createDBCluster_copyTagsToSnapshot,
    createDBCluster_tags,
    createDBCluster_port,
    createDBCluster_enableIAMDatabaseAuthentication,
    createDBCluster_enableCloudwatchLogsExports,
    createDBCluster_dbClusterIdentifier,
    createDBCluster_engine,
    createDBClusterResponse_dbCluster,
    createDBClusterResponse_httpStatus,

    -- ** RemoveRoleFromDBCluster
    removeRoleFromDBCluster_featureName,
    removeRoleFromDBCluster_dbClusterIdentifier,
    removeRoleFromDBCluster_roleArn,

    -- ** FailoverDBCluster
    failoverDBCluster_dbClusterIdentifier,
    failoverDBCluster_targetDBInstanceIdentifier,
    failoverDBClusterResponse_dbCluster,
    failoverDBClusterResponse_httpStatus,

    -- ** ModifyDBParameterGroup
    modifyDBParameterGroup_dbParameterGroupName,
    modifyDBParameterGroup_parameters,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** ApplyPendingMaintenanceAction
    applyPendingMaintenanceAction_resourceIdentifier,
    applyPendingMaintenanceAction_applyAction,
    applyPendingMaintenanceAction_optInType,
    applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions,
    applyPendingMaintenanceActionResponse_httpStatus,

    -- ** DescribeDBClusterParameters
    describeDBClusterParameters_filters,
    describeDBClusterParameters_marker,
    describeDBClusterParameters_maxRecords,
    describeDBClusterParameters_source,
    describeDBClusterParameters_dbClusterParameterGroupName,
    describeDBClusterParametersResponse_marker,
    describeDBClusterParametersResponse_parameters,
    describeDBClusterParametersResponse_httpStatus,

    -- ** DeleteDBSubnetGroup
    deleteDBSubnetGroup_dbSubnetGroupName,

    -- ** DescribeDBClusterSnapshots
    describeDBClusterSnapshots_dbClusterIdentifier,
    describeDBClusterSnapshots_includeShared,
    describeDBClusterSnapshots_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshots_filters,
    describeDBClusterSnapshots_snapshotType,
    describeDBClusterSnapshots_marker,
    describeDBClusterSnapshots_maxRecords,
    describeDBClusterSnapshots_includePublic,
    describeDBClusterSnapshotsResponse_marker,
    describeDBClusterSnapshotsResponse_dbClusterSnapshots,
    describeDBClusterSnapshotsResponse_httpStatus,

    -- ** RebootDBInstance
    rebootDBInstance_forceFailover,
    rebootDBInstance_dbInstanceIdentifier,
    rebootDBInstanceResponse_dbInstance,
    rebootDBInstanceResponse_httpStatus,

    -- ** CreateDBSubnetGroup
    createDBSubnetGroup_tags,
    createDBSubnetGroup_dbSubnetGroupName,
    createDBSubnetGroup_dbSubnetGroupDescription,
    createDBSubnetGroup_subnetIds,
    createDBSubnetGroupResponse_dbSubnetGroup,
    createDBSubnetGroupResponse_httpStatus,

    -- ** DeleteDBInstance
    deleteDBInstance_finalDBSnapshotIdentifier,
    deleteDBInstance_skipFinalSnapshot,
    deleteDBInstance_dbInstanceIdentifier,
    deleteDBInstanceResponse_dbInstance,
    deleteDBInstanceResponse_httpStatus,

    -- ** DeleteDBParameterGroup
    deleteDBParameterGroup_dbParameterGroupName,

    -- ** RestoreDBClusterToPointInTime
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
    restoreDBClusterToPointInTime_dbClusterParameterGroupName,
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_optionGroupName,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_tags,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_dbClusterIdentifier,
    restoreDBClusterToPointInTime_sourceDBClusterIdentifier,
    restoreDBClusterToPointInTimeResponse_dbCluster,
    restoreDBClusterToPointInTimeResponse_httpStatus,

    -- ** DescribeDBInstances
    describeDBInstances_filters,
    describeDBInstances_dbInstanceIdentifier,
    describeDBInstances_marker,
    describeDBInstances_maxRecords,
    describeDBInstancesResponse_dbInstances,
    describeDBInstancesResponse_marker,
    describeDBInstancesResponse_httpStatus,

    -- * Types

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** CharacterSet
    characterSet_characterSetName,
    characterSet_characterSetDescription,

    -- ** CloudwatchLogsExportConfiguration
    cloudwatchLogsExportConfiguration_disableLogTypes,
    cloudwatchLogsExportConfiguration_enableLogTypes,

    -- ** DBCluster
    dbCluster_engineVersion,
    dbCluster_status,
    dbCluster_deletionProtection,
    dbCluster_automaticRestartTime,
    dbCluster_storageEncrypted,
    dbCluster_dbClusterIdentifier,
    dbCluster_dbClusterMembers,
    dbCluster_readReplicaIdentifiers,
    dbCluster_replicationSourceIdentifier,
    dbCluster_hostedZoneId,
    dbCluster_dbClusterParameterGroup,
    dbCluster_masterUsername,
    dbCluster_iAMDatabaseAuthenticationEnabled,
    dbCluster_dbClusterResourceId,
    dbCluster_earliestRestorableTime,
    dbCluster_engine,
    dbCluster_dbClusterArn,
    dbCluster_cloneGroupId,
    dbCluster_latestRestorableTime,
    dbCluster_crossAccountClone,
    dbCluster_preferredMaintenanceWindow,
    dbCluster_availabilityZones,
    dbCluster_characterSetName,
    dbCluster_kmsKeyId,
    dbCluster_preferredBackupWindow,
    dbCluster_associatedRoles,
    dbCluster_vpcSecurityGroups,
    dbCluster_backupRetentionPeriod,
    dbCluster_dbSubnetGroup,
    dbCluster_databaseName,
    dbCluster_multiAZ,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_allocatedStorage,
    dbCluster_copyTagsToSnapshot,
    dbCluster_clusterCreateTime,
    dbCluster_endpoint,
    dbCluster_percentProgress,
    dbCluster_readerEndpoint,
    dbCluster_port,
    dbCluster_dbClusterOptionGroupMemberships,

    -- ** DBClusterEndpoint
    dbClusterEndpoint_status,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_staticMembers,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_endpoint,
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_excludedMembers,

    -- ** DBClusterMember
    dbClusterMember_promotionTier,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_isClusterWriter,
    dbClusterMember_dbClusterParameterGroupStatus,

    -- ** DBClusterOptionGroupStatus
    dbClusterOptionGroupStatus_status,
    dbClusterOptionGroupStatus_dbClusterOptionGroupName,

    -- ** DBClusterParameterGroup
    dbClusterParameterGroup_dbClusterParameterGroupArn,
    dbClusterParameterGroup_dbParameterGroupFamily,
    dbClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroup_description,

    -- ** DBClusterParameterGroupNameMessage
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DBClusterRole
    dbClusterRole_status,
    dbClusterRole_featureName,
    dbClusterRole_roleArn,

    -- ** DBClusterSnapshot
    dbClusterSnapshot_engineVersion,
    dbClusterSnapshot_status,
    dbClusterSnapshot_storageEncrypted,
    dbClusterSnapshot_dbClusterIdentifier,
    dbClusterSnapshot_masterUsername,
    dbClusterSnapshot_iAMDatabaseAuthenticationEnabled,
    dbClusterSnapshot_dbClusterSnapshotArn,
    dbClusterSnapshot_vpcId,
    dbClusterSnapshot_dbClusterSnapshotIdentifier,
    dbClusterSnapshot_engine,
    dbClusterSnapshot_licenseModel,
    dbClusterSnapshot_availabilityZones,
    dbClusterSnapshot_snapshotType,
    dbClusterSnapshot_kmsKeyId,
    dbClusterSnapshot_snapshotCreateTime,
    dbClusterSnapshot_allocatedStorage,
    dbClusterSnapshot_sourceDBClusterSnapshotArn,
    dbClusterSnapshot_clusterCreateTime,
    dbClusterSnapshot_percentProgress,
    dbClusterSnapshot_port,

    -- ** DBClusterSnapshotAttribute
    dbClusterSnapshotAttribute_attributeValues,
    dbClusterSnapshotAttribute_attributeName,

    -- ** DBClusterSnapshotAttributesResult
    dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes,

    -- ** DBEngineVersion
    dbEngineVersion_engineVersion,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_validUpgradeTarget,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_exportableLogTypes,

    -- ** DBInstance
    dbInstance_engineVersion,
    dbInstance_dbSecurityGroups,
    dbInstance_deletionProtection,
    dbInstance_storageEncrypted,
    dbInstance_dbClusterIdentifier,
    dbInstance_publiclyAccessible,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_dbInstanceArn,
    dbInstance_masterUsername,
    dbInstance_readReplicaDBInstanceIdentifiers,
    dbInstance_iAMDatabaseAuthenticationEnabled,
    dbInstance_monitoringRoleArn,
    dbInstance_iops,
    dbInstance_instanceCreateTime,
    dbInstance_readReplicaSourceDBInstanceIdentifier,
    dbInstance_monitoringInterval,
    dbInstance_engine,
    dbInstance_latestRestorableTime,
    dbInstance_dbInstanceClass,
    dbInstance_promotionTier,
    dbInstance_licenseModel,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_cACertificateIdentifier,
    dbInstance_dbInstanceIdentifier,
    dbInstance_characterSetName,
    dbInstance_kmsKeyId,
    dbInstance_preferredBackupWindow,
    dbInstance_availabilityZone,
    dbInstance_vpcSecurityGroups,
    dbInstance_backupRetentionPeriod,
    dbInstance_performanceInsightsKMSKeyId,
    dbInstance_dbSubnetGroup,
    dbInstance_multiAZ,
    dbInstance_optionGroupMemberships,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_enhancedMonitoringResourceArn,
    dbInstance_secondaryAvailabilityZone,
    dbInstance_performanceInsightsEnabled,
    dbInstance_allocatedStorage,
    dbInstance_dbiResourceId,
    dbInstance_dbParameterGroups,
    dbInstance_copyTagsToSnapshot,
    dbInstance_timezone,
    dbInstance_tdeCredentialArn,
    dbInstance_endpoint,
    dbInstance_dbInstanceStatus,
    dbInstance_dbInstancePort,
    dbInstance_pendingModifiedValues,
    dbInstance_readReplicaDBClusterIdentifiers,
    dbInstance_storageType,
    dbInstance_statusInfos,
    dbInstance_domainMemberships,
    dbInstance_dbName,

    -- ** DBInstanceStatusInfo
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_statusType,
    dbInstanceStatusInfo_message,

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
    dbSecurityGroupMembership_status,
    dbSecurityGroupMembership_dbSecurityGroupName,

    -- ** DBSubnetGroup
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_vpcId,
    dbSubnetGroup_subnets,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_subnetGroupStatus,

    -- ** DomainMembership
    domainMembership_status,
    domainMembership_fqdn,
    domainMembership_domain,
    domainMembership_iAMRoleName,

    -- ** DoubleRange
    doubleRange_to,
    doubleRange_from,

    -- ** Endpoint
    endpoint_hostedZoneId,
    endpoint_address,
    endpoint_port,

    -- ** EngineDefaults
    engineDefaults_dbParameterGroupFamily,
    engineDefaults_marker,
    engineDefaults_parameters,

    -- ** Event
    event_sourceType,
    event_sourceArn,
    event_sourceIdentifier,
    event_date,
    event_eventCategories,
    event_message,

    -- ** EventCategoriesMap
    eventCategoriesMap_sourceType,
    eventCategoriesMap_eventCategories,

    -- ** EventSubscription
    eventSubscription_status,
    eventSubscription_customerAwsId,
    eventSubscription_custSubscriptionId,
    eventSubscription_snsTopicArn,
    eventSubscription_eventSubscriptionArn,
    eventSubscription_enabled,
    eventSubscription_sourceType,
    eventSubscription_subscriptionCreationTime,
    eventSubscription_eventCategoriesList,
    eventSubscription_sourceIdsList,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** OptionGroupMembership
    optionGroupMembership_status,
    optionGroupMembership_optionGroupName,

    -- ** OrderableDBInstanceOption
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_minIopsPerGib,
    orderableDBInstanceOption_supportsIAMDatabaseAuthentication,
    orderableDBInstanceOption_minIopsPerDbInstance,
    orderableDBInstanceOption_multiAZCapable,
    orderableDBInstanceOption_maxStorageSize,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_minStorageSize,
    orderableDBInstanceOption_supportsIops,
    orderableDBInstanceOption_supportsPerformanceInsights,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_licenseModel,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_supportsStorageEncryption,
    orderableDBInstanceOption_readReplicaCapable,
    orderableDBInstanceOption_maxIopsPerGib,
    orderableDBInstanceOption_vpc,
    orderableDBInstanceOption_supportsEnhancedMonitoring,
    orderableDBInstanceOption_maxIopsPerDbInstance,
    orderableDBInstanceOption_storageType,

    -- ** Parameter
    parameter_applyType,
    parameter_parameterValue,
    parameter_applyMethod,
    parameter_minimumEngineVersion,
    parameter_source,
    parameter_isModifiable,
    parameter_dataType,
    parameter_allowedValues,
    parameter_parameterName,
    parameter_description,

    -- ** PendingCloudwatchLogsExports
    pendingCloudwatchLogsExports_logTypesToEnable,
    pendingCloudwatchLogsExports_logTypesToDisable,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_currentApplyDate,

    -- ** PendingModifiedValues
    pendingModifiedValues_engineVersion,
    pendingModifiedValues_masterUserPassword,
    pendingModifiedValues_dbSubnetGroupName,
    pendingModifiedValues_iops,
    pendingModifiedValues_dbInstanceClass,
    pendingModifiedValues_licenseModel,
    pendingModifiedValues_cACertificateIdentifier,
    pendingModifiedValues_dbInstanceIdentifier,
    pendingModifiedValues_pendingCloudwatchLogsExports,
    pendingModifiedValues_backupRetentionPeriod,
    pendingModifiedValues_multiAZ,
    pendingModifiedValues_allocatedStorage,
    pendingModifiedValues_port,
    pendingModifiedValues_storageType,

    -- ** Range
    range_to,
    range_from,
    range_step,

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- ** Subnet
    subnet_subnetStatus,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** Timezone
    timezone_timezoneName,

    -- ** UpgradeTarget
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,
    upgradeTarget_engine,
    upgradeTarget_autoUpgrade,
    upgradeTarget_description,

    -- ** ValidDBInstanceModificationsMessage
    validDBInstanceModificationsMessage_storage,

    -- ** ValidStorageOptions
    validStorageOptions_storageSize,
    validStorageOptions_provisionedIops,
    validStorageOptions_iopsToStorageRatio,
    validStorageOptions_storageType,

    -- ** VpcSecurityGroupMembership
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import Network.AWS.Neptune.AddRoleToDBCluster
import Network.AWS.Neptune.AddSourceIdentifierToSubscription
import Network.AWS.Neptune.AddTagsToResource
import Network.AWS.Neptune.ApplyPendingMaintenanceAction
import Network.AWS.Neptune.CopyDBClusterParameterGroup
import Network.AWS.Neptune.CopyDBClusterSnapshot
import Network.AWS.Neptune.CopyDBParameterGroup
import Network.AWS.Neptune.CreateDBCluster
import Network.AWS.Neptune.CreateDBClusterEndpoint
import Network.AWS.Neptune.CreateDBClusterParameterGroup
import Network.AWS.Neptune.CreateDBClusterSnapshot
import Network.AWS.Neptune.CreateDBInstance
import Network.AWS.Neptune.CreateDBParameterGroup
import Network.AWS.Neptune.CreateDBSubnetGroup
import Network.AWS.Neptune.CreateEventSubscription
import Network.AWS.Neptune.DeleteDBCluster
import Network.AWS.Neptune.DeleteDBClusterEndpoint
import Network.AWS.Neptune.DeleteDBClusterParameterGroup
import Network.AWS.Neptune.DeleteDBClusterSnapshot
import Network.AWS.Neptune.DeleteDBInstance
import Network.AWS.Neptune.DeleteDBParameterGroup
import Network.AWS.Neptune.DeleteDBSubnetGroup
import Network.AWS.Neptune.DeleteEventSubscription
import Network.AWS.Neptune.DescribeDBClusterEndpoints
import Network.AWS.Neptune.DescribeDBClusterParameterGroups
import Network.AWS.Neptune.DescribeDBClusterParameters
import Network.AWS.Neptune.DescribeDBClusterSnapshotAttributes
import Network.AWS.Neptune.DescribeDBClusterSnapshots
import Network.AWS.Neptune.DescribeDBClusters
import Network.AWS.Neptune.DescribeDBEngineVersions
import Network.AWS.Neptune.DescribeDBInstances
import Network.AWS.Neptune.DescribeDBParameterGroups
import Network.AWS.Neptune.DescribeDBParameters
import Network.AWS.Neptune.DescribeDBSubnetGroups
import Network.AWS.Neptune.DescribeEngineDefaultClusterParameters
import Network.AWS.Neptune.DescribeEngineDefaultParameters
import Network.AWS.Neptune.DescribeEventCategories
import Network.AWS.Neptune.DescribeEventSubscriptions
import Network.AWS.Neptune.DescribeEvents
import Network.AWS.Neptune.DescribeOrderableDBInstanceOptions
import Network.AWS.Neptune.DescribePendingMaintenanceActions
import Network.AWS.Neptune.DescribeValidDBInstanceModifications
import Network.AWS.Neptune.FailoverDBCluster
import Network.AWS.Neptune.ListTagsForResource
import Network.AWS.Neptune.ModifyDBCluster
import Network.AWS.Neptune.ModifyDBClusterEndpoint
import Network.AWS.Neptune.ModifyDBClusterParameterGroup
import Network.AWS.Neptune.ModifyDBClusterSnapshotAttribute
import Network.AWS.Neptune.ModifyDBInstance
import Network.AWS.Neptune.ModifyDBParameterGroup
import Network.AWS.Neptune.ModifyDBSubnetGroup
import Network.AWS.Neptune.ModifyEventSubscription
import Network.AWS.Neptune.PromoteReadReplicaDBCluster
import Network.AWS.Neptune.RebootDBInstance
import Network.AWS.Neptune.RemoveRoleFromDBCluster
import Network.AWS.Neptune.RemoveSourceIdentifierFromSubscription
import Network.AWS.Neptune.RemoveTagsFromResource
import Network.AWS.Neptune.ResetDBClusterParameterGroup
import Network.AWS.Neptune.ResetDBParameterGroup
import Network.AWS.Neptune.RestoreDBClusterFromSnapshot
import Network.AWS.Neptune.RestoreDBClusterToPointInTime
import Network.AWS.Neptune.StartDBCluster
import Network.AWS.Neptune.StopDBCluster
import Network.AWS.Neptune.Types.AvailabilityZone
import Network.AWS.Neptune.Types.CharacterSet
import Network.AWS.Neptune.Types.CloudwatchLogsExportConfiguration
import Network.AWS.Neptune.Types.DBCluster
import Network.AWS.Neptune.Types.DBClusterEndpoint
import Network.AWS.Neptune.Types.DBClusterMember
import Network.AWS.Neptune.Types.DBClusterOptionGroupStatus
import Network.AWS.Neptune.Types.DBClusterParameterGroup
import Network.AWS.Neptune.Types.DBClusterParameterGroupNameMessage
import Network.AWS.Neptune.Types.DBClusterRole
import Network.AWS.Neptune.Types.DBClusterSnapshot
import Network.AWS.Neptune.Types.DBClusterSnapshotAttribute
import Network.AWS.Neptune.Types.DBClusterSnapshotAttributesResult
import Network.AWS.Neptune.Types.DBEngineVersion
import Network.AWS.Neptune.Types.DBInstance
import Network.AWS.Neptune.Types.DBInstanceStatusInfo
import Network.AWS.Neptune.Types.DBParameterGroup
import Network.AWS.Neptune.Types.DBParameterGroupNameMessage
import Network.AWS.Neptune.Types.DBParameterGroupStatus
import Network.AWS.Neptune.Types.DBSecurityGroupMembership
import Network.AWS.Neptune.Types.DBSubnetGroup
import Network.AWS.Neptune.Types.DomainMembership
import Network.AWS.Neptune.Types.DoubleRange
import Network.AWS.Neptune.Types.Endpoint
import Network.AWS.Neptune.Types.EngineDefaults
import Network.AWS.Neptune.Types.Event
import Network.AWS.Neptune.Types.EventCategoriesMap
import Network.AWS.Neptune.Types.EventSubscription
import Network.AWS.Neptune.Types.Filter
import Network.AWS.Neptune.Types.OptionGroupMembership
import Network.AWS.Neptune.Types.OrderableDBInstanceOption
import Network.AWS.Neptune.Types.Parameter
import Network.AWS.Neptune.Types.PendingCloudwatchLogsExports
import Network.AWS.Neptune.Types.PendingMaintenanceAction
import Network.AWS.Neptune.Types.PendingModifiedValues
import Network.AWS.Neptune.Types.Range
import Network.AWS.Neptune.Types.ResourcePendingMaintenanceActions
import Network.AWS.Neptune.Types.Subnet
import Network.AWS.Neptune.Types.Tag
import Network.AWS.Neptune.Types.Timezone
import Network.AWS.Neptune.Types.UpgradeTarget
import Network.AWS.Neptune.Types.ValidDBInstanceModificationsMessage
import Network.AWS.Neptune.Types.ValidStorageOptions
import Network.AWS.Neptune.Types.VpcSecurityGroupMembership
