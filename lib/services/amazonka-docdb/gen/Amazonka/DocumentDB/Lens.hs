{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DocumentDB.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Lens
  ( -- * Operations

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

    -- ** CreateDBCluster
    createDBCluster_availabilityZones,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_deletionProtection,
    createDBCluster_enableCloudwatchLogsExports,
    createDBCluster_engineVersion,
    createDBCluster_globalClusterIdentifier,
    createDBCluster_kmsKeyId,
    createDBCluster_masterUserPassword,
    createDBCluster_masterUsername,
    createDBCluster_port,
    createDBCluster_preSignedUrl,
    createDBCluster_preferredBackupWindow,
    createDBCluster_preferredMaintenanceWindow,
    createDBCluster_storageEncrypted,
    createDBCluster_tags,
    createDBCluster_vpcSecurityGroupIds,
    createDBCluster_dbClusterIdentifier,
    createDBCluster_engine,
    createDBClusterResponse_dbCluster,
    createDBClusterResponse_httpStatus,

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
    createDBInstance_autoMinorVersionUpgrade,
    createDBInstance_availabilityZone,
    createDBInstance_copyTagsToSnapshot,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_performanceInsightsKMSKeyId,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_promotionTier,
    createDBInstance_tags,
    createDBInstance_dbInstanceIdentifier,
    createDBInstance_dbInstanceClass,
    createDBInstance_engine,
    createDBInstance_dbClusterIdentifier,
    createDBInstanceResponse_dbInstance,
    createDBInstanceResponse_httpStatus,

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
    createGlobalCluster_databaseName,
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

    -- ** DeleteDBClusterParameterGroup
    deleteDBClusterParameterGroup_dbClusterParameterGroupName,

    -- ** DeleteDBClusterSnapshot
    deleteDBClusterSnapshot_dbClusterSnapshotIdentifier,
    deleteDBClusterSnapshotResponse_dbClusterSnapshot,
    deleteDBClusterSnapshotResponse_httpStatus,

    -- ** DeleteDBInstance
    deleteDBInstance_dbInstanceIdentifier,
    deleteDBInstanceResponse_dbInstance,
    deleteDBInstanceResponse_httpStatus,

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

    -- ** DescribeCertificates
    describeCertificates_certificateIdentifier,
    describeCertificates_filters,
    describeCertificates_marker,
    describeCertificates_maxRecords,
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_marker,
    describeCertificatesResponse_httpStatus,

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
    describeGlobalClusters_filters,
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

    -- ** FailoverDBCluster
    failoverDBCluster_dbClusterIdentifier,
    failoverDBCluster_targetDBInstanceIdentifier,
    failoverDBClusterResponse_dbCluster,
    failoverDBClusterResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_filters,
    listTagsForResource_resourceName,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** ModifyDBCluster
    modifyDBCluster_applyImmediately,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_engineVersion,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_port,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_dbClusterIdentifier,
    modifyDBClusterResponse_dbCluster,
    modifyDBClusterResponse_httpStatus,

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
    modifyDBInstance_applyImmediately,
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_copyTagsToSnapshot,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_enablePerformanceInsights,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_performanceInsightsKMSKeyId,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_promotionTier,
    modifyDBInstance_dbInstanceIdentifier,
    modifyDBInstanceResponse_dbInstance,
    modifyDBInstanceResponse_httpStatus,

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
    modifyGlobalCluster_deletionProtection,
    modifyGlobalCluster_newGlobalClusterIdentifier,
    modifyGlobalCluster_globalClusterIdentifier,
    modifyGlobalClusterResponse_globalCluster,
    modifyGlobalClusterResponse_httpStatus,

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

    -- ** RestoreDBClusterFromSnapshot
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_deletionProtection,
    restoreDBClusterFromSnapshot_enableCloudwatchLogsExports,
    restoreDBClusterFromSnapshot_engineVersion,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_port,
    restoreDBClusterFromSnapshot_tags,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_dbClusterIdentifier,
    restoreDBClusterFromSnapshot_snapshotIdentifier,
    restoreDBClusterFromSnapshot_engine,
    restoreDBClusterFromSnapshotResponse_dbCluster,
    restoreDBClusterFromSnapshotResponse_httpStatus,

    -- ** RestoreDBClusterToPointInTime
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_restoreType,
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

    -- ** Certificate
    certificate_certificateArn,
    certificate_certificateIdentifier,
    certificate_certificateType,
    certificate_thumbprint,
    certificate_validFrom,
    certificate_validTill,

    -- ** CloudwatchLogsExportConfiguration
    cloudwatchLogsExportConfiguration_disableLogTypes,
    cloudwatchLogsExportConfiguration_enableLogTypes,

    -- ** DBCluster
    dbCluster_associatedRoles,
    dbCluster_availabilityZones,
    dbCluster_backupRetentionPeriod,
    dbCluster_cloneGroupId,
    dbCluster_clusterCreateTime,
    dbCluster_dbClusterArn,
    dbCluster_dbClusterIdentifier,
    dbCluster_dbClusterMembers,
    dbCluster_dbClusterParameterGroup,
    dbCluster_dbSubnetGroup,
    dbCluster_dbClusterResourceId,
    dbCluster_deletionProtection,
    dbCluster_earliestRestorableTime,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_endpoint,
    dbCluster_engine,
    dbCluster_engineVersion,
    dbCluster_hostedZoneId,
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
    dbCluster_status,
    dbCluster_storageEncrypted,
    dbCluster_vpcSecurityGroups,

    -- ** DBClusterMember
    dbClusterMember_dbClusterParameterGroupStatus,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_isClusterWriter,
    dbClusterMember_promotionTier,

    -- ** DBClusterParameterGroup
    dbClusterParameterGroup_dbClusterParameterGroupArn,
    dbClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroup_dbParameterGroupFamily,
    dbClusterParameterGroup_description,

    -- ** DBClusterParameterGroupNameMessage
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DBClusterRole
    dbClusterRole_roleArn,
    dbClusterRole_status,

    -- ** DBClusterSnapshot
    dbClusterSnapshot_availabilityZones,
    dbClusterSnapshot_clusterCreateTime,
    dbClusterSnapshot_dbClusterIdentifier,
    dbClusterSnapshot_dbClusterSnapshotArn,
    dbClusterSnapshot_dbClusterSnapshotIdentifier,
    dbClusterSnapshot_engine,
    dbClusterSnapshot_engineVersion,
    dbClusterSnapshot_kmsKeyId,
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
    dbEngineVersion_engine,
    dbEngineVersion_engineVersion,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_validUpgradeTarget,

    -- ** DBInstance
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_availabilityZone,
    dbInstance_backupRetentionPeriod,
    dbInstance_cACertificateIdentifier,
    dbInstance_copyTagsToSnapshot,
    dbInstance_dbClusterIdentifier,
    dbInstance_dbInstanceArn,
    dbInstance_dbInstanceClass,
    dbInstance_dbInstanceIdentifier,
    dbInstance_dbInstanceStatus,
    dbInstance_dbSubnetGroup,
    dbInstance_dbiResourceId,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_endpoint,
    dbInstance_engine,
    dbInstance_engineVersion,
    dbInstance_instanceCreateTime,
    dbInstance_kmsKeyId,
    dbInstance_latestRestorableTime,
    dbInstance_pendingModifiedValues,
    dbInstance_preferredBackupWindow,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_promotionTier,
    dbInstance_publiclyAccessible,
    dbInstance_statusInfos,
    dbInstance_storageEncrypted,
    dbInstance_vpcSecurityGroups,

    -- ** DBInstanceStatusInfo
    dbInstanceStatusInfo_message,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_statusType,

    -- ** DBSubnetGroup
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_subnetGroupStatus,
    dbSubnetGroup_subnets,
    dbSubnetGroup_vpcId,

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
    globalCluster_databaseName,
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

    -- ** OrderableDBInstanceOption
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_licenseModel,
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

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- ** Subnet
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,
    subnet_subnetStatus,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UpgradeTarget
    upgradeTarget_autoUpgrade,
    upgradeTarget_description,
    upgradeTarget_engine,
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,

    -- ** VpcSecurityGroupMembership
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import Amazonka.DocumentDB.AddSourceIdentifierToSubscription
import Amazonka.DocumentDB.AddTagsToResource
import Amazonka.DocumentDB.ApplyPendingMaintenanceAction
import Amazonka.DocumentDB.CopyDBClusterParameterGroup
import Amazonka.DocumentDB.CopyDBClusterSnapshot
import Amazonka.DocumentDB.CreateDBCluster
import Amazonka.DocumentDB.CreateDBClusterParameterGroup
import Amazonka.DocumentDB.CreateDBClusterSnapshot
import Amazonka.DocumentDB.CreateDBInstance
import Amazonka.DocumentDB.CreateDBSubnetGroup
import Amazonka.DocumentDB.CreateEventSubscription
import Amazonka.DocumentDB.CreateGlobalCluster
import Amazonka.DocumentDB.DeleteDBCluster
import Amazonka.DocumentDB.DeleteDBClusterParameterGroup
import Amazonka.DocumentDB.DeleteDBClusterSnapshot
import Amazonka.DocumentDB.DeleteDBInstance
import Amazonka.DocumentDB.DeleteDBSubnetGroup
import Amazonka.DocumentDB.DeleteEventSubscription
import Amazonka.DocumentDB.DeleteGlobalCluster
import Amazonka.DocumentDB.DescribeCertificates
import Amazonka.DocumentDB.DescribeDBClusterParameterGroups
import Amazonka.DocumentDB.DescribeDBClusterParameters
import Amazonka.DocumentDB.DescribeDBClusterSnapshotAttributes
import Amazonka.DocumentDB.DescribeDBClusterSnapshots
import Amazonka.DocumentDB.DescribeDBClusters
import Amazonka.DocumentDB.DescribeDBEngineVersions
import Amazonka.DocumentDB.DescribeDBInstances
import Amazonka.DocumentDB.DescribeDBSubnetGroups
import Amazonka.DocumentDB.DescribeEngineDefaultClusterParameters
import Amazonka.DocumentDB.DescribeEventCategories
import Amazonka.DocumentDB.DescribeEventSubscriptions
import Amazonka.DocumentDB.DescribeEvents
import Amazonka.DocumentDB.DescribeGlobalClusters
import Amazonka.DocumentDB.DescribeOrderableDBInstanceOptions
import Amazonka.DocumentDB.DescribePendingMaintenanceActions
import Amazonka.DocumentDB.FailoverDBCluster
import Amazonka.DocumentDB.ListTagsForResource
import Amazonka.DocumentDB.ModifyDBCluster
import Amazonka.DocumentDB.ModifyDBClusterParameterGroup
import Amazonka.DocumentDB.ModifyDBClusterSnapshotAttribute
import Amazonka.DocumentDB.ModifyDBInstance
import Amazonka.DocumentDB.ModifyDBSubnetGroup
import Amazonka.DocumentDB.ModifyEventSubscription
import Amazonka.DocumentDB.ModifyGlobalCluster
import Amazonka.DocumentDB.RebootDBInstance
import Amazonka.DocumentDB.RemoveFromGlobalCluster
import Amazonka.DocumentDB.RemoveSourceIdentifierFromSubscription
import Amazonka.DocumentDB.RemoveTagsFromResource
import Amazonka.DocumentDB.ResetDBClusterParameterGroup
import Amazonka.DocumentDB.RestoreDBClusterFromSnapshot
import Amazonka.DocumentDB.RestoreDBClusterToPointInTime
import Amazonka.DocumentDB.StartDBCluster
import Amazonka.DocumentDB.StopDBCluster
import Amazonka.DocumentDB.Types.AvailabilityZone
import Amazonka.DocumentDB.Types.Certificate
import Amazonka.DocumentDB.Types.CloudwatchLogsExportConfiguration
import Amazonka.DocumentDB.Types.DBCluster
import Amazonka.DocumentDB.Types.DBClusterMember
import Amazonka.DocumentDB.Types.DBClusterParameterGroup
import Amazonka.DocumentDB.Types.DBClusterParameterGroupNameMessage
import Amazonka.DocumentDB.Types.DBClusterRole
import Amazonka.DocumentDB.Types.DBClusterSnapshot
import Amazonka.DocumentDB.Types.DBClusterSnapshotAttribute
import Amazonka.DocumentDB.Types.DBClusterSnapshotAttributesResult
import Amazonka.DocumentDB.Types.DBEngineVersion
import Amazonka.DocumentDB.Types.DBInstance
import Amazonka.DocumentDB.Types.DBInstanceStatusInfo
import Amazonka.DocumentDB.Types.DBSubnetGroup
import Amazonka.DocumentDB.Types.Endpoint
import Amazonka.DocumentDB.Types.EngineDefaults
import Amazonka.DocumentDB.Types.Event
import Amazonka.DocumentDB.Types.EventCategoriesMap
import Amazonka.DocumentDB.Types.EventSubscription
import Amazonka.DocumentDB.Types.Filter
import Amazonka.DocumentDB.Types.GlobalCluster
import Amazonka.DocumentDB.Types.GlobalClusterMember
import Amazonka.DocumentDB.Types.OrderableDBInstanceOption
import Amazonka.DocumentDB.Types.Parameter
import Amazonka.DocumentDB.Types.PendingCloudwatchLogsExports
import Amazonka.DocumentDB.Types.PendingMaintenanceAction
import Amazonka.DocumentDB.Types.PendingModifiedValues
import Amazonka.DocumentDB.Types.ResourcePendingMaintenanceActions
import Amazonka.DocumentDB.Types.Subnet
import Amazonka.DocumentDB.Types.Tag
import Amazonka.DocumentDB.Types.UpgradeTarget
import Amazonka.DocumentDB.Types.VpcSecurityGroupMembership
