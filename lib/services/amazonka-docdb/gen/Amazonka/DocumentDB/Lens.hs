{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DocumentDB.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    copyDBClusterSnapshot_tags,
    copyDBClusterSnapshot_copyTags,
    copyDBClusterSnapshot_kmsKeyId,
    copyDBClusterSnapshot_preSignedUrl,
    copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier,
    copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier,
    copyDBClusterSnapshotResponse_dbClusterSnapshot,
    copyDBClusterSnapshotResponse_httpStatus,

    -- ** CreateDBCluster
    createDBCluster_tags,
    createDBCluster_port,
    createDBCluster_vpcSecurityGroupIds,
    createDBCluster_preferredBackupWindow,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_masterUsername,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_availabilityZones,
    createDBCluster_masterUserPassword,
    createDBCluster_enableCloudwatchLogsExports,
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
    createDBInstance_copyTagsToSnapshot,
    createDBInstance_promotionTier,
    createDBInstance_autoMinorVersionUpgrade,
    createDBInstance_performanceInsightsKMSKeyId,
    createDBInstance_availabilityZone,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_preferredMaintenanceWindow,
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
    createGlobalCluster_databaseName,
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
    describeCertificates_marker,
    describeCertificates_filters,
    describeCertificates_maxRecords,
    describeCertificates_certificateIdentifier,
    describeCertificatesResponse_marker,
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_httpStatus,

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
    describeGlobalClusters_filters,
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
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_port,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_engineVersion,
    modifyDBCluster_dbClusterIdentifier,
    modifyDBClusterResponse_dbCluster,
    modifyDBClusterResponse_httpStatus,

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
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_copyTagsToSnapshot,
    modifyDBInstance_promotionTier,
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_applyImmediately,
    modifyDBInstance_performanceInsightsKMSKeyId,
    modifyDBInstance_enablePerformanceInsights,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_preferredMaintenanceWindow,
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
    modifyEventSubscription_sourceType,
    modifyEventSubscription_enabled,
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_eventCategories,
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
    resetDBClusterParameterGroup_resetAllParameters,
    resetDBClusterParameterGroup_parameters,
    resetDBClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** RestoreDBClusterFromSnapshot
    restoreDBClusterFromSnapshot_tags,
    restoreDBClusterFromSnapshot_port,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_enableCloudwatchLogsExports,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_deletionProtection,
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
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_deletionProtection,
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
    certificate_thumbprint,
    certificate_validTill,
    certificate_validFrom,
    certificate_certificateIdentifier,
    certificate_certificateArn,
    certificate_certificateType,

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
    dbCluster_masterUsername,
    dbCluster_dbClusterMembers,
    dbCluster_dbClusterParameterGroup,
    dbCluster_latestRestorableTime,
    dbCluster_dbClusterIdentifier,
    dbCluster_availabilityZones,
    dbCluster_dbSubnetGroup,
    dbCluster_status,
    dbCluster_replicationSourceIdentifier,
    dbCluster_storageEncrypted,
    dbCluster_kmsKeyId,
    dbCluster_engine,
    dbCluster_readerEndpoint,
    dbCluster_earliestRestorableTime,
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

    -- ** DBClusterMember
    dbClusterMember_promotionTier,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_dbClusterParameterGroupStatus,
    dbClusterMember_isClusterWriter,

    -- ** DBClusterParameterGroup
    dbClusterParameterGroup_description,
    dbClusterParameterGroup_dbClusterParameterGroupArn,
    dbClusterParameterGroup_dbParameterGroupFamily,
    dbClusterParameterGroup_dbClusterParameterGroupName,

    -- ** DBClusterParameterGroupNameMessage
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DBClusterRole
    dbClusterRole_roleArn,
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
    dbClusterSnapshot_vpcId,
    dbClusterSnapshot_clusterCreateTime,
    dbClusterSnapshot_dbClusterSnapshotArn,
    dbClusterSnapshot_engineVersion,
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
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_engineVersion,
    dbEngineVersion_dbEngineDescription,

    -- ** DBInstance
    dbInstance_dbInstanceStatus,
    dbInstance_preferredBackupWindow,
    dbInstance_backupRetentionPeriod,
    dbInstance_dbInstanceClass,
    dbInstance_copyTagsToSnapshot,
    dbInstance_promotionTier,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_dbInstanceIdentifier,
    dbInstance_latestRestorableTime,
    dbInstance_dbClusterIdentifier,
    dbInstance_dbSubnetGroup,
    dbInstance_instanceCreateTime,
    dbInstance_availabilityZone,
    dbInstance_publiclyAccessible,
    dbInstance_dbInstanceArn,
    dbInstance_cACertificateIdentifier,
    dbInstance_storageEncrypted,
    dbInstance_kmsKeyId,
    dbInstance_engine,
    dbInstance_pendingModifiedValues,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_endpoint,
    dbInstance_dbiResourceId,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_engineVersion,
    dbInstance_statusInfos,
    dbInstance_vpcSecurityGroups,

    -- ** DBInstanceStatusInfo
    dbInstanceStatusInfo_message,
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_statusType,

    -- ** DBSubnetGroup
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_subnetGroupStatus,
    dbSubnetGroup_subnets,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_vpcId,

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
    globalCluster_databaseName,
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

    -- ** OrderableDBInstanceOption
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_vpc,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_engineVersion,
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
