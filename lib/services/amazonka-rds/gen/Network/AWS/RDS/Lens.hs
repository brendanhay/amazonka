{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Lens
  ( -- * Operations

    -- ** DescribeExportTasks
    describeExportTasks_filters,
    describeExportTasks_sourceArn,
    describeExportTasks_exportTaskIdentifier,
    describeExportTasks_maxRecords,
    describeExportTasks_marker,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_marker,
    describeExportTasksResponse_httpStatus,

    -- ** ModifyDBInstance
    modifyDBInstance_deletionProtection,
    modifyDBInstance_preferredBackupWindow,
    modifyDBInstance_backupRetentionPeriod,
    modifyDBInstance_enablePerformanceInsights,
    modifyDBInstance_dbPortNumber,
    modifyDBInstance_maxAllocatedStorage,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_dbSecurityGroups,
    modifyDBInstance_enableIAMDatabaseAuthentication,
    modifyDBInstance_useDefaultProcessorFeatures,
    modifyDBInstance_storageType,
    modifyDBInstance_optionGroupName,
    modifyDBInstance_monitoringInterval,
    modifyDBInstance_domain,
    modifyDBInstance_allowMajorVersionUpgrade,
    modifyDBInstance_monitoringRoleArn,
    modifyDBInstance_dbSubnetGroupName,
    modifyDBInstance_masterUserPassword,
    modifyDBInstance_publiclyAccessible,
    modifyDBInstance_multiAZ,
    modifyDBInstance_performanceInsightsKMSKeyId,
    modifyDBInstance_vpcSecurityGroupIds,
    modifyDBInstance_dbParameterGroupName,
    modifyDBInstance_engineVersion,
    modifyDBInstance_performanceInsightsRetentionPeriod,
    modifyDBInstance_licenseModel,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_processorFeatures,
    modifyDBInstance_promotionTier,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_awsBackupRecoveryPointArn,
    modifyDBInstance_tdeCredentialPassword,
    modifyDBInstance_domainIAMRoleName,
    modifyDBInstance_certificateRotationRestart,
    modifyDBInstance_tdeCredentialArn,
    modifyDBInstance_replicaMode,
    modifyDBInstance_copyTagsToSnapshot,
    modifyDBInstance_enableCustomerOwnedIp,
    modifyDBInstance_cloudwatchLogsExportConfiguration,
    modifyDBInstance_allocatedStorage,
    modifyDBInstance_applyImmediately,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_iops,
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_dbInstanceIdentifier,
    modifyDBInstanceResponse_dbInstance,
    modifyDBInstanceResponse_httpStatus,

    -- ** CopyDBSnapshot
    copyDBSnapshot_destinationRegion,
    copyDBSnapshot_optionGroupName,
    copyDBSnapshot_kmsKeyId,
    copyDBSnapshot_copyTags,
    copyDBSnapshot_tags,
    copyDBSnapshot_preSignedUrl,
    copyDBSnapshot_targetCustomAvailabilityZone,
    copyDBSnapshot_sourceDBSnapshotIdentifier,
    copyDBSnapshot_targetDBSnapshotIdentifier,
    copyDBSnapshotResponse_dbSnapshot,
    copyDBSnapshotResponse_httpStatus,

    -- ** StopDBInstance
    stopDBInstance_dbSnapshotIdentifier,
    stopDBInstance_dbInstanceIdentifier,
    stopDBInstanceResponse_dbInstance,
    stopDBInstanceResponse_httpStatus,

    -- ** DescribeEventCategories
    describeEventCategories_filters,
    describeEventCategories_sourceType,
    describeEventCategoriesResponse_eventCategoriesMapList,
    describeEventCategoriesResponse_httpStatus,

    -- ** CopyDBClusterParameterGroup
    copyDBClusterParameterGroup_tags,
    copyDBClusterParameterGroup_sourceDBClusterParameterGroupIdentifier,
    copyDBClusterParameterGroup_targetDBClusterParameterGroupIdentifier,
    copyDBClusterParameterGroup_targetDBClusterParameterGroupDescription,
    copyDBClusterParameterGroupResponse_dbClusterParameterGroup,
    copyDBClusterParameterGroupResponse_httpStatus,

    -- ** ResetDBClusterParameterGroup
    resetDBClusterParameterGroup_resetAllParameters,
    resetDBClusterParameterGroup_parameters,
    resetDBClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DescribeDBProxyEndpoints
    describeDBProxyEndpoints_dbProxyEndpointName,
    describeDBProxyEndpoints_filters,
    describeDBProxyEndpoints_dbProxyName,
    describeDBProxyEndpoints_maxRecords,
    describeDBProxyEndpoints_marker,
    describeDBProxyEndpointsResponse_dbProxyEndpoints,
    describeDBProxyEndpointsResponse_marker,
    describeDBProxyEndpointsResponse_httpStatus,

    -- ** StartDBInstance
    startDBInstance_dbInstanceIdentifier,
    startDBInstanceResponse_dbInstance,
    startDBInstanceResponse_httpStatus,

    -- ** ModifyDBCluster
    modifyDBCluster_deletionProtection,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_enableIAMDatabaseAuthentication,
    modifyDBCluster_dbInstanceParameterGroupName,
    modifyDBCluster_optionGroupName,
    modifyDBCluster_domain,
    modifyDBCluster_allowMajorVersionUpgrade,
    modifyDBCluster_scalingConfiguration,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_enableHttpEndpoint,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_engineVersion,
    modifyDBCluster_enableGlobalWriteForwarding,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_port,
    modifyDBCluster_domainIAMRoleName,
    modifyDBCluster_copyTagsToSnapshot,
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_backtrackWindow,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_dbClusterIdentifier,
    modifyDBClusterResponse_dbCluster,
    modifyDBClusterResponse_httpStatus,

    -- ** CreateDBClusterSnapshot
    createDBClusterSnapshot_tags,
    createDBClusterSnapshot_dbClusterSnapshotIdentifier,
    createDBClusterSnapshot_dbClusterIdentifier,
    createDBClusterSnapshotResponse_dbClusterSnapshot,
    createDBClusterSnapshotResponse_httpStatus,

    -- ** DescribeDBClusterParameterGroups
    describeDBClusterParameterGroups_filters,
    describeDBClusterParameterGroups_dbClusterParameterGroupName,
    describeDBClusterParameterGroups_maxRecords,
    describeDBClusterParameterGroups_marker,
    describeDBClusterParameterGroupsResponse_dbClusterParameterGroups,
    describeDBClusterParameterGroupsResponse_marker,
    describeDBClusterParameterGroupsResponse_httpStatus,

    -- ** ModifyOptionGroup
    modifyOptionGroup_optionsToInclude,
    modifyOptionGroup_optionsToRemove,
    modifyOptionGroup_applyImmediately,
    modifyOptionGroup_optionGroupName,
    modifyOptionGroupResponse_optionGroup,
    modifyOptionGroupResponse_httpStatus,

    -- ** StartDBCluster
    startDBCluster_dbClusterIdentifier,
    startDBClusterResponse_dbCluster,
    startDBClusterResponse_httpStatus,

    -- ** DescribeDBSnapshots
    describeDBSnapshots_dbSnapshotIdentifier,
    describeDBSnapshots_dbiResourceId,
    describeDBSnapshots_includeShared,
    describeDBSnapshots_snapshotType,
    describeDBSnapshots_dbInstanceIdentifier,
    describeDBSnapshots_filters,
    describeDBSnapshots_includePublic,
    describeDBSnapshots_maxRecords,
    describeDBSnapshots_marker,
    describeDBSnapshotsResponse_dbSnapshots,
    describeDBSnapshotsResponse_marker,
    describeDBSnapshotsResponse_httpStatus,

    -- ** StopDBCluster
    stopDBCluster_dbClusterIdentifier,
    stopDBClusterResponse_dbCluster,
    stopDBClusterResponse_httpStatus,

    -- ** PromoteReadReplica
    promoteReadReplica_preferredBackupWindow,
    promoteReadReplica_backupRetentionPeriod,
    promoteReadReplica_dbInstanceIdentifier,
    promoteReadReplicaResponse_dbInstance,
    promoteReadReplicaResponse_httpStatus,

    -- ** DescribeDBEngineVersions
    describeDBEngineVersions_listSupportedTimezones,
    describeDBEngineVersions_defaultOnly,
    describeDBEngineVersions_engineVersion,
    describeDBEngineVersions_listSupportedCharacterSets,
    describeDBEngineVersions_engine,
    describeDBEngineVersions_dbParameterGroupFamily,
    describeDBEngineVersions_filters,
    describeDBEngineVersions_includeAll,
    describeDBEngineVersions_maxRecords,
    describeDBEngineVersions_marker,
    describeDBEngineVersionsResponse_dbEngineVersions,
    describeDBEngineVersionsResponse_marker,
    describeDBEngineVersionsResponse_httpStatus,

    -- ** ModifyGlobalCluster
    modifyGlobalCluster_deletionProtection,
    modifyGlobalCluster_allowMajorVersionUpgrade,
    modifyGlobalCluster_engineVersion,
    modifyGlobalCluster_newGlobalClusterIdentifier,
    modifyGlobalCluster_globalClusterIdentifier,
    modifyGlobalClusterResponse_globalCluster,
    modifyGlobalClusterResponse_httpStatus,

    -- ** CreateDBInstanceReadReplica
    createDBInstanceReadReplica_deletionProtection,
    createDBInstanceReadReplica_enablePerformanceInsights,
    createDBInstanceReadReplica_destinationRegion,
    createDBInstanceReadReplica_maxAllocatedStorage,
    createDBInstanceReadReplica_enableIAMDatabaseAuthentication,
    createDBInstanceReadReplica_useDefaultProcessorFeatures,
    createDBInstanceReadReplica_enableCloudwatchLogsExports,
    createDBInstanceReadReplica_storageType,
    createDBInstanceReadReplica_optionGroupName,
    createDBInstanceReadReplica_monitoringInterval,
    createDBInstanceReadReplica_domain,
    createDBInstanceReadReplica_monitoringRoleArn,
    createDBInstanceReadReplica_dbSubnetGroupName,
    createDBInstanceReadReplica_publiclyAccessible,
    createDBInstanceReadReplica_multiAZ,
    createDBInstanceReadReplica_performanceInsightsKMSKeyId,
    createDBInstanceReadReplica_vpcSecurityGroupIds,
    createDBInstanceReadReplica_kmsKeyId,
    createDBInstanceReadReplica_dbParameterGroupName,
    createDBInstanceReadReplica_availabilityZone,
    createDBInstanceReadReplica_performanceInsightsRetentionPeriod,
    createDBInstanceReadReplica_processorFeatures,
    createDBInstanceReadReplica_tags,
    createDBInstanceReadReplica_dbInstanceClass,
    createDBInstanceReadReplica_port,
    createDBInstanceReadReplica_domainIAMRoleName,
    createDBInstanceReadReplica_preSignedUrl,
    createDBInstanceReadReplica_replicaMode,
    createDBInstanceReadReplica_copyTagsToSnapshot,
    createDBInstanceReadReplica_iops,
    createDBInstanceReadReplica_autoMinorVersionUpgrade,
    createDBInstanceReadReplica_dbInstanceIdentifier,
    createDBInstanceReadReplica_sourceDBInstanceIdentifier,
    createDBInstanceReadReplicaResponse_dbInstance,
    createDBInstanceReadReplicaResponse_httpStatus,

    -- ** StartActivityStream
    startActivityStream_engineNativeAuditFieldsIncluded,
    startActivityStream_applyImmediately,
    startActivityStream_resourceArn,
    startActivityStream_mode,
    startActivityStream_kmsKeyId,
    startActivityStreamResponse_status,
    startActivityStreamResponse_mode,
    startActivityStreamResponse_kmsKeyId,
    startActivityStreamResponse_kinesisStreamName,
    startActivityStreamResponse_engineNativeAuditFieldsIncluded,
    startActivityStreamResponse_applyImmediately,
    startActivityStreamResponse_httpStatus,

    -- ** DescribeDBInstanceAutomatedBackups
    describeDBInstanceAutomatedBackups_dbInstanceAutomatedBackupsArn,
    describeDBInstanceAutomatedBackups_dbiResourceId,
    describeDBInstanceAutomatedBackups_dbInstanceIdentifier,
    describeDBInstanceAutomatedBackups_filters,
    describeDBInstanceAutomatedBackups_maxRecords,
    describeDBInstanceAutomatedBackups_marker,
    describeDBInstanceAutomatedBackupsResponse_dbInstanceAutomatedBackups,
    describeDBInstanceAutomatedBackupsResponse_marker,
    describeDBInstanceAutomatedBackupsResponse_httpStatus,

    -- ** DeleteInstallationMedia
    deleteInstallationMedia_installationMediaId,
    installationMedia_status,
    installationMedia_customAvailabilityZoneId,
    installationMedia_installationMediaId,
    installationMedia_engineVersion,
    installationMedia_failureCause,
    installationMedia_oSInstallationMediaPath,
    installationMedia_engine,
    installationMedia_engineInstallationMediaPath,

    -- ** DescribeOptionGroupOptions
    describeOptionGroupOptions_majorEngineVersion,
    describeOptionGroupOptions_filters,
    describeOptionGroupOptions_maxRecords,
    describeOptionGroupOptions_marker,
    describeOptionGroupOptions_engineName,
    describeOptionGroupOptionsResponse_optionGroupOptions,
    describeOptionGroupOptionsResponse_marker,
    describeOptionGroupOptionsResponse_httpStatus,

    -- ** RestoreDBInstanceFromS
    restoreDBInstanceFromS_deletionProtection,
    restoreDBInstanceFromS_preferredBackupWindow,
    restoreDBInstanceFromS_backupRetentionPeriod,
    restoreDBInstanceFromS_storageEncrypted,
    restoreDBInstanceFromS_enablePerformanceInsights,
    restoreDBInstanceFromS_maxAllocatedStorage,
    restoreDBInstanceFromS_dbSecurityGroups,
    restoreDBInstanceFromS_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromS_useDefaultProcessorFeatures,
    restoreDBInstanceFromS_enableCloudwatchLogsExports,
    restoreDBInstanceFromS_storageType,
    restoreDBInstanceFromS_optionGroupName,
    restoreDBInstanceFromS_monitoringInterval,
    restoreDBInstanceFromS_monitoringRoleArn,
    restoreDBInstanceFromS_dbSubnetGroupName,
    restoreDBInstanceFromS_masterUserPassword,
    restoreDBInstanceFromS_publiclyAccessible,
    restoreDBInstanceFromS_multiAZ,
    restoreDBInstanceFromS_masterUsername,
    restoreDBInstanceFromS_performanceInsightsKMSKeyId,
    restoreDBInstanceFromS_vpcSecurityGroupIds,
    restoreDBInstanceFromS_kmsKeyId,
    restoreDBInstanceFromS_dbParameterGroupName,
    restoreDBInstanceFromS_availabilityZone,
    restoreDBInstanceFromS_engineVersion,
    restoreDBInstanceFromS_performanceInsightsRetentionPeriod,
    restoreDBInstanceFromS_licenseModel,
    restoreDBInstanceFromS_preferredMaintenanceWindow,
    restoreDBInstanceFromS_processorFeatures,
    restoreDBInstanceFromS_tags,
    restoreDBInstanceFromS_dbName,
    restoreDBInstanceFromS_port,
    restoreDBInstanceFromS_copyTagsToSnapshot,
    restoreDBInstanceFromS_s3Prefix,
    restoreDBInstanceFromS_allocatedStorage,
    restoreDBInstanceFromS_iops,
    restoreDBInstanceFromS_autoMinorVersionUpgrade,
    restoreDBInstanceFromS_dbInstanceIdentifier,
    restoreDBInstanceFromS_dbInstanceClass,
    restoreDBInstanceFromS_engine,
    restoreDBInstanceFromS_sourceEngine,
    restoreDBInstanceFromS_sourceEngineVersion,
    restoreDBInstanceFromS_s3BucketName,
    restoreDBInstanceFromS_s3IngestionRoleArn,
    restoreDBInstanceFromSResponse_dbInstance,
    restoreDBInstanceFromSResponse_httpStatus,

    -- ** DescribeDBParameters
    describeDBParameters_source,
    describeDBParameters_filters,
    describeDBParameters_maxRecords,
    describeDBParameters_marker,
    describeDBParameters_dbParameterGroupName,
    describeDBParametersResponse_parameters,
    describeDBParametersResponse_marker,
    describeDBParametersResponse_httpStatus,

    -- ** CopyOptionGroup
    copyOptionGroup_tags,
    copyOptionGroup_sourceOptionGroupIdentifier,
    copyOptionGroup_targetOptionGroupIdentifier,
    copyOptionGroup_targetOptionGroupDescription,
    copyOptionGroupResponse_optionGroup,
    copyOptionGroupResponse_httpStatus,

    -- ** StopActivityStream
    stopActivityStream_applyImmediately,
    stopActivityStream_resourceArn,
    stopActivityStreamResponse_status,
    stopActivityStreamResponse_kmsKeyId,
    stopActivityStreamResponse_kinesisStreamName,
    stopActivityStreamResponse_httpStatus,

    -- ** DescribeValidDBInstanceModifications
    describeValidDBInstanceModifications_dbInstanceIdentifier,
    describeValidDBInstanceModificationsResponse_validDBInstanceModificationsMessage,
    describeValidDBInstanceModificationsResponse_httpStatus,

    -- ** StartExportTask
    startExportTask_exportOnly,
    startExportTask_s3Prefix,
    startExportTask_exportTaskIdentifier,
    startExportTask_sourceArn,
    startExportTask_s3BucketName,
    startExportTask_iamRoleArn,
    startExportTask_kmsKeyId,
    exportTask_taskEndTime,
    exportTask_status,
    exportTask_iamRoleArn,
    exportTask_totalExtractedDataInGB,
    exportTask_warningMessage,
    exportTask_snapshotTime,
    exportTask_s3Bucket,
    exportTask_exportOnly,
    exportTask_kmsKeyId,
    exportTask_failureCause,
    exportTask_percentProgress,
    exportTask_sourceArn,
    exportTask_s3Prefix,
    exportTask_taskStartTime,
    exportTask_exportTaskIdentifier,

    -- ** DescribeDBProxies
    describeDBProxies_filters,
    describeDBProxies_dbProxyName,
    describeDBProxies_maxRecords,
    describeDBProxies_marker,
    describeDBProxiesResponse_dbProxies,
    describeDBProxiesResponse_marker,
    describeDBProxiesResponse_httpStatus,

    -- ** RestoreDBClusterToPointInTime
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_optionGroupName,
    restoreDBClusterToPointInTime_domain,
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_scalingConfiguration,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_engineMode,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_tags,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_domainIAMRoleName,
    restoreDBClusterToPointInTime_copyTagsToSnapshot,
    restoreDBClusterToPointInTime_backtrackWindow,
    restoreDBClusterToPointInTime_dbClusterParameterGroupName,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_dbClusterIdentifier,
    restoreDBClusterToPointInTime_sourceDBClusterIdentifier,
    restoreDBClusterToPointInTimeResponse_dbCluster,
    restoreDBClusterToPointInTimeResponse_httpStatus,

    -- ** DescribeDBInstances
    describeDBInstances_dbInstanceIdentifier,
    describeDBInstances_filters,
    describeDBInstances_maxRecords,
    describeDBInstances_marker,
    describeDBInstancesResponse_dbInstances,
    describeDBInstancesResponse_marker,
    describeDBInstancesResponse_httpStatus,

    -- ** DescribeDBClusterEndpoints
    describeDBClusterEndpoints_dbClusterEndpointIdentifier,
    describeDBClusterEndpoints_dbClusterIdentifier,
    describeDBClusterEndpoints_filters,
    describeDBClusterEndpoints_maxRecords,
    describeDBClusterEndpoints_marker,
    describeDBClusterEndpointsResponse_dbClusterEndpoints,
    describeDBClusterEndpointsResponse_marker,
    describeDBClusterEndpointsResponse_httpStatus,

    -- ** DescribeEventSubscriptions
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptions_filters,
    describeEventSubscriptions_maxRecords,
    describeEventSubscriptions_marker,
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_httpStatus,

    -- ** DeleteEventSubscription
    deleteEventSubscription_subscriptionName,
    deleteEventSubscriptionResponse_eventSubscription,
    deleteEventSubscriptionResponse_httpStatus,

    -- ** DescribeReservedDBInstancesOfferings
    describeReservedDBInstancesOfferings_duration,
    describeReservedDBInstancesOfferings_multiAZ,
    describeReservedDBInstancesOfferings_dbInstanceClass,
    describeReservedDBInstancesOfferings_filters,
    describeReservedDBInstancesOfferings_offeringType,
    describeReservedDBInstancesOfferings_productDescription,
    describeReservedDBInstancesOfferings_reservedDBInstancesOfferingId,
    describeReservedDBInstancesOfferings_maxRecords,
    describeReservedDBInstancesOfferings_marker,
    describeReservedDBInstancesOfferingsResponse_reservedDBInstancesOfferings,
    describeReservedDBInstancesOfferingsResponse_marker,
    describeReservedDBInstancesOfferingsResponse_httpStatus,

    -- ** RebootDBInstance
    rebootDBInstance_forceFailover,
    rebootDBInstance_dbInstanceIdentifier,
    rebootDBInstanceResponse_dbInstance,
    rebootDBInstanceResponse_httpStatus,

    -- ** DescribeDBClusterBacktracks
    describeDBClusterBacktracks_backtrackIdentifier,
    describeDBClusterBacktracks_filters,
    describeDBClusterBacktracks_maxRecords,
    describeDBClusterBacktracks_marker,
    describeDBClusterBacktracks_dbClusterIdentifier,
    describeDBClusterBacktracksResponse_dbClusterBacktracks,
    describeDBClusterBacktracksResponse_marker,
    describeDBClusterBacktracksResponse_httpStatus,

    -- ** DescribeDBParameterGroups
    describeDBParameterGroups_dbParameterGroupName,
    describeDBParameterGroups_filters,
    describeDBParameterGroups_maxRecords,
    describeDBParameterGroups_marker,
    describeDBParameterGroupsResponse_dbParameterGroups,
    describeDBParameterGroupsResponse_marker,
    describeDBParameterGroupsResponse_httpStatus,

    -- ** CreateDBClusterParameterGroup
    createDBClusterParameterGroup_tags,
    createDBClusterParameterGroup_dbClusterParameterGroupName,
    createDBClusterParameterGroup_dbParameterGroupFamily,
    createDBClusterParameterGroup_description,
    createDBClusterParameterGroupResponse_dbClusterParameterGroup,
    createDBClusterParameterGroupResponse_httpStatus,

    -- ** DeleteDBInstance
    deleteDBInstance_skipFinalSnapshot,
    deleteDBInstance_finalDBSnapshotIdentifier,
    deleteDBInstance_deleteAutomatedBackups,
    deleteDBInstance_dbInstanceIdentifier,
    deleteDBInstanceResponse_dbInstance,
    deleteDBInstanceResponse_httpStatus,

    -- ** CreateDBProxy
    createDBProxy_idleClientTimeout,
    createDBProxy_debugLogging,
    createDBProxy_vpcSecurityGroupIds,
    createDBProxy_tags,
    createDBProxy_requireTLS,
    createDBProxy_dbProxyName,
    createDBProxy_engineFamily,
    createDBProxy_auth,
    createDBProxy_roleArn,
    createDBProxy_vpcSubnetIds,
    createDBProxyResponse_dbProxy,
    createDBProxyResponse_httpStatus,

    -- ** ModifyCertificates
    modifyCertificates_certificateIdentifier,
    modifyCertificates_removeCustomerOverride,
    modifyCertificatesResponse_certificate,
    modifyCertificatesResponse_httpStatus,

    -- ** DeleteDBInstanceAutomatedBackup
    deleteDBInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn,
    deleteDBInstanceAutomatedBackup_dbiResourceId,
    deleteDBInstanceAutomatedBackupResponse_dbInstanceAutomatedBackup,
    deleteDBInstanceAutomatedBackupResponse_httpStatus,

    -- ** DescribeDBClusterSnapshots
    describeDBClusterSnapshots_includeShared,
    describeDBClusterSnapshots_dbClusterIdentifier,
    describeDBClusterSnapshots_snapshotType,
    describeDBClusterSnapshots_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshots_filters,
    describeDBClusterSnapshots_includePublic,
    describeDBClusterSnapshots_maxRecords,
    describeDBClusterSnapshots_marker,
    describeDBClusterSnapshotsResponse_dbClusterSnapshots,
    describeDBClusterSnapshotsResponse_marker,
    describeDBClusterSnapshotsResponse_httpStatus,

    -- ** DeleteDBClusterEndpoint
    deleteDBClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_status,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_excludedMembers,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_endpoint,
    dbClusterEndpoint_staticMembers,

    -- ** ModifyDBSnapshotAttribute
    modifyDBSnapshotAttribute_valuesToRemove,
    modifyDBSnapshotAttribute_valuesToAdd,
    modifyDBSnapshotAttribute_dbSnapshotIdentifier,
    modifyDBSnapshotAttribute_attributeName,
    modifyDBSnapshotAttributeResponse_dbSnapshotAttributesResult,
    modifyDBSnapshotAttributeResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceName,
    removeTagsFromResource_tagKeys,

    -- ** CreateDBSecurityGroup
    createDBSecurityGroup_tags,
    createDBSecurityGroup_dbSecurityGroupName,
    createDBSecurityGroup_dbSecurityGroupDescription,
    createDBSecurityGroupResponse_dbSecurityGroup,
    createDBSecurityGroupResponse_httpStatus,

    -- ** DescribeCertificates
    describeCertificates_certificateIdentifier,
    describeCertificates_filters,
    describeCertificates_maxRecords,
    describeCertificates_marker,
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_marker,
    describeCertificatesResponse_httpStatus,

    -- ** CreateDBInstance
    createDBInstance_deletionProtection,
    createDBInstance_preferredBackupWindow,
    createDBInstance_backupRetentionPeriod,
    createDBInstance_storageEncrypted,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_maxAllocatedStorage,
    createDBInstance_dbSecurityGroups,
    createDBInstance_enableIAMDatabaseAuthentication,
    createDBInstance_enableCloudwatchLogsExports,
    createDBInstance_storageType,
    createDBInstance_optionGroupName,
    createDBInstance_monitoringInterval,
    createDBInstance_domain,
    createDBInstance_monitoringRoleArn,
    createDBInstance_dbSubnetGroupName,
    createDBInstance_masterUserPassword,
    createDBInstance_publiclyAccessible,
    createDBInstance_ncharCharacterSetName,
    createDBInstance_multiAZ,
    createDBInstance_masterUsername,
    createDBInstance_performanceInsightsKMSKeyId,
    createDBInstance_vpcSecurityGroupIds,
    createDBInstance_kmsKeyId,
    createDBInstance_dbParameterGroupName,
    createDBInstance_dbClusterIdentifier,
    createDBInstance_availabilityZone,
    createDBInstance_engineVersion,
    createDBInstance_performanceInsightsRetentionPeriod,
    createDBInstance_licenseModel,
    createDBInstance_characterSetName,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_processorFeatures,
    createDBInstance_tags,
    createDBInstance_dbName,
    createDBInstance_promotionTier,
    createDBInstance_port,
    createDBInstance_tdeCredentialPassword,
    createDBInstance_domainIAMRoleName,
    createDBInstance_tdeCredentialArn,
    createDBInstance_copyTagsToSnapshot,
    createDBInstance_timezone,
    createDBInstance_enableCustomerOwnedIp,
    createDBInstance_allocatedStorage,
    createDBInstance_iops,
    createDBInstance_autoMinorVersionUpgrade,
    createDBInstance_dbInstanceIdentifier,
    createDBInstance_dbInstanceClass,
    createDBInstance_engine,
    createDBInstanceResponse_dbInstance,
    createDBInstanceResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributesResponse_accountQuotas,
    describeAccountAttributesResponse_httpStatus,

    -- ** RemoveRoleFromDBInstance
    removeRoleFromDBInstance_dbInstanceIdentifier,
    removeRoleFromDBInstance_roleArn,
    removeRoleFromDBInstance_featureName,

    -- ** DescribeDBClusterParameters
    describeDBClusterParameters_source,
    describeDBClusterParameters_filters,
    describeDBClusterParameters_maxRecords,
    describeDBClusterParameters_marker,
    describeDBClusterParameters_dbClusterParameterGroupName,
    describeDBClusterParametersResponse_parameters,
    describeDBClusterParametersResponse_marker,
    describeDBClusterParametersResponse_httpStatus,

    -- ** DeleteDBSubnetGroup
    deleteDBSubnetGroup_dbSubnetGroupName,

    -- ** DescribeEngineDefaultClusterParameters
    describeEngineDefaultClusterParameters_filters,
    describeEngineDefaultClusterParameters_maxRecords,
    describeEngineDefaultClusterParameters_marker,
    describeEngineDefaultClusterParameters_dbParameterGroupFamily,
    describeEngineDefaultClusterParametersResponse_engineDefaults,
    describeEngineDefaultClusterParametersResponse_httpStatus,

    -- ** StopDBInstanceAutomatedBackupsReplication
    stopDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn,
    stopDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup,
    stopDBInstanceAutomatedBackupsReplicationResponse_httpStatus,

    -- ** BacktrackDBCluster
    backtrackDBCluster_force,
    backtrackDBCluster_useEarliestTimeOnPointInTimeUnavailable,
    backtrackDBCluster_dbClusterIdentifier,
    backtrackDBCluster_backtrackTo,
    dbClusterBacktrack_status,
    dbClusterBacktrack_backtrackTo,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_backtrackRequestCreationTime,
    dbClusterBacktrack_backtrackedFrom,

    -- ** RemoveRoleFromDBCluster
    removeRoleFromDBCluster_featureName,
    removeRoleFromDBCluster_dbClusterIdentifier,
    removeRoleFromDBCluster_roleArn,

    -- ** CreateDBCluster
    createDBCluster_deletionProtection,
    createDBCluster_preferredBackupWindow,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_storageEncrypted,
    createDBCluster_availabilityZones,
    createDBCluster_destinationRegion,
    createDBCluster_enableIAMDatabaseAuthentication,
    createDBCluster_enableCloudwatchLogsExports,
    createDBCluster_optionGroupName,
    createDBCluster_domain,
    createDBCluster_scalingConfiguration,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_engineMode,
    createDBCluster_masterUserPassword,
    createDBCluster_masterUsername,
    createDBCluster_enableHttpEndpoint,
    createDBCluster_vpcSecurityGroupIds,
    createDBCluster_kmsKeyId,
    createDBCluster_engineVersion,
    createDBCluster_enableGlobalWriteForwarding,
    createDBCluster_characterSetName,
    createDBCluster_preferredMaintenanceWindow,
    createDBCluster_tags,
    createDBCluster_port,
    createDBCluster_domainIAMRoleName,
    createDBCluster_preSignedUrl,
    createDBCluster_copyTagsToSnapshot,
    createDBCluster_backtrackWindow,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_globalClusterIdentifier,
    createDBCluster_replicationSourceIdentifier,
    createDBCluster_databaseName,
    createDBCluster_dbClusterIdentifier,
    createDBCluster_engine,
    createDBClusterResponse_dbCluster,
    createDBClusterResponse_httpStatus,

    -- ** ApplyPendingMaintenanceAction
    applyPendingMaintenanceAction_resourceIdentifier,
    applyPendingMaintenanceAction_applyAction,
    applyPendingMaintenanceAction_optInType,
    applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions,
    applyPendingMaintenanceActionResponse_httpStatus,

    -- ** FailoverDBCluster
    failoverDBCluster_targetDBInstanceIdentifier,
    failoverDBCluster_dbClusterIdentifier,
    failoverDBClusterResponse_dbCluster,
    failoverDBClusterResponse_httpStatus,

    -- ** StartDBInstanceAutomatedBackupsReplication
    startDBInstanceAutomatedBackupsReplication_backupRetentionPeriod,
    startDBInstanceAutomatedBackupsReplication_destinationRegion,
    startDBInstanceAutomatedBackupsReplication_kmsKeyId,
    startDBInstanceAutomatedBackupsReplication_preSignedUrl,
    startDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn,
    startDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup,
    startDBInstanceAutomatedBackupsReplicationResponse_httpStatus,

    -- ** CreateCustomAvailabilityZone
    createCustomAvailabilityZone_vpnTunnelOriginatorIP,
    createCustomAvailabilityZone_existingVpnId,
    createCustomAvailabilityZone_newVpnTunnelName,
    createCustomAvailabilityZone_customAvailabilityZoneName,
    createCustomAvailabilityZoneResponse_customAvailabilityZone,
    createCustomAvailabilityZoneResponse_httpStatus,

    -- ** DeleteDBProxyEndpoint
    deleteDBProxyEndpoint_dbProxyEndpointName,
    deleteDBProxyEndpointResponse_dbProxyEndpoint,
    deleteDBProxyEndpointResponse_httpStatus,

    -- ** DescribeDBSnapshotAttributes
    describeDBSnapshotAttributes_dbSnapshotIdentifier,
    describeDBSnapshotAttributesResponse_dbSnapshotAttributesResult,
    describeDBSnapshotAttributesResponse_httpStatus,

    -- ** CreateOptionGroup
    createOptionGroup_tags,
    createOptionGroup_optionGroupName,
    createOptionGroup_engineName,
    createOptionGroup_majorEngineVersion,
    createOptionGroup_optionGroupDescription,
    createOptionGroupResponse_optionGroup,
    createOptionGroupResponse_httpStatus,

    -- ** DeleteDBCluster
    deleteDBCluster_skipFinalSnapshot,
    deleteDBCluster_finalDBSnapshotIdentifier,
    deleteDBCluster_dbClusterIdentifier,
    deleteDBClusterResponse_dbCluster,
    deleteDBClusterResponse_httpStatus,

    -- ** CreateDBProxyEndpoint
    createDBProxyEndpoint_vpcSecurityGroupIds,
    createDBProxyEndpoint_tags,
    createDBProxyEndpoint_targetRole,
    createDBProxyEndpoint_dbProxyName,
    createDBProxyEndpoint_dbProxyEndpointName,
    createDBProxyEndpoint_vpcSubnetIds,
    createDBProxyEndpointResponse_dbProxyEndpoint,
    createDBProxyEndpointResponse_httpStatus,

    -- ** DescribeReservedDBInstances
    describeReservedDBInstances_duration,
    describeReservedDBInstances_multiAZ,
    describeReservedDBInstances_dbInstanceClass,
    describeReservedDBInstances_reservedDBInstanceId,
    describeReservedDBInstances_leaseId,
    describeReservedDBInstances_filters,
    describeReservedDBInstances_offeringType,
    describeReservedDBInstances_productDescription,
    describeReservedDBInstances_reservedDBInstancesOfferingId,
    describeReservedDBInstances_maxRecords,
    describeReservedDBInstances_marker,
    describeReservedDBInstancesResponse_reservedDBInstances,
    describeReservedDBInstancesResponse_marker,
    describeReservedDBInstancesResponse_httpStatus,

    -- ** DeleteOptionGroup
    deleteOptionGroup_optionGroupName,

    -- ** DescribeEvents
    describeEvents_duration,
    describeEvents_startTime,
    describeEvents_endTime,
    describeEvents_eventCategories,
    describeEvents_sourceIdentifier,
    describeEvents_filters,
    describeEvents_sourceType,
    describeEvents_maxRecords,
    describeEvents_marker,
    describeEventsResponse_events,
    describeEventsResponse_marker,
    describeEventsResponse_httpStatus,

    -- ** DescribeDBProxyTargetGroups
    describeDBProxyTargetGroups_targetGroupName,
    describeDBProxyTargetGroups_filters,
    describeDBProxyTargetGroups_maxRecords,
    describeDBProxyTargetGroups_marker,
    describeDBProxyTargetGroups_dbProxyName,
    describeDBProxyTargetGroupsResponse_targetGroups,
    describeDBProxyTargetGroupsResponse_marker,
    describeDBProxyTargetGroupsResponse_httpStatus,

    -- ** ModifyDBClusterParameterGroup
    modifyDBClusterParameterGroup_dbClusterParameterGroupName,
    modifyDBClusterParameterGroup_parameters,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DescribeDBLogFiles
    describeDBLogFiles_fileSize,
    describeDBLogFiles_filenameContains,
    describeDBLogFiles_fileLastWritten,
    describeDBLogFiles_filters,
    describeDBLogFiles_maxRecords,
    describeDBLogFiles_marker,
    describeDBLogFiles_dbInstanceIdentifier,
    describeDBLogFilesResponse_describeDBLogFiles,
    describeDBLogFilesResponse_marker,
    describeDBLogFilesResponse_httpStatus,

    -- ** DescribeOptionGroups
    describeOptionGroups_engineName,
    describeOptionGroups_optionGroupName,
    describeOptionGroups_majorEngineVersion,
    describeOptionGroups_filters,
    describeOptionGroups_maxRecords,
    describeOptionGroups_marker,
    describeOptionGroupsResponse_optionGroupsList,
    describeOptionGroupsResponse_marker,
    describeOptionGroupsResponse_httpStatus,

    -- ** DescribePendingMaintenanceActions
    describePendingMaintenanceActions_resourceIdentifier,
    describePendingMaintenanceActions_filters,
    describePendingMaintenanceActions_maxRecords,
    describePendingMaintenanceActions_marker,
    describePendingMaintenanceActionsResponse_pendingMaintenanceActions,
    describePendingMaintenanceActionsResponse_marker,
    describePendingMaintenanceActionsResponse_httpStatus,

    -- ** RestoreDBClusterFromS
    restoreDBClusterFromS_deletionProtection,
    restoreDBClusterFromS_preferredBackupWindow,
    restoreDBClusterFromS_backupRetentionPeriod,
    restoreDBClusterFromS_storageEncrypted,
    restoreDBClusterFromS_availabilityZones,
    restoreDBClusterFromS_enableIAMDatabaseAuthentication,
    restoreDBClusterFromS_enableCloudwatchLogsExports,
    restoreDBClusterFromS_optionGroupName,
    restoreDBClusterFromS_domain,
    restoreDBClusterFromS_dbSubnetGroupName,
    restoreDBClusterFromS_vpcSecurityGroupIds,
    restoreDBClusterFromS_kmsKeyId,
    restoreDBClusterFromS_engineVersion,
    restoreDBClusterFromS_characterSetName,
    restoreDBClusterFromS_preferredMaintenanceWindow,
    restoreDBClusterFromS_tags,
    restoreDBClusterFromS_port,
    restoreDBClusterFromS_domainIAMRoleName,
    restoreDBClusterFromS_copyTagsToSnapshot,
    restoreDBClusterFromS_s3Prefix,
    restoreDBClusterFromS_backtrackWindow,
    restoreDBClusterFromS_dbClusterParameterGroupName,
    restoreDBClusterFromS_databaseName,
    restoreDBClusterFromS_dbClusterIdentifier,
    restoreDBClusterFromS_engine,
    restoreDBClusterFromS_masterUsername,
    restoreDBClusterFromS_masterUserPassword,
    restoreDBClusterFromS_sourceEngine,
    restoreDBClusterFromS_sourceEngineVersion,
    restoreDBClusterFromS_s3BucketName,
    restoreDBClusterFromS_s3IngestionRoleArn,
    restoreDBClusterFromSResponse_dbCluster,
    restoreDBClusterFromSResponse_httpStatus,

    -- ** DescribeDBClusters
    describeDBClusters_includeShared,
    describeDBClusters_dbClusterIdentifier,
    describeDBClusters_filters,
    describeDBClusters_maxRecords,
    describeDBClusters_marker,
    describeDBClustersResponse_dbClusters,
    describeDBClustersResponse_marker,
    describeDBClustersResponse_httpStatus,

    -- ** DescribeDBClusterSnapshotAttributes
    describeDBClusterSnapshotAttributes_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshotAttributesResponse_dbClusterSnapshotAttributesResult,
    describeDBClusterSnapshotAttributesResponse_httpStatus,

    -- ** ModifyDBProxy
    modifyDBProxy_roleArn,
    modifyDBProxy_newDBProxyName,
    modifyDBProxy_idleClientTimeout,
    modifyDBProxy_auth,
    modifyDBProxy_debugLogging,
    modifyDBProxy_securityGroups,
    modifyDBProxy_requireTLS,
    modifyDBProxy_dbProxyName,
    modifyDBProxyResponse_dbProxy,
    modifyDBProxyResponse_httpStatus,

    -- ** DescribeGlobalClusters
    describeGlobalClusters_filters,
    describeGlobalClusters_globalClusterIdentifier,
    describeGlobalClusters_maxRecords,
    describeGlobalClusters_marker,
    describeGlobalClustersResponse_globalClusters,
    describeGlobalClustersResponse_marker,
    describeGlobalClustersResponse_httpStatus,

    -- ** AddSourceIdentifierToSubscription
    addSourceIdentifierToSubscription_subscriptionName,
    addSourceIdentifierToSubscription_sourceIdentifier,
    addSourceIdentifierToSubscriptionResponse_eventSubscription,
    addSourceIdentifierToSubscriptionResponse_httpStatus,

    -- ** ModifyDBClusterEndpoint
    modifyDBClusterEndpoint_excludedMembers,
    modifyDBClusterEndpoint_endpointType,
    modifyDBClusterEndpoint_staticMembers,
    modifyDBClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_status,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_excludedMembers,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_endpoint,
    dbClusterEndpoint_staticMembers,

    -- ** ModifyEventSubscription
    modifyEventSubscription_enabled,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_sourceType,
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_subscriptionName,
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,

    -- ** CancelExportTask
    cancelExportTask_exportTaskIdentifier,
    exportTask_taskEndTime,
    exportTask_status,
    exportTask_iamRoleArn,
    exportTask_totalExtractedDataInGB,
    exportTask_warningMessage,
    exportTask_snapshotTime,
    exportTask_s3Bucket,
    exportTask_exportOnly,
    exportTask_kmsKeyId,
    exportTask_failureCause,
    exportTask_percentProgress,
    exportTask_sourceArn,
    exportTask_s3Prefix,
    exportTask_taskStartTime,
    exportTask_exportTaskIdentifier,

    -- ** CreateDBParameterGroup
    createDBParameterGroup_tags,
    createDBParameterGroup_dbParameterGroupName,
    createDBParameterGroup_dbParameterGroupFamily,
    createDBParameterGroup_description,
    createDBParameterGroupResponse_dbParameterGroup,
    createDBParameterGroupResponse_httpStatus,

    -- ** ModifyDBClusterSnapshotAttribute
    modifyDBClusterSnapshotAttribute_valuesToRemove,
    modifyDBClusterSnapshotAttribute_valuesToAdd,
    modifyDBClusterSnapshotAttribute_dbClusterSnapshotIdentifier,
    modifyDBClusterSnapshotAttribute_attributeName,
    modifyDBClusterSnapshotAttributeResponse_dbClusterSnapshotAttributesResult,
    modifyDBClusterSnapshotAttributeResponse_httpStatus,

    -- ** ModifyDBProxyTargetGroup
    modifyDBProxyTargetGroup_connectionPoolConfig,
    modifyDBProxyTargetGroup_newName,
    modifyDBProxyTargetGroup_targetGroupName,
    modifyDBProxyTargetGroup_dbProxyName,
    modifyDBProxyTargetGroupResponse_dbProxyTargetGroup,
    modifyDBProxyTargetGroupResponse_httpStatus,

    -- ** DescribeDBSubnetGroups
    describeDBSubnetGroups_dbSubnetGroupName,
    describeDBSubnetGroups_filters,
    describeDBSubnetGroups_maxRecords,
    describeDBSubnetGroups_marker,
    describeDBSubnetGroupsResponse_dbSubnetGroups,
    describeDBSubnetGroupsResponse_marker,
    describeDBSubnetGroupsResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceName,
    addTagsToResource_tags,

    -- ** ModifyDBProxyEndpoint
    modifyDBProxyEndpoint_vpcSecurityGroupIds,
    modifyDBProxyEndpoint_newDBProxyEndpointName,
    modifyDBProxyEndpoint_dbProxyEndpointName,
    modifyDBProxyEndpointResponse_dbProxyEndpoint,
    modifyDBProxyEndpointResponse_httpStatus,

    -- ** AddRoleToDBInstance
    addRoleToDBInstance_dbInstanceIdentifier,
    addRoleToDBInstance_roleArn,
    addRoleToDBInstance_featureName,

    -- ** RegisterDBProxyTargets
    registerDBProxyTargets_dbClusterIdentifiers,
    registerDBProxyTargets_targetGroupName,
    registerDBProxyTargets_dbInstanceIdentifiers,
    registerDBProxyTargets_dbProxyName,
    registerDBProxyTargetsResponse_dbProxyTargets,
    registerDBProxyTargetsResponse_httpStatus,

    -- ** ModifyCurrentDBClusterCapacity
    modifyCurrentDBClusterCapacity_timeoutAction,
    modifyCurrentDBClusterCapacity_capacity,
    modifyCurrentDBClusterCapacity_secondsBeforeTimeout,
    modifyCurrentDBClusterCapacity_dbClusterIdentifier,
    modifyCurrentDBClusterCapacityResponse_pendingCapacity,
    modifyCurrentDBClusterCapacityResponse_timeoutAction,
    modifyCurrentDBClusterCapacityResponse_dbClusterIdentifier,
    modifyCurrentDBClusterCapacityResponse_secondsBeforeTimeout,
    modifyCurrentDBClusterCapacityResponse_currentCapacity,
    modifyCurrentDBClusterCapacityResponse_httpStatus,

    -- ** DeleteDBParameterGroup
    deleteDBParameterGroup_dbParameterGroupName,

    -- ** DownloadDBLogFilePortion
    downloadDBLogFilePortion_numberOfLines,
    downloadDBLogFilePortion_marker,
    downloadDBLogFilePortion_dbInstanceIdentifier,
    downloadDBLogFilePortion_logFileName,
    downloadDBLogFilePortionResponse_additionalDataPending,
    downloadDBLogFilePortionResponse_logFileData,
    downloadDBLogFilePortionResponse_marker,
    downloadDBLogFilePortionResponse_httpStatus,

    -- ** DeleteDBClusterSnapshot
    deleteDBClusterSnapshot_dbClusterSnapshotIdentifier,
    deleteDBClusterSnapshotResponse_dbClusterSnapshot,
    deleteDBClusterSnapshotResponse_httpStatus,

    -- ** RemoveFromGlobalCluster
    removeFromGlobalCluster_dbClusterIdentifier,
    removeFromGlobalCluster_globalClusterIdentifier,
    removeFromGlobalClusterResponse_globalCluster,
    removeFromGlobalClusterResponse_httpStatus,

    -- ** DescribeDBSecurityGroups
    describeDBSecurityGroups_dbSecurityGroupName,
    describeDBSecurityGroups_filters,
    describeDBSecurityGroups_maxRecords,
    describeDBSecurityGroups_marker,
    describeDBSecurityGroupsResponse_dbSecurityGroups,
    describeDBSecurityGroupsResponse_marker,
    describeDBSecurityGroupsResponse_httpStatus,

    -- ** CreateDBSnapshot
    createDBSnapshot_tags,
    createDBSnapshot_dbSnapshotIdentifier,
    createDBSnapshot_dbInstanceIdentifier,
    createDBSnapshotResponse_dbSnapshot,
    createDBSnapshotResponse_httpStatus,

    -- ** RestoreDBClusterFromSnapshot
    restoreDBClusterFromSnapshot_deletionProtection,
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication,
    restoreDBClusterFromSnapshot_enableCloudwatchLogsExports,
    restoreDBClusterFromSnapshot_optionGroupName,
    restoreDBClusterFromSnapshot_domain,
    restoreDBClusterFromSnapshot_scalingConfiguration,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_engineMode,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_engineVersion,
    restoreDBClusterFromSnapshot_tags,
    restoreDBClusterFromSnapshot_port,
    restoreDBClusterFromSnapshot_domainIAMRoleName,
    restoreDBClusterFromSnapshot_copyTagsToSnapshot,
    restoreDBClusterFromSnapshot_backtrackWindow,
    restoreDBClusterFromSnapshot_dbClusterParameterGroupName,
    restoreDBClusterFromSnapshot_databaseName,
    restoreDBClusterFromSnapshot_dbClusterIdentifier,
    restoreDBClusterFromSnapshot_snapshotIdentifier,
    restoreDBClusterFromSnapshot_engine,
    restoreDBClusterFromSnapshotResponse_dbCluster,
    restoreDBClusterFromSnapshotResponse_httpStatus,

    -- ** CreateDBSubnetGroup
    createDBSubnetGroup_tags,
    createDBSubnetGroup_dbSubnetGroupName,
    createDBSubnetGroup_dbSubnetGroupDescription,
    createDBSubnetGroup_subnetIds,
    createDBSubnetGroupResponse_dbSubnetGroup,
    createDBSubnetGroupResponse_httpStatus,

    -- ** DescribeOrderableDBInstanceOptions
    describeOrderableDBInstanceOptions_engineVersion,
    describeOrderableDBInstanceOptions_licenseModel,
    describeOrderableDBInstanceOptions_dbInstanceClass,
    describeOrderableDBInstanceOptions_filters,
    describeOrderableDBInstanceOptions_availabilityZoneGroup,
    describeOrderableDBInstanceOptions_vpc,
    describeOrderableDBInstanceOptions_maxRecords,
    describeOrderableDBInstanceOptions_marker,
    describeOrderableDBInstanceOptions_engine,
    describeOrderableDBInstanceOptionsResponse_orderableDBInstanceOptions,
    describeOrderableDBInstanceOptionsResponse_marker,
    describeOrderableDBInstanceOptionsResponse_httpStatus,

    -- ** DeleteDBSecurityGroup
    deleteDBSecurityGroup_dbSecurityGroupName,

    -- ** CreateEventSubscription
    createEventSubscription_sourceIds,
    createEventSubscription_enabled,
    createEventSubscription_eventCategories,
    createEventSubscription_tags,
    createEventSubscription_sourceType,
    createEventSubscription_subscriptionName,
    createEventSubscription_snsTopicArn,
    createEventSubscriptionResponse_eventSubscription,
    createEventSubscriptionResponse_httpStatus,

    -- ** AuthorizeDBSecurityGroupIngress
    authorizeDBSecurityGroupIngress_cidrip,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupOwnerId,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupId,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupName,
    authorizeDBSecurityGroupIngress_dbSecurityGroupName,
    authorizeDBSecurityGroupIngressResponse_dbSecurityGroup,
    authorizeDBSecurityGroupIngressResponse_httpStatus,

    -- ** RestoreDBInstanceFromDBSnapshot
    restoreDBInstanceFromDBSnapshot_deletionProtection,
    restoreDBInstanceFromDBSnapshot_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromDBSnapshot_useDefaultProcessorFeatures,
    restoreDBInstanceFromDBSnapshot_enableCloudwatchLogsExports,
    restoreDBInstanceFromDBSnapshot_storageType,
    restoreDBInstanceFromDBSnapshot_optionGroupName,
    restoreDBInstanceFromDBSnapshot_domain,
    restoreDBInstanceFromDBSnapshot_dbSubnetGroupName,
    restoreDBInstanceFromDBSnapshot_publiclyAccessible,
    restoreDBInstanceFromDBSnapshot_multiAZ,
    restoreDBInstanceFromDBSnapshot_vpcSecurityGroupIds,
    restoreDBInstanceFromDBSnapshot_dbParameterGroupName,
    restoreDBInstanceFromDBSnapshot_availabilityZone,
    restoreDBInstanceFromDBSnapshot_licenseModel,
    restoreDBInstanceFromDBSnapshot_processorFeatures,
    restoreDBInstanceFromDBSnapshot_tags,
    restoreDBInstanceFromDBSnapshot_dbName,
    restoreDBInstanceFromDBSnapshot_dbInstanceClass,
    restoreDBInstanceFromDBSnapshot_port,
    restoreDBInstanceFromDBSnapshot_tdeCredentialPassword,
    restoreDBInstanceFromDBSnapshot_engine,
    restoreDBInstanceFromDBSnapshot_domainIAMRoleName,
    restoreDBInstanceFromDBSnapshot_tdeCredentialArn,
    restoreDBInstanceFromDBSnapshot_copyTagsToSnapshot,
    restoreDBInstanceFromDBSnapshot_enableCustomerOwnedIp,
    restoreDBInstanceFromDBSnapshot_iops,
    restoreDBInstanceFromDBSnapshot_autoMinorVersionUpgrade,
    restoreDBInstanceFromDBSnapshot_dbInstanceIdentifier,
    restoreDBInstanceFromDBSnapshot_dbSnapshotIdentifier,
    restoreDBInstanceFromDBSnapshotResponse_dbInstance,
    restoreDBInstanceFromDBSnapshotResponse_httpStatus,

    -- ** DescribeSourceRegions
    describeSourceRegions_regionName,
    describeSourceRegions_filters,
    describeSourceRegions_maxRecords,
    describeSourceRegions_marker,
    describeSourceRegionsResponse_sourceRegions,
    describeSourceRegionsResponse_marker,
    describeSourceRegionsResponse_httpStatus,

    -- ** DeleteDBSnapshot
    deleteDBSnapshot_dbSnapshotIdentifier,
    deleteDBSnapshotResponse_dbSnapshot,
    deleteDBSnapshotResponse_httpStatus,

    -- ** CreateDBClusterEndpoint
    createDBClusterEndpoint_excludedMembers,
    createDBClusterEndpoint_tags,
    createDBClusterEndpoint_staticMembers,
    createDBClusterEndpoint_dbClusterIdentifier,
    createDBClusterEndpoint_dbClusterEndpointIdentifier,
    createDBClusterEndpoint_endpointType,
    dbClusterEndpoint_status,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_excludedMembers,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_endpoint,
    dbClusterEndpoint_staticMembers,

    -- ** DeleteDBProxy
    deleteDBProxy_dbProxyName,
    deleteDBProxyResponse_dbProxy,
    deleteDBProxyResponse_httpStatus,

    -- ** PurchaseReservedDBInstancesOffering
    purchaseReservedDBInstancesOffering_dbInstanceCount,
    purchaseReservedDBInstancesOffering_tags,
    purchaseReservedDBInstancesOffering_reservedDBInstanceId,
    purchaseReservedDBInstancesOffering_reservedDBInstancesOfferingId,
    purchaseReservedDBInstancesOfferingResponse_reservedDBInstance,
    purchaseReservedDBInstancesOfferingResponse_httpStatus,

    -- ** DeleteDBClusterParameterGroup
    deleteDBClusterParameterGroup_dbClusterParameterGroupName,

    -- ** DeleteGlobalCluster
    deleteGlobalCluster_globalClusterIdentifier,
    deleteGlobalClusterResponse_globalCluster,
    deleteGlobalClusterResponse_httpStatus,

    -- ** ModifyDBParameterGroup
    modifyDBParameterGroup_dbParameterGroupName,
    modifyDBParameterGroup_parameters,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** RevokeDBSecurityGroupIngress
    revokeDBSecurityGroupIngress_cidrip,
    revokeDBSecurityGroupIngress_eC2SecurityGroupOwnerId,
    revokeDBSecurityGroupIngress_eC2SecurityGroupId,
    revokeDBSecurityGroupIngress_eC2SecurityGroupName,
    revokeDBSecurityGroupIngress_dbSecurityGroupName,
    revokeDBSecurityGroupIngressResponse_dbSecurityGroup,
    revokeDBSecurityGroupIngressResponse_httpStatus,

    -- ** PromoteReadReplicaDBCluster
    promoteReadReplicaDBCluster_dbClusterIdentifier,
    promoteReadReplicaDBClusterResponse_dbCluster,
    promoteReadReplicaDBClusterResponse_httpStatus,

    -- ** DeregisterDBProxyTargets
    deregisterDBProxyTargets_dbClusterIdentifiers,
    deregisterDBProxyTargets_targetGroupName,
    deregisterDBProxyTargets_dbInstanceIdentifiers,
    deregisterDBProxyTargets_dbProxyName,
    deregisterDBProxyTargetsResponse_httpStatus,

    -- ** FailoverGlobalCluster
    failoverGlobalCluster_globalClusterIdentifier,
    failoverGlobalCluster_targetDbClusterIdentifier,
    failoverGlobalClusterResponse_globalCluster,
    failoverGlobalClusterResponse_httpStatus,

    -- ** ResetDBParameterGroup
    resetDBParameterGroup_resetAllParameters,
    resetDBParameterGroup_parameters,
    resetDBParameterGroup_dbParameterGroupName,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** CopyDBParameterGroup
    copyDBParameterGroup_tags,
    copyDBParameterGroup_sourceDBParameterGroupIdentifier,
    copyDBParameterGroup_targetDBParameterGroupIdentifier,
    copyDBParameterGroup_targetDBParameterGroupDescription,
    copyDBParameterGroupResponse_dbParameterGroup,
    copyDBParameterGroupResponse_httpStatus,

    -- ** DescribeInstallationMedia
    describeInstallationMedia_installationMediaId,
    describeInstallationMedia_filters,
    describeInstallationMedia_maxRecords,
    describeInstallationMedia_marker,
    describeInstallationMediaResponse_installationMedia,
    describeInstallationMediaResponse_marker,
    describeInstallationMediaResponse_httpStatus,

    -- ** CopyDBClusterSnapshot
    copyDBClusterSnapshot_destinationRegion,
    copyDBClusterSnapshot_kmsKeyId,
    copyDBClusterSnapshot_copyTags,
    copyDBClusterSnapshot_tags,
    copyDBClusterSnapshot_preSignedUrl,
    copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier,
    copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier,
    copyDBClusterSnapshotResponse_dbClusterSnapshot,
    copyDBClusterSnapshotResponse_httpStatus,

    -- ** ImportInstallationMedia
    importInstallationMedia_customAvailabilityZoneId,
    importInstallationMedia_engine,
    importInstallationMedia_engineVersion,
    importInstallationMedia_engineInstallationMediaPath,
    importInstallationMedia_oSInstallationMediaPath,
    installationMedia_status,
    installationMedia_customAvailabilityZoneId,
    installationMedia_installationMediaId,
    installationMedia_engineVersion,
    installationMedia_failureCause,
    installationMedia_oSInstallationMediaPath,
    installationMedia_engine,
    installationMedia_engineInstallationMediaPath,

    -- ** DeleteCustomAvailabilityZone
    deleteCustomAvailabilityZone_customAvailabilityZoneId,
    deleteCustomAvailabilityZoneResponse_customAvailabilityZone,
    deleteCustomAvailabilityZoneResponse_httpStatus,

    -- ** CreateGlobalCluster
    createGlobalCluster_deletionProtection,
    createGlobalCluster_storageEncrypted,
    createGlobalCluster_engineVersion,
    createGlobalCluster_engine,
    createGlobalCluster_globalClusterIdentifier,
    createGlobalCluster_databaseName,
    createGlobalCluster_sourceDBClusterIdentifier,
    createGlobalClusterResponse_globalCluster,
    createGlobalClusterResponse_httpStatus,

    -- ** DescribeDBProxyTargets
    describeDBProxyTargets_targetGroupName,
    describeDBProxyTargets_filters,
    describeDBProxyTargets_maxRecords,
    describeDBProxyTargets_marker,
    describeDBProxyTargets_dbProxyName,
    describeDBProxyTargetsResponse_targets,
    describeDBProxyTargetsResponse_marker,
    describeDBProxyTargetsResponse_httpStatus,

    -- ** RemoveSourceIdentifierFromSubscription
    removeSourceIdentifierFromSubscription_subscriptionName,
    removeSourceIdentifierFromSubscription_sourceIdentifier,
    removeSourceIdentifierFromSubscriptionResponse_eventSubscription,
    removeSourceIdentifierFromSubscriptionResponse_httpStatus,

    -- ** DescribeCustomAvailabilityZones
    describeCustomAvailabilityZones_customAvailabilityZoneId,
    describeCustomAvailabilityZones_filters,
    describeCustomAvailabilityZones_maxRecords,
    describeCustomAvailabilityZones_marker,
    describeCustomAvailabilityZonesResponse_customAvailabilityZones,
    describeCustomAvailabilityZonesResponse_marker,
    describeCustomAvailabilityZonesResponse_httpStatus,

    -- ** ModifyDBSnapshot
    modifyDBSnapshot_optionGroupName,
    modifyDBSnapshot_engineVersion,
    modifyDBSnapshot_dbSnapshotIdentifier,
    modifyDBSnapshotResponse_dbSnapshot,
    modifyDBSnapshotResponse_httpStatus,

    -- ** AddRoleToDBCluster
    addRoleToDBCluster_featureName,
    addRoleToDBCluster_dbClusterIdentifier,
    addRoleToDBCluster_roleArn,

    -- ** DescribeEngineDefaultParameters
    describeEngineDefaultParameters_filters,
    describeEngineDefaultParameters_maxRecords,
    describeEngineDefaultParameters_marker,
    describeEngineDefaultParameters_dbParameterGroupFamily,
    describeEngineDefaultParametersResponse_httpStatus,
    describeEngineDefaultParametersResponse_engineDefaults,

    -- ** ListTagsForResource
    listTagsForResource_filters,
    listTagsForResource_resourceName,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** ModifyDBSubnetGroup
    modifyDBSubnetGroup_dbSubnetGroupDescription,
    modifyDBSubnetGroup_dbSubnetGroupName,
    modifyDBSubnetGroup_subnetIds,
    modifyDBSubnetGroupResponse_dbSubnetGroup,
    modifyDBSubnetGroupResponse_httpStatus,

    -- ** RestoreDBInstanceToPointInTime
    restoreDBInstanceToPointInTime_deletionProtection,
    restoreDBInstanceToPointInTime_sourceDBInstanceAutomatedBackupsArn,
    restoreDBInstanceToPointInTime_maxAllocatedStorage,
    restoreDBInstanceToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBInstanceToPointInTime_useDefaultProcessorFeatures,
    restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier,
    restoreDBInstanceToPointInTime_enableCloudwatchLogsExports,
    restoreDBInstanceToPointInTime_storageType,
    restoreDBInstanceToPointInTime_optionGroupName,
    restoreDBInstanceToPointInTime_domain,
    restoreDBInstanceToPointInTime_restoreTime,
    restoreDBInstanceToPointInTime_dbSubnetGroupName,
    restoreDBInstanceToPointInTime_publiclyAccessible,
    restoreDBInstanceToPointInTime_multiAZ,
    restoreDBInstanceToPointInTime_vpcSecurityGroupIds,
    restoreDBInstanceToPointInTime_dbParameterGroupName,
    restoreDBInstanceToPointInTime_availabilityZone,
    restoreDBInstanceToPointInTime_licenseModel,
    restoreDBInstanceToPointInTime_processorFeatures,
    restoreDBInstanceToPointInTime_tags,
    restoreDBInstanceToPointInTime_dbName,
    restoreDBInstanceToPointInTime_dbInstanceClass,
    restoreDBInstanceToPointInTime_port,
    restoreDBInstanceToPointInTime_tdeCredentialPassword,
    restoreDBInstanceToPointInTime_engine,
    restoreDBInstanceToPointInTime_domainIAMRoleName,
    restoreDBInstanceToPointInTime_tdeCredentialArn,
    restoreDBInstanceToPointInTime_copyTagsToSnapshot,
    restoreDBInstanceToPointInTime_enableCustomerOwnedIp,
    restoreDBInstanceToPointInTime_iops,
    restoreDBInstanceToPointInTime_sourceDbiResourceId,
    restoreDBInstanceToPointInTime_autoMinorVersionUpgrade,
    restoreDBInstanceToPointInTime_useLatestRestorableTime,
    restoreDBInstanceToPointInTime_targetDBInstanceIdentifier,
    restoreDBInstanceToPointInTimeResponse_dbInstance,
    restoreDBInstanceToPointInTimeResponse_httpStatus,

    -- * Types

    -- ** AccountQuota
    accountQuota_used,
    accountQuota_accountQuotaName,
    accountQuota_max,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** AvailableProcessorFeature
    availableProcessorFeature_allowedValues,
    availableProcessorFeature_name,
    availableProcessorFeature_defaultValue,

    -- ** Certificate
    certificate_certificateIdentifier,
    certificate_customerOverride,
    certificate_validFrom,
    certificate_certificateArn,
    certificate_certificateType,
    certificate_thumbprint,
    certificate_customerOverrideValidTill,
    certificate_validTill,

    -- ** CharacterSet
    characterSet_characterSetName,
    characterSet_characterSetDescription,

    -- ** CloudwatchLogsExportConfiguration
    cloudwatchLogsExportConfiguration_enableLogTypes,
    cloudwatchLogsExportConfiguration_disableLogTypes,

    -- ** ClusterPendingModifiedValues
    clusterPendingModifiedValues_masterUserPassword,
    clusterPendingModifiedValues_dbClusterIdentifier,
    clusterPendingModifiedValues_pendingCloudwatchLogsExports,
    clusterPendingModifiedValues_engineVersion,
    clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled,

    -- ** ConnectionPoolConfiguration
    connectionPoolConfiguration_sessionPinningFilters,
    connectionPoolConfiguration_maxIdleConnectionsPercent,
    connectionPoolConfiguration_connectionBorrowTimeout,
    connectionPoolConfiguration_initQuery,
    connectionPoolConfiguration_maxConnectionsPercent,

    -- ** ConnectionPoolConfigurationInfo
    connectionPoolConfigurationInfo_sessionPinningFilters,
    connectionPoolConfigurationInfo_maxIdleConnectionsPercent,
    connectionPoolConfigurationInfo_connectionBorrowTimeout,
    connectionPoolConfigurationInfo_initQuery,
    connectionPoolConfigurationInfo_maxConnectionsPercent,

    -- ** CustomAvailabilityZone
    customAvailabilityZone_customAvailabilityZoneId,
    customAvailabilityZone_customAvailabilityZoneName,
    customAvailabilityZone_vpnDetails,
    customAvailabilityZone_customAvailabilityZoneStatus,

    -- ** DBCluster
    dbCluster_associatedRoles,
    dbCluster_vpcSecurityGroups,
    dbCluster_automaticRestartTime,
    dbCluster_deletionProtection,
    dbCluster_preferredBackupWindow,
    dbCluster_dbClusterMembers,
    dbCluster_backupRetentionPeriod,
    dbCluster_storageEncrypted,
    dbCluster_availabilityZones,
    dbCluster_status,
    dbCluster_backtrackConsumedChangeRecords,
    dbCluster_crossAccountClone,
    dbCluster_globalWriteForwardingStatus,
    dbCluster_domainMemberships,
    dbCluster_dbClusterArn,
    dbCluster_cloneGroupId,
    dbCluster_dbClusterOptionGroupMemberships,
    dbCluster_latestRestorableTime,
    dbCluster_readerEndpoint,
    dbCluster_activityStreamKmsKeyId,
    dbCluster_httpEndpointEnabled,
    dbCluster_clusterCreateTime,
    dbCluster_earliestRestorableTime,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_activityStreamStatus,
    dbCluster_engineMode,
    dbCluster_multiAZ,
    dbCluster_dbSubnetGroup,
    dbCluster_readReplicaIdentifiers,
    dbCluster_masterUsername,
    dbCluster_hostedZoneId,
    dbCluster_kmsKeyId,
    dbCluster_dbClusterIdentifier,
    dbCluster_engineVersion,
    dbCluster_capacity,
    dbCluster_characterSetName,
    dbCluster_preferredMaintenanceWindow,
    dbCluster_port,
    dbCluster_percentProgress,
    dbCluster_engine,
    dbCluster_pendingModifiedValues,
    dbCluster_endpoint,
    dbCluster_copyTagsToSnapshot,
    dbCluster_scalingConfigurationInfo,
    dbCluster_dbClusterResourceId,
    dbCluster_customEndpoints,
    dbCluster_tagList,
    dbCluster_earliestBacktrackTime,
    dbCluster_backtrackWindow,
    dbCluster_allocatedStorage,
    dbCluster_globalWriteForwardingRequested,
    dbCluster_iAMDatabaseAuthenticationEnabled,
    dbCluster_dbClusterParameterGroup,
    dbCluster_replicationSourceIdentifier,
    dbCluster_activityStreamKinesisStreamName,
    dbCluster_databaseName,
    dbCluster_activityStreamMode,

    -- ** DBClusterBacktrack
    dbClusterBacktrack_status,
    dbClusterBacktrack_backtrackTo,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_backtrackRequestCreationTime,
    dbClusterBacktrack_backtrackedFrom,

    -- ** DBClusterEndpoint
    dbClusterEndpoint_status,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_excludedMembers,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_endpoint,
    dbClusterEndpoint_staticMembers,

    -- ** DBClusterMember
    dbClusterMember_isClusterWriter,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_promotionTier,
    dbClusterMember_dbClusterParameterGroupStatus,

    -- ** DBClusterOptionGroupStatus
    dbClusterOptionGroupStatus_status,
    dbClusterOptionGroupStatus_dbClusterOptionGroupName,

    -- ** DBClusterParameterGroup
    dbClusterParameterGroup_dbClusterParameterGroupArn,
    dbClusterParameterGroup_dbParameterGroupFamily,
    dbClusterParameterGroup_description,
    dbClusterParameterGroup_dbClusterParameterGroupName,

    -- ** DBClusterParameterGroupNameMessage
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DBClusterRole
    dbClusterRole_status,
    dbClusterRole_roleArn,
    dbClusterRole_featureName,

    -- ** DBClusterSnapshot
    dbClusterSnapshot_storageEncrypted,
    dbClusterSnapshot_availabilityZones,
    dbClusterSnapshot_status,
    dbClusterSnapshot_clusterCreateTime,
    dbClusterSnapshot_snapshotCreateTime,
    dbClusterSnapshot_engineMode,
    dbClusterSnapshot_masterUsername,
    dbClusterSnapshot_kmsKeyId,
    dbClusterSnapshot_dbClusterIdentifier,
    dbClusterSnapshot_engineVersion,
    dbClusterSnapshot_snapshotType,
    dbClusterSnapshot_licenseModel,
    dbClusterSnapshot_port,
    dbClusterSnapshot_percentProgress,
    dbClusterSnapshot_engine,
    dbClusterSnapshot_dbClusterSnapshotIdentifier,
    dbClusterSnapshot_sourceDBClusterSnapshotArn,
    dbClusterSnapshot_tagList,
    dbClusterSnapshot_vpcId,
    dbClusterSnapshot_allocatedStorage,
    dbClusterSnapshot_dbClusterSnapshotArn,
    dbClusterSnapshot_iAMDatabaseAuthenticationEnabled,

    -- ** DBClusterSnapshotAttribute
    dbClusterSnapshotAttribute_attributeName,
    dbClusterSnapshotAttribute_attributeValues,

    -- ** DBClusterSnapshotAttributesResult
    dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier,

    -- ** DBEngineVersion
    dbEngineVersion_status,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_validUpgradeTarget,
    dbEngineVersion_engineVersion,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_supportsParallelQuery,

    -- ** DBInstance
    dbInstance_associatedRoles,
    dbInstance_vpcSecurityGroups,
    dbInstance_automaticRestartTime,
    dbInstance_deletionProtection,
    dbInstance_preferredBackupWindow,
    dbInstance_backupRetentionPeriod,
    dbInstance_storageEncrypted,
    dbInstance_maxAllocatedStorage,
    dbInstance_cACertificateIdentifier,
    dbInstance_dbSecurityGroups,
    dbInstance_domainMemberships,
    dbInstance_statusInfos,
    dbInstance_latestRestorableTime,
    dbInstance_storageType,
    dbInstance_activityStreamKmsKeyId,
    dbInstance_dbInstancePort,
    dbInstance_monitoringInterval,
    dbInstance_dbParameterGroups,
    dbInstance_instanceCreateTime,
    dbInstance_dbiResourceId,
    dbInstance_optionGroupMemberships,
    dbInstance_monitoringRoleArn,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_activityStreamStatus,
    dbInstance_listenerEndpoint,
    dbInstance_publiclyAccessible,
    dbInstance_ncharCharacterSetName,
    dbInstance_multiAZ,
    dbInstance_dbSubnetGroup,
    dbInstance_masterUsername,
    dbInstance_performanceInsightsKMSKeyId,
    dbInstance_kmsKeyId,
    dbInstance_customerOwnedIpEnabled,
    dbInstance_dbClusterIdentifier,
    dbInstance_availabilityZone,
    dbInstance_engineVersion,
    dbInstance_performanceInsightsRetentionPeriod,
    dbInstance_licenseModel,
    dbInstance_characterSetName,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_dbInstanceIdentifier,
    dbInstance_processorFeatures,
    dbInstance_dbName,
    dbInstance_promotionTier,
    dbInstance_dbInstanceClass,
    dbInstance_awsBackupRecoveryPointArn,
    dbInstance_readReplicaDBClusterIdentifiers,
    dbInstance_engine,
    dbInstance_pendingModifiedValues,
    dbInstance_dbInstanceAutomatedBackupsReplications,
    dbInstance_tdeCredentialArn,
    dbInstance_endpoint,
    dbInstance_replicaMode,
    dbInstance_readReplicaSourceDBInstanceIdentifier,
    dbInstance_copyTagsToSnapshot,
    dbInstance_dbInstanceStatus,
    dbInstance_timezone,
    dbInstance_tagList,
    dbInstance_performanceInsightsEnabled,
    dbInstance_allocatedStorage,
    dbInstance_iops,
    dbInstance_enhancedMonitoringResourceArn,
    dbInstance_secondaryAvailabilityZone,
    dbInstance_readReplicaDBInstanceIdentifiers,
    dbInstance_iAMDatabaseAuthenticationEnabled,
    dbInstance_activityStreamEngineNativeAuditFieldsIncluded,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_activityStreamKinesisStreamName,
    dbInstance_dbInstanceArn,
    dbInstance_activityStreamMode,

    -- ** DBInstanceAutomatedBackup
    dbInstanceAutomatedBackup_backupRetentionPeriod,
    dbInstanceAutomatedBackup_status,
    dbInstanceAutomatedBackup_storageType,
    dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn,
    dbInstanceAutomatedBackup_encrypted,
    dbInstanceAutomatedBackup_optionGroupName,
    dbInstanceAutomatedBackup_instanceCreateTime,
    dbInstanceAutomatedBackup_dbiResourceId,
    dbInstanceAutomatedBackup_masterUsername,
    dbInstanceAutomatedBackup_kmsKeyId,
    dbInstanceAutomatedBackup_availabilityZone,
    dbInstanceAutomatedBackup_engineVersion,
    dbInstanceAutomatedBackup_restoreWindow,
    dbInstanceAutomatedBackup_licenseModel,
    dbInstanceAutomatedBackup_dbInstanceIdentifier,
    dbInstanceAutomatedBackup_port,
    dbInstanceAutomatedBackup_engine,
    dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications,
    dbInstanceAutomatedBackup_tdeCredentialArn,
    dbInstanceAutomatedBackup_timezone,
    dbInstanceAutomatedBackup_region,
    dbInstanceAutomatedBackup_vpcId,
    dbInstanceAutomatedBackup_allocatedStorage,
    dbInstanceAutomatedBackup_iops,
    dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled,
    dbInstanceAutomatedBackup_dbInstanceArn,

    -- ** DBInstanceAutomatedBackupsReplication
    dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn,

    -- ** DBInstanceRole
    dbInstanceRole_status,
    dbInstanceRole_roleArn,
    dbInstanceRole_featureName,

    -- ** DBInstanceStatusInfo
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_message,
    dbInstanceStatusInfo_statusType,

    -- ** DBParameterGroup
    dbParameterGroup_dbParameterGroupArn,
    dbParameterGroup_dbParameterGroupName,
    dbParameterGroup_dbParameterGroupFamily,
    dbParameterGroup_description,

    -- ** DBParameterGroupNameMessage
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** DBParameterGroupStatus
    dbParameterGroupStatus_dbParameterGroupName,
    dbParameterGroupStatus_parameterApplyStatus,

    -- ** DBProxy
    dbProxy_dbProxyArn,
    dbProxy_createdDate,
    dbProxy_status,
    dbProxy_roleArn,
    dbProxy_idleClientTimeout,
    dbProxy_auth,
    dbProxy_vpcSubnetIds,
    dbProxy_engineFamily,
    dbProxy_debugLogging,
    dbProxy_vpcSecurityGroupIds,
    dbProxy_updatedDate,
    dbProxy_requireTLS,
    dbProxy_endpoint,
    dbProxy_vpcId,
    dbProxy_dbProxyName,

    -- ** DBProxyEndpoint
    dbProxyEndpoint_createdDate,
    dbProxyEndpoint_dbProxyEndpointArn,
    dbProxyEndpoint_status,
    dbProxyEndpoint_isDefault,
    dbProxyEndpoint_vpcSubnetIds,
    dbProxyEndpoint_vpcSecurityGroupIds,
    dbProxyEndpoint_dbProxyEndpointName,
    dbProxyEndpoint_endpoint,
    dbProxyEndpoint_vpcId,
    dbProxyEndpoint_targetRole,
    dbProxyEndpoint_dbProxyName,

    -- ** DBProxyTarget
    dbProxyTarget_trackedClusterId,
    dbProxyTarget_rdsResourceId,
    dbProxyTarget_targetArn,
    dbProxyTarget_role,
    dbProxyTarget_port,
    dbProxyTarget_endpoint,
    dbProxyTarget_type,
    dbProxyTarget_targetHealth,

    -- ** DBProxyTargetGroup
    dbProxyTargetGroup_createdDate,
    dbProxyTargetGroup_status,
    dbProxyTargetGroup_isDefault,
    dbProxyTargetGroup_targetGroupName,
    dbProxyTargetGroup_targetGroupArn,
    dbProxyTargetGroup_connectionPoolConfig,
    dbProxyTargetGroup_updatedDate,
    dbProxyTargetGroup_dbProxyName,

    -- ** DBSecurityGroup
    dbSecurityGroup_ownerId,
    dbSecurityGroup_dbSecurityGroupName,
    dbSecurityGroup_iPRanges,
    dbSecurityGroup_dbSecurityGroupDescription,
    dbSecurityGroup_eC2SecurityGroups,
    dbSecurityGroup_vpcId,
    dbSecurityGroup_dbSecurityGroupArn,

    -- ** DBSecurityGroupMembership
    dbSecurityGroupMembership_status,
    dbSecurityGroupMembership_dbSecurityGroupName,

    -- ** DBSnapshot
    dbSnapshot_status,
    dbSnapshot_originalSnapshotCreateTime,
    dbSnapshot_storageType,
    dbSnapshot_dbSnapshotIdentifier,
    dbSnapshot_encrypted,
    dbSnapshot_optionGroupName,
    dbSnapshot_instanceCreateTime,
    dbSnapshot_dbiResourceId,
    dbSnapshot_snapshotCreateTime,
    dbSnapshot_masterUsername,
    dbSnapshot_kmsKeyId,
    dbSnapshot_dbSnapshotArn,
    dbSnapshot_availabilityZone,
    dbSnapshot_engineVersion,
    dbSnapshot_snapshotType,
    dbSnapshot_licenseModel,
    dbSnapshot_sourceDBSnapshotIdentifier,
    dbSnapshot_dbInstanceIdentifier,
    dbSnapshot_processorFeatures,
    dbSnapshot_port,
    dbSnapshot_percentProgress,
    dbSnapshot_engine,
    dbSnapshot_tdeCredentialArn,
    dbSnapshot_timezone,
    dbSnapshot_tagList,
    dbSnapshot_vpcId,
    dbSnapshot_allocatedStorage,
    dbSnapshot_iops,
    dbSnapshot_sourceRegion,
    dbSnapshot_iAMDatabaseAuthenticationEnabled,

    -- ** DBSnapshotAttribute
    dbSnapshotAttribute_attributeName,
    dbSnapshotAttribute_attributeValues,

    -- ** DBSnapshotAttributesResult
    dbSnapshotAttributesResult_dbSnapshotIdentifier,
    dbSnapshotAttributesResult_dbSnapshotAttributes,

    -- ** DBSubnetGroup
    dbSubnetGroup_subnetGroupStatus,
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_vpcId,
    dbSubnetGroup_subnets,

    -- ** DescribeDBLogFilesDetails
    describeDBLogFilesDetails_lastWritten,
    describeDBLogFilesDetails_logFileName,
    describeDBLogFilesDetails_size,

    -- ** DomainMembership
    domainMembership_status,
    domainMembership_domain,
    domainMembership_iAMRoleName,
    domainMembership_fqdn,

    -- ** DoubleRange
    doubleRange_to,
    doubleRange_from,

    -- ** EC2SecurityGroup
    eC2SecurityGroup_status,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_eC2SecurityGroupId,
    eC2SecurityGroup_eC2SecurityGroupName,

    -- ** Endpoint
    endpoint_address,
    endpoint_hostedZoneId,
    endpoint_port,

    -- ** EngineDefaults
    engineDefaults_dbParameterGroupFamily,
    engineDefaults_parameters,
    engineDefaults_marker,

    -- ** Event
    event_message,
    event_eventCategories,
    event_date,
    event_sourceIdentifier,
    event_sourceArn,
    event_sourceType,

    -- ** EventCategoriesMap
    eventCategoriesMap_eventCategories,
    eventCategoriesMap_sourceType,

    -- ** EventSubscription
    eventSubscription_custSubscriptionId,
    eventSubscription_status,
    eventSubscription_sourceIdsList,
    eventSubscription_eventCategoriesList,
    eventSubscription_enabled,
    eventSubscription_eventSubscriptionArn,
    eventSubscription_subscriptionCreationTime,
    eventSubscription_customerAwsId,
    eventSubscription_sourceType,
    eventSubscription_snsTopicArn,

    -- ** ExportTask
    exportTask_taskEndTime,
    exportTask_status,
    exportTask_iamRoleArn,
    exportTask_totalExtractedDataInGB,
    exportTask_warningMessage,
    exportTask_snapshotTime,
    exportTask_s3Bucket,
    exportTask_exportOnly,
    exportTask_kmsKeyId,
    exportTask_failureCause,
    exportTask_percentProgress,
    exportTask_sourceArn,
    exportTask_s3Prefix,
    exportTask_taskStartTime,
    exportTask_exportTaskIdentifier,

    -- ** FailoverState
    failoverState_status,
    failoverState_toDbClusterArn,
    failoverState_fromDbClusterArn,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** GlobalCluster
    globalCluster_deletionProtection,
    globalCluster_storageEncrypted,
    globalCluster_status,
    globalCluster_failoverState,
    globalCluster_engineVersion,
    globalCluster_globalClusterArn,
    globalCluster_globalClusterResourceId,
    globalCluster_engine,
    globalCluster_globalClusterIdentifier,
    globalCluster_globalClusterMembers,
    globalCluster_databaseName,

    -- ** GlobalClusterMember
    globalClusterMember_globalWriteForwardingStatus,
    globalClusterMember_dbClusterArn,
    globalClusterMember_readers,
    globalClusterMember_isWriter,

    -- ** IPRange
    iPRange_status,
    iPRange_cidrip,

    -- ** InstallationMedia
    installationMedia_status,
    installationMedia_customAvailabilityZoneId,
    installationMedia_installationMediaId,
    installationMedia_engineVersion,
    installationMedia_failureCause,
    installationMedia_oSInstallationMediaPath,
    installationMedia_engine,
    installationMedia_engineInstallationMediaPath,

    -- ** InstallationMediaFailureCause
    installationMediaFailureCause_message,

    -- ** MinimumEngineVersionPerAllowedValue
    minimumEngineVersionPerAllowedValue_allowedValue,
    minimumEngineVersionPerAllowedValue_minimumEngineVersion,

    -- ** Option
    option_optionName,
    option_optionVersion,
    option_dbSecurityGroupMemberships,
    option_optionDescription,
    option_port,
    option_optionSettings,
    option_persistent,
    option_vpcSecurityGroupMemberships,
    option_permanent,

    -- ** OptionConfiguration
    optionConfiguration_optionVersion,
    optionConfiguration_dbSecurityGroupMemberships,
    optionConfiguration_port,
    optionConfiguration_optionSettings,
    optionConfiguration_vpcSecurityGroupMemberships,
    optionConfiguration_optionName,

    -- ** OptionGroup
    optionGroup_optionGroupArn,
    optionGroup_engineName,
    optionGroup_allowsVpcAndNonVpcInstanceMemberships,
    optionGroup_optionGroupName,
    optionGroup_options,
    optionGroup_optionGroupDescription,
    optionGroup_majorEngineVersion,
    optionGroup_vpcId,

    -- ** OptionGroupMembership
    optionGroupMembership_status,
    optionGroupMembership_optionGroupName,

    -- ** OptionGroupOption
    optionGroupOption_optionsConflictsWith,
    optionGroupOption_engineName,
    optionGroupOption_vpcOnly,
    optionGroupOption_optionGroupOptionVersions,
    optionGroupOption_optionGroupOptionSettings,
    optionGroupOption_requiresAutoMinorEngineVersionUpgrade,
    optionGroupOption_defaultPort,
    optionGroupOption_name,
    optionGroupOption_majorEngineVersion,
    optionGroupOption_minimumRequiredMinorEngineVersion,
    optionGroupOption_description,
    optionGroupOption_optionsDependedOn,
    optionGroupOption_portRequired,
    optionGroupOption_persistent,
    optionGroupOption_permanent,
    optionGroupOption_supportsOptionVersionDowngrade,

    -- ** OptionGroupOptionSetting
    optionGroupOptionSetting_minimumEngineVersionPerAllowedValue,
    optionGroupOptionSetting_allowedValues,
    optionGroupOptionSetting_applyType,
    optionGroupOptionSetting_settingName,
    optionGroupOptionSetting_isRequired,
    optionGroupOptionSetting_defaultValue,
    optionGroupOptionSetting_settingDescription,
    optionGroupOptionSetting_isModifiable,

    -- ** OptionSetting
    optionSetting_isCollection,
    optionSetting_allowedValues,
    optionSetting_name,
    optionSetting_applyType,
    optionSetting_description,
    optionSetting_value,
    optionSetting_defaultValue,
    optionSetting_dataType,
    optionSetting_isModifiable,

    -- ** OptionVersion
    optionVersion_isDefault,
    optionVersion_version,

    -- ** OrderableDBInstanceOption
    orderableDBInstanceOption_minIopsPerDbInstance,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_supportsIops,
    orderableDBInstanceOption_supportsKerberosAuthentication,
    orderableDBInstanceOption_storageType,
    orderableDBInstanceOption_supportsEnhancedMonitoring,
    orderableDBInstanceOption_supportedEngineModes,
    orderableDBInstanceOption_maxStorageSize,
    orderableDBInstanceOption_availableProcessorFeatures,
    orderableDBInstanceOption_maxIopsPerGib,
    orderableDBInstanceOption_supportsStorageEncryption,
    orderableDBInstanceOption_multiAZCapable,
    orderableDBInstanceOption_supportedActivityStreamModes,
    orderableDBInstanceOption_supportsStorageAutoscaling,
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_minIopsPerGib,
    orderableDBInstanceOption_supportsIAMDatabaseAuthentication,
    orderableDBInstanceOption_licenseModel,
    orderableDBInstanceOption_maxIopsPerDbInstance,
    orderableDBInstanceOption_supportsGlobalDatabases,
    orderableDBInstanceOption_supportsPerformanceInsights,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_minStorageSize,
    orderableDBInstanceOption_outpostCapable,
    orderableDBInstanceOption_availabilityZoneGroup,
    orderableDBInstanceOption_vpc,
    orderableDBInstanceOption_readReplicaCapable,

    -- ** Outpost
    outpost_arn,

    -- ** Parameter
    parameter_supportedEngineModes,
    parameter_allowedValues,
    parameter_source,
    parameter_parameterValue,
    parameter_applyType,
    parameter_parameterName,
    parameter_description,
    parameter_applyMethod,
    parameter_dataType,
    parameter_isModifiable,
    parameter_minimumEngineVersion,

    -- ** PendingCloudwatchLogsExports
    pendingCloudwatchLogsExports_logTypesToDisable,
    pendingCloudwatchLogsExports_logTypesToEnable,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_description,

    -- ** PendingModifiedValues
    pendingModifiedValues_backupRetentionPeriod,
    pendingModifiedValues_cACertificateIdentifier,
    pendingModifiedValues_storageType,
    pendingModifiedValues_dbSubnetGroupName,
    pendingModifiedValues_masterUserPassword,
    pendingModifiedValues_multiAZ,
    pendingModifiedValues_pendingCloudwatchLogsExports,
    pendingModifiedValues_engineVersion,
    pendingModifiedValues_licenseModel,
    pendingModifiedValues_dbInstanceIdentifier,
    pendingModifiedValues_processorFeatures,
    pendingModifiedValues_dbInstanceClass,
    pendingModifiedValues_port,
    pendingModifiedValues_allocatedStorage,
    pendingModifiedValues_iops,
    pendingModifiedValues_iAMDatabaseAuthenticationEnabled,

    -- ** ProcessorFeature
    processorFeature_name,
    processorFeature_value,

    -- ** Range
    range_to,
    range_from,
    range_step,

    -- ** RecurringCharge
    recurringCharge_recurringChargeFrequency,
    recurringCharge_recurringChargeAmount,

    -- ** ReservedDBInstance
    reservedDBInstance_duration,
    reservedDBInstance_reservedDBInstanceArn,
    reservedDBInstance_startTime,
    reservedDBInstance_currencyCode,
    reservedDBInstance_multiAZ,
    reservedDBInstance_state,
    reservedDBInstance_dbInstanceCount,
    reservedDBInstance_dbInstanceClass,
    reservedDBInstance_reservedDBInstanceId,
    reservedDBInstance_fixedPrice,
    reservedDBInstance_leaseId,
    reservedDBInstance_usagePrice,
    reservedDBInstance_offeringType,
    reservedDBInstance_recurringCharges,
    reservedDBInstance_productDescription,
    reservedDBInstance_reservedDBInstancesOfferingId,

    -- ** ReservedDBInstancesOffering
    reservedDBInstancesOffering_duration,
    reservedDBInstancesOffering_currencyCode,
    reservedDBInstancesOffering_multiAZ,
    reservedDBInstancesOffering_dbInstanceClass,
    reservedDBInstancesOffering_fixedPrice,
    reservedDBInstancesOffering_usagePrice,
    reservedDBInstancesOffering_offeringType,
    reservedDBInstancesOffering_recurringCharges,
    reservedDBInstancesOffering_productDescription,
    reservedDBInstancesOffering_reservedDBInstancesOfferingId,

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- ** RestoreWindow
    restoreWindow_earliestTime,
    restoreWindow_latestTime,

    -- ** ScalingConfiguration
    scalingConfiguration_maxCapacity,
    scalingConfiguration_timeoutAction,
    scalingConfiguration_autoPause,
    scalingConfiguration_secondsUntilAutoPause,
    scalingConfiguration_secondsBeforeTimeout,
    scalingConfiguration_minCapacity,

    -- ** ScalingConfigurationInfo
    scalingConfigurationInfo_maxCapacity,
    scalingConfigurationInfo_timeoutAction,
    scalingConfigurationInfo_autoPause,
    scalingConfigurationInfo_secondsUntilAutoPause,
    scalingConfigurationInfo_secondsBeforeTimeout,
    scalingConfigurationInfo_minCapacity,

    -- ** SourceRegion
    sourceRegion_regionName,
    sourceRegion_status,
    sourceRegion_supportsDBInstanceAutomatedBackupsReplication,
    sourceRegion_endpoint,

    -- ** Subnet
    subnet_subnetStatus,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,
    subnet_subnetOutpost,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TargetHealth
    targetHealth_state,
    targetHealth_reason,
    targetHealth_description,

    -- ** Timezone
    timezone_timezoneName,

    -- ** UpgradeTarget
    upgradeTarget_autoUpgrade,
    upgradeTarget_supportedEngineModes,
    upgradeTarget_engineVersion,
    upgradeTarget_supportsGlobalDatabases,
    upgradeTarget_engine,
    upgradeTarget_description,
    upgradeTarget_isMajorVersionUpgrade,
    upgradeTarget_supportsParallelQuery,

    -- ** UserAuthConfig
    userAuthConfig_secretArn,
    userAuthConfig_authScheme,
    userAuthConfig_iAMAuth,
    userAuthConfig_userName,
    userAuthConfig_description,

    -- ** UserAuthConfigInfo
    userAuthConfigInfo_secretArn,
    userAuthConfigInfo_authScheme,
    userAuthConfigInfo_iAMAuth,
    userAuthConfigInfo_userName,
    userAuthConfigInfo_description,

    -- ** ValidDBInstanceModificationsMessage
    validDBInstanceModificationsMessage_validProcessorFeatures,
    validDBInstanceModificationsMessage_storage,

    -- ** ValidStorageOptions
    validStorageOptions_storageType,
    validStorageOptions_provisionedIops,
    validStorageOptions_supportsStorageAutoscaling,
    validStorageOptions_storageSize,
    validStorageOptions_iopsToStorageRatio,

    -- ** VpcSecurityGroupMembership
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,

    -- ** VpnDetails
    vpnDetails_vpnId,
    vpnDetails_vpnTunnelOriginatorIP,
    vpnDetails_vpnName,
    vpnDetails_vpnState,
    vpnDetails_vpnGatewayIp,
    vpnDetails_vpnPSK,
  )
where

import Network.AWS.RDS.AddRoleToDBCluster
import Network.AWS.RDS.AddRoleToDBInstance
import Network.AWS.RDS.AddSourceIdentifierToSubscription
import Network.AWS.RDS.AddTagsToResource
import Network.AWS.RDS.ApplyPendingMaintenanceAction
import Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
import Network.AWS.RDS.BacktrackDBCluster
import Network.AWS.RDS.CancelExportTask
import Network.AWS.RDS.CopyDBClusterParameterGroup
import Network.AWS.RDS.CopyDBClusterSnapshot
import Network.AWS.RDS.CopyDBParameterGroup
import Network.AWS.RDS.CopyDBSnapshot
import Network.AWS.RDS.CopyOptionGroup
import Network.AWS.RDS.CreateCustomAvailabilityZone
import Network.AWS.RDS.CreateDBCluster
import Network.AWS.RDS.CreateDBClusterEndpoint
import Network.AWS.RDS.CreateDBClusterParameterGroup
import Network.AWS.RDS.CreateDBClusterSnapshot
import Network.AWS.RDS.CreateDBInstance
import Network.AWS.RDS.CreateDBInstanceReadReplica
import Network.AWS.RDS.CreateDBParameterGroup
import Network.AWS.RDS.CreateDBProxy
import Network.AWS.RDS.CreateDBProxyEndpoint
import Network.AWS.RDS.CreateDBSecurityGroup
import Network.AWS.RDS.CreateDBSnapshot
import Network.AWS.RDS.CreateDBSubnetGroup
import Network.AWS.RDS.CreateEventSubscription
import Network.AWS.RDS.CreateGlobalCluster
import Network.AWS.RDS.CreateOptionGroup
import Network.AWS.RDS.DeleteCustomAvailabilityZone
import Network.AWS.RDS.DeleteDBCluster
import Network.AWS.RDS.DeleteDBClusterEndpoint
import Network.AWS.RDS.DeleteDBClusterParameterGroup
import Network.AWS.RDS.DeleteDBClusterSnapshot
import Network.AWS.RDS.DeleteDBInstance
import Network.AWS.RDS.DeleteDBInstanceAutomatedBackup
import Network.AWS.RDS.DeleteDBParameterGroup
import Network.AWS.RDS.DeleteDBProxy
import Network.AWS.RDS.DeleteDBProxyEndpoint
import Network.AWS.RDS.DeleteDBSecurityGroup
import Network.AWS.RDS.DeleteDBSnapshot
import Network.AWS.RDS.DeleteDBSubnetGroup
import Network.AWS.RDS.DeleteEventSubscription
import Network.AWS.RDS.DeleteGlobalCluster
import Network.AWS.RDS.DeleteInstallationMedia
import Network.AWS.RDS.DeleteOptionGroup
import Network.AWS.RDS.DeregisterDBProxyTargets
import Network.AWS.RDS.DescribeAccountAttributes
import Network.AWS.RDS.DescribeCertificates
import Network.AWS.RDS.DescribeCustomAvailabilityZones
import Network.AWS.RDS.DescribeDBClusterBacktracks
import Network.AWS.RDS.DescribeDBClusterEndpoints
import Network.AWS.RDS.DescribeDBClusterParameterGroups
import Network.AWS.RDS.DescribeDBClusterParameters
import Network.AWS.RDS.DescribeDBClusterSnapshotAttributes
import Network.AWS.RDS.DescribeDBClusterSnapshots
import Network.AWS.RDS.DescribeDBClusters
import Network.AWS.RDS.DescribeDBEngineVersions
import Network.AWS.RDS.DescribeDBInstanceAutomatedBackups
import Network.AWS.RDS.DescribeDBInstances
import Network.AWS.RDS.DescribeDBLogFiles
import Network.AWS.RDS.DescribeDBParameterGroups
import Network.AWS.RDS.DescribeDBParameters
import Network.AWS.RDS.DescribeDBProxies
import Network.AWS.RDS.DescribeDBProxyEndpoints
import Network.AWS.RDS.DescribeDBProxyTargetGroups
import Network.AWS.RDS.DescribeDBProxyTargets
import Network.AWS.RDS.DescribeDBSecurityGroups
import Network.AWS.RDS.DescribeDBSnapshotAttributes
import Network.AWS.RDS.DescribeDBSnapshots
import Network.AWS.RDS.DescribeDBSubnetGroups
import Network.AWS.RDS.DescribeEngineDefaultClusterParameters
import Network.AWS.RDS.DescribeEngineDefaultParameters
import Network.AWS.RDS.DescribeEventCategories
import Network.AWS.RDS.DescribeEventSubscriptions
import Network.AWS.RDS.DescribeEvents
import Network.AWS.RDS.DescribeExportTasks
import Network.AWS.RDS.DescribeGlobalClusters
import Network.AWS.RDS.DescribeInstallationMedia
import Network.AWS.RDS.DescribeOptionGroupOptions
import Network.AWS.RDS.DescribeOptionGroups
import Network.AWS.RDS.DescribeOrderableDBInstanceOptions
import Network.AWS.RDS.DescribePendingMaintenanceActions
import Network.AWS.RDS.DescribeReservedDBInstances
import Network.AWS.RDS.DescribeReservedDBInstancesOfferings
import Network.AWS.RDS.DescribeSourceRegions
import Network.AWS.RDS.DescribeValidDBInstanceModifications
import Network.AWS.RDS.DownloadDBLogFilePortion
import Network.AWS.RDS.FailoverDBCluster
import Network.AWS.RDS.FailoverGlobalCluster
import Network.AWS.RDS.ImportInstallationMedia
import Network.AWS.RDS.ListTagsForResource
import Network.AWS.RDS.ModifyCertificates
import Network.AWS.RDS.ModifyCurrentDBClusterCapacity
import Network.AWS.RDS.ModifyDBCluster
import Network.AWS.RDS.ModifyDBClusterEndpoint
import Network.AWS.RDS.ModifyDBClusterParameterGroup
import Network.AWS.RDS.ModifyDBClusterSnapshotAttribute
import Network.AWS.RDS.ModifyDBInstance
import Network.AWS.RDS.ModifyDBParameterGroup
import Network.AWS.RDS.ModifyDBProxy
import Network.AWS.RDS.ModifyDBProxyEndpoint
import Network.AWS.RDS.ModifyDBProxyTargetGroup
import Network.AWS.RDS.ModifyDBSnapshot
import Network.AWS.RDS.ModifyDBSnapshotAttribute
import Network.AWS.RDS.ModifyDBSubnetGroup
import Network.AWS.RDS.ModifyEventSubscription
import Network.AWS.RDS.ModifyGlobalCluster
import Network.AWS.RDS.ModifyOptionGroup
import Network.AWS.RDS.PromoteReadReplica
import Network.AWS.RDS.PromoteReadReplicaDBCluster
import Network.AWS.RDS.PurchaseReservedDBInstancesOffering
import Network.AWS.RDS.RebootDBInstance
import Network.AWS.RDS.RegisterDBProxyTargets
import Network.AWS.RDS.RemoveFromGlobalCluster
import Network.AWS.RDS.RemoveRoleFromDBCluster
import Network.AWS.RDS.RemoveRoleFromDBInstance
import Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
import Network.AWS.RDS.RemoveTagsFromResource
import Network.AWS.RDS.ResetDBClusterParameterGroup
import Network.AWS.RDS.ResetDBParameterGroup
import Network.AWS.RDS.RestoreDBClusterFromS
import Network.AWS.RDS.RestoreDBClusterFromSnapshot
import Network.AWS.RDS.RestoreDBClusterToPointInTime
import Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
import Network.AWS.RDS.RestoreDBInstanceFromS
import Network.AWS.RDS.RestoreDBInstanceToPointInTime
import Network.AWS.RDS.RevokeDBSecurityGroupIngress
import Network.AWS.RDS.StartActivityStream
import Network.AWS.RDS.StartDBCluster
import Network.AWS.RDS.StartDBInstance
import Network.AWS.RDS.StartDBInstanceAutomatedBackupsReplication
import Network.AWS.RDS.StartExportTask
import Network.AWS.RDS.StopActivityStream
import Network.AWS.RDS.StopDBCluster
import Network.AWS.RDS.StopDBInstance
import Network.AWS.RDS.StopDBInstanceAutomatedBackupsReplication
import Network.AWS.RDS.Types.AccountQuota
import Network.AWS.RDS.Types.AvailabilityZone
import Network.AWS.RDS.Types.AvailableProcessorFeature
import Network.AWS.RDS.Types.Certificate
import Network.AWS.RDS.Types.CharacterSet
import Network.AWS.RDS.Types.CloudwatchLogsExportConfiguration
import Network.AWS.RDS.Types.ClusterPendingModifiedValues
import Network.AWS.RDS.Types.ConnectionPoolConfiguration
import Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo
import Network.AWS.RDS.Types.CustomAvailabilityZone
import Network.AWS.RDS.Types.DBCluster
import Network.AWS.RDS.Types.DBClusterBacktrack
import Network.AWS.RDS.Types.DBClusterEndpoint
import Network.AWS.RDS.Types.DBClusterMember
import Network.AWS.RDS.Types.DBClusterOptionGroupStatus
import Network.AWS.RDS.Types.DBClusterParameterGroup
import Network.AWS.RDS.Types.DBClusterParameterGroupNameMessage
import Network.AWS.RDS.Types.DBClusterRole
import Network.AWS.RDS.Types.DBClusterSnapshot
import Network.AWS.RDS.Types.DBClusterSnapshotAttribute
import Network.AWS.RDS.Types.DBClusterSnapshotAttributesResult
import Network.AWS.RDS.Types.DBEngineVersion
import Network.AWS.RDS.Types.DBInstance
import Network.AWS.RDS.Types.DBInstanceAutomatedBackup
import Network.AWS.RDS.Types.DBInstanceAutomatedBackupsReplication
import Network.AWS.RDS.Types.DBInstanceRole
import Network.AWS.RDS.Types.DBInstanceStatusInfo
import Network.AWS.RDS.Types.DBParameterGroup
import Network.AWS.RDS.Types.DBParameterGroupNameMessage
import Network.AWS.RDS.Types.DBParameterGroupStatus
import Network.AWS.RDS.Types.DBProxy
import Network.AWS.RDS.Types.DBProxyEndpoint
import Network.AWS.RDS.Types.DBProxyTarget
import Network.AWS.RDS.Types.DBProxyTargetGroup
import Network.AWS.RDS.Types.DBSecurityGroup
import Network.AWS.RDS.Types.DBSecurityGroupMembership
import Network.AWS.RDS.Types.DBSnapshot
import Network.AWS.RDS.Types.DBSnapshotAttribute
import Network.AWS.RDS.Types.DBSnapshotAttributesResult
import Network.AWS.RDS.Types.DBSubnetGroup
import Network.AWS.RDS.Types.DescribeDBLogFilesDetails
import Network.AWS.RDS.Types.DomainMembership
import Network.AWS.RDS.Types.DoubleRange
import Network.AWS.RDS.Types.EC2SecurityGroup
import Network.AWS.RDS.Types.Endpoint
import Network.AWS.RDS.Types.EngineDefaults
import Network.AWS.RDS.Types.Event
import Network.AWS.RDS.Types.EventCategoriesMap
import Network.AWS.RDS.Types.EventSubscription
import Network.AWS.RDS.Types.ExportTask
import Network.AWS.RDS.Types.FailoverState
import Network.AWS.RDS.Types.Filter
import Network.AWS.RDS.Types.GlobalCluster
import Network.AWS.RDS.Types.GlobalClusterMember
import Network.AWS.RDS.Types.IPRange
import Network.AWS.RDS.Types.InstallationMedia
import Network.AWS.RDS.Types.InstallationMediaFailureCause
import Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue
import Network.AWS.RDS.Types.Option
import Network.AWS.RDS.Types.OptionConfiguration
import Network.AWS.RDS.Types.OptionGroup
import Network.AWS.RDS.Types.OptionGroupMembership
import Network.AWS.RDS.Types.OptionGroupOption
import Network.AWS.RDS.Types.OptionGroupOptionSetting
import Network.AWS.RDS.Types.OptionSetting
import Network.AWS.RDS.Types.OptionVersion
import Network.AWS.RDS.Types.OrderableDBInstanceOption
import Network.AWS.RDS.Types.Outpost
import Network.AWS.RDS.Types.Parameter
import Network.AWS.RDS.Types.PendingCloudwatchLogsExports
import Network.AWS.RDS.Types.PendingMaintenanceAction
import Network.AWS.RDS.Types.PendingModifiedValues
import Network.AWS.RDS.Types.ProcessorFeature
import Network.AWS.RDS.Types.Range
import Network.AWS.RDS.Types.RecurringCharge
import Network.AWS.RDS.Types.ReservedDBInstance
import Network.AWS.RDS.Types.ReservedDBInstancesOffering
import Network.AWS.RDS.Types.ResourcePendingMaintenanceActions
import Network.AWS.RDS.Types.RestoreWindow
import Network.AWS.RDS.Types.ScalingConfiguration
import Network.AWS.RDS.Types.ScalingConfigurationInfo
import Network.AWS.RDS.Types.SourceRegion
import Network.AWS.RDS.Types.Subnet
import Network.AWS.RDS.Types.Tag
import Network.AWS.RDS.Types.TargetHealth
import Network.AWS.RDS.Types.Timezone
import Network.AWS.RDS.Types.UpgradeTarget
import Network.AWS.RDS.Types.UserAuthConfig
import Network.AWS.RDS.Types.UserAuthConfigInfo
import Network.AWS.RDS.Types.ValidDBInstanceModificationsMessage
import Network.AWS.RDS.Types.ValidStorageOptions
import Network.AWS.RDS.Types.VpcSecurityGroupMembership
import Network.AWS.RDS.Types.VpnDetails
