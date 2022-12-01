{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDS.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Lens
  ( -- * Operations

    -- ** AddRoleToDBCluster
    addRoleToDBCluster_featureName,
    addRoleToDBCluster_dbClusterIdentifier,
    addRoleToDBCluster_roleArn,

    -- ** AddRoleToDBInstance
    addRoleToDBInstance_dbInstanceIdentifier,
    addRoleToDBInstance_roleArn,
    addRoleToDBInstance_featureName,

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

    -- ** AuthorizeDBSecurityGroupIngress
    authorizeDBSecurityGroupIngress_eC2SecurityGroupId,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupOwnerId,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupName,
    authorizeDBSecurityGroupIngress_cidrip,
    authorizeDBSecurityGroupIngress_dbSecurityGroupName,
    authorizeDBSecurityGroupIngressResponse_dbSecurityGroup,
    authorizeDBSecurityGroupIngressResponse_httpStatus,

    -- ** BacktrackDBCluster
    backtrackDBCluster_force,
    backtrackDBCluster_useEarliestTimeOnPointInTimeUnavailable,
    backtrackDBCluster_dbClusterIdentifier,
    backtrackDBCluster_backtrackTo,
    dbClusterBacktrack_backtrackedFrom,
    dbClusterBacktrack_backtrackRequestCreationTime,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_status,
    dbClusterBacktrack_backtrackTo,

    -- ** CancelExportTask
    cancelExportTask_exportTaskIdentifier,
    exportTask_s3Bucket,
    exportTask_percentProgress,
    exportTask_taskStartTime,
    exportTask_exportOnly,
    exportTask_sourceArn,
    exportTask_totalExtractedDataInGB,
    exportTask_status,
    exportTask_sourceType,
    exportTask_warningMessage,
    exportTask_snapshotTime,
    exportTask_exportTaskIdentifier,
    exportTask_iamRoleArn,
    exportTask_kmsKeyId,
    exportTask_failureCause,
    exportTask_taskEndTime,
    exportTask_s3Prefix,

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
    copyDBClusterSnapshot_destinationRegion,
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

    -- ** CopyDBSnapshot
    copyDBSnapshot_tags,
    copyDBSnapshot_targetCustomAvailabilityZone,
    copyDBSnapshot_optionGroupName,
    copyDBSnapshot_copyTags,
    copyDBSnapshot_kmsKeyId,
    copyDBSnapshot_destinationRegion,
    copyDBSnapshot_preSignedUrl,
    copyDBSnapshot_sourceDBSnapshotIdentifier,
    copyDBSnapshot_targetDBSnapshotIdentifier,
    copyDBSnapshotResponse_dbSnapshot,
    copyDBSnapshotResponse_httpStatus,

    -- ** CopyOptionGroup
    copyOptionGroup_tags,
    copyOptionGroup_sourceOptionGroupIdentifier,
    copyOptionGroup_targetOptionGroupIdentifier,
    copyOptionGroup_targetOptionGroupDescription,
    copyOptionGroupResponse_optionGroup,
    copyOptionGroupResponse_httpStatus,

    -- ** CreateCustomDBEngineVersion
    createCustomDBEngineVersion_tags,
    createCustomDBEngineVersion_description,
    createCustomDBEngineVersion_databaseInstallationFilesS3Prefix,
    createCustomDBEngineVersion_engine,
    createCustomDBEngineVersion_engineVersion,
    createCustomDBEngineVersion_databaseInstallationFilesS3BucketName,
    createCustomDBEngineVersion_kmsKeyId,
    createCustomDBEngineVersion_manifest,
    dbEngineVersion_validUpgradeTarget,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_supportsBabelfish,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_tagList,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_dbEngineVersionArn,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_status,
    dbEngineVersion_customDBEngineVersionManifest,
    dbEngineVersion_majorEngineVersion,
    dbEngineVersion_databaseInstallationFilesS3BucketName,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_supportsParallelQuery,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_kmsKeyId,
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_databaseInstallationFilesS3Prefix,
    dbEngineVersion_createTime,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_engineVersion,
    dbEngineVersion_dbEngineDescription,

    -- ** CreateDBCluster
    createDBCluster_tags,
    createDBCluster_port,
    createDBCluster_serverlessV2ScalingConfiguration,
    createDBCluster_enableGlobalWriteForwarding,
    createDBCluster_performanceInsightsRetentionPeriod,
    createDBCluster_vpcSecurityGroupIds,
    createDBCluster_preferredBackupWindow,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_characterSetName,
    createDBCluster_masterUsername,
    createDBCluster_copyTagsToSnapshot,
    createDBCluster_domainIAMRoleName,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_autoMinorVersionUpgrade,
    createDBCluster_dbClusterInstanceClass,
    createDBCluster_databaseName,
    createDBCluster_domain,
    createDBCluster_optionGroupName,
    createDBCluster_availabilityZones,
    createDBCluster_performanceInsightsKMSKeyId,
    createDBCluster_enableIAMDatabaseAuthentication,
    createDBCluster_monitoringInterval,
    createDBCluster_masterUserPassword,
    createDBCluster_publiclyAccessible,
    createDBCluster_storageType,
    createDBCluster_enableCloudwatchLogsExports,
    createDBCluster_enableHttpEndpoint,
    createDBCluster_backtrackWindow,
    createDBCluster_enablePerformanceInsights,
    createDBCluster_replicationSourceIdentifier,
    createDBCluster_scalingConfiguration,
    createDBCluster_monitoringRoleArn,
    createDBCluster_engineMode,
    createDBCluster_storageEncrypted,
    createDBCluster_kmsKeyId,
    createDBCluster_globalClusterIdentifier,
    createDBCluster_allocatedStorage,
    createDBCluster_deletionProtection,
    createDBCluster_preferredMaintenanceWindow,
    createDBCluster_destinationRegion,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_dbSystemId,
    createDBCluster_iops,
    createDBCluster_preSignedUrl,
    createDBCluster_engineVersion,
    createDBCluster_networkType,
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
    createDBInstance_maxAllocatedStorage,
    createDBInstance_port,
    createDBInstance_performanceInsightsRetentionPeriod,
    createDBInstance_vpcSecurityGroupIds,
    createDBInstance_dbParameterGroupName,
    createDBInstance_backupTarget,
    createDBInstance_preferredBackupWindow,
    createDBInstance_storageThroughput,
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
    createDBInstance_processorFeatures,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_tdeCredentialArn,
    createDBInstance_ncharCharacterSetName,
    createDBInstance_monitoringRoleArn,
    createDBInstance_storageEncrypted,
    createDBInstance_kmsKeyId,
    createDBInstance_allocatedStorage,
    createDBInstance_deletionProtection,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_customIamInstanceProfile,
    createDBInstance_iops,
    createDBInstance_engineVersion,
    createDBInstance_dbName,
    createDBInstance_networkType,
    createDBInstance_multiAZ,
    createDBInstance_enableCustomerOwnedIp,
    createDBInstance_licenseModel,
    createDBInstance_dbInstanceIdentifier,
    createDBInstance_dbInstanceClass,
    createDBInstance_engine,
    createDBInstanceResponse_dbInstance,
    createDBInstanceResponse_httpStatus,

    -- ** CreateDBInstanceReadReplica
    createDBInstanceReadReplica_tags,
    createDBInstanceReadReplica_maxAllocatedStorage,
    createDBInstanceReadReplica_port,
    createDBInstanceReadReplica_performanceInsightsRetentionPeriod,
    createDBInstanceReadReplica_vpcSecurityGroupIds,
    createDBInstanceReadReplica_dbParameterGroupName,
    createDBInstanceReadReplica_storageThroughput,
    createDBInstanceReadReplica_dbInstanceClass,
    createDBInstanceReadReplica_copyTagsToSnapshot,
    createDBInstanceReadReplica_domainIAMRoleName,
    createDBInstanceReadReplica_dbSubnetGroupName,
    createDBInstanceReadReplica_autoMinorVersionUpgrade,
    createDBInstanceReadReplica_domain,
    createDBInstanceReadReplica_optionGroupName,
    createDBInstanceReadReplica_performanceInsightsKMSKeyId,
    createDBInstanceReadReplica_enableIAMDatabaseAuthentication,
    createDBInstanceReadReplica_monitoringInterval,
    createDBInstanceReadReplica_availabilityZone,
    createDBInstanceReadReplica_publiclyAccessible,
    createDBInstanceReadReplica_storageType,
    createDBInstanceReadReplica_enableCloudwatchLogsExports,
    createDBInstanceReadReplica_processorFeatures,
    createDBInstanceReadReplica_enablePerformanceInsights,
    createDBInstanceReadReplica_monitoringRoleArn,
    createDBInstanceReadReplica_replicaMode,
    createDBInstanceReadReplica_kmsKeyId,
    createDBInstanceReadReplica_deletionProtection,
    createDBInstanceReadReplica_destinationRegion,
    createDBInstanceReadReplica_customIamInstanceProfile,
    createDBInstanceReadReplica_iops,
    createDBInstanceReadReplica_preSignedUrl,
    createDBInstanceReadReplica_networkType,
    createDBInstanceReadReplica_multiAZ,
    createDBInstanceReadReplica_useDefaultProcessorFeatures,
    createDBInstanceReadReplica_dbInstanceIdentifier,
    createDBInstanceReadReplica_sourceDBInstanceIdentifier,
    createDBInstanceReadReplicaResponse_dbInstance,
    createDBInstanceReadReplicaResponse_httpStatus,

    -- ** CreateDBParameterGroup
    createDBParameterGroup_tags,
    createDBParameterGroup_dbParameterGroupName,
    createDBParameterGroup_dbParameterGroupFamily,
    createDBParameterGroup_description,
    createDBParameterGroupResponse_dbParameterGroup,
    createDBParameterGroupResponse_httpStatus,

    -- ** CreateDBProxy
    createDBProxy_tags,
    createDBProxy_vpcSecurityGroupIds,
    createDBProxy_requireTLS,
    createDBProxy_debugLogging,
    createDBProxy_idleClientTimeout,
    createDBProxy_dbProxyName,
    createDBProxy_engineFamily,
    createDBProxy_auth,
    createDBProxy_roleArn,
    createDBProxy_vpcSubnetIds,
    createDBProxyResponse_dbProxy,
    createDBProxyResponse_httpStatus,

    -- ** CreateDBProxyEndpoint
    createDBProxyEndpoint_tags,
    createDBProxyEndpoint_vpcSecurityGroupIds,
    createDBProxyEndpoint_targetRole,
    createDBProxyEndpoint_dbProxyName,
    createDBProxyEndpoint_dbProxyEndpointName,
    createDBProxyEndpoint_vpcSubnetIds,
    createDBProxyEndpointResponse_dbProxyEndpoint,
    createDBProxyEndpointResponse_httpStatus,

    -- ** CreateDBSecurityGroup
    createDBSecurityGroup_tags,
    createDBSecurityGroup_dbSecurityGroupName,
    createDBSecurityGroup_dbSecurityGroupDescription,
    createDBSecurityGroupResponse_dbSecurityGroup,
    createDBSecurityGroupResponse_httpStatus,

    -- ** CreateDBSnapshot
    createDBSnapshot_tags,
    createDBSnapshot_dbSnapshotIdentifier,
    createDBSnapshot_dbInstanceIdentifier,
    createDBSnapshotResponse_dbSnapshot,
    createDBSnapshotResponse_httpStatus,

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
    createGlobalCluster_globalClusterIdentifier,
    createGlobalCluster_engine,
    createGlobalCluster_deletionProtection,
    createGlobalCluster_engineVersion,
    createGlobalClusterResponse_globalCluster,
    createGlobalClusterResponse_httpStatus,

    -- ** CreateOptionGroup
    createOptionGroup_tags,
    createOptionGroup_optionGroupName,
    createOptionGroup_engineName,
    createOptionGroup_majorEngineVersion,
    createOptionGroup_optionGroupDescription,
    createOptionGroupResponse_optionGroup,
    createOptionGroupResponse_httpStatus,

    -- ** DeleteCustomDBEngineVersion
    deleteCustomDBEngineVersion_engine,
    deleteCustomDBEngineVersion_engineVersion,
    dbEngineVersion_validUpgradeTarget,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_supportsBabelfish,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_tagList,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_dbEngineVersionArn,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_status,
    dbEngineVersion_customDBEngineVersionManifest,
    dbEngineVersion_majorEngineVersion,
    dbEngineVersion_databaseInstallationFilesS3BucketName,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_supportsParallelQuery,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_kmsKeyId,
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_databaseInstallationFilesS3Prefix,
    dbEngineVersion_createTime,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_engineVersion,
    dbEngineVersion_dbEngineDescription,

    -- ** DeleteDBCluster
    deleteDBCluster_finalDBSnapshotIdentifier,
    deleteDBCluster_skipFinalSnapshot,
    deleteDBCluster_dbClusterIdentifier,
    deleteDBClusterResponse_dbCluster,
    deleteDBClusterResponse_httpStatus,

    -- ** DeleteDBClusterEndpoint
    deleteDBClusterEndpoint_dbClusterEndpointIdentifier,
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

    -- ** DeleteDBClusterParameterGroup
    deleteDBClusterParameterGroup_dbClusterParameterGroupName,

    -- ** DeleteDBClusterSnapshot
    deleteDBClusterSnapshot_dbClusterSnapshotIdentifier,
    deleteDBClusterSnapshotResponse_dbClusterSnapshot,
    deleteDBClusterSnapshotResponse_httpStatus,

    -- ** DeleteDBInstance
    deleteDBInstance_finalDBSnapshotIdentifier,
    deleteDBInstance_deleteAutomatedBackups,
    deleteDBInstance_skipFinalSnapshot,
    deleteDBInstance_dbInstanceIdentifier,
    deleteDBInstanceResponse_dbInstance,
    deleteDBInstanceResponse_httpStatus,

    -- ** DeleteDBInstanceAutomatedBackup
    deleteDBInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn,
    deleteDBInstanceAutomatedBackup_dbiResourceId,
    deleteDBInstanceAutomatedBackupResponse_dbInstanceAutomatedBackup,
    deleteDBInstanceAutomatedBackupResponse_httpStatus,

    -- ** DeleteDBParameterGroup
    deleteDBParameterGroup_dbParameterGroupName,

    -- ** DeleteDBProxy
    deleteDBProxy_dbProxyName,
    deleteDBProxyResponse_dbProxy,
    deleteDBProxyResponse_httpStatus,

    -- ** DeleteDBProxyEndpoint
    deleteDBProxyEndpoint_dbProxyEndpointName,
    deleteDBProxyEndpointResponse_dbProxyEndpoint,
    deleteDBProxyEndpointResponse_httpStatus,

    -- ** DeleteDBSecurityGroup
    deleteDBSecurityGroup_dbSecurityGroupName,

    -- ** DeleteDBSnapshot
    deleteDBSnapshot_dbSnapshotIdentifier,
    deleteDBSnapshotResponse_dbSnapshot,
    deleteDBSnapshotResponse_httpStatus,

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

    -- ** DeleteOptionGroup
    deleteOptionGroup_optionGroupName,

    -- ** DeregisterDBProxyTargets
    deregisterDBProxyTargets_dbInstanceIdentifiers,
    deregisterDBProxyTargets_targetGroupName,
    deregisterDBProxyTargets_dbClusterIdentifiers,
    deregisterDBProxyTargets_dbProxyName,
    deregisterDBProxyTargetsResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributesResponse_accountQuotas,
    describeAccountAttributesResponse_httpStatus,

    -- ** DescribeCertificates
    describeCertificates_marker,
    describeCertificates_filters,
    describeCertificates_maxRecords,
    describeCertificates_certificateIdentifier,
    describeCertificatesResponse_marker,
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_httpStatus,

    -- ** DescribeDBClusterBacktracks
    describeDBClusterBacktracks_marker,
    describeDBClusterBacktracks_filters,
    describeDBClusterBacktracks_backtrackIdentifier,
    describeDBClusterBacktracks_maxRecords,
    describeDBClusterBacktracks_dbClusterIdentifier,
    describeDBClusterBacktracksResponse_marker,
    describeDBClusterBacktracksResponse_dbClusterBacktracks,
    describeDBClusterBacktracksResponse_httpStatus,

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
    describeDBClusters_includeShared,
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
    describeDBEngineVersions_includeAll,
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

    -- ** DescribeDBInstanceAutomatedBackups
    describeDBInstanceAutomatedBackups_marker,
    describeDBInstanceAutomatedBackups_dbInstanceIdentifier,
    describeDBInstanceAutomatedBackups_filters,
    describeDBInstanceAutomatedBackups_dbInstanceAutomatedBackupsArn,
    describeDBInstanceAutomatedBackups_maxRecords,
    describeDBInstanceAutomatedBackups_dbiResourceId,
    describeDBInstanceAutomatedBackupsResponse_marker,
    describeDBInstanceAutomatedBackupsResponse_dbInstanceAutomatedBackups,
    describeDBInstanceAutomatedBackupsResponse_httpStatus,

    -- ** DescribeDBInstances
    describeDBInstances_marker,
    describeDBInstances_dbInstanceIdentifier,
    describeDBInstances_filters,
    describeDBInstances_maxRecords,
    describeDBInstancesResponse_marker,
    describeDBInstancesResponse_dbInstances,
    describeDBInstancesResponse_httpStatus,

    -- ** DescribeDBLogFiles
    describeDBLogFiles_fileLastWritten,
    describeDBLogFiles_fileSize,
    describeDBLogFiles_marker,
    describeDBLogFiles_filters,
    describeDBLogFiles_maxRecords,
    describeDBLogFiles_filenameContains,
    describeDBLogFiles_dbInstanceIdentifier,
    describeDBLogFilesResponse_marker,
    describeDBLogFilesResponse_describeDBLogFiles,
    describeDBLogFilesResponse_httpStatus,

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

    -- ** DescribeDBProxies
    describeDBProxies_marker,
    describeDBProxies_filters,
    describeDBProxies_maxRecords,
    describeDBProxies_dbProxyName,
    describeDBProxiesResponse_dbProxies,
    describeDBProxiesResponse_marker,
    describeDBProxiesResponse_httpStatus,

    -- ** DescribeDBProxyEndpoints
    describeDBProxyEndpoints_marker,
    describeDBProxyEndpoints_filters,
    describeDBProxyEndpoints_maxRecords,
    describeDBProxyEndpoints_dbProxyEndpointName,
    describeDBProxyEndpoints_dbProxyName,
    describeDBProxyEndpointsResponse_marker,
    describeDBProxyEndpointsResponse_dbProxyEndpoints,
    describeDBProxyEndpointsResponse_httpStatus,

    -- ** DescribeDBProxyTargetGroups
    describeDBProxyTargetGroups_marker,
    describeDBProxyTargetGroups_filters,
    describeDBProxyTargetGroups_targetGroupName,
    describeDBProxyTargetGroups_maxRecords,
    describeDBProxyTargetGroups_dbProxyName,
    describeDBProxyTargetGroupsResponse_marker,
    describeDBProxyTargetGroupsResponse_targetGroups,
    describeDBProxyTargetGroupsResponse_httpStatus,

    -- ** DescribeDBProxyTargets
    describeDBProxyTargets_marker,
    describeDBProxyTargets_filters,
    describeDBProxyTargets_targetGroupName,
    describeDBProxyTargets_maxRecords,
    describeDBProxyTargets_dbProxyName,
    describeDBProxyTargetsResponse_marker,
    describeDBProxyTargetsResponse_targets,
    describeDBProxyTargetsResponse_httpStatus,

    -- ** DescribeDBSecurityGroups
    describeDBSecurityGroups_marker,
    describeDBSecurityGroups_filters,
    describeDBSecurityGroups_maxRecords,
    describeDBSecurityGroups_dbSecurityGroupName,
    describeDBSecurityGroupsResponse_marker,
    describeDBSecurityGroupsResponse_dbSecurityGroups,
    describeDBSecurityGroupsResponse_httpStatus,

    -- ** DescribeDBSnapshotAttributes
    describeDBSnapshotAttributes_dbSnapshotIdentifier,
    describeDBSnapshotAttributesResponse_dbSnapshotAttributesResult,
    describeDBSnapshotAttributesResponse_httpStatus,

    -- ** DescribeDBSnapshots
    describeDBSnapshots_includeShared,
    describeDBSnapshots_marker,
    describeDBSnapshots_dbInstanceIdentifier,
    describeDBSnapshots_dbSnapshotIdentifier,
    describeDBSnapshots_filters,
    describeDBSnapshots_maxRecords,
    describeDBSnapshots_includePublic,
    describeDBSnapshots_dbiResourceId,
    describeDBSnapshots_snapshotType,
    describeDBSnapshotsResponse_marker,
    describeDBSnapshotsResponse_dbSnapshots,
    describeDBSnapshotsResponse_httpStatus,

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
    describeEngineDefaultParametersResponse_httpStatus,
    describeEngineDefaultParametersResponse_engineDefaults,

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

    -- ** DescribeExportTasks
    describeExportTasks_marker,
    describeExportTasks_sourceArn,
    describeExportTasks_filters,
    describeExportTasks_sourceType,
    describeExportTasks_maxRecords,
    describeExportTasks_exportTaskIdentifier,
    describeExportTasksResponse_marker,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,

    -- ** DescribeGlobalClusters
    describeGlobalClusters_marker,
    describeGlobalClusters_filters,
    describeGlobalClusters_maxRecords,
    describeGlobalClusters_globalClusterIdentifier,
    describeGlobalClustersResponse_marker,
    describeGlobalClustersResponse_globalClusters,
    describeGlobalClustersResponse_httpStatus,

    -- ** DescribeOptionGroupOptions
    describeOptionGroupOptions_marker,
    describeOptionGroupOptions_filters,
    describeOptionGroupOptions_maxRecords,
    describeOptionGroupOptions_majorEngineVersion,
    describeOptionGroupOptions_engineName,
    describeOptionGroupOptionsResponse_optionGroupOptions,
    describeOptionGroupOptionsResponse_marker,
    describeOptionGroupOptionsResponse_httpStatus,

    -- ** DescribeOptionGroups
    describeOptionGroups_engineName,
    describeOptionGroups_marker,
    describeOptionGroups_filters,
    describeOptionGroups_optionGroupName,
    describeOptionGroups_maxRecords,
    describeOptionGroups_majorEngineVersion,
    describeOptionGroupsResponse_optionGroupsList,
    describeOptionGroupsResponse_marker,
    describeOptionGroupsResponse_httpStatus,

    -- ** DescribeOrderableDBInstanceOptions
    describeOrderableDBInstanceOptions_dbInstanceClass,
    describeOrderableDBInstanceOptions_marker,
    describeOrderableDBInstanceOptions_vpc,
    describeOrderableDBInstanceOptions_filters,
    describeOrderableDBInstanceOptions_availabilityZoneGroup,
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

    -- ** DescribeReservedDBInstances
    describeReservedDBInstances_dbInstanceClass,
    describeReservedDBInstances_marker,
    describeReservedDBInstances_reservedDBInstanceId,
    describeReservedDBInstances_filters,
    describeReservedDBInstances_offeringType,
    describeReservedDBInstances_maxRecords,
    describeReservedDBInstances_duration,
    describeReservedDBInstances_productDescription,
    describeReservedDBInstances_leaseId,
    describeReservedDBInstances_reservedDBInstancesOfferingId,
    describeReservedDBInstances_multiAZ,
    describeReservedDBInstancesResponse_marker,
    describeReservedDBInstancesResponse_reservedDBInstances,
    describeReservedDBInstancesResponse_httpStatus,

    -- ** DescribeReservedDBInstancesOfferings
    describeReservedDBInstancesOfferings_dbInstanceClass,
    describeReservedDBInstancesOfferings_marker,
    describeReservedDBInstancesOfferings_filters,
    describeReservedDBInstancesOfferings_offeringType,
    describeReservedDBInstancesOfferings_maxRecords,
    describeReservedDBInstancesOfferings_duration,
    describeReservedDBInstancesOfferings_productDescription,
    describeReservedDBInstancesOfferings_reservedDBInstancesOfferingId,
    describeReservedDBInstancesOfferings_multiAZ,
    describeReservedDBInstancesOfferingsResponse_marker,
    describeReservedDBInstancesOfferingsResponse_reservedDBInstancesOfferings,
    describeReservedDBInstancesOfferingsResponse_httpStatus,

    -- ** DescribeSourceRegions
    describeSourceRegions_marker,
    describeSourceRegions_filters,
    describeSourceRegions_maxRecords,
    describeSourceRegions_regionName,
    describeSourceRegionsResponse_marker,
    describeSourceRegionsResponse_sourceRegions,
    describeSourceRegionsResponse_httpStatus,

    -- ** DescribeValidDBInstanceModifications
    describeValidDBInstanceModifications_dbInstanceIdentifier,
    describeValidDBInstanceModificationsResponse_validDBInstanceModificationsMessage,
    describeValidDBInstanceModificationsResponse_httpStatus,

    -- ** DownloadDBLogFilePortion
    downloadDBLogFilePortion_marker,
    downloadDBLogFilePortion_numberOfLines,
    downloadDBLogFilePortion_dbInstanceIdentifier,
    downloadDBLogFilePortion_logFileName,
    downloadDBLogFilePortionResponse_marker,
    downloadDBLogFilePortionResponse_additionalDataPending,
    downloadDBLogFilePortionResponse_logFileData,
    downloadDBLogFilePortionResponse_httpStatus,

    -- ** FailoverDBCluster
    failoverDBCluster_targetDBInstanceIdentifier,
    failoverDBCluster_dbClusterIdentifier,
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

    -- ** ModifyActivityStream
    modifyActivityStream_resourceArn,
    modifyActivityStream_auditPolicyState,
    modifyActivityStreamResponse_status,
    modifyActivityStreamResponse_policyStatus,
    modifyActivityStreamResponse_engineNativeAuditFieldsIncluded,
    modifyActivityStreamResponse_kinesisStreamName,
    modifyActivityStreamResponse_mode,
    modifyActivityStreamResponse_kmsKeyId,
    modifyActivityStreamResponse_httpStatus,

    -- ** ModifyCertificates
    modifyCertificates_removeCustomerOverride,
    modifyCertificates_certificateIdentifier,
    modifyCertificatesResponse_certificate,
    modifyCertificatesResponse_httpStatus,

    -- ** ModifyCurrentDBClusterCapacity
    modifyCurrentDBClusterCapacity_timeoutAction,
    modifyCurrentDBClusterCapacity_secondsBeforeTimeout,
    modifyCurrentDBClusterCapacity_capacity,
    modifyCurrentDBClusterCapacity_dbClusterIdentifier,
    modifyCurrentDBClusterCapacityResponse_timeoutAction,
    modifyCurrentDBClusterCapacityResponse_dbClusterIdentifier,
    modifyCurrentDBClusterCapacityResponse_pendingCapacity,
    modifyCurrentDBClusterCapacityResponse_secondsBeforeTimeout,
    modifyCurrentDBClusterCapacityResponse_currentCapacity,
    modifyCurrentDBClusterCapacityResponse_httpStatus,

    -- ** ModifyCustomDBEngineVersion
    modifyCustomDBEngineVersion_status,
    modifyCustomDBEngineVersion_description,
    modifyCustomDBEngineVersion_engine,
    modifyCustomDBEngineVersion_engineVersion,
    dbEngineVersion_validUpgradeTarget,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_supportsBabelfish,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_tagList,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_dbEngineVersionArn,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_status,
    dbEngineVersion_customDBEngineVersionManifest,
    dbEngineVersion_majorEngineVersion,
    dbEngineVersion_databaseInstallationFilesS3BucketName,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_supportsParallelQuery,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_kmsKeyId,
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_databaseInstallationFilesS3Prefix,
    dbEngineVersion_createTime,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_engineVersion,
    dbEngineVersion_dbEngineDescription,

    -- ** ModifyDBCluster
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_port,
    modifyDBCluster_serverlessV2ScalingConfiguration,
    modifyDBCluster_enableGlobalWriteForwarding,
    modifyDBCluster_performanceInsightsRetentionPeriod,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_copyTagsToSnapshot,
    modifyDBCluster_domainIAMRoleName,
    modifyDBCluster_dbInstanceParameterGroupName,
    modifyDBCluster_autoMinorVersionUpgrade,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_allowMajorVersionUpgrade,
    modifyDBCluster_dbClusterInstanceClass,
    modifyDBCluster_domain,
    modifyDBCluster_optionGroupName,
    modifyDBCluster_performanceInsightsKMSKeyId,
    modifyDBCluster_enableIAMDatabaseAuthentication,
    modifyDBCluster_monitoringInterval,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_storageType,
    modifyDBCluster_enableHttpEndpoint,
    modifyDBCluster_backtrackWindow,
    modifyDBCluster_enablePerformanceInsights,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_scalingConfiguration,
    modifyDBCluster_monitoringRoleArn,
    modifyDBCluster_allocatedStorage,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_iops,
    modifyDBCluster_engineVersion,
    modifyDBCluster_networkType,
    modifyDBCluster_dbClusterIdentifier,
    modifyDBClusterResponse_dbCluster,
    modifyDBClusterResponse_httpStatus,

    -- ** ModifyDBClusterEndpoint
    modifyDBClusterEndpoint_staticMembers,
    modifyDBClusterEndpoint_excludedMembers,
    modifyDBClusterEndpoint_endpointType,
    modifyDBClusterEndpoint_dbClusterEndpointIdentifier,
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
    modifyDBInstance_maxAllocatedStorage,
    modifyDBInstance_performanceInsightsRetentionPeriod,
    modifyDBInstance_vpcSecurityGroupIds,
    modifyDBInstance_dbParameterGroupName,
    modifyDBInstance_preferredBackupWindow,
    modifyDBInstance_storageThroughput,
    modifyDBInstance_backupRetentionPeriod,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_copyTagsToSnapshot,
    modifyDBInstance_domainIAMRoleName,
    modifyDBInstance_promotionTier,
    modifyDBInstance_automationMode,
    modifyDBInstance_dbSubnetGroupName,
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_applyImmediately,
    modifyDBInstance_allowMajorVersionUpgrade,
    modifyDBInstance_dbPortNumber,
    modifyDBInstance_resumeFullAutomationModeMinutes,
    modifyDBInstance_domain,
    modifyDBInstance_optionGroupName,
    modifyDBInstance_performanceInsightsKMSKeyId,
    modifyDBInstance_enableIAMDatabaseAuthentication,
    modifyDBInstance_certificateRotationRestart,
    modifyDBInstance_dbSecurityGroups,
    modifyDBInstance_monitoringInterval,
    modifyDBInstance_tdeCredentialPassword,
    modifyDBInstance_masterUserPassword,
    modifyDBInstance_publiclyAccessible,
    modifyDBInstance_storageType,
    modifyDBInstance_processorFeatures,
    modifyDBInstance_enablePerformanceInsights,
    modifyDBInstance_tdeCredentialArn,
    modifyDBInstance_cloudwatchLogsExportConfiguration,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_monitoringRoleArn,
    modifyDBInstance_replicaMode,
    modifyDBInstance_allocatedStorage,
    modifyDBInstance_deletionProtection,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_awsBackupRecoveryPointArn,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_iops,
    modifyDBInstance_engineVersion,
    modifyDBInstance_networkType,
    modifyDBInstance_multiAZ,
    modifyDBInstance_enableCustomerOwnedIp,
    modifyDBInstance_licenseModel,
    modifyDBInstance_useDefaultProcessorFeatures,
    modifyDBInstance_dbInstanceIdentifier,
    modifyDBInstanceResponse_dbInstance,
    modifyDBInstanceResponse_httpStatus,

    -- ** ModifyDBParameterGroup
    modifyDBParameterGroup_dbParameterGroupName,
    modifyDBParameterGroup_parameters,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** ModifyDBProxy
    modifyDBProxy_roleArn,
    modifyDBProxy_requireTLS,
    modifyDBProxy_debugLogging,
    modifyDBProxy_securityGroups,
    modifyDBProxy_auth,
    modifyDBProxy_idleClientTimeout,
    modifyDBProxy_newDBProxyName,
    modifyDBProxy_dbProxyName,
    modifyDBProxyResponse_dbProxy,
    modifyDBProxyResponse_httpStatus,

    -- ** ModifyDBProxyEndpoint
    modifyDBProxyEndpoint_vpcSecurityGroupIds,
    modifyDBProxyEndpoint_newDBProxyEndpointName,
    modifyDBProxyEndpoint_dbProxyEndpointName,
    modifyDBProxyEndpointResponse_dbProxyEndpoint,
    modifyDBProxyEndpointResponse_httpStatus,

    -- ** ModifyDBProxyTargetGroup
    modifyDBProxyTargetGroup_newName,
    modifyDBProxyTargetGroup_connectionPoolConfig,
    modifyDBProxyTargetGroup_targetGroupName,
    modifyDBProxyTargetGroup_dbProxyName,
    modifyDBProxyTargetGroupResponse_dbProxyTargetGroup,
    modifyDBProxyTargetGroupResponse_httpStatus,

    -- ** ModifyDBSnapshot
    modifyDBSnapshot_optionGroupName,
    modifyDBSnapshot_engineVersion,
    modifyDBSnapshot_dbSnapshotIdentifier,
    modifyDBSnapshotResponse_dbSnapshot,
    modifyDBSnapshotResponse_httpStatus,

    -- ** ModifyDBSnapshotAttribute
    modifyDBSnapshotAttribute_valuesToRemove,
    modifyDBSnapshotAttribute_valuesToAdd,
    modifyDBSnapshotAttribute_dbSnapshotIdentifier,
    modifyDBSnapshotAttribute_attributeName,
    modifyDBSnapshotAttributeResponse_dbSnapshotAttributesResult,
    modifyDBSnapshotAttributeResponse_httpStatus,

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
    modifyGlobalCluster_globalClusterIdentifier,
    modifyGlobalCluster_deletionProtection,
    modifyGlobalCluster_newGlobalClusterIdentifier,
    modifyGlobalCluster_engineVersion,
    modifyGlobalClusterResponse_globalCluster,
    modifyGlobalClusterResponse_httpStatus,

    -- ** ModifyOptionGroup
    modifyOptionGroup_optionsToInclude,
    modifyOptionGroup_applyImmediately,
    modifyOptionGroup_optionsToRemove,
    modifyOptionGroup_optionGroupName,
    modifyOptionGroupResponse_optionGroup,
    modifyOptionGroupResponse_httpStatus,

    -- ** PromoteReadReplica
    promoteReadReplica_preferredBackupWindow,
    promoteReadReplica_backupRetentionPeriod,
    promoteReadReplica_dbInstanceIdentifier,
    promoteReadReplicaResponse_dbInstance,
    promoteReadReplicaResponse_httpStatus,

    -- ** PromoteReadReplicaDBCluster
    promoteReadReplicaDBCluster_dbClusterIdentifier,
    promoteReadReplicaDBClusterResponse_dbCluster,
    promoteReadReplicaDBClusterResponse_httpStatus,

    -- ** PurchaseReservedDBInstancesOffering
    purchaseReservedDBInstancesOffering_tags,
    purchaseReservedDBInstancesOffering_dbInstanceCount,
    purchaseReservedDBInstancesOffering_reservedDBInstanceId,
    purchaseReservedDBInstancesOffering_reservedDBInstancesOfferingId,
    purchaseReservedDBInstancesOfferingResponse_reservedDBInstance,
    purchaseReservedDBInstancesOfferingResponse_httpStatus,

    -- ** RebootDBCluster
    rebootDBCluster_dbClusterIdentifier,
    rebootDBClusterResponse_dbCluster,
    rebootDBClusterResponse_httpStatus,

    -- ** RebootDBInstance
    rebootDBInstance_forceFailover,
    rebootDBInstance_dbInstanceIdentifier,
    rebootDBInstanceResponse_dbInstance,
    rebootDBInstanceResponse_httpStatus,

    -- ** RegisterDBProxyTargets
    registerDBProxyTargets_dbInstanceIdentifiers,
    registerDBProxyTargets_targetGroupName,
    registerDBProxyTargets_dbClusterIdentifiers,
    registerDBProxyTargets_dbProxyName,
    registerDBProxyTargetsResponse_dbProxyTargets,
    registerDBProxyTargetsResponse_httpStatus,

    -- ** RemoveFromGlobalCluster
    removeFromGlobalCluster_dbClusterIdentifier,
    removeFromGlobalCluster_globalClusterIdentifier,
    removeFromGlobalClusterResponse_globalCluster,
    removeFromGlobalClusterResponse_httpStatus,

    -- ** RemoveRoleFromDBCluster
    removeRoleFromDBCluster_featureName,
    removeRoleFromDBCluster_dbClusterIdentifier,
    removeRoleFromDBCluster_roleArn,

    -- ** RemoveRoleFromDBInstance
    removeRoleFromDBInstance_dbInstanceIdentifier,
    removeRoleFromDBInstance_roleArn,
    removeRoleFromDBInstance_featureName,

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

    -- ** RestoreDBClusterFromS3
    restoreDBClusterFromS3_tags,
    restoreDBClusterFromS3_port,
    restoreDBClusterFromS3_serverlessV2ScalingConfiguration,
    restoreDBClusterFromS3_vpcSecurityGroupIds,
    restoreDBClusterFromS3_preferredBackupWindow,
    restoreDBClusterFromS3_backupRetentionPeriod,
    restoreDBClusterFromS3_characterSetName,
    restoreDBClusterFromS3_copyTagsToSnapshot,
    restoreDBClusterFromS3_domainIAMRoleName,
    restoreDBClusterFromS3_dbSubnetGroupName,
    restoreDBClusterFromS3_databaseName,
    restoreDBClusterFromS3_domain,
    restoreDBClusterFromS3_optionGroupName,
    restoreDBClusterFromS3_availabilityZones,
    restoreDBClusterFromS3_enableIAMDatabaseAuthentication,
    restoreDBClusterFromS3_enableCloudwatchLogsExports,
    restoreDBClusterFromS3_backtrackWindow,
    restoreDBClusterFromS3_storageEncrypted,
    restoreDBClusterFromS3_kmsKeyId,
    restoreDBClusterFromS3_deletionProtection,
    restoreDBClusterFromS3_preferredMaintenanceWindow,
    restoreDBClusterFromS3_dbClusterParameterGroupName,
    restoreDBClusterFromS3_engineVersion,
    restoreDBClusterFromS3_networkType,
    restoreDBClusterFromS3_s3Prefix,
    restoreDBClusterFromS3_dbClusterIdentifier,
    restoreDBClusterFromS3_engine,
    restoreDBClusterFromS3_masterUsername,
    restoreDBClusterFromS3_masterUserPassword,
    restoreDBClusterFromS3_sourceEngine,
    restoreDBClusterFromS3_sourceEngineVersion,
    restoreDBClusterFromS3_s3BucketName,
    restoreDBClusterFromS3_s3IngestionRoleArn,
    restoreDBClusterFromS3Response_dbCluster,
    restoreDBClusterFromS3Response_httpStatus,

    -- ** RestoreDBClusterFromSnapshot
    restoreDBClusterFromSnapshot_tags,
    restoreDBClusterFromSnapshot_port,
    restoreDBClusterFromSnapshot_serverlessV2ScalingConfiguration,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_copyTagsToSnapshot,
    restoreDBClusterFromSnapshot_domainIAMRoleName,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_dbClusterInstanceClass,
    restoreDBClusterFromSnapshot_databaseName,
    restoreDBClusterFromSnapshot_domain,
    restoreDBClusterFromSnapshot_optionGroupName,
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication,
    restoreDBClusterFromSnapshot_publiclyAccessible,
    restoreDBClusterFromSnapshot_storageType,
    restoreDBClusterFromSnapshot_enableCloudwatchLogsExports,
    restoreDBClusterFromSnapshot_backtrackWindow,
    restoreDBClusterFromSnapshot_scalingConfiguration,
    restoreDBClusterFromSnapshot_engineMode,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_deletionProtection,
    restoreDBClusterFromSnapshot_dbClusterParameterGroupName,
    restoreDBClusterFromSnapshot_iops,
    restoreDBClusterFromSnapshot_engineVersion,
    restoreDBClusterFromSnapshot_networkType,
    restoreDBClusterFromSnapshot_dbClusterIdentifier,
    restoreDBClusterFromSnapshot_snapshotIdentifier,
    restoreDBClusterFromSnapshot_engine,
    restoreDBClusterFromSnapshotResponse_dbCluster,
    restoreDBClusterFromSnapshotResponse_httpStatus,

    -- ** RestoreDBClusterToPointInTime
    restoreDBClusterToPointInTime_tags,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_serverlessV2ScalingConfiguration,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
    restoreDBClusterToPointInTime_copyTagsToSnapshot,
    restoreDBClusterToPointInTime_domainIAMRoleName,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_dbClusterInstanceClass,
    restoreDBClusterToPointInTime_domain,
    restoreDBClusterToPointInTime_optionGroupName,
    restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_publiclyAccessible,
    restoreDBClusterToPointInTime_storageType,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_backtrackWindow,
    restoreDBClusterToPointInTime_scalingConfiguration,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_engineMode,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_dbClusterParameterGroupName,
    restoreDBClusterToPointInTime_iops,
    restoreDBClusterToPointInTime_networkType,
    restoreDBClusterToPointInTime_dbClusterIdentifier,
    restoreDBClusterToPointInTime_sourceDBClusterIdentifier,
    restoreDBClusterToPointInTimeResponse_dbCluster,
    restoreDBClusterToPointInTimeResponse_httpStatus,

    -- ** RestoreDBInstanceFromDBSnapshot
    restoreDBInstanceFromDBSnapshot_tags,
    restoreDBInstanceFromDBSnapshot_port,
    restoreDBInstanceFromDBSnapshot_vpcSecurityGroupIds,
    restoreDBInstanceFromDBSnapshot_dbParameterGroupName,
    restoreDBInstanceFromDBSnapshot_backupTarget,
    restoreDBInstanceFromDBSnapshot_storageThroughput,
    restoreDBInstanceFromDBSnapshot_dbInstanceClass,
    restoreDBInstanceFromDBSnapshot_copyTagsToSnapshot,
    restoreDBInstanceFromDBSnapshot_domainIAMRoleName,
    restoreDBInstanceFromDBSnapshot_dbClusterSnapshotIdentifier,
    restoreDBInstanceFromDBSnapshot_dbSubnetGroupName,
    restoreDBInstanceFromDBSnapshot_autoMinorVersionUpgrade,
    restoreDBInstanceFromDBSnapshot_dbSnapshotIdentifier,
    restoreDBInstanceFromDBSnapshot_domain,
    restoreDBInstanceFromDBSnapshot_optionGroupName,
    restoreDBInstanceFromDBSnapshot_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromDBSnapshot_tdeCredentialPassword,
    restoreDBInstanceFromDBSnapshot_availabilityZone,
    restoreDBInstanceFromDBSnapshot_publiclyAccessible,
    restoreDBInstanceFromDBSnapshot_storageType,
    restoreDBInstanceFromDBSnapshot_enableCloudwatchLogsExports,
    restoreDBInstanceFromDBSnapshot_processorFeatures,
    restoreDBInstanceFromDBSnapshot_tdeCredentialArn,
    restoreDBInstanceFromDBSnapshot_engine,
    restoreDBInstanceFromDBSnapshot_deletionProtection,
    restoreDBInstanceFromDBSnapshot_customIamInstanceProfile,
    restoreDBInstanceFromDBSnapshot_iops,
    restoreDBInstanceFromDBSnapshot_dbName,
    restoreDBInstanceFromDBSnapshot_networkType,
    restoreDBInstanceFromDBSnapshot_multiAZ,
    restoreDBInstanceFromDBSnapshot_enableCustomerOwnedIp,
    restoreDBInstanceFromDBSnapshot_licenseModel,
    restoreDBInstanceFromDBSnapshot_useDefaultProcessorFeatures,
    restoreDBInstanceFromDBSnapshot_dbInstanceIdentifier,
    restoreDBInstanceFromDBSnapshotResponse_dbInstance,
    restoreDBInstanceFromDBSnapshotResponse_httpStatus,

    -- ** RestoreDBInstanceFromS3
    restoreDBInstanceFromS3_tags,
    restoreDBInstanceFromS3_maxAllocatedStorage,
    restoreDBInstanceFromS3_port,
    restoreDBInstanceFromS3_performanceInsightsRetentionPeriod,
    restoreDBInstanceFromS3_vpcSecurityGroupIds,
    restoreDBInstanceFromS3_dbParameterGroupName,
    restoreDBInstanceFromS3_preferredBackupWindow,
    restoreDBInstanceFromS3_storageThroughput,
    restoreDBInstanceFromS3_backupRetentionPeriod,
    restoreDBInstanceFromS3_masterUsername,
    restoreDBInstanceFromS3_copyTagsToSnapshot,
    restoreDBInstanceFromS3_dbSubnetGroupName,
    restoreDBInstanceFromS3_autoMinorVersionUpgrade,
    restoreDBInstanceFromS3_optionGroupName,
    restoreDBInstanceFromS3_performanceInsightsKMSKeyId,
    restoreDBInstanceFromS3_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromS3_dbSecurityGroups,
    restoreDBInstanceFromS3_monitoringInterval,
    restoreDBInstanceFromS3_availabilityZone,
    restoreDBInstanceFromS3_masterUserPassword,
    restoreDBInstanceFromS3_publiclyAccessible,
    restoreDBInstanceFromS3_storageType,
    restoreDBInstanceFromS3_enableCloudwatchLogsExports,
    restoreDBInstanceFromS3_processorFeatures,
    restoreDBInstanceFromS3_enablePerformanceInsights,
    restoreDBInstanceFromS3_monitoringRoleArn,
    restoreDBInstanceFromS3_storageEncrypted,
    restoreDBInstanceFromS3_kmsKeyId,
    restoreDBInstanceFromS3_allocatedStorage,
    restoreDBInstanceFromS3_deletionProtection,
    restoreDBInstanceFromS3_preferredMaintenanceWindow,
    restoreDBInstanceFromS3_iops,
    restoreDBInstanceFromS3_engineVersion,
    restoreDBInstanceFromS3_dbName,
    restoreDBInstanceFromS3_networkType,
    restoreDBInstanceFromS3_multiAZ,
    restoreDBInstanceFromS3_s3Prefix,
    restoreDBInstanceFromS3_licenseModel,
    restoreDBInstanceFromS3_useDefaultProcessorFeatures,
    restoreDBInstanceFromS3_dbInstanceIdentifier,
    restoreDBInstanceFromS3_dbInstanceClass,
    restoreDBInstanceFromS3_engine,
    restoreDBInstanceFromS3_sourceEngine,
    restoreDBInstanceFromS3_sourceEngineVersion,
    restoreDBInstanceFromS3_s3BucketName,
    restoreDBInstanceFromS3_s3IngestionRoleArn,
    restoreDBInstanceFromS3Response_dbInstance,
    restoreDBInstanceFromS3Response_httpStatus,

    -- ** RestoreDBInstanceToPointInTime
    restoreDBInstanceToPointInTime_tags,
    restoreDBInstanceToPointInTime_maxAllocatedStorage,
    restoreDBInstanceToPointInTime_port,
    restoreDBInstanceToPointInTime_vpcSecurityGroupIds,
    restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier,
    restoreDBInstanceToPointInTime_dbParameterGroupName,
    restoreDBInstanceToPointInTime_backupTarget,
    restoreDBInstanceToPointInTime_storageThroughput,
    restoreDBInstanceToPointInTime_dbInstanceClass,
    restoreDBInstanceToPointInTime_restoreTime,
    restoreDBInstanceToPointInTime_sourceDbiResourceId,
    restoreDBInstanceToPointInTime_copyTagsToSnapshot,
    restoreDBInstanceToPointInTime_domainIAMRoleName,
    restoreDBInstanceToPointInTime_dbSubnetGroupName,
    restoreDBInstanceToPointInTime_autoMinorVersionUpgrade,
    restoreDBInstanceToPointInTime_domain,
    restoreDBInstanceToPointInTime_optionGroupName,
    restoreDBInstanceToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBInstanceToPointInTime_tdeCredentialPassword,
    restoreDBInstanceToPointInTime_availabilityZone,
    restoreDBInstanceToPointInTime_publiclyAccessible,
    restoreDBInstanceToPointInTime_storageType,
    restoreDBInstanceToPointInTime_enableCloudwatchLogsExports,
    restoreDBInstanceToPointInTime_sourceDBInstanceAutomatedBackupsArn,
    restoreDBInstanceToPointInTime_processorFeatures,
    restoreDBInstanceToPointInTime_tdeCredentialArn,
    restoreDBInstanceToPointInTime_useLatestRestorableTime,
    restoreDBInstanceToPointInTime_engine,
    restoreDBInstanceToPointInTime_deletionProtection,
    restoreDBInstanceToPointInTime_customIamInstanceProfile,
    restoreDBInstanceToPointInTime_iops,
    restoreDBInstanceToPointInTime_dbName,
    restoreDBInstanceToPointInTime_networkType,
    restoreDBInstanceToPointInTime_multiAZ,
    restoreDBInstanceToPointInTime_enableCustomerOwnedIp,
    restoreDBInstanceToPointInTime_licenseModel,
    restoreDBInstanceToPointInTime_useDefaultProcessorFeatures,
    restoreDBInstanceToPointInTime_targetDBInstanceIdentifier,
    restoreDBInstanceToPointInTimeResponse_dbInstance,
    restoreDBInstanceToPointInTimeResponse_httpStatus,

    -- ** RevokeDBSecurityGroupIngress
    revokeDBSecurityGroupIngress_eC2SecurityGroupId,
    revokeDBSecurityGroupIngress_eC2SecurityGroupOwnerId,
    revokeDBSecurityGroupIngress_eC2SecurityGroupName,
    revokeDBSecurityGroupIngress_cidrip,
    revokeDBSecurityGroupIngress_dbSecurityGroupName,
    revokeDBSecurityGroupIngressResponse_dbSecurityGroup,
    revokeDBSecurityGroupIngressResponse_httpStatus,

    -- ** StartActivityStream
    startActivityStream_applyImmediately,
    startActivityStream_engineNativeAuditFieldsIncluded,
    startActivityStream_resourceArn,
    startActivityStream_mode,
    startActivityStream_kmsKeyId,
    startActivityStreamResponse_applyImmediately,
    startActivityStreamResponse_status,
    startActivityStreamResponse_engineNativeAuditFieldsIncluded,
    startActivityStreamResponse_kinesisStreamName,
    startActivityStreamResponse_mode,
    startActivityStreamResponse_kmsKeyId,
    startActivityStreamResponse_httpStatus,

    -- ** StartDBCluster
    startDBCluster_dbClusterIdentifier,
    startDBClusterResponse_dbCluster,
    startDBClusterResponse_httpStatus,

    -- ** StartDBInstance
    startDBInstance_dbInstanceIdentifier,
    startDBInstanceResponse_dbInstance,
    startDBInstanceResponse_httpStatus,

    -- ** StartDBInstanceAutomatedBackupsReplication
    startDBInstanceAutomatedBackupsReplication_backupRetentionPeriod,
    startDBInstanceAutomatedBackupsReplication_kmsKeyId,
    startDBInstanceAutomatedBackupsReplication_destinationRegion,
    startDBInstanceAutomatedBackupsReplication_preSignedUrl,
    startDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn,
    startDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup,
    startDBInstanceAutomatedBackupsReplicationResponse_httpStatus,

    -- ** StartExportTask
    startExportTask_exportOnly,
    startExportTask_s3Prefix,
    startExportTask_exportTaskIdentifier,
    startExportTask_sourceArn,
    startExportTask_s3BucketName,
    startExportTask_iamRoleArn,
    startExportTask_kmsKeyId,
    exportTask_s3Bucket,
    exportTask_percentProgress,
    exportTask_taskStartTime,
    exportTask_exportOnly,
    exportTask_sourceArn,
    exportTask_totalExtractedDataInGB,
    exportTask_status,
    exportTask_sourceType,
    exportTask_warningMessage,
    exportTask_snapshotTime,
    exportTask_exportTaskIdentifier,
    exportTask_iamRoleArn,
    exportTask_kmsKeyId,
    exportTask_failureCause,
    exportTask_taskEndTime,
    exportTask_s3Prefix,

    -- ** StopActivityStream
    stopActivityStream_applyImmediately,
    stopActivityStream_resourceArn,
    stopActivityStreamResponse_status,
    stopActivityStreamResponse_kinesisStreamName,
    stopActivityStreamResponse_kmsKeyId,
    stopActivityStreamResponse_httpStatus,

    -- ** StopDBCluster
    stopDBCluster_dbClusterIdentifier,
    stopDBClusterResponse_dbCluster,
    stopDBClusterResponse_httpStatus,

    -- ** StopDBInstance
    stopDBInstance_dbSnapshotIdentifier,
    stopDBInstance_dbInstanceIdentifier,
    stopDBInstanceResponse_dbInstance,
    stopDBInstanceResponse_httpStatus,

    -- ** StopDBInstanceAutomatedBackupsReplication
    stopDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn,
    stopDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup,
    stopDBInstanceAutomatedBackupsReplicationResponse_httpStatus,

    -- ** SwitchoverReadReplica
    switchoverReadReplica_dbInstanceIdentifier,
    switchoverReadReplicaResponse_dbInstance,
    switchoverReadReplicaResponse_httpStatus,

    -- * Types

    -- ** AccountQuota
    accountQuota_max,
    accountQuota_used,
    accountQuota_accountQuotaName,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** AvailableProcessorFeature
    availableProcessorFeature_name,
    availableProcessorFeature_defaultValue,
    availableProcessorFeature_allowedValues,

    -- ** Certificate
    certificate_thumbprint,
    certificate_validTill,
    certificate_validFrom,
    certificate_customerOverride,
    certificate_certificateIdentifier,
    certificate_certificateArn,
    certificate_certificateType,
    certificate_customerOverrideValidTill,

    -- ** CharacterSet
    characterSet_characterSetName,
    characterSet_characterSetDescription,

    -- ** CloudwatchLogsExportConfiguration
    cloudwatchLogsExportConfiguration_enableLogTypes,
    cloudwatchLogsExportConfiguration_disableLogTypes,

    -- ** ClusterPendingModifiedValues
    clusterPendingModifiedValues_backupRetentionPeriod,
    clusterPendingModifiedValues_dbClusterIdentifier,
    clusterPendingModifiedValues_pendingCloudwatchLogsExports,
    clusterPendingModifiedValues_masterUserPassword,
    clusterPendingModifiedValues_allocatedStorage,
    clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled,
    clusterPendingModifiedValues_iops,
    clusterPendingModifiedValues_engineVersion,

    -- ** ConnectionPoolConfiguration
    connectionPoolConfiguration_maxIdleConnectionsPercent,
    connectionPoolConfiguration_initQuery,
    connectionPoolConfiguration_connectionBorrowTimeout,
    connectionPoolConfiguration_maxConnectionsPercent,
    connectionPoolConfiguration_sessionPinningFilters,

    -- ** ConnectionPoolConfigurationInfo
    connectionPoolConfigurationInfo_maxIdleConnectionsPercent,
    connectionPoolConfigurationInfo_initQuery,
    connectionPoolConfigurationInfo_connectionBorrowTimeout,
    connectionPoolConfigurationInfo_maxConnectionsPercent,
    connectionPoolConfigurationInfo_sessionPinningFilters,

    -- ** DBCluster
    dbCluster_port,
    dbCluster_serverlessV2ScalingConfiguration,
    dbCluster_cloneGroupId,
    dbCluster_dbClusterArn,
    dbCluster_performanceInsightsRetentionPeriod,
    dbCluster_hostedZoneId,
    dbCluster_percentProgress,
    dbCluster_globalWriteForwardingRequested,
    dbCluster_preferredBackupWindow,
    dbCluster_backupRetentionPeriod,
    dbCluster_characterSetName,
    dbCluster_masterUsername,
    dbCluster_copyTagsToSnapshot,
    dbCluster_activityStreamStatus,
    dbCluster_dbClusterMembers,
    dbCluster_dbClusterParameterGroup,
    dbCluster_autoMinorVersionUpgrade,
    dbCluster_activityStreamMode,
    dbCluster_tagList,
    dbCluster_latestRestorableTime,
    dbCluster_dbClusterInstanceClass,
    dbCluster_databaseName,
    dbCluster_dbClusterIdentifier,
    dbCluster_domainMemberships,
    dbCluster_scalingConfigurationInfo,
    dbCluster_availabilityZones,
    dbCluster_earliestBacktrackTime,
    dbCluster_automaticRestartTime,
    dbCluster_performanceInsightsKMSKeyId,
    dbCluster_crossAccountClone,
    dbCluster_dbClusterOptionGroupMemberships,
    dbCluster_dbSubnetGroup,
    dbCluster_monitoringInterval,
    dbCluster_performanceInsightsEnabled,
    dbCluster_status,
    dbCluster_activityStreamKinesisStreamName,
    dbCluster_httpEndpointEnabled,
    dbCluster_publiclyAccessible,
    dbCluster_storageType,
    dbCluster_backtrackWindow,
    dbCluster_customEndpoints,
    dbCluster_replicationSourceIdentifier,
    dbCluster_monitoringRoleArn,
    dbCluster_backtrackConsumedChangeRecords,
    dbCluster_engineMode,
    dbCluster_storageEncrypted,
    dbCluster_kmsKeyId,
    dbCluster_engine,
    dbCluster_allocatedStorage,
    dbCluster_readerEndpoint,
    dbCluster_earliestRestorableTime,
    dbCluster_iAMDatabaseAuthenticationEnabled,
    dbCluster_deletionProtection,
    dbCluster_pendingModifiedValues,
    dbCluster_preferredMaintenanceWindow,
    dbCluster_endpoint,
    dbCluster_capacity,
    dbCluster_clusterCreateTime,
    dbCluster_readReplicaIdentifiers,
    dbCluster_dbSystemId,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_iops,
    dbCluster_dbClusterResourceId,
    dbCluster_associatedRoles,
    dbCluster_engineVersion,
    dbCluster_networkType,
    dbCluster_multiAZ,
    dbCluster_activityStreamKmsKeyId,
    dbCluster_globalWriteForwardingStatus,
    dbCluster_vpcSecurityGroups,

    -- ** DBClusterBacktrack
    dbClusterBacktrack_backtrackedFrom,
    dbClusterBacktrack_backtrackRequestCreationTime,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_status,
    dbClusterBacktrack_backtrackTo,

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
    dbClusterSnapshot_tagList,
    dbClusterSnapshot_dbClusterIdentifier,
    dbClusterSnapshot_availabilityZones,
    dbClusterSnapshot_sourceDBClusterSnapshotArn,
    dbClusterSnapshot_status,
    dbClusterSnapshot_snapshotCreateTime,
    dbClusterSnapshot_engineMode,
    dbClusterSnapshot_storageEncrypted,
    dbClusterSnapshot_kmsKeyId,
    dbClusterSnapshot_engine,
    dbClusterSnapshot_allocatedStorage,
    dbClusterSnapshot_iAMDatabaseAuthenticationEnabled,
    dbClusterSnapshot_vpcId,
    dbClusterSnapshot_clusterCreateTime,
    dbClusterSnapshot_dbSystemId,
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
    dbEngineVersion_supportsBabelfish,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_tagList,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_dbEngineVersionArn,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_status,
    dbEngineVersion_customDBEngineVersionManifest,
    dbEngineVersion_majorEngineVersion,
    dbEngineVersion_databaseInstallationFilesS3BucketName,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_supportsParallelQuery,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_kmsKeyId,
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_databaseInstallationFilesS3Prefix,
    dbEngineVersion_createTime,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_engineVersion,
    dbEngineVersion_dbEngineDescription,

    -- ** DBInstance
    dbInstance_maxAllocatedStorage,
    dbInstance_dbInstanceAutomatedBackupsReplications,
    dbInstance_listenerEndpoint,
    dbInstance_performanceInsightsRetentionPeriod,
    dbInstance_dbInstanceStatus,
    dbInstance_optionGroupMemberships,
    dbInstance_backupTarget,
    dbInstance_preferredBackupWindow,
    dbInstance_storageThroughput,
    dbInstance_backupRetentionPeriod,
    dbInstance_dbInstanceClass,
    dbInstance_characterSetName,
    dbInstance_masterUsername,
    dbInstance_copyTagsToSnapshot,
    dbInstance_activityStreamStatus,
    dbInstance_activityStreamEngineNativeAuditFieldsIncluded,
    dbInstance_promotionTier,
    dbInstance_automationMode,
    dbInstance_secondaryAvailabilityZone,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_dbInstanceIdentifier,
    dbInstance_dbInstancePort,
    dbInstance_activityStreamMode,
    dbInstance_tagList,
    dbInstance_latestRestorableTime,
    dbInstance_readReplicaDBInstanceIdentifiers,
    dbInstance_enhancedMonitoringResourceArn,
    dbInstance_dbClusterIdentifier,
    dbInstance_domainMemberships,
    dbInstance_timezone,
    dbInstance_automaticRestartTime,
    dbInstance_performanceInsightsKMSKeyId,
    dbInstance_dbSecurityGroups,
    dbInstance_dbSubnetGroup,
    dbInstance_monitoringInterval,
    dbInstance_performanceInsightsEnabled,
    dbInstance_instanceCreateTime,
    dbInstance_activityStreamKinesisStreamName,
    dbInstance_availabilityZone,
    dbInstance_resumeFullAutomationModeTime,
    dbInstance_publiclyAccessible,
    dbInstance_storageType,
    dbInstance_processorFeatures,
    dbInstance_readReplicaDBClusterIdentifiers,
    dbInstance_tdeCredentialArn,
    dbInstance_dbInstanceArn,
    dbInstance_customerOwnedIpEnabled,
    dbInstance_cACertificateIdentifier,
    dbInstance_ncharCharacterSetName,
    dbInstance_monitoringRoleArn,
    dbInstance_replicaMode,
    dbInstance_storageEncrypted,
    dbInstance_kmsKeyId,
    dbInstance_engine,
    dbInstance_allocatedStorage,
    dbInstance_iAMDatabaseAuthenticationEnabled,
    dbInstance_deletionProtection,
    dbInstance_pendingModifiedValues,
    dbInstance_awsBackupRecoveryPointArn,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_endpoint,
    dbInstance_dbiResourceId,
    dbInstance_customIamInstanceProfile,
    dbInstance_dbParameterGroups,
    dbInstance_dbSystemId,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_iops,
    dbInstance_associatedRoles,
    dbInstance_engineVersion,
    dbInstance_dbName,
    dbInstance_networkType,
    dbInstance_activityStreamPolicyStatus,
    dbInstance_multiAZ,
    dbInstance_readReplicaSourceDBInstanceIdentifier,
    dbInstance_activityStreamKmsKeyId,
    dbInstance_licenseModel,
    dbInstance_statusInfos,
    dbInstance_vpcSecurityGroups,

    -- ** DBInstanceAutomatedBackup
    dbInstanceAutomatedBackup_port,
    dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications,
    dbInstanceAutomatedBackup_backupTarget,
    dbInstanceAutomatedBackup_storageThroughput,
    dbInstanceAutomatedBackup_backupRetentionPeriod,
    dbInstanceAutomatedBackup_masterUsername,
    dbInstanceAutomatedBackup_dbInstanceIdentifier,
    dbInstanceAutomatedBackup_optionGroupName,
    dbInstanceAutomatedBackup_timezone,
    dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn,
    dbInstanceAutomatedBackup_instanceCreateTime,
    dbInstanceAutomatedBackup_status,
    dbInstanceAutomatedBackup_availabilityZone,
    dbInstanceAutomatedBackup_storageType,
    dbInstanceAutomatedBackup_region,
    dbInstanceAutomatedBackup_tdeCredentialArn,
    dbInstanceAutomatedBackup_dbInstanceArn,
    dbInstanceAutomatedBackup_encrypted,
    dbInstanceAutomatedBackup_kmsKeyId,
    dbInstanceAutomatedBackup_engine,
    dbInstanceAutomatedBackup_allocatedStorage,
    dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled,
    dbInstanceAutomatedBackup_vpcId,
    dbInstanceAutomatedBackup_dbiResourceId,
    dbInstanceAutomatedBackup_iops,
    dbInstanceAutomatedBackup_engineVersion,
    dbInstanceAutomatedBackup_restoreWindow,
    dbInstanceAutomatedBackup_licenseModel,

    -- ** DBInstanceAutomatedBackupsReplication
    dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn,

    -- ** DBInstanceRole
    dbInstanceRole_roleArn,
    dbInstanceRole_featureName,
    dbInstanceRole_status,

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

    -- ** DBProxy
    dbProxy_dbProxyArn,
    dbProxy_vpcSecurityGroupIds,
    dbProxy_engineFamily,
    dbProxy_roleArn,
    dbProxy_requireTLS,
    dbProxy_status,
    dbProxy_debugLogging,
    dbProxy_vpcSubnetIds,
    dbProxy_updatedDate,
    dbProxy_createdDate,
    dbProxy_endpoint,
    dbProxy_vpcId,
    dbProxy_auth,
    dbProxy_dbProxyName,
    dbProxy_idleClientTimeout,

    -- ** DBProxyEndpoint
    dbProxyEndpoint_vpcSecurityGroupIds,
    dbProxyEndpoint_status,
    dbProxyEndpoint_isDefault,
    dbProxyEndpoint_vpcSubnetIds,
    dbProxyEndpoint_dbProxyEndpointName,
    dbProxyEndpoint_createdDate,
    dbProxyEndpoint_dbProxyEndpointArn,
    dbProxyEndpoint_endpoint,
    dbProxyEndpoint_vpcId,
    dbProxyEndpoint_dbProxyName,
    dbProxyEndpoint_targetRole,

    -- ** DBProxyTarget
    dbProxyTarget_port,
    dbProxyTarget_type,
    dbProxyTarget_targetHealth,
    dbProxyTarget_targetArn,
    dbProxyTarget_rdsResourceId,
    dbProxyTarget_role,
    dbProxyTarget_endpoint,
    dbProxyTarget_trackedClusterId,

    -- ** DBProxyTargetGroup
    dbProxyTargetGroup_targetGroupName,
    dbProxyTargetGroup_status,
    dbProxyTargetGroup_targetGroupArn,
    dbProxyTargetGroup_isDefault,
    dbProxyTargetGroup_updatedDate,
    dbProxyTargetGroup_createdDate,
    dbProxyTargetGroup_connectionPoolConfig,
    dbProxyTargetGroup_dbProxyName,

    -- ** DBSecurityGroup
    dbSecurityGroup_ownerId,
    dbSecurityGroup_dbSecurityGroupDescription,
    dbSecurityGroup_dbSecurityGroupName,
    dbSecurityGroup_eC2SecurityGroups,
    dbSecurityGroup_dbSecurityGroupArn,
    dbSecurityGroup_iPRanges,
    dbSecurityGroup_vpcId,

    -- ** DBSecurityGroupMembership
    dbSecurityGroupMembership_status,
    dbSecurityGroupMembership_dbSecurityGroupName,

    -- ** DBSnapshot
    dbSnapshot_port,
    dbSnapshot_originalSnapshotCreateTime,
    dbSnapshot_percentProgress,
    dbSnapshot_storageThroughput,
    dbSnapshot_masterUsername,
    dbSnapshot_sourceRegion,
    dbSnapshot_dbInstanceIdentifier,
    dbSnapshot_tagList,
    dbSnapshot_dbSnapshotIdentifier,
    dbSnapshot_optionGroupName,
    dbSnapshot_sourceDBSnapshotIdentifier,
    dbSnapshot_timezone,
    dbSnapshot_instanceCreateTime,
    dbSnapshot_status,
    dbSnapshot_availabilityZone,
    dbSnapshot_snapshotCreateTime,
    dbSnapshot_snapshotDatabaseTime,
    dbSnapshot_storageType,
    dbSnapshot_processorFeatures,
    dbSnapshot_tdeCredentialArn,
    dbSnapshot_dbSnapshotArn,
    dbSnapshot_encrypted,
    dbSnapshot_kmsKeyId,
    dbSnapshot_engine,
    dbSnapshot_allocatedStorage,
    dbSnapshot_iAMDatabaseAuthenticationEnabled,
    dbSnapshot_vpcId,
    dbSnapshot_snapshotTarget,
    dbSnapshot_dbiResourceId,
    dbSnapshot_iops,
    dbSnapshot_engineVersion,
    dbSnapshot_licenseModel,
    dbSnapshot_snapshotType,

    -- ** DBSnapshotAttribute
    dbSnapshotAttribute_attributeValues,
    dbSnapshotAttribute_attributeName,

    -- ** DBSnapshotAttributesResult
    dbSnapshotAttributesResult_dbSnapshotAttributes,
    dbSnapshotAttributesResult_dbSnapshotIdentifier,

    -- ** DBSubnetGroup
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_subnetGroupStatus,
    dbSubnetGroup_subnets,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_vpcId,
    dbSubnetGroup_supportedNetworkTypes,

    -- ** DescribeDBLogFilesDetails
    describeDBLogFilesDetails_lastWritten,
    describeDBLogFilesDetails_logFileName,
    describeDBLogFilesDetails_size,

    -- ** DomainMembership
    domainMembership_domain,
    domainMembership_fqdn,
    domainMembership_status,
    domainMembership_iAMRoleName,

    -- ** DoubleRange
    doubleRange_from,
    doubleRange_to,

    -- ** EC2SecurityGroup
    eC2SecurityGroup_eC2SecurityGroupId,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_status,
    eC2SecurityGroup_eC2SecurityGroupName,

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

    -- ** ExportTask
    exportTask_s3Bucket,
    exportTask_percentProgress,
    exportTask_taskStartTime,
    exportTask_exportOnly,
    exportTask_sourceArn,
    exportTask_totalExtractedDataInGB,
    exportTask_status,
    exportTask_sourceType,
    exportTask_warningMessage,
    exportTask_snapshotTime,
    exportTask_exportTaskIdentifier,
    exportTask_iamRoleArn,
    exportTask_kmsKeyId,
    exportTask_failureCause,
    exportTask_taskEndTime,
    exportTask_s3Prefix,

    -- ** FailoverState
    failoverState_status,
    failoverState_fromDbClusterArn,
    failoverState_toDbClusterArn,

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
    globalCluster_failoverState,

    -- ** GlobalClusterMember
    globalClusterMember_dbClusterArn,
    globalClusterMember_isWriter,
    globalClusterMember_readers,
    globalClusterMember_globalWriteForwardingStatus,

    -- ** IPRange
    iPRange_status,
    iPRange_cidrip,

    -- ** MinimumEngineVersionPerAllowedValue
    minimumEngineVersionPerAllowedValue_minimumEngineVersion,
    minimumEngineVersionPerAllowedValue_allowedValue,

    -- ** Option
    option_port,
    option_dbSecurityGroupMemberships,
    option_optionVersion,
    option_persistent,
    option_optionName,
    option_optionDescription,
    option_permanent,
    option_vpcSecurityGroupMemberships,
    option_optionSettings,

    -- ** OptionConfiguration
    optionConfiguration_port,
    optionConfiguration_dbSecurityGroupMemberships,
    optionConfiguration_optionVersion,
    optionConfiguration_vpcSecurityGroupMemberships,
    optionConfiguration_optionSettings,
    optionConfiguration_optionName,

    -- ** OptionGroup
    optionGroup_allowsVpcAndNonVpcInstanceMemberships,
    optionGroup_engineName,
    optionGroup_optionGroupName,
    optionGroup_optionGroupDescription,
    optionGroup_majorEngineVersion,
    optionGroup_options,
    optionGroup_vpcId,
    optionGroup_optionGroupArn,

    -- ** OptionGroupMembership
    optionGroupMembership_optionGroupName,
    optionGroupMembership_status,

    -- ** OptionGroupOption
    optionGroupOption_name,
    optionGroupOption_engineName,
    optionGroupOption_vpcOnly,
    optionGroupOption_requiresAutoMinorEngineVersionUpgrade,
    optionGroupOption_optionsDependedOn,
    optionGroupOption_optionsConflictsWith,
    optionGroupOption_supportsOptionVersionDowngrade,
    optionGroupOption_persistent,
    optionGroupOption_description,
    optionGroupOption_optionGroupOptionSettings,
    optionGroupOption_majorEngineVersion,
    optionGroupOption_minimumRequiredMinorEngineVersion,
    optionGroupOption_optionGroupOptionVersions,
    optionGroupOption_permanent,
    optionGroupOption_defaultPort,
    optionGroupOption_portRequired,

    -- ** OptionGroupOptionSetting
    optionGroupOptionSetting_settingDescription,
    optionGroupOptionSetting_applyType,
    optionGroupOptionSetting_defaultValue,
    optionGroupOptionSetting_isModifiable,
    optionGroupOptionSetting_settingName,
    optionGroupOptionSetting_allowedValues,
    optionGroupOptionSetting_isRequired,
    optionGroupOptionSetting_minimumEngineVersionPerAllowedValue,

    -- ** OptionSetting
    optionSetting_name,
    optionSetting_applyType,
    optionSetting_defaultValue,
    optionSetting_isModifiable,
    optionSetting_description,
    optionSetting_isCollection,
    optionSetting_allowedValues,
    optionSetting_dataType,
    optionSetting_value,

    -- ** OptionVersion
    optionVersion_isDefault,
    optionVersion_version,

    -- ** OrderableDBInstanceOption
    orderableDBInstanceOption_supportsStorageEncryption,
    orderableDBInstanceOption_supportsClusters,
    orderableDBInstanceOption_maxStorageSize,
    orderableDBInstanceOption_multiAZCapable,
    orderableDBInstanceOption_maxStorageThroughputPerIops,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_vpc,
    orderableDBInstanceOption_supportsPerformanceInsights,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_availabilityZoneGroup,
    orderableDBInstanceOption_minIopsPerDbInstance,
    orderableDBInstanceOption_supportedActivityStreamModes,
    orderableDBInstanceOption_supportedEngineModes,
    orderableDBInstanceOption_minStorageSize,
    orderableDBInstanceOption_storageType,
    orderableDBInstanceOption_outpostCapable,
    orderableDBInstanceOption_supportsIops,
    orderableDBInstanceOption_maxIopsPerDbInstance,
    orderableDBInstanceOption_minStorageThroughputPerDbInstance,
    orderableDBInstanceOption_supportsIAMDatabaseAuthentication,
    orderableDBInstanceOption_supportsEnhancedMonitoring,
    orderableDBInstanceOption_availableProcessorFeatures,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_readReplicaCapable,
    orderableDBInstanceOption_supportsStorageAutoscaling,
    orderableDBInstanceOption_supportsGlobalDatabases,
    orderableDBInstanceOption_maxIopsPerGib,
    orderableDBInstanceOption_minStorageThroughputPerIops,
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_supportsKerberosAuthentication,
    orderableDBInstanceOption_minIopsPerGib,
    orderableDBInstanceOption_licenseModel,
    orderableDBInstanceOption_supportsStorageThroughput,
    orderableDBInstanceOption_maxStorageThroughputPerDbInstance,
    orderableDBInstanceOption_supportedNetworkTypes,

    -- ** Outpost
    outpost_arn,

    -- ** Parameter
    parameter_parameterValue,
    parameter_applyMethod,
    parameter_applyType,
    parameter_supportedEngineModes,
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
    pendingModifiedValues_storageThroughput,
    pendingModifiedValues_backupRetentionPeriod,
    pendingModifiedValues_dbInstanceClass,
    pendingModifiedValues_automationMode,
    pendingModifiedValues_dbSubnetGroupName,
    pendingModifiedValues_dbInstanceIdentifier,
    pendingModifiedValues_pendingCloudwatchLogsExports,
    pendingModifiedValues_resumeFullAutomationModeTime,
    pendingModifiedValues_masterUserPassword,
    pendingModifiedValues_storageType,
    pendingModifiedValues_processorFeatures,
    pendingModifiedValues_cACertificateIdentifier,
    pendingModifiedValues_allocatedStorage,
    pendingModifiedValues_iAMDatabaseAuthenticationEnabled,
    pendingModifiedValues_iops,
    pendingModifiedValues_engineVersion,
    pendingModifiedValues_multiAZ,
    pendingModifiedValues_licenseModel,

    -- ** ProcessorFeature
    processorFeature_name,
    processorFeature_value,

    -- ** Range
    range_from,
    range_to,
    range_step,

    -- ** RecurringCharge
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- ** ReservedDBInstance
    reservedDBInstance_dbInstanceClass,
    reservedDBInstance_dbInstanceCount,
    reservedDBInstance_reservedDBInstanceId,
    reservedDBInstance_recurringCharges,
    reservedDBInstance_state,
    reservedDBInstance_offeringType,
    reservedDBInstance_reservedDBInstanceArn,
    reservedDBInstance_duration,
    reservedDBInstance_currencyCode,
    reservedDBInstance_productDescription,
    reservedDBInstance_leaseId,
    reservedDBInstance_reservedDBInstancesOfferingId,
    reservedDBInstance_fixedPrice,
    reservedDBInstance_startTime,
    reservedDBInstance_usagePrice,
    reservedDBInstance_multiAZ,

    -- ** ReservedDBInstancesOffering
    reservedDBInstancesOffering_dbInstanceClass,
    reservedDBInstancesOffering_recurringCharges,
    reservedDBInstancesOffering_offeringType,
    reservedDBInstancesOffering_duration,
    reservedDBInstancesOffering_currencyCode,
    reservedDBInstancesOffering_productDescription,
    reservedDBInstancesOffering_reservedDBInstancesOfferingId,
    reservedDBInstancesOffering_fixedPrice,
    reservedDBInstancesOffering_usagePrice,
    reservedDBInstancesOffering_multiAZ,

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_resourceIdentifier,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,

    -- ** RestoreWindow
    restoreWindow_latestTime,
    restoreWindow_earliestTime,

    -- ** ScalingConfiguration
    scalingConfiguration_timeoutAction,
    scalingConfiguration_secondsBeforeTimeout,
    scalingConfiguration_secondsUntilAutoPause,
    scalingConfiguration_maxCapacity,
    scalingConfiguration_autoPause,
    scalingConfiguration_minCapacity,

    -- ** ScalingConfigurationInfo
    scalingConfigurationInfo_timeoutAction,
    scalingConfigurationInfo_secondsBeforeTimeout,
    scalingConfigurationInfo_secondsUntilAutoPause,
    scalingConfigurationInfo_maxCapacity,
    scalingConfigurationInfo_autoPause,
    scalingConfigurationInfo_minCapacity,

    -- ** ServerlessV2ScalingConfiguration
    serverlessV2ScalingConfiguration_maxCapacity,
    serverlessV2ScalingConfiguration_minCapacity,

    -- ** ServerlessV2ScalingConfigurationInfo
    serverlessV2ScalingConfigurationInfo_maxCapacity,
    serverlessV2ScalingConfigurationInfo_minCapacity,

    -- ** SourceRegion
    sourceRegion_status,
    sourceRegion_supportsDBInstanceAutomatedBackupsReplication,
    sourceRegion_regionName,
    sourceRegion_endpoint,

    -- ** Subnet
    subnet_subnetOutpost,
    subnet_subnetIdentifier,
    subnet_subnetStatus,
    subnet_subnetAvailabilityZone,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TargetHealth
    targetHealth_state,
    targetHealth_description,
    targetHealth_reason,

    -- ** Timezone
    timezone_timezoneName,

    -- ** UpgradeTarget
    upgradeTarget_supportsBabelfish,
    upgradeTarget_autoUpgrade,
    upgradeTarget_supportedEngineModes,
    upgradeTarget_description,
    upgradeTarget_supportsParallelQuery,
    upgradeTarget_engine,
    upgradeTarget_supportsGlobalDatabases,
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,

    -- ** UserAuthConfig
    userAuthConfig_userName,
    userAuthConfig_description,
    userAuthConfig_iAMAuth,
    userAuthConfig_secretArn,
    userAuthConfig_authScheme,

    -- ** UserAuthConfigInfo
    userAuthConfigInfo_userName,
    userAuthConfigInfo_description,
    userAuthConfigInfo_iAMAuth,
    userAuthConfigInfo_secretArn,
    userAuthConfigInfo_authScheme,

    -- ** ValidDBInstanceModificationsMessage
    validDBInstanceModificationsMessage_storage,
    validDBInstanceModificationsMessage_validProcessorFeatures,

    -- ** ValidStorageOptions
    validStorageOptions_storageSize,
    validStorageOptions_iopsToStorageRatio,
    validStorageOptions_provisionedIops,
    validStorageOptions_provisionedStorageThroughput,
    validStorageOptions_storageType,
    validStorageOptions_supportsStorageAutoscaling,
    validStorageOptions_storageThroughputToIopsRatio,

    -- ** VpcSecurityGroupMembership
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import Amazonka.RDS.AddRoleToDBCluster
import Amazonka.RDS.AddRoleToDBInstance
import Amazonka.RDS.AddSourceIdentifierToSubscription
import Amazonka.RDS.AddTagsToResource
import Amazonka.RDS.ApplyPendingMaintenanceAction
import Amazonka.RDS.AuthorizeDBSecurityGroupIngress
import Amazonka.RDS.BacktrackDBCluster
import Amazonka.RDS.CancelExportTask
import Amazonka.RDS.CopyDBClusterParameterGroup
import Amazonka.RDS.CopyDBClusterSnapshot
import Amazonka.RDS.CopyDBParameterGroup
import Amazonka.RDS.CopyDBSnapshot
import Amazonka.RDS.CopyOptionGroup
import Amazonka.RDS.CreateCustomDBEngineVersion
import Amazonka.RDS.CreateDBCluster
import Amazonka.RDS.CreateDBClusterEndpoint
import Amazonka.RDS.CreateDBClusterParameterGroup
import Amazonka.RDS.CreateDBClusterSnapshot
import Amazonka.RDS.CreateDBInstance
import Amazonka.RDS.CreateDBInstanceReadReplica
import Amazonka.RDS.CreateDBParameterGroup
import Amazonka.RDS.CreateDBProxy
import Amazonka.RDS.CreateDBProxyEndpoint
import Amazonka.RDS.CreateDBSecurityGroup
import Amazonka.RDS.CreateDBSnapshot
import Amazonka.RDS.CreateDBSubnetGroup
import Amazonka.RDS.CreateEventSubscription
import Amazonka.RDS.CreateGlobalCluster
import Amazonka.RDS.CreateOptionGroup
import Amazonka.RDS.DeleteCustomDBEngineVersion
import Amazonka.RDS.DeleteDBCluster
import Amazonka.RDS.DeleteDBClusterEndpoint
import Amazonka.RDS.DeleteDBClusterParameterGroup
import Amazonka.RDS.DeleteDBClusterSnapshot
import Amazonka.RDS.DeleteDBInstance
import Amazonka.RDS.DeleteDBInstanceAutomatedBackup
import Amazonka.RDS.DeleteDBParameterGroup
import Amazonka.RDS.DeleteDBProxy
import Amazonka.RDS.DeleteDBProxyEndpoint
import Amazonka.RDS.DeleteDBSecurityGroup
import Amazonka.RDS.DeleteDBSnapshot
import Amazonka.RDS.DeleteDBSubnetGroup
import Amazonka.RDS.DeleteEventSubscription
import Amazonka.RDS.DeleteGlobalCluster
import Amazonka.RDS.DeleteOptionGroup
import Amazonka.RDS.DeregisterDBProxyTargets
import Amazonka.RDS.DescribeAccountAttributes
import Amazonka.RDS.DescribeCertificates
import Amazonka.RDS.DescribeDBClusterBacktracks
import Amazonka.RDS.DescribeDBClusterEndpoints
import Amazonka.RDS.DescribeDBClusterParameterGroups
import Amazonka.RDS.DescribeDBClusterParameters
import Amazonka.RDS.DescribeDBClusterSnapshotAttributes
import Amazonka.RDS.DescribeDBClusterSnapshots
import Amazonka.RDS.DescribeDBClusters
import Amazonka.RDS.DescribeDBEngineVersions
import Amazonka.RDS.DescribeDBInstanceAutomatedBackups
import Amazonka.RDS.DescribeDBInstances
import Amazonka.RDS.DescribeDBLogFiles
import Amazonka.RDS.DescribeDBParameterGroups
import Amazonka.RDS.DescribeDBParameters
import Amazonka.RDS.DescribeDBProxies
import Amazonka.RDS.DescribeDBProxyEndpoints
import Amazonka.RDS.DescribeDBProxyTargetGroups
import Amazonka.RDS.DescribeDBProxyTargets
import Amazonka.RDS.DescribeDBSecurityGroups
import Amazonka.RDS.DescribeDBSnapshotAttributes
import Amazonka.RDS.DescribeDBSnapshots
import Amazonka.RDS.DescribeDBSubnetGroups
import Amazonka.RDS.DescribeEngineDefaultClusterParameters
import Amazonka.RDS.DescribeEngineDefaultParameters
import Amazonka.RDS.DescribeEventCategories
import Amazonka.RDS.DescribeEventSubscriptions
import Amazonka.RDS.DescribeEvents
import Amazonka.RDS.DescribeExportTasks
import Amazonka.RDS.DescribeGlobalClusters
import Amazonka.RDS.DescribeOptionGroupOptions
import Amazonka.RDS.DescribeOptionGroups
import Amazonka.RDS.DescribeOrderableDBInstanceOptions
import Amazonka.RDS.DescribePendingMaintenanceActions
import Amazonka.RDS.DescribeReservedDBInstances
import Amazonka.RDS.DescribeReservedDBInstancesOfferings
import Amazonka.RDS.DescribeSourceRegions
import Amazonka.RDS.DescribeValidDBInstanceModifications
import Amazonka.RDS.DownloadDBLogFilePortion
import Amazonka.RDS.FailoverDBCluster
import Amazonka.RDS.FailoverGlobalCluster
import Amazonka.RDS.ListTagsForResource
import Amazonka.RDS.ModifyActivityStream
import Amazonka.RDS.ModifyCertificates
import Amazonka.RDS.ModifyCurrentDBClusterCapacity
import Amazonka.RDS.ModifyCustomDBEngineVersion
import Amazonka.RDS.ModifyDBCluster
import Amazonka.RDS.ModifyDBClusterEndpoint
import Amazonka.RDS.ModifyDBClusterParameterGroup
import Amazonka.RDS.ModifyDBClusterSnapshotAttribute
import Amazonka.RDS.ModifyDBInstance
import Amazonka.RDS.ModifyDBParameterGroup
import Amazonka.RDS.ModifyDBProxy
import Amazonka.RDS.ModifyDBProxyEndpoint
import Amazonka.RDS.ModifyDBProxyTargetGroup
import Amazonka.RDS.ModifyDBSnapshot
import Amazonka.RDS.ModifyDBSnapshotAttribute
import Amazonka.RDS.ModifyDBSubnetGroup
import Amazonka.RDS.ModifyEventSubscription
import Amazonka.RDS.ModifyGlobalCluster
import Amazonka.RDS.ModifyOptionGroup
import Amazonka.RDS.PromoteReadReplica
import Amazonka.RDS.PromoteReadReplicaDBCluster
import Amazonka.RDS.PurchaseReservedDBInstancesOffering
import Amazonka.RDS.RebootDBCluster
import Amazonka.RDS.RebootDBInstance
import Amazonka.RDS.RegisterDBProxyTargets
import Amazonka.RDS.RemoveFromGlobalCluster
import Amazonka.RDS.RemoveRoleFromDBCluster
import Amazonka.RDS.RemoveRoleFromDBInstance
import Amazonka.RDS.RemoveSourceIdentifierFromSubscription
import Amazonka.RDS.RemoveTagsFromResource
import Amazonka.RDS.ResetDBClusterParameterGroup
import Amazonka.RDS.ResetDBParameterGroup
import Amazonka.RDS.RestoreDBClusterFromS3
import Amazonka.RDS.RestoreDBClusterFromSnapshot
import Amazonka.RDS.RestoreDBClusterToPointInTime
import Amazonka.RDS.RestoreDBInstanceFromDBSnapshot
import Amazonka.RDS.RestoreDBInstanceFromS3
import Amazonka.RDS.RestoreDBInstanceToPointInTime
import Amazonka.RDS.RevokeDBSecurityGroupIngress
import Amazonka.RDS.StartActivityStream
import Amazonka.RDS.StartDBCluster
import Amazonka.RDS.StartDBInstance
import Amazonka.RDS.StartDBInstanceAutomatedBackupsReplication
import Amazonka.RDS.StartExportTask
import Amazonka.RDS.StopActivityStream
import Amazonka.RDS.StopDBCluster
import Amazonka.RDS.StopDBInstance
import Amazonka.RDS.StopDBInstanceAutomatedBackupsReplication
import Amazonka.RDS.SwitchoverReadReplica
import Amazonka.RDS.Types.AccountQuota
import Amazonka.RDS.Types.AvailabilityZone
import Amazonka.RDS.Types.AvailableProcessorFeature
import Amazonka.RDS.Types.Certificate
import Amazonka.RDS.Types.CharacterSet
import Amazonka.RDS.Types.CloudwatchLogsExportConfiguration
import Amazonka.RDS.Types.ClusterPendingModifiedValues
import Amazonka.RDS.Types.ConnectionPoolConfiguration
import Amazonka.RDS.Types.ConnectionPoolConfigurationInfo
import Amazonka.RDS.Types.DBCluster
import Amazonka.RDS.Types.DBClusterBacktrack
import Amazonka.RDS.Types.DBClusterEndpoint
import Amazonka.RDS.Types.DBClusterMember
import Amazonka.RDS.Types.DBClusterOptionGroupStatus
import Amazonka.RDS.Types.DBClusterParameterGroup
import Amazonka.RDS.Types.DBClusterParameterGroupNameMessage
import Amazonka.RDS.Types.DBClusterRole
import Amazonka.RDS.Types.DBClusterSnapshot
import Amazonka.RDS.Types.DBClusterSnapshotAttribute
import Amazonka.RDS.Types.DBClusterSnapshotAttributesResult
import Amazonka.RDS.Types.DBEngineVersion
import Amazonka.RDS.Types.DBInstance
import Amazonka.RDS.Types.DBInstanceAutomatedBackup
import Amazonka.RDS.Types.DBInstanceAutomatedBackupsReplication
import Amazonka.RDS.Types.DBInstanceRole
import Amazonka.RDS.Types.DBInstanceStatusInfo
import Amazonka.RDS.Types.DBParameterGroup
import Amazonka.RDS.Types.DBParameterGroupNameMessage
import Amazonka.RDS.Types.DBParameterGroupStatus
import Amazonka.RDS.Types.DBProxy
import Amazonka.RDS.Types.DBProxyEndpoint
import Amazonka.RDS.Types.DBProxyTarget
import Amazonka.RDS.Types.DBProxyTargetGroup
import Amazonka.RDS.Types.DBSecurityGroup
import Amazonka.RDS.Types.DBSecurityGroupMembership
import Amazonka.RDS.Types.DBSnapshot
import Amazonka.RDS.Types.DBSnapshotAttribute
import Amazonka.RDS.Types.DBSnapshotAttributesResult
import Amazonka.RDS.Types.DBSubnetGroup
import Amazonka.RDS.Types.DescribeDBLogFilesDetails
import Amazonka.RDS.Types.DomainMembership
import Amazonka.RDS.Types.DoubleRange
import Amazonka.RDS.Types.EC2SecurityGroup
import Amazonka.RDS.Types.Endpoint
import Amazonka.RDS.Types.EngineDefaults
import Amazonka.RDS.Types.Event
import Amazonka.RDS.Types.EventCategoriesMap
import Amazonka.RDS.Types.EventSubscription
import Amazonka.RDS.Types.ExportTask
import Amazonka.RDS.Types.FailoverState
import Amazonka.RDS.Types.Filter
import Amazonka.RDS.Types.GlobalCluster
import Amazonka.RDS.Types.GlobalClusterMember
import Amazonka.RDS.Types.IPRange
import Amazonka.RDS.Types.MinimumEngineVersionPerAllowedValue
import Amazonka.RDS.Types.Option
import Amazonka.RDS.Types.OptionConfiguration
import Amazonka.RDS.Types.OptionGroup
import Amazonka.RDS.Types.OptionGroupMembership
import Amazonka.RDS.Types.OptionGroupOption
import Amazonka.RDS.Types.OptionGroupOptionSetting
import Amazonka.RDS.Types.OptionSetting
import Amazonka.RDS.Types.OptionVersion
import Amazonka.RDS.Types.OrderableDBInstanceOption
import Amazonka.RDS.Types.Outpost
import Amazonka.RDS.Types.Parameter
import Amazonka.RDS.Types.PendingCloudwatchLogsExports
import Amazonka.RDS.Types.PendingMaintenanceAction
import Amazonka.RDS.Types.PendingModifiedValues
import Amazonka.RDS.Types.ProcessorFeature
import Amazonka.RDS.Types.Range
import Amazonka.RDS.Types.RecurringCharge
import Amazonka.RDS.Types.ReservedDBInstance
import Amazonka.RDS.Types.ReservedDBInstancesOffering
import Amazonka.RDS.Types.ResourcePendingMaintenanceActions
import Amazonka.RDS.Types.RestoreWindow
import Amazonka.RDS.Types.ScalingConfiguration
import Amazonka.RDS.Types.ScalingConfigurationInfo
import Amazonka.RDS.Types.ServerlessV2ScalingConfiguration
import Amazonka.RDS.Types.ServerlessV2ScalingConfigurationInfo
import Amazonka.RDS.Types.SourceRegion
import Amazonka.RDS.Types.Subnet
import Amazonka.RDS.Types.Tag
import Amazonka.RDS.Types.TargetHealth
import Amazonka.RDS.Types.Timezone
import Amazonka.RDS.Types.UpgradeTarget
import Amazonka.RDS.Types.UserAuthConfig
import Amazonka.RDS.Types.UserAuthConfigInfo
import Amazonka.RDS.Types.ValidDBInstanceModificationsMessage
import Amazonka.RDS.Types.ValidStorageOptions
import Amazonka.RDS.Types.VpcSecurityGroupMembership
