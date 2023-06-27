{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDS.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    authorizeDBSecurityGroupIngress_cidrip,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupId,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupName,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupOwnerId,
    authorizeDBSecurityGroupIngress_dbSecurityGroupName,
    authorizeDBSecurityGroupIngressResponse_dbSecurityGroup,
    authorizeDBSecurityGroupIngressResponse_httpStatus,

    -- ** BacktrackDBCluster
    backtrackDBCluster_force,
    backtrackDBCluster_useEarliestTimeOnPointInTimeUnavailable,
    backtrackDBCluster_dbClusterIdentifier,
    backtrackDBCluster_backtrackTo,
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_backtrackRequestCreationTime,
    dbClusterBacktrack_backtrackTo,
    dbClusterBacktrack_backtrackedFrom,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_status,

    -- ** CancelExportTask
    cancelExportTask_exportTaskIdentifier,
    exportTask_exportOnly,
    exportTask_exportTaskIdentifier,
    exportTask_failureCause,
    exportTask_iamRoleArn,
    exportTask_kmsKeyId,
    exportTask_percentProgress,
    exportTask_s3Bucket,
    exportTask_s3Prefix,
    exportTask_snapshotTime,
    exportTask_sourceArn,
    exportTask_sourceType,
    exportTask_status,
    exportTask_taskEndTime,
    exportTask_taskStartTime,
    exportTask_totalExtractedDataInGB,
    exportTask_warningMessage,

    -- ** CopyDBClusterParameterGroup
    copyDBClusterParameterGroup_tags,
    copyDBClusterParameterGroup_sourceDBClusterParameterGroupIdentifier,
    copyDBClusterParameterGroup_targetDBClusterParameterGroupIdentifier,
    copyDBClusterParameterGroup_targetDBClusterParameterGroupDescription,
    copyDBClusterParameterGroupResponse_dbClusterParameterGroup,
    copyDBClusterParameterGroupResponse_httpStatus,

    -- ** CopyDBClusterSnapshot
    copyDBClusterSnapshot_copyTags,
    copyDBClusterSnapshot_destinationRegion,
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

    -- ** CopyDBSnapshot
    copyDBSnapshot_copyOptionGroup,
    copyDBSnapshot_copyTags,
    copyDBSnapshot_destinationRegion,
    copyDBSnapshot_kmsKeyId,
    copyDBSnapshot_optionGroupName,
    copyDBSnapshot_preSignedUrl,
    copyDBSnapshot_tags,
    copyDBSnapshot_targetCustomAvailabilityZone,
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

    -- ** CreateBlueGreenDeployment
    createBlueGreenDeployment_tags,
    createBlueGreenDeployment_targetDBClusterParameterGroupName,
    createBlueGreenDeployment_targetDBParameterGroupName,
    createBlueGreenDeployment_targetEngineVersion,
    createBlueGreenDeployment_blueGreenDeploymentName,
    createBlueGreenDeployment_source,
    createBlueGreenDeploymentResponse_blueGreenDeployment,
    createBlueGreenDeploymentResponse_httpStatus,

    -- ** CreateCustomDBEngineVersion
    createCustomDBEngineVersion_databaseInstallationFilesS3BucketName,
    createCustomDBEngineVersion_databaseInstallationFilesS3Prefix,
    createCustomDBEngineVersion_description,
    createCustomDBEngineVersion_imageId,
    createCustomDBEngineVersion_kmsKeyId,
    createCustomDBEngineVersion_manifest,
    createCustomDBEngineVersion_tags,
    createCustomDBEngineVersion_engine,
    createCustomDBEngineVersion_engineVersion,
    dbEngineVersion_createTime,
    dbEngineVersion_customDBEngineVersionManifest,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_dbEngineMediaType,
    dbEngineVersion_dbEngineVersionArn,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_databaseInstallationFilesS3BucketName,
    dbEngineVersion_databaseInstallationFilesS3Prefix,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_engine,
    dbEngineVersion_engineVersion,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_image,
    dbEngineVersion_kmsKeyId,
    dbEngineVersion_majorEngineVersion,
    dbEngineVersion_status,
    dbEngineVersion_supportedCACertificateIdentifiers,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsBabelfish,
    dbEngineVersion_supportsCertificateRotationWithoutRestart,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_supportsParallelQuery,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_tagList,
    dbEngineVersion_validUpgradeTarget,

    -- ** CreateDBCluster
    createDBCluster_allocatedStorage,
    createDBCluster_autoMinorVersionUpgrade,
    createDBCluster_availabilityZones,
    createDBCluster_backtrackWindow,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_characterSetName,
    createDBCluster_copyTagsToSnapshot,
    createDBCluster_dbClusterInstanceClass,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_dbSystemId,
    createDBCluster_databaseName,
    createDBCluster_deletionProtection,
    createDBCluster_destinationRegion,
    createDBCluster_domain,
    createDBCluster_domainIAMRoleName,
    createDBCluster_enableCloudwatchLogsExports,
    createDBCluster_enableGlobalWriteForwarding,
    createDBCluster_enableHttpEndpoint,
    createDBCluster_enableIAMDatabaseAuthentication,
    createDBCluster_enablePerformanceInsights,
    createDBCluster_engineMode,
    createDBCluster_engineVersion,
    createDBCluster_globalClusterIdentifier,
    createDBCluster_iops,
    createDBCluster_kmsKeyId,
    createDBCluster_manageMasterUserPassword,
    createDBCluster_masterUserPassword,
    createDBCluster_masterUserSecretKmsKeyId,
    createDBCluster_masterUsername,
    createDBCluster_monitoringInterval,
    createDBCluster_monitoringRoleArn,
    createDBCluster_networkType,
    createDBCluster_optionGroupName,
    createDBCluster_performanceInsightsKMSKeyId,
    createDBCluster_performanceInsightsRetentionPeriod,
    createDBCluster_port,
    createDBCluster_preSignedUrl,
    createDBCluster_preferredBackupWindow,
    createDBCluster_preferredMaintenanceWindow,
    createDBCluster_publiclyAccessible,
    createDBCluster_replicationSourceIdentifier,
    createDBCluster_scalingConfiguration,
    createDBCluster_serverlessV2ScalingConfiguration,
    createDBCluster_storageEncrypted,
    createDBCluster_storageType,
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
    createDBInstance_backupTarget,
    createDBInstance_cACertificateIdentifier,
    createDBInstance_characterSetName,
    createDBInstance_copyTagsToSnapshot,
    createDBInstance_customIamInstanceProfile,
    createDBInstance_dbClusterIdentifier,
    createDBInstance_dbName,
    createDBInstance_dbParameterGroupName,
    createDBInstance_dbSecurityGroups,
    createDBInstance_dbSubnetGroupName,
    createDBInstance_deletionProtection,
    createDBInstance_domain,
    createDBInstance_domainIAMRoleName,
    createDBInstance_enableCloudwatchLogsExports,
    createDBInstance_enableCustomerOwnedIp,
    createDBInstance_enableIAMDatabaseAuthentication,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_engineVersion,
    createDBInstance_iops,
    createDBInstance_kmsKeyId,
    createDBInstance_licenseModel,
    createDBInstance_manageMasterUserPassword,
    createDBInstance_masterUserPassword,
    createDBInstance_masterUserSecretKmsKeyId,
    createDBInstance_masterUsername,
    createDBInstance_maxAllocatedStorage,
    createDBInstance_monitoringInterval,
    createDBInstance_monitoringRoleArn,
    createDBInstance_multiAZ,
    createDBInstance_ncharCharacterSetName,
    createDBInstance_networkType,
    createDBInstance_optionGroupName,
    createDBInstance_performanceInsightsKMSKeyId,
    createDBInstance_performanceInsightsRetentionPeriod,
    createDBInstance_port,
    createDBInstance_preferredBackupWindow,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_processorFeatures,
    createDBInstance_promotionTier,
    createDBInstance_publiclyAccessible,
    createDBInstance_storageEncrypted,
    createDBInstance_storageThroughput,
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

    -- ** CreateDBInstanceReadReplica
    createDBInstanceReadReplica_allocatedStorage,
    createDBInstanceReadReplica_autoMinorVersionUpgrade,
    createDBInstanceReadReplica_availabilityZone,
    createDBInstanceReadReplica_copyTagsToSnapshot,
    createDBInstanceReadReplica_customIamInstanceProfile,
    createDBInstanceReadReplica_dbInstanceClass,
    createDBInstanceReadReplica_dbParameterGroupName,
    createDBInstanceReadReplica_dbSubnetGroupName,
    createDBInstanceReadReplica_deletionProtection,
    createDBInstanceReadReplica_destinationRegion,
    createDBInstanceReadReplica_domain,
    createDBInstanceReadReplica_domainIAMRoleName,
    createDBInstanceReadReplica_enableCloudwatchLogsExports,
    createDBInstanceReadReplica_enableCustomerOwnedIp,
    createDBInstanceReadReplica_enableIAMDatabaseAuthentication,
    createDBInstanceReadReplica_enablePerformanceInsights,
    createDBInstanceReadReplica_iops,
    createDBInstanceReadReplica_kmsKeyId,
    createDBInstanceReadReplica_maxAllocatedStorage,
    createDBInstanceReadReplica_monitoringInterval,
    createDBInstanceReadReplica_monitoringRoleArn,
    createDBInstanceReadReplica_multiAZ,
    createDBInstanceReadReplica_networkType,
    createDBInstanceReadReplica_optionGroupName,
    createDBInstanceReadReplica_performanceInsightsKMSKeyId,
    createDBInstanceReadReplica_performanceInsightsRetentionPeriod,
    createDBInstanceReadReplica_port,
    createDBInstanceReadReplica_preSignedUrl,
    createDBInstanceReadReplica_processorFeatures,
    createDBInstanceReadReplica_publiclyAccessible,
    createDBInstanceReadReplica_replicaMode,
    createDBInstanceReadReplica_sourceDBClusterIdentifier,
    createDBInstanceReadReplica_sourceDBInstanceIdentifier,
    createDBInstanceReadReplica_storageThroughput,
    createDBInstanceReadReplica_storageType,
    createDBInstanceReadReplica_tags,
    createDBInstanceReadReplica_useDefaultProcessorFeatures,
    createDBInstanceReadReplica_vpcSecurityGroupIds,
    createDBInstanceReadReplica_dbInstanceIdentifier,
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
    createDBProxy_debugLogging,
    createDBProxy_idleClientTimeout,
    createDBProxy_requireTLS,
    createDBProxy_tags,
    createDBProxy_vpcSecurityGroupIds,
    createDBProxy_dbProxyName,
    createDBProxy_engineFamily,
    createDBProxy_auth,
    createDBProxy_roleArn,
    createDBProxy_vpcSubnetIds,
    createDBProxyResponse_dbProxy,
    createDBProxyResponse_httpStatus,

    -- ** CreateDBProxyEndpoint
    createDBProxyEndpoint_tags,
    createDBProxyEndpoint_targetRole,
    createDBProxyEndpoint_vpcSecurityGroupIds,
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
    createGlobalCluster_globalClusterIdentifier,
    createGlobalCluster_sourceDBClusterIdentifier,
    createGlobalCluster_storageEncrypted,
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

    -- ** DeleteBlueGreenDeployment
    deleteBlueGreenDeployment_deleteTarget,
    deleteBlueGreenDeployment_blueGreenDeploymentIdentifier,
    deleteBlueGreenDeploymentResponse_blueGreenDeployment,
    deleteBlueGreenDeploymentResponse_httpStatus,

    -- ** DeleteCustomDBEngineVersion
    deleteCustomDBEngineVersion_engine,
    deleteCustomDBEngineVersion_engineVersion,
    dbEngineVersion_createTime,
    dbEngineVersion_customDBEngineVersionManifest,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_dbEngineMediaType,
    dbEngineVersion_dbEngineVersionArn,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_databaseInstallationFilesS3BucketName,
    dbEngineVersion_databaseInstallationFilesS3Prefix,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_engine,
    dbEngineVersion_engineVersion,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_image,
    dbEngineVersion_kmsKeyId,
    dbEngineVersion_majorEngineVersion,
    dbEngineVersion_status,
    dbEngineVersion_supportedCACertificateIdentifiers,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsBabelfish,
    dbEngineVersion_supportsCertificateRotationWithoutRestart,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_supportsParallelQuery,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_tagList,
    dbEngineVersion_validUpgradeTarget,

    -- ** DeleteDBCluster
    deleteDBCluster_finalDBSnapshotIdentifier,
    deleteDBCluster_skipFinalSnapshot,
    deleteDBCluster_dbClusterIdentifier,
    deleteDBClusterResponse_dbCluster,
    deleteDBClusterResponse_httpStatus,

    -- ** DeleteDBClusterEndpoint
    deleteDBClusterEndpoint_dbClusterEndpointIdentifier,
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

    -- ** DeleteDBClusterParameterGroup
    deleteDBClusterParameterGroup_dbClusterParameterGroupName,

    -- ** DeleteDBClusterSnapshot
    deleteDBClusterSnapshot_dbClusterSnapshotIdentifier,
    deleteDBClusterSnapshotResponse_dbClusterSnapshot,
    deleteDBClusterSnapshotResponse_httpStatus,

    -- ** DeleteDBInstance
    deleteDBInstance_deleteAutomatedBackups,
    deleteDBInstance_finalDBSnapshotIdentifier,
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
    deregisterDBProxyTargets_dbClusterIdentifiers,
    deregisterDBProxyTargets_dbInstanceIdentifiers,
    deregisterDBProxyTargets_targetGroupName,
    deregisterDBProxyTargets_dbProxyName,
    deregisterDBProxyTargetsResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributesResponse_accountQuotas,
    describeAccountAttributesResponse_httpStatus,

    -- ** DescribeBlueGreenDeployments
    describeBlueGreenDeployments_blueGreenDeploymentIdentifier,
    describeBlueGreenDeployments_filters,
    describeBlueGreenDeployments_marker,
    describeBlueGreenDeployments_maxRecords,
    describeBlueGreenDeploymentsResponse_blueGreenDeployments,
    describeBlueGreenDeploymentsResponse_marker,
    describeBlueGreenDeploymentsResponse_httpStatus,

    -- ** DescribeCertificates
    describeCertificates_certificateIdentifier,
    describeCertificates_filters,
    describeCertificates_marker,
    describeCertificates_maxRecords,
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_marker,
    describeCertificatesResponse_httpStatus,

    -- ** DescribeDBClusterBacktracks
    describeDBClusterBacktracks_backtrackIdentifier,
    describeDBClusterBacktracks_filters,
    describeDBClusterBacktracks_marker,
    describeDBClusterBacktracks_maxRecords,
    describeDBClusterBacktracks_dbClusterIdentifier,
    describeDBClusterBacktracksResponse_dbClusterBacktracks,
    describeDBClusterBacktracksResponse_marker,
    describeDBClusterBacktracksResponse_httpStatus,

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
    describeDBClusters_includeShared,
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
    describeDBEngineVersions_includeAll,
    describeDBEngineVersions_listSupportedCharacterSets,
    describeDBEngineVersions_listSupportedTimezones,
    describeDBEngineVersions_marker,
    describeDBEngineVersions_maxRecords,
    describeDBEngineVersionsResponse_dbEngineVersions,
    describeDBEngineVersionsResponse_marker,
    describeDBEngineVersionsResponse_httpStatus,

    -- ** DescribeDBInstanceAutomatedBackups
    describeDBInstanceAutomatedBackups_dbInstanceAutomatedBackupsArn,
    describeDBInstanceAutomatedBackups_dbInstanceIdentifier,
    describeDBInstanceAutomatedBackups_dbiResourceId,
    describeDBInstanceAutomatedBackups_filters,
    describeDBInstanceAutomatedBackups_marker,
    describeDBInstanceAutomatedBackups_maxRecords,
    describeDBInstanceAutomatedBackupsResponse_dbInstanceAutomatedBackups,
    describeDBInstanceAutomatedBackupsResponse_marker,
    describeDBInstanceAutomatedBackupsResponse_httpStatus,

    -- ** DescribeDBInstances
    describeDBInstances_dbInstanceIdentifier,
    describeDBInstances_filters,
    describeDBInstances_marker,
    describeDBInstances_maxRecords,
    describeDBInstancesResponse_dbInstances,
    describeDBInstancesResponse_marker,
    describeDBInstancesResponse_httpStatus,

    -- ** DescribeDBLogFiles
    describeDBLogFiles_fileLastWritten,
    describeDBLogFiles_fileSize,
    describeDBLogFiles_filenameContains,
    describeDBLogFiles_filters,
    describeDBLogFiles_marker,
    describeDBLogFiles_maxRecords,
    describeDBLogFiles_dbInstanceIdentifier,
    describeDBLogFilesResponse_describeDBLogFiles,
    describeDBLogFilesResponse_marker,
    describeDBLogFilesResponse_httpStatus,

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

    -- ** DescribeDBProxies
    describeDBProxies_dbProxyName,
    describeDBProxies_filters,
    describeDBProxies_marker,
    describeDBProxies_maxRecords,
    describeDBProxiesResponse_dbProxies,
    describeDBProxiesResponse_marker,
    describeDBProxiesResponse_httpStatus,

    -- ** DescribeDBProxyEndpoints
    describeDBProxyEndpoints_dbProxyEndpointName,
    describeDBProxyEndpoints_dbProxyName,
    describeDBProxyEndpoints_filters,
    describeDBProxyEndpoints_marker,
    describeDBProxyEndpoints_maxRecords,
    describeDBProxyEndpointsResponse_dbProxyEndpoints,
    describeDBProxyEndpointsResponse_marker,
    describeDBProxyEndpointsResponse_httpStatus,

    -- ** DescribeDBProxyTargetGroups
    describeDBProxyTargetGroups_filters,
    describeDBProxyTargetGroups_marker,
    describeDBProxyTargetGroups_maxRecords,
    describeDBProxyTargetGroups_targetGroupName,
    describeDBProxyTargetGroups_dbProxyName,
    describeDBProxyTargetGroupsResponse_marker,
    describeDBProxyTargetGroupsResponse_targetGroups,
    describeDBProxyTargetGroupsResponse_httpStatus,

    -- ** DescribeDBProxyTargets
    describeDBProxyTargets_filters,
    describeDBProxyTargets_marker,
    describeDBProxyTargets_maxRecords,
    describeDBProxyTargets_targetGroupName,
    describeDBProxyTargets_dbProxyName,
    describeDBProxyTargetsResponse_marker,
    describeDBProxyTargetsResponse_targets,
    describeDBProxyTargetsResponse_httpStatus,

    -- ** DescribeDBSecurityGroups
    describeDBSecurityGroups_dbSecurityGroupName,
    describeDBSecurityGroups_filters,
    describeDBSecurityGroups_marker,
    describeDBSecurityGroups_maxRecords,
    describeDBSecurityGroupsResponse_dbSecurityGroups,
    describeDBSecurityGroupsResponse_marker,
    describeDBSecurityGroupsResponse_httpStatus,

    -- ** DescribeDBSnapshotAttributes
    describeDBSnapshotAttributes_dbSnapshotIdentifier,
    describeDBSnapshotAttributesResponse_dbSnapshotAttributesResult,
    describeDBSnapshotAttributesResponse_httpStatus,

    -- ** DescribeDBSnapshots
    describeDBSnapshots_dbInstanceIdentifier,
    describeDBSnapshots_dbSnapshotIdentifier,
    describeDBSnapshots_dbiResourceId,
    describeDBSnapshots_filters,
    describeDBSnapshots_includePublic,
    describeDBSnapshots_includeShared,
    describeDBSnapshots_marker,
    describeDBSnapshots_maxRecords,
    describeDBSnapshots_snapshotType,
    describeDBSnapshotsResponse_dbSnapshots,
    describeDBSnapshotsResponse_marker,
    describeDBSnapshotsResponse_httpStatus,

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
    describeEngineDefaultParametersResponse_httpStatus,
    describeEngineDefaultParametersResponse_engineDefaults,

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

    -- ** DescribeExportTasks
    describeExportTasks_exportTaskIdentifier,
    describeExportTasks_filters,
    describeExportTasks_marker,
    describeExportTasks_maxRecords,
    describeExportTasks_sourceArn,
    describeExportTasks_sourceType,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_marker,
    describeExportTasksResponse_httpStatus,

    -- ** DescribeGlobalClusters
    describeGlobalClusters_filters,
    describeGlobalClusters_globalClusterIdentifier,
    describeGlobalClusters_marker,
    describeGlobalClusters_maxRecords,
    describeGlobalClustersResponse_globalClusters,
    describeGlobalClustersResponse_marker,
    describeGlobalClustersResponse_httpStatus,

    -- ** DescribeOptionGroupOptions
    describeOptionGroupOptions_filters,
    describeOptionGroupOptions_majorEngineVersion,
    describeOptionGroupOptions_marker,
    describeOptionGroupOptions_maxRecords,
    describeOptionGroupOptions_engineName,
    describeOptionGroupOptionsResponse_marker,
    describeOptionGroupOptionsResponse_optionGroupOptions,
    describeOptionGroupOptionsResponse_httpStatus,

    -- ** DescribeOptionGroups
    describeOptionGroups_engineName,
    describeOptionGroups_filters,
    describeOptionGroups_majorEngineVersion,
    describeOptionGroups_marker,
    describeOptionGroups_maxRecords,
    describeOptionGroups_optionGroupName,
    describeOptionGroupsResponse_marker,
    describeOptionGroupsResponse_optionGroupsList,
    describeOptionGroupsResponse_httpStatus,

    -- ** DescribeOrderableDBInstanceOptions
    describeOrderableDBInstanceOptions_availabilityZoneGroup,
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

    -- ** DescribeReservedDBInstances
    describeReservedDBInstances_dbInstanceClass,
    describeReservedDBInstances_duration,
    describeReservedDBInstances_filters,
    describeReservedDBInstances_leaseId,
    describeReservedDBInstances_marker,
    describeReservedDBInstances_maxRecords,
    describeReservedDBInstances_multiAZ,
    describeReservedDBInstances_offeringType,
    describeReservedDBInstances_productDescription,
    describeReservedDBInstances_reservedDBInstanceId,
    describeReservedDBInstances_reservedDBInstancesOfferingId,
    describeReservedDBInstancesResponse_marker,
    describeReservedDBInstancesResponse_reservedDBInstances,
    describeReservedDBInstancesResponse_httpStatus,

    -- ** DescribeReservedDBInstancesOfferings
    describeReservedDBInstancesOfferings_dbInstanceClass,
    describeReservedDBInstancesOfferings_duration,
    describeReservedDBInstancesOfferings_filters,
    describeReservedDBInstancesOfferings_marker,
    describeReservedDBInstancesOfferings_maxRecords,
    describeReservedDBInstancesOfferings_multiAZ,
    describeReservedDBInstancesOfferings_offeringType,
    describeReservedDBInstancesOfferings_productDescription,
    describeReservedDBInstancesOfferings_reservedDBInstancesOfferingId,
    describeReservedDBInstancesOfferingsResponse_marker,
    describeReservedDBInstancesOfferingsResponse_reservedDBInstancesOfferings,
    describeReservedDBInstancesOfferingsResponse_httpStatus,

    -- ** DescribeSourceRegions
    describeSourceRegions_filters,
    describeSourceRegions_marker,
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
    downloadDBLogFilePortionResponse_additionalDataPending,
    downloadDBLogFilePortionResponse_logFileData,
    downloadDBLogFilePortionResponse_marker,
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
    modifyActivityStream_auditPolicyState,
    modifyActivityStream_resourceArn,
    modifyActivityStreamResponse_engineNativeAuditFieldsIncluded,
    modifyActivityStreamResponse_kinesisStreamName,
    modifyActivityStreamResponse_kmsKeyId,
    modifyActivityStreamResponse_mode,
    modifyActivityStreamResponse_policyStatus,
    modifyActivityStreamResponse_status,
    modifyActivityStreamResponse_httpStatus,

    -- ** ModifyCertificates
    modifyCertificates_certificateIdentifier,
    modifyCertificates_removeCustomerOverride,
    modifyCertificatesResponse_certificate,
    modifyCertificatesResponse_httpStatus,

    -- ** ModifyCurrentDBClusterCapacity
    modifyCurrentDBClusterCapacity_capacity,
    modifyCurrentDBClusterCapacity_secondsBeforeTimeout,
    modifyCurrentDBClusterCapacity_timeoutAction,
    modifyCurrentDBClusterCapacity_dbClusterIdentifier,
    modifyCurrentDBClusterCapacityResponse_currentCapacity,
    modifyCurrentDBClusterCapacityResponse_dbClusterIdentifier,
    modifyCurrentDBClusterCapacityResponse_pendingCapacity,
    modifyCurrentDBClusterCapacityResponse_secondsBeforeTimeout,
    modifyCurrentDBClusterCapacityResponse_timeoutAction,
    modifyCurrentDBClusterCapacityResponse_httpStatus,

    -- ** ModifyCustomDBEngineVersion
    modifyCustomDBEngineVersion_description,
    modifyCustomDBEngineVersion_status,
    modifyCustomDBEngineVersion_engine,
    modifyCustomDBEngineVersion_engineVersion,
    dbEngineVersion_createTime,
    dbEngineVersion_customDBEngineVersionManifest,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_dbEngineMediaType,
    dbEngineVersion_dbEngineVersionArn,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_databaseInstallationFilesS3BucketName,
    dbEngineVersion_databaseInstallationFilesS3Prefix,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_engine,
    dbEngineVersion_engineVersion,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_image,
    dbEngineVersion_kmsKeyId,
    dbEngineVersion_majorEngineVersion,
    dbEngineVersion_status,
    dbEngineVersion_supportedCACertificateIdentifiers,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsBabelfish,
    dbEngineVersion_supportsCertificateRotationWithoutRestart,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_supportsParallelQuery,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_tagList,
    dbEngineVersion_validUpgradeTarget,

    -- ** ModifyDBCluster
    modifyDBCluster_allocatedStorage,
    modifyDBCluster_allowEngineModeChange,
    modifyDBCluster_allowMajorVersionUpgrade,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_autoMinorVersionUpgrade,
    modifyDBCluster_backtrackWindow,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_copyTagsToSnapshot,
    modifyDBCluster_dbClusterInstanceClass,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_dbInstanceParameterGroupName,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_domain,
    modifyDBCluster_domainIAMRoleName,
    modifyDBCluster_enableGlobalWriteForwarding,
    modifyDBCluster_enableHttpEndpoint,
    modifyDBCluster_enableIAMDatabaseAuthentication,
    modifyDBCluster_enablePerformanceInsights,
    modifyDBCluster_engineMode,
    modifyDBCluster_engineVersion,
    modifyDBCluster_iops,
    modifyDBCluster_manageMasterUserPassword,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_masterUserSecretKmsKeyId,
    modifyDBCluster_monitoringInterval,
    modifyDBCluster_monitoringRoleArn,
    modifyDBCluster_networkType,
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_optionGroupName,
    modifyDBCluster_performanceInsightsKMSKeyId,
    modifyDBCluster_performanceInsightsRetentionPeriod,
    modifyDBCluster_port,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_rotateMasterUserPassword,
    modifyDBCluster_scalingConfiguration,
    modifyDBCluster_serverlessV2ScalingConfiguration,
    modifyDBCluster_storageType,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_dbClusterIdentifier,
    modifyDBClusterResponse_dbCluster,
    modifyDBClusterResponse_httpStatus,

    -- ** ModifyDBClusterEndpoint
    modifyDBClusterEndpoint_endpointType,
    modifyDBClusterEndpoint_excludedMembers,
    modifyDBClusterEndpoint_staticMembers,
    modifyDBClusterEndpoint_dbClusterEndpointIdentifier,
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
    modifyDBInstance_automationMode,
    modifyDBInstance_awsBackupRecoveryPointArn,
    modifyDBInstance_backupRetentionPeriod,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_certificateRotationRestart,
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
    modifyDBInstance_enableCustomerOwnedIp,
    modifyDBInstance_enableIAMDatabaseAuthentication,
    modifyDBInstance_enablePerformanceInsights,
    modifyDBInstance_engine,
    modifyDBInstance_engineVersion,
    modifyDBInstance_iops,
    modifyDBInstance_licenseModel,
    modifyDBInstance_manageMasterUserPassword,
    modifyDBInstance_masterUserPassword,
    modifyDBInstance_masterUserSecretKmsKeyId,
    modifyDBInstance_maxAllocatedStorage,
    modifyDBInstance_monitoringInterval,
    modifyDBInstance_monitoringRoleArn,
    modifyDBInstance_multiAZ,
    modifyDBInstance_networkType,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_optionGroupName,
    modifyDBInstance_performanceInsightsKMSKeyId,
    modifyDBInstance_performanceInsightsRetentionPeriod,
    modifyDBInstance_preferredBackupWindow,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_processorFeatures,
    modifyDBInstance_promotionTier,
    modifyDBInstance_publiclyAccessible,
    modifyDBInstance_replicaMode,
    modifyDBInstance_resumeFullAutomationModeMinutes,
    modifyDBInstance_rotateMasterUserPassword,
    modifyDBInstance_storageThroughput,
    modifyDBInstance_storageType,
    modifyDBInstance_tdeCredentialArn,
    modifyDBInstance_tdeCredentialPassword,
    modifyDBInstance_useDefaultProcessorFeatures,
    modifyDBInstance_vpcSecurityGroupIds,
    modifyDBInstance_dbInstanceIdentifier,
    modifyDBInstanceResponse_dbInstance,
    modifyDBInstanceResponse_httpStatus,

    -- ** ModifyDBParameterGroup
    modifyDBParameterGroup_dbParameterGroupName,
    modifyDBParameterGroup_parameters,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** ModifyDBProxy
    modifyDBProxy_auth,
    modifyDBProxy_debugLogging,
    modifyDBProxy_idleClientTimeout,
    modifyDBProxy_newDBProxyName,
    modifyDBProxy_requireTLS,
    modifyDBProxy_roleArn,
    modifyDBProxy_securityGroups,
    modifyDBProxy_dbProxyName,
    modifyDBProxyResponse_dbProxy,
    modifyDBProxyResponse_httpStatus,

    -- ** ModifyDBProxyEndpoint
    modifyDBProxyEndpoint_newDBProxyEndpointName,
    modifyDBProxyEndpoint_vpcSecurityGroupIds,
    modifyDBProxyEndpoint_dbProxyEndpointName,
    modifyDBProxyEndpointResponse_dbProxyEndpoint,
    modifyDBProxyEndpointResponse_httpStatus,

    -- ** ModifyDBProxyTargetGroup
    modifyDBProxyTargetGroup_connectionPoolConfig,
    modifyDBProxyTargetGroup_newName,
    modifyDBProxyTargetGroup_targetGroupName,
    modifyDBProxyTargetGroup_dbProxyName,
    modifyDBProxyTargetGroupResponse_dbProxyTargetGroup,
    modifyDBProxyTargetGroupResponse_httpStatus,

    -- ** ModifyDBSnapshot
    modifyDBSnapshot_engineVersion,
    modifyDBSnapshot_optionGroupName,
    modifyDBSnapshot_dbSnapshotIdentifier,
    modifyDBSnapshotResponse_dbSnapshot,
    modifyDBSnapshotResponse_httpStatus,

    -- ** ModifyDBSnapshotAttribute
    modifyDBSnapshotAttribute_valuesToAdd,
    modifyDBSnapshotAttribute_valuesToRemove,
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
    modifyGlobalCluster_globalClusterIdentifier,
    modifyGlobalCluster_newGlobalClusterIdentifier,
    modifyGlobalClusterResponse_globalCluster,
    modifyGlobalClusterResponse_httpStatus,

    -- ** ModifyOptionGroup
    modifyOptionGroup_applyImmediately,
    modifyOptionGroup_optionsToInclude,
    modifyOptionGroup_optionsToRemove,
    modifyOptionGroup_optionGroupName,
    modifyOptionGroupResponse_optionGroup,
    modifyOptionGroupResponse_httpStatus,

    -- ** PromoteReadReplica
    promoteReadReplica_backupRetentionPeriod,
    promoteReadReplica_preferredBackupWindow,
    promoteReadReplica_dbInstanceIdentifier,
    promoteReadReplicaResponse_dbInstance,
    promoteReadReplicaResponse_httpStatus,

    -- ** PromoteReadReplicaDBCluster
    promoteReadReplicaDBCluster_dbClusterIdentifier,
    promoteReadReplicaDBClusterResponse_dbCluster,
    promoteReadReplicaDBClusterResponse_httpStatus,

    -- ** PurchaseReservedDBInstancesOffering
    purchaseReservedDBInstancesOffering_dbInstanceCount,
    purchaseReservedDBInstancesOffering_reservedDBInstanceId,
    purchaseReservedDBInstancesOffering_tags,
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
    registerDBProxyTargets_dbClusterIdentifiers,
    registerDBProxyTargets_dbInstanceIdentifiers,
    registerDBProxyTargets_targetGroupName,
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
    resetDBClusterParameterGroup_parameters,
    resetDBClusterParameterGroup_resetAllParameters,
    resetDBClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** ResetDBParameterGroup
    resetDBParameterGroup_parameters,
    resetDBParameterGroup_resetAllParameters,
    resetDBParameterGroup_dbParameterGroupName,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** RestoreDBClusterFromS3
    restoreDBClusterFromS3_availabilityZones,
    restoreDBClusterFromS3_backtrackWindow,
    restoreDBClusterFromS3_backupRetentionPeriod,
    restoreDBClusterFromS3_characterSetName,
    restoreDBClusterFromS3_copyTagsToSnapshot,
    restoreDBClusterFromS3_dbClusterParameterGroupName,
    restoreDBClusterFromS3_dbSubnetGroupName,
    restoreDBClusterFromS3_databaseName,
    restoreDBClusterFromS3_deletionProtection,
    restoreDBClusterFromS3_domain,
    restoreDBClusterFromS3_domainIAMRoleName,
    restoreDBClusterFromS3_enableCloudwatchLogsExports,
    restoreDBClusterFromS3_enableIAMDatabaseAuthentication,
    restoreDBClusterFromS3_engineVersion,
    restoreDBClusterFromS3_kmsKeyId,
    restoreDBClusterFromS3_manageMasterUserPassword,
    restoreDBClusterFromS3_masterUserPassword,
    restoreDBClusterFromS3_masterUserSecretKmsKeyId,
    restoreDBClusterFromS3_networkType,
    restoreDBClusterFromS3_optionGroupName,
    restoreDBClusterFromS3_port,
    restoreDBClusterFromS3_preferredBackupWindow,
    restoreDBClusterFromS3_preferredMaintenanceWindow,
    restoreDBClusterFromS3_s3Prefix,
    restoreDBClusterFromS3_serverlessV2ScalingConfiguration,
    restoreDBClusterFromS3_storageEncrypted,
    restoreDBClusterFromS3_storageType,
    restoreDBClusterFromS3_tags,
    restoreDBClusterFromS3_vpcSecurityGroupIds,
    restoreDBClusterFromS3_dbClusterIdentifier,
    restoreDBClusterFromS3_engine,
    restoreDBClusterFromS3_masterUsername,
    restoreDBClusterFromS3_sourceEngine,
    restoreDBClusterFromS3_sourceEngineVersion,
    restoreDBClusterFromS3_s3BucketName,
    restoreDBClusterFromS3_s3IngestionRoleArn,
    restoreDBClusterFromS3Response_dbCluster,
    restoreDBClusterFromS3Response_httpStatus,

    -- ** RestoreDBClusterFromSnapshot
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_backtrackWindow,
    restoreDBClusterFromSnapshot_copyTagsToSnapshot,
    restoreDBClusterFromSnapshot_dbClusterInstanceClass,
    restoreDBClusterFromSnapshot_dbClusterParameterGroupName,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_databaseName,
    restoreDBClusterFromSnapshot_deletionProtection,
    restoreDBClusterFromSnapshot_domain,
    restoreDBClusterFromSnapshot_domainIAMRoleName,
    restoreDBClusterFromSnapshot_enableCloudwatchLogsExports,
    restoreDBClusterFromSnapshot_enableIAMDatabaseAuthentication,
    restoreDBClusterFromSnapshot_engineMode,
    restoreDBClusterFromSnapshot_engineVersion,
    restoreDBClusterFromSnapshot_iops,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_networkType,
    restoreDBClusterFromSnapshot_optionGroupName,
    restoreDBClusterFromSnapshot_port,
    restoreDBClusterFromSnapshot_publiclyAccessible,
    restoreDBClusterFromSnapshot_scalingConfiguration,
    restoreDBClusterFromSnapshot_serverlessV2ScalingConfiguration,
    restoreDBClusterFromSnapshot_storageType,
    restoreDBClusterFromSnapshot_tags,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_dbClusterIdentifier,
    restoreDBClusterFromSnapshot_snapshotIdentifier,
    restoreDBClusterFromSnapshot_engine,
    restoreDBClusterFromSnapshotResponse_dbCluster,
    restoreDBClusterFromSnapshotResponse_httpStatus,

    -- ** RestoreDBClusterToPointInTime
    restoreDBClusterToPointInTime_backtrackWindow,
    restoreDBClusterToPointInTime_copyTagsToSnapshot,
    restoreDBClusterToPointInTime_dbClusterInstanceClass,
    restoreDBClusterToPointInTime_dbClusterParameterGroupName,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_domain,
    restoreDBClusterToPointInTime_domainIAMRoleName,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBClusterToPointInTime_engineMode,
    restoreDBClusterToPointInTime_iops,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_networkType,
    restoreDBClusterToPointInTime_optionGroupName,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_publiclyAccessible,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_scalingConfiguration,
    restoreDBClusterToPointInTime_serverlessV2ScalingConfiguration,
    restoreDBClusterToPointInTime_storageType,
    restoreDBClusterToPointInTime_tags,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
    restoreDBClusterToPointInTime_dbClusterIdentifier,
    restoreDBClusterToPointInTime_sourceDBClusterIdentifier,
    restoreDBClusterToPointInTimeResponse_dbCluster,
    restoreDBClusterToPointInTimeResponse_httpStatus,

    -- ** RestoreDBInstanceFromDBSnapshot
    restoreDBInstanceFromDBSnapshot_allocatedStorage,
    restoreDBInstanceFromDBSnapshot_autoMinorVersionUpgrade,
    restoreDBInstanceFromDBSnapshot_availabilityZone,
    restoreDBInstanceFromDBSnapshot_backupTarget,
    restoreDBInstanceFromDBSnapshot_copyTagsToSnapshot,
    restoreDBInstanceFromDBSnapshot_customIamInstanceProfile,
    restoreDBInstanceFromDBSnapshot_dbClusterSnapshotIdentifier,
    restoreDBInstanceFromDBSnapshot_dbInstanceClass,
    restoreDBInstanceFromDBSnapshot_dbName,
    restoreDBInstanceFromDBSnapshot_dbParameterGroupName,
    restoreDBInstanceFromDBSnapshot_dbSnapshotIdentifier,
    restoreDBInstanceFromDBSnapshot_dbSubnetGroupName,
    restoreDBInstanceFromDBSnapshot_deletionProtection,
    restoreDBInstanceFromDBSnapshot_domain,
    restoreDBInstanceFromDBSnapshot_domainIAMRoleName,
    restoreDBInstanceFromDBSnapshot_enableCloudwatchLogsExports,
    restoreDBInstanceFromDBSnapshot_enableCustomerOwnedIp,
    restoreDBInstanceFromDBSnapshot_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromDBSnapshot_engine,
    restoreDBInstanceFromDBSnapshot_iops,
    restoreDBInstanceFromDBSnapshot_licenseModel,
    restoreDBInstanceFromDBSnapshot_multiAZ,
    restoreDBInstanceFromDBSnapshot_networkType,
    restoreDBInstanceFromDBSnapshot_optionGroupName,
    restoreDBInstanceFromDBSnapshot_port,
    restoreDBInstanceFromDBSnapshot_processorFeatures,
    restoreDBInstanceFromDBSnapshot_publiclyAccessible,
    restoreDBInstanceFromDBSnapshot_storageThroughput,
    restoreDBInstanceFromDBSnapshot_storageType,
    restoreDBInstanceFromDBSnapshot_tags,
    restoreDBInstanceFromDBSnapshot_tdeCredentialArn,
    restoreDBInstanceFromDBSnapshot_tdeCredentialPassword,
    restoreDBInstanceFromDBSnapshot_useDefaultProcessorFeatures,
    restoreDBInstanceFromDBSnapshot_vpcSecurityGroupIds,
    restoreDBInstanceFromDBSnapshot_dbInstanceIdentifier,
    restoreDBInstanceFromDBSnapshotResponse_dbInstance,
    restoreDBInstanceFromDBSnapshotResponse_httpStatus,

    -- ** RestoreDBInstanceFromS3
    restoreDBInstanceFromS3_allocatedStorage,
    restoreDBInstanceFromS3_autoMinorVersionUpgrade,
    restoreDBInstanceFromS3_availabilityZone,
    restoreDBInstanceFromS3_backupRetentionPeriod,
    restoreDBInstanceFromS3_copyTagsToSnapshot,
    restoreDBInstanceFromS3_dbName,
    restoreDBInstanceFromS3_dbParameterGroupName,
    restoreDBInstanceFromS3_dbSecurityGroups,
    restoreDBInstanceFromS3_dbSubnetGroupName,
    restoreDBInstanceFromS3_deletionProtection,
    restoreDBInstanceFromS3_enableCloudwatchLogsExports,
    restoreDBInstanceFromS3_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromS3_enablePerformanceInsights,
    restoreDBInstanceFromS3_engineVersion,
    restoreDBInstanceFromS3_iops,
    restoreDBInstanceFromS3_kmsKeyId,
    restoreDBInstanceFromS3_licenseModel,
    restoreDBInstanceFromS3_manageMasterUserPassword,
    restoreDBInstanceFromS3_masterUserPassword,
    restoreDBInstanceFromS3_masterUserSecretKmsKeyId,
    restoreDBInstanceFromS3_masterUsername,
    restoreDBInstanceFromS3_maxAllocatedStorage,
    restoreDBInstanceFromS3_monitoringInterval,
    restoreDBInstanceFromS3_monitoringRoleArn,
    restoreDBInstanceFromS3_multiAZ,
    restoreDBInstanceFromS3_networkType,
    restoreDBInstanceFromS3_optionGroupName,
    restoreDBInstanceFromS3_performanceInsightsKMSKeyId,
    restoreDBInstanceFromS3_performanceInsightsRetentionPeriod,
    restoreDBInstanceFromS3_port,
    restoreDBInstanceFromS3_preferredBackupWindow,
    restoreDBInstanceFromS3_preferredMaintenanceWindow,
    restoreDBInstanceFromS3_processorFeatures,
    restoreDBInstanceFromS3_publiclyAccessible,
    restoreDBInstanceFromS3_s3Prefix,
    restoreDBInstanceFromS3_storageEncrypted,
    restoreDBInstanceFromS3_storageThroughput,
    restoreDBInstanceFromS3_storageType,
    restoreDBInstanceFromS3_tags,
    restoreDBInstanceFromS3_useDefaultProcessorFeatures,
    restoreDBInstanceFromS3_vpcSecurityGroupIds,
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
    restoreDBInstanceToPointInTime_allocatedStorage,
    restoreDBInstanceToPointInTime_autoMinorVersionUpgrade,
    restoreDBInstanceToPointInTime_availabilityZone,
    restoreDBInstanceToPointInTime_backupTarget,
    restoreDBInstanceToPointInTime_copyTagsToSnapshot,
    restoreDBInstanceToPointInTime_customIamInstanceProfile,
    restoreDBInstanceToPointInTime_dbInstanceClass,
    restoreDBInstanceToPointInTime_dbName,
    restoreDBInstanceToPointInTime_dbParameterGroupName,
    restoreDBInstanceToPointInTime_dbSubnetGroupName,
    restoreDBInstanceToPointInTime_deletionProtection,
    restoreDBInstanceToPointInTime_domain,
    restoreDBInstanceToPointInTime_domainIAMRoleName,
    restoreDBInstanceToPointInTime_enableCloudwatchLogsExports,
    restoreDBInstanceToPointInTime_enableCustomerOwnedIp,
    restoreDBInstanceToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBInstanceToPointInTime_engine,
    restoreDBInstanceToPointInTime_iops,
    restoreDBInstanceToPointInTime_licenseModel,
    restoreDBInstanceToPointInTime_maxAllocatedStorage,
    restoreDBInstanceToPointInTime_multiAZ,
    restoreDBInstanceToPointInTime_networkType,
    restoreDBInstanceToPointInTime_optionGroupName,
    restoreDBInstanceToPointInTime_port,
    restoreDBInstanceToPointInTime_processorFeatures,
    restoreDBInstanceToPointInTime_publiclyAccessible,
    restoreDBInstanceToPointInTime_restoreTime,
    restoreDBInstanceToPointInTime_sourceDBInstanceAutomatedBackupsArn,
    restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier,
    restoreDBInstanceToPointInTime_sourceDbiResourceId,
    restoreDBInstanceToPointInTime_storageThroughput,
    restoreDBInstanceToPointInTime_storageType,
    restoreDBInstanceToPointInTime_tags,
    restoreDBInstanceToPointInTime_tdeCredentialArn,
    restoreDBInstanceToPointInTime_tdeCredentialPassword,
    restoreDBInstanceToPointInTime_useDefaultProcessorFeatures,
    restoreDBInstanceToPointInTime_useLatestRestorableTime,
    restoreDBInstanceToPointInTime_vpcSecurityGroupIds,
    restoreDBInstanceToPointInTime_targetDBInstanceIdentifier,
    restoreDBInstanceToPointInTimeResponse_dbInstance,
    restoreDBInstanceToPointInTimeResponse_httpStatus,

    -- ** RevokeDBSecurityGroupIngress
    revokeDBSecurityGroupIngress_cidrip,
    revokeDBSecurityGroupIngress_eC2SecurityGroupId,
    revokeDBSecurityGroupIngress_eC2SecurityGroupName,
    revokeDBSecurityGroupIngress_eC2SecurityGroupOwnerId,
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
    startActivityStreamResponse_engineNativeAuditFieldsIncluded,
    startActivityStreamResponse_kinesisStreamName,
    startActivityStreamResponse_kmsKeyId,
    startActivityStreamResponse_mode,
    startActivityStreamResponse_status,
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
    startDBInstanceAutomatedBackupsReplication_destinationRegion,
    startDBInstanceAutomatedBackupsReplication_kmsKeyId,
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
    exportTask_exportOnly,
    exportTask_exportTaskIdentifier,
    exportTask_failureCause,
    exportTask_iamRoleArn,
    exportTask_kmsKeyId,
    exportTask_percentProgress,
    exportTask_s3Bucket,
    exportTask_s3Prefix,
    exportTask_snapshotTime,
    exportTask_sourceArn,
    exportTask_sourceType,
    exportTask_status,
    exportTask_taskEndTime,
    exportTask_taskStartTime,
    exportTask_totalExtractedDataInGB,
    exportTask_warningMessage,

    -- ** StopActivityStream
    stopActivityStream_applyImmediately,
    stopActivityStream_resourceArn,
    stopActivityStreamResponse_kinesisStreamName,
    stopActivityStreamResponse_kmsKeyId,
    stopActivityStreamResponse_status,
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

    -- ** SwitchoverBlueGreenDeployment
    switchoverBlueGreenDeployment_switchoverTimeout,
    switchoverBlueGreenDeployment_blueGreenDeploymentIdentifier,
    switchoverBlueGreenDeploymentResponse_blueGreenDeployment,
    switchoverBlueGreenDeploymentResponse_httpStatus,

    -- ** SwitchoverReadReplica
    switchoverReadReplica_dbInstanceIdentifier,
    switchoverReadReplicaResponse_dbInstance,
    switchoverReadReplicaResponse_httpStatus,

    -- * Types

    -- ** AccountQuota
    accountQuota_accountQuotaName,
    accountQuota_max,
    accountQuota_used,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** AvailableProcessorFeature
    availableProcessorFeature_allowedValues,
    availableProcessorFeature_defaultValue,
    availableProcessorFeature_name,

    -- ** BlueGreenDeployment
    blueGreenDeployment_blueGreenDeploymentIdentifier,
    blueGreenDeployment_blueGreenDeploymentName,
    blueGreenDeployment_createTime,
    blueGreenDeployment_deleteTime,
    blueGreenDeployment_source,
    blueGreenDeployment_status,
    blueGreenDeployment_statusDetails,
    blueGreenDeployment_switchoverDetails,
    blueGreenDeployment_tagList,
    blueGreenDeployment_target,
    blueGreenDeployment_tasks,

    -- ** BlueGreenDeploymentTask
    blueGreenDeploymentTask_name,
    blueGreenDeploymentTask_status,

    -- ** Certificate
    certificate_certificateArn,
    certificate_certificateIdentifier,
    certificate_certificateType,
    certificate_customerOverride,
    certificate_customerOverrideValidTill,
    certificate_thumbprint,
    certificate_validFrom,
    certificate_validTill,

    -- ** CertificateDetails
    certificateDetails_cAIdentifier,
    certificateDetails_validTill,

    -- ** CharacterSet
    characterSet_characterSetDescription,
    characterSet_characterSetName,

    -- ** CloudwatchLogsExportConfiguration
    cloudwatchLogsExportConfiguration_disableLogTypes,
    cloudwatchLogsExportConfiguration_enableLogTypes,

    -- ** ClusterPendingModifiedValues
    clusterPendingModifiedValues_allocatedStorage,
    clusterPendingModifiedValues_backupRetentionPeriod,
    clusterPendingModifiedValues_dbClusterIdentifier,
    clusterPendingModifiedValues_engineVersion,
    clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled,
    clusterPendingModifiedValues_iops,
    clusterPendingModifiedValues_masterUserPassword,
    clusterPendingModifiedValues_pendingCloudwatchLogsExports,
    clusterPendingModifiedValues_storageType,

    -- ** ConnectionPoolConfiguration
    connectionPoolConfiguration_connectionBorrowTimeout,
    connectionPoolConfiguration_initQuery,
    connectionPoolConfiguration_maxConnectionsPercent,
    connectionPoolConfiguration_maxIdleConnectionsPercent,
    connectionPoolConfiguration_sessionPinningFilters,

    -- ** ConnectionPoolConfigurationInfo
    connectionPoolConfigurationInfo_connectionBorrowTimeout,
    connectionPoolConfigurationInfo_initQuery,
    connectionPoolConfigurationInfo_maxConnectionsPercent,
    connectionPoolConfigurationInfo_maxIdleConnectionsPercent,
    connectionPoolConfigurationInfo_sessionPinningFilters,

    -- ** CustomDBEngineVersionAMI
    customDBEngineVersionAMI_imageId,
    customDBEngineVersionAMI_status,

    -- ** DBCluster
    dbCluster_activityStreamKinesisStreamName,
    dbCluster_activityStreamKmsKeyId,
    dbCluster_activityStreamMode,
    dbCluster_activityStreamStatus,
    dbCluster_allocatedStorage,
    dbCluster_associatedRoles,
    dbCluster_autoMinorVersionUpgrade,
    dbCluster_automaticRestartTime,
    dbCluster_availabilityZones,
    dbCluster_backtrackConsumedChangeRecords,
    dbCluster_backtrackWindow,
    dbCluster_backupRetentionPeriod,
    dbCluster_capacity,
    dbCluster_characterSetName,
    dbCluster_cloneGroupId,
    dbCluster_clusterCreateTime,
    dbCluster_copyTagsToSnapshot,
    dbCluster_crossAccountClone,
    dbCluster_customEndpoints,
    dbCluster_dbClusterArn,
    dbCluster_dbClusterIdentifier,
    dbCluster_dbClusterInstanceClass,
    dbCluster_dbClusterMembers,
    dbCluster_dbClusterOptionGroupMemberships,
    dbCluster_dbClusterParameterGroup,
    dbCluster_dbSubnetGroup,
    dbCluster_dbSystemId,
    dbCluster_databaseName,
    dbCluster_dbClusterResourceId,
    dbCluster_deletionProtection,
    dbCluster_domainMemberships,
    dbCluster_earliestBacktrackTime,
    dbCluster_earliestRestorableTime,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_endpoint,
    dbCluster_engine,
    dbCluster_engineMode,
    dbCluster_engineVersion,
    dbCluster_globalWriteForwardingRequested,
    dbCluster_globalWriteForwardingStatus,
    dbCluster_hostedZoneId,
    dbCluster_httpEndpointEnabled,
    dbCluster_iAMDatabaseAuthenticationEnabled,
    dbCluster_iOOptimizedNextAllowedModificationTime,
    dbCluster_iops,
    dbCluster_kmsKeyId,
    dbCluster_latestRestorableTime,
    dbCluster_masterUserSecret,
    dbCluster_masterUsername,
    dbCluster_monitoringInterval,
    dbCluster_monitoringRoleArn,
    dbCluster_multiAZ,
    dbCluster_networkType,
    dbCluster_pendingModifiedValues,
    dbCluster_percentProgress,
    dbCluster_performanceInsightsEnabled,
    dbCluster_performanceInsightsKMSKeyId,
    dbCluster_performanceInsightsRetentionPeriod,
    dbCluster_port,
    dbCluster_preferredBackupWindow,
    dbCluster_preferredMaintenanceWindow,
    dbCluster_publiclyAccessible,
    dbCluster_readReplicaIdentifiers,
    dbCluster_readerEndpoint,
    dbCluster_replicationSourceIdentifier,
    dbCluster_scalingConfigurationInfo,
    dbCluster_serverlessV2ScalingConfiguration,
    dbCluster_status,
    dbCluster_storageEncrypted,
    dbCluster_storageType,
    dbCluster_tagList,
    dbCluster_vpcSecurityGroups,

    -- ** DBClusterBacktrack
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_backtrackRequestCreationTime,
    dbClusterBacktrack_backtrackTo,
    dbClusterBacktrack_backtrackedFrom,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_status,

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
    dbClusterSnapshot_dbSystemId,
    dbClusterSnapshot_engine,
    dbClusterSnapshot_engineMode,
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
    dbClusterSnapshot_storageType,
    dbClusterSnapshot_tagList,
    dbClusterSnapshot_vpcId,

    -- ** DBClusterSnapshotAttribute
    dbClusterSnapshotAttribute_attributeName,
    dbClusterSnapshotAttribute_attributeValues,

    -- ** DBClusterSnapshotAttributesResult
    dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier,

    -- ** DBEngineVersion
    dbEngineVersion_createTime,
    dbEngineVersion_customDBEngineVersionManifest,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_dbEngineMediaType,
    dbEngineVersion_dbEngineVersionArn,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_databaseInstallationFilesS3BucketName,
    dbEngineVersion_databaseInstallationFilesS3Prefix,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_engine,
    dbEngineVersion_engineVersion,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_image,
    dbEngineVersion_kmsKeyId,
    dbEngineVersion_majorEngineVersion,
    dbEngineVersion_status,
    dbEngineVersion_supportedCACertificateIdentifiers,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsBabelfish,
    dbEngineVersion_supportsCertificateRotationWithoutRestart,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_supportsParallelQuery,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_tagList,
    dbEngineVersion_validUpgradeTarget,

    -- ** DBInstance
    dbInstance_activityStreamEngineNativeAuditFieldsIncluded,
    dbInstance_activityStreamKinesisStreamName,
    dbInstance_activityStreamKmsKeyId,
    dbInstance_activityStreamMode,
    dbInstance_activityStreamPolicyStatus,
    dbInstance_activityStreamStatus,
    dbInstance_allocatedStorage,
    dbInstance_associatedRoles,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_automaticRestartTime,
    dbInstance_automationMode,
    dbInstance_availabilityZone,
    dbInstance_awsBackupRecoveryPointArn,
    dbInstance_backupRetentionPeriod,
    dbInstance_backupTarget,
    dbInstance_cACertificateIdentifier,
    dbInstance_certificateDetails,
    dbInstance_characterSetName,
    dbInstance_copyTagsToSnapshot,
    dbInstance_customIamInstanceProfile,
    dbInstance_customerOwnedIpEnabled,
    dbInstance_dbClusterIdentifier,
    dbInstance_dbInstanceArn,
    dbInstance_dbInstanceAutomatedBackupsReplications,
    dbInstance_dbInstanceClass,
    dbInstance_dbInstanceIdentifier,
    dbInstance_dbInstanceStatus,
    dbInstance_dbName,
    dbInstance_dbParameterGroups,
    dbInstance_dbSecurityGroups,
    dbInstance_dbSubnetGroup,
    dbInstance_dbSystemId,
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
    dbInstance_listenerEndpoint,
    dbInstance_masterUserSecret,
    dbInstance_masterUsername,
    dbInstance_maxAllocatedStorage,
    dbInstance_monitoringInterval,
    dbInstance_monitoringRoleArn,
    dbInstance_multiAZ,
    dbInstance_ncharCharacterSetName,
    dbInstance_networkType,
    dbInstance_optionGroupMemberships,
    dbInstance_pendingModifiedValues,
    dbInstance_performanceInsightsEnabled,
    dbInstance_performanceInsightsKMSKeyId,
    dbInstance_performanceInsightsRetentionPeriod,
    dbInstance_preferredBackupWindow,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_processorFeatures,
    dbInstance_promotionTier,
    dbInstance_publiclyAccessible,
    dbInstance_readReplicaDBClusterIdentifiers,
    dbInstance_readReplicaDBInstanceIdentifiers,
    dbInstance_readReplicaSourceDBClusterIdentifier,
    dbInstance_readReplicaSourceDBInstanceIdentifier,
    dbInstance_replicaMode,
    dbInstance_resumeFullAutomationModeTime,
    dbInstance_secondaryAvailabilityZone,
    dbInstance_statusInfos,
    dbInstance_storageEncrypted,
    dbInstance_storageThroughput,
    dbInstance_storageType,
    dbInstance_tagList,
    dbInstance_tdeCredentialArn,
    dbInstance_timezone,
    dbInstance_vpcSecurityGroups,

    -- ** DBInstanceAutomatedBackup
    dbInstanceAutomatedBackup_allocatedStorage,
    dbInstanceAutomatedBackup_availabilityZone,
    dbInstanceAutomatedBackup_backupRetentionPeriod,
    dbInstanceAutomatedBackup_backupTarget,
    dbInstanceAutomatedBackup_dbInstanceArn,
    dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn,
    dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications,
    dbInstanceAutomatedBackup_dbInstanceIdentifier,
    dbInstanceAutomatedBackup_dbiResourceId,
    dbInstanceAutomatedBackup_encrypted,
    dbInstanceAutomatedBackup_engine,
    dbInstanceAutomatedBackup_engineVersion,
    dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled,
    dbInstanceAutomatedBackup_instanceCreateTime,
    dbInstanceAutomatedBackup_iops,
    dbInstanceAutomatedBackup_kmsKeyId,
    dbInstanceAutomatedBackup_licenseModel,
    dbInstanceAutomatedBackup_masterUsername,
    dbInstanceAutomatedBackup_optionGroupName,
    dbInstanceAutomatedBackup_port,
    dbInstanceAutomatedBackup_region,
    dbInstanceAutomatedBackup_restoreWindow,
    dbInstanceAutomatedBackup_status,
    dbInstanceAutomatedBackup_storageThroughput,
    dbInstanceAutomatedBackup_storageType,
    dbInstanceAutomatedBackup_tdeCredentialArn,
    dbInstanceAutomatedBackup_timezone,
    dbInstanceAutomatedBackup_vpcId,

    -- ** DBInstanceAutomatedBackupsReplication
    dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn,

    -- ** DBInstanceRole
    dbInstanceRole_featureName,
    dbInstanceRole_roleArn,
    dbInstanceRole_status,

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

    -- ** DBProxy
    dbProxy_auth,
    dbProxy_createdDate,
    dbProxy_dbProxyArn,
    dbProxy_dbProxyName,
    dbProxy_debugLogging,
    dbProxy_endpoint,
    dbProxy_engineFamily,
    dbProxy_idleClientTimeout,
    dbProxy_requireTLS,
    dbProxy_roleArn,
    dbProxy_status,
    dbProxy_updatedDate,
    dbProxy_vpcId,
    dbProxy_vpcSecurityGroupIds,
    dbProxy_vpcSubnetIds,

    -- ** DBProxyEndpoint
    dbProxyEndpoint_createdDate,
    dbProxyEndpoint_dbProxyEndpointArn,
    dbProxyEndpoint_dbProxyEndpointName,
    dbProxyEndpoint_dbProxyName,
    dbProxyEndpoint_endpoint,
    dbProxyEndpoint_isDefault,
    dbProxyEndpoint_status,
    dbProxyEndpoint_targetRole,
    dbProxyEndpoint_vpcId,
    dbProxyEndpoint_vpcSecurityGroupIds,
    dbProxyEndpoint_vpcSubnetIds,

    -- ** DBProxyTarget
    dbProxyTarget_endpoint,
    dbProxyTarget_port,
    dbProxyTarget_rdsResourceId,
    dbProxyTarget_role,
    dbProxyTarget_targetArn,
    dbProxyTarget_targetHealth,
    dbProxyTarget_trackedClusterId,
    dbProxyTarget_type,

    -- ** DBProxyTargetGroup
    dbProxyTargetGroup_connectionPoolConfig,
    dbProxyTargetGroup_createdDate,
    dbProxyTargetGroup_dbProxyName,
    dbProxyTargetGroup_isDefault,
    dbProxyTargetGroup_status,
    dbProxyTargetGroup_targetGroupArn,
    dbProxyTargetGroup_targetGroupName,
    dbProxyTargetGroup_updatedDate,

    -- ** DBSecurityGroup
    dbSecurityGroup_dbSecurityGroupArn,
    dbSecurityGroup_dbSecurityGroupDescription,
    dbSecurityGroup_dbSecurityGroupName,
    dbSecurityGroup_eC2SecurityGroups,
    dbSecurityGroup_iPRanges,
    dbSecurityGroup_ownerId,
    dbSecurityGroup_vpcId,

    -- ** DBSecurityGroupMembership
    dbSecurityGroupMembership_dbSecurityGroupName,
    dbSecurityGroupMembership_status,

    -- ** DBSnapshot
    dbSnapshot_allocatedStorage,
    dbSnapshot_availabilityZone,
    dbSnapshot_dbInstanceIdentifier,
    dbSnapshot_dbSnapshotArn,
    dbSnapshot_dbSnapshotIdentifier,
    dbSnapshot_dbiResourceId,
    dbSnapshot_encrypted,
    dbSnapshot_engine,
    dbSnapshot_engineVersion,
    dbSnapshot_iAMDatabaseAuthenticationEnabled,
    dbSnapshot_instanceCreateTime,
    dbSnapshot_iops,
    dbSnapshot_kmsKeyId,
    dbSnapshot_licenseModel,
    dbSnapshot_masterUsername,
    dbSnapshot_optionGroupName,
    dbSnapshot_originalSnapshotCreateTime,
    dbSnapshot_percentProgress,
    dbSnapshot_port,
    dbSnapshot_processorFeatures,
    dbSnapshot_snapshotCreateTime,
    dbSnapshot_snapshotDatabaseTime,
    dbSnapshot_snapshotTarget,
    dbSnapshot_snapshotType,
    dbSnapshot_sourceDBSnapshotIdentifier,
    dbSnapshot_sourceRegion,
    dbSnapshot_status,
    dbSnapshot_storageThroughput,
    dbSnapshot_storageType,
    dbSnapshot_tagList,
    dbSnapshot_tdeCredentialArn,
    dbSnapshot_timezone,
    dbSnapshot_vpcId,

    -- ** DBSnapshotAttribute
    dbSnapshotAttribute_attributeName,
    dbSnapshotAttribute_attributeValues,

    -- ** DBSnapshotAttributesResult
    dbSnapshotAttributesResult_dbSnapshotAttributes,
    dbSnapshotAttributesResult_dbSnapshotIdentifier,

    -- ** DBSubnetGroup
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_subnetGroupStatus,
    dbSubnetGroup_subnets,
    dbSubnetGroup_supportedNetworkTypes,
    dbSubnetGroup_vpcId,

    -- ** DescribeDBLogFilesDetails
    describeDBLogFilesDetails_lastWritten,
    describeDBLogFilesDetails_logFileName,
    describeDBLogFilesDetails_size,

    -- ** DomainMembership
    domainMembership_domain,
    domainMembership_fqdn,
    domainMembership_iAMRoleName,
    domainMembership_status,

    -- ** DoubleRange
    doubleRange_from,
    doubleRange_to,

    -- ** EC2SecurityGroup
    eC2SecurityGroup_eC2SecurityGroupId,
    eC2SecurityGroup_eC2SecurityGroupName,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_status,

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

    -- ** ExportTask
    exportTask_exportOnly,
    exportTask_exportTaskIdentifier,
    exportTask_failureCause,
    exportTask_iamRoleArn,
    exportTask_kmsKeyId,
    exportTask_percentProgress,
    exportTask_s3Bucket,
    exportTask_s3Prefix,
    exportTask_snapshotTime,
    exportTask_sourceArn,
    exportTask_sourceType,
    exportTask_status,
    exportTask_taskEndTime,
    exportTask_taskStartTime,
    exportTask_totalExtractedDataInGB,
    exportTask_warningMessage,

    -- ** FailoverState
    failoverState_fromDbClusterArn,
    failoverState_status,
    failoverState_toDbClusterArn,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** GlobalCluster
    globalCluster_databaseName,
    globalCluster_deletionProtection,
    globalCluster_engine,
    globalCluster_engineVersion,
    globalCluster_failoverState,
    globalCluster_globalClusterArn,
    globalCluster_globalClusterIdentifier,
    globalCluster_globalClusterMembers,
    globalCluster_globalClusterResourceId,
    globalCluster_status,
    globalCluster_storageEncrypted,

    -- ** GlobalClusterMember
    globalClusterMember_dbClusterArn,
    globalClusterMember_globalWriteForwardingStatus,
    globalClusterMember_isWriter,
    globalClusterMember_readers,

    -- ** IPRange
    iPRange_cidrip,
    iPRange_status,

    -- ** MasterUserSecret
    masterUserSecret_kmsKeyId,
    masterUserSecret_secretArn,
    masterUserSecret_secretStatus,

    -- ** MinimumEngineVersionPerAllowedValue
    minimumEngineVersionPerAllowedValue_allowedValue,
    minimumEngineVersionPerAllowedValue_minimumEngineVersion,

    -- ** Option
    option_dbSecurityGroupMemberships,
    option_optionDescription,
    option_optionName,
    option_optionSettings,
    option_optionVersion,
    option_permanent,
    option_persistent,
    option_port,
    option_vpcSecurityGroupMemberships,

    -- ** OptionConfiguration
    optionConfiguration_dbSecurityGroupMemberships,
    optionConfiguration_optionSettings,
    optionConfiguration_optionVersion,
    optionConfiguration_port,
    optionConfiguration_vpcSecurityGroupMemberships,
    optionConfiguration_optionName,

    -- ** OptionGroup
    optionGroup_allowsVpcAndNonVpcInstanceMemberships,
    optionGroup_copyTimestamp,
    optionGroup_engineName,
    optionGroup_majorEngineVersion,
    optionGroup_optionGroupArn,
    optionGroup_optionGroupDescription,
    optionGroup_optionGroupName,
    optionGroup_options,
    optionGroup_sourceAccountId,
    optionGroup_sourceOptionGroup,
    optionGroup_vpcId,

    -- ** OptionGroupMembership
    optionGroupMembership_optionGroupName,
    optionGroupMembership_status,

    -- ** OptionGroupOption
    optionGroupOption_copyableCrossAccount,
    optionGroupOption_defaultPort,
    optionGroupOption_description,
    optionGroupOption_engineName,
    optionGroupOption_majorEngineVersion,
    optionGroupOption_minimumRequiredMinorEngineVersion,
    optionGroupOption_name,
    optionGroupOption_optionGroupOptionSettings,
    optionGroupOption_optionGroupOptionVersions,
    optionGroupOption_optionsConflictsWith,
    optionGroupOption_optionsDependedOn,
    optionGroupOption_permanent,
    optionGroupOption_persistent,
    optionGroupOption_portRequired,
    optionGroupOption_requiresAutoMinorEngineVersionUpgrade,
    optionGroupOption_supportsOptionVersionDowngrade,
    optionGroupOption_vpcOnly,

    -- ** OptionGroupOptionSetting
    optionGroupOptionSetting_allowedValues,
    optionGroupOptionSetting_applyType,
    optionGroupOptionSetting_defaultValue,
    optionGroupOptionSetting_isModifiable,
    optionGroupOptionSetting_isRequired,
    optionGroupOptionSetting_minimumEngineVersionPerAllowedValue,
    optionGroupOptionSetting_settingDescription,
    optionGroupOptionSetting_settingName,

    -- ** OptionSetting
    optionSetting_allowedValues,
    optionSetting_applyType,
    optionSetting_dataType,
    optionSetting_defaultValue,
    optionSetting_description,
    optionSetting_isCollection,
    optionSetting_isModifiable,
    optionSetting_name,
    optionSetting_value,

    -- ** OptionVersion
    optionVersion_isDefault,
    optionVersion_version,

    -- ** OrderableDBInstanceOption
    orderableDBInstanceOption_availabilityZoneGroup,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_availableProcessorFeatures,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_licenseModel,
    orderableDBInstanceOption_maxIopsPerDbInstance,
    orderableDBInstanceOption_maxIopsPerGib,
    orderableDBInstanceOption_maxStorageSize,
    orderableDBInstanceOption_maxStorageThroughputPerDbInstance,
    orderableDBInstanceOption_maxStorageThroughputPerIops,
    orderableDBInstanceOption_minIopsPerDbInstance,
    orderableDBInstanceOption_minIopsPerGib,
    orderableDBInstanceOption_minStorageSize,
    orderableDBInstanceOption_minStorageThroughputPerDbInstance,
    orderableDBInstanceOption_minStorageThroughputPerIops,
    orderableDBInstanceOption_multiAZCapable,
    orderableDBInstanceOption_outpostCapable,
    orderableDBInstanceOption_readReplicaCapable,
    orderableDBInstanceOption_storageType,
    orderableDBInstanceOption_supportedActivityStreamModes,
    orderableDBInstanceOption_supportedEngineModes,
    orderableDBInstanceOption_supportedNetworkTypes,
    orderableDBInstanceOption_supportsClusters,
    orderableDBInstanceOption_supportsEnhancedMonitoring,
    orderableDBInstanceOption_supportsGlobalDatabases,
    orderableDBInstanceOption_supportsIAMDatabaseAuthentication,
    orderableDBInstanceOption_supportsIops,
    orderableDBInstanceOption_supportsKerberosAuthentication,
    orderableDBInstanceOption_supportsPerformanceInsights,
    orderableDBInstanceOption_supportsStorageAutoscaling,
    orderableDBInstanceOption_supportsStorageEncryption,
    orderableDBInstanceOption_supportsStorageThroughput,
    orderableDBInstanceOption_vpc,

    -- ** Outpost
    outpost_arn,

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
    parameter_supportedEngineModes,

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
    pendingModifiedValues_automationMode,
    pendingModifiedValues_backupRetentionPeriod,
    pendingModifiedValues_cACertificateIdentifier,
    pendingModifiedValues_dbInstanceClass,
    pendingModifiedValues_dbInstanceIdentifier,
    pendingModifiedValues_dbSubnetGroupName,
    pendingModifiedValues_engine,
    pendingModifiedValues_engineVersion,
    pendingModifiedValues_iAMDatabaseAuthenticationEnabled,
    pendingModifiedValues_iops,
    pendingModifiedValues_licenseModel,
    pendingModifiedValues_masterUserPassword,
    pendingModifiedValues_multiAZ,
    pendingModifiedValues_pendingCloudwatchLogsExports,
    pendingModifiedValues_port,
    pendingModifiedValues_processorFeatures,
    pendingModifiedValues_resumeFullAutomationModeTime,
    pendingModifiedValues_storageThroughput,
    pendingModifiedValues_storageType,

    -- ** ProcessorFeature
    processorFeature_name,
    processorFeature_value,

    -- ** Range
    range_from,
    range_step,
    range_to,

    -- ** RecurringCharge
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- ** ReservedDBInstance
    reservedDBInstance_currencyCode,
    reservedDBInstance_dbInstanceClass,
    reservedDBInstance_dbInstanceCount,
    reservedDBInstance_duration,
    reservedDBInstance_fixedPrice,
    reservedDBInstance_leaseId,
    reservedDBInstance_multiAZ,
    reservedDBInstance_offeringType,
    reservedDBInstance_productDescription,
    reservedDBInstance_recurringCharges,
    reservedDBInstance_reservedDBInstanceArn,
    reservedDBInstance_reservedDBInstanceId,
    reservedDBInstance_reservedDBInstancesOfferingId,
    reservedDBInstance_startTime,
    reservedDBInstance_state,
    reservedDBInstance_usagePrice,

    -- ** ReservedDBInstancesOffering
    reservedDBInstancesOffering_currencyCode,
    reservedDBInstancesOffering_dbInstanceClass,
    reservedDBInstancesOffering_duration,
    reservedDBInstancesOffering_fixedPrice,
    reservedDBInstancesOffering_multiAZ,
    reservedDBInstancesOffering_offeringType,
    reservedDBInstancesOffering_productDescription,
    reservedDBInstancesOffering_recurringCharges,
    reservedDBInstancesOffering_reservedDBInstancesOfferingId,
    reservedDBInstancesOffering_usagePrice,

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- ** RestoreWindow
    restoreWindow_earliestTime,
    restoreWindow_latestTime,

    -- ** ScalingConfiguration
    scalingConfiguration_autoPause,
    scalingConfiguration_maxCapacity,
    scalingConfiguration_minCapacity,
    scalingConfiguration_secondsBeforeTimeout,
    scalingConfiguration_secondsUntilAutoPause,
    scalingConfiguration_timeoutAction,

    -- ** ScalingConfigurationInfo
    scalingConfigurationInfo_autoPause,
    scalingConfigurationInfo_maxCapacity,
    scalingConfigurationInfo_minCapacity,
    scalingConfigurationInfo_secondsBeforeTimeout,
    scalingConfigurationInfo_secondsUntilAutoPause,
    scalingConfigurationInfo_timeoutAction,

    -- ** ServerlessV2ScalingConfiguration
    serverlessV2ScalingConfiguration_maxCapacity,
    serverlessV2ScalingConfiguration_minCapacity,

    -- ** ServerlessV2ScalingConfigurationInfo
    serverlessV2ScalingConfigurationInfo_maxCapacity,
    serverlessV2ScalingConfigurationInfo_minCapacity,

    -- ** SourceRegion
    sourceRegion_endpoint,
    sourceRegion_regionName,
    sourceRegion_status,
    sourceRegion_supportsDBInstanceAutomatedBackupsReplication,

    -- ** Subnet
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,
    subnet_subnetOutpost,
    subnet_subnetStatus,

    -- ** SwitchoverDetail
    switchoverDetail_sourceMember,
    switchoverDetail_status,
    switchoverDetail_targetMember,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TargetHealth
    targetHealth_description,
    targetHealth_reason,
    targetHealth_state,

    -- ** Timezone
    timezone_timezoneName,

    -- ** UpgradeTarget
    upgradeTarget_autoUpgrade,
    upgradeTarget_description,
    upgradeTarget_engine,
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,
    upgradeTarget_supportedEngineModes,
    upgradeTarget_supportsBabelfish,
    upgradeTarget_supportsGlobalDatabases,
    upgradeTarget_supportsParallelQuery,

    -- ** UserAuthConfig
    userAuthConfig_authScheme,
    userAuthConfig_clientPasswordAuthType,
    userAuthConfig_description,
    userAuthConfig_iAMAuth,
    userAuthConfig_secretArn,
    userAuthConfig_userName,

    -- ** UserAuthConfigInfo
    userAuthConfigInfo_authScheme,
    userAuthConfigInfo_clientPasswordAuthType,
    userAuthConfigInfo_description,
    userAuthConfigInfo_iAMAuth,
    userAuthConfigInfo_secretArn,
    userAuthConfigInfo_userName,

    -- ** ValidDBInstanceModificationsMessage
    validDBInstanceModificationsMessage_storage,
    validDBInstanceModificationsMessage_validProcessorFeatures,

    -- ** ValidStorageOptions
    validStorageOptions_iopsToStorageRatio,
    validStorageOptions_provisionedIops,
    validStorageOptions_provisionedStorageThroughput,
    validStorageOptions_storageSize,
    validStorageOptions_storageThroughputToIopsRatio,
    validStorageOptions_storageType,
    validStorageOptions_supportsStorageAutoscaling,

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
import Amazonka.RDS.CreateBlueGreenDeployment
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
import Amazonka.RDS.DeleteBlueGreenDeployment
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
import Amazonka.RDS.DescribeBlueGreenDeployments
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
import Amazonka.RDS.SwitchoverBlueGreenDeployment
import Amazonka.RDS.SwitchoverReadReplica
import Amazonka.RDS.Types.AccountQuota
import Amazonka.RDS.Types.AvailabilityZone
import Amazonka.RDS.Types.AvailableProcessorFeature
import Amazonka.RDS.Types.BlueGreenDeployment
import Amazonka.RDS.Types.BlueGreenDeploymentTask
import Amazonka.RDS.Types.Certificate
import Amazonka.RDS.Types.CertificateDetails
import Amazonka.RDS.Types.CharacterSet
import Amazonka.RDS.Types.CloudwatchLogsExportConfiguration
import Amazonka.RDS.Types.ClusterPendingModifiedValues
import Amazonka.RDS.Types.ConnectionPoolConfiguration
import Amazonka.RDS.Types.ConnectionPoolConfigurationInfo
import Amazonka.RDS.Types.CustomDBEngineVersionAMI
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
import Amazonka.RDS.Types.MasterUserSecret
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
import Amazonka.RDS.Types.SwitchoverDetail
import Amazonka.RDS.Types.Tag
import Amazonka.RDS.Types.TargetHealth
import Amazonka.RDS.Types.Timezone
import Amazonka.RDS.Types.UpgradeTarget
import Amazonka.RDS.Types.UserAuthConfig
import Amazonka.RDS.Types.UserAuthConfigInfo
import Amazonka.RDS.Types.ValidDBInstanceModificationsMessage
import Amazonka.RDS.Types.ValidStorageOptions
import Amazonka.RDS.Types.VpcSecurityGroupMembership
