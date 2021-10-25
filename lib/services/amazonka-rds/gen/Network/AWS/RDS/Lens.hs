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

    -- ** PromoteReadReplica
    promoteReadReplica_preferredBackupWindow,
    promoteReadReplica_backupRetentionPeriod,
    promoteReadReplica_dbInstanceIdentifier,
    promoteReadReplicaResponse_dbInstance,
    promoteReadReplicaResponse_httpStatus,

    -- ** DescribeDBEngineVersions
    describeDBEngineVersions_engineVersion,
    describeDBEngineVersions_listSupportedTimezones,
    describeDBEngineVersions_defaultOnly,
    describeDBEngineVersions_includeAll,
    describeDBEngineVersions_filters,
    describeDBEngineVersions_engine,
    describeDBEngineVersions_dbParameterGroupFamily,
    describeDBEngineVersions_listSupportedCharacterSets,
    describeDBEngineVersions_marker,
    describeDBEngineVersions_maxRecords,
    describeDBEngineVersionsResponse_marker,
    describeDBEngineVersionsResponse_dbEngineVersions,
    describeDBEngineVersionsResponse_httpStatus,

    -- ** StopDBInstance
    stopDBInstance_dbSnapshotIdentifier,
    stopDBInstance_dbInstanceIdentifier,
    stopDBInstanceResponse_dbInstance,
    stopDBInstanceResponse_httpStatus,

    -- ** ModifyDBClusterEndpoint
    modifyDBClusterEndpoint_staticMembers,
    modifyDBClusterEndpoint_endpointType,
    modifyDBClusterEndpoint_excludedMembers,
    modifyDBClusterEndpoint_dbClusterEndpointIdentifier,
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

    -- ** DescribeDBProxyEndpoints
    describeDBProxyEndpoints_filters,
    describeDBProxyEndpoints_marker,
    describeDBProxyEndpoints_maxRecords,
    describeDBProxyEndpoints_dbProxyName,
    describeDBProxyEndpoints_dbProxyEndpointName,
    describeDBProxyEndpointsResponse_dbProxyEndpoints,
    describeDBProxyEndpointsResponse_marker,
    describeDBProxyEndpointsResponse_httpStatus,

    -- ** CopyDBSnapshot
    copyDBSnapshot_targetCustomAvailabilityZone,
    copyDBSnapshot_preSignedUrl,
    copyDBSnapshot_copyTags,
    copyDBSnapshot_destinationRegion,
    copyDBSnapshot_kmsKeyId,
    copyDBSnapshot_optionGroupName,
    copyDBSnapshot_tags,
    copyDBSnapshot_sourceDBSnapshotIdentifier,
    copyDBSnapshot_targetDBSnapshotIdentifier,
    copyDBSnapshotResponse_dbSnapshot,
    copyDBSnapshotResponse_httpStatus,

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
    modifyDBInstance_enableCustomerOwnedIp,
    modifyDBInstance_replicaMode,
    modifyDBInstance_monitoringInterval,
    modifyDBInstance_cloudwatchLogsExportConfiguration,
    modifyDBInstance_certificateRotationRestart,
    modifyDBInstance_tdeCredentialPassword,
    modifyDBInstance_processorFeatures,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_promotionTier,
    modifyDBInstance_awsBackupRecoveryPointArn,
    modifyDBInstance_licenseModel,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_performanceInsightsRetentionPeriod,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_maxAllocatedStorage,
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
    modifyDBInstance_useDefaultProcessorFeatures,
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

    -- ** DescribeCustomAvailabilityZones
    describeCustomAvailabilityZones_filters,
    describeCustomAvailabilityZones_customAvailabilityZoneId,
    describeCustomAvailabilityZones_marker,
    describeCustomAvailabilityZones_maxRecords,
    describeCustomAvailabilityZonesResponse_customAvailabilityZones,
    describeCustomAvailabilityZonesResponse_marker,
    describeCustomAvailabilityZonesResponse_httpStatus,

    -- ** RestoreDBClusterFromS3
    restoreDBClusterFromS3_engineVersion,
    restoreDBClusterFromS3_deletionProtection,
    restoreDBClusterFromS3_storageEncrypted,
    restoreDBClusterFromS3_dbSubnetGroupName,
    restoreDBClusterFromS3_domain,
    restoreDBClusterFromS3_backtrackWindow,
    restoreDBClusterFromS3_preferredMaintenanceWindow,
    restoreDBClusterFromS3_availabilityZones,
    restoreDBClusterFromS3_characterSetName,
    restoreDBClusterFromS3_kmsKeyId,
    restoreDBClusterFromS3_preferredBackupWindow,
    restoreDBClusterFromS3_backupRetentionPeriod,
    restoreDBClusterFromS3_vpcSecurityGroupIds,
    restoreDBClusterFromS3_databaseName,
    restoreDBClusterFromS3_dbClusterParameterGroupName,
    restoreDBClusterFromS3_s3Prefix,
    restoreDBClusterFromS3_optionGroupName,
    restoreDBClusterFromS3_copyTagsToSnapshot,
    restoreDBClusterFromS3_domainIAMRoleName,
    restoreDBClusterFromS3_tags,
    restoreDBClusterFromS3_port,
    restoreDBClusterFromS3_enableIAMDatabaseAuthentication,
    restoreDBClusterFromS3_enableCloudwatchLogsExports,
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
    describeEngineDefaultParametersResponse_httpStatus,
    describeEngineDefaultParametersResponse_engineDefaults,

    -- ** DescribeOptionGroups
    describeOptionGroups_filters,
    describeOptionGroups_engineName,
    describeOptionGroups_majorEngineVersion,
    describeOptionGroups_marker,
    describeOptionGroups_maxRecords,
    describeOptionGroups_optionGroupName,
    describeOptionGroupsResponse_marker,
    describeOptionGroupsResponse_optionGroupsList,
    describeOptionGroupsResponse_httpStatus,

    -- ** DescribeDBLogFiles
    describeDBLogFiles_filenameContains,
    describeDBLogFiles_filters,
    describeDBLogFiles_fileSize,
    describeDBLogFiles_fileLastWritten,
    describeDBLogFiles_marker,
    describeDBLogFiles_maxRecords,
    describeDBLogFiles_dbInstanceIdentifier,
    describeDBLogFilesResponse_describeDBLogFiles,
    describeDBLogFilesResponse_marker,
    describeDBLogFilesResponse_httpStatus,

    -- ** DescribeDBClusters
    describeDBClusters_dbClusterIdentifier,
    describeDBClusters_includeShared,
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

    -- ** DeleteOptionGroup
    deleteOptionGroup_optionGroupName,

    -- ** CreateDBProxyEndpoint
    createDBProxyEndpoint_targetRole,
    createDBProxyEndpoint_vpcSecurityGroupIds,
    createDBProxyEndpoint_tags,
    createDBProxyEndpoint_dbProxyName,
    createDBProxyEndpoint_dbProxyEndpointName,
    createDBProxyEndpoint_vpcSubnetIds,
    createDBProxyEndpointResponse_dbProxyEndpoint,
    createDBProxyEndpointResponse_httpStatus,

    -- ** DeleteDBCluster
    deleteDBCluster_finalDBSnapshotIdentifier,
    deleteDBCluster_skipFinalSnapshot,
    deleteDBCluster_dbClusterIdentifier,
    deleteDBClusterResponse_dbCluster,
    deleteDBClusterResponse_httpStatus,

    -- ** DescribeReservedDBInstances
    describeReservedDBInstances_productDescription,
    describeReservedDBInstances_filters,
    describeReservedDBInstances_leaseId,
    describeReservedDBInstances_reservedDBInstanceId,
    describeReservedDBInstances_dbInstanceClass,
    describeReservedDBInstances_marker,
    describeReservedDBInstances_maxRecords,
    describeReservedDBInstances_multiAZ,
    describeReservedDBInstances_reservedDBInstancesOfferingId,
    describeReservedDBInstances_offeringType,
    describeReservedDBInstances_duration,
    describeReservedDBInstancesResponse_reservedDBInstances,
    describeReservedDBInstancesResponse_marker,
    describeReservedDBInstancesResponse_httpStatus,

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

    -- ** DeleteCustomAvailabilityZone
    deleteCustomAvailabilityZone_customAvailabilityZoneId,
    deleteCustomAvailabilityZoneResponse_customAvailabilityZone,
    deleteCustomAvailabilityZoneResponse_httpStatus,

    -- ** DescribeDBProxyTargets
    describeDBProxyTargets_filters,
    describeDBProxyTargets_marker,
    describeDBProxyTargets_maxRecords,
    describeDBProxyTargets_targetGroupName,
    describeDBProxyTargets_dbProxyName,
    describeDBProxyTargetsResponse_targets,
    describeDBProxyTargetsResponse_marker,
    describeDBProxyTargetsResponse_httpStatus,

    -- ** StartDBInstanceAutomatedBackupsReplication
    startDBInstanceAutomatedBackupsReplication_preSignedUrl,
    startDBInstanceAutomatedBackupsReplication_destinationRegion,
    startDBInstanceAutomatedBackupsReplication_kmsKeyId,
    startDBInstanceAutomatedBackupsReplication_backupRetentionPeriod,
    startDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn,
    startDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup,
    startDBInstanceAutomatedBackupsReplicationResponse_httpStatus,

    -- ** DescribeEngineDefaultClusterParameters
    describeEngineDefaultClusterParameters_filters,
    describeEngineDefaultClusterParameters_marker,
    describeEngineDefaultClusterParameters_maxRecords,
    describeEngineDefaultClusterParameters_dbParameterGroupFamily,
    describeEngineDefaultClusterParametersResponse_engineDefaults,
    describeEngineDefaultClusterParametersResponse_httpStatus,

    -- ** DescribeDBSnapshotAttributes
    describeDBSnapshotAttributes_dbSnapshotIdentifier,
    describeDBSnapshotAttributesResponse_dbSnapshotAttributesResult,
    describeDBSnapshotAttributesResponse_httpStatus,

    -- ** CreateCustomAvailabilityZone
    createCustomAvailabilityZone_vpnTunnelOriginatorIP,
    createCustomAvailabilityZone_newVpnTunnelName,
    createCustomAvailabilityZone_existingVpnId,
    createCustomAvailabilityZone_customAvailabilityZoneName,
    createCustomAvailabilityZoneResponse_customAvailabilityZone,
    createCustomAvailabilityZoneResponse_httpStatus,

    -- ** BacktrackDBCluster
    backtrackDBCluster_force,
    backtrackDBCluster_useEarliestTimeOnPointInTimeUnavailable,
    backtrackDBCluster_dbClusterIdentifier,
    backtrackDBCluster_backtrackTo,
    dbClusterBacktrack_status,
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_backtrackTo,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_backtrackedFrom,
    dbClusterBacktrack_backtrackRequestCreationTime,

    -- ** DeleteGlobalCluster
    deleteGlobalCluster_globalClusterIdentifier,
    deleteGlobalClusterResponse_globalCluster,
    deleteGlobalClusterResponse_httpStatus,

    -- ** PromoteReadReplicaDBCluster
    promoteReadReplicaDBCluster_dbClusterIdentifier,
    promoteReadReplicaDBClusterResponse_dbCluster,
    promoteReadReplicaDBClusterResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceName,
    removeTagsFromResource_tagKeys,

    -- ** RestoreDBInstanceFromDBSnapshot
    restoreDBInstanceFromDBSnapshot_deletionProtection,
    restoreDBInstanceFromDBSnapshot_publiclyAccessible,
    restoreDBInstanceFromDBSnapshot_autoMinorVersionUpgrade,
    restoreDBInstanceFromDBSnapshot_dbSubnetGroupName,
    restoreDBInstanceFromDBSnapshot_iops,
    restoreDBInstanceFromDBSnapshot_domain,
    restoreDBInstanceFromDBSnapshot_enableCustomerOwnedIp,
    restoreDBInstanceFromDBSnapshot_engine,
    restoreDBInstanceFromDBSnapshot_tdeCredentialPassword,
    restoreDBInstanceFromDBSnapshot_processorFeatures,
    restoreDBInstanceFromDBSnapshot_dbInstanceClass,
    restoreDBInstanceFromDBSnapshot_licenseModel,
    restoreDBInstanceFromDBSnapshot_dbParameterGroupName,
    restoreDBInstanceFromDBSnapshot_availabilityZone,
    restoreDBInstanceFromDBSnapshot_vpcSecurityGroupIds,
    restoreDBInstanceFromDBSnapshot_multiAZ,
    restoreDBInstanceFromDBSnapshot_optionGroupName,
    restoreDBInstanceFromDBSnapshot_copyTagsToSnapshot,
    restoreDBInstanceFromDBSnapshot_tdeCredentialArn,
    restoreDBInstanceFromDBSnapshot_domainIAMRoleName,
    restoreDBInstanceFromDBSnapshot_tags,
    restoreDBInstanceFromDBSnapshot_port,
    restoreDBInstanceFromDBSnapshot_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromDBSnapshot_useDefaultProcessorFeatures,
    restoreDBInstanceFromDBSnapshot_storageType,
    restoreDBInstanceFromDBSnapshot_enableCloudwatchLogsExports,
    restoreDBInstanceFromDBSnapshot_dbName,
    restoreDBInstanceFromDBSnapshot_dbInstanceIdentifier,
    restoreDBInstanceFromDBSnapshot_dbSnapshotIdentifier,
    restoreDBInstanceFromDBSnapshotResponse_dbInstance,
    restoreDBInstanceFromDBSnapshotResponse_httpStatus,

    -- ** DeleteDBProxy
    deleteDBProxy_dbProxyName,
    deleteDBProxyResponse_dbProxy,
    deleteDBProxyResponse_httpStatus,

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

    -- ** PurchaseReservedDBInstancesOffering
    purchaseReservedDBInstancesOffering_dbInstanceCount,
    purchaseReservedDBInstancesOffering_reservedDBInstanceId,
    purchaseReservedDBInstancesOffering_tags,
    purchaseReservedDBInstancesOffering_reservedDBInstancesOfferingId,
    purchaseReservedDBInstancesOfferingResponse_reservedDBInstance,
    purchaseReservedDBInstancesOfferingResponse_httpStatus,

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
    createDBInstance_enableCustomerOwnedIp,
    createDBInstance_monitoringInterval,
    createDBInstance_tdeCredentialPassword,
    createDBInstance_processorFeatures,
    createDBInstance_promotionTier,
    createDBInstance_licenseModel,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_performanceInsightsRetentionPeriod,
    createDBInstance_characterSetName,
    createDBInstance_maxAllocatedStorage,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_kmsKeyId,
    createDBInstance_dbParameterGroupName,
    createDBInstance_preferredBackupWindow,
    createDBInstance_availabilityZone,
    createDBInstance_backupRetentionPeriod,
    createDBInstance_ncharCharacterSetName,
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

    -- ** DescribeCertificates
    describeCertificates_filters,
    describeCertificates_certificateIdentifier,
    describeCertificates_marker,
    describeCertificates_maxRecords,
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_marker,
    describeCertificatesResponse_httpStatus,

    -- ** AuthorizeDBSecurityGroupIngress
    authorizeDBSecurityGroupIngress_eC2SecurityGroupOwnerId,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupName,
    authorizeDBSecurityGroupIngress_cidrip,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupId,
    authorizeDBSecurityGroupIngress_dbSecurityGroupName,
    authorizeDBSecurityGroupIngressResponse_dbSecurityGroup,
    authorizeDBSecurityGroupIngressResponse_httpStatus,

    -- ** RemoveRoleFromDBInstance
    removeRoleFromDBInstance_dbInstanceIdentifier,
    removeRoleFromDBInstance_roleArn,
    removeRoleFromDBInstance_featureName,

    -- ** DescribeSourceRegions
    describeSourceRegions_regionName,
    describeSourceRegions_filters,
    describeSourceRegions_marker,
    describeSourceRegions_maxRecords,
    describeSourceRegionsResponse_marker,
    describeSourceRegionsResponse_sourceRegions,
    describeSourceRegionsResponse_httpStatus,

    -- ** CreateDBClusterEndpoint
    createDBClusterEndpoint_staticMembers,
    createDBClusterEndpoint_excludedMembers,
    createDBClusterEndpoint_tags,
    createDBClusterEndpoint_dbClusterIdentifier,
    createDBClusterEndpoint_dbClusterEndpointIdentifier,
    createDBClusterEndpoint_endpointType,
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

    -- ** RestoreDBClusterFromSnapshot
    restoreDBClusterFromSnapshot_engineVersion,
    restoreDBClusterFromSnapshot_deletionProtection,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_domain,
    restoreDBClusterFromSnapshot_backtrackWindow,
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_databaseName,
    restoreDBClusterFromSnapshot_dbClusterParameterGroupName,
    restoreDBClusterFromSnapshot_engineMode,
    restoreDBClusterFromSnapshot_scalingConfiguration,
    restoreDBClusterFromSnapshot_optionGroupName,
    restoreDBClusterFromSnapshot_copyTagsToSnapshot,
    restoreDBClusterFromSnapshot_domainIAMRoleName,
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
    describeOrderableDBInstanceOptions_availabilityZoneGroup,
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

    -- ** CreateDBProxy
    createDBProxy_debugLogging,
    createDBProxy_requireTLS,
    createDBProxy_idleClientTimeout,
    createDBProxy_vpcSecurityGroupIds,
    createDBProxy_tags,
    createDBProxy_dbProxyName,
    createDBProxy_engineFamily,
    createDBProxy_auth,
    createDBProxy_roleArn,
    createDBProxy_vpcSubnetIds,
    createDBProxyResponse_dbProxy,
    createDBProxyResponse_httpStatus,

    -- ** DeleteDBInstanceAutomatedBackup
    deleteDBInstanceAutomatedBackup_dbiResourceId,
    deleteDBInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn,
    deleteDBInstanceAutomatedBackupResponse_dbInstanceAutomatedBackup,
    deleteDBInstanceAutomatedBackupResponse_httpStatus,

    -- ** CreateDBClusterParameterGroup
    createDBClusterParameterGroup_tags,
    createDBClusterParameterGroup_dbClusterParameterGroupName,
    createDBClusterParameterGroup_dbParameterGroupFamily,
    createDBClusterParameterGroup_description,
    createDBClusterParameterGroupResponse_dbClusterParameterGroup,
    createDBClusterParameterGroupResponse_httpStatus,

    -- ** CreateDBSnapshot
    createDBSnapshot_tags,
    createDBSnapshot_dbSnapshotIdentifier,
    createDBSnapshot_dbInstanceIdentifier,
    createDBSnapshotResponse_dbSnapshot,
    createDBSnapshotResponse_httpStatus,

    -- ** DeleteEventSubscription
    deleteEventSubscription_subscriptionName,
    deleteEventSubscriptionResponse_eventSubscription,
    deleteEventSubscriptionResponse_httpStatus,

    -- ** DescribeDBClusterBacktracks
    describeDBClusterBacktracks_backtrackIdentifier,
    describeDBClusterBacktracks_filters,
    describeDBClusterBacktracks_marker,
    describeDBClusterBacktracks_maxRecords,
    describeDBClusterBacktracks_dbClusterIdentifier,
    describeDBClusterBacktracksResponse_marker,
    describeDBClusterBacktracksResponse_dbClusterBacktracks,
    describeDBClusterBacktracksResponse_httpStatus,

    -- ** DescribeDBParameterGroups
    describeDBParameterGroups_filters,
    describeDBParameterGroups_dbParameterGroupName,
    describeDBParameterGroups_marker,
    describeDBParameterGroups_maxRecords,
    describeDBParameterGroupsResponse_marker,
    describeDBParameterGroupsResponse_dbParameterGroups,
    describeDBParameterGroupsResponse_httpStatus,

    -- ** ModifyDBSnapshotAttribute
    modifyDBSnapshotAttribute_valuesToAdd,
    modifyDBSnapshotAttribute_valuesToRemove,
    modifyDBSnapshotAttribute_dbSnapshotIdentifier,
    modifyDBSnapshotAttribute_attributeName,
    modifyDBSnapshotAttributeResponse_dbSnapshotAttributesResult,
    modifyDBSnapshotAttributeResponse_httpStatus,

    -- ** DescribeDBInstanceAutomatedBackups
    describeDBInstanceAutomatedBackups_filters,
    describeDBInstanceAutomatedBackups_dbInstanceIdentifier,
    describeDBInstanceAutomatedBackups_marker,
    describeDBInstanceAutomatedBackups_maxRecords,
    describeDBInstanceAutomatedBackups_dbiResourceId,
    describeDBInstanceAutomatedBackups_dbInstanceAutomatedBackupsArn,
    describeDBInstanceAutomatedBackupsResponse_dbInstanceAutomatedBackups,
    describeDBInstanceAutomatedBackupsResponse_marker,
    describeDBInstanceAutomatedBackupsResponse_httpStatus,

    -- ** RemoveFromGlobalCluster
    removeFromGlobalCluster_dbClusterIdentifier,
    removeFromGlobalCluster_globalClusterIdentifier,
    removeFromGlobalClusterResponse_globalCluster,
    removeFromGlobalClusterResponse_httpStatus,

    -- ** AddRoleToDBInstance
    addRoleToDBInstance_dbInstanceIdentifier,
    addRoleToDBInstance_roleArn,
    addRoleToDBInstance_featureName,

    -- ** DeleteDBClusterSnapshot
    deleteDBClusterSnapshot_dbClusterSnapshotIdentifier,
    deleteDBClusterSnapshotResponse_dbClusterSnapshot,
    deleteDBClusterSnapshotResponse_httpStatus,

    -- ** ModifyDBProxyEndpoint
    modifyDBProxyEndpoint_vpcSecurityGroupIds,
    modifyDBProxyEndpoint_newDBProxyEndpointName,
    modifyDBProxyEndpoint_dbProxyEndpointName,
    modifyDBProxyEndpointResponse_dbProxyEndpoint,
    modifyDBProxyEndpointResponse_httpStatus,

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

    -- ** DescribeOptionGroupOptions
    describeOptionGroupOptions_filters,
    describeOptionGroupOptions_majorEngineVersion,
    describeOptionGroupOptions_marker,
    describeOptionGroupOptions_maxRecords,
    describeOptionGroupOptions_engineName,
    describeOptionGroupOptionsResponse_optionGroupOptions,
    describeOptionGroupOptionsResponse_marker,
    describeOptionGroupOptionsResponse_httpStatus,

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

    -- ** StopActivityStream
    stopActivityStream_applyImmediately,
    stopActivityStream_resourceArn,
    stopActivityStreamResponse_status,
    stopActivityStreamResponse_kinesisStreamName,
    stopActivityStreamResponse_kmsKeyId,
    stopActivityStreamResponse_httpStatus,

    -- ** CreateDBClusterSnapshot
    createDBClusterSnapshot_tags,
    createDBClusterSnapshot_dbClusterSnapshotIdentifier,
    createDBClusterSnapshot_dbClusterIdentifier,
    createDBClusterSnapshotResponse_dbClusterSnapshot,
    createDBClusterSnapshotResponse_httpStatus,

    -- ** DescribeDBSnapshots
    describeDBSnapshots_includeShared,
    describeDBSnapshots_filters,
    describeDBSnapshots_dbSnapshotIdentifier,
    describeDBSnapshots_snapshotType,
    describeDBSnapshots_dbInstanceIdentifier,
    describeDBSnapshots_marker,
    describeDBSnapshots_maxRecords,
    describeDBSnapshots_includePublic,
    describeDBSnapshots_dbiResourceId,
    describeDBSnapshotsResponse_marker,
    describeDBSnapshotsResponse_dbSnapshots,
    describeDBSnapshotsResponse_httpStatus,

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
    describeDBSubnetGroups_marker,
    describeDBSubnetGroups_maxRecords,
    describeDBSubnetGroupsResponse_dbSubnetGroups,
    describeDBSubnetGroupsResponse_marker,
    describeDBSubnetGroupsResponse_httpStatus,

    -- ** ModifyOptionGroup
    modifyOptionGroup_optionsToInclude,
    modifyOptionGroup_optionsToRemove,
    modifyOptionGroup_applyImmediately,
    modifyOptionGroup_optionGroupName,
    modifyOptionGroupResponse_optionGroup,
    modifyOptionGroupResponse_httpStatus,

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
    modifyDBCluster_enableGlobalWriteForwarding,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_enableHttpEndpoint,
    modifyDBCluster_allowMajorVersionUpgrade,
    modifyDBCluster_domain,
    modifyDBCluster_backtrackWindow,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_scalingConfiguration,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_optionGroupName,
    modifyDBCluster_copyTagsToSnapshot,
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_dbInstanceParameterGroupName,
    modifyDBCluster_domainIAMRoleName,
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

    -- ** DescribeGlobalClusters
    describeGlobalClusters_globalClusterIdentifier,
    describeGlobalClusters_filters,
    describeGlobalClusters_marker,
    describeGlobalClusters_maxRecords,
    describeGlobalClustersResponse_globalClusters,
    describeGlobalClustersResponse_marker,
    describeGlobalClustersResponse_httpStatus,

    -- ** StartDBInstance
    startDBInstance_dbInstanceIdentifier,
    startDBInstanceResponse_dbInstance,
    startDBInstanceResponse_httpStatus,

    -- ** DescribeExportTasks
    describeExportTasks_sourceArn,
    describeExportTasks_filters,
    describeExportTasks_marker,
    describeExportTasks_exportTaskIdentifier,
    describeExportTasks_maxRecords,
    describeExportTasksResponse_marker,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,

    -- ** CancelExportTask
    cancelExportTask_exportTaskIdentifier,
    exportTask_totalExtractedDataInGB,
    exportTask_status,
    exportTask_iamRoleArn,
    exportTask_sourceArn,
    exportTask_exportOnly,
    exportTask_taskStartTime,
    exportTask_warningMessage,
    exportTask_snapshotTime,
    exportTask_kmsKeyId,
    exportTask_taskEndTime,
    exportTask_exportTaskIdentifier,
    exportTask_s3Prefix,
    exportTask_percentProgress,
    exportTask_s3Bucket,
    exportTask_failureCause,

    -- ** ModifyDBClusterParameterGroup
    modifyDBClusterParameterGroup_dbClusterParameterGroupName,
    modifyDBClusterParameterGroup_parameters,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** RestoreDBInstanceToPointInTime
    restoreDBInstanceToPointInTime_deletionProtection,
    restoreDBInstanceToPointInTime_useLatestRestorableTime,
    restoreDBInstanceToPointInTime_publiclyAccessible,
    restoreDBInstanceToPointInTime_autoMinorVersionUpgrade,
    restoreDBInstanceToPointInTime_dbSubnetGroupName,
    restoreDBInstanceToPointInTime_restoreTime,
    restoreDBInstanceToPointInTime_iops,
    restoreDBInstanceToPointInTime_domain,
    restoreDBInstanceToPointInTime_enableCustomerOwnedIp,
    restoreDBInstanceToPointInTime_engine,
    restoreDBInstanceToPointInTime_tdeCredentialPassword,
    restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier,
    restoreDBInstanceToPointInTime_processorFeatures,
    restoreDBInstanceToPointInTime_dbInstanceClass,
    restoreDBInstanceToPointInTime_licenseModel,
    restoreDBInstanceToPointInTime_sourceDBInstanceAutomatedBackupsArn,
    restoreDBInstanceToPointInTime_maxAllocatedStorage,
    restoreDBInstanceToPointInTime_dbParameterGroupName,
    restoreDBInstanceToPointInTime_availabilityZone,
    restoreDBInstanceToPointInTime_vpcSecurityGroupIds,
    restoreDBInstanceToPointInTime_multiAZ,
    restoreDBInstanceToPointInTime_sourceDbiResourceId,
    restoreDBInstanceToPointInTime_optionGroupName,
    restoreDBInstanceToPointInTime_copyTagsToSnapshot,
    restoreDBInstanceToPointInTime_tdeCredentialArn,
    restoreDBInstanceToPointInTime_domainIAMRoleName,
    restoreDBInstanceToPointInTime_tags,
    restoreDBInstanceToPointInTime_port,
    restoreDBInstanceToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBInstanceToPointInTime_useDefaultProcessorFeatures,
    restoreDBInstanceToPointInTime_storageType,
    restoreDBInstanceToPointInTime_enableCloudwatchLogsExports,
    restoreDBInstanceToPointInTime_dbName,
    restoreDBInstanceToPointInTime_targetDBInstanceIdentifier,
    restoreDBInstanceToPointInTimeResponse_dbInstance,
    restoreDBInstanceToPointInTimeResponse_httpStatus,

    -- ** DescribeDBClusterSnapshotAttributes
    describeDBClusterSnapshotAttributes_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshotAttributesResponse_dbClusterSnapshotAttributesResult,
    describeDBClusterSnapshotAttributesResponse_httpStatus,

    -- ** ModifyDBSnapshot
    modifyDBSnapshot_engineVersion,
    modifyDBSnapshot_optionGroupName,
    modifyDBSnapshot_dbSnapshotIdentifier,
    modifyDBSnapshotResponse_dbSnapshot,
    modifyDBSnapshotResponse_httpStatus,

    -- ** DescribeDBProxyTargetGroups
    describeDBProxyTargetGroups_filters,
    describeDBProxyTargetGroups_marker,
    describeDBProxyTargetGroups_maxRecords,
    describeDBProxyTargetGroups_targetGroupName,
    describeDBProxyTargetGroups_dbProxyName,
    describeDBProxyTargetGroupsResponse_marker,
    describeDBProxyTargetGroupsResponse_targetGroups,
    describeDBProxyTargetGroupsResponse_httpStatus,

    -- ** ModifyDBProxy
    modifyDBProxy_debugLogging,
    modifyDBProxy_securityGroups,
    modifyDBProxy_auth,
    modifyDBProxy_requireTLS,
    modifyDBProxy_idleClientTimeout,
    modifyDBProxy_newDBProxyName,
    modifyDBProxy_roleArn,
    modifyDBProxy_dbProxyName,
    modifyDBProxyResponse_dbProxy,
    modifyDBProxyResponse_httpStatus,

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
    copyDBClusterSnapshot_destinationRegion,
    copyDBClusterSnapshot_kmsKeyId,
    copyDBClusterSnapshot_tags,
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
    installationMedia_engineVersion,
    installationMedia_status,
    installationMedia_installationMediaId,
    installationMedia_engineInstallationMediaPath,
    installationMedia_engine,
    installationMedia_oSInstallationMediaPath,
    installationMedia_customAvailabilityZoneId,
    installationMedia_failureCause,

    -- ** CreateGlobalCluster
    createGlobalCluster_engineVersion,
    createGlobalCluster_deletionProtection,
    createGlobalCluster_storageEncrypted,
    createGlobalCluster_sourceDBClusterIdentifier,
    createGlobalCluster_globalClusterIdentifier,
    createGlobalCluster_engine,
    createGlobalCluster_databaseName,
    createGlobalClusterResponse_globalCluster,
    createGlobalClusterResponse_httpStatus,

    -- ** ResetDBParameterGroup
    resetDBParameterGroup_resetAllParameters,
    resetDBParameterGroup_parameters,
    resetDBParameterGroup_dbParameterGroupName,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- ** FailoverGlobalCluster
    failoverGlobalCluster_globalClusterIdentifier,
    failoverGlobalCluster_targetDbClusterIdentifier,
    failoverGlobalClusterResponse_globalCluster,
    failoverGlobalClusterResponse_httpStatus,

    -- ** DescribeInstallationMedia
    describeInstallationMedia_installationMediaId,
    describeInstallationMedia_filters,
    describeInstallationMedia_marker,
    describeInstallationMedia_maxRecords,
    describeInstallationMediaResponse_marker,
    describeInstallationMediaResponse_installationMedia,
    describeInstallationMediaResponse_httpStatus,

    -- ** DeregisterDBProxyTargets
    deregisterDBProxyTargets_dbClusterIdentifiers,
    deregisterDBProxyTargets_dbInstanceIdentifiers,
    deregisterDBProxyTargets_targetGroupName,
    deregisterDBProxyTargets_dbProxyName,
    deregisterDBProxyTargetsResponse_httpStatus,

    -- ** CreateDBCluster
    createDBCluster_engineVersion,
    createDBCluster_enableGlobalWriteForwarding,
    createDBCluster_deletionProtection,
    createDBCluster_storageEncrypted,
    createDBCluster_masterUserPassword,
    createDBCluster_replicationSourceIdentifier,
    createDBCluster_enableHttpEndpoint,
    createDBCluster_globalClusterIdentifier,
    createDBCluster_masterUsername,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_domain,
    createDBCluster_backtrackWindow,
    createDBCluster_preSignedUrl,
    createDBCluster_preferredMaintenanceWindow,
    createDBCluster_availabilityZones,
    createDBCluster_destinationRegion,
    createDBCluster_characterSetName,
    createDBCluster_kmsKeyId,
    createDBCluster_preferredBackupWindow,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_vpcSecurityGroupIds,
    createDBCluster_databaseName,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_engineMode,
    createDBCluster_scalingConfiguration,
    createDBCluster_optionGroupName,
    createDBCluster_copyTagsToSnapshot,
    createDBCluster_domainIAMRoleName,
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
    failoverDBCluster_targetDBInstanceIdentifier,
    failoverDBCluster_dbClusterIdentifier,
    failoverDBClusterResponse_dbCluster,
    failoverDBClusterResponse_httpStatus,

    -- ** RevokeDBSecurityGroupIngress
    revokeDBSecurityGroupIngress_eC2SecurityGroupOwnerId,
    revokeDBSecurityGroupIngress_eC2SecurityGroupName,
    revokeDBSecurityGroupIngress_cidrip,
    revokeDBSecurityGroupIngress_eC2SecurityGroupId,
    revokeDBSecurityGroupIngress_dbSecurityGroupName,
    revokeDBSecurityGroupIngressResponse_dbSecurityGroup,
    revokeDBSecurityGroupIngressResponse_httpStatus,

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

    -- ** DeleteDBProxyEndpoint
    deleteDBProxyEndpoint_dbProxyEndpointName,
    deleteDBProxyEndpointResponse_dbProxyEndpoint,
    deleteDBProxyEndpointResponse_httpStatus,

    -- ** StopDBInstanceAutomatedBackupsReplication
    stopDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn,
    stopDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup,
    stopDBInstanceAutomatedBackupsReplicationResponse_httpStatus,

    -- ** CreateOptionGroup
    createOptionGroup_tags,
    createOptionGroup_optionGroupName,
    createOptionGroup_engineName,
    createOptionGroup_majorEngineVersion,
    createOptionGroup_optionGroupDescription,
    createOptionGroupResponse_optionGroup,
    createOptionGroupResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributesResponse_accountQuotas,
    describeAccountAttributesResponse_httpStatus,

    -- ** DeleteDBSnapshot
    deleteDBSnapshot_dbSnapshotIdentifier,
    deleteDBSnapshotResponse_dbSnapshot,
    deleteDBSnapshotResponse_httpStatus,

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

    -- ** CreateDBSecurityGroup
    createDBSecurityGroup_tags,
    createDBSecurityGroup_dbSecurityGroupName,
    createDBSecurityGroup_dbSecurityGroupDescription,
    createDBSecurityGroupResponse_dbSecurityGroup,
    createDBSecurityGroupResponse_httpStatus,

    -- ** ModifyCertificates
    modifyCertificates_certificateIdentifier,
    modifyCertificates_removeCustomerOverride,
    modifyCertificatesResponse_certificate,
    modifyCertificatesResponse_httpStatus,

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

    -- ** DescribeReservedDBInstancesOfferings
    describeReservedDBInstancesOfferings_productDescription,
    describeReservedDBInstancesOfferings_filters,
    describeReservedDBInstancesOfferings_dbInstanceClass,
    describeReservedDBInstancesOfferings_marker,
    describeReservedDBInstancesOfferings_maxRecords,
    describeReservedDBInstancesOfferings_multiAZ,
    describeReservedDBInstancesOfferings_reservedDBInstancesOfferingId,
    describeReservedDBInstancesOfferings_offeringType,
    describeReservedDBInstancesOfferings_duration,
    describeReservedDBInstancesOfferingsResponse_marker,
    describeReservedDBInstancesOfferingsResponse_reservedDBInstancesOfferings,
    describeReservedDBInstancesOfferingsResponse_httpStatus,

    -- ** DeleteDBSecurityGroup
    deleteDBSecurityGroup_dbSecurityGroupName,

    -- ** DeleteDBInstance
    deleteDBInstance_finalDBSnapshotIdentifier,
    deleteDBInstance_deleteAutomatedBackups,
    deleteDBInstance_skipFinalSnapshot,
    deleteDBInstance_dbInstanceIdentifier,
    deleteDBInstanceResponse_dbInstance,
    deleteDBInstanceResponse_httpStatus,

    -- ** StartActivityStream
    startActivityStream_engineNativeAuditFieldsIncluded,
    startActivityStream_applyImmediately,
    startActivityStream_resourceArn,
    startActivityStream_mode,
    startActivityStream_kmsKeyId,
    startActivityStreamResponse_status,
    startActivityStreamResponse_kinesisStreamName,
    startActivityStreamResponse_engineNativeAuditFieldsIncluded,
    startActivityStreamResponse_mode,
    startActivityStreamResponse_kmsKeyId,
    startActivityStreamResponse_applyImmediately,
    startActivityStreamResponse_httpStatus,

    -- ** CreateDBInstanceReadReplica
    createDBInstanceReadReplica_deletionProtection,
    createDBInstanceReadReplica_publiclyAccessible,
    createDBInstanceReadReplica_autoMinorVersionUpgrade,
    createDBInstanceReadReplica_dbSubnetGroupName,
    createDBInstanceReadReplica_monitoringRoleArn,
    createDBInstanceReadReplica_iops,
    createDBInstanceReadReplica_domain,
    createDBInstanceReadReplica_replicaMode,
    createDBInstanceReadReplica_monitoringInterval,
    createDBInstanceReadReplica_preSignedUrl,
    createDBInstanceReadReplica_processorFeatures,
    createDBInstanceReadReplica_dbInstanceClass,
    createDBInstanceReadReplica_performanceInsightsRetentionPeriod,
    createDBInstanceReadReplica_destinationRegion,
    createDBInstanceReadReplica_maxAllocatedStorage,
    createDBInstanceReadReplica_enablePerformanceInsights,
    createDBInstanceReadReplica_kmsKeyId,
    createDBInstanceReadReplica_dbParameterGroupName,
    createDBInstanceReadReplica_availabilityZone,
    createDBInstanceReadReplica_performanceInsightsKMSKeyId,
    createDBInstanceReadReplica_vpcSecurityGroupIds,
    createDBInstanceReadReplica_multiAZ,
    createDBInstanceReadReplica_optionGroupName,
    createDBInstanceReadReplica_copyTagsToSnapshot,
    createDBInstanceReadReplica_domainIAMRoleName,
    createDBInstanceReadReplica_tags,
    createDBInstanceReadReplica_port,
    createDBInstanceReadReplica_enableIAMDatabaseAuthentication,
    createDBInstanceReadReplica_useDefaultProcessorFeatures,
    createDBInstanceReadReplica_storageType,
    createDBInstanceReadReplica_enableCloudwatchLogsExports,
    createDBInstanceReadReplica_dbInstanceIdentifier,
    createDBInstanceReadReplica_sourceDBInstanceIdentifier,
    createDBInstanceReadReplicaResponse_dbInstance,
    createDBInstanceReadReplicaResponse_httpStatus,

    -- ** DeleteDBParameterGroup
    deleteDBParameterGroup_dbParameterGroupName,

    -- ** ModifyCurrentDBClusterCapacity
    modifyCurrentDBClusterCapacity_timeoutAction,
    modifyCurrentDBClusterCapacity_capacity,
    modifyCurrentDBClusterCapacity_secondsBeforeTimeout,
    modifyCurrentDBClusterCapacity_dbClusterIdentifier,
    modifyCurrentDBClusterCapacityResponse_dbClusterIdentifier,
    modifyCurrentDBClusterCapacityResponse_timeoutAction,
    modifyCurrentDBClusterCapacityResponse_currentCapacity,
    modifyCurrentDBClusterCapacityResponse_pendingCapacity,
    modifyCurrentDBClusterCapacityResponse_secondsBeforeTimeout,
    modifyCurrentDBClusterCapacityResponse_httpStatus,

    -- ** ModifyGlobalCluster
    modifyGlobalCluster_engineVersion,
    modifyGlobalCluster_deletionProtection,
    modifyGlobalCluster_globalClusterIdentifier,
    modifyGlobalCluster_allowMajorVersionUpgrade,
    modifyGlobalCluster_newGlobalClusterIdentifier,
    modifyGlobalClusterResponse_globalCluster,
    modifyGlobalClusterResponse_httpStatus,

    -- ** RegisterDBProxyTargets
    registerDBProxyTargets_dbClusterIdentifiers,
    registerDBProxyTargets_dbInstanceIdentifiers,
    registerDBProxyTargets_targetGroupName,
    registerDBProxyTargets_dbProxyName,
    registerDBProxyTargetsResponse_dbProxyTargets,
    registerDBProxyTargetsResponse_httpStatus,

    -- ** DescribeDBSecurityGroups
    describeDBSecurityGroups_filters,
    describeDBSecurityGroups_marker,
    describeDBSecurityGroups_maxRecords,
    describeDBSecurityGroups_dbSecurityGroupName,
    describeDBSecurityGroupsResponse_dbSecurityGroups,
    describeDBSecurityGroupsResponse_marker,
    describeDBSecurityGroupsResponse_httpStatus,

    -- ** CopyOptionGroup
    copyOptionGroup_tags,
    copyOptionGroup_sourceOptionGroupIdentifier,
    copyOptionGroup_targetOptionGroupIdentifier,
    copyOptionGroup_targetOptionGroupDescription,
    copyOptionGroupResponse_optionGroup,
    copyOptionGroupResponse_httpStatus,

    -- ** RestoreDBClusterToPointInTime
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_domain,
    restoreDBClusterToPointInTime_backtrackWindow,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
    restoreDBClusterToPointInTime_dbClusterParameterGroupName,
    restoreDBClusterToPointInTime_engineMode,
    restoreDBClusterToPointInTime_scalingConfiguration,
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_optionGroupName,
    restoreDBClusterToPointInTime_copyTagsToSnapshot,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_domainIAMRoleName,
    restoreDBClusterToPointInTime_tags,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_dbClusterIdentifier,
    restoreDBClusterToPointInTime_sourceDBClusterIdentifier,
    restoreDBClusterToPointInTimeResponse_dbCluster,
    restoreDBClusterToPointInTimeResponse_httpStatus,

    -- ** DeleteInstallationMedia
    deleteInstallationMedia_installationMediaId,
    installationMedia_engineVersion,
    installationMedia_status,
    installationMedia_installationMediaId,
    installationMedia_engineInstallationMediaPath,
    installationMedia_engine,
    installationMedia_oSInstallationMediaPath,
    installationMedia_customAvailabilityZoneId,
    installationMedia_failureCause,

    -- ** DescribeDBInstances
    describeDBInstances_filters,
    describeDBInstances_dbInstanceIdentifier,
    describeDBInstances_marker,
    describeDBInstances_maxRecords,
    describeDBInstancesResponse_dbInstances,
    describeDBInstancesResponse_marker,
    describeDBInstancesResponse_httpStatus,

    -- ** RestoreDBInstanceFromS3
    restoreDBInstanceFromS3_engineVersion,
    restoreDBInstanceFromS3_dbSecurityGroups,
    restoreDBInstanceFromS3_deletionProtection,
    restoreDBInstanceFromS3_storageEncrypted,
    restoreDBInstanceFromS3_masterUserPassword,
    restoreDBInstanceFromS3_publiclyAccessible,
    restoreDBInstanceFromS3_autoMinorVersionUpgrade,
    restoreDBInstanceFromS3_masterUsername,
    restoreDBInstanceFromS3_dbSubnetGroupName,
    restoreDBInstanceFromS3_monitoringRoleArn,
    restoreDBInstanceFromS3_iops,
    restoreDBInstanceFromS3_monitoringInterval,
    restoreDBInstanceFromS3_processorFeatures,
    restoreDBInstanceFromS3_licenseModel,
    restoreDBInstanceFromS3_preferredMaintenanceWindow,
    restoreDBInstanceFromS3_performanceInsightsRetentionPeriod,
    restoreDBInstanceFromS3_maxAllocatedStorage,
    restoreDBInstanceFromS3_enablePerformanceInsights,
    restoreDBInstanceFromS3_kmsKeyId,
    restoreDBInstanceFromS3_dbParameterGroupName,
    restoreDBInstanceFromS3_preferredBackupWindow,
    restoreDBInstanceFromS3_availabilityZone,
    restoreDBInstanceFromS3_backupRetentionPeriod,
    restoreDBInstanceFromS3_performanceInsightsKMSKeyId,
    restoreDBInstanceFromS3_vpcSecurityGroupIds,
    restoreDBInstanceFromS3_multiAZ,
    restoreDBInstanceFromS3_s3Prefix,
    restoreDBInstanceFromS3_allocatedStorage,
    restoreDBInstanceFromS3_optionGroupName,
    restoreDBInstanceFromS3_copyTagsToSnapshot,
    restoreDBInstanceFromS3_tags,
    restoreDBInstanceFromS3_port,
    restoreDBInstanceFromS3_enableIAMDatabaseAuthentication,
    restoreDBInstanceFromS3_useDefaultProcessorFeatures,
    restoreDBInstanceFromS3_storageType,
    restoreDBInstanceFromS3_enableCloudwatchLogsExports,
    restoreDBInstanceFromS3_dbName,
    restoreDBInstanceFromS3_dbInstanceIdentifier,
    restoreDBInstanceFromS3_dbInstanceClass,
    restoreDBInstanceFromS3_engine,
    restoreDBInstanceFromS3_sourceEngine,
    restoreDBInstanceFromS3_sourceEngineVersion,
    restoreDBInstanceFromS3_s3BucketName,
    restoreDBInstanceFromS3_s3IngestionRoleArn,
    restoreDBInstanceFromS3Response_dbInstance,
    restoreDBInstanceFromS3Response_httpStatus,

    -- ** DownloadDBLogFilePortion
    downloadDBLogFilePortion_numberOfLines,
    downloadDBLogFilePortion_marker,
    downloadDBLogFilePortion_dbInstanceIdentifier,
    downloadDBLogFilePortion_logFileName,
    downloadDBLogFilePortionResponse_logFileData,
    downloadDBLogFilePortionResponse_additionalDataPending,
    downloadDBLogFilePortionResponse_marker,
    downloadDBLogFilePortionResponse_httpStatus,

    -- ** DescribeDBProxies
    describeDBProxies_filters,
    describeDBProxies_marker,
    describeDBProxies_maxRecords,
    describeDBProxies_dbProxyName,
    describeDBProxiesResponse_dbProxies,
    describeDBProxiesResponse_marker,
    describeDBProxiesResponse_httpStatus,

    -- ** StartExportTask
    startExportTask_exportOnly,
    startExportTask_s3Prefix,
    startExportTask_exportTaskIdentifier,
    startExportTask_sourceArn,
    startExportTask_s3BucketName,
    startExportTask_iamRoleArn,
    startExportTask_kmsKeyId,
    exportTask_totalExtractedDataInGB,
    exportTask_status,
    exportTask_iamRoleArn,
    exportTask_sourceArn,
    exportTask_exportOnly,
    exportTask_taskStartTime,
    exportTask_warningMessage,
    exportTask_snapshotTime,
    exportTask_kmsKeyId,
    exportTask_taskEndTime,
    exportTask_exportTaskIdentifier,
    exportTask_s3Prefix,
    exportTask_percentProgress,
    exportTask_s3Bucket,
    exportTask_failureCause,

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
    certificate_certificateType,
    certificate_customerOverride,
    certificate_certificateArn,
    certificate_customerOverrideValidTill,
    certificate_validTill,
    certificate_certificateIdentifier,
    certificate_thumbprint,
    certificate_validFrom,

    -- ** CharacterSet
    characterSet_characterSetName,
    characterSet_characterSetDescription,

    -- ** CloudwatchLogsExportConfiguration
    cloudwatchLogsExportConfiguration_disableLogTypes,
    cloudwatchLogsExportConfiguration_enableLogTypes,

    -- ** ClusterPendingModifiedValues
    clusterPendingModifiedValues_engineVersion,
    clusterPendingModifiedValues_dbClusterIdentifier,
    clusterPendingModifiedValues_masterUserPassword,
    clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled,
    clusterPendingModifiedValues_pendingCloudwatchLogsExports,

    -- ** ConnectionPoolConfiguration
    connectionPoolConfiguration_maxIdleConnectionsPercent,
    connectionPoolConfiguration_sessionPinningFilters,
    connectionPoolConfiguration_maxConnectionsPercent,
    connectionPoolConfiguration_connectionBorrowTimeout,
    connectionPoolConfiguration_initQuery,

    -- ** ConnectionPoolConfigurationInfo
    connectionPoolConfigurationInfo_maxIdleConnectionsPercent,
    connectionPoolConfigurationInfo_sessionPinningFilters,
    connectionPoolConfigurationInfo_maxConnectionsPercent,
    connectionPoolConfigurationInfo_connectionBorrowTimeout,
    connectionPoolConfigurationInfo_initQuery,

    -- ** CustomAvailabilityZone
    customAvailabilityZone_vpnDetails,
    customAvailabilityZone_customAvailabilityZoneName,
    customAvailabilityZone_customAvailabilityZoneId,
    customAvailabilityZone_customAvailabilityZoneStatus,

    -- ** DBCluster
    dbCluster_backtrackConsumedChangeRecords,
    dbCluster_engineVersion,
    dbCluster_status,
    dbCluster_deletionProtection,
    dbCluster_automaticRestartTime,
    dbCluster_storageEncrypted,
    dbCluster_dbClusterIdentifier,
    dbCluster_dbClusterMembers,
    dbCluster_readReplicaIdentifiers,
    dbCluster_replicationSourceIdentifier,
    dbCluster_activityStreamKinesisStreamName,
    dbCluster_hostedZoneId,
    dbCluster_dbClusterParameterGroup,
    dbCluster_masterUsername,
    dbCluster_iAMDatabaseAuthenticationEnabled,
    dbCluster_globalWriteForwardingRequested,
    dbCluster_earliestBacktrackTime,
    dbCluster_backtrackWindow,
    dbCluster_tagList,
    dbCluster_dbClusterResourceId,
    dbCluster_earliestRestorableTime,
    dbCluster_customEndpoints,
    dbCluster_engine,
    dbCluster_httpEndpointEnabled,
    dbCluster_dbClusterArn,
    dbCluster_cloneGroupId,
    dbCluster_latestRestorableTime,
    dbCluster_crossAccountClone,
    dbCluster_capacity,
    dbCluster_preferredMaintenanceWindow,
    dbCluster_availabilityZones,
    dbCluster_characterSetName,
    dbCluster_kmsKeyId,
    dbCluster_preferredBackupWindow,
    dbCluster_associatedRoles,
    dbCluster_vpcSecurityGroups,
    dbCluster_backupRetentionPeriod,
    dbCluster_dbSubnetGroup,
    dbCluster_activityStreamMode,
    dbCluster_databaseName,
    dbCluster_multiAZ,
    dbCluster_engineMode,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_activityStreamStatus,
    dbCluster_allocatedStorage,
    dbCluster_copyTagsToSnapshot,
    dbCluster_clusterCreateTime,
    dbCluster_endpoint,
    dbCluster_scalingConfigurationInfo,
    dbCluster_activityStreamKmsKeyId,
    dbCluster_percentProgress,
    dbCluster_pendingModifiedValues,
    dbCluster_readerEndpoint,
    dbCluster_globalWriteForwardingStatus,
    dbCluster_port,
    dbCluster_domainMemberships,
    dbCluster_dbClusterOptionGroupMemberships,

    -- ** DBClusterBacktrack
    dbClusterBacktrack_status,
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_backtrackTo,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_backtrackedFrom,
    dbClusterBacktrack_backtrackRequestCreationTime,

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
    dbClusterSnapshot_tagList,
    dbClusterSnapshot_dbClusterSnapshotIdentifier,
    dbClusterSnapshot_engine,
    dbClusterSnapshot_licenseModel,
    dbClusterSnapshot_availabilityZones,
    dbClusterSnapshot_snapshotType,
    dbClusterSnapshot_kmsKeyId,
    dbClusterSnapshot_engineMode,
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
    dbEngineVersion_status,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_validUpgradeTarget,
    dbEngineVersion_supportsParallelQuery,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_exportableLogTypes,

    -- ** DBInstance
    dbInstance_engineVersion,
    dbInstance_dbSecurityGroups,
    dbInstance_deletionProtection,
    dbInstance_automaticRestartTime,
    dbInstance_storageEncrypted,
    dbInstance_dbClusterIdentifier,
    dbInstance_publiclyAccessible,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_dbInstanceArn,
    dbInstance_activityStreamKinesisStreamName,
    dbInstance_activityStreamEngineNativeAuditFieldsIncluded,
    dbInstance_masterUsername,
    dbInstance_readReplicaDBInstanceIdentifiers,
    dbInstance_iAMDatabaseAuthenticationEnabled,
    dbInstance_monitoringRoleArn,
    dbInstance_iops,
    dbInstance_instanceCreateTime,
    dbInstance_tagList,
    dbInstance_readReplicaSourceDBInstanceIdentifier,
    dbInstance_replicaMode,
    dbInstance_monitoringInterval,
    dbInstance_engine,
    dbInstance_processorFeatures,
    dbInstance_latestRestorableTime,
    dbInstance_dbInstanceClass,
    dbInstance_promotionTier,
    dbInstance_awsBackupRecoveryPointArn,
    dbInstance_licenseModel,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_performanceInsightsRetentionPeriod,
    dbInstance_cACertificateIdentifier,
    dbInstance_dbInstanceIdentifier,
    dbInstance_characterSetName,
    dbInstance_maxAllocatedStorage,
    dbInstance_customerOwnedIpEnabled,
    dbInstance_kmsKeyId,
    dbInstance_preferredBackupWindow,
    dbInstance_associatedRoles,
    dbInstance_availabilityZone,
    dbInstance_vpcSecurityGroups,
    dbInstance_backupRetentionPeriod,
    dbInstance_ncharCharacterSetName,
    dbInstance_performanceInsightsKMSKeyId,
    dbInstance_dbSubnetGroup,
    dbInstance_activityStreamMode,
    dbInstance_multiAZ,
    dbInstance_listenerEndpoint,
    dbInstance_optionGroupMemberships,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_enhancedMonitoringResourceArn,
    dbInstance_secondaryAvailabilityZone,
    dbInstance_activityStreamStatus,
    dbInstance_performanceInsightsEnabled,
    dbInstance_allocatedStorage,
    dbInstance_dbiResourceId,
    dbInstance_dbParameterGroups,
    dbInstance_copyTagsToSnapshot,
    dbInstance_timezone,
    dbInstance_tdeCredentialArn,
    dbInstance_dbInstanceAutomatedBackupsReplications,
    dbInstance_endpoint,
    dbInstance_dbInstanceStatus,
    dbInstance_dbInstancePort,
    dbInstance_activityStreamKmsKeyId,
    dbInstance_pendingModifiedValues,
    dbInstance_readReplicaDBClusterIdentifiers,
    dbInstance_storageType,
    dbInstance_statusInfos,
    dbInstance_domainMemberships,
    dbInstance_dbName,

    -- ** DBInstanceAutomatedBackup
    dbInstanceAutomatedBackup_restoreWindow,
    dbInstanceAutomatedBackup_engineVersion,
    dbInstanceAutomatedBackup_status,
    dbInstanceAutomatedBackup_dbInstanceArn,
    dbInstanceAutomatedBackup_masterUsername,
    dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled,
    dbInstanceAutomatedBackup_iops,
    dbInstanceAutomatedBackup_vpcId,
    dbInstanceAutomatedBackup_instanceCreateTime,
    dbInstanceAutomatedBackup_engine,
    dbInstanceAutomatedBackup_encrypted,
    dbInstanceAutomatedBackup_licenseModel,
    dbInstanceAutomatedBackup_dbInstanceIdentifier,
    dbInstanceAutomatedBackup_kmsKeyId,
    dbInstanceAutomatedBackup_availabilityZone,
    dbInstanceAutomatedBackup_backupRetentionPeriod,
    dbInstanceAutomatedBackup_region,
    dbInstanceAutomatedBackup_allocatedStorage,
    dbInstanceAutomatedBackup_dbiResourceId,
    dbInstanceAutomatedBackup_optionGroupName,
    dbInstanceAutomatedBackup_timezone,
    dbInstanceAutomatedBackup_tdeCredentialArn,
    dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications,
    dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn,
    dbInstanceAutomatedBackup_port,
    dbInstanceAutomatedBackup_storageType,

    -- ** DBInstanceAutomatedBackupsReplication
    dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn,

    -- ** DBInstanceRole
    dbInstanceRole_status,
    dbInstanceRole_featureName,
    dbInstanceRole_roleArn,

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

    -- ** DBProxy
    dbProxy_status,
    dbProxy_dbProxyArn,
    dbProxy_debugLogging,
    dbProxy_vpcSubnetIds,
    dbProxy_vpcId,
    dbProxy_engineFamily,
    dbProxy_auth,
    dbProxy_requireTLS,
    dbProxy_idleClientTimeout,
    dbProxy_updatedDate,
    dbProxy_createdDate,
    dbProxy_vpcSecurityGroupIds,
    dbProxy_dbProxyName,
    dbProxy_endpoint,
    dbProxy_roleArn,

    -- ** DBProxyEndpoint
    dbProxyEndpoint_status,
    dbProxyEndpoint_dbProxyEndpointArn,
    dbProxyEndpoint_targetRole,
    dbProxyEndpoint_vpcSubnetIds,
    dbProxyEndpoint_vpcId,
    dbProxyEndpoint_createdDate,
    dbProxyEndpoint_vpcSecurityGroupIds,
    dbProxyEndpoint_dbProxyName,
    dbProxyEndpoint_endpoint,
    dbProxyEndpoint_dbProxyEndpointName,
    dbProxyEndpoint_isDefault,

    -- ** DBProxyTarget
    dbProxyTarget_targetArn,
    dbProxyTarget_targetHealth,
    dbProxyTarget_trackedClusterId,
    dbProxyTarget_role,
    dbProxyTarget_rdsResourceId,
    dbProxyTarget_type,
    dbProxyTarget_endpoint,
    dbProxyTarget_port,

    -- ** DBProxyTargetGroup
    dbProxyTargetGroup_status,
    dbProxyTargetGroup_connectionPoolConfig,
    dbProxyTargetGroup_targetGroupArn,
    dbProxyTargetGroup_updatedDate,
    dbProxyTargetGroup_createdDate,
    dbProxyTargetGroup_dbProxyName,
    dbProxyTargetGroup_targetGroupName,
    dbProxyTargetGroup_isDefault,

    -- ** DBSecurityGroup
    dbSecurityGroup_vpcId,
    dbSecurityGroup_ownerId,
    dbSecurityGroup_dbSecurityGroupArn,
    dbSecurityGroup_iPRanges,
    dbSecurityGroup_dbSecurityGroupName,
    dbSecurityGroup_eC2SecurityGroups,
    dbSecurityGroup_dbSecurityGroupDescription,

    -- ** DBSecurityGroupMembership
    dbSecurityGroupMembership_status,
    dbSecurityGroupMembership_dbSecurityGroupName,

    -- ** DBSnapshot
    dbSnapshot_originalSnapshotCreateTime,
    dbSnapshot_engineVersion,
    dbSnapshot_status,
    dbSnapshot_dbSnapshotArn,
    dbSnapshot_masterUsername,
    dbSnapshot_sourceRegion,
    dbSnapshot_iAMDatabaseAuthenticationEnabled,
    dbSnapshot_iops,
    dbSnapshot_vpcId,
    dbSnapshot_instanceCreateTime,
    dbSnapshot_tagList,
    dbSnapshot_engine,
    dbSnapshot_encrypted,
    dbSnapshot_dbSnapshotIdentifier,
    dbSnapshot_processorFeatures,
    dbSnapshot_licenseModel,
    dbSnapshot_sourceDBSnapshotIdentifier,
    dbSnapshot_snapshotType,
    dbSnapshot_dbInstanceIdentifier,
    dbSnapshot_kmsKeyId,
    dbSnapshot_availabilityZone,
    dbSnapshot_snapshotCreateTime,
    dbSnapshot_allocatedStorage,
    dbSnapshot_dbiResourceId,
    dbSnapshot_optionGroupName,
    dbSnapshot_timezone,
    dbSnapshot_tdeCredentialArn,
    dbSnapshot_percentProgress,
    dbSnapshot_port,
    dbSnapshot_storageType,

    -- ** DBSnapshotAttribute
    dbSnapshotAttribute_attributeValues,
    dbSnapshotAttribute_attributeName,

    -- ** DBSnapshotAttributesResult
    dbSnapshotAttributesResult_dbSnapshotIdentifier,
    dbSnapshotAttributesResult_dbSnapshotAttributes,

    -- ** DBSubnetGroup
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_vpcId,
    dbSubnetGroup_subnets,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_subnetGroupStatus,

    -- ** DescribeDBLogFilesDetails
    describeDBLogFilesDetails_lastWritten,
    describeDBLogFilesDetails_size,
    describeDBLogFilesDetails_logFileName,

    -- ** DomainMembership
    domainMembership_status,
    domainMembership_fqdn,
    domainMembership_domain,
    domainMembership_iAMRoleName,

    -- ** DoubleRange
    doubleRange_to,
    doubleRange_from,

    -- ** EC2SecurityGroup
    eC2SecurityGroup_status,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_eC2SecurityGroupName,
    eC2SecurityGroup_eC2SecurityGroupId,

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

    -- ** ExportTask
    exportTask_totalExtractedDataInGB,
    exportTask_status,
    exportTask_iamRoleArn,
    exportTask_sourceArn,
    exportTask_exportOnly,
    exportTask_taskStartTime,
    exportTask_warningMessage,
    exportTask_snapshotTime,
    exportTask_kmsKeyId,
    exportTask_taskEndTime,
    exportTask_exportTaskIdentifier,
    exportTask_s3Prefix,
    exportTask_percentProgress,
    exportTask_s3Bucket,
    exportTask_failureCause,

    -- ** FailoverState
    failoverState_status,
    failoverState_toDbClusterArn,
    failoverState_fromDbClusterArn,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** GlobalCluster
    globalCluster_engineVersion,
    globalCluster_status,
    globalCluster_deletionProtection,
    globalCluster_storageEncrypted,
    globalCluster_globalClusterIdentifier,
    globalCluster_engine,
    globalCluster_globalClusterArn,
    globalCluster_databaseName,
    globalCluster_globalClusterMembers,
    globalCluster_failoverState,
    globalCluster_globalClusterResourceId,

    -- ** GlobalClusterMember
    globalClusterMember_readers,
    globalClusterMember_dbClusterArn,
    globalClusterMember_isWriter,
    globalClusterMember_globalWriteForwardingStatus,

    -- ** IPRange
    iPRange_status,
    iPRange_cidrip,

    -- ** InstallationMedia
    installationMedia_engineVersion,
    installationMedia_status,
    installationMedia_installationMediaId,
    installationMedia_engineInstallationMediaPath,
    installationMedia_engine,
    installationMedia_oSInstallationMediaPath,
    installationMedia_customAvailabilityZoneId,
    installationMedia_failureCause,

    -- ** InstallationMediaFailureCause
    installationMediaFailureCause_message,

    -- ** MinimumEngineVersionPerAllowedValue
    minimumEngineVersionPerAllowedValue_minimumEngineVersion,
    minimumEngineVersionPerAllowedValue_allowedValue,

    -- ** Option
    option_optionName,
    option_permanent,
    option_persistent,
    option_optionDescription,
    option_optionSettings,
    option_vpcSecurityGroupMemberships,
    option_dbSecurityGroupMemberships,
    option_optionVersion,
    option_port,

    -- ** OptionConfiguration
    optionConfiguration_optionSettings,
    optionConfiguration_vpcSecurityGroupMemberships,
    optionConfiguration_dbSecurityGroupMemberships,
    optionConfiguration_optionVersion,
    optionConfiguration_port,
    optionConfiguration_optionName,

    -- ** OptionGroup
    optionGroup_optionGroupDescription,
    optionGroup_vpcId,
    optionGroup_allowsVpcAndNonVpcInstanceMemberships,
    optionGroup_engineName,
    optionGroup_optionGroupArn,
    optionGroup_majorEngineVersion,
    optionGroup_options,
    optionGroup_optionGroupName,

    -- ** OptionGroupMembership
    optionGroupMembership_status,
    optionGroupMembership_optionGroupName,

    -- ** OptionGroupOption
    optionGroupOption_minimumRequiredMinorEngineVersion,
    optionGroupOption_optionsConflictsWith,
    optionGroupOption_permanent,
    optionGroupOption_persistent,
    optionGroupOption_optionGroupOptionVersions,
    optionGroupOption_engineName,
    optionGroupOption_majorEngineVersion,
    optionGroupOption_name,
    optionGroupOption_supportsOptionVersionDowngrade,
    optionGroupOption_defaultPort,
    optionGroupOption_optionGroupOptionSettings,
    optionGroupOption_requiresAutoMinorEngineVersionUpgrade,
    optionGroupOption_portRequired,
    optionGroupOption_description,
    optionGroupOption_optionsDependedOn,
    optionGroupOption_vpcOnly,

    -- ** OptionGroupOptionSetting
    optionGroupOptionSetting_applyType,
    optionGroupOptionSetting_minimumEngineVersionPerAllowedValue,
    optionGroupOptionSetting_settingName,
    optionGroupOptionSetting_defaultValue,
    optionGroupOptionSetting_isModifiable,
    optionGroupOptionSetting_settingDescription,
    optionGroupOptionSetting_allowedValues,
    optionGroupOptionSetting_isRequired,

    -- ** OptionSetting
    optionSetting_isCollection,
    optionSetting_applyType,
    optionSetting_value,
    optionSetting_name,
    optionSetting_defaultValue,
    optionSetting_isModifiable,
    optionSetting_dataType,
    optionSetting_allowedValues,
    optionSetting_description,

    -- ** OptionVersion
    optionVersion_version,
    optionVersion_isDefault,

    -- ** OrderableDBInstanceOption
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_minIopsPerGib,
    orderableDBInstanceOption_supportsIAMDatabaseAuthentication,
    orderableDBInstanceOption_minIopsPerDbInstance,
    orderableDBInstanceOption_multiAZCapable,
    orderableDBInstanceOption_maxStorageSize,
    orderableDBInstanceOption_supportedEngineModes,
    orderableDBInstanceOption_availabilityZoneGroup,
    orderableDBInstanceOption_availableProcessorFeatures,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_minStorageSize,
    orderableDBInstanceOption_outpostCapable,
    orderableDBInstanceOption_supportsIops,
    orderableDBInstanceOption_supportsKerberosAuthentication,
    orderableDBInstanceOption_supportsPerformanceInsights,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_supportsGlobalDatabases,
    orderableDBInstanceOption_licenseModel,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_supportedActivityStreamModes,
    orderableDBInstanceOption_supportsStorageAutoscaling,
    orderableDBInstanceOption_supportsStorageEncryption,
    orderableDBInstanceOption_readReplicaCapable,
    orderableDBInstanceOption_maxIopsPerGib,
    orderableDBInstanceOption_vpc,
    orderableDBInstanceOption_supportsEnhancedMonitoring,
    orderableDBInstanceOption_maxIopsPerDbInstance,
    orderableDBInstanceOption_storageType,

    -- ** Outpost
    outpost_arn,

    -- ** Parameter
    parameter_applyType,
    parameter_parameterValue,
    parameter_supportedEngineModes,
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
    pendingModifiedValues_iAMDatabaseAuthenticationEnabled,
    pendingModifiedValues_iops,
    pendingModifiedValues_processorFeatures,
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

    -- ** ProcessorFeature
    processorFeature_value,
    processorFeature_name,

    -- ** Range
    range_to,
    range_from,
    range_step,

    -- ** RecurringCharge
    recurringCharge_recurringChargeFrequency,
    recurringCharge_recurringChargeAmount,

    -- ** ReservedDBInstance
    reservedDBInstance_dbInstanceCount,
    reservedDBInstance_state,
    reservedDBInstance_currencyCode,
    reservedDBInstance_startTime,
    reservedDBInstance_productDescription,
    reservedDBInstance_leaseId,
    reservedDBInstance_reservedDBInstanceId,
    reservedDBInstance_reservedDBInstanceArn,
    reservedDBInstance_dbInstanceClass,
    reservedDBInstance_multiAZ,
    reservedDBInstance_reservedDBInstancesOfferingId,
    reservedDBInstance_recurringCharges,
    reservedDBInstance_offeringType,
    reservedDBInstance_usagePrice,
    reservedDBInstance_fixedPrice,
    reservedDBInstance_duration,

    -- ** ReservedDBInstancesOffering
    reservedDBInstancesOffering_currencyCode,
    reservedDBInstancesOffering_productDescription,
    reservedDBInstancesOffering_dbInstanceClass,
    reservedDBInstancesOffering_multiAZ,
    reservedDBInstancesOffering_reservedDBInstancesOfferingId,
    reservedDBInstancesOffering_recurringCharges,
    reservedDBInstancesOffering_offeringType,
    reservedDBInstancesOffering_usagePrice,
    reservedDBInstancesOffering_fixedPrice,
    reservedDBInstancesOffering_duration,

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- ** RestoreWindow
    restoreWindow_latestTime,
    restoreWindow_earliestTime,

    -- ** ScalingConfiguration
    scalingConfiguration_secondsUntilAutoPause,
    scalingConfiguration_timeoutAction,
    scalingConfiguration_autoPause,
    scalingConfiguration_maxCapacity,
    scalingConfiguration_minCapacity,
    scalingConfiguration_secondsBeforeTimeout,

    -- ** ScalingConfigurationInfo
    scalingConfigurationInfo_secondsUntilAutoPause,
    scalingConfigurationInfo_timeoutAction,
    scalingConfigurationInfo_autoPause,
    scalingConfigurationInfo_maxCapacity,
    scalingConfigurationInfo_minCapacity,
    scalingConfigurationInfo_secondsBeforeTimeout,

    -- ** SourceRegion
    sourceRegion_supportsDBInstanceAutomatedBackupsReplication,
    sourceRegion_status,
    sourceRegion_regionName,
    sourceRegion_endpoint,

    -- ** Subnet
    subnet_subnetStatus,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,
    subnet_subnetOutpost,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TargetHealth
    targetHealth_state,
    targetHealth_reason,
    targetHealth_description,

    -- ** Timezone
    timezone_timezoneName,

    -- ** UpgradeTarget
    upgradeTarget_engineVersion,
    upgradeTarget_supportedEngineModes,
    upgradeTarget_isMajorVersionUpgrade,
    upgradeTarget_engine,
    upgradeTarget_supportsGlobalDatabases,
    upgradeTarget_autoUpgrade,
    upgradeTarget_supportsParallelQuery,
    upgradeTarget_description,

    -- ** UserAuthConfig
    userAuthConfig_iAMAuth,
    userAuthConfig_userName,
    userAuthConfig_authScheme,
    userAuthConfig_secretArn,
    userAuthConfig_description,

    -- ** UserAuthConfigInfo
    userAuthConfigInfo_iAMAuth,
    userAuthConfigInfo_userName,
    userAuthConfigInfo_authScheme,
    userAuthConfigInfo_secretArn,
    userAuthConfigInfo_description,

    -- ** ValidDBInstanceModificationsMessage
    validDBInstanceModificationsMessage_validProcessorFeatures,
    validDBInstanceModificationsMessage_storage,

    -- ** ValidStorageOptions
    validStorageOptions_storageSize,
    validStorageOptions_provisionedIops,
    validStorageOptions_iopsToStorageRatio,
    validStorageOptions_supportsStorageAutoscaling,
    validStorageOptions_storageType,

    -- ** VpcSecurityGroupMembership
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,

    -- ** VpnDetails
    vpnDetails_vpnName,
    vpnDetails_vpnTunnelOriginatorIP,
    vpnDetails_vpnId,
    vpnDetails_vpnState,
    vpnDetails_vpnPSK,
    vpnDetails_vpnGatewayIp,
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
import Network.AWS.RDS.RestoreDBClusterFromS3
import Network.AWS.RDS.RestoreDBClusterFromSnapshot
import Network.AWS.RDS.RestoreDBClusterToPointInTime
import Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
import Network.AWS.RDS.RestoreDBInstanceFromS3
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
