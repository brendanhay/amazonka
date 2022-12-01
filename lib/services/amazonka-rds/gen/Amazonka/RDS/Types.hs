{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDS.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _OptionGroupNotFoundFault,
    _InsufficientDBInstanceCapacityFault,
    _InvalidDBClusterSnapshotStateFault,
    _DBSecurityGroupNotSupportedFault,
    _SubnetAlreadyInUse,
    _PointInTimeRestoreNotEnabledFault,
    _InvalidDBSecurityGroupStateFault,
    _InvalidDBParameterGroupStateFault,
    _DBClusterBacktrackNotFoundFault,
    _DBSecurityGroupAlreadyExistsFault,
    _SnapshotQuotaExceededFault,
    _InvalidSubnet,
    _DBClusterAlreadyExistsFault,
    _DBParameterGroupNotFoundFault,
    _AuthorizationAlreadyExistsFault,
    _DBClusterEndpointAlreadyExistsFault,
    _ReservedDBInstanceNotFoundFault,
    _DBProxyNotFoundFault,
    _SubscriptionAlreadyExistFault,
    _DBInstanceAlreadyExistsFault,
    _InvalidDBSubnetGroupStateFault,
    _SNSInvalidTopicFault,
    _DBClusterEndpointNotFoundFault,
    _DBProxyTargetNotFoundFault,
    _InvalidDBClusterCapacityFault,
    _InvalidDBProxyEndpointStateFault,
    _DBParameterGroupQuotaExceededFault,
    _SubscriptionNotFoundFault,
    _ExportTaskNotFoundFault,
    _DBSnapshotNotFoundFault,
    _InvalidDBSubnetStateFault,
    _DBUpgradeDependencyFailureFault,
    _DBInstanceRoleNotFoundFault,
    _DBLogFileNotFoundFault,
    _DBSubnetGroupAlreadyExistsFault,
    _DBInstanceNotFoundFault,
    _InstanceQuotaExceededFault,
    _InvalidOptionGroupStateFault,
    _InvalidExportTaskStateFault,
    _DBSubnetGroupNotAllowedFault,
    _DBInstanceAutomatedBackupQuotaExceededFault,
    _IamRoleMissingPermissionsFault,
    _DBClusterRoleAlreadyExistsFault,
    _DBProxyAlreadyExistsFault,
    _InvalidDBClusterStateFault,
    _InvalidDBInstanceStateFault,
    _DBProxyEndpointAlreadyExistsFault,
    _GlobalClusterNotFoundFault,
    _CustomDBEngineVersionNotFoundFault,
    _BackupPolicyNotFoundFault,
    _AuthorizationNotFoundFault,
    _NetworkTypeNotSupported,
    _DBSubnetGroupQuotaExceededFault,
    _DBInstanceAutomatedBackupNotFoundFault,
    _InsufficientStorageClusterCapacityFault,
    _SubscriptionCategoryNotFoundFault,
    _CustomDBEngineVersionQuotaExceededFault,
    _SNSNoAuthorizationFault,
    _KMSKeyNotAccessibleFault,
    _SNSTopicArnNotFoundFault,
    _OptionGroupAlreadyExistsFault,
    _DBSubnetGroupDoesNotCoverEnoughAZs,
    _GlobalClusterQuotaExceededFault,
    _StorageTypeNotSupportedFault,
    _CertificateNotFoundFault,
    _CustomAvailabilityZoneNotFoundFault,
    _DBProxyEndpointNotFoundFault,
    _InvalidDBInstanceAutomatedBackupStateFault,
    _DBClusterEndpointQuotaExceededFault,
    _AuthorizationQuotaExceededFault,
    _EventSubscriptionQuotaExceededFault,
    _ReservedDBInstancesOfferingNotFoundFault,
    _DBSnapshotAlreadyExistsFault,
    _InvalidRestoreFault,
    _DBClusterQuotaExceededFault,
    _DBClusterParameterGroupNotFoundFault,
    _DBInstanceRoleAlreadyExistsFault,
    _DBSubnetQuotaExceededFault,
    _DBProxyTargetGroupNotFoundFault,
    _OptionGroupQuotaExceededFault,
    _GlobalClusterAlreadyExistsFault,
    _ResourceNotFoundFault,
    _CustomDBEngineVersionAlreadyExistsFault,
    _InsufficientDBClusterCapacityFault,
    _SourceNotFoundFault,
    _DBClusterSnapshotAlreadyExistsFault,
    _InvalidExportOnlyFault,
    _InvalidDBSubnetGroupFault,
    _DBProxyTargetAlreadyRegisteredFault,
    _DBParameterGroupAlreadyExistsFault,
    _InvalidExportSourceStateFault,
    _InvalidVPCNetworkStateFault,
    _InvalidDBProxyStateFault,
    _DBClusterNotFoundFault,
    _InsufficientAvailableIPsInSubnetFault,
    _DBClusterRoleNotFoundFault,
    _InvalidCustomDBEngineVersionStateFault,
    _ProvisionedIopsNotAvailableInAZFault,
    _ReservedDBInstanceAlreadyExistsFault,
    _InvalidGlobalClusterStateFault,
    _DBInstanceRoleQuotaExceededFault,
    _IamRoleNotFoundFault,
    _InvalidDBClusterEndpointStateFault,
    _DBProxyEndpointQuotaExceededFault,
    _InvalidEventSubscriptionStateFault,
    _StorageQuotaExceededFault,
    _ExportTaskAlreadyExistsFault,
    _DBSubnetGroupNotFoundFault,
    _InvalidS3BucketFault,
    _DBClusterRoleQuotaExceededFault,
    _DBSecurityGroupNotFoundFault,
    _SharedSnapshotQuotaExceededFault,
    _DBProxyQuotaExceededFault,
    _DBSecurityGroupQuotaExceededFault,
    _DBClusterSnapshotNotFoundFault,
    _InvalidDBSnapshotStateFault,
    _ReservedDBInstanceQuotaExceededFault,
    _DomainNotFoundFault,

    -- * ActivityStreamMode
    ActivityStreamMode (..),

    -- * ActivityStreamPolicyStatus
    ActivityStreamPolicyStatus (..),

    -- * ActivityStreamStatus
    ActivityStreamStatus (..),

    -- * ApplyMethod
    ApplyMethod (..),

    -- * AuditPolicyState
    AuditPolicyState (..),

    -- * AuthScheme
    AuthScheme (..),

    -- * AutomationMode
    AutomationMode (..),

    -- * CustomEngineVersionStatus
    CustomEngineVersionStatus (..),

    -- * DBProxyEndpointStatus
    DBProxyEndpointStatus (..),

    -- * DBProxyEndpointTargetRole
    DBProxyEndpointTargetRole (..),

    -- * DBProxyStatus
    DBProxyStatus (..),

    -- * EngineFamily
    EngineFamily (..),

    -- * ExportSourceType
    ExportSourceType (..),

    -- * FailoverStatus
    FailoverStatus (..),

    -- * IAMAuthMode
    IAMAuthMode (..),

    -- * ReplicaMode
    ReplicaMode (..),

    -- * SourceType
    SourceType (..),

    -- * TargetHealthReason
    TargetHealthReason (..),

    -- * TargetRole
    TargetRole (..),

    -- * TargetState
    TargetState (..),

    -- * TargetType
    TargetType (..),

    -- * WriteForwardingStatus
    WriteForwardingStatus (..),

    -- * AccountQuota
    AccountQuota (..),
    newAccountQuota,
    accountQuota_max,
    accountQuota_used,
    accountQuota_accountQuotaName,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,

    -- * AvailableProcessorFeature
    AvailableProcessorFeature (..),
    newAvailableProcessorFeature,
    availableProcessorFeature_name,
    availableProcessorFeature_defaultValue,
    availableProcessorFeature_allowedValues,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_thumbprint,
    certificate_validTill,
    certificate_validFrom,
    certificate_customerOverride,
    certificate_certificateIdentifier,
    certificate_certificateArn,
    certificate_certificateType,
    certificate_customerOverrideValidTill,

    -- * CharacterSet
    CharacterSet (..),
    newCharacterSet,
    characterSet_characterSetName,
    characterSet_characterSetDescription,

    -- * CloudwatchLogsExportConfiguration
    CloudwatchLogsExportConfiguration (..),
    newCloudwatchLogsExportConfiguration,
    cloudwatchLogsExportConfiguration_enableLogTypes,
    cloudwatchLogsExportConfiguration_disableLogTypes,

    -- * ClusterPendingModifiedValues
    ClusterPendingModifiedValues (..),
    newClusterPendingModifiedValues,
    clusterPendingModifiedValues_backupRetentionPeriod,
    clusterPendingModifiedValues_dbClusterIdentifier,
    clusterPendingModifiedValues_pendingCloudwatchLogsExports,
    clusterPendingModifiedValues_masterUserPassword,
    clusterPendingModifiedValues_allocatedStorage,
    clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled,
    clusterPendingModifiedValues_iops,
    clusterPendingModifiedValues_engineVersion,

    -- * ConnectionPoolConfiguration
    ConnectionPoolConfiguration (..),
    newConnectionPoolConfiguration,
    connectionPoolConfiguration_maxIdleConnectionsPercent,
    connectionPoolConfiguration_initQuery,
    connectionPoolConfiguration_connectionBorrowTimeout,
    connectionPoolConfiguration_maxConnectionsPercent,
    connectionPoolConfiguration_sessionPinningFilters,

    -- * ConnectionPoolConfigurationInfo
    ConnectionPoolConfigurationInfo (..),
    newConnectionPoolConfigurationInfo,
    connectionPoolConfigurationInfo_maxIdleConnectionsPercent,
    connectionPoolConfigurationInfo_initQuery,
    connectionPoolConfigurationInfo_connectionBorrowTimeout,
    connectionPoolConfigurationInfo_maxConnectionsPercent,
    connectionPoolConfigurationInfo_sessionPinningFilters,

    -- * DBCluster
    DBCluster (..),
    newDBCluster,
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

    -- * DBClusterBacktrack
    DBClusterBacktrack (..),
    newDBClusterBacktrack,
    dbClusterBacktrack_backtrackedFrom,
    dbClusterBacktrack_backtrackRequestCreationTime,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_status,
    dbClusterBacktrack_backtrackTo,

    -- * DBClusterEndpoint
    DBClusterEndpoint (..),
    newDBClusterEndpoint,
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

    -- * DBClusterMember
    DBClusterMember (..),
    newDBClusterMember,
    dbClusterMember_promotionTier,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_dbClusterParameterGroupStatus,
    dbClusterMember_isClusterWriter,

    -- * DBClusterOptionGroupStatus
    DBClusterOptionGroupStatus (..),
    newDBClusterOptionGroupStatus,
    dbClusterOptionGroupStatus_status,
    dbClusterOptionGroupStatus_dbClusterOptionGroupName,

    -- * DBClusterParameterGroup
    DBClusterParameterGroup (..),
    newDBClusterParameterGroup,
    dbClusterParameterGroup_description,
    dbClusterParameterGroup_dbClusterParameterGroupArn,
    dbClusterParameterGroup_dbParameterGroupFamily,
    dbClusterParameterGroup_dbClusterParameterGroupName,

    -- * DBClusterParameterGroupNameMessage
    DBClusterParameterGroupNameMessage (..),
    newDBClusterParameterGroupNameMessage,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- * DBClusterRole
    DBClusterRole (..),
    newDBClusterRole,
    dbClusterRole_roleArn,
    dbClusterRole_featureName,
    dbClusterRole_status,

    -- * DBClusterSnapshot
    DBClusterSnapshot (..),
    newDBClusterSnapshot,
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

    -- * DBClusterSnapshotAttribute
    DBClusterSnapshotAttribute (..),
    newDBClusterSnapshotAttribute,
    dbClusterSnapshotAttribute_attributeValues,
    dbClusterSnapshotAttribute_attributeName,

    -- * DBClusterSnapshotAttributesResult
    DBClusterSnapshotAttributesResult (..),
    newDBClusterSnapshotAttributesResult,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes,

    -- * DBEngineVersion
    DBEngineVersion (..),
    newDBEngineVersion,
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

    -- * DBInstance
    DBInstance (..),
    newDBInstance,
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

    -- * DBInstanceAutomatedBackup
    DBInstanceAutomatedBackup (..),
    newDBInstanceAutomatedBackup,
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

    -- * DBInstanceAutomatedBackupsReplication
    DBInstanceAutomatedBackupsReplication (..),
    newDBInstanceAutomatedBackupsReplication,
    dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn,

    -- * DBInstanceRole
    DBInstanceRole (..),
    newDBInstanceRole,
    dbInstanceRole_roleArn,
    dbInstanceRole_featureName,
    dbInstanceRole_status,

    -- * DBInstanceStatusInfo
    DBInstanceStatusInfo (..),
    newDBInstanceStatusInfo,
    dbInstanceStatusInfo_message,
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_statusType,

    -- * DBParameterGroup
    DBParameterGroup (..),
    newDBParameterGroup,
    dbParameterGroup_dbParameterGroupName,
    dbParameterGroup_description,
    dbParameterGroup_dbParameterGroupFamily,
    dbParameterGroup_dbParameterGroupArn,

    -- * DBParameterGroupNameMessage
    DBParameterGroupNameMessage (..),
    newDBParameterGroupNameMessage,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- * DBParameterGroupStatus
    DBParameterGroupStatus (..),
    newDBParameterGroupStatus,
    dbParameterGroupStatus_dbParameterGroupName,
    dbParameterGroupStatus_parameterApplyStatus,

    -- * DBProxy
    DBProxy (..),
    newDBProxy,
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

    -- * DBProxyEndpoint
    DBProxyEndpoint (..),
    newDBProxyEndpoint,
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

    -- * DBProxyTarget
    DBProxyTarget (..),
    newDBProxyTarget,
    dbProxyTarget_port,
    dbProxyTarget_type,
    dbProxyTarget_targetHealth,
    dbProxyTarget_targetArn,
    dbProxyTarget_rdsResourceId,
    dbProxyTarget_role,
    dbProxyTarget_endpoint,
    dbProxyTarget_trackedClusterId,

    -- * DBProxyTargetGroup
    DBProxyTargetGroup (..),
    newDBProxyTargetGroup,
    dbProxyTargetGroup_targetGroupName,
    dbProxyTargetGroup_status,
    dbProxyTargetGroup_targetGroupArn,
    dbProxyTargetGroup_isDefault,
    dbProxyTargetGroup_updatedDate,
    dbProxyTargetGroup_createdDate,
    dbProxyTargetGroup_connectionPoolConfig,
    dbProxyTargetGroup_dbProxyName,

    -- * DBSecurityGroup
    DBSecurityGroup (..),
    newDBSecurityGroup,
    dbSecurityGroup_ownerId,
    dbSecurityGroup_dbSecurityGroupDescription,
    dbSecurityGroup_dbSecurityGroupName,
    dbSecurityGroup_eC2SecurityGroups,
    dbSecurityGroup_dbSecurityGroupArn,
    dbSecurityGroup_iPRanges,
    dbSecurityGroup_vpcId,

    -- * DBSecurityGroupMembership
    DBSecurityGroupMembership (..),
    newDBSecurityGroupMembership,
    dbSecurityGroupMembership_status,
    dbSecurityGroupMembership_dbSecurityGroupName,

    -- * DBSnapshot
    DBSnapshot (..),
    newDBSnapshot,
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

    -- * DBSnapshotAttribute
    DBSnapshotAttribute (..),
    newDBSnapshotAttribute,
    dbSnapshotAttribute_attributeValues,
    dbSnapshotAttribute_attributeName,

    -- * DBSnapshotAttributesResult
    DBSnapshotAttributesResult (..),
    newDBSnapshotAttributesResult,
    dbSnapshotAttributesResult_dbSnapshotAttributes,
    dbSnapshotAttributesResult_dbSnapshotIdentifier,

    -- * DBSubnetGroup
    DBSubnetGroup (..),
    newDBSubnetGroup,
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_subnetGroupStatus,
    dbSubnetGroup_subnets,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_vpcId,
    dbSubnetGroup_supportedNetworkTypes,

    -- * DescribeDBLogFilesDetails
    DescribeDBLogFilesDetails (..),
    newDescribeDBLogFilesDetails,
    describeDBLogFilesDetails_lastWritten,
    describeDBLogFilesDetails_logFileName,
    describeDBLogFilesDetails_size,

    -- * DomainMembership
    DomainMembership (..),
    newDomainMembership,
    domainMembership_domain,
    domainMembership_fqdn,
    domainMembership_status,
    domainMembership_iAMRoleName,

    -- * DoubleRange
    DoubleRange (..),
    newDoubleRange,
    doubleRange_from,
    doubleRange_to,

    -- * EC2SecurityGroup
    EC2SecurityGroup (..),
    newEC2SecurityGroup,
    eC2SecurityGroup_eC2SecurityGroupId,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_status,
    eC2SecurityGroup_eC2SecurityGroupName,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_port,
    endpoint_hostedZoneId,
    endpoint_address,

    -- * EngineDefaults
    EngineDefaults (..),
    newEngineDefaults,
    engineDefaults_marker,
    engineDefaults_dbParameterGroupFamily,
    engineDefaults_parameters,

    -- * Event
    Event (..),
    newEvent,
    event_message,
    event_sourceArn,
    event_date,
    event_sourceType,
    event_sourceIdentifier,
    event_eventCategories,

    -- * EventCategoriesMap
    EventCategoriesMap (..),
    newEventCategoriesMap,
    eventCategoriesMap_sourceType,
    eventCategoriesMap_eventCategories,

    -- * EventSubscription
    EventSubscription (..),
    newEventSubscription,
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

    -- * ExportTask
    ExportTask (..),
    newExportTask,
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

    -- * FailoverState
    FailoverState (..),
    newFailoverState,
    failoverState_status,
    failoverState_fromDbClusterArn,
    failoverState_toDbClusterArn,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * GlobalCluster
    GlobalCluster (..),
    newGlobalCluster,
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

    -- * GlobalClusterMember
    GlobalClusterMember (..),
    newGlobalClusterMember,
    globalClusterMember_dbClusterArn,
    globalClusterMember_isWriter,
    globalClusterMember_readers,
    globalClusterMember_globalWriteForwardingStatus,

    -- * IPRange
    IPRange (..),
    newIPRange,
    iPRange_status,
    iPRange_cidrip,

    -- * MinimumEngineVersionPerAllowedValue
    MinimumEngineVersionPerAllowedValue (..),
    newMinimumEngineVersionPerAllowedValue,
    minimumEngineVersionPerAllowedValue_minimumEngineVersion,
    minimumEngineVersionPerAllowedValue_allowedValue,

    -- * Option
    Option (..),
    newOption,
    option_port,
    option_dbSecurityGroupMemberships,
    option_optionVersion,
    option_persistent,
    option_optionName,
    option_optionDescription,
    option_permanent,
    option_vpcSecurityGroupMemberships,
    option_optionSettings,

    -- * OptionConfiguration
    OptionConfiguration (..),
    newOptionConfiguration,
    optionConfiguration_port,
    optionConfiguration_dbSecurityGroupMemberships,
    optionConfiguration_optionVersion,
    optionConfiguration_vpcSecurityGroupMemberships,
    optionConfiguration_optionSettings,
    optionConfiguration_optionName,

    -- * OptionGroup
    OptionGroup (..),
    newOptionGroup,
    optionGroup_allowsVpcAndNonVpcInstanceMemberships,
    optionGroup_engineName,
    optionGroup_optionGroupName,
    optionGroup_optionGroupDescription,
    optionGroup_majorEngineVersion,
    optionGroup_options,
    optionGroup_vpcId,
    optionGroup_optionGroupArn,

    -- * OptionGroupMembership
    OptionGroupMembership (..),
    newOptionGroupMembership,
    optionGroupMembership_optionGroupName,
    optionGroupMembership_status,

    -- * OptionGroupOption
    OptionGroupOption (..),
    newOptionGroupOption,
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

    -- * OptionGroupOptionSetting
    OptionGroupOptionSetting (..),
    newOptionGroupOptionSetting,
    optionGroupOptionSetting_settingDescription,
    optionGroupOptionSetting_applyType,
    optionGroupOptionSetting_defaultValue,
    optionGroupOptionSetting_isModifiable,
    optionGroupOptionSetting_settingName,
    optionGroupOptionSetting_allowedValues,
    optionGroupOptionSetting_isRequired,
    optionGroupOptionSetting_minimumEngineVersionPerAllowedValue,

    -- * OptionSetting
    OptionSetting (..),
    newOptionSetting,
    optionSetting_name,
    optionSetting_applyType,
    optionSetting_defaultValue,
    optionSetting_isModifiable,
    optionSetting_description,
    optionSetting_isCollection,
    optionSetting_allowedValues,
    optionSetting_dataType,
    optionSetting_value,

    -- * OptionVersion
    OptionVersion (..),
    newOptionVersion,
    optionVersion_isDefault,
    optionVersion_version,

    -- * OrderableDBInstanceOption
    OrderableDBInstanceOption (..),
    newOrderableDBInstanceOption,
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

    -- * Outpost
    Outpost (..),
    newOutpost,
    outpost_arn,

    -- * Parameter
    Parameter (..),
    newParameter,
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

    -- * PendingCloudwatchLogsExports
    PendingCloudwatchLogsExports (..),
    newPendingCloudwatchLogsExports,
    pendingCloudwatchLogsExports_logTypesToEnable,
    pendingCloudwatchLogsExports_logTypesToDisable,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_forcedApplyDate,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    newPendingModifiedValues,
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

    -- * ProcessorFeature
    ProcessorFeature (..),
    newProcessorFeature,
    processorFeature_name,
    processorFeature_value,

    -- * Range
    Range (..),
    newRange,
    range_from,
    range_to,
    range_step,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- * ReservedDBInstance
    ReservedDBInstance (..),
    newReservedDBInstance,
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

    -- * ReservedDBInstancesOffering
    ReservedDBInstancesOffering (..),
    newReservedDBInstancesOffering,
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

    -- * ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    newResourcePendingMaintenanceActions,
    resourcePendingMaintenanceActions_resourceIdentifier,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,

    -- * RestoreWindow
    RestoreWindow (..),
    newRestoreWindow,
    restoreWindow_latestTime,
    restoreWindow_earliestTime,

    -- * ScalingConfiguration
    ScalingConfiguration (..),
    newScalingConfiguration,
    scalingConfiguration_timeoutAction,
    scalingConfiguration_secondsBeforeTimeout,
    scalingConfiguration_secondsUntilAutoPause,
    scalingConfiguration_maxCapacity,
    scalingConfiguration_autoPause,
    scalingConfiguration_minCapacity,

    -- * ScalingConfigurationInfo
    ScalingConfigurationInfo (..),
    newScalingConfigurationInfo,
    scalingConfigurationInfo_timeoutAction,
    scalingConfigurationInfo_secondsBeforeTimeout,
    scalingConfigurationInfo_secondsUntilAutoPause,
    scalingConfigurationInfo_maxCapacity,
    scalingConfigurationInfo_autoPause,
    scalingConfigurationInfo_minCapacity,

    -- * ServerlessV2ScalingConfiguration
    ServerlessV2ScalingConfiguration (..),
    newServerlessV2ScalingConfiguration,
    serverlessV2ScalingConfiguration_maxCapacity,
    serverlessV2ScalingConfiguration_minCapacity,

    -- * ServerlessV2ScalingConfigurationInfo
    ServerlessV2ScalingConfigurationInfo (..),
    newServerlessV2ScalingConfigurationInfo,
    serverlessV2ScalingConfigurationInfo_maxCapacity,
    serverlessV2ScalingConfigurationInfo_minCapacity,

    -- * SourceRegion
    SourceRegion (..),
    newSourceRegion,
    sourceRegion_status,
    sourceRegion_supportsDBInstanceAutomatedBackupsReplication,
    sourceRegion_regionName,
    sourceRegion_endpoint,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetOutpost,
    subnet_subnetIdentifier,
    subnet_subnetStatus,
    subnet_subnetAvailabilityZone,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TargetHealth
    TargetHealth (..),
    newTargetHealth,
    targetHealth_state,
    targetHealth_description,
    targetHealth_reason,

    -- * Timezone
    Timezone (..),
    newTimezone,
    timezone_timezoneName,

    -- * UpgradeTarget
    UpgradeTarget (..),
    newUpgradeTarget,
    upgradeTarget_supportsBabelfish,
    upgradeTarget_autoUpgrade,
    upgradeTarget_supportedEngineModes,
    upgradeTarget_description,
    upgradeTarget_supportsParallelQuery,
    upgradeTarget_engine,
    upgradeTarget_supportsGlobalDatabases,
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,

    -- * UserAuthConfig
    UserAuthConfig (..),
    newUserAuthConfig,
    userAuthConfig_userName,
    userAuthConfig_description,
    userAuthConfig_iAMAuth,
    userAuthConfig_secretArn,
    userAuthConfig_authScheme,

    -- * UserAuthConfigInfo
    UserAuthConfigInfo (..),
    newUserAuthConfigInfo,
    userAuthConfigInfo_userName,
    userAuthConfigInfo_description,
    userAuthConfigInfo_iAMAuth,
    userAuthConfigInfo_secretArn,
    userAuthConfigInfo_authScheme,

    -- * ValidDBInstanceModificationsMessage
    ValidDBInstanceModificationsMessage (..),
    newValidDBInstanceModificationsMessage,
    validDBInstanceModificationsMessage_storage,
    validDBInstanceModificationsMessage_validProcessorFeatures,

    -- * ValidStorageOptions
    ValidStorageOptions (..),
    newValidStorageOptions,
    validStorageOptions_storageSize,
    validStorageOptions_iopsToStorageRatio,
    validStorageOptions_provisionedIops,
    validStorageOptions_provisionedStorageThroughput,
    validStorageOptions_storageType,
    validStorageOptions_supportsStorageAutoscaling,
    validStorageOptions_storageThroughputToIopsRatio,

    -- * VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    newVpcSecurityGroupMembership,
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.AccountQuota
import Amazonka.RDS.Types.ActivityStreamMode
import Amazonka.RDS.Types.ActivityStreamPolicyStatus
import Amazonka.RDS.Types.ActivityStreamStatus
import Amazonka.RDS.Types.ApplyMethod
import Amazonka.RDS.Types.AuditPolicyState
import Amazonka.RDS.Types.AuthScheme
import Amazonka.RDS.Types.AutomationMode
import Amazonka.RDS.Types.AvailabilityZone
import Amazonka.RDS.Types.AvailableProcessorFeature
import Amazonka.RDS.Types.Certificate
import Amazonka.RDS.Types.CharacterSet
import Amazonka.RDS.Types.CloudwatchLogsExportConfiguration
import Amazonka.RDS.Types.ClusterPendingModifiedValues
import Amazonka.RDS.Types.ConnectionPoolConfiguration
import Amazonka.RDS.Types.ConnectionPoolConfigurationInfo
import Amazonka.RDS.Types.CustomEngineVersionStatus
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
import Amazonka.RDS.Types.DBProxyEndpointStatus
import Amazonka.RDS.Types.DBProxyEndpointTargetRole
import Amazonka.RDS.Types.DBProxyStatus
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
import Amazonka.RDS.Types.EngineFamily
import Amazonka.RDS.Types.Event
import Amazonka.RDS.Types.EventCategoriesMap
import Amazonka.RDS.Types.EventSubscription
import Amazonka.RDS.Types.ExportSourceType
import Amazonka.RDS.Types.ExportTask
import Amazonka.RDS.Types.FailoverState
import Amazonka.RDS.Types.FailoverStatus
import Amazonka.RDS.Types.Filter
import Amazonka.RDS.Types.GlobalCluster
import Amazonka.RDS.Types.GlobalClusterMember
import Amazonka.RDS.Types.IAMAuthMode
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
import Amazonka.RDS.Types.ReplicaMode
import Amazonka.RDS.Types.ReservedDBInstance
import Amazonka.RDS.Types.ReservedDBInstancesOffering
import Amazonka.RDS.Types.ResourcePendingMaintenanceActions
import Amazonka.RDS.Types.RestoreWindow
import Amazonka.RDS.Types.ScalingConfiguration
import Amazonka.RDS.Types.ScalingConfigurationInfo
import Amazonka.RDS.Types.ServerlessV2ScalingConfiguration
import Amazonka.RDS.Types.ServerlessV2ScalingConfigurationInfo
import Amazonka.RDS.Types.SourceRegion
import Amazonka.RDS.Types.SourceType
import Amazonka.RDS.Types.Subnet
import Amazonka.RDS.Types.Tag
import Amazonka.RDS.Types.TargetHealth
import Amazonka.RDS.Types.TargetHealthReason
import Amazonka.RDS.Types.TargetRole
import Amazonka.RDS.Types.TargetState
import Amazonka.RDS.Types.TargetType
import Amazonka.RDS.Types.Timezone
import Amazonka.RDS.Types.UpgradeTarget
import Amazonka.RDS.Types.UserAuthConfig
import Amazonka.RDS.Types.UserAuthConfigInfo
import Amazonka.RDS.Types.ValidDBInstanceModificationsMessage
import Amazonka.RDS.Types.ValidStorageOptions
import Amazonka.RDS.Types.VpcSecurityGroupMembership
import Amazonka.RDS.Types.WriteForwardingStatus
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2014-10-31@ of the Amazon Relational Database Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "RDS",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "rds",
      Core.signingName = "rds",
      Core.version = "2014-10-31",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "RDS",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified option group could not be found.
_OptionGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OptionGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The specified DB instance class isn\'t available in the specified
-- Availability Zone.
_InsufficientDBInstanceCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientDBInstanceCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBInstanceCapacity"
    Prelude.. Core.hasStatus 400

-- | The supplied value isn\'t a valid DB cluster snapshot state.
_InvalidDBClusterSnapshotStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterSnapshotStateFault"
    Prelude.. Core.hasStatus 400

-- | A DB security group isn\'t allowed for this action.
_DBSecurityGroupNotSupportedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupNotSupported"
    Prelude.. Core.hasStatus 400

-- | The DB subnet is already in use in the Availability Zone.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"
    Prelude.. Core.hasStatus 400

-- | @SourceDBInstanceIdentifier@ refers to a DB instance with
-- @BackupRetentionPeriod@ equal to 0.
_PointInTimeRestoreNotEnabledFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PointInTimeRestoreNotEnabledFault =
  Core._MatchServiceError
    defaultService
    "PointInTimeRestoreNotEnabled"
    Prelude.. Core.hasStatus 400

-- | The state of the DB security group doesn\'t allow deletion.
_InvalidDBSecurityGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSecurityGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSecurityGroupState"
    Prelude.. Core.hasStatus 400

-- | The DB parameter group is in use or is in an invalid state. If you are
-- attempting to delete the parameter group, you can\'t delete it when the
-- parameter group is in this state.
_InvalidDBParameterGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBParameterGroupState"
    Prelude.. Core.hasStatus 400

-- | @BacktrackIdentifier@ doesn\'t refer to an existing backtrack.
_DBClusterBacktrackNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterBacktrackNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterBacktrackNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | A DB security group with the name specified in @DBSecurityGroupName@
-- already exists.
_DBSecurityGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- snapshots.
_SnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The requested subnet is invalid, or multiple subnets were requested that
-- are not all in a common VPC.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | The user already has a DB cluster with the given identifier.
_DBClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | @DBParameterGroupName@ doesn\'t refer to an existing DB parameter group.
_DBParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified CIDR IP range or Amazon EC2 security group is already
-- authorized for the specified DB security group.
_AuthorizationAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The specified custom endpoint can\'t be created because it already
-- exists.
_DBClusterEndpointAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The specified reserved DB Instance not found.
_ReservedDBInstanceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstanceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstanceNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified proxy name doesn\'t correspond to a proxy owned by your
-- Amazon Web Services account in the specified Amazon Web Services Region.
_DBProxyNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The supplied subscription name already exists.
_SubscriptionAlreadyExistFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionAlreadyExistFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionAlreadyExist"
    Prelude.. Core.hasStatus 400

-- | The user already has a DB instance with the given identifier.
_DBInstanceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The DB subnet group cannot be deleted because it\'s in use.
_InvalidDBSubnetGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetGroupStateFault"
    Prelude.. Core.hasStatus 400

-- | SNS has responded that there is a problem with the SNS topic specified.
_SNSInvalidTopicFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError
    defaultService
    "SNSInvalidTopic"
    Prelude.. Core.hasStatus 400

-- | The specified custom endpoint doesn\'t exist.
_DBClusterEndpointNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The specified RDS DB instance or Aurora DB cluster isn\'t available for
-- a proxy owned by your Amazon Web Services account in the specified
-- Amazon Web Services Region.
_DBProxyTargetNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyTargetNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | @Capacity@ isn\'t a valid Aurora Serverless DB cluster capacity. Valid
-- capacity values are @2@, @4@, @8@, @16@, @32@, @64@, @128@, and @256@.
_InvalidDBClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterCapacityFault"
    Prelude.. Core.hasStatus 400

-- | You can\'t perform this operation while the DB proxy endpoint is in a
-- particular state.
_InvalidDBProxyEndpointStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBProxyEndpointStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBProxyEndpointStateFault"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- parameter groups.
_DBParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The subscription name does not exist.
_SubscriptionNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionNotFound"
    Prelude.. Core.hasStatus 404

-- | The export task doesn\'t exist.
_ExportTaskNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExportTaskNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ExportTaskNotFound"
    Prelude.. Core.hasStatus 404

-- | @DBSnapshotIdentifier@ doesn\'t refer to an existing DB snapshot.
_DBSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotNotFound"
    Prelude.. Core.hasStatus 404

-- | The DB subnet isn\'t in the /available/ state.
_InvalidDBSubnetStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetStateFault"
    Prelude.. Core.hasStatus 400

-- | The DB upgrade failed because a resource the DB depends on can\'t be
-- modified.
_DBUpgradeDependencyFailureFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBUpgradeDependencyFailureFault =
  Core._MatchServiceError
    defaultService
    "DBUpgradeDependencyFailure"
    Prelude.. Core.hasStatus 400

-- | The specified @RoleArn@ value doesn\'t match the specified feature for
-- the DB instance.
_DBInstanceRoleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceRoleNotFound"
    Prelude.. Core.hasStatus 404

-- | @LogFileName@ doesn\'t refer to an existing DB log file.
_DBLogFileNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBLogFileNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBLogFileNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | @DBSubnetGroupName@ is already used by an existing DB subnet group.
_DBSubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | @DBInstanceIdentifier@ doesn\'t refer to an existing DB instance.
_DBInstanceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed number of DB
-- instances.
_InstanceQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "InstanceQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The option group isn\'t in the /available/ state.
_InvalidOptionGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOptionGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidOptionGroupStateFault"
    Prelude.. Core.hasStatus 400

-- | You can\'t cancel an export task that has completed.
_InvalidExportTaskStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidExportTaskStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidExportTaskStateFault"
    Prelude.. Core.hasStatus 400

-- | The DBSubnetGroup shouldn\'t be specified while creating read replicas
-- that lie in the same region as the source instance.
_DBSubnetGroupNotAllowedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupNotAllowedFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupNotAllowedFault"
    Prelude.. Core.hasStatus 400

-- | The quota for retained automated backups was exceeded. This prevents you
-- from retaining any additional automated backups. The retained automated
-- backups quota is the same as your DB Instance quota.
_DBInstanceAutomatedBackupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceAutomatedBackupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAutomatedBackupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The IAM role requires additional permissions to export to an Amazon S3
-- bucket.
_IamRoleMissingPermissionsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IamRoleMissingPermissionsFault =
  Core._MatchServiceError
    defaultService
    "IamRoleMissingPermissions"
    Prelude.. Core.hasStatus 400

-- | The specified IAM role Amazon Resource Name (ARN) is already associated
-- with the specified DB cluster.
_DBClusterRoleAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The specified proxy name must be unique for all proxies owned by your
-- Amazon Web Services account in the specified Amazon Web Services Region.
_DBProxyAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBProxyAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The requested operation can\'t be performed while the cluster is in this
-- state.
_InvalidDBClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | The DB instance isn\'t in a valid state.
_InvalidDBInstanceStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBInstanceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBInstanceState"
    Prelude.. Core.hasStatus 400

-- | The specified DB proxy endpoint name must be unique for all DB proxy
-- endpoints owned by your Amazon Web Services account in the specified
-- Amazon Web Services Region.
_DBProxyEndpointAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyEndpointAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBProxyEndpointAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The @GlobalClusterIdentifier@ doesn\'t refer to an existing global
-- database cluster.
_GlobalClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The specified CEV was not found.
_CustomDBEngineVersionNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomDBEngineVersionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CustomDBEngineVersionNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | Prism for BackupPolicyNotFoundFault' errors.
_BackupPolicyNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BackupPolicyNotFoundFault =
  Core._MatchServiceError
    defaultService
    "BackupPolicyNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The specified CIDR IP range or Amazon EC2 security group might not be
-- authorized for the specified DB security group.
--
-- Or, RDS might not be authorized to perform necessary actions using IAM
-- on your behalf.
_AuthorizationNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | The network type is invalid for the DB instance. Valid nework type
-- values are @IPV4@ and @DUAL@.
_NetworkTypeNotSupported :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NetworkTypeNotSupported =
  Core._MatchServiceError
    defaultService
    "NetworkTypeNotSupported"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- subnet groups.
_DBSubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | No automated backup for this DB instance was found.
_DBInstanceAutomatedBackupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceAutomatedBackupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAutomatedBackupNotFound"
    Prelude.. Core.hasStatus 404

-- | There is insufficient storage available for the current action. You
-- might be able to resolve this error by updating your subnet group to use
-- different Availability Zones that have more storage available.
_InsufficientStorageClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientStorageClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientStorageClusterCapacity"
    Prelude.. Core.hasStatus 400

-- | The supplied category does not exist.
_SubscriptionCategoryNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionCategoryNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionCategoryNotFound"
    Prelude.. Core.hasStatus 404

-- | You have exceeded your CEV quota.
_CustomDBEngineVersionQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomDBEngineVersionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CustomDBEngineVersionQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | You do not have permission to publish to the SNS topic ARN.
_SNSNoAuthorizationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorization"
    Prelude.. Core.hasStatus 400

-- | An error occurred accessing an Amazon Web Services KMS key.
_KMSKeyNotAccessibleFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSKeyNotAccessibleFault =
  Core._MatchServiceError
    defaultService
    "KMSKeyNotAccessibleFault"
    Prelude.. Core.hasStatus 400

-- | The SNS topic ARN does not exist.
_SNSTopicArnNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSTopicArnNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SNSTopicArnNotFound"
    Prelude.. Core.hasStatus 404

-- | The option group you are trying to create already exists.
_OptionGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OptionGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | Subnets in the DB subnet group should cover at least two Availability
-- Zones unless there is only one Availability Zone.
_DBSubnetGroupDoesNotCoverEnoughAZs :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupDoesNotCoverEnoughAZs"
    Prelude.. Core.hasStatus 400

-- | The number of global database clusters for this account is already at
-- the maximum allowed.
_GlobalClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | Storage of the @StorageType@ specified can\'t be associated with the DB
-- instance.
_StorageTypeNotSupportedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageTypeNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "StorageTypeNotSupported"
    Prelude.. Core.hasStatus 400

-- | @CertificateIdentifier@ doesn\'t refer to an existing certificate.
_CertificateNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Prelude.. Core.hasStatus 404

-- | @CustomAvailabilityZoneId@ doesn\'t refer to an existing custom
-- Availability Zone identifier.
_CustomAvailabilityZoneNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomAvailabilityZoneNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CustomAvailabilityZoneNotFound"
    Prelude.. Core.hasStatus 404

-- | The DB proxy endpoint doesn\'t exist.
_DBProxyEndpointNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyEndpointNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyEndpointNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The automated backup is in an invalid state. For example, this automated
-- backup is associated with an active instance.
_InvalidDBInstanceAutomatedBackupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBInstanceAutomatedBackupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBInstanceAutomatedBackupState"
    Prelude.. Core.hasStatus 400

-- | The cluster already has the maximum number of custom endpoints.
_DBClusterEndpointQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointQuotaExceededFault"
    Prelude.. Core.hasStatus 403

-- | The DB security group authorization quota has been reached.
_AuthorizationQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of event subscriptions.
_EventSubscriptionQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EventSubscriptionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "EventSubscriptionQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | Specified offering does not exist.
_ReservedDBInstancesOfferingNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstancesOfferingNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstancesOfferingNotFound"
    Prelude.. Core.hasStatus 404

-- | @DBSnapshotIdentifier@ is already used by an existing snapshot.
_DBSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Cannot restore from VPC backup to non-VPC DB instance.
_InvalidRestoreFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRestoreFault =
  Core._MatchServiceError
    defaultService
    "InvalidRestoreFault"
    Prelude.. Core.hasStatus 400

-- | The user attempted to create a new DB cluster and the user has already
-- reached the maximum allowed DB cluster quota.
_DBClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 403

-- | @DBClusterParameterGroupName@ doesn\'t refer to an existing DB cluster
-- parameter group.
_DBClusterParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified @RoleArn@ or @FeatureName@ value is already associated
-- with the DB instance.
_DBInstanceRoleAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceRoleAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceRoleAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of
-- subnets in a DB subnet groups.
_DBSubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The specified target group isn\'t available for a proxy owned by your
-- Amazon Web Services account in the specified Amazon Web Services Region.
_DBProxyTargetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyTargetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The quota of 20 option groups was exceeded for this Amazon Web Services
-- account.
_OptionGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OptionGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The @GlobalClusterIdentifier@ already exists. Choose a new global
-- database identifier (unique name) to create a new global database
-- cluster.
_GlobalClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The specified resource ID was not found.
_ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | A CEV with the specified name already exists.
_CustomDBEngineVersionAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomDBEngineVersionAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CustomDBEngineVersionAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The DB cluster doesn\'t have enough capacity for the current operation.
_InsufficientDBClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientDBClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBClusterCapacityFault"
    Prelude.. Core.hasStatus 403

-- | The requested source could not be found.
_SourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SourceNotFound"
    Prelude.. Core.hasStatus 404

-- | The user already has a DB cluster snapshot with the given identifier.
_DBClusterSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The export is invalid for exporting to an Amazon S3 bucket.
_InvalidExportOnlyFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidExportOnlyFault =
  Core._MatchServiceError
    defaultService
    "InvalidExportOnly"
    Prelude.. Core.hasStatus 400

-- | The DBSubnetGroup doesn\'t belong to the same VPC as that of an existing
-- cross-region read replica of the same source instance.
_InvalidDBSubnetGroupFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetGroupFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetGroupFault"
    Prelude.. Core.hasStatus 400

-- | The proxy is already associated with the specified RDS DB instance or
-- Aurora DB cluster.
_DBProxyTargetAlreadyRegisteredFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyTargetAlreadyRegisteredFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetAlreadyRegisteredFault"
    Prelude.. Core.hasStatus 400

-- | A DB parameter group with the same name exists.
_DBParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The state of the export snapshot is invalid for exporting to an Amazon
-- S3 bucket.
_InvalidExportSourceStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidExportSourceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidExportSourceState"
    Prelude.. Core.hasStatus 400

-- | The DB subnet group doesn\'t cover all Availability Zones after it\'s
-- created because of users\' change.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Core.hasStatus 400

-- | The requested operation can\'t be performed while the proxy is in this
-- state.
_InvalidDBProxyStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBProxyStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBProxyStateFault"
    Prelude.. Core.hasStatus 400

-- | @DBClusterIdentifier@ doesn\'t refer to an existing DB cluster.
_DBClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The requested operation can\'t be performed because there aren\'t enough
-- available IP addresses in the proxy\'s subnets. Add more CIDR blocks to
-- the VPC or remove IP address that aren\'t required from the subnets.
_InsufficientAvailableIPsInSubnetFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientAvailableIPsInSubnetFault =
  Core._MatchServiceError
    defaultService
    "InsufficientAvailableIPsInSubnetFault"
    Prelude.. Core.hasStatus 400

-- | The specified IAM role Amazon Resource Name (ARN) isn\'t associated with
-- the specified DB cluster.
_DBClusterRoleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleNotFound"
    Prelude.. Core.hasStatus 404

-- | You can\'t delete the CEV.
_InvalidCustomDBEngineVersionStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCustomDBEngineVersionStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCustomDBEngineVersionStateFault"
    Prelude.. Core.hasStatus 400

-- | Provisioned IOPS not available in the specified Availability Zone.
_ProvisionedIopsNotAvailableInAZFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ProvisionedIopsNotAvailableInAZFault =
  Core._MatchServiceError
    defaultService
    "ProvisionedIopsNotAvailableInAZFault"
    Prelude.. Core.hasStatus 400

-- | User already has a reservation with the given identifier.
_ReservedDBInstanceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstanceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstanceAlreadyExists"
    Prelude.. Core.hasStatus 404

-- | The global cluster is in an invalid state and can\'t perform the
-- requested operation.
_InvalidGlobalClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGlobalClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidGlobalClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | You can\'t associate any more Amazon Web Services Identity and Access
-- Management (IAM) roles with the DB instance because the quota has been
-- reached.
_DBInstanceRoleQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceRoleQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceRoleQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The IAM role is missing for exporting to an Amazon S3 bucket.
_IamRoleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IamRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "IamRoleNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested operation can\'t be performed on the endpoint while the
-- endpoint is in this state.
_InvalidDBClusterEndpointStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterEndpointStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterEndpointStateFault"
    Prelude.. Core.hasStatus 400

-- | The DB proxy already has the maximum number of endpoints.
_DBProxyEndpointQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyEndpointQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBProxyEndpointQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | This error can occur if someone else is modifying a subscription. You
-- should retry the action.
_InvalidEventSubscriptionStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEventSubscriptionStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidEventSubscriptionState"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed amount of
-- storage available across all DB instances.
_StorageQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "StorageQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | You can\'t start an export task that\'s already running.
_ExportTaskAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExportTaskAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ExportTaskAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | @DBSubnetGroupName@ doesn\'t refer to an existing DB subnet group.
_DBSubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The specified Amazon S3 bucket name can\'t be found or Amazon RDS isn\'t
-- authorized to access the specified Amazon S3 bucket. Verify the
-- __SourceS3BucketName__ and __S3IngestionRoleArn__ values and try again.
_InvalidS3BucketFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidS3BucketFault =
  Core._MatchServiceError
    defaultService
    "InvalidS3BucketFault"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the maximum number of IAM roles that can be associated
-- with the specified DB cluster.
_DBClusterRoleQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | @DBSecurityGroupName@ doesn\'t refer to an existing DB security group.
_DBSecurityGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | You have exceeded the maximum number of accounts that you can share a
-- manual DB snapshot with.
_SharedSnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SharedSnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SharedSnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | Your Amazon Web Services account already has the maximum number of
-- proxies in the specified Amazon Web Services Region.
_DBProxyQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBProxyQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- security groups.
_DBSecurityGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "QuotaExceeded.DBSecurityGroup"
    Prelude.. Core.hasStatus 400

-- | @DBClusterSnapshotIdentifier@ doesn\'t refer to an existing DB cluster
-- snapshot.
_DBClusterSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The state of the DB snapshot doesn\'t allow deletion.
_InvalidDBSnapshotStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSnapshotState"
    Prelude.. Core.hasStatus 400

-- | Request would exceed the user\'s DB Instance quota.
_ReservedDBInstanceQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstanceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstanceQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | @Domain@ doesn\'t refer to an existing Active Directory domain.
_DomainNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DomainNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DomainNotFoundFault"
    Prelude.. Core.hasStatus 404
