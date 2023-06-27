{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDS.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AuthorizationAlreadyExistsFault,
    _AuthorizationNotFoundFault,
    _AuthorizationQuotaExceededFault,
    _BackupPolicyNotFoundFault,
    _BlueGreenDeploymentAlreadyExistsFault,
    _BlueGreenDeploymentNotFoundFault,
    _CertificateNotFoundFault,
    _CreateCustomDBEngineVersionFault,
    _CustomAvailabilityZoneNotFoundFault,
    _CustomDBEngineVersionAlreadyExistsFault,
    _CustomDBEngineVersionNotFoundFault,
    _CustomDBEngineVersionQuotaExceededFault,
    _DBClusterAlreadyExistsFault,
    _DBClusterBacktrackNotFoundFault,
    _DBClusterEndpointAlreadyExistsFault,
    _DBClusterEndpointNotFoundFault,
    _DBClusterEndpointQuotaExceededFault,
    _DBClusterNotFoundFault,
    _DBClusterParameterGroupNotFoundFault,
    _DBClusterQuotaExceededFault,
    _DBClusterRoleAlreadyExistsFault,
    _DBClusterRoleNotFoundFault,
    _DBClusterRoleQuotaExceededFault,
    _DBClusterSnapshotAlreadyExistsFault,
    _DBClusterSnapshotNotFoundFault,
    _DBInstanceAlreadyExistsFault,
    _DBInstanceAutomatedBackupNotFoundFault,
    _DBInstanceAutomatedBackupQuotaExceededFault,
    _DBInstanceNotFoundFault,
    _DBInstanceRoleAlreadyExistsFault,
    _DBInstanceRoleNotFoundFault,
    _DBInstanceRoleQuotaExceededFault,
    _DBLogFileNotFoundFault,
    _DBParameterGroupAlreadyExistsFault,
    _DBParameterGroupNotFoundFault,
    _DBParameterGroupQuotaExceededFault,
    _DBProxyAlreadyExistsFault,
    _DBProxyEndpointAlreadyExistsFault,
    _DBProxyEndpointNotFoundFault,
    _DBProxyEndpointQuotaExceededFault,
    _DBProxyNotFoundFault,
    _DBProxyQuotaExceededFault,
    _DBProxyTargetAlreadyRegisteredFault,
    _DBProxyTargetGroupNotFoundFault,
    _DBProxyTargetNotFoundFault,
    _DBSecurityGroupAlreadyExistsFault,
    _DBSecurityGroupNotFoundFault,
    _DBSecurityGroupNotSupportedFault,
    _DBSecurityGroupQuotaExceededFault,
    _DBSnapshotAlreadyExistsFault,
    _DBSnapshotNotFoundFault,
    _DBSubnetGroupAlreadyExistsFault,
    _DBSubnetGroupDoesNotCoverEnoughAZs,
    _DBSubnetGroupNotAllowedFault,
    _DBSubnetGroupNotFoundFault,
    _DBSubnetGroupQuotaExceededFault,
    _DBSubnetQuotaExceededFault,
    _DBUpgradeDependencyFailureFault,
    _DomainNotFoundFault,
    _Ec2ImagePropertiesNotSupportedFault,
    _EventSubscriptionQuotaExceededFault,
    _ExportTaskAlreadyExistsFault,
    _ExportTaskNotFoundFault,
    _GlobalClusterAlreadyExistsFault,
    _GlobalClusterNotFoundFault,
    _GlobalClusterQuotaExceededFault,
    _IamRoleMissingPermissionsFault,
    _IamRoleNotFoundFault,
    _InstanceQuotaExceededFault,
    _InsufficientAvailableIPsInSubnetFault,
    _InsufficientDBClusterCapacityFault,
    _InsufficientDBInstanceCapacityFault,
    _InsufficientStorageClusterCapacityFault,
    _InvalidBlueGreenDeploymentStateFault,
    _InvalidCustomDBEngineVersionStateFault,
    _InvalidDBClusterCapacityFault,
    _InvalidDBClusterEndpointStateFault,
    _InvalidDBClusterSnapshotStateFault,
    _InvalidDBClusterStateFault,
    _InvalidDBInstanceAutomatedBackupStateFault,
    _InvalidDBInstanceStateFault,
    _InvalidDBParameterGroupStateFault,
    _InvalidDBProxyEndpointStateFault,
    _InvalidDBProxyStateFault,
    _InvalidDBSecurityGroupStateFault,
    _InvalidDBSnapshotStateFault,
    _InvalidDBSubnetGroupFault,
    _InvalidDBSubnetGroupStateFault,
    _InvalidDBSubnetStateFault,
    _InvalidEventSubscriptionStateFault,
    _InvalidExportOnlyFault,
    _InvalidExportSourceStateFault,
    _InvalidExportTaskStateFault,
    _InvalidGlobalClusterStateFault,
    _InvalidOptionGroupStateFault,
    _InvalidRestoreFault,
    _InvalidS3BucketFault,
    _InvalidSubnet,
    _InvalidVPCNetworkStateFault,
    _KMSKeyNotAccessibleFault,
    _NetworkTypeNotSupported,
    _OptionGroupAlreadyExistsFault,
    _OptionGroupNotFoundFault,
    _OptionGroupQuotaExceededFault,
    _PointInTimeRestoreNotEnabledFault,
    _ProvisionedIopsNotAvailableInAZFault,
    _ReservedDBInstanceAlreadyExistsFault,
    _ReservedDBInstanceNotFoundFault,
    _ReservedDBInstanceQuotaExceededFault,
    _ReservedDBInstancesOfferingNotFoundFault,
    _ResourceNotFoundFault,
    _SNSInvalidTopicFault,
    _SNSNoAuthorizationFault,
    _SNSTopicArnNotFoundFault,
    _SharedSnapshotQuotaExceededFault,
    _SnapshotQuotaExceededFault,
    _SourceClusterNotSupportedFault,
    _SourceDatabaseNotSupportedFault,
    _SourceNotFoundFault,
    _StorageQuotaExceededFault,
    _StorageTypeNotAvailableFault,
    _StorageTypeNotSupportedFault,
    _SubnetAlreadyInUse,
    _SubscriptionAlreadyExistFault,
    _SubscriptionCategoryNotFoundFault,
    _SubscriptionNotFoundFault,

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

    -- * ClientPasswordAuthType
    ClientPasswordAuthType (..),

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
    accountQuota_accountQuotaName,
    accountQuota_max,
    accountQuota_used,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,

    -- * AvailableProcessorFeature
    AvailableProcessorFeature (..),
    newAvailableProcessorFeature,
    availableProcessorFeature_allowedValues,
    availableProcessorFeature_defaultValue,
    availableProcessorFeature_name,

    -- * BlueGreenDeployment
    BlueGreenDeployment (..),
    newBlueGreenDeployment,
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

    -- * BlueGreenDeploymentTask
    BlueGreenDeploymentTask (..),
    newBlueGreenDeploymentTask,
    blueGreenDeploymentTask_name,
    blueGreenDeploymentTask_status,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_certificateArn,
    certificate_certificateIdentifier,
    certificate_certificateType,
    certificate_customerOverride,
    certificate_customerOverrideValidTill,
    certificate_thumbprint,
    certificate_validFrom,
    certificate_validTill,

    -- * CertificateDetails
    CertificateDetails (..),
    newCertificateDetails,
    certificateDetails_cAIdentifier,
    certificateDetails_validTill,

    -- * CharacterSet
    CharacterSet (..),
    newCharacterSet,
    characterSet_characterSetDescription,
    characterSet_characterSetName,

    -- * CloudwatchLogsExportConfiguration
    CloudwatchLogsExportConfiguration (..),
    newCloudwatchLogsExportConfiguration,
    cloudwatchLogsExportConfiguration_disableLogTypes,
    cloudwatchLogsExportConfiguration_enableLogTypes,

    -- * ClusterPendingModifiedValues
    ClusterPendingModifiedValues (..),
    newClusterPendingModifiedValues,
    clusterPendingModifiedValues_allocatedStorage,
    clusterPendingModifiedValues_backupRetentionPeriod,
    clusterPendingModifiedValues_dbClusterIdentifier,
    clusterPendingModifiedValues_engineVersion,
    clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled,
    clusterPendingModifiedValues_iops,
    clusterPendingModifiedValues_masterUserPassword,
    clusterPendingModifiedValues_pendingCloudwatchLogsExports,
    clusterPendingModifiedValues_storageType,

    -- * ConnectionPoolConfiguration
    ConnectionPoolConfiguration (..),
    newConnectionPoolConfiguration,
    connectionPoolConfiguration_connectionBorrowTimeout,
    connectionPoolConfiguration_initQuery,
    connectionPoolConfiguration_maxConnectionsPercent,
    connectionPoolConfiguration_maxIdleConnectionsPercent,
    connectionPoolConfiguration_sessionPinningFilters,

    -- * ConnectionPoolConfigurationInfo
    ConnectionPoolConfigurationInfo (..),
    newConnectionPoolConfigurationInfo,
    connectionPoolConfigurationInfo_connectionBorrowTimeout,
    connectionPoolConfigurationInfo_initQuery,
    connectionPoolConfigurationInfo_maxConnectionsPercent,
    connectionPoolConfigurationInfo_maxIdleConnectionsPercent,
    connectionPoolConfigurationInfo_sessionPinningFilters,

    -- * CustomDBEngineVersionAMI
    CustomDBEngineVersionAMI (..),
    newCustomDBEngineVersionAMI,
    customDBEngineVersionAMI_imageId,
    customDBEngineVersionAMI_status,

    -- * DBCluster
    DBCluster (..),
    newDBCluster,
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

    -- * DBClusterBacktrack
    DBClusterBacktrack (..),
    newDBClusterBacktrack,
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_backtrackRequestCreationTime,
    dbClusterBacktrack_backtrackTo,
    dbClusterBacktrack_backtrackedFrom,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_status,

    -- * DBClusterEndpoint
    DBClusterEndpoint (..),
    newDBClusterEndpoint,
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

    -- * DBClusterMember
    DBClusterMember (..),
    newDBClusterMember,
    dbClusterMember_dbClusterParameterGroupStatus,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_isClusterWriter,
    dbClusterMember_promotionTier,

    -- * DBClusterOptionGroupStatus
    DBClusterOptionGroupStatus (..),
    newDBClusterOptionGroupStatus,
    dbClusterOptionGroupStatus_dbClusterOptionGroupName,
    dbClusterOptionGroupStatus_status,

    -- * DBClusterParameterGroup
    DBClusterParameterGroup (..),
    newDBClusterParameterGroup,
    dbClusterParameterGroup_dbClusterParameterGroupArn,
    dbClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroup_dbParameterGroupFamily,
    dbClusterParameterGroup_description,

    -- * DBClusterParameterGroupNameMessage
    DBClusterParameterGroupNameMessage (..),
    newDBClusterParameterGroupNameMessage,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- * DBClusterRole
    DBClusterRole (..),
    newDBClusterRole,
    dbClusterRole_featureName,
    dbClusterRole_roleArn,
    dbClusterRole_status,

    -- * DBClusterSnapshot
    DBClusterSnapshot (..),
    newDBClusterSnapshot,
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

    -- * DBClusterSnapshotAttribute
    DBClusterSnapshotAttribute (..),
    newDBClusterSnapshotAttribute,
    dbClusterSnapshotAttribute_attributeName,
    dbClusterSnapshotAttribute_attributeValues,

    -- * DBClusterSnapshotAttributesResult
    DBClusterSnapshotAttributesResult (..),
    newDBClusterSnapshotAttributesResult,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier,

    -- * DBEngineVersion
    DBEngineVersion (..),
    newDBEngineVersion,
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

    -- * DBInstance
    DBInstance (..),
    newDBInstance,
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

    -- * DBInstanceAutomatedBackup
    DBInstanceAutomatedBackup (..),
    newDBInstanceAutomatedBackup,
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

    -- * DBInstanceAutomatedBackupsReplication
    DBInstanceAutomatedBackupsReplication (..),
    newDBInstanceAutomatedBackupsReplication,
    dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn,

    -- * DBInstanceRole
    DBInstanceRole (..),
    newDBInstanceRole,
    dbInstanceRole_featureName,
    dbInstanceRole_roleArn,
    dbInstanceRole_status,

    -- * DBInstanceStatusInfo
    DBInstanceStatusInfo (..),
    newDBInstanceStatusInfo,
    dbInstanceStatusInfo_message,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_statusType,

    -- * DBParameterGroup
    DBParameterGroup (..),
    newDBParameterGroup,
    dbParameterGroup_dbParameterGroupArn,
    dbParameterGroup_dbParameterGroupFamily,
    dbParameterGroup_dbParameterGroupName,
    dbParameterGroup_description,

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

    -- * DBProxyEndpoint
    DBProxyEndpoint (..),
    newDBProxyEndpoint,
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

    -- * DBProxyTarget
    DBProxyTarget (..),
    newDBProxyTarget,
    dbProxyTarget_endpoint,
    dbProxyTarget_port,
    dbProxyTarget_rdsResourceId,
    dbProxyTarget_role,
    dbProxyTarget_targetArn,
    dbProxyTarget_targetHealth,
    dbProxyTarget_trackedClusterId,
    dbProxyTarget_type,

    -- * DBProxyTargetGroup
    DBProxyTargetGroup (..),
    newDBProxyTargetGroup,
    dbProxyTargetGroup_connectionPoolConfig,
    dbProxyTargetGroup_createdDate,
    dbProxyTargetGroup_dbProxyName,
    dbProxyTargetGroup_isDefault,
    dbProxyTargetGroup_status,
    dbProxyTargetGroup_targetGroupArn,
    dbProxyTargetGroup_targetGroupName,
    dbProxyTargetGroup_updatedDate,

    -- * DBSecurityGroup
    DBSecurityGroup (..),
    newDBSecurityGroup,
    dbSecurityGroup_dbSecurityGroupArn,
    dbSecurityGroup_dbSecurityGroupDescription,
    dbSecurityGroup_dbSecurityGroupName,
    dbSecurityGroup_eC2SecurityGroups,
    dbSecurityGroup_iPRanges,
    dbSecurityGroup_ownerId,
    dbSecurityGroup_vpcId,

    -- * DBSecurityGroupMembership
    DBSecurityGroupMembership (..),
    newDBSecurityGroupMembership,
    dbSecurityGroupMembership_dbSecurityGroupName,
    dbSecurityGroupMembership_status,

    -- * DBSnapshot
    DBSnapshot (..),
    newDBSnapshot,
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

    -- * DBSnapshotAttribute
    DBSnapshotAttribute (..),
    newDBSnapshotAttribute,
    dbSnapshotAttribute_attributeName,
    dbSnapshotAttribute_attributeValues,

    -- * DBSnapshotAttributesResult
    DBSnapshotAttributesResult (..),
    newDBSnapshotAttributesResult,
    dbSnapshotAttributesResult_dbSnapshotAttributes,
    dbSnapshotAttributesResult_dbSnapshotIdentifier,

    -- * DBSubnetGroup
    DBSubnetGroup (..),
    newDBSubnetGroup,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_subnetGroupStatus,
    dbSubnetGroup_subnets,
    dbSubnetGroup_supportedNetworkTypes,
    dbSubnetGroup_vpcId,

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
    domainMembership_iAMRoleName,
    domainMembership_status,

    -- * DoubleRange
    DoubleRange (..),
    newDoubleRange,
    doubleRange_from,
    doubleRange_to,

    -- * EC2SecurityGroup
    EC2SecurityGroup (..),
    newEC2SecurityGroup,
    eC2SecurityGroup_eC2SecurityGroupId,
    eC2SecurityGroup_eC2SecurityGroupName,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_status,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_hostedZoneId,
    endpoint_port,

    -- * EngineDefaults
    EngineDefaults (..),
    newEngineDefaults,
    engineDefaults_dbParameterGroupFamily,
    engineDefaults_marker,
    engineDefaults_parameters,

    -- * Event
    Event (..),
    newEvent,
    event_date,
    event_eventCategories,
    event_message,
    event_sourceArn,
    event_sourceIdentifier,
    event_sourceType,

    -- * EventCategoriesMap
    EventCategoriesMap (..),
    newEventCategoriesMap,
    eventCategoriesMap_eventCategories,
    eventCategoriesMap_sourceType,

    -- * EventSubscription
    EventSubscription (..),
    newEventSubscription,
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

    -- * ExportTask
    ExportTask (..),
    newExportTask,
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

    -- * FailoverState
    FailoverState (..),
    newFailoverState,
    failoverState_fromDbClusterArn,
    failoverState_status,
    failoverState_toDbClusterArn,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * GlobalCluster
    GlobalCluster (..),
    newGlobalCluster,
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

    -- * GlobalClusterMember
    GlobalClusterMember (..),
    newGlobalClusterMember,
    globalClusterMember_dbClusterArn,
    globalClusterMember_globalWriteForwardingStatus,
    globalClusterMember_isWriter,
    globalClusterMember_readers,

    -- * IPRange
    IPRange (..),
    newIPRange,
    iPRange_cidrip,
    iPRange_status,

    -- * MasterUserSecret
    MasterUserSecret (..),
    newMasterUserSecret,
    masterUserSecret_kmsKeyId,
    masterUserSecret_secretArn,
    masterUserSecret_secretStatus,

    -- * MinimumEngineVersionPerAllowedValue
    MinimumEngineVersionPerAllowedValue (..),
    newMinimumEngineVersionPerAllowedValue,
    minimumEngineVersionPerAllowedValue_allowedValue,
    minimumEngineVersionPerAllowedValue_minimumEngineVersion,

    -- * Option
    Option (..),
    newOption,
    option_dbSecurityGroupMemberships,
    option_optionDescription,
    option_optionName,
    option_optionSettings,
    option_optionVersion,
    option_permanent,
    option_persistent,
    option_port,
    option_vpcSecurityGroupMemberships,

    -- * OptionConfiguration
    OptionConfiguration (..),
    newOptionConfiguration,
    optionConfiguration_dbSecurityGroupMemberships,
    optionConfiguration_optionSettings,
    optionConfiguration_optionVersion,
    optionConfiguration_port,
    optionConfiguration_vpcSecurityGroupMemberships,
    optionConfiguration_optionName,

    -- * OptionGroup
    OptionGroup (..),
    newOptionGroup,
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

    -- * OptionGroupMembership
    OptionGroupMembership (..),
    newOptionGroupMembership,
    optionGroupMembership_optionGroupName,
    optionGroupMembership_status,

    -- * OptionGroupOption
    OptionGroupOption (..),
    newOptionGroupOption,
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

    -- * OptionGroupOptionSetting
    OptionGroupOptionSetting (..),
    newOptionGroupOptionSetting,
    optionGroupOptionSetting_allowedValues,
    optionGroupOptionSetting_applyType,
    optionGroupOptionSetting_defaultValue,
    optionGroupOptionSetting_isModifiable,
    optionGroupOptionSetting_isRequired,
    optionGroupOptionSetting_minimumEngineVersionPerAllowedValue,
    optionGroupOptionSetting_settingDescription,
    optionGroupOptionSetting_settingName,

    -- * OptionSetting
    OptionSetting (..),
    newOptionSetting,
    optionSetting_allowedValues,
    optionSetting_applyType,
    optionSetting_dataType,
    optionSetting_defaultValue,
    optionSetting_description,
    optionSetting_isCollection,
    optionSetting_isModifiable,
    optionSetting_name,
    optionSetting_value,

    -- * OptionVersion
    OptionVersion (..),
    newOptionVersion,
    optionVersion_isDefault,
    optionVersion_version,

    -- * OrderableDBInstanceOption
    OrderableDBInstanceOption (..),
    newOrderableDBInstanceOption,
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

    -- * Outpost
    Outpost (..),
    newOutpost,
    outpost_arn,

    -- * Parameter
    Parameter (..),
    newParameter,
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

    -- * PendingCloudwatchLogsExports
    PendingCloudwatchLogsExports (..),
    newPendingCloudwatchLogsExports,
    pendingCloudwatchLogsExports_logTypesToDisable,
    pendingCloudwatchLogsExports_logTypesToEnable,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_optInStatus,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    newPendingModifiedValues,
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

    -- * ProcessorFeature
    ProcessorFeature (..),
    newProcessorFeature,
    processorFeature_name,
    processorFeature_value,

    -- * Range
    Range (..),
    newRange,
    range_from,
    range_step,
    range_to,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- * ReservedDBInstance
    ReservedDBInstance (..),
    newReservedDBInstance,
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

    -- * ReservedDBInstancesOffering
    ReservedDBInstancesOffering (..),
    newReservedDBInstancesOffering,
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

    -- * ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    newResourcePendingMaintenanceActions,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- * RestoreWindow
    RestoreWindow (..),
    newRestoreWindow,
    restoreWindow_earliestTime,
    restoreWindow_latestTime,

    -- * ScalingConfiguration
    ScalingConfiguration (..),
    newScalingConfiguration,
    scalingConfiguration_autoPause,
    scalingConfiguration_maxCapacity,
    scalingConfiguration_minCapacity,
    scalingConfiguration_secondsBeforeTimeout,
    scalingConfiguration_secondsUntilAutoPause,
    scalingConfiguration_timeoutAction,

    -- * ScalingConfigurationInfo
    ScalingConfigurationInfo (..),
    newScalingConfigurationInfo,
    scalingConfigurationInfo_autoPause,
    scalingConfigurationInfo_maxCapacity,
    scalingConfigurationInfo_minCapacity,
    scalingConfigurationInfo_secondsBeforeTimeout,
    scalingConfigurationInfo_secondsUntilAutoPause,
    scalingConfigurationInfo_timeoutAction,

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
    sourceRegion_endpoint,
    sourceRegion_regionName,
    sourceRegion_status,
    sourceRegion_supportsDBInstanceAutomatedBackupsReplication,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,
    subnet_subnetOutpost,
    subnet_subnetStatus,

    -- * SwitchoverDetail
    SwitchoverDetail (..),
    newSwitchoverDetail,
    switchoverDetail_sourceMember,
    switchoverDetail_status,
    switchoverDetail_targetMember,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TargetHealth
    TargetHealth (..),
    newTargetHealth,
    targetHealth_description,
    targetHealth_reason,
    targetHealth_state,

    -- * Timezone
    Timezone (..),
    newTimezone,
    timezone_timezoneName,

    -- * UpgradeTarget
    UpgradeTarget (..),
    newUpgradeTarget,
    upgradeTarget_autoUpgrade,
    upgradeTarget_description,
    upgradeTarget_engine,
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,
    upgradeTarget_supportedEngineModes,
    upgradeTarget_supportsBabelfish,
    upgradeTarget_supportsGlobalDatabases,
    upgradeTarget_supportsParallelQuery,

    -- * UserAuthConfig
    UserAuthConfig (..),
    newUserAuthConfig,
    userAuthConfig_authScheme,
    userAuthConfig_clientPasswordAuthType,
    userAuthConfig_description,
    userAuthConfig_iAMAuth,
    userAuthConfig_secretArn,
    userAuthConfig_userName,

    -- * UserAuthConfigInfo
    UserAuthConfigInfo (..),
    newUserAuthConfigInfo,
    userAuthConfigInfo_authScheme,
    userAuthConfigInfo_clientPasswordAuthType,
    userAuthConfigInfo_description,
    userAuthConfigInfo_iAMAuth,
    userAuthConfigInfo_secretArn,
    userAuthConfigInfo_userName,

    -- * ValidDBInstanceModificationsMessage
    ValidDBInstanceModificationsMessage (..),
    newValidDBInstanceModificationsMessage,
    validDBInstanceModificationsMessage_storage,
    validDBInstanceModificationsMessage_validProcessorFeatures,

    -- * ValidStorageOptions
    ValidStorageOptions (..),
    newValidStorageOptions,
    validStorageOptions_iopsToStorageRatio,
    validStorageOptions_provisionedIops,
    validStorageOptions_provisionedStorageThroughput,
    validStorageOptions_storageSize,
    validStorageOptions_storageThroughputToIopsRatio,
    validStorageOptions_storageType,
    validStorageOptions_supportsStorageAutoscaling,

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
import Amazonka.RDS.Types.BlueGreenDeployment
import Amazonka.RDS.Types.BlueGreenDeploymentTask
import Amazonka.RDS.Types.Certificate
import Amazonka.RDS.Types.CertificateDetails
import Amazonka.RDS.Types.CharacterSet
import Amazonka.RDS.Types.ClientPasswordAuthType
import Amazonka.RDS.Types.CloudwatchLogsExportConfiguration
import Amazonka.RDS.Types.ClusterPendingModifiedValues
import Amazonka.RDS.Types.ConnectionPoolConfiguration
import Amazonka.RDS.Types.ConnectionPoolConfigurationInfo
import Amazonka.RDS.Types.CustomDBEngineVersionAMI
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
import Amazonka.RDS.Types.SwitchoverDetail
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified CIDR IP range or Amazon EC2 security group is already
-- authorized for the specified DB security group.
_AuthorizationAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthorizationAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The specified CIDR IP range or Amazon EC2 security group might not be
-- authorized for the specified DB security group.
--
-- Or, RDS might not be authorized to perform necessary actions using IAM
-- on your behalf.
_AuthorizationNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | The DB security group authorization quota has been reached.
_AuthorizationQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthorizationQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | Prism for BackupPolicyNotFoundFault' errors.
_BackupPolicyNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BackupPolicyNotFoundFault =
  Core._MatchServiceError
    defaultService
    "BackupPolicyNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | A blue\/green deployment with the specified name already exists.
_BlueGreenDeploymentAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BlueGreenDeploymentAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "BlueGreenDeploymentAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | @BlueGreenDeploymentIdentifier@ doesn\'t refer to an existing
-- blue\/green deployment.
_BlueGreenDeploymentNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BlueGreenDeploymentNotFoundFault =
  Core._MatchServiceError
    defaultService
    "BlueGreenDeploymentNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | @CertificateIdentifier@ doesn\'t refer to an existing certificate.
_CertificateNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CertificateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Prelude.. Core.hasStatus 404

-- | An error occurred while trying to create the CEV.
_CreateCustomDBEngineVersionFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CreateCustomDBEngineVersionFault =
  Core._MatchServiceError
    defaultService
    "CreateCustomDBEngineVersionFault"
    Prelude.. Core.hasStatus 400

-- | @CustomAvailabilityZoneId@ doesn\'t refer to an existing custom
-- Availability Zone identifier.
_CustomAvailabilityZoneNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CustomAvailabilityZoneNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CustomAvailabilityZoneNotFound"
    Prelude.. Core.hasStatus 404

-- | A CEV with the specified name already exists.
_CustomDBEngineVersionAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CustomDBEngineVersionAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CustomDBEngineVersionAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The specified CEV was not found.
_CustomDBEngineVersionNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CustomDBEngineVersionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CustomDBEngineVersionNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | You have exceeded your CEV quota.
_CustomDBEngineVersionQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CustomDBEngineVersionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CustomDBEngineVersionQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The user already has a DB cluster with the given identifier.
_DBClusterAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | @BacktrackIdentifier@ doesn\'t refer to an existing backtrack.
_DBClusterBacktrackNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterBacktrackNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterBacktrackNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The specified custom endpoint can\'t be created because it already
-- exists.
_DBClusterEndpointAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterEndpointAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The specified custom endpoint doesn\'t exist.
_DBClusterEndpointNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterEndpointNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The cluster already has the maximum number of custom endpoints.
_DBClusterEndpointQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterEndpointQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointQuotaExceededFault"
    Prelude.. Core.hasStatus 403

-- | @DBClusterIdentifier@ doesn\'t refer to an existing DB cluster.
_DBClusterNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | @DBClusterParameterGroupName@ doesn\'t refer to an existing DB cluster
-- parameter group.
_DBClusterParameterGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The user attempted to create a new DB cluster and the user has already
-- reached the maximum allowed DB cluster quota.
_DBClusterQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 403

-- | The specified IAM role Amazon Resource Name (ARN) is already associated
-- with the specified DB cluster.
_DBClusterRoleAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterRoleAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The specified IAM role Amazon Resource Name (ARN) isn\'t associated with
-- the specified DB cluster.
_DBClusterRoleNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleNotFound"
    Prelude.. Core.hasStatus 404

-- | You have exceeded the maximum number of IAM roles that can be associated
-- with the specified DB cluster.
_DBClusterRoleQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterRoleQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The user already has a DB cluster snapshot with the given identifier.
_DBClusterSnapshotAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | @DBClusterSnapshotIdentifier@ doesn\'t refer to an existing DB cluster
-- snapshot.
_DBClusterSnapshotNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The user already has a DB instance with the given identifier.
_DBInstanceAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBInstanceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | No automated backup for this DB instance was found.
_DBInstanceAutomatedBackupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBInstanceAutomatedBackupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAutomatedBackupNotFound"
    Prelude.. Core.hasStatus 404

-- | The quota for retained automated backups was exceeded. This prevents you
-- from retaining any additional automated backups. The retained automated
-- backups quota is the same as your DB Instance quota.
_DBInstanceAutomatedBackupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBInstanceAutomatedBackupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAutomatedBackupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | @DBInstanceIdentifier@ doesn\'t refer to an existing DB instance.
_DBInstanceNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBInstanceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified @RoleArn@ or @FeatureName@ value is already associated
-- with the DB instance.
_DBInstanceRoleAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBInstanceRoleAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceRoleAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The specified @RoleArn@ value doesn\'t match the specified feature for
-- the DB instance.
_DBInstanceRoleNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBInstanceRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceRoleNotFound"
    Prelude.. Core.hasStatus 404

-- | You can\'t associate any more Amazon Web Services Identity and Access
-- Management (IAM) roles with the DB instance because the quota has been
-- reached.
_DBInstanceRoleQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBInstanceRoleQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceRoleQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | @LogFileName@ doesn\'t refer to an existing DB log file.
_DBLogFileNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBLogFileNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBLogFileNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | A DB parameter group with the same name exists.
_DBParameterGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | @DBParameterGroupName@ doesn\'t refer to an existing DB parameter group.
_DBParameterGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed number of DB
-- parameter groups.
_DBParameterGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The specified proxy name must be unique for all proxies owned by your
-- Amazon Web Services account in the specified Amazon Web Services Region.
_DBProxyAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBProxyAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBProxyAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The specified DB proxy endpoint name must be unique for all DB proxy
-- endpoints owned by your Amazon Web Services account in the specified
-- Amazon Web Services Region.
_DBProxyEndpointAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBProxyEndpointAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBProxyEndpointAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The DB proxy endpoint doesn\'t exist.
_DBProxyEndpointNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBProxyEndpointNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyEndpointNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The DB proxy already has the maximum number of endpoints.
_DBProxyEndpointQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBProxyEndpointQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBProxyEndpointQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The specified proxy name doesn\'t correspond to a proxy owned by your
-- Amazon Web Services account in the specified Amazon Web Services Region.
_DBProxyNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBProxyNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | Your Amazon Web Services account already has the maximum number of
-- proxies in the specified Amazon Web Services Region.
_DBProxyQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBProxyQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBProxyQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The proxy is already associated with the specified RDS DB instance or
-- Aurora DB cluster.
_DBProxyTargetAlreadyRegisteredFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBProxyTargetAlreadyRegisteredFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetAlreadyRegisteredFault"
    Prelude.. Core.hasStatus 400

-- | The specified target group isn\'t available for a proxy owned by your
-- Amazon Web Services account in the specified Amazon Web Services Region.
_DBProxyTargetGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBProxyTargetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The specified RDS DB instance or Aurora DB cluster isn\'t available for
-- a proxy owned by your Amazon Web Services account in the specified
-- Amazon Web Services Region.
_DBProxyTargetNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBProxyTargetNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | A DB security group with the name specified in @DBSecurityGroupName@
-- already exists.
_DBSecurityGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSecurityGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | @DBSecurityGroupName@ doesn\'t refer to an existing DB security group.
_DBSecurityGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | A DB security group isn\'t allowed for this action.
_DBSecurityGroupNotSupportedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSecurityGroupNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupNotSupported"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- security groups.
_DBSecurityGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSecurityGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "QuotaExceeded.DBSecurityGroup"
    Prelude.. Core.hasStatus 400

-- | @DBSnapshotIdentifier@ is already used by an existing snapshot.
_DBSnapshotAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | @DBSnapshotIdentifier@ doesn\'t refer to an existing DB snapshot.
_DBSnapshotNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotNotFound"
    Prelude.. Core.hasStatus 404

-- | @DBSubnetGroupName@ is already used by an existing DB subnet group.
_DBSubnetGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Subnets in the DB subnet group should cover at least two Availability
-- Zones unless there is only one Availability Zone.
_DBSubnetGroupDoesNotCoverEnoughAZs :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupDoesNotCoverEnoughAZs"
    Prelude.. Core.hasStatus 400

-- | The DBSubnetGroup shouldn\'t be specified while creating read replicas
-- that lie in the same region as the source instance.
_DBSubnetGroupNotAllowedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupNotAllowedFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupNotAllowedFault"
    Prelude.. Core.hasStatus 400

-- | @DBSubnetGroupName@ doesn\'t refer to an existing DB subnet group.
_DBSubnetGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed number of DB
-- subnet groups.
_DBSubnetGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of
-- subnets in a DB subnet groups.
_DBSubnetQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The DB upgrade failed because a resource the DB depends on can\'t be
-- modified.
_DBUpgradeDependencyFailureFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBUpgradeDependencyFailureFault =
  Core._MatchServiceError
    defaultService
    "DBUpgradeDependencyFailure"
    Prelude.. Core.hasStatus 400

-- | @Domain@ doesn\'t refer to an existing Active Directory domain.
_DomainNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DomainNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DomainNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The AMI configuration prerequisite has not been met.
_Ec2ImagePropertiesNotSupportedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_Ec2ImagePropertiesNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "Ec2ImagePropertiesNotSupportedFault"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of event subscriptions.
_EventSubscriptionQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EventSubscriptionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "EventSubscriptionQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | You can\'t start an export task that\'s already running.
_ExportTaskAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExportTaskAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ExportTaskAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The export task doesn\'t exist.
_ExportTaskNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExportTaskNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ExportTaskNotFound"
    Prelude.. Core.hasStatus 404

-- | The @GlobalClusterIdentifier@ already exists. Choose a new global
-- database identifier (unique name) to create a new global database
-- cluster.
_GlobalClusterAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GlobalClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The @GlobalClusterIdentifier@ doesn\'t refer to an existing global
-- database cluster.
_GlobalClusterNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GlobalClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The number of global database clusters for this account is already at
-- the maximum allowed.
_GlobalClusterQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GlobalClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The IAM role requires additional permissions to export to an Amazon S3
-- bucket.
_IamRoleMissingPermissionsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IamRoleMissingPermissionsFault =
  Core._MatchServiceError
    defaultService
    "IamRoleMissingPermissions"
    Prelude.. Core.hasStatus 400

-- | The IAM role is missing for exporting to an Amazon S3 bucket.
_IamRoleNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IamRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "IamRoleNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed number of DB
-- instances.
_InstanceQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InstanceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "InstanceQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The requested operation can\'t be performed because there aren\'t enough
-- available IP addresses in the proxy\'s subnets. Add more CIDR blocks to
-- the VPC or remove IP address that aren\'t required from the subnets.
_InsufficientAvailableIPsInSubnetFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientAvailableIPsInSubnetFault =
  Core._MatchServiceError
    defaultService
    "InsufficientAvailableIPsInSubnetFault"
    Prelude.. Core.hasStatus 400

-- | The DB cluster doesn\'t have enough capacity for the current operation.
_InsufficientDBClusterCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientDBClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBClusterCapacityFault"
    Prelude.. Core.hasStatus 403

-- | The specified DB instance class isn\'t available in the specified
-- Availability Zone.
_InsufficientDBInstanceCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientDBInstanceCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBInstanceCapacity"
    Prelude.. Core.hasStatus 400

-- | There is insufficient storage available for the current action. You
-- might be able to resolve this error by updating your subnet group to use
-- different Availability Zones that have more storage available.
_InsufficientStorageClusterCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientStorageClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientStorageClusterCapacity"
    Prelude.. Core.hasStatus 400

-- | The blue\/green deployment can\'t be switched over or deleted because
-- there is an invalid configuration in the green environment.
_InvalidBlueGreenDeploymentStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidBlueGreenDeploymentStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidBlueGreenDeploymentStateFault"
    Prelude.. Core.hasStatus 400

-- | You can\'t delete the CEV.
_InvalidCustomDBEngineVersionStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCustomDBEngineVersionStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCustomDBEngineVersionStateFault"
    Prelude.. Core.hasStatus 400

-- | @Capacity@ isn\'t a valid Aurora Serverless DB cluster capacity. Valid
-- capacity values are @2@, @4@, @8@, @16@, @32@, @64@, @128@, and @256@.
_InvalidDBClusterCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterCapacityFault"
    Prelude.. Core.hasStatus 400

-- | The requested operation can\'t be performed on the endpoint while the
-- endpoint is in this state.
_InvalidDBClusterEndpointStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBClusterEndpointStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterEndpointStateFault"
    Prelude.. Core.hasStatus 400

-- | The supplied value isn\'t a valid DB cluster snapshot state.
_InvalidDBClusterSnapshotStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBClusterSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterSnapshotStateFault"
    Prelude.. Core.hasStatus 400

-- | The requested operation can\'t be performed while the cluster is in this
-- state.
_InvalidDBClusterStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | The automated backup is in an invalid state. For example, this automated
-- backup is associated with an active instance.
_InvalidDBInstanceAutomatedBackupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBInstanceAutomatedBackupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBInstanceAutomatedBackupState"
    Prelude.. Core.hasStatus 400

-- | The DB instance isn\'t in a valid state.
_InvalidDBInstanceStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBInstanceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBInstanceState"
    Prelude.. Core.hasStatus 400

-- | The DB parameter group is in use or is in an invalid state. If you are
-- attempting to delete the parameter group, you can\'t delete it when the
-- parameter group is in this state.
_InvalidDBParameterGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBParameterGroupState"
    Prelude.. Core.hasStatus 400

-- | You can\'t perform this operation while the DB proxy endpoint is in a
-- particular state.
_InvalidDBProxyEndpointStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBProxyEndpointStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBProxyEndpointStateFault"
    Prelude.. Core.hasStatus 400

-- | The requested operation can\'t be performed while the proxy is in this
-- state.
_InvalidDBProxyStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBProxyStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBProxyStateFault"
    Prelude.. Core.hasStatus 400

-- | The state of the DB security group doesn\'t allow deletion.
_InvalidDBSecurityGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSecurityGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSecurityGroupState"
    Prelude.. Core.hasStatus 400

-- | The state of the DB snapshot doesn\'t allow deletion.
_InvalidDBSnapshotStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSnapshotState"
    Prelude.. Core.hasStatus 400

-- | The DBSubnetGroup doesn\'t belong to the same VPC as that of an existing
-- cross-region read replica of the same source instance.
_InvalidDBSubnetGroupFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSubnetGroupFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetGroupFault"
    Prelude.. Core.hasStatus 400

-- | The DB subnet group cannot be deleted because it\'s in use.
_InvalidDBSubnetGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSubnetGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetGroupStateFault"
    Prelude.. Core.hasStatus 400

-- | The DB subnet isn\'t in the /available/ state.
_InvalidDBSubnetStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSubnetStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetStateFault"
    Prelude.. Core.hasStatus 400

-- | This error can occur if someone else is modifying a subscription. You
-- should retry the action.
_InvalidEventSubscriptionStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidEventSubscriptionStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidEventSubscriptionState"
    Prelude.. Core.hasStatus 400

-- | The export is invalid for exporting to an Amazon S3 bucket.
_InvalidExportOnlyFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidExportOnlyFault =
  Core._MatchServiceError
    defaultService
    "InvalidExportOnly"
    Prelude.. Core.hasStatus 400

-- | The state of the export snapshot is invalid for exporting to an Amazon
-- S3 bucket.
_InvalidExportSourceStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidExportSourceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidExportSourceState"
    Prelude.. Core.hasStatus 400

-- | You can\'t cancel an export task that has completed.
_InvalidExportTaskStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidExportTaskStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidExportTaskStateFault"
    Prelude.. Core.hasStatus 400

-- | The global cluster is in an invalid state and can\'t perform the
-- requested operation.
_InvalidGlobalClusterStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidGlobalClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidGlobalClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | The option group isn\'t in the /available/ state.
_InvalidOptionGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidOptionGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidOptionGroupStateFault"
    Prelude.. Core.hasStatus 400

-- | Cannot restore from VPC backup to non-VPC DB instance.
_InvalidRestoreFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRestoreFault =
  Core._MatchServiceError
    defaultService
    "InvalidRestoreFault"
    Prelude.. Core.hasStatus 400

-- | The specified Amazon S3 bucket name can\'t be found or Amazon RDS isn\'t
-- authorized to access the specified Amazon S3 bucket. Verify the
-- __SourceS3BucketName__ and __S3IngestionRoleArn__ values and try again.
_InvalidS3BucketFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidS3BucketFault =
  Core._MatchServiceError
    defaultService
    "InvalidS3BucketFault"
    Prelude.. Core.hasStatus 400

-- | The requested subnet is invalid, or multiple subnets were requested that
-- are not all in a common VPC.
_InvalidSubnet :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | The DB subnet group doesn\'t cover all Availability Zones after it\'s
-- created because of users\' change.
_InvalidVPCNetworkStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Core.hasStatus 400

-- | An error occurred accessing an Amazon Web Services KMS key.
_KMSKeyNotAccessibleFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSKeyNotAccessibleFault =
  Core._MatchServiceError
    defaultService
    "KMSKeyNotAccessibleFault"
    Prelude.. Core.hasStatus 400

-- | The network type is invalid for the DB instance. Valid nework type
-- values are @IPV4@ and @DUAL@.
_NetworkTypeNotSupported :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NetworkTypeNotSupported =
  Core._MatchServiceError
    defaultService
    "NetworkTypeNotSupported"
    Prelude.. Core.hasStatus 400

-- | The option group you are trying to create already exists.
_OptionGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OptionGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The specified option group could not be found.
_OptionGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OptionGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The quota of 20 option groups was exceeded for this Amazon Web Services
-- account.
_OptionGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OptionGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | @SourceDBInstanceIdentifier@ refers to a DB instance with
-- @BackupRetentionPeriod@ equal to 0.
_PointInTimeRestoreNotEnabledFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PointInTimeRestoreNotEnabledFault =
  Core._MatchServiceError
    defaultService
    "PointInTimeRestoreNotEnabled"
    Prelude.. Core.hasStatus 400

-- | Provisioned IOPS not available in the specified Availability Zone.
_ProvisionedIopsNotAvailableInAZFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ProvisionedIopsNotAvailableInAZFault =
  Core._MatchServiceError
    defaultService
    "ProvisionedIopsNotAvailableInAZFault"
    Prelude.. Core.hasStatus 400

-- | User already has a reservation with the given identifier.
_ReservedDBInstanceAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedDBInstanceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstanceAlreadyExists"
    Prelude.. Core.hasStatus 404

-- | The specified reserved DB Instance not found.
_ReservedDBInstanceNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedDBInstanceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstanceNotFound"
    Prelude.. Core.hasStatus 404

-- | Request would exceed the user\'s DB Instance quota.
_ReservedDBInstanceQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedDBInstanceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstanceQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | Specified offering does not exist.
_ReservedDBInstancesOfferingNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedDBInstancesOfferingNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstancesOfferingNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified resource ID was not found.
_ResourceNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | SNS has responded that there is a problem with the SNS topic specified.
_SNSInvalidTopicFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError
    defaultService
    "SNSInvalidTopic"
    Prelude.. Core.hasStatus 400

-- | You do not have permission to publish to the SNS topic ARN.
_SNSNoAuthorizationFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorization"
    Prelude.. Core.hasStatus 400

-- | The SNS topic ARN does not exist.
_SNSTopicArnNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SNSTopicArnNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SNSTopicArnNotFound"
    Prelude.. Core.hasStatus 404

-- | You have exceeded the maximum number of accounts that you can share a
-- manual DB snapshot with.
_SharedSnapshotQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SharedSnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SharedSnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- snapshots.
_SnapshotQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The source DB cluster isn\'t supported for a blue\/green deployment.
_SourceClusterNotSupportedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SourceClusterNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "SourceClusterNotSupportedFault"
    Prelude.. Core.hasStatus 400

-- | The source DB instance isn\'t supported for a blue\/green deployment.
_SourceDatabaseNotSupportedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SourceDatabaseNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "SourceDatabaseNotSupportedFault"
    Prelude.. Core.hasStatus 400

-- | The requested source could not be found.
_SourceNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SourceNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed amount of
-- storage available across all DB instances.
_StorageQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StorageQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "StorageQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The @aurora-iopt1@ storage type isn\'t available, because you modified
-- the DB cluster to use this storage type less than one month ago.
_StorageTypeNotAvailableFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StorageTypeNotAvailableFault =
  Core._MatchServiceError
    defaultService
    "StorageTypeNotAvailableFault"
    Prelude.. Core.hasStatus 400

-- | The specified @StorageType@ can\'t be associated with the DB instance.
_StorageTypeNotSupportedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StorageTypeNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "StorageTypeNotSupported"
    Prelude.. Core.hasStatus 400

-- | The DB subnet is already in use in the Availability Zone.
_SubnetAlreadyInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"
    Prelude.. Core.hasStatus 400

-- | The supplied subscription name already exists.
_SubscriptionAlreadyExistFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionAlreadyExistFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionAlreadyExist"
    Prelude.. Core.hasStatus 400

-- | The supplied category does not exist.
_SubscriptionCategoryNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionCategoryNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionCategoryNotFound"
    Prelude.. Core.hasStatus 404

-- | The subscription name does not exist.
_SubscriptionNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionNotFound"
    Prelude.. Core.hasStatus 404
