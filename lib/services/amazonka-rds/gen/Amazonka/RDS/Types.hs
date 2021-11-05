{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _PointInTimeRestoreNotEnabledFault,
    _InvalidDBParameterGroupStateFault,
    _ReservedDBInstanceQuotaExceededFault,
    _SourceNotFoundFault,
    _CertificateNotFoundFault,
    _AuthorizationQuotaExceededFault,
    _DBClusterSnapshotAlreadyExistsFault,
    _DBParameterGroupAlreadyExistsFault,
    _DBInstanceRoleQuotaExceededFault,
    _DBInstanceRoleAlreadyExistsFault,
    _DBParameterGroupQuotaExceededFault,
    _BackupPolicyNotFoundFault,
    _InsufficientDBClusterCapacityFault,
    _ReservedDBInstanceAlreadyExistsFault,
    _ProvisionedIopsNotAvailableInAZFault,
    _DBProxyTargetAlreadyRegisteredFault,
    _AuthorizationAlreadyExistsFault,
    _SubscriptionCategoryNotFoundFault,
    _DBProxyTargetNotFoundFault,
    _SubscriptionNotFoundFault,
    _InvalidSubnet,
    _SharedSnapshotQuotaExceededFault,
    _DBSubnetQuotaExceededFault,
    _GlobalClusterAlreadyExistsFault,
    _OptionGroupNotFoundFault,
    _DBClusterNotFoundFault,
    _InvalidDBProxyEndpointStateFault,
    _DBLogFileNotFoundFault,
    _DBProxyTargetGroupNotFoundFault,
    _InvalidS3BucketFault,
    _DBProxyQuotaExceededFault,
    _IamRoleNotFoundFault,
    _DBClusterAlreadyExistsFault,
    _StorageTypeNotSupportedFault,
    _DBSecurityGroupQuotaExceededFault,
    _DBProxyEndpointNotFoundFault,
    _OptionGroupAlreadyExistsFault,
    _ExportTaskNotFoundFault,
    _InsufficientAvailableIPsInSubnetFault,
    _DBProxyNotFoundFault,
    _OptionGroupQuotaExceededFault,
    _DBSecurityGroupAlreadyExistsFault,
    _SNSTopicArnNotFoundFault,
    _InvalidDBClusterEndpointStateFault,
    _InvalidEventSubscriptionStateFault,
    _InvalidDBInstanceAutomatedBackupStateFault,
    _KMSKeyNotAccessibleFault,
    _DBSnapshotNotFoundFault,
    _DBClusterParameterGroupNotFoundFault,
    _DBClusterQuotaExceededFault,
    _InvalidExportOnlyFault,
    _SnapshotQuotaExceededFault,
    _InvalidDBClusterCapacityFault,
    _DBSubnetGroupAlreadyExistsFault,
    _SNSNoAuthorizationFault,
    _DBSecurityGroupNotFoundFault,
    _DBSecurityGroupNotSupportedFault,
    _InvalidDBProxyStateFault,
    _DBProxyEndpointQuotaExceededFault,
    _InstanceQuotaExceededFault,
    _DBClusterBacktrackNotFoundFault,
    _DomainNotFoundFault,
    _DBParameterGroupNotFoundFault,
    _InvalidDBSubnetGroupFault,
    _ReservedDBInstancesOfferingNotFoundFault,
    _InvalidDBSubnetStateFault,
    _DBClusterSnapshotNotFoundFault,
    _SNSInvalidTopicFault,
    _InsufficientDBInstanceCapacityFault,
    _InvalidDBClusterSnapshotStateFault,
    _InstallationMediaAlreadyExistsFault,
    _SubscriptionAlreadyExistFault,
    _DBClusterRoleAlreadyExistsFault,
    _IamRoleMissingPermissionsFault,
    _DBClusterRoleQuotaExceededFault,
    _InvalidVPCNetworkStateFault,
    _DBInstanceRoleNotFoundFault,
    _AuthorizationNotFoundFault,
    _ReservedDBInstanceNotFoundFault,
    _DBSubnetGroupQuotaExceededFault,
    _CustomAvailabilityZoneNotFoundFault,
    _DBProxyEndpointAlreadyExistsFault,
    _InvalidGlobalClusterStateFault,
    _DBSubnetGroupNotAllowedFault,
    _InvalidExportTaskStateFault,
    _InvalidExportSourceStateFault,
    _ExportTaskAlreadyExistsFault,
    _EventSubscriptionQuotaExceededFault,
    _InsufficientStorageClusterCapacityFault,
    _DBClusterEndpointQuotaExceededFault,
    _InvalidOptionGroupStateFault,
    _DBInstanceAutomatedBackupQuotaExceededFault,
    _CustomAvailabilityZoneAlreadyExistsFault,
    _InvalidDBClusterStateFault,
    _GlobalClusterNotFoundFault,
    _DBInstanceAlreadyExistsFault,
    _InvalidRestoreFault,
    _InvalidDBSecurityGroupStateFault,
    _ResourceNotFoundFault,
    _DBSubnetGroupNotFoundFault,
    _DBUpgradeDependencyFailureFault,
    _CustomAvailabilityZoneQuotaExceededFault,
    _InvalidDBInstanceStateFault,
    _DBClusterEndpointAlreadyExistsFault,
    _DBSnapshotAlreadyExistsFault,
    _DBInstanceNotFoundFault,
    _StorageQuotaExceededFault,
    _DBProxyAlreadyExistsFault,
    _DBInstanceAutomatedBackupNotFoundFault,
    _InvalidDBSnapshotStateFault,
    _InvalidDBSubnetGroupStateFault,
    _GlobalClusterQuotaExceededFault,
    _DBClusterEndpointNotFoundFault,
    _InstallationMediaNotFoundFault,
    _DBSubnetGroupDoesNotCoverEnoughAZs,
    _SubnetAlreadyInUse,
    _DBClusterRoleNotFoundFault,

    -- * ActivityStreamMode
    ActivityStreamMode (..),

    -- * ActivityStreamStatus
    ActivityStreamStatus (..),

    -- * ApplyMethod
    ApplyMethod (..),

    -- * AuthScheme
    AuthScheme (..),

    -- * DBProxyEndpointStatus
    DBProxyEndpointStatus (..),

    -- * DBProxyEndpointTargetRole
    DBProxyEndpointTargetRole (..),

    -- * DBProxyStatus
    DBProxyStatus (..),

    -- * EngineFamily
    EngineFamily (..),

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
    certificate_certificateType,
    certificate_customerOverride,
    certificate_certificateArn,
    certificate_customerOverrideValidTill,
    certificate_validTill,
    certificate_certificateIdentifier,
    certificate_thumbprint,
    certificate_validFrom,

    -- * CharacterSet
    CharacterSet (..),
    newCharacterSet,
    characterSet_characterSetName,
    characterSet_characterSetDescription,

    -- * CloudwatchLogsExportConfiguration
    CloudwatchLogsExportConfiguration (..),
    newCloudwatchLogsExportConfiguration,
    cloudwatchLogsExportConfiguration_disableLogTypes,
    cloudwatchLogsExportConfiguration_enableLogTypes,

    -- * ClusterPendingModifiedValues
    ClusterPendingModifiedValues (..),
    newClusterPendingModifiedValues,
    clusterPendingModifiedValues_engineVersion,
    clusterPendingModifiedValues_dbClusterIdentifier,
    clusterPendingModifiedValues_masterUserPassword,
    clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled,
    clusterPendingModifiedValues_pendingCloudwatchLogsExports,

    -- * ConnectionPoolConfiguration
    ConnectionPoolConfiguration (..),
    newConnectionPoolConfiguration,
    connectionPoolConfiguration_maxIdleConnectionsPercent,
    connectionPoolConfiguration_sessionPinningFilters,
    connectionPoolConfiguration_maxConnectionsPercent,
    connectionPoolConfiguration_connectionBorrowTimeout,
    connectionPoolConfiguration_initQuery,

    -- * ConnectionPoolConfigurationInfo
    ConnectionPoolConfigurationInfo (..),
    newConnectionPoolConfigurationInfo,
    connectionPoolConfigurationInfo_maxIdleConnectionsPercent,
    connectionPoolConfigurationInfo_sessionPinningFilters,
    connectionPoolConfigurationInfo_maxConnectionsPercent,
    connectionPoolConfigurationInfo_connectionBorrowTimeout,
    connectionPoolConfigurationInfo_initQuery,

    -- * CustomAvailabilityZone
    CustomAvailabilityZone (..),
    newCustomAvailabilityZone,
    customAvailabilityZone_vpnDetails,
    customAvailabilityZone_customAvailabilityZoneName,
    customAvailabilityZone_customAvailabilityZoneId,
    customAvailabilityZone_customAvailabilityZoneStatus,

    -- * DBCluster
    DBCluster (..),
    newDBCluster,
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

    -- * DBClusterBacktrack
    DBClusterBacktrack (..),
    newDBClusterBacktrack,
    dbClusterBacktrack_status,
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_backtrackTo,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_backtrackedFrom,
    dbClusterBacktrack_backtrackRequestCreationTime,

    -- * DBClusterEndpoint
    DBClusterEndpoint (..),
    newDBClusterEndpoint,
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

    -- * DBClusterMember
    DBClusterMember (..),
    newDBClusterMember,
    dbClusterMember_promotionTier,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_isClusterWriter,
    dbClusterMember_dbClusterParameterGroupStatus,

    -- * DBClusterOptionGroupStatus
    DBClusterOptionGroupStatus (..),
    newDBClusterOptionGroupStatus,
    dbClusterOptionGroupStatus_status,
    dbClusterOptionGroupStatus_dbClusterOptionGroupName,

    -- * DBClusterParameterGroup
    DBClusterParameterGroup (..),
    newDBClusterParameterGroup,
    dbClusterParameterGroup_dbClusterParameterGroupArn,
    dbClusterParameterGroup_dbParameterGroupFamily,
    dbClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroup_description,

    -- * DBClusterParameterGroupNameMessage
    DBClusterParameterGroupNameMessage (..),
    newDBClusterParameterGroupNameMessage,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- * DBClusterRole
    DBClusterRole (..),
    newDBClusterRole,
    dbClusterRole_status,
    dbClusterRole_featureName,
    dbClusterRole_roleArn,

    -- * DBClusterSnapshot
    DBClusterSnapshot (..),
    newDBClusterSnapshot,
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

    -- * DBInstance
    DBInstance (..),
    newDBInstance,
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

    -- * DBInstanceAutomatedBackup
    DBInstanceAutomatedBackup (..),
    newDBInstanceAutomatedBackup,
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

    -- * DBInstanceAutomatedBackupsReplication
    DBInstanceAutomatedBackupsReplication (..),
    newDBInstanceAutomatedBackupsReplication,
    dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn,

    -- * DBInstanceRole
    DBInstanceRole (..),
    newDBInstanceRole,
    dbInstanceRole_status,
    dbInstanceRole_featureName,
    dbInstanceRole_roleArn,

    -- * DBInstanceStatusInfo
    DBInstanceStatusInfo (..),
    newDBInstanceStatusInfo,
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_statusType,
    dbInstanceStatusInfo_message,

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

    -- * DBProxyEndpoint
    DBProxyEndpoint (..),
    newDBProxyEndpoint,
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

    -- * DBProxyTarget
    DBProxyTarget (..),
    newDBProxyTarget,
    dbProxyTarget_targetArn,
    dbProxyTarget_targetHealth,
    dbProxyTarget_trackedClusterId,
    dbProxyTarget_role,
    dbProxyTarget_rdsResourceId,
    dbProxyTarget_type,
    dbProxyTarget_endpoint,
    dbProxyTarget_port,

    -- * DBProxyTargetGroup
    DBProxyTargetGroup (..),
    newDBProxyTargetGroup,
    dbProxyTargetGroup_status,
    dbProxyTargetGroup_connectionPoolConfig,
    dbProxyTargetGroup_targetGroupArn,
    dbProxyTargetGroup_updatedDate,
    dbProxyTargetGroup_createdDate,
    dbProxyTargetGroup_dbProxyName,
    dbProxyTargetGroup_targetGroupName,
    dbProxyTargetGroup_isDefault,

    -- * DBSecurityGroup
    DBSecurityGroup (..),
    newDBSecurityGroup,
    dbSecurityGroup_vpcId,
    dbSecurityGroup_ownerId,
    dbSecurityGroup_dbSecurityGroupArn,
    dbSecurityGroup_iPRanges,
    dbSecurityGroup_dbSecurityGroupName,
    dbSecurityGroup_eC2SecurityGroups,
    dbSecurityGroup_dbSecurityGroupDescription,

    -- * DBSecurityGroupMembership
    DBSecurityGroupMembership (..),
    newDBSecurityGroupMembership,
    dbSecurityGroupMembership_status,
    dbSecurityGroupMembership_dbSecurityGroupName,

    -- * DBSnapshot
    DBSnapshot (..),
    newDBSnapshot,
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

    -- * DBSnapshotAttribute
    DBSnapshotAttribute (..),
    newDBSnapshotAttribute,
    dbSnapshotAttribute_attributeValues,
    dbSnapshotAttribute_attributeName,

    -- * DBSnapshotAttributesResult
    DBSnapshotAttributesResult (..),
    newDBSnapshotAttributesResult,
    dbSnapshotAttributesResult_dbSnapshotIdentifier,
    dbSnapshotAttributesResult_dbSnapshotAttributes,

    -- * DBSubnetGroup
    DBSubnetGroup (..),
    newDBSubnetGroup,
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_vpcId,
    dbSubnetGroup_subnets,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_subnetGroupStatus,

    -- * DescribeDBLogFilesDetails
    DescribeDBLogFilesDetails (..),
    newDescribeDBLogFilesDetails,
    describeDBLogFilesDetails_lastWritten,
    describeDBLogFilesDetails_size,
    describeDBLogFilesDetails_logFileName,

    -- * DomainMembership
    DomainMembership (..),
    newDomainMembership,
    domainMembership_status,
    domainMembership_fqdn,
    domainMembership_domain,
    domainMembership_iAMRoleName,

    -- * DoubleRange
    DoubleRange (..),
    newDoubleRange,
    doubleRange_to,
    doubleRange_from,

    -- * EC2SecurityGroup
    EC2SecurityGroup (..),
    newEC2SecurityGroup,
    eC2SecurityGroup_status,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_eC2SecurityGroupName,
    eC2SecurityGroup_eC2SecurityGroupId,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_hostedZoneId,
    endpoint_address,
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
    event_sourceType,
    event_sourceArn,
    event_sourceIdentifier,
    event_date,
    event_eventCategories,
    event_message,

    -- * EventCategoriesMap
    EventCategoriesMap (..),
    newEventCategoriesMap,
    eventCategoriesMap_sourceType,
    eventCategoriesMap_eventCategories,

    -- * EventSubscription
    EventSubscription (..),
    newEventSubscription,
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

    -- * ExportTask
    ExportTask (..),
    newExportTask,
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

    -- * FailoverState
    FailoverState (..),
    newFailoverState,
    failoverState_status,
    failoverState_toDbClusterArn,
    failoverState_fromDbClusterArn,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * GlobalCluster
    GlobalCluster (..),
    newGlobalCluster,
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

    -- * GlobalClusterMember
    GlobalClusterMember (..),
    newGlobalClusterMember,
    globalClusterMember_readers,
    globalClusterMember_dbClusterArn,
    globalClusterMember_isWriter,
    globalClusterMember_globalWriteForwardingStatus,

    -- * IPRange
    IPRange (..),
    newIPRange,
    iPRange_status,
    iPRange_cidrip,

    -- * InstallationMedia
    InstallationMedia (..),
    newInstallationMedia,
    installationMedia_engineVersion,
    installationMedia_status,
    installationMedia_installationMediaId,
    installationMedia_engineInstallationMediaPath,
    installationMedia_engine,
    installationMedia_oSInstallationMediaPath,
    installationMedia_customAvailabilityZoneId,
    installationMedia_failureCause,

    -- * InstallationMediaFailureCause
    InstallationMediaFailureCause (..),
    newInstallationMediaFailureCause,
    installationMediaFailureCause_message,

    -- * MinimumEngineVersionPerAllowedValue
    MinimumEngineVersionPerAllowedValue (..),
    newMinimumEngineVersionPerAllowedValue,
    minimumEngineVersionPerAllowedValue_minimumEngineVersion,
    minimumEngineVersionPerAllowedValue_allowedValue,

    -- * Option
    Option (..),
    newOption,
    option_optionName,
    option_permanent,
    option_persistent,
    option_optionDescription,
    option_optionSettings,
    option_vpcSecurityGroupMemberships,
    option_dbSecurityGroupMemberships,
    option_optionVersion,
    option_port,

    -- * OptionConfiguration
    OptionConfiguration (..),
    newOptionConfiguration,
    optionConfiguration_optionSettings,
    optionConfiguration_vpcSecurityGroupMemberships,
    optionConfiguration_dbSecurityGroupMemberships,
    optionConfiguration_optionVersion,
    optionConfiguration_port,
    optionConfiguration_optionName,

    -- * OptionGroup
    OptionGroup (..),
    newOptionGroup,
    optionGroup_optionGroupDescription,
    optionGroup_vpcId,
    optionGroup_allowsVpcAndNonVpcInstanceMemberships,
    optionGroup_engineName,
    optionGroup_optionGroupArn,
    optionGroup_majorEngineVersion,
    optionGroup_options,
    optionGroup_optionGroupName,

    -- * OptionGroupMembership
    OptionGroupMembership (..),
    newOptionGroupMembership,
    optionGroupMembership_status,
    optionGroupMembership_optionGroupName,

    -- * OptionGroupOption
    OptionGroupOption (..),
    newOptionGroupOption,
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

    -- * OptionGroupOptionSetting
    OptionGroupOptionSetting (..),
    newOptionGroupOptionSetting,
    optionGroupOptionSetting_applyType,
    optionGroupOptionSetting_minimumEngineVersionPerAllowedValue,
    optionGroupOptionSetting_settingName,
    optionGroupOptionSetting_defaultValue,
    optionGroupOptionSetting_isModifiable,
    optionGroupOptionSetting_settingDescription,
    optionGroupOptionSetting_allowedValues,
    optionGroupOptionSetting_isRequired,

    -- * OptionSetting
    OptionSetting (..),
    newOptionSetting,
    optionSetting_isCollection,
    optionSetting_applyType,
    optionSetting_value,
    optionSetting_name,
    optionSetting_defaultValue,
    optionSetting_isModifiable,
    optionSetting_dataType,
    optionSetting_allowedValues,
    optionSetting_description,

    -- * OptionVersion
    OptionVersion (..),
    newOptionVersion,
    optionVersion_version,
    optionVersion_isDefault,

    -- * OrderableDBInstanceOption
    OrderableDBInstanceOption (..),
    newOrderableDBInstanceOption,
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

    -- * Outpost
    Outpost (..),
    newOutpost,
    outpost_arn,

    -- * Parameter
    Parameter (..),
    newParameter,
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

    -- * PendingCloudwatchLogsExports
    PendingCloudwatchLogsExports (..),
    newPendingCloudwatchLogsExports,
    pendingCloudwatchLogsExports_logTypesToEnable,
    pendingCloudwatchLogsExports_logTypesToDisable,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_currentApplyDate,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    newPendingModifiedValues,
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

    -- * ProcessorFeature
    ProcessorFeature (..),
    newProcessorFeature,
    processorFeature_value,
    processorFeature_name,

    -- * Range
    Range (..),
    newRange,
    range_to,
    range_from,
    range_step,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_recurringChargeFrequency,
    recurringCharge_recurringChargeAmount,

    -- * ReservedDBInstance
    ReservedDBInstance (..),
    newReservedDBInstance,
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

    -- * ReservedDBInstancesOffering
    ReservedDBInstancesOffering (..),
    newReservedDBInstancesOffering,
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

    -- * ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    newResourcePendingMaintenanceActions,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- * RestoreWindow
    RestoreWindow (..),
    newRestoreWindow,
    restoreWindow_latestTime,
    restoreWindow_earliestTime,

    -- * ScalingConfiguration
    ScalingConfiguration (..),
    newScalingConfiguration,
    scalingConfiguration_secondsUntilAutoPause,
    scalingConfiguration_timeoutAction,
    scalingConfiguration_autoPause,
    scalingConfiguration_maxCapacity,
    scalingConfiguration_minCapacity,
    scalingConfiguration_secondsBeforeTimeout,

    -- * ScalingConfigurationInfo
    ScalingConfigurationInfo (..),
    newScalingConfigurationInfo,
    scalingConfigurationInfo_secondsUntilAutoPause,
    scalingConfigurationInfo_timeoutAction,
    scalingConfigurationInfo_autoPause,
    scalingConfigurationInfo_maxCapacity,
    scalingConfigurationInfo_minCapacity,
    scalingConfigurationInfo_secondsBeforeTimeout,

    -- * SourceRegion
    SourceRegion (..),
    newSourceRegion,
    sourceRegion_supportsDBInstanceAutomatedBackupsReplication,
    sourceRegion_status,
    sourceRegion_regionName,
    sourceRegion_endpoint,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetStatus,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,
    subnet_subnetOutpost,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TargetHealth
    TargetHealth (..),
    newTargetHealth,
    targetHealth_state,
    targetHealth_reason,
    targetHealth_description,

    -- * Timezone
    Timezone (..),
    newTimezone,
    timezone_timezoneName,

    -- * UpgradeTarget
    UpgradeTarget (..),
    newUpgradeTarget,
    upgradeTarget_engineVersion,
    upgradeTarget_supportedEngineModes,
    upgradeTarget_isMajorVersionUpgrade,
    upgradeTarget_engine,
    upgradeTarget_supportsGlobalDatabases,
    upgradeTarget_autoUpgrade,
    upgradeTarget_supportsParallelQuery,
    upgradeTarget_description,

    -- * UserAuthConfig
    UserAuthConfig (..),
    newUserAuthConfig,
    userAuthConfig_iAMAuth,
    userAuthConfig_userName,
    userAuthConfig_authScheme,
    userAuthConfig_secretArn,
    userAuthConfig_description,

    -- * UserAuthConfigInfo
    UserAuthConfigInfo (..),
    newUserAuthConfigInfo,
    userAuthConfigInfo_iAMAuth,
    userAuthConfigInfo_userName,
    userAuthConfigInfo_authScheme,
    userAuthConfigInfo_secretArn,
    userAuthConfigInfo_description,

    -- * ValidDBInstanceModificationsMessage
    ValidDBInstanceModificationsMessage (..),
    newValidDBInstanceModificationsMessage,
    validDBInstanceModificationsMessage_validProcessorFeatures,
    validDBInstanceModificationsMessage_storage,

    -- * ValidStorageOptions
    ValidStorageOptions (..),
    newValidStorageOptions,
    validStorageOptions_storageSize,
    validStorageOptions_provisionedIops,
    validStorageOptions_iopsToStorageRatio,
    validStorageOptions_supportsStorageAutoscaling,
    validStorageOptions_storageType,

    -- * VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    newVpcSecurityGroupMembership,
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,

    -- * VpnDetails
    VpnDetails (..),
    newVpnDetails,
    vpnDetails_vpnName,
    vpnDetails_vpnTunnelOriginatorIP,
    vpnDetails_vpnId,
    vpnDetails_vpnState,
    vpnDetails_vpnPSK,
    vpnDetails_vpnGatewayIp,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.AccountQuota
import Amazonka.RDS.Types.ActivityStreamMode
import Amazonka.RDS.Types.ActivityStreamStatus
import Amazonka.RDS.Types.ApplyMethod
import Amazonka.RDS.Types.AuthScheme
import Amazonka.RDS.Types.AvailabilityZone
import Amazonka.RDS.Types.AvailableProcessorFeature
import Amazonka.RDS.Types.Certificate
import Amazonka.RDS.Types.CharacterSet
import Amazonka.RDS.Types.CloudwatchLogsExportConfiguration
import Amazonka.RDS.Types.ClusterPendingModifiedValues
import Amazonka.RDS.Types.ConnectionPoolConfiguration
import Amazonka.RDS.Types.ConnectionPoolConfigurationInfo
import Amazonka.RDS.Types.CustomAvailabilityZone
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
import Amazonka.RDS.Types.ExportTask
import Amazonka.RDS.Types.FailoverState
import Amazonka.RDS.Types.FailoverStatus
import Amazonka.RDS.Types.Filter
import Amazonka.RDS.Types.GlobalCluster
import Amazonka.RDS.Types.GlobalClusterMember
import Amazonka.RDS.Types.IAMAuthMode
import Amazonka.RDS.Types.IPRange
import Amazonka.RDS.Types.InstallationMedia
import Amazonka.RDS.Types.InstallationMediaFailureCause
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
import Amazonka.RDS.Types.VpnDetails
import Amazonka.RDS.Types.WriteForwardingStatus
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2014-10-31@ of the Amazon Relational Database Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "RDS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "rds",
      Core._serviceSigningName = "rds",
      Core._serviceVersion = "2014-10-31",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "RDS",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | @SourceDBInstanceIdentifier@ refers to a DB instance with
-- @BackupRetentionPeriod@ equal to 0.
_PointInTimeRestoreNotEnabledFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PointInTimeRestoreNotEnabledFault =
  Core._MatchServiceError
    defaultService
    "PointInTimeRestoreNotEnabled"
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

-- | Request would exceed the user\'s DB Instance quota.
_ReservedDBInstanceQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstanceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstanceQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The requested source could not be found.
_SourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SourceNotFound"
    Prelude.. Core.hasStatus 404

-- | @CertificateIdentifier@ doesn\'t refer to an existing certificate.
_CertificateNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Prelude.. Core.hasStatus 404

-- | The DB security group authorization quota has been reached.
_AuthorizationQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The user already has a DB cluster snapshot with the given identifier.
_DBClusterSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | A DB parameter group with the same name exists.
_DBParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupAlreadyExists"
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

-- | The specified @RoleArn@ or @FeatureName@ value is already associated
-- with the DB instance.
_DBInstanceRoleAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceRoleAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceRoleAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- parameter groups.
_DBParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | Prism for BackupPolicyNotFoundFault' errors.
_BackupPolicyNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BackupPolicyNotFoundFault =
  Core._MatchServiceError
    defaultService
    "BackupPolicyNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The DB cluster doesn\'t have enough capacity for the current operation.
_InsufficientDBClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientDBClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBClusterCapacityFault"
    Prelude.. Core.hasStatus 403

-- | User already has a reservation with the given identifier.
_ReservedDBInstanceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstanceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstanceAlreadyExists"
    Prelude.. Core.hasStatus 404

-- | Provisioned IOPS not available in the specified Availability Zone.
_ProvisionedIopsNotAvailableInAZFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ProvisionedIopsNotAvailableInAZFault =
  Core._MatchServiceError
    defaultService
    "ProvisionedIopsNotAvailableInAZFault"
    Prelude.. Core.hasStatus 400

-- | The proxy is already associated with the specified RDS DB instance or
-- Aurora DB cluster.
_DBProxyTargetAlreadyRegisteredFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyTargetAlreadyRegisteredFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetAlreadyRegisteredFault"
    Prelude.. Core.hasStatus 400

-- | The specified CIDR IP range or Amazon EC2 security group is already
-- authorized for the specified DB security group.
_AuthorizationAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The supplied category does not exist.
_SubscriptionCategoryNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionCategoryNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionCategoryNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified RDS DB instance or Aurora DB cluster isn\'t available for
-- a proxy owned by your Amazon Web Services account in the specified
-- Amazon Web Services Region.
_DBProxyTargetNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyTargetNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The subscription name does not exist.
_SubscriptionNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested subnet is invalid, or multiple subnets were requested that
-- are not all in a common VPC.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the maximum number of accounts that you can share a
-- manual DB snapshot with.
_SharedSnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SharedSnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SharedSnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of
-- subnets in a DB subnet groups.
_DBSubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetQuotaExceededFault"
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

-- | The specified option group could not be found.
_OptionGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OptionGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | @DBClusterIdentifier@ doesn\'t refer to an existing DB cluster.
_DBClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | You can\'t perform this operation while the DB proxy endpoint is in a
-- particular state.
_InvalidDBProxyEndpointStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBProxyEndpointStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBProxyEndpointStateFault"
    Prelude.. Core.hasStatus 400

-- | @LogFileName@ doesn\'t refer to an existing DB log file.
_DBLogFileNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBLogFileNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBLogFileNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The specified target group isn\'t available for a proxy owned by your
-- Amazon Web Services account in the specified Amazon Web Services Region.
_DBProxyTargetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyTargetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetGroupNotFoundFault"
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

-- | Your Amazon Web Services account already has the maximum number of
-- proxies in the specified Amazon Web Services Region.
_DBProxyQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBProxyQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The IAM role is missing for exporting to an Amazon S3 bucket.
_IamRoleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IamRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "IamRoleNotFound"
    Prelude.. Core.hasStatus 404

-- | The user already has a DB cluster with the given identifier.
_DBClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | Storage of the @StorageType@ specified can\'t be associated with the DB
-- instance.
_StorageTypeNotSupportedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageTypeNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "StorageTypeNotSupported"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- security groups.
_DBSecurityGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "QuotaExceeded.DBSecurityGroup"
    Prelude.. Core.hasStatus 400

-- | The DB proxy endpoint doesn\'t exist.
_DBProxyEndpointNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyEndpointNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyEndpointNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The option group you are trying to create already exists.
_OptionGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OptionGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The export task doesn\'t exist.
_ExportTaskNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExportTaskNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ExportTaskNotFound"
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

-- | The specified proxy name doesn\'t correspond to a proxy owned by your
-- Amazon Web Services account in the specified Amazon Web Services Region.
_DBProxyNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The quota of 20 option groups was exceeded for this Amazon Web Services
-- account.
_OptionGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OptionGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | A DB security group with the name specified in @DBSecurityGroupName@
-- already exists.
_DBSecurityGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The SNS topic ARN does not exist.
_SNSTopicArnNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSTopicArnNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SNSTopicArnNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested operation can\'t be performed on the endpoint while the
-- endpoint is in this state.
_InvalidDBClusterEndpointStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterEndpointStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterEndpointStateFault"
    Prelude.. Core.hasStatus 400

-- | This error can occur if someone else is modifying a subscription. You
-- should retry the action.
_InvalidEventSubscriptionStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEventSubscriptionStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidEventSubscriptionState"
    Prelude.. Core.hasStatus 400

-- | The automated backup is in an invalid state. For example, this automated
-- backup is associated with an active instance.
_InvalidDBInstanceAutomatedBackupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBInstanceAutomatedBackupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBInstanceAutomatedBackupState"
    Prelude.. Core.hasStatus 400

-- | An error occurred accessing an Amazon Web Services KMS key.
_KMSKeyNotAccessibleFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSKeyNotAccessibleFault =
  Core._MatchServiceError
    defaultService
    "KMSKeyNotAccessibleFault"
    Prelude.. Core.hasStatus 400

-- | @DBSnapshotIdentifier@ doesn\'t refer to an existing DB snapshot.
_DBSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotNotFound"
    Prelude.. Core.hasStatus 404

-- | @DBClusterParameterGroupName@ doesn\'t refer to an existing DB cluster
-- parameter group.
_DBClusterParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The user attempted to create a new DB cluster and the user has already
-- reached the maximum allowed DB cluster quota.
_DBClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 403

-- | The export is invalid for exporting to an Amazon S3 bucket.
_InvalidExportOnlyFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidExportOnlyFault =
  Core._MatchServiceError
    defaultService
    "InvalidExportOnly"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- snapshots.
_SnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | @Capacity@ isn\'t a valid Aurora Serverless DB cluster capacity. Valid
-- capacity values are @2@, @4@, @8@, @16@, @32@, @64@, @128@, and @256@.
_InvalidDBClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterCapacityFault"
    Prelude.. Core.hasStatus 400

-- | @DBSubnetGroupName@ is already used by an existing DB subnet group.
_DBSubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | You do not have permission to publish to the SNS topic ARN.
_SNSNoAuthorizationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorization"
    Prelude.. Core.hasStatus 400

-- | @DBSecurityGroupName@ doesn\'t refer to an existing DB security group.
_DBSecurityGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | A DB security group isn\'t allowed for this action.
_DBSecurityGroupNotSupportedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupNotSupported"
    Prelude.. Core.hasStatus 400

-- | The requested operation can\'t be performed while the proxy is in this
-- state.
_InvalidDBProxyStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBProxyStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBProxyStateFault"
    Prelude.. Core.hasStatus 400

-- | The DB proxy already has the maximum number of endpoints.
_DBProxyEndpointQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyEndpointQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBProxyEndpointQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- instances.
_InstanceQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "InstanceQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | @BacktrackIdentifier@ doesn\'t refer to an existing backtrack.
_DBClusterBacktrackNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterBacktrackNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterBacktrackNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | @Domain@ doesn\'t refer to an existing Active Directory domain.
_DomainNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DomainNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DomainNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | @DBParameterGroupName@ doesn\'t refer to an existing DB parameter group.
_DBParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The DBSubnetGroup doesn\'t belong to the same VPC as that of an existing
-- cross-region read replica of the same source instance.
_InvalidDBSubnetGroupFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetGroupFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetGroupFault"
    Prelude.. Core.hasStatus 400

-- | Specified offering does not exist.
_ReservedDBInstancesOfferingNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstancesOfferingNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstancesOfferingNotFound"
    Prelude.. Core.hasStatus 404

-- | The DB subnet isn\'t in the /available/ state.
_InvalidDBSubnetStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetStateFault"
    Prelude.. Core.hasStatus 400

-- | @DBClusterSnapshotIdentifier@ doesn\'t refer to an existing DB cluster
-- snapshot.
_DBClusterSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | SNS has responded that there is a problem with the SND topic specified.
_SNSInvalidTopicFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError
    defaultService
    "SNSInvalidTopic"
    Prelude.. Core.hasStatus 400

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

-- | The specified installation medium has already been imported.
_InstallationMediaAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstallationMediaAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "InstallationMediaAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The supplied subscription name already exists.
_SubscriptionAlreadyExistFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionAlreadyExistFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionAlreadyExist"
    Prelude.. Core.hasStatus 400

-- | The specified IAM role Amazon Resource Name (ARN) is already associated
-- with the specified DB cluster.
_DBClusterRoleAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The IAM role requires additional permissions to export to an Amazon S3
-- bucket.
_IamRoleMissingPermissionsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IamRoleMissingPermissionsFault =
  Core._MatchServiceError
    defaultService
    "IamRoleMissingPermissions"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the maximum number of IAM roles that can be associated
-- with the specified DB cluster.
_DBClusterRoleQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The DB subnet group doesn\'t cover all Availability Zones after it\'s
-- created because of users\' change.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Core.hasStatus 400

-- | The specified @RoleArn@ value doesn\'t match the specified feature for
-- the DB instance.
_DBInstanceRoleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceRoleNotFound"
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

-- | The specified reserved DB Instance not found.
_ReservedDBInstanceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstanceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstanceNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed number of DB
-- subnet groups.
_DBSubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | @CustomAvailabilityZoneId@ doesn\'t refer to an existing custom
-- Availability Zone identifier.
_CustomAvailabilityZoneNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomAvailabilityZoneNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CustomAvailabilityZoneNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified DB proxy endpoint name must be unique for all DB proxy
-- endpoints owned by your Amazon Web Services account in the specified
-- Amazon Web Services Region.
_DBProxyEndpointAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyEndpointAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBProxyEndpointAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The global cluster is in an invalid state and can\'t perform the
-- requested operation.
_InvalidGlobalClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGlobalClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidGlobalClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | The DBSubnetGroup shouldn\'t be specified while creating read replicas
-- that lie in the same region as the source instance.
_DBSubnetGroupNotAllowedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupNotAllowedFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupNotAllowedFault"
    Prelude.. Core.hasStatus 400

-- | You can\'t cancel an export task that has completed.
_InvalidExportTaskStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidExportTaskStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidExportTaskStateFault"
    Prelude.. Core.hasStatus 400

-- | The state of the export snapshot is invalid for exporting to an Amazon
-- S3 bucket.
_InvalidExportSourceStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidExportSourceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidExportSourceState"
    Prelude.. Core.hasStatus 400

-- | You can\'t start an export task that\'s already running.
_ExportTaskAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExportTaskAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ExportTaskAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of event subscriptions.
_EventSubscriptionQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EventSubscriptionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "EventSubscriptionQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | There is insufficient storage available for the current action. You
-- might be able to resolve this error by updating your subnet group to use
-- different Availability Zones that have more storage available.
_InsufficientStorageClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientStorageClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientStorageClusterCapacity"
    Prelude.. Core.hasStatus 400

-- | The cluster already has the maximum number of custom endpoints.
_DBClusterEndpointQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointQuotaExceededFault"
    Prelude.. Core.hasStatus 403

-- | The option group isn\'t in the /available/ state.
_InvalidOptionGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOptionGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidOptionGroupStateFault"
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

-- | @CustomAvailabilityZoneName@ is already used by an existing custom
-- Availability Zone.
_CustomAvailabilityZoneAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomAvailabilityZoneAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CustomAvailabilityZoneAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The requested operation can\'t be performed while the cluster is in this
-- state.
_InvalidDBClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | The @GlobalClusterIdentifier@ doesn\'t refer to an existing global
-- database cluster.
_GlobalClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The user already has a DB instance with the given identifier.
_DBInstanceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Cannot restore from VPC backup to non-VPC DB instance.
_InvalidRestoreFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRestoreFault =
  Core._MatchServiceError
    defaultService
    "InvalidRestoreFault"
    Prelude.. Core.hasStatus 400

-- | The state of the DB security group doesn\'t allow deletion.
_InvalidDBSecurityGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSecurityGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSecurityGroupState"
    Prelude.. Core.hasStatus 400

-- | The specified resource ID was not found.
_ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | @DBSubnetGroupName@ doesn\'t refer to an existing DB subnet group.
_DBSubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The DB upgrade failed because a resource the DB depends on can\'t be
-- modified.
_DBUpgradeDependencyFailureFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBUpgradeDependencyFailureFault =
  Core._MatchServiceError
    defaultService
    "DBUpgradeDependencyFailure"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the maximum number of custom Availability Zones.
_CustomAvailabilityZoneQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CustomAvailabilityZoneQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CustomAvailabilityZoneQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The DB instance isn\'t in a valid state.
_InvalidDBInstanceStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBInstanceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBInstanceState"
    Prelude.. Core.hasStatus 400

-- | The specified custom endpoint can\'t be created because it already
-- exists.
_DBClusterEndpointAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | @DBSnapshotIdentifier@ is already used by an existing snapshot.
_DBSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | @DBInstanceIdentifier@ doesn\'t refer to an existing DB instance.
_DBInstanceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed amount of
-- storage available across all DB instances.
_StorageQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "StorageQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The specified proxy name must be unique for all proxies owned by your
-- Amazon Web Services account in the specified Amazon Web Services Region.
_DBProxyAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBProxyAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBProxyAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | No automated backup for this DB instance was found.
_DBInstanceAutomatedBackupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceAutomatedBackupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAutomatedBackupNotFound"
    Prelude.. Core.hasStatus 404

-- | The state of the DB snapshot doesn\'t allow deletion.
_InvalidDBSnapshotStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSnapshotState"
    Prelude.. Core.hasStatus 400

-- | The DB subnet group cannot be deleted because it\'s in use.
_InvalidDBSubnetGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetGroupStateFault"
    Prelude.. Core.hasStatus 400

-- | The number of global database clusters for this account is already at
-- the maximum allowed.
_GlobalClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The specified custom endpoint doesn\'t exist.
_DBClusterEndpointNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | @InstallationMediaID@ doesn\'t refer to an existing installation medium.
_InstallationMediaNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstallationMediaNotFoundFault =
  Core._MatchServiceError
    defaultService
    "InstallationMediaNotFound"
    Prelude.. Core.hasStatus 404

-- | Subnets in the DB subnet group should cover at least two Availability
-- Zones unless there is only one Availability Zone.
_DBSubnetGroupDoesNotCoverEnoughAZs :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupDoesNotCoverEnoughAZs"
    Prelude.. Core.hasStatus 400

-- | The DB subnet is already in use in the Availability Zone.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"
    Prelude.. Core.hasStatus 400

-- | The specified IAM role Amazon Resource Name (ARN) isn\'t associated with
-- the specified DB cluster.
_DBClusterRoleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleNotFound"
    Prelude.. Core.hasStatus 404
