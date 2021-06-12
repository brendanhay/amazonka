{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DBInstanceRoleAlreadyExistsFault,
    _ReservedDBInstanceAlreadyExistsFault,
    _DBParameterGroupQuotaExceededFault,
    _InsufficientDBClusterCapacityFault,
    _InvalidVPCNetworkStateFault,
    _ProvisionedIopsNotAvailableInAZFault,
    _DBClusterRoleQuotaExceededFault,
    _CertificateNotFoundFault,
    _DBClusterRoleAlreadyExistsFault,
    _InstallationMediaAlreadyExistsFault,
    _DBParameterGroupAlreadyExistsFault,
    _InsufficientDBInstanceCapacityFault,
    _SubscriptionAlreadyExistFault,
    _DBInstanceRoleQuotaExceededFault,
    _ReservedDBInstanceQuotaExceededFault,
    _ReservedDBInstancesOfferingNotFoundFault,
    _DBClusterSnapshotNotFoundFault,
    _DBInstanceAutomatedBackupNotFoundFault,
    _SNSNoAuthorizationFault,
    _InvalidDBSubnetGroupStateFault,
    _DBSubnetGroupAlreadyExistsFault,
    _DBClusterEndpointNotFoundFault,
    _DBSnapshotAlreadyExistsFault,
    _DBUpgradeDependencyFailureFault,
    _InvalidDBInstanceAutomatedBackupStateFault,
    _ResourceNotFoundFault,
    _SNSTopicArnNotFoundFault,
    _InvalidRestoreFault,
    _InvalidEventSubscriptionStateFault,
    _DBClusterEndpointAlreadyExistsFault,
    _InvalidDBInstanceStateFault,
    _DBClusterParameterGroupNotFoundFault,
    _KMSKeyNotAccessibleFault,
    _DBProxyNotFoundFault,
    _InvalidDBClusterEndpointStateFault,
    _SnapshotQuotaExceededFault,
    _InvalidExportOnlyFault,
    _InsufficientStorageClusterCapacityFault,
    _DBInstanceAutomatedBackupQuotaExceededFault,
    _StorageTypeNotSupportedFault,
    _IamRoleNotFoundFault,
    _EventSubscriptionQuotaExceededFault,
    _DBClusterEndpointQuotaExceededFault,
    _InvalidGlobalClusterStateFault,
    _DBProxyQuotaExceededFault,
    _DBSubnetGroupNotAllowedFault,
    _ExportTaskAlreadyExistsFault,
    _DBProxyTargetGroupNotFoundFault,
    _GlobalClusterAlreadyExistsFault,
    _DBProxyTargetNotFoundFault,
    _SharedSnapshotQuotaExceededFault,
    _ReservedDBInstanceNotFoundFault,
    _DBSubnetQuotaExceededFault,
    _DBInstanceRoleNotFoundFault,
    _BackupPolicyNotFoundFault,
    _IamRoleMissingPermissionsFault,
    _DBProxyTargetAlreadyRegisteredFault,
    _AuthorizationAlreadyExistsFault,
    _InvalidDBParameterGroupStateFault,
    _SNSInvalidTopicFault,
    _SourceNotFoundFault,
    _PointInTimeRestoreNotEnabledFault,
    _InvalidDBClusterSnapshotStateFault,
    _DBClusterSnapshotAlreadyExistsFault,
    _AuthorizationQuotaExceededFault,
    _InstallationMediaNotFoundFault,
    _DBParameterGroupNotFoundFault,
    _DBClusterRoleNotFoundFault,
    _DBSubnetGroupDoesNotCoverEnoughAZs,
    _InvalidDBSubnetStateFault,
    _DomainNotFoundFault,
    _InvalidDBSubnetGroupFault,
    _SubnetAlreadyInUse,
    _DBClusterBacktrackNotFoundFault,
    _DBProxyAlreadyExistsFault,
    _DBSecurityGroupNotSupportedFault,
    _StorageQuotaExceededFault,
    _InstanceQuotaExceededFault,
    _InvalidDBSnapshotStateFault,
    _DBInstanceNotFoundFault,
    _InvalidDBProxyStateFault,
    _GlobalClusterQuotaExceededFault,
    _DBSecurityGroupNotFoundFault,
    _DBSecurityGroupAlreadyExistsFault,
    _DBInstanceAlreadyExistsFault,
    _OptionGroupQuotaExceededFault,
    _InvalidDBSecurityGroupStateFault,
    _DBSnapshotNotFoundFault,
    _DBSubnetGroupNotFoundFault,
    _DBClusterQuotaExceededFault,
    _InvalidDBClusterCapacityFault,
    _CustomAvailabilityZoneQuotaExceededFault,
    _ExportTaskNotFoundFault,
    _InsufficientAvailableIPsInSubnetFault,
    _DBSecurityGroupQuotaExceededFault,
    _InvalidDBClusterStateFault,
    _OptionGroupAlreadyExistsFault,
    _CustomAvailabilityZoneAlreadyExistsFault,
    _DBClusterAlreadyExistsFault,
    _InvalidOptionGroupStateFault,
    _GlobalClusterNotFoundFault,
    _InvalidS3BucketFault,
    _InvalidExportSourceStateFault,
    _DBClusterNotFoundFault,
    _CustomAvailabilityZoneNotFoundFault,
    _DBSubnetGroupQuotaExceededFault,
    _OptionGroupNotFoundFault,
    _DBLogFileNotFoundFault,
    _InvalidExportTaskStateFault,
    _SubscriptionCategoryNotFoundFault,
    _AuthorizationNotFoundFault,
    _InvalidSubnet,
    _SubscriptionNotFoundFault,

    -- * ActivityStreamMode
    ActivityStreamMode (..),

    -- * ActivityStreamStatus
    ActivityStreamStatus (..),

    -- * ApplyMethod
    ApplyMethod (..),

    -- * AuthScheme
    AuthScheme (..),

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

    -- * TargetState
    TargetState (..),

    -- * TargetType
    TargetType (..),

    -- * WriteForwardingStatus
    WriteForwardingStatus (..),

    -- * AccountQuota
    AccountQuota (..),
    newAccountQuota,
    accountQuota_used,
    accountQuota_accountQuotaName,
    accountQuota_max,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,

    -- * AvailableProcessorFeature
    AvailableProcessorFeature (..),
    newAvailableProcessorFeature,
    availableProcessorFeature_allowedValues,
    availableProcessorFeature_name,
    availableProcessorFeature_defaultValue,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_certificateIdentifier,
    certificate_validFrom,
    certificate_customerOverride,
    certificate_certificateArn,
    certificate_certificateType,
    certificate_thumbprint,
    certificate_customerOverrideValidTill,
    certificate_validTill,

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
    clusterPendingModifiedValues_masterUserPassword,
    clusterPendingModifiedValues_pendingCloudwatchLogsExports,
    clusterPendingModifiedValues_dbClusterIdentifier,
    clusterPendingModifiedValues_engineVersion,
    clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled,

    -- * ConnectionPoolConfiguration
    ConnectionPoolConfiguration (..),
    newConnectionPoolConfiguration,
    connectionPoolConfiguration_sessionPinningFilters,
    connectionPoolConfiguration_maxIdleConnectionsPercent,
    connectionPoolConfiguration_connectionBorrowTimeout,
    connectionPoolConfiguration_initQuery,
    connectionPoolConfiguration_maxConnectionsPercent,

    -- * ConnectionPoolConfigurationInfo
    ConnectionPoolConfigurationInfo (..),
    newConnectionPoolConfigurationInfo,
    connectionPoolConfigurationInfo_sessionPinningFilters,
    connectionPoolConfigurationInfo_maxIdleConnectionsPercent,
    connectionPoolConfigurationInfo_connectionBorrowTimeout,
    connectionPoolConfigurationInfo_initQuery,
    connectionPoolConfigurationInfo_maxConnectionsPercent,

    -- * CustomAvailabilityZone
    CustomAvailabilityZone (..),
    newCustomAvailabilityZone,
    customAvailabilityZone_customAvailabilityZoneId,
    customAvailabilityZone_customAvailabilityZoneName,
    customAvailabilityZone_vpnDetails,
    customAvailabilityZone_customAvailabilityZoneStatus,

    -- * DBCluster
    DBCluster (..),
    newDBCluster,
    dbCluster_backupRetentionPeriod,
    dbCluster_deletionProtection,
    dbCluster_storageEncrypted,
    dbCluster_associatedRoles,
    dbCluster_vpcSecurityGroups,
    dbCluster_preferredBackupWindow,
    dbCluster_dbClusterMembers,
    dbCluster_status,
    dbCluster_backtrackConsumedChangeRecords,
    dbCluster_crossAccountClone,
    dbCluster_availabilityZones,
    dbCluster_globalWriteForwardingStatus,
    dbCluster_dbClusterOptionGroupMemberships,
    dbCluster_cloneGroupId,
    dbCluster_latestRestorableTime,
    dbCluster_dbClusterArn,
    dbCluster_domainMemberships,
    dbCluster_activityStreamKmsKeyId,
    dbCluster_readerEndpoint,
    dbCluster_httpEndpointEnabled,
    dbCluster_clusterCreateTime,
    dbCluster_earliestRestorableTime,
    dbCluster_engineMode,
    dbCluster_activityStreamStatus,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_hostedZoneId,
    dbCluster_dbSubnetGroup,
    dbCluster_readReplicaIdentifiers,
    dbCluster_masterUsername,
    dbCluster_multiAZ,
    dbCluster_kmsKeyId,
    dbCluster_dbClusterIdentifier,
    dbCluster_capacity,
    dbCluster_engineVersion,
    dbCluster_preferredMaintenanceWindow,
    dbCluster_characterSetName,
    dbCluster_port,
    dbCluster_percentProgress,
    dbCluster_engine,
    dbCluster_pendingModifiedValues,
    dbCluster_dbClusterResourceId,
    dbCluster_copyTagsToSnapshot,
    dbCluster_customEndpoints,
    dbCluster_endpoint,
    dbCluster_scalingConfigurationInfo,
    dbCluster_earliestBacktrackTime,
    dbCluster_tagList,
    dbCluster_allocatedStorage,
    dbCluster_backtrackWindow,
    dbCluster_iAMDatabaseAuthenticationEnabled,
    dbCluster_globalWriteForwardingRequested,
    dbCluster_dbClusterParameterGroup,
    dbCluster_replicationSourceIdentifier,
    dbCluster_databaseName,
    dbCluster_activityStreamKinesisStreamName,
    dbCluster_activityStreamMode,

    -- * DBClusterBacktrack
    DBClusterBacktrack (..),
    newDBClusterBacktrack,
    dbClusterBacktrack_status,
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_backtrackTo,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_backtrackRequestCreationTime,
    dbClusterBacktrack_backtrackedFrom,

    -- * DBClusterEndpoint
    DBClusterEndpoint (..),
    newDBClusterEndpoint,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_status,
    dbClusterEndpoint_excludedMembers,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_endpoint,
    dbClusterEndpoint_staticMembers,

    -- * DBClusterMember
    DBClusterMember (..),
    newDBClusterMember,
    dbClusterMember_isClusterWriter,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_promotionTier,
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
    dbClusterParameterGroup_description,
    dbClusterParameterGroup_dbClusterParameterGroupName,

    -- * DBClusterParameterGroupNameMessage
    DBClusterParameterGroupNameMessage (..),
    newDBClusterParameterGroupNameMessage,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- * DBClusterRole
    DBClusterRole (..),
    newDBClusterRole,
    dbClusterRole_status,
    dbClusterRole_roleArn,
    dbClusterRole_featureName,

    -- * DBClusterSnapshot
    DBClusterSnapshot (..),
    newDBClusterSnapshot,
    dbClusterSnapshot_storageEncrypted,
    dbClusterSnapshot_status,
    dbClusterSnapshot_availabilityZones,
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
    dbClusterSnapshot_iAMDatabaseAuthenticationEnabled,
    dbClusterSnapshot_dbClusterSnapshotArn,

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
    dbEngineVersion_status,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_validUpgradeTarget,
    dbEngineVersion_engineVersion,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_engine,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_supportsParallelQuery,

    -- * DBInstance
    DBInstance (..),
    newDBInstance,
    dbInstance_backupRetentionPeriod,
    dbInstance_deletionProtection,
    dbInstance_storageEncrypted,
    dbInstance_associatedRoles,
    dbInstance_vpcSecurityGroups,
    dbInstance_preferredBackupWindow,
    dbInstance_cACertificateIdentifier,
    dbInstance_dbSecurityGroups,
    dbInstance_maxAllocatedStorage,
    dbInstance_statusInfos,
    dbInstance_storageType,
    dbInstance_latestRestorableTime,
    dbInstance_domainMemberships,
    dbInstance_dbInstancePort,
    dbInstance_monitoringInterval,
    dbInstance_dbParameterGroups,
    dbInstance_instanceCreateTime,
    dbInstance_dbiResourceId,
    dbInstance_optionGroupMemberships,
    dbInstance_listenerEndpoint,
    dbInstance_monitoringRoleArn,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_dbSubnetGroup,
    dbInstance_ncharCharacterSetName,
    dbInstance_masterUsername,
    dbInstance_multiAZ,
    dbInstance_publiclyAccessible,
    dbInstance_performanceInsightsKMSKeyId,
    dbInstance_kmsKeyId,
    dbInstance_availabilityZone,
    dbInstance_dbClusterIdentifier,
    dbInstance_customerOwnedIpEnabled,
    dbInstance_engineVersion,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_characterSetName,
    dbInstance_performanceInsightsRetentionPeriod,
    dbInstance_licenseModel,
    dbInstance_dbInstanceIdentifier,
    dbInstance_promotionTier,
    dbInstance_processorFeatures,
    dbInstance_awsBackupRecoveryPointArn,
    dbInstance_dbInstanceClass,
    dbInstance_dbName,
    dbInstance_engine,
    dbInstance_readReplicaDBClusterIdentifiers,
    dbInstance_pendingModifiedValues,
    dbInstance_tdeCredentialArn,
    dbInstance_timezone,
    dbInstance_dbInstanceStatus,
    dbInstance_dbInstanceAutomatedBackupsReplications,
    dbInstance_copyTagsToSnapshot,
    dbInstance_readReplicaSourceDBInstanceIdentifier,
    dbInstance_replicaMode,
    dbInstance_endpoint,
    dbInstance_tagList,
    dbInstance_performanceInsightsEnabled,
    dbInstance_allocatedStorage,
    dbInstance_iAMDatabaseAuthenticationEnabled,
    dbInstance_secondaryAvailabilityZone,
    dbInstance_iops,
    dbInstance_enhancedMonitoringResourceArn,
    dbInstance_readReplicaDBInstanceIdentifiers,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_dbInstanceArn,

    -- * DBInstanceAutomatedBackup
    DBInstanceAutomatedBackup (..),
    newDBInstanceAutomatedBackup,
    dbInstanceAutomatedBackup_backupRetentionPeriod,
    dbInstanceAutomatedBackup_status,
    dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn,
    dbInstanceAutomatedBackup_storageType,
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
    dbInstanceAutomatedBackup_tdeCredentialArn,
    dbInstanceAutomatedBackup_timezone,
    dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications,
    dbInstanceAutomatedBackup_region,
    dbInstanceAutomatedBackup_vpcId,
    dbInstanceAutomatedBackup_allocatedStorage,
    dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled,
    dbInstanceAutomatedBackup_iops,
    dbInstanceAutomatedBackup_dbInstanceArn,

    -- * DBInstanceAutomatedBackupsReplication
    DBInstanceAutomatedBackupsReplication (..),
    newDBInstanceAutomatedBackupsReplication,
    dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn,

    -- * DBInstanceRole
    DBInstanceRole (..),
    newDBInstanceRole,
    dbInstanceRole_status,
    dbInstanceRole_roleArn,
    dbInstanceRole_featureName,

    -- * DBInstanceStatusInfo
    DBInstanceStatusInfo (..),
    newDBInstanceStatusInfo,
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_message,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_statusType,

    -- * DBParameterGroup
    DBParameterGroup (..),
    newDBParameterGroup,
    dbParameterGroup_dbParameterGroupArn,
    dbParameterGroup_dbParameterGroupName,
    dbParameterGroup_dbParameterGroupFamily,
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
    dbProxy_dbProxyArn,
    dbProxy_status,
    dbProxy_createdDate,
    dbProxy_roleArn,
    dbProxy_idleClientTimeout,
    dbProxy_vpcSubnetIds,
    dbProxy_auth,
    dbProxy_engineFamily,
    dbProxy_vpcSecurityGroupIds,
    dbProxy_debugLogging,
    dbProxy_updatedDate,
    dbProxy_requireTLS,
    dbProxy_endpoint,
    dbProxy_dbProxyName,

    -- * DBProxyTarget
    DBProxyTarget (..),
    newDBProxyTarget,
    dbProxyTarget_trackedClusterId,
    dbProxyTarget_rdsResourceId,
    dbProxyTarget_targetArn,
    dbProxyTarget_port,
    dbProxyTarget_endpoint,
    dbProxyTarget_type,
    dbProxyTarget_targetHealth,

    -- * DBProxyTargetGroup
    DBProxyTargetGroup (..),
    newDBProxyTargetGroup,
    dbProxyTargetGroup_status,
    dbProxyTargetGroup_createdDate,
    dbProxyTargetGroup_isDefault,
    dbProxyTargetGroup_targetGroupName,
    dbProxyTargetGroup_targetGroupArn,
    dbProxyTargetGroup_connectionPoolConfig,
    dbProxyTargetGroup_updatedDate,
    dbProxyTargetGroup_dbProxyName,

    -- * DBSecurityGroup
    DBSecurityGroup (..),
    newDBSecurityGroup,
    dbSecurityGroup_ownerId,
    dbSecurityGroup_dbSecurityGroupName,
    dbSecurityGroup_iPRanges,
    dbSecurityGroup_dbSecurityGroupDescription,
    dbSecurityGroup_eC2SecurityGroups,
    dbSecurityGroup_vpcId,
    dbSecurityGroup_dbSecurityGroupArn,

    -- * DBSecurityGroupMembership
    DBSecurityGroupMembership (..),
    newDBSecurityGroupMembership,
    dbSecurityGroupMembership_status,
    dbSecurityGroupMembership_dbSecurityGroupName,

    -- * DBSnapshot
    DBSnapshot (..),
    newDBSnapshot,
    dbSnapshot_status,
    dbSnapshot_storageType,
    dbSnapshot_dbSnapshotIdentifier,
    dbSnapshot_encrypted,
    dbSnapshot_optionGroupName,
    dbSnapshot_instanceCreateTime,
    dbSnapshot_dbiResourceId,
    dbSnapshot_snapshotCreateTime,
    dbSnapshot_masterUsername,
    dbSnapshot_kmsKeyId,
    dbSnapshot_availabilityZone,
    dbSnapshot_dbSnapshotArn,
    dbSnapshot_engineVersion,
    dbSnapshot_snapshotType,
    dbSnapshot_sourceDBSnapshotIdentifier,
    dbSnapshot_licenseModel,
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
    dbSnapshot_sourceRegion,
    dbSnapshot_iAMDatabaseAuthenticationEnabled,
    dbSnapshot_iops,

    -- * DBSnapshotAttribute
    DBSnapshotAttribute (..),
    newDBSnapshotAttribute,
    dbSnapshotAttribute_attributeName,
    dbSnapshotAttribute_attributeValues,

    -- * DBSnapshotAttributesResult
    DBSnapshotAttributesResult (..),
    newDBSnapshotAttributesResult,
    dbSnapshotAttributesResult_dbSnapshotIdentifier,
    dbSnapshotAttributesResult_dbSnapshotAttributes,

    -- * DBSubnetGroup
    DBSubnetGroup (..),
    newDBSubnetGroup,
    dbSubnetGroup_subnetGroupStatus,
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_subnets,
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
    domainMembership_status,
    domainMembership_domain,
    domainMembership_iAMRoleName,
    domainMembership_fqdn,

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
    eC2SecurityGroup_eC2SecurityGroupId,
    eC2SecurityGroup_eC2SecurityGroupName,

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
    engineDefaults_parameters,
    engineDefaults_marker,

    -- * Event
    Event (..),
    newEvent,
    event_message,
    event_eventCategories,
    event_date,
    event_sourceIdentifier,
    event_sourceArn,
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
    eventSubscription_status,
    eventSubscription_sourceIdsList,
    eventSubscription_eventCategoriesList,
    eventSubscription_enabled,
    eventSubscription_eventSubscriptionArn,
    eventSubscription_subscriptionCreationTime,
    eventSubscription_customerAwsId,
    eventSubscription_sourceType,
    eventSubscription_snsTopicArn,

    -- * ExportTask
    ExportTask (..),
    newExportTask,
    exportTask_taskEndTime,
    exportTask_iamRoleArn,
    exportTask_status,
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
    globalCluster_deletionProtection,
    globalCluster_storageEncrypted,
    globalCluster_status,
    globalCluster_failoverState,
    globalCluster_engineVersion,
    globalCluster_globalClusterArn,
    globalCluster_engine,
    globalCluster_globalClusterResourceId,
    globalCluster_globalClusterMembers,
    globalCluster_globalClusterIdentifier,
    globalCluster_databaseName,

    -- * GlobalClusterMember
    GlobalClusterMember (..),
    newGlobalClusterMember,
    globalClusterMember_globalWriteForwardingStatus,
    globalClusterMember_dbClusterArn,
    globalClusterMember_readers,
    globalClusterMember_isWriter,

    -- * IPRange
    IPRange (..),
    newIPRange,
    iPRange_status,
    iPRange_cidrip,

    -- * InstallationMedia
    InstallationMedia (..),
    newInstallationMedia,
    installationMedia_status,
    installationMedia_customAvailabilityZoneId,
    installationMedia_installationMediaId,
    installationMedia_engineVersion,
    installationMedia_oSInstallationMediaPath,
    installationMedia_failureCause,
    installationMedia_engine,
    installationMedia_engineInstallationMediaPath,

    -- * InstallationMediaFailureCause
    InstallationMediaFailureCause (..),
    newInstallationMediaFailureCause,
    installationMediaFailureCause_message,

    -- * MinimumEngineVersionPerAllowedValue
    MinimumEngineVersionPerAllowedValue (..),
    newMinimumEngineVersionPerAllowedValue,
    minimumEngineVersionPerAllowedValue_allowedValue,
    minimumEngineVersionPerAllowedValue_minimumEngineVersion,

    -- * Option
    Option (..),
    newOption,
    option_optionName,
    option_optionVersion,
    option_dbSecurityGroupMemberships,
    option_optionDescription,
    option_port,
    option_optionSettings,
    option_persistent,
    option_vpcSecurityGroupMemberships,
    option_permanent,

    -- * OptionConfiguration
    OptionConfiguration (..),
    newOptionConfiguration,
    optionConfiguration_optionVersion,
    optionConfiguration_dbSecurityGroupMemberships,
    optionConfiguration_port,
    optionConfiguration_optionSettings,
    optionConfiguration_vpcSecurityGroupMemberships,
    optionConfiguration_optionName,

    -- * OptionGroup
    OptionGroup (..),
    newOptionGroup,
    optionGroup_engineName,
    optionGroup_optionGroupArn,
    optionGroup_allowsVpcAndNonVpcInstanceMemberships,
    optionGroup_optionGroupName,
    optionGroup_options,
    optionGroup_optionGroupDescription,
    optionGroup_majorEngineVersion,
    optionGroup_vpcId,

    -- * OptionGroupMembership
    OptionGroupMembership (..),
    newOptionGroupMembership,
    optionGroupMembership_status,
    optionGroupMembership_optionGroupName,

    -- * OptionGroupOption
    OptionGroupOption (..),
    newOptionGroupOption,
    optionGroupOption_optionsConflictsWith,
    optionGroupOption_vpcOnly,
    optionGroupOption_engineName,
    optionGroupOption_optionGroupOptionVersions,
    optionGroupOption_defaultPort,
    optionGroupOption_requiresAutoMinorEngineVersionUpgrade,
    optionGroupOption_optionGroupOptionSettings,
    optionGroupOption_majorEngineVersion,
    optionGroupOption_name,
    optionGroupOption_minimumRequiredMinorEngineVersion,
    optionGroupOption_optionsDependedOn,
    optionGroupOption_description,
    optionGroupOption_portRequired,
    optionGroupOption_persistent,
    optionGroupOption_permanent,
    optionGroupOption_supportsOptionVersionDowngrade,

    -- * OptionGroupOptionSetting
    OptionGroupOptionSetting (..),
    newOptionGroupOptionSetting,
    optionGroupOptionSetting_minimumEngineVersionPerAllowedValue,
    optionGroupOptionSetting_allowedValues,
    optionGroupOptionSetting_applyType,
    optionGroupOptionSetting_settingName,
    optionGroupOptionSetting_isRequired,
    optionGroupOptionSetting_settingDescription,
    optionGroupOptionSetting_isModifiable,
    optionGroupOptionSetting_defaultValue,

    -- * OptionSetting
    OptionSetting (..),
    newOptionSetting,
    optionSetting_isCollection,
    optionSetting_allowedValues,
    optionSetting_name,
    optionSetting_applyType,
    optionSetting_description,
    optionSetting_value,
    optionSetting_dataType,
    optionSetting_isModifiable,
    optionSetting_defaultValue,

    -- * OptionVersion
    OptionVersion (..),
    newOptionVersion,
    optionVersion_isDefault,
    optionVersion_version,

    -- * OrderableDBInstanceOption
    OrderableDBInstanceOption (..),
    newOrderableDBInstanceOption,
    orderableDBInstanceOption_minIopsPerDbInstance,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_supportsKerberosAuthentication,
    orderableDBInstanceOption_supportsIops,
    orderableDBInstanceOption_storageType,
    orderableDBInstanceOption_supportsEnhancedMonitoring,
    orderableDBInstanceOption_availableProcessorFeatures,
    orderableDBInstanceOption_maxStorageSize,
    orderableDBInstanceOption_supportedEngineModes,
    orderableDBInstanceOption_maxIopsPerGib,
    orderableDBInstanceOption_supportsStorageEncryption,
    orderableDBInstanceOption_multiAZCapable,
    orderableDBInstanceOption_supportsStorageAutoscaling,
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_minIopsPerGib,
    orderableDBInstanceOption_supportsIAMDatabaseAuthentication,
    orderableDBInstanceOption_licenseModel,
    orderableDBInstanceOption_supportsGlobalDatabases,
    orderableDBInstanceOption_supportsPerformanceInsights,
    orderableDBInstanceOption_maxIopsPerDbInstance,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_outpostCapable,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_minStorageSize,
    orderableDBInstanceOption_availabilityZoneGroup,
    orderableDBInstanceOption_vpc,
    orderableDBInstanceOption_readReplicaCapable,

    -- * Outpost
    Outpost (..),
    newOutpost,
    outpost_arn,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_allowedValues,
    parameter_supportedEngineModes,
    parameter_source,
    parameter_parameterValue,
    parameter_applyType,
    parameter_parameterName,
    parameter_description,
    parameter_applyMethod,
    parameter_dataType,
    parameter_isModifiable,
    parameter_minimumEngineVersion,

    -- * PendingCloudwatchLogsExports
    PendingCloudwatchLogsExports (..),
    newPendingCloudwatchLogsExports,
    pendingCloudwatchLogsExports_logTypesToDisable,
    pendingCloudwatchLogsExports_logTypesToEnable,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_description,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    newPendingModifiedValues,
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
    pendingModifiedValues_port,
    pendingModifiedValues_dbInstanceClass,
    pendingModifiedValues_allocatedStorage,
    pendingModifiedValues_iAMDatabaseAuthenticationEnabled,
    pendingModifiedValues_iops,

    -- * ProcessorFeature
    ProcessorFeature (..),
    newProcessorFeature,
    processorFeature_name,
    processorFeature_value,

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

    -- * ReservedDBInstancesOffering
    ReservedDBInstancesOffering (..),
    newReservedDBInstancesOffering,
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
    scalingConfiguration_maxCapacity,
    scalingConfiguration_autoPause,
    scalingConfiguration_timeoutAction,
    scalingConfiguration_secondsUntilAutoPause,
    scalingConfiguration_minCapacity,

    -- * ScalingConfigurationInfo
    ScalingConfigurationInfo (..),
    newScalingConfigurationInfo,
    scalingConfigurationInfo_maxCapacity,
    scalingConfigurationInfo_autoPause,
    scalingConfigurationInfo_timeoutAction,
    scalingConfigurationInfo_secondsUntilAutoPause,
    scalingConfigurationInfo_minCapacity,

    -- * SourceRegion
    SourceRegion (..),
    newSourceRegion,
    sourceRegion_regionName,
    sourceRegion_status,
    sourceRegion_supportsDBInstanceAutomatedBackupsReplication,
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
    tag_key,
    tag_value,

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
    upgradeTarget_autoUpgrade,
    upgradeTarget_supportedEngineModes,
    upgradeTarget_engineVersion,
    upgradeTarget_supportsGlobalDatabases,
    upgradeTarget_engine,
    upgradeTarget_description,
    upgradeTarget_isMajorVersionUpgrade,
    upgradeTarget_supportsParallelQuery,

    -- * UserAuthConfig
    UserAuthConfig (..),
    newUserAuthConfig,
    userAuthConfig_secretArn,
    userAuthConfig_iAMAuth,
    userAuthConfig_authScheme,
    userAuthConfig_userName,
    userAuthConfig_description,

    -- * UserAuthConfigInfo
    UserAuthConfigInfo (..),
    newUserAuthConfigInfo,
    userAuthConfigInfo_secretArn,
    userAuthConfigInfo_iAMAuth,
    userAuthConfigInfo_authScheme,
    userAuthConfigInfo_userName,
    userAuthConfigInfo_description,

    -- * ValidDBInstanceModificationsMessage
    ValidDBInstanceModificationsMessage (..),
    newValidDBInstanceModificationsMessage,
    validDBInstanceModificationsMessage_validProcessorFeatures,
    validDBInstanceModificationsMessage_storage,

    -- * ValidStorageOptions
    ValidStorageOptions (..),
    newValidStorageOptions,
    validStorageOptions_storageType,
    validStorageOptions_provisionedIops,
    validStorageOptions_supportsStorageAutoscaling,
    validStorageOptions_storageSize,
    validStorageOptions_iopsToStorageRatio,

    -- * VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    newVpcSecurityGroupMembership,
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,

    -- * VpnDetails
    VpnDetails (..),
    newVpnDetails,
    vpnDetails_vpnTunnelOriginatorIP,
    vpnDetails_vpnId,
    vpnDetails_vpnName,
    vpnDetails_vpnState,
    vpnDetails_vpnGatewayIp,
    vpnDetails_vpnPSK,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.AccountQuota
import Network.AWS.RDS.Types.ActivityStreamMode
import Network.AWS.RDS.Types.ActivityStreamStatus
import Network.AWS.RDS.Types.ApplyMethod
import Network.AWS.RDS.Types.AuthScheme
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
import Network.AWS.RDS.Types.DBProxyStatus
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
import Network.AWS.RDS.Types.EngineFamily
import Network.AWS.RDS.Types.Event
import Network.AWS.RDS.Types.EventCategoriesMap
import Network.AWS.RDS.Types.EventSubscription
import Network.AWS.RDS.Types.ExportTask
import Network.AWS.RDS.Types.FailoverState
import Network.AWS.RDS.Types.FailoverStatus
import Network.AWS.RDS.Types.Filter
import Network.AWS.RDS.Types.GlobalCluster
import Network.AWS.RDS.Types.GlobalClusterMember
import Network.AWS.RDS.Types.IAMAuthMode
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
import Network.AWS.RDS.Types.ReplicaMode
import Network.AWS.RDS.Types.ReservedDBInstance
import Network.AWS.RDS.Types.ReservedDBInstancesOffering
import Network.AWS.RDS.Types.ResourcePendingMaintenanceActions
import Network.AWS.RDS.Types.RestoreWindow
import Network.AWS.RDS.Types.ScalingConfiguration
import Network.AWS.RDS.Types.ScalingConfigurationInfo
import Network.AWS.RDS.Types.SourceRegion
import Network.AWS.RDS.Types.SourceType
import Network.AWS.RDS.Types.Subnet
import Network.AWS.RDS.Types.Tag
import Network.AWS.RDS.Types.TargetHealth
import Network.AWS.RDS.Types.TargetHealthReason
import Network.AWS.RDS.Types.TargetState
import Network.AWS.RDS.Types.TargetType
import Network.AWS.RDS.Types.Timezone
import Network.AWS.RDS.Types.UpgradeTarget
import Network.AWS.RDS.Types.UserAuthConfig
import Network.AWS.RDS.Types.UserAuthConfigInfo
import Network.AWS.RDS.Types.ValidDBInstanceModificationsMessage
import Network.AWS.RDS.Types.ValidStorageOptions
import Network.AWS.RDS.Types.VpcSecurityGroupMembership
import Network.AWS.RDS.Types.VpnDetails
import Network.AWS.RDS.Types.WriteForwardingStatus
import qualified Network.AWS.Sign.V4 as Sign

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
      Core._serviceTimeout = Core.Just 70,
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The specified @RoleArn@ or @FeatureName@ value is already associated
-- with the DB instance.
_DBInstanceRoleAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceRoleAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceRoleAlreadyExists"
    Core.. Core.hasStatus 400

-- | User already has a reservation with the given identifier.
_ReservedDBInstanceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstanceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstanceAlreadyExists"
    Core.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed number of DB
-- parameter groups.
_DBParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupQuotaExceeded"
    Core.. Core.hasStatus 400

-- | The DB cluster doesn\'t have enough capacity for the current operation.
_InsufficientDBClusterCapacityFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientDBClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBClusterCapacityFault"
    Core.. Core.hasStatus 403

-- | The DB subnet group doesn\'t cover all Availability Zones after it\'s
-- created because of users\' change.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Core.. Core.hasStatus 400

-- | Provisioned IOPS not available in the specified Availability Zone.
_ProvisionedIopsNotAvailableInAZFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProvisionedIopsNotAvailableInAZFault =
  Core._MatchServiceError
    defaultService
    "ProvisionedIopsNotAvailableInAZFault"
    Core.. Core.hasStatus 400

-- | You have exceeded the maximum number of IAM roles that can be associated
-- with the specified DB cluster.
_DBClusterRoleQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleQuotaExceeded"
    Core.. Core.hasStatus 400

-- | @CertificateIdentifier@ doesn\'t refer to an existing certificate.
_CertificateNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Core.. Core.hasStatus 404

-- | The specified IAM role Amazon Resource Name (ARN) is already associated
-- with the specified DB cluster.
_DBClusterRoleAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleAlreadyExists"
    Core.. Core.hasStatus 400

-- | The specified installation medium has already been imported.
_InstallationMediaAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstallationMediaAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "InstallationMediaAlreadyExists"
    Core.. Core.hasStatus 400

-- | A DB parameter group with the same name exists.
_DBParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupAlreadyExists"
    Core.. Core.hasStatus 400

-- | The specified DB instance class isn\'t available in the specified
-- Availability Zone.
_InsufficientDBInstanceCapacityFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientDBInstanceCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBInstanceCapacity"
    Core.. Core.hasStatus 400

-- | The supplied subscription name already exists.
_SubscriptionAlreadyExistFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubscriptionAlreadyExistFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionAlreadyExist"
    Core.. Core.hasStatus 400

-- | You can\'t associate any more AWS Identity and Access Management (IAM)
-- roles with the DB instance because the quota has been reached.
_DBInstanceRoleQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceRoleQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceRoleQuotaExceeded"
    Core.. Core.hasStatus 400

-- | Request would exceed the user\'s DB Instance quota.
_ReservedDBInstanceQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstanceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstanceQuotaExceeded"
    Core.. Core.hasStatus 400

-- | Specified offering does not exist.
_ReservedDBInstancesOfferingNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstancesOfferingNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstancesOfferingNotFound"
    Core.. Core.hasStatus 404

-- | @DBClusterSnapshotIdentifier@ doesn\'t refer to an existing DB cluster
-- snapshot.
_DBClusterSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotNotFoundFault"
    Core.. Core.hasStatus 404

-- | No automated backup for this DB instance was found.
_DBInstanceAutomatedBackupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceAutomatedBackupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAutomatedBackupNotFound"
    Core.. Core.hasStatus 404

-- | You do not have permission to publish to the SNS topic ARN.
_SNSNoAuthorizationFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorization"
    Core.. Core.hasStatus 400

-- | The DB subnet group cannot be deleted because it\'s in use.
_InvalidDBSubnetGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetGroupStateFault"
    Core.. Core.hasStatus 400

-- | @DBSubnetGroupName@ is already used by an existing DB subnet group.
_DBSubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupAlreadyExists"
    Core.. Core.hasStatus 400

-- | The specified custom endpoint doesn\'t exist.
_DBClusterEndpointNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointNotFoundFault"
    Core.. Core.hasStatus 400

-- | @DBSnapshotIdentifier@ is already used by an existing snapshot.
_DBSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotAlreadyExists"
    Core.. Core.hasStatus 400

-- | The DB upgrade failed because a resource the DB depends on can\'t be
-- modified.
_DBUpgradeDependencyFailureFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBUpgradeDependencyFailureFault =
  Core._MatchServiceError
    defaultService
    "DBUpgradeDependencyFailure"
    Core.. Core.hasStatus 400

-- | The automated backup is in an invalid state. For example, this automated
-- backup is associated with an active instance.
_InvalidDBInstanceAutomatedBackupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBInstanceAutomatedBackupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBInstanceAutomatedBackupState"
    Core.. Core.hasStatus 400

-- | The specified resource ID was not found.
_ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundFault"
    Core.. Core.hasStatus 404

-- | The SNS topic ARN does not exist.
_SNSTopicArnNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SNSTopicArnNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SNSTopicArnNotFound"
    Core.. Core.hasStatus 404

-- | Cannot restore from VPC backup to non-VPC DB instance.
_InvalidRestoreFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRestoreFault =
  Core._MatchServiceError
    defaultService
    "InvalidRestoreFault"
    Core.. Core.hasStatus 400

-- | This error can occur if someone else is modifying a subscription. You
-- should retry the action.
_InvalidEventSubscriptionStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEventSubscriptionStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidEventSubscriptionState"
    Core.. Core.hasStatus 400

-- | The specified custom endpoint can\'t be created because it already
-- exists.
_DBClusterEndpointAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointAlreadyExistsFault"
    Core.. Core.hasStatus 400

-- | The DB instance isn\'t in a valid state.
_InvalidDBInstanceStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBInstanceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBInstanceState"
    Core.. Core.hasStatus 400

-- | @DBClusterParameterGroupName@ doesn\'t refer to an existing DB cluster
-- parameter group.
_DBClusterParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterParameterGroupNotFound"
    Core.. Core.hasStatus 404

-- | An error occurred accessing an AWS KMS key.
_KMSKeyNotAccessibleFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSKeyNotAccessibleFault =
  Core._MatchServiceError
    defaultService
    "KMSKeyNotAccessibleFault"
    Core.. Core.hasStatus 400

-- | The specified proxy name doesn\'t correspond to a proxy owned by your
-- AWS account in the specified AWS Region.
_DBProxyNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBProxyNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyNotFoundFault"
    Core.. Core.hasStatus 404

-- | The requested operation can\'t be performed on the endpoint while the
-- endpoint is in this state.
_InvalidDBClusterEndpointStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterEndpointStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterEndpointStateFault"
    Core.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- snapshots.
_SnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceeded"
    Core.. Core.hasStatus 400

-- | The export is invalid for exporting to an Amazon S3 bucket.
_InvalidExportOnlyFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidExportOnlyFault =
  Core._MatchServiceError
    defaultService
    "InvalidExportOnly"
    Core.. Core.hasStatus 400

-- | There is insufficient storage available for the current action. You
-- might be able to resolve this error by updating your subnet group to use
-- different Availability Zones that have more storage available.
_InsufficientStorageClusterCapacityFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientStorageClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientStorageClusterCapacity"
    Core.. Core.hasStatus 400

-- | The quota for retained automated backups was exceeded. This prevents you
-- from retaining any additional automated backups. The retained automated
-- backups quota is the same as your DB Instance quota.
_DBInstanceAutomatedBackupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceAutomatedBackupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAutomatedBackupQuotaExceeded"
    Core.. Core.hasStatus 400

-- | Storage of the @StorageType@ specified can\'t be associated with the DB
-- instance.
_StorageTypeNotSupportedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StorageTypeNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "StorageTypeNotSupported"
    Core.. Core.hasStatus 400

-- | The IAM role is missing for exporting to an Amazon S3 bucket.
_IamRoleNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IamRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "IamRoleNotFound"
    Core.. Core.hasStatus 404

-- | You have reached the maximum number of event subscriptions.
_EventSubscriptionQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EventSubscriptionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "EventSubscriptionQuotaExceeded"
    Core.. Core.hasStatus 400

-- | The cluster already has the maximum number of custom endpoints.
_DBClusterEndpointQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointQuotaExceededFault"
    Core.. Core.hasStatus 403

-- | The global cluster is in an invalid state and can\'t perform the
-- requested operation.
_InvalidGlobalClusterStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidGlobalClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidGlobalClusterStateFault"
    Core.. Core.hasStatus 400

-- | Your AWS account already has the maximum number of proxies in the
-- specified AWS Region.
_DBProxyQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBProxyQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBProxyQuotaExceededFault"
    Core.. Core.hasStatus 400

-- | The DBSubnetGroup shouldn\'t be specified while creating read replicas
-- that lie in the same region as the source instance.
_DBSubnetGroupNotAllowedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupNotAllowedFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupNotAllowedFault"
    Core.. Core.hasStatus 400

-- | You can\'t start an export task that\'s already running.
_ExportTaskAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExportTaskAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ExportTaskAlreadyExists"
    Core.. Core.hasStatus 400

-- | The specified target group isn\'t available for a proxy owned by your
-- AWS account in the specified AWS Region.
_DBProxyTargetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBProxyTargetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetGroupNotFoundFault"
    Core.. Core.hasStatus 404

-- | The @GlobalClusterIdentifier@ already exists. Choose a new global
-- database identifier (unique name) to create a new global database
-- cluster.
_GlobalClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GlobalClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterAlreadyExistsFault"
    Core.. Core.hasStatus 400

-- | The specified RDS DB instance or Aurora DB cluster isn\'t available for
-- a proxy owned by your AWS account in the specified AWS Region.
_DBProxyTargetNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBProxyTargetNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetNotFoundFault"
    Core.. Core.hasStatus 404

-- | You have exceeded the maximum number of accounts that you can share a
-- manual DB snapshot with.
_SharedSnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SharedSnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SharedSnapshotQuotaExceeded"
    Core.. Core.hasStatus 400

-- | The specified reserved DB Instance not found.
_ReservedDBInstanceNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstanceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedDBInstanceNotFound"
    Core.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed number of
-- subnets in a DB subnet groups.
_DBSubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetQuotaExceededFault"
    Core.. Core.hasStatus 400

-- | The specified @RoleArn@ value doesn\'t match the specified feature for
-- the DB instance.
_DBInstanceRoleNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceRoleNotFound"
    Core.. Core.hasStatus 404

-- | Prism for BackupPolicyNotFoundFault' errors.
_BackupPolicyNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BackupPolicyNotFoundFault =
  Core._MatchServiceError
    defaultService
    "BackupPolicyNotFoundFault"
    Core.. Core.hasStatus 404

-- | The IAM role requires additional permissions to export to an Amazon S3
-- bucket.
_IamRoleMissingPermissionsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IamRoleMissingPermissionsFault =
  Core._MatchServiceError
    defaultService
    "IamRoleMissingPermissions"
    Core.. Core.hasStatus 400

-- | The proxy is already associated with the specified RDS DB instance or
-- Aurora DB cluster.
_DBProxyTargetAlreadyRegisteredFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBProxyTargetAlreadyRegisteredFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetAlreadyRegisteredFault"
    Core.. Core.hasStatus 400

-- | The specified CIDR IP range or Amazon EC2 security group is already
-- authorized for the specified DB security group.
_AuthorizationAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationAlreadyExists"
    Core.. Core.hasStatus 400

-- | The DB parameter group is in use or is in an invalid state. If you are
-- attempting to delete the parameter group, you can\'t delete it when the
-- parameter group is in this state.
_InvalidDBParameterGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBParameterGroupState"
    Core.. Core.hasStatus 400

-- | SNS has responded that there is a problem with the SND topic specified.
_SNSInvalidTopicFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError
    defaultService
    "SNSInvalidTopic"
    Core.. Core.hasStatus 400

-- | The requested source could not be found.
_SourceNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SourceNotFound"
    Core.. Core.hasStatus 404

-- | @SourceDBInstanceIdentifier@ refers to a DB instance with
-- @BackupRetentionPeriod@ equal to 0.
_PointInTimeRestoreNotEnabledFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PointInTimeRestoreNotEnabledFault =
  Core._MatchServiceError
    defaultService
    "PointInTimeRestoreNotEnabled"
    Core.. Core.hasStatus 400

-- | The supplied value isn\'t a valid DB cluster snapshot state.
_InvalidDBClusterSnapshotStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterSnapshotStateFault"
    Core.. Core.hasStatus 400

-- | The user already has a DB cluster snapshot with the given identifier.
_DBClusterSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotAlreadyExistsFault"
    Core.. Core.hasStatus 400

-- | The DB security group authorization quota has been reached.
_AuthorizationQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationQuotaExceeded"
    Core.. Core.hasStatus 400

-- | @InstallationMediaID@ doesn\'t refer to an existing installation medium.
_InstallationMediaNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstallationMediaNotFoundFault =
  Core._MatchServiceError
    defaultService
    "InstallationMediaNotFound"
    Core.. Core.hasStatus 404

-- | @DBParameterGroupName@ doesn\'t refer to an existing DB parameter group.
_DBParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupNotFound"
    Core.. Core.hasStatus 404

-- | The specified IAM role Amazon Resource Name (ARN) isn\'t associated with
-- the specified DB cluster.
_DBClusterRoleNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleNotFound"
    Core.. Core.hasStatus 404

-- | Subnets in the DB subnet group should cover at least two Availability
-- Zones unless there is only one Availability Zone.
_DBSubnetGroupDoesNotCoverEnoughAZs :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupDoesNotCoverEnoughAZs"
    Core.. Core.hasStatus 400

-- | The DB subnet isn\'t in the /available/ state.
_InvalidDBSubnetStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetStateFault"
    Core.. Core.hasStatus 400

-- | @Domain@ doesn\'t refer to an existing Active Directory domain.
_DomainNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DomainNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DomainNotFoundFault"
    Core.. Core.hasStatus 404

-- | The DBSubnetGroup doesn\'t belong to the same VPC as that of an existing
-- cross-region read replica of the same source instance.
_InvalidDBSubnetGroupFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetGroupFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetGroupFault"
    Core.. Core.hasStatus 400

-- | The DB subnet is already in use in the Availability Zone.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"
    Core.. Core.hasStatus 400

-- | @BacktrackIdentifier@ doesn\'t refer to an existing backtrack.
_DBClusterBacktrackNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterBacktrackNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterBacktrackNotFoundFault"
    Core.. Core.hasStatus 404

-- | The specified proxy name must be unique for all proxies owned by your
-- AWS account in the specified AWS Region.
_DBProxyAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBProxyAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBProxyTargetExistsFault"
    Core.. Core.hasStatus 400

-- | A DB security group isn\'t allowed for this action.
_DBSecurityGroupNotSupportedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupNotSupported"
    Core.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed amount of
-- storage available across all DB instances.
_StorageQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StorageQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "StorageQuotaExceeded"
    Core.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- instances.
_InstanceQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstanceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "InstanceQuotaExceeded"
    Core.. Core.hasStatus 400

-- | The state of the DB snapshot doesn\'t allow deletion.
_InvalidDBSnapshotStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSnapshotState"
    Core.. Core.hasStatus 400

-- | @DBInstanceIdentifier@ doesn\'t refer to an existing DB instance.
_DBInstanceNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceNotFound"
    Core.. Core.hasStatus 404

-- | The requested operation can\'t be performed while the proxy is in this
-- state.
_InvalidDBProxyStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBProxyStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBProxyStateFault"
    Core.. Core.hasStatus 400

-- | The number of global database clusters for this account is already at
-- the maximum allowed.
_GlobalClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GlobalClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterQuotaExceededFault"
    Core.. Core.hasStatus 400

-- | @DBSecurityGroupName@ doesn\'t refer to an existing DB security group.
_DBSecurityGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupNotFound"
    Core.. Core.hasStatus 404

-- | A DB security group with the name specified in @DBSecurityGroupName@
-- already exists.
_DBSecurityGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupAlreadyExists"
    Core.. Core.hasStatus 400

-- | The user already has a DB instance with the given identifier.
_DBInstanceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAlreadyExists"
    Core.. Core.hasStatus 400

-- | The quota of 20 option groups was exceeded for this AWS account.
_OptionGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OptionGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupQuotaExceededFault"
    Core.. Core.hasStatus 400

-- | The state of the DB security group doesn\'t allow deletion.
_InvalidDBSecurityGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBSecurityGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSecurityGroupState"
    Core.. Core.hasStatus 400

-- | @DBSnapshotIdentifier@ doesn\'t refer to an existing DB snapshot.
_DBSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotNotFound"
    Core.. Core.hasStatus 404

-- | @DBSubnetGroupName@ doesn\'t refer to an existing DB subnet group.
_DBSubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupNotFoundFault"
    Core.. Core.hasStatus 404

-- | The user attempted to create a new DB cluster and the user has already
-- reached the maximum allowed DB cluster quota.
_DBClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterQuotaExceededFault"
    Core.. Core.hasStatus 403

-- | @Capacity@ isn\'t a valid Aurora Serverless DB cluster capacity. Valid
-- capacity values are @2@, @4@, @8@, @16@, @32@, @64@, @128@, and @256@.
_InvalidDBClusterCapacityFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterCapacityFault"
    Core.. Core.hasStatus 400

-- | You have exceeded the maximum number of custom Availability Zones.
_CustomAvailabilityZoneQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomAvailabilityZoneQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CustomAvailabilityZoneQuotaExceeded"
    Core.. Core.hasStatus 400

-- | The export task doesn\'t exist.
_ExportTaskNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExportTaskNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ExportTaskNotFound"
    Core.. Core.hasStatus 404

-- | The requested operation can\'t be performed because there aren\'t enough
-- available IP addresses in the proxy\'s subnets. Add more CIDR blocks to
-- the VPC or remove IP address that aren\'t required from the subnets.
_InsufficientAvailableIPsInSubnetFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientAvailableIPsInSubnetFault =
  Core._MatchServiceError
    defaultService
    "InsufficientAvailableIPsInSubnetFault"
    Core.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB
-- security groups.
_DBSecurityGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "QuotaExceeded.DBSecurityGroup"
    Core.. Core.hasStatus 400

-- | The requested operation can\'t be performed while the cluster is in this
-- state.
_InvalidDBClusterStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterStateFault"
    Core.. Core.hasStatus 400

-- | The option group you are trying to create already exists.
_OptionGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OptionGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupAlreadyExistsFault"
    Core.. Core.hasStatus 400

-- | @CustomAvailabilityZoneName@ is already used by an existing custom
-- Availability Zone.
_CustomAvailabilityZoneAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomAvailabilityZoneAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CustomAvailabilityZoneAlreadyExists"
    Core.. Core.hasStatus 400

-- | The user already has a DB cluster with the given identifier.
_DBClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterAlreadyExistsFault"
    Core.. Core.hasStatus 400

-- | The option group isn\'t in the /available/ state.
_InvalidOptionGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOptionGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidOptionGroupStateFault"
    Core.. Core.hasStatus 400

-- | The @GlobalClusterIdentifier@ doesn\'t refer to an existing global
-- database cluster.
_GlobalClusterNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GlobalClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterNotFoundFault"
    Core.. Core.hasStatus 404

-- | The specified Amazon S3 bucket name can\'t be found or Amazon RDS isn\'t
-- authorized to access the specified Amazon S3 bucket. Verify the
-- __SourceS3BucketName__ and __S3IngestionRoleArn__ values and try again.
_InvalidS3BucketFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3BucketFault =
  Core._MatchServiceError
    defaultService
    "InvalidS3BucketFault"
    Core.. Core.hasStatus 400

-- | The state of the export snapshot is invalid for exporting to an Amazon
-- S3 bucket.
_InvalidExportSourceStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidExportSourceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidExportSourceState"
    Core.. Core.hasStatus 400

-- | @DBClusterIdentifier@ doesn\'t refer to an existing DB cluster.
_DBClusterNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterNotFoundFault"
    Core.. Core.hasStatus 404

-- | @CustomAvailabilityZoneId@ doesn\'t refer to an existing custom
-- Availability Zone identifier.
_CustomAvailabilityZoneNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomAvailabilityZoneNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CustomAvailabilityZoneNotFound"
    Core.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed number of DB
-- subnet groups.
_DBSubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupQuotaExceeded"
    Core.. Core.hasStatus 400

-- | The specified option group could not be found.
_OptionGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OptionGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupNotFoundFault"
    Core.. Core.hasStatus 404

-- | @LogFileName@ doesn\'t refer to an existing DB log file.
_DBLogFileNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBLogFileNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBLogFileNotFoundFault"
    Core.. Core.hasStatus 404

-- | You can\'t cancel an export task that has completed.
_InvalidExportTaskStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidExportTaskStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidExportTaskStateFault"
    Core.. Core.hasStatus 400

-- | The supplied category does not exist.
_SubscriptionCategoryNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubscriptionCategoryNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionCategoryNotFound"
    Core.. Core.hasStatus 404

-- | The specified CIDR IP range or Amazon EC2 security group might not be
-- authorized for the specified DB security group.
--
-- Or, RDS might not be authorized to perform necessary actions using IAM
-- on your behalf.
_AuthorizationNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Core.. Core.hasStatus 404

-- | The requested subnet is invalid, or multiple subnets were requested that
-- are not all in a common VPC.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Core.. Core.hasStatus 400

-- | The subscription name does not exist.
_SubscriptionNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubscriptionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionNotFound"
    Core.. Core.hasStatus 404
