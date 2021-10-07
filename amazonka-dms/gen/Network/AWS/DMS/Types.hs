{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _KMSDisabledFault,
    _KMSFault,
    _KMSAccessDeniedFault,
    _AccessDeniedFault,
    _InvalidCertificateFault,
    _SNSNoAuthorizationFault,
    _InvalidResourceStateFault,
    _KMSKeyNotAccessibleFault,
    _ResourceNotFoundFault,
    _ResourceQuotaExceededFault,
    _SNSInvalidTopicFault,
    _KMSNotFoundFault,
    _InsufficientResourceCapacityFault,
    _KMSInvalidStateFault,
    _SubnetAlreadyInUse,
    _S3AccessDeniedFault,
    _ResourceAlreadyExistsFault,
    _StorageQuotaExceededFault,
    _S3ResourceNotFoundFault,
    _ReplicationSubnetGroupDoesNotCoverEnoughAZs,
    _UpgradeDependencyFailureFault,
    _KMSThrottlingFault,
    _InvalidSubnet,

    -- * AuthMechanismValue
    AuthMechanismValue (..),

    -- * AuthTypeValue
    AuthTypeValue (..),

    -- * CannedAclForObjectsValue
    CannedAclForObjectsValue (..),

    -- * CharLengthSemantics
    CharLengthSemantics (..),

    -- * CompressionTypeValue
    CompressionTypeValue (..),

    -- * DataFormatValue
    DataFormatValue (..),

    -- * DatePartitionDelimiterValue
    DatePartitionDelimiterValue (..),

    -- * DatePartitionSequenceValue
    DatePartitionSequenceValue (..),

    -- * DmsSslModeValue
    DmsSslModeValue (..),

    -- * EncodingTypeValue
    EncodingTypeValue (..),

    -- * EncryptionModeValue
    EncryptionModeValue (..),

    -- * EndpointSettingTypeValue
    EndpointSettingTypeValue (..),

    -- * KafkaSecurityProtocol
    KafkaSecurityProtocol (..),

    -- * MessageFormatValue
    MessageFormatValue (..),

    -- * MigrationTypeValue
    MigrationTypeValue (..),

    -- * NestingLevelValue
    NestingLevelValue (..),

    -- * ParquetVersionValue
    ParquetVersionValue (..),

    -- * PluginNameValue
    PluginNameValue (..),

    -- * RedisAuthTypeValue
    RedisAuthTypeValue (..),

    -- * RefreshSchemasStatusTypeValue
    RefreshSchemasStatusTypeValue (..),

    -- * ReleaseStatusValues
    ReleaseStatusValues (..),

    -- * ReloadOptionValue
    ReloadOptionValue (..),

    -- * ReplicationEndpointTypeValue
    ReplicationEndpointTypeValue (..),

    -- * SafeguardPolicy
    SafeguardPolicy (..),

    -- * SourceType
    SourceType (..),

    -- * SslSecurityProtocolValue
    SslSecurityProtocolValue (..),

    -- * StartReplicationTaskTypeValue
    StartReplicationTaskTypeValue (..),

    -- * TargetDbType
    TargetDbType (..),

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

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_certificateOwner,
    certificate_signingAlgorithm,
    certificate_validToDate,
    certificate_certificateIdentifier,
    certificate_keyLength,
    certificate_certificateArn,
    certificate_certificateWallet,
    certificate_validFromDate,
    certificate_certificateCreationDate,
    certificate_certificatePem,

    -- * Connection
    Connection (..),
    newConnection,
    connection_status,
    connection_lastFailureMessage,
    connection_replicationInstanceIdentifier,
    connection_endpointArn,
    connection_endpointIdentifier,
    connection_replicationInstanceArn,

    -- * DmsTransferSettings
    DmsTransferSettings (..),
    newDmsTransferSettings,
    dmsTransferSettings_bucketName,
    dmsTransferSettings_serviceAccessRoleArn,

    -- * DocDbSettings
    DocDbSettings (..),
    newDocDbSettings,
    docDbSettings_secretsManagerSecretId,
    docDbSettings_serverName,
    docDbSettings_kmsKeyId,
    docDbSettings_password,
    docDbSettings_port,
    docDbSettings_username,
    docDbSettings_secretsManagerAccessRoleArn,
    docDbSettings_extractDocId,
    docDbSettings_docsToInvestigate,
    docDbSettings_nestingLevel,
    docDbSettings_databaseName,

    -- * DynamoDbSettings
    DynamoDbSettings (..),
    newDynamoDbSettings,
    dynamoDbSettings_serviceAccessRoleArn,

    -- * ElasticsearchSettings
    ElasticsearchSettings (..),
    newElasticsearchSettings,
    elasticsearchSettings_errorRetryDuration,
    elasticsearchSettings_fullLoadErrorPercentage,
    elasticsearchSettings_serviceAccessRoleArn,
    elasticsearchSettings_endpointUri,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_mongoDbSettings,
    endpoint_sslMode,
    endpoint_status,
    endpoint_neptuneSettings,
    endpoint_redisSettings,
    endpoint_engineName,
    endpoint_externalTableDefinition,
    endpoint_elasticsearchSettings,
    endpoint_postgreSQLSettings,
    endpoint_oracleSettings,
    endpoint_endpointType,
    endpoint_certificateArn,
    endpoint_serviceAccessRoleArn,
    endpoint_s3Settings,
    endpoint_microsoftSQLServerSettings,
    endpoint_serverName,
    endpoint_kmsKeyId,
    endpoint_iBMDb2Settings,
    endpoint_mySQLSettings,
    endpoint_dmsTransferSettings,
    endpoint_port,
    endpoint_redshiftSettings,
    endpoint_endpointArn,
    endpoint_username,
    endpoint_extraConnectionAttributes,
    endpoint_kafkaSettings,
    endpoint_docDbSettings,
    endpoint_engineDisplayName,
    endpoint_dynamoDbSettings,
    endpoint_endpointIdentifier,
    endpoint_externalId,
    endpoint_kinesisSettings,
    endpoint_sybaseSettings,
    endpoint_databaseName,

    -- * EndpointSetting
    EndpointSetting (..),
    newEndpointSetting,
    endpointSetting_applicability,
    endpointSetting_intValueMin,
    endpointSetting_name,
    endpointSetting_intValueMax,
    endpointSetting_sensitive,
    endpointSetting_enumValues,
    endpointSetting_defaultValue,
    endpointSetting_type,
    endpointSetting_units,

    -- * Event
    Event (..),
    newEvent,
    event_message,
    event_eventCategories,
    event_date,
    event_sourceIdentifier,
    event_sourceType,

    -- * EventCategoryGroup
    EventCategoryGroup (..),
    newEventCategoryGroup,
    eventCategoryGroup_eventCategories,
    eventCategoryGroup_sourceType,

    -- * EventSubscription
    EventSubscription (..),
    newEventSubscription,
    eventSubscription_custSubscriptionId,
    eventSubscription_status,
    eventSubscription_sourceIdsList,
    eventSubscription_eventCategoriesList,
    eventSubscription_enabled,
    eventSubscription_subscriptionCreationTime,
    eventSubscription_customerAwsId,
    eventSubscription_sourceType,
    eventSubscription_snsTopicArn,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * IBMDb2Settings
    IBMDb2Settings (..),
    newIBMDb2Settings,
    iBMDb2Settings_currentLsn,
    iBMDb2Settings_secretsManagerSecretId,
    iBMDb2Settings_serverName,
    iBMDb2Settings_password,
    iBMDb2Settings_port,
    iBMDb2Settings_username,
    iBMDb2Settings_secretsManagerAccessRoleArn,
    iBMDb2Settings_maxKBytesPerRead,
    iBMDb2Settings_setDataCaptureChanges,
    iBMDb2Settings_databaseName,

    -- * KafkaSettings
    KafkaSettings (..),
    newKafkaSettings,
    kafkaSettings_noHexPrefix,
    kafkaSettings_includeNullAndEmpty,
    kafkaSettings_sslCaCertificateArn,
    kafkaSettings_messageFormat,
    kafkaSettings_sslClientCertificateArn,
    kafkaSettings_sslClientKeyPassword,
    kafkaSettings_sslClientKeyArn,
    kafkaSettings_includeControlDetails,
    kafkaSettings_partitionIncludeSchemaTable,
    kafkaSettings_topic,
    kafkaSettings_messageMaxBytes,
    kafkaSettings_broker,
    kafkaSettings_saslUsername,
    kafkaSettings_securityProtocol,
    kafkaSettings_includePartitionValue,
    kafkaSettings_includeTransactionDetails,
    kafkaSettings_saslPassword,
    kafkaSettings_includeTableAlterOperations,

    -- * KinesisSettings
    KinesisSettings (..),
    newKinesisSettings,
    kinesisSettings_noHexPrefix,
    kinesisSettings_includeNullAndEmpty,
    kinesisSettings_messageFormat,
    kinesisSettings_serviceAccessRoleArn,
    kinesisSettings_includeControlDetails,
    kinesisSettings_partitionIncludeSchemaTable,
    kinesisSettings_streamArn,
    kinesisSettings_includePartitionValue,
    kinesisSettings_includeTransactionDetails,
    kinesisSettings_includeTableAlterOperations,

    -- * MicrosoftSQLServerSettings
    MicrosoftSQLServerSettings (..),
    newMicrosoftSQLServerSettings,
    microsoftSQLServerSettings_useBcpFullLoad,
    microsoftSQLServerSettings_safeguardPolicy,
    microsoftSQLServerSettings_useThirdPartyBackupDevice,
    microsoftSQLServerSettings_secretsManagerSecretId,
    microsoftSQLServerSettings_serverName,
    microsoftSQLServerSettings_password,
    microsoftSQLServerSettings_bcpPacketSize,
    microsoftSQLServerSettings_port,
    microsoftSQLServerSettings_username,
    microsoftSQLServerSettings_controlTablesFileGroup,
    microsoftSQLServerSettings_secretsManagerAccessRoleArn,
    microsoftSQLServerSettings_readBackupOnly,
    microsoftSQLServerSettings_querySingleAlwaysOnNode,
    microsoftSQLServerSettings_databaseName,

    -- * MongoDbSettings
    MongoDbSettings (..),
    newMongoDbSettings,
    mongoDbSettings_authSource,
    mongoDbSettings_secretsManagerSecretId,
    mongoDbSettings_serverName,
    mongoDbSettings_kmsKeyId,
    mongoDbSettings_password,
    mongoDbSettings_port,
    mongoDbSettings_username,
    mongoDbSettings_authMechanism,
    mongoDbSettings_secretsManagerAccessRoleArn,
    mongoDbSettings_authType,
    mongoDbSettings_extractDocId,
    mongoDbSettings_docsToInvestigate,
    mongoDbSettings_nestingLevel,
    mongoDbSettings_databaseName,

    -- * MySQLSettings
    MySQLSettings (..),
    newMySQLSettings,
    mySQLSettings_targetDbType,
    mySQLSettings_serverTimezone,
    mySQLSettings_afterConnectScript,
    mySQLSettings_secretsManagerSecretId,
    mySQLSettings_serverName,
    mySQLSettings_maxFileSize,
    mySQLSettings_password,
    mySQLSettings_eventsPollInterval,
    mySQLSettings_port,
    mySQLSettings_username,
    mySQLSettings_secretsManagerAccessRoleArn,
    mySQLSettings_parallelLoadThreads,
    mySQLSettings_cleanSourceMetadataOnMismatch,
    mySQLSettings_databaseName,

    -- * NeptuneSettings
    NeptuneSettings (..),
    newNeptuneSettings,
    neptuneSettings_errorRetryDuration,
    neptuneSettings_serviceAccessRoleArn,
    neptuneSettings_maxFileSize,
    neptuneSettings_maxRetryCount,
    neptuneSettings_iamAuthEnabled,
    neptuneSettings_s3BucketName,
    neptuneSettings_s3BucketFolder,

    -- * OracleSettings
    OracleSettings (..),
    newOracleSettings,
    oracleSettings_failTasksOnLobTruncation,
    oracleSettings_retryInterval,
    oracleSettings_useLogminerReader,
    oracleSettings_standbyDelayTime,
    oracleSettings_useBFile,
    oracleSettings_accessAlternateDirectly,
    oracleSettings_secretsManagerOracleAsmSecretId,
    oracleSettings_useAlternateFolderForOnline,
    oracleSettings_useDirectPathFullLoad,
    oracleSettings_numberDatatypeScale,
    oracleSettings_oraclePathPrefix,
    oracleSettings_securityDbEncryptionName,
    oracleSettings_asmPassword,
    oracleSettings_additionalArchivedLogDestId,
    oracleSettings_directPathNoLog,
    oracleSettings_archivedLogsOnly,
    oracleSettings_directPathParallelLoad,
    oracleSettings_secretsManagerSecretId,
    oracleSettings_serverName,
    oracleSettings_asmServer,
    oracleSettings_password,
    oracleSettings_readTableSpaceName,
    oracleSettings_securityDbEncryption,
    oracleSettings_archivedLogDestId,
    oracleSettings_replacePathPrefix,
    oracleSettings_readAheadBlocks,
    oracleSettings_port,
    oracleSettings_allowSelectNestedTables,
    oracleSettings_asmUser,
    oracleSettings_usePathPrefix,
    oracleSettings_username,
    oracleSettings_parallelAsmReadThreads,
    oracleSettings_secretsManagerAccessRoleArn,
    oracleSettings_enableHomogenousTablespace,
    oracleSettings_extraArchivedLogDestIds,
    oracleSettings_charLengthSemantics,
    oracleSettings_spatialDataOptionToGeoJsonFunctionName,
    oracleSettings_addSupplementalLogging,
    oracleSettings_databaseName,
    oracleSettings_secretsManagerOracleAsmAccessRoleArn,

    -- * OrderableReplicationInstance
    OrderableReplicationInstance (..),
    newOrderableReplicationInstance,
    orderableReplicationInstance_availabilityZones,
    orderableReplicationInstance_maxAllocatedStorage,
    orderableReplicationInstance_storageType,
    orderableReplicationInstance_defaultAllocatedStorage,
    orderableReplicationInstance_includedAllocatedStorage,
    orderableReplicationInstance_releaseStatus,
    orderableReplicationInstance_engineVersion,
    orderableReplicationInstance_replicationInstanceClass,
    orderableReplicationInstance_minAllocatedStorage,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_description,

    -- * PostgreSQLSettings
    PostgreSQLSettings (..),
    newPostgreSQLSettings,
    postgreSQLSettings_failTasksOnLobTruncation,
    postgreSQLSettings_executeTimeout,
    postgreSQLSettings_heartbeatEnable,
    postgreSQLSettings_heartbeatSchema,
    postgreSQLSettings_pluginName,
    postgreSQLSettings_captureDdls,
    postgreSQLSettings_slotName,
    postgreSQLSettings_ddlArtifactsSchema,
    postgreSQLSettings_afterConnectScript,
    postgreSQLSettings_secretsManagerSecretId,
    postgreSQLSettings_serverName,
    postgreSQLSettings_maxFileSize,
    postgreSQLSettings_password,
    postgreSQLSettings_heartbeatFrequency,
    postgreSQLSettings_port,
    postgreSQLSettings_username,
    postgreSQLSettings_secretsManagerAccessRoleArn,
    postgreSQLSettings_databaseName,

    -- * RedisSettings
    RedisSettings (..),
    newRedisSettings,
    redisSettings_sslCaCertificateArn,
    redisSettings_authPassword,
    redisSettings_sslSecurityProtocol,
    redisSettings_authUserName,
    redisSettings_authType,
    redisSettings_serverName,
    redisSettings_port,

    -- * RedshiftSettings
    RedshiftSettings (..),
    newRedshiftSettings,
    redshiftSettings_replaceChars,
    redshiftSettings_caseSensitiveNames,
    redshiftSettings_fileTransferUploadStreams,
    redshiftSettings_bucketName,
    redshiftSettings_timeFormat,
    redshiftSettings_replaceInvalidChars,
    redshiftSettings_serverSideEncryptionKmsKeyId,
    redshiftSettings_bucketFolder,
    redshiftSettings_writeBufferSize,
    redshiftSettings_serviceAccessRoleArn,
    redshiftSettings_afterConnectScript,
    redshiftSettings_secretsManagerSecretId,
    redshiftSettings_connectionTimeout,
    redshiftSettings_loadTimeout,
    redshiftSettings_acceptAnyDate,
    redshiftSettings_serverName,
    redshiftSettings_dateFormat,
    redshiftSettings_maxFileSize,
    redshiftSettings_password,
    redshiftSettings_removeQuotes,
    redshiftSettings_encryptionMode,
    redshiftSettings_emptyAsNull,
    redshiftSettings_port,
    redshiftSettings_username,
    redshiftSettings_trimBlanks,
    redshiftSettings_truncateColumns,
    redshiftSettings_secretsManagerAccessRoleArn,
    redshiftSettings_explicitIds,
    redshiftSettings_compUpdate,
    redshiftSettings_databaseName,

    -- * RefreshSchemasStatus
    RefreshSchemasStatus (..),
    newRefreshSchemasStatus,
    refreshSchemasStatus_status,
    refreshSchemasStatus_lastFailureMessage,
    refreshSchemasStatus_endpointArn,
    refreshSchemasStatus_lastRefreshDate,
    refreshSchemasStatus_replicationInstanceArn,

    -- * ReplicationInstance
    ReplicationInstance (..),
    newReplicationInstance,
    replicationInstance_vpcSecurityGroups,
    replicationInstance_replicationInstancePrivateIpAddress,
    replicationInstance_freeUntil,
    replicationInstance_instanceCreateTime,
    replicationInstance_replicationSubnetGroup,
    replicationInstance_publiclyAccessible,
    replicationInstance_multiAZ,
    replicationInstance_kmsKeyId,
    replicationInstance_availabilityZone,
    replicationInstance_engineVersion,
    replicationInstance_preferredMaintenanceWindow,
    replicationInstance_replicationInstancePrivateIpAddresses,
    replicationInstance_replicationInstanceStatus,
    replicationInstance_replicationInstanceIdentifier,
    replicationInstance_pendingModifiedValues,
    replicationInstance_replicationInstanceClass,
    replicationInstance_replicationInstancePublicIpAddress,
    replicationInstance_dnsNameServers,
    replicationInstance_replicationInstanceArn,
    replicationInstance_allocatedStorage,
    replicationInstance_replicationInstancePublicIpAddresses,
    replicationInstance_secondaryAvailabilityZone,
    replicationInstance_autoMinorVersionUpgrade,

    -- * ReplicationInstanceTaskLog
    ReplicationInstanceTaskLog (..),
    newReplicationInstanceTaskLog,
    replicationInstanceTaskLog_replicationTaskName,
    replicationInstanceTaskLog_replicationTaskArn,
    replicationInstanceTaskLog_replicationInstanceTaskLogSize,

    -- * ReplicationPendingModifiedValues
    ReplicationPendingModifiedValues (..),
    newReplicationPendingModifiedValues,
    replicationPendingModifiedValues_multiAZ,
    replicationPendingModifiedValues_engineVersion,
    replicationPendingModifiedValues_replicationInstanceClass,
    replicationPendingModifiedValues_allocatedStorage,

    -- * ReplicationSubnetGroup
    ReplicationSubnetGroup (..),
    newReplicationSubnetGroup,
    replicationSubnetGroup_replicationSubnetGroupIdentifier,
    replicationSubnetGroup_subnetGroupStatus,
    replicationSubnetGroup_replicationSubnetGroupDescription,
    replicationSubnetGroup_vpcId,
    replicationSubnetGroup_subnets,

    -- * ReplicationTask
    ReplicationTask (..),
    newReplicationTask,
    replicationTask_status,
    replicationTask_stopReason,
    replicationTask_migrationType,
    replicationTask_replicationTaskCreationDate,
    replicationTask_recoveryCheckpoint,
    replicationTask_targetReplicationInstanceArn,
    replicationTask_taskData,
    replicationTask_targetEndpointArn,
    replicationTask_replicationTaskSettings,
    replicationTask_replicationTaskArn,
    replicationTask_tableMappings,
    replicationTask_lastFailureMessage,
    replicationTask_sourceEndpointArn,
    replicationTask_replicationInstanceArn,
    replicationTask_replicationTaskStats,
    replicationTask_replicationTaskStartDate,
    replicationTask_cdcStopPosition,
    replicationTask_cdcStartPosition,
    replicationTask_replicationTaskIdentifier,

    -- * ReplicationTaskAssessmentResult
    ReplicationTaskAssessmentResult (..),
    newReplicationTaskAssessmentResult,
    replicationTaskAssessmentResult_s3ObjectUrl,
    replicationTaskAssessmentResult_assessmentStatus,
    replicationTaskAssessmentResult_assessmentResults,
    replicationTaskAssessmentResult_replicationTaskArn,
    replicationTaskAssessmentResult_replicationTaskLastAssessmentDate,
    replicationTaskAssessmentResult_replicationTaskIdentifier,
    replicationTaskAssessmentResult_assessmentResultsFile,

    -- * ReplicationTaskAssessmentRun
    ReplicationTaskAssessmentRun (..),
    newReplicationTaskAssessmentRun,
    replicationTaskAssessmentRun_status,
    replicationTaskAssessmentRun_resultKmsKeyArn,
    replicationTaskAssessmentRun_replicationTaskAssessmentRunCreationDate,
    replicationTaskAssessmentRun_assessmentProgress,
    replicationTaskAssessmentRun_replicationTaskAssessmentRunArn,
    replicationTaskAssessmentRun_assessmentRunName,
    replicationTaskAssessmentRun_serviceAccessRoleArn,
    replicationTaskAssessmentRun_resultEncryptionMode,
    replicationTaskAssessmentRun_replicationTaskArn,
    replicationTaskAssessmentRun_lastFailureMessage,
    replicationTaskAssessmentRun_resultLocationFolder,
    replicationTaskAssessmentRun_resultLocationBucket,

    -- * ReplicationTaskAssessmentRunProgress
    ReplicationTaskAssessmentRunProgress (..),
    newReplicationTaskAssessmentRunProgress,
    replicationTaskAssessmentRunProgress_individualAssessmentCount,
    replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount,

    -- * ReplicationTaskIndividualAssessment
    ReplicationTaskIndividualAssessment (..),
    newReplicationTaskIndividualAssessment,
    replicationTaskIndividualAssessment_status,
    replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn,
    replicationTaskIndividualAssessment_individualAssessmentName,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn,

    -- * ReplicationTaskStats
    ReplicationTaskStats (..),
    newReplicationTaskStats,
    replicationTaskStats_tablesErrored,
    replicationTaskStats_stopDate,
    replicationTaskStats_startDate,
    replicationTaskStats_freshStartDate,
    replicationTaskStats_tablesLoading,
    replicationTaskStats_fullLoadStartDate,
    replicationTaskStats_elapsedTimeMillis,
    replicationTaskStats_fullLoadProgressPercent,
    replicationTaskStats_tablesQueued,
    replicationTaskStats_tablesLoaded,
    replicationTaskStats_fullLoadFinishDate,

    -- * ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    newResourcePendingMaintenanceActions,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- * S3Settings
    S3Settings (..),
    newS3Settings,
    s3Settings_timestampColumnName,
    s3Settings_csvRowDelimiter,
    s3Settings_preserveTransactions,
    s3Settings_parquetVersion,
    s3Settings_datePartitionSequence,
    s3Settings_bucketName,
    s3Settings_cdcPath,
    s3Settings_serverSideEncryptionKmsKeyId,
    s3Settings_externalTableDefinition,
    s3Settings_csvNullValue,
    s3Settings_dataPageSize,
    s3Settings_dataFormat,
    s3Settings_datePartitionEnabled,
    s3Settings_encodingType,
    s3Settings_bucketFolder,
    s3Settings_cannedAclForObjects,
    s3Settings_serviceAccessRoleArn,
    s3Settings_addColumnName,
    s3Settings_datePartitionDelimiter,
    s3Settings_maxFileSize,
    s3Settings_encryptionMode,
    s3Settings_cdcInsertsOnly,
    s3Settings_enableStatistics,
    s3Settings_useCsvNoSupValue,
    s3Settings_cdcInsertsAndUpdates,
    s3Settings_rowGroupLength,
    s3Settings_dictPageSizeLimit,
    s3Settings_ignoreHeaderRows,
    s3Settings_compressionType,
    s3Settings_cdcMaxBatchInterval,
    s3Settings_cdcMinFileSize,
    s3Settings_includeOpForFullLoad,
    s3Settings_csvDelimiter,
    s3Settings_parquetTimestampInMillisecond,
    s3Settings_rfc4180,
    s3Settings_csvNoSupValue,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetStatus,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,

    -- * SupportedEndpointType
    SupportedEndpointType (..),
    newSupportedEndpointType,
    supportedEndpointType_replicationInstanceEngineMinimumVersion,
    supportedEndpointType_engineName,
    supportedEndpointType_endpointType,
    supportedEndpointType_supportsCDC,
    supportedEndpointType_engineDisplayName,

    -- * SybaseSettings
    SybaseSettings (..),
    newSybaseSettings,
    sybaseSettings_secretsManagerSecretId,
    sybaseSettings_serverName,
    sybaseSettings_password,
    sybaseSettings_port,
    sybaseSettings_username,
    sybaseSettings_secretsManagerAccessRoleArn,
    sybaseSettings_databaseName,

    -- * TableStatistics
    TableStatistics (..),
    newTableStatistics,
    tableStatistics_fullLoadCondtnlChkFailedRows,
    tableStatistics_fullLoadRows,
    tableStatistics_fullLoadErrorRows,
    tableStatistics_tableName,
    tableStatistics_tableState,
    tableStatistics_lastUpdateTime,
    tableStatistics_validationFailedRecords,
    tableStatistics_fullLoadStartTime,
    tableStatistics_updates,
    tableStatistics_deletes,
    tableStatistics_ddls,
    tableStatistics_fullLoadEndTime,
    tableStatistics_inserts,
    tableStatistics_validationState,
    tableStatistics_validationStateDetails,
    tableStatistics_validationSuspendedRecords,
    tableStatistics_schemaName,
    tableStatistics_fullLoadReloaded,
    tableStatistics_validationPendingRecords,

    -- * TableToReload
    TableToReload (..),
    newTableToReload,
    tableToReload_schemaName,
    tableToReload_tableName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_resourceArn,
    tag_value,

    -- * VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    newVpcSecurityGroupMembership,
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.AccountQuota
import Network.AWS.DMS.Types.AuthMechanismValue
import Network.AWS.DMS.Types.AuthTypeValue
import Network.AWS.DMS.Types.AvailabilityZone
import Network.AWS.DMS.Types.CannedAclForObjectsValue
import Network.AWS.DMS.Types.Certificate
import Network.AWS.DMS.Types.CharLengthSemantics
import Network.AWS.DMS.Types.CompressionTypeValue
import Network.AWS.DMS.Types.Connection
import Network.AWS.DMS.Types.DataFormatValue
import Network.AWS.DMS.Types.DatePartitionDelimiterValue
import Network.AWS.DMS.Types.DatePartitionSequenceValue
import Network.AWS.DMS.Types.DmsSslModeValue
import Network.AWS.DMS.Types.DmsTransferSettings
import Network.AWS.DMS.Types.DocDbSettings
import Network.AWS.DMS.Types.DynamoDbSettings
import Network.AWS.DMS.Types.ElasticsearchSettings
import Network.AWS.DMS.Types.EncodingTypeValue
import Network.AWS.DMS.Types.EncryptionModeValue
import Network.AWS.DMS.Types.Endpoint
import Network.AWS.DMS.Types.EndpointSetting
import Network.AWS.DMS.Types.EndpointSettingTypeValue
import Network.AWS.DMS.Types.Event
import Network.AWS.DMS.Types.EventCategoryGroup
import Network.AWS.DMS.Types.EventSubscription
import Network.AWS.DMS.Types.Filter
import Network.AWS.DMS.Types.IBMDb2Settings
import Network.AWS.DMS.Types.KafkaSecurityProtocol
import Network.AWS.DMS.Types.KafkaSettings
import Network.AWS.DMS.Types.KinesisSettings
import Network.AWS.DMS.Types.MessageFormatValue
import Network.AWS.DMS.Types.MicrosoftSQLServerSettings
import Network.AWS.DMS.Types.MigrationTypeValue
import Network.AWS.DMS.Types.MongoDbSettings
import Network.AWS.DMS.Types.MySQLSettings
import Network.AWS.DMS.Types.NeptuneSettings
import Network.AWS.DMS.Types.NestingLevelValue
import Network.AWS.DMS.Types.OracleSettings
import Network.AWS.DMS.Types.OrderableReplicationInstance
import Network.AWS.DMS.Types.ParquetVersionValue
import Network.AWS.DMS.Types.PendingMaintenanceAction
import Network.AWS.DMS.Types.PluginNameValue
import Network.AWS.DMS.Types.PostgreSQLSettings
import Network.AWS.DMS.Types.RedisAuthTypeValue
import Network.AWS.DMS.Types.RedisSettings
import Network.AWS.DMS.Types.RedshiftSettings
import Network.AWS.DMS.Types.RefreshSchemasStatus
import Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue
import Network.AWS.DMS.Types.ReleaseStatusValues
import Network.AWS.DMS.Types.ReloadOptionValue
import Network.AWS.DMS.Types.ReplicationEndpointTypeValue
import Network.AWS.DMS.Types.ReplicationInstance
import Network.AWS.DMS.Types.ReplicationInstanceTaskLog
import Network.AWS.DMS.Types.ReplicationPendingModifiedValues
import Network.AWS.DMS.Types.ReplicationSubnetGroup
import Network.AWS.DMS.Types.ReplicationTask
import Network.AWS.DMS.Types.ReplicationTaskAssessmentResult
import Network.AWS.DMS.Types.ReplicationTaskAssessmentRun
import Network.AWS.DMS.Types.ReplicationTaskAssessmentRunProgress
import Network.AWS.DMS.Types.ReplicationTaskIndividualAssessment
import Network.AWS.DMS.Types.ReplicationTaskStats
import Network.AWS.DMS.Types.ResourcePendingMaintenanceActions
import Network.AWS.DMS.Types.S3Settings
import Network.AWS.DMS.Types.SafeguardPolicy
import Network.AWS.DMS.Types.SourceType
import Network.AWS.DMS.Types.SslSecurityProtocolValue
import Network.AWS.DMS.Types.StartReplicationTaskTypeValue
import Network.AWS.DMS.Types.Subnet
import Network.AWS.DMS.Types.SupportedEndpointType
import Network.AWS.DMS.Types.SybaseSettings
import Network.AWS.DMS.Types.TableStatistics
import Network.AWS.DMS.Types.TableToReload
import Network.AWS.DMS.Types.Tag
import Network.AWS.DMS.Types.TargetDbType
import Network.AWS.DMS.Types.VpcSecurityGroupMembership
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-01-01@ of the Amazon Database Migration Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "DMS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "dms",
      Core._serviceSigningName = "dms",
      Core._serviceVersion = "2016-01-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "DMS",
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
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | The specified master key (CMK) isn\'t enabled.
_KMSDisabledFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSDisabledFault =
  Core._MatchServiceError
    defaultService
    "KMSDisabledFault"

-- | An Key Management Service (KMS) error is preventing access to KMS.
_KMSFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSFault =
  Core._MatchServiceError defaultService "KMSFault"

-- | The ciphertext references a key that doesn\'t exist or that the DMS
-- account doesn\'t have access to.
_KMSAccessDeniedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSAccessDeniedFault =
  Core._MatchServiceError
    defaultService
    "KMSAccessDeniedFault"

-- | DMS was denied access to the endpoint. Check that the role is correctly
-- configured.
_AccessDeniedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedFault =
  Core._MatchServiceError
    defaultService
    "AccessDeniedFault"

-- | The certificate was not valid.
_InvalidCertificateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCertificateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCertificateFault"

-- | You are not authorized for the SNS subscription.
_SNSNoAuthorizationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorizationFault"

-- | The resource is in a state that prevents it from being used for database
-- migration.
_InvalidResourceStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidResourceStateFault"

-- | DMS cannot access the KMS key.
_KMSKeyNotAccessibleFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSKeyNotAccessibleFault =
  Core._MatchServiceError
    defaultService
    "KMSKeyNotAccessibleFault"

-- | The resource could not be found.
_ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundFault"

-- | The quota for this resource quota has been exceeded.
_ResourceQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ResourceQuotaExceededFault"

-- | The SNS topic is invalid.
_SNSInvalidTopicFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError
    defaultService
    "SNSInvalidTopicFault"

-- | The specified KMS entity or resource can\'t be found.
_KMSNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSNotFoundFault =
  Core._MatchServiceError
    defaultService
    "KMSNotFoundFault"

-- | There are not enough resources allocated to the database migration.
_InsufficientResourceCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientResourceCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientResourceCapacityFault"

-- | The state of the specified KMS resource isn\'t valid for this request.
_KMSInvalidStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateFault =
  Core._MatchServiceError
    defaultService
    "KMSInvalidStateFault"

-- | The specified subnet is already in use.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"

-- | Insufficient privileges are preventing access to an Amazon S3 object.
_S3AccessDeniedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_S3AccessDeniedFault =
  Core._MatchServiceError
    defaultService
    "S3AccessDeniedFault"

-- | The resource you are attempting to create already exists.
_ResourceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsFault"

-- | The storage quota has been exceeded.
_StorageQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "StorageQuotaExceededFault"

-- | A specified Amazon S3 bucket, bucket folder, or other object can\'t be
-- found.
_S3ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_S3ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "S3ResourceNotFoundFault"

-- | The replication subnet group does not cover enough Availability Zones
-- (AZs). Edit the replication subnet group and add more AZs.
_ReplicationSubnetGroupDoesNotCoverEnoughAZs :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationSubnetGroupDoesNotCoverEnoughAZs =
  Core._MatchServiceError
    defaultService
    "ReplicationSubnetGroupDoesNotCoverEnoughAZs"

-- | An upgrade dependency is preventing the database migration.
_UpgradeDependencyFailureFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UpgradeDependencyFailureFault =
  Core._MatchServiceError
    defaultService
    "UpgradeDependencyFailureFault"

-- | This request triggered KMS request throttling.
_KMSThrottlingFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSThrottlingFault =
  Core._MatchServiceError
    defaultService
    "KMSThrottlingFault"

-- | The subnet provided is invalid.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
