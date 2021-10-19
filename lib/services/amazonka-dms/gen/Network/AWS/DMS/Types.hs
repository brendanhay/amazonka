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
    _KMSAccessDeniedFault,
    _KMSDisabledFault,
    _KMSFault,
    _InvalidSubnet,
    _KMSKeyNotAccessibleFault,
    _ReplicationSubnetGroupDoesNotCoverEnoughAZs,
    _S3ResourceNotFoundFault,
    _InvalidResourceStateFault,
    _InvalidCertificateFault,
    _SNSNoAuthorizationFault,
    _ResourceAlreadyExistsFault,
    _InsufficientResourceCapacityFault,
    _S3AccessDeniedFault,
    _SNSInvalidTopicFault,
    _KMSNotFoundFault,
    _KMSThrottlingFault,
    _ResourceQuotaExceededFault,
    _UpgradeDependencyFailureFault,
    _ResourceNotFoundFault,
    _StorageQuotaExceededFault,
    _AccessDeniedFault,
    _SubnetAlreadyInUse,
    _KMSInvalidStateFault,

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
    accountQuota_max,
    accountQuota_used,
    accountQuota_accountQuotaName,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_certificateOwner,
    certificate_signingAlgorithm,
    certificate_validFromDate,
    certificate_certificatePem,
    certificate_certificateArn,
    certificate_certificateCreationDate,
    certificate_certificateIdentifier,
    certificate_certificateWallet,
    certificate_keyLength,
    certificate_validToDate,

    -- * Connection
    Connection (..),
    newConnection,
    connection_status,
    connection_replicationInstanceArn,
    connection_endpointIdentifier,
    connection_replicationInstanceIdentifier,
    connection_endpointArn,
    connection_lastFailureMessage,

    -- * DmsTransferSettings
    DmsTransferSettings (..),
    newDmsTransferSettings,
    dmsTransferSettings_serviceAccessRoleArn,
    dmsTransferSettings_bucketName,

    -- * DocDbSettings
    DocDbSettings (..),
    newDocDbSettings,
    docDbSettings_serverName,
    docDbSettings_secretsManagerAccessRoleArn,
    docDbSettings_username,
    docDbSettings_kmsKeyId,
    docDbSettings_password,
    docDbSettings_nestingLevel,
    docDbSettings_databaseName,
    docDbSettings_docsToInvestigate,
    docDbSettings_secretsManagerSecretId,
    docDbSettings_extractDocId,
    docDbSettings_port,

    -- * DynamoDbSettings
    DynamoDbSettings (..),
    newDynamoDbSettings,
    dynamoDbSettings_serviceAccessRoleArn,

    -- * ElasticsearchSettings
    ElasticsearchSettings (..),
    newElasticsearchSettings,
    elasticsearchSettings_fullLoadErrorPercentage,
    elasticsearchSettings_errorRetryDuration,
    elasticsearchSettings_serviceAccessRoleArn,
    elasticsearchSettings_endpointUri,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_status,
    endpoint_dmsTransferSettings,
    endpoint_mySQLSettings,
    endpoint_serverName,
    endpoint_microsoftSQLServerSettings,
    endpoint_certificateArn,
    endpoint_serviceAccessRoleArn,
    endpoint_docDbSettings,
    endpoint_engineDisplayName,
    endpoint_postgreSQLSettings,
    endpoint_extraConnectionAttributes,
    endpoint_kafkaSettings,
    endpoint_oracleSettings,
    endpoint_endpointType,
    endpoint_redshiftSettings,
    endpoint_elasticsearchSettings,
    endpoint_username,
    endpoint_externalTableDefinition,
    endpoint_engineName,
    endpoint_redisSettings,
    endpoint_neptuneSettings,
    endpoint_iBMDb2Settings,
    endpoint_kmsKeyId,
    endpoint_mongoDbSettings,
    endpoint_sslMode,
    endpoint_sybaseSettings,
    endpoint_databaseName,
    endpoint_s3Settings,
    endpoint_kinesisSettings,
    endpoint_endpointIdentifier,
    endpoint_externalId,
    endpoint_dynamoDbSettings,
    endpoint_endpointArn,
    endpoint_port,

    -- * EndpointSetting
    EndpointSetting (..),
    newEndpointSetting,
    endpointSetting_sensitive,
    endpointSetting_intValueMax,
    endpointSetting_applicability,
    endpointSetting_name,
    endpointSetting_intValueMin,
    endpointSetting_units,
    endpointSetting_defaultValue,
    endpointSetting_type,
    endpointSetting_enumValues,

    -- * Event
    Event (..),
    newEvent,
    event_sourceType,
    event_sourceIdentifier,
    event_date,
    event_eventCategories,
    event_message,

    -- * EventCategoryGroup
    EventCategoryGroup (..),
    newEventCategoryGroup,
    eventCategoryGroup_sourceType,
    eventCategoryGroup_eventCategories,

    -- * EventSubscription
    EventSubscription (..),
    newEventSubscription,
    eventSubscription_status,
    eventSubscription_customerAwsId,
    eventSubscription_custSubscriptionId,
    eventSubscription_snsTopicArn,
    eventSubscription_enabled,
    eventSubscription_sourceType,
    eventSubscription_subscriptionCreationTime,
    eventSubscription_eventCategoriesList,
    eventSubscription_sourceIdsList,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * IBMDb2Settings
    IBMDb2Settings (..),
    newIBMDb2Settings,
    iBMDb2Settings_serverName,
    iBMDb2Settings_currentLsn,
    iBMDb2Settings_setDataCaptureChanges,
    iBMDb2Settings_secretsManagerAccessRoleArn,
    iBMDb2Settings_username,
    iBMDb2Settings_password,
    iBMDb2Settings_databaseName,
    iBMDb2Settings_secretsManagerSecretId,
    iBMDb2Settings_maxKBytesPerRead,
    iBMDb2Settings_port,

    -- * KafkaSettings
    KafkaSettings (..),
    newKafkaSettings,
    kafkaSettings_sslClientKeyArn,
    kafkaSettings_includeTransactionDetails,
    kafkaSettings_includeTableAlterOperations,
    kafkaSettings_sslClientCertificateArn,
    kafkaSettings_sslCaCertificateArn,
    kafkaSettings_partitionIncludeSchemaTable,
    kafkaSettings_topic,
    kafkaSettings_includeControlDetails,
    kafkaSettings_noHexPrefix,
    kafkaSettings_saslPassword,
    kafkaSettings_sslClientKeyPassword,
    kafkaSettings_includePartitionValue,
    kafkaSettings_messageFormat,
    kafkaSettings_securityProtocol,
    kafkaSettings_saslUsername,
    kafkaSettings_broker,
    kafkaSettings_messageMaxBytes,
    kafkaSettings_includeNullAndEmpty,

    -- * KinesisSettings
    KinesisSettings (..),
    newKinesisSettings,
    kinesisSettings_includeTransactionDetails,
    kinesisSettings_includeTableAlterOperations,
    kinesisSettings_serviceAccessRoleArn,
    kinesisSettings_partitionIncludeSchemaTable,
    kinesisSettings_streamArn,
    kinesisSettings_includeControlDetails,
    kinesisSettings_noHexPrefix,
    kinesisSettings_includePartitionValue,
    kinesisSettings_messageFormat,
    kinesisSettings_includeNullAndEmpty,

    -- * MicrosoftSQLServerSettings
    MicrosoftSQLServerSettings (..),
    newMicrosoftSQLServerSettings,
    microsoftSQLServerSettings_bcpPacketSize,
    microsoftSQLServerSettings_useBcpFullLoad,
    microsoftSQLServerSettings_serverName,
    microsoftSQLServerSettings_querySingleAlwaysOnNode,
    microsoftSQLServerSettings_secretsManagerAccessRoleArn,
    microsoftSQLServerSettings_username,
    microsoftSQLServerSettings_safeguardPolicy,
    microsoftSQLServerSettings_password,
    microsoftSQLServerSettings_databaseName,
    microsoftSQLServerSettings_secretsManagerSecretId,
    microsoftSQLServerSettings_readBackupOnly,
    microsoftSQLServerSettings_useThirdPartyBackupDevice,
    microsoftSQLServerSettings_controlTablesFileGroup,
    microsoftSQLServerSettings_port,

    -- * MongoDbSettings
    MongoDbSettings (..),
    newMongoDbSettings,
    mongoDbSettings_serverName,
    mongoDbSettings_secretsManagerAccessRoleArn,
    mongoDbSettings_authMechanism,
    mongoDbSettings_username,
    mongoDbSettings_kmsKeyId,
    mongoDbSettings_password,
    mongoDbSettings_nestingLevel,
    mongoDbSettings_databaseName,
    mongoDbSettings_docsToInvestigate,
    mongoDbSettings_authSource,
    mongoDbSettings_secretsManagerSecretId,
    mongoDbSettings_extractDocId,
    mongoDbSettings_authType,
    mongoDbSettings_port,

    -- * MySQLSettings
    MySQLSettings (..),
    newMySQLSettings,
    mySQLSettings_maxFileSize,
    mySQLSettings_targetDbType,
    mySQLSettings_serverName,
    mySQLSettings_cleanSourceMetadataOnMismatch,
    mySQLSettings_parallelLoadThreads,
    mySQLSettings_secretsManagerAccessRoleArn,
    mySQLSettings_username,
    mySQLSettings_password,
    mySQLSettings_eventsPollInterval,
    mySQLSettings_databaseName,
    mySQLSettings_afterConnectScript,
    mySQLSettings_secretsManagerSecretId,
    mySQLSettings_serverTimezone,
    mySQLSettings_port,

    -- * NeptuneSettings
    NeptuneSettings (..),
    newNeptuneSettings,
    neptuneSettings_maxFileSize,
    neptuneSettings_maxRetryCount,
    neptuneSettings_serviceAccessRoleArn,
    neptuneSettings_iamAuthEnabled,
    neptuneSettings_errorRetryDuration,
    neptuneSettings_s3BucketName,
    neptuneSettings_s3BucketFolder,

    -- * OracleSettings
    OracleSettings (..),
    newOracleSettings,
    oracleSettings_useBFile,
    oracleSettings_standbyDelayTime,
    oracleSettings_failTasksOnLobTruncation,
    oracleSettings_serverName,
    oracleSettings_directPathNoLog,
    oracleSettings_extraArchivedLogDestIds,
    oracleSettings_securityDbEncryptionName,
    oracleSettings_oraclePathPrefix,
    oracleSettings_secretsManagerAccessRoleArn,
    oracleSettings_useDirectPathFullLoad,
    oracleSettings_username,
    oracleSettings_allowSelectNestedTables,
    oracleSettings_readAheadBlocks,
    oracleSettings_archivedLogDestId,
    oracleSettings_replacePathPrefix,
    oracleSettings_accessAlternateDirectly,
    oracleSettings_secretsManagerOracleAsmSecretId,
    oracleSettings_securityDbEncryption,
    oracleSettings_useLogminerReader,
    oracleSettings_readTableSpaceName,
    oracleSettings_retryInterval,
    oracleSettings_password,
    oracleSettings_spatialDataOptionToGeoJsonFunctionName,
    oracleSettings_databaseName,
    oracleSettings_addSupplementalLogging,
    oracleSettings_secretsManagerOracleAsmAccessRoleArn,
    oracleSettings_asmServer,
    oracleSettings_charLengthSemantics,
    oracleSettings_archivedLogsOnly,
    oracleSettings_directPathParallelLoad,
    oracleSettings_secretsManagerSecretId,
    oracleSettings_additionalArchivedLogDestId,
    oracleSettings_asmPassword,
    oracleSettings_enableHomogenousTablespace,
    oracleSettings_parallelAsmReadThreads,
    oracleSettings_numberDatatypeScale,
    oracleSettings_usePathPrefix,
    oracleSettings_asmUser,
    oracleSettings_useAlternateFolderForOnline,
    oracleSettings_port,

    -- * OrderableReplicationInstance
    OrderableReplicationInstance (..),
    newOrderableReplicationInstance,
    orderableReplicationInstance_engineVersion,
    orderableReplicationInstance_minAllocatedStorage,
    orderableReplicationInstance_releaseStatus,
    orderableReplicationInstance_includedAllocatedStorage,
    orderableReplicationInstance_availabilityZones,
    orderableReplicationInstance_maxAllocatedStorage,
    orderableReplicationInstance_replicationInstanceClass,
    orderableReplicationInstance_defaultAllocatedStorage,
    orderableReplicationInstance_storageType,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_currentApplyDate,

    -- * PostgreSQLSettings
    PostgreSQLSettings (..),
    newPostgreSQLSettings,
    postgreSQLSettings_executeTimeout,
    postgreSQLSettings_maxFileSize,
    postgreSQLSettings_failTasksOnLobTruncation,
    postgreSQLSettings_serverName,
    postgreSQLSettings_ddlArtifactsSchema,
    postgreSQLSettings_slotName,
    postgreSQLSettings_secretsManagerAccessRoleArn,
    postgreSQLSettings_username,
    postgreSQLSettings_heartbeatFrequency,
    postgreSQLSettings_password,
    postgreSQLSettings_databaseName,
    postgreSQLSettings_afterConnectScript,
    postgreSQLSettings_secretsManagerSecretId,
    postgreSQLSettings_captureDdls,
    postgreSQLSettings_pluginName,
    postgreSQLSettings_port,
    postgreSQLSettings_heartbeatSchema,
    postgreSQLSettings_heartbeatEnable,

    -- * RedisSettings
    RedisSettings (..),
    newRedisSettings,
    redisSettings_sslSecurityProtocol,
    redisSettings_authUserName,
    redisSettings_sslCaCertificateArn,
    redisSettings_authPassword,
    redisSettings_authType,
    redisSettings_serverName,
    redisSettings_port,

    -- * RedshiftSettings
    RedshiftSettings (..),
    newRedshiftSettings,
    redshiftSettings_emptyAsNull,
    redshiftSettings_caseSensitiveNames,
    redshiftSettings_maxFileSize,
    redshiftSettings_replaceChars,
    redshiftSettings_serverName,
    redshiftSettings_connectionTimeout,
    redshiftSettings_loadTimeout,
    redshiftSettings_serviceAccessRoleArn,
    redshiftSettings_explicitIds,
    redshiftSettings_bucketFolder,
    redshiftSettings_truncateColumns,
    redshiftSettings_secretsManagerAccessRoleArn,
    redshiftSettings_replaceInvalidChars,
    redshiftSettings_username,
    redshiftSettings_bucketName,
    redshiftSettings_encryptionMode,
    redshiftSettings_dateFormat,
    redshiftSettings_removeQuotes,
    redshiftSettings_password,
    redshiftSettings_databaseName,
    redshiftSettings_acceptAnyDate,
    redshiftSettings_afterConnectScript,
    redshiftSettings_secretsManagerSecretId,
    redshiftSettings_writeBufferSize,
    redshiftSettings_compUpdate,
    redshiftSettings_trimBlanks,
    redshiftSettings_timeFormat,
    redshiftSettings_serverSideEncryptionKmsKeyId,
    redshiftSettings_port,
    redshiftSettings_fileTransferUploadStreams,

    -- * RefreshSchemasStatus
    RefreshSchemasStatus (..),
    newRefreshSchemasStatus,
    refreshSchemasStatus_status,
    refreshSchemasStatus_lastRefreshDate,
    refreshSchemasStatus_replicationInstanceArn,
    refreshSchemasStatus_endpointArn,
    refreshSchemasStatus_lastFailureMessage,

    -- * ReplicationInstance
    ReplicationInstance (..),
    newReplicationInstance,
    replicationInstance_engineVersion,
    replicationInstance_publiclyAccessible,
    replicationInstance_autoMinorVersionUpgrade,
    replicationInstance_replicationInstancePublicIpAddresses,
    replicationInstance_replicationSubnetGroup,
    replicationInstance_instanceCreateTime,
    replicationInstance_freeUntil,
    replicationInstance_replicationInstanceStatus,
    replicationInstance_replicationInstancePrivateIpAddresses,
    replicationInstance_preferredMaintenanceWindow,
    replicationInstance_replicationInstancePrivateIpAddress,
    replicationInstance_kmsKeyId,
    replicationInstance_availabilityZone,
    replicationInstance_vpcSecurityGroups,
    replicationInstance_multiAZ,
    replicationInstance_secondaryAvailabilityZone,
    replicationInstance_replicationInstanceArn,
    replicationInstance_allocatedStorage,
    replicationInstance_dnsNameServers,
    replicationInstance_replicationInstancePublicIpAddress,
    replicationInstance_replicationInstanceClass,
    replicationInstance_replicationInstanceIdentifier,
    replicationInstance_pendingModifiedValues,

    -- * ReplicationInstanceTaskLog
    ReplicationInstanceTaskLog (..),
    newReplicationInstanceTaskLog,
    replicationInstanceTaskLog_replicationTaskName,
    replicationInstanceTaskLog_replicationTaskArn,
    replicationInstanceTaskLog_replicationInstanceTaskLogSize,

    -- * ReplicationPendingModifiedValues
    ReplicationPendingModifiedValues (..),
    newReplicationPendingModifiedValues,
    replicationPendingModifiedValues_engineVersion,
    replicationPendingModifiedValues_multiAZ,
    replicationPendingModifiedValues_allocatedStorage,
    replicationPendingModifiedValues_replicationInstanceClass,

    -- * ReplicationSubnetGroup
    ReplicationSubnetGroup (..),
    newReplicationSubnetGroup,
    replicationSubnetGroup_vpcId,
    replicationSubnetGroup_subnets,
    replicationSubnetGroup_replicationSubnetGroupIdentifier,
    replicationSubnetGroup_subnetGroupStatus,
    replicationSubnetGroup_replicationSubnetGroupDescription,

    -- * ReplicationTask
    ReplicationTask (..),
    newReplicationTask,
    replicationTask_replicationTaskSettings,
    replicationTask_status,
    replicationTask_stopReason,
    replicationTask_targetEndpointArn,
    replicationTask_replicationTaskIdentifier,
    replicationTask_cdcStartPosition,
    replicationTask_replicationTaskStartDate,
    replicationTask_sourceEndpointArn,
    replicationTask_recoveryCheckpoint,
    replicationTask_tableMappings,
    replicationTask_targetReplicationInstanceArn,
    replicationTask_replicationTaskCreationDate,
    replicationTask_migrationType,
    replicationTask_replicationTaskArn,
    replicationTask_taskData,
    replicationTask_cdcStopPosition,
    replicationTask_replicationTaskStats,
    replicationTask_replicationInstanceArn,
    replicationTask_lastFailureMessage,

    -- * ReplicationTaskAssessmentResult
    ReplicationTaskAssessmentResult (..),
    newReplicationTaskAssessmentResult,
    replicationTaskAssessmentResult_assessmentResults,
    replicationTaskAssessmentResult_assessmentResultsFile,
    replicationTaskAssessmentResult_replicationTaskIdentifier,
    replicationTaskAssessmentResult_assessmentStatus,
    replicationTaskAssessmentResult_s3ObjectUrl,
    replicationTaskAssessmentResult_replicationTaskLastAssessmentDate,
    replicationTaskAssessmentResult_replicationTaskArn,

    -- * ReplicationTaskAssessmentRun
    ReplicationTaskAssessmentRun (..),
    newReplicationTaskAssessmentRun,
    replicationTaskAssessmentRun_status,
    replicationTaskAssessmentRun_serviceAccessRoleArn,
    replicationTaskAssessmentRun_replicationTaskAssessmentRunCreationDate,
    replicationTaskAssessmentRun_assessmentProgress,
    replicationTaskAssessmentRun_resultKmsKeyArn,
    replicationTaskAssessmentRun_replicationTaskArn,
    replicationTaskAssessmentRun_resultLocationBucket,
    replicationTaskAssessmentRun_resultLocationFolder,
    replicationTaskAssessmentRun_resultEncryptionMode,
    replicationTaskAssessmentRun_assessmentRunName,
    replicationTaskAssessmentRun_replicationTaskAssessmentRunArn,
    replicationTaskAssessmentRun_lastFailureMessage,

    -- * ReplicationTaskAssessmentRunProgress
    ReplicationTaskAssessmentRunProgress (..),
    newReplicationTaskAssessmentRunProgress,
    replicationTaskAssessmentRunProgress_individualAssessmentCount,
    replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount,

    -- * ReplicationTaskIndividualAssessment
    ReplicationTaskIndividualAssessment (..),
    newReplicationTaskIndividualAssessment,
    replicationTaskIndividualAssessment_status,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate,
    replicationTaskIndividualAssessment_individualAssessmentName,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn,
    replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn,

    -- * ReplicationTaskStats
    ReplicationTaskStats (..),
    newReplicationTaskStats,
    replicationTaskStats_stopDate,
    replicationTaskStats_fullLoadProgressPercent,
    replicationTaskStats_fullLoadStartDate,
    replicationTaskStats_elapsedTimeMillis,
    replicationTaskStats_startDate,
    replicationTaskStats_tablesErrored,
    replicationTaskStats_fullLoadFinishDate,
    replicationTaskStats_tablesLoaded,
    replicationTaskStats_tablesQueued,
    replicationTaskStats_tablesLoading,
    replicationTaskStats_freshStartDate,

    -- * ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    newResourcePendingMaintenanceActions,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- * S3Settings
    S3Settings (..),
    newS3Settings,
    s3Settings_parquetVersion,
    s3Settings_preserveTransactions,
    s3Settings_maxFileSize,
    s3Settings_csvNoSupValue,
    s3Settings_rfc4180,
    s3Settings_parquetTimestampInMillisecond,
    s3Settings_includeOpForFullLoad,
    s3Settings_cdcMinFileSize,
    s3Settings_csvDelimiter,
    s3Settings_serviceAccessRoleArn,
    s3Settings_bucketFolder,
    s3Settings_dataFormat,
    s3Settings_datePartitionEnabled,
    s3Settings_encodingType,
    s3Settings_cdcMaxBatchInterval,
    s3Settings_ignoreHeaderRows,
    s3Settings_externalTableDefinition,
    s3Settings_dictPageSizeLimit,
    s3Settings_bucketName,
    s3Settings_encryptionMode,
    s3Settings_enableStatistics,
    s3Settings_cdcInsertsOnly,
    s3Settings_timestampColumnName,
    s3Settings_csvRowDelimiter,
    s3Settings_datePartitionDelimiter,
    s3Settings_addColumnName,
    s3Settings_cannedAclForObjects,
    s3Settings_compressionType,
    s3Settings_csvNullValue,
    s3Settings_serverSideEncryptionKmsKeyId,
    s3Settings_dataPageSize,
    s3Settings_useCsvNoSupValue,
    s3Settings_cdcInsertsAndUpdates,
    s3Settings_datePartitionSequence,
    s3Settings_rowGroupLength,
    s3Settings_cdcPath,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetStatus,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,

    -- * SupportedEndpointType
    SupportedEndpointType (..),
    newSupportedEndpointType,
    supportedEndpointType_engineDisplayName,
    supportedEndpointType_endpointType,
    supportedEndpointType_engineName,
    supportedEndpointType_replicationInstanceEngineMinimumVersion,
    supportedEndpointType_supportsCDC,

    -- * SybaseSettings
    SybaseSettings (..),
    newSybaseSettings,
    sybaseSettings_serverName,
    sybaseSettings_secretsManagerAccessRoleArn,
    sybaseSettings_username,
    sybaseSettings_password,
    sybaseSettings_databaseName,
    sybaseSettings_secretsManagerSecretId,
    sybaseSettings_port,

    -- * TableStatistics
    TableStatistics (..),
    newTableStatistics,
    tableStatistics_validationState,
    tableStatistics_fullLoadRows,
    tableStatistics_inserts,
    tableStatistics_fullLoadEndTime,
    tableStatistics_fullLoadCondtnlChkFailedRows,
    tableStatistics_fullLoadReloaded,
    tableStatistics_validationFailedRecords,
    tableStatistics_validationSuspendedRecords,
    tableStatistics_schemaName,
    tableStatistics_validationStateDetails,
    tableStatistics_tableState,
    tableStatistics_fullLoadErrorRows,
    tableStatistics_ddls,
    tableStatistics_deletes,
    tableStatistics_updates,
    tableStatistics_validationPendingRecords,
    tableStatistics_fullLoadStartTime,
    tableStatistics_lastUpdateTime,
    tableStatistics_tableName,

    -- * TableToReload
    TableToReload (..),
    newTableToReload,
    tableToReload_schemaName,
    tableToReload_tableName,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_resourceArn,
    tag_key,

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

-- | The ciphertext references a key that doesn\'t exist or that the DMS
-- account doesn\'t have access to.
_KMSAccessDeniedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSAccessDeniedFault =
  Core._MatchServiceError
    defaultService
    "KMSAccessDeniedFault"

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

-- | The subnet provided is invalid.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"

-- | DMS cannot access the KMS key.
_KMSKeyNotAccessibleFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSKeyNotAccessibleFault =
  Core._MatchServiceError
    defaultService
    "KMSKeyNotAccessibleFault"

-- | The replication subnet group does not cover enough Availability Zones
-- (AZs). Edit the replication subnet group and add more AZs.
_ReplicationSubnetGroupDoesNotCoverEnoughAZs :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationSubnetGroupDoesNotCoverEnoughAZs =
  Core._MatchServiceError
    defaultService
    "ReplicationSubnetGroupDoesNotCoverEnoughAZs"

-- | A specified Amazon S3 bucket, bucket folder, or other object can\'t be
-- found.
_S3ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_S3ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "S3ResourceNotFoundFault"

-- | The resource is in a state that prevents it from being used for database
-- migration.
_InvalidResourceStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidResourceStateFault"

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

-- | The resource you are attempting to create already exists.
_ResourceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsFault"

-- | There are not enough resources allocated to the database migration.
_InsufficientResourceCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientResourceCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientResourceCapacityFault"

-- | Insufficient privileges are preventing access to an Amazon S3 object.
_S3AccessDeniedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_S3AccessDeniedFault =
  Core._MatchServiceError
    defaultService
    "S3AccessDeniedFault"

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

-- | This request triggered KMS request throttling.
_KMSThrottlingFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSThrottlingFault =
  Core._MatchServiceError
    defaultService
    "KMSThrottlingFault"

-- | The quota for this resource quota has been exceeded.
_ResourceQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ResourceQuotaExceededFault"

-- | An upgrade dependency is preventing the database migration.
_UpgradeDependencyFailureFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UpgradeDependencyFailureFault =
  Core._MatchServiceError
    defaultService
    "UpgradeDependencyFailureFault"

-- | The resource could not be found.
_ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundFault"

-- | The storage quota has been exceeded.
_StorageQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "StorageQuotaExceededFault"

-- | DMS was denied access to the endpoint. Check that the role is correctly
-- configured.
_AccessDeniedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedFault =
  Core._MatchServiceError
    defaultService
    "AccessDeniedFault"

-- | The specified subnet is already in use.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"

-- | The state of the specified KMS resource isn\'t valid for this request.
_KMSInvalidStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateFault =
  Core._MatchServiceError
    defaultService
    "KMSInvalidStateFault"
