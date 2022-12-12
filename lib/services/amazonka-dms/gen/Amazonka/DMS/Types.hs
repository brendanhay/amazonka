{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DMS.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedFault,
    _CollectorNotFoundFault,
    _InsufficientResourceCapacityFault,
    _InvalidCertificateFault,
    _InvalidOperationFault,
    _InvalidResourceStateFault,
    _InvalidSubnet,
    _KMSAccessDeniedFault,
    _KMSDisabledFault,
    _KMSFault,
    _KMSInvalidStateFault,
    _KMSKeyNotAccessibleFault,
    _KMSNotFoundFault,
    _KMSThrottlingFault,
    _ReplicationSubnetGroupDoesNotCoverEnoughAZs,
    _ResourceAlreadyExistsFault,
    _ResourceNotFoundFault,
    _ResourceQuotaExceededFault,
    _S3AccessDeniedFault,
    _S3ResourceNotFoundFault,
    _SNSInvalidTopicFault,
    _SNSNoAuthorizationFault,
    _StorageQuotaExceededFault,
    _SubnetAlreadyInUse,
    _UpgradeDependencyFailureFault,

    -- * AuthMechanismValue
    AuthMechanismValue (..),

    -- * AuthTypeValue
    AuthTypeValue (..),

    -- * CannedAclForObjectsValue
    CannedAclForObjectsValue (..),

    -- * CharLengthSemantics
    CharLengthSemantics (..),

    -- * CollectorStatus
    CollectorStatus (..),

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

    -- * VersionStatus
    VersionStatus (..),

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

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_certificateArn,
    certificate_certificateCreationDate,
    certificate_certificateIdentifier,
    certificate_certificateOwner,
    certificate_certificatePem,
    certificate_certificateWallet,
    certificate_keyLength,
    certificate_signingAlgorithm,
    certificate_validFromDate,
    certificate_validToDate,

    -- * CollectorHealthCheck
    CollectorHealthCheck (..),
    newCollectorHealthCheck,
    collectorHealthCheck_collectorStatus,
    collectorHealthCheck_localCollectorS3Access,
    collectorHealthCheck_webCollectorGrantedRoleBasedAccess,
    collectorHealthCheck_webCollectorS3Access,

    -- * CollectorResponse
    CollectorResponse (..),
    newCollectorResponse,
    collectorResponse_collectorHealthCheck,
    collectorResponse_collectorName,
    collectorResponse_collectorReferencedId,
    collectorResponse_collectorVersion,
    collectorResponse_createdDate,
    collectorResponse_description,
    collectorResponse_inventoryData,
    collectorResponse_lastDataReceived,
    collectorResponse_modifiedDate,
    collectorResponse_registeredDate,
    collectorResponse_s3BucketName,
    collectorResponse_serviceAccessRoleArn,
    collectorResponse_versionStatus,

    -- * CollectorShortInfoResponse
    CollectorShortInfoResponse (..),
    newCollectorShortInfoResponse,
    collectorShortInfoResponse_collectorName,
    collectorShortInfoResponse_collectorReferencedId,

    -- * Connection
    Connection (..),
    newConnection,
    connection_endpointArn,
    connection_endpointIdentifier,
    connection_lastFailureMessage,
    connection_replicationInstanceArn,
    connection_replicationInstanceIdentifier,
    connection_status,

    -- * DatabaseInstanceSoftwareDetailsResponse
    DatabaseInstanceSoftwareDetailsResponse (..),
    newDatabaseInstanceSoftwareDetailsResponse,
    databaseInstanceSoftwareDetailsResponse_engine,
    databaseInstanceSoftwareDetailsResponse_engineEdition,
    databaseInstanceSoftwareDetailsResponse_engineVersion,
    databaseInstanceSoftwareDetailsResponse_osArchitecture,
    databaseInstanceSoftwareDetailsResponse_servicePack,
    databaseInstanceSoftwareDetailsResponse_supportLevel,
    databaseInstanceSoftwareDetailsResponse_tooltip,

    -- * DatabaseResponse
    DatabaseResponse (..),
    newDatabaseResponse,
    databaseResponse_collectors,
    databaseResponse_databaseId,
    databaseResponse_databaseName,
    databaseResponse_ipAddress,
    databaseResponse_numberOfSchemas,
    databaseResponse_server,
    databaseResponse_softwareDetails,

    -- * DatabaseShortInfoResponse
    DatabaseShortInfoResponse (..),
    newDatabaseShortInfoResponse,
    databaseShortInfoResponse_databaseEngine,
    databaseShortInfoResponse_databaseId,
    databaseShortInfoResponse_databaseIpAddress,
    databaseShortInfoResponse_databaseName,

    -- * DmsTransferSettings
    DmsTransferSettings (..),
    newDmsTransferSettings,
    dmsTransferSettings_bucketName,
    dmsTransferSettings_serviceAccessRoleArn,

    -- * DocDbSettings
    DocDbSettings (..),
    newDocDbSettings,
    docDbSettings_databaseName,
    docDbSettings_docsToInvestigate,
    docDbSettings_extractDocId,
    docDbSettings_kmsKeyId,
    docDbSettings_nestingLevel,
    docDbSettings_password,
    docDbSettings_port,
    docDbSettings_secretsManagerAccessRoleArn,
    docDbSettings_secretsManagerSecretId,
    docDbSettings_serverName,
    docDbSettings_username,

    -- * DynamoDbSettings
    DynamoDbSettings (..),
    newDynamoDbSettings,
    dynamoDbSettings_serviceAccessRoleArn,

    -- * ElasticsearchSettings
    ElasticsearchSettings (..),
    newElasticsearchSettings,
    elasticsearchSettings_errorRetryDuration,
    elasticsearchSettings_fullLoadErrorPercentage,
    elasticsearchSettings_useNewMappingType,
    elasticsearchSettings_serviceAccessRoleArn,
    elasticsearchSettings_endpointUri,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_certificateArn,
    endpoint_databaseName,
    endpoint_dmsTransferSettings,
    endpoint_docDbSettings,
    endpoint_dynamoDbSettings,
    endpoint_elasticsearchSettings,
    endpoint_endpointArn,
    endpoint_endpointIdentifier,
    endpoint_endpointType,
    endpoint_engineDisplayName,
    endpoint_engineName,
    endpoint_externalId,
    endpoint_externalTableDefinition,
    endpoint_extraConnectionAttributes,
    endpoint_gcpMySQLSettings,
    endpoint_iBMDb2Settings,
    endpoint_kafkaSettings,
    endpoint_kinesisSettings,
    endpoint_kmsKeyId,
    endpoint_microsoftSQLServerSettings,
    endpoint_mongoDbSettings,
    endpoint_mySQLSettings,
    endpoint_neptuneSettings,
    endpoint_oracleSettings,
    endpoint_port,
    endpoint_postgreSQLSettings,
    endpoint_redisSettings,
    endpoint_redshiftSettings,
    endpoint_s3Settings,
    endpoint_serverName,
    endpoint_serviceAccessRoleArn,
    endpoint_sslMode,
    endpoint_status,
    endpoint_sybaseSettings,
    endpoint_username,

    -- * EndpointSetting
    EndpointSetting (..),
    newEndpointSetting,
    endpointSetting_applicability,
    endpointSetting_defaultValue,
    endpointSetting_enumValues,
    endpointSetting_intValueMax,
    endpointSetting_intValueMin,
    endpointSetting_name,
    endpointSetting_sensitive,
    endpointSetting_type,
    endpointSetting_units,

    -- * Event
    Event (..),
    newEvent,
    event_date,
    event_eventCategories,
    event_message,
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
    eventSubscription_customerAwsId,
    eventSubscription_enabled,
    eventSubscription_eventCategoriesList,
    eventSubscription_snsTopicArn,
    eventSubscription_sourceIdsList,
    eventSubscription_sourceType,
    eventSubscription_status,
    eventSubscription_subscriptionCreationTime,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * FleetAdvisorLsaAnalysisResponse
    FleetAdvisorLsaAnalysisResponse (..),
    newFleetAdvisorLsaAnalysisResponse,
    fleetAdvisorLsaAnalysisResponse_lsaAnalysisId,
    fleetAdvisorLsaAnalysisResponse_status,

    -- * FleetAdvisorSchemaObjectResponse
    FleetAdvisorSchemaObjectResponse (..),
    newFleetAdvisorSchemaObjectResponse,
    fleetAdvisorSchemaObjectResponse_codeLineCount,
    fleetAdvisorSchemaObjectResponse_codeSize,
    fleetAdvisorSchemaObjectResponse_numberOfObjects,
    fleetAdvisorSchemaObjectResponse_objectType,
    fleetAdvisorSchemaObjectResponse_schemaId,

    -- * GcpMySQLSettings
    GcpMySQLSettings (..),
    newGcpMySQLSettings,
    gcpMySQLSettings_afterConnectScript,
    gcpMySQLSettings_cleanSourceMetadataOnMismatch,
    gcpMySQLSettings_databaseName,
    gcpMySQLSettings_eventsPollInterval,
    gcpMySQLSettings_maxFileSize,
    gcpMySQLSettings_parallelLoadThreads,
    gcpMySQLSettings_password,
    gcpMySQLSettings_port,
    gcpMySQLSettings_secretsManagerAccessRoleArn,
    gcpMySQLSettings_secretsManagerSecretId,
    gcpMySQLSettings_serverName,
    gcpMySQLSettings_serverTimezone,
    gcpMySQLSettings_targetDbType,
    gcpMySQLSettings_username,

    -- * IBMDb2Settings
    IBMDb2Settings (..),
    newIBMDb2Settings,
    iBMDb2Settings_currentLsn,
    iBMDb2Settings_databaseName,
    iBMDb2Settings_maxKBytesPerRead,
    iBMDb2Settings_password,
    iBMDb2Settings_port,
    iBMDb2Settings_secretsManagerAccessRoleArn,
    iBMDb2Settings_secretsManagerSecretId,
    iBMDb2Settings_serverName,
    iBMDb2Settings_setDataCaptureChanges,
    iBMDb2Settings_username,

    -- * InventoryData
    InventoryData (..),
    newInventoryData,
    inventoryData_numberOfDatabases,
    inventoryData_numberOfSchemas,

    -- * KafkaSettings
    KafkaSettings (..),
    newKafkaSettings,
    kafkaSettings_broker,
    kafkaSettings_includeControlDetails,
    kafkaSettings_includeNullAndEmpty,
    kafkaSettings_includePartitionValue,
    kafkaSettings_includeTableAlterOperations,
    kafkaSettings_includeTransactionDetails,
    kafkaSettings_messageFormat,
    kafkaSettings_messageMaxBytes,
    kafkaSettings_noHexPrefix,
    kafkaSettings_partitionIncludeSchemaTable,
    kafkaSettings_saslPassword,
    kafkaSettings_saslUsername,
    kafkaSettings_securityProtocol,
    kafkaSettings_sslCaCertificateArn,
    kafkaSettings_sslClientCertificateArn,
    kafkaSettings_sslClientKeyArn,
    kafkaSettings_sslClientKeyPassword,
    kafkaSettings_topic,

    -- * KinesisSettings
    KinesisSettings (..),
    newKinesisSettings,
    kinesisSettings_includeControlDetails,
    kinesisSettings_includeNullAndEmpty,
    kinesisSettings_includePartitionValue,
    kinesisSettings_includeTableAlterOperations,
    kinesisSettings_includeTransactionDetails,
    kinesisSettings_messageFormat,
    kinesisSettings_noHexPrefix,
    kinesisSettings_partitionIncludeSchemaTable,
    kinesisSettings_serviceAccessRoleArn,
    kinesisSettings_streamArn,

    -- * MicrosoftSQLServerSettings
    MicrosoftSQLServerSettings (..),
    newMicrosoftSQLServerSettings,
    microsoftSQLServerSettings_bcpPacketSize,
    microsoftSQLServerSettings_controlTablesFileGroup,
    microsoftSQLServerSettings_databaseName,
    microsoftSQLServerSettings_password,
    microsoftSQLServerSettings_port,
    microsoftSQLServerSettings_querySingleAlwaysOnNode,
    microsoftSQLServerSettings_readBackupOnly,
    microsoftSQLServerSettings_safeguardPolicy,
    microsoftSQLServerSettings_secretsManagerAccessRoleArn,
    microsoftSQLServerSettings_secretsManagerSecretId,
    microsoftSQLServerSettings_serverName,
    microsoftSQLServerSettings_trimSpaceInChar,
    microsoftSQLServerSettings_useBcpFullLoad,
    microsoftSQLServerSettings_useThirdPartyBackupDevice,
    microsoftSQLServerSettings_username,

    -- * MongoDbSettings
    MongoDbSettings (..),
    newMongoDbSettings,
    mongoDbSettings_authMechanism,
    mongoDbSettings_authSource,
    mongoDbSettings_authType,
    mongoDbSettings_databaseName,
    mongoDbSettings_docsToInvestigate,
    mongoDbSettings_extractDocId,
    mongoDbSettings_kmsKeyId,
    mongoDbSettings_nestingLevel,
    mongoDbSettings_password,
    mongoDbSettings_port,
    mongoDbSettings_secretsManagerAccessRoleArn,
    mongoDbSettings_secretsManagerSecretId,
    mongoDbSettings_serverName,
    mongoDbSettings_username,

    -- * MySQLSettings
    MySQLSettings (..),
    newMySQLSettings,
    mySQLSettings_afterConnectScript,
    mySQLSettings_cleanSourceMetadataOnMismatch,
    mySQLSettings_databaseName,
    mySQLSettings_eventsPollInterval,
    mySQLSettings_maxFileSize,
    mySQLSettings_parallelLoadThreads,
    mySQLSettings_password,
    mySQLSettings_port,
    mySQLSettings_secretsManagerAccessRoleArn,
    mySQLSettings_secretsManagerSecretId,
    mySQLSettings_serverName,
    mySQLSettings_serverTimezone,
    mySQLSettings_targetDbType,
    mySQLSettings_username,

    -- * NeptuneSettings
    NeptuneSettings (..),
    newNeptuneSettings,
    neptuneSettings_errorRetryDuration,
    neptuneSettings_iamAuthEnabled,
    neptuneSettings_maxFileSize,
    neptuneSettings_maxRetryCount,
    neptuneSettings_serviceAccessRoleArn,
    neptuneSettings_s3BucketName,
    neptuneSettings_s3BucketFolder,

    -- * OracleSettings
    OracleSettings (..),
    newOracleSettings,
    oracleSettings_accessAlternateDirectly,
    oracleSettings_addSupplementalLogging,
    oracleSettings_additionalArchivedLogDestId,
    oracleSettings_allowSelectNestedTables,
    oracleSettings_archivedLogDestId,
    oracleSettings_archivedLogsOnly,
    oracleSettings_asmPassword,
    oracleSettings_asmServer,
    oracleSettings_asmUser,
    oracleSettings_charLengthSemantics,
    oracleSettings_databaseName,
    oracleSettings_directPathNoLog,
    oracleSettings_directPathParallelLoad,
    oracleSettings_enableHomogenousTablespace,
    oracleSettings_extraArchivedLogDestIds,
    oracleSettings_failTasksOnLobTruncation,
    oracleSettings_numberDatatypeScale,
    oracleSettings_oraclePathPrefix,
    oracleSettings_parallelAsmReadThreads,
    oracleSettings_password,
    oracleSettings_port,
    oracleSettings_readAheadBlocks,
    oracleSettings_readTableSpaceName,
    oracleSettings_replacePathPrefix,
    oracleSettings_retryInterval,
    oracleSettings_secretsManagerAccessRoleArn,
    oracleSettings_secretsManagerOracleAsmAccessRoleArn,
    oracleSettings_secretsManagerOracleAsmSecretId,
    oracleSettings_secretsManagerSecretId,
    oracleSettings_securityDbEncryption,
    oracleSettings_securityDbEncryptionName,
    oracleSettings_serverName,
    oracleSettings_spatialDataOptionToGeoJsonFunctionName,
    oracleSettings_standbyDelayTime,
    oracleSettings_trimSpaceInChar,
    oracleSettings_useAlternateFolderForOnline,
    oracleSettings_useBFile,
    oracleSettings_useDirectPathFullLoad,
    oracleSettings_useLogminerReader,
    oracleSettings_usePathPrefix,
    oracleSettings_username,

    -- * OrderableReplicationInstance
    OrderableReplicationInstance (..),
    newOrderableReplicationInstance,
    orderableReplicationInstance_availabilityZones,
    orderableReplicationInstance_defaultAllocatedStorage,
    orderableReplicationInstance_engineVersion,
    orderableReplicationInstance_includedAllocatedStorage,
    orderableReplicationInstance_maxAllocatedStorage,
    orderableReplicationInstance_minAllocatedStorage,
    orderableReplicationInstance_releaseStatus,
    orderableReplicationInstance_replicationInstanceClass,
    orderableReplicationInstance_storageType,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_optInStatus,

    -- * PostgreSQLSettings
    PostgreSQLSettings (..),
    newPostgreSQLSettings,
    postgreSQLSettings_afterConnectScript,
    postgreSQLSettings_captureDdls,
    postgreSQLSettings_databaseName,
    postgreSQLSettings_ddlArtifactsSchema,
    postgreSQLSettings_executeTimeout,
    postgreSQLSettings_failTasksOnLobTruncation,
    postgreSQLSettings_heartbeatEnable,
    postgreSQLSettings_heartbeatFrequency,
    postgreSQLSettings_heartbeatSchema,
    postgreSQLSettings_maxFileSize,
    postgreSQLSettings_password,
    postgreSQLSettings_pluginName,
    postgreSQLSettings_port,
    postgreSQLSettings_secretsManagerAccessRoleArn,
    postgreSQLSettings_secretsManagerSecretId,
    postgreSQLSettings_serverName,
    postgreSQLSettings_slotName,
    postgreSQLSettings_trimSpaceInChar,
    postgreSQLSettings_username,

    -- * RedisSettings
    RedisSettings (..),
    newRedisSettings,
    redisSettings_authPassword,
    redisSettings_authType,
    redisSettings_authUserName,
    redisSettings_sslCaCertificateArn,
    redisSettings_sslSecurityProtocol,
    redisSettings_serverName,
    redisSettings_port,

    -- * RedshiftSettings
    RedshiftSettings (..),
    newRedshiftSettings,
    redshiftSettings_acceptAnyDate,
    redshiftSettings_afterConnectScript,
    redshiftSettings_bucketFolder,
    redshiftSettings_bucketName,
    redshiftSettings_caseSensitiveNames,
    redshiftSettings_compUpdate,
    redshiftSettings_connectionTimeout,
    redshiftSettings_databaseName,
    redshiftSettings_dateFormat,
    redshiftSettings_emptyAsNull,
    redshiftSettings_encryptionMode,
    redshiftSettings_explicitIds,
    redshiftSettings_fileTransferUploadStreams,
    redshiftSettings_loadTimeout,
    redshiftSettings_maxFileSize,
    redshiftSettings_password,
    redshiftSettings_port,
    redshiftSettings_removeQuotes,
    redshiftSettings_replaceChars,
    redshiftSettings_replaceInvalidChars,
    redshiftSettings_secretsManagerAccessRoleArn,
    redshiftSettings_secretsManagerSecretId,
    redshiftSettings_serverName,
    redshiftSettings_serverSideEncryptionKmsKeyId,
    redshiftSettings_serviceAccessRoleArn,
    redshiftSettings_timeFormat,
    redshiftSettings_trimBlanks,
    redshiftSettings_truncateColumns,
    redshiftSettings_username,
    redshiftSettings_writeBufferSize,

    -- * RefreshSchemasStatus
    RefreshSchemasStatus (..),
    newRefreshSchemasStatus,
    refreshSchemasStatus_endpointArn,
    refreshSchemasStatus_lastFailureMessage,
    refreshSchemasStatus_lastRefreshDate,
    refreshSchemasStatus_replicationInstanceArn,
    refreshSchemasStatus_status,

    -- * ReplicationInstance
    ReplicationInstance (..),
    newReplicationInstance,
    replicationInstance_allocatedStorage,
    replicationInstance_autoMinorVersionUpgrade,
    replicationInstance_availabilityZone,
    replicationInstance_dnsNameServers,
    replicationInstance_engineVersion,
    replicationInstance_freeUntil,
    replicationInstance_instanceCreateTime,
    replicationInstance_kmsKeyId,
    replicationInstance_multiAZ,
    replicationInstance_networkType,
    replicationInstance_pendingModifiedValues,
    replicationInstance_preferredMaintenanceWindow,
    replicationInstance_publiclyAccessible,
    replicationInstance_replicationInstanceArn,
    replicationInstance_replicationInstanceClass,
    replicationInstance_replicationInstanceIdentifier,
    replicationInstance_replicationInstanceIpv6Addresses,
    replicationInstance_replicationInstancePrivateIpAddress,
    replicationInstance_replicationInstancePrivateIpAddresses,
    replicationInstance_replicationInstancePublicIpAddress,
    replicationInstance_replicationInstancePublicIpAddresses,
    replicationInstance_replicationInstanceStatus,
    replicationInstance_replicationSubnetGroup,
    replicationInstance_secondaryAvailabilityZone,
    replicationInstance_vpcSecurityGroups,

    -- * ReplicationInstanceTaskLog
    ReplicationInstanceTaskLog (..),
    newReplicationInstanceTaskLog,
    replicationInstanceTaskLog_replicationInstanceTaskLogSize,
    replicationInstanceTaskLog_replicationTaskArn,
    replicationInstanceTaskLog_replicationTaskName,

    -- * ReplicationPendingModifiedValues
    ReplicationPendingModifiedValues (..),
    newReplicationPendingModifiedValues,
    replicationPendingModifiedValues_allocatedStorage,
    replicationPendingModifiedValues_engineVersion,
    replicationPendingModifiedValues_multiAZ,
    replicationPendingModifiedValues_networkType,
    replicationPendingModifiedValues_replicationInstanceClass,

    -- * ReplicationSubnetGroup
    ReplicationSubnetGroup (..),
    newReplicationSubnetGroup,
    replicationSubnetGroup_replicationSubnetGroupDescription,
    replicationSubnetGroup_replicationSubnetGroupIdentifier,
    replicationSubnetGroup_subnetGroupStatus,
    replicationSubnetGroup_subnets,
    replicationSubnetGroup_supportedNetworkTypes,
    replicationSubnetGroup_vpcId,

    -- * ReplicationTask
    ReplicationTask (..),
    newReplicationTask,
    replicationTask_cdcStartPosition,
    replicationTask_cdcStopPosition,
    replicationTask_lastFailureMessage,
    replicationTask_migrationType,
    replicationTask_recoveryCheckpoint,
    replicationTask_replicationInstanceArn,
    replicationTask_replicationTaskArn,
    replicationTask_replicationTaskCreationDate,
    replicationTask_replicationTaskIdentifier,
    replicationTask_replicationTaskSettings,
    replicationTask_replicationTaskStartDate,
    replicationTask_replicationTaskStats,
    replicationTask_sourceEndpointArn,
    replicationTask_status,
    replicationTask_stopReason,
    replicationTask_tableMappings,
    replicationTask_targetEndpointArn,
    replicationTask_targetReplicationInstanceArn,
    replicationTask_taskData,

    -- * ReplicationTaskAssessmentResult
    ReplicationTaskAssessmentResult (..),
    newReplicationTaskAssessmentResult,
    replicationTaskAssessmentResult_assessmentResults,
    replicationTaskAssessmentResult_assessmentResultsFile,
    replicationTaskAssessmentResult_assessmentStatus,
    replicationTaskAssessmentResult_replicationTaskArn,
    replicationTaskAssessmentResult_replicationTaskIdentifier,
    replicationTaskAssessmentResult_replicationTaskLastAssessmentDate,
    replicationTaskAssessmentResult_s3ObjectUrl,

    -- * ReplicationTaskAssessmentRun
    ReplicationTaskAssessmentRun (..),
    newReplicationTaskAssessmentRun,
    replicationTaskAssessmentRun_assessmentProgress,
    replicationTaskAssessmentRun_assessmentRunName,
    replicationTaskAssessmentRun_lastFailureMessage,
    replicationTaskAssessmentRun_replicationTaskArn,
    replicationTaskAssessmentRun_replicationTaskAssessmentRunArn,
    replicationTaskAssessmentRun_replicationTaskAssessmentRunCreationDate,
    replicationTaskAssessmentRun_resultEncryptionMode,
    replicationTaskAssessmentRun_resultKmsKeyArn,
    replicationTaskAssessmentRun_resultLocationBucket,
    replicationTaskAssessmentRun_resultLocationFolder,
    replicationTaskAssessmentRun_serviceAccessRoleArn,
    replicationTaskAssessmentRun_status,

    -- * ReplicationTaskAssessmentRunProgress
    ReplicationTaskAssessmentRunProgress (..),
    newReplicationTaskAssessmentRunProgress,
    replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount,
    replicationTaskAssessmentRunProgress_individualAssessmentCount,

    -- * ReplicationTaskIndividualAssessment
    ReplicationTaskIndividualAssessment (..),
    newReplicationTaskIndividualAssessment,
    replicationTaskIndividualAssessment_individualAssessmentName,
    replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate,
    replicationTaskIndividualAssessment_status,

    -- * ReplicationTaskStats
    ReplicationTaskStats (..),
    newReplicationTaskStats,
    replicationTaskStats_elapsedTimeMillis,
    replicationTaskStats_freshStartDate,
    replicationTaskStats_fullLoadFinishDate,
    replicationTaskStats_fullLoadProgressPercent,
    replicationTaskStats_fullLoadStartDate,
    replicationTaskStats_startDate,
    replicationTaskStats_stopDate,
    replicationTaskStats_tablesErrored,
    replicationTaskStats_tablesLoaded,
    replicationTaskStats_tablesLoading,
    replicationTaskStats_tablesQueued,

    -- * ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    newResourcePendingMaintenanceActions,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- * S3Settings
    S3Settings (..),
    newS3Settings,
    s3Settings_addColumnName,
    s3Settings_addTrailingPaddingCharacter,
    s3Settings_bucketFolder,
    s3Settings_bucketName,
    s3Settings_cannedAclForObjects,
    s3Settings_cdcInsertsAndUpdates,
    s3Settings_cdcInsertsOnly,
    s3Settings_cdcMaxBatchInterval,
    s3Settings_cdcMinFileSize,
    s3Settings_cdcPath,
    s3Settings_compressionType,
    s3Settings_csvDelimiter,
    s3Settings_csvNoSupValue,
    s3Settings_csvNullValue,
    s3Settings_csvRowDelimiter,
    s3Settings_dataFormat,
    s3Settings_dataPageSize,
    s3Settings_datePartitionDelimiter,
    s3Settings_datePartitionEnabled,
    s3Settings_datePartitionSequence,
    s3Settings_datePartitionTimezone,
    s3Settings_dictPageSizeLimit,
    s3Settings_enableStatistics,
    s3Settings_encodingType,
    s3Settings_encryptionMode,
    s3Settings_expectedBucketOwner,
    s3Settings_externalTableDefinition,
    s3Settings_ignoreHeaderRows,
    s3Settings_includeOpForFullLoad,
    s3Settings_maxFileSize,
    s3Settings_parquetTimestampInMillisecond,
    s3Settings_parquetVersion,
    s3Settings_preserveTransactions,
    s3Settings_rfc4180,
    s3Settings_rowGroupLength,
    s3Settings_serverSideEncryptionKmsKeyId,
    s3Settings_serviceAccessRoleArn,
    s3Settings_timestampColumnName,
    s3Settings_useCsvNoSupValue,
    s3Settings_useTaskStartTimeForFullLoadTimestamp,

    -- * SchemaResponse
    SchemaResponse (..),
    newSchemaResponse,
    schemaResponse_codeLineCount,
    schemaResponse_codeSize,
    schemaResponse_complexity,
    schemaResponse_databaseInstance,
    schemaResponse_originalSchema,
    schemaResponse_schemaId,
    schemaResponse_schemaName,
    schemaResponse_server,
    schemaResponse_similarity,

    -- * SchemaShortInfoResponse
    SchemaShortInfoResponse (..),
    newSchemaShortInfoResponse,
    schemaShortInfoResponse_databaseId,
    schemaShortInfoResponse_databaseIpAddress,
    schemaShortInfoResponse_databaseName,
    schemaShortInfoResponse_schemaId,
    schemaShortInfoResponse_schemaName,

    -- * ServerShortInfoResponse
    ServerShortInfoResponse (..),
    newServerShortInfoResponse,
    serverShortInfoResponse_ipAddress,
    serverShortInfoResponse_serverId,
    serverShortInfoResponse_serverName,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,
    subnet_subnetStatus,

    -- * SupportedEndpointType
    SupportedEndpointType (..),
    newSupportedEndpointType,
    supportedEndpointType_endpointType,
    supportedEndpointType_engineDisplayName,
    supportedEndpointType_engineName,
    supportedEndpointType_replicationInstanceEngineMinimumVersion,
    supportedEndpointType_supportsCDC,

    -- * SybaseSettings
    SybaseSettings (..),
    newSybaseSettings,
    sybaseSettings_databaseName,
    sybaseSettings_password,
    sybaseSettings_port,
    sybaseSettings_secretsManagerAccessRoleArn,
    sybaseSettings_secretsManagerSecretId,
    sybaseSettings_serverName,
    sybaseSettings_username,

    -- * TableStatistics
    TableStatistics (..),
    newTableStatistics,
    tableStatistics_appliedDdls,
    tableStatistics_appliedDeletes,
    tableStatistics_appliedInserts,
    tableStatistics_appliedUpdates,
    tableStatistics_ddls,
    tableStatistics_deletes,
    tableStatistics_fullLoadCondtnlChkFailedRows,
    tableStatistics_fullLoadEndTime,
    tableStatistics_fullLoadErrorRows,
    tableStatistics_fullLoadReloaded,
    tableStatistics_fullLoadRows,
    tableStatistics_fullLoadStartTime,
    tableStatistics_inserts,
    tableStatistics_lastUpdateTime,
    tableStatistics_schemaName,
    tableStatistics_tableName,
    tableStatistics_tableState,
    tableStatistics_updates,
    tableStatistics_validationFailedRecords,
    tableStatistics_validationPendingRecords,
    tableStatistics_validationState,
    tableStatistics_validationStateDetails,
    tableStatistics_validationSuspendedRecords,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.AccountQuota
import Amazonka.DMS.Types.AuthMechanismValue
import Amazonka.DMS.Types.AuthTypeValue
import Amazonka.DMS.Types.AvailabilityZone
import Amazonka.DMS.Types.CannedAclForObjectsValue
import Amazonka.DMS.Types.Certificate
import Amazonka.DMS.Types.CharLengthSemantics
import Amazonka.DMS.Types.CollectorHealthCheck
import Amazonka.DMS.Types.CollectorResponse
import Amazonka.DMS.Types.CollectorShortInfoResponse
import Amazonka.DMS.Types.CollectorStatus
import Amazonka.DMS.Types.CompressionTypeValue
import Amazonka.DMS.Types.Connection
import Amazonka.DMS.Types.DataFormatValue
import Amazonka.DMS.Types.DatabaseInstanceSoftwareDetailsResponse
import Amazonka.DMS.Types.DatabaseResponse
import Amazonka.DMS.Types.DatabaseShortInfoResponse
import Amazonka.DMS.Types.DatePartitionDelimiterValue
import Amazonka.DMS.Types.DatePartitionSequenceValue
import Amazonka.DMS.Types.DmsSslModeValue
import Amazonka.DMS.Types.DmsTransferSettings
import Amazonka.DMS.Types.DocDbSettings
import Amazonka.DMS.Types.DynamoDbSettings
import Amazonka.DMS.Types.ElasticsearchSettings
import Amazonka.DMS.Types.EncodingTypeValue
import Amazonka.DMS.Types.EncryptionModeValue
import Amazonka.DMS.Types.Endpoint
import Amazonka.DMS.Types.EndpointSetting
import Amazonka.DMS.Types.EndpointSettingTypeValue
import Amazonka.DMS.Types.Event
import Amazonka.DMS.Types.EventCategoryGroup
import Amazonka.DMS.Types.EventSubscription
import Amazonka.DMS.Types.Filter
import Amazonka.DMS.Types.FleetAdvisorLsaAnalysisResponse
import Amazonka.DMS.Types.FleetAdvisorSchemaObjectResponse
import Amazonka.DMS.Types.GcpMySQLSettings
import Amazonka.DMS.Types.IBMDb2Settings
import Amazonka.DMS.Types.InventoryData
import Amazonka.DMS.Types.KafkaSecurityProtocol
import Amazonka.DMS.Types.KafkaSettings
import Amazonka.DMS.Types.KinesisSettings
import Amazonka.DMS.Types.MessageFormatValue
import Amazonka.DMS.Types.MicrosoftSQLServerSettings
import Amazonka.DMS.Types.MigrationTypeValue
import Amazonka.DMS.Types.MongoDbSettings
import Amazonka.DMS.Types.MySQLSettings
import Amazonka.DMS.Types.NeptuneSettings
import Amazonka.DMS.Types.NestingLevelValue
import Amazonka.DMS.Types.OracleSettings
import Amazonka.DMS.Types.OrderableReplicationInstance
import Amazonka.DMS.Types.ParquetVersionValue
import Amazonka.DMS.Types.PendingMaintenanceAction
import Amazonka.DMS.Types.PluginNameValue
import Amazonka.DMS.Types.PostgreSQLSettings
import Amazonka.DMS.Types.RedisAuthTypeValue
import Amazonka.DMS.Types.RedisSettings
import Amazonka.DMS.Types.RedshiftSettings
import Amazonka.DMS.Types.RefreshSchemasStatus
import Amazonka.DMS.Types.RefreshSchemasStatusTypeValue
import Amazonka.DMS.Types.ReleaseStatusValues
import Amazonka.DMS.Types.ReloadOptionValue
import Amazonka.DMS.Types.ReplicationEndpointTypeValue
import Amazonka.DMS.Types.ReplicationInstance
import Amazonka.DMS.Types.ReplicationInstanceTaskLog
import Amazonka.DMS.Types.ReplicationPendingModifiedValues
import Amazonka.DMS.Types.ReplicationSubnetGroup
import Amazonka.DMS.Types.ReplicationTask
import Amazonka.DMS.Types.ReplicationTaskAssessmentResult
import Amazonka.DMS.Types.ReplicationTaskAssessmentRun
import Amazonka.DMS.Types.ReplicationTaskAssessmentRunProgress
import Amazonka.DMS.Types.ReplicationTaskIndividualAssessment
import Amazonka.DMS.Types.ReplicationTaskStats
import Amazonka.DMS.Types.ResourcePendingMaintenanceActions
import Amazonka.DMS.Types.S3Settings
import Amazonka.DMS.Types.SafeguardPolicy
import Amazonka.DMS.Types.SchemaResponse
import Amazonka.DMS.Types.SchemaShortInfoResponse
import Amazonka.DMS.Types.ServerShortInfoResponse
import Amazonka.DMS.Types.SourceType
import Amazonka.DMS.Types.SslSecurityProtocolValue
import Amazonka.DMS.Types.StartReplicationTaskTypeValue
import Amazonka.DMS.Types.Subnet
import Amazonka.DMS.Types.SupportedEndpointType
import Amazonka.DMS.Types.SybaseSettings
import Amazonka.DMS.Types.TableStatistics
import Amazonka.DMS.Types.TableToReload
import Amazonka.DMS.Types.Tag
import Amazonka.DMS.Types.TargetDbType
import Amazonka.DMS.Types.VersionStatus
import Amazonka.DMS.Types.VpcSecurityGroupMembership
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-01-01@ of the Amazon Database Migration Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DMS",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "dms",
      Core.signingName = "dms",
      Core.version = "2016-01-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DMS",
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

-- | DMS was denied access to the endpoint. Check that the role is correctly
-- configured.
_AccessDeniedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedFault =
  Core._MatchServiceError
    defaultService
    "AccessDeniedFault"

-- | The specified collector doesn\'t exist.
_CollectorNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CollectorNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CollectorNotFoundFault"

-- | There are not enough resources allocated to the database migration.
_InsufficientResourceCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientResourceCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientResourceCapacityFault"

-- | The certificate was not valid.
_InvalidCertificateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCertificateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCertificateFault"

-- | The action or operation requested isn\'t valid.
_InvalidOperationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationFault =
  Core._MatchServiceError
    defaultService
    "InvalidOperationFault"

-- | The resource is in a state that prevents it from being used for database
-- migration.
_InvalidResourceStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidResourceStateFault"

-- | The subnet provided is invalid.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"

-- | The ciphertext references a key that doesn\'t exist or that the DMS
-- account doesn\'t have access to.
_KMSAccessDeniedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSAccessDeniedFault =
  Core._MatchServiceError
    defaultService
    "KMSAccessDeniedFault"

-- | The specified KMS key isn\'t enabled.
_KMSDisabledFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSDisabledFault =
  Core._MatchServiceError
    defaultService
    "KMSDisabledFault"

-- | An Key Management Service (KMS) error is preventing access to KMS.
_KMSFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSFault =
  Core._MatchServiceError defaultService "KMSFault"

-- | The state of the specified KMS resource isn\'t valid for this request.
_KMSInvalidStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateFault =
  Core._MatchServiceError
    defaultService
    "KMSInvalidStateFault"

-- | DMS cannot access the KMS key.
_KMSKeyNotAccessibleFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSKeyNotAccessibleFault =
  Core._MatchServiceError
    defaultService
    "KMSKeyNotAccessibleFault"

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

-- | The replication subnet group does not cover enough Availability Zones
-- (AZs). Edit the replication subnet group and add more AZs.
_ReplicationSubnetGroupDoesNotCoverEnoughAZs :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationSubnetGroupDoesNotCoverEnoughAZs =
  Core._MatchServiceError
    defaultService
    "ReplicationSubnetGroupDoesNotCoverEnoughAZs"

-- | The resource you are attempting to create already exists.
_ResourceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsFault"

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

-- | Insufficient privileges are preventing access to an Amazon S3 object.
_S3AccessDeniedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_S3AccessDeniedFault =
  Core._MatchServiceError
    defaultService
    "S3AccessDeniedFault"

-- | A specified Amazon S3 bucket, bucket folder, or other object can\'t be
-- found.
_S3ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_S3ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "S3ResourceNotFoundFault"

-- | The SNS topic is invalid.
_SNSInvalidTopicFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError
    defaultService
    "SNSInvalidTopicFault"

-- | You are not authorized for the SNS subscription.
_SNSNoAuthorizationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorizationFault"

-- | The storage quota has been exceeded.
_StorageQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "StorageQuotaExceededFault"

-- | The specified subnet is already in use.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"

-- | An upgrade dependency is preventing the database migration.
_UpgradeDependencyFailureFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UpgradeDependencyFailureFault =
  Core._MatchServiceError
    defaultService
    "UpgradeDependencyFailureFault"
