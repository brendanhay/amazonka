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
    _SubnetAlreadyInUse,
    _InvalidCertificateFault,
    _KMSNotFoundFault,
    _InvalidSubnet,
    _ResourceQuotaExceededFault,
    _SNSInvalidTopicFault,
    _KMSThrottlingFault,
    _AccessDeniedFault,
    _ResourceAlreadyExistsFault,
    _KMSFault,
    _KMSAccessDeniedFault,
    _KMSInvalidStateFault,
    _InsufficientResourceCapacityFault,
    _SNSNoAuthorizationFault,
    _KMSKeyNotAccessibleFault,
    _CollectorNotFoundFault,
    _InvalidResourceStateFault,
    _ReplicationSubnetGroupDoesNotCoverEnoughAZs,
    _KMSDisabledFault,
    _ResourceNotFoundFault,
    _InvalidOperationFault,
    _UpgradeDependencyFailureFault,
    _S3ResourceNotFoundFault,
    _StorageQuotaExceededFault,
    _S3AccessDeniedFault,

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
    certificate_keyLength,
    certificate_certificateCreationDate,
    certificate_certificateWallet,
    certificate_certificateOwner,
    certificate_validToDate,
    certificate_certificateIdentifier,
    certificate_certificateArn,
    certificate_certificatePem,
    certificate_signingAlgorithm,
    certificate_validFromDate,

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
    collectorResponse_collectorName,
    collectorResponse_s3BucketName,
    collectorResponse_versionStatus,
    collectorResponse_serviceAccessRoleArn,
    collectorResponse_inventoryData,
    collectorResponse_description,
    collectorResponse_lastDataReceived,
    collectorResponse_collectorHealthCheck,
    collectorResponse_collectorReferencedId,
    collectorResponse_registeredDate,
    collectorResponse_createdDate,
    collectorResponse_modifiedDate,
    collectorResponse_collectorVersion,

    -- * CollectorShortInfoResponse
    CollectorShortInfoResponse (..),
    newCollectorShortInfoResponse,
    collectorShortInfoResponse_collectorName,
    collectorShortInfoResponse_collectorReferencedId,

    -- * Connection
    Connection (..),
    newConnection,
    connection_replicationInstanceIdentifier,
    connection_replicationInstanceArn,
    connection_lastFailureMessage,
    connection_endpointIdentifier,
    connection_status,
    connection_endpointArn,

    -- * DatabaseInstanceSoftwareDetailsResponse
    DatabaseInstanceSoftwareDetailsResponse (..),
    newDatabaseInstanceSoftwareDetailsResponse,
    databaseInstanceSoftwareDetailsResponse_engineEdition,
    databaseInstanceSoftwareDetailsResponse_servicePack,
    databaseInstanceSoftwareDetailsResponse_engine,
    databaseInstanceSoftwareDetailsResponse_tooltip,
    databaseInstanceSoftwareDetailsResponse_supportLevel,
    databaseInstanceSoftwareDetailsResponse_engineVersion,
    databaseInstanceSoftwareDetailsResponse_osArchitecture,

    -- * DatabaseResponse
    DatabaseResponse (..),
    newDatabaseResponse,
    databaseResponse_numberOfSchemas,
    databaseResponse_databaseName,
    databaseResponse_databaseId,
    databaseResponse_server,
    databaseResponse_collectors,
    databaseResponse_ipAddress,
    databaseResponse_softwareDetails,

    -- * DatabaseShortInfoResponse
    DatabaseShortInfoResponse (..),
    newDatabaseShortInfoResponse,
    databaseShortInfoResponse_databaseEngine,
    databaseShortInfoResponse_databaseName,
    databaseShortInfoResponse_databaseId,
    databaseShortInfoResponse_databaseIpAddress,

    -- * DmsTransferSettings
    DmsTransferSettings (..),
    newDmsTransferSettings,
    dmsTransferSettings_serviceAccessRoleArn,
    dmsTransferSettings_bucketName,

    -- * DocDbSettings
    DocDbSettings (..),
    newDocDbSettings,
    docDbSettings_port,
    docDbSettings_secretsManagerAccessRoleArn,
    docDbSettings_password,
    docDbSettings_serverName,
    docDbSettings_databaseName,
    docDbSettings_username,
    docDbSettings_nestingLevel,
    docDbSettings_docsToInvestigate,
    docDbSettings_secretsManagerSecretId,
    docDbSettings_kmsKeyId,
    docDbSettings_extractDocId,

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
    endpoint_port,
    endpoint_elasticsearchSettings,
    endpoint_redshiftSettings,
    endpoint_externalTableDefinition,
    endpoint_engineName,
    endpoint_engineDisplayName,
    endpoint_mySQLSettings,
    endpoint_sslMode,
    endpoint_endpointIdentifier,
    endpoint_serverName,
    endpoint_docDbSettings,
    endpoint_databaseName,
    endpoint_username,
    endpoint_gcpMySQLSettings,
    endpoint_serviceAccessRoleArn,
    endpoint_extraConnectionAttributes,
    endpoint_neptuneSettings,
    endpoint_externalId,
    endpoint_status,
    endpoint_endpointType,
    endpoint_kinesisSettings,
    endpoint_oracleSettings,
    endpoint_certificateArn,
    endpoint_dynamoDbSettings,
    endpoint_redisSettings,
    endpoint_s3Settings,
    endpoint_kmsKeyId,
    endpoint_microsoftSQLServerSettings,
    endpoint_kafkaSettings,
    endpoint_dmsTransferSettings,
    endpoint_sybaseSettings,
    endpoint_postgreSQLSettings,
    endpoint_iBMDb2Settings,
    endpoint_mongoDbSettings,
    endpoint_endpointArn,

    -- * EndpointSetting
    EndpointSetting (..),
    newEndpointSetting,
    endpointSetting_intValueMax,
    endpointSetting_name,
    endpointSetting_type,
    endpointSetting_defaultValue,
    endpointSetting_units,
    endpointSetting_applicability,
    endpointSetting_sensitive,
    endpointSetting_intValueMin,
    endpointSetting_enumValues,

    -- * Event
    Event (..),
    newEvent,
    event_message,
    event_date,
    event_sourceType,
    event_sourceIdentifier,
    event_eventCategories,

    -- * EventCategoryGroup
    EventCategoryGroup (..),
    newEventCategoryGroup,
    eventCategoryGroup_sourceType,
    eventCategoryGroup_eventCategories,

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
    eventSubscription_customerAwsId,

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
    fleetAdvisorSchemaObjectResponse_numberOfObjects,
    fleetAdvisorSchemaObjectResponse_schemaId,
    fleetAdvisorSchemaObjectResponse_objectType,
    fleetAdvisorSchemaObjectResponse_codeSize,

    -- * GcpMySQLSettings
    GcpMySQLSettings (..),
    newGcpMySQLSettings,
    gcpMySQLSettings_port,
    gcpMySQLSettings_maxFileSize,
    gcpMySQLSettings_targetDbType,
    gcpMySQLSettings_secretsManagerAccessRoleArn,
    gcpMySQLSettings_password,
    gcpMySQLSettings_serverName,
    gcpMySQLSettings_databaseName,
    gcpMySQLSettings_username,
    gcpMySQLSettings_parallelLoadThreads,
    gcpMySQLSettings_afterConnectScript,
    gcpMySQLSettings_eventsPollInterval,
    gcpMySQLSettings_cleanSourceMetadataOnMismatch,
    gcpMySQLSettings_secretsManagerSecretId,
    gcpMySQLSettings_serverTimezone,

    -- * IBMDb2Settings
    IBMDb2Settings (..),
    newIBMDb2Settings,
    iBMDb2Settings_port,
    iBMDb2Settings_secretsManagerAccessRoleArn,
    iBMDb2Settings_password,
    iBMDb2Settings_serverName,
    iBMDb2Settings_databaseName,
    iBMDb2Settings_username,
    iBMDb2Settings_setDataCaptureChanges,
    iBMDb2Settings_maxKBytesPerRead,
    iBMDb2Settings_secretsManagerSecretId,
    iBMDb2Settings_currentLsn,

    -- * InventoryData
    InventoryData (..),
    newInventoryData,
    inventoryData_numberOfSchemas,
    inventoryData_numberOfDatabases,

    -- * KafkaSettings
    KafkaSettings (..),
    newKafkaSettings,
    kafkaSettings_broker,
    kafkaSettings_messageMaxBytes,
    kafkaSettings_sslClientKeyArn,
    kafkaSettings_noHexPrefix,
    kafkaSettings_saslPassword,
    kafkaSettings_messageFormat,
    kafkaSettings_sslClientCertificateArn,
    kafkaSettings_includePartitionValue,
    kafkaSettings_includeTableAlterOperations,
    kafkaSettings_sslClientKeyPassword,
    kafkaSettings_saslUsername,
    kafkaSettings_securityProtocol,
    kafkaSettings_partitionIncludeSchemaTable,
    kafkaSettings_includeNullAndEmpty,
    kafkaSettings_includeTransactionDetails,
    kafkaSettings_topic,
    kafkaSettings_includeControlDetails,
    kafkaSettings_sslCaCertificateArn,

    -- * KinesisSettings
    KinesisSettings (..),
    newKinesisSettings,
    kinesisSettings_noHexPrefix,
    kinesisSettings_messageFormat,
    kinesisSettings_serviceAccessRoleArn,
    kinesisSettings_includePartitionValue,
    kinesisSettings_includeTableAlterOperations,
    kinesisSettings_partitionIncludeSchemaTable,
    kinesisSettings_includeNullAndEmpty,
    kinesisSettings_includeTransactionDetails,
    kinesisSettings_streamArn,
    kinesisSettings_includeControlDetails,

    -- * MicrosoftSQLServerSettings
    MicrosoftSQLServerSettings (..),
    newMicrosoftSQLServerSettings,
    microsoftSQLServerSettings_port,
    microsoftSQLServerSettings_controlTablesFileGroup,
    microsoftSQLServerSettings_trimSpaceInChar,
    microsoftSQLServerSettings_secretsManagerAccessRoleArn,
    microsoftSQLServerSettings_password,
    microsoftSQLServerSettings_serverName,
    microsoftSQLServerSettings_querySingleAlwaysOnNode,
    microsoftSQLServerSettings_databaseName,
    microsoftSQLServerSettings_username,
    microsoftSQLServerSettings_bcpPacketSize,
    microsoftSQLServerSettings_secretsManagerSecretId,
    microsoftSQLServerSettings_useBcpFullLoad,
    microsoftSQLServerSettings_readBackupOnly,
    microsoftSQLServerSettings_safeguardPolicy,
    microsoftSQLServerSettings_useThirdPartyBackupDevice,

    -- * MongoDbSettings
    MongoDbSettings (..),
    newMongoDbSettings,
    mongoDbSettings_port,
    mongoDbSettings_secretsManagerAccessRoleArn,
    mongoDbSettings_password,
    mongoDbSettings_authSource,
    mongoDbSettings_serverName,
    mongoDbSettings_databaseName,
    mongoDbSettings_username,
    mongoDbSettings_nestingLevel,
    mongoDbSettings_docsToInvestigate,
    mongoDbSettings_secretsManagerSecretId,
    mongoDbSettings_kmsKeyId,
    mongoDbSettings_authMechanism,
    mongoDbSettings_extractDocId,
    mongoDbSettings_authType,

    -- * MySQLSettings
    MySQLSettings (..),
    newMySQLSettings,
    mySQLSettings_port,
    mySQLSettings_maxFileSize,
    mySQLSettings_targetDbType,
    mySQLSettings_secretsManagerAccessRoleArn,
    mySQLSettings_password,
    mySQLSettings_serverName,
    mySQLSettings_databaseName,
    mySQLSettings_username,
    mySQLSettings_parallelLoadThreads,
    mySQLSettings_afterConnectScript,
    mySQLSettings_eventsPollInterval,
    mySQLSettings_cleanSourceMetadataOnMismatch,
    mySQLSettings_secretsManagerSecretId,
    mySQLSettings_serverTimezone,

    -- * NeptuneSettings
    NeptuneSettings (..),
    newNeptuneSettings,
    neptuneSettings_maxFileSize,
    neptuneSettings_maxRetryCount,
    neptuneSettings_serviceAccessRoleArn,
    neptuneSettings_errorRetryDuration,
    neptuneSettings_iamAuthEnabled,
    neptuneSettings_s3BucketName,
    neptuneSettings_s3BucketFolder,

    -- * OracleSettings
    OracleSettings (..),
    newOracleSettings,
    oracleSettings_parallelAsmReadThreads,
    oracleSettings_port,
    oracleSettings_readTableSpaceName,
    oracleSettings_asmServer,
    oracleSettings_useDirectPathFullLoad,
    oracleSettings_archivedLogDestId,
    oracleSettings_secretsManagerOracleAsmAccessRoleArn,
    oracleSettings_charLengthSemantics,
    oracleSettings_trimSpaceInChar,
    oracleSettings_readAheadBlocks,
    oracleSettings_secretsManagerAccessRoleArn,
    oracleSettings_standbyDelayTime,
    oracleSettings_password,
    oracleSettings_serverName,
    oracleSettings_securityDbEncryptionName,
    oracleSettings_extraArchivedLogDestIds,
    oracleSettings_databaseName,
    oracleSettings_username,
    oracleSettings_oraclePathPrefix,
    oracleSettings_retryInterval,
    oracleSettings_asmPassword,
    oracleSettings_additionalArchivedLogDestId,
    oracleSettings_failTasksOnLobTruncation,
    oracleSettings_archivedLogsOnly,
    oracleSettings_usePathPrefix,
    oracleSettings_replacePathPrefix,
    oracleSettings_securityDbEncryption,
    oracleSettings_directPathParallelLoad,
    oracleSettings_secretsManagerSecretId,
    oracleSettings_addSupplementalLogging,
    oracleSettings_spatialDataOptionToGeoJsonFunctionName,
    oracleSettings_accessAlternateDirectly,
    oracleSettings_numberDatatypeScale,
    oracleSettings_directPathNoLog,
    oracleSettings_asmUser,
    oracleSettings_allowSelectNestedTables,
    oracleSettings_enableHomogenousTablespace,
    oracleSettings_secretsManagerOracleAsmSecretId,
    oracleSettings_useBFile,
    oracleSettings_useLogminerReader,
    oracleSettings_useAlternateFolderForOnline,

    -- * OrderableReplicationInstance
    OrderableReplicationInstance (..),
    newOrderableReplicationInstance,
    orderableReplicationInstance_maxAllocatedStorage,
    orderableReplicationInstance_defaultAllocatedStorage,
    orderableReplicationInstance_availabilityZones,
    orderableReplicationInstance_minAllocatedStorage,
    orderableReplicationInstance_storageType,
    orderableReplicationInstance_releaseStatus,
    orderableReplicationInstance_includedAllocatedStorage,
    orderableReplicationInstance_replicationInstanceClass,
    orderableReplicationInstance_engineVersion,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_forcedApplyDate,

    -- * PostgreSQLSettings
    PostgreSQLSettings (..),
    newPostgreSQLSettings,
    postgreSQLSettings_port,
    postgreSQLSettings_maxFileSize,
    postgreSQLSettings_slotName,
    postgreSQLSettings_trimSpaceInChar,
    postgreSQLSettings_secretsManagerAccessRoleArn,
    postgreSQLSettings_executeTimeout,
    postgreSQLSettings_password,
    postgreSQLSettings_serverName,
    postgreSQLSettings_databaseName,
    postgreSQLSettings_username,
    postgreSQLSettings_ddlArtifactsSchema,
    postgreSQLSettings_captureDdls,
    postgreSQLSettings_failTasksOnLobTruncation,
    postgreSQLSettings_afterConnectScript,
    postgreSQLSettings_heartbeatFrequency,
    postgreSQLSettings_secretsManagerSecretId,
    postgreSQLSettings_pluginName,
    postgreSQLSettings_heartbeatSchema,
    postgreSQLSettings_heartbeatEnable,

    -- * RedisSettings
    RedisSettings (..),
    newRedisSettings,
    redisSettings_authUserName,
    redisSettings_sslSecurityProtocol,
    redisSettings_authPassword,
    redisSettings_sslCaCertificateArn,
    redisSettings_authType,
    redisSettings_serverName,
    redisSettings_port,

    -- * RedshiftSettings
    RedshiftSettings (..),
    newRedshiftSettings,
    redshiftSettings_emptyAsNull,
    redshiftSettings_port,
    redshiftSettings_truncateColumns,
    redshiftSettings_maxFileSize,
    redshiftSettings_replaceChars,
    redshiftSettings_timeFormat,
    redshiftSettings_loadTimeout,
    redshiftSettings_bucketFolder,
    redshiftSettings_secretsManagerAccessRoleArn,
    redshiftSettings_connectionTimeout,
    redshiftSettings_password,
    redshiftSettings_serverName,
    redshiftSettings_caseSensitiveNames,
    redshiftSettings_databaseName,
    redshiftSettings_username,
    redshiftSettings_removeQuotes,
    redshiftSettings_serviceAccessRoleArn,
    redshiftSettings_compUpdate,
    redshiftSettings_replaceInvalidChars,
    redshiftSettings_afterConnectScript,
    redshiftSettings_fileTransferUploadStreams,
    redshiftSettings_bucketName,
    redshiftSettings_secretsManagerSecretId,
    redshiftSettings_explicitIds,
    redshiftSettings_serverSideEncryptionKmsKeyId,
    redshiftSettings_dateFormat,
    redshiftSettings_writeBufferSize,
    redshiftSettings_trimBlanks,
    redshiftSettings_acceptAnyDate,
    redshiftSettings_encryptionMode,

    -- * RefreshSchemasStatus
    RefreshSchemasStatus (..),
    newRefreshSchemasStatus,
    refreshSchemasStatus_replicationInstanceArn,
    refreshSchemasStatus_lastRefreshDate,
    refreshSchemasStatus_lastFailureMessage,
    refreshSchemasStatus_status,
    refreshSchemasStatus_endpointArn,

    -- * ReplicationInstance
    ReplicationInstance (..),
    newReplicationInstance,
    replicationInstance_replicationInstanceIdentifier,
    replicationInstance_replicationInstanceArn,
    replicationInstance_secondaryAvailabilityZone,
    replicationInstance_autoMinorVersionUpgrade,
    replicationInstance_replicationInstanceIpv6Addresses,
    replicationInstance_replicationInstancePublicIpAddresses,
    replicationInstance_replicationInstancePrivateIpAddresses,
    replicationInstance_instanceCreateTime,
    replicationInstance_availabilityZone,
    replicationInstance_publiclyAccessible,
    replicationInstance_replicationInstancePrivateIpAddress,
    replicationInstance_freeUntil,
    replicationInstance_replicationInstancePublicIpAddress,
    replicationInstance_kmsKeyId,
    replicationInstance_allocatedStorage,
    replicationInstance_pendingModifiedValues,
    replicationInstance_preferredMaintenanceWindow,
    replicationInstance_dnsNameServers,
    replicationInstance_replicationSubnetGroup,
    replicationInstance_replicationInstanceStatus,
    replicationInstance_replicationInstanceClass,
    replicationInstance_engineVersion,
    replicationInstance_networkType,
    replicationInstance_multiAZ,
    replicationInstance_vpcSecurityGroups,

    -- * ReplicationInstanceTaskLog
    ReplicationInstanceTaskLog (..),
    newReplicationInstanceTaskLog,
    replicationInstanceTaskLog_replicationInstanceTaskLogSize,
    replicationInstanceTaskLog_replicationTaskName,
    replicationInstanceTaskLog_replicationTaskArn,

    -- * ReplicationPendingModifiedValues
    ReplicationPendingModifiedValues (..),
    newReplicationPendingModifiedValues,
    replicationPendingModifiedValues_allocatedStorage,
    replicationPendingModifiedValues_replicationInstanceClass,
    replicationPendingModifiedValues_engineVersion,
    replicationPendingModifiedValues_networkType,
    replicationPendingModifiedValues_multiAZ,

    -- * ReplicationSubnetGroup
    ReplicationSubnetGroup (..),
    newReplicationSubnetGroup,
    replicationSubnetGroup_subnetGroupStatus,
    replicationSubnetGroup_subnets,
    replicationSubnetGroup_replicationSubnetGroupDescription,
    replicationSubnetGroup_vpcId,
    replicationSubnetGroup_replicationSubnetGroupIdentifier,
    replicationSubnetGroup_supportedNetworkTypes,

    -- * ReplicationTask
    ReplicationTask (..),
    newReplicationTask,
    replicationTask_targetReplicationInstanceArn,
    replicationTask_stopReason,
    replicationTask_replicationInstanceArn,
    replicationTask_lastFailureMessage,
    replicationTask_replicationTaskStats,
    replicationTask_targetEndpointArn,
    replicationTask_replicationTaskCreationDate,
    replicationTask_replicationTaskIdentifier,
    replicationTask_taskData,
    replicationTask_cdcStartPosition,
    replicationTask_replicationTaskSettings,
    replicationTask_status,
    replicationTask_replicationTaskStartDate,
    replicationTask_replicationTaskArn,
    replicationTask_sourceEndpointArn,
    replicationTask_tableMappings,
    replicationTask_migrationType,
    replicationTask_recoveryCheckpoint,
    replicationTask_cdcStopPosition,

    -- * ReplicationTaskAssessmentResult
    ReplicationTaskAssessmentResult (..),
    newReplicationTaskAssessmentResult,
    replicationTaskAssessmentResult_assessmentResults,
    replicationTaskAssessmentResult_s3ObjectUrl,
    replicationTaskAssessmentResult_replicationTaskIdentifier,
    replicationTaskAssessmentResult_replicationTaskLastAssessmentDate,
    replicationTaskAssessmentResult_replicationTaskArn,
    replicationTaskAssessmentResult_assessmentResultsFile,
    replicationTaskAssessmentResult_assessmentStatus,

    -- * ReplicationTaskAssessmentRun
    ReplicationTaskAssessmentRun (..),
    newReplicationTaskAssessmentRun,
    replicationTaskAssessmentRun_lastFailureMessage,
    replicationTaskAssessmentRun_serviceAccessRoleArn,
    replicationTaskAssessmentRun_status,
    replicationTaskAssessmentRun_resultLocationFolder,
    replicationTaskAssessmentRun_replicationTaskArn,
    replicationTaskAssessmentRun_resultLocationBucket,
    replicationTaskAssessmentRun_replicationTaskAssessmentRunCreationDate,
    replicationTaskAssessmentRun_resultEncryptionMode,
    replicationTaskAssessmentRun_assessmentRunName,
    replicationTaskAssessmentRun_replicationTaskAssessmentRunArn,
    replicationTaskAssessmentRun_resultKmsKeyArn,
    replicationTaskAssessmentRun_assessmentProgress,

    -- * ReplicationTaskAssessmentRunProgress
    ReplicationTaskAssessmentRunProgress (..),
    newReplicationTaskAssessmentRunProgress,
    replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount,
    replicationTaskAssessmentRunProgress_individualAssessmentCount,

    -- * ReplicationTaskIndividualAssessment
    ReplicationTaskIndividualAssessment (..),
    newReplicationTaskIndividualAssessment,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate,
    replicationTaskIndividualAssessment_individualAssessmentName,
    replicationTaskIndividualAssessment_status,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn,
    replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn,

    -- * ReplicationTaskStats
    ReplicationTaskStats (..),
    newReplicationTaskStats,
    replicationTaskStats_elapsedTimeMillis,
    replicationTaskStats_fullLoadStartDate,
    replicationTaskStats_stopDate,
    replicationTaskStats_tablesErrored,
    replicationTaskStats_freshStartDate,
    replicationTaskStats_tablesLoading,
    replicationTaskStats_startDate,
    replicationTaskStats_fullLoadFinishDate,
    replicationTaskStats_tablesLoaded,
    replicationTaskStats_fullLoadProgressPercent,
    replicationTaskStats_tablesQueued,

    -- * ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    newResourcePendingMaintenanceActions,
    resourcePendingMaintenanceActions_resourceIdentifier,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,

    -- * S3Settings
    S3Settings (..),
    newS3Settings,
    s3Settings_dictPageSizeLimit,
    s3Settings_timestampColumnName,
    s3Settings_maxFileSize,
    s3Settings_ignoreHeaderRows,
    s3Settings_externalTableDefinition,
    s3Settings_cdcMinFileSize,
    s3Settings_datePartitionDelimiter,
    s3Settings_bucketFolder,
    s3Settings_useCsvNoSupValue,
    s3Settings_dataFormat,
    s3Settings_datePartitionSequence,
    s3Settings_dataPageSize,
    s3Settings_csvNullValue,
    s3Settings_datePartitionEnabled,
    s3Settings_useTaskStartTimeForFullLoadTimestamp,
    s3Settings_expectedBucketOwner,
    s3Settings_preserveTransactions,
    s3Settings_serviceAccessRoleArn,
    s3Settings_cdcInsertsOnly,
    s3Settings_addTrailingPaddingCharacter,
    s3Settings_cdcMaxBatchInterval,
    s3Settings_cdcInsertsAndUpdates,
    s3Settings_datePartitionTimezone,
    s3Settings_compressionType,
    s3Settings_bucketName,
    s3Settings_enableStatistics,
    s3Settings_cdcPath,
    s3Settings_parquetTimestampInMillisecond,
    s3Settings_cannedAclForObjects,
    s3Settings_csvDelimiter,
    s3Settings_csvNoSupValue,
    s3Settings_parquetVersion,
    s3Settings_serverSideEncryptionKmsKeyId,
    s3Settings_rowGroupLength,
    s3Settings_rfc4180,
    s3Settings_addColumnName,
    s3Settings_includeOpForFullLoad,
    s3Settings_encryptionMode,
    s3Settings_csvRowDelimiter,
    s3Settings_encodingType,

    -- * SchemaResponse
    SchemaResponse (..),
    newSchemaResponse,
    schemaResponse_databaseInstance,
    schemaResponse_similarity,
    schemaResponse_originalSchema,
    schemaResponse_codeLineCount,
    schemaResponse_schemaName,
    schemaResponse_server,
    schemaResponse_complexity,
    schemaResponse_schemaId,
    schemaResponse_codeSize,

    -- * SchemaShortInfoResponse
    SchemaShortInfoResponse (..),
    newSchemaShortInfoResponse,
    schemaShortInfoResponse_databaseName,
    schemaShortInfoResponse_schemaName,
    schemaShortInfoResponse_databaseId,
    schemaShortInfoResponse_databaseIpAddress,
    schemaShortInfoResponse_schemaId,

    -- * ServerShortInfoResponse
    ServerShortInfoResponse (..),
    newServerShortInfoResponse,
    serverShortInfoResponse_serverName,
    serverShortInfoResponse_serverId,
    serverShortInfoResponse_ipAddress,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetIdentifier,
    subnet_subnetStatus,
    subnet_subnetAvailabilityZone,

    -- * SupportedEndpointType
    SupportedEndpointType (..),
    newSupportedEndpointType,
    supportedEndpointType_engineName,
    supportedEndpointType_engineDisplayName,
    supportedEndpointType_supportsCDC,
    supportedEndpointType_endpointType,
    supportedEndpointType_replicationInstanceEngineMinimumVersion,

    -- * SybaseSettings
    SybaseSettings (..),
    newSybaseSettings,
    sybaseSettings_port,
    sybaseSettings_secretsManagerAccessRoleArn,
    sybaseSettings_password,
    sybaseSettings_serverName,
    sybaseSettings_databaseName,
    sybaseSettings_username,
    sybaseSettings_secretsManagerSecretId,

    -- * TableStatistics
    TableStatistics (..),
    newTableStatistics,
    tableStatistics_tableName,
    tableStatistics_ddls,
    tableStatistics_fullLoadCondtnlChkFailedRows,
    tableStatistics_tableState,
    tableStatistics_fullLoadEndTime,
    tableStatistics_fullLoadStartTime,
    tableStatistics_appliedInserts,
    tableStatistics_fullLoadReloaded,
    tableStatistics_fullLoadErrorRows,
    tableStatistics_schemaName,
    tableStatistics_validationSuspendedRecords,
    tableStatistics_validationFailedRecords,
    tableStatistics_appliedDeletes,
    tableStatistics_validationState,
    tableStatistics_appliedUpdates,
    tableStatistics_fullLoadRows,
    tableStatistics_validationStateDetails,
    tableStatistics_lastUpdateTime,
    tableStatistics_appliedDdls,
    tableStatistics_inserts,
    tableStatistics_validationPendingRecords,
    tableStatistics_deletes,
    tableStatistics_updates,

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

-- | The specified subnet is already in use.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"

-- | The certificate was not valid.
_InvalidCertificateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCertificateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCertificateFault"

-- | The specified KMS entity or resource can\'t be found.
_KMSNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSNotFoundFault =
  Core._MatchServiceError
    defaultService
    "KMSNotFoundFault"

-- | The subnet provided is invalid.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"

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

-- | This request triggered KMS request throttling.
_KMSThrottlingFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSThrottlingFault =
  Core._MatchServiceError
    defaultService
    "KMSThrottlingFault"

-- | DMS was denied access to the endpoint. Check that the role is correctly
-- configured.
_AccessDeniedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedFault =
  Core._MatchServiceError
    defaultService
    "AccessDeniedFault"

-- | The resource you are attempting to create already exists.
_ResourceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsFault"

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

-- | The state of the specified KMS resource isn\'t valid for this request.
_KMSInvalidStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateFault =
  Core._MatchServiceError
    defaultService
    "KMSInvalidStateFault"

-- | There are not enough resources allocated to the database migration.
_InsufficientResourceCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientResourceCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientResourceCapacityFault"

-- | You are not authorized for the SNS subscription.
_SNSNoAuthorizationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorizationFault"

-- | DMS cannot access the KMS key.
_KMSKeyNotAccessibleFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSKeyNotAccessibleFault =
  Core._MatchServiceError
    defaultService
    "KMSKeyNotAccessibleFault"

-- | The specified collector doesn\'t exist.
_CollectorNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CollectorNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CollectorNotFoundFault"

-- | The resource is in a state that prevents it from being used for database
-- migration.
_InvalidResourceStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidResourceStateFault"

-- | The replication subnet group does not cover enough Availability Zones
-- (AZs). Edit the replication subnet group and add more AZs.
_ReplicationSubnetGroupDoesNotCoverEnoughAZs :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationSubnetGroupDoesNotCoverEnoughAZs =
  Core._MatchServiceError
    defaultService
    "ReplicationSubnetGroupDoesNotCoverEnoughAZs"

-- | The specified KMS key isn\'t enabled.
_KMSDisabledFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSDisabledFault =
  Core._MatchServiceError
    defaultService
    "KMSDisabledFault"

-- | The resource could not be found.
_ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundFault"

-- | The action or operation requested isn\'t valid.
_InvalidOperationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationFault =
  Core._MatchServiceError
    defaultService
    "InvalidOperationFault"

-- | An upgrade dependency is preventing the database migration.
_UpgradeDependencyFailureFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UpgradeDependencyFailureFault =
  Core._MatchServiceError
    defaultService
    "UpgradeDependencyFailureFault"

-- | A specified Amazon S3 bucket, bucket folder, or other object can\'t be
-- found.
_S3ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_S3ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "S3ResourceNotFoundFault"

-- | The storage quota has been exceeded.
_StorageQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "StorageQuotaExceededFault"

-- | Insufficient privileges are preventing access to an Amazon S3 object.
_S3AccessDeniedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_S3AccessDeniedFault =
  Core._MatchServiceError
    defaultService
    "S3AccessDeniedFault"
