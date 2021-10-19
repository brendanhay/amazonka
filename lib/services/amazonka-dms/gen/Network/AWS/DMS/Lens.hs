{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Lens
  ( -- * Operations

    -- ** DeleteReplicationInstance
    deleteReplicationInstance_replicationInstanceArn,
    deleteReplicationInstanceResponse_replicationInstance,
    deleteReplicationInstanceResponse_httpStatus,

    -- ** RebootReplicationInstance
    rebootReplicationInstance_forcePlannedFailover,
    rebootReplicationInstance_forceFailover,
    rebootReplicationInstance_replicationInstanceArn,
    rebootReplicationInstanceResponse_replicationInstance,
    rebootReplicationInstanceResponse_httpStatus,

    -- ** ReloadTables
    reloadTables_reloadOption,
    reloadTables_replicationTaskArn,
    reloadTables_tablesToReload,
    reloadTablesResponse_replicationTaskArn,
    reloadTablesResponse_httpStatus,

    -- ** StartReplicationTaskAssessment
    startReplicationTaskAssessment_replicationTaskArn,
    startReplicationTaskAssessmentResponse_replicationTask,
    startReplicationTaskAssessmentResponse_httpStatus,

    -- ** DeleteReplicationTaskAssessmentRun
    deleteReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn,
    deleteReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    deleteReplicationTaskAssessmentRunResponse_httpStatus,

    -- ** CreateEndpoint
    createEndpoint_dmsTransferSettings,
    createEndpoint_mySQLSettings,
    createEndpoint_serverName,
    createEndpoint_microsoftSQLServerSettings,
    createEndpoint_certificateArn,
    createEndpoint_serviceAccessRoleArn,
    createEndpoint_docDbSettings,
    createEndpoint_postgreSQLSettings,
    createEndpoint_extraConnectionAttributes,
    createEndpoint_kafkaSettings,
    createEndpoint_oracleSettings,
    createEndpoint_redshiftSettings,
    createEndpoint_elasticsearchSettings,
    createEndpoint_username,
    createEndpoint_externalTableDefinition,
    createEndpoint_redisSettings,
    createEndpoint_neptuneSettings,
    createEndpoint_iBMDb2Settings,
    createEndpoint_kmsKeyId,
    createEndpoint_mongoDbSettings,
    createEndpoint_sslMode,
    createEndpoint_password,
    createEndpoint_sybaseSettings,
    createEndpoint_databaseName,
    createEndpoint_s3Settings,
    createEndpoint_kinesisSettings,
    createEndpoint_dynamoDbSettings,
    createEndpoint_resourceIdentifier,
    createEndpoint_tags,
    createEndpoint_port,
    createEndpoint_endpointIdentifier,
    createEndpoint_endpointType,
    createEndpoint_engineName,
    createEndpointResponse_endpoint,
    createEndpointResponse_httpStatus,

    -- ** DescribeSchemas
    describeSchemas_marker,
    describeSchemas_maxRecords,
    describeSchemas_endpointArn,
    describeSchemasResponse_schemas,
    describeSchemasResponse_marker,
    describeSchemasResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_endpointArn,
    deleteConnection_replicationInstanceArn,
    deleteConnectionResponse_connection,
    deleteConnectionResponse_httpStatus,

    -- ** ModifyEventSubscription
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_enabled,
    modifyEventSubscription_sourceType,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_subscriptionName,
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,

    -- ** DescribeReplicationInstanceTaskLogs
    describeReplicationInstanceTaskLogs_marker,
    describeReplicationInstanceTaskLogs_maxRecords,
    describeReplicationInstanceTaskLogs_replicationInstanceArn,
    describeReplicationInstanceTaskLogsResponse_replicationInstanceTaskLogs,
    describeReplicationInstanceTaskLogsResponse_marker,
    describeReplicationInstanceTaskLogsResponse_replicationInstanceArn,
    describeReplicationInstanceTaskLogsResponse_httpStatus,

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

    -- ** DeleteEndpoint
    deleteEndpoint_endpointArn,
    deleteEndpointResponse_endpoint,
    deleteEndpointResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResource_resourceArnList,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeEndpointTypes
    describeEndpointTypes_filters,
    describeEndpointTypes_marker,
    describeEndpointTypes_maxRecords,
    describeEndpointTypesResponse_supportedEndpointTypes,
    describeEndpointTypesResponse_marker,
    describeEndpointTypesResponse_httpStatus,

    -- ** DeleteReplicationTask
    deleteReplicationTask_replicationTaskArn,
    deleteReplicationTaskResponse_replicationTask,
    deleteReplicationTaskResponse_httpStatus,

    -- ** DescribeReplicationTaskAssessmentRuns
    describeReplicationTaskAssessmentRuns_filters,
    describeReplicationTaskAssessmentRuns_marker,
    describeReplicationTaskAssessmentRuns_maxRecords,
    describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns,
    describeReplicationTaskAssessmentRunsResponse_marker,
    describeReplicationTaskAssessmentRunsResponse_httpStatus,

    -- ** DescribeReplicationTaskAssessmentResults
    describeReplicationTaskAssessmentResults_replicationTaskArn,
    describeReplicationTaskAssessmentResults_marker,
    describeReplicationTaskAssessmentResults_maxRecords,
    describeReplicationTaskAssessmentResultsResponse_bucketName,
    describeReplicationTaskAssessmentResultsResponse_marker,
    describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults,
    describeReplicationTaskAssessmentResultsResponse_httpStatus,

    -- ** TestConnection
    testConnection_replicationInstanceArn,
    testConnection_endpointArn,
    testConnectionResponse_connection,
    testConnectionResponse_httpStatus,

    -- ** DescribeConnections
    describeConnections_filters,
    describeConnections_marker,
    describeConnections_maxRecords,
    describeConnectionsResponse_connections,
    describeConnectionsResponse_marker,
    describeConnectionsResponse_httpStatus,

    -- ** MoveReplicationTask
    moveReplicationTask_replicationTaskArn,
    moveReplicationTask_targetReplicationInstanceArn,
    moveReplicationTaskResponse_replicationTask,
    moveReplicationTaskResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceArn,
    removeTagsFromResource_tagKeys,
    removeTagsFromResourceResponse_httpStatus,

    -- ** ModifyEndpoint
    modifyEndpoint_dmsTransferSettings,
    modifyEndpoint_mySQLSettings,
    modifyEndpoint_serverName,
    modifyEndpoint_microsoftSQLServerSettings,
    modifyEndpoint_certificateArn,
    modifyEndpoint_serviceAccessRoleArn,
    modifyEndpoint_docDbSettings,
    modifyEndpoint_postgreSQLSettings,
    modifyEndpoint_extraConnectionAttributes,
    modifyEndpoint_kafkaSettings,
    modifyEndpoint_oracleSettings,
    modifyEndpoint_endpointType,
    modifyEndpoint_redshiftSettings,
    modifyEndpoint_elasticsearchSettings,
    modifyEndpoint_exactSettings,
    modifyEndpoint_username,
    modifyEndpoint_externalTableDefinition,
    modifyEndpoint_engineName,
    modifyEndpoint_redisSettings,
    modifyEndpoint_neptuneSettings,
    modifyEndpoint_iBMDb2Settings,
    modifyEndpoint_mongoDbSettings,
    modifyEndpoint_sslMode,
    modifyEndpoint_password,
    modifyEndpoint_sybaseSettings,
    modifyEndpoint_databaseName,
    modifyEndpoint_s3Settings,
    modifyEndpoint_kinesisSettings,
    modifyEndpoint_endpointIdentifier,
    modifyEndpoint_dynamoDbSettings,
    modifyEndpoint_port,
    modifyEndpoint_endpointArn,
    modifyEndpointResponse_endpoint,
    modifyEndpointResponse_httpStatus,

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

    -- ** DescribeEndpointSettings
    describeEndpointSettings_marker,
    describeEndpointSettings_maxRecords,
    describeEndpointSettings_engineName,
    describeEndpointSettingsResponse_endpointSettings,
    describeEndpointSettingsResponse_marker,
    describeEndpointSettingsResponse_httpStatus,

    -- ** DescribeCertificates
    describeCertificates_filters,
    describeCertificates_marker,
    describeCertificates_maxRecords,
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_marker,
    describeCertificatesResponse_httpStatus,

    -- ** StartReplicationTaskAssessmentRun
    startReplicationTaskAssessmentRun_includeOnly,
    startReplicationTaskAssessmentRun_resultKmsKeyArn,
    startReplicationTaskAssessmentRun_resultLocationFolder,
    startReplicationTaskAssessmentRun_resultEncryptionMode,
    startReplicationTaskAssessmentRun_exclude,
    startReplicationTaskAssessmentRun_replicationTaskArn,
    startReplicationTaskAssessmentRun_serviceAccessRoleArn,
    startReplicationTaskAssessmentRun_resultLocationBucket,
    startReplicationTaskAssessmentRun_assessmentRunName,
    startReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    startReplicationTaskAssessmentRunResponse_httpStatus,

    -- ** DeleteEventSubscription
    deleteEventSubscription_subscriptionName,
    deleteEventSubscriptionResponse_eventSubscription,
    deleteEventSubscriptionResponse_httpStatus,

    -- ** DescribeTableStatistics
    describeTableStatistics_filters,
    describeTableStatistics_marker,
    describeTableStatistics_maxRecords,
    describeTableStatistics_replicationTaskArn,
    describeTableStatisticsResponse_replicationTaskArn,
    describeTableStatisticsResponse_marker,
    describeTableStatisticsResponse_tableStatistics,
    describeTableStatisticsResponse_httpStatus,

    -- ** DescribeReplicationSubnetGroups
    describeReplicationSubnetGroups_filters,
    describeReplicationSubnetGroups_marker,
    describeReplicationSubnetGroups_maxRecords,
    describeReplicationSubnetGroupsResponse_marker,
    describeReplicationSubnetGroupsResponse_replicationSubnetGroups,
    describeReplicationSubnetGroupsResponse_httpStatus,

    -- ** StartReplicationTask
    startReplicationTask_cdcStartPosition,
    startReplicationTask_cdcStopPosition,
    startReplicationTask_cdcStartTime,
    startReplicationTask_replicationTaskArn,
    startReplicationTask_startReplicationTaskType,
    startReplicationTaskResponse_replicationTask,
    startReplicationTaskResponse_httpStatus,

    -- ** DescribeEventSubscriptions
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptions_filters,
    describeEventSubscriptions_marker,
    describeEventSubscriptions_maxRecords,
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceArn,
    addTagsToResource_tags,
    addTagsToResourceResponse_httpStatus,

    -- ** CreateReplicationSubnetGroup
    createReplicationSubnetGroup_tags,
    createReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    createReplicationSubnetGroup_replicationSubnetGroupDescription,
    createReplicationSubnetGroup_subnetIds,
    createReplicationSubnetGroupResponse_replicationSubnetGroup,
    createReplicationSubnetGroupResponse_httpStatus,

    -- ** DescribeApplicableIndividualAssessments
    describeApplicableIndividualAssessments_migrationType,
    describeApplicableIndividualAssessments_sourceEngineName,
    describeApplicableIndividualAssessments_replicationTaskArn,
    describeApplicableIndividualAssessments_marker,
    describeApplicableIndividualAssessments_maxRecords,
    describeApplicableIndividualAssessments_targetEngineName,
    describeApplicableIndividualAssessments_replicationInstanceArn,
    describeApplicableIndividualAssessmentsResponse_marker,
    describeApplicableIndividualAssessmentsResponse_individualAssessmentNames,
    describeApplicableIndividualAssessmentsResponse_httpStatus,

    -- ** DeleteCertificate
    deleteCertificate_certificateArn,
    deleteCertificateResponse_certificate,
    deleteCertificateResponse_httpStatus,

    -- ** RefreshSchemas
    refreshSchemas_endpointArn,
    refreshSchemas_replicationInstanceArn,
    refreshSchemasResponse_refreshSchemasStatus,
    refreshSchemasResponse_httpStatus,

    -- ** DescribeReplicationTasks
    describeReplicationTasks_filters,
    describeReplicationTasks_withoutSettings,
    describeReplicationTasks_marker,
    describeReplicationTasks_maxRecords,
    describeReplicationTasksResponse_replicationTasks,
    describeReplicationTasksResponse_marker,
    describeReplicationTasksResponse_httpStatus,

    -- ** DescribeEventCategories
    describeEventCategories_sourceType,
    describeEventCategories_filters,
    describeEventCategoriesResponse_eventCategoryGroupList,
    describeEventCategoriesResponse_httpStatus,

    -- ** DescribeOrderableReplicationInstances
    describeOrderableReplicationInstances_marker,
    describeOrderableReplicationInstances_maxRecords,
    describeOrderableReplicationInstancesResponse_marker,
    describeOrderableReplicationInstancesResponse_orderableReplicationInstances,
    describeOrderableReplicationInstancesResponse_httpStatus,

    -- ** DescribePendingMaintenanceActions
    describePendingMaintenanceActions_filters,
    describePendingMaintenanceActions_marker,
    describePendingMaintenanceActions_maxRecords,
    describePendingMaintenanceActions_replicationInstanceArn,
    describePendingMaintenanceActionsResponse_pendingMaintenanceActions,
    describePendingMaintenanceActionsResponse_marker,
    describePendingMaintenanceActionsResponse_httpStatus,

    -- ** CreateReplicationTask
    createReplicationTask_replicationTaskSettings,
    createReplicationTask_cdcStartPosition,
    createReplicationTask_taskData,
    createReplicationTask_cdcStopPosition,
    createReplicationTask_resourceIdentifier,
    createReplicationTask_tags,
    createReplicationTask_cdcStartTime,
    createReplicationTask_replicationTaskIdentifier,
    createReplicationTask_sourceEndpointArn,
    createReplicationTask_targetEndpointArn,
    createReplicationTask_replicationInstanceArn,
    createReplicationTask_migrationType,
    createReplicationTask_tableMappings,
    createReplicationTaskResponse_replicationTask,
    createReplicationTaskResponse_httpStatus,

    -- ** DescribeEndpoints
    describeEndpoints_filters,
    describeEndpoints_marker,
    describeEndpoints_maxRecords,
    describeEndpointsResponse_marker,
    describeEndpointsResponse_endpoints,
    describeEndpointsResponse_httpStatus,

    -- ** ModifyReplicationInstance
    modifyReplicationInstance_engineVersion,
    modifyReplicationInstance_autoMinorVersionUpgrade,
    modifyReplicationInstance_allowMajorVersionUpgrade,
    modifyReplicationInstance_preferredMaintenanceWindow,
    modifyReplicationInstance_vpcSecurityGroupIds,
    modifyReplicationInstance_multiAZ,
    modifyReplicationInstance_allocatedStorage,
    modifyReplicationInstance_applyImmediately,
    modifyReplicationInstance_replicationInstanceClass,
    modifyReplicationInstance_replicationInstanceIdentifier,
    modifyReplicationInstance_replicationInstanceArn,
    modifyReplicationInstanceResponse_replicationInstance,
    modifyReplicationInstanceResponse_httpStatus,

    -- ** ImportCertificate
    importCertificate_certificatePem,
    importCertificate_certificateWallet,
    importCertificate_tags,
    importCertificate_certificateIdentifier,
    importCertificateResponse_certificate,
    importCertificateResponse_httpStatus,

    -- ** CancelReplicationTaskAssessmentRun
    cancelReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn,
    cancelReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    cancelReplicationTaskAssessmentRunResponse_httpStatus,

    -- ** ModifyReplicationSubnetGroup
    modifyReplicationSubnetGroup_replicationSubnetGroupDescription,
    modifyReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    modifyReplicationSubnetGroup_subnetIds,
    modifyReplicationSubnetGroupResponse_replicationSubnetGroup,
    modifyReplicationSubnetGroupResponse_httpStatus,

    -- ** DescribeReplicationTaskIndividualAssessments
    describeReplicationTaskIndividualAssessments_filters,
    describeReplicationTaskIndividualAssessments_marker,
    describeReplicationTaskIndividualAssessments_maxRecords,
    describeReplicationTaskIndividualAssessmentsResponse_replicationTaskIndividualAssessments,
    describeReplicationTaskIndividualAssessmentsResponse_marker,
    describeReplicationTaskIndividualAssessmentsResponse_httpStatus,

    -- ** ApplyPendingMaintenanceAction
    applyPendingMaintenanceAction_replicationInstanceArn,
    applyPendingMaintenanceAction_applyAction,
    applyPendingMaintenanceAction_optInType,
    applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions,
    applyPendingMaintenanceActionResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributesResponse_accountQuotas,
    describeAccountAttributesResponse_uniqueAccountIdentifier,
    describeAccountAttributesResponse_httpStatus,

    -- ** DescribeReplicationInstances
    describeReplicationInstances_filters,
    describeReplicationInstances_marker,
    describeReplicationInstances_maxRecords,
    describeReplicationInstancesResponse_marker,
    describeReplicationInstancesResponse_replicationInstances,
    describeReplicationInstancesResponse_httpStatus,

    -- ** DescribeRefreshSchemasStatus
    describeRefreshSchemasStatus_endpointArn,
    describeRefreshSchemasStatusResponse_refreshSchemasStatus,
    describeRefreshSchemasStatusResponse_httpStatus,

    -- ** StopReplicationTask
    stopReplicationTask_replicationTaskArn,
    stopReplicationTaskResponse_replicationTask,
    stopReplicationTaskResponse_httpStatus,

    -- ** ModifyReplicationTask
    modifyReplicationTask_replicationTaskSettings,
    modifyReplicationTask_replicationTaskIdentifier,
    modifyReplicationTask_cdcStartPosition,
    modifyReplicationTask_tableMappings,
    modifyReplicationTask_migrationType,
    modifyReplicationTask_taskData,
    modifyReplicationTask_cdcStopPosition,
    modifyReplicationTask_cdcStartTime,
    modifyReplicationTask_replicationTaskArn,
    modifyReplicationTaskResponse_replicationTask,
    modifyReplicationTaskResponse_httpStatus,

    -- ** CreateReplicationInstance
    createReplicationInstance_engineVersion,
    createReplicationInstance_publiclyAccessible,
    createReplicationInstance_autoMinorVersionUpgrade,
    createReplicationInstance_replicationSubnetGroupIdentifier,
    createReplicationInstance_preferredMaintenanceWindow,
    createReplicationInstance_kmsKeyId,
    createReplicationInstance_availabilityZone,
    createReplicationInstance_vpcSecurityGroupIds,
    createReplicationInstance_multiAZ,
    createReplicationInstance_allocatedStorage,
    createReplicationInstance_dnsNameServers,
    createReplicationInstance_resourceIdentifier,
    createReplicationInstance_tags,
    createReplicationInstance_replicationInstanceIdentifier,
    createReplicationInstance_replicationInstanceClass,
    createReplicationInstanceResponse_replicationInstance,
    createReplicationInstanceResponse_httpStatus,

    -- ** DeleteReplicationSubnetGroup
    deleteReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    deleteReplicationSubnetGroupResponse_httpStatus,

    -- * Types

    -- ** AccountQuota
    accountQuota_max,
    accountQuota_used,
    accountQuota_accountQuotaName,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** Certificate
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

    -- ** Connection
    connection_status,
    connection_replicationInstanceArn,
    connection_endpointIdentifier,
    connection_replicationInstanceIdentifier,
    connection_endpointArn,
    connection_lastFailureMessage,

    -- ** DmsTransferSettings
    dmsTransferSettings_serviceAccessRoleArn,
    dmsTransferSettings_bucketName,

    -- ** DocDbSettings
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

    -- ** DynamoDbSettings
    dynamoDbSettings_serviceAccessRoleArn,

    -- ** ElasticsearchSettings
    elasticsearchSettings_fullLoadErrorPercentage,
    elasticsearchSettings_errorRetryDuration,
    elasticsearchSettings_serviceAccessRoleArn,
    elasticsearchSettings_endpointUri,

    -- ** Endpoint
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

    -- ** EndpointSetting
    endpointSetting_sensitive,
    endpointSetting_intValueMax,
    endpointSetting_applicability,
    endpointSetting_name,
    endpointSetting_intValueMin,
    endpointSetting_units,
    endpointSetting_defaultValue,
    endpointSetting_type,
    endpointSetting_enumValues,

    -- ** Event
    event_sourceType,
    event_sourceIdentifier,
    event_date,
    event_eventCategories,
    event_message,

    -- ** EventCategoryGroup
    eventCategoryGroup_sourceType,
    eventCategoryGroup_eventCategories,

    -- ** EventSubscription
    eventSubscription_status,
    eventSubscription_customerAwsId,
    eventSubscription_custSubscriptionId,
    eventSubscription_snsTopicArn,
    eventSubscription_enabled,
    eventSubscription_sourceType,
    eventSubscription_subscriptionCreationTime,
    eventSubscription_eventCategoriesList,
    eventSubscription_sourceIdsList,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** IBMDb2Settings
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

    -- ** KafkaSettings
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

    -- ** KinesisSettings
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

    -- ** MicrosoftSQLServerSettings
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

    -- ** MongoDbSettings
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

    -- ** MySQLSettings
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

    -- ** NeptuneSettings
    neptuneSettings_maxFileSize,
    neptuneSettings_maxRetryCount,
    neptuneSettings_serviceAccessRoleArn,
    neptuneSettings_iamAuthEnabled,
    neptuneSettings_errorRetryDuration,
    neptuneSettings_s3BucketName,
    neptuneSettings_s3BucketFolder,

    -- ** OracleSettings
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

    -- ** OrderableReplicationInstance
    orderableReplicationInstance_engineVersion,
    orderableReplicationInstance_minAllocatedStorage,
    orderableReplicationInstance_releaseStatus,
    orderableReplicationInstance_includedAllocatedStorage,
    orderableReplicationInstance_availabilityZones,
    orderableReplicationInstance_maxAllocatedStorage,
    orderableReplicationInstance_replicationInstanceClass,
    orderableReplicationInstance_defaultAllocatedStorage,
    orderableReplicationInstance_storageType,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_currentApplyDate,

    -- ** PostgreSQLSettings
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

    -- ** RedisSettings
    redisSettings_sslSecurityProtocol,
    redisSettings_authUserName,
    redisSettings_sslCaCertificateArn,
    redisSettings_authPassword,
    redisSettings_authType,
    redisSettings_serverName,
    redisSettings_port,

    -- ** RedshiftSettings
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

    -- ** RefreshSchemasStatus
    refreshSchemasStatus_status,
    refreshSchemasStatus_lastRefreshDate,
    refreshSchemasStatus_replicationInstanceArn,
    refreshSchemasStatus_endpointArn,
    refreshSchemasStatus_lastFailureMessage,

    -- ** ReplicationInstance
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

    -- ** ReplicationInstanceTaskLog
    replicationInstanceTaskLog_replicationTaskName,
    replicationInstanceTaskLog_replicationTaskArn,
    replicationInstanceTaskLog_replicationInstanceTaskLogSize,

    -- ** ReplicationPendingModifiedValues
    replicationPendingModifiedValues_engineVersion,
    replicationPendingModifiedValues_multiAZ,
    replicationPendingModifiedValues_allocatedStorage,
    replicationPendingModifiedValues_replicationInstanceClass,

    -- ** ReplicationSubnetGroup
    replicationSubnetGroup_vpcId,
    replicationSubnetGroup_subnets,
    replicationSubnetGroup_replicationSubnetGroupIdentifier,
    replicationSubnetGroup_subnetGroupStatus,
    replicationSubnetGroup_replicationSubnetGroupDescription,

    -- ** ReplicationTask
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

    -- ** ReplicationTaskAssessmentResult
    replicationTaskAssessmentResult_assessmentResults,
    replicationTaskAssessmentResult_assessmentResultsFile,
    replicationTaskAssessmentResult_replicationTaskIdentifier,
    replicationTaskAssessmentResult_assessmentStatus,
    replicationTaskAssessmentResult_s3ObjectUrl,
    replicationTaskAssessmentResult_replicationTaskLastAssessmentDate,
    replicationTaskAssessmentResult_replicationTaskArn,

    -- ** ReplicationTaskAssessmentRun
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

    -- ** ReplicationTaskAssessmentRunProgress
    replicationTaskAssessmentRunProgress_individualAssessmentCount,
    replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount,

    -- ** ReplicationTaskIndividualAssessment
    replicationTaskIndividualAssessment_status,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate,
    replicationTaskIndividualAssessment_individualAssessmentName,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn,
    replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn,

    -- ** ReplicationTaskStats
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

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- ** S3Settings
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

    -- ** Subnet
    subnet_subnetStatus,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,

    -- ** SupportedEndpointType
    supportedEndpointType_engineDisplayName,
    supportedEndpointType_endpointType,
    supportedEndpointType_engineName,
    supportedEndpointType_replicationInstanceEngineMinimumVersion,
    supportedEndpointType_supportsCDC,

    -- ** SybaseSettings
    sybaseSettings_serverName,
    sybaseSettings_secretsManagerAccessRoleArn,
    sybaseSettings_username,
    sybaseSettings_password,
    sybaseSettings_databaseName,
    sybaseSettings_secretsManagerSecretId,
    sybaseSettings_port,

    -- ** TableStatistics
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

    -- ** TableToReload
    tableToReload_schemaName,
    tableToReload_tableName,

    -- ** Tag
    tag_value,
    tag_resourceArn,
    tag_key,

    -- ** VpcSecurityGroupMembership
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import Network.AWS.DMS.AddTagsToResource
import Network.AWS.DMS.ApplyPendingMaintenanceAction
import Network.AWS.DMS.CancelReplicationTaskAssessmentRun
import Network.AWS.DMS.CreateEndpoint
import Network.AWS.DMS.CreateEventSubscription
import Network.AWS.DMS.CreateReplicationInstance
import Network.AWS.DMS.CreateReplicationSubnetGroup
import Network.AWS.DMS.CreateReplicationTask
import Network.AWS.DMS.DeleteCertificate
import Network.AWS.DMS.DeleteConnection
import Network.AWS.DMS.DeleteEndpoint
import Network.AWS.DMS.DeleteEventSubscription
import Network.AWS.DMS.DeleteReplicationInstance
import Network.AWS.DMS.DeleteReplicationSubnetGroup
import Network.AWS.DMS.DeleteReplicationTask
import Network.AWS.DMS.DeleteReplicationTaskAssessmentRun
import Network.AWS.DMS.DescribeAccountAttributes
import Network.AWS.DMS.DescribeApplicableIndividualAssessments
import Network.AWS.DMS.DescribeCertificates
import Network.AWS.DMS.DescribeConnections
import Network.AWS.DMS.DescribeEndpointSettings
import Network.AWS.DMS.DescribeEndpointTypes
import Network.AWS.DMS.DescribeEndpoints
import Network.AWS.DMS.DescribeEventCategories
import Network.AWS.DMS.DescribeEventSubscriptions
import Network.AWS.DMS.DescribeEvents
import Network.AWS.DMS.DescribeOrderableReplicationInstances
import Network.AWS.DMS.DescribePendingMaintenanceActions
import Network.AWS.DMS.DescribeRefreshSchemasStatus
import Network.AWS.DMS.DescribeReplicationInstanceTaskLogs
import Network.AWS.DMS.DescribeReplicationInstances
import Network.AWS.DMS.DescribeReplicationSubnetGroups
import Network.AWS.DMS.DescribeReplicationTaskAssessmentResults
import Network.AWS.DMS.DescribeReplicationTaskAssessmentRuns
import Network.AWS.DMS.DescribeReplicationTaskIndividualAssessments
import Network.AWS.DMS.DescribeReplicationTasks
import Network.AWS.DMS.DescribeSchemas
import Network.AWS.DMS.DescribeTableStatistics
import Network.AWS.DMS.ImportCertificate
import Network.AWS.DMS.ListTagsForResource
import Network.AWS.DMS.ModifyEndpoint
import Network.AWS.DMS.ModifyEventSubscription
import Network.AWS.DMS.ModifyReplicationInstance
import Network.AWS.DMS.ModifyReplicationSubnetGroup
import Network.AWS.DMS.ModifyReplicationTask
import Network.AWS.DMS.MoveReplicationTask
import Network.AWS.DMS.RebootReplicationInstance
import Network.AWS.DMS.RefreshSchemas
import Network.AWS.DMS.ReloadTables
import Network.AWS.DMS.RemoveTagsFromResource
import Network.AWS.DMS.StartReplicationTask
import Network.AWS.DMS.StartReplicationTaskAssessment
import Network.AWS.DMS.StartReplicationTaskAssessmentRun
import Network.AWS.DMS.StopReplicationTask
import Network.AWS.DMS.TestConnection
import Network.AWS.DMS.Types.AccountQuota
import Network.AWS.DMS.Types.AvailabilityZone
import Network.AWS.DMS.Types.Certificate
import Network.AWS.DMS.Types.Connection
import Network.AWS.DMS.Types.DmsTransferSettings
import Network.AWS.DMS.Types.DocDbSettings
import Network.AWS.DMS.Types.DynamoDbSettings
import Network.AWS.DMS.Types.ElasticsearchSettings
import Network.AWS.DMS.Types.Endpoint
import Network.AWS.DMS.Types.EndpointSetting
import Network.AWS.DMS.Types.Event
import Network.AWS.DMS.Types.EventCategoryGroup
import Network.AWS.DMS.Types.EventSubscription
import Network.AWS.DMS.Types.Filter
import Network.AWS.DMS.Types.IBMDb2Settings
import Network.AWS.DMS.Types.KafkaSettings
import Network.AWS.DMS.Types.KinesisSettings
import Network.AWS.DMS.Types.MicrosoftSQLServerSettings
import Network.AWS.DMS.Types.MongoDbSettings
import Network.AWS.DMS.Types.MySQLSettings
import Network.AWS.DMS.Types.NeptuneSettings
import Network.AWS.DMS.Types.OracleSettings
import Network.AWS.DMS.Types.OrderableReplicationInstance
import Network.AWS.DMS.Types.PendingMaintenanceAction
import Network.AWS.DMS.Types.PostgreSQLSettings
import Network.AWS.DMS.Types.RedisSettings
import Network.AWS.DMS.Types.RedshiftSettings
import Network.AWS.DMS.Types.RefreshSchemasStatus
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
import Network.AWS.DMS.Types.Subnet
import Network.AWS.DMS.Types.SupportedEndpointType
import Network.AWS.DMS.Types.SybaseSettings
import Network.AWS.DMS.Types.TableStatistics
import Network.AWS.DMS.Types.TableToReload
import Network.AWS.DMS.Types.Tag
import Network.AWS.DMS.Types.VpcSecurityGroupMembership
