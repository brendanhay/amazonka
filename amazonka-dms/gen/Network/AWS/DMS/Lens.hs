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

    -- ** CreateEndpoint
    createEndpoint_mongoDbSettings,
    createEndpoint_sslMode,
    createEndpoint_neptuneSettings,
    createEndpoint_redisSettings,
    createEndpoint_externalTableDefinition,
    createEndpoint_elasticsearchSettings,
    createEndpoint_postgreSQLSettings,
    createEndpoint_oracleSettings,
    createEndpoint_certificateArn,
    createEndpoint_serviceAccessRoleArn,
    createEndpoint_s3Settings,
    createEndpoint_microsoftSQLServerSettings,
    createEndpoint_serverName,
    createEndpoint_kmsKeyId,
    createEndpoint_iBMDb2Settings,
    createEndpoint_password,
    createEndpoint_mySQLSettings,
    createEndpoint_dmsTransferSettings,
    createEndpoint_tags,
    createEndpoint_resourceIdentifier,
    createEndpoint_port,
    createEndpoint_redshiftSettings,
    createEndpoint_username,
    createEndpoint_extraConnectionAttributes,
    createEndpoint_kafkaSettings,
    createEndpoint_docDbSettings,
    createEndpoint_dynamoDbSettings,
    createEndpoint_kinesisSettings,
    createEndpoint_sybaseSettings,
    createEndpoint_databaseName,
    createEndpoint_endpointIdentifier,
    createEndpoint_endpointType,
    createEndpoint_engineName,
    createEndpointResponse_endpoint,
    createEndpointResponse_httpStatus,

    -- ** DescribeEventCategories
    describeEventCategories_filters,
    describeEventCategories_sourceType,
    describeEventCategoriesResponse_eventCategoryGroupList,
    describeEventCategoriesResponse_httpStatus,

    -- ** DescribeOrderableReplicationInstances
    describeOrderableReplicationInstances_maxRecords,
    describeOrderableReplicationInstances_marker,
    describeOrderableReplicationInstancesResponse_orderableReplicationInstances,
    describeOrderableReplicationInstancesResponse_marker,
    describeOrderableReplicationInstancesResponse_httpStatus,

    -- ** StartReplicationTaskAssessment
    startReplicationTaskAssessment_replicationTaskArn,
    startReplicationTaskAssessmentResponse_replicationTask,
    startReplicationTaskAssessmentResponse_httpStatus,

    -- ** DeleteReplicationTaskAssessmentRun
    deleteReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn,
    deleteReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    deleteReplicationTaskAssessmentRunResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_endpointArn,
    deleteConnection_replicationInstanceArn,
    deleteConnectionResponse_connection,
    deleteConnectionResponse_httpStatus,

    -- ** ReloadTables
    reloadTables_reloadOption,
    reloadTables_replicationTaskArn,
    reloadTables_tablesToReload,
    reloadTablesResponse_replicationTaskArn,
    reloadTablesResponse_httpStatus,

    -- ** DescribeApplicableIndividualAssessments
    describeApplicableIndividualAssessments_migrationType,
    describeApplicableIndividualAssessments_sourceEngineName,
    describeApplicableIndividualAssessments_replicationTaskArn,
    describeApplicableIndividualAssessments_replicationInstanceArn,
    describeApplicableIndividualAssessments_targetEngineName,
    describeApplicableIndividualAssessments_maxRecords,
    describeApplicableIndividualAssessments_marker,
    describeApplicableIndividualAssessmentsResponse_individualAssessmentNames,
    describeApplicableIndividualAssessmentsResponse_marker,
    describeApplicableIndividualAssessmentsResponse_httpStatus,

    -- ** DeleteCertificate
    deleteCertificate_certificateArn,
    deleteCertificateResponse_certificate,
    deleteCertificateResponse_httpStatus,

    -- ** StartReplicationTask
    startReplicationTask_cdcStartTime,
    startReplicationTask_cdcStopPosition,
    startReplicationTask_cdcStartPosition,
    startReplicationTask_replicationTaskArn,
    startReplicationTask_startReplicationTaskType,
    startReplicationTaskResponse_replicationTask,
    startReplicationTaskResponse_httpStatus,

    -- ** StopReplicationTask
    stopReplicationTask_replicationTaskArn,
    stopReplicationTaskResponse_replicationTask,
    stopReplicationTaskResponse_httpStatus,

    -- ** CreateReplicationInstance
    createReplicationInstance_replicationSubnetGroupIdentifier,
    createReplicationInstance_publiclyAccessible,
    createReplicationInstance_multiAZ,
    createReplicationInstance_vpcSecurityGroupIds,
    createReplicationInstance_kmsKeyId,
    createReplicationInstance_availabilityZone,
    createReplicationInstance_engineVersion,
    createReplicationInstance_preferredMaintenanceWindow,
    createReplicationInstance_tags,
    createReplicationInstance_resourceIdentifier,
    createReplicationInstance_dnsNameServers,
    createReplicationInstance_allocatedStorage,
    createReplicationInstance_autoMinorVersionUpgrade,
    createReplicationInstance_replicationInstanceIdentifier,
    createReplicationInstance_replicationInstanceClass,
    createReplicationInstanceResponse_replicationInstance,
    createReplicationInstanceResponse_httpStatus,

    -- ** ModifyReplicationTask
    modifyReplicationTask_migrationType,
    modifyReplicationTask_taskData,
    modifyReplicationTask_replicationTaskSettings,
    modifyReplicationTask_tableMappings,
    modifyReplicationTask_cdcStartTime,
    modifyReplicationTask_cdcStopPosition,
    modifyReplicationTask_cdcStartPosition,
    modifyReplicationTask_replicationTaskIdentifier,
    modifyReplicationTask_replicationTaskArn,
    modifyReplicationTaskResponse_replicationTask,
    modifyReplicationTaskResponse_httpStatus,

    -- ** DescribeEventSubscriptions
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptions_filters,
    describeEventSubscriptions_maxRecords,
    describeEventSubscriptions_marker,
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_httpStatus,

    -- ** DeleteEventSubscription
    deleteEventSubscription_subscriptionName,
    deleteEventSubscriptionResponse_eventSubscription,
    deleteEventSubscriptionResponse_httpStatus,

    -- ** DescribeTableStatistics
    describeTableStatistics_filters,
    describeTableStatistics_maxRecords,
    describeTableStatistics_marker,
    describeTableStatistics_replicationTaskArn,
    describeTableStatisticsResponse_tableStatistics,
    describeTableStatisticsResponse_replicationTaskArn,
    describeTableStatisticsResponse_marker,
    describeTableStatisticsResponse_httpStatus,

    -- ** DescribeReplicationSubnetGroups
    describeReplicationSubnetGroups_filters,
    describeReplicationSubnetGroups_maxRecords,
    describeReplicationSubnetGroups_marker,
    describeReplicationSubnetGroupsResponse_replicationSubnetGroups,
    describeReplicationSubnetGroupsResponse_marker,
    describeReplicationSubnetGroupsResponse_httpStatus,

    -- ** DescribeRefreshSchemasStatus
    describeRefreshSchemasStatus_endpointArn,
    describeRefreshSchemasStatusResponse_refreshSchemasStatus,
    describeRefreshSchemasStatusResponse_httpStatus,

    -- ** StartReplicationTaskAssessmentRun
    startReplicationTaskAssessmentRun_resultKmsKeyArn,
    startReplicationTaskAssessmentRun_resultEncryptionMode,
    startReplicationTaskAssessmentRun_exclude,
    startReplicationTaskAssessmentRun_includeOnly,
    startReplicationTaskAssessmentRun_resultLocationFolder,
    startReplicationTaskAssessmentRun_replicationTaskArn,
    startReplicationTaskAssessmentRun_serviceAccessRoleArn,
    startReplicationTaskAssessmentRun_resultLocationBucket,
    startReplicationTaskAssessmentRun_assessmentRunName,
    startReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    startReplicationTaskAssessmentRunResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceArn,
    removeTagsFromResource_tagKeys,
    removeTagsFromResourceResponse_httpStatus,

    -- ** DescribeCertificates
    describeCertificates_filters,
    describeCertificates_maxRecords,
    describeCertificates_marker,
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_marker,
    describeCertificatesResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributesResponse_accountQuotas,
    describeAccountAttributesResponse_uniqueAccountIdentifier,
    describeAccountAttributesResponse_httpStatus,

    -- ** ModifyEndpoint
    modifyEndpoint_mongoDbSettings,
    modifyEndpoint_sslMode,
    modifyEndpoint_neptuneSettings,
    modifyEndpoint_redisSettings,
    modifyEndpoint_engineName,
    modifyEndpoint_exactSettings,
    modifyEndpoint_externalTableDefinition,
    modifyEndpoint_elasticsearchSettings,
    modifyEndpoint_postgreSQLSettings,
    modifyEndpoint_oracleSettings,
    modifyEndpoint_endpointType,
    modifyEndpoint_certificateArn,
    modifyEndpoint_serviceAccessRoleArn,
    modifyEndpoint_s3Settings,
    modifyEndpoint_microsoftSQLServerSettings,
    modifyEndpoint_serverName,
    modifyEndpoint_iBMDb2Settings,
    modifyEndpoint_password,
    modifyEndpoint_mySQLSettings,
    modifyEndpoint_dmsTransferSettings,
    modifyEndpoint_port,
    modifyEndpoint_redshiftSettings,
    modifyEndpoint_username,
    modifyEndpoint_extraConnectionAttributes,
    modifyEndpoint_kafkaSettings,
    modifyEndpoint_docDbSettings,
    modifyEndpoint_dynamoDbSettings,
    modifyEndpoint_endpointIdentifier,
    modifyEndpoint_kinesisSettings,
    modifyEndpoint_sybaseSettings,
    modifyEndpoint_databaseName,
    modifyEndpoint_endpointArn,
    modifyEndpointResponse_endpoint,
    modifyEndpointResponse_httpStatus,

    -- ** DescribeEndpointSettings
    describeEndpointSettings_maxRecords,
    describeEndpointSettings_marker,
    describeEndpointSettings_engineName,
    describeEndpointSettingsResponse_endpointSettings,
    describeEndpointSettingsResponse_marker,
    describeEndpointSettingsResponse_httpStatus,

    -- ** ApplyPendingMaintenanceAction
    applyPendingMaintenanceAction_replicationInstanceArn,
    applyPendingMaintenanceAction_applyAction,
    applyPendingMaintenanceAction_optInType,
    applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions,
    applyPendingMaintenanceActionResponse_httpStatus,

    -- ** ImportCertificate
    importCertificate_certificateWallet,
    importCertificate_tags,
    importCertificate_certificatePem,
    importCertificate_certificateIdentifier,
    importCertificateResponse_certificate,
    importCertificateResponse_httpStatus,

    -- ** DescribeReplicationTaskAssessmentResults
    describeReplicationTaskAssessmentResults_replicationTaskArn,
    describeReplicationTaskAssessmentResults_maxRecords,
    describeReplicationTaskAssessmentResults_marker,
    describeReplicationTaskAssessmentResultsResponse_bucketName,
    describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults,
    describeReplicationTaskAssessmentResultsResponse_marker,
    describeReplicationTaskAssessmentResultsResponse_httpStatus,

    -- ** TestConnection
    testConnection_replicationInstanceArn,
    testConnection_endpointArn,
    testConnectionResponse_connection,
    testConnectionResponse_httpStatus,

    -- ** DescribeEndpointTypes
    describeEndpointTypes_filters,
    describeEndpointTypes_maxRecords,
    describeEndpointTypes_marker,
    describeEndpointTypesResponse_supportedEndpointTypes,
    describeEndpointTypesResponse_marker,
    describeEndpointTypesResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_duration,
    describeEvents_startTime,
    describeEvents_endTime,
    describeEvents_eventCategories,
    describeEvents_sourceIdentifier,
    describeEvents_filters,
    describeEvents_sourceType,
    describeEvents_maxRecords,
    describeEvents_marker,
    describeEventsResponse_events,
    describeEventsResponse_marker,
    describeEventsResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointArn,
    deleteEndpointResponse_endpoint,
    deleteEndpointResponse_httpStatus,

    -- ** DescribePendingMaintenanceActions
    describePendingMaintenanceActions_filters,
    describePendingMaintenanceActions_replicationInstanceArn,
    describePendingMaintenanceActions_maxRecords,
    describePendingMaintenanceActions_marker,
    describePendingMaintenanceActionsResponse_pendingMaintenanceActions,
    describePendingMaintenanceActionsResponse_marker,
    describePendingMaintenanceActionsResponse_httpStatus,

    -- ** DescribeSchemas
    describeSchemas_maxRecords,
    describeSchemas_marker,
    describeSchemas_endpointArn,
    describeSchemasResponse_schemas,
    describeSchemasResponse_marker,
    describeSchemasResponse_httpStatus,

    -- ** RefreshSchemas
    refreshSchemas_endpointArn,
    refreshSchemas_replicationInstanceArn,
    refreshSchemasResponse_refreshSchemasStatus,
    refreshSchemasResponse_httpStatus,

    -- ** ModifyEventSubscription
    modifyEventSubscription_enabled,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_sourceType,
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_subscriptionName,
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,

    -- ** DescribeReplicationTasks
    describeReplicationTasks_withoutSettings,
    describeReplicationTasks_filters,
    describeReplicationTasks_maxRecords,
    describeReplicationTasks_marker,
    describeReplicationTasksResponse_replicationTasks,
    describeReplicationTasksResponse_marker,
    describeReplicationTasksResponse_httpStatus,

    -- ** CreateReplicationSubnetGroup
    createReplicationSubnetGroup_tags,
    createReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    createReplicationSubnetGroup_replicationSubnetGroupDescription,
    createReplicationSubnetGroup_subnetIds,
    createReplicationSubnetGroupResponse_replicationSubnetGroup,
    createReplicationSubnetGroupResponse_httpStatus,

    -- ** DeleteReplicationInstance
    deleteReplicationInstance_replicationInstanceArn,
    deleteReplicationInstanceResponse_replicationInstance,
    deleteReplicationInstanceResponse_httpStatus,

    -- ** RebootReplicationInstance
    rebootReplicationInstance_forceFailover,
    rebootReplicationInstance_forcePlannedFailover,
    rebootReplicationInstance_replicationInstanceArn,
    rebootReplicationInstanceResponse_replicationInstance,
    rebootReplicationInstanceResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceArn,
    addTagsToResource_tags,
    addTagsToResourceResponse_httpStatus,

    -- ** DeleteReplicationSubnetGroup
    deleteReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    deleteReplicationSubnetGroupResponse_httpStatus,

    -- ** CreateEventSubscription
    createEventSubscription_sourceIds,
    createEventSubscription_enabled,
    createEventSubscription_eventCategories,
    createEventSubscription_tags,
    createEventSubscription_sourceType,
    createEventSubscription_subscriptionName,
    createEventSubscription_snsTopicArn,
    createEventSubscriptionResponse_eventSubscription,
    createEventSubscriptionResponse_httpStatus,

    -- ** DescribeReplicationInstances
    describeReplicationInstances_filters,
    describeReplicationInstances_maxRecords,
    describeReplicationInstances_marker,
    describeReplicationInstancesResponse_replicationInstances,
    describeReplicationInstancesResponse_marker,
    describeReplicationInstancesResponse_httpStatus,

    -- ** MoveReplicationTask
    moveReplicationTask_replicationTaskArn,
    moveReplicationTask_targetReplicationInstanceArn,
    moveReplicationTaskResponse_replicationTask,
    moveReplicationTaskResponse_httpStatus,

    -- ** DescribeConnections
    describeConnections_filters,
    describeConnections_maxRecords,
    describeConnections_marker,
    describeConnectionsResponse_connections,
    describeConnectionsResponse_marker,
    describeConnectionsResponse_httpStatus,

    -- ** ModifyReplicationSubnetGroup
    modifyReplicationSubnetGroup_replicationSubnetGroupDescription,
    modifyReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    modifyReplicationSubnetGroup_subnetIds,
    modifyReplicationSubnetGroupResponse_replicationSubnetGroup,
    modifyReplicationSubnetGroupResponse_httpStatus,

    -- ** DeleteReplicationTask
    deleteReplicationTask_replicationTaskArn,
    deleteReplicationTaskResponse_replicationTask,
    deleteReplicationTaskResponse_httpStatus,

    -- ** CancelReplicationTaskAssessmentRun
    cancelReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn,
    cancelReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    cancelReplicationTaskAssessmentRunResponse_httpStatus,

    -- ** DescribeReplicationTaskAssessmentRuns
    describeReplicationTaskAssessmentRuns_filters,
    describeReplicationTaskAssessmentRuns_maxRecords,
    describeReplicationTaskAssessmentRuns_marker,
    describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns,
    describeReplicationTaskAssessmentRunsResponse_marker,
    describeReplicationTaskAssessmentRunsResponse_httpStatus,

    -- ** DescribeReplicationTaskIndividualAssessments
    describeReplicationTaskIndividualAssessments_filters,
    describeReplicationTaskIndividualAssessments_maxRecords,
    describeReplicationTaskIndividualAssessments_marker,
    describeReplicationTaskIndividualAssessmentsResponse_replicationTaskIndividualAssessments,
    describeReplicationTaskIndividualAssessmentsResponse_marker,
    describeReplicationTaskIndividualAssessmentsResponse_httpStatus,

    -- ** CreateReplicationTask
    createReplicationTask_taskData,
    createReplicationTask_replicationTaskSettings,
    createReplicationTask_tags,
    createReplicationTask_resourceIdentifier,
    createReplicationTask_cdcStartTime,
    createReplicationTask_cdcStopPosition,
    createReplicationTask_cdcStartPosition,
    createReplicationTask_replicationTaskIdentifier,
    createReplicationTask_sourceEndpointArn,
    createReplicationTask_targetEndpointArn,
    createReplicationTask_replicationInstanceArn,
    createReplicationTask_migrationType,
    createReplicationTask_tableMappings,
    createReplicationTaskResponse_replicationTask,
    createReplicationTaskResponse_httpStatus,

    -- ** ModifyReplicationInstance
    modifyReplicationInstance_allowMajorVersionUpgrade,
    modifyReplicationInstance_multiAZ,
    modifyReplicationInstance_vpcSecurityGroupIds,
    modifyReplicationInstance_engineVersion,
    modifyReplicationInstance_preferredMaintenanceWindow,
    modifyReplicationInstance_replicationInstanceIdentifier,
    modifyReplicationInstance_replicationInstanceClass,
    modifyReplicationInstance_allocatedStorage,
    modifyReplicationInstance_applyImmediately,
    modifyReplicationInstance_autoMinorVersionUpgrade,
    modifyReplicationInstance_replicationInstanceArn,
    modifyReplicationInstanceResponse_replicationInstance,
    modifyReplicationInstanceResponse_httpStatus,

    -- ** DescribeEndpoints
    describeEndpoints_filters,
    describeEndpoints_maxRecords,
    describeEndpoints_marker,
    describeEndpointsResponse_endpoints,
    describeEndpointsResponse_marker,
    describeEndpointsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResource_resourceArnList,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeReplicationInstanceTaskLogs
    describeReplicationInstanceTaskLogs_maxRecords,
    describeReplicationInstanceTaskLogs_marker,
    describeReplicationInstanceTaskLogs_replicationInstanceArn,
    describeReplicationInstanceTaskLogsResponse_replicationInstanceTaskLogs,
    describeReplicationInstanceTaskLogsResponse_replicationInstanceArn,
    describeReplicationInstanceTaskLogsResponse_marker,
    describeReplicationInstanceTaskLogsResponse_httpStatus,

    -- * Types

    -- ** AccountQuota
    accountQuota_used,
    accountQuota_accountQuotaName,
    accountQuota_max,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** Certificate
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

    -- ** Connection
    connection_status,
    connection_lastFailureMessage,
    connection_replicationInstanceIdentifier,
    connection_endpointArn,
    connection_endpointIdentifier,
    connection_replicationInstanceArn,

    -- ** DmsTransferSettings
    dmsTransferSettings_bucketName,
    dmsTransferSettings_serviceAccessRoleArn,

    -- ** DocDbSettings
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

    -- ** DynamoDbSettings
    dynamoDbSettings_serviceAccessRoleArn,

    -- ** ElasticsearchSettings
    elasticsearchSettings_errorRetryDuration,
    elasticsearchSettings_fullLoadErrorPercentage,
    elasticsearchSettings_serviceAccessRoleArn,
    elasticsearchSettings_endpointUri,

    -- ** Endpoint
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

    -- ** EndpointSetting
    endpointSetting_applicability,
    endpointSetting_intValueMin,
    endpointSetting_name,
    endpointSetting_intValueMax,
    endpointSetting_sensitive,
    endpointSetting_enumValues,
    endpointSetting_defaultValue,
    endpointSetting_type,
    endpointSetting_units,

    -- ** Event
    event_message,
    event_eventCategories,
    event_date,
    event_sourceIdentifier,
    event_sourceType,

    -- ** EventCategoryGroup
    eventCategoryGroup_eventCategories,
    eventCategoryGroup_sourceType,

    -- ** EventSubscription
    eventSubscription_custSubscriptionId,
    eventSubscription_status,
    eventSubscription_sourceIdsList,
    eventSubscription_eventCategoriesList,
    eventSubscription_enabled,
    eventSubscription_subscriptionCreationTime,
    eventSubscription_customerAwsId,
    eventSubscription_sourceType,
    eventSubscription_snsTopicArn,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** IBMDb2Settings
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

    -- ** KafkaSettings
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

    -- ** KinesisSettings
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

    -- ** MicrosoftSQLServerSettings
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

    -- ** MongoDbSettings
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

    -- ** MySQLSettings
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

    -- ** NeptuneSettings
    neptuneSettings_errorRetryDuration,
    neptuneSettings_serviceAccessRoleArn,
    neptuneSettings_maxFileSize,
    neptuneSettings_maxRetryCount,
    neptuneSettings_iamAuthEnabled,
    neptuneSettings_s3BucketName,
    neptuneSettings_s3BucketFolder,

    -- ** OracleSettings
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

    -- ** OrderableReplicationInstance
    orderableReplicationInstance_availabilityZones,
    orderableReplicationInstance_maxAllocatedStorage,
    orderableReplicationInstance_storageType,
    orderableReplicationInstance_defaultAllocatedStorage,
    orderableReplicationInstance_includedAllocatedStorage,
    orderableReplicationInstance_releaseStatus,
    orderableReplicationInstance_engineVersion,
    orderableReplicationInstance_replicationInstanceClass,
    orderableReplicationInstance_minAllocatedStorage,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_description,

    -- ** PostgreSQLSettings
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

    -- ** RedisSettings
    redisSettings_sslCaCertificateArn,
    redisSettings_authPassword,
    redisSettings_sslSecurityProtocol,
    redisSettings_authUserName,
    redisSettings_authType,
    redisSettings_serverName,
    redisSettings_port,

    -- ** RedshiftSettings
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

    -- ** RefreshSchemasStatus
    refreshSchemasStatus_status,
    refreshSchemasStatus_lastFailureMessage,
    refreshSchemasStatus_endpointArn,
    refreshSchemasStatus_lastRefreshDate,
    refreshSchemasStatus_replicationInstanceArn,

    -- ** ReplicationInstance
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

    -- ** ReplicationInstanceTaskLog
    replicationInstanceTaskLog_replicationTaskName,
    replicationInstanceTaskLog_replicationTaskArn,
    replicationInstanceTaskLog_replicationInstanceTaskLogSize,

    -- ** ReplicationPendingModifiedValues
    replicationPendingModifiedValues_multiAZ,
    replicationPendingModifiedValues_engineVersion,
    replicationPendingModifiedValues_replicationInstanceClass,
    replicationPendingModifiedValues_allocatedStorage,

    -- ** ReplicationSubnetGroup
    replicationSubnetGroup_replicationSubnetGroupIdentifier,
    replicationSubnetGroup_subnetGroupStatus,
    replicationSubnetGroup_replicationSubnetGroupDescription,
    replicationSubnetGroup_vpcId,
    replicationSubnetGroup_subnets,

    -- ** ReplicationTask
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

    -- ** ReplicationTaskAssessmentResult
    replicationTaskAssessmentResult_s3ObjectUrl,
    replicationTaskAssessmentResult_assessmentStatus,
    replicationTaskAssessmentResult_assessmentResults,
    replicationTaskAssessmentResult_replicationTaskArn,
    replicationTaskAssessmentResult_replicationTaskLastAssessmentDate,
    replicationTaskAssessmentResult_replicationTaskIdentifier,
    replicationTaskAssessmentResult_assessmentResultsFile,

    -- ** ReplicationTaskAssessmentRun
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

    -- ** ReplicationTaskAssessmentRunProgress
    replicationTaskAssessmentRunProgress_individualAssessmentCount,
    replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount,

    -- ** ReplicationTaskIndividualAssessment
    replicationTaskIndividualAssessment_status,
    replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn,
    replicationTaskIndividualAssessment_individualAssessmentName,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn,

    -- ** ReplicationTaskStats
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

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- ** S3Settings
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

    -- ** Subnet
    subnet_subnetStatus,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,

    -- ** SupportedEndpointType
    supportedEndpointType_replicationInstanceEngineMinimumVersion,
    supportedEndpointType_engineName,
    supportedEndpointType_endpointType,
    supportedEndpointType_supportsCDC,
    supportedEndpointType_engineDisplayName,

    -- ** SybaseSettings
    sybaseSettings_secretsManagerSecretId,
    sybaseSettings_serverName,
    sybaseSettings_password,
    sybaseSettings_port,
    sybaseSettings_username,
    sybaseSettings_secretsManagerAccessRoleArn,
    sybaseSettings_databaseName,

    -- ** TableStatistics
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

    -- ** TableToReload
    tableToReload_schemaName,
    tableToReload_tableName,

    -- ** Tag
    tag_key,
    tag_resourceArn,
    tag_value,

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
