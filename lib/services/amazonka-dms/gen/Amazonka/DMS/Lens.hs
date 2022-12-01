{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DMS.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Lens
  ( -- * Operations

    -- ** AddTagsToResource
    addTagsToResource_resourceArn,
    addTagsToResource_tags,
    addTagsToResourceResponse_httpStatus,

    -- ** ApplyPendingMaintenanceAction
    applyPendingMaintenanceAction_replicationInstanceArn,
    applyPendingMaintenanceAction_applyAction,
    applyPendingMaintenanceAction_optInType,
    applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions,
    applyPendingMaintenanceActionResponse_httpStatus,

    -- ** CancelReplicationTaskAssessmentRun
    cancelReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn,
    cancelReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    cancelReplicationTaskAssessmentRunResponse_httpStatus,

    -- ** CreateEndpoint
    createEndpoint_tags,
    createEndpoint_port,
    createEndpoint_elasticsearchSettings,
    createEndpoint_redshiftSettings,
    createEndpoint_externalTableDefinition,
    createEndpoint_mySQLSettings,
    createEndpoint_sslMode,
    createEndpoint_password,
    createEndpoint_serverName,
    createEndpoint_docDbSettings,
    createEndpoint_databaseName,
    createEndpoint_username,
    createEndpoint_gcpMySQLSettings,
    createEndpoint_serviceAccessRoleArn,
    createEndpoint_extraConnectionAttributes,
    createEndpoint_neptuneSettings,
    createEndpoint_kinesisSettings,
    createEndpoint_oracleSettings,
    createEndpoint_certificateArn,
    createEndpoint_resourceIdentifier,
    createEndpoint_dynamoDbSettings,
    createEndpoint_redisSettings,
    createEndpoint_s3Settings,
    createEndpoint_kmsKeyId,
    createEndpoint_microsoftSQLServerSettings,
    createEndpoint_kafkaSettings,
    createEndpoint_dmsTransferSettings,
    createEndpoint_sybaseSettings,
    createEndpoint_postgreSQLSettings,
    createEndpoint_iBMDb2Settings,
    createEndpoint_mongoDbSettings,
    createEndpoint_endpointIdentifier,
    createEndpoint_endpointType,
    createEndpoint_engineName,
    createEndpointResponse_endpoint,
    createEndpointResponse_httpStatus,

    -- ** CreateEventSubscription
    createEventSubscription_tags,
    createEventSubscription_sourceIds,
    createEventSubscription_sourceType,
    createEventSubscription_enabled,
    createEventSubscription_eventCategories,
    createEventSubscription_subscriptionName,
    createEventSubscription_snsTopicArn,
    createEventSubscriptionResponse_eventSubscription,
    createEventSubscriptionResponse_httpStatus,

    -- ** CreateFleetAdvisorCollector
    createFleetAdvisorCollector_description,
    createFleetAdvisorCollector_collectorName,
    createFleetAdvisorCollector_serviceAccessRoleArn,
    createFleetAdvisorCollector_s3BucketName,
    createFleetAdvisorCollectorResponse_collectorName,
    createFleetAdvisorCollectorResponse_s3BucketName,
    createFleetAdvisorCollectorResponse_serviceAccessRoleArn,
    createFleetAdvisorCollectorResponse_description,
    createFleetAdvisorCollectorResponse_collectorReferencedId,
    createFleetAdvisorCollectorResponse_httpStatus,

    -- ** CreateReplicationInstance
    createReplicationInstance_tags,
    createReplicationInstance_vpcSecurityGroupIds,
    createReplicationInstance_autoMinorVersionUpgrade,
    createReplicationInstance_availabilityZone,
    createReplicationInstance_publiclyAccessible,
    createReplicationInstance_resourceIdentifier,
    createReplicationInstance_kmsKeyId,
    createReplicationInstance_allocatedStorage,
    createReplicationInstance_preferredMaintenanceWindow,
    createReplicationInstance_dnsNameServers,
    createReplicationInstance_replicationSubnetGroupIdentifier,
    createReplicationInstance_engineVersion,
    createReplicationInstance_networkType,
    createReplicationInstance_multiAZ,
    createReplicationInstance_replicationInstanceIdentifier,
    createReplicationInstance_replicationInstanceClass,
    createReplicationInstanceResponse_replicationInstance,
    createReplicationInstanceResponse_httpStatus,

    -- ** CreateReplicationSubnetGroup
    createReplicationSubnetGroup_tags,
    createReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    createReplicationSubnetGroup_replicationSubnetGroupDescription,
    createReplicationSubnetGroup_subnetIds,
    createReplicationSubnetGroupResponse_replicationSubnetGroup,
    createReplicationSubnetGroupResponse_httpStatus,

    -- ** CreateReplicationTask
    createReplicationTask_tags,
    createReplicationTask_cdcStartTime,
    createReplicationTask_taskData,
    createReplicationTask_cdcStartPosition,
    createReplicationTask_replicationTaskSettings,
    createReplicationTask_resourceIdentifier,
    createReplicationTask_cdcStopPosition,
    createReplicationTask_replicationTaskIdentifier,
    createReplicationTask_sourceEndpointArn,
    createReplicationTask_targetEndpointArn,
    createReplicationTask_replicationInstanceArn,
    createReplicationTask_migrationType,
    createReplicationTask_tableMappings,
    createReplicationTaskResponse_replicationTask,
    createReplicationTaskResponse_httpStatus,

    -- ** DeleteCertificate
    deleteCertificate_certificateArn,
    deleteCertificateResponse_certificate,
    deleteCertificateResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_endpointArn,
    deleteConnection_replicationInstanceArn,
    deleteConnectionResponse_connection,
    deleteConnectionResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointArn,
    deleteEndpointResponse_endpoint,
    deleteEndpointResponse_httpStatus,

    -- ** DeleteEventSubscription
    deleteEventSubscription_subscriptionName,
    deleteEventSubscriptionResponse_eventSubscription,
    deleteEventSubscriptionResponse_httpStatus,

    -- ** DeleteFleetAdvisorCollector
    deleteFleetAdvisorCollector_collectorReferencedId,

    -- ** DeleteFleetAdvisorDatabases
    deleteFleetAdvisorDatabases_databaseIds,
    deleteFleetAdvisorDatabasesResponse_databaseIds,
    deleteFleetAdvisorDatabasesResponse_httpStatus,

    -- ** DeleteReplicationInstance
    deleteReplicationInstance_replicationInstanceArn,
    deleteReplicationInstanceResponse_replicationInstance,
    deleteReplicationInstanceResponse_httpStatus,

    -- ** DeleteReplicationSubnetGroup
    deleteReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    deleteReplicationSubnetGroupResponse_httpStatus,

    -- ** DeleteReplicationTask
    deleteReplicationTask_replicationTaskArn,
    deleteReplicationTaskResponse_replicationTask,
    deleteReplicationTaskResponse_httpStatus,

    -- ** DeleteReplicationTaskAssessmentRun
    deleteReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn,
    deleteReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    deleteReplicationTaskAssessmentRunResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributesResponse_uniqueAccountIdentifier,
    describeAccountAttributesResponse_accountQuotas,
    describeAccountAttributesResponse_httpStatus,

    -- ** DescribeApplicableIndividualAssessments
    describeApplicableIndividualAssessments_replicationInstanceArn,
    describeApplicableIndividualAssessments_marker,
    describeApplicableIndividualAssessments_maxRecords,
    describeApplicableIndividualAssessments_replicationTaskArn,
    describeApplicableIndividualAssessments_sourceEngineName,
    describeApplicableIndividualAssessments_migrationType,
    describeApplicableIndividualAssessments_targetEngineName,
    describeApplicableIndividualAssessmentsResponse_individualAssessmentNames,
    describeApplicableIndividualAssessmentsResponse_marker,
    describeApplicableIndividualAssessmentsResponse_httpStatus,

    -- ** DescribeCertificates
    describeCertificates_marker,
    describeCertificates_filters,
    describeCertificates_maxRecords,
    describeCertificatesResponse_marker,
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_httpStatus,

    -- ** DescribeConnections
    describeConnections_marker,
    describeConnections_filters,
    describeConnections_maxRecords,
    describeConnectionsResponse_marker,
    describeConnectionsResponse_connections,
    describeConnectionsResponse_httpStatus,

    -- ** DescribeEndpointSettings
    describeEndpointSettings_marker,
    describeEndpointSettings_maxRecords,
    describeEndpointSettings_engineName,
    describeEndpointSettingsResponse_marker,
    describeEndpointSettingsResponse_endpointSettings,
    describeEndpointSettingsResponse_httpStatus,

    -- ** DescribeEndpointTypes
    describeEndpointTypes_marker,
    describeEndpointTypes_filters,
    describeEndpointTypes_maxRecords,
    describeEndpointTypesResponse_marker,
    describeEndpointTypesResponse_supportedEndpointTypes,
    describeEndpointTypesResponse_httpStatus,

    -- ** DescribeEndpoints
    describeEndpoints_marker,
    describeEndpoints_filters,
    describeEndpoints_maxRecords,
    describeEndpointsResponse_marker,
    describeEndpointsResponse_endpoints,
    describeEndpointsResponse_httpStatus,

    -- ** DescribeEventCategories
    describeEventCategories_filters,
    describeEventCategories_sourceType,
    describeEventCategoriesResponse_eventCategoryGroupList,
    describeEventCategoriesResponse_httpStatus,

    -- ** DescribeEventSubscriptions
    describeEventSubscriptions_marker,
    describeEventSubscriptions_filters,
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptions_maxRecords,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_marker,
    describeEvents_filters,
    describeEvents_sourceType,
    describeEvents_endTime,
    describeEvents_maxRecords,
    describeEvents_duration,
    describeEvents_sourceIdentifier,
    describeEvents_eventCategories,
    describeEvents_startTime,
    describeEventsResponse_marker,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** DescribeFleetAdvisorCollectors
    describeFleetAdvisorCollectors_nextToken,
    describeFleetAdvisorCollectors_filters,
    describeFleetAdvisorCollectors_maxRecords,
    describeFleetAdvisorCollectorsResponse_nextToken,
    describeFleetAdvisorCollectorsResponse_collectors,
    describeFleetAdvisorCollectorsResponse_httpStatus,

    -- ** DescribeFleetAdvisorDatabases
    describeFleetAdvisorDatabases_nextToken,
    describeFleetAdvisorDatabases_filters,
    describeFleetAdvisorDatabases_maxRecords,
    describeFleetAdvisorDatabasesResponse_nextToken,
    describeFleetAdvisorDatabasesResponse_databases,
    describeFleetAdvisorDatabasesResponse_httpStatus,

    -- ** DescribeFleetAdvisorLsaAnalysis
    describeFleetAdvisorLsaAnalysis_nextToken,
    describeFleetAdvisorLsaAnalysis_maxRecords,
    describeFleetAdvisorLsaAnalysisResponse_nextToken,
    describeFleetAdvisorLsaAnalysisResponse_analysis,
    describeFleetAdvisorLsaAnalysisResponse_httpStatus,

    -- ** DescribeFleetAdvisorSchemaObjectSummary
    describeFleetAdvisorSchemaObjectSummary_nextToken,
    describeFleetAdvisorSchemaObjectSummary_filters,
    describeFleetAdvisorSchemaObjectSummary_maxRecords,
    describeFleetAdvisorSchemaObjectSummaryResponse_nextToken,
    describeFleetAdvisorSchemaObjectSummaryResponse_fleetAdvisorSchemaObjects,
    describeFleetAdvisorSchemaObjectSummaryResponse_httpStatus,

    -- ** DescribeFleetAdvisorSchemas
    describeFleetAdvisorSchemas_nextToken,
    describeFleetAdvisorSchemas_filters,
    describeFleetAdvisorSchemas_maxRecords,
    describeFleetAdvisorSchemasResponse_nextToken,
    describeFleetAdvisorSchemasResponse_fleetAdvisorSchemas,
    describeFleetAdvisorSchemasResponse_httpStatus,

    -- ** DescribeOrderableReplicationInstances
    describeOrderableReplicationInstances_marker,
    describeOrderableReplicationInstances_maxRecords,
    describeOrderableReplicationInstancesResponse_marker,
    describeOrderableReplicationInstancesResponse_orderableReplicationInstances,
    describeOrderableReplicationInstancesResponse_httpStatus,

    -- ** DescribePendingMaintenanceActions
    describePendingMaintenanceActions_replicationInstanceArn,
    describePendingMaintenanceActions_marker,
    describePendingMaintenanceActions_filters,
    describePendingMaintenanceActions_maxRecords,
    describePendingMaintenanceActionsResponse_marker,
    describePendingMaintenanceActionsResponse_pendingMaintenanceActions,
    describePendingMaintenanceActionsResponse_httpStatus,

    -- ** DescribeRefreshSchemasStatus
    describeRefreshSchemasStatus_endpointArn,
    describeRefreshSchemasStatusResponse_refreshSchemasStatus,
    describeRefreshSchemasStatusResponse_httpStatus,

    -- ** DescribeReplicationInstanceTaskLogs
    describeReplicationInstanceTaskLogs_marker,
    describeReplicationInstanceTaskLogs_maxRecords,
    describeReplicationInstanceTaskLogs_replicationInstanceArn,
    describeReplicationInstanceTaskLogsResponse_replicationInstanceArn,
    describeReplicationInstanceTaskLogsResponse_marker,
    describeReplicationInstanceTaskLogsResponse_replicationInstanceTaskLogs,
    describeReplicationInstanceTaskLogsResponse_httpStatus,

    -- ** DescribeReplicationInstances
    describeReplicationInstances_marker,
    describeReplicationInstances_filters,
    describeReplicationInstances_maxRecords,
    describeReplicationInstancesResponse_marker,
    describeReplicationInstancesResponse_replicationInstances,
    describeReplicationInstancesResponse_httpStatus,

    -- ** DescribeReplicationSubnetGroups
    describeReplicationSubnetGroups_marker,
    describeReplicationSubnetGroups_filters,
    describeReplicationSubnetGroups_maxRecords,
    describeReplicationSubnetGroupsResponse_marker,
    describeReplicationSubnetGroupsResponse_replicationSubnetGroups,
    describeReplicationSubnetGroupsResponse_httpStatus,

    -- ** DescribeReplicationTaskAssessmentResults
    describeReplicationTaskAssessmentResults_marker,
    describeReplicationTaskAssessmentResults_maxRecords,
    describeReplicationTaskAssessmentResults_replicationTaskArn,
    describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults,
    describeReplicationTaskAssessmentResultsResponse_marker,
    describeReplicationTaskAssessmentResultsResponse_bucketName,
    describeReplicationTaskAssessmentResultsResponse_httpStatus,

    -- ** DescribeReplicationTaskAssessmentRuns
    describeReplicationTaskAssessmentRuns_marker,
    describeReplicationTaskAssessmentRuns_filters,
    describeReplicationTaskAssessmentRuns_maxRecords,
    describeReplicationTaskAssessmentRunsResponse_marker,
    describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns,
    describeReplicationTaskAssessmentRunsResponse_httpStatus,

    -- ** DescribeReplicationTaskIndividualAssessments
    describeReplicationTaskIndividualAssessments_marker,
    describeReplicationTaskIndividualAssessments_filters,
    describeReplicationTaskIndividualAssessments_maxRecords,
    describeReplicationTaskIndividualAssessmentsResponse_marker,
    describeReplicationTaskIndividualAssessmentsResponse_replicationTaskIndividualAssessments,
    describeReplicationTaskIndividualAssessmentsResponse_httpStatus,

    -- ** DescribeReplicationTasks
    describeReplicationTasks_marker,
    describeReplicationTasks_filters,
    describeReplicationTasks_maxRecords,
    describeReplicationTasks_withoutSettings,
    describeReplicationTasksResponse_marker,
    describeReplicationTasksResponse_replicationTasks,
    describeReplicationTasksResponse_httpStatus,

    -- ** DescribeSchemas
    describeSchemas_marker,
    describeSchemas_maxRecords,
    describeSchemas_endpointArn,
    describeSchemasResponse_marker,
    describeSchemasResponse_schemas,
    describeSchemasResponse_httpStatus,

    -- ** DescribeTableStatistics
    describeTableStatistics_marker,
    describeTableStatistics_filters,
    describeTableStatistics_maxRecords,
    describeTableStatistics_replicationTaskArn,
    describeTableStatisticsResponse_marker,
    describeTableStatisticsResponse_tableStatistics,
    describeTableStatisticsResponse_replicationTaskArn,
    describeTableStatisticsResponse_httpStatus,

    -- ** ImportCertificate
    importCertificate_tags,
    importCertificate_certificateWallet,
    importCertificate_certificatePem,
    importCertificate_certificateIdentifier,
    importCertificateResponse_certificate,
    importCertificateResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArnList,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** ModifyEndpoint
    modifyEndpoint_port,
    modifyEndpoint_elasticsearchSettings,
    modifyEndpoint_redshiftSettings,
    modifyEndpoint_externalTableDefinition,
    modifyEndpoint_engineName,
    modifyEndpoint_mySQLSettings,
    modifyEndpoint_sslMode,
    modifyEndpoint_endpointIdentifier,
    modifyEndpoint_password,
    modifyEndpoint_serverName,
    modifyEndpoint_docDbSettings,
    modifyEndpoint_exactSettings,
    modifyEndpoint_databaseName,
    modifyEndpoint_username,
    modifyEndpoint_gcpMySQLSettings,
    modifyEndpoint_serviceAccessRoleArn,
    modifyEndpoint_extraConnectionAttributes,
    modifyEndpoint_neptuneSettings,
    modifyEndpoint_endpointType,
    modifyEndpoint_kinesisSettings,
    modifyEndpoint_oracleSettings,
    modifyEndpoint_certificateArn,
    modifyEndpoint_dynamoDbSettings,
    modifyEndpoint_redisSettings,
    modifyEndpoint_s3Settings,
    modifyEndpoint_microsoftSQLServerSettings,
    modifyEndpoint_kafkaSettings,
    modifyEndpoint_dmsTransferSettings,
    modifyEndpoint_sybaseSettings,
    modifyEndpoint_postgreSQLSettings,
    modifyEndpoint_iBMDb2Settings,
    modifyEndpoint_mongoDbSettings,
    modifyEndpoint_endpointArn,
    modifyEndpointResponse_endpoint,
    modifyEndpointResponse_httpStatus,

    -- ** ModifyEventSubscription
    modifyEventSubscription_sourceType,
    modifyEventSubscription_enabled,
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_subscriptionName,
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,

    -- ** ModifyReplicationInstance
    modifyReplicationInstance_replicationInstanceIdentifier,
    modifyReplicationInstance_vpcSecurityGroupIds,
    modifyReplicationInstance_autoMinorVersionUpgrade,
    modifyReplicationInstance_applyImmediately,
    modifyReplicationInstance_allowMajorVersionUpgrade,
    modifyReplicationInstance_allocatedStorage,
    modifyReplicationInstance_preferredMaintenanceWindow,
    modifyReplicationInstance_replicationInstanceClass,
    modifyReplicationInstance_engineVersion,
    modifyReplicationInstance_networkType,
    modifyReplicationInstance_multiAZ,
    modifyReplicationInstance_replicationInstanceArn,
    modifyReplicationInstanceResponse_replicationInstance,
    modifyReplicationInstanceResponse_httpStatus,

    -- ** ModifyReplicationSubnetGroup
    modifyReplicationSubnetGroup_replicationSubnetGroupDescription,
    modifyReplicationSubnetGroup_replicationSubnetGroupIdentifier,
    modifyReplicationSubnetGroup_subnetIds,
    modifyReplicationSubnetGroupResponse_replicationSubnetGroup,
    modifyReplicationSubnetGroupResponse_httpStatus,

    -- ** ModifyReplicationTask
    modifyReplicationTask_cdcStartTime,
    modifyReplicationTask_replicationTaskIdentifier,
    modifyReplicationTask_taskData,
    modifyReplicationTask_cdcStartPosition,
    modifyReplicationTask_replicationTaskSettings,
    modifyReplicationTask_tableMappings,
    modifyReplicationTask_migrationType,
    modifyReplicationTask_cdcStopPosition,
    modifyReplicationTask_replicationTaskArn,
    modifyReplicationTaskResponse_replicationTask,
    modifyReplicationTaskResponse_httpStatus,

    -- ** MoveReplicationTask
    moveReplicationTask_replicationTaskArn,
    moveReplicationTask_targetReplicationInstanceArn,
    moveReplicationTaskResponse_replicationTask,
    moveReplicationTaskResponse_httpStatus,

    -- ** RebootReplicationInstance
    rebootReplicationInstance_forcePlannedFailover,
    rebootReplicationInstance_forceFailover,
    rebootReplicationInstance_replicationInstanceArn,
    rebootReplicationInstanceResponse_replicationInstance,
    rebootReplicationInstanceResponse_httpStatus,

    -- ** RefreshSchemas
    refreshSchemas_endpointArn,
    refreshSchemas_replicationInstanceArn,
    refreshSchemasResponse_refreshSchemasStatus,
    refreshSchemasResponse_httpStatus,

    -- ** ReloadTables
    reloadTables_reloadOption,
    reloadTables_replicationTaskArn,
    reloadTables_tablesToReload,
    reloadTablesResponse_replicationTaskArn,
    reloadTablesResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceArn,
    removeTagsFromResource_tagKeys,
    removeTagsFromResourceResponse_httpStatus,

    -- ** RunFleetAdvisorLsaAnalysis
    runFleetAdvisorLsaAnalysisResponse_lsaAnalysisId,
    runFleetAdvisorLsaAnalysisResponse_status,
    runFleetAdvisorLsaAnalysisResponse_httpStatus,

    -- ** StartReplicationTask
    startReplicationTask_cdcStartTime,
    startReplicationTask_cdcStartPosition,
    startReplicationTask_cdcStopPosition,
    startReplicationTask_replicationTaskArn,
    startReplicationTask_startReplicationTaskType,
    startReplicationTaskResponse_replicationTask,
    startReplicationTaskResponse_httpStatus,

    -- ** StartReplicationTaskAssessment
    startReplicationTaskAssessment_replicationTaskArn,
    startReplicationTaskAssessmentResponse_replicationTask,
    startReplicationTaskAssessmentResponse_httpStatus,

    -- ** StartReplicationTaskAssessmentRun
    startReplicationTaskAssessmentRun_resultLocationFolder,
    startReplicationTaskAssessmentRun_resultEncryptionMode,
    startReplicationTaskAssessmentRun_includeOnly,
    startReplicationTaskAssessmentRun_resultKmsKeyArn,
    startReplicationTaskAssessmentRun_exclude,
    startReplicationTaskAssessmentRun_replicationTaskArn,
    startReplicationTaskAssessmentRun_serviceAccessRoleArn,
    startReplicationTaskAssessmentRun_resultLocationBucket,
    startReplicationTaskAssessmentRun_assessmentRunName,
    startReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    startReplicationTaskAssessmentRunResponse_httpStatus,

    -- ** StopReplicationTask
    stopReplicationTask_replicationTaskArn,
    stopReplicationTaskResponse_replicationTask,
    stopReplicationTaskResponse_httpStatus,

    -- ** TestConnection
    testConnection_replicationInstanceArn,
    testConnection_endpointArn,
    testConnectionResponse_connection,
    testConnectionResponse_httpStatus,

    -- ** UpdateSubscriptionsToEventBridge
    updateSubscriptionsToEventBridge_forceMove,
    updateSubscriptionsToEventBridgeResponse_result,
    updateSubscriptionsToEventBridgeResponse_httpStatus,

    -- * Types

    -- ** AccountQuota
    accountQuota_max,
    accountQuota_used,
    accountQuota_accountQuotaName,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** Certificate
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

    -- ** CollectorHealthCheck
    collectorHealthCheck_collectorStatus,
    collectorHealthCheck_localCollectorS3Access,
    collectorHealthCheck_webCollectorGrantedRoleBasedAccess,
    collectorHealthCheck_webCollectorS3Access,

    -- ** CollectorResponse
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

    -- ** CollectorShortInfoResponse
    collectorShortInfoResponse_collectorName,
    collectorShortInfoResponse_collectorReferencedId,

    -- ** Connection
    connection_replicationInstanceIdentifier,
    connection_replicationInstanceArn,
    connection_lastFailureMessage,
    connection_endpointIdentifier,
    connection_status,
    connection_endpointArn,

    -- ** DatabaseInstanceSoftwareDetailsResponse
    databaseInstanceSoftwareDetailsResponse_engineEdition,
    databaseInstanceSoftwareDetailsResponse_servicePack,
    databaseInstanceSoftwareDetailsResponse_engine,
    databaseInstanceSoftwareDetailsResponse_tooltip,
    databaseInstanceSoftwareDetailsResponse_supportLevel,
    databaseInstanceSoftwareDetailsResponse_engineVersion,
    databaseInstanceSoftwareDetailsResponse_osArchitecture,

    -- ** DatabaseResponse
    databaseResponse_numberOfSchemas,
    databaseResponse_databaseName,
    databaseResponse_databaseId,
    databaseResponse_server,
    databaseResponse_collectors,
    databaseResponse_ipAddress,
    databaseResponse_softwareDetails,

    -- ** DatabaseShortInfoResponse
    databaseShortInfoResponse_databaseEngine,
    databaseShortInfoResponse_databaseName,
    databaseShortInfoResponse_databaseId,
    databaseShortInfoResponse_databaseIpAddress,

    -- ** DmsTransferSettings
    dmsTransferSettings_serviceAccessRoleArn,
    dmsTransferSettings_bucketName,

    -- ** DocDbSettings
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

    -- ** DynamoDbSettings
    dynamoDbSettings_serviceAccessRoleArn,

    -- ** ElasticsearchSettings
    elasticsearchSettings_errorRetryDuration,
    elasticsearchSettings_fullLoadErrorPercentage,
    elasticsearchSettings_useNewMappingType,
    elasticsearchSettings_serviceAccessRoleArn,
    elasticsearchSettings_endpointUri,

    -- ** Endpoint
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

    -- ** EndpointSetting
    endpointSetting_intValueMax,
    endpointSetting_name,
    endpointSetting_type,
    endpointSetting_defaultValue,
    endpointSetting_units,
    endpointSetting_applicability,
    endpointSetting_sensitive,
    endpointSetting_intValueMin,
    endpointSetting_enumValues,

    -- ** Event
    event_message,
    event_date,
    event_sourceType,
    event_sourceIdentifier,
    event_eventCategories,

    -- ** EventCategoryGroup
    eventCategoryGroup_sourceType,
    eventCategoryGroup_eventCategories,

    -- ** EventSubscription
    eventSubscription_subscriptionCreationTime,
    eventSubscription_custSubscriptionId,
    eventSubscription_sourceIdsList,
    eventSubscription_status,
    eventSubscription_sourceType,
    eventSubscription_enabled,
    eventSubscription_snsTopicArn,
    eventSubscription_eventCategoriesList,
    eventSubscription_customerAwsId,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** FleetAdvisorLsaAnalysisResponse
    fleetAdvisorLsaAnalysisResponse_lsaAnalysisId,
    fleetAdvisorLsaAnalysisResponse_status,

    -- ** FleetAdvisorSchemaObjectResponse
    fleetAdvisorSchemaObjectResponse_codeLineCount,
    fleetAdvisorSchemaObjectResponse_numberOfObjects,
    fleetAdvisorSchemaObjectResponse_schemaId,
    fleetAdvisorSchemaObjectResponse_objectType,
    fleetAdvisorSchemaObjectResponse_codeSize,

    -- ** GcpMySQLSettings
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

    -- ** IBMDb2Settings
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

    -- ** InventoryData
    inventoryData_numberOfSchemas,
    inventoryData_numberOfDatabases,

    -- ** KafkaSettings
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

    -- ** KinesisSettings
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

    -- ** MicrosoftSQLServerSettings
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

    -- ** MongoDbSettings
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

    -- ** MySQLSettings
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

    -- ** NeptuneSettings
    neptuneSettings_maxFileSize,
    neptuneSettings_maxRetryCount,
    neptuneSettings_serviceAccessRoleArn,
    neptuneSettings_errorRetryDuration,
    neptuneSettings_iamAuthEnabled,
    neptuneSettings_s3BucketName,
    neptuneSettings_s3BucketFolder,

    -- ** OracleSettings
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

    -- ** OrderableReplicationInstance
    orderableReplicationInstance_maxAllocatedStorage,
    orderableReplicationInstance_defaultAllocatedStorage,
    orderableReplicationInstance_availabilityZones,
    orderableReplicationInstance_minAllocatedStorage,
    orderableReplicationInstance_storageType,
    orderableReplicationInstance_releaseStatus,
    orderableReplicationInstance_includedAllocatedStorage,
    orderableReplicationInstance_replicationInstanceClass,
    orderableReplicationInstance_engineVersion,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_forcedApplyDate,

    -- ** PostgreSQLSettings
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

    -- ** RedisSettings
    redisSettings_authUserName,
    redisSettings_sslSecurityProtocol,
    redisSettings_authPassword,
    redisSettings_sslCaCertificateArn,
    redisSettings_authType,
    redisSettings_serverName,
    redisSettings_port,

    -- ** RedshiftSettings
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

    -- ** RefreshSchemasStatus
    refreshSchemasStatus_replicationInstanceArn,
    refreshSchemasStatus_lastRefreshDate,
    refreshSchemasStatus_lastFailureMessage,
    refreshSchemasStatus_status,
    refreshSchemasStatus_endpointArn,

    -- ** ReplicationInstance
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

    -- ** ReplicationInstanceTaskLog
    replicationInstanceTaskLog_replicationInstanceTaskLogSize,
    replicationInstanceTaskLog_replicationTaskName,
    replicationInstanceTaskLog_replicationTaskArn,

    -- ** ReplicationPendingModifiedValues
    replicationPendingModifiedValues_allocatedStorage,
    replicationPendingModifiedValues_replicationInstanceClass,
    replicationPendingModifiedValues_engineVersion,
    replicationPendingModifiedValues_networkType,
    replicationPendingModifiedValues_multiAZ,

    -- ** ReplicationSubnetGroup
    replicationSubnetGroup_subnetGroupStatus,
    replicationSubnetGroup_subnets,
    replicationSubnetGroup_replicationSubnetGroupDescription,
    replicationSubnetGroup_vpcId,
    replicationSubnetGroup_replicationSubnetGroupIdentifier,
    replicationSubnetGroup_supportedNetworkTypes,

    -- ** ReplicationTask
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

    -- ** ReplicationTaskAssessmentResult
    replicationTaskAssessmentResult_assessmentResults,
    replicationTaskAssessmentResult_s3ObjectUrl,
    replicationTaskAssessmentResult_replicationTaskIdentifier,
    replicationTaskAssessmentResult_replicationTaskLastAssessmentDate,
    replicationTaskAssessmentResult_replicationTaskArn,
    replicationTaskAssessmentResult_assessmentResultsFile,
    replicationTaskAssessmentResult_assessmentStatus,

    -- ** ReplicationTaskAssessmentRun
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

    -- ** ReplicationTaskAssessmentRunProgress
    replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount,
    replicationTaskAssessmentRunProgress_individualAssessmentCount,

    -- ** ReplicationTaskIndividualAssessment
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate,
    replicationTaskIndividualAssessment_individualAssessmentName,
    replicationTaskIndividualAssessment_status,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn,
    replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn,

    -- ** ReplicationTaskStats
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

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_resourceIdentifier,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,

    -- ** S3Settings
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

    -- ** SchemaResponse
    schemaResponse_databaseInstance,
    schemaResponse_similarity,
    schemaResponse_originalSchema,
    schemaResponse_codeLineCount,
    schemaResponse_schemaName,
    schemaResponse_server,
    schemaResponse_complexity,
    schemaResponse_schemaId,
    schemaResponse_codeSize,

    -- ** SchemaShortInfoResponse
    schemaShortInfoResponse_databaseName,
    schemaShortInfoResponse_schemaName,
    schemaShortInfoResponse_databaseId,
    schemaShortInfoResponse_databaseIpAddress,
    schemaShortInfoResponse_schemaId,

    -- ** ServerShortInfoResponse
    serverShortInfoResponse_serverName,
    serverShortInfoResponse_serverId,
    serverShortInfoResponse_ipAddress,

    -- ** Subnet
    subnet_subnetIdentifier,
    subnet_subnetStatus,
    subnet_subnetAvailabilityZone,

    -- ** SupportedEndpointType
    supportedEndpointType_engineName,
    supportedEndpointType_engineDisplayName,
    supportedEndpointType_supportsCDC,
    supportedEndpointType_endpointType,
    supportedEndpointType_replicationInstanceEngineMinimumVersion,

    -- ** SybaseSettings
    sybaseSettings_port,
    sybaseSettings_secretsManagerAccessRoleArn,
    sybaseSettings_password,
    sybaseSettings_serverName,
    sybaseSettings_databaseName,
    sybaseSettings_username,
    sybaseSettings_secretsManagerSecretId,

    -- ** TableStatistics
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

import Amazonka.DMS.AddTagsToResource
import Amazonka.DMS.ApplyPendingMaintenanceAction
import Amazonka.DMS.CancelReplicationTaskAssessmentRun
import Amazonka.DMS.CreateEndpoint
import Amazonka.DMS.CreateEventSubscription
import Amazonka.DMS.CreateFleetAdvisorCollector
import Amazonka.DMS.CreateReplicationInstance
import Amazonka.DMS.CreateReplicationSubnetGroup
import Amazonka.DMS.CreateReplicationTask
import Amazonka.DMS.DeleteCertificate
import Amazonka.DMS.DeleteConnection
import Amazonka.DMS.DeleteEndpoint
import Amazonka.DMS.DeleteEventSubscription
import Amazonka.DMS.DeleteFleetAdvisorCollector
import Amazonka.DMS.DeleteFleetAdvisorDatabases
import Amazonka.DMS.DeleteReplicationInstance
import Amazonka.DMS.DeleteReplicationSubnetGroup
import Amazonka.DMS.DeleteReplicationTask
import Amazonka.DMS.DeleteReplicationTaskAssessmentRun
import Amazonka.DMS.DescribeAccountAttributes
import Amazonka.DMS.DescribeApplicableIndividualAssessments
import Amazonka.DMS.DescribeCertificates
import Amazonka.DMS.DescribeConnections
import Amazonka.DMS.DescribeEndpointSettings
import Amazonka.DMS.DescribeEndpointTypes
import Amazonka.DMS.DescribeEndpoints
import Amazonka.DMS.DescribeEventCategories
import Amazonka.DMS.DescribeEventSubscriptions
import Amazonka.DMS.DescribeEvents
import Amazonka.DMS.DescribeFleetAdvisorCollectors
import Amazonka.DMS.DescribeFleetAdvisorDatabases
import Amazonka.DMS.DescribeFleetAdvisorLsaAnalysis
import Amazonka.DMS.DescribeFleetAdvisorSchemaObjectSummary
import Amazonka.DMS.DescribeFleetAdvisorSchemas
import Amazonka.DMS.DescribeOrderableReplicationInstances
import Amazonka.DMS.DescribePendingMaintenanceActions
import Amazonka.DMS.DescribeRefreshSchemasStatus
import Amazonka.DMS.DescribeReplicationInstanceTaskLogs
import Amazonka.DMS.DescribeReplicationInstances
import Amazonka.DMS.DescribeReplicationSubnetGroups
import Amazonka.DMS.DescribeReplicationTaskAssessmentResults
import Amazonka.DMS.DescribeReplicationTaskAssessmentRuns
import Amazonka.DMS.DescribeReplicationTaskIndividualAssessments
import Amazonka.DMS.DescribeReplicationTasks
import Amazonka.DMS.DescribeSchemas
import Amazonka.DMS.DescribeTableStatistics
import Amazonka.DMS.ImportCertificate
import Amazonka.DMS.ListTagsForResource
import Amazonka.DMS.ModifyEndpoint
import Amazonka.DMS.ModifyEventSubscription
import Amazonka.DMS.ModifyReplicationInstance
import Amazonka.DMS.ModifyReplicationSubnetGroup
import Amazonka.DMS.ModifyReplicationTask
import Amazonka.DMS.MoveReplicationTask
import Amazonka.DMS.RebootReplicationInstance
import Amazonka.DMS.RefreshSchemas
import Amazonka.DMS.ReloadTables
import Amazonka.DMS.RemoveTagsFromResource
import Amazonka.DMS.RunFleetAdvisorLsaAnalysis
import Amazonka.DMS.StartReplicationTask
import Amazonka.DMS.StartReplicationTaskAssessment
import Amazonka.DMS.StartReplicationTaskAssessmentRun
import Amazonka.DMS.StopReplicationTask
import Amazonka.DMS.TestConnection
import Amazonka.DMS.Types.AccountQuota
import Amazonka.DMS.Types.AvailabilityZone
import Amazonka.DMS.Types.Certificate
import Amazonka.DMS.Types.CollectorHealthCheck
import Amazonka.DMS.Types.CollectorResponse
import Amazonka.DMS.Types.CollectorShortInfoResponse
import Amazonka.DMS.Types.Connection
import Amazonka.DMS.Types.DatabaseInstanceSoftwareDetailsResponse
import Amazonka.DMS.Types.DatabaseResponse
import Amazonka.DMS.Types.DatabaseShortInfoResponse
import Amazonka.DMS.Types.DmsTransferSettings
import Amazonka.DMS.Types.DocDbSettings
import Amazonka.DMS.Types.DynamoDbSettings
import Amazonka.DMS.Types.ElasticsearchSettings
import Amazonka.DMS.Types.Endpoint
import Amazonka.DMS.Types.EndpointSetting
import Amazonka.DMS.Types.Event
import Amazonka.DMS.Types.EventCategoryGroup
import Amazonka.DMS.Types.EventSubscription
import Amazonka.DMS.Types.Filter
import Amazonka.DMS.Types.FleetAdvisorLsaAnalysisResponse
import Amazonka.DMS.Types.FleetAdvisorSchemaObjectResponse
import Amazonka.DMS.Types.GcpMySQLSettings
import Amazonka.DMS.Types.IBMDb2Settings
import Amazonka.DMS.Types.InventoryData
import Amazonka.DMS.Types.KafkaSettings
import Amazonka.DMS.Types.KinesisSettings
import Amazonka.DMS.Types.MicrosoftSQLServerSettings
import Amazonka.DMS.Types.MongoDbSettings
import Amazonka.DMS.Types.MySQLSettings
import Amazonka.DMS.Types.NeptuneSettings
import Amazonka.DMS.Types.OracleSettings
import Amazonka.DMS.Types.OrderableReplicationInstance
import Amazonka.DMS.Types.PendingMaintenanceAction
import Amazonka.DMS.Types.PostgreSQLSettings
import Amazonka.DMS.Types.RedisSettings
import Amazonka.DMS.Types.RedshiftSettings
import Amazonka.DMS.Types.RefreshSchemasStatus
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
import Amazonka.DMS.Types.SchemaResponse
import Amazonka.DMS.Types.SchemaShortInfoResponse
import Amazonka.DMS.Types.ServerShortInfoResponse
import Amazonka.DMS.Types.Subnet
import Amazonka.DMS.Types.SupportedEndpointType
import Amazonka.DMS.Types.SybaseSettings
import Amazonka.DMS.Types.TableStatistics
import Amazonka.DMS.Types.TableToReload
import Amazonka.DMS.Types.Tag
import Amazonka.DMS.Types.VpcSecurityGroupMembership
import Amazonka.DMS.UpdateSubscriptionsToEventBridge
