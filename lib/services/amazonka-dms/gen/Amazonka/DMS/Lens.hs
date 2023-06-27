{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DMS.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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

    -- ** BatchStartRecommendations
    batchStartRecommendations_data,
    batchStartRecommendationsResponse_errorEntries,
    batchStartRecommendationsResponse_httpStatus,

    -- ** CancelReplicationTaskAssessmentRun
    cancelReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn,
    cancelReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    cancelReplicationTaskAssessmentRunResponse_httpStatus,

    -- ** CreateEndpoint
    createEndpoint_certificateArn,
    createEndpoint_databaseName,
    createEndpoint_dmsTransferSettings,
    createEndpoint_docDbSettings,
    createEndpoint_dynamoDbSettings,
    createEndpoint_elasticsearchSettings,
    createEndpoint_externalTableDefinition,
    createEndpoint_extraConnectionAttributes,
    createEndpoint_gcpMySQLSettings,
    createEndpoint_iBMDb2Settings,
    createEndpoint_kafkaSettings,
    createEndpoint_kinesisSettings,
    createEndpoint_kmsKeyId,
    createEndpoint_microsoftSQLServerSettings,
    createEndpoint_mongoDbSettings,
    createEndpoint_mySQLSettings,
    createEndpoint_neptuneSettings,
    createEndpoint_oracleSettings,
    createEndpoint_password,
    createEndpoint_port,
    createEndpoint_postgreSQLSettings,
    createEndpoint_redisSettings,
    createEndpoint_redshiftSettings,
    createEndpoint_resourceIdentifier,
    createEndpoint_s3Settings,
    createEndpoint_serverName,
    createEndpoint_serviceAccessRoleArn,
    createEndpoint_sslMode,
    createEndpoint_sybaseSettings,
    createEndpoint_tags,
    createEndpoint_username,
    createEndpoint_endpointIdentifier,
    createEndpoint_endpointType,
    createEndpoint_engineName,
    createEndpointResponse_endpoint,
    createEndpointResponse_httpStatus,

    -- ** CreateEventSubscription
    createEventSubscription_enabled,
    createEventSubscription_eventCategories,
    createEventSubscription_sourceIds,
    createEventSubscription_sourceType,
    createEventSubscription_tags,
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
    createFleetAdvisorCollectorResponse_collectorReferencedId,
    createFleetAdvisorCollectorResponse_description,
    createFleetAdvisorCollectorResponse_s3BucketName,
    createFleetAdvisorCollectorResponse_serviceAccessRoleArn,
    createFleetAdvisorCollectorResponse_httpStatus,

    -- ** CreateReplicationInstance
    createReplicationInstance_allocatedStorage,
    createReplicationInstance_autoMinorVersionUpgrade,
    createReplicationInstance_availabilityZone,
    createReplicationInstance_dnsNameServers,
    createReplicationInstance_engineVersion,
    createReplicationInstance_kmsKeyId,
    createReplicationInstance_multiAZ,
    createReplicationInstance_networkType,
    createReplicationInstance_preferredMaintenanceWindow,
    createReplicationInstance_publiclyAccessible,
    createReplicationInstance_replicationSubnetGroupIdentifier,
    createReplicationInstance_resourceIdentifier,
    createReplicationInstance_tags,
    createReplicationInstance_vpcSecurityGroupIds,
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
    createReplicationTask_cdcStartPosition,
    createReplicationTask_cdcStartTime,
    createReplicationTask_cdcStopPosition,
    createReplicationTask_replicationTaskSettings,
    createReplicationTask_resourceIdentifier,
    createReplicationTask_tags,
    createReplicationTask_taskData,
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
    describeAccountAttributesResponse_accountQuotas,
    describeAccountAttributesResponse_uniqueAccountIdentifier,
    describeAccountAttributesResponse_httpStatus,

    -- ** DescribeApplicableIndividualAssessments
    describeApplicableIndividualAssessments_marker,
    describeApplicableIndividualAssessments_maxRecords,
    describeApplicableIndividualAssessments_migrationType,
    describeApplicableIndividualAssessments_replicationInstanceArn,
    describeApplicableIndividualAssessments_replicationTaskArn,
    describeApplicableIndividualAssessments_sourceEngineName,
    describeApplicableIndividualAssessments_targetEngineName,
    describeApplicableIndividualAssessmentsResponse_individualAssessmentNames,
    describeApplicableIndividualAssessmentsResponse_marker,
    describeApplicableIndividualAssessmentsResponse_httpStatus,

    -- ** DescribeCertificates
    describeCertificates_filters,
    describeCertificates_marker,
    describeCertificates_maxRecords,
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_marker,
    describeCertificatesResponse_httpStatus,

    -- ** DescribeConnections
    describeConnections_filters,
    describeConnections_marker,
    describeConnections_maxRecords,
    describeConnectionsResponse_connections,
    describeConnectionsResponse_marker,
    describeConnectionsResponse_httpStatus,

    -- ** DescribeEndpointSettings
    describeEndpointSettings_marker,
    describeEndpointSettings_maxRecords,
    describeEndpointSettings_engineName,
    describeEndpointSettingsResponse_endpointSettings,
    describeEndpointSettingsResponse_marker,
    describeEndpointSettingsResponse_httpStatus,

    -- ** DescribeEndpointTypes
    describeEndpointTypes_filters,
    describeEndpointTypes_marker,
    describeEndpointTypes_maxRecords,
    describeEndpointTypesResponse_marker,
    describeEndpointTypesResponse_supportedEndpointTypes,
    describeEndpointTypesResponse_httpStatus,

    -- ** DescribeEndpoints
    describeEndpoints_filters,
    describeEndpoints_marker,
    describeEndpoints_maxRecords,
    describeEndpointsResponse_endpoints,
    describeEndpointsResponse_marker,
    describeEndpointsResponse_httpStatus,

    -- ** DescribeEventCategories
    describeEventCategories_filters,
    describeEventCategories_sourceType,
    describeEventCategoriesResponse_eventCategoryGroupList,
    describeEventCategoriesResponse_httpStatus,

    -- ** DescribeEventSubscriptions
    describeEventSubscriptions_filters,
    describeEventSubscriptions_marker,
    describeEventSubscriptions_maxRecords,
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_duration,
    describeEvents_endTime,
    describeEvents_eventCategories,
    describeEvents_filters,
    describeEvents_marker,
    describeEvents_maxRecords,
    describeEvents_sourceIdentifier,
    describeEvents_sourceType,
    describeEvents_startTime,
    describeEventsResponse_events,
    describeEventsResponse_marker,
    describeEventsResponse_httpStatus,

    -- ** DescribeFleetAdvisorCollectors
    describeFleetAdvisorCollectors_filters,
    describeFleetAdvisorCollectors_maxRecords,
    describeFleetAdvisorCollectors_nextToken,
    describeFleetAdvisorCollectorsResponse_collectors,
    describeFleetAdvisorCollectorsResponse_nextToken,
    describeFleetAdvisorCollectorsResponse_httpStatus,

    -- ** DescribeFleetAdvisorDatabases
    describeFleetAdvisorDatabases_filters,
    describeFleetAdvisorDatabases_maxRecords,
    describeFleetAdvisorDatabases_nextToken,
    describeFleetAdvisorDatabasesResponse_databases,
    describeFleetAdvisorDatabasesResponse_nextToken,
    describeFleetAdvisorDatabasesResponse_httpStatus,

    -- ** DescribeFleetAdvisorLsaAnalysis
    describeFleetAdvisorLsaAnalysis_maxRecords,
    describeFleetAdvisorLsaAnalysis_nextToken,
    describeFleetAdvisorLsaAnalysisResponse_analysis,
    describeFleetAdvisorLsaAnalysisResponse_nextToken,
    describeFleetAdvisorLsaAnalysisResponse_httpStatus,

    -- ** DescribeFleetAdvisorSchemaObjectSummary
    describeFleetAdvisorSchemaObjectSummary_filters,
    describeFleetAdvisorSchemaObjectSummary_maxRecords,
    describeFleetAdvisorSchemaObjectSummary_nextToken,
    describeFleetAdvisorSchemaObjectSummaryResponse_fleetAdvisorSchemaObjects,
    describeFleetAdvisorSchemaObjectSummaryResponse_nextToken,
    describeFleetAdvisorSchemaObjectSummaryResponse_httpStatus,

    -- ** DescribeFleetAdvisorSchemas
    describeFleetAdvisorSchemas_filters,
    describeFleetAdvisorSchemas_maxRecords,
    describeFleetAdvisorSchemas_nextToken,
    describeFleetAdvisorSchemasResponse_fleetAdvisorSchemas,
    describeFleetAdvisorSchemasResponse_nextToken,
    describeFleetAdvisorSchemasResponse_httpStatus,

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
    describePendingMaintenanceActionsResponse_marker,
    describePendingMaintenanceActionsResponse_pendingMaintenanceActions,
    describePendingMaintenanceActionsResponse_httpStatus,

    -- ** DescribeRecommendationLimitations
    describeRecommendationLimitations_filters,
    describeRecommendationLimitations_maxRecords,
    describeRecommendationLimitations_nextToken,
    describeRecommendationLimitationsResponse_limitations,
    describeRecommendationLimitationsResponse_nextToken,
    describeRecommendationLimitationsResponse_httpStatus,

    -- ** DescribeRecommendations
    describeRecommendations_filters,
    describeRecommendations_maxRecords,
    describeRecommendations_nextToken,
    describeRecommendationsResponse_nextToken,
    describeRecommendationsResponse_recommendations,
    describeRecommendationsResponse_httpStatus,

    -- ** DescribeRefreshSchemasStatus
    describeRefreshSchemasStatus_endpointArn,
    describeRefreshSchemasStatusResponse_refreshSchemasStatus,
    describeRefreshSchemasStatusResponse_httpStatus,

    -- ** DescribeReplicationInstanceTaskLogs
    describeReplicationInstanceTaskLogs_marker,
    describeReplicationInstanceTaskLogs_maxRecords,
    describeReplicationInstanceTaskLogs_replicationInstanceArn,
    describeReplicationInstanceTaskLogsResponse_marker,
    describeReplicationInstanceTaskLogsResponse_replicationInstanceArn,
    describeReplicationInstanceTaskLogsResponse_replicationInstanceTaskLogs,
    describeReplicationInstanceTaskLogsResponse_httpStatus,

    -- ** DescribeReplicationInstances
    describeReplicationInstances_filters,
    describeReplicationInstances_marker,
    describeReplicationInstances_maxRecords,
    describeReplicationInstancesResponse_marker,
    describeReplicationInstancesResponse_replicationInstances,
    describeReplicationInstancesResponse_httpStatus,

    -- ** DescribeReplicationSubnetGroups
    describeReplicationSubnetGroups_filters,
    describeReplicationSubnetGroups_marker,
    describeReplicationSubnetGroups_maxRecords,
    describeReplicationSubnetGroupsResponse_marker,
    describeReplicationSubnetGroupsResponse_replicationSubnetGroups,
    describeReplicationSubnetGroupsResponse_httpStatus,

    -- ** DescribeReplicationTaskAssessmentResults
    describeReplicationTaskAssessmentResults_marker,
    describeReplicationTaskAssessmentResults_maxRecords,
    describeReplicationTaskAssessmentResults_replicationTaskArn,
    describeReplicationTaskAssessmentResultsResponse_bucketName,
    describeReplicationTaskAssessmentResultsResponse_marker,
    describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults,
    describeReplicationTaskAssessmentResultsResponse_httpStatus,

    -- ** DescribeReplicationTaskAssessmentRuns
    describeReplicationTaskAssessmentRuns_filters,
    describeReplicationTaskAssessmentRuns_marker,
    describeReplicationTaskAssessmentRuns_maxRecords,
    describeReplicationTaskAssessmentRunsResponse_marker,
    describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns,
    describeReplicationTaskAssessmentRunsResponse_httpStatus,

    -- ** DescribeReplicationTaskIndividualAssessments
    describeReplicationTaskIndividualAssessments_filters,
    describeReplicationTaskIndividualAssessments_marker,
    describeReplicationTaskIndividualAssessments_maxRecords,
    describeReplicationTaskIndividualAssessmentsResponse_marker,
    describeReplicationTaskIndividualAssessmentsResponse_replicationTaskIndividualAssessments,
    describeReplicationTaskIndividualAssessmentsResponse_httpStatus,

    -- ** DescribeReplicationTasks
    describeReplicationTasks_filters,
    describeReplicationTasks_marker,
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
    describeTableStatistics_filters,
    describeTableStatistics_marker,
    describeTableStatistics_maxRecords,
    describeTableStatistics_replicationTaskArn,
    describeTableStatisticsResponse_marker,
    describeTableStatisticsResponse_replicationTaskArn,
    describeTableStatisticsResponse_tableStatistics,
    describeTableStatisticsResponse_httpStatus,

    -- ** ImportCertificate
    importCertificate_certificatePem,
    importCertificate_certificateWallet,
    importCertificate_tags,
    importCertificate_certificateIdentifier,
    importCertificateResponse_certificate,
    importCertificateResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResource_resourceArnList,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** ModifyEndpoint
    modifyEndpoint_certificateArn,
    modifyEndpoint_databaseName,
    modifyEndpoint_dmsTransferSettings,
    modifyEndpoint_docDbSettings,
    modifyEndpoint_dynamoDbSettings,
    modifyEndpoint_elasticsearchSettings,
    modifyEndpoint_endpointIdentifier,
    modifyEndpoint_endpointType,
    modifyEndpoint_engineName,
    modifyEndpoint_exactSettings,
    modifyEndpoint_externalTableDefinition,
    modifyEndpoint_extraConnectionAttributes,
    modifyEndpoint_gcpMySQLSettings,
    modifyEndpoint_iBMDb2Settings,
    modifyEndpoint_kafkaSettings,
    modifyEndpoint_kinesisSettings,
    modifyEndpoint_microsoftSQLServerSettings,
    modifyEndpoint_mongoDbSettings,
    modifyEndpoint_mySQLSettings,
    modifyEndpoint_neptuneSettings,
    modifyEndpoint_oracleSettings,
    modifyEndpoint_password,
    modifyEndpoint_port,
    modifyEndpoint_postgreSQLSettings,
    modifyEndpoint_redisSettings,
    modifyEndpoint_redshiftSettings,
    modifyEndpoint_s3Settings,
    modifyEndpoint_serverName,
    modifyEndpoint_serviceAccessRoleArn,
    modifyEndpoint_sslMode,
    modifyEndpoint_sybaseSettings,
    modifyEndpoint_username,
    modifyEndpoint_endpointArn,
    modifyEndpointResponse_endpoint,
    modifyEndpointResponse_httpStatus,

    -- ** ModifyEventSubscription
    modifyEventSubscription_enabled,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_sourceType,
    modifyEventSubscription_subscriptionName,
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,

    -- ** ModifyReplicationInstance
    modifyReplicationInstance_allocatedStorage,
    modifyReplicationInstance_allowMajorVersionUpgrade,
    modifyReplicationInstance_applyImmediately,
    modifyReplicationInstance_autoMinorVersionUpgrade,
    modifyReplicationInstance_engineVersion,
    modifyReplicationInstance_multiAZ,
    modifyReplicationInstance_networkType,
    modifyReplicationInstance_preferredMaintenanceWindow,
    modifyReplicationInstance_replicationInstanceClass,
    modifyReplicationInstance_replicationInstanceIdentifier,
    modifyReplicationInstance_vpcSecurityGroupIds,
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
    modifyReplicationTask_cdcStartPosition,
    modifyReplicationTask_cdcStartTime,
    modifyReplicationTask_cdcStopPosition,
    modifyReplicationTask_migrationType,
    modifyReplicationTask_replicationTaskIdentifier,
    modifyReplicationTask_replicationTaskSettings,
    modifyReplicationTask_tableMappings,
    modifyReplicationTask_taskData,
    modifyReplicationTask_replicationTaskArn,
    modifyReplicationTaskResponse_replicationTask,
    modifyReplicationTaskResponse_httpStatus,

    -- ** MoveReplicationTask
    moveReplicationTask_replicationTaskArn,
    moveReplicationTask_targetReplicationInstanceArn,
    moveReplicationTaskResponse_replicationTask,
    moveReplicationTaskResponse_httpStatus,

    -- ** RebootReplicationInstance
    rebootReplicationInstance_forceFailover,
    rebootReplicationInstance_forcePlannedFailover,
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

    -- ** StartRecommendations
    startRecommendations_databaseId,
    startRecommendations_settings,

    -- ** StartReplicationTask
    startReplicationTask_cdcStartPosition,
    startReplicationTask_cdcStartTime,
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
    startReplicationTaskAssessmentRun_exclude,
    startReplicationTaskAssessmentRun_includeOnly,
    startReplicationTaskAssessmentRun_resultEncryptionMode,
    startReplicationTaskAssessmentRun_resultKmsKeyArn,
    startReplicationTaskAssessmentRun_resultLocationFolder,
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
    accountQuota_accountQuotaName,
    accountQuota_max,
    accountQuota_used,

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** BatchStartRecommendationsErrorEntry
    batchStartRecommendationsErrorEntry_code,
    batchStartRecommendationsErrorEntry_databaseId,
    batchStartRecommendationsErrorEntry_message,

    -- ** Certificate
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

    -- ** CollectorHealthCheck
    collectorHealthCheck_collectorStatus,
    collectorHealthCheck_localCollectorS3Access,
    collectorHealthCheck_webCollectorGrantedRoleBasedAccess,
    collectorHealthCheck_webCollectorS3Access,

    -- ** CollectorResponse
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

    -- ** CollectorShortInfoResponse
    collectorShortInfoResponse_collectorName,
    collectorShortInfoResponse_collectorReferencedId,

    -- ** Connection
    connection_endpointArn,
    connection_endpointIdentifier,
    connection_lastFailureMessage,
    connection_replicationInstanceArn,
    connection_replicationInstanceIdentifier,
    connection_status,

    -- ** DatabaseInstanceSoftwareDetailsResponse
    databaseInstanceSoftwareDetailsResponse_engine,
    databaseInstanceSoftwareDetailsResponse_engineEdition,
    databaseInstanceSoftwareDetailsResponse_engineVersion,
    databaseInstanceSoftwareDetailsResponse_osArchitecture,
    databaseInstanceSoftwareDetailsResponse_servicePack,
    databaseInstanceSoftwareDetailsResponse_supportLevel,
    databaseInstanceSoftwareDetailsResponse_tooltip,

    -- ** DatabaseResponse
    databaseResponse_collectors,
    databaseResponse_databaseId,
    databaseResponse_databaseName,
    databaseResponse_ipAddress,
    databaseResponse_numberOfSchemas,
    databaseResponse_server,
    databaseResponse_softwareDetails,

    -- ** DatabaseShortInfoResponse
    databaseShortInfoResponse_databaseEngine,
    databaseShortInfoResponse_databaseId,
    databaseShortInfoResponse_databaseIpAddress,
    databaseShortInfoResponse_databaseName,

    -- ** DmsTransferSettings
    dmsTransferSettings_bucketName,
    dmsTransferSettings_serviceAccessRoleArn,

    -- ** DocDbSettings
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

    -- ** DynamoDbSettings
    dynamoDbSettings_serviceAccessRoleArn,

    -- ** ElasticsearchSettings
    elasticsearchSettings_errorRetryDuration,
    elasticsearchSettings_fullLoadErrorPercentage,
    elasticsearchSettings_useNewMappingType,
    elasticsearchSettings_serviceAccessRoleArn,
    elasticsearchSettings_endpointUri,

    -- ** Endpoint
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

    -- ** EndpointSetting
    endpointSetting_applicability,
    endpointSetting_defaultValue,
    endpointSetting_enumValues,
    endpointSetting_intValueMax,
    endpointSetting_intValueMin,
    endpointSetting_name,
    endpointSetting_sensitive,
    endpointSetting_type,
    endpointSetting_units,

    -- ** Event
    event_date,
    event_eventCategories,
    event_message,
    event_sourceIdentifier,
    event_sourceType,

    -- ** EventCategoryGroup
    eventCategoryGroup_eventCategories,
    eventCategoryGroup_sourceType,

    -- ** EventSubscription
    eventSubscription_custSubscriptionId,
    eventSubscription_customerAwsId,
    eventSubscription_enabled,
    eventSubscription_eventCategoriesList,
    eventSubscription_snsTopicArn,
    eventSubscription_sourceIdsList,
    eventSubscription_sourceType,
    eventSubscription_status,
    eventSubscription_subscriptionCreationTime,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** FleetAdvisorLsaAnalysisResponse
    fleetAdvisorLsaAnalysisResponse_lsaAnalysisId,
    fleetAdvisorLsaAnalysisResponse_status,

    -- ** FleetAdvisorSchemaObjectResponse
    fleetAdvisorSchemaObjectResponse_codeLineCount,
    fleetAdvisorSchemaObjectResponse_codeSize,
    fleetAdvisorSchemaObjectResponse_numberOfObjects,
    fleetAdvisorSchemaObjectResponse_objectType,
    fleetAdvisorSchemaObjectResponse_schemaId,

    -- ** GcpMySQLSettings
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

    -- ** IBMDb2Settings
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

    -- ** InventoryData
    inventoryData_numberOfDatabases,
    inventoryData_numberOfSchemas,

    -- ** KafkaSettings
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
    kafkaSettings_saslMechanism,
    kafkaSettings_saslPassword,
    kafkaSettings_saslUsername,
    kafkaSettings_securityProtocol,
    kafkaSettings_sslCaCertificateArn,
    kafkaSettings_sslClientCertificateArn,
    kafkaSettings_sslClientKeyArn,
    kafkaSettings_sslClientKeyPassword,
    kafkaSettings_topic,

    -- ** KinesisSettings
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

    -- ** Limitation
    limitation_databaseId,
    limitation_description,
    limitation_engineName,
    limitation_impact,
    limitation_name,
    limitation_type,

    -- ** MicrosoftSQLServerSettings
    microsoftSQLServerSettings_bcpPacketSize,
    microsoftSQLServerSettings_controlTablesFileGroup,
    microsoftSQLServerSettings_databaseName,
    microsoftSQLServerSettings_forceLobLookup,
    microsoftSQLServerSettings_password,
    microsoftSQLServerSettings_port,
    microsoftSQLServerSettings_querySingleAlwaysOnNode,
    microsoftSQLServerSettings_readBackupOnly,
    microsoftSQLServerSettings_safeguardPolicy,
    microsoftSQLServerSettings_secretsManagerAccessRoleArn,
    microsoftSQLServerSettings_secretsManagerSecretId,
    microsoftSQLServerSettings_serverName,
    microsoftSQLServerSettings_tlogAccessMode,
    microsoftSQLServerSettings_trimSpaceInChar,
    microsoftSQLServerSettings_useBcpFullLoad,
    microsoftSQLServerSettings_useThirdPartyBackupDevice,
    microsoftSQLServerSettings_username,

    -- ** MongoDbSettings
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

    -- ** MySQLSettings
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

    -- ** NeptuneSettings
    neptuneSettings_errorRetryDuration,
    neptuneSettings_iamAuthEnabled,
    neptuneSettings_maxFileSize,
    neptuneSettings_maxRetryCount,
    neptuneSettings_serviceAccessRoleArn,
    neptuneSettings_s3BucketName,
    neptuneSettings_s3BucketFolder,

    -- ** OracleSettings
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
    oracleSettings_convertTimestampWithZoneToUTC,
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

    -- ** OrderableReplicationInstance
    orderableReplicationInstance_availabilityZones,
    orderableReplicationInstance_defaultAllocatedStorage,
    orderableReplicationInstance_engineVersion,
    orderableReplicationInstance_includedAllocatedStorage,
    orderableReplicationInstance_maxAllocatedStorage,
    orderableReplicationInstance_minAllocatedStorage,
    orderableReplicationInstance_releaseStatus,
    orderableReplicationInstance_replicationInstanceClass,
    orderableReplicationInstance_storageType,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_optInStatus,

    -- ** PostgreSQLSettings
    postgreSQLSettings_afterConnectScript,
    postgreSQLSettings_captureDdls,
    postgreSQLSettings_databaseName,
    postgreSQLSettings_ddlArtifactsSchema,
    postgreSQLSettings_executeTimeout,
    postgreSQLSettings_failTasksOnLobTruncation,
    postgreSQLSettings_heartbeatEnable,
    postgreSQLSettings_heartbeatFrequency,
    postgreSQLSettings_heartbeatSchema,
    postgreSQLSettings_mapBooleanAsBoolean,
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

    -- ** RdsConfiguration
    rdsConfiguration_deploymentOption,
    rdsConfiguration_engineEdition,
    rdsConfiguration_instanceMemory,
    rdsConfiguration_instanceType,
    rdsConfiguration_instanceVcpu,
    rdsConfiguration_storageIops,
    rdsConfiguration_storageSize,
    rdsConfiguration_storageType,

    -- ** RdsRecommendation
    rdsRecommendation_requirementsToTarget,
    rdsRecommendation_targetConfiguration,

    -- ** RdsRequirements
    rdsRequirements_deploymentOption,
    rdsRequirements_engineEdition,
    rdsRequirements_instanceMemory,
    rdsRequirements_instanceVcpu,
    rdsRequirements_storageIops,
    rdsRequirements_storageSize,

    -- ** Recommendation
    recommendation_createdDate,
    recommendation_data,
    recommendation_databaseId,
    recommendation_engineName,
    recommendation_preferred,
    recommendation_settings,
    recommendation_status,

    -- ** RecommendationData
    recommendationData_rdsEngine,

    -- ** RecommendationSettings
    recommendationSettings_instanceSizingType,
    recommendationSettings_workloadType,

    -- ** RedisSettings
    redisSettings_authPassword,
    redisSettings_authType,
    redisSettings_authUserName,
    redisSettings_sslCaCertificateArn,
    redisSettings_sslSecurityProtocol,
    redisSettings_serverName,
    redisSettings_port,

    -- ** RedshiftSettings
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
    redshiftSettings_mapBooleanAsBoolean,
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

    -- ** RefreshSchemasStatus
    refreshSchemasStatus_endpointArn,
    refreshSchemasStatus_lastFailureMessage,
    refreshSchemasStatus_lastRefreshDate,
    refreshSchemasStatus_replicationInstanceArn,
    refreshSchemasStatus_status,

    -- ** ReplicationInstance
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

    -- ** ReplicationInstanceTaskLog
    replicationInstanceTaskLog_replicationInstanceTaskLogSize,
    replicationInstanceTaskLog_replicationTaskArn,
    replicationInstanceTaskLog_replicationTaskName,

    -- ** ReplicationPendingModifiedValues
    replicationPendingModifiedValues_allocatedStorage,
    replicationPendingModifiedValues_engineVersion,
    replicationPendingModifiedValues_multiAZ,
    replicationPendingModifiedValues_networkType,
    replicationPendingModifiedValues_replicationInstanceClass,

    -- ** ReplicationSubnetGroup
    replicationSubnetGroup_replicationSubnetGroupDescription,
    replicationSubnetGroup_replicationSubnetGroupIdentifier,
    replicationSubnetGroup_subnetGroupStatus,
    replicationSubnetGroup_subnets,
    replicationSubnetGroup_supportedNetworkTypes,
    replicationSubnetGroup_vpcId,

    -- ** ReplicationTask
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

    -- ** ReplicationTaskAssessmentResult
    replicationTaskAssessmentResult_assessmentResults,
    replicationTaskAssessmentResult_assessmentResultsFile,
    replicationTaskAssessmentResult_assessmentStatus,
    replicationTaskAssessmentResult_replicationTaskArn,
    replicationTaskAssessmentResult_replicationTaskIdentifier,
    replicationTaskAssessmentResult_replicationTaskLastAssessmentDate,
    replicationTaskAssessmentResult_s3ObjectUrl,

    -- ** ReplicationTaskAssessmentRun
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

    -- ** ReplicationTaskAssessmentRunProgress
    replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount,
    replicationTaskAssessmentRunProgress_individualAssessmentCount,

    -- ** ReplicationTaskIndividualAssessment
    replicationTaskIndividualAssessment_individualAssessmentName,
    replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn,
    replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate,
    replicationTaskIndividualAssessment_status,

    -- ** ReplicationTaskStats
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

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- ** S3Settings
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
    s3Settings_glueCatalogGeneration,
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

    -- ** SchemaResponse
    schemaResponse_codeLineCount,
    schemaResponse_codeSize,
    schemaResponse_complexity,
    schemaResponse_databaseInstance,
    schemaResponse_originalSchema,
    schemaResponse_schemaId,
    schemaResponse_schemaName,
    schemaResponse_server,
    schemaResponse_similarity,

    -- ** SchemaShortInfoResponse
    schemaShortInfoResponse_databaseId,
    schemaShortInfoResponse_databaseIpAddress,
    schemaShortInfoResponse_databaseName,
    schemaShortInfoResponse_schemaId,
    schemaShortInfoResponse_schemaName,

    -- ** ServerShortInfoResponse
    serverShortInfoResponse_ipAddress,
    serverShortInfoResponse_serverId,
    serverShortInfoResponse_serverName,

    -- ** StartRecommendationsRequestEntry
    startRecommendationsRequestEntry_databaseId,
    startRecommendationsRequestEntry_settings,

    -- ** Subnet
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,
    subnet_subnetStatus,

    -- ** SupportedEndpointType
    supportedEndpointType_endpointType,
    supportedEndpointType_engineDisplayName,
    supportedEndpointType_engineName,
    supportedEndpointType_replicationInstanceEngineMinimumVersion,
    supportedEndpointType_supportsCDC,

    -- ** SybaseSettings
    sybaseSettings_databaseName,
    sybaseSettings_password,
    sybaseSettings_port,
    sybaseSettings_secretsManagerAccessRoleArn,
    sybaseSettings_secretsManagerSecretId,
    sybaseSettings_serverName,
    sybaseSettings_username,

    -- ** TableStatistics
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
import Amazonka.DMS.BatchStartRecommendations
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
import Amazonka.DMS.DescribeRecommendationLimitations
import Amazonka.DMS.DescribeRecommendations
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
import Amazonka.DMS.StartRecommendations
import Amazonka.DMS.StartReplicationTask
import Amazonka.DMS.StartReplicationTaskAssessment
import Amazonka.DMS.StartReplicationTaskAssessmentRun
import Amazonka.DMS.StopReplicationTask
import Amazonka.DMS.TestConnection
import Amazonka.DMS.Types.AccountQuota
import Amazonka.DMS.Types.AvailabilityZone
import Amazonka.DMS.Types.BatchStartRecommendationsErrorEntry
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
import Amazonka.DMS.Types.Limitation
import Amazonka.DMS.Types.MicrosoftSQLServerSettings
import Amazonka.DMS.Types.MongoDbSettings
import Amazonka.DMS.Types.MySQLSettings
import Amazonka.DMS.Types.NeptuneSettings
import Amazonka.DMS.Types.OracleSettings
import Amazonka.DMS.Types.OrderableReplicationInstance
import Amazonka.DMS.Types.PendingMaintenanceAction
import Amazonka.DMS.Types.PostgreSQLSettings
import Amazonka.DMS.Types.RdsConfiguration
import Amazonka.DMS.Types.RdsRecommendation
import Amazonka.DMS.Types.RdsRequirements
import Amazonka.DMS.Types.Recommendation
import Amazonka.DMS.Types.RecommendationData
import Amazonka.DMS.Types.RecommendationSettings
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
import Amazonka.DMS.Types.StartRecommendationsRequestEntry
import Amazonka.DMS.Types.Subnet
import Amazonka.DMS.Types.SupportedEndpointType
import Amazonka.DMS.Types.SybaseSettings
import Amazonka.DMS.Types.TableStatistics
import Amazonka.DMS.Types.TableToReload
import Amazonka.DMS.Types.Tag
import Amazonka.DMS.Types.VpcSecurityGroupMembership
import Amazonka.DMS.UpdateSubscriptionsToEventBridge
