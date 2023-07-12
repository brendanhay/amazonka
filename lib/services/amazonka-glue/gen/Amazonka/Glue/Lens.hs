{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Lens
  ( -- * Operations

    -- ** BatchCreatePartition
    batchCreatePartition_catalogId,
    batchCreatePartition_databaseName,
    batchCreatePartition_tableName,
    batchCreatePartition_partitionInputList,
    batchCreatePartitionResponse_errors,
    batchCreatePartitionResponse_httpStatus,

    -- ** BatchDeleteConnection
    batchDeleteConnection_catalogId,
    batchDeleteConnection_connectionNameList,
    batchDeleteConnectionResponse_errors,
    batchDeleteConnectionResponse_succeeded,
    batchDeleteConnectionResponse_httpStatus,

    -- ** BatchDeletePartition
    batchDeletePartition_catalogId,
    batchDeletePartition_databaseName,
    batchDeletePartition_tableName,
    batchDeletePartition_partitionsToDelete,
    batchDeletePartitionResponse_errors,
    batchDeletePartitionResponse_httpStatus,

    -- ** BatchDeleteTable
    batchDeleteTable_catalogId,
    batchDeleteTable_transactionId,
    batchDeleteTable_databaseName,
    batchDeleteTable_tablesToDelete,
    batchDeleteTableResponse_errors,
    batchDeleteTableResponse_httpStatus,

    -- ** BatchDeleteTableVersion
    batchDeleteTableVersion_catalogId,
    batchDeleteTableVersion_databaseName,
    batchDeleteTableVersion_tableName,
    batchDeleteTableVersion_versionIds,
    batchDeleteTableVersionResponse_errors,
    batchDeleteTableVersionResponse_httpStatus,

    -- ** BatchGetBlueprints
    batchGetBlueprints_includeBlueprint,
    batchGetBlueprints_includeParameterSpec,
    batchGetBlueprints_names,
    batchGetBlueprintsResponse_blueprints,
    batchGetBlueprintsResponse_missingBlueprints,
    batchGetBlueprintsResponse_httpStatus,

    -- ** BatchGetCrawlers
    batchGetCrawlers_crawlerNames,
    batchGetCrawlersResponse_crawlers,
    batchGetCrawlersResponse_crawlersNotFound,
    batchGetCrawlersResponse_httpStatus,

    -- ** BatchGetCustomEntityTypes
    batchGetCustomEntityTypes_names,
    batchGetCustomEntityTypesResponse_customEntityTypes,
    batchGetCustomEntityTypesResponse_customEntityTypesNotFound,
    batchGetCustomEntityTypesResponse_httpStatus,

    -- ** BatchGetDataQualityResult
    batchGetDataQualityResult_resultIds,
    batchGetDataQualityResultResponse_resultsNotFound,
    batchGetDataQualityResultResponse_httpStatus,
    batchGetDataQualityResultResponse_results,

    -- ** BatchGetDevEndpoints
    batchGetDevEndpoints_devEndpointNames,
    batchGetDevEndpointsResponse_devEndpoints,
    batchGetDevEndpointsResponse_devEndpointsNotFound,
    batchGetDevEndpointsResponse_httpStatus,

    -- ** BatchGetJobs
    batchGetJobs_jobNames,
    batchGetJobsResponse_jobs,
    batchGetJobsResponse_jobsNotFound,
    batchGetJobsResponse_httpStatus,

    -- ** BatchGetPartition
    batchGetPartition_catalogId,
    batchGetPartition_databaseName,
    batchGetPartition_tableName,
    batchGetPartition_partitionsToGet,
    batchGetPartitionResponse_partitions,
    batchGetPartitionResponse_unprocessedKeys,
    batchGetPartitionResponse_httpStatus,

    -- ** BatchGetTriggers
    batchGetTriggers_triggerNames,
    batchGetTriggersResponse_triggers,
    batchGetTriggersResponse_triggersNotFound,
    batchGetTriggersResponse_httpStatus,

    -- ** BatchGetWorkflows
    batchGetWorkflows_includeGraph,
    batchGetWorkflows_names,
    batchGetWorkflowsResponse_missingWorkflows,
    batchGetWorkflowsResponse_workflows,
    batchGetWorkflowsResponse_httpStatus,

    -- ** BatchStopJobRun
    batchStopJobRun_jobName,
    batchStopJobRun_jobRunIds,
    batchStopJobRunResponse_errors,
    batchStopJobRunResponse_successfulSubmissions,
    batchStopJobRunResponse_httpStatus,

    -- ** BatchUpdatePartition
    batchUpdatePartition_catalogId,
    batchUpdatePartition_databaseName,
    batchUpdatePartition_tableName,
    batchUpdatePartition_entries,
    batchUpdatePartitionResponse_errors,
    batchUpdatePartitionResponse_httpStatus,

    -- ** CancelDataQualityRuleRecommendationRun
    cancelDataQualityRuleRecommendationRun_runId,
    cancelDataQualityRuleRecommendationRunResponse_httpStatus,

    -- ** CancelDataQualityRulesetEvaluationRun
    cancelDataQualityRulesetEvaluationRun_runId,
    cancelDataQualityRulesetEvaluationRunResponse_httpStatus,

    -- ** CancelMLTaskRun
    cancelMLTaskRun_transformId,
    cancelMLTaskRun_taskRunId,
    cancelMLTaskRunResponse_status,
    cancelMLTaskRunResponse_taskRunId,
    cancelMLTaskRunResponse_transformId,
    cancelMLTaskRunResponse_httpStatus,

    -- ** CancelStatement
    cancelStatement_requestOrigin,
    cancelStatement_sessionId,
    cancelStatement_id,
    cancelStatementResponse_httpStatus,

    -- ** CheckSchemaVersionValidity
    checkSchemaVersionValidity_dataFormat,
    checkSchemaVersionValidity_schemaDefinition,
    checkSchemaVersionValidityResponse_error,
    checkSchemaVersionValidityResponse_valid,
    checkSchemaVersionValidityResponse_httpStatus,

    -- ** CreateBlueprint
    createBlueprint_description,
    createBlueprint_tags,
    createBlueprint_name,
    createBlueprint_blueprintLocation,
    createBlueprintResponse_name,
    createBlueprintResponse_httpStatus,

    -- ** CreateClassifier
    createClassifier_csvClassifier,
    createClassifier_grokClassifier,
    createClassifier_jsonClassifier,
    createClassifier_xMLClassifier,
    createClassifierResponse_httpStatus,

    -- ** CreateConnection
    createConnection_catalogId,
    createConnection_tags,
    createConnection_connectionInput,
    createConnectionResponse_httpStatus,

    -- ** CreateCrawler
    createCrawler_classifiers,
    createCrawler_configuration,
    createCrawler_crawlerSecurityConfiguration,
    createCrawler_databaseName,
    createCrawler_description,
    createCrawler_lakeFormationConfiguration,
    createCrawler_lineageConfiguration,
    createCrawler_recrawlPolicy,
    createCrawler_schedule,
    createCrawler_schemaChangePolicy,
    createCrawler_tablePrefix,
    createCrawler_tags,
    createCrawler_name,
    createCrawler_role,
    createCrawler_targets,
    createCrawlerResponse_httpStatus,

    -- ** CreateCustomEntityType
    createCustomEntityType_contextWords,
    createCustomEntityType_name,
    createCustomEntityType_regexString,
    createCustomEntityTypeResponse_name,
    createCustomEntityTypeResponse_httpStatus,

    -- ** CreateDataQualityRuleset
    createDataQualityRuleset_clientToken,
    createDataQualityRuleset_description,
    createDataQualityRuleset_tags,
    createDataQualityRuleset_targetTable,
    createDataQualityRuleset_name,
    createDataQualityRuleset_ruleset,
    createDataQualityRulesetResponse_name,
    createDataQualityRulesetResponse_httpStatus,

    -- ** CreateDatabase
    createDatabase_catalogId,
    createDatabase_tags,
    createDatabase_databaseInput,
    createDatabaseResponse_httpStatus,

    -- ** CreateDevEndpoint
    createDevEndpoint_arguments,
    createDevEndpoint_extraJarsS3Path,
    createDevEndpoint_extraPythonLibsS3Path,
    createDevEndpoint_glueVersion,
    createDevEndpoint_numberOfNodes,
    createDevEndpoint_numberOfWorkers,
    createDevEndpoint_publicKey,
    createDevEndpoint_publicKeys,
    createDevEndpoint_securityConfiguration,
    createDevEndpoint_securityGroupIds,
    createDevEndpoint_subnetId,
    createDevEndpoint_tags,
    createDevEndpoint_workerType,
    createDevEndpoint_endpointName,
    createDevEndpoint_roleArn,
    createDevEndpointResponse_arguments,
    createDevEndpointResponse_availabilityZone,
    createDevEndpointResponse_createdTimestamp,
    createDevEndpointResponse_endpointName,
    createDevEndpointResponse_extraJarsS3Path,
    createDevEndpointResponse_extraPythonLibsS3Path,
    createDevEndpointResponse_failureReason,
    createDevEndpointResponse_glueVersion,
    createDevEndpointResponse_numberOfNodes,
    createDevEndpointResponse_numberOfWorkers,
    createDevEndpointResponse_roleArn,
    createDevEndpointResponse_securityConfiguration,
    createDevEndpointResponse_securityGroupIds,
    createDevEndpointResponse_status,
    createDevEndpointResponse_subnetId,
    createDevEndpointResponse_vpcId,
    createDevEndpointResponse_workerType,
    createDevEndpointResponse_yarnEndpointAddress,
    createDevEndpointResponse_zeppelinRemoteSparkInterpreterPort,
    createDevEndpointResponse_httpStatus,

    -- ** CreateJob
    createJob_allocatedCapacity,
    createJob_codeGenConfigurationNodes,
    createJob_connections,
    createJob_defaultArguments,
    createJob_description,
    createJob_executionClass,
    createJob_executionProperty,
    createJob_glueVersion,
    createJob_logUri,
    createJob_maxCapacity,
    createJob_maxRetries,
    createJob_nonOverridableArguments,
    createJob_notificationProperty,
    createJob_numberOfWorkers,
    createJob_securityConfiguration,
    createJob_sourceControlDetails,
    createJob_tags,
    createJob_timeout,
    createJob_workerType,
    createJob_name,
    createJob_role,
    createJob_command,
    createJobResponse_name,
    createJobResponse_httpStatus,

    -- ** CreateMLTransform
    createMLTransform_description,
    createMLTransform_glueVersion,
    createMLTransform_maxCapacity,
    createMLTransform_maxRetries,
    createMLTransform_numberOfWorkers,
    createMLTransform_tags,
    createMLTransform_timeout,
    createMLTransform_transformEncryption,
    createMLTransform_workerType,
    createMLTransform_name,
    createMLTransform_inputRecordTables,
    createMLTransform_parameters,
    createMLTransform_role,
    createMLTransformResponse_transformId,
    createMLTransformResponse_httpStatus,

    -- ** CreatePartition
    createPartition_catalogId,
    createPartition_databaseName,
    createPartition_tableName,
    createPartition_partitionInput,
    createPartitionResponse_httpStatus,

    -- ** CreatePartitionIndex
    createPartitionIndex_catalogId,
    createPartitionIndex_databaseName,
    createPartitionIndex_tableName,
    createPartitionIndex_partitionIndex,
    createPartitionIndexResponse_httpStatus,

    -- ** CreateRegistry
    createRegistry_description,
    createRegistry_tags,
    createRegistry_registryName,
    createRegistryResponse_description,
    createRegistryResponse_registryArn,
    createRegistryResponse_registryName,
    createRegistryResponse_tags,
    createRegistryResponse_httpStatus,

    -- ** CreateSchema
    createSchema_compatibility,
    createSchema_description,
    createSchema_registryId,
    createSchema_schemaDefinition,
    createSchema_tags,
    createSchema_schemaName,
    createSchema_dataFormat,
    createSchemaResponse_compatibility,
    createSchemaResponse_dataFormat,
    createSchemaResponse_description,
    createSchemaResponse_latestSchemaVersion,
    createSchemaResponse_nextSchemaVersion,
    createSchemaResponse_registryArn,
    createSchemaResponse_registryName,
    createSchemaResponse_schemaArn,
    createSchemaResponse_schemaCheckpoint,
    createSchemaResponse_schemaName,
    createSchemaResponse_schemaStatus,
    createSchemaResponse_schemaVersionId,
    createSchemaResponse_schemaVersionStatus,
    createSchemaResponse_tags,
    createSchemaResponse_httpStatus,

    -- ** CreateScript
    createScript_dagEdges,
    createScript_dagNodes,
    createScript_language,
    createScriptResponse_pythonScript,
    createScriptResponse_scalaCode,
    createScriptResponse_httpStatus,

    -- ** CreateSecurityConfiguration
    createSecurityConfiguration_name,
    createSecurityConfiguration_encryptionConfiguration,
    createSecurityConfigurationResponse_createdTimestamp,
    createSecurityConfigurationResponse_name,
    createSecurityConfigurationResponse_httpStatus,

    -- ** CreateSession
    createSession_connections,
    createSession_defaultArguments,
    createSession_description,
    createSession_glueVersion,
    createSession_idleTimeout,
    createSession_maxCapacity,
    createSession_numberOfWorkers,
    createSession_requestOrigin,
    createSession_securityConfiguration,
    createSession_tags,
    createSession_timeout,
    createSession_workerType,
    createSession_id,
    createSession_role,
    createSession_command,
    createSessionResponse_session,
    createSessionResponse_httpStatus,

    -- ** CreateTable
    createTable_catalogId,
    createTable_partitionIndexes,
    createTable_transactionId,
    createTable_databaseName,
    createTable_tableInput,
    createTableResponse_httpStatus,

    -- ** CreateTrigger
    createTrigger_description,
    createTrigger_eventBatchingCondition,
    createTrigger_predicate,
    createTrigger_schedule,
    createTrigger_startOnCreation,
    createTrigger_tags,
    createTrigger_workflowName,
    createTrigger_name,
    createTrigger_type,
    createTrigger_actions,
    createTriggerResponse_name,
    createTriggerResponse_httpStatus,

    -- ** CreateUserDefinedFunction
    createUserDefinedFunction_catalogId,
    createUserDefinedFunction_databaseName,
    createUserDefinedFunction_functionInput,
    createUserDefinedFunctionResponse_httpStatus,

    -- ** CreateWorkflow
    createWorkflow_defaultRunProperties,
    createWorkflow_description,
    createWorkflow_maxConcurrentRuns,
    createWorkflow_tags,
    createWorkflow_name,
    createWorkflowResponse_name,
    createWorkflowResponse_httpStatus,

    -- ** DeleteBlueprint
    deleteBlueprint_name,
    deleteBlueprintResponse_name,
    deleteBlueprintResponse_httpStatus,

    -- ** DeleteClassifier
    deleteClassifier_name,
    deleteClassifierResponse_httpStatus,

    -- ** DeleteColumnStatisticsForPartition
    deleteColumnStatisticsForPartition_catalogId,
    deleteColumnStatisticsForPartition_databaseName,
    deleteColumnStatisticsForPartition_tableName,
    deleteColumnStatisticsForPartition_partitionValues,
    deleteColumnStatisticsForPartition_columnName,
    deleteColumnStatisticsForPartitionResponse_httpStatus,

    -- ** DeleteColumnStatisticsForTable
    deleteColumnStatisticsForTable_catalogId,
    deleteColumnStatisticsForTable_databaseName,
    deleteColumnStatisticsForTable_tableName,
    deleteColumnStatisticsForTable_columnName,
    deleteColumnStatisticsForTableResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_catalogId,
    deleteConnection_connectionName,
    deleteConnectionResponse_httpStatus,

    -- ** DeleteCrawler
    deleteCrawler_name,
    deleteCrawlerResponse_httpStatus,

    -- ** DeleteCustomEntityType
    deleteCustomEntityType_name,
    deleteCustomEntityTypeResponse_name,
    deleteCustomEntityTypeResponse_httpStatus,

    -- ** DeleteDataQualityRuleset
    deleteDataQualityRuleset_name,
    deleteDataQualityRulesetResponse_httpStatus,

    -- ** DeleteDatabase
    deleteDatabase_catalogId,
    deleteDatabase_name,
    deleteDatabaseResponse_httpStatus,

    -- ** DeleteDevEndpoint
    deleteDevEndpoint_endpointName,
    deleteDevEndpointResponse_httpStatus,

    -- ** DeleteJob
    deleteJob_jobName,
    deleteJobResponse_jobName,
    deleteJobResponse_httpStatus,

    -- ** DeleteMLTransform
    deleteMLTransform_transformId,
    deleteMLTransformResponse_transformId,
    deleteMLTransformResponse_httpStatus,

    -- ** DeletePartition
    deletePartition_catalogId,
    deletePartition_databaseName,
    deletePartition_tableName,
    deletePartition_partitionValues,
    deletePartitionResponse_httpStatus,

    -- ** DeletePartitionIndex
    deletePartitionIndex_catalogId,
    deletePartitionIndex_databaseName,
    deletePartitionIndex_tableName,
    deletePartitionIndex_indexName,
    deletePartitionIndexResponse_httpStatus,

    -- ** DeleteRegistry
    deleteRegistry_registryId,
    deleteRegistryResponse_registryArn,
    deleteRegistryResponse_registryName,
    deleteRegistryResponse_status,
    deleteRegistryResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_policyHashCondition,
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteSchema
    deleteSchema_schemaId,
    deleteSchemaResponse_schemaArn,
    deleteSchemaResponse_schemaName,
    deleteSchemaResponse_status,
    deleteSchemaResponse_httpStatus,

    -- ** DeleteSchemaVersions
    deleteSchemaVersions_schemaId,
    deleteSchemaVersions_versions,
    deleteSchemaVersionsResponse_schemaVersionErrors,
    deleteSchemaVersionsResponse_httpStatus,

    -- ** DeleteSecurityConfiguration
    deleteSecurityConfiguration_name,
    deleteSecurityConfigurationResponse_httpStatus,

    -- ** DeleteSession
    deleteSession_requestOrigin,
    deleteSession_id,
    deleteSessionResponse_id,
    deleteSessionResponse_httpStatus,

    -- ** DeleteTable
    deleteTable_catalogId,
    deleteTable_transactionId,
    deleteTable_databaseName,
    deleteTable_name,
    deleteTableResponse_httpStatus,

    -- ** DeleteTableVersion
    deleteTableVersion_catalogId,
    deleteTableVersion_databaseName,
    deleteTableVersion_tableName,
    deleteTableVersion_versionId,
    deleteTableVersionResponse_httpStatus,

    -- ** DeleteTrigger
    deleteTrigger_name,
    deleteTriggerResponse_name,
    deleteTriggerResponse_httpStatus,

    -- ** DeleteUserDefinedFunction
    deleteUserDefinedFunction_catalogId,
    deleteUserDefinedFunction_databaseName,
    deleteUserDefinedFunction_functionName,
    deleteUserDefinedFunctionResponse_httpStatus,

    -- ** DeleteWorkflow
    deleteWorkflow_name,
    deleteWorkflowResponse_name,
    deleteWorkflowResponse_httpStatus,

    -- ** GetBlueprint
    getBlueprint_includeBlueprint,
    getBlueprint_includeParameterSpec,
    getBlueprint_name,
    getBlueprintResponse_blueprint,
    getBlueprintResponse_httpStatus,

    -- ** GetBlueprintRun
    getBlueprintRun_blueprintName,
    getBlueprintRun_runId,
    getBlueprintRunResponse_blueprintRun,
    getBlueprintRunResponse_httpStatus,

    -- ** GetBlueprintRuns
    getBlueprintRuns_maxResults,
    getBlueprintRuns_nextToken,
    getBlueprintRuns_blueprintName,
    getBlueprintRunsResponse_blueprintRuns,
    getBlueprintRunsResponse_nextToken,
    getBlueprintRunsResponse_httpStatus,

    -- ** GetCatalogImportStatus
    getCatalogImportStatus_catalogId,
    getCatalogImportStatusResponse_importStatus,
    getCatalogImportStatusResponse_httpStatus,

    -- ** GetClassifier
    getClassifier_name,
    getClassifierResponse_classifier,
    getClassifierResponse_httpStatus,

    -- ** GetClassifiers
    getClassifiers_maxResults,
    getClassifiers_nextToken,
    getClassifiersResponse_classifiers,
    getClassifiersResponse_nextToken,
    getClassifiersResponse_httpStatus,

    -- ** GetColumnStatisticsForPartition
    getColumnStatisticsForPartition_catalogId,
    getColumnStatisticsForPartition_databaseName,
    getColumnStatisticsForPartition_tableName,
    getColumnStatisticsForPartition_partitionValues,
    getColumnStatisticsForPartition_columnNames,
    getColumnStatisticsForPartitionResponse_columnStatisticsList,
    getColumnStatisticsForPartitionResponse_errors,
    getColumnStatisticsForPartitionResponse_httpStatus,

    -- ** GetColumnStatisticsForTable
    getColumnStatisticsForTable_catalogId,
    getColumnStatisticsForTable_databaseName,
    getColumnStatisticsForTable_tableName,
    getColumnStatisticsForTable_columnNames,
    getColumnStatisticsForTableResponse_columnStatisticsList,
    getColumnStatisticsForTableResponse_errors,
    getColumnStatisticsForTableResponse_httpStatus,

    -- ** GetConnection
    getConnection_catalogId,
    getConnection_hidePassword,
    getConnection_name,
    getConnectionResponse_connection,
    getConnectionResponse_httpStatus,

    -- ** GetConnections
    getConnections_catalogId,
    getConnections_filter,
    getConnections_hidePassword,
    getConnections_maxResults,
    getConnections_nextToken,
    getConnectionsResponse_connectionList,
    getConnectionsResponse_nextToken,
    getConnectionsResponse_httpStatus,

    -- ** GetCrawler
    getCrawler_name,
    getCrawlerResponse_crawler,
    getCrawlerResponse_httpStatus,

    -- ** GetCrawlerMetrics
    getCrawlerMetrics_crawlerNameList,
    getCrawlerMetrics_maxResults,
    getCrawlerMetrics_nextToken,
    getCrawlerMetricsResponse_crawlerMetricsList,
    getCrawlerMetricsResponse_nextToken,
    getCrawlerMetricsResponse_httpStatus,

    -- ** GetCrawlers
    getCrawlers_maxResults,
    getCrawlers_nextToken,
    getCrawlersResponse_crawlers,
    getCrawlersResponse_nextToken,
    getCrawlersResponse_httpStatus,

    -- ** GetCustomEntityType
    getCustomEntityType_name,
    getCustomEntityTypeResponse_contextWords,
    getCustomEntityTypeResponse_name,
    getCustomEntityTypeResponse_regexString,
    getCustomEntityTypeResponse_httpStatus,

    -- ** GetDataCatalogEncryptionSettings
    getDataCatalogEncryptionSettings_catalogId,
    getDataCatalogEncryptionSettingsResponse_dataCatalogEncryptionSettings,
    getDataCatalogEncryptionSettingsResponse_httpStatus,

    -- ** GetDataQualityResult
    getDataQualityResult_resultId,
    getDataQualityResultResponse_completedOn,
    getDataQualityResultResponse_dataSource,
    getDataQualityResultResponse_evaluationContext,
    getDataQualityResultResponse_jobName,
    getDataQualityResultResponse_jobRunId,
    getDataQualityResultResponse_resultId,
    getDataQualityResultResponse_ruleResults,
    getDataQualityResultResponse_rulesetEvaluationRunId,
    getDataQualityResultResponse_rulesetName,
    getDataQualityResultResponse_score,
    getDataQualityResultResponse_startedOn,
    getDataQualityResultResponse_httpStatus,

    -- ** GetDataQualityRuleRecommendationRun
    getDataQualityRuleRecommendationRun_runId,
    getDataQualityRuleRecommendationRunResponse_completedOn,
    getDataQualityRuleRecommendationRunResponse_createdRulesetName,
    getDataQualityRuleRecommendationRunResponse_dataSource,
    getDataQualityRuleRecommendationRunResponse_errorString,
    getDataQualityRuleRecommendationRunResponse_executionTime,
    getDataQualityRuleRecommendationRunResponse_lastModifiedOn,
    getDataQualityRuleRecommendationRunResponse_numberOfWorkers,
    getDataQualityRuleRecommendationRunResponse_recommendedRuleset,
    getDataQualityRuleRecommendationRunResponse_role,
    getDataQualityRuleRecommendationRunResponse_runId,
    getDataQualityRuleRecommendationRunResponse_startedOn,
    getDataQualityRuleRecommendationRunResponse_status,
    getDataQualityRuleRecommendationRunResponse_timeout,
    getDataQualityRuleRecommendationRunResponse_httpStatus,

    -- ** GetDataQualityRuleset
    getDataQualityRuleset_name,
    getDataQualityRulesetResponse_createdOn,
    getDataQualityRulesetResponse_description,
    getDataQualityRulesetResponse_lastModifiedOn,
    getDataQualityRulesetResponse_name,
    getDataQualityRulesetResponse_recommendationRunId,
    getDataQualityRulesetResponse_ruleset,
    getDataQualityRulesetResponse_targetTable,
    getDataQualityRulesetResponse_httpStatus,

    -- ** GetDataQualityRulesetEvaluationRun
    getDataQualityRulesetEvaluationRun_runId,
    getDataQualityRulesetEvaluationRunResponse_additionalRunOptions,
    getDataQualityRulesetEvaluationRunResponse_completedOn,
    getDataQualityRulesetEvaluationRunResponse_dataSource,
    getDataQualityRulesetEvaluationRunResponse_errorString,
    getDataQualityRulesetEvaluationRunResponse_executionTime,
    getDataQualityRulesetEvaluationRunResponse_lastModifiedOn,
    getDataQualityRulesetEvaluationRunResponse_numberOfWorkers,
    getDataQualityRulesetEvaluationRunResponse_resultIds,
    getDataQualityRulesetEvaluationRunResponse_role,
    getDataQualityRulesetEvaluationRunResponse_rulesetNames,
    getDataQualityRulesetEvaluationRunResponse_runId,
    getDataQualityRulesetEvaluationRunResponse_startedOn,
    getDataQualityRulesetEvaluationRunResponse_status,
    getDataQualityRulesetEvaluationRunResponse_timeout,
    getDataQualityRulesetEvaluationRunResponse_httpStatus,

    -- ** GetDatabase
    getDatabase_catalogId,
    getDatabase_name,
    getDatabaseResponse_database,
    getDatabaseResponse_httpStatus,

    -- ** GetDatabases
    getDatabases_catalogId,
    getDatabases_maxResults,
    getDatabases_nextToken,
    getDatabases_resourceShareType,
    getDatabasesResponse_nextToken,
    getDatabasesResponse_httpStatus,
    getDatabasesResponse_databaseList,

    -- ** GetDataflowGraph
    getDataflowGraph_pythonScript,
    getDataflowGraphResponse_dagEdges,
    getDataflowGraphResponse_dagNodes,
    getDataflowGraphResponse_httpStatus,

    -- ** GetDevEndpoint
    getDevEndpoint_endpointName,
    getDevEndpointResponse_devEndpoint,
    getDevEndpointResponse_httpStatus,

    -- ** GetDevEndpoints
    getDevEndpoints_maxResults,
    getDevEndpoints_nextToken,
    getDevEndpointsResponse_devEndpoints,
    getDevEndpointsResponse_nextToken,
    getDevEndpointsResponse_httpStatus,

    -- ** GetJob
    getJob_jobName,
    getJobResponse_job,
    getJobResponse_httpStatus,

    -- ** GetJobBookmark
    getJobBookmark_runId,
    getJobBookmark_jobName,
    getJobBookmarkResponse_jobBookmarkEntry,
    getJobBookmarkResponse_httpStatus,

    -- ** GetJobRun
    getJobRun_predecessorsIncluded,
    getJobRun_jobName,
    getJobRun_runId,
    getJobRunResponse_jobRun,
    getJobRunResponse_httpStatus,

    -- ** GetJobRuns
    getJobRuns_maxResults,
    getJobRuns_nextToken,
    getJobRuns_jobName,
    getJobRunsResponse_jobRuns,
    getJobRunsResponse_nextToken,
    getJobRunsResponse_httpStatus,

    -- ** GetJobs
    getJobs_maxResults,
    getJobs_nextToken,
    getJobsResponse_jobs,
    getJobsResponse_nextToken,
    getJobsResponse_httpStatus,

    -- ** GetMLTaskRun
    getMLTaskRun_transformId,
    getMLTaskRun_taskRunId,
    getMLTaskRunResponse_completedOn,
    getMLTaskRunResponse_errorString,
    getMLTaskRunResponse_executionTime,
    getMLTaskRunResponse_lastModifiedOn,
    getMLTaskRunResponse_logGroupName,
    getMLTaskRunResponse_properties,
    getMLTaskRunResponse_startedOn,
    getMLTaskRunResponse_status,
    getMLTaskRunResponse_taskRunId,
    getMLTaskRunResponse_transformId,
    getMLTaskRunResponse_httpStatus,

    -- ** GetMLTaskRuns
    getMLTaskRuns_filter,
    getMLTaskRuns_maxResults,
    getMLTaskRuns_nextToken,
    getMLTaskRuns_sort,
    getMLTaskRuns_transformId,
    getMLTaskRunsResponse_nextToken,
    getMLTaskRunsResponse_taskRuns,
    getMLTaskRunsResponse_httpStatus,

    -- ** GetMLTransform
    getMLTransform_transformId,
    getMLTransformResponse_createdOn,
    getMLTransformResponse_description,
    getMLTransformResponse_evaluationMetrics,
    getMLTransformResponse_glueVersion,
    getMLTransformResponse_inputRecordTables,
    getMLTransformResponse_labelCount,
    getMLTransformResponse_lastModifiedOn,
    getMLTransformResponse_maxCapacity,
    getMLTransformResponse_maxRetries,
    getMLTransformResponse_name,
    getMLTransformResponse_numberOfWorkers,
    getMLTransformResponse_parameters,
    getMLTransformResponse_role,
    getMLTransformResponse_schema,
    getMLTransformResponse_status,
    getMLTransformResponse_timeout,
    getMLTransformResponse_transformEncryption,
    getMLTransformResponse_transformId,
    getMLTransformResponse_workerType,
    getMLTransformResponse_httpStatus,

    -- ** GetMLTransforms
    getMLTransforms_filter,
    getMLTransforms_maxResults,
    getMLTransforms_nextToken,
    getMLTransforms_sort,
    getMLTransformsResponse_nextToken,
    getMLTransformsResponse_httpStatus,
    getMLTransformsResponse_transforms,

    -- ** GetMapping
    getMapping_location,
    getMapping_sinks,
    getMapping_source,
    getMappingResponse_httpStatus,
    getMappingResponse_mapping,

    -- ** GetPartition
    getPartition_catalogId,
    getPartition_databaseName,
    getPartition_tableName,
    getPartition_partitionValues,
    getPartitionResponse_partition,
    getPartitionResponse_httpStatus,

    -- ** GetPartitionIndexes
    getPartitionIndexes_catalogId,
    getPartitionIndexes_nextToken,
    getPartitionIndexes_databaseName,
    getPartitionIndexes_tableName,
    getPartitionIndexesResponse_nextToken,
    getPartitionIndexesResponse_partitionIndexDescriptorList,
    getPartitionIndexesResponse_httpStatus,

    -- ** GetPartitions
    getPartitions_catalogId,
    getPartitions_excludeColumnSchema,
    getPartitions_expression,
    getPartitions_maxResults,
    getPartitions_nextToken,
    getPartitions_queryAsOfTime,
    getPartitions_segment,
    getPartitions_transactionId,
    getPartitions_databaseName,
    getPartitions_tableName,
    getPartitionsResponse_nextToken,
    getPartitionsResponse_partitions,
    getPartitionsResponse_httpStatus,

    -- ** GetPlan
    getPlan_additionalPlanOptionsMap,
    getPlan_language,
    getPlan_location,
    getPlan_sinks,
    getPlan_mapping,
    getPlan_source,
    getPlanResponse_pythonScript,
    getPlanResponse_scalaCode,
    getPlanResponse_httpStatus,

    -- ** GetRegistry
    getRegistry_registryId,
    getRegistryResponse_createdTime,
    getRegistryResponse_description,
    getRegistryResponse_registryArn,
    getRegistryResponse_registryName,
    getRegistryResponse_status,
    getRegistryResponse_updatedTime,
    getRegistryResponse_httpStatus,

    -- ** GetResourcePolicies
    getResourcePolicies_maxResults,
    getResourcePolicies_nextToken,
    getResourcePoliciesResponse_getResourcePoliciesResponseList,
    getResourcePoliciesResponse_nextToken,
    getResourcePoliciesResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_resourceArn,
    getResourcePolicyResponse_createTime,
    getResourcePolicyResponse_policyHash,
    getResourcePolicyResponse_policyInJson,
    getResourcePolicyResponse_updateTime,
    getResourcePolicyResponse_httpStatus,

    -- ** GetSchema
    getSchema_schemaId,
    getSchemaResponse_compatibility,
    getSchemaResponse_createdTime,
    getSchemaResponse_dataFormat,
    getSchemaResponse_description,
    getSchemaResponse_latestSchemaVersion,
    getSchemaResponse_nextSchemaVersion,
    getSchemaResponse_registryArn,
    getSchemaResponse_registryName,
    getSchemaResponse_schemaArn,
    getSchemaResponse_schemaCheckpoint,
    getSchemaResponse_schemaName,
    getSchemaResponse_schemaStatus,
    getSchemaResponse_updatedTime,
    getSchemaResponse_httpStatus,

    -- ** GetSchemaByDefinition
    getSchemaByDefinition_schemaId,
    getSchemaByDefinition_schemaDefinition,
    getSchemaByDefinitionResponse_createdTime,
    getSchemaByDefinitionResponse_dataFormat,
    getSchemaByDefinitionResponse_schemaArn,
    getSchemaByDefinitionResponse_schemaVersionId,
    getSchemaByDefinitionResponse_status,
    getSchemaByDefinitionResponse_httpStatus,

    -- ** GetSchemaVersion
    getSchemaVersion_schemaId,
    getSchemaVersion_schemaVersionId,
    getSchemaVersion_schemaVersionNumber,
    getSchemaVersionResponse_createdTime,
    getSchemaVersionResponse_dataFormat,
    getSchemaVersionResponse_schemaArn,
    getSchemaVersionResponse_schemaDefinition,
    getSchemaVersionResponse_schemaVersionId,
    getSchemaVersionResponse_status,
    getSchemaVersionResponse_versionNumber,
    getSchemaVersionResponse_httpStatus,

    -- ** GetSchemaVersionsDiff
    getSchemaVersionsDiff_schemaId,
    getSchemaVersionsDiff_firstSchemaVersionNumber,
    getSchemaVersionsDiff_secondSchemaVersionNumber,
    getSchemaVersionsDiff_schemaDiffType,
    getSchemaVersionsDiffResponse_diff,
    getSchemaVersionsDiffResponse_httpStatus,

    -- ** GetSecurityConfiguration
    getSecurityConfiguration_name,
    getSecurityConfigurationResponse_securityConfiguration,
    getSecurityConfigurationResponse_httpStatus,

    -- ** GetSecurityConfigurations
    getSecurityConfigurations_maxResults,
    getSecurityConfigurations_nextToken,
    getSecurityConfigurationsResponse_nextToken,
    getSecurityConfigurationsResponse_securityConfigurations,
    getSecurityConfigurationsResponse_httpStatus,

    -- ** GetSession
    getSession_requestOrigin,
    getSession_id,
    getSessionResponse_session,
    getSessionResponse_httpStatus,

    -- ** GetStatement
    getStatement_requestOrigin,
    getStatement_sessionId,
    getStatement_id,
    getStatementResponse_statement,
    getStatementResponse_httpStatus,

    -- ** GetTable
    getTable_catalogId,
    getTable_queryAsOfTime,
    getTable_transactionId,
    getTable_databaseName,
    getTable_name,
    getTableResponse_table,
    getTableResponse_httpStatus,

    -- ** GetTableVersion
    getTableVersion_catalogId,
    getTableVersion_versionId,
    getTableVersion_databaseName,
    getTableVersion_tableName,
    getTableVersionResponse_tableVersion,
    getTableVersionResponse_httpStatus,

    -- ** GetTableVersions
    getTableVersions_catalogId,
    getTableVersions_maxResults,
    getTableVersions_nextToken,
    getTableVersions_databaseName,
    getTableVersions_tableName,
    getTableVersionsResponse_nextToken,
    getTableVersionsResponse_tableVersions,
    getTableVersionsResponse_httpStatus,

    -- ** GetTables
    getTables_catalogId,
    getTables_expression,
    getTables_maxResults,
    getTables_nextToken,
    getTables_queryAsOfTime,
    getTables_transactionId,
    getTables_databaseName,
    getTablesResponse_nextToken,
    getTablesResponse_tableList,
    getTablesResponse_httpStatus,

    -- ** GetTags
    getTags_resourceArn,
    getTagsResponse_tags,
    getTagsResponse_httpStatus,

    -- ** GetTrigger
    getTrigger_name,
    getTriggerResponse_trigger,
    getTriggerResponse_httpStatus,

    -- ** GetTriggers
    getTriggers_dependentJobName,
    getTriggers_maxResults,
    getTriggers_nextToken,
    getTriggersResponse_nextToken,
    getTriggersResponse_triggers,
    getTriggersResponse_httpStatus,

    -- ** GetUnfilteredPartitionMetadata
    getUnfilteredPartitionMetadata_auditContext,
    getUnfilteredPartitionMetadata_catalogId,
    getUnfilteredPartitionMetadata_databaseName,
    getUnfilteredPartitionMetadata_tableName,
    getUnfilteredPartitionMetadata_partitionValues,
    getUnfilteredPartitionMetadata_supportedPermissionTypes,
    getUnfilteredPartitionMetadataResponse_authorizedColumns,
    getUnfilteredPartitionMetadataResponse_isRegisteredWithLakeFormation,
    getUnfilteredPartitionMetadataResponse_partition,
    getUnfilteredPartitionMetadataResponse_httpStatus,

    -- ** GetUnfilteredPartitionsMetadata
    getUnfilteredPartitionsMetadata_auditContext,
    getUnfilteredPartitionsMetadata_expression,
    getUnfilteredPartitionsMetadata_maxResults,
    getUnfilteredPartitionsMetadata_nextToken,
    getUnfilteredPartitionsMetadata_segment,
    getUnfilteredPartitionsMetadata_catalogId,
    getUnfilteredPartitionsMetadata_databaseName,
    getUnfilteredPartitionsMetadata_tableName,
    getUnfilteredPartitionsMetadata_supportedPermissionTypes,
    getUnfilteredPartitionsMetadataResponse_nextToken,
    getUnfilteredPartitionsMetadataResponse_unfilteredPartitions,
    getUnfilteredPartitionsMetadataResponse_httpStatus,

    -- ** GetUnfilteredTableMetadata
    getUnfilteredTableMetadata_auditContext,
    getUnfilteredTableMetadata_catalogId,
    getUnfilteredTableMetadata_databaseName,
    getUnfilteredTableMetadata_name,
    getUnfilteredTableMetadata_supportedPermissionTypes,
    getUnfilteredTableMetadataResponse_authorizedColumns,
    getUnfilteredTableMetadataResponse_cellFilters,
    getUnfilteredTableMetadataResponse_isRegisteredWithLakeFormation,
    getUnfilteredTableMetadataResponse_table,
    getUnfilteredTableMetadataResponse_httpStatus,

    -- ** GetUserDefinedFunction
    getUserDefinedFunction_catalogId,
    getUserDefinedFunction_databaseName,
    getUserDefinedFunction_functionName,
    getUserDefinedFunctionResponse_userDefinedFunction,
    getUserDefinedFunctionResponse_httpStatus,

    -- ** GetUserDefinedFunctions
    getUserDefinedFunctions_catalogId,
    getUserDefinedFunctions_databaseName,
    getUserDefinedFunctions_maxResults,
    getUserDefinedFunctions_nextToken,
    getUserDefinedFunctions_pattern,
    getUserDefinedFunctionsResponse_nextToken,
    getUserDefinedFunctionsResponse_userDefinedFunctions,
    getUserDefinedFunctionsResponse_httpStatus,

    -- ** GetWorkflow
    getWorkflow_includeGraph,
    getWorkflow_name,
    getWorkflowResponse_workflow,
    getWorkflowResponse_httpStatus,

    -- ** GetWorkflowRun
    getWorkflowRun_includeGraph,
    getWorkflowRun_name,
    getWorkflowRun_runId,
    getWorkflowRunResponse_run,
    getWorkflowRunResponse_httpStatus,

    -- ** GetWorkflowRunProperties
    getWorkflowRunProperties_name,
    getWorkflowRunProperties_runId,
    getWorkflowRunPropertiesResponse_runProperties,
    getWorkflowRunPropertiesResponse_httpStatus,

    -- ** GetWorkflowRuns
    getWorkflowRuns_includeGraph,
    getWorkflowRuns_maxResults,
    getWorkflowRuns_nextToken,
    getWorkflowRuns_name,
    getWorkflowRunsResponse_nextToken,
    getWorkflowRunsResponse_runs,
    getWorkflowRunsResponse_httpStatus,

    -- ** ImportCatalogToGlue
    importCatalogToGlue_catalogId,
    importCatalogToGlueResponse_httpStatus,

    -- ** ListBlueprints
    listBlueprints_maxResults,
    listBlueprints_nextToken,
    listBlueprints_tags,
    listBlueprintsResponse_blueprints,
    listBlueprintsResponse_nextToken,
    listBlueprintsResponse_httpStatus,

    -- ** ListCrawlers
    listCrawlers_maxResults,
    listCrawlers_nextToken,
    listCrawlers_tags,
    listCrawlersResponse_crawlerNames,
    listCrawlersResponse_nextToken,
    listCrawlersResponse_httpStatus,

    -- ** ListCrawls
    listCrawls_filters,
    listCrawls_maxResults,
    listCrawls_nextToken,
    listCrawls_crawlerName,
    listCrawlsResponse_crawls,
    listCrawlsResponse_nextToken,
    listCrawlsResponse_httpStatus,

    -- ** ListCustomEntityTypes
    listCustomEntityTypes_maxResults,
    listCustomEntityTypes_nextToken,
    listCustomEntityTypesResponse_customEntityTypes,
    listCustomEntityTypesResponse_nextToken,
    listCustomEntityTypesResponse_httpStatus,

    -- ** ListDataQualityResults
    listDataQualityResults_filter,
    listDataQualityResults_maxResults,
    listDataQualityResults_nextToken,
    listDataQualityResultsResponse_nextToken,
    listDataQualityResultsResponse_httpStatus,
    listDataQualityResultsResponse_results,

    -- ** ListDataQualityRuleRecommendationRuns
    listDataQualityRuleRecommendationRuns_filter,
    listDataQualityRuleRecommendationRuns_maxResults,
    listDataQualityRuleRecommendationRuns_nextToken,
    listDataQualityRuleRecommendationRunsResponse_nextToken,
    listDataQualityRuleRecommendationRunsResponse_runs,
    listDataQualityRuleRecommendationRunsResponse_httpStatus,

    -- ** ListDataQualityRulesetEvaluationRuns
    listDataQualityRulesetEvaluationRuns_filter,
    listDataQualityRulesetEvaluationRuns_maxResults,
    listDataQualityRulesetEvaluationRuns_nextToken,
    listDataQualityRulesetEvaluationRunsResponse_nextToken,
    listDataQualityRulesetEvaluationRunsResponse_runs,
    listDataQualityRulesetEvaluationRunsResponse_httpStatus,

    -- ** ListDataQualityRulesets
    listDataQualityRulesets_filter,
    listDataQualityRulesets_maxResults,
    listDataQualityRulesets_nextToken,
    listDataQualityRulesets_tags,
    listDataQualityRulesetsResponse_nextToken,
    listDataQualityRulesetsResponse_rulesets,
    listDataQualityRulesetsResponse_httpStatus,

    -- ** ListDevEndpoints
    listDevEndpoints_maxResults,
    listDevEndpoints_nextToken,
    listDevEndpoints_tags,
    listDevEndpointsResponse_devEndpointNames,
    listDevEndpointsResponse_nextToken,
    listDevEndpointsResponse_httpStatus,

    -- ** ListJobs
    listJobs_maxResults,
    listJobs_nextToken,
    listJobs_tags,
    listJobsResponse_jobNames,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,

    -- ** ListMLTransforms
    listMLTransforms_filter,
    listMLTransforms_maxResults,
    listMLTransforms_nextToken,
    listMLTransforms_sort,
    listMLTransforms_tags,
    listMLTransformsResponse_nextToken,
    listMLTransformsResponse_httpStatus,
    listMLTransformsResponse_transformIds,

    -- ** ListRegistries
    listRegistries_maxResults,
    listRegistries_nextToken,
    listRegistriesResponse_nextToken,
    listRegistriesResponse_registries,
    listRegistriesResponse_httpStatus,

    -- ** ListSchemaVersions
    listSchemaVersions_maxResults,
    listSchemaVersions_nextToken,
    listSchemaVersions_schemaId,
    listSchemaVersionsResponse_nextToken,
    listSchemaVersionsResponse_schemas,
    listSchemaVersionsResponse_httpStatus,

    -- ** ListSchemas
    listSchemas_maxResults,
    listSchemas_nextToken,
    listSchemas_registryId,
    listSchemasResponse_nextToken,
    listSchemasResponse_schemas,
    listSchemasResponse_httpStatus,

    -- ** ListSessions
    listSessions_maxResults,
    listSessions_nextToken,
    listSessions_requestOrigin,
    listSessions_tags,
    listSessionsResponse_ids,
    listSessionsResponse_nextToken,
    listSessionsResponse_sessions,
    listSessionsResponse_httpStatus,

    -- ** ListStatements
    listStatements_nextToken,
    listStatements_requestOrigin,
    listStatements_sessionId,
    listStatementsResponse_nextToken,
    listStatementsResponse_statements,
    listStatementsResponse_httpStatus,

    -- ** ListTriggers
    listTriggers_dependentJobName,
    listTriggers_maxResults,
    listTriggers_nextToken,
    listTriggers_tags,
    listTriggersResponse_nextToken,
    listTriggersResponse_triggerNames,
    listTriggersResponse_httpStatus,

    -- ** ListWorkflows
    listWorkflows_maxResults,
    listWorkflows_nextToken,
    listWorkflowsResponse_nextToken,
    listWorkflowsResponse_workflows,
    listWorkflowsResponse_httpStatus,

    -- ** PutDataCatalogEncryptionSettings
    putDataCatalogEncryptionSettings_catalogId,
    putDataCatalogEncryptionSettings_dataCatalogEncryptionSettings,
    putDataCatalogEncryptionSettingsResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_enableHybrid,
    putResourcePolicy_policyExistsCondition,
    putResourcePolicy_policyHashCondition,
    putResourcePolicy_resourceArn,
    putResourcePolicy_policyInJson,
    putResourcePolicyResponse_policyHash,
    putResourcePolicyResponse_httpStatus,

    -- ** PutSchemaVersionMetadata
    putSchemaVersionMetadata_schemaId,
    putSchemaVersionMetadata_schemaVersionId,
    putSchemaVersionMetadata_schemaVersionNumber,
    putSchemaVersionMetadata_metadataKeyValue,
    putSchemaVersionMetadataResponse_latestVersion,
    putSchemaVersionMetadataResponse_metadataKey,
    putSchemaVersionMetadataResponse_metadataValue,
    putSchemaVersionMetadataResponse_registryName,
    putSchemaVersionMetadataResponse_schemaArn,
    putSchemaVersionMetadataResponse_schemaName,
    putSchemaVersionMetadataResponse_schemaVersionId,
    putSchemaVersionMetadataResponse_versionNumber,
    putSchemaVersionMetadataResponse_httpStatus,

    -- ** PutWorkflowRunProperties
    putWorkflowRunProperties_name,
    putWorkflowRunProperties_runId,
    putWorkflowRunProperties_runProperties,
    putWorkflowRunPropertiesResponse_httpStatus,

    -- ** QuerySchemaVersionMetadata
    querySchemaVersionMetadata_maxResults,
    querySchemaVersionMetadata_metadataList,
    querySchemaVersionMetadata_nextToken,
    querySchemaVersionMetadata_schemaId,
    querySchemaVersionMetadata_schemaVersionId,
    querySchemaVersionMetadata_schemaVersionNumber,
    querySchemaVersionMetadataResponse_metadataInfoMap,
    querySchemaVersionMetadataResponse_nextToken,
    querySchemaVersionMetadataResponse_schemaVersionId,
    querySchemaVersionMetadataResponse_httpStatus,

    -- ** RegisterSchemaVersion
    registerSchemaVersion_schemaId,
    registerSchemaVersion_schemaDefinition,
    registerSchemaVersionResponse_schemaVersionId,
    registerSchemaVersionResponse_status,
    registerSchemaVersionResponse_versionNumber,
    registerSchemaVersionResponse_httpStatus,

    -- ** RemoveSchemaVersionMetadata
    removeSchemaVersionMetadata_schemaId,
    removeSchemaVersionMetadata_schemaVersionId,
    removeSchemaVersionMetadata_schemaVersionNumber,
    removeSchemaVersionMetadata_metadataKeyValue,
    removeSchemaVersionMetadataResponse_latestVersion,
    removeSchemaVersionMetadataResponse_metadataKey,
    removeSchemaVersionMetadataResponse_metadataValue,
    removeSchemaVersionMetadataResponse_registryName,
    removeSchemaVersionMetadataResponse_schemaArn,
    removeSchemaVersionMetadataResponse_schemaName,
    removeSchemaVersionMetadataResponse_schemaVersionId,
    removeSchemaVersionMetadataResponse_versionNumber,
    removeSchemaVersionMetadataResponse_httpStatus,

    -- ** ResetJobBookmark
    resetJobBookmark_runId,
    resetJobBookmark_jobName,
    resetJobBookmarkResponse_jobBookmarkEntry,
    resetJobBookmarkResponse_httpStatus,

    -- ** ResumeWorkflowRun
    resumeWorkflowRun_name,
    resumeWorkflowRun_runId,
    resumeWorkflowRun_nodeIds,
    resumeWorkflowRunResponse_nodeIds,
    resumeWorkflowRunResponse_runId,
    resumeWorkflowRunResponse_httpStatus,

    -- ** RunStatement
    runStatement_requestOrigin,
    runStatement_sessionId,
    runStatement_code,
    runStatementResponse_id,
    runStatementResponse_httpStatus,

    -- ** SearchTables
    searchTables_catalogId,
    searchTables_filters,
    searchTables_maxResults,
    searchTables_nextToken,
    searchTables_resourceShareType,
    searchTables_searchText,
    searchTables_sortCriteria,
    searchTablesResponse_nextToken,
    searchTablesResponse_tableList,
    searchTablesResponse_httpStatus,

    -- ** StartBlueprintRun
    startBlueprintRun_parameters,
    startBlueprintRun_blueprintName,
    startBlueprintRun_roleArn,
    startBlueprintRunResponse_runId,
    startBlueprintRunResponse_httpStatus,

    -- ** StartCrawler
    startCrawler_name,
    startCrawlerResponse_httpStatus,

    -- ** StartCrawlerSchedule
    startCrawlerSchedule_crawlerName,
    startCrawlerScheduleResponse_httpStatus,

    -- ** StartDataQualityRuleRecommendationRun
    startDataQualityRuleRecommendationRun_clientToken,
    startDataQualityRuleRecommendationRun_createdRulesetName,
    startDataQualityRuleRecommendationRun_numberOfWorkers,
    startDataQualityRuleRecommendationRun_timeout,
    startDataQualityRuleRecommendationRun_dataSource,
    startDataQualityRuleRecommendationRun_role,
    startDataQualityRuleRecommendationRunResponse_runId,
    startDataQualityRuleRecommendationRunResponse_httpStatus,

    -- ** StartDataQualityRulesetEvaluationRun
    startDataQualityRulesetEvaluationRun_additionalRunOptions,
    startDataQualityRulesetEvaluationRun_clientToken,
    startDataQualityRulesetEvaluationRun_numberOfWorkers,
    startDataQualityRulesetEvaluationRun_timeout,
    startDataQualityRulesetEvaluationRun_dataSource,
    startDataQualityRulesetEvaluationRun_role,
    startDataQualityRulesetEvaluationRun_rulesetNames,
    startDataQualityRulesetEvaluationRunResponse_runId,
    startDataQualityRulesetEvaluationRunResponse_httpStatus,

    -- ** StartExportLabelsTaskRun
    startExportLabelsTaskRun_transformId,
    startExportLabelsTaskRun_outputS3Path,
    startExportLabelsTaskRunResponse_taskRunId,
    startExportLabelsTaskRunResponse_httpStatus,

    -- ** StartImportLabelsTaskRun
    startImportLabelsTaskRun_replaceAllLabels,
    startImportLabelsTaskRun_transformId,
    startImportLabelsTaskRun_inputS3Path,
    startImportLabelsTaskRunResponse_taskRunId,
    startImportLabelsTaskRunResponse_httpStatus,

    -- ** StartJobRun
    startJobRun_allocatedCapacity,
    startJobRun_arguments,
    startJobRun_executionClass,
    startJobRun_jobRunId,
    startJobRun_maxCapacity,
    startJobRun_notificationProperty,
    startJobRun_numberOfWorkers,
    startJobRun_securityConfiguration,
    startJobRun_timeout,
    startJobRun_workerType,
    startJobRun_jobName,
    startJobRunResponse_jobRunId,
    startJobRunResponse_httpStatus,

    -- ** StartMLEvaluationTaskRun
    startMLEvaluationTaskRun_transformId,
    startMLEvaluationTaskRunResponse_taskRunId,
    startMLEvaluationTaskRunResponse_httpStatus,

    -- ** StartMLLabelingSetGenerationTaskRun
    startMLLabelingSetGenerationTaskRun_transformId,
    startMLLabelingSetGenerationTaskRun_outputS3Path,
    startMLLabelingSetGenerationTaskRunResponse_taskRunId,
    startMLLabelingSetGenerationTaskRunResponse_httpStatus,

    -- ** StartTrigger
    startTrigger_name,
    startTriggerResponse_name,
    startTriggerResponse_httpStatus,

    -- ** StartWorkflowRun
    startWorkflowRun_runProperties,
    startWorkflowRun_name,
    startWorkflowRunResponse_runId,
    startWorkflowRunResponse_httpStatus,

    -- ** StopCrawler
    stopCrawler_name,
    stopCrawlerResponse_httpStatus,

    -- ** StopCrawlerSchedule
    stopCrawlerSchedule_crawlerName,
    stopCrawlerScheduleResponse_httpStatus,

    -- ** StopSession
    stopSession_requestOrigin,
    stopSession_id,
    stopSessionResponse_id,
    stopSessionResponse_httpStatus,

    -- ** StopTrigger
    stopTrigger_name,
    stopTriggerResponse_name,
    stopTriggerResponse_httpStatus,

    -- ** StopWorkflowRun
    stopWorkflowRun_name,
    stopWorkflowRun_runId,
    stopWorkflowRunResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tagsToAdd,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagsToRemove,
    untagResourceResponse_httpStatus,

    -- ** UpdateBlueprint
    updateBlueprint_description,
    updateBlueprint_name,
    updateBlueprint_blueprintLocation,
    updateBlueprintResponse_name,
    updateBlueprintResponse_httpStatus,

    -- ** UpdateClassifier
    updateClassifier_csvClassifier,
    updateClassifier_grokClassifier,
    updateClassifier_jsonClassifier,
    updateClassifier_xMLClassifier,
    updateClassifierResponse_httpStatus,

    -- ** UpdateColumnStatisticsForPartition
    updateColumnStatisticsForPartition_catalogId,
    updateColumnStatisticsForPartition_databaseName,
    updateColumnStatisticsForPartition_tableName,
    updateColumnStatisticsForPartition_partitionValues,
    updateColumnStatisticsForPartition_columnStatisticsList,
    updateColumnStatisticsForPartitionResponse_errors,
    updateColumnStatisticsForPartitionResponse_httpStatus,

    -- ** UpdateColumnStatisticsForTable
    updateColumnStatisticsForTable_catalogId,
    updateColumnStatisticsForTable_databaseName,
    updateColumnStatisticsForTable_tableName,
    updateColumnStatisticsForTable_columnStatisticsList,
    updateColumnStatisticsForTableResponse_errors,
    updateColumnStatisticsForTableResponse_httpStatus,

    -- ** UpdateConnection
    updateConnection_catalogId,
    updateConnection_name,
    updateConnection_connectionInput,
    updateConnectionResponse_httpStatus,

    -- ** UpdateCrawler
    updateCrawler_classifiers,
    updateCrawler_configuration,
    updateCrawler_crawlerSecurityConfiguration,
    updateCrawler_databaseName,
    updateCrawler_description,
    updateCrawler_lakeFormationConfiguration,
    updateCrawler_lineageConfiguration,
    updateCrawler_recrawlPolicy,
    updateCrawler_role,
    updateCrawler_schedule,
    updateCrawler_schemaChangePolicy,
    updateCrawler_tablePrefix,
    updateCrawler_targets,
    updateCrawler_name,
    updateCrawlerResponse_httpStatus,

    -- ** UpdateCrawlerSchedule
    updateCrawlerSchedule_schedule,
    updateCrawlerSchedule_crawlerName,
    updateCrawlerScheduleResponse_httpStatus,

    -- ** UpdateDataQualityRuleset
    updateDataQualityRuleset_description,
    updateDataQualityRuleset_ruleset,
    updateDataQualityRuleset_updatedName,
    updateDataQualityRuleset_name,
    updateDataQualityRulesetResponse_description,
    updateDataQualityRulesetResponse_name,
    updateDataQualityRulesetResponse_ruleset,
    updateDataQualityRulesetResponse_httpStatus,

    -- ** UpdateDatabase
    updateDatabase_catalogId,
    updateDatabase_name,
    updateDatabase_databaseInput,
    updateDatabaseResponse_httpStatus,

    -- ** UpdateDevEndpoint
    updateDevEndpoint_addArguments,
    updateDevEndpoint_addPublicKeys,
    updateDevEndpoint_customLibraries,
    updateDevEndpoint_deleteArguments,
    updateDevEndpoint_deletePublicKeys,
    updateDevEndpoint_publicKey,
    updateDevEndpoint_updateEtlLibraries,
    updateDevEndpoint_endpointName,
    updateDevEndpointResponse_httpStatus,

    -- ** UpdateJob
    updateJob_jobName,
    updateJob_jobUpdate,
    updateJobResponse_jobName,
    updateJobResponse_httpStatus,

    -- ** UpdateJobFromSourceControl
    updateJobFromSourceControl_authStrategy,
    updateJobFromSourceControl_authToken,
    updateJobFromSourceControl_branchName,
    updateJobFromSourceControl_commitId,
    updateJobFromSourceControl_folder,
    updateJobFromSourceControl_jobName,
    updateJobFromSourceControl_provider,
    updateJobFromSourceControl_repositoryName,
    updateJobFromSourceControl_repositoryOwner,
    updateJobFromSourceControlResponse_jobName,
    updateJobFromSourceControlResponse_httpStatus,

    -- ** UpdateMLTransform
    updateMLTransform_description,
    updateMLTransform_glueVersion,
    updateMLTransform_maxCapacity,
    updateMLTransform_maxRetries,
    updateMLTransform_name,
    updateMLTransform_numberOfWorkers,
    updateMLTransform_parameters,
    updateMLTransform_role,
    updateMLTransform_timeout,
    updateMLTransform_workerType,
    updateMLTransform_transformId,
    updateMLTransformResponse_transformId,
    updateMLTransformResponse_httpStatus,

    -- ** UpdatePartition
    updatePartition_catalogId,
    updatePartition_databaseName,
    updatePartition_tableName,
    updatePartition_partitionValueList,
    updatePartition_partitionInput,
    updatePartitionResponse_httpStatus,

    -- ** UpdateRegistry
    updateRegistry_registryId,
    updateRegistry_description,
    updateRegistryResponse_registryArn,
    updateRegistryResponse_registryName,
    updateRegistryResponse_httpStatus,

    -- ** UpdateSchema
    updateSchema_compatibility,
    updateSchema_description,
    updateSchema_schemaVersionNumber,
    updateSchema_schemaId,
    updateSchemaResponse_registryName,
    updateSchemaResponse_schemaArn,
    updateSchemaResponse_schemaName,
    updateSchemaResponse_httpStatus,

    -- ** UpdateSourceControlFromJob
    updateSourceControlFromJob_authStrategy,
    updateSourceControlFromJob_authToken,
    updateSourceControlFromJob_branchName,
    updateSourceControlFromJob_commitId,
    updateSourceControlFromJob_folder,
    updateSourceControlFromJob_jobName,
    updateSourceControlFromJob_provider,
    updateSourceControlFromJob_repositoryName,
    updateSourceControlFromJob_repositoryOwner,
    updateSourceControlFromJobResponse_jobName,
    updateSourceControlFromJobResponse_httpStatus,

    -- ** UpdateTable
    updateTable_catalogId,
    updateTable_skipArchive,
    updateTable_transactionId,
    updateTable_versionId,
    updateTable_databaseName,
    updateTable_tableInput,
    updateTableResponse_httpStatus,

    -- ** UpdateTrigger
    updateTrigger_name,
    updateTrigger_triggerUpdate,
    updateTriggerResponse_trigger,
    updateTriggerResponse_httpStatus,

    -- ** UpdateUserDefinedFunction
    updateUserDefinedFunction_catalogId,
    updateUserDefinedFunction_databaseName,
    updateUserDefinedFunction_functionName,
    updateUserDefinedFunction_functionInput,
    updateUserDefinedFunctionResponse_httpStatus,

    -- ** UpdateWorkflow
    updateWorkflow_defaultRunProperties,
    updateWorkflow_description,
    updateWorkflow_maxConcurrentRuns,
    updateWorkflow_name,
    updateWorkflowResponse_name,
    updateWorkflowResponse_httpStatus,

    -- * Types

    -- ** Action
    action_arguments,
    action_crawlerName,
    action_jobName,
    action_notificationProperty,
    action_securityConfiguration,
    action_timeout,

    -- ** Aggregate
    aggregate_name,
    aggregate_inputs,
    aggregate_groups,
    aggregate_aggs,

    -- ** AggregateOperation
    aggregateOperation_column,
    aggregateOperation_aggFunc,

    -- ** ApplyMapping
    applyMapping_name,
    applyMapping_inputs,
    applyMapping_mapping,

    -- ** AthenaConnectorSource
    athenaConnectorSource_connectionTable,
    athenaConnectorSource_outputSchemas,
    athenaConnectorSource_name,
    athenaConnectorSource_connectionName,
    athenaConnectorSource_connectorName,
    athenaConnectorSource_connectionType,
    athenaConnectorSource_schemaName,

    -- ** AuditContext
    auditContext_additionalAuditContext,
    auditContext_allColumnsRequested,
    auditContext_requestedColumns,

    -- ** BackfillError
    backfillError_code,
    backfillError_partitions,

    -- ** BasicCatalogTarget
    basicCatalogTarget_name,
    basicCatalogTarget_inputs,
    basicCatalogTarget_database,
    basicCatalogTarget_table,

    -- ** BatchStopJobRunError
    batchStopJobRunError_errorDetail,
    batchStopJobRunError_jobName,
    batchStopJobRunError_jobRunId,

    -- ** BatchStopJobRunSuccessfulSubmission
    batchStopJobRunSuccessfulSubmission_jobName,
    batchStopJobRunSuccessfulSubmission_jobRunId,

    -- ** BatchUpdatePartitionFailureEntry
    batchUpdatePartitionFailureEntry_errorDetail,
    batchUpdatePartitionFailureEntry_partitionValueList,

    -- ** BatchUpdatePartitionRequestEntry
    batchUpdatePartitionRequestEntry_partitionValueList,
    batchUpdatePartitionRequestEntry_partitionInput,

    -- ** BinaryColumnStatisticsData
    binaryColumnStatisticsData_maximumLength,
    binaryColumnStatisticsData_averageLength,
    binaryColumnStatisticsData_numberOfNulls,

    -- ** Blueprint
    blueprint_blueprintLocation,
    blueprint_blueprintServiceLocation,
    blueprint_createdOn,
    blueprint_description,
    blueprint_errorMessage,
    blueprint_lastActiveDefinition,
    blueprint_lastModifiedOn,
    blueprint_name,
    blueprint_parameterSpec,
    blueprint_status,

    -- ** BlueprintDetails
    blueprintDetails_blueprintName,
    blueprintDetails_runId,

    -- ** BlueprintRun
    blueprintRun_blueprintName,
    blueprintRun_completedOn,
    blueprintRun_errorMessage,
    blueprintRun_parameters,
    blueprintRun_roleArn,
    blueprintRun_rollbackErrorMessage,
    blueprintRun_runId,
    blueprintRun_startedOn,
    blueprintRun_state,
    blueprintRun_workflowName,

    -- ** BooleanColumnStatisticsData
    booleanColumnStatisticsData_numberOfTrues,
    booleanColumnStatisticsData_numberOfFalses,
    booleanColumnStatisticsData_numberOfNulls,

    -- ** CatalogEntry
    catalogEntry_databaseName,
    catalogEntry_tableName,

    -- ** CatalogImportStatus
    catalogImportStatus_importCompleted,
    catalogImportStatus_importTime,
    catalogImportStatus_importedBy,

    -- ** CatalogKafkaSource
    catalogKafkaSource_dataPreviewOptions,
    catalogKafkaSource_detectSchema,
    catalogKafkaSource_streamingOptions,
    catalogKafkaSource_windowSize,
    catalogKafkaSource_name,
    catalogKafkaSource_table,
    catalogKafkaSource_database,

    -- ** CatalogKinesisSource
    catalogKinesisSource_dataPreviewOptions,
    catalogKinesisSource_detectSchema,
    catalogKinesisSource_streamingOptions,
    catalogKinesisSource_windowSize,
    catalogKinesisSource_name,
    catalogKinesisSource_table,
    catalogKinesisSource_database,

    -- ** CatalogSchemaChangePolicy
    catalogSchemaChangePolicy_enableUpdateCatalog,
    catalogSchemaChangePolicy_updateBehavior,

    -- ** CatalogSource
    catalogSource_name,
    catalogSource_database,
    catalogSource_table,

    -- ** CatalogTarget
    catalogTarget_connectionName,
    catalogTarget_dlqEventQueueArn,
    catalogTarget_eventQueueArn,
    catalogTarget_databaseName,
    catalogTarget_tables,

    -- ** Classifier
    classifier_csvClassifier,
    classifier_grokClassifier,
    classifier_jsonClassifier,
    classifier_xMLClassifier,

    -- ** CloudWatchEncryption
    cloudWatchEncryption_cloudWatchEncryptionMode,
    cloudWatchEncryption_kmsKeyArn,

    -- ** CodeGenConfigurationNode
    codeGenConfigurationNode_aggregate,
    codeGenConfigurationNode_applyMapping,
    codeGenConfigurationNode_athenaConnectorSource,
    codeGenConfigurationNode_catalogKafkaSource,
    codeGenConfigurationNode_catalogKinesisSource,
    codeGenConfigurationNode_catalogSource,
    codeGenConfigurationNode_catalogTarget,
    codeGenConfigurationNode_customCode,
    codeGenConfigurationNode_directKafkaSource,
    codeGenConfigurationNode_directKinesisSource,
    codeGenConfigurationNode_dropDuplicates,
    codeGenConfigurationNode_dropFields,
    codeGenConfigurationNode_dropNullFields,
    codeGenConfigurationNode_dynamicTransform,
    codeGenConfigurationNode_dynamoDBCatalogSource,
    codeGenConfigurationNode_evaluateDataQuality,
    codeGenConfigurationNode_fillMissingValues,
    codeGenConfigurationNode_filter,
    codeGenConfigurationNode_governedCatalogSource,
    codeGenConfigurationNode_governedCatalogTarget,
    codeGenConfigurationNode_jDBCConnectorSource,
    codeGenConfigurationNode_jDBCConnectorTarget,
    codeGenConfigurationNode_join,
    codeGenConfigurationNode_merge,
    codeGenConfigurationNode_microsoftSQLServerCatalogSource,
    codeGenConfigurationNode_microsoftSQLServerCatalogTarget,
    codeGenConfigurationNode_mySQLCatalogSource,
    codeGenConfigurationNode_mySQLCatalogTarget,
    codeGenConfigurationNode_oracleSQLCatalogSource,
    codeGenConfigurationNode_oracleSQLCatalogTarget,
    codeGenConfigurationNode_pIIDetection,
    codeGenConfigurationNode_postgreSQLCatalogSource,
    codeGenConfigurationNode_postgreSQLCatalogTarget,
    codeGenConfigurationNode_redshiftSource,
    codeGenConfigurationNode_redshiftTarget,
    codeGenConfigurationNode_relationalCatalogSource,
    codeGenConfigurationNode_renameField,
    codeGenConfigurationNode_s3CatalogSource,
    codeGenConfigurationNode_s3CatalogTarget,
    codeGenConfigurationNode_s3CsvSource,
    codeGenConfigurationNode_s3DirectTarget,
    codeGenConfigurationNode_s3GlueParquetTarget,
    codeGenConfigurationNode_s3JsonSource,
    codeGenConfigurationNode_s3ParquetSource,
    codeGenConfigurationNode_selectFields,
    codeGenConfigurationNode_selectFromCollection,
    codeGenConfigurationNode_sparkConnectorSource,
    codeGenConfigurationNode_sparkConnectorTarget,
    codeGenConfigurationNode_sparkSQL,
    codeGenConfigurationNode_spigot,
    codeGenConfigurationNode_splitFields,
    codeGenConfigurationNode_union,

    -- ** CodeGenEdge
    codeGenEdge_targetParameter,
    codeGenEdge_source,
    codeGenEdge_target,

    -- ** CodeGenNode
    codeGenNode_lineNumber,
    codeGenNode_id,
    codeGenNode_nodeType,
    codeGenNode_args,

    -- ** CodeGenNodeArg
    codeGenNodeArg_param,
    codeGenNodeArg_name,
    codeGenNodeArg_value,

    -- ** Column
    column_comment,
    column_parameters,
    column_type,
    column_name,

    -- ** ColumnError
    columnError_columnName,
    columnError_error,

    -- ** ColumnImportance
    columnImportance_columnName,
    columnImportance_importance,

    -- ** ColumnRowFilter
    columnRowFilter_columnName,
    columnRowFilter_rowFilterExpression,

    -- ** ColumnStatistics
    columnStatistics_columnName,
    columnStatistics_columnType,
    columnStatistics_analyzedTime,
    columnStatistics_statisticsData,

    -- ** ColumnStatisticsData
    columnStatisticsData_binaryColumnStatisticsData,
    columnStatisticsData_booleanColumnStatisticsData,
    columnStatisticsData_dateColumnStatisticsData,
    columnStatisticsData_decimalColumnStatisticsData,
    columnStatisticsData_doubleColumnStatisticsData,
    columnStatisticsData_longColumnStatisticsData,
    columnStatisticsData_stringColumnStatisticsData,
    columnStatisticsData_type,

    -- ** ColumnStatisticsError
    columnStatisticsError_columnStatistics,
    columnStatisticsError_error,

    -- ** Condition
    condition_crawlState,
    condition_crawlerName,
    condition_jobName,
    condition_logicalOperator,
    condition_state,

    -- ** ConfusionMatrix
    confusionMatrix_numFalseNegatives,
    confusionMatrix_numFalsePositives,
    confusionMatrix_numTrueNegatives,
    confusionMatrix_numTruePositives,

    -- ** Connection
    connection_connectionProperties,
    connection_connectionType,
    connection_creationTime,
    connection_description,
    connection_lastUpdatedBy,
    connection_lastUpdatedTime,
    connection_matchCriteria,
    connection_name,
    connection_physicalConnectionRequirements,

    -- ** ConnectionInput
    connectionInput_description,
    connectionInput_matchCriteria,
    connectionInput_physicalConnectionRequirements,
    connectionInput_name,
    connectionInput_connectionType,
    connectionInput_connectionProperties,

    -- ** ConnectionPasswordEncryption
    connectionPasswordEncryption_awsKmsKeyId,
    connectionPasswordEncryption_returnConnectionPasswordEncrypted,

    -- ** ConnectionsList
    connectionsList_connections,

    -- ** Crawl
    crawl_completedOn,
    crawl_errorMessage,
    crawl_logGroup,
    crawl_logStream,
    crawl_startedOn,
    crawl_state,

    -- ** Crawler
    crawler_classifiers,
    crawler_configuration,
    crawler_crawlElapsedTime,
    crawler_crawlerSecurityConfiguration,
    crawler_creationTime,
    crawler_databaseName,
    crawler_description,
    crawler_lakeFormationConfiguration,
    crawler_lastCrawl,
    crawler_lastUpdated,
    crawler_lineageConfiguration,
    crawler_name,
    crawler_recrawlPolicy,
    crawler_role,
    crawler_schedule,
    crawler_schemaChangePolicy,
    crawler_state,
    crawler_tablePrefix,
    crawler_targets,
    crawler_version,

    -- ** CrawlerHistory
    crawlerHistory_crawlId,
    crawlerHistory_dPUHour,
    crawlerHistory_endTime,
    crawlerHistory_errorMessage,
    crawlerHistory_logGroup,
    crawlerHistory_logStream,
    crawlerHistory_messagePrefix,
    crawlerHistory_startTime,
    crawlerHistory_state,
    crawlerHistory_summary,

    -- ** CrawlerMetrics
    crawlerMetrics_crawlerName,
    crawlerMetrics_lastRuntimeSeconds,
    crawlerMetrics_medianRuntimeSeconds,
    crawlerMetrics_stillEstimating,
    crawlerMetrics_tablesCreated,
    crawlerMetrics_tablesDeleted,
    crawlerMetrics_tablesUpdated,
    crawlerMetrics_timeLeftSeconds,

    -- ** CrawlerNodeDetails
    crawlerNodeDetails_crawls,

    -- ** CrawlerTargets
    crawlerTargets_catalogTargets,
    crawlerTargets_deltaTargets,
    crawlerTargets_dynamoDBTargets,
    crawlerTargets_jdbcTargets,
    crawlerTargets_mongoDBTargets,
    crawlerTargets_s3Targets,

    -- ** CrawlsFilter
    crawlsFilter_fieldName,
    crawlsFilter_fieldValue,
    crawlsFilter_filterOperator,

    -- ** CreateCsvClassifierRequest
    createCsvClassifierRequest_allowSingleColumn,
    createCsvClassifierRequest_containsHeader,
    createCsvClassifierRequest_customDatatypeConfigured,
    createCsvClassifierRequest_customDatatypes,
    createCsvClassifierRequest_delimiter,
    createCsvClassifierRequest_disableValueTrimming,
    createCsvClassifierRequest_header,
    createCsvClassifierRequest_quoteSymbol,
    createCsvClassifierRequest_name,

    -- ** CreateGrokClassifierRequest
    createGrokClassifierRequest_customPatterns,
    createGrokClassifierRequest_classification,
    createGrokClassifierRequest_name,
    createGrokClassifierRequest_grokPattern,

    -- ** CreateJsonClassifierRequest
    createJsonClassifierRequest_name,
    createJsonClassifierRequest_jsonPath,

    -- ** CreateXMLClassifierRequest
    createXMLClassifierRequest_rowTag,
    createXMLClassifierRequest_classification,
    createXMLClassifierRequest_name,

    -- ** CsvClassifier
    csvClassifier_allowSingleColumn,
    csvClassifier_containsHeader,
    csvClassifier_creationTime,
    csvClassifier_customDatatypeConfigured,
    csvClassifier_customDatatypes,
    csvClassifier_delimiter,
    csvClassifier_disableValueTrimming,
    csvClassifier_header,
    csvClassifier_lastUpdated,
    csvClassifier_quoteSymbol,
    csvClassifier_version,
    csvClassifier_name,

    -- ** CustomCode
    customCode_outputSchemas,
    customCode_name,
    customCode_inputs,
    customCode_code,
    customCode_className,

    -- ** CustomEntityType
    customEntityType_contextWords,
    customEntityType_name,
    customEntityType_regexString,

    -- ** DQResultsPublishingOptions
    dQResultsPublishingOptions_cloudWatchMetricsEnabled,
    dQResultsPublishingOptions_evaluationContext,
    dQResultsPublishingOptions_resultsPublishingEnabled,
    dQResultsPublishingOptions_resultsS3Prefix,

    -- ** DQStopJobOnFailureOptions
    dQStopJobOnFailureOptions_stopJobOnFailureTiming,

    -- ** DataCatalogEncryptionSettings
    dataCatalogEncryptionSettings_connectionPasswordEncryption,
    dataCatalogEncryptionSettings_encryptionAtRest,

    -- ** DataLakePrincipal
    dataLakePrincipal_dataLakePrincipalIdentifier,

    -- ** DataQualityEvaluationRunAdditionalRunOptions
    dataQualityEvaluationRunAdditionalRunOptions_cloudWatchMetricsEnabled,
    dataQualityEvaluationRunAdditionalRunOptions_resultsS3Prefix,

    -- ** DataQualityResult
    dataQualityResult_completedOn,
    dataQualityResult_dataSource,
    dataQualityResult_evaluationContext,
    dataQualityResult_jobName,
    dataQualityResult_jobRunId,
    dataQualityResult_resultId,
    dataQualityResult_ruleResults,
    dataQualityResult_rulesetEvaluationRunId,
    dataQualityResult_rulesetName,
    dataQualityResult_score,
    dataQualityResult_startedOn,

    -- ** DataQualityResultDescription
    dataQualityResultDescription_dataSource,
    dataQualityResultDescription_jobName,
    dataQualityResultDescription_jobRunId,
    dataQualityResultDescription_resultId,
    dataQualityResultDescription_startedOn,

    -- ** DataQualityResultFilterCriteria
    dataQualityResultFilterCriteria_dataSource,
    dataQualityResultFilterCriteria_jobName,
    dataQualityResultFilterCriteria_jobRunId,
    dataQualityResultFilterCriteria_startedAfter,
    dataQualityResultFilterCriteria_startedBefore,

    -- ** DataQualityRuleRecommendationRunDescription
    dataQualityRuleRecommendationRunDescription_dataSource,
    dataQualityRuleRecommendationRunDescription_runId,
    dataQualityRuleRecommendationRunDescription_startedOn,
    dataQualityRuleRecommendationRunDescription_status,

    -- ** DataQualityRuleRecommendationRunFilter
    dataQualityRuleRecommendationRunFilter_startedAfter,
    dataQualityRuleRecommendationRunFilter_startedBefore,
    dataQualityRuleRecommendationRunFilter_dataSource,

    -- ** DataQualityRuleResult
    dataQualityRuleResult_description,
    dataQualityRuleResult_evaluationMessage,
    dataQualityRuleResult_name,
    dataQualityRuleResult_result,

    -- ** DataQualityRulesetEvaluationRunDescription
    dataQualityRulesetEvaluationRunDescription_dataSource,
    dataQualityRulesetEvaluationRunDescription_runId,
    dataQualityRulesetEvaluationRunDescription_startedOn,
    dataQualityRulesetEvaluationRunDescription_status,

    -- ** DataQualityRulesetEvaluationRunFilter
    dataQualityRulesetEvaluationRunFilter_startedAfter,
    dataQualityRulesetEvaluationRunFilter_startedBefore,
    dataQualityRulesetEvaluationRunFilter_dataSource,

    -- ** DataQualityRulesetFilterCriteria
    dataQualityRulesetFilterCriteria_createdAfter,
    dataQualityRulesetFilterCriteria_createdBefore,
    dataQualityRulesetFilterCriteria_description,
    dataQualityRulesetFilterCriteria_lastModifiedAfter,
    dataQualityRulesetFilterCriteria_lastModifiedBefore,
    dataQualityRulesetFilterCriteria_name,
    dataQualityRulesetFilterCriteria_targetTable,

    -- ** DataQualityRulesetListDetails
    dataQualityRulesetListDetails_createdOn,
    dataQualityRulesetListDetails_description,
    dataQualityRulesetListDetails_lastModifiedOn,
    dataQualityRulesetListDetails_name,
    dataQualityRulesetListDetails_recommendationRunId,
    dataQualityRulesetListDetails_ruleCount,
    dataQualityRulesetListDetails_targetTable,

    -- ** DataQualityTargetTable
    dataQualityTargetTable_tableName,
    dataQualityTargetTable_databaseName,

    -- ** DataSource
    dataSource_glueTable,

    -- ** Database
    database_catalogId,
    database_createTableDefaultPermissions,
    database_createTime,
    database_description,
    database_locationUri,
    database_parameters,
    database_targetDatabase,
    database_name,

    -- ** DatabaseIdentifier
    databaseIdentifier_catalogId,
    databaseIdentifier_databaseName,

    -- ** DatabaseInput
    databaseInput_createTableDefaultPermissions,
    databaseInput_description,
    databaseInput_locationUri,
    databaseInput_parameters,
    databaseInput_targetDatabase,
    databaseInput_name,

    -- ** Datatype
    datatype_id,
    datatype_label,

    -- ** DateColumnStatisticsData
    dateColumnStatisticsData_maximumValue,
    dateColumnStatisticsData_minimumValue,
    dateColumnStatisticsData_numberOfNulls,
    dateColumnStatisticsData_numberOfDistinctValues,

    -- ** DecimalColumnStatisticsData
    decimalColumnStatisticsData_maximumValue,
    decimalColumnStatisticsData_minimumValue,
    decimalColumnStatisticsData_numberOfNulls,
    decimalColumnStatisticsData_numberOfDistinctValues,

    -- ** DecimalNumber
    decimalNumber_unscaledValue,
    decimalNumber_scale,

    -- ** DeltaTarget
    deltaTarget_connectionName,
    deltaTarget_createNativeDeltaTable,
    deltaTarget_deltaTables,
    deltaTarget_writeManifest,

    -- ** DevEndpoint
    devEndpoint_arguments,
    devEndpoint_availabilityZone,
    devEndpoint_createdTimestamp,
    devEndpoint_endpointName,
    devEndpoint_extraJarsS3Path,
    devEndpoint_extraPythonLibsS3Path,
    devEndpoint_failureReason,
    devEndpoint_glueVersion,
    devEndpoint_lastModifiedTimestamp,
    devEndpoint_lastUpdateStatus,
    devEndpoint_numberOfNodes,
    devEndpoint_numberOfWorkers,
    devEndpoint_privateAddress,
    devEndpoint_publicAddress,
    devEndpoint_publicKey,
    devEndpoint_publicKeys,
    devEndpoint_roleArn,
    devEndpoint_securityConfiguration,
    devEndpoint_securityGroupIds,
    devEndpoint_status,
    devEndpoint_subnetId,
    devEndpoint_vpcId,
    devEndpoint_workerType,
    devEndpoint_yarnEndpointAddress,
    devEndpoint_zeppelinRemoteSparkInterpreterPort,

    -- ** DevEndpointCustomLibraries
    devEndpointCustomLibraries_extraJarsS3Path,
    devEndpointCustomLibraries_extraPythonLibsS3Path,

    -- ** DirectKafkaSource
    directKafkaSource_dataPreviewOptions,
    directKafkaSource_detectSchema,
    directKafkaSource_streamingOptions,
    directKafkaSource_windowSize,
    directKafkaSource_name,

    -- ** DirectKinesisSource
    directKinesisSource_dataPreviewOptions,
    directKinesisSource_detectSchema,
    directKinesisSource_streamingOptions,
    directKinesisSource_windowSize,
    directKinesisSource_name,

    -- ** DirectSchemaChangePolicy
    directSchemaChangePolicy_database,
    directSchemaChangePolicy_enableUpdateCatalog,
    directSchemaChangePolicy_table,
    directSchemaChangePolicy_updateBehavior,

    -- ** DoubleColumnStatisticsData
    doubleColumnStatisticsData_maximumValue,
    doubleColumnStatisticsData_minimumValue,
    doubleColumnStatisticsData_numberOfNulls,
    doubleColumnStatisticsData_numberOfDistinctValues,

    -- ** DropDuplicates
    dropDuplicates_columns,
    dropDuplicates_name,
    dropDuplicates_inputs,

    -- ** DropFields
    dropFields_name,
    dropFields_inputs,
    dropFields_paths,

    -- ** DropNullFields
    dropNullFields_nullCheckBoxList,
    dropNullFields_nullTextList,
    dropNullFields_name,
    dropNullFields_inputs,

    -- ** DynamicTransform
    dynamicTransform_parameters,
    dynamicTransform_version,
    dynamicTransform_name,
    dynamicTransform_transformName,
    dynamicTransform_inputs,
    dynamicTransform_functionName,
    dynamicTransform_path,

    -- ** DynamoDBCatalogSource
    dynamoDBCatalogSource_name,
    dynamoDBCatalogSource_database,
    dynamoDBCatalogSource_table,

    -- ** DynamoDBTarget
    dynamoDBTarget_path,
    dynamoDBTarget_scanAll,
    dynamoDBTarget_scanRate,

    -- ** Edge
    edge_destinationId,
    edge_sourceId,

    -- ** EncryptionAtRest
    encryptionAtRest_sseAwsKmsKeyId,
    encryptionAtRest_catalogEncryptionMode,

    -- ** EncryptionConfiguration
    encryptionConfiguration_cloudWatchEncryption,
    encryptionConfiguration_jobBookmarksEncryption,
    encryptionConfiguration_s3Encryption,

    -- ** ErrorDetail
    errorDetail_errorCode,
    errorDetail_errorMessage,

    -- ** ErrorDetails
    errorDetails_errorCode,
    errorDetails_errorMessage,

    -- ** EvaluateDataQuality
    evaluateDataQuality_output,
    evaluateDataQuality_publishingOptions,
    evaluateDataQuality_stopJobOnFailureOptions,
    evaluateDataQuality_name,
    evaluateDataQuality_inputs,
    evaluateDataQuality_ruleset,

    -- ** EvaluationMetrics
    evaluationMetrics_findMatchesMetrics,
    evaluationMetrics_transformType,

    -- ** EventBatchingCondition
    eventBatchingCondition_batchWindow,
    eventBatchingCondition_batchSize,

    -- ** ExecutionProperty
    executionProperty_maxConcurrentRuns,

    -- ** ExportLabelsTaskRunProperties
    exportLabelsTaskRunProperties_outputS3Path,

    -- ** FillMissingValues
    fillMissingValues_filledPath,
    fillMissingValues_name,
    fillMissingValues_inputs,
    fillMissingValues_imputedPath,

    -- ** Filter
    filter_name,
    filter_inputs,
    filter_logicalOperator,
    filter_filters,

    -- ** FilterExpression
    filterExpression_negated,
    filterExpression_operation,
    filterExpression_values,

    -- ** FilterValue
    filterValue_type,
    filterValue_value,

    -- ** FindMatchesMetrics
    findMatchesMetrics_areaUnderPRCurve,
    findMatchesMetrics_columnImportances,
    findMatchesMetrics_confusionMatrix,
    findMatchesMetrics_f1,
    findMatchesMetrics_precision,
    findMatchesMetrics_recall,

    -- ** FindMatchesParameters
    findMatchesParameters_accuracyCostTradeoff,
    findMatchesParameters_enforceProvidedLabels,
    findMatchesParameters_precisionRecallTradeoff,
    findMatchesParameters_primaryKeyColumnName,

    -- ** FindMatchesTaskRunProperties
    findMatchesTaskRunProperties_jobId,
    findMatchesTaskRunProperties_jobName,
    findMatchesTaskRunProperties_jobRunId,

    -- ** GetConnectionsFilter
    getConnectionsFilter_connectionType,
    getConnectionsFilter_matchCriteria,

    -- ** GluePolicy
    gluePolicy_createTime,
    gluePolicy_policyHash,
    gluePolicy_policyInJson,
    gluePolicy_updateTime,

    -- ** GlueSchema
    glueSchema_columns,

    -- ** GlueStudioSchemaColumn
    glueStudioSchemaColumn_type,
    glueStudioSchemaColumn_name,

    -- ** GlueTable
    glueTable_additionalOptions,
    glueTable_catalogId,
    glueTable_connectionName,
    glueTable_databaseName,
    glueTable_tableName,

    -- ** GovernedCatalogSource
    governedCatalogSource_additionalOptions,
    governedCatalogSource_partitionPredicate,
    governedCatalogSource_name,
    governedCatalogSource_database,
    governedCatalogSource_table,

    -- ** GovernedCatalogTarget
    governedCatalogTarget_partitionKeys,
    governedCatalogTarget_schemaChangePolicy,
    governedCatalogTarget_name,
    governedCatalogTarget_inputs,
    governedCatalogTarget_table,
    governedCatalogTarget_database,

    -- ** GrokClassifier
    grokClassifier_creationTime,
    grokClassifier_customPatterns,
    grokClassifier_lastUpdated,
    grokClassifier_version,
    grokClassifier_name,
    grokClassifier_classification,
    grokClassifier_grokPattern,

    -- ** ImportLabelsTaskRunProperties
    importLabelsTaskRunProperties_inputS3Path,
    importLabelsTaskRunProperties_replace,

    -- ** JDBCConnectorOptions
    jDBCConnectorOptions_dataTypeMapping,
    jDBCConnectorOptions_filterPredicate,
    jDBCConnectorOptions_jobBookmarkKeys,
    jDBCConnectorOptions_jobBookmarkKeysSortOrder,
    jDBCConnectorOptions_lowerBound,
    jDBCConnectorOptions_numPartitions,
    jDBCConnectorOptions_partitionColumn,
    jDBCConnectorOptions_upperBound,

    -- ** JDBCConnectorSource
    jDBCConnectorSource_additionalOptions,
    jDBCConnectorSource_connectionTable,
    jDBCConnectorSource_outputSchemas,
    jDBCConnectorSource_query,
    jDBCConnectorSource_name,
    jDBCConnectorSource_connectionName,
    jDBCConnectorSource_connectorName,
    jDBCConnectorSource_connectionType,

    -- ** JDBCConnectorTarget
    jDBCConnectorTarget_additionalOptions,
    jDBCConnectorTarget_outputSchemas,
    jDBCConnectorTarget_name,
    jDBCConnectorTarget_inputs,
    jDBCConnectorTarget_connectionName,
    jDBCConnectorTarget_connectionTable,
    jDBCConnectorTarget_connectorName,
    jDBCConnectorTarget_connectionType,

    -- ** JdbcTarget
    jdbcTarget_connectionName,
    jdbcTarget_enableAdditionalMetadata,
    jdbcTarget_exclusions,
    jdbcTarget_path,

    -- ** Job
    job_allocatedCapacity,
    job_codeGenConfigurationNodes,
    job_command,
    job_connections,
    job_createdOn,
    job_defaultArguments,
    job_description,
    job_executionClass,
    job_executionProperty,
    job_glueVersion,
    job_lastModifiedOn,
    job_logUri,
    job_maxCapacity,
    job_maxRetries,
    job_name,
    job_nonOverridableArguments,
    job_notificationProperty,
    job_numberOfWorkers,
    job_role,
    job_securityConfiguration,
    job_sourceControlDetails,
    job_timeout,
    job_workerType,

    -- ** JobBookmarkEntry
    jobBookmarkEntry_attempt,
    jobBookmarkEntry_jobBookmark,
    jobBookmarkEntry_jobName,
    jobBookmarkEntry_previousRunId,
    jobBookmarkEntry_run,
    jobBookmarkEntry_runId,
    jobBookmarkEntry_version,

    -- ** JobBookmarksEncryption
    jobBookmarksEncryption_jobBookmarksEncryptionMode,
    jobBookmarksEncryption_kmsKeyArn,

    -- ** JobCommand
    jobCommand_name,
    jobCommand_pythonVersion,
    jobCommand_scriptLocation,

    -- ** JobNodeDetails
    jobNodeDetails_jobRuns,

    -- ** JobRun
    jobRun_allocatedCapacity,
    jobRun_arguments,
    jobRun_attempt,
    jobRun_completedOn,
    jobRun_dPUSeconds,
    jobRun_errorMessage,
    jobRun_executionClass,
    jobRun_executionTime,
    jobRun_glueVersion,
    jobRun_id,
    jobRun_jobName,
    jobRun_jobRunState,
    jobRun_lastModifiedOn,
    jobRun_logGroupName,
    jobRun_maxCapacity,
    jobRun_notificationProperty,
    jobRun_numberOfWorkers,
    jobRun_predecessorRuns,
    jobRun_previousRunId,
    jobRun_securityConfiguration,
    jobRun_startedOn,
    jobRun_timeout,
    jobRun_triggerName,
    jobRun_workerType,

    -- ** JobUpdate
    jobUpdate_allocatedCapacity,
    jobUpdate_codeGenConfigurationNodes,
    jobUpdate_command,
    jobUpdate_connections,
    jobUpdate_defaultArguments,
    jobUpdate_description,
    jobUpdate_executionClass,
    jobUpdate_executionProperty,
    jobUpdate_glueVersion,
    jobUpdate_logUri,
    jobUpdate_maxCapacity,
    jobUpdate_maxRetries,
    jobUpdate_nonOverridableArguments,
    jobUpdate_notificationProperty,
    jobUpdate_numberOfWorkers,
    jobUpdate_role,
    jobUpdate_securityConfiguration,
    jobUpdate_sourceControlDetails,
    jobUpdate_timeout,
    jobUpdate_workerType,

    -- ** Join
    join_name,
    join_inputs,
    join_joinType,
    join_columns,

    -- ** JoinColumn
    joinColumn_from,
    joinColumn_keys,

    -- ** JsonClassifier
    jsonClassifier_creationTime,
    jsonClassifier_lastUpdated,
    jsonClassifier_version,
    jsonClassifier_name,
    jsonClassifier_jsonPath,

    -- ** KafkaStreamingSourceOptions
    kafkaStreamingSourceOptions_assign,
    kafkaStreamingSourceOptions_bootstrapServers,
    kafkaStreamingSourceOptions_classification,
    kafkaStreamingSourceOptions_connectionName,
    kafkaStreamingSourceOptions_delimiter,
    kafkaStreamingSourceOptions_endingOffsets,
    kafkaStreamingSourceOptions_maxOffsetsPerTrigger,
    kafkaStreamingSourceOptions_minPartitions,
    kafkaStreamingSourceOptions_numRetries,
    kafkaStreamingSourceOptions_pollTimeoutMs,
    kafkaStreamingSourceOptions_retryIntervalMs,
    kafkaStreamingSourceOptions_securityProtocol,
    kafkaStreamingSourceOptions_startingOffsets,
    kafkaStreamingSourceOptions_subscribePattern,
    kafkaStreamingSourceOptions_topicName,

    -- ** KeySchemaElement
    keySchemaElement_name,
    keySchemaElement_type,

    -- ** KinesisStreamingSourceOptions
    kinesisStreamingSourceOptions_addIdleTimeBetweenReads,
    kinesisStreamingSourceOptions_avoidEmptyBatches,
    kinesisStreamingSourceOptions_classification,
    kinesisStreamingSourceOptions_delimiter,
    kinesisStreamingSourceOptions_describeShardInterval,
    kinesisStreamingSourceOptions_endpointUrl,
    kinesisStreamingSourceOptions_idleTimeBetweenReadsInMs,
    kinesisStreamingSourceOptions_maxFetchRecordsPerShard,
    kinesisStreamingSourceOptions_maxFetchTimeInMs,
    kinesisStreamingSourceOptions_maxRecordPerRead,
    kinesisStreamingSourceOptions_maxRetryIntervalMs,
    kinesisStreamingSourceOptions_numRetries,
    kinesisStreamingSourceOptions_retryIntervalMs,
    kinesisStreamingSourceOptions_roleArn,
    kinesisStreamingSourceOptions_roleSessionName,
    kinesisStreamingSourceOptions_startingPosition,
    kinesisStreamingSourceOptions_streamArn,
    kinesisStreamingSourceOptions_streamName,

    -- ** LabelingSetGenerationTaskRunProperties
    labelingSetGenerationTaskRunProperties_outputS3Path,

    -- ** LakeFormationConfiguration
    lakeFormationConfiguration_accountId,
    lakeFormationConfiguration_useLakeFormationCredentials,

    -- ** LastActiveDefinition
    lastActiveDefinition_blueprintLocation,
    lastActiveDefinition_blueprintServiceLocation,
    lastActiveDefinition_description,
    lastActiveDefinition_lastModifiedOn,
    lastActiveDefinition_parameterSpec,

    -- ** LastCrawlInfo
    lastCrawlInfo_errorMessage,
    lastCrawlInfo_logGroup,
    lastCrawlInfo_logStream,
    lastCrawlInfo_messagePrefix,
    lastCrawlInfo_startTime,
    lastCrawlInfo_status,

    -- ** LineageConfiguration
    lineageConfiguration_crawlerLineageSettings,

    -- ** Location
    location_dynamoDB,
    location_jdbc,
    location_s3,

    -- ** LongColumnStatisticsData
    longColumnStatisticsData_maximumValue,
    longColumnStatisticsData_minimumValue,
    longColumnStatisticsData_numberOfNulls,
    longColumnStatisticsData_numberOfDistinctValues,

    -- ** MLTransform
    mLTransform_createdOn,
    mLTransform_description,
    mLTransform_evaluationMetrics,
    mLTransform_glueVersion,
    mLTransform_inputRecordTables,
    mLTransform_labelCount,
    mLTransform_lastModifiedOn,
    mLTransform_maxCapacity,
    mLTransform_maxRetries,
    mLTransform_name,
    mLTransform_numberOfWorkers,
    mLTransform_parameters,
    mLTransform_role,
    mLTransform_schema,
    mLTransform_status,
    mLTransform_timeout,
    mLTransform_transformEncryption,
    mLTransform_transformId,
    mLTransform_workerType,

    -- ** MLUserDataEncryption
    mLUserDataEncryption_kmsKeyId,
    mLUserDataEncryption_mlUserDataEncryptionMode,

    -- ** Mapping
    mapping_children,
    mapping_dropped,
    mapping_fromPath,
    mapping_fromType,
    mapping_toKey,
    mapping_toType,

    -- ** MappingEntry
    mappingEntry_sourcePath,
    mappingEntry_sourceTable,
    mappingEntry_sourceType,
    mappingEntry_targetPath,
    mappingEntry_targetTable,
    mappingEntry_targetType,

    -- ** Merge
    merge_name,
    merge_inputs,
    merge_source,
    merge_primaryKeys,

    -- ** MetadataInfo
    metadataInfo_createdTime,
    metadataInfo_metadataValue,
    metadataInfo_otherMetadataValueList,

    -- ** MetadataKeyValuePair
    metadataKeyValuePair_metadataKey,
    metadataKeyValuePair_metadataValue,

    -- ** MicrosoftSQLServerCatalogSource
    microsoftSQLServerCatalogSource_name,
    microsoftSQLServerCatalogSource_database,
    microsoftSQLServerCatalogSource_table,

    -- ** MicrosoftSQLServerCatalogTarget
    microsoftSQLServerCatalogTarget_name,
    microsoftSQLServerCatalogTarget_inputs,
    microsoftSQLServerCatalogTarget_database,
    microsoftSQLServerCatalogTarget_table,

    -- ** MongoDBTarget
    mongoDBTarget_connectionName,
    mongoDBTarget_path,
    mongoDBTarget_scanAll,

    -- ** MySQLCatalogSource
    mySQLCatalogSource_name,
    mySQLCatalogSource_database,
    mySQLCatalogSource_table,

    -- ** MySQLCatalogTarget
    mySQLCatalogTarget_name,
    mySQLCatalogTarget_inputs,
    mySQLCatalogTarget_database,
    mySQLCatalogTarget_table,

    -- ** Node
    node_crawlerDetails,
    node_jobDetails,
    node_name,
    node_triggerDetails,
    node_type,
    node_uniqueId,

    -- ** NotificationProperty
    notificationProperty_notifyDelayAfter,

    -- ** NullCheckBoxList
    nullCheckBoxList_isEmpty,
    nullCheckBoxList_isNegOne,
    nullCheckBoxList_isNullString,

    -- ** NullValueField
    nullValueField_value,
    nullValueField_datatype,

    -- ** OracleSQLCatalogSource
    oracleSQLCatalogSource_name,
    oracleSQLCatalogSource_database,
    oracleSQLCatalogSource_table,

    -- ** OracleSQLCatalogTarget
    oracleSQLCatalogTarget_name,
    oracleSQLCatalogTarget_inputs,
    oracleSQLCatalogTarget_database,
    oracleSQLCatalogTarget_table,

    -- ** Order
    order_column,
    order_sortOrder,

    -- ** OtherMetadataValueListItem
    otherMetadataValueListItem_createdTime,
    otherMetadataValueListItem_metadataValue,

    -- ** PIIDetection
    pIIDetection_maskValue,
    pIIDetection_outputColumnName,
    pIIDetection_sampleFraction,
    pIIDetection_thresholdFraction,
    pIIDetection_name,
    pIIDetection_inputs,
    pIIDetection_piiType,
    pIIDetection_entityTypesToDetect,

    -- ** Partition
    partition_catalogId,
    partition_creationTime,
    partition_databaseName,
    partition_lastAccessTime,
    partition_lastAnalyzedTime,
    partition_parameters,
    partition_storageDescriptor,
    partition_tableName,
    partition_values,

    -- ** PartitionError
    partitionError_errorDetail,
    partitionError_partitionValues,

    -- ** PartitionIndex
    partitionIndex_keys,
    partitionIndex_indexName,

    -- ** PartitionIndexDescriptor
    partitionIndexDescriptor_backfillErrors,
    partitionIndexDescriptor_indexName,
    partitionIndexDescriptor_keys,
    partitionIndexDescriptor_indexStatus,

    -- ** PartitionInput
    partitionInput_lastAccessTime,
    partitionInput_lastAnalyzedTime,
    partitionInput_parameters,
    partitionInput_storageDescriptor,
    partitionInput_values,

    -- ** PartitionValueList
    partitionValueList_values,

    -- ** PhysicalConnectionRequirements
    physicalConnectionRequirements_availabilityZone,
    physicalConnectionRequirements_securityGroupIdList,
    physicalConnectionRequirements_subnetId,

    -- ** PostgreSQLCatalogSource
    postgreSQLCatalogSource_name,
    postgreSQLCatalogSource_database,
    postgreSQLCatalogSource_table,

    -- ** PostgreSQLCatalogTarget
    postgreSQLCatalogTarget_name,
    postgreSQLCatalogTarget_inputs,
    postgreSQLCatalogTarget_database,
    postgreSQLCatalogTarget_table,

    -- ** Predecessor
    predecessor_jobName,
    predecessor_runId,

    -- ** Predicate
    predicate_conditions,
    predicate_logical,

    -- ** PrincipalPermissions
    principalPermissions_permissions,
    principalPermissions_principal,

    -- ** PropertyPredicate
    propertyPredicate_comparator,
    propertyPredicate_key,
    propertyPredicate_value,

    -- ** RecrawlPolicy
    recrawlPolicy_recrawlBehavior,

    -- ** RedshiftSource
    redshiftSource_redshiftTmpDir,
    redshiftSource_tmpDirIAMRole,
    redshiftSource_name,
    redshiftSource_database,
    redshiftSource_table,

    -- ** RedshiftTarget
    redshiftTarget_redshiftTmpDir,
    redshiftTarget_tmpDirIAMRole,
    redshiftTarget_upsertRedshiftOptions,
    redshiftTarget_name,
    redshiftTarget_inputs,
    redshiftTarget_database,
    redshiftTarget_table,

    -- ** RegistryId
    registryId_registryArn,
    registryId_registryName,

    -- ** RegistryListItem
    registryListItem_createdTime,
    registryListItem_description,
    registryListItem_registryArn,
    registryListItem_registryName,
    registryListItem_status,
    registryListItem_updatedTime,

    -- ** RelationalCatalogSource
    relationalCatalogSource_name,
    relationalCatalogSource_database,
    relationalCatalogSource_table,

    -- ** RenameField
    renameField_name,
    renameField_inputs,
    renameField_sourcePath,
    renameField_targetPath,

    -- ** ResourceUri
    resourceUri_resourceType,
    resourceUri_uri,

    -- ** S3CatalogSource
    s3CatalogSource_additionalOptions,
    s3CatalogSource_partitionPredicate,
    s3CatalogSource_name,
    s3CatalogSource_database,
    s3CatalogSource_table,

    -- ** S3CatalogTarget
    s3CatalogTarget_partitionKeys,
    s3CatalogTarget_schemaChangePolicy,
    s3CatalogTarget_name,
    s3CatalogTarget_inputs,
    s3CatalogTarget_table,
    s3CatalogTarget_database,

    -- ** S3CsvSource
    s3CsvSource_additionalOptions,
    s3CsvSource_compressionType,
    s3CsvSource_escaper,
    s3CsvSource_exclusions,
    s3CsvSource_groupFiles,
    s3CsvSource_groupSize,
    s3CsvSource_maxBand,
    s3CsvSource_maxFilesInBand,
    s3CsvSource_multiline,
    s3CsvSource_optimizePerformance,
    s3CsvSource_outputSchemas,
    s3CsvSource_recurse,
    s3CsvSource_skipFirst,
    s3CsvSource_withHeader,
    s3CsvSource_writeHeader,
    s3CsvSource_name,
    s3CsvSource_paths,
    s3CsvSource_separator,
    s3CsvSource_quoteChar,

    -- ** S3DirectSourceAdditionalOptions
    s3DirectSourceAdditionalOptions_boundedFiles,
    s3DirectSourceAdditionalOptions_boundedSize,
    s3DirectSourceAdditionalOptions_enableSamplePath,
    s3DirectSourceAdditionalOptions_samplePath,

    -- ** S3DirectTarget
    s3DirectTarget_compression,
    s3DirectTarget_partitionKeys,
    s3DirectTarget_schemaChangePolicy,
    s3DirectTarget_name,
    s3DirectTarget_inputs,
    s3DirectTarget_path,
    s3DirectTarget_format,

    -- ** S3Encryption
    s3Encryption_kmsKeyArn,
    s3Encryption_s3EncryptionMode,

    -- ** S3GlueParquetTarget
    s3GlueParquetTarget_compression,
    s3GlueParquetTarget_partitionKeys,
    s3GlueParquetTarget_schemaChangePolicy,
    s3GlueParquetTarget_name,
    s3GlueParquetTarget_inputs,
    s3GlueParquetTarget_path,

    -- ** S3JsonSource
    s3JsonSource_additionalOptions,
    s3JsonSource_compressionType,
    s3JsonSource_exclusions,
    s3JsonSource_groupFiles,
    s3JsonSource_groupSize,
    s3JsonSource_jsonPath,
    s3JsonSource_maxBand,
    s3JsonSource_maxFilesInBand,
    s3JsonSource_multiline,
    s3JsonSource_outputSchemas,
    s3JsonSource_recurse,
    s3JsonSource_name,
    s3JsonSource_paths,

    -- ** S3ParquetSource
    s3ParquetSource_additionalOptions,
    s3ParquetSource_compressionType,
    s3ParquetSource_exclusions,
    s3ParquetSource_groupFiles,
    s3ParquetSource_groupSize,
    s3ParquetSource_maxBand,
    s3ParquetSource_maxFilesInBand,
    s3ParquetSource_outputSchemas,
    s3ParquetSource_recurse,
    s3ParquetSource_name,
    s3ParquetSource_paths,

    -- ** S3SourceAdditionalOptions
    s3SourceAdditionalOptions_boundedFiles,
    s3SourceAdditionalOptions_boundedSize,

    -- ** S3Target
    s3Target_connectionName,
    s3Target_dlqEventQueueArn,
    s3Target_eventQueueArn,
    s3Target_exclusions,
    s3Target_path,
    s3Target_sampleSize,

    -- ** Schedule
    schedule_scheduleExpression,
    schedule_state,

    -- ** SchemaChangePolicy
    schemaChangePolicy_deleteBehavior,
    schemaChangePolicy_updateBehavior,

    -- ** SchemaColumn
    schemaColumn_dataType,
    schemaColumn_name,

    -- ** SchemaId
    schemaId_registryName,
    schemaId_schemaArn,
    schemaId_schemaName,

    -- ** SchemaListItem
    schemaListItem_createdTime,
    schemaListItem_description,
    schemaListItem_registryName,
    schemaListItem_schemaArn,
    schemaListItem_schemaName,
    schemaListItem_schemaStatus,
    schemaListItem_updatedTime,

    -- ** SchemaReference
    schemaReference_schemaId,
    schemaReference_schemaVersionId,
    schemaReference_schemaVersionNumber,

    -- ** SchemaVersionErrorItem
    schemaVersionErrorItem_errorDetails,
    schemaVersionErrorItem_versionNumber,

    -- ** SchemaVersionListItem
    schemaVersionListItem_createdTime,
    schemaVersionListItem_schemaArn,
    schemaVersionListItem_schemaVersionId,
    schemaVersionListItem_status,
    schemaVersionListItem_versionNumber,

    -- ** SchemaVersionNumber
    schemaVersionNumber_latestVersion,
    schemaVersionNumber_versionNumber,

    -- ** SecurityConfiguration
    securityConfiguration_createdTimeStamp,
    securityConfiguration_encryptionConfiguration,
    securityConfiguration_name,

    -- ** Segment
    segment_segmentNumber,
    segment_totalSegments,

    -- ** SelectFields
    selectFields_name,
    selectFields_inputs,
    selectFields_paths,

    -- ** SelectFromCollection
    selectFromCollection_name,
    selectFromCollection_inputs,
    selectFromCollection_index,

    -- ** SerDeInfo
    serDeInfo_name,
    serDeInfo_parameters,
    serDeInfo_serializationLibrary,

    -- ** Session
    session_command,
    session_connections,
    session_createdOn,
    session_defaultArguments,
    session_description,
    session_errorMessage,
    session_glueVersion,
    session_id,
    session_maxCapacity,
    session_progress,
    session_role,
    session_securityConfiguration,
    session_status,

    -- ** SessionCommand
    sessionCommand_name,
    sessionCommand_pythonVersion,

    -- ** SkewedInfo
    skewedInfo_skewedColumnNames,
    skewedInfo_skewedColumnValueLocationMaps,
    skewedInfo_skewedColumnValues,

    -- ** SortCriterion
    sortCriterion_fieldName,
    sortCriterion_sort,

    -- ** SourceControlDetails
    sourceControlDetails_authStrategy,
    sourceControlDetails_authToken,
    sourceControlDetails_branch,
    sourceControlDetails_folder,
    sourceControlDetails_lastCommitId,
    sourceControlDetails_owner,
    sourceControlDetails_provider,
    sourceControlDetails_repository,

    -- ** SparkConnectorSource
    sparkConnectorSource_additionalOptions,
    sparkConnectorSource_outputSchemas,
    sparkConnectorSource_name,
    sparkConnectorSource_connectionName,
    sparkConnectorSource_connectorName,
    sparkConnectorSource_connectionType,

    -- ** SparkConnectorTarget
    sparkConnectorTarget_additionalOptions,
    sparkConnectorTarget_outputSchemas,
    sparkConnectorTarget_name,
    sparkConnectorTarget_inputs,
    sparkConnectorTarget_connectionName,
    sparkConnectorTarget_connectorName,
    sparkConnectorTarget_connectionType,

    -- ** SparkSQL
    sparkSQL_outputSchemas,
    sparkSQL_name,
    sparkSQL_inputs,
    sparkSQL_sqlQuery,
    sparkSQL_sqlAliases,

    -- ** Spigot
    spigot_prob,
    spigot_topk,
    spigot_name,
    spigot_inputs,
    spigot_path,

    -- ** SplitFields
    splitFields_name,
    splitFields_inputs,
    splitFields_paths,

    -- ** SqlAlias
    sqlAlias_from,
    sqlAlias_alias,

    -- ** StartingEventBatchCondition
    startingEventBatchCondition_batchSize,
    startingEventBatchCondition_batchWindow,

    -- ** Statement
    statement_code,
    statement_completedOn,
    statement_id,
    statement_output,
    statement_progress,
    statement_startedOn,
    statement_state,

    -- ** StatementOutput
    statementOutput_data,
    statementOutput_errorName,
    statementOutput_errorValue,
    statementOutput_executionCount,
    statementOutput_status,
    statementOutput_traceback,

    -- ** StatementOutputData
    statementOutputData_textPlain,

    -- ** StorageDescriptor
    storageDescriptor_additionalLocations,
    storageDescriptor_bucketColumns,
    storageDescriptor_columns,
    storageDescriptor_compressed,
    storageDescriptor_inputFormat,
    storageDescriptor_location,
    storageDescriptor_numberOfBuckets,
    storageDescriptor_outputFormat,
    storageDescriptor_parameters,
    storageDescriptor_schemaReference,
    storageDescriptor_serdeInfo,
    storageDescriptor_skewedInfo,
    storageDescriptor_sortColumns,
    storageDescriptor_storedAsSubDirectories,

    -- ** StreamingDataPreviewOptions
    streamingDataPreviewOptions_pollingTime,
    streamingDataPreviewOptions_recordPollingLimit,

    -- ** StringColumnStatisticsData
    stringColumnStatisticsData_maximumLength,
    stringColumnStatisticsData_averageLength,
    stringColumnStatisticsData_numberOfNulls,
    stringColumnStatisticsData_numberOfDistinctValues,

    -- ** Table
    table_catalogId,
    table_createTime,
    table_createdBy,
    table_databaseName,
    table_description,
    table_isRegisteredWithLakeFormation,
    table_lastAccessTime,
    table_lastAnalyzedTime,
    table_owner,
    table_parameters,
    table_partitionKeys,
    table_retention,
    table_storageDescriptor,
    table_tableType,
    table_targetTable,
    table_updateTime,
    table_versionId,
    table_viewExpandedText,
    table_viewOriginalText,
    table_name,

    -- ** TableError
    tableError_errorDetail,
    tableError_tableName,

    -- ** TableIdentifier
    tableIdentifier_catalogId,
    tableIdentifier_databaseName,
    tableIdentifier_name,

    -- ** TableInput
    tableInput_description,
    tableInput_lastAccessTime,
    tableInput_lastAnalyzedTime,
    tableInput_owner,
    tableInput_parameters,
    tableInput_partitionKeys,
    tableInput_retention,
    tableInput_storageDescriptor,
    tableInput_tableType,
    tableInput_targetTable,
    tableInput_viewExpandedText,
    tableInput_viewOriginalText,
    tableInput_name,

    -- ** TableVersion
    tableVersion_table,
    tableVersion_versionId,

    -- ** TableVersionError
    tableVersionError_errorDetail,
    tableVersionError_tableName,
    tableVersionError_versionId,

    -- ** TaskRun
    taskRun_completedOn,
    taskRun_errorString,
    taskRun_executionTime,
    taskRun_lastModifiedOn,
    taskRun_logGroupName,
    taskRun_properties,
    taskRun_startedOn,
    taskRun_status,
    taskRun_taskRunId,
    taskRun_transformId,

    -- ** TaskRunFilterCriteria
    taskRunFilterCriteria_startedAfter,
    taskRunFilterCriteria_startedBefore,
    taskRunFilterCriteria_status,
    taskRunFilterCriteria_taskRunType,

    -- ** TaskRunProperties
    taskRunProperties_exportLabelsTaskRunProperties,
    taskRunProperties_findMatchesTaskRunProperties,
    taskRunProperties_importLabelsTaskRunProperties,
    taskRunProperties_labelingSetGenerationTaskRunProperties,
    taskRunProperties_taskType,

    -- ** TaskRunSortCriteria
    taskRunSortCriteria_column,
    taskRunSortCriteria_sortDirection,

    -- ** TransformConfigParameter
    transformConfigParameter_isOptional,
    transformConfigParameter_listType,
    transformConfigParameter_validationMessage,
    transformConfigParameter_validationRule,
    transformConfigParameter_value,
    transformConfigParameter_name,
    transformConfigParameter_type,

    -- ** TransformEncryption
    transformEncryption_mlUserDataEncryption,
    transformEncryption_taskRunSecurityConfigurationName,

    -- ** TransformFilterCriteria
    transformFilterCriteria_createdAfter,
    transformFilterCriteria_createdBefore,
    transformFilterCriteria_glueVersion,
    transformFilterCriteria_lastModifiedAfter,
    transformFilterCriteria_lastModifiedBefore,
    transformFilterCriteria_name,
    transformFilterCriteria_schema,
    transformFilterCriteria_status,
    transformFilterCriteria_transformType,

    -- ** TransformParameters
    transformParameters_findMatchesParameters,
    transformParameters_transformType,

    -- ** TransformSortCriteria
    transformSortCriteria_column,
    transformSortCriteria_sortDirection,

    -- ** Trigger
    trigger_actions,
    trigger_description,
    trigger_eventBatchingCondition,
    trigger_id,
    trigger_name,
    trigger_predicate,
    trigger_schedule,
    trigger_state,
    trigger_type,
    trigger_workflowName,

    -- ** TriggerNodeDetails
    triggerNodeDetails_trigger,

    -- ** TriggerUpdate
    triggerUpdate_actions,
    triggerUpdate_description,
    triggerUpdate_eventBatchingCondition,
    triggerUpdate_name,
    triggerUpdate_predicate,
    triggerUpdate_schedule,

    -- ** UnfilteredPartition
    unfilteredPartition_authorizedColumns,
    unfilteredPartition_isRegisteredWithLakeFormation,
    unfilteredPartition_partition,

    -- ** Union
    union_name,
    union_inputs,
    union_unionType,

    -- ** UpdateCsvClassifierRequest
    updateCsvClassifierRequest_allowSingleColumn,
    updateCsvClassifierRequest_containsHeader,
    updateCsvClassifierRequest_customDatatypeConfigured,
    updateCsvClassifierRequest_customDatatypes,
    updateCsvClassifierRequest_delimiter,
    updateCsvClassifierRequest_disableValueTrimming,
    updateCsvClassifierRequest_header,
    updateCsvClassifierRequest_quoteSymbol,
    updateCsvClassifierRequest_name,

    -- ** UpdateGrokClassifierRequest
    updateGrokClassifierRequest_classification,
    updateGrokClassifierRequest_customPatterns,
    updateGrokClassifierRequest_grokPattern,
    updateGrokClassifierRequest_name,

    -- ** UpdateJsonClassifierRequest
    updateJsonClassifierRequest_jsonPath,
    updateJsonClassifierRequest_name,

    -- ** UpdateXMLClassifierRequest
    updateXMLClassifierRequest_classification,
    updateXMLClassifierRequest_rowTag,
    updateXMLClassifierRequest_name,

    -- ** UpsertRedshiftTargetOptions
    upsertRedshiftTargetOptions_connectionName,
    upsertRedshiftTargetOptions_tableLocation,
    upsertRedshiftTargetOptions_upsertKeys,

    -- ** UserDefinedFunction
    userDefinedFunction_catalogId,
    userDefinedFunction_className,
    userDefinedFunction_createTime,
    userDefinedFunction_databaseName,
    userDefinedFunction_functionName,
    userDefinedFunction_ownerName,
    userDefinedFunction_ownerType,
    userDefinedFunction_resourceUris,

    -- ** UserDefinedFunctionInput
    userDefinedFunctionInput_className,
    userDefinedFunctionInput_functionName,
    userDefinedFunctionInput_ownerName,
    userDefinedFunctionInput_ownerType,
    userDefinedFunctionInput_resourceUris,

    -- ** Workflow
    workflow_blueprintDetails,
    workflow_createdOn,
    workflow_defaultRunProperties,
    workflow_description,
    workflow_graph,
    workflow_lastModifiedOn,
    workflow_lastRun,
    workflow_maxConcurrentRuns,
    workflow_name,

    -- ** WorkflowGraph
    workflowGraph_edges,
    workflowGraph_nodes,

    -- ** WorkflowRun
    workflowRun_completedOn,
    workflowRun_errorMessage,
    workflowRun_graph,
    workflowRun_name,
    workflowRun_previousRunId,
    workflowRun_startedOn,
    workflowRun_startingEventBatchCondition,
    workflowRun_statistics,
    workflowRun_status,
    workflowRun_workflowRunId,
    workflowRun_workflowRunProperties,

    -- ** WorkflowRunStatistics
    workflowRunStatistics_erroredActions,
    workflowRunStatistics_failedActions,
    workflowRunStatistics_runningActions,
    workflowRunStatistics_stoppedActions,
    workflowRunStatistics_succeededActions,
    workflowRunStatistics_timeoutActions,
    workflowRunStatistics_totalActions,
    workflowRunStatistics_waitingActions,

    -- ** XMLClassifier
    xMLClassifier_creationTime,
    xMLClassifier_lastUpdated,
    xMLClassifier_rowTag,
    xMLClassifier_version,
    xMLClassifier_name,
    xMLClassifier_classification,
  )
where

import Amazonka.Glue.BatchCreatePartition
import Amazonka.Glue.BatchDeleteConnection
import Amazonka.Glue.BatchDeletePartition
import Amazonka.Glue.BatchDeleteTable
import Amazonka.Glue.BatchDeleteTableVersion
import Amazonka.Glue.BatchGetBlueprints
import Amazonka.Glue.BatchGetCrawlers
import Amazonka.Glue.BatchGetCustomEntityTypes
import Amazonka.Glue.BatchGetDataQualityResult
import Amazonka.Glue.BatchGetDevEndpoints
import Amazonka.Glue.BatchGetJobs
import Amazonka.Glue.BatchGetPartition
import Amazonka.Glue.BatchGetTriggers
import Amazonka.Glue.BatchGetWorkflows
import Amazonka.Glue.BatchStopJobRun
import Amazonka.Glue.BatchUpdatePartition
import Amazonka.Glue.CancelDataQualityRuleRecommendationRun
import Amazonka.Glue.CancelDataQualityRulesetEvaluationRun
import Amazonka.Glue.CancelMLTaskRun
import Amazonka.Glue.CancelStatement
import Amazonka.Glue.CheckSchemaVersionValidity
import Amazonka.Glue.CreateBlueprint
import Amazonka.Glue.CreateClassifier
import Amazonka.Glue.CreateConnection
import Amazonka.Glue.CreateCrawler
import Amazonka.Glue.CreateCustomEntityType
import Amazonka.Glue.CreateDataQualityRuleset
import Amazonka.Glue.CreateDatabase
import Amazonka.Glue.CreateDevEndpoint
import Amazonka.Glue.CreateJob
import Amazonka.Glue.CreateMLTransform
import Amazonka.Glue.CreatePartition
import Amazonka.Glue.CreatePartitionIndex
import Amazonka.Glue.CreateRegistry
import Amazonka.Glue.CreateSchema
import Amazonka.Glue.CreateScript
import Amazonka.Glue.CreateSecurityConfiguration
import Amazonka.Glue.CreateSession
import Amazonka.Glue.CreateTable
import Amazonka.Glue.CreateTrigger
import Amazonka.Glue.CreateUserDefinedFunction
import Amazonka.Glue.CreateWorkflow
import Amazonka.Glue.DeleteBlueprint
import Amazonka.Glue.DeleteClassifier
import Amazonka.Glue.DeleteColumnStatisticsForPartition
import Amazonka.Glue.DeleteColumnStatisticsForTable
import Amazonka.Glue.DeleteConnection
import Amazonka.Glue.DeleteCrawler
import Amazonka.Glue.DeleteCustomEntityType
import Amazonka.Glue.DeleteDataQualityRuleset
import Amazonka.Glue.DeleteDatabase
import Amazonka.Glue.DeleteDevEndpoint
import Amazonka.Glue.DeleteJob
import Amazonka.Glue.DeleteMLTransform
import Amazonka.Glue.DeletePartition
import Amazonka.Glue.DeletePartitionIndex
import Amazonka.Glue.DeleteRegistry
import Amazonka.Glue.DeleteResourcePolicy
import Amazonka.Glue.DeleteSchema
import Amazonka.Glue.DeleteSchemaVersions
import Amazonka.Glue.DeleteSecurityConfiguration
import Amazonka.Glue.DeleteSession
import Amazonka.Glue.DeleteTable
import Amazonka.Glue.DeleteTableVersion
import Amazonka.Glue.DeleteTrigger
import Amazonka.Glue.DeleteUserDefinedFunction
import Amazonka.Glue.DeleteWorkflow
import Amazonka.Glue.GetBlueprint
import Amazonka.Glue.GetBlueprintRun
import Amazonka.Glue.GetBlueprintRuns
import Amazonka.Glue.GetCatalogImportStatus
import Amazonka.Glue.GetClassifier
import Amazonka.Glue.GetClassifiers
import Amazonka.Glue.GetColumnStatisticsForPartition
import Amazonka.Glue.GetColumnStatisticsForTable
import Amazonka.Glue.GetConnection
import Amazonka.Glue.GetConnections
import Amazonka.Glue.GetCrawler
import Amazonka.Glue.GetCrawlerMetrics
import Amazonka.Glue.GetCrawlers
import Amazonka.Glue.GetCustomEntityType
import Amazonka.Glue.GetDataCatalogEncryptionSettings
import Amazonka.Glue.GetDataQualityResult
import Amazonka.Glue.GetDataQualityRuleRecommendationRun
import Amazonka.Glue.GetDataQualityRuleset
import Amazonka.Glue.GetDataQualityRulesetEvaluationRun
import Amazonka.Glue.GetDatabase
import Amazonka.Glue.GetDatabases
import Amazonka.Glue.GetDataflowGraph
import Amazonka.Glue.GetDevEndpoint
import Amazonka.Glue.GetDevEndpoints
import Amazonka.Glue.GetJob
import Amazonka.Glue.GetJobBookmark
import Amazonka.Glue.GetJobRun
import Amazonka.Glue.GetJobRuns
import Amazonka.Glue.GetJobs
import Amazonka.Glue.GetMLTaskRun
import Amazonka.Glue.GetMLTaskRuns
import Amazonka.Glue.GetMLTransform
import Amazonka.Glue.GetMLTransforms
import Amazonka.Glue.GetMapping
import Amazonka.Glue.GetPartition
import Amazonka.Glue.GetPartitionIndexes
import Amazonka.Glue.GetPartitions
import Amazonka.Glue.GetPlan
import Amazonka.Glue.GetRegistry
import Amazonka.Glue.GetResourcePolicies
import Amazonka.Glue.GetResourcePolicy
import Amazonka.Glue.GetSchema
import Amazonka.Glue.GetSchemaByDefinition
import Amazonka.Glue.GetSchemaVersion
import Amazonka.Glue.GetSchemaVersionsDiff
import Amazonka.Glue.GetSecurityConfiguration
import Amazonka.Glue.GetSecurityConfigurations
import Amazonka.Glue.GetSession
import Amazonka.Glue.GetStatement
import Amazonka.Glue.GetTable
import Amazonka.Glue.GetTableVersion
import Amazonka.Glue.GetTableVersions
import Amazonka.Glue.GetTables
import Amazonka.Glue.GetTags
import Amazonka.Glue.GetTrigger
import Amazonka.Glue.GetTriggers
import Amazonka.Glue.GetUnfilteredPartitionMetadata
import Amazonka.Glue.GetUnfilteredPartitionsMetadata
import Amazonka.Glue.GetUnfilteredTableMetadata
import Amazonka.Glue.GetUserDefinedFunction
import Amazonka.Glue.GetUserDefinedFunctions
import Amazonka.Glue.GetWorkflow
import Amazonka.Glue.GetWorkflowRun
import Amazonka.Glue.GetWorkflowRunProperties
import Amazonka.Glue.GetWorkflowRuns
import Amazonka.Glue.ImportCatalogToGlue
import Amazonka.Glue.ListBlueprints
import Amazonka.Glue.ListCrawlers
import Amazonka.Glue.ListCrawls
import Amazonka.Glue.ListCustomEntityTypes
import Amazonka.Glue.ListDataQualityResults
import Amazonka.Glue.ListDataQualityRuleRecommendationRuns
import Amazonka.Glue.ListDataQualityRulesetEvaluationRuns
import Amazonka.Glue.ListDataQualityRulesets
import Amazonka.Glue.ListDevEndpoints
import Amazonka.Glue.ListJobs
import Amazonka.Glue.ListMLTransforms
import Amazonka.Glue.ListRegistries
import Amazonka.Glue.ListSchemaVersions
import Amazonka.Glue.ListSchemas
import Amazonka.Glue.ListSessions
import Amazonka.Glue.ListStatements
import Amazonka.Glue.ListTriggers
import Amazonka.Glue.ListWorkflows
import Amazonka.Glue.PutDataCatalogEncryptionSettings
import Amazonka.Glue.PutResourcePolicy
import Amazonka.Glue.PutSchemaVersionMetadata
import Amazonka.Glue.PutWorkflowRunProperties
import Amazonka.Glue.QuerySchemaVersionMetadata
import Amazonka.Glue.RegisterSchemaVersion
import Amazonka.Glue.RemoveSchemaVersionMetadata
import Amazonka.Glue.ResetJobBookmark
import Amazonka.Glue.ResumeWorkflowRun
import Amazonka.Glue.RunStatement
import Amazonka.Glue.SearchTables
import Amazonka.Glue.StartBlueprintRun
import Amazonka.Glue.StartCrawler
import Amazonka.Glue.StartCrawlerSchedule
import Amazonka.Glue.StartDataQualityRuleRecommendationRun
import Amazonka.Glue.StartDataQualityRulesetEvaluationRun
import Amazonka.Glue.StartExportLabelsTaskRun
import Amazonka.Glue.StartImportLabelsTaskRun
import Amazonka.Glue.StartJobRun
import Amazonka.Glue.StartMLEvaluationTaskRun
import Amazonka.Glue.StartMLLabelingSetGenerationTaskRun
import Amazonka.Glue.StartTrigger
import Amazonka.Glue.StartWorkflowRun
import Amazonka.Glue.StopCrawler
import Amazonka.Glue.StopCrawlerSchedule
import Amazonka.Glue.StopSession
import Amazonka.Glue.StopTrigger
import Amazonka.Glue.StopWorkflowRun
import Amazonka.Glue.TagResource
import Amazonka.Glue.Types.Action
import Amazonka.Glue.Types.Aggregate
import Amazonka.Glue.Types.AggregateOperation
import Amazonka.Glue.Types.ApplyMapping
import Amazonka.Glue.Types.AthenaConnectorSource
import Amazonka.Glue.Types.AuditContext
import Amazonka.Glue.Types.BackfillError
import Amazonka.Glue.Types.BasicCatalogTarget
import Amazonka.Glue.Types.BatchStopJobRunError
import Amazonka.Glue.Types.BatchStopJobRunSuccessfulSubmission
import Amazonka.Glue.Types.BatchUpdatePartitionFailureEntry
import Amazonka.Glue.Types.BatchUpdatePartitionRequestEntry
import Amazonka.Glue.Types.BinaryColumnStatisticsData
import Amazonka.Glue.Types.Blueprint
import Amazonka.Glue.Types.BlueprintDetails
import Amazonka.Glue.Types.BlueprintRun
import Amazonka.Glue.Types.BooleanColumnStatisticsData
import Amazonka.Glue.Types.CatalogEntry
import Amazonka.Glue.Types.CatalogImportStatus
import Amazonka.Glue.Types.CatalogKafkaSource
import Amazonka.Glue.Types.CatalogKinesisSource
import Amazonka.Glue.Types.CatalogSchemaChangePolicy
import Amazonka.Glue.Types.CatalogSource
import Amazonka.Glue.Types.CatalogTarget
import Amazonka.Glue.Types.Classifier
import Amazonka.Glue.Types.CloudWatchEncryption
import Amazonka.Glue.Types.CodeGenConfigurationNode
import Amazonka.Glue.Types.CodeGenEdge
import Amazonka.Glue.Types.CodeGenNode
import Amazonka.Glue.Types.CodeGenNodeArg
import Amazonka.Glue.Types.Column
import Amazonka.Glue.Types.ColumnError
import Amazonka.Glue.Types.ColumnImportance
import Amazonka.Glue.Types.ColumnRowFilter
import Amazonka.Glue.Types.ColumnStatistics
import Amazonka.Glue.Types.ColumnStatisticsData
import Amazonka.Glue.Types.ColumnStatisticsError
import Amazonka.Glue.Types.Condition
import Amazonka.Glue.Types.ConfusionMatrix
import Amazonka.Glue.Types.Connection
import Amazonka.Glue.Types.ConnectionInput
import Amazonka.Glue.Types.ConnectionPasswordEncryption
import Amazonka.Glue.Types.ConnectionsList
import Amazonka.Glue.Types.Crawl
import Amazonka.Glue.Types.Crawler
import Amazonka.Glue.Types.CrawlerHistory
import Amazonka.Glue.Types.CrawlerMetrics
import Amazonka.Glue.Types.CrawlerNodeDetails
import Amazonka.Glue.Types.CrawlerTargets
import Amazonka.Glue.Types.CrawlsFilter
import Amazonka.Glue.Types.CreateCsvClassifierRequest
import Amazonka.Glue.Types.CreateGrokClassifierRequest
import Amazonka.Glue.Types.CreateJsonClassifierRequest
import Amazonka.Glue.Types.CreateXMLClassifierRequest
import Amazonka.Glue.Types.CsvClassifier
import Amazonka.Glue.Types.CustomCode
import Amazonka.Glue.Types.CustomEntityType
import Amazonka.Glue.Types.DQResultsPublishingOptions
import Amazonka.Glue.Types.DQStopJobOnFailureOptions
import Amazonka.Glue.Types.DataCatalogEncryptionSettings
import Amazonka.Glue.Types.DataLakePrincipal
import Amazonka.Glue.Types.DataQualityEvaluationRunAdditionalRunOptions
import Amazonka.Glue.Types.DataQualityResult
import Amazonka.Glue.Types.DataQualityResultDescription
import Amazonka.Glue.Types.DataQualityResultFilterCriteria
import Amazonka.Glue.Types.DataQualityRuleRecommendationRunDescription
import Amazonka.Glue.Types.DataQualityRuleRecommendationRunFilter
import Amazonka.Glue.Types.DataQualityRuleResult
import Amazonka.Glue.Types.DataQualityRulesetEvaluationRunDescription
import Amazonka.Glue.Types.DataQualityRulesetEvaluationRunFilter
import Amazonka.Glue.Types.DataQualityRulesetFilterCriteria
import Amazonka.Glue.Types.DataQualityRulesetListDetails
import Amazonka.Glue.Types.DataQualityTargetTable
import Amazonka.Glue.Types.DataSource
import Amazonka.Glue.Types.Database
import Amazonka.Glue.Types.DatabaseIdentifier
import Amazonka.Glue.Types.DatabaseInput
import Amazonka.Glue.Types.Datatype
import Amazonka.Glue.Types.DateColumnStatisticsData
import Amazonka.Glue.Types.DecimalColumnStatisticsData
import Amazonka.Glue.Types.DecimalNumber
import Amazonka.Glue.Types.DeltaTarget
import Amazonka.Glue.Types.DevEndpoint
import Amazonka.Glue.Types.DevEndpointCustomLibraries
import Amazonka.Glue.Types.DirectKafkaSource
import Amazonka.Glue.Types.DirectKinesisSource
import Amazonka.Glue.Types.DirectSchemaChangePolicy
import Amazonka.Glue.Types.DoubleColumnStatisticsData
import Amazonka.Glue.Types.DropDuplicates
import Amazonka.Glue.Types.DropFields
import Amazonka.Glue.Types.DropNullFields
import Amazonka.Glue.Types.DynamicTransform
import Amazonka.Glue.Types.DynamoDBCatalogSource
import Amazonka.Glue.Types.DynamoDBTarget
import Amazonka.Glue.Types.Edge
import Amazonka.Glue.Types.EncryptionAtRest
import Amazonka.Glue.Types.EncryptionConfiguration
import Amazonka.Glue.Types.ErrorDetail
import Amazonka.Glue.Types.ErrorDetails
import Amazonka.Glue.Types.EvaluateDataQuality
import Amazonka.Glue.Types.EvaluationMetrics
import Amazonka.Glue.Types.EventBatchingCondition
import Amazonka.Glue.Types.ExecutionProperty
import Amazonka.Glue.Types.ExportLabelsTaskRunProperties
import Amazonka.Glue.Types.FillMissingValues
import Amazonka.Glue.Types.Filter
import Amazonka.Glue.Types.FilterExpression
import Amazonka.Glue.Types.FilterValue
import Amazonka.Glue.Types.FindMatchesMetrics
import Amazonka.Glue.Types.FindMatchesParameters
import Amazonka.Glue.Types.FindMatchesTaskRunProperties
import Amazonka.Glue.Types.GetConnectionsFilter
import Amazonka.Glue.Types.GluePolicy
import Amazonka.Glue.Types.GlueSchema
import Amazonka.Glue.Types.GlueStudioSchemaColumn
import Amazonka.Glue.Types.GlueTable
import Amazonka.Glue.Types.GovernedCatalogSource
import Amazonka.Glue.Types.GovernedCatalogTarget
import Amazonka.Glue.Types.GrokClassifier
import Amazonka.Glue.Types.ImportLabelsTaskRunProperties
import Amazonka.Glue.Types.JDBCConnectorOptions
import Amazonka.Glue.Types.JDBCConnectorSource
import Amazonka.Glue.Types.JDBCConnectorTarget
import Amazonka.Glue.Types.JdbcTarget
import Amazonka.Glue.Types.Job
import Amazonka.Glue.Types.JobBookmarkEntry
import Amazonka.Glue.Types.JobBookmarksEncryption
import Amazonka.Glue.Types.JobCommand
import Amazonka.Glue.Types.JobNodeDetails
import Amazonka.Glue.Types.JobRun
import Amazonka.Glue.Types.JobUpdate
import Amazonka.Glue.Types.Join
import Amazonka.Glue.Types.JoinColumn
import Amazonka.Glue.Types.JsonClassifier
import Amazonka.Glue.Types.KafkaStreamingSourceOptions
import Amazonka.Glue.Types.KeySchemaElement
import Amazonka.Glue.Types.KinesisStreamingSourceOptions
import Amazonka.Glue.Types.LabelingSetGenerationTaskRunProperties
import Amazonka.Glue.Types.LakeFormationConfiguration
import Amazonka.Glue.Types.LastActiveDefinition
import Amazonka.Glue.Types.LastCrawlInfo
import Amazonka.Glue.Types.LineageConfiguration
import Amazonka.Glue.Types.Location
import Amazonka.Glue.Types.LongColumnStatisticsData
import Amazonka.Glue.Types.MLTransform
import Amazonka.Glue.Types.MLUserDataEncryption
import Amazonka.Glue.Types.Mapping
import Amazonka.Glue.Types.MappingEntry
import Amazonka.Glue.Types.Merge
import Amazonka.Glue.Types.MetadataInfo
import Amazonka.Glue.Types.MetadataKeyValuePair
import Amazonka.Glue.Types.MicrosoftSQLServerCatalogSource
import Amazonka.Glue.Types.MicrosoftSQLServerCatalogTarget
import Amazonka.Glue.Types.MongoDBTarget
import Amazonka.Glue.Types.MySQLCatalogSource
import Amazonka.Glue.Types.MySQLCatalogTarget
import Amazonka.Glue.Types.Node
import Amazonka.Glue.Types.NotificationProperty
import Amazonka.Glue.Types.NullCheckBoxList
import Amazonka.Glue.Types.NullValueField
import Amazonka.Glue.Types.OracleSQLCatalogSource
import Amazonka.Glue.Types.OracleSQLCatalogTarget
import Amazonka.Glue.Types.Order
import Amazonka.Glue.Types.OtherMetadataValueListItem
import Amazonka.Glue.Types.PIIDetection
import Amazonka.Glue.Types.Partition
import Amazonka.Glue.Types.PartitionError
import Amazonka.Glue.Types.PartitionIndex
import Amazonka.Glue.Types.PartitionIndexDescriptor
import Amazonka.Glue.Types.PartitionInput
import Amazonka.Glue.Types.PartitionValueList
import Amazonka.Glue.Types.PhysicalConnectionRequirements
import Amazonka.Glue.Types.PostgreSQLCatalogSource
import Amazonka.Glue.Types.PostgreSQLCatalogTarget
import Amazonka.Glue.Types.Predecessor
import Amazonka.Glue.Types.Predicate
import Amazonka.Glue.Types.PrincipalPermissions
import Amazonka.Glue.Types.PropertyPredicate
import Amazonka.Glue.Types.RecrawlPolicy
import Amazonka.Glue.Types.RedshiftSource
import Amazonka.Glue.Types.RedshiftTarget
import Amazonka.Glue.Types.RegistryId
import Amazonka.Glue.Types.RegistryListItem
import Amazonka.Glue.Types.RelationalCatalogSource
import Amazonka.Glue.Types.RenameField
import Amazonka.Glue.Types.ResourceUri
import Amazonka.Glue.Types.S3CatalogSource
import Amazonka.Glue.Types.S3CatalogTarget
import Amazonka.Glue.Types.S3CsvSource
import Amazonka.Glue.Types.S3DirectSourceAdditionalOptions
import Amazonka.Glue.Types.S3DirectTarget
import Amazonka.Glue.Types.S3Encryption
import Amazonka.Glue.Types.S3GlueParquetTarget
import Amazonka.Glue.Types.S3JsonSource
import Amazonka.Glue.Types.S3ParquetSource
import Amazonka.Glue.Types.S3SourceAdditionalOptions
import Amazonka.Glue.Types.S3Target
import Amazonka.Glue.Types.Schedule
import Amazonka.Glue.Types.SchemaChangePolicy
import Amazonka.Glue.Types.SchemaColumn
import Amazonka.Glue.Types.SchemaId
import Amazonka.Glue.Types.SchemaListItem
import Amazonka.Glue.Types.SchemaReference
import Amazonka.Glue.Types.SchemaVersionErrorItem
import Amazonka.Glue.Types.SchemaVersionListItem
import Amazonka.Glue.Types.SchemaVersionNumber
import Amazonka.Glue.Types.SecurityConfiguration
import Amazonka.Glue.Types.Segment
import Amazonka.Glue.Types.SelectFields
import Amazonka.Glue.Types.SelectFromCollection
import Amazonka.Glue.Types.SerDeInfo
import Amazonka.Glue.Types.Session
import Amazonka.Glue.Types.SessionCommand
import Amazonka.Glue.Types.SkewedInfo
import Amazonka.Glue.Types.SortCriterion
import Amazonka.Glue.Types.SourceControlDetails
import Amazonka.Glue.Types.SparkConnectorSource
import Amazonka.Glue.Types.SparkConnectorTarget
import Amazonka.Glue.Types.SparkSQL
import Amazonka.Glue.Types.Spigot
import Amazonka.Glue.Types.SplitFields
import Amazonka.Glue.Types.SqlAlias
import Amazonka.Glue.Types.StartingEventBatchCondition
import Amazonka.Glue.Types.Statement
import Amazonka.Glue.Types.StatementOutput
import Amazonka.Glue.Types.StatementOutputData
import Amazonka.Glue.Types.StorageDescriptor
import Amazonka.Glue.Types.StreamingDataPreviewOptions
import Amazonka.Glue.Types.StringColumnStatisticsData
import Amazonka.Glue.Types.Table
import Amazonka.Glue.Types.TableError
import Amazonka.Glue.Types.TableIdentifier
import Amazonka.Glue.Types.TableInput
import Amazonka.Glue.Types.TableVersion
import Amazonka.Glue.Types.TableVersionError
import Amazonka.Glue.Types.TaskRun
import Amazonka.Glue.Types.TaskRunFilterCriteria
import Amazonka.Glue.Types.TaskRunProperties
import Amazonka.Glue.Types.TaskRunSortCriteria
import Amazonka.Glue.Types.TransformConfigParameter
import Amazonka.Glue.Types.TransformEncryption
import Amazonka.Glue.Types.TransformFilterCriteria
import Amazonka.Glue.Types.TransformParameters
import Amazonka.Glue.Types.TransformSortCriteria
import Amazonka.Glue.Types.Trigger
import Amazonka.Glue.Types.TriggerNodeDetails
import Amazonka.Glue.Types.TriggerUpdate
import Amazonka.Glue.Types.UnfilteredPartition
import Amazonka.Glue.Types.Union
import Amazonka.Glue.Types.UpdateCsvClassifierRequest
import Amazonka.Glue.Types.UpdateGrokClassifierRequest
import Amazonka.Glue.Types.UpdateJsonClassifierRequest
import Amazonka.Glue.Types.UpdateXMLClassifierRequest
import Amazonka.Glue.Types.UpsertRedshiftTargetOptions
import Amazonka.Glue.Types.UserDefinedFunction
import Amazonka.Glue.Types.UserDefinedFunctionInput
import Amazonka.Glue.Types.Workflow
import Amazonka.Glue.Types.WorkflowGraph
import Amazonka.Glue.Types.WorkflowRun
import Amazonka.Glue.Types.WorkflowRunStatistics
import Amazonka.Glue.Types.XMLClassifier
import Amazonka.Glue.UntagResource
import Amazonka.Glue.UpdateBlueprint
import Amazonka.Glue.UpdateClassifier
import Amazonka.Glue.UpdateColumnStatisticsForPartition
import Amazonka.Glue.UpdateColumnStatisticsForTable
import Amazonka.Glue.UpdateConnection
import Amazonka.Glue.UpdateCrawler
import Amazonka.Glue.UpdateCrawlerSchedule
import Amazonka.Glue.UpdateDataQualityRuleset
import Amazonka.Glue.UpdateDatabase
import Amazonka.Glue.UpdateDevEndpoint
import Amazonka.Glue.UpdateJob
import Amazonka.Glue.UpdateJobFromSourceControl
import Amazonka.Glue.UpdateMLTransform
import Amazonka.Glue.UpdatePartition
import Amazonka.Glue.UpdateRegistry
import Amazonka.Glue.UpdateSchema
import Amazonka.Glue.UpdateSourceControlFromJob
import Amazonka.Glue.UpdateTable
import Amazonka.Glue.UpdateTrigger
import Amazonka.Glue.UpdateUserDefinedFunction
import Amazonka.Glue.UpdateWorkflow
