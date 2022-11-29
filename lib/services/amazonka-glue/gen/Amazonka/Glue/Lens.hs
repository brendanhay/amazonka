{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    batchDeleteConnectionResponse_succeeded,
    batchDeleteConnectionResponse_errors,
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

    -- ** BatchGetDevEndpoints
    batchGetDevEndpoints_devEndpointNames,
    batchGetDevEndpointsResponse_devEndpointsNotFound,
    batchGetDevEndpointsResponse_devEndpoints,
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
    batchGetPartitionResponse_unprocessedKeys,
    batchGetPartitionResponse_partitions,
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
    batchStopJobRunResponse_successfulSubmissions,
    batchStopJobRunResponse_errors,
    batchStopJobRunResponse_httpStatus,

    -- ** BatchUpdatePartition
    batchUpdatePartition_catalogId,
    batchUpdatePartition_databaseName,
    batchUpdatePartition_tableName,
    batchUpdatePartition_entries,
    batchUpdatePartitionResponse_errors,
    batchUpdatePartitionResponse_httpStatus,

    -- ** CancelMLTaskRun
    cancelMLTaskRun_transformId,
    cancelMLTaskRun_taskRunId,
    cancelMLTaskRunResponse_status,
    cancelMLTaskRunResponse_transformId,
    cancelMLTaskRunResponse_taskRunId,
    cancelMLTaskRunResponse_httpStatus,

    -- ** CancelStatement
    cancelStatement_requestOrigin,
    cancelStatement_sessionId,
    cancelStatement_id,
    cancelStatementResponse_httpStatus,

    -- ** CheckSchemaVersionValidity
    checkSchemaVersionValidity_dataFormat,
    checkSchemaVersionValidity_schemaDefinition,
    checkSchemaVersionValidityResponse_valid,
    checkSchemaVersionValidityResponse_error,
    checkSchemaVersionValidityResponse_httpStatus,

    -- ** CreateBlueprint
    createBlueprint_tags,
    createBlueprint_description,
    createBlueprint_name,
    createBlueprint_blueprintLocation,
    createBlueprintResponse_name,
    createBlueprintResponse_httpStatus,

    -- ** CreateClassifier
    createClassifier_csvClassifier,
    createClassifier_xMLClassifier,
    createClassifier_grokClassifier,
    createClassifier_jsonClassifier,
    createClassifierResponse_httpStatus,

    -- ** CreateConnection
    createConnection_tags,
    createConnection_catalogId,
    createConnection_connectionInput,
    createConnectionResponse_httpStatus,

    -- ** CreateCrawler
    createCrawler_tags,
    createCrawler_schedule,
    createCrawler_recrawlPolicy,
    createCrawler_classifiers,
    createCrawler_schemaChangePolicy,
    createCrawler_databaseName,
    createCrawler_configuration,
    createCrawler_tablePrefix,
    createCrawler_description,
    createCrawler_lineageConfiguration,
    createCrawler_crawlerSecurityConfiguration,
    createCrawler_lakeFormationConfiguration,
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

    -- ** CreateDatabase
    createDatabase_tags,
    createDatabase_catalogId,
    createDatabase_databaseInput,
    createDatabaseResponse_httpStatus,

    -- ** CreateDevEndpoint
    createDevEndpoint_securityConfiguration,
    createDevEndpoint_tags,
    createDevEndpoint_publicKey,
    createDevEndpoint_numberOfWorkers,
    createDevEndpoint_securityGroupIds,
    createDevEndpoint_glueVersion,
    createDevEndpoint_subnetId,
    createDevEndpoint_workerType,
    createDevEndpoint_extraJarsS3Path,
    createDevEndpoint_numberOfNodes,
    createDevEndpoint_arguments,
    createDevEndpoint_extraPythonLibsS3Path,
    createDevEndpoint_publicKeys,
    createDevEndpoint_endpointName,
    createDevEndpoint_roleArn,
    createDevEndpointResponse_securityConfiguration,
    createDevEndpointResponse_zeppelinRemoteSparkInterpreterPort,
    createDevEndpointResponse_roleArn,
    createDevEndpointResponse_numberOfWorkers,
    createDevEndpointResponse_securityGroupIds,
    createDevEndpointResponse_endpointName,
    createDevEndpointResponse_glueVersion,
    createDevEndpointResponse_createdTimestamp,
    createDevEndpointResponse_subnetId,
    createDevEndpointResponse_workerType,
    createDevEndpointResponse_status,
    createDevEndpointResponse_availabilityZone,
    createDevEndpointResponse_extraJarsS3Path,
    createDevEndpointResponse_numberOfNodes,
    createDevEndpointResponse_arguments,
    createDevEndpointResponse_yarnEndpointAddress,
    createDevEndpointResponse_vpcId,
    createDevEndpointResponse_extraPythonLibsS3Path,
    createDevEndpointResponse_failureReason,
    createDevEndpointResponse_httpStatus,

    -- ** CreateJob
    createJob_securityConfiguration,
    createJob_tags,
    createJob_timeout,
    createJob_nonOverridableArguments,
    createJob_numberOfWorkers,
    createJob_glueVersion,
    createJob_notificationProperty,
    createJob_workerType,
    createJob_executionProperty,
    createJob_allocatedCapacity,
    createJob_description,
    createJob_maxRetries,
    createJob_codeGenConfigurationNodes,
    createJob_defaultArguments,
    createJob_sourceControlDetails,
    createJob_logUri,
    createJob_connections,
    createJob_maxCapacity,
    createJob_executionClass,
    createJob_name,
    createJob_role,
    createJob_command,
    createJobResponse_name,
    createJobResponse_httpStatus,

    -- ** CreateMLTransform
    createMLTransform_tags,
    createMLTransform_timeout,
    createMLTransform_numberOfWorkers,
    createMLTransform_glueVersion,
    createMLTransform_workerType,
    createMLTransform_description,
    createMLTransform_maxRetries,
    createMLTransform_transformEncryption,
    createMLTransform_maxCapacity,
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
    createRegistry_tags,
    createRegistry_description,
    createRegistry_registryName,
    createRegistryResponse_tags,
    createRegistryResponse_registryName,
    createRegistryResponse_description,
    createRegistryResponse_registryArn,
    createRegistryResponse_httpStatus,

    -- ** CreateSchema
    createSchema_compatibility,
    createSchema_tags,
    createSchema_description,
    createSchema_registryId,
    createSchema_schemaDefinition,
    createSchema_schemaName,
    createSchema_dataFormat,
    createSchemaResponse_compatibility,
    createSchemaResponse_tags,
    createSchemaResponse_registryName,
    createSchemaResponse_schemaStatus,
    createSchemaResponse_dataFormat,
    createSchemaResponse_schemaVersionStatus,
    createSchemaResponse_schemaName,
    createSchemaResponse_description,
    createSchemaResponse_schemaArn,
    createSchemaResponse_registryArn,
    createSchemaResponse_nextSchemaVersion,
    createSchemaResponse_schemaCheckpoint,
    createSchemaResponse_schemaVersionId,
    createSchemaResponse_latestSchemaVersion,
    createSchemaResponse_httpStatus,

    -- ** CreateScript
    createScript_dagNodes,
    createScript_language,
    createScript_dagEdges,
    createScriptResponse_scalaCode,
    createScriptResponse_pythonScript,
    createScriptResponse_httpStatus,

    -- ** CreateSecurityConfiguration
    createSecurityConfiguration_name,
    createSecurityConfiguration_encryptionConfiguration,
    createSecurityConfigurationResponse_name,
    createSecurityConfigurationResponse_createdTimestamp,
    createSecurityConfigurationResponse_httpStatus,

    -- ** CreateSession
    createSession_securityConfiguration,
    createSession_tags,
    createSession_timeout,
    createSession_numberOfWorkers,
    createSession_glueVersion,
    createSession_requestOrigin,
    createSession_idleTimeout,
    createSession_workerType,
    createSession_description,
    createSession_defaultArguments,
    createSession_connections,
    createSession_maxCapacity,
    createSession_id,
    createSession_role,
    createSession_command,
    createSessionResponse_session,
    createSessionResponse_httpStatus,

    -- ** CreateTable
    createTable_catalogId,
    createTable_transactionId,
    createTable_partitionIndexes,
    createTable_databaseName,
    createTable_tableInput,
    createTableResponse_httpStatus,

    -- ** CreateTrigger
    createTrigger_tags,
    createTrigger_eventBatchingCondition,
    createTrigger_schedule,
    createTrigger_workflowName,
    createTrigger_predicate,
    createTrigger_description,
    createTrigger_startOnCreation,
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
    createWorkflow_tags,
    createWorkflow_maxConcurrentRuns,
    createWorkflow_defaultRunProperties,
    createWorkflow_description,
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
    deleteRegistryResponse_registryName,
    deleteRegistryResponse_status,
    deleteRegistryResponse_registryArn,
    deleteRegistryResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_policyHashCondition,
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteSchema
    deleteSchema_schemaId,
    deleteSchemaResponse_schemaName,
    deleteSchemaResponse_status,
    deleteSchemaResponse_schemaArn,
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
    getBlueprintRuns_nextToken,
    getBlueprintRuns_maxResults,
    getBlueprintRuns_blueprintName,
    getBlueprintRunsResponse_nextToken,
    getBlueprintRunsResponse_blueprintRuns,
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
    getClassifiers_nextToken,
    getClassifiers_maxResults,
    getClassifiersResponse_nextToken,
    getClassifiersResponse_classifiers,
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
    getConnection_hidePassword,
    getConnection_catalogId,
    getConnection_name,
    getConnectionResponse_connection,
    getConnectionResponse_httpStatus,

    -- ** GetConnections
    getConnections_nextToken,
    getConnections_hidePassword,
    getConnections_filter,
    getConnections_maxResults,
    getConnections_catalogId,
    getConnectionsResponse_nextToken,
    getConnectionsResponse_connectionList,
    getConnectionsResponse_httpStatus,

    -- ** GetCrawler
    getCrawler_name,
    getCrawlerResponse_crawler,
    getCrawlerResponse_httpStatus,

    -- ** GetCrawlerMetrics
    getCrawlerMetrics_nextToken,
    getCrawlerMetrics_maxResults,
    getCrawlerMetrics_crawlerNameList,
    getCrawlerMetricsResponse_nextToken,
    getCrawlerMetricsResponse_crawlerMetricsList,
    getCrawlerMetricsResponse_httpStatus,

    -- ** GetCrawlers
    getCrawlers_nextToken,
    getCrawlers_maxResults,
    getCrawlersResponse_nextToken,
    getCrawlersResponse_crawlers,
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

    -- ** GetDatabase
    getDatabase_catalogId,
    getDatabase_name,
    getDatabaseResponse_database,
    getDatabaseResponse_httpStatus,

    -- ** GetDatabases
    getDatabases_nextToken,
    getDatabases_resourceShareType,
    getDatabases_maxResults,
    getDatabases_catalogId,
    getDatabasesResponse_nextToken,
    getDatabasesResponse_httpStatus,
    getDatabasesResponse_databaseList,

    -- ** GetDataflowGraph
    getDataflowGraph_pythonScript,
    getDataflowGraphResponse_dagNodes,
    getDataflowGraphResponse_dagEdges,
    getDataflowGraphResponse_httpStatus,

    -- ** GetDevEndpoint
    getDevEndpoint_endpointName,
    getDevEndpointResponse_devEndpoint,
    getDevEndpointResponse_httpStatus,

    -- ** GetDevEndpoints
    getDevEndpoints_nextToken,
    getDevEndpoints_maxResults,
    getDevEndpointsResponse_nextToken,
    getDevEndpointsResponse_devEndpoints,
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
    getJobRuns_nextToken,
    getJobRuns_maxResults,
    getJobRuns_jobName,
    getJobRunsResponse_nextToken,
    getJobRunsResponse_jobRuns,
    getJobRunsResponse_httpStatus,

    -- ** GetJobs
    getJobs_nextToken,
    getJobs_maxResults,
    getJobsResponse_nextToken,
    getJobsResponse_jobs,
    getJobsResponse_httpStatus,

    -- ** GetMLTaskRun
    getMLTaskRun_transformId,
    getMLTaskRun_taskRunId,
    getMLTaskRunResponse_lastModifiedOn,
    getMLTaskRunResponse_startedOn,
    getMLTaskRunResponse_properties,
    getMLTaskRunResponse_executionTime,
    getMLTaskRunResponse_status,
    getMLTaskRunResponse_transformId,
    getMLTaskRunResponse_completedOn,
    getMLTaskRunResponse_taskRunId,
    getMLTaskRunResponse_errorString,
    getMLTaskRunResponse_logGroupName,
    getMLTaskRunResponse_httpStatus,

    -- ** GetMLTaskRuns
    getMLTaskRuns_nextToken,
    getMLTaskRuns_sort,
    getMLTaskRuns_filter,
    getMLTaskRuns_maxResults,
    getMLTaskRuns_transformId,
    getMLTaskRunsResponse_nextToken,
    getMLTaskRunsResponse_taskRuns,
    getMLTaskRunsResponse_httpStatus,

    -- ** GetMLTransform
    getMLTransform_transformId,
    getMLTransformResponse_evaluationMetrics,
    getMLTransformResponse_createdOn,
    getMLTransformResponse_timeout,
    getMLTransformResponse_name,
    getMLTransformResponse_lastModifiedOn,
    getMLTransformResponse_numberOfWorkers,
    getMLTransformResponse_glueVersion,
    getMLTransformResponse_workerType,
    getMLTransformResponse_inputRecordTables,
    getMLTransformResponse_status,
    getMLTransformResponse_labelCount,
    getMLTransformResponse_description,
    getMLTransformResponse_maxRetries,
    getMLTransformResponse_transformId,
    getMLTransformResponse_transformEncryption,
    getMLTransformResponse_schema,
    getMLTransformResponse_role,
    getMLTransformResponse_maxCapacity,
    getMLTransformResponse_parameters,
    getMLTransformResponse_httpStatus,

    -- ** GetMLTransforms
    getMLTransforms_nextToken,
    getMLTransforms_sort,
    getMLTransforms_filter,
    getMLTransforms_maxResults,
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
    getPartitionIndexes_nextToken,
    getPartitionIndexes_catalogId,
    getPartitionIndexes_databaseName,
    getPartitionIndexes_tableName,
    getPartitionIndexesResponse_nextToken,
    getPartitionIndexesResponse_partitionIndexDescriptorList,
    getPartitionIndexesResponse_httpStatus,

    -- ** GetPartitions
    getPartitions_nextToken,
    getPartitions_queryAsOfTime,
    getPartitions_expression,
    getPartitions_segment,
    getPartitions_maxResults,
    getPartitions_catalogId,
    getPartitions_transactionId,
    getPartitions_excludeColumnSchema,
    getPartitions_databaseName,
    getPartitions_tableName,
    getPartitionsResponse_nextToken,
    getPartitionsResponse_partitions,
    getPartitionsResponse_httpStatus,

    -- ** GetPlan
    getPlan_location,
    getPlan_additionalPlanOptionsMap,
    getPlan_sinks,
    getPlan_language,
    getPlan_mapping,
    getPlan_source,
    getPlanResponse_scalaCode,
    getPlanResponse_pythonScript,
    getPlanResponse_httpStatus,

    -- ** GetRegistry
    getRegistry_registryId,
    getRegistryResponse_registryName,
    getRegistryResponse_createdTime,
    getRegistryResponse_status,
    getRegistryResponse_description,
    getRegistryResponse_registryArn,
    getRegistryResponse_updatedTime,
    getRegistryResponse_httpStatus,

    -- ** GetResourcePolicies
    getResourcePolicies_nextToken,
    getResourcePolicies_maxResults,
    getResourcePoliciesResponse_getResourcePoliciesResponseList,
    getResourcePoliciesResponse_nextToken,
    getResourcePoliciesResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_resourceArn,
    getResourcePolicyResponse_policyInJson,
    getResourcePolicyResponse_updateTime,
    getResourcePolicyResponse_createTime,
    getResourcePolicyResponse_policyHash,
    getResourcePolicyResponse_httpStatus,

    -- ** GetSchema
    getSchema_schemaId,
    getSchemaResponse_compatibility,
    getSchemaResponse_registryName,
    getSchemaResponse_createdTime,
    getSchemaResponse_schemaStatus,
    getSchemaResponse_dataFormat,
    getSchemaResponse_schemaName,
    getSchemaResponse_description,
    getSchemaResponse_schemaArn,
    getSchemaResponse_registryArn,
    getSchemaResponse_nextSchemaVersion,
    getSchemaResponse_schemaCheckpoint,
    getSchemaResponse_latestSchemaVersion,
    getSchemaResponse_updatedTime,
    getSchemaResponse_httpStatus,

    -- ** GetSchemaByDefinition
    getSchemaByDefinition_schemaId,
    getSchemaByDefinition_schemaDefinition,
    getSchemaByDefinitionResponse_createdTime,
    getSchemaByDefinitionResponse_dataFormat,
    getSchemaByDefinitionResponse_status,
    getSchemaByDefinitionResponse_schemaArn,
    getSchemaByDefinitionResponse_schemaVersionId,
    getSchemaByDefinitionResponse_httpStatus,

    -- ** GetSchemaVersion
    getSchemaVersion_schemaVersionNumber,
    getSchemaVersion_schemaVersionId,
    getSchemaVersion_schemaId,
    getSchemaVersionResponse_createdTime,
    getSchemaVersionResponse_dataFormat,
    getSchemaVersionResponse_status,
    getSchemaVersionResponse_schemaArn,
    getSchemaVersionResponse_versionNumber,
    getSchemaVersionResponse_schemaVersionId,
    getSchemaVersionResponse_schemaDefinition,
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
    getSecurityConfigurations_nextToken,
    getSecurityConfigurations_maxResults,
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
    getTable_queryAsOfTime,
    getTable_catalogId,
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
    getTableVersions_nextToken,
    getTableVersions_maxResults,
    getTableVersions_catalogId,
    getTableVersions_databaseName,
    getTableVersions_tableName,
    getTableVersionsResponse_nextToken,
    getTableVersionsResponse_tableVersions,
    getTableVersionsResponse_httpStatus,

    -- ** GetTables
    getTables_nextToken,
    getTables_queryAsOfTime,
    getTables_expression,
    getTables_maxResults,
    getTables_catalogId,
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
    getTriggers_nextToken,
    getTriggers_maxResults,
    getTriggers_dependentJobName,
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
    getUnfilteredPartitionMetadataResponse_partition,
    getUnfilteredPartitionMetadataResponse_isRegisteredWithLakeFormation,
    getUnfilteredPartitionMetadataResponse_httpStatus,

    -- ** GetUnfilteredPartitionsMetadata
    getUnfilteredPartitionsMetadata_nextToken,
    getUnfilteredPartitionsMetadata_auditContext,
    getUnfilteredPartitionsMetadata_expression,
    getUnfilteredPartitionsMetadata_segment,
    getUnfilteredPartitionsMetadata_maxResults,
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
    getUnfilteredTableMetadataResponse_cellFilters,
    getUnfilteredTableMetadataResponse_authorizedColumns,
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
    getUserDefinedFunctions_nextToken,
    getUserDefinedFunctions_databaseName,
    getUserDefinedFunctions_maxResults,
    getUserDefinedFunctions_catalogId,
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
    getWorkflowRuns_nextToken,
    getWorkflowRuns_maxResults,
    getWorkflowRuns_includeGraph,
    getWorkflowRuns_name,
    getWorkflowRunsResponse_nextToken,
    getWorkflowRunsResponse_runs,
    getWorkflowRunsResponse_httpStatus,

    -- ** ImportCatalogToGlue
    importCatalogToGlue_catalogId,
    importCatalogToGlueResponse_httpStatus,

    -- ** ListBlueprints
    listBlueprints_tags,
    listBlueprints_nextToken,
    listBlueprints_maxResults,
    listBlueprintsResponse_nextToken,
    listBlueprintsResponse_blueprints,
    listBlueprintsResponse_httpStatus,

    -- ** ListCrawlers
    listCrawlers_tags,
    listCrawlers_nextToken,
    listCrawlers_maxResults,
    listCrawlersResponse_nextToken,
    listCrawlersResponse_crawlerNames,
    listCrawlersResponse_httpStatus,

    -- ** ListCrawls
    listCrawls_nextToken,
    listCrawls_filters,
    listCrawls_maxResults,
    listCrawls_crawlerName,
    listCrawlsResponse_crawls,
    listCrawlsResponse_nextToken,
    listCrawlsResponse_httpStatus,

    -- ** ListCustomEntityTypes
    listCustomEntityTypes_nextToken,
    listCustomEntityTypes_maxResults,
    listCustomEntityTypesResponse_nextToken,
    listCustomEntityTypesResponse_customEntityTypes,
    listCustomEntityTypesResponse_httpStatus,

    -- ** ListDevEndpoints
    listDevEndpoints_tags,
    listDevEndpoints_nextToken,
    listDevEndpoints_maxResults,
    listDevEndpointsResponse_nextToken,
    listDevEndpointsResponse_devEndpointNames,
    listDevEndpointsResponse_httpStatus,

    -- ** ListJobs
    listJobs_tags,
    listJobs_nextToken,
    listJobs_maxResults,
    listJobsResponse_nextToken,
    listJobsResponse_jobNames,
    listJobsResponse_httpStatus,

    -- ** ListMLTransforms
    listMLTransforms_tags,
    listMLTransforms_nextToken,
    listMLTransforms_sort,
    listMLTransforms_filter,
    listMLTransforms_maxResults,
    listMLTransformsResponse_nextToken,
    listMLTransformsResponse_httpStatus,
    listMLTransformsResponse_transformIds,

    -- ** ListRegistries
    listRegistries_nextToken,
    listRegistries_maxResults,
    listRegistriesResponse_nextToken,
    listRegistriesResponse_registries,
    listRegistriesResponse_httpStatus,

    -- ** ListSchemaVersions
    listSchemaVersions_nextToken,
    listSchemaVersions_maxResults,
    listSchemaVersions_schemaId,
    listSchemaVersionsResponse_nextToken,
    listSchemaVersionsResponse_schemas,
    listSchemaVersionsResponse_httpStatus,

    -- ** ListSchemas
    listSchemas_nextToken,
    listSchemas_maxResults,
    listSchemas_registryId,
    listSchemasResponse_nextToken,
    listSchemasResponse_schemas,
    listSchemasResponse_httpStatus,

    -- ** ListSessions
    listSessions_tags,
    listSessions_nextToken,
    listSessions_requestOrigin,
    listSessions_maxResults,
    listSessionsResponse_nextToken,
    listSessionsResponse_sessions,
    listSessionsResponse_ids,
    listSessionsResponse_httpStatus,

    -- ** ListStatements
    listStatements_nextToken,
    listStatements_requestOrigin,
    listStatements_sessionId,
    listStatementsResponse_nextToken,
    listStatementsResponse_statements,
    listStatementsResponse_httpStatus,

    -- ** ListTriggers
    listTriggers_tags,
    listTriggers_nextToken,
    listTriggers_maxResults,
    listTriggers_dependentJobName,
    listTriggersResponse_nextToken,
    listTriggersResponse_triggerNames,
    listTriggersResponse_httpStatus,

    -- ** ListWorkflows
    listWorkflows_nextToken,
    listWorkflows_maxResults,
    listWorkflowsResponse_nextToken,
    listWorkflowsResponse_workflows,
    listWorkflowsResponse_httpStatus,

    -- ** PutDataCatalogEncryptionSettings
    putDataCatalogEncryptionSettings_catalogId,
    putDataCatalogEncryptionSettings_dataCatalogEncryptionSettings,
    putDataCatalogEncryptionSettingsResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_enableHybrid,
    putResourcePolicy_policyHashCondition,
    putResourcePolicy_policyExistsCondition,
    putResourcePolicy_resourceArn,
    putResourcePolicy_policyInJson,
    putResourcePolicyResponse_policyHash,
    putResourcePolicyResponse_httpStatus,

    -- ** PutSchemaVersionMetadata
    putSchemaVersionMetadata_schemaVersionNumber,
    putSchemaVersionMetadata_schemaVersionId,
    putSchemaVersionMetadata_schemaId,
    putSchemaVersionMetadata_metadataKeyValue,
    putSchemaVersionMetadataResponse_registryName,
    putSchemaVersionMetadataResponse_schemaName,
    putSchemaVersionMetadataResponse_latestVersion,
    putSchemaVersionMetadataResponse_metadataValue,
    putSchemaVersionMetadataResponse_schemaArn,
    putSchemaVersionMetadataResponse_versionNumber,
    putSchemaVersionMetadataResponse_metadataKey,
    putSchemaVersionMetadataResponse_schemaVersionId,
    putSchemaVersionMetadataResponse_httpStatus,

    -- ** PutWorkflowRunProperties
    putWorkflowRunProperties_name,
    putWorkflowRunProperties_runId,
    putWorkflowRunProperties_runProperties,
    putWorkflowRunPropertiesResponse_httpStatus,

    -- ** QuerySchemaVersionMetadata
    querySchemaVersionMetadata_nextToken,
    querySchemaVersionMetadata_maxResults,
    querySchemaVersionMetadata_schemaVersionNumber,
    querySchemaVersionMetadata_metadataList,
    querySchemaVersionMetadata_schemaVersionId,
    querySchemaVersionMetadata_schemaId,
    querySchemaVersionMetadataResponse_nextToken,
    querySchemaVersionMetadataResponse_schemaVersionId,
    querySchemaVersionMetadataResponse_metadataInfoMap,
    querySchemaVersionMetadataResponse_httpStatus,

    -- ** RegisterSchemaVersion
    registerSchemaVersion_schemaId,
    registerSchemaVersion_schemaDefinition,
    registerSchemaVersionResponse_status,
    registerSchemaVersionResponse_versionNumber,
    registerSchemaVersionResponse_schemaVersionId,
    registerSchemaVersionResponse_httpStatus,

    -- ** RemoveSchemaVersionMetadata
    removeSchemaVersionMetadata_schemaVersionNumber,
    removeSchemaVersionMetadata_schemaVersionId,
    removeSchemaVersionMetadata_schemaId,
    removeSchemaVersionMetadata_metadataKeyValue,
    removeSchemaVersionMetadataResponse_registryName,
    removeSchemaVersionMetadataResponse_schemaName,
    removeSchemaVersionMetadataResponse_latestVersion,
    removeSchemaVersionMetadataResponse_metadataValue,
    removeSchemaVersionMetadataResponse_schemaArn,
    removeSchemaVersionMetadataResponse_versionNumber,
    removeSchemaVersionMetadataResponse_metadataKey,
    removeSchemaVersionMetadataResponse_schemaVersionId,
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
    searchTables_sortCriteria,
    searchTables_nextToken,
    searchTables_searchText,
    searchTables_resourceShareType,
    searchTables_filters,
    searchTables_maxResults,
    searchTables_catalogId,
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
    startJobRun_securityConfiguration,
    startJobRun_timeout,
    startJobRun_numberOfWorkers,
    startJobRun_notificationProperty,
    startJobRun_jobRunId,
    startJobRun_workerType,
    startJobRun_allocatedCapacity,
    startJobRun_arguments,
    startJobRun_maxCapacity,
    startJobRun_executionClass,
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
    updateClassifier_xMLClassifier,
    updateClassifier_grokClassifier,
    updateClassifier_jsonClassifier,
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
    updateCrawler_schedule,
    updateCrawler_recrawlPolicy,
    updateCrawler_classifiers,
    updateCrawler_schemaChangePolicy,
    updateCrawler_databaseName,
    updateCrawler_configuration,
    updateCrawler_tablePrefix,
    updateCrawler_targets,
    updateCrawler_description,
    updateCrawler_lineageConfiguration,
    updateCrawler_role,
    updateCrawler_crawlerSecurityConfiguration,
    updateCrawler_lakeFormationConfiguration,
    updateCrawler_name,
    updateCrawlerResponse_httpStatus,

    -- ** UpdateCrawlerSchedule
    updateCrawlerSchedule_schedule,
    updateCrawlerSchedule_crawlerName,
    updateCrawlerScheduleResponse_httpStatus,

    -- ** UpdateDatabase
    updateDatabase_catalogId,
    updateDatabase_name,
    updateDatabase_databaseInput,
    updateDatabaseResponse_httpStatus,

    -- ** UpdateDevEndpoint
    updateDevEndpoint_deletePublicKeys,
    updateDevEndpoint_publicKey,
    updateDevEndpoint_addArguments,
    updateDevEndpoint_customLibraries,
    updateDevEndpoint_addPublicKeys,
    updateDevEndpoint_deleteArguments,
    updateDevEndpoint_updateEtlLibraries,
    updateDevEndpoint_endpointName,
    updateDevEndpointResponse_httpStatus,

    -- ** UpdateJob
    updateJob_jobName,
    updateJob_jobUpdate,
    updateJobResponse_jobName,
    updateJobResponse_httpStatus,

    -- ** UpdateJobFromSourceControl
    updateJobFromSourceControl_repositoryOwner,
    updateJobFromSourceControl_commitId,
    updateJobFromSourceControl_branchName,
    updateJobFromSourceControl_folder,
    updateJobFromSourceControl_jobName,
    updateJobFromSourceControl_repositoryName,
    updateJobFromSourceControl_authToken,
    updateJobFromSourceControl_provider,
    updateJobFromSourceControl_authStrategy,
    updateJobFromSourceControlResponse_jobName,
    updateJobFromSourceControlResponse_httpStatus,

    -- ** UpdateMLTransform
    updateMLTransform_timeout,
    updateMLTransform_name,
    updateMLTransform_numberOfWorkers,
    updateMLTransform_glueVersion,
    updateMLTransform_workerType,
    updateMLTransform_description,
    updateMLTransform_maxRetries,
    updateMLTransform_role,
    updateMLTransform_maxCapacity,
    updateMLTransform_parameters,
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
    updateRegistryResponse_registryName,
    updateRegistryResponse_registryArn,
    updateRegistryResponse_httpStatus,

    -- ** UpdateSchema
    updateSchema_compatibility,
    updateSchema_description,
    updateSchema_schemaVersionNumber,
    updateSchema_schemaId,
    updateSchemaResponse_registryName,
    updateSchemaResponse_schemaName,
    updateSchemaResponse_schemaArn,
    updateSchemaResponse_httpStatus,

    -- ** UpdateSourceControlFromJob
    updateSourceControlFromJob_repositoryOwner,
    updateSourceControlFromJob_commitId,
    updateSourceControlFromJob_branchName,
    updateSourceControlFromJob_folder,
    updateSourceControlFromJob_jobName,
    updateSourceControlFromJob_repositoryName,
    updateSourceControlFromJob_authToken,
    updateSourceControlFromJob_provider,
    updateSourceControlFromJob_authStrategy,
    updateSourceControlFromJobResponse_jobName,
    updateSourceControlFromJobResponse_httpStatus,

    -- ** UpdateTable
    updateTable_skipArchive,
    updateTable_catalogId,
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
    updateWorkflow_maxConcurrentRuns,
    updateWorkflow_defaultRunProperties,
    updateWorkflow_description,
    updateWorkflow_name,
    updateWorkflowResponse_name,
    updateWorkflowResponse_httpStatus,

    -- * Types

    -- ** Action
    action_securityConfiguration,
    action_timeout,
    action_jobName,
    action_notificationProperty,
    action_crawlerName,
    action_arguments,

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
    athenaConnectorSource_outputSchemas,
    athenaConnectorSource_connectionTable,
    athenaConnectorSource_name,
    athenaConnectorSource_connectionName,
    athenaConnectorSource_connectorName,
    athenaConnectorSource_connectionType,
    athenaConnectorSource_schemaName,

    -- ** AuditContext
    auditContext_requestedColumns,
    auditContext_additionalAuditContext,
    auditContext_allColumnsRequested,

    -- ** BackfillError
    backfillError_code,
    backfillError_partitions,

    -- ** BasicCatalogTarget
    basicCatalogTarget_name,
    basicCatalogTarget_inputs,
    basicCatalogTarget_database,
    basicCatalogTarget_table,

    -- ** BatchStopJobRunError
    batchStopJobRunError_jobName,
    batchStopJobRunError_jobRunId,
    batchStopJobRunError_errorDetail,

    -- ** BatchStopJobRunSuccessfulSubmission
    batchStopJobRunSuccessfulSubmission_jobName,
    batchStopJobRunSuccessfulSubmission_jobRunId,

    -- ** BatchUpdatePartitionFailureEntry
    batchUpdatePartitionFailureEntry_partitionValueList,
    batchUpdatePartitionFailureEntry_errorDetail,

    -- ** BatchUpdatePartitionRequestEntry
    batchUpdatePartitionRequestEntry_partitionValueList,
    batchUpdatePartitionRequestEntry_partitionInput,

    -- ** BinaryColumnStatisticsData
    binaryColumnStatisticsData_maximumLength,
    binaryColumnStatisticsData_averageLength,
    binaryColumnStatisticsData_numberOfNulls,

    -- ** Blueprint
    blueprint_createdOn,
    blueprint_name,
    blueprint_lastModifiedOn,
    blueprint_errorMessage,
    blueprint_lastActiveDefinition,
    blueprint_status,
    blueprint_parameterSpec,
    blueprint_description,
    blueprint_blueprintLocation,
    blueprint_blueprintServiceLocation,

    -- ** BlueprintDetails
    blueprintDetails_blueprintName,
    blueprintDetails_runId,

    -- ** BlueprintRun
    blueprintRun_roleArn,
    blueprintRun_startedOn,
    blueprintRun_errorMessage,
    blueprintRun_workflowName,
    blueprintRun_state,
    blueprintRun_completedOn,
    blueprintRun_blueprintName,
    blueprintRun_rollbackErrorMessage,
    blueprintRun_runId,
    blueprintRun_parameters,

    -- ** BooleanColumnStatisticsData
    booleanColumnStatisticsData_numberOfTrues,
    booleanColumnStatisticsData_numberOfFalses,
    booleanColumnStatisticsData_numberOfNulls,

    -- ** CatalogEntry
    catalogEntry_databaseName,
    catalogEntry_tableName,

    -- ** CatalogImportStatus
    catalogImportStatus_importCompleted,
    catalogImportStatus_importedBy,
    catalogImportStatus_importTime,

    -- ** CatalogKafkaSource
    catalogKafkaSource_windowSize,
    catalogKafkaSource_streamingOptions,
    catalogKafkaSource_detectSchema,
    catalogKafkaSource_dataPreviewOptions,
    catalogKafkaSource_name,
    catalogKafkaSource_table,
    catalogKafkaSource_database,

    -- ** CatalogKinesisSource
    catalogKinesisSource_windowSize,
    catalogKinesisSource_streamingOptions,
    catalogKinesisSource_detectSchema,
    catalogKinesisSource_dataPreviewOptions,
    catalogKinesisSource_name,
    catalogKinesisSource_table,
    catalogKinesisSource_database,

    -- ** CatalogSchemaChangePolicy
    catalogSchemaChangePolicy_updateBehavior,
    catalogSchemaChangePolicy_enableUpdateCatalog,

    -- ** CatalogSource
    catalogSource_name,
    catalogSource_database,
    catalogSource_table,

    -- ** CatalogTarget
    catalogTarget_dlqEventQueueArn,
    catalogTarget_eventQueueArn,
    catalogTarget_connectionName,
    catalogTarget_databaseName,
    catalogTarget_tables,

    -- ** Classifier
    classifier_csvClassifier,
    classifier_xMLClassifier,
    classifier_grokClassifier,
    classifier_jsonClassifier,

    -- ** CloudWatchEncryption
    cloudWatchEncryption_kmsKeyArn,
    cloudWatchEncryption_cloudWatchEncryptionMode,

    -- ** CodeGenConfigurationNode
    codeGenConfigurationNode_selectFields,
    codeGenConfigurationNode_s3CatalogSource,
    codeGenConfigurationNode_merge,
    codeGenConfigurationNode_dropDuplicates,
    codeGenConfigurationNode_oracleSQLCatalogTarget,
    codeGenConfigurationNode_applyMapping,
    codeGenConfigurationNode_postgreSQLCatalogTarget,
    codeGenConfigurationNode_s3JsonSource,
    codeGenConfigurationNode_athenaConnectorSource,
    codeGenConfigurationNode_jDBCConnectorSource,
    codeGenConfigurationNode_spigot,
    codeGenConfigurationNode_microsoftSQLServerCatalogTarget,
    codeGenConfigurationNode_redshiftSource,
    codeGenConfigurationNode_sparkConnectorTarget,
    codeGenConfigurationNode_aggregate,
    codeGenConfigurationNode_s3ParquetSource,
    codeGenConfigurationNode_renameField,
    codeGenConfigurationNode_pIIDetection,
    codeGenConfigurationNode_fillMissingValues,
    codeGenConfigurationNode_directKinesisSource,
    codeGenConfigurationNode_sparkConnectorSource,
    codeGenConfigurationNode_directKafkaSource,
    codeGenConfigurationNode_s3CsvSource,
    codeGenConfigurationNode_s3DirectTarget,
    codeGenConfigurationNode_governedCatalogTarget,
    codeGenConfigurationNode_union,
    codeGenConfigurationNode_relationalCatalogSource,
    codeGenConfigurationNode_postgreSQLCatalogSource,
    codeGenConfigurationNode_mySQLCatalogSource,
    codeGenConfigurationNode_filter,
    codeGenConfigurationNode_governedCatalogSource,
    codeGenConfigurationNode_redshiftTarget,
    codeGenConfigurationNode_jDBCConnectorTarget,
    codeGenConfigurationNode_selectFromCollection,
    codeGenConfigurationNode_microsoftSQLServerCatalogSource,
    codeGenConfigurationNode_mySQLCatalogTarget,
    codeGenConfigurationNode_catalogKinesisSource,
    codeGenConfigurationNode_oracleSQLCatalogSource,
    codeGenConfigurationNode_s3GlueParquetTarget,
    codeGenConfigurationNode_sparkSQL,
    codeGenConfigurationNode_dropNullFields,
    codeGenConfigurationNode_catalogSource,
    codeGenConfigurationNode_join,
    codeGenConfigurationNode_dynamoDBCatalogSource,
    codeGenConfigurationNode_customCode,
    codeGenConfigurationNode_catalogTarget,
    codeGenConfigurationNode_splitFields,
    codeGenConfigurationNode_catalogKafkaSource,
    codeGenConfigurationNode_dropFields,
    codeGenConfigurationNode_s3CatalogTarget,

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
    column_type,
    column_comment,
    column_parameters,
    column_name,

    -- ** ColumnError
    columnError_columnName,
    columnError_error,

    -- ** ColumnImportance
    columnImportance_importance,
    columnImportance_columnName,

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
    columnStatisticsData_decimalColumnStatisticsData,
    columnStatisticsData_stringColumnStatisticsData,
    columnStatisticsData_booleanColumnStatisticsData,
    columnStatisticsData_doubleColumnStatisticsData,
    columnStatisticsData_longColumnStatisticsData,
    columnStatisticsData_dateColumnStatisticsData,
    columnStatisticsData_type,

    -- ** ColumnStatisticsError
    columnStatisticsError_columnStatistics,
    columnStatisticsError_error,

    -- ** Condition
    condition_logicalOperator,
    condition_crawlState,
    condition_jobName,
    condition_state,
    condition_crawlerName,

    -- ** ConfusionMatrix
    confusionMatrix_numFalseNegatives,
    confusionMatrix_numTruePositives,
    confusionMatrix_numFalsePositives,
    confusionMatrix_numTrueNegatives,

    -- ** Connection
    connection_name,
    connection_connectionType,
    connection_connectionProperties,
    connection_physicalConnectionRequirements,
    connection_lastUpdatedTime,
    connection_description,
    connection_matchCriteria,
    connection_creationTime,
    connection_lastUpdatedBy,

    -- ** ConnectionInput
    connectionInput_physicalConnectionRequirements,
    connectionInput_description,
    connectionInput_matchCriteria,
    connectionInput_name,
    connectionInput_connectionType,
    connectionInput_connectionProperties,

    -- ** ConnectionPasswordEncryption
    connectionPasswordEncryption_awsKmsKeyId,
    connectionPasswordEncryption_returnConnectionPasswordEncrypted,

    -- ** ConnectionsList
    connectionsList_connections,

    -- ** Crawl
    crawl_logGroup,
    crawl_startedOn,
    crawl_logStream,
    crawl_errorMessage,
    crawl_state,
    crawl_completedOn,

    -- ** Crawler
    crawler_schedule,
    crawler_name,
    crawler_recrawlPolicy,
    crawler_classifiers,
    crawler_schemaChangePolicy,
    crawler_databaseName,
    crawler_configuration,
    crawler_state,
    crawler_tablePrefix,
    crawler_targets,
    crawler_description,
    crawler_lastUpdated,
    crawler_crawlElapsedTime,
    crawler_lineageConfiguration,
    crawler_role,
    crawler_creationTime,
    crawler_lastCrawl,
    crawler_crawlerSecurityConfiguration,
    crawler_lakeFormationConfiguration,
    crawler_version,

    -- ** CrawlerHistory
    crawlerHistory_logGroup,
    crawlerHistory_dPUHour,
    crawlerHistory_logStream,
    crawlerHistory_errorMessage,
    crawlerHistory_state,
    crawlerHistory_summary,
    crawlerHistory_endTime,
    crawlerHistory_messagePrefix,
    crawlerHistory_crawlId,
    crawlerHistory_startTime,

    -- ** CrawlerMetrics
    crawlerMetrics_medianRuntimeSeconds,
    crawlerMetrics_lastRuntimeSeconds,
    crawlerMetrics_stillEstimating,
    crawlerMetrics_tablesUpdated,
    crawlerMetrics_crawlerName,
    crawlerMetrics_tablesCreated,
    crawlerMetrics_tablesDeleted,
    crawlerMetrics_timeLeftSeconds,

    -- ** CrawlerNodeDetails
    crawlerNodeDetails_crawls,

    -- ** CrawlerTargets
    crawlerTargets_mongoDBTargets,
    crawlerTargets_dynamoDBTargets,
    crawlerTargets_deltaTargets,
    crawlerTargets_jdbcTargets,
    crawlerTargets_s3Targets,
    crawlerTargets_catalogTargets,

    -- ** CrawlsFilter
    crawlsFilter_fieldValue,
    crawlsFilter_fieldName,
    crawlsFilter_filterOperator,

    -- ** CreateCsvClassifierRequest
    createCsvClassifierRequest_quoteSymbol,
    createCsvClassifierRequest_header,
    createCsvClassifierRequest_containsHeader,
    createCsvClassifierRequest_disableValueTrimming,
    createCsvClassifierRequest_allowSingleColumn,
    createCsvClassifierRequest_delimiter,
    createCsvClassifierRequest_customDatatypeConfigured,
    createCsvClassifierRequest_customDatatypes,
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
    csvClassifier_quoteSymbol,
    csvClassifier_header,
    csvClassifier_containsHeader,
    csvClassifier_disableValueTrimming,
    csvClassifier_allowSingleColumn,
    csvClassifier_lastUpdated,
    csvClassifier_delimiter,
    csvClassifier_customDatatypeConfigured,
    csvClassifier_creationTime,
    csvClassifier_customDatatypes,
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

    -- ** DataCatalogEncryptionSettings
    dataCatalogEncryptionSettings_connectionPasswordEncryption,
    dataCatalogEncryptionSettings_encryptionAtRest,

    -- ** DataLakePrincipal
    dataLakePrincipal_dataLakePrincipalIdentifier,

    -- ** Database
    database_targetDatabase,
    database_description,
    database_catalogId,
    database_locationUri,
    database_createTime,
    database_createTableDefaultPermissions,
    database_parameters,
    database_name,

    -- ** DatabaseIdentifier
    databaseIdentifier_databaseName,
    databaseIdentifier_catalogId,

    -- ** DatabaseInput
    databaseInput_targetDatabase,
    databaseInput_description,
    databaseInput_locationUri,
    databaseInput_createTableDefaultPermissions,
    databaseInput_parameters,
    databaseInput_name,

    -- ** Datatype
    datatype_id,
    datatype_label,

    -- ** DateColumnStatisticsData
    dateColumnStatisticsData_minimumValue,
    dateColumnStatisticsData_maximumValue,
    dateColumnStatisticsData_numberOfNulls,
    dateColumnStatisticsData_numberOfDistinctValues,

    -- ** DecimalColumnStatisticsData
    decimalColumnStatisticsData_minimumValue,
    decimalColumnStatisticsData_maximumValue,
    decimalColumnStatisticsData_numberOfNulls,
    decimalColumnStatisticsData_numberOfDistinctValues,

    -- ** DecimalNumber
    decimalNumber_unscaledValue,
    decimalNumber_scale,

    -- ** DeltaTarget
    deltaTarget_deltaTables,
    deltaTarget_writeManifest,
    deltaTarget_connectionName,

    -- ** DevEndpoint
    devEndpoint_securityConfiguration,
    devEndpoint_zeppelinRemoteSparkInterpreterPort,
    devEndpoint_roleArn,
    devEndpoint_publicKey,
    devEndpoint_numberOfWorkers,
    devEndpoint_securityGroupIds,
    devEndpoint_endpointName,
    devEndpoint_glueVersion,
    devEndpoint_createdTimestamp,
    devEndpoint_lastModifiedTimestamp,
    devEndpoint_subnetId,
    devEndpoint_workerType,
    devEndpoint_publicAddress,
    devEndpoint_status,
    devEndpoint_availabilityZone,
    devEndpoint_extraJarsS3Path,
    devEndpoint_privateAddress,
    devEndpoint_numberOfNodes,
    devEndpoint_arguments,
    devEndpoint_lastUpdateStatus,
    devEndpoint_yarnEndpointAddress,
    devEndpoint_vpcId,
    devEndpoint_extraPythonLibsS3Path,
    devEndpoint_failureReason,
    devEndpoint_publicKeys,

    -- ** DevEndpointCustomLibraries
    devEndpointCustomLibraries_extraJarsS3Path,
    devEndpointCustomLibraries_extraPythonLibsS3Path,

    -- ** DirectKafkaSource
    directKafkaSource_windowSize,
    directKafkaSource_streamingOptions,
    directKafkaSource_detectSchema,
    directKafkaSource_dataPreviewOptions,
    directKafkaSource_name,

    -- ** DirectKinesisSource
    directKinesisSource_windowSize,
    directKinesisSource_streamingOptions,
    directKinesisSource_detectSchema,
    directKinesisSource_dataPreviewOptions,
    directKinesisSource_name,

    -- ** DirectSchemaChangePolicy
    directSchemaChangePolicy_database,
    directSchemaChangePolicy_updateBehavior,
    directSchemaChangePolicy_enableUpdateCatalog,
    directSchemaChangePolicy_table,

    -- ** DoubleColumnStatisticsData
    doubleColumnStatisticsData_minimumValue,
    doubleColumnStatisticsData_maximumValue,
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

    -- ** DynamoDBCatalogSource
    dynamoDBCatalogSource_name,
    dynamoDBCatalogSource_database,
    dynamoDBCatalogSource_table,

    -- ** DynamoDBTarget
    dynamoDBTarget_scanAll,
    dynamoDBTarget_path,
    dynamoDBTarget_scanRate,

    -- ** Edge
    edge_sourceId,
    edge_destinationId,

    -- ** EncryptionAtRest
    encryptionAtRest_sseAwsKmsKeyId,
    encryptionAtRest_catalogEncryptionMode,

    -- ** EncryptionConfiguration
    encryptionConfiguration_s3Encryption,
    encryptionConfiguration_cloudWatchEncryption,
    encryptionConfiguration_jobBookmarksEncryption,

    -- ** ErrorDetail
    errorDetail_errorMessage,
    errorDetail_errorCode,

    -- ** ErrorDetails
    errorDetails_errorMessage,
    errorDetails_errorCode,

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
    findMatchesMetrics_f1,
    findMatchesMetrics_columnImportances,
    findMatchesMetrics_recall,
    findMatchesMetrics_confusionMatrix,
    findMatchesMetrics_precision,
    findMatchesMetrics_areaUnderPRCurve,

    -- ** FindMatchesParameters
    findMatchesParameters_accuracyCostTradeoff,
    findMatchesParameters_primaryKeyColumnName,
    findMatchesParameters_precisionRecallTradeoff,
    findMatchesParameters_enforceProvidedLabels,

    -- ** FindMatchesTaskRunProperties
    findMatchesTaskRunProperties_jobName,
    findMatchesTaskRunProperties_jobRunId,
    findMatchesTaskRunProperties_jobId,

    -- ** GetConnectionsFilter
    getConnectionsFilter_connectionType,
    getConnectionsFilter_matchCriteria,

    -- ** GluePolicy
    gluePolicy_policyInJson,
    gluePolicy_updateTime,
    gluePolicy_createTime,
    gluePolicy_policyHash,

    -- ** GlueSchema
    glueSchema_columns,

    -- ** GlueStudioSchemaColumn
    glueStudioSchemaColumn_type,
    glueStudioSchemaColumn_name,

    -- ** GlueTable
    glueTable_catalogId,
    glueTable_connectionName,
    glueTable_databaseName,
    glueTable_tableName,

    -- ** GovernedCatalogSource
    governedCatalogSource_partitionPredicate,
    governedCatalogSource_additionalOptions,
    governedCatalogSource_name,
    governedCatalogSource_database,
    governedCatalogSource_table,

    -- ** GovernedCatalogTarget
    governedCatalogTarget_schemaChangePolicy,
    governedCatalogTarget_partitionKeys,
    governedCatalogTarget_name,
    governedCatalogTarget_inputs,
    governedCatalogTarget_table,
    governedCatalogTarget_database,

    -- ** GrokClassifier
    grokClassifier_lastUpdated,
    grokClassifier_creationTime,
    grokClassifier_customPatterns,
    grokClassifier_version,
    grokClassifier_name,
    grokClassifier_classification,
    grokClassifier_grokPattern,

    -- ** ImportLabelsTaskRunProperties
    importLabelsTaskRunProperties_inputS3Path,
    importLabelsTaskRunProperties_replace,

    -- ** JDBCConnectorOptions
    jDBCConnectorOptions_jobBookmarkKeysSortOrder,
    jDBCConnectorOptions_dataTypeMapping,
    jDBCConnectorOptions_filterPredicate,
    jDBCConnectorOptions_numPartitions,
    jDBCConnectorOptions_lowerBound,
    jDBCConnectorOptions_jobBookmarkKeys,
    jDBCConnectorOptions_upperBound,
    jDBCConnectorOptions_partitionColumn,

    -- ** JDBCConnectorSource
    jDBCConnectorSource_outputSchemas,
    jDBCConnectorSource_additionalOptions,
    jDBCConnectorSource_query,
    jDBCConnectorSource_connectionTable,
    jDBCConnectorSource_name,
    jDBCConnectorSource_connectionName,
    jDBCConnectorSource_connectorName,
    jDBCConnectorSource_connectionType,

    -- ** JDBCConnectorTarget
    jDBCConnectorTarget_outputSchemas,
    jDBCConnectorTarget_additionalOptions,
    jDBCConnectorTarget_name,
    jDBCConnectorTarget_inputs,
    jDBCConnectorTarget_connectionName,
    jDBCConnectorTarget_connectionTable,
    jDBCConnectorTarget_connectorName,
    jDBCConnectorTarget_connectionType,

    -- ** JdbcTarget
    jdbcTarget_enableAdditionalMetadata,
    jdbcTarget_path,
    jdbcTarget_exclusions,
    jdbcTarget_connectionName,

    -- ** Job
    job_securityConfiguration,
    job_createdOn,
    job_timeout,
    job_name,
    job_lastModifiedOn,
    job_nonOverridableArguments,
    job_numberOfWorkers,
    job_glueVersion,
    job_notificationProperty,
    job_workerType,
    job_executionProperty,
    job_allocatedCapacity,
    job_command,
    job_description,
    job_maxRetries,
    job_codeGenConfigurationNodes,
    job_defaultArguments,
    job_sourceControlDetails,
    job_logUri,
    job_connections,
    job_role,
    job_maxCapacity,
    job_executionClass,

    -- ** JobBookmarkEntry
    jobBookmarkEntry_attempt,
    jobBookmarkEntry_jobName,
    jobBookmarkEntry_previousRunId,
    jobBookmarkEntry_jobBookmark,
    jobBookmarkEntry_run,
    jobBookmarkEntry_runId,
    jobBookmarkEntry_version,

    -- ** JobBookmarksEncryption
    jobBookmarksEncryption_jobBookmarksEncryptionMode,
    jobBookmarksEncryption_kmsKeyArn,

    -- ** JobCommand
    jobCommand_name,
    jobCommand_scriptLocation,
    jobCommand_pythonVersion,

    -- ** JobNodeDetails
    jobNodeDetails_jobRuns,

    -- ** JobRun
    jobRun_securityConfiguration,
    jobRun_timeout,
    jobRun_triggerName,
    jobRun_lastModifiedOn,
    jobRun_numberOfWorkers,
    jobRun_startedOn,
    jobRun_errorMessage,
    jobRun_attempt,
    jobRun_glueVersion,
    jobRun_jobName,
    jobRun_previousRunId,
    jobRun_notificationProperty,
    jobRun_workerType,
    jobRun_executionTime,
    jobRun_allocatedCapacity,
    jobRun_predecessorRuns,
    jobRun_id,
    jobRun_completedOn,
    jobRun_arguments,
    jobRun_jobRunState,
    jobRun_dPUSeconds,
    jobRun_maxCapacity,
    jobRun_executionClass,
    jobRun_logGroupName,

    -- ** JobUpdate
    jobUpdate_securityConfiguration,
    jobUpdate_timeout,
    jobUpdate_nonOverridableArguments,
    jobUpdate_numberOfWorkers,
    jobUpdate_glueVersion,
    jobUpdate_notificationProperty,
    jobUpdate_workerType,
    jobUpdate_executionProperty,
    jobUpdate_allocatedCapacity,
    jobUpdate_command,
    jobUpdate_description,
    jobUpdate_maxRetries,
    jobUpdate_codeGenConfigurationNodes,
    jobUpdate_defaultArguments,
    jobUpdate_sourceControlDetails,
    jobUpdate_logUri,
    jobUpdate_connections,
    jobUpdate_role,
    jobUpdate_maxCapacity,
    jobUpdate_executionClass,

    -- ** Join
    join_name,
    join_inputs,
    join_joinType,
    join_columns,

    -- ** JoinColumn
    joinColumn_from,
    joinColumn_keys,

    -- ** JsonClassifier
    jsonClassifier_lastUpdated,
    jsonClassifier_creationTime,
    jsonClassifier_version,
    jsonClassifier_name,
    jsonClassifier_jsonPath,

    -- ** KafkaStreamingSourceOptions
    kafkaStreamingSourceOptions_maxOffsetsPerTrigger,
    kafkaStreamingSourceOptions_numRetries,
    kafkaStreamingSourceOptions_subscribePattern,
    kafkaStreamingSourceOptions_retryIntervalMs,
    kafkaStreamingSourceOptions_endingOffsets,
    kafkaStreamingSourceOptions_minPartitions,
    kafkaStreamingSourceOptions_startingOffsets,
    kafkaStreamingSourceOptions_delimiter,
    kafkaStreamingSourceOptions_topicName,
    kafkaStreamingSourceOptions_securityProtocol,
    kafkaStreamingSourceOptions_bootstrapServers,
    kafkaStreamingSourceOptions_pollTimeoutMs,
    kafkaStreamingSourceOptions_connectionName,
    kafkaStreamingSourceOptions_assign,
    kafkaStreamingSourceOptions_classification,

    -- ** KeySchemaElement
    keySchemaElement_name,
    keySchemaElement_type,

    -- ** KinesisStreamingSourceOptions
    kinesisStreamingSourceOptions_roleArn,
    kinesisStreamingSourceOptions_startingPosition,
    kinesisStreamingSourceOptions_numRetries,
    kinesisStreamingSourceOptions_idleTimeBetweenReadsInMs,
    kinesisStreamingSourceOptions_retryIntervalMs,
    kinesisStreamingSourceOptions_maxRetryIntervalMs,
    kinesisStreamingSourceOptions_avoidEmptyBatches,
    kinesisStreamingSourceOptions_describeShardInterval,
    kinesisStreamingSourceOptions_maxRecordPerRead,
    kinesisStreamingSourceOptions_maxFetchTimeInMs,
    kinesisStreamingSourceOptions_delimiter,
    kinesisStreamingSourceOptions_endpointUrl,
    kinesisStreamingSourceOptions_maxFetchRecordsPerShard,
    kinesisStreamingSourceOptions_roleSessionName,
    kinesisStreamingSourceOptions_addIdleTimeBetweenReads,
    kinesisStreamingSourceOptions_streamArn,
    kinesisStreamingSourceOptions_streamName,
    kinesisStreamingSourceOptions_classification,

    -- ** LabelingSetGenerationTaskRunProperties
    labelingSetGenerationTaskRunProperties_outputS3Path,

    -- ** LakeFormationConfiguration
    lakeFormationConfiguration_useLakeFormationCredentials,
    lakeFormationConfiguration_accountId,

    -- ** LastActiveDefinition
    lastActiveDefinition_lastModifiedOn,
    lastActiveDefinition_parameterSpec,
    lastActiveDefinition_description,
    lastActiveDefinition_blueprintLocation,
    lastActiveDefinition_blueprintServiceLocation,

    -- ** LastCrawlInfo
    lastCrawlInfo_logGroup,
    lastCrawlInfo_logStream,
    lastCrawlInfo_errorMessage,
    lastCrawlInfo_status,
    lastCrawlInfo_messagePrefix,
    lastCrawlInfo_startTime,

    -- ** LineageConfiguration
    lineageConfiguration_crawlerLineageSettings,

    -- ** Location
    location_s3,
    location_dynamoDB,
    location_jdbc,

    -- ** LongColumnStatisticsData
    longColumnStatisticsData_minimumValue,
    longColumnStatisticsData_maximumValue,
    longColumnStatisticsData_numberOfNulls,
    longColumnStatisticsData_numberOfDistinctValues,

    -- ** MLTransform
    mLTransform_evaluationMetrics,
    mLTransform_createdOn,
    mLTransform_timeout,
    mLTransform_name,
    mLTransform_lastModifiedOn,
    mLTransform_numberOfWorkers,
    mLTransform_glueVersion,
    mLTransform_workerType,
    mLTransform_inputRecordTables,
    mLTransform_status,
    mLTransform_labelCount,
    mLTransform_description,
    mLTransform_maxRetries,
    mLTransform_transformId,
    mLTransform_transformEncryption,
    mLTransform_schema,
    mLTransform_role,
    mLTransform_maxCapacity,
    mLTransform_parameters,

    -- ** MLUserDataEncryption
    mLUserDataEncryption_kmsKeyId,
    mLUserDataEncryption_mlUserDataEncryptionMode,

    -- ** Mapping
    mapping_toType,
    mapping_fromType,
    mapping_dropped,
    mapping_fromPath,
    mapping_children,
    mapping_toKey,

    -- ** MappingEntry
    mappingEntry_targetTable,
    mappingEntry_sourceTable,
    mappingEntry_targetPath,
    mappingEntry_sourceType,
    mappingEntry_targetType,
    mappingEntry_sourcePath,

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
    metadataKeyValuePair_metadataValue,
    metadataKeyValuePair_metadataKey,

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
    mongoDBTarget_scanAll,
    mongoDBTarget_path,
    mongoDBTarget_connectionName,

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
    node_name,
    node_type,
    node_jobDetails,
    node_triggerDetails,
    node_uniqueId,
    node_crawlerDetails,

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
    pIIDetection_sampleFraction,
    pIIDetection_outputColumnName,
    pIIDetection_thresholdFraction,
    pIIDetection_name,
    pIIDetection_inputs,
    pIIDetection_piiType,
    pIIDetection_entityTypesToDetect,

    -- ** Partition
    partition_tableName,
    partition_lastAccessTime,
    partition_databaseName,
    partition_catalogId,
    partition_creationTime,
    partition_storageDescriptor,
    partition_values,
    partition_lastAnalyzedTime,
    partition_parameters,

    -- ** PartitionError
    partitionError_partitionValues,
    partitionError_errorDetail,

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
    partitionInput_storageDescriptor,
    partitionInput_values,
    partitionInput_lastAnalyzedTime,
    partitionInput_parameters,

    -- ** PartitionValueList
    partitionValueList_values,

    -- ** PhysicalConnectionRequirements
    physicalConnectionRequirements_subnetId,
    physicalConnectionRequirements_availabilityZone,
    physicalConnectionRequirements_securityGroupIdList,

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
    predicate_logical,
    predicate_conditions,

    -- ** PrincipalPermissions
    principalPermissions_principal,
    principalPermissions_permissions,

    -- ** PropertyPredicate
    propertyPredicate_key,
    propertyPredicate_comparator,
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
    redshiftTarget_upsertRedshiftOptions,
    redshiftTarget_redshiftTmpDir,
    redshiftTarget_tmpDirIAMRole,
    redshiftTarget_name,
    redshiftTarget_inputs,
    redshiftTarget_database,
    redshiftTarget_table,

    -- ** RegistryId
    registryId_registryName,
    registryId_registryArn,

    -- ** RegistryListItem
    registryListItem_registryName,
    registryListItem_createdTime,
    registryListItem_status,
    registryListItem_description,
    registryListItem_registryArn,
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
    s3CatalogSource_partitionPredicate,
    s3CatalogSource_additionalOptions,
    s3CatalogSource_name,
    s3CatalogSource_database,
    s3CatalogSource_table,

    -- ** S3CatalogTarget
    s3CatalogTarget_schemaChangePolicy,
    s3CatalogTarget_partitionKeys,
    s3CatalogTarget_name,
    s3CatalogTarget_inputs,
    s3CatalogTarget_table,
    s3CatalogTarget_database,

    -- ** S3CsvSource
    s3CsvSource_outputSchemas,
    s3CsvSource_groupFiles,
    s3CsvSource_maxBand,
    s3CsvSource_writeHeader,
    s3CsvSource_maxFilesInBand,
    s3CsvSource_optimizePerformance,
    s3CsvSource_recurse,
    s3CsvSource_additionalOptions,
    s3CsvSource_compressionType,
    s3CsvSource_skipFirst,
    s3CsvSource_withHeader,
    s3CsvSource_multiline,
    s3CsvSource_exclusions,
    s3CsvSource_groupSize,
    s3CsvSource_escaper,
    s3CsvSource_name,
    s3CsvSource_paths,
    s3CsvSource_separator,
    s3CsvSource_quoteChar,

    -- ** S3DirectSourceAdditionalOptions
    s3DirectSourceAdditionalOptions_samplePath,
    s3DirectSourceAdditionalOptions_enableSamplePath,
    s3DirectSourceAdditionalOptions_boundedSize,
    s3DirectSourceAdditionalOptions_boundedFiles,

    -- ** S3DirectTarget
    s3DirectTarget_compression,
    s3DirectTarget_schemaChangePolicy,
    s3DirectTarget_partitionKeys,
    s3DirectTarget_name,
    s3DirectTarget_inputs,
    s3DirectTarget_path,
    s3DirectTarget_format,

    -- ** S3Encryption
    s3Encryption_s3EncryptionMode,
    s3Encryption_kmsKeyArn,

    -- ** S3GlueParquetTarget
    s3GlueParquetTarget_compression,
    s3GlueParquetTarget_schemaChangePolicy,
    s3GlueParquetTarget_partitionKeys,
    s3GlueParquetTarget_name,
    s3GlueParquetTarget_inputs,
    s3GlueParquetTarget_path,

    -- ** S3JsonSource
    s3JsonSource_outputSchemas,
    s3JsonSource_groupFiles,
    s3JsonSource_maxBand,
    s3JsonSource_maxFilesInBand,
    s3JsonSource_recurse,
    s3JsonSource_jsonPath,
    s3JsonSource_additionalOptions,
    s3JsonSource_compressionType,
    s3JsonSource_multiline,
    s3JsonSource_exclusions,
    s3JsonSource_groupSize,
    s3JsonSource_name,
    s3JsonSource_paths,

    -- ** S3ParquetSource
    s3ParquetSource_outputSchemas,
    s3ParquetSource_groupFiles,
    s3ParquetSource_maxBand,
    s3ParquetSource_maxFilesInBand,
    s3ParquetSource_recurse,
    s3ParquetSource_additionalOptions,
    s3ParquetSource_compressionType,
    s3ParquetSource_exclusions,
    s3ParquetSource_groupSize,
    s3ParquetSource_name,
    s3ParquetSource_paths,

    -- ** S3SourceAdditionalOptions
    s3SourceAdditionalOptions_boundedSize,
    s3SourceAdditionalOptions_boundedFiles,

    -- ** S3Target
    s3Target_dlqEventQueueArn,
    s3Target_path,
    s3Target_sampleSize,
    s3Target_exclusions,
    s3Target_eventQueueArn,
    s3Target_connectionName,

    -- ** Schedule
    schedule_state,
    schedule_scheduleExpression,

    -- ** SchemaChangePolicy
    schemaChangePolicy_updateBehavior,
    schemaChangePolicy_deleteBehavior,

    -- ** SchemaColumn
    schemaColumn_name,
    schemaColumn_dataType,

    -- ** SchemaId
    schemaId_registryName,
    schemaId_schemaName,
    schemaId_schemaArn,

    -- ** SchemaListItem
    schemaListItem_registryName,
    schemaListItem_createdTime,
    schemaListItem_schemaStatus,
    schemaListItem_schemaName,
    schemaListItem_description,
    schemaListItem_schemaArn,
    schemaListItem_updatedTime,

    -- ** SchemaReference
    schemaReference_schemaVersionNumber,
    schemaReference_schemaVersionId,
    schemaReference_schemaId,

    -- ** SchemaVersionErrorItem
    schemaVersionErrorItem_errorDetails,
    schemaVersionErrorItem_versionNumber,

    -- ** SchemaVersionListItem
    schemaVersionListItem_createdTime,
    schemaVersionListItem_status,
    schemaVersionListItem_schemaArn,
    schemaVersionListItem_versionNumber,
    schemaVersionListItem_schemaVersionId,

    -- ** SchemaVersionNumber
    schemaVersionNumber_latestVersion,
    schemaVersionNumber_versionNumber,

    -- ** SecurityConfiguration
    securityConfiguration_name,
    securityConfiguration_createdTimeStamp,
    securityConfiguration_encryptionConfiguration,

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
    serDeInfo_serializationLibrary,
    serDeInfo_parameters,

    -- ** Session
    session_securityConfiguration,
    session_progress,
    session_createdOn,
    session_errorMessage,
    session_glueVersion,
    session_command,
    session_status,
    session_id,
    session_description,
    session_defaultArguments,
    session_connections,
    session_role,
    session_maxCapacity,

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
    sourceControlDetails_branch,
    sourceControlDetails_folder,
    sourceControlDetails_repository,
    sourceControlDetails_authToken,
    sourceControlDetails_provider,
    sourceControlDetails_owner,
    sourceControlDetails_lastCommitId,
    sourceControlDetails_authStrategy,

    -- ** SparkConnectorSource
    sparkConnectorSource_outputSchemas,
    sparkConnectorSource_additionalOptions,
    sparkConnectorSource_name,
    sparkConnectorSource_connectionName,
    sparkConnectorSource_connectorName,
    sparkConnectorSource_connectionType,

    -- ** SparkConnectorTarget
    sparkConnectorTarget_outputSchemas,
    sparkConnectorTarget_additionalOptions,
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
    startingEventBatchCondition_batchWindow,
    startingEventBatchCondition_batchSize,

    -- ** Statement
    statement_progress,
    statement_code,
    statement_startedOn,
    statement_state,
    statement_id,
    statement_completedOn,
    statement_output,

    -- ** StatementOutput
    statementOutput_status,
    statementOutput_errorValue,
    statementOutput_executionCount,
    statementOutput_errorName,
    statementOutput_traceback,
    statementOutput_data,

    -- ** StatementOutputData
    statementOutputData_textPlain,

    -- ** StorageDescriptor
    storageDescriptor_sortColumns,
    storageDescriptor_storedAsSubDirectories,
    storageDescriptor_skewedInfo,
    storageDescriptor_columns,
    storageDescriptor_bucketColumns,
    storageDescriptor_serdeInfo,
    storageDescriptor_outputFormat,
    storageDescriptor_location,
    storageDescriptor_compressed,
    storageDescriptor_additionalLocations,
    storageDescriptor_schemaReference,
    storageDescriptor_inputFormat,
    storageDescriptor_numberOfBuckets,
    storageDescriptor_parameters,

    -- ** StreamingDataPreviewOptions
    streamingDataPreviewOptions_pollingTime,
    streamingDataPreviewOptions_recordPollingLimit,

    -- ** StringColumnStatisticsData
    stringColumnStatisticsData_maximumLength,
    stringColumnStatisticsData_averageLength,
    stringColumnStatisticsData_numberOfNulls,
    stringColumnStatisticsData_numberOfDistinctValues,

    -- ** Table
    table_targetTable,
    table_lastAccessTime,
    table_databaseName,
    table_viewOriginalText,
    table_owner,
    table_viewExpandedText,
    table_description,
    table_partitionKeys,
    table_isRegisteredWithLakeFormation,
    table_catalogId,
    table_tableType,
    table_storageDescriptor,
    table_updateTime,
    table_createTime,
    table_retention,
    table_createdBy,
    table_lastAnalyzedTime,
    table_versionId,
    table_parameters,
    table_name,

    -- ** TableError
    tableError_tableName,
    tableError_errorDetail,

    -- ** TableIdentifier
    tableIdentifier_name,
    tableIdentifier_databaseName,
    tableIdentifier_catalogId,

    -- ** TableInput
    tableInput_targetTable,
    tableInput_lastAccessTime,
    tableInput_viewOriginalText,
    tableInput_owner,
    tableInput_viewExpandedText,
    tableInput_description,
    tableInput_partitionKeys,
    tableInput_tableType,
    tableInput_storageDescriptor,
    tableInput_retention,
    tableInput_lastAnalyzedTime,
    tableInput_parameters,
    tableInput_name,

    -- ** TableVersion
    tableVersion_table,
    tableVersion_versionId,

    -- ** TableVersionError
    tableVersionError_tableName,
    tableVersionError_errorDetail,
    tableVersionError_versionId,

    -- ** TaskRun
    taskRun_lastModifiedOn,
    taskRun_startedOn,
    taskRun_properties,
    taskRun_executionTime,
    taskRun_status,
    taskRun_transformId,
    taskRun_completedOn,
    taskRun_taskRunId,
    taskRun_errorString,
    taskRun_logGroupName,

    -- ** TaskRunFilterCriteria
    taskRunFilterCriteria_startedBefore,
    taskRunFilterCriteria_status,
    taskRunFilterCriteria_startedAfter,
    taskRunFilterCriteria_taskRunType,

    -- ** TaskRunProperties
    taskRunProperties_importLabelsTaskRunProperties,
    taskRunProperties_findMatchesTaskRunProperties,
    taskRunProperties_taskType,
    taskRunProperties_labelingSetGenerationTaskRunProperties,
    taskRunProperties_exportLabelsTaskRunProperties,

    -- ** TaskRunSortCriteria
    taskRunSortCriteria_column,
    taskRunSortCriteria_sortDirection,

    -- ** TransformEncryption
    transformEncryption_taskRunSecurityConfigurationName,
    transformEncryption_mlUserDataEncryption,

    -- ** TransformFilterCriteria
    transformFilterCriteria_name,
    transformFilterCriteria_transformType,
    transformFilterCriteria_glueVersion,
    transformFilterCriteria_createdBefore,
    transformFilterCriteria_status,
    transformFilterCriteria_schema,
    transformFilterCriteria_createdAfter,
    transformFilterCriteria_lastModifiedAfter,
    transformFilterCriteria_lastModifiedBefore,

    -- ** TransformParameters
    transformParameters_findMatchesParameters,
    transformParameters_transformType,

    -- ** TransformSortCriteria
    transformSortCriteria_column,
    transformSortCriteria_sortDirection,

    -- ** Trigger
    trigger_eventBatchingCondition,
    trigger_schedule,
    trigger_name,
    trigger_type,
    trigger_workflowName,
    trigger_predicate,
    trigger_state,
    trigger_id,
    trigger_description,
    trigger_actions,

    -- ** TriggerNodeDetails
    triggerNodeDetails_trigger,

    -- ** TriggerUpdate
    triggerUpdate_eventBatchingCondition,
    triggerUpdate_schedule,
    triggerUpdate_name,
    triggerUpdate_predicate,
    triggerUpdate_description,
    triggerUpdate_actions,

    -- ** UnfilteredPartition
    unfilteredPartition_authorizedColumns,
    unfilteredPartition_partition,
    unfilteredPartition_isRegisteredWithLakeFormation,

    -- ** Union
    union_name,
    union_inputs,
    union_unionType,

    -- ** UpdateCsvClassifierRequest
    updateCsvClassifierRequest_quoteSymbol,
    updateCsvClassifierRequest_header,
    updateCsvClassifierRequest_containsHeader,
    updateCsvClassifierRequest_disableValueTrimming,
    updateCsvClassifierRequest_allowSingleColumn,
    updateCsvClassifierRequest_delimiter,
    updateCsvClassifierRequest_customDatatypeConfigured,
    updateCsvClassifierRequest_customDatatypes,
    updateCsvClassifierRequest_name,

    -- ** UpdateGrokClassifierRequest
    updateGrokClassifierRequest_customPatterns,
    updateGrokClassifierRequest_grokPattern,
    updateGrokClassifierRequest_classification,
    updateGrokClassifierRequest_name,

    -- ** UpdateJsonClassifierRequest
    updateJsonClassifierRequest_jsonPath,
    updateJsonClassifierRequest_name,

    -- ** UpdateXMLClassifierRequest
    updateXMLClassifierRequest_rowTag,
    updateXMLClassifierRequest_classification,
    updateXMLClassifierRequest_name,

    -- ** UpsertRedshiftTargetOptions
    upsertRedshiftTargetOptions_tableLocation,
    upsertRedshiftTargetOptions_upsertKeys,
    upsertRedshiftTargetOptions_connectionName,

    -- ** UserDefinedFunction
    userDefinedFunction_ownerType,
    userDefinedFunction_ownerName,
    userDefinedFunction_databaseName,
    userDefinedFunction_resourceUris,
    userDefinedFunction_functionName,
    userDefinedFunction_className,
    userDefinedFunction_catalogId,
    userDefinedFunction_createTime,

    -- ** UserDefinedFunctionInput
    userDefinedFunctionInput_ownerType,
    userDefinedFunctionInput_ownerName,
    userDefinedFunctionInput_resourceUris,
    userDefinedFunctionInput_functionName,
    userDefinedFunctionInput_className,

    -- ** Workflow
    workflow_lastRun,
    workflow_createdOn,
    workflow_name,
    workflow_lastModifiedOn,
    workflow_maxConcurrentRuns,
    workflow_graph,
    workflow_defaultRunProperties,
    workflow_description,
    workflow_blueprintDetails,

    -- ** WorkflowGraph
    workflowGraph_edges,
    workflowGraph_nodes,

    -- ** WorkflowRun
    workflowRun_name,
    workflowRun_startedOn,
    workflowRun_graph,
    workflowRun_errorMessage,
    workflowRun_statistics,
    workflowRun_workflowRunProperties,
    workflowRun_previousRunId,
    workflowRun_startingEventBatchCondition,
    workflowRun_status,
    workflowRun_completedOn,
    workflowRun_workflowRunId,

    -- ** WorkflowRunStatistics
    workflowRunStatistics_timeoutActions,
    workflowRunStatistics_waitingActions,
    workflowRunStatistics_succeededActions,
    workflowRunStatistics_totalActions,
    workflowRunStatistics_stoppedActions,
    workflowRunStatistics_failedActions,
    workflowRunStatistics_runningActions,
    workflowRunStatistics_erroredActions,

    -- ** XMLClassifier
    xMLClassifier_rowTag,
    xMLClassifier_lastUpdated,
    xMLClassifier_creationTime,
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
import Amazonka.Glue.BatchGetDevEndpoints
import Amazonka.Glue.BatchGetJobs
import Amazonka.Glue.BatchGetPartition
import Amazonka.Glue.BatchGetTriggers
import Amazonka.Glue.BatchGetWorkflows
import Amazonka.Glue.BatchStopJobRun
import Amazonka.Glue.BatchUpdatePartition
import Amazonka.Glue.CancelMLTaskRun
import Amazonka.Glue.CancelStatement
import Amazonka.Glue.CheckSchemaVersionValidity
import Amazonka.Glue.CreateBlueprint
import Amazonka.Glue.CreateClassifier
import Amazonka.Glue.CreateConnection
import Amazonka.Glue.CreateCrawler
import Amazonka.Glue.CreateCustomEntityType
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
import Amazonka.Glue.Types.DataCatalogEncryptionSettings
import Amazonka.Glue.Types.DataLakePrincipal
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
import Amazonka.Glue.Types.DynamoDBCatalogSource
import Amazonka.Glue.Types.DynamoDBTarget
import Amazonka.Glue.Types.Edge
import Amazonka.Glue.Types.EncryptionAtRest
import Amazonka.Glue.Types.EncryptionConfiguration
import Amazonka.Glue.Types.ErrorDetail
import Amazonka.Glue.Types.ErrorDetails
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
