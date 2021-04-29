{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Lens
  ( -- * Operations

    -- ** GetDataCatalogEncryptionSettings
    getDataCatalogEncryptionSettings_catalogId,
    getDataCatalogEncryptionSettingsResponse_dataCatalogEncryptionSettings,
    getDataCatalogEncryptionSettingsResponse_httpStatus,

    -- ** UpdateColumnStatisticsForTable
    updateColumnStatisticsForTable_catalogId,
    updateColumnStatisticsForTable_databaseName,
    updateColumnStatisticsForTable_tableName,
    updateColumnStatisticsForTable_columnStatisticsList,
    updateColumnStatisticsForTableResponse_errors,
    updateColumnStatisticsForTableResponse_httpStatus,

    -- ** StartMLLabelingSetGenerationTaskRun
    startMLLabelingSetGenerationTaskRun_transformId,
    startMLLabelingSetGenerationTaskRun_outputS3Path,
    startMLLabelingSetGenerationTaskRunResponse_taskRunId,
    startMLLabelingSetGenerationTaskRunResponse_httpStatus,

    -- ** DeleteColumnStatisticsForTable
    deleteColumnStatisticsForTable_catalogId,
    deleteColumnStatisticsForTable_databaseName,
    deleteColumnStatisticsForTable_tableName,
    deleteColumnStatisticsForTable_columnName,
    deleteColumnStatisticsForTableResponse_httpStatus,

    -- ** GetSchema
    getSchema_schemaId,
    getSchemaResponse_schemaArn,
    getSchemaResponse_nextSchemaVersion,
    getSchemaResponse_schemaCheckpoint,
    getSchemaResponse_dataFormat,
    getSchemaResponse_updatedTime,
    getSchemaResponse_createdTime,
    getSchemaResponse_registryName,
    getSchemaResponse_schemaName,
    getSchemaResponse_description,
    getSchemaResponse_compatibility,
    getSchemaResponse_registryArn,
    getSchemaResponse_latestSchemaVersion,
    getSchemaResponse_schemaStatus,
    getSchemaResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_catalogId,
    deleteConnection_connectionName,
    deleteConnectionResponse_httpStatus,

    -- ** UpdateConnection
    updateConnection_catalogId,
    updateConnection_name,
    updateConnection_connectionInput,
    updateConnectionResponse_httpStatus,

    -- ** CheckSchemaVersionValidity
    checkSchemaVersionValidity_dataFormat,
    checkSchemaVersionValidity_schemaDefinition,
    checkSchemaVersionValidityResponse_valid,
    checkSchemaVersionValidityResponse_error,
    checkSchemaVersionValidityResponse_httpStatus,

    -- ** CreateWorkflow
    createWorkflow_defaultRunProperties,
    createWorkflow_maxConcurrentRuns,
    createWorkflow_tags,
    createWorkflow_description,
    createWorkflow_name,
    createWorkflowResponse_name,
    createWorkflowResponse_httpStatus,

    -- ** GetPartitions
    getPartitions_nextToken,
    getPartitions_catalogId,
    getPartitions_maxResults,
    getPartitions_segment,
    getPartitions_excludeColumnSchema,
    getPartitions_expression,
    getPartitions_databaseName,
    getPartitions_tableName,
    getPartitionsResponse_nextToken,
    getPartitionsResponse_partitions,
    getPartitionsResponse_httpStatus,

    -- ** DeleteSecurityConfiguration
    deleteSecurityConfiguration_name,
    deleteSecurityConfigurationResponse_httpStatus,

    -- ** GetPartition
    getPartition_catalogId,
    getPartition_databaseName,
    getPartition_tableName,
    getPartition_partitionValues,
    getPartitionResponse_partition,
    getPartitionResponse_httpStatus,

    -- ** UpdateRegistry
    updateRegistry_registryId,
    updateRegistry_description,
    updateRegistryResponse_registryName,
    updateRegistryResponse_registryArn,
    updateRegistryResponse_httpStatus,

    -- ** ListMLTransforms
    listMLTransforms_nextToken,
    listMLTransforms_maxResults,
    listMLTransforms_tags,
    listMLTransforms_filter,
    listMLTransforms_sort,
    listMLTransformsResponse_nextToken,
    listMLTransformsResponse_httpStatus,
    listMLTransformsResponse_transformIds,

    -- ** StopCrawler
    stopCrawler_name,
    stopCrawlerResponse_httpStatus,

    -- ** StartImportLabelsTaskRun
    startImportLabelsTaskRun_replaceAllLabels,
    startImportLabelsTaskRun_transformId,
    startImportLabelsTaskRun_inputS3Path,
    startImportLabelsTaskRunResponse_taskRunId,
    startImportLabelsTaskRunResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_resourceArn,
    getResourcePolicyResponse_policyInJson,
    getResourcePolicyResponse_updateTime,
    getResourcePolicyResponse_createTime,
    getResourcePolicyResponse_policyHash,
    getResourcePolicyResponse_httpStatus,

    -- ** QuerySchemaVersionMetadata
    querySchemaVersionMetadata_nextToken,
    querySchemaVersionMetadata_schemaVersionId,
    querySchemaVersionMetadata_maxResults,
    querySchemaVersionMetadata_schemaVersionNumber,
    querySchemaVersionMetadata_metadataList,
    querySchemaVersionMetadata_schemaId,
    querySchemaVersionMetadataResponse_metadataInfoMap,
    querySchemaVersionMetadataResponse_nextToken,
    querySchemaVersionMetadataResponse_schemaVersionId,
    querySchemaVersionMetadataResponse_httpStatus,

    -- ** DeleteRegistry
    deleteRegistry_registryId,
    deleteRegistryResponse_status,
    deleteRegistryResponse_registryName,
    deleteRegistryResponse_registryArn,
    deleteRegistryResponse_httpStatus,

    -- ** GetPartitionIndexes
    getPartitionIndexes_nextToken,
    getPartitionIndexes_catalogId,
    getPartitionIndexes_databaseName,
    getPartitionIndexes_tableName,
    getPartitionIndexesResponse_nextToken,
    getPartitionIndexesResponse_partitionIndexDescriptorList,
    getPartitionIndexesResponse_httpStatus,

    -- ** StartCrawler
    startCrawler_name,
    startCrawlerResponse_httpStatus,

    -- ** GetCatalogImportStatus
    getCatalogImportStatus_catalogId,
    getCatalogImportStatusResponse_importStatus,
    getCatalogImportStatusResponse_httpStatus,

    -- ** GetColumnStatisticsForPartition
    getColumnStatisticsForPartition_catalogId,
    getColumnStatisticsForPartition_databaseName,
    getColumnStatisticsForPartition_tableName,
    getColumnStatisticsForPartition_partitionValues,
    getColumnStatisticsForPartition_columnNames,
    getColumnStatisticsForPartitionResponse_columnStatisticsList,
    getColumnStatisticsForPartitionResponse_errors,
    getColumnStatisticsForPartitionResponse_httpStatus,

    -- ** CreateRegistry
    createRegistry_tags,
    createRegistry_description,
    createRegistry_registryName,
    createRegistryResponse_registryName,
    createRegistryResponse_tags,
    createRegistryResponse_description,
    createRegistryResponse_registryArn,
    createRegistryResponse_httpStatus,

    -- ** ListTriggers
    listTriggers_nextToken,
    listTriggers_maxResults,
    listTriggers_tags,
    listTriggers_dependentJobName,
    listTriggersResponse_nextToken,
    listTriggersResponse_triggerNames,
    listTriggersResponse_httpStatus,

    -- ** CreateMLTransform
    createMLTransform_transformEncryption,
    createMLTransform_timeout,
    createMLTransform_maxCapacity,
    createMLTransform_numberOfWorkers,
    createMLTransform_glueVersion,
    createMLTransform_tags,
    createMLTransform_workerType,
    createMLTransform_description,
    createMLTransform_maxRetries,
    createMLTransform_name,
    createMLTransform_inputRecordTables,
    createMLTransform_parameters,
    createMLTransform_role,
    createMLTransformResponse_transformId,
    createMLTransformResponse_httpStatus,

    -- ** StopCrawlerSchedule
    stopCrawlerSchedule_crawlerName,
    stopCrawlerScheduleResponse_httpStatus,

    -- ** UpdateTrigger
    updateTrigger_name,
    updateTrigger_triggerUpdate,
    updateTriggerResponse_trigger,
    updateTriggerResponse_httpStatus,

    -- ** GetSchemaByDefinition
    getSchemaByDefinition_schemaId,
    getSchemaByDefinition_schemaDefinition,
    getSchemaByDefinitionResponse_schemaArn,
    getSchemaByDefinitionResponse_status,
    getSchemaByDefinitionResponse_schemaVersionId,
    getSchemaByDefinitionResponse_dataFormat,
    getSchemaByDefinitionResponse_createdTime,
    getSchemaByDefinitionResponse_httpStatus,

    -- ** ListRegistries
    listRegistries_nextToken,
    listRegistries_maxResults,
    listRegistriesResponse_nextToken,
    listRegistriesResponse_registries,
    listRegistriesResponse_httpStatus,

    -- ** StartCrawlerSchedule
    startCrawlerSchedule_crawlerName,
    startCrawlerScheduleResponse_httpStatus,

    -- ** DeleteTrigger
    deleteTrigger_name,
    deleteTriggerResponse_name,
    deleteTriggerResponse_httpStatus,

    -- ** GetJob
    getJob_jobName,
    getJobResponse_job,
    getJobResponse_httpStatus,

    -- ** UpdateClassifier
    updateClassifier_xMLClassifier,
    updateClassifier_jsonClassifier,
    updateClassifier_csvClassifier,
    updateClassifier_grokClassifier,
    updateClassifierResponse_httpStatus,

    -- ** DeleteClassifier
    deleteClassifier_name,
    deleteClassifierResponse_httpStatus,

    -- ** DeleteJob
    deleteJob_jobName,
    deleteJobResponse_jobName,
    deleteJobResponse_httpStatus,

    -- ** UpdateJob
    updateJob_jobName,
    updateJob_jobUpdate,
    updateJobResponse_jobName,
    updateJobResponse_httpStatus,

    -- ** CreateUserDefinedFunction
    createUserDefinedFunction_catalogId,
    createUserDefinedFunction_databaseName,
    createUserDefinedFunction_functionInput,
    createUserDefinedFunctionResponse_httpStatus,

    -- ** GetTrigger
    getTrigger_name,
    getTriggerResponse_trigger,
    getTriggerResponse_httpStatus,

    -- ** BatchGetJobs
    batchGetJobs_jobNames,
    batchGetJobsResponse_jobsNotFound,
    batchGetJobsResponse_jobs,
    batchGetJobsResponse_httpStatus,

    -- ** CreateClassifier
    createClassifier_xMLClassifier,
    createClassifier_jsonClassifier,
    createClassifier_csvClassifier,
    createClassifier_grokClassifier,
    createClassifierResponse_httpStatus,

    -- ** GetSecurityConfigurations
    getSecurityConfigurations_nextToken,
    getSecurityConfigurations_maxResults,
    getSecurityConfigurationsResponse_nextToken,
    getSecurityConfigurationsResponse_securityConfigurations,
    getSecurityConfigurationsResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_resourceArn,
    putResourcePolicy_enableHybrid,
    putResourcePolicy_policyHashCondition,
    putResourcePolicy_policyExistsCondition,
    putResourcePolicy_policyInJson,
    putResourcePolicyResponse_policyHash,
    putResourcePolicyResponse_httpStatus,

    -- ** UpdatePartition
    updatePartition_catalogId,
    updatePartition_databaseName,
    updatePartition_tableName,
    updatePartition_partitionValueList,
    updatePartition_partitionInput,
    updatePartitionResponse_httpStatus,

    -- ** GetSchemaVersionsDiff
    getSchemaVersionsDiff_schemaId,
    getSchemaVersionsDiff_firstSchemaVersionNumber,
    getSchemaVersionsDiff_secondSchemaVersionNumber,
    getSchemaVersionsDiff_schemaDiffType,
    getSchemaVersionsDiffResponse_diff,
    getSchemaVersionsDiffResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagsToRemove,
    untagResourceResponse_httpStatus,

    -- ** BatchDeleteTable
    batchDeleteTable_catalogId,
    batchDeleteTable_databaseName,
    batchDeleteTable_tablesToDelete,
    batchDeleteTableResponse_errors,
    batchDeleteTableResponse_httpStatus,

    -- ** StartMLEvaluationTaskRun
    startMLEvaluationTaskRun_transformId,
    startMLEvaluationTaskRunResponse_taskRunId,
    startMLEvaluationTaskRunResponse_httpStatus,

    -- ** GetDatabase
    getDatabase_catalogId,
    getDatabase_name,
    getDatabaseResponse_database,
    getDatabaseResponse_httpStatus,

    -- ** DeletePartition
    deletePartition_catalogId,
    deletePartition_databaseName,
    deletePartition_tableName,
    deletePartition_partitionValues,
    deletePartitionResponse_httpStatus,

    -- ** GetJobRuns
    getJobRuns_nextToken,
    getJobRuns_maxResults,
    getJobRuns_jobName,
    getJobRunsResponse_nextToken,
    getJobRunsResponse_jobRuns,
    getJobRunsResponse_httpStatus,

    -- ** GetMLTransforms
    getMLTransforms_nextToken,
    getMLTransforms_maxResults,
    getMLTransforms_filter,
    getMLTransforms_sort,
    getMLTransformsResponse_nextToken,
    getMLTransformsResponse_httpStatus,
    getMLTransformsResponse_transforms,

    -- ** GetJobRun
    getJobRun_predecessorsIncluded,
    getJobRun_jobName,
    getJobRun_runId,
    getJobRunResponse_jobRun,
    getJobRunResponse_httpStatus,

    -- ** CreateDevEndpoint
    createDevEndpoint_securityGroupIds,
    createDevEndpoint_securityConfiguration,
    createDevEndpoint_publicKey,
    createDevEndpoint_extraPythonLibsS3Path,
    createDevEndpoint_numberOfWorkers,
    createDevEndpoint_glueVersion,
    createDevEndpoint_tags,
    createDevEndpoint_numberOfNodes,
    createDevEndpoint_workerType,
    createDevEndpoint_subnetId,
    createDevEndpoint_arguments,
    createDevEndpoint_publicKeys,
    createDevEndpoint_extraJarsS3Path,
    createDevEndpoint_endpointName,
    createDevEndpoint_roleArn,
    createDevEndpointResponse_securityGroupIds,
    createDevEndpointResponse_status,
    createDevEndpointResponse_endpointName,
    createDevEndpointResponse_roleArn,
    createDevEndpointResponse_yarnEndpointAddress,
    createDevEndpointResponse_securityConfiguration,
    createDevEndpointResponse_createdTimestamp,
    createDevEndpointResponse_extraPythonLibsS3Path,
    createDevEndpointResponse_numberOfWorkers,
    createDevEndpointResponse_zeppelinRemoteSparkInterpreterPort,
    createDevEndpointResponse_availabilityZone,
    createDevEndpointResponse_failureReason,
    createDevEndpointResponse_glueVersion,
    createDevEndpointResponse_numberOfNodes,
    createDevEndpointResponse_workerType,
    createDevEndpointResponse_subnetId,
    createDevEndpointResponse_vpcId,
    createDevEndpointResponse_arguments,
    createDevEndpointResponse_extraJarsS3Path,
    createDevEndpointResponse_httpStatus,

    -- ** CreatePartitionIndex
    createPartitionIndex_catalogId,
    createPartitionIndex_databaseName,
    createPartitionIndex_tableName,
    createPartitionIndex_partitionIndex,
    createPartitionIndexResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tagsToAdd,
    tagResourceResponse_httpStatus,

    -- ** GetSecurityConfiguration
    getSecurityConfiguration_name,
    getSecurityConfigurationResponse_securityConfiguration,
    getSecurityConfigurationResponse_httpStatus,

    -- ** CreateCrawler
    createCrawler_schemaChangePolicy,
    createCrawler_recrawlPolicy,
    createCrawler_classifiers,
    createCrawler_configuration,
    createCrawler_lineageConfiguration,
    createCrawler_tags,
    createCrawler_tablePrefix,
    createCrawler_description,
    createCrawler_schedule,
    createCrawler_crawlerSecurityConfiguration,
    createCrawler_databaseName,
    createCrawler_name,
    createCrawler_role,
    createCrawler_targets,
    createCrawlerResponse_httpStatus,

    -- ** GetMLTaskRuns
    getMLTaskRuns_nextToken,
    getMLTaskRuns_maxResults,
    getMLTaskRuns_filter,
    getMLTaskRuns_sort,
    getMLTaskRuns_transformId,
    getMLTaskRunsResponse_nextToken,
    getMLTaskRunsResponse_taskRuns,
    getMLTaskRunsResponse_httpStatus,

    -- ** ListCrawlers
    listCrawlers_nextToken,
    listCrawlers_maxResults,
    listCrawlers_tags,
    listCrawlersResponse_nextToken,
    listCrawlersResponse_crawlerNames,
    listCrawlersResponse_httpStatus,

    -- ** UpdateDevEndpoint
    updateDevEndpoint_publicKey,
    updateDevEndpoint_updateEtlLibraries,
    updateDevEndpoint_addPublicKeys,
    updateDevEndpoint_deletePublicKeys,
    updateDevEndpoint_addArguments,
    updateDevEndpoint_deleteArguments,
    updateDevEndpoint_customLibraries,
    updateDevEndpoint_endpointName,
    updateDevEndpointResponse_httpStatus,

    -- ** CreateSchema
    createSchema_schemaDefinition,
    createSchema_registryId,
    createSchema_tags,
    createSchema_description,
    createSchema_compatibility,
    createSchema_schemaName,
    createSchema_dataFormat,
    createSchemaResponse_schemaArn,
    createSchemaResponse_nextSchemaVersion,
    createSchemaResponse_schemaVersionId,
    createSchemaResponse_schemaCheckpoint,
    createSchemaResponse_dataFormat,
    createSchemaResponse_registryName,
    createSchemaResponse_schemaVersionStatus,
    createSchemaResponse_tags,
    createSchemaResponse_schemaName,
    createSchemaResponse_description,
    createSchemaResponse_compatibility,
    createSchemaResponse_registryArn,
    createSchemaResponse_latestSchemaVersion,
    createSchemaResponse_schemaStatus,
    createSchemaResponse_httpStatus,

    -- ** ListDevEndpoints
    listDevEndpoints_nextToken,
    listDevEndpoints_maxResults,
    listDevEndpoints_tags,
    listDevEndpointsResponse_nextToken,
    listDevEndpointsResponse_devEndpointNames,
    listDevEndpointsResponse_httpStatus,

    -- ** DeleteCrawler
    deleteCrawler_name,
    deleteCrawlerResponse_httpStatus,

    -- ** DeleteDevEndpoint
    deleteDevEndpoint_endpointName,
    deleteDevEndpointResponse_httpStatus,

    -- ** GetWorkflow
    getWorkflow_includeGraph,
    getWorkflow_name,
    getWorkflowResponse_workflow,
    getWorkflowResponse_httpStatus,

    -- ** GetSchemaVersion
    getSchemaVersion_schemaVersionId,
    getSchemaVersion_schemaVersionNumber,
    getSchemaVersion_schemaId,
    getSchemaVersionResponse_schemaArn,
    getSchemaVersionResponse_status,
    getSchemaVersionResponse_schemaDefinition,
    getSchemaVersionResponse_schemaVersionId,
    getSchemaVersionResponse_dataFormat,
    getSchemaVersionResponse_createdTime,
    getSchemaVersionResponse_versionNumber,
    getSchemaVersionResponse_httpStatus,

    -- ** UpdateCrawler
    updateCrawler_schemaChangePolicy,
    updateCrawler_recrawlPolicy,
    updateCrawler_classifiers,
    updateCrawler_configuration,
    updateCrawler_lineageConfiguration,
    updateCrawler_targets,
    updateCrawler_role,
    updateCrawler_tablePrefix,
    updateCrawler_description,
    updateCrawler_schedule,
    updateCrawler_crawlerSecurityConfiguration,
    updateCrawler_databaseName,
    updateCrawler_name,
    updateCrawlerResponse_httpStatus,

    -- ** DeleteWorkflow
    deleteWorkflow_name,
    deleteWorkflowResponse_name,
    deleteWorkflowResponse_httpStatus,

    -- ** RegisterSchemaVersion
    registerSchemaVersion_schemaId,
    registerSchemaVersion_schemaDefinition,
    registerSchemaVersionResponse_status,
    registerSchemaVersionResponse_schemaVersionId,
    registerSchemaVersionResponse_versionNumber,
    registerSchemaVersionResponse_httpStatus,

    -- ** GetMapping
    getMapping_sinks,
    getMapping_location,
    getMapping_source,
    getMappingResponse_httpStatus,
    getMappingResponse_mapping,

    -- ** StopWorkflowRun
    stopWorkflowRun_name,
    stopWorkflowRun_runId,
    stopWorkflowRunResponse_httpStatus,

    -- ** CreateConnection
    createConnection_catalogId,
    createConnection_connectionInput,
    createConnectionResponse_httpStatus,

    -- ** BatchCreatePartition
    batchCreatePartition_catalogId,
    batchCreatePartition_databaseName,
    batchCreatePartition_tableName,
    batchCreatePartition_partitionInputList,
    batchCreatePartitionResponse_errors,
    batchCreatePartitionResponse_httpStatus,

    -- ** CreateTable
    createTable_catalogId,
    createTable_partitionIndexes,
    createTable_databaseName,
    createTable_tableInput,
    createTableResponse_httpStatus,

    -- ** UpdateWorkflow
    updateWorkflow_defaultRunProperties,
    updateWorkflow_maxConcurrentRuns,
    updateWorkflow_description,
    updateWorkflow_name,
    updateWorkflowResponse_name,
    updateWorkflowResponse_httpStatus,

    -- ** GetClassifiers
    getClassifiers_nextToken,
    getClassifiers_maxResults,
    getClassifiersResponse_nextToken,
    getClassifiersResponse_classifiers,
    getClassifiersResponse_httpStatus,

    -- ** BatchStopJobRun
    batchStopJobRun_jobName,
    batchStopJobRun_jobRunIds,
    batchStopJobRunResponse_successfulSubmissions,
    batchStopJobRunResponse_errors,
    batchStopJobRunResponse_httpStatus,

    -- ** StartWorkflowRun
    startWorkflowRun_name,
    startWorkflowRunResponse_runId,
    startWorkflowRunResponse_httpStatus,

    -- ** ListWorkflows
    listWorkflows_nextToken,
    listWorkflows_maxResults,
    listWorkflowsResponse_nextToken,
    listWorkflowsResponse_workflows,
    listWorkflowsResponse_httpStatus,

    -- ** ListSchemaVersions
    listSchemaVersions_nextToken,
    listSchemaVersions_maxResults,
    listSchemaVersions_schemaId,
    listSchemaVersionsResponse_nextToken,
    listSchemaVersionsResponse_schemas,
    listSchemaVersionsResponse_httpStatus,

    -- ** BatchDeletePartition
    batchDeletePartition_catalogId,
    batchDeletePartition_databaseName,
    batchDeletePartition_tableName,
    batchDeletePartition_partitionsToDelete,
    batchDeletePartitionResponse_errors,
    batchDeletePartitionResponse_httpStatus,

    -- ** PutSchemaVersionMetadata
    putSchemaVersionMetadata_schemaVersionId,
    putSchemaVersionMetadata_schemaVersionNumber,
    putSchemaVersionMetadata_schemaId,
    putSchemaVersionMetadata_metadataKeyValue,
    putSchemaVersionMetadataResponse_schemaArn,
    putSchemaVersionMetadataResponse_latestVersion,
    putSchemaVersionMetadataResponse_schemaVersionId,
    putSchemaVersionMetadataResponse_metadataKey,
    putSchemaVersionMetadataResponse_registryName,
    putSchemaVersionMetadataResponse_versionNumber,
    putSchemaVersionMetadataResponse_schemaName,
    putSchemaVersionMetadataResponse_metadataValue,
    putSchemaVersionMetadataResponse_httpStatus,

    -- ** GetWorkflowRuns
    getWorkflowRuns_nextToken,
    getWorkflowRuns_maxResults,
    getWorkflowRuns_includeGraph,
    getWorkflowRuns_name,
    getWorkflowRunsResponse_nextToken,
    getWorkflowRunsResponse_runs,
    getWorkflowRunsResponse_httpStatus,

    -- ** GetTags
    getTags_resourceArn,
    getTagsResponse_tags,
    getTagsResponse_httpStatus,

    -- ** BatchUpdatePartition
    batchUpdatePartition_catalogId,
    batchUpdatePartition_databaseName,
    batchUpdatePartition_tableName,
    batchUpdatePartition_entries,
    batchUpdatePartitionResponse_errors,
    batchUpdatePartitionResponse_httpStatus,

    -- ** GetUserDefinedFunctions
    getUserDefinedFunctions_nextToken,
    getUserDefinedFunctions_catalogId,
    getUserDefinedFunctions_maxResults,
    getUserDefinedFunctions_databaseName,
    getUserDefinedFunctions_pattern,
    getUserDefinedFunctionsResponse_nextToken,
    getUserDefinedFunctionsResponse_userDefinedFunctions,
    getUserDefinedFunctionsResponse_httpStatus,

    -- ** UpdateTable
    updateTable_catalogId,
    updateTable_skipArchive,
    updateTable_databaseName,
    updateTable_tableInput,
    updateTableResponse_httpStatus,

    -- ** DeleteTable
    deleteTable_catalogId,
    deleteTable_databaseName,
    deleteTable_name,
    deleteTableResponse_httpStatus,

    -- ** DeleteDatabase
    deleteDatabase_catalogId,
    deleteDatabase_name,
    deleteDatabaseResponse_httpStatus,

    -- ** UpdateDatabase
    updateDatabase_catalogId,
    updateDatabase_name,
    updateDatabase_databaseInput,
    updateDatabaseResponse_httpStatus,

    -- ** GetUserDefinedFunction
    getUserDefinedFunction_catalogId,
    getUserDefinedFunction_databaseName,
    getUserDefinedFunction_functionName,
    getUserDefinedFunctionResponse_userDefinedFunction,
    getUserDefinedFunctionResponse_httpStatus,

    -- ** UpdateMLTransform
    updateMLTransform_timeout,
    updateMLTransform_maxCapacity,
    updateMLTransform_numberOfWorkers,
    updateMLTransform_name,
    updateMLTransform_role,
    updateMLTransform_glueVersion,
    updateMLTransform_workerType,
    updateMLTransform_description,
    updateMLTransform_parameters,
    updateMLTransform_maxRetries,
    updateMLTransform_transformId,
    updateMLTransformResponse_transformId,
    updateMLTransformResponse_httpStatus,

    -- ** GetWorkflowRun
    getWorkflowRun_includeGraph,
    getWorkflowRun_name,
    getWorkflowRun_runId,
    getWorkflowRunResponse_run,
    getWorkflowRunResponse_httpStatus,

    -- ** DeleteMLTransform
    deleteMLTransform_transformId,
    deleteMLTransformResponse_transformId,
    deleteMLTransformResponse_httpStatus,

    -- ** CreateTrigger
    createTrigger_workflowName,
    createTrigger_startOnCreation,
    createTrigger_predicate,
    createTrigger_tags,
    createTrigger_description,
    createTrigger_schedule,
    createTrigger_name,
    createTrigger_type,
    createTrigger_actions,
    createTriggerResponse_name,
    createTriggerResponse_httpStatus,

    -- ** CreateDatabase
    createDatabase_catalogId,
    createDatabase_databaseInput,
    createDatabaseResponse_httpStatus,

    -- ** GetClassifier
    getClassifier_name,
    getClassifierResponse_classifier,
    getClassifierResponse_httpStatus,

    -- ** DeleteSchemaVersions
    deleteSchemaVersions_schemaId,
    deleteSchemaVersions_versions,
    deleteSchemaVersionsResponse_schemaVersionErrors,
    deleteSchemaVersionsResponse_httpStatus,

    -- ** BatchGetTriggers
    batchGetTriggers_triggerNames,
    batchGetTriggersResponse_triggers,
    batchGetTriggersResponse_triggersNotFound,
    batchGetTriggersResponse_httpStatus,

    -- ** BatchDeleteTableVersion
    batchDeleteTableVersion_catalogId,
    batchDeleteTableVersion_databaseName,
    batchDeleteTableVersion_tableName,
    batchDeleteTableVersion_versionIds,
    batchDeleteTableVersionResponse_errors,
    batchDeleteTableVersionResponse_httpStatus,

    -- ** GetTableVersions
    getTableVersions_nextToken,
    getTableVersions_catalogId,
    getTableVersions_maxResults,
    getTableVersions_databaseName,
    getTableVersions_tableName,
    getTableVersionsResponse_nextToken,
    getTableVersionsResponse_tableVersions,
    getTableVersionsResponse_httpStatus,

    -- ** GetDevEndpoints
    getDevEndpoints_nextToken,
    getDevEndpoints_maxResults,
    getDevEndpointsResponse_nextToken,
    getDevEndpointsResponse_devEndpoints,
    getDevEndpointsResponse_httpStatus,

    -- ** GetCrawlers
    getCrawlers_nextToken,
    getCrawlers_maxResults,
    getCrawlersResponse_nextToken,
    getCrawlersResponse_crawlers,
    getCrawlersResponse_httpStatus,

    -- ** StartJobRun
    startJobRun_securityConfiguration,
    startJobRun_timeout,
    startJobRun_maxCapacity,
    startJobRun_notificationProperty,
    startJobRun_numberOfWorkers,
    startJobRun_workerType,
    startJobRun_jobRunId,
    startJobRun_arguments,
    startJobRun_allocatedCapacity,
    startJobRun_jobName,
    startJobRunResponse_jobRunId,
    startJobRunResponse_httpStatus,

    -- ** ImportCatalogToGlue
    importCatalogToGlue_catalogId,
    importCatalogToGlueResponse_httpStatus,

    -- ** CreatePartition
    createPartition_catalogId,
    createPartition_databaseName,
    createPartition_tableName,
    createPartition_partitionInput,
    createPartitionResponse_httpStatus,

    -- ** ResetJobBookmark
    resetJobBookmark_runId,
    resetJobBookmark_jobName,
    resetJobBookmarkResponse_jobBookmarkEntry,
    resetJobBookmarkResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_maxResults,
    listJobs_tags,
    listJobsResponse_nextToken,
    listJobsResponse_jobNames,
    listJobsResponse_httpStatus,

    -- ** BatchDeleteConnection
    batchDeleteConnection_catalogId,
    batchDeleteConnection_connectionNameList,
    batchDeleteConnectionResponse_succeeded,
    batchDeleteConnectionResponse_errors,
    batchDeleteConnectionResponse_httpStatus,

    -- ** GetTables
    getTables_nextToken,
    getTables_catalogId,
    getTables_maxResults,
    getTables_expression,
    getTables_databaseName,
    getTablesResponse_nextToken,
    getTablesResponse_tableList,
    getTablesResponse_httpStatus,

    -- ** DeleteColumnStatisticsForPartition
    deleteColumnStatisticsForPartition_catalogId,
    deleteColumnStatisticsForPartition_databaseName,
    deleteColumnStatisticsForPartition_tableName,
    deleteColumnStatisticsForPartition_partitionValues,
    deleteColumnStatisticsForPartition_columnName,
    deleteColumnStatisticsForPartitionResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicy_policyHashCondition,
    deleteResourcePolicyResponse_httpStatus,

    -- ** GetRegistry
    getRegistry_registryId,
    getRegistryResponse_status,
    getRegistryResponse_updatedTime,
    getRegistryResponse_createdTime,
    getRegistryResponse_registryName,
    getRegistryResponse_description,
    getRegistryResponse_registryArn,
    getRegistryResponse_httpStatus,

    -- ** ResumeWorkflowRun
    resumeWorkflowRun_name,
    resumeWorkflowRun_runId,
    resumeWorkflowRun_nodeIds,
    resumeWorkflowRunResponse_runId,
    resumeWorkflowRunResponse_nodeIds,
    resumeWorkflowRunResponse_httpStatus,

    -- ** CancelMLTaskRun
    cancelMLTaskRun_transformId,
    cancelMLTaskRun_taskRunId,
    cancelMLTaskRunResponse_status,
    cancelMLTaskRunResponse_transformId,
    cancelMLTaskRunResponse_taskRunId,
    cancelMLTaskRunResponse_httpStatus,

    -- ** CreateJob
    createJob_nonOverridableArguments,
    createJob_securityConfiguration,
    createJob_timeout,
    createJob_maxCapacity,
    createJob_connections,
    createJob_notificationProperty,
    createJob_numberOfWorkers,
    createJob_glueVersion,
    createJob_tags,
    createJob_workerType,
    createJob_description,
    createJob_defaultArguments,
    createJob_allocatedCapacity,
    createJob_executionProperty,
    createJob_maxRetries,
    createJob_logUri,
    createJob_name,
    createJob_role,
    createJob_command,
    createJobResponse_name,
    createJobResponse_httpStatus,

    -- ** SearchTables
    searchTables_nextToken,
    searchTables_sortCriteria,
    searchTables_catalogId,
    searchTables_maxResults,
    searchTables_searchText,
    searchTables_resourceShareType,
    searchTables_filters,
    searchTablesResponse_nextToken,
    searchTablesResponse_tableList,
    searchTablesResponse_httpStatus,

    -- ** UpdateUserDefinedFunction
    updateUserDefinedFunction_catalogId,
    updateUserDefinedFunction_databaseName,
    updateUserDefinedFunction_functionName,
    updateUserDefinedFunction_functionInput,
    updateUserDefinedFunctionResponse_httpStatus,

    -- ** UpdateColumnStatisticsForPartition
    updateColumnStatisticsForPartition_catalogId,
    updateColumnStatisticsForPartition_databaseName,
    updateColumnStatisticsForPartition_tableName,
    updateColumnStatisticsForPartition_partitionValues,
    updateColumnStatisticsForPartition_columnStatisticsList,
    updateColumnStatisticsForPartitionResponse_errors,
    updateColumnStatisticsForPartitionResponse_httpStatus,

    -- ** GetConnections
    getConnections_nextToken,
    getConnections_catalogId,
    getConnections_maxResults,
    getConnections_hidePassword,
    getConnections_filter,
    getConnectionsResponse_nextToken,
    getConnectionsResponse_connectionList,
    getConnectionsResponse_httpStatus,

    -- ** GetMLTransform
    getMLTransform_transformId,
    getMLTransformResponse_status,
    getMLTransformResponse_transformId,
    getMLTransformResponse_schema,
    getMLTransformResponse_createdOn,
    getMLTransformResponse_inputRecordTables,
    getMLTransformResponse_transformEncryption,
    getMLTransformResponse_timeout,
    getMLTransformResponse_maxCapacity,
    getMLTransformResponse_lastModifiedOn,
    getMLTransformResponse_numberOfWorkers,
    getMLTransformResponse_name,
    getMLTransformResponse_role,
    getMLTransformResponse_glueVersion,
    getMLTransformResponse_evaluationMetrics,
    getMLTransformResponse_workerType,
    getMLTransformResponse_description,
    getMLTransformResponse_labelCount,
    getMLTransformResponse_parameters,
    getMLTransformResponse_maxRetries,
    getMLTransformResponse_httpStatus,

    -- ** CreateScript
    createScript_dagNodes,
    createScript_language,
    createScript_dagEdges,
    createScriptResponse_pythonScript,
    createScriptResponse_scalaCode,
    createScriptResponse_httpStatus,

    -- ** GetMLTaskRun
    getMLTaskRun_transformId,
    getMLTaskRun_taskRunId,
    getMLTaskRunResponse_executionTime,
    getMLTaskRunResponse_status,
    getMLTaskRunResponse_transformId,
    getMLTaskRunResponse_taskRunId,
    getMLTaskRunResponse_errorString,
    getMLTaskRunResponse_lastModifiedOn,
    getMLTaskRunResponse_logGroupName,
    getMLTaskRunResponse_completedOn,
    getMLTaskRunResponse_properties,
    getMLTaskRunResponse_startedOn,
    getMLTaskRunResponse_httpStatus,

    -- ** DeleteUserDefinedFunction
    deleteUserDefinedFunction_catalogId,
    deleteUserDefinedFunction_databaseName,
    deleteUserDefinedFunction_functionName,
    deleteUserDefinedFunctionResponse_httpStatus,

    -- ** StartTrigger
    startTrigger_name,
    startTriggerResponse_name,
    startTriggerResponse_httpStatus,

    -- ** PutDataCatalogEncryptionSettings
    putDataCatalogEncryptionSettings_catalogId,
    putDataCatalogEncryptionSettings_dataCatalogEncryptionSettings,
    putDataCatalogEncryptionSettingsResponse_httpStatus,

    -- ** RemoveSchemaVersionMetadata
    removeSchemaVersionMetadata_schemaVersionId,
    removeSchemaVersionMetadata_schemaVersionNumber,
    removeSchemaVersionMetadata_schemaId,
    removeSchemaVersionMetadata_metadataKeyValue,
    removeSchemaVersionMetadataResponse_schemaArn,
    removeSchemaVersionMetadataResponse_latestVersion,
    removeSchemaVersionMetadataResponse_schemaVersionId,
    removeSchemaVersionMetadataResponse_metadataKey,
    removeSchemaVersionMetadataResponse_registryName,
    removeSchemaVersionMetadataResponse_versionNumber,
    removeSchemaVersionMetadataResponse_schemaName,
    removeSchemaVersionMetadataResponse_metadataValue,
    removeSchemaVersionMetadataResponse_httpStatus,

    -- ** BatchGetPartition
    batchGetPartition_catalogId,
    batchGetPartition_databaseName,
    batchGetPartition_tableName,
    batchGetPartition_partitionsToGet,
    batchGetPartitionResponse_partitions,
    batchGetPartitionResponse_unprocessedKeys,
    batchGetPartitionResponse_httpStatus,

    -- ** GetTable
    getTable_catalogId,
    getTable_databaseName,
    getTable_name,
    getTableResponse_table,
    getTableResponse_httpStatus,

    -- ** UpdateCrawlerSchedule
    updateCrawlerSchedule_schedule,
    updateCrawlerSchedule_crawlerName,
    updateCrawlerScheduleResponse_httpStatus,

    -- ** GetColumnStatisticsForTable
    getColumnStatisticsForTable_catalogId,
    getColumnStatisticsForTable_databaseName,
    getColumnStatisticsForTable_tableName,
    getColumnStatisticsForTable_columnNames,
    getColumnStatisticsForTableResponse_columnStatisticsList,
    getColumnStatisticsForTableResponse_errors,
    getColumnStatisticsForTableResponse_httpStatus,

    -- ** StopTrigger
    stopTrigger_name,
    stopTriggerResponse_name,
    stopTriggerResponse_httpStatus,

    -- ** ListSchemas
    listSchemas_nextToken,
    listSchemas_maxResults,
    listSchemas_registryId,
    listSchemasResponse_nextToken,
    listSchemasResponse_schemas,
    listSchemasResponse_httpStatus,

    -- ** GetConnection
    getConnection_catalogId,
    getConnection_hidePassword,
    getConnection_name,
    getConnectionResponse_connection,
    getConnectionResponse_httpStatus,

    -- ** GetDatabases
    getDatabases_nextToken,
    getDatabases_catalogId,
    getDatabases_maxResults,
    getDatabases_resourceShareType,
    getDatabasesResponse_nextToken,
    getDatabasesResponse_httpStatus,
    getDatabasesResponse_databaseList,

    -- ** DeleteSchema
    deleteSchema_schemaId,
    deleteSchemaResponse_schemaArn,
    deleteSchemaResponse_status,
    deleteSchemaResponse_schemaName,
    deleteSchemaResponse_httpStatus,

    -- ** UpdateSchema
    updateSchema_schemaVersionNumber,
    updateSchema_description,
    updateSchema_compatibility,
    updateSchema_schemaId,
    updateSchemaResponse_schemaArn,
    updateSchemaResponse_registryName,
    updateSchemaResponse_schemaName,
    updateSchemaResponse_httpStatus,

    -- ** GetDataflowGraph
    getDataflowGraph_pythonScript,
    getDataflowGraphResponse_dagNodes,
    getDataflowGraphResponse_dagEdges,
    getDataflowGraphResponse_httpStatus,

    -- ** BatchGetDevEndpoints
    batchGetDevEndpoints_devEndpointNames,
    batchGetDevEndpointsResponse_devEndpoints,
    batchGetDevEndpointsResponse_devEndpointsNotFound,
    batchGetDevEndpointsResponse_httpStatus,

    -- ** StartExportLabelsTaskRun
    startExportLabelsTaskRun_transformId,
    startExportLabelsTaskRun_outputS3Path,
    startExportLabelsTaskRunResponse_taskRunId,
    startExportLabelsTaskRunResponse_httpStatus,

    -- ** GetTriggers
    getTriggers_nextToken,
    getTriggers_maxResults,
    getTriggers_dependentJobName,
    getTriggersResponse_nextToken,
    getTriggersResponse_triggers,
    getTriggersResponse_httpStatus,

    -- ** BatchGetCrawlers
    batchGetCrawlers_crawlerNames,
    batchGetCrawlersResponse_crawlers,
    batchGetCrawlersResponse_crawlersNotFound,
    batchGetCrawlersResponse_httpStatus,

    -- ** GetPlan
    getPlan_additionalPlanOptionsMap,
    getPlan_sinks,
    getPlan_location,
    getPlan_language,
    getPlan_mapping,
    getPlan_source,
    getPlanResponse_pythonScript,
    getPlanResponse_scalaCode,
    getPlanResponse_httpStatus,

    -- ** GetCrawlerMetrics
    getCrawlerMetrics_nextToken,
    getCrawlerMetrics_crawlerNameList,
    getCrawlerMetrics_maxResults,
    getCrawlerMetricsResponse_nextToken,
    getCrawlerMetricsResponse_crawlerMetricsList,
    getCrawlerMetricsResponse_httpStatus,

    -- ** GetWorkflowRunProperties
    getWorkflowRunProperties_name,
    getWorkflowRunProperties_runId,
    getWorkflowRunPropertiesResponse_runProperties,
    getWorkflowRunPropertiesResponse_httpStatus,

    -- ** DeletePartitionIndex
    deletePartitionIndex_catalogId,
    deletePartitionIndex_databaseName,
    deletePartitionIndex_tableName,
    deletePartitionIndex_indexName,
    deletePartitionIndexResponse_httpStatus,

    -- ** GetJobBookmark
    getJobBookmark_runId,
    getJobBookmark_jobName,
    getJobBookmarkResponse_jobBookmarkEntry,
    getJobBookmarkResponse_httpStatus,

    -- ** DeleteTableVersion
    deleteTableVersion_catalogId,
    deleteTableVersion_databaseName,
    deleteTableVersion_tableName,
    deleteTableVersion_versionId,
    deleteTableVersionResponse_httpStatus,

    -- ** GetTableVersion
    getTableVersion_catalogId,
    getTableVersion_versionId,
    getTableVersion_databaseName,
    getTableVersion_tableName,
    getTableVersionResponse_tableVersion,
    getTableVersionResponse_httpStatus,

    -- ** PutWorkflowRunProperties
    putWorkflowRunProperties_name,
    putWorkflowRunProperties_runId,
    putWorkflowRunProperties_runProperties,
    putWorkflowRunPropertiesResponse_httpStatus,

    -- ** BatchGetWorkflows
    batchGetWorkflows_includeGraph,
    batchGetWorkflows_names,
    batchGetWorkflowsResponse_missingWorkflows,
    batchGetWorkflowsResponse_workflows,
    batchGetWorkflowsResponse_httpStatus,

    -- ** GetResourcePolicies
    getResourcePolicies_nextToken,
    getResourcePolicies_maxResults,
    getResourcePoliciesResponse_nextToken,
    getResourcePoliciesResponse_getResourcePoliciesResponseList,
    getResourcePoliciesResponse_httpStatus,

    -- ** GetJobs
    getJobs_nextToken,
    getJobs_maxResults,
    getJobsResponse_nextToken,
    getJobsResponse_jobs,
    getJobsResponse_httpStatus,

    -- ** GetDevEndpoint
    getDevEndpoint_endpointName,
    getDevEndpointResponse_devEndpoint,
    getDevEndpointResponse_httpStatus,

    -- ** GetCrawler
    getCrawler_name,
    getCrawlerResponse_crawler,
    getCrawlerResponse_httpStatus,

    -- ** CreateSecurityConfiguration
    createSecurityConfiguration_name,
    createSecurityConfiguration_encryptionConfiguration,
    createSecurityConfigurationResponse_createdTimestamp,
    createSecurityConfigurationResponse_name,
    createSecurityConfigurationResponse_httpStatus,

    -- * Types

    -- ** Action
    action_securityConfiguration,
    action_crawlerName,
    action_timeout,
    action_notificationProperty,
    action_jobName,
    action_arguments,

    -- ** BackfillError
    backfillError_partitions,
    backfillError_code,

    -- ** BatchStopJobRunError
    batchStopJobRunError_errorDetail,
    batchStopJobRunError_jobRunId,
    batchStopJobRunError_jobName,

    -- ** BatchStopJobRunSuccessfulSubmission
    batchStopJobRunSuccessfulSubmission_jobRunId,
    batchStopJobRunSuccessfulSubmission_jobName,

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

    -- ** BooleanColumnStatisticsData
    booleanColumnStatisticsData_numberOfTrues,
    booleanColumnStatisticsData_numberOfFalses,
    booleanColumnStatisticsData_numberOfNulls,

    -- ** CatalogEntry
    catalogEntry_databaseName,
    catalogEntry_tableName,

    -- ** CatalogImportStatus
    catalogImportStatus_importedBy,
    catalogImportStatus_importCompleted,
    catalogImportStatus_importTime,

    -- ** CatalogTarget
    catalogTarget_databaseName,
    catalogTarget_tables,

    -- ** Classifier
    classifier_xMLClassifier,
    classifier_jsonClassifier,
    classifier_csvClassifier,
    classifier_grokClassifier,

    -- ** CloudWatchEncryption
    cloudWatchEncryption_cloudWatchEncryptionMode,
    cloudWatchEncryption_kmsKeyArn,

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
    column_type,
    column_parameters,
    column_name,

    -- ** ColumnError
    columnError_columnName,
    columnError_error,

    -- ** ColumnImportance
    columnImportance_importance,
    columnImportance_columnName,

    -- ** ColumnStatistics
    columnStatistics_columnName,
    columnStatistics_columnType,
    columnStatistics_analyzedTime,
    columnStatistics_statisticsData,

    -- ** ColumnStatisticsData
    columnStatisticsData_dateColumnStatisticsData,
    columnStatisticsData_binaryColumnStatisticsData,
    columnStatisticsData_booleanColumnStatisticsData,
    columnStatisticsData_longColumnStatisticsData,
    columnStatisticsData_stringColumnStatisticsData,
    columnStatisticsData_doubleColumnStatisticsData,
    columnStatisticsData_decimalColumnStatisticsData,
    columnStatisticsData_type,

    -- ** ColumnStatisticsError
    columnStatisticsError_columnStatistics,
    columnStatisticsError_error,

    -- ** Condition
    condition_crawlState,
    condition_crawlerName,
    condition_state,
    condition_logicalOperator,
    condition_jobName,

    -- ** ConfusionMatrix
    confusionMatrix_numFalsePositives,
    confusionMatrix_numTrueNegatives,
    confusionMatrix_numFalseNegatives,
    confusionMatrix_numTruePositives,

    -- ** Connection
    connection_connectionProperties,
    connection_creationTime,
    connection_connectionType,
    connection_physicalConnectionRequirements,
    connection_name,
    connection_lastUpdatedBy,
    connection_description,
    connection_matchCriteria,
    connection_lastUpdatedTime,

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
    crawl_state,
    crawl_completedOn,
    crawl_errorMessage,
    crawl_startedOn,
    crawl_logStream,

    -- ** Crawler
    crawler_schemaChangePolicy,
    crawler_recrawlPolicy,
    crawler_classifiers,
    crawler_creationTime,
    crawler_configuration,
    crawler_lineageConfiguration,
    crawler_version,
    crawler_targets,
    crawler_lastUpdated,
    crawler_state,
    crawler_name,
    crawler_crawlElapsedTime,
    crawler_role,
    crawler_lastCrawl,
    crawler_tablePrefix,
    crawler_description,
    crawler_schedule,
    crawler_crawlerSecurityConfiguration,
    crawler_databaseName,

    -- ** CrawlerMetrics
    crawlerMetrics_crawlerName,
    crawlerMetrics_tablesDeleted,
    crawlerMetrics_tablesUpdated,
    crawlerMetrics_tablesCreated,
    crawlerMetrics_medianRuntimeSeconds,
    crawlerMetrics_stillEstimating,
    crawlerMetrics_timeLeftSeconds,
    crawlerMetrics_lastRuntimeSeconds,

    -- ** CrawlerNodeDetails
    crawlerNodeDetails_crawls,

    -- ** CrawlerTargets
    crawlerTargets_catalogTargets,
    crawlerTargets_mongoDBTargets,
    crawlerTargets_dynamoDBTargets,
    crawlerTargets_jdbcTargets,
    crawlerTargets_s3Targets,

    -- ** CreateCsvClassifierRequest
    createCsvClassifierRequest_containsHeader,
    createCsvClassifierRequest_delimiter,
    createCsvClassifierRequest_disableValueTrimming,
    createCsvClassifierRequest_header,
    createCsvClassifierRequest_quoteSymbol,
    createCsvClassifierRequest_allowSingleColumn,
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
    csvClassifier_creationTime,
    csvClassifier_containsHeader,
    csvClassifier_delimiter,
    csvClassifier_disableValueTrimming,
    csvClassifier_version,
    csvClassifier_lastUpdated,
    csvClassifier_header,
    csvClassifier_quoteSymbol,
    csvClassifier_allowSingleColumn,
    csvClassifier_name,

    -- ** DataCatalogEncryptionSettings
    dataCatalogEncryptionSettings_encryptionAtRest,
    dataCatalogEncryptionSettings_connectionPasswordEncryption,

    -- ** DataLakePrincipal
    dataLakePrincipal_dataLakePrincipalIdentifier,

    -- ** Database
    database_createTableDefaultPermissions,
    database_catalogId,
    database_targetDatabase,
    database_createTime,
    database_description,
    database_locationUri,
    database_parameters,
    database_name,

    -- ** DatabaseIdentifier
    databaseIdentifier_catalogId,
    databaseIdentifier_databaseName,

    -- ** DatabaseInput
    databaseInput_createTableDefaultPermissions,
    databaseInput_targetDatabase,
    databaseInput_description,
    databaseInput_locationUri,
    databaseInput_parameters,
    databaseInput_name,

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

    -- ** DevEndpoint
    devEndpoint_securityGroupIds,
    devEndpoint_lastUpdateStatus,
    devEndpoint_publicAddress,
    devEndpoint_status,
    devEndpoint_endpointName,
    devEndpoint_roleArn,
    devEndpoint_yarnEndpointAddress,
    devEndpoint_securityConfiguration,
    devEndpoint_publicKey,
    devEndpoint_createdTimestamp,
    devEndpoint_privateAddress,
    devEndpoint_lastModifiedTimestamp,
    devEndpoint_extraPythonLibsS3Path,
    devEndpoint_numberOfWorkers,
    devEndpoint_zeppelinRemoteSparkInterpreterPort,
    devEndpoint_availabilityZone,
    devEndpoint_failureReason,
    devEndpoint_glueVersion,
    devEndpoint_numberOfNodes,
    devEndpoint_workerType,
    devEndpoint_subnetId,
    devEndpoint_vpcId,
    devEndpoint_arguments,
    devEndpoint_publicKeys,
    devEndpoint_extraJarsS3Path,

    -- ** DevEndpointCustomLibraries
    devEndpointCustomLibraries_extraPythonLibsS3Path,
    devEndpointCustomLibraries_extraJarsS3Path,

    -- ** DoubleColumnStatisticsData
    doubleColumnStatisticsData_maximumValue,
    doubleColumnStatisticsData_minimumValue,
    doubleColumnStatisticsData_numberOfNulls,
    doubleColumnStatisticsData_numberOfDistinctValues,

    -- ** DynamoDBTarget
    dynamoDBTarget_scanAll,
    dynamoDBTarget_scanRate,
    dynamoDBTarget_path,

    -- ** Edge
    edge_destinationId,
    edge_sourceId,

    -- ** EncryptionAtRest
    encryptionAtRest_sseAwsKmsKeyId,
    encryptionAtRest_catalogEncryptionMode,

    -- ** EncryptionConfiguration
    encryptionConfiguration_jobBookmarksEncryption,
    encryptionConfiguration_s3Encryption,
    encryptionConfiguration_cloudWatchEncryption,

    -- ** ErrorDetail
    errorDetail_errorMessage,
    errorDetail_errorCode,

    -- ** ErrorDetails
    errorDetails_errorMessage,
    errorDetails_errorCode,

    -- ** EvaluationMetrics
    evaluationMetrics_findMatchesMetrics,
    evaluationMetrics_transformType,

    -- ** ExecutionProperty
    executionProperty_maxConcurrentRuns,

    -- ** ExportLabelsTaskRunProperties
    exportLabelsTaskRunProperties_outputS3Path,

    -- ** FindMatchesMetrics
    findMatchesMetrics_f1,
    findMatchesMetrics_confusionMatrix,
    findMatchesMetrics_columnImportances,
    findMatchesMetrics_precision,
    findMatchesMetrics_areaUnderPRCurve,
    findMatchesMetrics_recall,

    -- ** FindMatchesParameters
    findMatchesParameters_accuracyCostTradeoff,
    findMatchesParameters_enforceProvidedLabels,
    findMatchesParameters_precisionRecallTradeoff,
    findMatchesParameters_primaryKeyColumnName,

    -- ** FindMatchesTaskRunProperties
    findMatchesTaskRunProperties_jobRunId,
    findMatchesTaskRunProperties_jobName,
    findMatchesTaskRunProperties_jobId,

    -- ** GetConnectionsFilter
    getConnectionsFilter_connectionType,
    getConnectionsFilter_matchCriteria,

    -- ** GluePolicy
    gluePolicy_policyInJson,
    gluePolicy_updateTime,
    gluePolicy_createTime,
    gluePolicy_policyHash,

    -- ** GlueTable
    glueTable_connectionName,
    glueTable_catalogId,
    glueTable_databaseName,
    glueTable_tableName,

    -- ** GrokClassifier
    grokClassifier_creationTime,
    grokClassifier_version,
    grokClassifier_lastUpdated,
    grokClassifier_customPatterns,
    grokClassifier_name,
    grokClassifier_classification,
    grokClassifier_grokPattern,

    -- ** ImportLabelsTaskRunProperties
    importLabelsTaskRunProperties_replace,
    importLabelsTaskRunProperties_inputS3Path,

    -- ** JdbcTarget
    jdbcTarget_connectionName,
    jdbcTarget_exclusions,
    jdbcTarget_path,

    -- ** Job
    job_nonOverridableArguments,
    job_createdOn,
    job_securityConfiguration,
    job_timeout,
    job_maxCapacity,
    job_connections,
    job_notificationProperty,
    job_lastModifiedOn,
    job_command,
    job_numberOfWorkers,
    job_name,
    job_role,
    job_glueVersion,
    job_workerType,
    job_description,
    job_defaultArguments,
    job_allocatedCapacity,
    job_executionProperty,
    job_maxRetries,
    job_logUri,

    -- ** JobBookmarkEntry
    jobBookmarkEntry_runId,
    jobBookmarkEntry_jobBookmark,
    jobBookmarkEntry_version,
    jobBookmarkEntry_run,
    jobBookmarkEntry_jobName,
    jobBookmarkEntry_previousRunId,
    jobBookmarkEntry_attempt,

    -- ** JobBookmarksEncryption
    jobBookmarksEncryption_jobBookmarksEncryptionMode,
    jobBookmarksEncryption_kmsKeyArn,

    -- ** JobCommand
    jobCommand_pythonVersion,
    jobCommand_scriptLocation,
    jobCommand_name,

    -- ** JobNodeDetails
    jobNodeDetails_jobRuns,

    -- ** JobRun
    jobRun_predecessorRuns,
    jobRun_executionTime,
    jobRun_securityConfiguration,
    jobRun_timeout,
    jobRun_maxCapacity,
    jobRun_id,
    jobRun_notificationProperty,
    jobRun_lastModifiedOn,
    jobRun_triggerName,
    jobRun_numberOfWorkers,
    jobRun_logGroupName,
    jobRun_completedOn,
    jobRun_glueVersion,
    jobRun_jobRunState,
    jobRun_workerType,
    jobRun_errorMessage,
    jobRun_startedOn,
    jobRun_jobName,
    jobRun_arguments,
    jobRun_allocatedCapacity,
    jobRun_previousRunId,
    jobRun_attempt,

    -- ** JobUpdate
    jobUpdate_nonOverridableArguments,
    jobUpdate_securityConfiguration,
    jobUpdate_timeout,
    jobUpdate_maxCapacity,
    jobUpdate_connections,
    jobUpdate_notificationProperty,
    jobUpdate_command,
    jobUpdate_numberOfWorkers,
    jobUpdate_role,
    jobUpdate_glueVersion,
    jobUpdate_workerType,
    jobUpdate_description,
    jobUpdate_defaultArguments,
    jobUpdate_allocatedCapacity,
    jobUpdate_executionProperty,
    jobUpdate_maxRetries,
    jobUpdate_logUri,

    -- ** JsonClassifier
    jsonClassifier_creationTime,
    jsonClassifier_version,
    jsonClassifier_lastUpdated,
    jsonClassifier_name,
    jsonClassifier_jsonPath,

    -- ** KeySchemaElement
    keySchemaElement_name,
    keySchemaElement_type,

    -- ** LabelingSetGenerationTaskRunProperties
    labelingSetGenerationTaskRunProperties_outputS3Path,

    -- ** LastCrawlInfo
    lastCrawlInfo_status,
    lastCrawlInfo_messagePrefix,
    lastCrawlInfo_logGroup,
    lastCrawlInfo_startTime,
    lastCrawlInfo_errorMessage,
    lastCrawlInfo_logStream,

    -- ** LineageConfiguration
    lineageConfiguration_crawlerLineageSettings,

    -- ** Location
    location_jdbc,
    location_dynamoDB,
    location_s3,

    -- ** LongColumnStatisticsData
    longColumnStatisticsData_maximumValue,
    longColumnStatisticsData_minimumValue,
    longColumnStatisticsData_numberOfNulls,
    longColumnStatisticsData_numberOfDistinctValues,

    -- ** MLTransform
    mLTransform_status,
    mLTransform_transformId,
    mLTransform_schema,
    mLTransform_createdOn,
    mLTransform_inputRecordTables,
    mLTransform_transformEncryption,
    mLTransform_timeout,
    mLTransform_maxCapacity,
    mLTransform_lastModifiedOn,
    mLTransform_numberOfWorkers,
    mLTransform_name,
    mLTransform_role,
    mLTransform_glueVersion,
    mLTransform_evaluationMetrics,
    mLTransform_workerType,
    mLTransform_description,
    mLTransform_labelCount,
    mLTransform_parameters,
    mLTransform_maxRetries,

    -- ** MLUserDataEncryption
    mLUserDataEncryption_kmsKeyId,
    mLUserDataEncryption_mlUserDataEncryptionMode,

    -- ** MappingEntry
    mappingEntry_targetType,
    mappingEntry_targetTable,
    mappingEntry_targetPath,
    mappingEntry_sourceTable,
    mappingEntry_sourcePath,
    mappingEntry_sourceType,

    -- ** MetadataInfo
    metadataInfo_createdTime,
    metadataInfo_metadataValue,

    -- ** MetadataKeyValuePair
    metadataKeyValuePair_metadataKey,
    metadataKeyValuePair_metadataValue,

    -- ** MongoDBTarget
    mongoDBTarget_connectionName,
    mongoDBTarget_scanAll,
    mongoDBTarget_path,

    -- ** Node
    node_jobDetails,
    node_triggerDetails,
    node_name,
    node_uniqueId,
    node_crawlerDetails,
    node_type,

    -- ** NotificationProperty
    notificationProperty_notifyDelayAfter,

    -- ** Order
    order_column,
    order_sortOrder,

    -- ** Partition
    partition_creationTime,
    partition_tableName,
    partition_catalogId,
    partition_values,
    partition_storageDescriptor,
    partition_lastAnalyzedTime,
    partition_lastAccessTime,
    partition_parameters,
    partition_databaseName,

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
    partitionInput_values,
    partitionInput_storageDescriptor,
    partitionInput_lastAnalyzedTime,
    partitionInput_lastAccessTime,
    partitionInput_parameters,

    -- ** PartitionValueList
    partitionValueList_values,

    -- ** PhysicalConnectionRequirements
    physicalConnectionRequirements_securityGroupIdList,
    physicalConnectionRequirements_availabilityZone,
    physicalConnectionRequirements_subnetId,

    -- ** Predecessor
    predecessor_runId,
    predecessor_jobName,

    -- ** Predicate
    predicate_logical,
    predicate_conditions,

    -- ** PrincipalPermissions
    principalPermissions_permissions,
    principalPermissions_principal,

    -- ** PropertyPredicate
    propertyPredicate_key,
    propertyPredicate_value,
    propertyPredicate_comparator,

    -- ** RecrawlPolicy
    recrawlPolicy_recrawlBehavior,

    -- ** RegistryId
    registryId_registryName,
    registryId_registryArn,

    -- ** RegistryListItem
    registryListItem_status,
    registryListItem_updatedTime,
    registryListItem_createdTime,
    registryListItem_registryName,
    registryListItem_description,
    registryListItem_registryArn,

    -- ** ResourceUri
    resourceUri_uri,
    resourceUri_resourceType,

    -- ** S3Encryption
    s3Encryption_s3EncryptionMode,
    s3Encryption_kmsKeyArn,

    -- ** S3Target
    s3Target_connectionName,
    s3Target_exclusions,
    s3Target_path,

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
    schemaId_schemaArn,
    schemaId_registryName,
    schemaId_schemaName,

    -- ** SchemaListItem
    schemaListItem_schemaArn,
    schemaListItem_updatedTime,
    schemaListItem_createdTime,
    schemaListItem_registryName,
    schemaListItem_schemaName,
    schemaListItem_description,
    schemaListItem_schemaStatus,

    -- ** SchemaReference
    schemaReference_schemaVersionId,
    schemaReference_schemaVersionNumber,
    schemaReference_schemaId,

    -- ** SchemaVersionErrorItem
    schemaVersionErrorItem_versionNumber,
    schemaVersionErrorItem_errorDetails,

    -- ** SchemaVersionListItem
    schemaVersionListItem_schemaArn,
    schemaVersionListItem_status,
    schemaVersionListItem_schemaVersionId,
    schemaVersionListItem_createdTime,
    schemaVersionListItem_versionNumber,

    -- ** SchemaVersionNumber
    schemaVersionNumber_latestVersion,
    schemaVersionNumber_versionNumber,

    -- ** SecurityConfiguration
    securityConfiguration_encryptionConfiguration,
    securityConfiguration_createdTimeStamp,
    securityConfiguration_name,

    -- ** Segment
    segment_segmentNumber,
    segment_totalSegments,

    -- ** SerDeInfo
    serDeInfo_serializationLibrary,
    serDeInfo_name,
    serDeInfo_parameters,

    -- ** SkewedInfo
    skewedInfo_skewedColumnNames,
    skewedInfo_skewedColumnValues,
    skewedInfo_skewedColumnValueLocationMaps,

    -- ** SortCriterion
    sortCriterion_fieldName,
    sortCriterion_sort,

    -- ** StorageDescriptor
    storageDescriptor_compressed,
    storageDescriptor_numberOfBuckets,
    storageDescriptor_skewedInfo,
    storageDescriptor_schemaReference,
    storageDescriptor_sortColumns,
    storageDescriptor_outputFormat,
    storageDescriptor_bucketColumns,
    storageDescriptor_serdeInfo,
    storageDescriptor_location,
    storageDescriptor_columns,
    storageDescriptor_inputFormat,
    storageDescriptor_parameters,
    storageDescriptor_storedAsSubDirectories,

    -- ** StringColumnStatisticsData
    stringColumnStatisticsData_maximumLength,
    stringColumnStatisticsData_averageLength,
    stringColumnStatisticsData_numberOfNulls,
    stringColumnStatisticsData_numberOfDistinctValues,

    -- ** Table
    table_viewOriginalText,
    table_catalogId,
    table_tableType,
    table_storageDescriptor,
    table_lastAnalyzedTime,
    table_viewExpandedText,
    table_targetTable,
    table_retention,
    table_updateTime,
    table_createTime,
    table_owner,
    table_partitionKeys,
    table_description,
    table_lastAccessTime,
    table_createdBy,
    table_isRegisteredWithLakeFormation,
    table_parameters,
    table_databaseName,
    table_name,

    -- ** TableError
    tableError_tableName,
    tableError_errorDetail,

    -- ** TableIdentifier
    tableIdentifier_catalogId,
    tableIdentifier_name,
    tableIdentifier_databaseName,

    -- ** TableInput
    tableInput_viewOriginalText,
    tableInput_tableType,
    tableInput_storageDescriptor,
    tableInput_lastAnalyzedTime,
    tableInput_viewExpandedText,
    tableInput_targetTable,
    tableInput_retention,
    tableInput_owner,
    tableInput_partitionKeys,
    tableInput_description,
    tableInput_lastAccessTime,
    tableInput_parameters,
    tableInput_name,

    -- ** TableVersion
    tableVersion_versionId,
    tableVersion_table,

    -- ** TableVersionError
    tableVersionError_tableName,
    tableVersionError_errorDetail,
    tableVersionError_versionId,

    -- ** TaskRun
    taskRun_executionTime,
    taskRun_status,
    taskRun_transformId,
    taskRun_taskRunId,
    taskRun_errorString,
    taskRun_lastModifiedOn,
    taskRun_logGroupName,
    taskRun_completedOn,
    taskRun_properties,
    taskRun_startedOn,

    -- ** TaskRunFilterCriteria
    taskRunFilterCriteria_status,
    taskRunFilterCriteria_taskRunType,
    taskRunFilterCriteria_startedBefore,
    taskRunFilterCriteria_startedAfter,

    -- ** TaskRunProperties
    taskRunProperties_exportLabelsTaskRunProperties,
    taskRunProperties_findMatchesTaskRunProperties,
    taskRunProperties_labelingSetGenerationTaskRunProperties,
    taskRunProperties_taskType,
    taskRunProperties_importLabelsTaskRunProperties,

    -- ** TaskRunSortCriteria
    taskRunSortCriteria_column,
    taskRunSortCriteria_sortDirection,

    -- ** TransformEncryption
    transformEncryption_mlUserDataEncryption,
    transformEncryption_taskRunSecurityConfigurationName,

    -- ** TransformFilterCriteria
    transformFilterCriteria_createdAfter,
    transformFilterCriteria_status,
    transformFilterCriteria_transformType,
    transformFilterCriteria_schema,
    transformFilterCriteria_createdBefore,
    transformFilterCriteria_lastModifiedBefore,
    transformFilterCriteria_lastModifiedAfter,
    transformFilterCriteria_name,
    transformFilterCriteria_glueVersion,

    -- ** TransformParameters
    transformParameters_findMatchesParameters,
    transformParameters_transformType,

    -- ** TransformSortCriteria
    transformSortCriteria_column,
    transformSortCriteria_sortDirection,

    -- ** Trigger
    trigger_workflowName,
    trigger_id,
    trigger_actions,
    trigger_state,
    trigger_name,
    trigger_predicate,
    trigger_description,
    trigger_type,
    trigger_schedule,

    -- ** TriggerNodeDetails
    triggerNodeDetails_trigger,

    -- ** TriggerUpdate
    triggerUpdate_actions,
    triggerUpdate_name,
    triggerUpdate_predicate,
    triggerUpdate_description,
    triggerUpdate_schedule,

    -- ** UpdateCsvClassifierRequest
    updateCsvClassifierRequest_containsHeader,
    updateCsvClassifierRequest_delimiter,
    updateCsvClassifierRequest_disableValueTrimming,
    updateCsvClassifierRequest_header,
    updateCsvClassifierRequest_quoteSymbol,
    updateCsvClassifierRequest_allowSingleColumn,
    updateCsvClassifierRequest_name,

    -- ** UpdateGrokClassifierRequest
    updateGrokClassifierRequest_grokPattern,
    updateGrokClassifierRequest_classification,
    updateGrokClassifierRequest_customPatterns,
    updateGrokClassifierRequest_name,

    -- ** UpdateJsonClassifierRequest
    updateJsonClassifierRequest_jsonPath,
    updateJsonClassifierRequest_name,

    -- ** UpdateXMLClassifierRequest
    updateXMLClassifierRequest_classification,
    updateXMLClassifierRequest_rowTag,
    updateXMLClassifierRequest_name,

    -- ** UserDefinedFunction
    userDefinedFunction_ownerType,
    userDefinedFunction_className,
    userDefinedFunction_catalogId,
    userDefinedFunction_ownerName,
    userDefinedFunction_functionName,
    userDefinedFunction_resourceUris,
    userDefinedFunction_createTime,
    userDefinedFunction_databaseName,

    -- ** UserDefinedFunctionInput
    userDefinedFunctionInput_ownerType,
    userDefinedFunctionInput_className,
    userDefinedFunctionInput_ownerName,
    userDefinedFunctionInput_functionName,
    userDefinedFunctionInput_resourceUris,

    -- ** Workflow
    workflow_createdOn,
    workflow_defaultRunProperties,
    workflow_lastRun,
    workflow_maxConcurrentRuns,
    workflow_lastModifiedOn,
    workflow_name,
    workflow_graph,
    workflow_description,

    -- ** WorkflowGraph
    workflowGraph_nodes,
    workflowGraph_edges,

    -- ** WorkflowRun
    workflowRun_workflowRunId,
    workflowRun_status,
    workflowRun_workflowRunProperties,
    workflowRun_statistics,
    workflowRun_name,
    workflowRun_completedOn,
    workflowRun_graph,
    workflowRun_errorMessage,
    workflowRun_startedOn,
    workflowRun_previousRunId,

    -- ** WorkflowRunStatistics
    workflowRunStatistics_timeoutActions,
    workflowRunStatistics_succeededActions,
    workflowRunStatistics_runningActions,
    workflowRunStatistics_totalActions,
    workflowRunStatistics_stoppedActions,
    workflowRunStatistics_failedActions,

    -- ** XMLClassifier
    xMLClassifier_creationTime,
    xMLClassifier_version,
    xMLClassifier_lastUpdated,
    xMLClassifier_rowTag,
    xMLClassifier_name,
    xMLClassifier_classification,
  )
where

import Network.AWS.Glue.BatchCreatePartition
import Network.AWS.Glue.BatchDeleteConnection
import Network.AWS.Glue.BatchDeletePartition
import Network.AWS.Glue.BatchDeleteTable
import Network.AWS.Glue.BatchDeleteTableVersion
import Network.AWS.Glue.BatchGetCrawlers
import Network.AWS.Glue.BatchGetDevEndpoints
import Network.AWS.Glue.BatchGetJobs
import Network.AWS.Glue.BatchGetPartition
import Network.AWS.Glue.BatchGetTriggers
import Network.AWS.Glue.BatchGetWorkflows
import Network.AWS.Glue.BatchStopJobRun
import Network.AWS.Glue.BatchUpdatePartition
import Network.AWS.Glue.CancelMLTaskRun
import Network.AWS.Glue.CheckSchemaVersionValidity
import Network.AWS.Glue.CreateClassifier
import Network.AWS.Glue.CreateConnection
import Network.AWS.Glue.CreateCrawler
import Network.AWS.Glue.CreateDatabase
import Network.AWS.Glue.CreateDevEndpoint
import Network.AWS.Glue.CreateJob
import Network.AWS.Glue.CreateMLTransform
import Network.AWS.Glue.CreatePartition
import Network.AWS.Glue.CreatePartitionIndex
import Network.AWS.Glue.CreateRegistry
import Network.AWS.Glue.CreateSchema
import Network.AWS.Glue.CreateScript
import Network.AWS.Glue.CreateSecurityConfiguration
import Network.AWS.Glue.CreateTable
import Network.AWS.Glue.CreateTrigger
import Network.AWS.Glue.CreateUserDefinedFunction
import Network.AWS.Glue.CreateWorkflow
import Network.AWS.Glue.DeleteClassifier
import Network.AWS.Glue.DeleteColumnStatisticsForPartition
import Network.AWS.Glue.DeleteColumnStatisticsForTable
import Network.AWS.Glue.DeleteConnection
import Network.AWS.Glue.DeleteCrawler
import Network.AWS.Glue.DeleteDatabase
import Network.AWS.Glue.DeleteDevEndpoint
import Network.AWS.Glue.DeleteJob
import Network.AWS.Glue.DeleteMLTransform
import Network.AWS.Glue.DeletePartition
import Network.AWS.Glue.DeletePartitionIndex
import Network.AWS.Glue.DeleteRegistry
import Network.AWS.Glue.DeleteResourcePolicy
import Network.AWS.Glue.DeleteSchema
import Network.AWS.Glue.DeleteSchemaVersions
import Network.AWS.Glue.DeleteSecurityConfiguration
import Network.AWS.Glue.DeleteTable
import Network.AWS.Glue.DeleteTableVersion
import Network.AWS.Glue.DeleteTrigger
import Network.AWS.Glue.DeleteUserDefinedFunction
import Network.AWS.Glue.DeleteWorkflow
import Network.AWS.Glue.GetCatalogImportStatus
import Network.AWS.Glue.GetClassifier
import Network.AWS.Glue.GetClassifiers
import Network.AWS.Glue.GetColumnStatisticsForPartition
import Network.AWS.Glue.GetColumnStatisticsForTable
import Network.AWS.Glue.GetConnection
import Network.AWS.Glue.GetConnections
import Network.AWS.Glue.GetCrawler
import Network.AWS.Glue.GetCrawlerMetrics
import Network.AWS.Glue.GetCrawlers
import Network.AWS.Glue.GetDataCatalogEncryptionSettings
import Network.AWS.Glue.GetDatabase
import Network.AWS.Glue.GetDatabases
import Network.AWS.Glue.GetDataflowGraph
import Network.AWS.Glue.GetDevEndpoint
import Network.AWS.Glue.GetDevEndpoints
import Network.AWS.Glue.GetJob
import Network.AWS.Glue.GetJobBookmark
import Network.AWS.Glue.GetJobRun
import Network.AWS.Glue.GetJobRuns
import Network.AWS.Glue.GetJobs
import Network.AWS.Glue.GetMLTaskRun
import Network.AWS.Glue.GetMLTaskRuns
import Network.AWS.Glue.GetMLTransform
import Network.AWS.Glue.GetMLTransforms
import Network.AWS.Glue.GetMapping
import Network.AWS.Glue.GetPartition
import Network.AWS.Glue.GetPartitionIndexes
import Network.AWS.Glue.GetPartitions
import Network.AWS.Glue.GetPlan
import Network.AWS.Glue.GetRegistry
import Network.AWS.Glue.GetResourcePolicies
import Network.AWS.Glue.GetResourcePolicy
import Network.AWS.Glue.GetSchema
import Network.AWS.Glue.GetSchemaByDefinition
import Network.AWS.Glue.GetSchemaVersion
import Network.AWS.Glue.GetSchemaVersionsDiff
import Network.AWS.Glue.GetSecurityConfiguration
import Network.AWS.Glue.GetSecurityConfigurations
import Network.AWS.Glue.GetTable
import Network.AWS.Glue.GetTableVersion
import Network.AWS.Glue.GetTableVersions
import Network.AWS.Glue.GetTables
import Network.AWS.Glue.GetTags
import Network.AWS.Glue.GetTrigger
import Network.AWS.Glue.GetTriggers
import Network.AWS.Glue.GetUserDefinedFunction
import Network.AWS.Glue.GetUserDefinedFunctions
import Network.AWS.Glue.GetWorkflow
import Network.AWS.Glue.GetWorkflowRun
import Network.AWS.Glue.GetWorkflowRunProperties
import Network.AWS.Glue.GetWorkflowRuns
import Network.AWS.Glue.ImportCatalogToGlue
import Network.AWS.Glue.ListCrawlers
import Network.AWS.Glue.ListDevEndpoints
import Network.AWS.Glue.ListJobs
import Network.AWS.Glue.ListMLTransforms
import Network.AWS.Glue.ListRegistries
import Network.AWS.Glue.ListSchemaVersions
import Network.AWS.Glue.ListSchemas
import Network.AWS.Glue.ListTriggers
import Network.AWS.Glue.ListWorkflows
import Network.AWS.Glue.PutDataCatalogEncryptionSettings
import Network.AWS.Glue.PutResourcePolicy
import Network.AWS.Glue.PutSchemaVersionMetadata
import Network.AWS.Glue.PutWorkflowRunProperties
import Network.AWS.Glue.QuerySchemaVersionMetadata
import Network.AWS.Glue.RegisterSchemaVersion
import Network.AWS.Glue.RemoveSchemaVersionMetadata
import Network.AWS.Glue.ResetJobBookmark
import Network.AWS.Glue.ResumeWorkflowRun
import Network.AWS.Glue.SearchTables
import Network.AWS.Glue.StartCrawler
import Network.AWS.Glue.StartCrawlerSchedule
import Network.AWS.Glue.StartExportLabelsTaskRun
import Network.AWS.Glue.StartImportLabelsTaskRun
import Network.AWS.Glue.StartJobRun
import Network.AWS.Glue.StartMLEvaluationTaskRun
import Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun
import Network.AWS.Glue.StartTrigger
import Network.AWS.Glue.StartWorkflowRun
import Network.AWS.Glue.StopCrawler
import Network.AWS.Glue.StopCrawlerSchedule
import Network.AWS.Glue.StopTrigger
import Network.AWS.Glue.StopWorkflowRun
import Network.AWS.Glue.TagResource
import Network.AWS.Glue.Types.Action
import Network.AWS.Glue.Types.BackfillError
import Network.AWS.Glue.Types.BatchStopJobRunError
import Network.AWS.Glue.Types.BatchStopJobRunSuccessfulSubmission
import Network.AWS.Glue.Types.BatchUpdatePartitionFailureEntry
import Network.AWS.Glue.Types.BatchUpdatePartitionRequestEntry
import Network.AWS.Glue.Types.BinaryColumnStatisticsData
import Network.AWS.Glue.Types.BooleanColumnStatisticsData
import Network.AWS.Glue.Types.CatalogEntry
import Network.AWS.Glue.Types.CatalogImportStatus
import Network.AWS.Glue.Types.CatalogTarget
import Network.AWS.Glue.Types.Classifier
import Network.AWS.Glue.Types.CloudWatchEncryption
import Network.AWS.Glue.Types.CodeGenEdge
import Network.AWS.Glue.Types.CodeGenNode
import Network.AWS.Glue.Types.CodeGenNodeArg
import Network.AWS.Glue.Types.Column
import Network.AWS.Glue.Types.ColumnError
import Network.AWS.Glue.Types.ColumnImportance
import Network.AWS.Glue.Types.ColumnStatistics
import Network.AWS.Glue.Types.ColumnStatisticsData
import Network.AWS.Glue.Types.ColumnStatisticsError
import Network.AWS.Glue.Types.Condition
import Network.AWS.Glue.Types.ConfusionMatrix
import Network.AWS.Glue.Types.Connection
import Network.AWS.Glue.Types.ConnectionInput
import Network.AWS.Glue.Types.ConnectionPasswordEncryption
import Network.AWS.Glue.Types.ConnectionsList
import Network.AWS.Glue.Types.Crawl
import Network.AWS.Glue.Types.Crawler
import Network.AWS.Glue.Types.CrawlerMetrics
import Network.AWS.Glue.Types.CrawlerNodeDetails
import Network.AWS.Glue.Types.CrawlerTargets
import Network.AWS.Glue.Types.CreateCsvClassifierRequest
import Network.AWS.Glue.Types.CreateGrokClassifierRequest
import Network.AWS.Glue.Types.CreateJsonClassifierRequest
import Network.AWS.Glue.Types.CreateXMLClassifierRequest
import Network.AWS.Glue.Types.CsvClassifier
import Network.AWS.Glue.Types.DataCatalogEncryptionSettings
import Network.AWS.Glue.Types.DataLakePrincipal
import Network.AWS.Glue.Types.Database
import Network.AWS.Glue.Types.DatabaseIdentifier
import Network.AWS.Glue.Types.DatabaseInput
import Network.AWS.Glue.Types.DateColumnStatisticsData
import Network.AWS.Glue.Types.DecimalColumnStatisticsData
import Network.AWS.Glue.Types.DecimalNumber
import Network.AWS.Glue.Types.DevEndpoint
import Network.AWS.Glue.Types.DevEndpointCustomLibraries
import Network.AWS.Glue.Types.DoubleColumnStatisticsData
import Network.AWS.Glue.Types.DynamoDBTarget
import Network.AWS.Glue.Types.Edge
import Network.AWS.Glue.Types.EncryptionAtRest
import Network.AWS.Glue.Types.EncryptionConfiguration
import Network.AWS.Glue.Types.ErrorDetail
import Network.AWS.Glue.Types.ErrorDetails
import Network.AWS.Glue.Types.EvaluationMetrics
import Network.AWS.Glue.Types.ExecutionProperty
import Network.AWS.Glue.Types.ExportLabelsTaskRunProperties
import Network.AWS.Glue.Types.FindMatchesMetrics
import Network.AWS.Glue.Types.FindMatchesParameters
import Network.AWS.Glue.Types.FindMatchesTaskRunProperties
import Network.AWS.Glue.Types.GetConnectionsFilter
import Network.AWS.Glue.Types.GluePolicy
import Network.AWS.Glue.Types.GlueTable
import Network.AWS.Glue.Types.GrokClassifier
import Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
import Network.AWS.Glue.Types.JdbcTarget
import Network.AWS.Glue.Types.Job
import Network.AWS.Glue.Types.JobBookmarkEntry
import Network.AWS.Glue.Types.JobBookmarksEncryption
import Network.AWS.Glue.Types.JobCommand
import Network.AWS.Glue.Types.JobNodeDetails
import Network.AWS.Glue.Types.JobRun
import Network.AWS.Glue.Types.JobUpdate
import Network.AWS.Glue.Types.JsonClassifier
import Network.AWS.Glue.Types.KeySchemaElement
import Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties
import Network.AWS.Glue.Types.LastCrawlInfo
import Network.AWS.Glue.Types.LineageConfiguration
import Network.AWS.Glue.Types.Location
import Network.AWS.Glue.Types.LongColumnStatisticsData
import Network.AWS.Glue.Types.MLTransform
import Network.AWS.Glue.Types.MLUserDataEncryption
import Network.AWS.Glue.Types.MappingEntry
import Network.AWS.Glue.Types.MetadataInfo
import Network.AWS.Glue.Types.MetadataKeyValuePair
import Network.AWS.Glue.Types.MongoDBTarget
import Network.AWS.Glue.Types.Node
import Network.AWS.Glue.Types.NotificationProperty
import Network.AWS.Glue.Types.Order
import Network.AWS.Glue.Types.Partition
import Network.AWS.Glue.Types.PartitionError
import Network.AWS.Glue.Types.PartitionIndex
import Network.AWS.Glue.Types.PartitionIndexDescriptor
import Network.AWS.Glue.Types.PartitionInput
import Network.AWS.Glue.Types.PartitionValueList
import Network.AWS.Glue.Types.PhysicalConnectionRequirements
import Network.AWS.Glue.Types.Predecessor
import Network.AWS.Glue.Types.Predicate
import Network.AWS.Glue.Types.PrincipalPermissions
import Network.AWS.Glue.Types.PropertyPredicate
import Network.AWS.Glue.Types.RecrawlPolicy
import Network.AWS.Glue.Types.RegistryId
import Network.AWS.Glue.Types.RegistryListItem
import Network.AWS.Glue.Types.ResourceUri
import Network.AWS.Glue.Types.S3Encryption
import Network.AWS.Glue.Types.S3Target
import Network.AWS.Glue.Types.Schedule
import Network.AWS.Glue.Types.SchemaChangePolicy
import Network.AWS.Glue.Types.SchemaColumn
import Network.AWS.Glue.Types.SchemaId
import Network.AWS.Glue.Types.SchemaListItem
import Network.AWS.Glue.Types.SchemaReference
import Network.AWS.Glue.Types.SchemaVersionErrorItem
import Network.AWS.Glue.Types.SchemaVersionListItem
import Network.AWS.Glue.Types.SchemaVersionNumber
import Network.AWS.Glue.Types.SecurityConfiguration
import Network.AWS.Glue.Types.Segment
import Network.AWS.Glue.Types.SerDeInfo
import Network.AWS.Glue.Types.SkewedInfo
import Network.AWS.Glue.Types.SortCriterion
import Network.AWS.Glue.Types.StorageDescriptor
import Network.AWS.Glue.Types.StringColumnStatisticsData
import Network.AWS.Glue.Types.Table
import Network.AWS.Glue.Types.TableError
import Network.AWS.Glue.Types.TableIdentifier
import Network.AWS.Glue.Types.TableInput
import Network.AWS.Glue.Types.TableVersion
import Network.AWS.Glue.Types.TableVersionError
import Network.AWS.Glue.Types.TaskRun
import Network.AWS.Glue.Types.TaskRunFilterCriteria
import Network.AWS.Glue.Types.TaskRunProperties
import Network.AWS.Glue.Types.TaskRunSortCriteria
import Network.AWS.Glue.Types.TransformEncryption
import Network.AWS.Glue.Types.TransformFilterCriteria
import Network.AWS.Glue.Types.TransformParameters
import Network.AWS.Glue.Types.TransformSortCriteria
import Network.AWS.Glue.Types.Trigger
import Network.AWS.Glue.Types.TriggerNodeDetails
import Network.AWS.Glue.Types.TriggerUpdate
import Network.AWS.Glue.Types.UpdateCsvClassifierRequest
import Network.AWS.Glue.Types.UpdateGrokClassifierRequest
import Network.AWS.Glue.Types.UpdateJsonClassifierRequest
import Network.AWS.Glue.Types.UpdateXMLClassifierRequest
import Network.AWS.Glue.Types.UserDefinedFunction
import Network.AWS.Glue.Types.UserDefinedFunctionInput
import Network.AWS.Glue.Types.Workflow
import Network.AWS.Glue.Types.WorkflowGraph
import Network.AWS.Glue.Types.WorkflowRun
import Network.AWS.Glue.Types.WorkflowRunStatistics
import Network.AWS.Glue.Types.XMLClassifier
import Network.AWS.Glue.UntagResource
import Network.AWS.Glue.UpdateClassifier
import Network.AWS.Glue.UpdateColumnStatisticsForPartition
import Network.AWS.Glue.UpdateColumnStatisticsForTable
import Network.AWS.Glue.UpdateConnection
import Network.AWS.Glue.UpdateCrawler
import Network.AWS.Glue.UpdateCrawlerSchedule
import Network.AWS.Glue.UpdateDatabase
import Network.AWS.Glue.UpdateDevEndpoint
import Network.AWS.Glue.UpdateJob
import Network.AWS.Glue.UpdateMLTransform
import Network.AWS.Glue.UpdatePartition
import Network.AWS.Glue.UpdateRegistry
import Network.AWS.Glue.UpdateSchema
import Network.AWS.Glue.UpdateTable
import Network.AWS.Glue.UpdateTrigger
import Network.AWS.Glue.UpdateUserDefinedFunction
import Network.AWS.Glue.UpdateWorkflow
