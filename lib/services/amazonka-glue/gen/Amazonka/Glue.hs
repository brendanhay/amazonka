{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Glue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-03-31@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Glue
--
-- Defines the public endpoint for the Glue service.
module Amazonka.Glue
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ConcurrentRunsExceededException
    _ConcurrentRunsExceededException,

    -- ** ConditionCheckFailureException
    _ConditionCheckFailureException,

    -- ** ConflictException
    _ConflictException,

    -- ** CrawlerNotRunningException
    _CrawlerNotRunningException,

    -- ** CrawlerRunningException
    _CrawlerRunningException,

    -- ** CrawlerStoppingException
    _CrawlerStoppingException,

    -- ** EntityNotFoundException
    _EntityNotFoundException,

    -- ** GlueEncryptionException
    _GlueEncryptionException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** IllegalBlueprintStateException
    _IllegalBlueprintStateException,

    -- ** IllegalSessionStateException
    _IllegalSessionStateException,

    -- ** IllegalWorkflowStateException
    _IllegalWorkflowStateException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** InvalidStateException
    _InvalidStateException,

    -- ** MLTransformNotReadyException
    _MLTransformNotReadyException,

    -- ** NoScheduleException
    _NoScheduleException,

    -- ** OperationTimeoutException
    _OperationTimeoutException,

    -- ** PermissionTypeMismatchException
    _PermissionTypeMismatchException,

    -- ** ResourceNotReadyException
    _ResourceNotReadyException,

    -- ** ResourceNumberLimitExceededException
    _ResourceNumberLimitExceededException,

    -- ** SchedulerNotRunningException
    _SchedulerNotRunningException,

    -- ** SchedulerRunningException
    _SchedulerRunningException,

    -- ** SchedulerTransitioningException
    _SchedulerTransitioningException,

    -- ** ValidationException
    _ValidationException,

    -- ** VersionMismatchException
    _VersionMismatchException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchCreatePartition
    BatchCreatePartition (BatchCreatePartition'),
    newBatchCreatePartition,
    BatchCreatePartitionResponse (BatchCreatePartitionResponse'),
    newBatchCreatePartitionResponse,

    -- ** BatchDeleteConnection
    BatchDeleteConnection (BatchDeleteConnection'),
    newBatchDeleteConnection,
    BatchDeleteConnectionResponse (BatchDeleteConnectionResponse'),
    newBatchDeleteConnectionResponse,

    -- ** BatchDeletePartition
    BatchDeletePartition (BatchDeletePartition'),
    newBatchDeletePartition,
    BatchDeletePartitionResponse (BatchDeletePartitionResponse'),
    newBatchDeletePartitionResponse,

    -- ** BatchDeleteTable
    BatchDeleteTable (BatchDeleteTable'),
    newBatchDeleteTable,
    BatchDeleteTableResponse (BatchDeleteTableResponse'),
    newBatchDeleteTableResponse,

    -- ** BatchDeleteTableVersion
    BatchDeleteTableVersion (BatchDeleteTableVersion'),
    newBatchDeleteTableVersion,
    BatchDeleteTableVersionResponse (BatchDeleteTableVersionResponse'),
    newBatchDeleteTableVersionResponse,

    -- ** BatchGetBlueprints
    BatchGetBlueprints (BatchGetBlueprints'),
    newBatchGetBlueprints,
    BatchGetBlueprintsResponse (BatchGetBlueprintsResponse'),
    newBatchGetBlueprintsResponse,

    -- ** BatchGetCrawlers
    BatchGetCrawlers (BatchGetCrawlers'),
    newBatchGetCrawlers,
    BatchGetCrawlersResponse (BatchGetCrawlersResponse'),
    newBatchGetCrawlersResponse,

    -- ** BatchGetCustomEntityTypes
    BatchGetCustomEntityTypes (BatchGetCustomEntityTypes'),
    newBatchGetCustomEntityTypes,
    BatchGetCustomEntityTypesResponse (BatchGetCustomEntityTypesResponse'),
    newBatchGetCustomEntityTypesResponse,

    -- ** BatchGetDataQualityResult
    BatchGetDataQualityResult (BatchGetDataQualityResult'),
    newBatchGetDataQualityResult,
    BatchGetDataQualityResultResponse (BatchGetDataQualityResultResponse'),
    newBatchGetDataQualityResultResponse,

    -- ** BatchGetDevEndpoints
    BatchGetDevEndpoints (BatchGetDevEndpoints'),
    newBatchGetDevEndpoints,
    BatchGetDevEndpointsResponse (BatchGetDevEndpointsResponse'),
    newBatchGetDevEndpointsResponse,

    -- ** BatchGetJobs
    BatchGetJobs (BatchGetJobs'),
    newBatchGetJobs,
    BatchGetJobsResponse (BatchGetJobsResponse'),
    newBatchGetJobsResponse,

    -- ** BatchGetPartition
    BatchGetPartition (BatchGetPartition'),
    newBatchGetPartition,
    BatchGetPartitionResponse (BatchGetPartitionResponse'),
    newBatchGetPartitionResponse,

    -- ** BatchGetTriggers
    BatchGetTriggers (BatchGetTriggers'),
    newBatchGetTriggers,
    BatchGetTriggersResponse (BatchGetTriggersResponse'),
    newBatchGetTriggersResponse,

    -- ** BatchGetWorkflows
    BatchGetWorkflows (BatchGetWorkflows'),
    newBatchGetWorkflows,
    BatchGetWorkflowsResponse (BatchGetWorkflowsResponse'),
    newBatchGetWorkflowsResponse,

    -- ** BatchStopJobRun
    BatchStopJobRun (BatchStopJobRun'),
    newBatchStopJobRun,
    BatchStopJobRunResponse (BatchStopJobRunResponse'),
    newBatchStopJobRunResponse,

    -- ** BatchUpdatePartition
    BatchUpdatePartition (BatchUpdatePartition'),
    newBatchUpdatePartition,
    BatchUpdatePartitionResponse (BatchUpdatePartitionResponse'),
    newBatchUpdatePartitionResponse,

    -- ** CancelDataQualityRuleRecommendationRun
    CancelDataQualityRuleRecommendationRun (CancelDataQualityRuleRecommendationRun'),
    newCancelDataQualityRuleRecommendationRun,
    CancelDataQualityRuleRecommendationRunResponse (CancelDataQualityRuleRecommendationRunResponse'),
    newCancelDataQualityRuleRecommendationRunResponse,

    -- ** CancelDataQualityRulesetEvaluationRun
    CancelDataQualityRulesetEvaluationRun (CancelDataQualityRulesetEvaluationRun'),
    newCancelDataQualityRulesetEvaluationRun,
    CancelDataQualityRulesetEvaluationRunResponse (CancelDataQualityRulesetEvaluationRunResponse'),
    newCancelDataQualityRulesetEvaluationRunResponse,

    -- ** CancelMLTaskRun
    CancelMLTaskRun (CancelMLTaskRun'),
    newCancelMLTaskRun,
    CancelMLTaskRunResponse (CancelMLTaskRunResponse'),
    newCancelMLTaskRunResponse,

    -- ** CancelStatement
    CancelStatement (CancelStatement'),
    newCancelStatement,
    CancelStatementResponse (CancelStatementResponse'),
    newCancelStatementResponse,

    -- ** CheckSchemaVersionValidity
    CheckSchemaVersionValidity (CheckSchemaVersionValidity'),
    newCheckSchemaVersionValidity,
    CheckSchemaVersionValidityResponse (CheckSchemaVersionValidityResponse'),
    newCheckSchemaVersionValidityResponse,

    -- ** CreateBlueprint
    CreateBlueprint (CreateBlueprint'),
    newCreateBlueprint,
    CreateBlueprintResponse (CreateBlueprintResponse'),
    newCreateBlueprintResponse,

    -- ** CreateClassifier
    CreateClassifier (CreateClassifier'),
    newCreateClassifier,
    CreateClassifierResponse (CreateClassifierResponse'),
    newCreateClassifierResponse,

    -- ** CreateConnection
    CreateConnection (CreateConnection'),
    newCreateConnection,
    CreateConnectionResponse (CreateConnectionResponse'),
    newCreateConnectionResponse,

    -- ** CreateCrawler
    CreateCrawler (CreateCrawler'),
    newCreateCrawler,
    CreateCrawlerResponse (CreateCrawlerResponse'),
    newCreateCrawlerResponse,

    -- ** CreateCustomEntityType
    CreateCustomEntityType (CreateCustomEntityType'),
    newCreateCustomEntityType,
    CreateCustomEntityTypeResponse (CreateCustomEntityTypeResponse'),
    newCreateCustomEntityTypeResponse,

    -- ** CreateDataQualityRuleset
    CreateDataQualityRuleset (CreateDataQualityRuleset'),
    newCreateDataQualityRuleset,
    CreateDataQualityRulesetResponse (CreateDataQualityRulesetResponse'),
    newCreateDataQualityRulesetResponse,

    -- ** CreateDatabase
    CreateDatabase (CreateDatabase'),
    newCreateDatabase,
    CreateDatabaseResponse (CreateDatabaseResponse'),
    newCreateDatabaseResponse,

    -- ** CreateDevEndpoint
    CreateDevEndpoint (CreateDevEndpoint'),
    newCreateDevEndpoint,
    CreateDevEndpointResponse (CreateDevEndpointResponse'),
    newCreateDevEndpointResponse,

    -- ** CreateJob
    CreateJob (CreateJob'),
    newCreateJob,
    CreateJobResponse (CreateJobResponse'),
    newCreateJobResponse,

    -- ** CreateMLTransform
    CreateMLTransform (CreateMLTransform'),
    newCreateMLTransform,
    CreateMLTransformResponse (CreateMLTransformResponse'),
    newCreateMLTransformResponse,

    -- ** CreatePartition
    CreatePartition (CreatePartition'),
    newCreatePartition,
    CreatePartitionResponse (CreatePartitionResponse'),
    newCreatePartitionResponse,

    -- ** CreatePartitionIndex
    CreatePartitionIndex (CreatePartitionIndex'),
    newCreatePartitionIndex,
    CreatePartitionIndexResponse (CreatePartitionIndexResponse'),
    newCreatePartitionIndexResponse,

    -- ** CreateRegistry
    CreateRegistry (CreateRegistry'),
    newCreateRegistry,
    CreateRegistryResponse (CreateRegistryResponse'),
    newCreateRegistryResponse,

    -- ** CreateSchema
    CreateSchema (CreateSchema'),
    newCreateSchema,
    CreateSchemaResponse (CreateSchemaResponse'),
    newCreateSchemaResponse,

    -- ** CreateScript
    CreateScript (CreateScript'),
    newCreateScript,
    CreateScriptResponse (CreateScriptResponse'),
    newCreateScriptResponse,

    -- ** CreateSecurityConfiguration
    CreateSecurityConfiguration (CreateSecurityConfiguration'),
    newCreateSecurityConfiguration,
    CreateSecurityConfigurationResponse (CreateSecurityConfigurationResponse'),
    newCreateSecurityConfigurationResponse,

    -- ** CreateSession
    CreateSession (CreateSession'),
    newCreateSession,
    CreateSessionResponse (CreateSessionResponse'),
    newCreateSessionResponse,

    -- ** CreateTable
    CreateTable (CreateTable'),
    newCreateTable,
    CreateTableResponse (CreateTableResponse'),
    newCreateTableResponse,

    -- ** CreateTrigger
    CreateTrigger (CreateTrigger'),
    newCreateTrigger,
    CreateTriggerResponse (CreateTriggerResponse'),
    newCreateTriggerResponse,

    -- ** CreateUserDefinedFunction
    CreateUserDefinedFunction (CreateUserDefinedFunction'),
    newCreateUserDefinedFunction,
    CreateUserDefinedFunctionResponse (CreateUserDefinedFunctionResponse'),
    newCreateUserDefinedFunctionResponse,

    -- ** CreateWorkflow
    CreateWorkflow (CreateWorkflow'),
    newCreateWorkflow,
    CreateWorkflowResponse (CreateWorkflowResponse'),
    newCreateWorkflowResponse,

    -- ** DeleteBlueprint
    DeleteBlueprint (DeleteBlueprint'),
    newDeleteBlueprint,
    DeleteBlueprintResponse (DeleteBlueprintResponse'),
    newDeleteBlueprintResponse,

    -- ** DeleteClassifier
    DeleteClassifier (DeleteClassifier'),
    newDeleteClassifier,
    DeleteClassifierResponse (DeleteClassifierResponse'),
    newDeleteClassifierResponse,

    -- ** DeleteColumnStatisticsForPartition
    DeleteColumnStatisticsForPartition (DeleteColumnStatisticsForPartition'),
    newDeleteColumnStatisticsForPartition,
    DeleteColumnStatisticsForPartitionResponse (DeleteColumnStatisticsForPartitionResponse'),
    newDeleteColumnStatisticsForPartitionResponse,

    -- ** DeleteColumnStatisticsForTable
    DeleteColumnStatisticsForTable (DeleteColumnStatisticsForTable'),
    newDeleteColumnStatisticsForTable,
    DeleteColumnStatisticsForTableResponse (DeleteColumnStatisticsForTableResponse'),
    newDeleteColumnStatisticsForTableResponse,

    -- ** DeleteConnection
    DeleteConnection (DeleteConnection'),
    newDeleteConnection,
    DeleteConnectionResponse (DeleteConnectionResponse'),
    newDeleteConnectionResponse,

    -- ** DeleteCrawler
    DeleteCrawler (DeleteCrawler'),
    newDeleteCrawler,
    DeleteCrawlerResponse (DeleteCrawlerResponse'),
    newDeleteCrawlerResponse,

    -- ** DeleteCustomEntityType
    DeleteCustomEntityType (DeleteCustomEntityType'),
    newDeleteCustomEntityType,
    DeleteCustomEntityTypeResponse (DeleteCustomEntityTypeResponse'),
    newDeleteCustomEntityTypeResponse,

    -- ** DeleteDataQualityRuleset
    DeleteDataQualityRuleset (DeleteDataQualityRuleset'),
    newDeleteDataQualityRuleset,
    DeleteDataQualityRulesetResponse (DeleteDataQualityRulesetResponse'),
    newDeleteDataQualityRulesetResponse,

    -- ** DeleteDatabase
    DeleteDatabase (DeleteDatabase'),
    newDeleteDatabase,
    DeleteDatabaseResponse (DeleteDatabaseResponse'),
    newDeleteDatabaseResponse,

    -- ** DeleteDevEndpoint
    DeleteDevEndpoint (DeleteDevEndpoint'),
    newDeleteDevEndpoint,
    DeleteDevEndpointResponse (DeleteDevEndpointResponse'),
    newDeleteDevEndpointResponse,

    -- ** DeleteJob
    DeleteJob (DeleteJob'),
    newDeleteJob,
    DeleteJobResponse (DeleteJobResponse'),
    newDeleteJobResponse,

    -- ** DeleteMLTransform
    DeleteMLTransform (DeleteMLTransform'),
    newDeleteMLTransform,
    DeleteMLTransformResponse (DeleteMLTransformResponse'),
    newDeleteMLTransformResponse,

    -- ** DeletePartition
    DeletePartition (DeletePartition'),
    newDeletePartition,
    DeletePartitionResponse (DeletePartitionResponse'),
    newDeletePartitionResponse,

    -- ** DeletePartitionIndex
    DeletePartitionIndex (DeletePartitionIndex'),
    newDeletePartitionIndex,
    DeletePartitionIndexResponse (DeletePartitionIndexResponse'),
    newDeletePartitionIndexResponse,

    -- ** DeleteRegistry
    DeleteRegistry (DeleteRegistry'),
    newDeleteRegistry,
    DeleteRegistryResponse (DeleteRegistryResponse'),
    newDeleteRegistryResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** DeleteSchema
    DeleteSchema (DeleteSchema'),
    newDeleteSchema,
    DeleteSchemaResponse (DeleteSchemaResponse'),
    newDeleteSchemaResponse,

    -- ** DeleteSchemaVersions
    DeleteSchemaVersions (DeleteSchemaVersions'),
    newDeleteSchemaVersions,
    DeleteSchemaVersionsResponse (DeleteSchemaVersionsResponse'),
    newDeleteSchemaVersionsResponse,

    -- ** DeleteSecurityConfiguration
    DeleteSecurityConfiguration (DeleteSecurityConfiguration'),
    newDeleteSecurityConfiguration,
    DeleteSecurityConfigurationResponse (DeleteSecurityConfigurationResponse'),
    newDeleteSecurityConfigurationResponse,

    -- ** DeleteSession
    DeleteSession (DeleteSession'),
    newDeleteSession,
    DeleteSessionResponse (DeleteSessionResponse'),
    newDeleteSessionResponse,

    -- ** DeleteTable
    DeleteTable (DeleteTable'),
    newDeleteTable,
    DeleteTableResponse (DeleteTableResponse'),
    newDeleteTableResponse,

    -- ** DeleteTableVersion
    DeleteTableVersion (DeleteTableVersion'),
    newDeleteTableVersion,
    DeleteTableVersionResponse (DeleteTableVersionResponse'),
    newDeleteTableVersionResponse,

    -- ** DeleteTrigger
    DeleteTrigger (DeleteTrigger'),
    newDeleteTrigger,
    DeleteTriggerResponse (DeleteTriggerResponse'),
    newDeleteTriggerResponse,

    -- ** DeleteUserDefinedFunction
    DeleteUserDefinedFunction (DeleteUserDefinedFunction'),
    newDeleteUserDefinedFunction,
    DeleteUserDefinedFunctionResponse (DeleteUserDefinedFunctionResponse'),
    newDeleteUserDefinedFunctionResponse,

    -- ** DeleteWorkflow
    DeleteWorkflow (DeleteWorkflow'),
    newDeleteWorkflow,
    DeleteWorkflowResponse (DeleteWorkflowResponse'),
    newDeleteWorkflowResponse,

    -- ** GetBlueprint
    GetBlueprint (GetBlueprint'),
    newGetBlueprint,
    GetBlueprintResponse (GetBlueprintResponse'),
    newGetBlueprintResponse,

    -- ** GetBlueprintRun
    GetBlueprintRun (GetBlueprintRun'),
    newGetBlueprintRun,
    GetBlueprintRunResponse (GetBlueprintRunResponse'),
    newGetBlueprintRunResponse,

    -- ** GetBlueprintRuns
    GetBlueprintRuns (GetBlueprintRuns'),
    newGetBlueprintRuns,
    GetBlueprintRunsResponse (GetBlueprintRunsResponse'),
    newGetBlueprintRunsResponse,

    -- ** GetCatalogImportStatus
    GetCatalogImportStatus (GetCatalogImportStatus'),
    newGetCatalogImportStatus,
    GetCatalogImportStatusResponse (GetCatalogImportStatusResponse'),
    newGetCatalogImportStatusResponse,

    -- ** GetClassifier
    GetClassifier (GetClassifier'),
    newGetClassifier,
    GetClassifierResponse (GetClassifierResponse'),
    newGetClassifierResponse,

    -- ** GetClassifiers (Paginated)
    GetClassifiers (GetClassifiers'),
    newGetClassifiers,
    GetClassifiersResponse (GetClassifiersResponse'),
    newGetClassifiersResponse,

    -- ** GetColumnStatisticsForPartition
    GetColumnStatisticsForPartition (GetColumnStatisticsForPartition'),
    newGetColumnStatisticsForPartition,
    GetColumnStatisticsForPartitionResponse (GetColumnStatisticsForPartitionResponse'),
    newGetColumnStatisticsForPartitionResponse,

    -- ** GetColumnStatisticsForTable
    GetColumnStatisticsForTable (GetColumnStatisticsForTable'),
    newGetColumnStatisticsForTable,
    GetColumnStatisticsForTableResponse (GetColumnStatisticsForTableResponse'),
    newGetColumnStatisticsForTableResponse,

    -- ** GetConnection
    GetConnection (GetConnection'),
    newGetConnection,
    GetConnectionResponse (GetConnectionResponse'),
    newGetConnectionResponse,

    -- ** GetConnections (Paginated)
    GetConnections (GetConnections'),
    newGetConnections,
    GetConnectionsResponse (GetConnectionsResponse'),
    newGetConnectionsResponse,

    -- ** GetCrawler
    GetCrawler (GetCrawler'),
    newGetCrawler,
    GetCrawlerResponse (GetCrawlerResponse'),
    newGetCrawlerResponse,

    -- ** GetCrawlerMetrics (Paginated)
    GetCrawlerMetrics (GetCrawlerMetrics'),
    newGetCrawlerMetrics,
    GetCrawlerMetricsResponse (GetCrawlerMetricsResponse'),
    newGetCrawlerMetricsResponse,

    -- ** GetCrawlers (Paginated)
    GetCrawlers (GetCrawlers'),
    newGetCrawlers,
    GetCrawlersResponse (GetCrawlersResponse'),
    newGetCrawlersResponse,

    -- ** GetCustomEntityType
    GetCustomEntityType (GetCustomEntityType'),
    newGetCustomEntityType,
    GetCustomEntityTypeResponse (GetCustomEntityTypeResponse'),
    newGetCustomEntityTypeResponse,

    -- ** GetDataCatalogEncryptionSettings
    GetDataCatalogEncryptionSettings (GetDataCatalogEncryptionSettings'),
    newGetDataCatalogEncryptionSettings,
    GetDataCatalogEncryptionSettingsResponse (GetDataCatalogEncryptionSettingsResponse'),
    newGetDataCatalogEncryptionSettingsResponse,

    -- ** GetDataQualityResult
    GetDataQualityResult (GetDataQualityResult'),
    newGetDataQualityResult,
    GetDataQualityResultResponse (GetDataQualityResultResponse'),
    newGetDataQualityResultResponse,

    -- ** GetDataQualityRuleRecommendationRun
    GetDataQualityRuleRecommendationRun (GetDataQualityRuleRecommendationRun'),
    newGetDataQualityRuleRecommendationRun,
    GetDataQualityRuleRecommendationRunResponse (GetDataQualityRuleRecommendationRunResponse'),
    newGetDataQualityRuleRecommendationRunResponse,

    -- ** GetDataQualityRuleset
    GetDataQualityRuleset (GetDataQualityRuleset'),
    newGetDataQualityRuleset,
    GetDataQualityRulesetResponse (GetDataQualityRulesetResponse'),
    newGetDataQualityRulesetResponse,

    -- ** GetDataQualityRulesetEvaluationRun
    GetDataQualityRulesetEvaluationRun (GetDataQualityRulesetEvaluationRun'),
    newGetDataQualityRulesetEvaluationRun,
    GetDataQualityRulesetEvaluationRunResponse (GetDataQualityRulesetEvaluationRunResponse'),
    newGetDataQualityRulesetEvaluationRunResponse,

    -- ** GetDatabase
    GetDatabase (GetDatabase'),
    newGetDatabase,
    GetDatabaseResponse (GetDatabaseResponse'),
    newGetDatabaseResponse,

    -- ** GetDatabases (Paginated)
    GetDatabases (GetDatabases'),
    newGetDatabases,
    GetDatabasesResponse (GetDatabasesResponse'),
    newGetDatabasesResponse,

    -- ** GetDataflowGraph
    GetDataflowGraph (GetDataflowGraph'),
    newGetDataflowGraph,
    GetDataflowGraphResponse (GetDataflowGraphResponse'),
    newGetDataflowGraphResponse,

    -- ** GetDevEndpoint
    GetDevEndpoint (GetDevEndpoint'),
    newGetDevEndpoint,
    GetDevEndpointResponse (GetDevEndpointResponse'),
    newGetDevEndpointResponse,

    -- ** GetDevEndpoints (Paginated)
    GetDevEndpoints (GetDevEndpoints'),
    newGetDevEndpoints,
    GetDevEndpointsResponse (GetDevEndpointsResponse'),
    newGetDevEndpointsResponse,

    -- ** GetJob
    GetJob (GetJob'),
    newGetJob,
    GetJobResponse (GetJobResponse'),
    newGetJobResponse,

    -- ** GetJobBookmark
    GetJobBookmark (GetJobBookmark'),
    newGetJobBookmark,
    GetJobBookmarkResponse (GetJobBookmarkResponse'),
    newGetJobBookmarkResponse,

    -- ** GetJobRun
    GetJobRun (GetJobRun'),
    newGetJobRun,
    GetJobRunResponse (GetJobRunResponse'),
    newGetJobRunResponse,

    -- ** GetJobRuns (Paginated)
    GetJobRuns (GetJobRuns'),
    newGetJobRuns,
    GetJobRunsResponse (GetJobRunsResponse'),
    newGetJobRunsResponse,

    -- ** GetJobs (Paginated)
    GetJobs (GetJobs'),
    newGetJobs,
    GetJobsResponse (GetJobsResponse'),
    newGetJobsResponse,

    -- ** GetMLTaskRun
    GetMLTaskRun (GetMLTaskRun'),
    newGetMLTaskRun,
    GetMLTaskRunResponse (GetMLTaskRunResponse'),
    newGetMLTaskRunResponse,

    -- ** GetMLTaskRuns
    GetMLTaskRuns (GetMLTaskRuns'),
    newGetMLTaskRuns,
    GetMLTaskRunsResponse (GetMLTaskRunsResponse'),
    newGetMLTaskRunsResponse,

    -- ** GetMLTransform
    GetMLTransform (GetMLTransform'),
    newGetMLTransform,
    GetMLTransformResponse (GetMLTransformResponse'),
    newGetMLTransformResponse,

    -- ** GetMLTransforms
    GetMLTransforms (GetMLTransforms'),
    newGetMLTransforms,
    GetMLTransformsResponse (GetMLTransformsResponse'),
    newGetMLTransformsResponse,

    -- ** GetMapping
    GetMapping (GetMapping'),
    newGetMapping,
    GetMappingResponse (GetMappingResponse'),
    newGetMappingResponse,

    -- ** GetPartition
    GetPartition (GetPartition'),
    newGetPartition,
    GetPartitionResponse (GetPartitionResponse'),
    newGetPartitionResponse,

    -- ** GetPartitionIndexes (Paginated)
    GetPartitionIndexes (GetPartitionIndexes'),
    newGetPartitionIndexes,
    GetPartitionIndexesResponse (GetPartitionIndexesResponse'),
    newGetPartitionIndexesResponse,

    -- ** GetPartitions (Paginated)
    GetPartitions (GetPartitions'),
    newGetPartitions,
    GetPartitionsResponse (GetPartitionsResponse'),
    newGetPartitionsResponse,

    -- ** GetPlan
    GetPlan (GetPlan'),
    newGetPlan,
    GetPlanResponse (GetPlanResponse'),
    newGetPlanResponse,

    -- ** GetRegistry
    GetRegistry (GetRegistry'),
    newGetRegistry,
    GetRegistryResponse (GetRegistryResponse'),
    newGetRegistryResponse,

    -- ** GetResourcePolicies (Paginated)
    GetResourcePolicies (GetResourcePolicies'),
    newGetResourcePolicies,
    GetResourcePoliciesResponse (GetResourcePoliciesResponse'),
    newGetResourcePoliciesResponse,

    -- ** GetResourcePolicy
    GetResourcePolicy (GetResourcePolicy'),
    newGetResourcePolicy,
    GetResourcePolicyResponse (GetResourcePolicyResponse'),
    newGetResourcePolicyResponse,

    -- ** GetSchema
    GetSchema (GetSchema'),
    newGetSchema,
    GetSchemaResponse (GetSchemaResponse'),
    newGetSchemaResponse,

    -- ** GetSchemaByDefinition
    GetSchemaByDefinition (GetSchemaByDefinition'),
    newGetSchemaByDefinition,
    GetSchemaByDefinitionResponse (GetSchemaByDefinitionResponse'),
    newGetSchemaByDefinitionResponse,

    -- ** GetSchemaVersion
    GetSchemaVersion (GetSchemaVersion'),
    newGetSchemaVersion,
    GetSchemaVersionResponse (GetSchemaVersionResponse'),
    newGetSchemaVersionResponse,

    -- ** GetSchemaVersionsDiff
    GetSchemaVersionsDiff (GetSchemaVersionsDiff'),
    newGetSchemaVersionsDiff,
    GetSchemaVersionsDiffResponse (GetSchemaVersionsDiffResponse'),
    newGetSchemaVersionsDiffResponse,

    -- ** GetSecurityConfiguration
    GetSecurityConfiguration (GetSecurityConfiguration'),
    newGetSecurityConfiguration,
    GetSecurityConfigurationResponse (GetSecurityConfigurationResponse'),
    newGetSecurityConfigurationResponse,

    -- ** GetSecurityConfigurations (Paginated)
    GetSecurityConfigurations (GetSecurityConfigurations'),
    newGetSecurityConfigurations,
    GetSecurityConfigurationsResponse (GetSecurityConfigurationsResponse'),
    newGetSecurityConfigurationsResponse,

    -- ** GetSession
    GetSession (GetSession'),
    newGetSession,
    GetSessionResponse (GetSessionResponse'),
    newGetSessionResponse,

    -- ** GetStatement
    GetStatement (GetStatement'),
    newGetStatement,
    GetStatementResponse (GetStatementResponse'),
    newGetStatementResponse,

    -- ** GetTable
    GetTable (GetTable'),
    newGetTable,
    GetTableResponse (GetTableResponse'),
    newGetTableResponse,

    -- ** GetTableVersion
    GetTableVersion (GetTableVersion'),
    newGetTableVersion,
    GetTableVersionResponse (GetTableVersionResponse'),
    newGetTableVersionResponse,

    -- ** GetTableVersions (Paginated)
    GetTableVersions (GetTableVersions'),
    newGetTableVersions,
    GetTableVersionsResponse (GetTableVersionsResponse'),
    newGetTableVersionsResponse,

    -- ** GetTables (Paginated)
    GetTables (GetTables'),
    newGetTables,
    GetTablesResponse (GetTablesResponse'),
    newGetTablesResponse,

    -- ** GetTags
    GetTags (GetTags'),
    newGetTags,
    GetTagsResponse (GetTagsResponse'),
    newGetTagsResponse,

    -- ** GetTrigger
    GetTrigger (GetTrigger'),
    newGetTrigger,
    GetTriggerResponse (GetTriggerResponse'),
    newGetTriggerResponse,

    -- ** GetTriggers (Paginated)
    GetTriggers (GetTriggers'),
    newGetTriggers,
    GetTriggersResponse (GetTriggersResponse'),
    newGetTriggersResponse,

    -- ** GetUnfilteredPartitionMetadata
    GetUnfilteredPartitionMetadata (GetUnfilteredPartitionMetadata'),
    newGetUnfilteredPartitionMetadata,
    GetUnfilteredPartitionMetadataResponse (GetUnfilteredPartitionMetadataResponse'),
    newGetUnfilteredPartitionMetadataResponse,

    -- ** GetUnfilteredPartitionsMetadata
    GetUnfilteredPartitionsMetadata (GetUnfilteredPartitionsMetadata'),
    newGetUnfilteredPartitionsMetadata,
    GetUnfilteredPartitionsMetadataResponse (GetUnfilteredPartitionsMetadataResponse'),
    newGetUnfilteredPartitionsMetadataResponse,

    -- ** GetUnfilteredTableMetadata
    GetUnfilteredTableMetadata (GetUnfilteredTableMetadata'),
    newGetUnfilteredTableMetadata,
    GetUnfilteredTableMetadataResponse (GetUnfilteredTableMetadataResponse'),
    newGetUnfilteredTableMetadataResponse,

    -- ** GetUserDefinedFunction
    GetUserDefinedFunction (GetUserDefinedFunction'),
    newGetUserDefinedFunction,
    GetUserDefinedFunctionResponse (GetUserDefinedFunctionResponse'),
    newGetUserDefinedFunctionResponse,

    -- ** GetUserDefinedFunctions (Paginated)
    GetUserDefinedFunctions (GetUserDefinedFunctions'),
    newGetUserDefinedFunctions,
    GetUserDefinedFunctionsResponse (GetUserDefinedFunctionsResponse'),
    newGetUserDefinedFunctionsResponse,

    -- ** GetWorkflow
    GetWorkflow (GetWorkflow'),
    newGetWorkflow,
    GetWorkflowResponse (GetWorkflowResponse'),
    newGetWorkflowResponse,

    -- ** GetWorkflowRun
    GetWorkflowRun (GetWorkflowRun'),
    newGetWorkflowRun,
    GetWorkflowRunResponse (GetWorkflowRunResponse'),
    newGetWorkflowRunResponse,

    -- ** GetWorkflowRunProperties
    GetWorkflowRunProperties (GetWorkflowRunProperties'),
    newGetWorkflowRunProperties,
    GetWorkflowRunPropertiesResponse (GetWorkflowRunPropertiesResponse'),
    newGetWorkflowRunPropertiesResponse,

    -- ** GetWorkflowRuns
    GetWorkflowRuns (GetWorkflowRuns'),
    newGetWorkflowRuns,
    GetWorkflowRunsResponse (GetWorkflowRunsResponse'),
    newGetWorkflowRunsResponse,

    -- ** ImportCatalogToGlue
    ImportCatalogToGlue (ImportCatalogToGlue'),
    newImportCatalogToGlue,
    ImportCatalogToGlueResponse (ImportCatalogToGlueResponse'),
    newImportCatalogToGlueResponse,

    -- ** ListBlueprints
    ListBlueprints (ListBlueprints'),
    newListBlueprints,
    ListBlueprintsResponse (ListBlueprintsResponse'),
    newListBlueprintsResponse,

    -- ** ListCrawlers
    ListCrawlers (ListCrawlers'),
    newListCrawlers,
    ListCrawlersResponse (ListCrawlersResponse'),
    newListCrawlersResponse,

    -- ** ListCrawls
    ListCrawls (ListCrawls'),
    newListCrawls,
    ListCrawlsResponse (ListCrawlsResponse'),
    newListCrawlsResponse,

    -- ** ListCustomEntityTypes
    ListCustomEntityTypes (ListCustomEntityTypes'),
    newListCustomEntityTypes,
    ListCustomEntityTypesResponse (ListCustomEntityTypesResponse'),
    newListCustomEntityTypesResponse,

    -- ** ListDataQualityResults
    ListDataQualityResults (ListDataQualityResults'),
    newListDataQualityResults,
    ListDataQualityResultsResponse (ListDataQualityResultsResponse'),
    newListDataQualityResultsResponse,

    -- ** ListDataQualityRuleRecommendationRuns
    ListDataQualityRuleRecommendationRuns (ListDataQualityRuleRecommendationRuns'),
    newListDataQualityRuleRecommendationRuns,
    ListDataQualityRuleRecommendationRunsResponse (ListDataQualityRuleRecommendationRunsResponse'),
    newListDataQualityRuleRecommendationRunsResponse,

    -- ** ListDataQualityRulesetEvaluationRuns
    ListDataQualityRulesetEvaluationRuns (ListDataQualityRulesetEvaluationRuns'),
    newListDataQualityRulesetEvaluationRuns,
    ListDataQualityRulesetEvaluationRunsResponse (ListDataQualityRulesetEvaluationRunsResponse'),
    newListDataQualityRulesetEvaluationRunsResponse,

    -- ** ListDataQualityRulesets
    ListDataQualityRulesets (ListDataQualityRulesets'),
    newListDataQualityRulesets,
    ListDataQualityRulesetsResponse (ListDataQualityRulesetsResponse'),
    newListDataQualityRulesetsResponse,

    -- ** ListDevEndpoints
    ListDevEndpoints (ListDevEndpoints'),
    newListDevEndpoints,
    ListDevEndpointsResponse (ListDevEndpointsResponse'),
    newListDevEndpointsResponse,

    -- ** ListJobs
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** ListMLTransforms
    ListMLTransforms (ListMLTransforms'),
    newListMLTransforms,
    ListMLTransformsResponse (ListMLTransformsResponse'),
    newListMLTransformsResponse,

    -- ** ListRegistries (Paginated)
    ListRegistries (ListRegistries'),
    newListRegistries,
    ListRegistriesResponse (ListRegistriesResponse'),
    newListRegistriesResponse,

    -- ** ListSchemaVersions (Paginated)
    ListSchemaVersions (ListSchemaVersions'),
    newListSchemaVersions,
    ListSchemaVersionsResponse (ListSchemaVersionsResponse'),
    newListSchemaVersionsResponse,

    -- ** ListSchemas (Paginated)
    ListSchemas (ListSchemas'),
    newListSchemas,
    ListSchemasResponse (ListSchemasResponse'),
    newListSchemasResponse,

    -- ** ListSessions
    ListSessions (ListSessions'),
    newListSessions,
    ListSessionsResponse (ListSessionsResponse'),
    newListSessionsResponse,

    -- ** ListStatements
    ListStatements (ListStatements'),
    newListStatements,
    ListStatementsResponse (ListStatementsResponse'),
    newListStatementsResponse,

    -- ** ListTriggers
    ListTriggers (ListTriggers'),
    newListTriggers,
    ListTriggersResponse (ListTriggersResponse'),
    newListTriggersResponse,

    -- ** ListWorkflows
    ListWorkflows (ListWorkflows'),
    newListWorkflows,
    ListWorkflowsResponse (ListWorkflowsResponse'),
    newListWorkflowsResponse,

    -- ** PutDataCatalogEncryptionSettings
    PutDataCatalogEncryptionSettings (PutDataCatalogEncryptionSettings'),
    newPutDataCatalogEncryptionSettings,
    PutDataCatalogEncryptionSettingsResponse (PutDataCatalogEncryptionSettingsResponse'),
    newPutDataCatalogEncryptionSettingsResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** PutSchemaVersionMetadata
    PutSchemaVersionMetadata (PutSchemaVersionMetadata'),
    newPutSchemaVersionMetadata,
    PutSchemaVersionMetadataResponse (PutSchemaVersionMetadataResponse'),
    newPutSchemaVersionMetadataResponse,

    -- ** PutWorkflowRunProperties
    PutWorkflowRunProperties (PutWorkflowRunProperties'),
    newPutWorkflowRunProperties,
    PutWorkflowRunPropertiesResponse (PutWorkflowRunPropertiesResponse'),
    newPutWorkflowRunPropertiesResponse,

    -- ** QuerySchemaVersionMetadata
    QuerySchemaVersionMetadata (QuerySchemaVersionMetadata'),
    newQuerySchemaVersionMetadata,
    QuerySchemaVersionMetadataResponse (QuerySchemaVersionMetadataResponse'),
    newQuerySchemaVersionMetadataResponse,

    -- ** RegisterSchemaVersion
    RegisterSchemaVersion (RegisterSchemaVersion'),
    newRegisterSchemaVersion,
    RegisterSchemaVersionResponse (RegisterSchemaVersionResponse'),
    newRegisterSchemaVersionResponse,

    -- ** RemoveSchemaVersionMetadata
    RemoveSchemaVersionMetadata (RemoveSchemaVersionMetadata'),
    newRemoveSchemaVersionMetadata,
    RemoveSchemaVersionMetadataResponse (RemoveSchemaVersionMetadataResponse'),
    newRemoveSchemaVersionMetadataResponse,

    -- ** ResetJobBookmark
    ResetJobBookmark (ResetJobBookmark'),
    newResetJobBookmark,
    ResetJobBookmarkResponse (ResetJobBookmarkResponse'),
    newResetJobBookmarkResponse,

    -- ** ResumeWorkflowRun
    ResumeWorkflowRun (ResumeWorkflowRun'),
    newResumeWorkflowRun,
    ResumeWorkflowRunResponse (ResumeWorkflowRunResponse'),
    newResumeWorkflowRunResponse,

    -- ** RunStatement
    RunStatement (RunStatement'),
    newRunStatement,
    RunStatementResponse (RunStatementResponse'),
    newRunStatementResponse,

    -- ** SearchTables
    SearchTables (SearchTables'),
    newSearchTables,
    SearchTablesResponse (SearchTablesResponse'),
    newSearchTablesResponse,

    -- ** StartBlueprintRun
    StartBlueprintRun (StartBlueprintRun'),
    newStartBlueprintRun,
    StartBlueprintRunResponse (StartBlueprintRunResponse'),
    newStartBlueprintRunResponse,

    -- ** StartCrawler
    StartCrawler (StartCrawler'),
    newStartCrawler,
    StartCrawlerResponse (StartCrawlerResponse'),
    newStartCrawlerResponse,

    -- ** StartCrawlerSchedule
    StartCrawlerSchedule (StartCrawlerSchedule'),
    newStartCrawlerSchedule,
    StartCrawlerScheduleResponse (StartCrawlerScheduleResponse'),
    newStartCrawlerScheduleResponse,

    -- ** StartDataQualityRuleRecommendationRun
    StartDataQualityRuleRecommendationRun (StartDataQualityRuleRecommendationRun'),
    newStartDataQualityRuleRecommendationRun,
    StartDataQualityRuleRecommendationRunResponse (StartDataQualityRuleRecommendationRunResponse'),
    newStartDataQualityRuleRecommendationRunResponse,

    -- ** StartDataQualityRulesetEvaluationRun
    StartDataQualityRulesetEvaluationRun (StartDataQualityRulesetEvaluationRun'),
    newStartDataQualityRulesetEvaluationRun,
    StartDataQualityRulesetEvaluationRunResponse (StartDataQualityRulesetEvaluationRunResponse'),
    newStartDataQualityRulesetEvaluationRunResponse,

    -- ** StartExportLabelsTaskRun
    StartExportLabelsTaskRun (StartExportLabelsTaskRun'),
    newStartExportLabelsTaskRun,
    StartExportLabelsTaskRunResponse (StartExportLabelsTaskRunResponse'),
    newStartExportLabelsTaskRunResponse,

    -- ** StartImportLabelsTaskRun
    StartImportLabelsTaskRun (StartImportLabelsTaskRun'),
    newStartImportLabelsTaskRun,
    StartImportLabelsTaskRunResponse (StartImportLabelsTaskRunResponse'),
    newStartImportLabelsTaskRunResponse,

    -- ** StartJobRun
    StartJobRun (StartJobRun'),
    newStartJobRun,
    StartJobRunResponse (StartJobRunResponse'),
    newStartJobRunResponse,

    -- ** StartMLEvaluationTaskRun
    StartMLEvaluationTaskRun (StartMLEvaluationTaskRun'),
    newStartMLEvaluationTaskRun,
    StartMLEvaluationTaskRunResponse (StartMLEvaluationTaskRunResponse'),
    newStartMLEvaluationTaskRunResponse,

    -- ** StartMLLabelingSetGenerationTaskRun
    StartMLLabelingSetGenerationTaskRun (StartMLLabelingSetGenerationTaskRun'),
    newStartMLLabelingSetGenerationTaskRun,
    StartMLLabelingSetGenerationTaskRunResponse (StartMLLabelingSetGenerationTaskRunResponse'),
    newStartMLLabelingSetGenerationTaskRunResponse,

    -- ** StartTrigger
    StartTrigger (StartTrigger'),
    newStartTrigger,
    StartTriggerResponse (StartTriggerResponse'),
    newStartTriggerResponse,

    -- ** StartWorkflowRun
    StartWorkflowRun (StartWorkflowRun'),
    newStartWorkflowRun,
    StartWorkflowRunResponse (StartWorkflowRunResponse'),
    newStartWorkflowRunResponse,

    -- ** StopCrawler
    StopCrawler (StopCrawler'),
    newStopCrawler,
    StopCrawlerResponse (StopCrawlerResponse'),
    newStopCrawlerResponse,

    -- ** StopCrawlerSchedule
    StopCrawlerSchedule (StopCrawlerSchedule'),
    newStopCrawlerSchedule,
    StopCrawlerScheduleResponse (StopCrawlerScheduleResponse'),
    newStopCrawlerScheduleResponse,

    -- ** StopSession
    StopSession (StopSession'),
    newStopSession,
    StopSessionResponse (StopSessionResponse'),
    newStopSessionResponse,

    -- ** StopTrigger
    StopTrigger (StopTrigger'),
    newStopTrigger,
    StopTriggerResponse (StopTriggerResponse'),
    newStopTriggerResponse,

    -- ** StopWorkflowRun
    StopWorkflowRun (StopWorkflowRun'),
    newStopWorkflowRun,
    StopWorkflowRunResponse (StopWorkflowRunResponse'),
    newStopWorkflowRunResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateBlueprint
    UpdateBlueprint (UpdateBlueprint'),
    newUpdateBlueprint,
    UpdateBlueprintResponse (UpdateBlueprintResponse'),
    newUpdateBlueprintResponse,

    -- ** UpdateClassifier
    UpdateClassifier (UpdateClassifier'),
    newUpdateClassifier,
    UpdateClassifierResponse (UpdateClassifierResponse'),
    newUpdateClassifierResponse,

    -- ** UpdateColumnStatisticsForPartition
    UpdateColumnStatisticsForPartition (UpdateColumnStatisticsForPartition'),
    newUpdateColumnStatisticsForPartition,
    UpdateColumnStatisticsForPartitionResponse (UpdateColumnStatisticsForPartitionResponse'),
    newUpdateColumnStatisticsForPartitionResponse,

    -- ** UpdateColumnStatisticsForTable
    UpdateColumnStatisticsForTable (UpdateColumnStatisticsForTable'),
    newUpdateColumnStatisticsForTable,
    UpdateColumnStatisticsForTableResponse (UpdateColumnStatisticsForTableResponse'),
    newUpdateColumnStatisticsForTableResponse,

    -- ** UpdateConnection
    UpdateConnection (UpdateConnection'),
    newUpdateConnection,
    UpdateConnectionResponse (UpdateConnectionResponse'),
    newUpdateConnectionResponse,

    -- ** UpdateCrawler
    UpdateCrawler (UpdateCrawler'),
    newUpdateCrawler,
    UpdateCrawlerResponse (UpdateCrawlerResponse'),
    newUpdateCrawlerResponse,

    -- ** UpdateCrawlerSchedule
    UpdateCrawlerSchedule (UpdateCrawlerSchedule'),
    newUpdateCrawlerSchedule,
    UpdateCrawlerScheduleResponse (UpdateCrawlerScheduleResponse'),
    newUpdateCrawlerScheduleResponse,

    -- ** UpdateDataQualityRuleset
    UpdateDataQualityRuleset (UpdateDataQualityRuleset'),
    newUpdateDataQualityRuleset,
    UpdateDataQualityRulesetResponse (UpdateDataQualityRulesetResponse'),
    newUpdateDataQualityRulesetResponse,

    -- ** UpdateDatabase
    UpdateDatabase (UpdateDatabase'),
    newUpdateDatabase,
    UpdateDatabaseResponse (UpdateDatabaseResponse'),
    newUpdateDatabaseResponse,

    -- ** UpdateDevEndpoint
    UpdateDevEndpoint (UpdateDevEndpoint'),
    newUpdateDevEndpoint,
    UpdateDevEndpointResponse (UpdateDevEndpointResponse'),
    newUpdateDevEndpointResponse,

    -- ** UpdateJob
    UpdateJob (UpdateJob'),
    newUpdateJob,
    UpdateJobResponse (UpdateJobResponse'),
    newUpdateJobResponse,

    -- ** UpdateJobFromSourceControl
    UpdateJobFromSourceControl (UpdateJobFromSourceControl'),
    newUpdateJobFromSourceControl,
    UpdateJobFromSourceControlResponse (UpdateJobFromSourceControlResponse'),
    newUpdateJobFromSourceControlResponse,

    -- ** UpdateMLTransform
    UpdateMLTransform (UpdateMLTransform'),
    newUpdateMLTransform,
    UpdateMLTransformResponse (UpdateMLTransformResponse'),
    newUpdateMLTransformResponse,

    -- ** UpdatePartition
    UpdatePartition (UpdatePartition'),
    newUpdatePartition,
    UpdatePartitionResponse (UpdatePartitionResponse'),
    newUpdatePartitionResponse,

    -- ** UpdateRegistry
    UpdateRegistry (UpdateRegistry'),
    newUpdateRegistry,
    UpdateRegistryResponse (UpdateRegistryResponse'),
    newUpdateRegistryResponse,

    -- ** UpdateSchema
    UpdateSchema (UpdateSchema'),
    newUpdateSchema,
    UpdateSchemaResponse (UpdateSchemaResponse'),
    newUpdateSchemaResponse,

    -- ** UpdateSourceControlFromJob
    UpdateSourceControlFromJob (UpdateSourceControlFromJob'),
    newUpdateSourceControlFromJob,
    UpdateSourceControlFromJobResponse (UpdateSourceControlFromJobResponse'),
    newUpdateSourceControlFromJobResponse,

    -- ** UpdateTable
    UpdateTable (UpdateTable'),
    newUpdateTable,
    UpdateTableResponse (UpdateTableResponse'),
    newUpdateTableResponse,

    -- ** UpdateTrigger
    UpdateTrigger (UpdateTrigger'),
    newUpdateTrigger,
    UpdateTriggerResponse (UpdateTriggerResponse'),
    newUpdateTriggerResponse,

    -- ** UpdateUserDefinedFunction
    UpdateUserDefinedFunction (UpdateUserDefinedFunction'),
    newUpdateUserDefinedFunction,
    UpdateUserDefinedFunctionResponse (UpdateUserDefinedFunctionResponse'),
    newUpdateUserDefinedFunctionResponse,

    -- ** UpdateWorkflow
    UpdateWorkflow (UpdateWorkflow'),
    newUpdateWorkflow,
    UpdateWorkflowResponse (UpdateWorkflowResponse'),
    newUpdateWorkflowResponse,

    -- * Types

    -- ** AggFunction
    AggFunction (..),

    -- ** BackfillErrorCode
    BackfillErrorCode (..),

    -- ** BlueprintRunState
    BlueprintRunState (..),

    -- ** BlueprintStatus
    BlueprintStatus (..),

    -- ** CatalogEncryptionMode
    CatalogEncryptionMode (..),

    -- ** CloudWatchEncryptionMode
    CloudWatchEncryptionMode (..),

    -- ** ColumnStatisticsType
    ColumnStatisticsType (..),

    -- ** Comparator
    Comparator (..),

    -- ** Compatibility
    Compatibility (..),

    -- ** CompressionType
    CompressionType (..),

    -- ** ConnectionPropertyKey
    ConnectionPropertyKey (..),

    -- ** ConnectionType
    ConnectionType (..),

    -- ** CrawlState
    CrawlState (..),

    -- ** CrawlerHistoryState
    CrawlerHistoryState (..),

    -- ** CrawlerLineageSettings
    CrawlerLineageSettings (..),

    -- ** CrawlerState
    CrawlerState (..),

    -- ** CsvHeaderOption
    CsvHeaderOption (..),

    -- ** DQStopJobOnFailureTiming
    DQStopJobOnFailureTiming (..),

    -- ** DQTransformOutput
    DQTransformOutput (..),

    -- ** DataFormat
    DataFormat (..),

    -- ** DataQualityRuleResultStatus
    DataQualityRuleResultStatus (..),

    -- ** DeleteBehavior
    DeleteBehavior (..),

    -- ** EnableHybridValues
    EnableHybridValues (..),

    -- ** ExecutionClass
    ExecutionClass (..),

    -- ** ExistCondition
    ExistCondition (..),

    -- ** FieldName
    FieldName (..),

    -- ** FilterLogicalOperator
    FilterLogicalOperator (..),

    -- ** FilterOperation
    FilterOperation (..),

    -- ** FilterOperator
    FilterOperator (..),

    -- ** FilterValueType
    FilterValueType (..),

    -- ** GlueRecordType
    GlueRecordType (..),

    -- ** JDBCDataType
    JDBCDataType (..),

    -- ** JdbcMetadataEntry
    JdbcMetadataEntry (..),

    -- ** JobBookmarksEncryptionMode
    JobBookmarksEncryptionMode (..),

    -- ** JobRunState
    JobRunState (..),

    -- ** JoinType
    JoinType (..),

    -- ** Language
    Language (..),

    -- ** LastCrawlStatus
    LastCrawlStatus (..),

    -- ** Logical
    Logical (..),

    -- ** LogicalOperator
    LogicalOperator (..),

    -- ** MLUserDataEncryptionModeString
    MLUserDataEncryptionModeString (..),

    -- ** NodeType
    NodeType (..),

    -- ** ParamType
    ParamType (..),

    -- ** ParquetCompressionType
    ParquetCompressionType (..),

    -- ** PartitionIndexStatus
    PartitionIndexStatus (..),

    -- ** Permission
    Permission (..),

    -- ** PermissionType
    PermissionType (..),

    -- ** PiiType
    PiiType (..),

    -- ** PrincipalType
    PrincipalType (..),

    -- ** QuoteChar
    QuoteChar (..),

    -- ** RecrawlBehavior
    RecrawlBehavior (..),

    -- ** RegistryStatus
    RegistryStatus (..),

    -- ** ResourceShareType
    ResourceShareType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** S3EncryptionMode
    S3EncryptionMode (..),

    -- ** ScheduleState
    ScheduleState (..),

    -- ** SchemaDiffType
    SchemaDiffType (..),

    -- ** SchemaStatus
    SchemaStatus (..),

    -- ** SchemaVersionStatus
    SchemaVersionStatus (..),

    -- ** Separator
    Separator (..),

    -- ** SessionStatus
    SessionStatus (..),

    -- ** Sort
    Sort (..),

    -- ** SortDirectionType
    SortDirectionType (..),

    -- ** SourceControlAuthStrategy
    SourceControlAuthStrategy (..),

    -- ** SourceControlProvider
    SourceControlProvider (..),

    -- ** StartingPosition
    StartingPosition (..),

    -- ** StatementState
    StatementState (..),

    -- ** TargetFormat
    TargetFormat (..),

    -- ** TaskRunSortColumnType
    TaskRunSortColumnType (..),

    -- ** TaskStatusType
    TaskStatusType (..),

    -- ** TaskType
    TaskType (..),

    -- ** TransformSortColumnType
    TransformSortColumnType (..),

    -- ** TransformStatusType
    TransformStatusType (..),

    -- ** TransformType
    TransformType (..),

    -- ** TriggerState
    TriggerState (..),

    -- ** TriggerType
    TriggerType (..),

    -- ** UnionType
    UnionType (..),

    -- ** UpdateBehavior
    UpdateBehavior (..),

    -- ** UpdateCatalogBehavior
    UpdateCatalogBehavior (..),

    -- ** WorkerType
    WorkerType (..),

    -- ** WorkflowRunStatus
    WorkflowRunStatus (..),

    -- ** Action
    Action (Action'),
    newAction,

    -- ** Aggregate
    Aggregate (Aggregate'),
    newAggregate,

    -- ** AggregateOperation
    AggregateOperation (AggregateOperation'),
    newAggregateOperation,

    -- ** ApplyMapping
    ApplyMapping (ApplyMapping'),
    newApplyMapping,

    -- ** AthenaConnectorSource
    AthenaConnectorSource (AthenaConnectorSource'),
    newAthenaConnectorSource,

    -- ** AuditContext
    AuditContext (AuditContext'),
    newAuditContext,

    -- ** BackfillError
    BackfillError (BackfillError'),
    newBackfillError,

    -- ** BasicCatalogTarget
    BasicCatalogTarget (BasicCatalogTarget'),
    newBasicCatalogTarget,

    -- ** BatchStopJobRunError
    BatchStopJobRunError (BatchStopJobRunError'),
    newBatchStopJobRunError,

    -- ** BatchStopJobRunSuccessfulSubmission
    BatchStopJobRunSuccessfulSubmission (BatchStopJobRunSuccessfulSubmission'),
    newBatchStopJobRunSuccessfulSubmission,

    -- ** BatchUpdatePartitionFailureEntry
    BatchUpdatePartitionFailureEntry (BatchUpdatePartitionFailureEntry'),
    newBatchUpdatePartitionFailureEntry,

    -- ** BatchUpdatePartitionRequestEntry
    BatchUpdatePartitionRequestEntry (BatchUpdatePartitionRequestEntry'),
    newBatchUpdatePartitionRequestEntry,

    -- ** BinaryColumnStatisticsData
    BinaryColumnStatisticsData (BinaryColumnStatisticsData'),
    newBinaryColumnStatisticsData,

    -- ** Blueprint
    Blueprint (Blueprint'),
    newBlueprint,

    -- ** BlueprintDetails
    BlueprintDetails (BlueprintDetails'),
    newBlueprintDetails,

    -- ** BlueprintRun
    BlueprintRun (BlueprintRun'),
    newBlueprintRun,

    -- ** BooleanColumnStatisticsData
    BooleanColumnStatisticsData (BooleanColumnStatisticsData'),
    newBooleanColumnStatisticsData,

    -- ** CatalogEntry
    CatalogEntry (CatalogEntry'),
    newCatalogEntry,

    -- ** CatalogImportStatus
    CatalogImportStatus (CatalogImportStatus'),
    newCatalogImportStatus,

    -- ** CatalogKafkaSource
    CatalogKafkaSource (CatalogKafkaSource'),
    newCatalogKafkaSource,

    -- ** CatalogKinesisSource
    CatalogKinesisSource (CatalogKinesisSource'),
    newCatalogKinesisSource,

    -- ** CatalogSchemaChangePolicy
    CatalogSchemaChangePolicy (CatalogSchemaChangePolicy'),
    newCatalogSchemaChangePolicy,

    -- ** CatalogSource
    CatalogSource (CatalogSource'),
    newCatalogSource,

    -- ** CatalogTarget
    CatalogTarget (CatalogTarget'),
    newCatalogTarget,

    -- ** Classifier
    Classifier (Classifier'),
    newClassifier,

    -- ** CloudWatchEncryption
    CloudWatchEncryption (CloudWatchEncryption'),
    newCloudWatchEncryption,

    -- ** CodeGenConfigurationNode
    CodeGenConfigurationNode (CodeGenConfigurationNode'),
    newCodeGenConfigurationNode,

    -- ** CodeGenEdge
    CodeGenEdge (CodeGenEdge'),
    newCodeGenEdge,

    -- ** CodeGenNode
    CodeGenNode (CodeGenNode'),
    newCodeGenNode,

    -- ** CodeGenNodeArg
    CodeGenNodeArg (CodeGenNodeArg'),
    newCodeGenNodeArg,

    -- ** Column
    Column (Column'),
    newColumn,

    -- ** ColumnError
    ColumnError (ColumnError'),
    newColumnError,

    -- ** ColumnImportance
    ColumnImportance (ColumnImportance'),
    newColumnImportance,

    -- ** ColumnRowFilter
    ColumnRowFilter (ColumnRowFilter'),
    newColumnRowFilter,

    -- ** ColumnStatistics
    ColumnStatistics (ColumnStatistics'),
    newColumnStatistics,

    -- ** ColumnStatisticsData
    ColumnStatisticsData (ColumnStatisticsData'),
    newColumnStatisticsData,

    -- ** ColumnStatisticsError
    ColumnStatisticsError (ColumnStatisticsError'),
    newColumnStatisticsError,

    -- ** Condition
    Condition (Condition'),
    newCondition,

    -- ** ConfusionMatrix
    ConfusionMatrix (ConfusionMatrix'),
    newConfusionMatrix,

    -- ** Connection
    Connection (Connection'),
    newConnection,

    -- ** ConnectionInput
    ConnectionInput (ConnectionInput'),
    newConnectionInput,

    -- ** ConnectionPasswordEncryption
    ConnectionPasswordEncryption (ConnectionPasswordEncryption'),
    newConnectionPasswordEncryption,

    -- ** ConnectionsList
    ConnectionsList (ConnectionsList'),
    newConnectionsList,

    -- ** Crawl
    Crawl (Crawl'),
    newCrawl,

    -- ** Crawler
    Crawler (Crawler'),
    newCrawler,

    -- ** CrawlerHistory
    CrawlerHistory (CrawlerHistory'),
    newCrawlerHistory,

    -- ** CrawlerMetrics
    CrawlerMetrics (CrawlerMetrics'),
    newCrawlerMetrics,

    -- ** CrawlerNodeDetails
    CrawlerNodeDetails (CrawlerNodeDetails'),
    newCrawlerNodeDetails,

    -- ** CrawlerTargets
    CrawlerTargets (CrawlerTargets'),
    newCrawlerTargets,

    -- ** CrawlsFilter
    CrawlsFilter (CrawlsFilter'),
    newCrawlsFilter,

    -- ** CreateCsvClassifierRequest
    CreateCsvClassifierRequest (CreateCsvClassifierRequest'),
    newCreateCsvClassifierRequest,

    -- ** CreateGrokClassifierRequest
    CreateGrokClassifierRequest (CreateGrokClassifierRequest'),
    newCreateGrokClassifierRequest,

    -- ** CreateJsonClassifierRequest
    CreateJsonClassifierRequest (CreateJsonClassifierRequest'),
    newCreateJsonClassifierRequest,

    -- ** CreateXMLClassifierRequest
    CreateXMLClassifierRequest (CreateXMLClassifierRequest'),
    newCreateXMLClassifierRequest,

    -- ** CsvClassifier
    CsvClassifier (CsvClassifier'),
    newCsvClassifier,

    -- ** CustomCode
    CustomCode (CustomCode'),
    newCustomCode,

    -- ** CustomEntityType
    CustomEntityType (CustomEntityType'),
    newCustomEntityType,

    -- ** DQResultsPublishingOptions
    DQResultsPublishingOptions (DQResultsPublishingOptions'),
    newDQResultsPublishingOptions,

    -- ** DQStopJobOnFailureOptions
    DQStopJobOnFailureOptions (DQStopJobOnFailureOptions'),
    newDQStopJobOnFailureOptions,

    -- ** DataCatalogEncryptionSettings
    DataCatalogEncryptionSettings (DataCatalogEncryptionSettings'),
    newDataCatalogEncryptionSettings,

    -- ** DataLakePrincipal
    DataLakePrincipal (DataLakePrincipal'),
    newDataLakePrincipal,

    -- ** DataQualityEvaluationRunAdditionalRunOptions
    DataQualityEvaluationRunAdditionalRunOptions (DataQualityEvaluationRunAdditionalRunOptions'),
    newDataQualityEvaluationRunAdditionalRunOptions,

    -- ** DataQualityResult
    DataQualityResult (DataQualityResult'),
    newDataQualityResult,

    -- ** DataQualityResultDescription
    DataQualityResultDescription (DataQualityResultDescription'),
    newDataQualityResultDescription,

    -- ** DataQualityResultFilterCriteria
    DataQualityResultFilterCriteria (DataQualityResultFilterCriteria'),
    newDataQualityResultFilterCriteria,

    -- ** DataQualityRuleRecommendationRunDescription
    DataQualityRuleRecommendationRunDescription (DataQualityRuleRecommendationRunDescription'),
    newDataQualityRuleRecommendationRunDescription,

    -- ** DataQualityRuleRecommendationRunFilter
    DataQualityRuleRecommendationRunFilter (DataQualityRuleRecommendationRunFilter'),
    newDataQualityRuleRecommendationRunFilter,

    -- ** DataQualityRuleResult
    DataQualityRuleResult (DataQualityRuleResult'),
    newDataQualityRuleResult,

    -- ** DataQualityRulesetEvaluationRunDescription
    DataQualityRulesetEvaluationRunDescription (DataQualityRulesetEvaluationRunDescription'),
    newDataQualityRulesetEvaluationRunDescription,

    -- ** DataQualityRulesetEvaluationRunFilter
    DataQualityRulesetEvaluationRunFilter (DataQualityRulesetEvaluationRunFilter'),
    newDataQualityRulesetEvaluationRunFilter,

    -- ** DataQualityRulesetFilterCriteria
    DataQualityRulesetFilterCriteria (DataQualityRulesetFilterCriteria'),
    newDataQualityRulesetFilterCriteria,

    -- ** DataQualityRulesetListDetails
    DataQualityRulesetListDetails (DataQualityRulesetListDetails'),
    newDataQualityRulesetListDetails,

    -- ** DataQualityTargetTable
    DataQualityTargetTable (DataQualityTargetTable'),
    newDataQualityTargetTable,

    -- ** DataSource
    DataSource (DataSource'),
    newDataSource,

    -- ** Database
    Database (Database'),
    newDatabase,

    -- ** DatabaseIdentifier
    DatabaseIdentifier (DatabaseIdentifier'),
    newDatabaseIdentifier,

    -- ** DatabaseInput
    DatabaseInput (DatabaseInput'),
    newDatabaseInput,

    -- ** Datatype
    Datatype (Datatype'),
    newDatatype,

    -- ** DateColumnStatisticsData
    DateColumnStatisticsData (DateColumnStatisticsData'),
    newDateColumnStatisticsData,

    -- ** DecimalColumnStatisticsData
    DecimalColumnStatisticsData (DecimalColumnStatisticsData'),
    newDecimalColumnStatisticsData,

    -- ** DecimalNumber
    DecimalNumber (DecimalNumber'),
    newDecimalNumber,

    -- ** DeltaTarget
    DeltaTarget (DeltaTarget'),
    newDeltaTarget,

    -- ** DevEndpoint
    DevEndpoint (DevEndpoint'),
    newDevEndpoint,

    -- ** DevEndpointCustomLibraries
    DevEndpointCustomLibraries (DevEndpointCustomLibraries'),
    newDevEndpointCustomLibraries,

    -- ** DirectKafkaSource
    DirectKafkaSource (DirectKafkaSource'),
    newDirectKafkaSource,

    -- ** DirectKinesisSource
    DirectKinesisSource (DirectKinesisSource'),
    newDirectKinesisSource,

    -- ** DirectSchemaChangePolicy
    DirectSchemaChangePolicy (DirectSchemaChangePolicy'),
    newDirectSchemaChangePolicy,

    -- ** DoubleColumnStatisticsData
    DoubleColumnStatisticsData (DoubleColumnStatisticsData'),
    newDoubleColumnStatisticsData,

    -- ** DropDuplicates
    DropDuplicates (DropDuplicates'),
    newDropDuplicates,

    -- ** DropFields
    DropFields (DropFields'),
    newDropFields,

    -- ** DropNullFields
    DropNullFields (DropNullFields'),
    newDropNullFields,

    -- ** DynamicTransform
    DynamicTransform (DynamicTransform'),
    newDynamicTransform,

    -- ** DynamoDBCatalogSource
    DynamoDBCatalogSource (DynamoDBCatalogSource'),
    newDynamoDBCatalogSource,

    -- ** DynamoDBTarget
    DynamoDBTarget (DynamoDBTarget'),
    newDynamoDBTarget,

    -- ** Edge
    Edge (Edge'),
    newEdge,

    -- ** EncryptionAtRest
    EncryptionAtRest (EncryptionAtRest'),
    newEncryptionAtRest,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (EncryptionConfiguration'),
    newEncryptionConfiguration,

    -- ** ErrorDetail
    ErrorDetail (ErrorDetail'),
    newErrorDetail,

    -- ** ErrorDetails
    ErrorDetails (ErrorDetails'),
    newErrorDetails,

    -- ** EvaluateDataQuality
    EvaluateDataQuality (EvaluateDataQuality'),
    newEvaluateDataQuality,

    -- ** EvaluationMetrics
    EvaluationMetrics (EvaluationMetrics'),
    newEvaluationMetrics,

    -- ** EventBatchingCondition
    EventBatchingCondition (EventBatchingCondition'),
    newEventBatchingCondition,

    -- ** ExecutionProperty
    ExecutionProperty (ExecutionProperty'),
    newExecutionProperty,

    -- ** ExportLabelsTaskRunProperties
    ExportLabelsTaskRunProperties (ExportLabelsTaskRunProperties'),
    newExportLabelsTaskRunProperties,

    -- ** FillMissingValues
    FillMissingValues (FillMissingValues'),
    newFillMissingValues,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** FilterExpression
    FilterExpression (FilterExpression'),
    newFilterExpression,

    -- ** FilterValue
    FilterValue (FilterValue'),
    newFilterValue,

    -- ** FindMatchesMetrics
    FindMatchesMetrics (FindMatchesMetrics'),
    newFindMatchesMetrics,

    -- ** FindMatchesParameters
    FindMatchesParameters (FindMatchesParameters'),
    newFindMatchesParameters,

    -- ** FindMatchesTaskRunProperties
    FindMatchesTaskRunProperties (FindMatchesTaskRunProperties'),
    newFindMatchesTaskRunProperties,

    -- ** GetConnectionsFilter
    GetConnectionsFilter (GetConnectionsFilter'),
    newGetConnectionsFilter,

    -- ** GluePolicy
    GluePolicy (GluePolicy'),
    newGluePolicy,

    -- ** GlueSchema
    GlueSchema (GlueSchema'),
    newGlueSchema,

    -- ** GlueStudioSchemaColumn
    GlueStudioSchemaColumn (GlueStudioSchemaColumn'),
    newGlueStudioSchemaColumn,

    -- ** GlueTable
    GlueTable (GlueTable'),
    newGlueTable,

    -- ** GovernedCatalogSource
    GovernedCatalogSource (GovernedCatalogSource'),
    newGovernedCatalogSource,

    -- ** GovernedCatalogTarget
    GovernedCatalogTarget (GovernedCatalogTarget'),
    newGovernedCatalogTarget,

    -- ** GrokClassifier
    GrokClassifier (GrokClassifier'),
    newGrokClassifier,

    -- ** ImportLabelsTaskRunProperties
    ImportLabelsTaskRunProperties (ImportLabelsTaskRunProperties'),
    newImportLabelsTaskRunProperties,

    -- ** JDBCConnectorOptions
    JDBCConnectorOptions (JDBCConnectorOptions'),
    newJDBCConnectorOptions,

    -- ** JDBCConnectorSource
    JDBCConnectorSource (JDBCConnectorSource'),
    newJDBCConnectorSource,

    -- ** JDBCConnectorTarget
    JDBCConnectorTarget (JDBCConnectorTarget'),
    newJDBCConnectorTarget,

    -- ** JdbcTarget
    JdbcTarget (JdbcTarget'),
    newJdbcTarget,

    -- ** Job
    Job (Job'),
    newJob,

    -- ** JobBookmarkEntry
    JobBookmarkEntry (JobBookmarkEntry'),
    newJobBookmarkEntry,

    -- ** JobBookmarksEncryption
    JobBookmarksEncryption (JobBookmarksEncryption'),
    newJobBookmarksEncryption,

    -- ** JobCommand
    JobCommand (JobCommand'),
    newJobCommand,

    -- ** JobNodeDetails
    JobNodeDetails (JobNodeDetails'),
    newJobNodeDetails,

    -- ** JobRun
    JobRun (JobRun'),
    newJobRun,

    -- ** JobUpdate
    JobUpdate (JobUpdate'),
    newJobUpdate,

    -- ** Join
    Join (Join'),
    newJoin,

    -- ** JoinColumn
    JoinColumn (JoinColumn'),
    newJoinColumn,

    -- ** JsonClassifier
    JsonClassifier (JsonClassifier'),
    newJsonClassifier,

    -- ** KafkaStreamingSourceOptions
    KafkaStreamingSourceOptions (KafkaStreamingSourceOptions'),
    newKafkaStreamingSourceOptions,

    -- ** KeySchemaElement
    KeySchemaElement (KeySchemaElement'),
    newKeySchemaElement,

    -- ** KinesisStreamingSourceOptions
    KinesisStreamingSourceOptions (KinesisStreamingSourceOptions'),
    newKinesisStreamingSourceOptions,

    -- ** LabelingSetGenerationTaskRunProperties
    LabelingSetGenerationTaskRunProperties (LabelingSetGenerationTaskRunProperties'),
    newLabelingSetGenerationTaskRunProperties,

    -- ** LakeFormationConfiguration
    LakeFormationConfiguration (LakeFormationConfiguration'),
    newLakeFormationConfiguration,

    -- ** LastActiveDefinition
    LastActiveDefinition (LastActiveDefinition'),
    newLastActiveDefinition,

    -- ** LastCrawlInfo
    LastCrawlInfo (LastCrawlInfo'),
    newLastCrawlInfo,

    -- ** LineageConfiguration
    LineageConfiguration (LineageConfiguration'),
    newLineageConfiguration,

    -- ** Location
    Location (Location'),
    newLocation,

    -- ** LongColumnStatisticsData
    LongColumnStatisticsData (LongColumnStatisticsData'),
    newLongColumnStatisticsData,

    -- ** MLTransform
    MLTransform (MLTransform'),
    newMLTransform,

    -- ** MLUserDataEncryption
    MLUserDataEncryption (MLUserDataEncryption'),
    newMLUserDataEncryption,

    -- ** Mapping
    Mapping (Mapping'),
    newMapping,

    -- ** MappingEntry
    MappingEntry (MappingEntry'),
    newMappingEntry,

    -- ** Merge
    Merge (Merge'),
    newMerge,

    -- ** MetadataInfo
    MetadataInfo (MetadataInfo'),
    newMetadataInfo,

    -- ** MetadataKeyValuePair
    MetadataKeyValuePair (MetadataKeyValuePair'),
    newMetadataKeyValuePair,

    -- ** MicrosoftSQLServerCatalogSource
    MicrosoftSQLServerCatalogSource (MicrosoftSQLServerCatalogSource'),
    newMicrosoftSQLServerCatalogSource,

    -- ** MicrosoftSQLServerCatalogTarget
    MicrosoftSQLServerCatalogTarget (MicrosoftSQLServerCatalogTarget'),
    newMicrosoftSQLServerCatalogTarget,

    -- ** MongoDBTarget
    MongoDBTarget (MongoDBTarget'),
    newMongoDBTarget,

    -- ** MySQLCatalogSource
    MySQLCatalogSource (MySQLCatalogSource'),
    newMySQLCatalogSource,

    -- ** MySQLCatalogTarget
    MySQLCatalogTarget (MySQLCatalogTarget'),
    newMySQLCatalogTarget,

    -- ** Node
    Node (Node'),
    newNode,

    -- ** NotificationProperty
    NotificationProperty (NotificationProperty'),
    newNotificationProperty,

    -- ** NullCheckBoxList
    NullCheckBoxList (NullCheckBoxList'),
    newNullCheckBoxList,

    -- ** NullValueField
    NullValueField (NullValueField'),
    newNullValueField,

    -- ** OracleSQLCatalogSource
    OracleSQLCatalogSource (OracleSQLCatalogSource'),
    newOracleSQLCatalogSource,

    -- ** OracleSQLCatalogTarget
    OracleSQLCatalogTarget (OracleSQLCatalogTarget'),
    newOracleSQLCatalogTarget,

    -- ** Order
    Order (Order'),
    newOrder,

    -- ** OtherMetadataValueListItem
    OtherMetadataValueListItem (OtherMetadataValueListItem'),
    newOtherMetadataValueListItem,

    -- ** PIIDetection
    PIIDetection (PIIDetection'),
    newPIIDetection,

    -- ** Partition
    Partition (Partition'),
    newPartition,

    -- ** PartitionError
    PartitionError (PartitionError'),
    newPartitionError,

    -- ** PartitionIndex
    PartitionIndex (PartitionIndex'),
    newPartitionIndex,

    -- ** PartitionIndexDescriptor
    PartitionIndexDescriptor (PartitionIndexDescriptor'),
    newPartitionIndexDescriptor,

    -- ** PartitionInput
    PartitionInput (PartitionInput'),
    newPartitionInput,

    -- ** PartitionValueList
    PartitionValueList (PartitionValueList'),
    newPartitionValueList,

    -- ** PhysicalConnectionRequirements
    PhysicalConnectionRequirements (PhysicalConnectionRequirements'),
    newPhysicalConnectionRequirements,

    -- ** PostgreSQLCatalogSource
    PostgreSQLCatalogSource (PostgreSQLCatalogSource'),
    newPostgreSQLCatalogSource,

    -- ** PostgreSQLCatalogTarget
    PostgreSQLCatalogTarget (PostgreSQLCatalogTarget'),
    newPostgreSQLCatalogTarget,

    -- ** Predecessor
    Predecessor (Predecessor'),
    newPredecessor,

    -- ** Predicate
    Predicate (Predicate'),
    newPredicate,

    -- ** PrincipalPermissions
    PrincipalPermissions (PrincipalPermissions'),
    newPrincipalPermissions,

    -- ** PropertyPredicate
    PropertyPredicate (PropertyPredicate'),
    newPropertyPredicate,

    -- ** RecrawlPolicy
    RecrawlPolicy (RecrawlPolicy'),
    newRecrawlPolicy,

    -- ** RedshiftSource
    RedshiftSource (RedshiftSource'),
    newRedshiftSource,

    -- ** RedshiftTarget
    RedshiftTarget (RedshiftTarget'),
    newRedshiftTarget,

    -- ** RegistryId
    RegistryId (RegistryId'),
    newRegistryId,

    -- ** RegistryListItem
    RegistryListItem (RegistryListItem'),
    newRegistryListItem,

    -- ** RelationalCatalogSource
    RelationalCatalogSource (RelationalCatalogSource'),
    newRelationalCatalogSource,

    -- ** RenameField
    RenameField (RenameField'),
    newRenameField,

    -- ** ResourceUri
    ResourceUri (ResourceUri'),
    newResourceUri,

    -- ** S3CatalogSource
    S3CatalogSource (S3CatalogSource'),
    newS3CatalogSource,

    -- ** S3CatalogTarget
    S3CatalogTarget (S3CatalogTarget'),
    newS3CatalogTarget,

    -- ** S3CsvSource
    S3CsvSource (S3CsvSource'),
    newS3CsvSource,

    -- ** S3DirectSourceAdditionalOptions
    S3DirectSourceAdditionalOptions (S3DirectSourceAdditionalOptions'),
    newS3DirectSourceAdditionalOptions,

    -- ** S3DirectTarget
    S3DirectTarget (S3DirectTarget'),
    newS3DirectTarget,

    -- ** S3Encryption
    S3Encryption (S3Encryption'),
    newS3Encryption,

    -- ** S3GlueParquetTarget
    S3GlueParquetTarget (S3GlueParquetTarget'),
    newS3GlueParquetTarget,

    -- ** S3JsonSource
    S3JsonSource (S3JsonSource'),
    newS3JsonSource,

    -- ** S3ParquetSource
    S3ParquetSource (S3ParquetSource'),
    newS3ParquetSource,

    -- ** S3SourceAdditionalOptions
    S3SourceAdditionalOptions (S3SourceAdditionalOptions'),
    newS3SourceAdditionalOptions,

    -- ** S3Target
    S3Target (S3Target'),
    newS3Target,

    -- ** Schedule
    Schedule (Schedule'),
    newSchedule,

    -- ** SchemaChangePolicy
    SchemaChangePolicy (SchemaChangePolicy'),
    newSchemaChangePolicy,

    -- ** SchemaColumn
    SchemaColumn (SchemaColumn'),
    newSchemaColumn,

    -- ** SchemaId
    SchemaId (SchemaId'),
    newSchemaId,

    -- ** SchemaListItem
    SchemaListItem (SchemaListItem'),
    newSchemaListItem,

    -- ** SchemaReference
    SchemaReference (SchemaReference'),
    newSchemaReference,

    -- ** SchemaVersionErrorItem
    SchemaVersionErrorItem (SchemaVersionErrorItem'),
    newSchemaVersionErrorItem,

    -- ** SchemaVersionListItem
    SchemaVersionListItem (SchemaVersionListItem'),
    newSchemaVersionListItem,

    -- ** SchemaVersionNumber
    SchemaVersionNumber (SchemaVersionNumber'),
    newSchemaVersionNumber,

    -- ** SecurityConfiguration
    SecurityConfiguration (SecurityConfiguration'),
    newSecurityConfiguration,

    -- ** Segment
    Segment (Segment'),
    newSegment,

    -- ** SelectFields
    SelectFields (SelectFields'),
    newSelectFields,

    -- ** SelectFromCollection
    SelectFromCollection (SelectFromCollection'),
    newSelectFromCollection,

    -- ** SerDeInfo
    SerDeInfo (SerDeInfo'),
    newSerDeInfo,

    -- ** Session
    Session (Session'),
    newSession,

    -- ** SessionCommand
    SessionCommand (SessionCommand'),
    newSessionCommand,

    -- ** SkewedInfo
    SkewedInfo (SkewedInfo'),
    newSkewedInfo,

    -- ** SortCriterion
    SortCriterion (SortCriterion'),
    newSortCriterion,

    -- ** SourceControlDetails
    SourceControlDetails (SourceControlDetails'),
    newSourceControlDetails,

    -- ** SparkConnectorSource
    SparkConnectorSource (SparkConnectorSource'),
    newSparkConnectorSource,

    -- ** SparkConnectorTarget
    SparkConnectorTarget (SparkConnectorTarget'),
    newSparkConnectorTarget,

    -- ** SparkSQL
    SparkSQL (SparkSQL'),
    newSparkSQL,

    -- ** Spigot
    Spigot (Spigot'),
    newSpigot,

    -- ** SplitFields
    SplitFields (SplitFields'),
    newSplitFields,

    -- ** SqlAlias
    SqlAlias (SqlAlias'),
    newSqlAlias,

    -- ** StartingEventBatchCondition
    StartingEventBatchCondition (StartingEventBatchCondition'),
    newStartingEventBatchCondition,

    -- ** Statement
    Statement (Statement'),
    newStatement,

    -- ** StatementOutput
    StatementOutput (StatementOutput'),
    newStatementOutput,

    -- ** StatementOutputData
    StatementOutputData (StatementOutputData'),
    newStatementOutputData,

    -- ** StorageDescriptor
    StorageDescriptor (StorageDescriptor'),
    newStorageDescriptor,

    -- ** StreamingDataPreviewOptions
    StreamingDataPreviewOptions (StreamingDataPreviewOptions'),
    newStreamingDataPreviewOptions,

    -- ** StringColumnStatisticsData
    StringColumnStatisticsData (StringColumnStatisticsData'),
    newStringColumnStatisticsData,

    -- ** Table
    Table (Table'),
    newTable,

    -- ** TableError
    TableError (TableError'),
    newTableError,

    -- ** TableIdentifier
    TableIdentifier (TableIdentifier'),
    newTableIdentifier,

    -- ** TableInput
    TableInput (TableInput'),
    newTableInput,

    -- ** TableVersion
    TableVersion (TableVersion'),
    newTableVersion,

    -- ** TableVersionError
    TableVersionError (TableVersionError'),
    newTableVersionError,

    -- ** TaskRun
    TaskRun (TaskRun'),
    newTaskRun,

    -- ** TaskRunFilterCriteria
    TaskRunFilterCriteria (TaskRunFilterCriteria'),
    newTaskRunFilterCriteria,

    -- ** TaskRunProperties
    TaskRunProperties (TaskRunProperties'),
    newTaskRunProperties,

    -- ** TaskRunSortCriteria
    TaskRunSortCriteria (TaskRunSortCriteria'),
    newTaskRunSortCriteria,

    -- ** TransformConfigParameter
    TransformConfigParameter (TransformConfigParameter'),
    newTransformConfigParameter,

    -- ** TransformEncryption
    TransformEncryption (TransformEncryption'),
    newTransformEncryption,

    -- ** TransformFilterCriteria
    TransformFilterCriteria (TransformFilterCriteria'),
    newTransformFilterCriteria,

    -- ** TransformParameters
    TransformParameters (TransformParameters'),
    newTransformParameters,

    -- ** TransformSortCriteria
    TransformSortCriteria (TransformSortCriteria'),
    newTransformSortCriteria,

    -- ** Trigger
    Trigger (Trigger'),
    newTrigger,

    -- ** TriggerNodeDetails
    TriggerNodeDetails (TriggerNodeDetails'),
    newTriggerNodeDetails,

    -- ** TriggerUpdate
    TriggerUpdate (TriggerUpdate'),
    newTriggerUpdate,

    -- ** UnfilteredPartition
    UnfilteredPartition (UnfilteredPartition'),
    newUnfilteredPartition,

    -- ** Union
    Union (Union'),
    newUnion,

    -- ** UpdateCsvClassifierRequest
    UpdateCsvClassifierRequest (UpdateCsvClassifierRequest'),
    newUpdateCsvClassifierRequest,

    -- ** UpdateGrokClassifierRequest
    UpdateGrokClassifierRequest (UpdateGrokClassifierRequest'),
    newUpdateGrokClassifierRequest,

    -- ** UpdateJsonClassifierRequest
    UpdateJsonClassifierRequest (UpdateJsonClassifierRequest'),
    newUpdateJsonClassifierRequest,

    -- ** UpdateXMLClassifierRequest
    UpdateXMLClassifierRequest (UpdateXMLClassifierRequest'),
    newUpdateXMLClassifierRequest,

    -- ** UpsertRedshiftTargetOptions
    UpsertRedshiftTargetOptions (UpsertRedshiftTargetOptions'),
    newUpsertRedshiftTargetOptions,

    -- ** UserDefinedFunction
    UserDefinedFunction (UserDefinedFunction'),
    newUserDefinedFunction,

    -- ** UserDefinedFunctionInput
    UserDefinedFunctionInput (UserDefinedFunctionInput'),
    newUserDefinedFunctionInput,

    -- ** Workflow
    Workflow (Workflow'),
    newWorkflow,

    -- ** WorkflowGraph
    WorkflowGraph (WorkflowGraph'),
    newWorkflowGraph,

    -- ** WorkflowRun
    WorkflowRun (WorkflowRun'),
    newWorkflowRun,

    -- ** WorkflowRunStatistics
    WorkflowRunStatistics (WorkflowRunStatistics'),
    newWorkflowRunStatistics,

    -- ** XMLClassifier
    XMLClassifier (XMLClassifier'),
    newXMLClassifier,
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
import Amazonka.Glue.Lens
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
import Amazonka.Glue.Types
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
import Amazonka.Glue.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Glue'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
