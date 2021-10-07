{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Glue
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Glue
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** EntityNotFoundException
    _EntityNotFoundException,

    -- ** CrawlerNotRunningException
    _CrawlerNotRunningException,

    -- ** VersionMismatchException
    _VersionMismatchException,

    -- ** GlueEncryptionException
    _GlueEncryptionException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** ConcurrentRunsExceededException
    _ConcurrentRunsExceededException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** IllegalWorkflowStateException
    _IllegalWorkflowStateException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ConditionCheckFailureException
    _ConditionCheckFailureException,

    -- ** SchedulerTransitioningException
    _SchedulerTransitioningException,

    -- ** SchedulerRunningException
    _SchedulerRunningException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** CrawlerRunningException
    _CrawlerRunningException,

    -- ** MLTransformNotReadyException
    _MLTransformNotReadyException,

    -- ** ValidationException
    _ValidationException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** OperationTimeoutException
    _OperationTimeoutException,

    -- ** IllegalBlueprintStateException
    _IllegalBlueprintStateException,

    -- ** ConflictException
    _ConflictException,

    -- ** CrawlerStoppingException
    _CrawlerStoppingException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** NoScheduleException
    _NoScheduleException,

    -- ** ResourceNumberLimitExceededException
    _ResourceNumberLimitExceededException,

    -- ** SchedulerNotRunningException
    _SchedulerNotRunningException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateConnection
    UpdateConnection (UpdateConnection'),
    newUpdateConnection,
    UpdateConnectionResponse (UpdateConnectionResponse'),
    newUpdateConnectionResponse,

    -- ** UpdateColumnStatisticsForTable
    UpdateColumnStatisticsForTable (UpdateColumnStatisticsForTable'),
    newUpdateColumnStatisticsForTable,
    UpdateColumnStatisticsForTableResponse (UpdateColumnStatisticsForTableResponse'),
    newUpdateColumnStatisticsForTableResponse,

    -- ** DeleteSecurityConfiguration
    DeleteSecurityConfiguration (DeleteSecurityConfiguration'),
    newDeleteSecurityConfiguration,
    DeleteSecurityConfigurationResponse (DeleteSecurityConfigurationResponse'),
    newDeleteSecurityConfigurationResponse,

    -- ** CreateWorkflow
    CreateWorkflow (CreateWorkflow'),
    newCreateWorkflow,
    CreateWorkflowResponse (CreateWorkflowResponse'),
    newCreateWorkflowResponse,

    -- ** StartMLLabelingSetGenerationTaskRun
    StartMLLabelingSetGenerationTaskRun (StartMLLabelingSetGenerationTaskRun'),
    newStartMLLabelingSetGenerationTaskRun,
    StartMLLabelingSetGenerationTaskRunResponse (StartMLLabelingSetGenerationTaskRunResponse'),
    newStartMLLabelingSetGenerationTaskRunResponse,

    -- ** GetPartitions (Paginated)
    GetPartitions (GetPartitions'),
    newGetPartitions,
    GetPartitionsResponse (GetPartitionsResponse'),
    newGetPartitionsResponse,

    -- ** DeleteColumnStatisticsForTable
    DeleteColumnStatisticsForTable (DeleteColumnStatisticsForTable'),
    newDeleteColumnStatisticsForTable,
    DeleteColumnStatisticsForTableResponse (DeleteColumnStatisticsForTableResponse'),
    newDeleteColumnStatisticsForTableResponse,

    -- ** GetDataCatalogEncryptionSettings
    GetDataCatalogEncryptionSettings (GetDataCatalogEncryptionSettings'),
    newGetDataCatalogEncryptionSettings,
    GetDataCatalogEncryptionSettingsResponse (GetDataCatalogEncryptionSettingsResponse'),
    newGetDataCatalogEncryptionSettingsResponse,

    -- ** DeleteConnection
    DeleteConnection (DeleteConnection'),
    newDeleteConnection,
    DeleteConnectionResponse (DeleteConnectionResponse'),
    newDeleteConnectionResponse,

    -- ** GetSchema
    GetSchema (GetSchema'),
    newGetSchema,
    GetSchemaResponse (GetSchemaResponse'),
    newGetSchemaResponse,

    -- ** CheckSchemaVersionValidity
    CheckSchemaVersionValidity (CheckSchemaVersionValidity'),
    newCheckSchemaVersionValidity,
    CheckSchemaVersionValidityResponse (CheckSchemaVersionValidityResponse'),
    newCheckSchemaVersionValidityResponse,

    -- ** GetResourcePolicy
    GetResourcePolicy (GetResourcePolicy'),
    newGetResourcePolicy,
    GetResourcePolicyResponse (GetResourcePolicyResponse'),
    newGetResourcePolicyResponse,

    -- ** StartImportLabelsTaskRun
    StartImportLabelsTaskRun (StartImportLabelsTaskRun'),
    newStartImportLabelsTaskRun,
    StartImportLabelsTaskRunResponse (StartImportLabelsTaskRunResponse'),
    newStartImportLabelsTaskRunResponse,

    -- ** GetPartitionIndexes (Paginated)
    GetPartitionIndexes (GetPartitionIndexes'),
    newGetPartitionIndexes,
    GetPartitionIndexesResponse (GetPartitionIndexesResponse'),
    newGetPartitionIndexesResponse,

    -- ** GetCatalogImportStatus
    GetCatalogImportStatus (GetCatalogImportStatus'),
    newGetCatalogImportStatus,
    GetCatalogImportStatusResponse (GetCatalogImportStatusResponse'),
    newGetCatalogImportStatusResponse,

    -- ** StopCrawler
    StopCrawler (StopCrawler'),
    newStopCrawler,
    StopCrawlerResponse (StopCrawlerResponse'),
    newStopCrawlerResponse,

    -- ** UpdateRegistry
    UpdateRegistry (UpdateRegistry'),
    newUpdateRegistry,
    UpdateRegistryResponse (UpdateRegistryResponse'),
    newUpdateRegistryResponse,

    -- ** QuerySchemaVersionMetadata
    QuerySchemaVersionMetadata (QuerySchemaVersionMetadata'),
    newQuerySchemaVersionMetadata,
    QuerySchemaVersionMetadataResponse (QuerySchemaVersionMetadataResponse'),
    newQuerySchemaVersionMetadataResponse,

    -- ** DeleteRegistry
    DeleteRegistry (DeleteRegistry'),
    newDeleteRegistry,
    DeleteRegistryResponse (DeleteRegistryResponse'),
    newDeleteRegistryResponse,

    -- ** GetPartition
    GetPartition (GetPartition'),
    newGetPartition,
    GetPartitionResponse (GetPartitionResponse'),
    newGetPartitionResponse,

    -- ** ListMLTransforms
    ListMLTransforms (ListMLTransforms'),
    newListMLTransforms,
    ListMLTransformsResponse (ListMLTransformsResponse'),
    newListMLTransformsResponse,

    -- ** StartCrawler
    StartCrawler (StartCrawler'),
    newStartCrawler,
    StartCrawlerResponse (StartCrawlerResponse'),
    newStartCrawlerResponse,

    -- ** GetColumnStatisticsForPartition
    GetColumnStatisticsForPartition (GetColumnStatisticsForPartition'),
    newGetColumnStatisticsForPartition,
    GetColumnStatisticsForPartitionResponse (GetColumnStatisticsForPartitionResponse'),
    newGetColumnStatisticsForPartitionResponse,

    -- ** ListRegistries (Paginated)
    ListRegistries (ListRegistries'),
    newListRegistries,
    ListRegistriesResponse (ListRegistriesResponse'),
    newListRegistriesResponse,

    -- ** CreateRegistry
    CreateRegistry (CreateRegistry'),
    newCreateRegistry,
    CreateRegistryResponse (CreateRegistryResponse'),
    newCreateRegistryResponse,

    -- ** StartCrawlerSchedule
    StartCrawlerSchedule (StartCrawlerSchedule'),
    newStartCrawlerSchedule,
    StartCrawlerScheduleResponse (StartCrawlerScheduleResponse'),
    newStartCrawlerScheduleResponse,

    -- ** GetJob
    GetJob (GetJob'),
    newGetJob,
    GetJobResponse (GetJobResponse'),
    newGetJobResponse,

    -- ** DeleteTrigger
    DeleteTrigger (DeleteTrigger'),
    newDeleteTrigger,
    DeleteTriggerResponse (DeleteTriggerResponse'),
    newDeleteTriggerResponse,

    -- ** CreateMLTransform
    CreateMLTransform (CreateMLTransform'),
    newCreateMLTransform,
    CreateMLTransformResponse (CreateMLTransformResponse'),
    newCreateMLTransformResponse,

    -- ** StopCrawlerSchedule
    StopCrawlerSchedule (StopCrawlerSchedule'),
    newStopCrawlerSchedule,
    StopCrawlerScheduleResponse (StopCrawlerScheduleResponse'),
    newStopCrawlerScheduleResponse,

    -- ** UpdateTrigger
    UpdateTrigger (UpdateTrigger'),
    newUpdateTrigger,
    UpdateTriggerResponse (UpdateTriggerResponse'),
    newUpdateTriggerResponse,

    -- ** ListTriggers
    ListTriggers (ListTriggers'),
    newListTriggers,
    ListTriggersResponse (ListTriggersResponse'),
    newListTriggersResponse,

    -- ** GetSchemaByDefinition
    GetSchemaByDefinition (GetSchemaByDefinition'),
    newGetSchemaByDefinition,
    GetSchemaByDefinitionResponse (GetSchemaByDefinitionResponse'),
    newGetSchemaByDefinitionResponse,

    -- ** DeleteClassifier
    DeleteClassifier (DeleteClassifier'),
    newDeleteClassifier,
    DeleteClassifierResponse (DeleteClassifierResponse'),
    newDeleteClassifierResponse,

    -- ** StartBlueprintRun
    StartBlueprintRun (StartBlueprintRun'),
    newStartBlueprintRun,
    StartBlueprintRunResponse (StartBlueprintRunResponse'),
    newStartBlueprintRunResponse,

    -- ** UpdateJob
    UpdateJob (UpdateJob'),
    newUpdateJob,
    UpdateJobResponse (UpdateJobResponse'),
    newUpdateJobResponse,

    -- ** DeleteJob
    DeleteJob (DeleteJob'),
    newDeleteJob,
    DeleteJobResponse (DeleteJobResponse'),
    newDeleteJobResponse,

    -- ** UpdateClassifier
    UpdateClassifier (UpdateClassifier'),
    newUpdateClassifier,
    UpdateClassifierResponse (UpdateClassifierResponse'),
    newUpdateClassifierResponse,

    -- ** ListBlueprints
    ListBlueprints (ListBlueprints'),
    newListBlueprints,
    ListBlueprintsResponse (ListBlueprintsResponse'),
    newListBlueprintsResponse,

    -- ** CreateUserDefinedFunction
    CreateUserDefinedFunction (CreateUserDefinedFunction'),
    newCreateUserDefinedFunction,
    CreateUserDefinedFunctionResponse (CreateUserDefinedFunctionResponse'),
    newCreateUserDefinedFunctionResponse,

    -- ** GetTrigger
    GetTrigger (GetTrigger'),
    newGetTrigger,
    GetTriggerResponse (GetTriggerResponse'),
    newGetTriggerResponse,

    -- ** BatchGetJobs
    BatchGetJobs (BatchGetJobs'),
    newBatchGetJobs,
    BatchGetJobsResponse (BatchGetJobsResponse'),
    newBatchGetJobsResponse,

    -- ** GetDatabase
    GetDatabase (GetDatabase'),
    newGetDatabase,
    GetDatabaseResponse (GetDatabaseResponse'),
    newGetDatabaseResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** StartMLEvaluationTaskRun
    StartMLEvaluationTaskRun (StartMLEvaluationTaskRun'),
    newStartMLEvaluationTaskRun,
    StartMLEvaluationTaskRunResponse (StartMLEvaluationTaskRunResponse'),
    newStartMLEvaluationTaskRunResponse,

    -- ** DeletePartition
    DeletePartition (DeletePartition'),
    newDeletePartition,
    DeletePartitionResponse (DeletePartitionResponse'),
    newDeletePartitionResponse,

    -- ** CreateBlueprint
    CreateBlueprint (CreateBlueprint'),
    newCreateBlueprint,
    CreateBlueprintResponse (CreateBlueprintResponse'),
    newCreateBlueprintResponse,

    -- ** BatchDeleteTable
    BatchDeleteTable (BatchDeleteTable'),
    newBatchDeleteTable,
    BatchDeleteTableResponse (BatchDeleteTableResponse'),
    newBatchDeleteTableResponse,

    -- ** UpdatePartition
    UpdatePartition (UpdatePartition'),
    newUpdatePartition,
    UpdatePartitionResponse (UpdatePartitionResponse'),
    newUpdatePartitionResponse,

    -- ** GetSchemaVersionsDiff
    GetSchemaVersionsDiff (GetSchemaVersionsDiff'),
    newGetSchemaVersionsDiff,
    GetSchemaVersionsDiffResponse (GetSchemaVersionsDiffResponse'),
    newGetSchemaVersionsDiffResponse,

    -- ** GetJobRuns (Paginated)
    GetJobRuns (GetJobRuns'),
    newGetJobRuns,
    GetJobRunsResponse (GetJobRunsResponse'),
    newGetJobRunsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetSecurityConfigurations (Paginated)
    GetSecurityConfigurations (GetSecurityConfigurations'),
    newGetSecurityConfigurations,
    GetSecurityConfigurationsResponse (GetSecurityConfigurationsResponse'),
    newGetSecurityConfigurationsResponse,

    -- ** CreateClassifier
    CreateClassifier (CreateClassifier'),
    newCreateClassifier,
    CreateClassifierResponse (CreateClassifierResponse'),
    newCreateClassifierResponse,

    -- ** CreatePartitionIndex
    CreatePartitionIndex (CreatePartitionIndex'),
    newCreatePartitionIndex,
    CreatePartitionIndexResponse (CreatePartitionIndexResponse'),
    newCreatePartitionIndexResponse,

    -- ** GetSecurityConfiguration
    GetSecurityConfiguration (GetSecurityConfiguration'),
    newGetSecurityConfiguration,
    GetSecurityConfigurationResponse (GetSecurityConfigurationResponse'),
    newGetSecurityConfigurationResponse,

    -- ** GetMLTransforms
    GetMLTransforms (GetMLTransforms'),
    newGetMLTransforms,
    GetMLTransformsResponse (GetMLTransformsResponse'),
    newGetMLTransformsResponse,

    -- ** GetJobRun
    GetJobRun (GetJobRun'),
    newGetJobRun,
    GetJobRunResponse (GetJobRunResponse'),
    newGetJobRunResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CreateCrawler
    CreateCrawler (CreateCrawler'),
    newCreateCrawler,
    CreateCrawlerResponse (CreateCrawlerResponse'),
    newCreateCrawlerResponse,

    -- ** CreateDevEndpoint
    CreateDevEndpoint (CreateDevEndpoint'),
    newCreateDevEndpoint,
    CreateDevEndpointResponse (CreateDevEndpointResponse'),
    newCreateDevEndpointResponse,

    -- ** GetMLTaskRuns
    GetMLTaskRuns (GetMLTaskRuns'),
    newGetMLTaskRuns,
    GetMLTaskRunsResponse (GetMLTaskRunsResponse'),
    newGetMLTaskRunsResponse,

    -- ** DeleteCrawler
    DeleteCrawler (DeleteCrawler'),
    newDeleteCrawler,
    DeleteCrawlerResponse (DeleteCrawlerResponse'),
    newDeleteCrawlerResponse,

    -- ** ListDevEndpoints
    ListDevEndpoints (ListDevEndpoints'),
    newListDevEndpoints,
    ListDevEndpointsResponse (ListDevEndpointsResponse'),
    newListDevEndpointsResponse,

    -- ** DeleteDevEndpoint
    DeleteDevEndpoint (DeleteDevEndpoint'),
    newDeleteDevEndpoint,
    DeleteDevEndpointResponse (DeleteDevEndpointResponse'),
    newDeleteDevEndpointResponse,

    -- ** UpdateDevEndpoint
    UpdateDevEndpoint (UpdateDevEndpoint'),
    newUpdateDevEndpoint,
    UpdateDevEndpointResponse (UpdateDevEndpointResponse'),
    newUpdateDevEndpointResponse,

    -- ** UpdateCrawler
    UpdateCrawler (UpdateCrawler'),
    newUpdateCrawler,
    UpdateCrawlerResponse (UpdateCrawlerResponse'),
    newUpdateCrawlerResponse,

    -- ** GetSchemaVersion
    GetSchemaVersion (GetSchemaVersion'),
    newGetSchemaVersion,
    GetSchemaVersionResponse (GetSchemaVersionResponse'),
    newGetSchemaVersionResponse,

    -- ** CreateSchema
    CreateSchema (CreateSchema'),
    newCreateSchema,
    CreateSchemaResponse (CreateSchemaResponse'),
    newCreateSchemaResponse,

    -- ** GetWorkflow
    GetWorkflow (GetWorkflow'),
    newGetWorkflow,
    GetWorkflowResponse (GetWorkflowResponse'),
    newGetWorkflowResponse,

    -- ** ListCrawlers
    ListCrawlers (ListCrawlers'),
    newListCrawlers,
    ListCrawlersResponse (ListCrawlersResponse'),
    newListCrawlersResponse,

    -- ** CreateTable
    CreateTable (CreateTable'),
    newCreateTable,
    CreateTableResponse (CreateTableResponse'),
    newCreateTableResponse,

    -- ** GetMapping
    GetMapping (GetMapping'),
    newGetMapping,
    GetMappingResponse (GetMappingResponse'),
    newGetMappingResponse,

    -- ** BatchStopJobRun
    BatchStopJobRun (BatchStopJobRun'),
    newBatchStopJobRun,
    BatchStopJobRunResponse (BatchStopJobRunResponse'),
    newBatchStopJobRunResponse,

    -- ** StartWorkflowRun
    StartWorkflowRun (StartWorkflowRun'),
    newStartWorkflowRun,
    StartWorkflowRunResponse (StartWorkflowRunResponse'),
    newStartWorkflowRunResponse,

    -- ** CreateConnection
    CreateConnection (CreateConnection'),
    newCreateConnection,
    CreateConnectionResponse (CreateConnectionResponse'),
    newCreateConnectionResponse,

    -- ** GetClassifiers (Paginated)
    GetClassifiers (GetClassifiers'),
    newGetClassifiers,
    GetClassifiersResponse (GetClassifiersResponse'),
    newGetClassifiersResponse,

    -- ** BatchCreatePartition
    BatchCreatePartition (BatchCreatePartition'),
    newBatchCreatePartition,
    BatchCreatePartitionResponse (BatchCreatePartitionResponse'),
    newBatchCreatePartitionResponse,

    -- ** ListWorkflows
    ListWorkflows (ListWorkflows'),
    newListWorkflows,
    ListWorkflowsResponse (ListWorkflowsResponse'),
    newListWorkflowsResponse,

    -- ** UpdateWorkflow
    UpdateWorkflow (UpdateWorkflow'),
    newUpdateWorkflow,
    UpdateWorkflowResponse (UpdateWorkflowResponse'),
    newUpdateWorkflowResponse,

    -- ** RegisterSchemaVersion
    RegisterSchemaVersion (RegisterSchemaVersion'),
    newRegisterSchemaVersion,
    RegisterSchemaVersionResponse (RegisterSchemaVersionResponse'),
    newRegisterSchemaVersionResponse,

    -- ** StopWorkflowRun
    StopWorkflowRun (StopWorkflowRun'),
    newStopWorkflowRun,
    StopWorkflowRunResponse (StopWorkflowRunResponse'),
    newStopWorkflowRunResponse,

    -- ** DeleteWorkflow
    DeleteWorkflow (DeleteWorkflow'),
    newDeleteWorkflow,
    DeleteWorkflowResponse (DeleteWorkflowResponse'),
    newDeleteWorkflowResponse,

    -- ** ListSchemaVersions (Paginated)
    ListSchemaVersions (ListSchemaVersions'),
    newListSchemaVersions,
    ListSchemaVersionsResponse (ListSchemaVersionsResponse'),
    newListSchemaVersionsResponse,

    -- ** BatchUpdatePartition
    BatchUpdatePartition (BatchUpdatePartition'),
    newBatchUpdatePartition,
    BatchUpdatePartitionResponse (BatchUpdatePartitionResponse'),
    newBatchUpdatePartitionResponse,

    -- ** BatchDeletePartition
    BatchDeletePartition (BatchDeletePartition'),
    newBatchDeletePartition,
    BatchDeletePartitionResponse (BatchDeletePartitionResponse'),
    newBatchDeletePartitionResponse,

    -- ** GetWorkflowRuns
    GetWorkflowRuns (GetWorkflowRuns'),
    newGetWorkflowRuns,
    GetWorkflowRunsResponse (GetWorkflowRunsResponse'),
    newGetWorkflowRunsResponse,

    -- ** GetTags
    GetTags (GetTags'),
    newGetTags,
    GetTagsResponse (GetTagsResponse'),
    newGetTagsResponse,

    -- ** UpdateTable
    UpdateTable (UpdateTable'),
    newUpdateTable,
    UpdateTableResponse (UpdateTableResponse'),
    newUpdateTableResponse,

    -- ** DeleteTable
    DeleteTable (DeleteTable'),
    newDeleteTable,
    DeleteTableResponse (DeleteTableResponse'),
    newDeleteTableResponse,

    -- ** GetUserDefinedFunctions (Paginated)
    GetUserDefinedFunctions (GetUserDefinedFunctions'),
    newGetUserDefinedFunctions,
    GetUserDefinedFunctionsResponse (GetUserDefinedFunctionsResponse'),
    newGetUserDefinedFunctionsResponse,

    -- ** PutSchemaVersionMetadata
    PutSchemaVersionMetadata (PutSchemaVersionMetadata'),
    newPutSchemaVersionMetadata,
    PutSchemaVersionMetadataResponse (PutSchemaVersionMetadataResponse'),
    newPutSchemaVersionMetadataResponse,

    -- ** UpdateDatabase
    UpdateDatabase (UpdateDatabase'),
    newUpdateDatabase,
    UpdateDatabaseResponse (UpdateDatabaseResponse'),
    newUpdateDatabaseResponse,

    -- ** GetUserDefinedFunction
    GetUserDefinedFunction (GetUserDefinedFunction'),
    newGetUserDefinedFunction,
    GetUserDefinedFunctionResponse (GetUserDefinedFunctionResponse'),
    newGetUserDefinedFunctionResponse,

    -- ** CreateTrigger
    CreateTrigger (CreateTrigger'),
    newCreateTrigger,
    CreateTriggerResponse (CreateTriggerResponse'),
    newCreateTriggerResponse,

    -- ** DeleteDatabase
    DeleteDatabase (DeleteDatabase'),
    newDeleteDatabase,
    DeleteDatabaseResponse (DeleteDatabaseResponse'),
    newDeleteDatabaseResponse,

    -- ** UpdateMLTransform
    UpdateMLTransform (UpdateMLTransform'),
    newUpdateMLTransform,
    UpdateMLTransformResponse (UpdateMLTransformResponse'),
    newUpdateMLTransformResponse,

    -- ** DeleteMLTransform
    DeleteMLTransform (DeleteMLTransform'),
    newDeleteMLTransform,
    DeleteMLTransformResponse (DeleteMLTransformResponse'),
    newDeleteMLTransformResponse,

    -- ** GetWorkflowRun
    GetWorkflowRun (GetWorkflowRun'),
    newGetWorkflowRun,
    GetWorkflowRunResponse (GetWorkflowRunResponse'),
    newGetWorkflowRunResponse,

    -- ** GetTableVersions (Paginated)
    GetTableVersions (GetTableVersions'),
    newGetTableVersions,
    GetTableVersionsResponse (GetTableVersionsResponse'),
    newGetTableVersionsResponse,

    -- ** DeleteSchemaVersions
    DeleteSchemaVersions (DeleteSchemaVersions'),
    newDeleteSchemaVersions,
    DeleteSchemaVersionsResponse (DeleteSchemaVersionsResponse'),
    newDeleteSchemaVersionsResponse,

    -- ** BatchGetTriggers
    BatchGetTriggers (BatchGetTriggers'),
    newBatchGetTriggers,
    BatchGetTriggersResponse (BatchGetTriggersResponse'),
    newBatchGetTriggersResponse,

    -- ** GetClassifier
    GetClassifier (GetClassifier'),
    newGetClassifier,
    GetClassifierResponse (GetClassifierResponse'),
    newGetClassifierResponse,

    -- ** CreateDatabase
    CreateDatabase (CreateDatabase'),
    newCreateDatabase,
    CreateDatabaseResponse (CreateDatabaseResponse'),
    newCreateDatabaseResponse,

    -- ** GetCrawlers (Paginated)
    GetCrawlers (GetCrawlers'),
    newGetCrawlers,
    GetCrawlersResponse (GetCrawlersResponse'),
    newGetCrawlersResponse,

    -- ** GetBlueprint
    GetBlueprint (GetBlueprint'),
    newGetBlueprint,
    GetBlueprintResponse (GetBlueprintResponse'),
    newGetBlueprintResponse,

    -- ** GetDevEndpoints (Paginated)
    GetDevEndpoints (GetDevEndpoints'),
    newGetDevEndpoints,
    GetDevEndpointsResponse (GetDevEndpointsResponse'),
    newGetDevEndpointsResponse,

    -- ** BatchDeleteTableVersion
    BatchDeleteTableVersion (BatchDeleteTableVersion'),
    newBatchDeleteTableVersion,
    BatchDeleteTableVersionResponse (BatchDeleteTableVersionResponse'),
    newBatchDeleteTableVersionResponse,

    -- ** DeleteBlueprint
    DeleteBlueprint (DeleteBlueprint'),
    newDeleteBlueprint,
    DeleteBlueprintResponse (DeleteBlueprintResponse'),
    newDeleteBlueprintResponse,

    -- ** StartJobRun
    StartJobRun (StartJobRun'),
    newStartJobRun,
    StartJobRunResponse (StartJobRunResponse'),
    newStartJobRunResponse,

    -- ** ImportCatalogToGlue
    ImportCatalogToGlue (ImportCatalogToGlue'),
    newImportCatalogToGlue,
    ImportCatalogToGlueResponse (ImportCatalogToGlueResponse'),
    newImportCatalogToGlueResponse,

    -- ** CreatePartition
    CreatePartition (CreatePartition'),
    newCreatePartition,
    CreatePartitionResponse (CreatePartitionResponse'),
    newCreatePartitionResponse,

    -- ** UpdateBlueprint
    UpdateBlueprint (UpdateBlueprint'),
    newUpdateBlueprint,
    UpdateBlueprintResponse (UpdateBlueprintResponse'),
    newUpdateBlueprintResponse,

    -- ** ListJobs
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** ResetJobBookmark
    ResetJobBookmark (ResetJobBookmark'),
    newResetJobBookmark,
    ResetJobBookmarkResponse (ResetJobBookmarkResponse'),
    newResetJobBookmarkResponse,

    -- ** BatchGetBlueprints
    BatchGetBlueprints (BatchGetBlueprints'),
    newBatchGetBlueprints,
    BatchGetBlueprintsResponse (BatchGetBlueprintsResponse'),
    newBatchGetBlueprintsResponse,

    -- ** GetMLTransform
    GetMLTransform (GetMLTransform'),
    newGetMLTransform,
    GetMLTransformResponse (GetMLTransformResponse'),
    newGetMLTransformResponse,

    -- ** CreateScript
    CreateScript (CreateScript'),
    newCreateScript,
    CreateScriptResponse (CreateScriptResponse'),
    newCreateScriptResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** GetRegistry
    GetRegistry (GetRegistry'),
    newGetRegistry,
    GetRegistryResponse (GetRegistryResponse'),
    newGetRegistryResponse,

    -- ** GetMLTaskRun
    GetMLTaskRun (GetMLTaskRun'),
    newGetMLTaskRun,
    GetMLTaskRunResponse (GetMLTaskRunResponse'),
    newGetMLTaskRunResponse,

    -- ** CreateJob
    CreateJob (CreateJob'),
    newCreateJob,
    CreateJobResponse (CreateJobResponse'),
    newCreateJobResponse,

    -- ** UpdateUserDefinedFunction
    UpdateUserDefinedFunction (UpdateUserDefinedFunction'),
    newUpdateUserDefinedFunction,
    UpdateUserDefinedFunctionResponse (UpdateUserDefinedFunctionResponse'),
    newUpdateUserDefinedFunctionResponse,

    -- ** DeleteColumnStatisticsForPartition
    DeleteColumnStatisticsForPartition (DeleteColumnStatisticsForPartition'),
    newDeleteColumnStatisticsForPartition,
    DeleteColumnStatisticsForPartitionResponse (DeleteColumnStatisticsForPartitionResponse'),
    newDeleteColumnStatisticsForPartitionResponse,

    -- ** DeleteUserDefinedFunction
    DeleteUserDefinedFunction (DeleteUserDefinedFunction'),
    newDeleteUserDefinedFunction,
    DeleteUserDefinedFunctionResponse (DeleteUserDefinedFunctionResponse'),
    newDeleteUserDefinedFunctionResponse,

    -- ** CancelMLTaskRun
    CancelMLTaskRun (CancelMLTaskRun'),
    newCancelMLTaskRun,
    CancelMLTaskRunResponse (CancelMLTaskRunResponse'),
    newCancelMLTaskRunResponse,

    -- ** SearchTables
    SearchTables (SearchTables'),
    newSearchTables,
    SearchTablesResponse (SearchTablesResponse'),
    newSearchTablesResponse,

    -- ** UpdateColumnStatisticsForPartition
    UpdateColumnStatisticsForPartition (UpdateColumnStatisticsForPartition'),
    newUpdateColumnStatisticsForPartition,
    UpdateColumnStatisticsForPartitionResponse (UpdateColumnStatisticsForPartitionResponse'),
    newUpdateColumnStatisticsForPartitionResponse,

    -- ** GetTables (Paginated)
    GetTables (GetTables'),
    newGetTables,
    GetTablesResponse (GetTablesResponse'),
    newGetTablesResponse,

    -- ** BatchDeleteConnection
    BatchDeleteConnection (BatchDeleteConnection'),
    newBatchDeleteConnection,
    BatchDeleteConnectionResponse (BatchDeleteConnectionResponse'),
    newBatchDeleteConnectionResponse,

    -- ** GetConnections (Paginated)
    GetConnections (GetConnections'),
    newGetConnections,
    GetConnectionsResponse (GetConnectionsResponse'),
    newGetConnectionsResponse,

    -- ** GetBlueprintRuns
    GetBlueprintRuns (GetBlueprintRuns'),
    newGetBlueprintRuns,
    GetBlueprintRunsResponse (GetBlueprintRunsResponse'),
    newGetBlueprintRunsResponse,

    -- ** ResumeWorkflowRun
    ResumeWorkflowRun (ResumeWorkflowRun'),
    newResumeWorkflowRun,
    ResumeWorkflowRunResponse (ResumeWorkflowRunResponse'),
    newResumeWorkflowRunResponse,

    -- ** UpdateCrawlerSchedule
    UpdateCrawlerSchedule (UpdateCrawlerSchedule'),
    newUpdateCrawlerSchedule,
    UpdateCrawlerScheduleResponse (UpdateCrawlerScheduleResponse'),
    newUpdateCrawlerScheduleResponse,

    -- ** StartTrigger
    StartTrigger (StartTrigger'),
    newStartTrigger,
    StartTriggerResponse (StartTriggerResponse'),
    newStartTriggerResponse,

    -- ** GetConnection
    GetConnection (GetConnection'),
    newGetConnection,
    GetConnectionResponse (GetConnectionResponse'),
    newGetConnectionResponse,

    -- ** RemoveSchemaVersionMetadata
    RemoveSchemaVersionMetadata (RemoveSchemaVersionMetadata'),
    newRemoveSchemaVersionMetadata,
    RemoveSchemaVersionMetadataResponse (RemoveSchemaVersionMetadataResponse'),
    newRemoveSchemaVersionMetadataResponse,

    -- ** GetTable
    GetTable (GetTable'),
    newGetTable,
    GetTableResponse (GetTableResponse'),
    newGetTableResponse,

    -- ** DeleteSchema
    DeleteSchema (DeleteSchema'),
    newDeleteSchema,
    DeleteSchemaResponse (DeleteSchemaResponse'),
    newDeleteSchemaResponse,

    -- ** PutDataCatalogEncryptionSettings
    PutDataCatalogEncryptionSettings (PutDataCatalogEncryptionSettings'),
    newPutDataCatalogEncryptionSettings,
    PutDataCatalogEncryptionSettingsResponse (PutDataCatalogEncryptionSettingsResponse'),
    newPutDataCatalogEncryptionSettingsResponse,

    -- ** GetBlueprintRun
    GetBlueprintRun (GetBlueprintRun'),
    newGetBlueprintRun,
    GetBlueprintRunResponse (GetBlueprintRunResponse'),
    newGetBlueprintRunResponse,

    -- ** UpdateSchema
    UpdateSchema (UpdateSchema'),
    newUpdateSchema,
    UpdateSchemaResponse (UpdateSchemaResponse'),
    newUpdateSchemaResponse,

    -- ** StopTrigger
    StopTrigger (StopTrigger'),
    newStopTrigger,
    StopTriggerResponse (StopTriggerResponse'),
    newStopTriggerResponse,

    -- ** ListSchemas (Paginated)
    ListSchemas (ListSchemas'),
    newListSchemas,
    ListSchemasResponse (ListSchemasResponse'),
    newListSchemasResponse,

    -- ** GetDataflowGraph
    GetDataflowGraph (GetDataflowGraph'),
    newGetDataflowGraph,
    GetDataflowGraphResponse (GetDataflowGraphResponse'),
    newGetDataflowGraphResponse,

    -- ** BatchGetPartition
    BatchGetPartition (BatchGetPartition'),
    newBatchGetPartition,
    BatchGetPartitionResponse (BatchGetPartitionResponse'),
    newBatchGetPartitionResponse,

    -- ** GetColumnStatisticsForTable
    GetColumnStatisticsForTable (GetColumnStatisticsForTable'),
    newGetColumnStatisticsForTable,
    GetColumnStatisticsForTableResponse (GetColumnStatisticsForTableResponse'),
    newGetColumnStatisticsForTableResponse,

    -- ** GetDatabases (Paginated)
    GetDatabases (GetDatabases'),
    newGetDatabases,
    GetDatabasesResponse (GetDatabasesResponse'),
    newGetDatabasesResponse,

    -- ** DeleteTableVersion
    DeleteTableVersion (DeleteTableVersion'),
    newDeleteTableVersion,
    DeleteTableVersionResponse (DeleteTableVersionResponse'),
    newDeleteTableVersionResponse,

    -- ** GetTriggers (Paginated)
    GetTriggers (GetTriggers'),
    newGetTriggers,
    GetTriggersResponse (GetTriggersResponse'),
    newGetTriggersResponse,

    -- ** BatchGetCrawlers
    BatchGetCrawlers (BatchGetCrawlers'),
    newBatchGetCrawlers,
    BatchGetCrawlersResponse (BatchGetCrawlersResponse'),
    newBatchGetCrawlersResponse,

    -- ** GetJobBookmark
    GetJobBookmark (GetJobBookmark'),
    newGetJobBookmark,
    GetJobBookmarkResponse (GetJobBookmarkResponse'),
    newGetJobBookmarkResponse,

    -- ** StartExportLabelsTaskRun
    StartExportLabelsTaskRun (StartExportLabelsTaskRun'),
    newStartExportLabelsTaskRun,
    StartExportLabelsTaskRunResponse (StartExportLabelsTaskRunResponse'),
    newStartExportLabelsTaskRunResponse,

    -- ** GetWorkflowRunProperties
    GetWorkflowRunProperties (GetWorkflowRunProperties'),
    newGetWorkflowRunProperties,
    GetWorkflowRunPropertiesResponse (GetWorkflowRunPropertiesResponse'),
    newGetWorkflowRunPropertiesResponse,

    -- ** GetCrawlerMetrics (Paginated)
    GetCrawlerMetrics (GetCrawlerMetrics'),
    newGetCrawlerMetrics,
    GetCrawlerMetricsResponse (GetCrawlerMetricsResponse'),
    newGetCrawlerMetricsResponse,

    -- ** DeletePartitionIndex
    DeletePartitionIndex (DeletePartitionIndex'),
    newDeletePartitionIndex,
    DeletePartitionIndexResponse (DeletePartitionIndexResponse'),
    newDeletePartitionIndexResponse,

    -- ** BatchGetDevEndpoints
    BatchGetDevEndpoints (BatchGetDevEndpoints'),
    newBatchGetDevEndpoints,
    BatchGetDevEndpointsResponse (BatchGetDevEndpointsResponse'),
    newBatchGetDevEndpointsResponse,

    -- ** GetPlan
    GetPlan (GetPlan'),
    newGetPlan,
    GetPlanResponse (GetPlanResponse'),
    newGetPlanResponse,

    -- ** CreateSecurityConfiguration
    CreateSecurityConfiguration (CreateSecurityConfiguration'),
    newCreateSecurityConfiguration,
    CreateSecurityConfigurationResponse (CreateSecurityConfigurationResponse'),
    newCreateSecurityConfigurationResponse,

    -- ** GetResourcePolicies (Paginated)
    GetResourcePolicies (GetResourcePolicies'),
    newGetResourcePolicies,
    GetResourcePoliciesResponse (GetResourcePoliciesResponse'),
    newGetResourcePoliciesResponse,

    -- ** GetDevEndpoint
    GetDevEndpoint (GetDevEndpoint'),
    newGetDevEndpoint,
    GetDevEndpointResponse (GetDevEndpointResponse'),
    newGetDevEndpointResponse,

    -- ** PutWorkflowRunProperties
    PutWorkflowRunProperties (PutWorkflowRunProperties'),
    newPutWorkflowRunProperties,
    PutWorkflowRunPropertiesResponse (PutWorkflowRunPropertiesResponse'),
    newPutWorkflowRunPropertiesResponse,

    -- ** BatchGetWorkflows
    BatchGetWorkflows (BatchGetWorkflows'),
    newBatchGetWorkflows,
    BatchGetWorkflowsResponse (BatchGetWorkflowsResponse'),
    newBatchGetWorkflowsResponse,

    -- ** GetTableVersion
    GetTableVersion (GetTableVersion'),
    newGetTableVersion,
    GetTableVersionResponse (GetTableVersionResponse'),
    newGetTableVersionResponse,

    -- ** GetJobs (Paginated)
    GetJobs (GetJobs'),
    newGetJobs,
    GetJobsResponse (GetJobsResponse'),
    newGetJobsResponse,

    -- ** GetCrawler
    GetCrawler (GetCrawler'),
    newGetCrawler,
    GetCrawlerResponse (GetCrawlerResponse'),
    newGetCrawlerResponse,

    -- * Types

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

    -- ** ConnectionPropertyKey
    ConnectionPropertyKey (..),

    -- ** ConnectionType
    ConnectionType (..),

    -- ** CrawlState
    CrawlState (..),

    -- ** CrawlerLineageSettings
    CrawlerLineageSettings (..),

    -- ** CrawlerState
    CrawlerState (..),

    -- ** CsvHeaderOption
    CsvHeaderOption (..),

    -- ** DataFormat
    DataFormat (..),

    -- ** DeleteBehavior
    DeleteBehavior (..),

    -- ** EnableHybridValues
    EnableHybridValues (..),

    -- ** ExistCondition
    ExistCondition (..),

    -- ** JobBookmarksEncryptionMode
    JobBookmarksEncryptionMode (..),

    -- ** JobRunState
    JobRunState (..),

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

    -- ** PartitionIndexStatus
    PartitionIndexStatus (..),

    -- ** Permission
    Permission (..),

    -- ** PrincipalType
    PrincipalType (..),

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

    -- ** Sort
    Sort (..),

    -- ** SortDirectionType
    SortDirectionType (..),

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

    -- ** UpdateBehavior
    UpdateBehavior (..),

    -- ** WorkerType
    WorkerType (..),

    -- ** WorkflowRunStatus
    WorkflowRunStatus (..),

    -- ** Action
    Action (Action'),
    newAction,

    -- ** BackfillError
    BackfillError (BackfillError'),
    newBackfillError,

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

    -- ** CatalogTarget
    CatalogTarget (CatalogTarget'),
    newCatalogTarget,

    -- ** Classifier
    Classifier (Classifier'),
    newClassifier,

    -- ** CloudWatchEncryption
    CloudWatchEncryption (CloudWatchEncryption'),
    newCloudWatchEncryption,

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

    -- ** CrawlerMetrics
    CrawlerMetrics (CrawlerMetrics'),
    newCrawlerMetrics,

    -- ** CrawlerNodeDetails
    CrawlerNodeDetails (CrawlerNodeDetails'),
    newCrawlerNodeDetails,

    -- ** CrawlerTargets
    CrawlerTargets (CrawlerTargets'),
    newCrawlerTargets,

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

    -- ** DataCatalogEncryptionSettings
    DataCatalogEncryptionSettings (DataCatalogEncryptionSettings'),
    newDataCatalogEncryptionSettings,

    -- ** DataLakePrincipal
    DataLakePrincipal (DataLakePrincipal'),
    newDataLakePrincipal,

    -- ** Database
    Database (Database'),
    newDatabase,

    -- ** DatabaseIdentifier
    DatabaseIdentifier (DatabaseIdentifier'),
    newDatabaseIdentifier,

    -- ** DatabaseInput
    DatabaseInput (DatabaseInput'),
    newDatabaseInput,

    -- ** DateColumnStatisticsData
    DateColumnStatisticsData (DateColumnStatisticsData'),
    newDateColumnStatisticsData,

    -- ** DecimalColumnStatisticsData
    DecimalColumnStatisticsData (DecimalColumnStatisticsData'),
    newDecimalColumnStatisticsData,

    -- ** DecimalNumber
    DecimalNumber (DecimalNumber'),
    newDecimalNumber,

    -- ** DevEndpoint
    DevEndpoint (DevEndpoint'),
    newDevEndpoint,

    -- ** DevEndpointCustomLibraries
    DevEndpointCustomLibraries (DevEndpointCustomLibraries'),
    newDevEndpointCustomLibraries,

    -- ** DoubleColumnStatisticsData
    DoubleColumnStatisticsData (DoubleColumnStatisticsData'),
    newDoubleColumnStatisticsData,

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

    -- ** GlueTable
    GlueTable (GlueTable'),
    newGlueTable,

    -- ** GrokClassifier
    GrokClassifier (GrokClassifier'),
    newGrokClassifier,

    -- ** ImportLabelsTaskRunProperties
    ImportLabelsTaskRunProperties (ImportLabelsTaskRunProperties'),
    newImportLabelsTaskRunProperties,

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

    -- ** JsonClassifier
    JsonClassifier (JsonClassifier'),
    newJsonClassifier,

    -- ** KeySchemaElement
    KeySchemaElement (KeySchemaElement'),
    newKeySchemaElement,

    -- ** LabelingSetGenerationTaskRunProperties
    LabelingSetGenerationTaskRunProperties (LabelingSetGenerationTaskRunProperties'),
    newLabelingSetGenerationTaskRunProperties,

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

    -- ** MappingEntry
    MappingEntry (MappingEntry'),
    newMappingEntry,

    -- ** MetadataInfo
    MetadataInfo (MetadataInfo'),
    newMetadataInfo,

    -- ** MetadataKeyValuePair
    MetadataKeyValuePair (MetadataKeyValuePair'),
    newMetadataKeyValuePair,

    -- ** MongoDBTarget
    MongoDBTarget (MongoDBTarget'),
    newMongoDBTarget,

    -- ** Node
    Node (Node'),
    newNode,

    -- ** NotificationProperty
    NotificationProperty (NotificationProperty'),
    newNotificationProperty,

    -- ** Order
    Order (Order'),
    newOrder,

    -- ** OtherMetadataValueListItem
    OtherMetadataValueListItem (OtherMetadataValueListItem'),
    newOtherMetadataValueListItem,

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

    -- ** RegistryId
    RegistryId (RegistryId'),
    newRegistryId,

    -- ** RegistryListItem
    RegistryListItem (RegistryListItem'),
    newRegistryListItem,

    -- ** ResourceUri
    ResourceUri (ResourceUri'),
    newResourceUri,

    -- ** S3Encryption
    S3Encryption (S3Encryption'),
    newS3Encryption,

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

    -- ** SerDeInfo
    SerDeInfo (SerDeInfo'),
    newSerDeInfo,

    -- ** SkewedInfo
    SkewedInfo (SkewedInfo'),
    newSkewedInfo,

    -- ** SortCriterion
    SortCriterion (SortCriterion'),
    newSortCriterion,

    -- ** StartingEventBatchCondition
    StartingEventBatchCondition (StartingEventBatchCondition'),
    newStartingEventBatchCondition,

    -- ** StorageDescriptor
    StorageDescriptor (StorageDescriptor'),
    newStorageDescriptor,

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

import Network.AWS.Glue.BatchCreatePartition
import Network.AWS.Glue.BatchDeleteConnection
import Network.AWS.Glue.BatchDeletePartition
import Network.AWS.Glue.BatchDeleteTable
import Network.AWS.Glue.BatchDeleteTableVersion
import Network.AWS.Glue.BatchGetBlueprints
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
import Network.AWS.Glue.CreateBlueprint
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
import Network.AWS.Glue.DeleteBlueprint
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
import Network.AWS.Glue.GetBlueprint
import Network.AWS.Glue.GetBlueprintRun
import Network.AWS.Glue.GetBlueprintRuns
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
import Network.AWS.Glue.Lens
import Network.AWS.Glue.ListBlueprints
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
import Network.AWS.Glue.StartBlueprintRun
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
import Network.AWS.Glue.Types
import Network.AWS.Glue.UntagResource
import Network.AWS.Glue.UpdateBlueprint
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
import Network.AWS.Glue.Waiters

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
