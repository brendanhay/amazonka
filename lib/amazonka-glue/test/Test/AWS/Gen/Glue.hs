{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Glue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Glue where

import Data.Proxy
import Network.AWS.Glue
import Test.AWS.Fixture
import Test.AWS.Glue.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStartImportLabelsTaskRun $
--             startImportLabelsTaskRun
--
--         , requestUpdateMLTransform $
--             updateMLTransform
--
--         , requestUpdateRegistry $
--             updateRegistry
--
--         , requestDeleteRegistry $
--             deleteRegistry
--
--         , requestDeleteMLTransform $
--             deleteMLTransform
--
--         , requestStartCrawler $
--             startCrawler
--
--         , requestGetCatalogImportStatus $
--             getCatalogImportStatus
--
--         , requestListMLTransforms $
--             listMLTransforms
--
--         , requestGetPartition $
--             getPartition
--
--         , requestQuerySchemaVersionMetadata $
--             querySchemaVersionMetadata
--
--         , requestCreateTrigger $
--             createTrigger
--
--         , requestCheckSchemaVersionValidity $
--             checkSchemaVersionValidity
--
--         , requestDeleteTable $
--             deleteTable
--
--         , requestUpdateTable $
--             updateTable
--
--         , requestGetWorkflowRuns $
--             getWorkflowRuns
--
--         , requestCreateWorkflow $
--             createWorkflow
--
--         , requestUpdateColumnStatisticsForTable $
--             updateColumnStatisticsForTable
--
--         , requestDeleteColumnStatisticsForTable $
--             deleteColumnStatisticsForTable
--
--         , requestDeleteConnection $
--             deleteConnection
--
--         , requestUpdateConnection $
--             updateConnection
--
--         , requestGetUserDefinedFunctions $
--             getUserDefinedFunctions
--
--         , requestGetTags $
--             getTags
--
--         , requestGetDataCatalogEncryptionSettings $
--             getDataCatalogEncryptionSettings
--
--         , requestBatchCreatePartition $
--             batchCreatePartition
--
--         , requestGetMapping $
--             getMapping
--
--         , requestDeleteWorkflow $
--             deleteWorkflow
--
--         , requestUpdateWorkflow $
--             updateWorkflow
--
--         , requestGetTableVersion $
--             getTableVersion
--
--         , requestCreateSecurityConfiguration $
--             createSecurityConfiguration
--
--         , requestStartWorkflowRun $
--             startWorkflowRun
--
--         , requestGetJobs $
--             getJobs
--
--         , requestBatchGetWorkflows $
--             batchGetWorkflows
--
--         , requestGetClassifiers $
--             getClassifiers
--
--         , requestGetResourcePolicies $
--             getResourcePolicies
--
--         , requestCreateConnection $
--             createConnection
--
--         , requestListSchemaVersions $
--             listSchemaVersions
--
--         , requestGetWorkflowRunProperties $
--             getWorkflowRunProperties
--
--         , requestBatchGetDevEndpoints $
--             batchGetDevEndpoints
--
--         , requestDeletePartitionIndex $
--             deletePartitionIndex
--
--         , requestDeleteTableVersion $
--             deleteTableVersion
--
--         , requestDeleteDevEndpoint $
--             deleteDevEndpoint
--
--         , requestUpdateDevEndpoint $
--             updateDevEndpoint
--
--         , requestGetWorkflow $
--             getWorkflow
--
--         , requestBatchGetCrawlers $
--             batchGetCrawlers
--
--         , requestGetJobBookmark $
--             getJobBookmark
--
--         , requestDeleteCrawler $
--             deleteCrawler
--
--         , requestUpdateCrawler $
--             updateCrawler
--
--         , requestStartExportLabelsTaskRun $
--             startExportLabelsTaskRun
--
--         , requestGetSecurityConfiguration $
--             getSecurityConfiguration
--
--         , requestCreatePartitionIndex $
--             createPartitionIndex
--
--         , requestRemoveSchemaVersionMetadata $
--             removeSchemaVersionMetadata
--
--         , requestListSchemas $
--             listSchemas
--
--         , requestGetConnection $
--             getConnection
--
--         , requestGetColumnStatisticsForTable $
--             getColumnStatisticsForTable
--
--         , requestBatchGetPartition $
--             batchGetPartition
--
--         , requestStopTrigger $
--             stopTrigger
--
--         , requestUpdateCrawlerSchedule $
--             updateCrawlerSchedule
--
--         , requestStartMLEvaluationTaskRun $
--             startMLEvaluationTaskRun
--
--         , requestDeleteUserDefinedFunction $
--             deleteUserDefinedFunction
--
--         , requestUpdateUserDefinedFunction $
--             updateUserDefinedFunction
--
--         , requestGetRegistry $
--             getRegistry
--
--         , requestBatchDeleteTable $
--             batchDeleteTable
--
--         , requestCancelMLTaskRun $
--             cancelMLTaskRun
--
--         , requestGetTables $
--             getTables
--
--         , requestResumeWorkflowRun $
--             resumeWorkflowRun
--
--         , requestCreateClassifier $
--             createClassifier
--
--         , requestBatchDeleteConnection $
--             batchDeleteConnection
--
--         , requestCreateJob $
--             createJob
--
--         , requestGetJobRuns $
--             getJobRuns
--
--         , requestCreateUserDefinedFunction $
--             createUserDefinedFunction
--
--         , requestResetJobBookmark $
--             resetJobBookmark
--
--         , requestListJobs $
--             listJobs
--
--         , requestDeleteJob $
--             deleteJob
--
--         , requestUpdateJob $
--             updateJob
--
--         , requestCreateRegistry $
--             createRegistry
--
--         , requestGetCrawlers $
--             getCrawlers
--
--         , requestListTriggers $
--             listTriggers
--
--         , requestGetClassifier $
--             getClassifier
--
--         , requestGetJob $
--             getJob
--
--         , requestListRegistries $
--             listRegistries
--
--         , requestBatchDeleteTableVersion $
--             batchDeleteTableVersion
--
--         , requestGetDevEndpoints $
--             getDevEndpoints
--
--         , requestStartCrawlerSchedule $
--             startCrawlerSchedule
--
--         , requestGetPartitionIndexes $
--             getPartitionIndexes
--
--         , requestGetUserDefinedFunction $
--             getUserDefinedFunction
--
--         , requestGetResourcePolicy $
--             getResourcePolicy
--
--         , requestGetWorkflowRun $
--             getWorkflowRun
--
--         , requestDeleteDatabase $
--             deleteDatabase
--
--         , requestUpdateDatabase $
--             updateDatabase
--
--         , requestGetColumnStatisticsForPartition $
--             getColumnStatisticsForPartition
--
--         , requestStopCrawler $
--             stopCrawler
--
--         , requestDeleteSecurityConfiguration $
--             deleteSecurityConfiguration
--
--         , requestGetPartitions $
--             getPartitions
--
--         , requestPutSchemaVersionMetadata $
--             putSchemaVersionMetadata
--
--         , requestGetSchema $
--             getSchema
--
--         , requestBatchDeletePartition $
--             batchDeletePartition
--
--         , requestStartMLLabelingSetGenerationTaskRun $
--             startMLLabelingSetGenerationTaskRun
--
--         , requestBatchUpdatePartition $
--             batchUpdatePartition
--
--         , requestRegisterSchemaVersion $
--             registerSchemaVersion
--
--         , requestStopWorkflowRun $
--             stopWorkflowRun
--
--         , requestGetCrawler $
--             getCrawler
--
--         , requestListWorkflows $
--             listWorkflows
--
--         , requestBatchStopJobRun $
--             batchStopJobRun
--
--         , requestGetDevEndpoint $
--             getDevEndpoint
--
--         , requestPutWorkflowRunProperties $
--             putWorkflowRunProperties
--
--         , requestCreateTable $
--             createTable
--
--         , requestListCrawlers $
--             listCrawlers
--
--         , requestGetCrawlerMetrics $
--             getCrawlerMetrics
--
--         , requestGetSchemaVersion $
--             getSchemaVersion
--
--         , requestGetPlan $
--             getPlan
--
--         , requestGetTriggers $
--             getTriggers
--
--         , requestCreateSchema $
--             createSchema
--
--         , requestListDevEndpoints $
--             listDevEndpoints
--
--         , requestStartTrigger $
--             startTrigger
--
--         , requestGetDataflowGraph $
--             getDataflowGraph
--
--         , requestGetDatabases $
--             getDatabases
--
--         , requestGetTable $
--             getTable
--
--         , requestCreateCrawler $
--             createCrawler
--
--         , requestGetJobRun $
--             getJobRun
--
--         , requestCreateDevEndpoint $
--             createDevEndpoint
--
--         , requestGetMLTaskRuns $
--             getMLTaskRuns
--
--         , requestTagResource $
--             tagResource
--
--         , requestPutDataCatalogEncryptionSettings $
--             putDataCatalogEncryptionSettings
--
--         , requestGetMLTransforms $
--             getMLTransforms
--
--         , requestUpdateSchema $
--             updateSchema
--
--         , requestDeleteSchema $
--             deleteSchema
--
--         , requestGetDatabase $
--             getDatabase
--
--         , requestDeleteColumnStatisticsForPartition $
--             deleteColumnStatisticsForPartition
--
--         , requestUpdateColumnStatisticsForPartition $
--             updateColumnStatisticsForPartition
--
--         , requestGetMLTaskRun $
--             getMLTaskRun
--
--         , requestDeletePartition $
--             deletePartition
--
--         , requestUpdatePartition $
--             updatePartition
--
--         , requestGetMLTransform $
--             getMLTransform
--
--         , requestCreateScript $
--             createScript
--
--         , requestPutResourcePolicy $
--             putResourcePolicy
--
--         , requestGetSecurityConfigurations $
--             getSecurityConfigurations
--
--         , requestDeleteResourcePolicy $
--             deleteResourcePolicy
--
--         , requestGetConnections $
--             getConnections
--
--         , requestUntagResource $
--             untagResource
--
--         , requestGetSchemaVersionsDiff $
--             getSchemaVersionsDiff
--
--         , requestSearchTables $
--             searchTables
--
--         , requestGetTrigger $
--             getTrigger
--
--         , requestBatchGetJobs $
--             batchGetJobs
--
--         , requestImportCatalogToGlue $
--             importCatalogToGlue
--
--         , requestDeleteClassifier $
--             deleteClassifier
--
--         , requestUpdateClassifier $
--             updateClassifier
--
--         , requestStartJobRun $
--             startJobRun
--
--         , requestCreatePartition $
--             createPartition
--
--         , requestBatchGetTriggers $
--             batchGetTriggers
--
--         , requestStopCrawlerSchedule $
--             stopCrawlerSchedule
--
--         , requestGetSchemaByDefinition $
--             getSchemaByDefinition
--
--         , requestCreateDatabase $
--             createDatabase
--
--         , requestGetTableVersions $
--             getTableVersions
--
--         , requestCreateMLTransform $
--             createMLTransform
--
--         , requestDeleteSchemaVersions $
--             deleteSchemaVersions
--
--         , requestDeleteTrigger $
--             deleteTrigger
--
--         , requestUpdateTrigger $
--             updateTrigger
--
--           ]

--     , testGroup "response"
--         [ responseStartImportLabelsTaskRun $
--             startImportLabelsTaskRunResponse
--
--         , responseUpdateMLTransform $
--             updateMLTransformResponse
--
--         , responseUpdateRegistry $
--             updateRegistryResponse
--
--         , responseDeleteRegistry $
--             deleteRegistryResponse
--
--         , responseDeleteMLTransform $
--             deleteMLTransformResponse
--
--         , responseStartCrawler $
--             startCrawlerResponse
--
--         , responseGetCatalogImportStatus $
--             getCatalogImportStatusResponse
--
--         , responseListMLTransforms $
--             listMLTransformsResponse
--
--         , responseGetPartition $
--             getPartitionResponse
--
--         , responseQuerySchemaVersionMetadata $
--             querySchemaVersionMetadataResponse
--
--         , responseCreateTrigger $
--             createTriggerResponse
--
--         , responseCheckSchemaVersionValidity $
--             checkSchemaVersionValidityResponse
--
--         , responseDeleteTable $
--             deleteTableResponse
--
--         , responseUpdateTable $
--             updateTableResponse
--
--         , responseGetWorkflowRuns $
--             getWorkflowRunsResponse
--
--         , responseCreateWorkflow $
--             createWorkflowResponse
--
--         , responseUpdateColumnStatisticsForTable $
--             updateColumnStatisticsForTableResponse
--
--         , responseDeleteColumnStatisticsForTable $
--             deleteColumnStatisticsForTableResponse
--
--         , responseDeleteConnection $
--             deleteConnectionResponse
--
--         , responseUpdateConnection $
--             updateConnectionResponse
--
--         , responseGetUserDefinedFunctions $
--             getUserDefinedFunctionsResponse
--
--         , responseGetTags $
--             getTagsResponse
--
--         , responseGetDataCatalogEncryptionSettings $
--             getDataCatalogEncryptionSettingsResponse
--
--         , responseBatchCreatePartition $
--             batchCreatePartitionResponse
--
--         , responseGetMapping $
--             getMappingResponse
--
--         , responseDeleteWorkflow $
--             deleteWorkflowResponse
--
--         , responseUpdateWorkflow $
--             updateWorkflowResponse
--
--         , responseGetTableVersion $
--             getTableVersionResponse
--
--         , responseCreateSecurityConfiguration $
--             createSecurityConfigurationResponse
--
--         , responseStartWorkflowRun $
--             startWorkflowRunResponse
--
--         , responseGetJobs $
--             getJobsResponse
--
--         , responseBatchGetWorkflows $
--             batchGetWorkflowsResponse
--
--         , responseGetClassifiers $
--             getClassifiersResponse
--
--         , responseGetResourcePolicies $
--             getResourcePoliciesResponse
--
--         , responseCreateConnection $
--             createConnectionResponse
--
--         , responseListSchemaVersions $
--             listSchemaVersionsResponse
--
--         , responseGetWorkflowRunProperties $
--             getWorkflowRunPropertiesResponse
--
--         , responseBatchGetDevEndpoints $
--             batchGetDevEndpointsResponse
--
--         , responseDeletePartitionIndex $
--             deletePartitionIndexResponse
--
--         , responseDeleteTableVersion $
--             deleteTableVersionResponse
--
--         , responseDeleteDevEndpoint $
--             deleteDevEndpointResponse
--
--         , responseUpdateDevEndpoint $
--             updateDevEndpointResponse
--
--         , responseGetWorkflow $
--             getWorkflowResponse
--
--         , responseBatchGetCrawlers $
--             batchGetCrawlersResponse
--
--         , responseGetJobBookmark $
--             getJobBookmarkResponse
--
--         , responseDeleteCrawler $
--             deleteCrawlerResponse
--
--         , responseUpdateCrawler $
--             updateCrawlerResponse
--
--         , responseStartExportLabelsTaskRun $
--             startExportLabelsTaskRunResponse
--
--         , responseGetSecurityConfiguration $
--             getSecurityConfigurationResponse
--
--         , responseCreatePartitionIndex $
--             createPartitionIndexResponse
--
--         , responseRemoveSchemaVersionMetadata $
--             removeSchemaVersionMetadataResponse
--
--         , responseListSchemas $
--             listSchemasResponse
--
--         , responseGetConnection $
--             getConnectionResponse
--
--         , responseGetColumnStatisticsForTable $
--             getColumnStatisticsForTableResponse
--
--         , responseBatchGetPartition $
--             batchGetPartitionResponse
--
--         , responseStopTrigger $
--             stopTriggerResponse
--
--         , responseUpdateCrawlerSchedule $
--             updateCrawlerScheduleResponse
--
--         , responseStartMLEvaluationTaskRun $
--             startMLEvaluationTaskRunResponse
--
--         , responseDeleteUserDefinedFunction $
--             deleteUserDefinedFunctionResponse
--
--         , responseUpdateUserDefinedFunction $
--             updateUserDefinedFunctionResponse
--
--         , responseGetRegistry $
--             getRegistryResponse
--
--         , responseBatchDeleteTable $
--             batchDeleteTableResponse
--
--         , responseCancelMLTaskRun $
--             cancelMLTaskRunResponse
--
--         , responseGetTables $
--             getTablesResponse
--
--         , responseResumeWorkflowRun $
--             resumeWorkflowRunResponse
--
--         , responseCreateClassifier $
--             createClassifierResponse
--
--         , responseBatchDeleteConnection $
--             batchDeleteConnectionResponse
--
--         , responseCreateJob $
--             createJobResponse
--
--         , responseGetJobRuns $
--             getJobRunsResponse
--
--         , responseCreateUserDefinedFunction $
--             createUserDefinedFunctionResponse
--
--         , responseResetJobBookmark $
--             resetJobBookmarkResponse
--
--         , responseListJobs $
--             listJobsResponse
--
--         , responseDeleteJob $
--             deleteJobResponse
--
--         , responseUpdateJob $
--             updateJobResponse
--
--         , responseCreateRegistry $
--             createRegistryResponse
--
--         , responseGetCrawlers $
--             getCrawlersResponse
--
--         , responseListTriggers $
--             listTriggersResponse
--
--         , responseGetClassifier $
--             getClassifierResponse
--
--         , responseGetJob $
--             getJobResponse
--
--         , responseListRegistries $
--             listRegistriesResponse
--
--         , responseBatchDeleteTableVersion $
--             batchDeleteTableVersionResponse
--
--         , responseGetDevEndpoints $
--             getDevEndpointsResponse
--
--         , responseStartCrawlerSchedule $
--             startCrawlerScheduleResponse
--
--         , responseGetPartitionIndexes $
--             getPartitionIndexesResponse
--
--         , responseGetUserDefinedFunction $
--             getUserDefinedFunctionResponse
--
--         , responseGetResourcePolicy $
--             getResourcePolicyResponse
--
--         , responseGetWorkflowRun $
--             getWorkflowRunResponse
--
--         , responseDeleteDatabase $
--             deleteDatabaseResponse
--
--         , responseUpdateDatabase $
--             updateDatabaseResponse
--
--         , responseGetColumnStatisticsForPartition $
--             getColumnStatisticsForPartitionResponse
--
--         , responseStopCrawler $
--             stopCrawlerResponse
--
--         , responseDeleteSecurityConfiguration $
--             deleteSecurityConfigurationResponse
--
--         , responseGetPartitions $
--             getPartitionsResponse
--
--         , responsePutSchemaVersionMetadata $
--             putSchemaVersionMetadataResponse
--
--         , responseGetSchema $
--             getSchemaResponse
--
--         , responseBatchDeletePartition $
--             batchDeletePartitionResponse
--
--         , responseStartMLLabelingSetGenerationTaskRun $
--             startMLLabelingSetGenerationTaskRunResponse
--
--         , responseBatchUpdatePartition $
--             batchUpdatePartitionResponse
--
--         , responseRegisterSchemaVersion $
--             registerSchemaVersionResponse
--
--         , responseStopWorkflowRun $
--             stopWorkflowRunResponse
--
--         , responseGetCrawler $
--             getCrawlerResponse
--
--         , responseListWorkflows $
--             listWorkflowsResponse
--
--         , responseBatchStopJobRun $
--             batchStopJobRunResponse
--
--         , responseGetDevEndpoint $
--             getDevEndpointResponse
--
--         , responsePutWorkflowRunProperties $
--             putWorkflowRunPropertiesResponse
--
--         , responseCreateTable $
--             createTableResponse
--
--         , responseListCrawlers $
--             listCrawlersResponse
--
--         , responseGetCrawlerMetrics $
--             getCrawlerMetricsResponse
--
--         , responseGetSchemaVersion $
--             getSchemaVersionResponse
--
--         , responseGetPlan $
--             getPlanResponse
--
--         , responseGetTriggers $
--             getTriggersResponse
--
--         , responseCreateSchema $
--             createSchemaResponse
--
--         , responseListDevEndpoints $
--             listDevEndpointsResponse
--
--         , responseStartTrigger $
--             startTriggerResponse
--
--         , responseGetDataflowGraph $
--             getDataflowGraphResponse
--
--         , responseGetDatabases $
--             getDatabasesResponse
--
--         , responseGetTable $
--             getTableResponse
--
--         , responseCreateCrawler $
--             createCrawlerResponse
--
--         , responseGetJobRun $
--             getJobRunResponse
--
--         , responseCreateDevEndpoint $
--             createDevEndpointResponse
--
--         , responseGetMLTaskRuns $
--             getMLTaskRunsResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responsePutDataCatalogEncryptionSettings $
--             putDataCatalogEncryptionSettingsResponse
--
--         , responseGetMLTransforms $
--             getMLTransformsResponse
--
--         , responseUpdateSchema $
--             updateSchemaResponse
--
--         , responseDeleteSchema $
--             deleteSchemaResponse
--
--         , responseGetDatabase $
--             getDatabaseResponse
--
--         , responseDeleteColumnStatisticsForPartition $
--             deleteColumnStatisticsForPartitionResponse
--
--         , responseUpdateColumnStatisticsForPartition $
--             updateColumnStatisticsForPartitionResponse
--
--         , responseGetMLTaskRun $
--             getMLTaskRunResponse
--
--         , responseDeletePartition $
--             deletePartitionResponse
--
--         , responseUpdatePartition $
--             updatePartitionResponse
--
--         , responseGetMLTransform $
--             getMLTransformResponse
--
--         , responseCreateScript $
--             createScriptResponse
--
--         , responsePutResourcePolicy $
--             putResourcePolicyResponse
--
--         , responseGetSecurityConfigurations $
--             getSecurityConfigurationsResponse
--
--         , responseDeleteResourcePolicy $
--             deleteResourcePolicyResponse
--
--         , responseGetConnections $
--             getConnectionsResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseGetSchemaVersionsDiff $
--             getSchemaVersionsDiffResponse
--
--         , responseSearchTables $
--             searchTablesResponse
--
--         , responseGetTrigger $
--             getTriggerResponse
--
--         , responseBatchGetJobs $
--             batchGetJobsResponse
--
--         , responseImportCatalogToGlue $
--             importCatalogToGlueResponse
--
--         , responseDeleteClassifier $
--             deleteClassifierResponse
--
--         , responseUpdateClassifier $
--             updateClassifierResponse
--
--         , responseStartJobRun $
--             startJobRunResponse
--
--         , responseCreatePartition $
--             createPartitionResponse
--
--         , responseBatchGetTriggers $
--             batchGetTriggersResponse
--
--         , responseStopCrawlerSchedule $
--             stopCrawlerScheduleResponse
--
--         , responseGetSchemaByDefinition $
--             getSchemaByDefinitionResponse
--
--         , responseCreateDatabase $
--             createDatabaseResponse
--
--         , responseGetTableVersions $
--             getTableVersionsResponse
--
--         , responseCreateMLTransform $
--             createMLTransformResponse
--
--         , responseDeleteSchemaVersions $
--             deleteSchemaVersionsResponse
--
--         , responseDeleteTrigger $
--             deleteTriggerResponse
--
--         , responseUpdateTrigger $
--             updateTriggerResponse
--
--           ]
--     ]

-- Requests

requestStartImportLabelsTaskRun :: StartImportLabelsTaskRun -> TestTree
requestStartImportLabelsTaskRun =
  req
    "StartImportLabelsTaskRun"
    "fixture/StartImportLabelsTaskRun.yaml"

requestUpdateMLTransform :: UpdateMLTransform -> TestTree
requestUpdateMLTransform =
  req
    "UpdateMLTransform"
    "fixture/UpdateMLTransform.yaml"

requestUpdateRegistry :: UpdateRegistry -> TestTree
requestUpdateRegistry =
  req
    "UpdateRegistry"
    "fixture/UpdateRegistry.yaml"

requestDeleteRegistry :: DeleteRegistry -> TestTree
requestDeleteRegistry =
  req
    "DeleteRegistry"
    "fixture/DeleteRegistry.yaml"

requestDeleteMLTransform :: DeleteMLTransform -> TestTree
requestDeleteMLTransform =
  req
    "DeleteMLTransform"
    "fixture/DeleteMLTransform.yaml"

requestStartCrawler :: StartCrawler -> TestTree
requestStartCrawler =
  req
    "StartCrawler"
    "fixture/StartCrawler.yaml"

requestGetCatalogImportStatus :: GetCatalogImportStatus -> TestTree
requestGetCatalogImportStatus =
  req
    "GetCatalogImportStatus"
    "fixture/GetCatalogImportStatus.yaml"

requestListMLTransforms :: ListMLTransforms -> TestTree
requestListMLTransforms =
  req
    "ListMLTransforms"
    "fixture/ListMLTransforms.yaml"

requestGetPartition :: GetPartition -> TestTree
requestGetPartition =
  req
    "GetPartition"
    "fixture/GetPartition.yaml"

requestQuerySchemaVersionMetadata :: QuerySchemaVersionMetadata -> TestTree
requestQuerySchemaVersionMetadata =
  req
    "QuerySchemaVersionMetadata"
    "fixture/QuerySchemaVersionMetadata.yaml"

requestCreateTrigger :: CreateTrigger -> TestTree
requestCreateTrigger =
  req
    "CreateTrigger"
    "fixture/CreateTrigger.yaml"

requestCheckSchemaVersionValidity :: CheckSchemaVersionValidity -> TestTree
requestCheckSchemaVersionValidity =
  req
    "CheckSchemaVersionValidity"
    "fixture/CheckSchemaVersionValidity.yaml"

requestDeleteTable :: DeleteTable -> TestTree
requestDeleteTable =
  req
    "DeleteTable"
    "fixture/DeleteTable.yaml"

requestUpdateTable :: UpdateTable -> TestTree
requestUpdateTable =
  req
    "UpdateTable"
    "fixture/UpdateTable.yaml"

requestGetWorkflowRuns :: GetWorkflowRuns -> TestTree
requestGetWorkflowRuns =
  req
    "GetWorkflowRuns"
    "fixture/GetWorkflowRuns.yaml"

requestCreateWorkflow :: CreateWorkflow -> TestTree
requestCreateWorkflow =
  req
    "CreateWorkflow"
    "fixture/CreateWorkflow.yaml"

requestUpdateColumnStatisticsForTable :: UpdateColumnStatisticsForTable -> TestTree
requestUpdateColumnStatisticsForTable =
  req
    "UpdateColumnStatisticsForTable"
    "fixture/UpdateColumnStatisticsForTable.yaml"

requestDeleteColumnStatisticsForTable :: DeleteColumnStatisticsForTable -> TestTree
requestDeleteColumnStatisticsForTable =
  req
    "DeleteColumnStatisticsForTable"
    "fixture/DeleteColumnStatisticsForTable.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestUpdateConnection :: UpdateConnection -> TestTree
requestUpdateConnection =
  req
    "UpdateConnection"
    "fixture/UpdateConnection.yaml"

requestGetUserDefinedFunctions :: GetUserDefinedFunctions -> TestTree
requestGetUserDefinedFunctions =
  req
    "GetUserDefinedFunctions"
    "fixture/GetUserDefinedFunctions.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags =
  req
    "GetTags"
    "fixture/GetTags.yaml"

requestGetDataCatalogEncryptionSettings :: GetDataCatalogEncryptionSettings -> TestTree
requestGetDataCatalogEncryptionSettings =
  req
    "GetDataCatalogEncryptionSettings"
    "fixture/GetDataCatalogEncryptionSettings.yaml"

requestBatchCreatePartition :: BatchCreatePartition -> TestTree
requestBatchCreatePartition =
  req
    "BatchCreatePartition"
    "fixture/BatchCreatePartition.yaml"

requestGetMapping :: GetMapping -> TestTree
requestGetMapping =
  req
    "GetMapping"
    "fixture/GetMapping.yaml"

requestDeleteWorkflow :: DeleteWorkflow -> TestTree
requestDeleteWorkflow =
  req
    "DeleteWorkflow"
    "fixture/DeleteWorkflow.yaml"

requestUpdateWorkflow :: UpdateWorkflow -> TestTree
requestUpdateWorkflow =
  req
    "UpdateWorkflow"
    "fixture/UpdateWorkflow.yaml"

requestGetTableVersion :: GetTableVersion -> TestTree
requestGetTableVersion =
  req
    "GetTableVersion"
    "fixture/GetTableVersion.yaml"

requestCreateSecurityConfiguration :: CreateSecurityConfiguration -> TestTree
requestCreateSecurityConfiguration =
  req
    "CreateSecurityConfiguration"
    "fixture/CreateSecurityConfiguration.yaml"

requestStartWorkflowRun :: StartWorkflowRun -> TestTree
requestStartWorkflowRun =
  req
    "StartWorkflowRun"
    "fixture/StartWorkflowRun.yaml"

requestGetJobs :: GetJobs -> TestTree
requestGetJobs =
  req
    "GetJobs"
    "fixture/GetJobs.yaml"

requestBatchGetWorkflows :: BatchGetWorkflows -> TestTree
requestBatchGetWorkflows =
  req
    "BatchGetWorkflows"
    "fixture/BatchGetWorkflows.yaml"

requestGetClassifiers :: GetClassifiers -> TestTree
requestGetClassifiers =
  req
    "GetClassifiers"
    "fixture/GetClassifiers.yaml"

requestGetResourcePolicies :: GetResourcePolicies -> TestTree
requestGetResourcePolicies =
  req
    "GetResourcePolicies"
    "fixture/GetResourcePolicies.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestListSchemaVersions :: ListSchemaVersions -> TestTree
requestListSchemaVersions =
  req
    "ListSchemaVersions"
    "fixture/ListSchemaVersions.yaml"

requestGetWorkflowRunProperties :: GetWorkflowRunProperties -> TestTree
requestGetWorkflowRunProperties =
  req
    "GetWorkflowRunProperties"
    "fixture/GetWorkflowRunProperties.yaml"

requestBatchGetDevEndpoints :: BatchGetDevEndpoints -> TestTree
requestBatchGetDevEndpoints =
  req
    "BatchGetDevEndpoints"
    "fixture/BatchGetDevEndpoints.yaml"

requestDeletePartitionIndex :: DeletePartitionIndex -> TestTree
requestDeletePartitionIndex =
  req
    "DeletePartitionIndex"
    "fixture/DeletePartitionIndex.yaml"

requestDeleteTableVersion :: DeleteTableVersion -> TestTree
requestDeleteTableVersion =
  req
    "DeleteTableVersion"
    "fixture/DeleteTableVersion.yaml"

requestDeleteDevEndpoint :: DeleteDevEndpoint -> TestTree
requestDeleteDevEndpoint =
  req
    "DeleteDevEndpoint"
    "fixture/DeleteDevEndpoint.yaml"

requestUpdateDevEndpoint :: UpdateDevEndpoint -> TestTree
requestUpdateDevEndpoint =
  req
    "UpdateDevEndpoint"
    "fixture/UpdateDevEndpoint.yaml"

requestGetWorkflow :: GetWorkflow -> TestTree
requestGetWorkflow =
  req
    "GetWorkflow"
    "fixture/GetWorkflow.yaml"

requestBatchGetCrawlers :: BatchGetCrawlers -> TestTree
requestBatchGetCrawlers =
  req
    "BatchGetCrawlers"
    "fixture/BatchGetCrawlers.yaml"

requestGetJobBookmark :: GetJobBookmark -> TestTree
requestGetJobBookmark =
  req
    "GetJobBookmark"
    "fixture/GetJobBookmark.yaml"

requestDeleteCrawler :: DeleteCrawler -> TestTree
requestDeleteCrawler =
  req
    "DeleteCrawler"
    "fixture/DeleteCrawler.yaml"

requestUpdateCrawler :: UpdateCrawler -> TestTree
requestUpdateCrawler =
  req
    "UpdateCrawler"
    "fixture/UpdateCrawler.yaml"

requestStartExportLabelsTaskRun :: StartExportLabelsTaskRun -> TestTree
requestStartExportLabelsTaskRun =
  req
    "StartExportLabelsTaskRun"
    "fixture/StartExportLabelsTaskRun.yaml"

requestGetSecurityConfiguration :: GetSecurityConfiguration -> TestTree
requestGetSecurityConfiguration =
  req
    "GetSecurityConfiguration"
    "fixture/GetSecurityConfiguration.yaml"

requestCreatePartitionIndex :: CreatePartitionIndex -> TestTree
requestCreatePartitionIndex =
  req
    "CreatePartitionIndex"
    "fixture/CreatePartitionIndex.yaml"

requestRemoveSchemaVersionMetadata :: RemoveSchemaVersionMetadata -> TestTree
requestRemoveSchemaVersionMetadata =
  req
    "RemoveSchemaVersionMetadata"
    "fixture/RemoveSchemaVersionMetadata.yaml"

requestListSchemas :: ListSchemas -> TestTree
requestListSchemas =
  req
    "ListSchemas"
    "fixture/ListSchemas.yaml"

requestGetConnection :: GetConnection -> TestTree
requestGetConnection =
  req
    "GetConnection"
    "fixture/GetConnection.yaml"

requestGetColumnStatisticsForTable :: GetColumnStatisticsForTable -> TestTree
requestGetColumnStatisticsForTable =
  req
    "GetColumnStatisticsForTable"
    "fixture/GetColumnStatisticsForTable.yaml"

requestBatchGetPartition :: BatchGetPartition -> TestTree
requestBatchGetPartition =
  req
    "BatchGetPartition"
    "fixture/BatchGetPartition.yaml"

requestStopTrigger :: StopTrigger -> TestTree
requestStopTrigger =
  req
    "StopTrigger"
    "fixture/StopTrigger.yaml"

requestUpdateCrawlerSchedule :: UpdateCrawlerSchedule -> TestTree
requestUpdateCrawlerSchedule =
  req
    "UpdateCrawlerSchedule"
    "fixture/UpdateCrawlerSchedule.yaml"

requestStartMLEvaluationTaskRun :: StartMLEvaluationTaskRun -> TestTree
requestStartMLEvaluationTaskRun =
  req
    "StartMLEvaluationTaskRun"
    "fixture/StartMLEvaluationTaskRun.yaml"

requestDeleteUserDefinedFunction :: DeleteUserDefinedFunction -> TestTree
requestDeleteUserDefinedFunction =
  req
    "DeleteUserDefinedFunction"
    "fixture/DeleteUserDefinedFunction.yaml"

requestUpdateUserDefinedFunction :: UpdateUserDefinedFunction -> TestTree
requestUpdateUserDefinedFunction =
  req
    "UpdateUserDefinedFunction"
    "fixture/UpdateUserDefinedFunction.yaml"

requestGetRegistry :: GetRegistry -> TestTree
requestGetRegistry =
  req
    "GetRegistry"
    "fixture/GetRegistry.yaml"

requestBatchDeleteTable :: BatchDeleteTable -> TestTree
requestBatchDeleteTable =
  req
    "BatchDeleteTable"
    "fixture/BatchDeleteTable.yaml"

requestCancelMLTaskRun :: CancelMLTaskRun -> TestTree
requestCancelMLTaskRun =
  req
    "CancelMLTaskRun"
    "fixture/CancelMLTaskRun.yaml"

requestGetTables :: GetTables -> TestTree
requestGetTables =
  req
    "GetTables"
    "fixture/GetTables.yaml"

requestResumeWorkflowRun :: ResumeWorkflowRun -> TestTree
requestResumeWorkflowRun =
  req
    "ResumeWorkflowRun"
    "fixture/ResumeWorkflowRun.yaml"

requestCreateClassifier :: CreateClassifier -> TestTree
requestCreateClassifier =
  req
    "CreateClassifier"
    "fixture/CreateClassifier.yaml"

requestBatchDeleteConnection :: BatchDeleteConnection -> TestTree
requestBatchDeleteConnection =
  req
    "BatchDeleteConnection"
    "fixture/BatchDeleteConnection.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestGetJobRuns :: GetJobRuns -> TestTree
requestGetJobRuns =
  req
    "GetJobRuns"
    "fixture/GetJobRuns.yaml"

requestCreateUserDefinedFunction :: CreateUserDefinedFunction -> TestTree
requestCreateUserDefinedFunction =
  req
    "CreateUserDefinedFunction"
    "fixture/CreateUserDefinedFunction.yaml"

requestResetJobBookmark :: ResetJobBookmark -> TestTree
requestResetJobBookmark =
  req
    "ResetJobBookmark"
    "fixture/ResetJobBookmark.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestUpdateJob :: UpdateJob -> TestTree
requestUpdateJob =
  req
    "UpdateJob"
    "fixture/UpdateJob.yaml"

requestCreateRegistry :: CreateRegistry -> TestTree
requestCreateRegistry =
  req
    "CreateRegistry"
    "fixture/CreateRegistry.yaml"

requestGetCrawlers :: GetCrawlers -> TestTree
requestGetCrawlers =
  req
    "GetCrawlers"
    "fixture/GetCrawlers.yaml"

requestListTriggers :: ListTriggers -> TestTree
requestListTriggers =
  req
    "ListTriggers"
    "fixture/ListTriggers.yaml"

requestGetClassifier :: GetClassifier -> TestTree
requestGetClassifier =
  req
    "GetClassifier"
    "fixture/GetClassifier.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestListRegistries :: ListRegistries -> TestTree
requestListRegistries =
  req
    "ListRegistries"
    "fixture/ListRegistries.yaml"

requestBatchDeleteTableVersion :: BatchDeleteTableVersion -> TestTree
requestBatchDeleteTableVersion =
  req
    "BatchDeleteTableVersion"
    "fixture/BatchDeleteTableVersion.yaml"

requestGetDevEndpoints :: GetDevEndpoints -> TestTree
requestGetDevEndpoints =
  req
    "GetDevEndpoints"
    "fixture/GetDevEndpoints.yaml"

requestStartCrawlerSchedule :: StartCrawlerSchedule -> TestTree
requestStartCrawlerSchedule =
  req
    "StartCrawlerSchedule"
    "fixture/StartCrawlerSchedule.yaml"

requestGetPartitionIndexes :: GetPartitionIndexes -> TestTree
requestGetPartitionIndexes =
  req
    "GetPartitionIndexes"
    "fixture/GetPartitionIndexes.yaml"

requestGetUserDefinedFunction :: GetUserDefinedFunction -> TestTree
requestGetUserDefinedFunction =
  req
    "GetUserDefinedFunction"
    "fixture/GetUserDefinedFunction.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestGetWorkflowRun :: GetWorkflowRun -> TestTree
requestGetWorkflowRun =
  req
    "GetWorkflowRun"
    "fixture/GetWorkflowRun.yaml"

requestDeleteDatabase :: DeleteDatabase -> TestTree
requestDeleteDatabase =
  req
    "DeleteDatabase"
    "fixture/DeleteDatabase.yaml"

requestUpdateDatabase :: UpdateDatabase -> TestTree
requestUpdateDatabase =
  req
    "UpdateDatabase"
    "fixture/UpdateDatabase.yaml"

requestGetColumnStatisticsForPartition :: GetColumnStatisticsForPartition -> TestTree
requestGetColumnStatisticsForPartition =
  req
    "GetColumnStatisticsForPartition"
    "fixture/GetColumnStatisticsForPartition.yaml"

requestStopCrawler :: StopCrawler -> TestTree
requestStopCrawler =
  req
    "StopCrawler"
    "fixture/StopCrawler.yaml"

requestDeleteSecurityConfiguration :: DeleteSecurityConfiguration -> TestTree
requestDeleteSecurityConfiguration =
  req
    "DeleteSecurityConfiguration"
    "fixture/DeleteSecurityConfiguration.yaml"

requestGetPartitions :: GetPartitions -> TestTree
requestGetPartitions =
  req
    "GetPartitions"
    "fixture/GetPartitions.yaml"

requestPutSchemaVersionMetadata :: PutSchemaVersionMetadata -> TestTree
requestPutSchemaVersionMetadata =
  req
    "PutSchemaVersionMetadata"
    "fixture/PutSchemaVersionMetadata.yaml"

requestGetSchema :: GetSchema -> TestTree
requestGetSchema =
  req
    "GetSchema"
    "fixture/GetSchema.yaml"

requestBatchDeletePartition :: BatchDeletePartition -> TestTree
requestBatchDeletePartition =
  req
    "BatchDeletePartition"
    "fixture/BatchDeletePartition.yaml"

requestStartMLLabelingSetGenerationTaskRun :: StartMLLabelingSetGenerationTaskRun -> TestTree
requestStartMLLabelingSetGenerationTaskRun =
  req
    "StartMLLabelingSetGenerationTaskRun"
    "fixture/StartMLLabelingSetGenerationTaskRun.yaml"

requestBatchUpdatePartition :: BatchUpdatePartition -> TestTree
requestBatchUpdatePartition =
  req
    "BatchUpdatePartition"
    "fixture/BatchUpdatePartition.yaml"

requestRegisterSchemaVersion :: RegisterSchemaVersion -> TestTree
requestRegisterSchemaVersion =
  req
    "RegisterSchemaVersion"
    "fixture/RegisterSchemaVersion.yaml"

requestStopWorkflowRun :: StopWorkflowRun -> TestTree
requestStopWorkflowRun =
  req
    "StopWorkflowRun"
    "fixture/StopWorkflowRun.yaml"

requestGetCrawler :: GetCrawler -> TestTree
requestGetCrawler =
  req
    "GetCrawler"
    "fixture/GetCrawler.yaml"

requestListWorkflows :: ListWorkflows -> TestTree
requestListWorkflows =
  req
    "ListWorkflows"
    "fixture/ListWorkflows.yaml"

requestBatchStopJobRun :: BatchStopJobRun -> TestTree
requestBatchStopJobRun =
  req
    "BatchStopJobRun"
    "fixture/BatchStopJobRun.yaml"

requestGetDevEndpoint :: GetDevEndpoint -> TestTree
requestGetDevEndpoint =
  req
    "GetDevEndpoint"
    "fixture/GetDevEndpoint.yaml"

requestPutWorkflowRunProperties :: PutWorkflowRunProperties -> TestTree
requestPutWorkflowRunProperties =
  req
    "PutWorkflowRunProperties"
    "fixture/PutWorkflowRunProperties.yaml"

requestCreateTable :: CreateTable -> TestTree
requestCreateTable =
  req
    "CreateTable"
    "fixture/CreateTable.yaml"

requestListCrawlers :: ListCrawlers -> TestTree
requestListCrawlers =
  req
    "ListCrawlers"
    "fixture/ListCrawlers.yaml"

requestGetCrawlerMetrics :: GetCrawlerMetrics -> TestTree
requestGetCrawlerMetrics =
  req
    "GetCrawlerMetrics"
    "fixture/GetCrawlerMetrics.yaml"

requestGetSchemaVersion :: GetSchemaVersion -> TestTree
requestGetSchemaVersion =
  req
    "GetSchemaVersion"
    "fixture/GetSchemaVersion.yaml"

requestGetPlan :: GetPlan -> TestTree
requestGetPlan =
  req
    "GetPlan"
    "fixture/GetPlan.yaml"

requestGetTriggers :: GetTriggers -> TestTree
requestGetTriggers =
  req
    "GetTriggers"
    "fixture/GetTriggers.yaml"

requestCreateSchema :: CreateSchema -> TestTree
requestCreateSchema =
  req
    "CreateSchema"
    "fixture/CreateSchema.yaml"

requestListDevEndpoints :: ListDevEndpoints -> TestTree
requestListDevEndpoints =
  req
    "ListDevEndpoints"
    "fixture/ListDevEndpoints.yaml"

requestStartTrigger :: StartTrigger -> TestTree
requestStartTrigger =
  req
    "StartTrigger"
    "fixture/StartTrigger.yaml"

requestGetDataflowGraph :: GetDataflowGraph -> TestTree
requestGetDataflowGraph =
  req
    "GetDataflowGraph"
    "fixture/GetDataflowGraph.yaml"

requestGetDatabases :: GetDatabases -> TestTree
requestGetDatabases =
  req
    "GetDatabases"
    "fixture/GetDatabases.yaml"

requestGetTable :: GetTable -> TestTree
requestGetTable =
  req
    "GetTable"
    "fixture/GetTable.yaml"

requestCreateCrawler :: CreateCrawler -> TestTree
requestCreateCrawler =
  req
    "CreateCrawler"
    "fixture/CreateCrawler.yaml"

requestGetJobRun :: GetJobRun -> TestTree
requestGetJobRun =
  req
    "GetJobRun"
    "fixture/GetJobRun.yaml"

requestCreateDevEndpoint :: CreateDevEndpoint -> TestTree
requestCreateDevEndpoint =
  req
    "CreateDevEndpoint"
    "fixture/CreateDevEndpoint.yaml"

requestGetMLTaskRuns :: GetMLTaskRuns -> TestTree
requestGetMLTaskRuns =
  req
    "GetMLTaskRuns"
    "fixture/GetMLTaskRuns.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestPutDataCatalogEncryptionSettings :: PutDataCatalogEncryptionSettings -> TestTree
requestPutDataCatalogEncryptionSettings =
  req
    "PutDataCatalogEncryptionSettings"
    "fixture/PutDataCatalogEncryptionSettings.yaml"

requestGetMLTransforms :: GetMLTransforms -> TestTree
requestGetMLTransforms =
  req
    "GetMLTransforms"
    "fixture/GetMLTransforms.yaml"

requestUpdateSchema :: UpdateSchema -> TestTree
requestUpdateSchema =
  req
    "UpdateSchema"
    "fixture/UpdateSchema.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema =
  req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestGetDatabase :: GetDatabase -> TestTree
requestGetDatabase =
  req
    "GetDatabase"
    "fixture/GetDatabase.yaml"

requestDeleteColumnStatisticsForPartition :: DeleteColumnStatisticsForPartition -> TestTree
requestDeleteColumnStatisticsForPartition =
  req
    "DeleteColumnStatisticsForPartition"
    "fixture/DeleteColumnStatisticsForPartition.yaml"

requestUpdateColumnStatisticsForPartition :: UpdateColumnStatisticsForPartition -> TestTree
requestUpdateColumnStatisticsForPartition =
  req
    "UpdateColumnStatisticsForPartition"
    "fixture/UpdateColumnStatisticsForPartition.yaml"

requestGetMLTaskRun :: GetMLTaskRun -> TestTree
requestGetMLTaskRun =
  req
    "GetMLTaskRun"
    "fixture/GetMLTaskRun.yaml"

requestDeletePartition :: DeletePartition -> TestTree
requestDeletePartition =
  req
    "DeletePartition"
    "fixture/DeletePartition.yaml"

requestUpdatePartition :: UpdatePartition -> TestTree
requestUpdatePartition =
  req
    "UpdatePartition"
    "fixture/UpdatePartition.yaml"

requestGetMLTransform :: GetMLTransform -> TestTree
requestGetMLTransform =
  req
    "GetMLTransform"
    "fixture/GetMLTransform.yaml"

requestCreateScript :: CreateScript -> TestTree
requestCreateScript =
  req
    "CreateScript"
    "fixture/CreateScript.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestGetSecurityConfigurations :: GetSecurityConfigurations -> TestTree
requestGetSecurityConfigurations =
  req
    "GetSecurityConfigurations"
    "fixture/GetSecurityConfigurations.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestGetConnections :: GetConnections -> TestTree
requestGetConnections =
  req
    "GetConnections"
    "fixture/GetConnections.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetSchemaVersionsDiff :: GetSchemaVersionsDiff -> TestTree
requestGetSchemaVersionsDiff =
  req
    "GetSchemaVersionsDiff"
    "fixture/GetSchemaVersionsDiff.yaml"

requestSearchTables :: SearchTables -> TestTree
requestSearchTables =
  req
    "SearchTables"
    "fixture/SearchTables.yaml"

requestGetTrigger :: GetTrigger -> TestTree
requestGetTrigger =
  req
    "GetTrigger"
    "fixture/GetTrigger.yaml"

requestBatchGetJobs :: BatchGetJobs -> TestTree
requestBatchGetJobs =
  req
    "BatchGetJobs"
    "fixture/BatchGetJobs.yaml"

requestImportCatalogToGlue :: ImportCatalogToGlue -> TestTree
requestImportCatalogToGlue =
  req
    "ImportCatalogToGlue"
    "fixture/ImportCatalogToGlue.yaml"

requestDeleteClassifier :: DeleteClassifier -> TestTree
requestDeleteClassifier =
  req
    "DeleteClassifier"
    "fixture/DeleteClassifier.yaml"

requestUpdateClassifier :: UpdateClassifier -> TestTree
requestUpdateClassifier =
  req
    "UpdateClassifier"
    "fixture/UpdateClassifier.yaml"

requestStartJobRun :: StartJobRun -> TestTree
requestStartJobRun =
  req
    "StartJobRun"
    "fixture/StartJobRun.yaml"

requestCreatePartition :: CreatePartition -> TestTree
requestCreatePartition =
  req
    "CreatePartition"
    "fixture/CreatePartition.yaml"

requestBatchGetTriggers :: BatchGetTriggers -> TestTree
requestBatchGetTriggers =
  req
    "BatchGetTriggers"
    "fixture/BatchGetTriggers.yaml"

requestStopCrawlerSchedule :: StopCrawlerSchedule -> TestTree
requestStopCrawlerSchedule =
  req
    "StopCrawlerSchedule"
    "fixture/StopCrawlerSchedule.yaml"

requestGetSchemaByDefinition :: GetSchemaByDefinition -> TestTree
requestGetSchemaByDefinition =
  req
    "GetSchemaByDefinition"
    "fixture/GetSchemaByDefinition.yaml"

requestCreateDatabase :: CreateDatabase -> TestTree
requestCreateDatabase =
  req
    "CreateDatabase"
    "fixture/CreateDatabase.yaml"

requestGetTableVersions :: GetTableVersions -> TestTree
requestGetTableVersions =
  req
    "GetTableVersions"
    "fixture/GetTableVersions.yaml"

requestCreateMLTransform :: CreateMLTransform -> TestTree
requestCreateMLTransform =
  req
    "CreateMLTransform"
    "fixture/CreateMLTransform.yaml"

requestDeleteSchemaVersions :: DeleteSchemaVersions -> TestTree
requestDeleteSchemaVersions =
  req
    "DeleteSchemaVersions"
    "fixture/DeleteSchemaVersions.yaml"

requestDeleteTrigger :: DeleteTrigger -> TestTree
requestDeleteTrigger =
  req
    "DeleteTrigger"
    "fixture/DeleteTrigger.yaml"

requestUpdateTrigger :: UpdateTrigger -> TestTree
requestUpdateTrigger =
  req
    "UpdateTrigger"
    "fixture/UpdateTrigger.yaml"

-- Responses

responseStartImportLabelsTaskRun :: StartImportLabelsTaskRunResponse -> TestTree
responseStartImportLabelsTaskRun =
  res
    "StartImportLabelsTaskRunResponse"
    "fixture/StartImportLabelsTaskRunResponse.proto"
    glue
    (Proxy :: Proxy StartImportLabelsTaskRun)

responseUpdateMLTransform :: UpdateMLTransformResponse -> TestTree
responseUpdateMLTransform =
  res
    "UpdateMLTransformResponse"
    "fixture/UpdateMLTransformResponse.proto"
    glue
    (Proxy :: Proxy UpdateMLTransform)

responseUpdateRegistry :: UpdateRegistryResponse -> TestTree
responseUpdateRegistry =
  res
    "UpdateRegistryResponse"
    "fixture/UpdateRegistryResponse.proto"
    glue
    (Proxy :: Proxy UpdateRegistry)

responseDeleteRegistry :: DeleteRegistryResponse -> TestTree
responseDeleteRegistry =
  res
    "DeleteRegistryResponse"
    "fixture/DeleteRegistryResponse.proto"
    glue
    (Proxy :: Proxy DeleteRegistry)

responseDeleteMLTransform :: DeleteMLTransformResponse -> TestTree
responseDeleteMLTransform =
  res
    "DeleteMLTransformResponse"
    "fixture/DeleteMLTransformResponse.proto"
    glue
    (Proxy :: Proxy DeleteMLTransform)

responseStartCrawler :: StartCrawlerResponse -> TestTree
responseStartCrawler =
  res
    "StartCrawlerResponse"
    "fixture/StartCrawlerResponse.proto"
    glue
    (Proxy :: Proxy StartCrawler)

responseGetCatalogImportStatus :: GetCatalogImportStatusResponse -> TestTree
responseGetCatalogImportStatus =
  res
    "GetCatalogImportStatusResponse"
    "fixture/GetCatalogImportStatusResponse.proto"
    glue
    (Proxy :: Proxy GetCatalogImportStatus)

responseListMLTransforms :: ListMLTransformsResponse -> TestTree
responseListMLTransforms =
  res
    "ListMLTransformsResponse"
    "fixture/ListMLTransformsResponse.proto"
    glue
    (Proxy :: Proxy ListMLTransforms)

responseGetPartition :: GetPartitionResponse -> TestTree
responseGetPartition =
  res
    "GetPartitionResponse"
    "fixture/GetPartitionResponse.proto"
    glue
    (Proxy :: Proxy GetPartition)

responseQuerySchemaVersionMetadata :: QuerySchemaVersionMetadataResponse -> TestTree
responseQuerySchemaVersionMetadata =
  res
    "QuerySchemaVersionMetadataResponse"
    "fixture/QuerySchemaVersionMetadataResponse.proto"
    glue
    (Proxy :: Proxy QuerySchemaVersionMetadata)

responseCreateTrigger :: CreateTriggerResponse -> TestTree
responseCreateTrigger =
  res
    "CreateTriggerResponse"
    "fixture/CreateTriggerResponse.proto"
    glue
    (Proxy :: Proxy CreateTrigger)

responseCheckSchemaVersionValidity :: CheckSchemaVersionValidityResponse -> TestTree
responseCheckSchemaVersionValidity =
  res
    "CheckSchemaVersionValidityResponse"
    "fixture/CheckSchemaVersionValidityResponse.proto"
    glue
    (Proxy :: Proxy CheckSchemaVersionValidity)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable =
  res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    glue
    (Proxy :: Proxy DeleteTable)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable =
  res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    glue
    (Proxy :: Proxy UpdateTable)

responseGetWorkflowRuns :: GetWorkflowRunsResponse -> TestTree
responseGetWorkflowRuns =
  res
    "GetWorkflowRunsResponse"
    "fixture/GetWorkflowRunsResponse.proto"
    glue
    (Proxy :: Proxy GetWorkflowRuns)

responseCreateWorkflow :: CreateWorkflowResponse -> TestTree
responseCreateWorkflow =
  res
    "CreateWorkflowResponse"
    "fixture/CreateWorkflowResponse.proto"
    glue
    (Proxy :: Proxy CreateWorkflow)

responseUpdateColumnStatisticsForTable :: UpdateColumnStatisticsForTableResponse -> TestTree
responseUpdateColumnStatisticsForTable =
  res
    "UpdateColumnStatisticsForTableResponse"
    "fixture/UpdateColumnStatisticsForTableResponse.proto"
    glue
    (Proxy :: Proxy UpdateColumnStatisticsForTable)

responseDeleteColumnStatisticsForTable :: DeleteColumnStatisticsForTableResponse -> TestTree
responseDeleteColumnStatisticsForTable =
  res
    "DeleteColumnStatisticsForTableResponse"
    "fixture/DeleteColumnStatisticsForTableResponse.proto"
    glue
    (Proxy :: Proxy DeleteColumnStatisticsForTable)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    glue
    (Proxy :: Proxy DeleteConnection)

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    glue
    (Proxy :: Proxy UpdateConnection)

responseGetUserDefinedFunctions :: GetUserDefinedFunctionsResponse -> TestTree
responseGetUserDefinedFunctions =
  res
    "GetUserDefinedFunctionsResponse"
    "fixture/GetUserDefinedFunctionsResponse.proto"
    glue
    (Proxy :: Proxy GetUserDefinedFunctions)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    glue
    (Proxy :: Proxy GetTags)

responseGetDataCatalogEncryptionSettings :: GetDataCatalogEncryptionSettingsResponse -> TestTree
responseGetDataCatalogEncryptionSettings =
  res
    "GetDataCatalogEncryptionSettingsResponse"
    "fixture/GetDataCatalogEncryptionSettingsResponse.proto"
    glue
    (Proxy :: Proxy GetDataCatalogEncryptionSettings)

responseBatchCreatePartition :: BatchCreatePartitionResponse -> TestTree
responseBatchCreatePartition =
  res
    "BatchCreatePartitionResponse"
    "fixture/BatchCreatePartitionResponse.proto"
    glue
    (Proxy :: Proxy BatchCreatePartition)

responseGetMapping :: GetMappingResponse -> TestTree
responseGetMapping =
  res
    "GetMappingResponse"
    "fixture/GetMappingResponse.proto"
    glue
    (Proxy :: Proxy GetMapping)

responseDeleteWorkflow :: DeleteWorkflowResponse -> TestTree
responseDeleteWorkflow =
  res
    "DeleteWorkflowResponse"
    "fixture/DeleteWorkflowResponse.proto"
    glue
    (Proxy :: Proxy DeleteWorkflow)

responseUpdateWorkflow :: UpdateWorkflowResponse -> TestTree
responseUpdateWorkflow =
  res
    "UpdateWorkflowResponse"
    "fixture/UpdateWorkflowResponse.proto"
    glue
    (Proxy :: Proxy UpdateWorkflow)

responseGetTableVersion :: GetTableVersionResponse -> TestTree
responseGetTableVersion =
  res
    "GetTableVersionResponse"
    "fixture/GetTableVersionResponse.proto"
    glue
    (Proxy :: Proxy GetTableVersion)

responseCreateSecurityConfiguration :: CreateSecurityConfigurationResponse -> TestTree
responseCreateSecurityConfiguration =
  res
    "CreateSecurityConfigurationResponse"
    "fixture/CreateSecurityConfigurationResponse.proto"
    glue
    (Proxy :: Proxy CreateSecurityConfiguration)

responseStartWorkflowRun :: StartWorkflowRunResponse -> TestTree
responseStartWorkflowRun =
  res
    "StartWorkflowRunResponse"
    "fixture/StartWorkflowRunResponse.proto"
    glue
    (Proxy :: Proxy StartWorkflowRun)

responseGetJobs :: GetJobsResponse -> TestTree
responseGetJobs =
  res
    "GetJobsResponse"
    "fixture/GetJobsResponse.proto"
    glue
    (Proxy :: Proxy GetJobs)

responseBatchGetWorkflows :: BatchGetWorkflowsResponse -> TestTree
responseBatchGetWorkflows =
  res
    "BatchGetWorkflowsResponse"
    "fixture/BatchGetWorkflowsResponse.proto"
    glue
    (Proxy :: Proxy BatchGetWorkflows)

responseGetClassifiers :: GetClassifiersResponse -> TestTree
responseGetClassifiers =
  res
    "GetClassifiersResponse"
    "fixture/GetClassifiersResponse.proto"
    glue
    (Proxy :: Proxy GetClassifiers)

responseGetResourcePolicies :: GetResourcePoliciesResponse -> TestTree
responseGetResourcePolicies =
  res
    "GetResourcePoliciesResponse"
    "fixture/GetResourcePoliciesResponse.proto"
    glue
    (Proxy :: Proxy GetResourcePolicies)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    glue
    (Proxy :: Proxy CreateConnection)

responseListSchemaVersions :: ListSchemaVersionsResponse -> TestTree
responseListSchemaVersions =
  res
    "ListSchemaVersionsResponse"
    "fixture/ListSchemaVersionsResponse.proto"
    glue
    (Proxy :: Proxy ListSchemaVersions)

responseGetWorkflowRunProperties :: GetWorkflowRunPropertiesResponse -> TestTree
responseGetWorkflowRunProperties =
  res
    "GetWorkflowRunPropertiesResponse"
    "fixture/GetWorkflowRunPropertiesResponse.proto"
    glue
    (Proxy :: Proxy GetWorkflowRunProperties)

responseBatchGetDevEndpoints :: BatchGetDevEndpointsResponse -> TestTree
responseBatchGetDevEndpoints =
  res
    "BatchGetDevEndpointsResponse"
    "fixture/BatchGetDevEndpointsResponse.proto"
    glue
    (Proxy :: Proxy BatchGetDevEndpoints)

responseDeletePartitionIndex :: DeletePartitionIndexResponse -> TestTree
responseDeletePartitionIndex =
  res
    "DeletePartitionIndexResponse"
    "fixture/DeletePartitionIndexResponse.proto"
    glue
    (Proxy :: Proxy DeletePartitionIndex)

responseDeleteTableVersion :: DeleteTableVersionResponse -> TestTree
responseDeleteTableVersion =
  res
    "DeleteTableVersionResponse"
    "fixture/DeleteTableVersionResponse.proto"
    glue
    (Proxy :: Proxy DeleteTableVersion)

responseDeleteDevEndpoint :: DeleteDevEndpointResponse -> TestTree
responseDeleteDevEndpoint =
  res
    "DeleteDevEndpointResponse"
    "fixture/DeleteDevEndpointResponse.proto"
    glue
    (Proxy :: Proxy DeleteDevEndpoint)

responseUpdateDevEndpoint :: UpdateDevEndpointResponse -> TestTree
responseUpdateDevEndpoint =
  res
    "UpdateDevEndpointResponse"
    "fixture/UpdateDevEndpointResponse.proto"
    glue
    (Proxy :: Proxy UpdateDevEndpoint)

responseGetWorkflow :: GetWorkflowResponse -> TestTree
responseGetWorkflow =
  res
    "GetWorkflowResponse"
    "fixture/GetWorkflowResponse.proto"
    glue
    (Proxy :: Proxy GetWorkflow)

responseBatchGetCrawlers :: BatchGetCrawlersResponse -> TestTree
responseBatchGetCrawlers =
  res
    "BatchGetCrawlersResponse"
    "fixture/BatchGetCrawlersResponse.proto"
    glue
    (Proxy :: Proxy BatchGetCrawlers)

responseGetJobBookmark :: GetJobBookmarkResponse -> TestTree
responseGetJobBookmark =
  res
    "GetJobBookmarkResponse"
    "fixture/GetJobBookmarkResponse.proto"
    glue
    (Proxy :: Proxy GetJobBookmark)

responseDeleteCrawler :: DeleteCrawlerResponse -> TestTree
responseDeleteCrawler =
  res
    "DeleteCrawlerResponse"
    "fixture/DeleteCrawlerResponse.proto"
    glue
    (Proxy :: Proxy DeleteCrawler)

responseUpdateCrawler :: UpdateCrawlerResponse -> TestTree
responseUpdateCrawler =
  res
    "UpdateCrawlerResponse"
    "fixture/UpdateCrawlerResponse.proto"
    glue
    (Proxy :: Proxy UpdateCrawler)

responseStartExportLabelsTaskRun :: StartExportLabelsTaskRunResponse -> TestTree
responseStartExportLabelsTaskRun =
  res
    "StartExportLabelsTaskRunResponse"
    "fixture/StartExportLabelsTaskRunResponse.proto"
    glue
    (Proxy :: Proxy StartExportLabelsTaskRun)

responseGetSecurityConfiguration :: GetSecurityConfigurationResponse -> TestTree
responseGetSecurityConfiguration =
  res
    "GetSecurityConfigurationResponse"
    "fixture/GetSecurityConfigurationResponse.proto"
    glue
    (Proxy :: Proxy GetSecurityConfiguration)

responseCreatePartitionIndex :: CreatePartitionIndexResponse -> TestTree
responseCreatePartitionIndex =
  res
    "CreatePartitionIndexResponse"
    "fixture/CreatePartitionIndexResponse.proto"
    glue
    (Proxy :: Proxy CreatePartitionIndex)

responseRemoveSchemaVersionMetadata :: RemoveSchemaVersionMetadataResponse -> TestTree
responseRemoveSchemaVersionMetadata =
  res
    "RemoveSchemaVersionMetadataResponse"
    "fixture/RemoveSchemaVersionMetadataResponse.proto"
    glue
    (Proxy :: Proxy RemoveSchemaVersionMetadata)

responseListSchemas :: ListSchemasResponse -> TestTree
responseListSchemas =
  res
    "ListSchemasResponse"
    "fixture/ListSchemasResponse.proto"
    glue
    (Proxy :: Proxy ListSchemas)

responseGetConnection :: GetConnectionResponse -> TestTree
responseGetConnection =
  res
    "GetConnectionResponse"
    "fixture/GetConnectionResponse.proto"
    glue
    (Proxy :: Proxy GetConnection)

responseGetColumnStatisticsForTable :: GetColumnStatisticsForTableResponse -> TestTree
responseGetColumnStatisticsForTable =
  res
    "GetColumnStatisticsForTableResponse"
    "fixture/GetColumnStatisticsForTableResponse.proto"
    glue
    (Proxy :: Proxy GetColumnStatisticsForTable)

responseBatchGetPartition :: BatchGetPartitionResponse -> TestTree
responseBatchGetPartition =
  res
    "BatchGetPartitionResponse"
    "fixture/BatchGetPartitionResponse.proto"
    glue
    (Proxy :: Proxy BatchGetPartition)

responseStopTrigger :: StopTriggerResponse -> TestTree
responseStopTrigger =
  res
    "StopTriggerResponse"
    "fixture/StopTriggerResponse.proto"
    glue
    (Proxy :: Proxy StopTrigger)

responseUpdateCrawlerSchedule :: UpdateCrawlerScheduleResponse -> TestTree
responseUpdateCrawlerSchedule =
  res
    "UpdateCrawlerScheduleResponse"
    "fixture/UpdateCrawlerScheduleResponse.proto"
    glue
    (Proxy :: Proxy UpdateCrawlerSchedule)

responseStartMLEvaluationTaskRun :: StartMLEvaluationTaskRunResponse -> TestTree
responseStartMLEvaluationTaskRun =
  res
    "StartMLEvaluationTaskRunResponse"
    "fixture/StartMLEvaluationTaskRunResponse.proto"
    glue
    (Proxy :: Proxy StartMLEvaluationTaskRun)

responseDeleteUserDefinedFunction :: DeleteUserDefinedFunctionResponse -> TestTree
responseDeleteUserDefinedFunction =
  res
    "DeleteUserDefinedFunctionResponse"
    "fixture/DeleteUserDefinedFunctionResponse.proto"
    glue
    (Proxy :: Proxy DeleteUserDefinedFunction)

responseUpdateUserDefinedFunction :: UpdateUserDefinedFunctionResponse -> TestTree
responseUpdateUserDefinedFunction =
  res
    "UpdateUserDefinedFunctionResponse"
    "fixture/UpdateUserDefinedFunctionResponse.proto"
    glue
    (Proxy :: Proxy UpdateUserDefinedFunction)

responseGetRegistry :: GetRegistryResponse -> TestTree
responseGetRegistry =
  res
    "GetRegistryResponse"
    "fixture/GetRegistryResponse.proto"
    glue
    (Proxy :: Proxy GetRegistry)

responseBatchDeleteTable :: BatchDeleteTableResponse -> TestTree
responseBatchDeleteTable =
  res
    "BatchDeleteTableResponse"
    "fixture/BatchDeleteTableResponse.proto"
    glue
    (Proxy :: Proxy BatchDeleteTable)

responseCancelMLTaskRun :: CancelMLTaskRunResponse -> TestTree
responseCancelMLTaskRun =
  res
    "CancelMLTaskRunResponse"
    "fixture/CancelMLTaskRunResponse.proto"
    glue
    (Proxy :: Proxy CancelMLTaskRun)

responseGetTables :: GetTablesResponse -> TestTree
responseGetTables =
  res
    "GetTablesResponse"
    "fixture/GetTablesResponse.proto"
    glue
    (Proxy :: Proxy GetTables)

responseResumeWorkflowRun :: ResumeWorkflowRunResponse -> TestTree
responseResumeWorkflowRun =
  res
    "ResumeWorkflowRunResponse"
    "fixture/ResumeWorkflowRunResponse.proto"
    glue
    (Proxy :: Proxy ResumeWorkflowRun)

responseCreateClassifier :: CreateClassifierResponse -> TestTree
responseCreateClassifier =
  res
    "CreateClassifierResponse"
    "fixture/CreateClassifierResponse.proto"
    glue
    (Proxy :: Proxy CreateClassifier)

responseBatchDeleteConnection :: BatchDeleteConnectionResponse -> TestTree
responseBatchDeleteConnection =
  res
    "BatchDeleteConnectionResponse"
    "fixture/BatchDeleteConnectionResponse.proto"
    glue
    (Proxy :: Proxy BatchDeleteConnection)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    glue
    (Proxy :: Proxy CreateJob)

responseGetJobRuns :: GetJobRunsResponse -> TestTree
responseGetJobRuns =
  res
    "GetJobRunsResponse"
    "fixture/GetJobRunsResponse.proto"
    glue
    (Proxy :: Proxy GetJobRuns)

responseCreateUserDefinedFunction :: CreateUserDefinedFunctionResponse -> TestTree
responseCreateUserDefinedFunction =
  res
    "CreateUserDefinedFunctionResponse"
    "fixture/CreateUserDefinedFunctionResponse.proto"
    glue
    (Proxy :: Proxy CreateUserDefinedFunction)

responseResetJobBookmark :: ResetJobBookmarkResponse -> TestTree
responseResetJobBookmark =
  res
    "ResetJobBookmarkResponse"
    "fixture/ResetJobBookmarkResponse.proto"
    glue
    (Proxy :: Proxy ResetJobBookmark)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    glue
    (Proxy :: Proxy ListJobs)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    glue
    (Proxy :: Proxy DeleteJob)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    glue
    (Proxy :: Proxy UpdateJob)

responseCreateRegistry :: CreateRegistryResponse -> TestTree
responseCreateRegistry =
  res
    "CreateRegistryResponse"
    "fixture/CreateRegistryResponse.proto"
    glue
    (Proxy :: Proxy CreateRegistry)

responseGetCrawlers :: GetCrawlersResponse -> TestTree
responseGetCrawlers =
  res
    "GetCrawlersResponse"
    "fixture/GetCrawlersResponse.proto"
    glue
    (Proxy :: Proxy GetCrawlers)

responseListTriggers :: ListTriggersResponse -> TestTree
responseListTriggers =
  res
    "ListTriggersResponse"
    "fixture/ListTriggersResponse.proto"
    glue
    (Proxy :: Proxy ListTriggers)

responseGetClassifier :: GetClassifierResponse -> TestTree
responseGetClassifier =
  res
    "GetClassifierResponse"
    "fixture/GetClassifierResponse.proto"
    glue
    (Proxy :: Proxy GetClassifier)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    glue
    (Proxy :: Proxy GetJob)

responseListRegistries :: ListRegistriesResponse -> TestTree
responseListRegistries =
  res
    "ListRegistriesResponse"
    "fixture/ListRegistriesResponse.proto"
    glue
    (Proxy :: Proxy ListRegistries)

responseBatchDeleteTableVersion :: BatchDeleteTableVersionResponse -> TestTree
responseBatchDeleteTableVersion =
  res
    "BatchDeleteTableVersionResponse"
    "fixture/BatchDeleteTableVersionResponse.proto"
    glue
    (Proxy :: Proxy BatchDeleteTableVersion)

responseGetDevEndpoints :: GetDevEndpointsResponse -> TestTree
responseGetDevEndpoints =
  res
    "GetDevEndpointsResponse"
    "fixture/GetDevEndpointsResponse.proto"
    glue
    (Proxy :: Proxy GetDevEndpoints)

responseStartCrawlerSchedule :: StartCrawlerScheduleResponse -> TestTree
responseStartCrawlerSchedule =
  res
    "StartCrawlerScheduleResponse"
    "fixture/StartCrawlerScheduleResponse.proto"
    glue
    (Proxy :: Proxy StartCrawlerSchedule)

responseGetPartitionIndexes :: GetPartitionIndexesResponse -> TestTree
responseGetPartitionIndexes =
  res
    "GetPartitionIndexesResponse"
    "fixture/GetPartitionIndexesResponse.proto"
    glue
    (Proxy :: Proxy GetPartitionIndexes)

responseGetUserDefinedFunction :: GetUserDefinedFunctionResponse -> TestTree
responseGetUserDefinedFunction =
  res
    "GetUserDefinedFunctionResponse"
    "fixture/GetUserDefinedFunctionResponse.proto"
    glue
    (Proxy :: Proxy GetUserDefinedFunction)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    glue
    (Proxy :: Proxy GetResourcePolicy)

responseGetWorkflowRun :: GetWorkflowRunResponse -> TestTree
responseGetWorkflowRun =
  res
    "GetWorkflowRunResponse"
    "fixture/GetWorkflowRunResponse.proto"
    glue
    (Proxy :: Proxy GetWorkflowRun)

responseDeleteDatabase :: DeleteDatabaseResponse -> TestTree
responseDeleteDatabase =
  res
    "DeleteDatabaseResponse"
    "fixture/DeleteDatabaseResponse.proto"
    glue
    (Proxy :: Proxy DeleteDatabase)

responseUpdateDatabase :: UpdateDatabaseResponse -> TestTree
responseUpdateDatabase =
  res
    "UpdateDatabaseResponse"
    "fixture/UpdateDatabaseResponse.proto"
    glue
    (Proxy :: Proxy UpdateDatabase)

responseGetColumnStatisticsForPartition :: GetColumnStatisticsForPartitionResponse -> TestTree
responseGetColumnStatisticsForPartition =
  res
    "GetColumnStatisticsForPartitionResponse"
    "fixture/GetColumnStatisticsForPartitionResponse.proto"
    glue
    (Proxy :: Proxy GetColumnStatisticsForPartition)

responseStopCrawler :: StopCrawlerResponse -> TestTree
responseStopCrawler =
  res
    "StopCrawlerResponse"
    "fixture/StopCrawlerResponse.proto"
    glue
    (Proxy :: Proxy StopCrawler)

responseDeleteSecurityConfiguration :: DeleteSecurityConfigurationResponse -> TestTree
responseDeleteSecurityConfiguration =
  res
    "DeleteSecurityConfigurationResponse"
    "fixture/DeleteSecurityConfigurationResponse.proto"
    glue
    (Proxy :: Proxy DeleteSecurityConfiguration)

responseGetPartitions :: GetPartitionsResponse -> TestTree
responseGetPartitions =
  res
    "GetPartitionsResponse"
    "fixture/GetPartitionsResponse.proto"
    glue
    (Proxy :: Proxy GetPartitions)

responsePutSchemaVersionMetadata :: PutSchemaVersionMetadataResponse -> TestTree
responsePutSchemaVersionMetadata =
  res
    "PutSchemaVersionMetadataResponse"
    "fixture/PutSchemaVersionMetadataResponse.proto"
    glue
    (Proxy :: Proxy PutSchemaVersionMetadata)

responseGetSchema :: GetSchemaResponse -> TestTree
responseGetSchema =
  res
    "GetSchemaResponse"
    "fixture/GetSchemaResponse.proto"
    glue
    (Proxy :: Proxy GetSchema)

responseBatchDeletePartition :: BatchDeletePartitionResponse -> TestTree
responseBatchDeletePartition =
  res
    "BatchDeletePartitionResponse"
    "fixture/BatchDeletePartitionResponse.proto"
    glue
    (Proxy :: Proxy BatchDeletePartition)

responseStartMLLabelingSetGenerationTaskRun :: StartMLLabelingSetGenerationTaskRunResponse -> TestTree
responseStartMLLabelingSetGenerationTaskRun =
  res
    "StartMLLabelingSetGenerationTaskRunResponse"
    "fixture/StartMLLabelingSetGenerationTaskRunResponse.proto"
    glue
    (Proxy :: Proxy StartMLLabelingSetGenerationTaskRun)

responseBatchUpdatePartition :: BatchUpdatePartitionResponse -> TestTree
responseBatchUpdatePartition =
  res
    "BatchUpdatePartitionResponse"
    "fixture/BatchUpdatePartitionResponse.proto"
    glue
    (Proxy :: Proxy BatchUpdatePartition)

responseRegisterSchemaVersion :: RegisterSchemaVersionResponse -> TestTree
responseRegisterSchemaVersion =
  res
    "RegisterSchemaVersionResponse"
    "fixture/RegisterSchemaVersionResponse.proto"
    glue
    (Proxy :: Proxy RegisterSchemaVersion)

responseStopWorkflowRun :: StopWorkflowRunResponse -> TestTree
responseStopWorkflowRun =
  res
    "StopWorkflowRunResponse"
    "fixture/StopWorkflowRunResponse.proto"
    glue
    (Proxy :: Proxy StopWorkflowRun)

responseGetCrawler :: GetCrawlerResponse -> TestTree
responseGetCrawler =
  res
    "GetCrawlerResponse"
    "fixture/GetCrawlerResponse.proto"
    glue
    (Proxy :: Proxy GetCrawler)

responseListWorkflows :: ListWorkflowsResponse -> TestTree
responseListWorkflows =
  res
    "ListWorkflowsResponse"
    "fixture/ListWorkflowsResponse.proto"
    glue
    (Proxy :: Proxy ListWorkflows)

responseBatchStopJobRun :: BatchStopJobRunResponse -> TestTree
responseBatchStopJobRun =
  res
    "BatchStopJobRunResponse"
    "fixture/BatchStopJobRunResponse.proto"
    glue
    (Proxy :: Proxy BatchStopJobRun)

responseGetDevEndpoint :: GetDevEndpointResponse -> TestTree
responseGetDevEndpoint =
  res
    "GetDevEndpointResponse"
    "fixture/GetDevEndpointResponse.proto"
    glue
    (Proxy :: Proxy GetDevEndpoint)

responsePutWorkflowRunProperties :: PutWorkflowRunPropertiesResponse -> TestTree
responsePutWorkflowRunProperties =
  res
    "PutWorkflowRunPropertiesResponse"
    "fixture/PutWorkflowRunPropertiesResponse.proto"
    glue
    (Proxy :: Proxy PutWorkflowRunProperties)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable =
  res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    glue
    (Proxy :: Proxy CreateTable)

responseListCrawlers :: ListCrawlersResponse -> TestTree
responseListCrawlers =
  res
    "ListCrawlersResponse"
    "fixture/ListCrawlersResponse.proto"
    glue
    (Proxy :: Proxy ListCrawlers)

responseGetCrawlerMetrics :: GetCrawlerMetricsResponse -> TestTree
responseGetCrawlerMetrics =
  res
    "GetCrawlerMetricsResponse"
    "fixture/GetCrawlerMetricsResponse.proto"
    glue
    (Proxy :: Proxy GetCrawlerMetrics)

responseGetSchemaVersion :: GetSchemaVersionResponse -> TestTree
responseGetSchemaVersion =
  res
    "GetSchemaVersionResponse"
    "fixture/GetSchemaVersionResponse.proto"
    glue
    (Proxy :: Proxy GetSchemaVersion)

responseGetPlan :: GetPlanResponse -> TestTree
responseGetPlan =
  res
    "GetPlanResponse"
    "fixture/GetPlanResponse.proto"
    glue
    (Proxy :: Proxy GetPlan)

responseGetTriggers :: GetTriggersResponse -> TestTree
responseGetTriggers =
  res
    "GetTriggersResponse"
    "fixture/GetTriggersResponse.proto"
    glue
    (Proxy :: Proxy GetTriggers)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    glue
    (Proxy :: Proxy CreateSchema)

responseListDevEndpoints :: ListDevEndpointsResponse -> TestTree
responseListDevEndpoints =
  res
    "ListDevEndpointsResponse"
    "fixture/ListDevEndpointsResponse.proto"
    glue
    (Proxy :: Proxy ListDevEndpoints)

responseStartTrigger :: StartTriggerResponse -> TestTree
responseStartTrigger =
  res
    "StartTriggerResponse"
    "fixture/StartTriggerResponse.proto"
    glue
    (Proxy :: Proxy StartTrigger)

responseGetDataflowGraph :: GetDataflowGraphResponse -> TestTree
responseGetDataflowGraph =
  res
    "GetDataflowGraphResponse"
    "fixture/GetDataflowGraphResponse.proto"
    glue
    (Proxy :: Proxy GetDataflowGraph)

responseGetDatabases :: GetDatabasesResponse -> TestTree
responseGetDatabases =
  res
    "GetDatabasesResponse"
    "fixture/GetDatabasesResponse.proto"
    glue
    (Proxy :: Proxy GetDatabases)

responseGetTable :: GetTableResponse -> TestTree
responseGetTable =
  res
    "GetTableResponse"
    "fixture/GetTableResponse.proto"
    glue
    (Proxy :: Proxy GetTable)

responseCreateCrawler :: CreateCrawlerResponse -> TestTree
responseCreateCrawler =
  res
    "CreateCrawlerResponse"
    "fixture/CreateCrawlerResponse.proto"
    glue
    (Proxy :: Proxy CreateCrawler)

responseGetJobRun :: GetJobRunResponse -> TestTree
responseGetJobRun =
  res
    "GetJobRunResponse"
    "fixture/GetJobRunResponse.proto"
    glue
    (Proxy :: Proxy GetJobRun)

responseCreateDevEndpoint :: CreateDevEndpointResponse -> TestTree
responseCreateDevEndpoint =
  res
    "CreateDevEndpointResponse"
    "fixture/CreateDevEndpointResponse.proto"
    glue
    (Proxy :: Proxy CreateDevEndpoint)

responseGetMLTaskRuns :: GetMLTaskRunsResponse -> TestTree
responseGetMLTaskRuns =
  res
    "GetMLTaskRunsResponse"
    "fixture/GetMLTaskRunsResponse.proto"
    glue
    (Proxy :: Proxy GetMLTaskRuns)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    glue
    (Proxy :: Proxy TagResource)

responsePutDataCatalogEncryptionSettings :: PutDataCatalogEncryptionSettingsResponse -> TestTree
responsePutDataCatalogEncryptionSettings =
  res
    "PutDataCatalogEncryptionSettingsResponse"
    "fixture/PutDataCatalogEncryptionSettingsResponse.proto"
    glue
    (Proxy :: Proxy PutDataCatalogEncryptionSettings)

responseGetMLTransforms :: GetMLTransformsResponse -> TestTree
responseGetMLTransforms =
  res
    "GetMLTransformsResponse"
    "fixture/GetMLTransformsResponse.proto"
    glue
    (Proxy :: Proxy GetMLTransforms)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    glue
    (Proxy :: Proxy UpdateSchema)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    glue
    (Proxy :: Proxy DeleteSchema)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    glue
    (Proxy :: Proxy GetDatabase)

responseDeleteColumnStatisticsForPartition :: DeleteColumnStatisticsForPartitionResponse -> TestTree
responseDeleteColumnStatisticsForPartition =
  res
    "DeleteColumnStatisticsForPartitionResponse"
    "fixture/DeleteColumnStatisticsForPartitionResponse.proto"
    glue
    (Proxy :: Proxy DeleteColumnStatisticsForPartition)

responseUpdateColumnStatisticsForPartition :: UpdateColumnStatisticsForPartitionResponse -> TestTree
responseUpdateColumnStatisticsForPartition =
  res
    "UpdateColumnStatisticsForPartitionResponse"
    "fixture/UpdateColumnStatisticsForPartitionResponse.proto"
    glue
    (Proxy :: Proxy UpdateColumnStatisticsForPartition)

responseGetMLTaskRun :: GetMLTaskRunResponse -> TestTree
responseGetMLTaskRun =
  res
    "GetMLTaskRunResponse"
    "fixture/GetMLTaskRunResponse.proto"
    glue
    (Proxy :: Proxy GetMLTaskRun)

responseDeletePartition :: DeletePartitionResponse -> TestTree
responseDeletePartition =
  res
    "DeletePartitionResponse"
    "fixture/DeletePartitionResponse.proto"
    glue
    (Proxy :: Proxy DeletePartition)

responseUpdatePartition :: UpdatePartitionResponse -> TestTree
responseUpdatePartition =
  res
    "UpdatePartitionResponse"
    "fixture/UpdatePartitionResponse.proto"
    glue
    (Proxy :: Proxy UpdatePartition)

responseGetMLTransform :: GetMLTransformResponse -> TestTree
responseGetMLTransform =
  res
    "GetMLTransformResponse"
    "fixture/GetMLTransformResponse.proto"
    glue
    (Proxy :: Proxy GetMLTransform)

responseCreateScript :: CreateScriptResponse -> TestTree
responseCreateScript =
  res
    "CreateScriptResponse"
    "fixture/CreateScriptResponse.proto"
    glue
    (Proxy :: Proxy CreateScript)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    glue
    (Proxy :: Proxy PutResourcePolicy)

responseGetSecurityConfigurations :: GetSecurityConfigurationsResponse -> TestTree
responseGetSecurityConfigurations =
  res
    "GetSecurityConfigurationsResponse"
    "fixture/GetSecurityConfigurationsResponse.proto"
    glue
    (Proxy :: Proxy GetSecurityConfigurations)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    glue
    (Proxy :: Proxy DeleteResourcePolicy)

responseGetConnections :: GetConnectionsResponse -> TestTree
responseGetConnections =
  res
    "GetConnectionsResponse"
    "fixture/GetConnectionsResponse.proto"
    glue
    (Proxy :: Proxy GetConnections)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    glue
    (Proxy :: Proxy UntagResource)

responseGetSchemaVersionsDiff :: GetSchemaVersionsDiffResponse -> TestTree
responseGetSchemaVersionsDiff =
  res
    "GetSchemaVersionsDiffResponse"
    "fixture/GetSchemaVersionsDiffResponse.proto"
    glue
    (Proxy :: Proxy GetSchemaVersionsDiff)

responseSearchTables :: SearchTablesResponse -> TestTree
responseSearchTables =
  res
    "SearchTablesResponse"
    "fixture/SearchTablesResponse.proto"
    glue
    (Proxy :: Proxy SearchTables)

responseGetTrigger :: GetTriggerResponse -> TestTree
responseGetTrigger =
  res
    "GetTriggerResponse"
    "fixture/GetTriggerResponse.proto"
    glue
    (Proxy :: Proxy GetTrigger)

responseBatchGetJobs :: BatchGetJobsResponse -> TestTree
responseBatchGetJobs =
  res
    "BatchGetJobsResponse"
    "fixture/BatchGetJobsResponse.proto"
    glue
    (Proxy :: Proxy BatchGetJobs)

responseImportCatalogToGlue :: ImportCatalogToGlueResponse -> TestTree
responseImportCatalogToGlue =
  res
    "ImportCatalogToGlueResponse"
    "fixture/ImportCatalogToGlueResponse.proto"
    glue
    (Proxy :: Proxy ImportCatalogToGlue)

responseDeleteClassifier :: DeleteClassifierResponse -> TestTree
responseDeleteClassifier =
  res
    "DeleteClassifierResponse"
    "fixture/DeleteClassifierResponse.proto"
    glue
    (Proxy :: Proxy DeleteClassifier)

responseUpdateClassifier :: UpdateClassifierResponse -> TestTree
responseUpdateClassifier =
  res
    "UpdateClassifierResponse"
    "fixture/UpdateClassifierResponse.proto"
    glue
    (Proxy :: Proxy UpdateClassifier)

responseStartJobRun :: StartJobRunResponse -> TestTree
responseStartJobRun =
  res
    "StartJobRunResponse"
    "fixture/StartJobRunResponse.proto"
    glue
    (Proxy :: Proxy StartJobRun)

responseCreatePartition :: CreatePartitionResponse -> TestTree
responseCreatePartition =
  res
    "CreatePartitionResponse"
    "fixture/CreatePartitionResponse.proto"
    glue
    (Proxy :: Proxy CreatePartition)

responseBatchGetTriggers :: BatchGetTriggersResponse -> TestTree
responseBatchGetTriggers =
  res
    "BatchGetTriggersResponse"
    "fixture/BatchGetTriggersResponse.proto"
    glue
    (Proxy :: Proxy BatchGetTriggers)

responseStopCrawlerSchedule :: StopCrawlerScheduleResponse -> TestTree
responseStopCrawlerSchedule =
  res
    "StopCrawlerScheduleResponse"
    "fixture/StopCrawlerScheduleResponse.proto"
    glue
    (Proxy :: Proxy StopCrawlerSchedule)

responseGetSchemaByDefinition :: GetSchemaByDefinitionResponse -> TestTree
responseGetSchemaByDefinition =
  res
    "GetSchemaByDefinitionResponse"
    "fixture/GetSchemaByDefinitionResponse.proto"
    glue
    (Proxy :: Proxy GetSchemaByDefinition)

responseCreateDatabase :: CreateDatabaseResponse -> TestTree
responseCreateDatabase =
  res
    "CreateDatabaseResponse"
    "fixture/CreateDatabaseResponse.proto"
    glue
    (Proxy :: Proxy CreateDatabase)

responseGetTableVersions :: GetTableVersionsResponse -> TestTree
responseGetTableVersions =
  res
    "GetTableVersionsResponse"
    "fixture/GetTableVersionsResponse.proto"
    glue
    (Proxy :: Proxy GetTableVersions)

responseCreateMLTransform :: CreateMLTransformResponse -> TestTree
responseCreateMLTransform =
  res
    "CreateMLTransformResponse"
    "fixture/CreateMLTransformResponse.proto"
    glue
    (Proxy :: Proxy CreateMLTransform)

responseDeleteSchemaVersions :: DeleteSchemaVersionsResponse -> TestTree
responseDeleteSchemaVersions =
  res
    "DeleteSchemaVersionsResponse"
    "fixture/DeleteSchemaVersionsResponse.proto"
    glue
    (Proxy :: Proxy DeleteSchemaVersions)

responseDeleteTrigger :: DeleteTriggerResponse -> TestTree
responseDeleteTrigger =
  res
    "DeleteTriggerResponse"
    "fixture/DeleteTriggerResponse.proto"
    glue
    (Proxy :: Proxy DeleteTrigger)

responseUpdateTrigger :: UpdateTriggerResponse -> TestTree
responseUpdateTrigger =
  res
    "UpdateTriggerResponse"
    "fixture/UpdateTriggerResponse.proto"
    glue
    (Proxy :: Proxy UpdateTrigger)
