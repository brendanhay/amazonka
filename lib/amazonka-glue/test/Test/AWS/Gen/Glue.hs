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
--             mkStartImportLabelsTaskRun
--
--         , requestUpdateMLTransform $
--             mkUpdateMLTransform
--
--         , requestUpdateRegistry $
--             mkUpdateRegistry
--
--         , requestDeleteRegistry $
--             mkDeleteRegistry
--
--         , requestDeleteMLTransform $
--             mkDeleteMLTransform
--
--         , requestStartCrawler $
--             mkStartCrawler
--
--         , requestGetCatalogImportStatus $
--             mkGetCatalogImportStatus
--
--         , requestListMLTransforms $
--             mkListMLTransforms
--
--         , requestGetPartition $
--             mkGetPartition
--
--         , requestQuerySchemaVersionMetadata $
--             mkQuerySchemaVersionMetadata
--
--         , requestCreateTrigger $
--             mkCreateTrigger
--
--         , requestCheckSchemaVersionValidity $
--             mkCheckSchemaVersionValidity
--
--         , requestDeleteTable $
--             mkDeleteTable
--
--         , requestUpdateTable $
--             mkUpdateTable
--
--         , requestGetWorkflowRuns $
--             mkGetWorkflowRuns
--
--         , requestCreateWorkflow $
--             mkCreateWorkflow
--
--         , requestUpdateColumnStatisticsForTable $
--             mkUpdateColumnStatisticsForTable
--
--         , requestDeleteColumnStatisticsForTable $
--             mkDeleteColumnStatisticsForTable
--
--         , requestDeleteConnection $
--             mkDeleteConnection
--
--         , requestUpdateConnection $
--             mkUpdateConnection
--
--         , requestGetUserDefinedFunctions $
--             mkGetUserDefinedFunctions
--
--         , requestGetTags $
--             mkGetTags
--
--         , requestGetDataCatalogEncryptionSettings $
--             mkGetDataCatalogEncryptionSettings
--
--         , requestBatchCreatePartition $
--             mkBatchCreatePartition
--
--         , requestGetMapping $
--             mkGetMapping
--
--         , requestDeleteWorkflow $
--             mkDeleteWorkflow
--
--         , requestUpdateWorkflow $
--             mkUpdateWorkflow
--
--         , requestGetTableVersion $
--             mkGetTableVersion
--
--         , requestCreateSecurityConfiguration $
--             mkCreateSecurityConfiguration
--
--         , requestStartWorkflowRun $
--             mkStartWorkflowRun
--
--         , requestGetJobs $
--             mkGetJobs
--
--         , requestBatchGetWorkflows $
--             mkBatchGetWorkflows
--
--         , requestGetClassifiers $
--             mkGetClassifiers
--
--         , requestGetResourcePolicies $
--             mkGetResourcePolicies
--
--         , requestCreateConnection $
--             mkCreateConnection
--
--         , requestListSchemaVersions $
--             mkListSchemaVersions
--
--         , requestGetWorkflowRunProperties $
--             mkGetWorkflowRunProperties
--
--         , requestBatchGetDevEndpoints $
--             mkBatchGetDevEndpoints
--
--         , requestDeletePartitionIndex $
--             mkDeletePartitionIndex
--
--         , requestDeleteTableVersion $
--             mkDeleteTableVersion
--
--         , requestDeleteDevEndpoint $
--             mkDeleteDevEndpoint
--
--         , requestUpdateDevEndpoint $
--             mkUpdateDevEndpoint
--
--         , requestGetWorkflow $
--             mkGetWorkflow
--
--         , requestBatchGetCrawlers $
--             mkBatchGetCrawlers
--
--         , requestGetJobBookmark $
--             mkGetJobBookmark
--
--         , requestDeleteCrawler $
--             mkDeleteCrawler
--
--         , requestUpdateCrawler $
--             mkUpdateCrawler
--
--         , requestStartExportLabelsTaskRun $
--             mkStartExportLabelsTaskRun
--
--         , requestGetSecurityConfiguration $
--             mkGetSecurityConfiguration
--
--         , requestCreatePartitionIndex $
--             mkCreatePartitionIndex
--
--         , requestRemoveSchemaVersionMetadata $
--             mkRemoveSchemaVersionMetadata
--
--         , requestListSchemas $
--             mkListSchemas
--
--         , requestGetConnection $
--             mkGetConnection
--
--         , requestGetColumnStatisticsForTable $
--             mkGetColumnStatisticsForTable
--
--         , requestBatchGetPartition $
--             mkBatchGetPartition
--
--         , requestStopTrigger $
--             mkStopTrigger
--
--         , requestUpdateCrawlerSchedule $
--             mkUpdateCrawlerSchedule
--
--         , requestStartMLEvaluationTaskRun $
--             mkStartMLEvaluationTaskRun
--
--         , requestDeleteUserDefinedFunction $
--             mkDeleteUserDefinedFunction
--
--         , requestUpdateUserDefinedFunction $
--             mkUpdateUserDefinedFunction
--
--         , requestGetRegistry $
--             mkGetRegistry
--
--         , requestBatchDeleteTable $
--             mkBatchDeleteTable
--
--         , requestCancelMLTaskRun $
--             mkCancelMLTaskRun
--
--         , requestGetTables $
--             mkGetTables
--
--         , requestResumeWorkflowRun $
--             mkResumeWorkflowRun
--
--         , requestCreateClassifier $
--             mkCreateClassifier
--
--         , requestBatchDeleteConnection $
--             mkBatchDeleteConnection
--
--         , requestCreateJob $
--             mkCreateJob
--
--         , requestGetJobRuns $
--             mkGetJobRuns
--
--         , requestCreateUserDefinedFunction $
--             mkCreateUserDefinedFunction
--
--         , requestResetJobBookmark $
--             mkResetJobBookmark
--
--         , requestListJobs $
--             mkListJobs
--
--         , requestDeleteJob $
--             mkDeleteJob
--
--         , requestUpdateJob $
--             mkUpdateJob
--
--         , requestCreateRegistry $
--             mkCreateRegistry
--
--         , requestGetCrawlers $
--             mkGetCrawlers
--
--         , requestListTriggers $
--             mkListTriggers
--
--         , requestGetClassifier $
--             mkGetClassifier
--
--         , requestGetJob $
--             mkGetJob
--
--         , requestListRegistries $
--             mkListRegistries
--
--         , requestBatchDeleteTableVersion $
--             mkBatchDeleteTableVersion
--
--         , requestGetDevEndpoints $
--             mkGetDevEndpoints
--
--         , requestStartCrawlerSchedule $
--             mkStartCrawlerSchedule
--
--         , requestGetPartitionIndexes $
--             mkGetPartitionIndexes
--
--         , requestGetUserDefinedFunction $
--             mkGetUserDefinedFunction
--
--         , requestGetResourcePolicy $
--             mkGetResourcePolicy
--
--         , requestGetWorkflowRun $
--             mkGetWorkflowRun
--
--         , requestDeleteDatabase $
--             mkDeleteDatabase
--
--         , requestUpdateDatabase $
--             mkUpdateDatabase
--
--         , requestGetColumnStatisticsForPartition $
--             mkGetColumnStatisticsForPartition
--
--         , requestStopCrawler $
--             mkStopCrawler
--
--         , requestDeleteSecurityConfiguration $
--             mkDeleteSecurityConfiguration
--
--         , requestGetPartitions $
--             mkGetPartitions
--
--         , requestPutSchemaVersionMetadata $
--             mkPutSchemaVersionMetadata
--
--         , requestGetSchema $
--             mkGetSchema
--
--         , requestBatchDeletePartition $
--             mkBatchDeletePartition
--
--         , requestStartMLLabelingSetGenerationTaskRun $
--             mkStartMLLabelingSetGenerationTaskRun
--
--         , requestBatchUpdatePartition $
--             mkBatchUpdatePartition
--
--         , requestRegisterSchemaVersion $
--             mkRegisterSchemaVersion
--
--         , requestStopWorkflowRun $
--             mkStopWorkflowRun
--
--         , requestGetCrawler $
--             mkGetCrawler
--
--         , requestListWorkflows $
--             mkListWorkflows
--
--         , requestBatchStopJobRun $
--             mkBatchStopJobRun
--
--         , requestGetDevEndpoint $
--             mkGetDevEndpoint
--
--         , requestPutWorkflowRunProperties $
--             mkPutWorkflowRunProperties
--
--         , requestCreateTable $
--             mkCreateTable
--
--         , requestListCrawlers $
--             mkListCrawlers
--
--         , requestGetCrawlerMetrics $
--             mkGetCrawlerMetrics
--
--         , requestGetSchemaVersion $
--             mkGetSchemaVersion
--
--         , requestGetPlan $
--             mkGetPlan
--
--         , requestGetTriggers $
--             mkGetTriggers
--
--         , requestCreateSchema $
--             mkCreateSchema
--
--         , requestListDevEndpoints $
--             mkListDevEndpoints
--
--         , requestStartTrigger $
--             mkStartTrigger
--
--         , requestGetDataflowGraph $
--             mkGetDataflowGraph
--
--         , requestGetDatabases $
--             mkGetDatabases
--
--         , requestGetTable $
--             mkGetTable
--
--         , requestCreateCrawler $
--             mkCreateCrawler
--
--         , requestGetJobRun $
--             mkGetJobRun
--
--         , requestCreateDevEndpoint $
--             mkCreateDevEndpoint
--
--         , requestGetMLTaskRuns $
--             mkGetMLTaskRuns
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestPutDataCatalogEncryptionSettings $
--             mkPutDataCatalogEncryptionSettings
--
--         , requestGetMLTransforms $
--             mkGetMLTransforms
--
--         , requestUpdateSchema $
--             mkUpdateSchema
--
--         , requestDeleteSchema $
--             mkDeleteSchema
--
--         , requestGetDatabase $
--             mkGetDatabase
--
--         , requestDeleteColumnStatisticsForPartition $
--             mkDeleteColumnStatisticsForPartition
--
--         , requestUpdateColumnStatisticsForPartition $
--             mkUpdateColumnStatisticsForPartition
--
--         , requestGetMLTaskRun $
--             mkGetMLTaskRun
--
--         , requestDeletePartition $
--             mkDeletePartition
--
--         , requestUpdatePartition $
--             mkUpdatePartition
--
--         , requestGetMLTransform $
--             mkGetMLTransform
--
--         , requestCreateScript $
--             mkCreateScript
--
--         , requestPutResourcePolicy $
--             mkPutResourcePolicy
--
--         , requestGetSecurityConfigurations $
--             mkGetSecurityConfigurations
--
--         , requestDeleteResourcePolicy $
--             mkDeleteResourcePolicy
--
--         , requestGetConnections $
--             mkGetConnections
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestGetSchemaVersionsDiff $
--             mkGetSchemaVersionsDiff
--
--         , requestSearchTables $
--             mkSearchTables
--
--         , requestGetTrigger $
--             mkGetTrigger
--
--         , requestBatchGetJobs $
--             mkBatchGetJobs
--
--         , requestImportCatalogToGlue $
--             mkImportCatalogToGlue
--
--         , requestDeleteClassifier $
--             mkDeleteClassifier
--
--         , requestUpdateClassifier $
--             mkUpdateClassifier
--
--         , requestStartJobRun $
--             mkStartJobRun
--
--         , requestCreatePartition $
--             mkCreatePartition
--
--         , requestBatchGetTriggers $
--             mkBatchGetTriggers
--
--         , requestStopCrawlerSchedule $
--             mkStopCrawlerSchedule
--
--         , requestGetSchemaByDefinition $
--             mkGetSchemaByDefinition
--
--         , requestCreateDatabase $
--             mkCreateDatabase
--
--         , requestGetTableVersions $
--             mkGetTableVersions
--
--         , requestCreateMLTransform $
--             mkCreateMLTransform
--
--         , requestDeleteSchemaVersions $
--             mkDeleteSchemaVersions
--
--         , requestDeleteTrigger $
--             mkDeleteTrigger
--
--         , requestUpdateTrigger $
--             mkUpdateTrigger
--
--           ]

--     , testGroup "response"
--         [ responseStartImportLabelsTaskRun $
--             mkStartImportLabelsTaskRunResponse
--
--         , responseUpdateMLTransform $
--             mkUpdateMLTransformResponse
--
--         , responseUpdateRegistry $
--             mkUpdateRegistryResponse
--
--         , responseDeleteRegistry $
--             mkDeleteRegistryResponse
--
--         , responseDeleteMLTransform $
--             mkDeleteMLTransformResponse
--
--         , responseStartCrawler $
--             mkStartCrawlerResponse
--
--         , responseGetCatalogImportStatus $
--             mkGetCatalogImportStatusResponse
--
--         , responseListMLTransforms $
--             mkListMLTransformsResponse
--
--         , responseGetPartition $
--             mkGetPartitionResponse
--
--         , responseQuerySchemaVersionMetadata $
--             mkQuerySchemaVersionMetadataResponse
--
--         , responseCreateTrigger $
--             mkCreateTriggerResponse
--
--         , responseCheckSchemaVersionValidity $
--             mkCheckSchemaVersionValidityResponse
--
--         , responseDeleteTable $
--             mkDeleteTableResponse
--
--         , responseUpdateTable $
--             mkUpdateTableResponse
--
--         , responseGetWorkflowRuns $
--             mkGetWorkflowRunsResponse
--
--         , responseCreateWorkflow $
--             mkCreateWorkflowResponse
--
--         , responseUpdateColumnStatisticsForTable $
--             mkUpdateColumnStatisticsForTableResponse
--
--         , responseDeleteColumnStatisticsForTable $
--             mkDeleteColumnStatisticsForTableResponse
--
--         , responseDeleteConnection $
--             mkDeleteConnectionResponse
--
--         , responseUpdateConnection $
--             mkUpdateConnectionResponse
--
--         , responseGetUserDefinedFunctions $
--             mkGetUserDefinedFunctionsResponse
--
--         , responseGetTags $
--             mkGetTagsResponse
--
--         , responseGetDataCatalogEncryptionSettings $
--             mkGetDataCatalogEncryptionSettingsResponse
--
--         , responseBatchCreatePartition $
--             mkBatchCreatePartitionResponse
--
--         , responseGetMapping $
--             mkGetMappingResponse
--
--         , responseDeleteWorkflow $
--             mkDeleteWorkflowResponse
--
--         , responseUpdateWorkflow $
--             mkUpdateWorkflowResponse
--
--         , responseGetTableVersion $
--             mkGetTableVersionResponse
--
--         , responseCreateSecurityConfiguration $
--             mkCreateSecurityConfigurationResponse
--
--         , responseStartWorkflowRun $
--             mkStartWorkflowRunResponse
--
--         , responseGetJobs $
--             mkGetJobsResponse
--
--         , responseBatchGetWorkflows $
--             mkBatchGetWorkflowsResponse
--
--         , responseGetClassifiers $
--             mkGetClassifiersResponse
--
--         , responseGetResourcePolicies $
--             mkGetResourcePoliciesResponse
--
--         , responseCreateConnection $
--             mkCreateConnectionResponse
--
--         , responseListSchemaVersions $
--             mkListSchemaVersionsResponse
--
--         , responseGetWorkflowRunProperties $
--             mkGetWorkflowRunPropertiesResponse
--
--         , responseBatchGetDevEndpoints $
--             mkBatchGetDevEndpointsResponse
--
--         , responseDeletePartitionIndex $
--             mkDeletePartitionIndexResponse
--
--         , responseDeleteTableVersion $
--             mkDeleteTableVersionResponse
--
--         , responseDeleteDevEndpoint $
--             mkDeleteDevEndpointResponse
--
--         , responseUpdateDevEndpoint $
--             mkUpdateDevEndpointResponse
--
--         , responseGetWorkflow $
--             mkGetWorkflowResponse
--
--         , responseBatchGetCrawlers $
--             mkBatchGetCrawlersResponse
--
--         , responseGetJobBookmark $
--             mkGetJobBookmarkResponse
--
--         , responseDeleteCrawler $
--             mkDeleteCrawlerResponse
--
--         , responseUpdateCrawler $
--             mkUpdateCrawlerResponse
--
--         , responseStartExportLabelsTaskRun $
--             mkStartExportLabelsTaskRunResponse
--
--         , responseGetSecurityConfiguration $
--             mkGetSecurityConfigurationResponse
--
--         , responseCreatePartitionIndex $
--             mkCreatePartitionIndexResponse
--
--         , responseRemoveSchemaVersionMetadata $
--             mkRemoveSchemaVersionMetadataResponse
--
--         , responseListSchemas $
--             mkListSchemasResponse
--
--         , responseGetConnection $
--             mkGetConnectionResponse
--
--         , responseGetColumnStatisticsForTable $
--             mkGetColumnStatisticsForTableResponse
--
--         , responseBatchGetPartition $
--             mkBatchGetPartitionResponse
--
--         , responseStopTrigger $
--             mkStopTriggerResponse
--
--         , responseUpdateCrawlerSchedule $
--             mkUpdateCrawlerScheduleResponse
--
--         , responseStartMLEvaluationTaskRun $
--             mkStartMLEvaluationTaskRunResponse
--
--         , responseDeleteUserDefinedFunction $
--             mkDeleteUserDefinedFunctionResponse
--
--         , responseUpdateUserDefinedFunction $
--             mkUpdateUserDefinedFunctionResponse
--
--         , responseGetRegistry $
--             mkGetRegistryResponse
--
--         , responseBatchDeleteTable $
--             mkBatchDeleteTableResponse
--
--         , responseCancelMLTaskRun $
--             mkCancelMLTaskRunResponse
--
--         , responseGetTables $
--             mkGetTablesResponse
--
--         , responseResumeWorkflowRun $
--             mkResumeWorkflowRunResponse
--
--         , responseCreateClassifier $
--             mkCreateClassifierResponse
--
--         , responseBatchDeleteConnection $
--             mkBatchDeleteConnectionResponse
--
--         , responseCreateJob $
--             mkCreateJobResponse
--
--         , responseGetJobRuns $
--             mkGetJobRunsResponse
--
--         , responseCreateUserDefinedFunction $
--             mkCreateUserDefinedFunctionResponse
--
--         , responseResetJobBookmark $
--             mkResetJobBookmarkResponse
--
--         , responseListJobs $
--             mkListJobsResponse
--
--         , responseDeleteJob $
--             mkDeleteJobResponse
--
--         , responseUpdateJob $
--             mkUpdateJobResponse
--
--         , responseCreateRegistry $
--             mkCreateRegistryResponse
--
--         , responseGetCrawlers $
--             mkGetCrawlersResponse
--
--         , responseListTriggers $
--             mkListTriggersResponse
--
--         , responseGetClassifier $
--             mkGetClassifierResponse
--
--         , responseGetJob $
--             mkGetJobResponse
--
--         , responseListRegistries $
--             mkListRegistriesResponse
--
--         , responseBatchDeleteTableVersion $
--             mkBatchDeleteTableVersionResponse
--
--         , responseGetDevEndpoints $
--             mkGetDevEndpointsResponse
--
--         , responseStartCrawlerSchedule $
--             mkStartCrawlerScheduleResponse
--
--         , responseGetPartitionIndexes $
--             mkGetPartitionIndexesResponse
--
--         , responseGetUserDefinedFunction $
--             mkGetUserDefinedFunctionResponse
--
--         , responseGetResourcePolicy $
--             mkGetResourcePolicyResponse
--
--         , responseGetWorkflowRun $
--             mkGetWorkflowRunResponse
--
--         , responseDeleteDatabase $
--             mkDeleteDatabaseResponse
--
--         , responseUpdateDatabase $
--             mkUpdateDatabaseResponse
--
--         , responseGetColumnStatisticsForPartition $
--             mkGetColumnStatisticsForPartitionResponse
--
--         , responseStopCrawler $
--             mkStopCrawlerResponse
--
--         , responseDeleteSecurityConfiguration $
--             mkDeleteSecurityConfigurationResponse
--
--         , responseGetPartitions $
--             mkGetPartitionsResponse
--
--         , responsePutSchemaVersionMetadata $
--             mkPutSchemaVersionMetadataResponse
--
--         , responseGetSchema $
--             mkGetSchemaResponse
--
--         , responseBatchDeletePartition $
--             mkBatchDeletePartitionResponse
--
--         , responseStartMLLabelingSetGenerationTaskRun $
--             mkStartMLLabelingSetGenerationTaskRunResponse
--
--         , responseBatchUpdatePartition $
--             mkBatchUpdatePartitionResponse
--
--         , responseRegisterSchemaVersion $
--             mkRegisterSchemaVersionResponse
--
--         , responseStopWorkflowRun $
--             mkStopWorkflowRunResponse
--
--         , responseGetCrawler $
--             mkGetCrawlerResponse
--
--         , responseListWorkflows $
--             mkListWorkflowsResponse
--
--         , responseBatchStopJobRun $
--             mkBatchStopJobRunResponse
--
--         , responseGetDevEndpoint $
--             mkGetDevEndpointResponse
--
--         , responsePutWorkflowRunProperties $
--             mkPutWorkflowRunPropertiesResponse
--
--         , responseCreateTable $
--             mkCreateTableResponse
--
--         , responseListCrawlers $
--             mkListCrawlersResponse
--
--         , responseGetCrawlerMetrics $
--             mkGetCrawlerMetricsResponse
--
--         , responseGetSchemaVersion $
--             mkGetSchemaVersionResponse
--
--         , responseGetPlan $
--             mkGetPlanResponse
--
--         , responseGetTriggers $
--             mkGetTriggersResponse
--
--         , responseCreateSchema $
--             mkCreateSchemaResponse
--
--         , responseListDevEndpoints $
--             mkListDevEndpointsResponse
--
--         , responseStartTrigger $
--             mkStartTriggerResponse
--
--         , responseGetDataflowGraph $
--             mkGetDataflowGraphResponse
--
--         , responseGetDatabases $
--             mkGetDatabasesResponse
--
--         , responseGetTable $
--             mkGetTableResponse
--
--         , responseCreateCrawler $
--             mkCreateCrawlerResponse
--
--         , responseGetJobRun $
--             mkGetJobRunResponse
--
--         , responseCreateDevEndpoint $
--             mkCreateDevEndpointResponse
--
--         , responseGetMLTaskRuns $
--             mkGetMLTaskRunsResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responsePutDataCatalogEncryptionSettings $
--             mkPutDataCatalogEncryptionSettingsResponse
--
--         , responseGetMLTransforms $
--             mkGetMLTransformsResponse
--
--         , responseUpdateSchema $
--             mkUpdateSchemaResponse
--
--         , responseDeleteSchema $
--             mkDeleteSchemaResponse
--
--         , responseGetDatabase $
--             mkGetDatabaseResponse
--
--         , responseDeleteColumnStatisticsForPartition $
--             mkDeleteColumnStatisticsForPartitionResponse
--
--         , responseUpdateColumnStatisticsForPartition $
--             mkUpdateColumnStatisticsForPartitionResponse
--
--         , responseGetMLTaskRun $
--             mkGetMLTaskRunResponse
--
--         , responseDeletePartition $
--             mkDeletePartitionResponse
--
--         , responseUpdatePartition $
--             mkUpdatePartitionResponse
--
--         , responseGetMLTransform $
--             mkGetMLTransformResponse
--
--         , responseCreateScript $
--             mkCreateScriptResponse
--
--         , responsePutResourcePolicy $
--             mkPutResourcePolicyResponse
--
--         , responseGetSecurityConfigurations $
--             mkGetSecurityConfigurationsResponse
--
--         , responseDeleteResourcePolicy $
--             mkDeleteResourcePolicyResponse
--
--         , responseGetConnections $
--             mkGetConnectionsResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseGetSchemaVersionsDiff $
--             mkGetSchemaVersionsDiffResponse
--
--         , responseSearchTables $
--             mkSearchTablesResponse
--
--         , responseGetTrigger $
--             mkGetTriggerResponse
--
--         , responseBatchGetJobs $
--             mkBatchGetJobsResponse
--
--         , responseImportCatalogToGlue $
--             mkImportCatalogToGlueResponse
--
--         , responseDeleteClassifier $
--             mkDeleteClassifierResponse
--
--         , responseUpdateClassifier $
--             mkUpdateClassifierResponse
--
--         , responseStartJobRun $
--             mkStartJobRunResponse
--
--         , responseCreatePartition $
--             mkCreatePartitionResponse
--
--         , responseBatchGetTriggers $
--             mkBatchGetTriggersResponse
--
--         , responseStopCrawlerSchedule $
--             mkStopCrawlerScheduleResponse
--
--         , responseGetSchemaByDefinition $
--             mkGetSchemaByDefinitionResponse
--
--         , responseCreateDatabase $
--             mkCreateDatabaseResponse
--
--         , responseGetTableVersions $
--             mkGetTableVersionsResponse
--
--         , responseCreateMLTransform $
--             mkCreateMLTransformResponse
--
--         , responseDeleteSchemaVersions $
--             mkDeleteSchemaVersionsResponse
--
--         , responseDeleteTrigger $
--             mkDeleteTriggerResponse
--
--         , responseUpdateTrigger $
--             mkUpdateTriggerResponse
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
    glueService
    (Proxy :: Proxy StartImportLabelsTaskRun)

responseUpdateMLTransform :: UpdateMLTransformResponse -> TestTree
responseUpdateMLTransform =
  res
    "UpdateMLTransformResponse"
    "fixture/UpdateMLTransformResponse.proto"
    glueService
    (Proxy :: Proxy UpdateMLTransform)

responseUpdateRegistry :: UpdateRegistryResponse -> TestTree
responseUpdateRegistry =
  res
    "UpdateRegistryResponse"
    "fixture/UpdateRegistryResponse.proto"
    glueService
    (Proxy :: Proxy UpdateRegistry)

responseDeleteRegistry :: DeleteRegistryResponse -> TestTree
responseDeleteRegistry =
  res
    "DeleteRegistryResponse"
    "fixture/DeleteRegistryResponse.proto"
    glueService
    (Proxy :: Proxy DeleteRegistry)

responseDeleteMLTransform :: DeleteMLTransformResponse -> TestTree
responseDeleteMLTransform =
  res
    "DeleteMLTransformResponse"
    "fixture/DeleteMLTransformResponse.proto"
    glueService
    (Proxy :: Proxy DeleteMLTransform)

responseStartCrawler :: StartCrawlerResponse -> TestTree
responseStartCrawler =
  res
    "StartCrawlerResponse"
    "fixture/StartCrawlerResponse.proto"
    glueService
    (Proxy :: Proxy StartCrawler)

responseGetCatalogImportStatus :: GetCatalogImportStatusResponse -> TestTree
responseGetCatalogImportStatus =
  res
    "GetCatalogImportStatusResponse"
    "fixture/GetCatalogImportStatusResponse.proto"
    glueService
    (Proxy :: Proxy GetCatalogImportStatus)

responseListMLTransforms :: ListMLTransformsResponse -> TestTree
responseListMLTransforms =
  res
    "ListMLTransformsResponse"
    "fixture/ListMLTransformsResponse.proto"
    glueService
    (Proxy :: Proxy ListMLTransforms)

responseGetPartition :: GetPartitionResponse -> TestTree
responseGetPartition =
  res
    "GetPartitionResponse"
    "fixture/GetPartitionResponse.proto"
    glueService
    (Proxy :: Proxy GetPartition)

responseQuerySchemaVersionMetadata :: QuerySchemaVersionMetadataResponse -> TestTree
responseQuerySchemaVersionMetadata =
  res
    "QuerySchemaVersionMetadataResponse"
    "fixture/QuerySchemaVersionMetadataResponse.proto"
    glueService
    (Proxy :: Proxy QuerySchemaVersionMetadata)

responseCreateTrigger :: CreateTriggerResponse -> TestTree
responseCreateTrigger =
  res
    "CreateTriggerResponse"
    "fixture/CreateTriggerResponse.proto"
    glueService
    (Proxy :: Proxy CreateTrigger)

responseCheckSchemaVersionValidity :: CheckSchemaVersionValidityResponse -> TestTree
responseCheckSchemaVersionValidity =
  res
    "CheckSchemaVersionValidityResponse"
    "fixture/CheckSchemaVersionValidityResponse.proto"
    glueService
    (Proxy :: Proxy CheckSchemaVersionValidity)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable =
  res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    glueService
    (Proxy :: Proxy DeleteTable)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable =
  res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    glueService
    (Proxy :: Proxy UpdateTable)

responseGetWorkflowRuns :: GetWorkflowRunsResponse -> TestTree
responseGetWorkflowRuns =
  res
    "GetWorkflowRunsResponse"
    "fixture/GetWorkflowRunsResponse.proto"
    glueService
    (Proxy :: Proxy GetWorkflowRuns)

responseCreateWorkflow :: CreateWorkflowResponse -> TestTree
responseCreateWorkflow =
  res
    "CreateWorkflowResponse"
    "fixture/CreateWorkflowResponse.proto"
    glueService
    (Proxy :: Proxy CreateWorkflow)

responseUpdateColumnStatisticsForTable :: UpdateColumnStatisticsForTableResponse -> TestTree
responseUpdateColumnStatisticsForTable =
  res
    "UpdateColumnStatisticsForTableResponse"
    "fixture/UpdateColumnStatisticsForTableResponse.proto"
    glueService
    (Proxy :: Proxy UpdateColumnStatisticsForTable)

responseDeleteColumnStatisticsForTable :: DeleteColumnStatisticsForTableResponse -> TestTree
responseDeleteColumnStatisticsForTable =
  res
    "DeleteColumnStatisticsForTableResponse"
    "fixture/DeleteColumnStatisticsForTableResponse.proto"
    glueService
    (Proxy :: Proxy DeleteColumnStatisticsForTable)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    glueService
    (Proxy :: Proxy DeleteConnection)

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    glueService
    (Proxy :: Proxy UpdateConnection)

responseGetUserDefinedFunctions :: GetUserDefinedFunctionsResponse -> TestTree
responseGetUserDefinedFunctions =
  res
    "GetUserDefinedFunctionsResponse"
    "fixture/GetUserDefinedFunctionsResponse.proto"
    glueService
    (Proxy :: Proxy GetUserDefinedFunctions)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    glueService
    (Proxy :: Proxy GetTags)

responseGetDataCatalogEncryptionSettings :: GetDataCatalogEncryptionSettingsResponse -> TestTree
responseGetDataCatalogEncryptionSettings =
  res
    "GetDataCatalogEncryptionSettingsResponse"
    "fixture/GetDataCatalogEncryptionSettingsResponse.proto"
    glueService
    (Proxy :: Proxy GetDataCatalogEncryptionSettings)

responseBatchCreatePartition :: BatchCreatePartitionResponse -> TestTree
responseBatchCreatePartition =
  res
    "BatchCreatePartitionResponse"
    "fixture/BatchCreatePartitionResponse.proto"
    glueService
    (Proxy :: Proxy BatchCreatePartition)

responseGetMapping :: GetMappingResponse -> TestTree
responseGetMapping =
  res
    "GetMappingResponse"
    "fixture/GetMappingResponse.proto"
    glueService
    (Proxy :: Proxy GetMapping)

responseDeleteWorkflow :: DeleteWorkflowResponse -> TestTree
responseDeleteWorkflow =
  res
    "DeleteWorkflowResponse"
    "fixture/DeleteWorkflowResponse.proto"
    glueService
    (Proxy :: Proxy DeleteWorkflow)

responseUpdateWorkflow :: UpdateWorkflowResponse -> TestTree
responseUpdateWorkflow =
  res
    "UpdateWorkflowResponse"
    "fixture/UpdateWorkflowResponse.proto"
    glueService
    (Proxy :: Proxy UpdateWorkflow)

responseGetTableVersion :: GetTableVersionResponse -> TestTree
responseGetTableVersion =
  res
    "GetTableVersionResponse"
    "fixture/GetTableVersionResponse.proto"
    glueService
    (Proxy :: Proxy GetTableVersion)

responseCreateSecurityConfiguration :: CreateSecurityConfigurationResponse -> TestTree
responseCreateSecurityConfiguration =
  res
    "CreateSecurityConfigurationResponse"
    "fixture/CreateSecurityConfigurationResponse.proto"
    glueService
    (Proxy :: Proxy CreateSecurityConfiguration)

responseStartWorkflowRun :: StartWorkflowRunResponse -> TestTree
responseStartWorkflowRun =
  res
    "StartWorkflowRunResponse"
    "fixture/StartWorkflowRunResponse.proto"
    glueService
    (Proxy :: Proxy StartWorkflowRun)

responseGetJobs :: GetJobsResponse -> TestTree
responseGetJobs =
  res
    "GetJobsResponse"
    "fixture/GetJobsResponse.proto"
    glueService
    (Proxy :: Proxy GetJobs)

responseBatchGetWorkflows :: BatchGetWorkflowsResponse -> TestTree
responseBatchGetWorkflows =
  res
    "BatchGetWorkflowsResponse"
    "fixture/BatchGetWorkflowsResponse.proto"
    glueService
    (Proxy :: Proxy BatchGetWorkflows)

responseGetClassifiers :: GetClassifiersResponse -> TestTree
responseGetClassifiers =
  res
    "GetClassifiersResponse"
    "fixture/GetClassifiersResponse.proto"
    glueService
    (Proxy :: Proxy GetClassifiers)

responseGetResourcePolicies :: GetResourcePoliciesResponse -> TestTree
responseGetResourcePolicies =
  res
    "GetResourcePoliciesResponse"
    "fixture/GetResourcePoliciesResponse.proto"
    glueService
    (Proxy :: Proxy GetResourcePolicies)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    glueService
    (Proxy :: Proxy CreateConnection)

responseListSchemaVersions :: ListSchemaVersionsResponse -> TestTree
responseListSchemaVersions =
  res
    "ListSchemaVersionsResponse"
    "fixture/ListSchemaVersionsResponse.proto"
    glueService
    (Proxy :: Proxy ListSchemaVersions)

responseGetWorkflowRunProperties :: GetWorkflowRunPropertiesResponse -> TestTree
responseGetWorkflowRunProperties =
  res
    "GetWorkflowRunPropertiesResponse"
    "fixture/GetWorkflowRunPropertiesResponse.proto"
    glueService
    (Proxy :: Proxy GetWorkflowRunProperties)

responseBatchGetDevEndpoints :: BatchGetDevEndpointsResponse -> TestTree
responseBatchGetDevEndpoints =
  res
    "BatchGetDevEndpointsResponse"
    "fixture/BatchGetDevEndpointsResponse.proto"
    glueService
    (Proxy :: Proxy BatchGetDevEndpoints)

responseDeletePartitionIndex :: DeletePartitionIndexResponse -> TestTree
responseDeletePartitionIndex =
  res
    "DeletePartitionIndexResponse"
    "fixture/DeletePartitionIndexResponse.proto"
    glueService
    (Proxy :: Proxy DeletePartitionIndex)

responseDeleteTableVersion :: DeleteTableVersionResponse -> TestTree
responseDeleteTableVersion =
  res
    "DeleteTableVersionResponse"
    "fixture/DeleteTableVersionResponse.proto"
    glueService
    (Proxy :: Proxy DeleteTableVersion)

responseDeleteDevEndpoint :: DeleteDevEndpointResponse -> TestTree
responseDeleteDevEndpoint =
  res
    "DeleteDevEndpointResponse"
    "fixture/DeleteDevEndpointResponse.proto"
    glueService
    (Proxy :: Proxy DeleteDevEndpoint)

responseUpdateDevEndpoint :: UpdateDevEndpointResponse -> TestTree
responseUpdateDevEndpoint =
  res
    "UpdateDevEndpointResponse"
    "fixture/UpdateDevEndpointResponse.proto"
    glueService
    (Proxy :: Proxy UpdateDevEndpoint)

responseGetWorkflow :: GetWorkflowResponse -> TestTree
responseGetWorkflow =
  res
    "GetWorkflowResponse"
    "fixture/GetWorkflowResponse.proto"
    glueService
    (Proxy :: Proxy GetWorkflow)

responseBatchGetCrawlers :: BatchGetCrawlersResponse -> TestTree
responseBatchGetCrawlers =
  res
    "BatchGetCrawlersResponse"
    "fixture/BatchGetCrawlersResponse.proto"
    glueService
    (Proxy :: Proxy BatchGetCrawlers)

responseGetJobBookmark :: GetJobBookmarkResponse -> TestTree
responseGetJobBookmark =
  res
    "GetJobBookmarkResponse"
    "fixture/GetJobBookmarkResponse.proto"
    glueService
    (Proxy :: Proxy GetJobBookmark)

responseDeleteCrawler :: DeleteCrawlerResponse -> TestTree
responseDeleteCrawler =
  res
    "DeleteCrawlerResponse"
    "fixture/DeleteCrawlerResponse.proto"
    glueService
    (Proxy :: Proxy DeleteCrawler)

responseUpdateCrawler :: UpdateCrawlerResponse -> TestTree
responseUpdateCrawler =
  res
    "UpdateCrawlerResponse"
    "fixture/UpdateCrawlerResponse.proto"
    glueService
    (Proxy :: Proxy UpdateCrawler)

responseStartExportLabelsTaskRun :: StartExportLabelsTaskRunResponse -> TestTree
responseStartExportLabelsTaskRun =
  res
    "StartExportLabelsTaskRunResponse"
    "fixture/StartExportLabelsTaskRunResponse.proto"
    glueService
    (Proxy :: Proxy StartExportLabelsTaskRun)

responseGetSecurityConfiguration :: GetSecurityConfigurationResponse -> TestTree
responseGetSecurityConfiguration =
  res
    "GetSecurityConfigurationResponse"
    "fixture/GetSecurityConfigurationResponse.proto"
    glueService
    (Proxy :: Proxy GetSecurityConfiguration)

responseCreatePartitionIndex :: CreatePartitionIndexResponse -> TestTree
responseCreatePartitionIndex =
  res
    "CreatePartitionIndexResponse"
    "fixture/CreatePartitionIndexResponse.proto"
    glueService
    (Proxy :: Proxy CreatePartitionIndex)

responseRemoveSchemaVersionMetadata :: RemoveSchemaVersionMetadataResponse -> TestTree
responseRemoveSchemaVersionMetadata =
  res
    "RemoveSchemaVersionMetadataResponse"
    "fixture/RemoveSchemaVersionMetadataResponse.proto"
    glueService
    (Proxy :: Proxy RemoveSchemaVersionMetadata)

responseListSchemas :: ListSchemasResponse -> TestTree
responseListSchemas =
  res
    "ListSchemasResponse"
    "fixture/ListSchemasResponse.proto"
    glueService
    (Proxy :: Proxy ListSchemas)

responseGetConnection :: GetConnectionResponse -> TestTree
responseGetConnection =
  res
    "GetConnectionResponse"
    "fixture/GetConnectionResponse.proto"
    glueService
    (Proxy :: Proxy GetConnection)

responseGetColumnStatisticsForTable :: GetColumnStatisticsForTableResponse -> TestTree
responseGetColumnStatisticsForTable =
  res
    "GetColumnStatisticsForTableResponse"
    "fixture/GetColumnStatisticsForTableResponse.proto"
    glueService
    (Proxy :: Proxy GetColumnStatisticsForTable)

responseBatchGetPartition :: BatchGetPartitionResponse -> TestTree
responseBatchGetPartition =
  res
    "BatchGetPartitionResponse"
    "fixture/BatchGetPartitionResponse.proto"
    glueService
    (Proxy :: Proxy BatchGetPartition)

responseStopTrigger :: StopTriggerResponse -> TestTree
responseStopTrigger =
  res
    "StopTriggerResponse"
    "fixture/StopTriggerResponse.proto"
    glueService
    (Proxy :: Proxy StopTrigger)

responseUpdateCrawlerSchedule :: UpdateCrawlerScheduleResponse -> TestTree
responseUpdateCrawlerSchedule =
  res
    "UpdateCrawlerScheduleResponse"
    "fixture/UpdateCrawlerScheduleResponse.proto"
    glueService
    (Proxy :: Proxy UpdateCrawlerSchedule)

responseStartMLEvaluationTaskRun :: StartMLEvaluationTaskRunResponse -> TestTree
responseStartMLEvaluationTaskRun =
  res
    "StartMLEvaluationTaskRunResponse"
    "fixture/StartMLEvaluationTaskRunResponse.proto"
    glueService
    (Proxy :: Proxy StartMLEvaluationTaskRun)

responseDeleteUserDefinedFunction :: DeleteUserDefinedFunctionResponse -> TestTree
responseDeleteUserDefinedFunction =
  res
    "DeleteUserDefinedFunctionResponse"
    "fixture/DeleteUserDefinedFunctionResponse.proto"
    glueService
    (Proxy :: Proxy DeleteUserDefinedFunction)

responseUpdateUserDefinedFunction :: UpdateUserDefinedFunctionResponse -> TestTree
responseUpdateUserDefinedFunction =
  res
    "UpdateUserDefinedFunctionResponse"
    "fixture/UpdateUserDefinedFunctionResponse.proto"
    glueService
    (Proxy :: Proxy UpdateUserDefinedFunction)

responseGetRegistry :: GetRegistryResponse -> TestTree
responseGetRegistry =
  res
    "GetRegistryResponse"
    "fixture/GetRegistryResponse.proto"
    glueService
    (Proxy :: Proxy GetRegistry)

responseBatchDeleteTable :: BatchDeleteTableResponse -> TestTree
responseBatchDeleteTable =
  res
    "BatchDeleteTableResponse"
    "fixture/BatchDeleteTableResponse.proto"
    glueService
    (Proxy :: Proxy BatchDeleteTable)

responseCancelMLTaskRun :: CancelMLTaskRunResponse -> TestTree
responseCancelMLTaskRun =
  res
    "CancelMLTaskRunResponse"
    "fixture/CancelMLTaskRunResponse.proto"
    glueService
    (Proxy :: Proxy CancelMLTaskRun)

responseGetTables :: GetTablesResponse -> TestTree
responseGetTables =
  res
    "GetTablesResponse"
    "fixture/GetTablesResponse.proto"
    glueService
    (Proxy :: Proxy GetTables)

responseResumeWorkflowRun :: ResumeWorkflowRunResponse -> TestTree
responseResumeWorkflowRun =
  res
    "ResumeWorkflowRunResponse"
    "fixture/ResumeWorkflowRunResponse.proto"
    glueService
    (Proxy :: Proxy ResumeWorkflowRun)

responseCreateClassifier :: CreateClassifierResponse -> TestTree
responseCreateClassifier =
  res
    "CreateClassifierResponse"
    "fixture/CreateClassifierResponse.proto"
    glueService
    (Proxy :: Proxy CreateClassifier)

responseBatchDeleteConnection :: BatchDeleteConnectionResponse -> TestTree
responseBatchDeleteConnection =
  res
    "BatchDeleteConnectionResponse"
    "fixture/BatchDeleteConnectionResponse.proto"
    glueService
    (Proxy :: Proxy BatchDeleteConnection)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    glueService
    (Proxy :: Proxy CreateJob)

responseGetJobRuns :: GetJobRunsResponse -> TestTree
responseGetJobRuns =
  res
    "GetJobRunsResponse"
    "fixture/GetJobRunsResponse.proto"
    glueService
    (Proxy :: Proxy GetJobRuns)

responseCreateUserDefinedFunction :: CreateUserDefinedFunctionResponse -> TestTree
responseCreateUserDefinedFunction =
  res
    "CreateUserDefinedFunctionResponse"
    "fixture/CreateUserDefinedFunctionResponse.proto"
    glueService
    (Proxy :: Proxy CreateUserDefinedFunction)

responseResetJobBookmark :: ResetJobBookmarkResponse -> TestTree
responseResetJobBookmark =
  res
    "ResetJobBookmarkResponse"
    "fixture/ResetJobBookmarkResponse.proto"
    glueService
    (Proxy :: Proxy ResetJobBookmark)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    glueService
    (Proxy :: Proxy ListJobs)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    glueService
    (Proxy :: Proxy DeleteJob)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    glueService
    (Proxy :: Proxy UpdateJob)

responseCreateRegistry :: CreateRegistryResponse -> TestTree
responseCreateRegistry =
  res
    "CreateRegistryResponse"
    "fixture/CreateRegistryResponse.proto"
    glueService
    (Proxy :: Proxy CreateRegistry)

responseGetCrawlers :: GetCrawlersResponse -> TestTree
responseGetCrawlers =
  res
    "GetCrawlersResponse"
    "fixture/GetCrawlersResponse.proto"
    glueService
    (Proxy :: Proxy GetCrawlers)

responseListTriggers :: ListTriggersResponse -> TestTree
responseListTriggers =
  res
    "ListTriggersResponse"
    "fixture/ListTriggersResponse.proto"
    glueService
    (Proxy :: Proxy ListTriggers)

responseGetClassifier :: GetClassifierResponse -> TestTree
responseGetClassifier =
  res
    "GetClassifierResponse"
    "fixture/GetClassifierResponse.proto"
    glueService
    (Proxy :: Proxy GetClassifier)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    glueService
    (Proxy :: Proxy GetJob)

responseListRegistries :: ListRegistriesResponse -> TestTree
responseListRegistries =
  res
    "ListRegistriesResponse"
    "fixture/ListRegistriesResponse.proto"
    glueService
    (Proxy :: Proxy ListRegistries)

responseBatchDeleteTableVersion :: BatchDeleteTableVersionResponse -> TestTree
responseBatchDeleteTableVersion =
  res
    "BatchDeleteTableVersionResponse"
    "fixture/BatchDeleteTableVersionResponse.proto"
    glueService
    (Proxy :: Proxy BatchDeleteTableVersion)

responseGetDevEndpoints :: GetDevEndpointsResponse -> TestTree
responseGetDevEndpoints =
  res
    "GetDevEndpointsResponse"
    "fixture/GetDevEndpointsResponse.proto"
    glueService
    (Proxy :: Proxy GetDevEndpoints)

responseStartCrawlerSchedule :: StartCrawlerScheduleResponse -> TestTree
responseStartCrawlerSchedule =
  res
    "StartCrawlerScheduleResponse"
    "fixture/StartCrawlerScheduleResponse.proto"
    glueService
    (Proxy :: Proxy StartCrawlerSchedule)

responseGetPartitionIndexes :: GetPartitionIndexesResponse -> TestTree
responseGetPartitionIndexes =
  res
    "GetPartitionIndexesResponse"
    "fixture/GetPartitionIndexesResponse.proto"
    glueService
    (Proxy :: Proxy GetPartitionIndexes)

responseGetUserDefinedFunction :: GetUserDefinedFunctionResponse -> TestTree
responseGetUserDefinedFunction =
  res
    "GetUserDefinedFunctionResponse"
    "fixture/GetUserDefinedFunctionResponse.proto"
    glueService
    (Proxy :: Proxy GetUserDefinedFunction)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    glueService
    (Proxy :: Proxy GetResourcePolicy)

responseGetWorkflowRun :: GetWorkflowRunResponse -> TestTree
responseGetWorkflowRun =
  res
    "GetWorkflowRunResponse"
    "fixture/GetWorkflowRunResponse.proto"
    glueService
    (Proxy :: Proxy GetWorkflowRun)

responseDeleteDatabase :: DeleteDatabaseResponse -> TestTree
responseDeleteDatabase =
  res
    "DeleteDatabaseResponse"
    "fixture/DeleteDatabaseResponse.proto"
    glueService
    (Proxy :: Proxy DeleteDatabase)

responseUpdateDatabase :: UpdateDatabaseResponse -> TestTree
responseUpdateDatabase =
  res
    "UpdateDatabaseResponse"
    "fixture/UpdateDatabaseResponse.proto"
    glueService
    (Proxy :: Proxy UpdateDatabase)

responseGetColumnStatisticsForPartition :: GetColumnStatisticsForPartitionResponse -> TestTree
responseGetColumnStatisticsForPartition =
  res
    "GetColumnStatisticsForPartitionResponse"
    "fixture/GetColumnStatisticsForPartitionResponse.proto"
    glueService
    (Proxy :: Proxy GetColumnStatisticsForPartition)

responseStopCrawler :: StopCrawlerResponse -> TestTree
responseStopCrawler =
  res
    "StopCrawlerResponse"
    "fixture/StopCrawlerResponse.proto"
    glueService
    (Proxy :: Proxy StopCrawler)

responseDeleteSecurityConfiguration :: DeleteSecurityConfigurationResponse -> TestTree
responseDeleteSecurityConfiguration =
  res
    "DeleteSecurityConfigurationResponse"
    "fixture/DeleteSecurityConfigurationResponse.proto"
    glueService
    (Proxy :: Proxy DeleteSecurityConfiguration)

responseGetPartitions :: GetPartitionsResponse -> TestTree
responseGetPartitions =
  res
    "GetPartitionsResponse"
    "fixture/GetPartitionsResponse.proto"
    glueService
    (Proxy :: Proxy GetPartitions)

responsePutSchemaVersionMetadata :: PutSchemaVersionMetadataResponse -> TestTree
responsePutSchemaVersionMetadata =
  res
    "PutSchemaVersionMetadataResponse"
    "fixture/PutSchemaVersionMetadataResponse.proto"
    glueService
    (Proxy :: Proxy PutSchemaVersionMetadata)

responseGetSchema :: GetSchemaResponse -> TestTree
responseGetSchema =
  res
    "GetSchemaResponse"
    "fixture/GetSchemaResponse.proto"
    glueService
    (Proxy :: Proxy GetSchema)

responseBatchDeletePartition :: BatchDeletePartitionResponse -> TestTree
responseBatchDeletePartition =
  res
    "BatchDeletePartitionResponse"
    "fixture/BatchDeletePartitionResponse.proto"
    glueService
    (Proxy :: Proxy BatchDeletePartition)

responseStartMLLabelingSetGenerationTaskRun :: StartMLLabelingSetGenerationTaskRunResponse -> TestTree
responseStartMLLabelingSetGenerationTaskRun =
  res
    "StartMLLabelingSetGenerationTaskRunResponse"
    "fixture/StartMLLabelingSetGenerationTaskRunResponse.proto"
    glueService
    (Proxy :: Proxy StartMLLabelingSetGenerationTaskRun)

responseBatchUpdatePartition :: BatchUpdatePartitionResponse -> TestTree
responseBatchUpdatePartition =
  res
    "BatchUpdatePartitionResponse"
    "fixture/BatchUpdatePartitionResponse.proto"
    glueService
    (Proxy :: Proxy BatchUpdatePartition)

responseRegisterSchemaVersion :: RegisterSchemaVersionResponse -> TestTree
responseRegisterSchemaVersion =
  res
    "RegisterSchemaVersionResponse"
    "fixture/RegisterSchemaVersionResponse.proto"
    glueService
    (Proxy :: Proxy RegisterSchemaVersion)

responseStopWorkflowRun :: StopWorkflowRunResponse -> TestTree
responseStopWorkflowRun =
  res
    "StopWorkflowRunResponse"
    "fixture/StopWorkflowRunResponse.proto"
    glueService
    (Proxy :: Proxy StopWorkflowRun)

responseGetCrawler :: GetCrawlerResponse -> TestTree
responseGetCrawler =
  res
    "GetCrawlerResponse"
    "fixture/GetCrawlerResponse.proto"
    glueService
    (Proxy :: Proxy GetCrawler)

responseListWorkflows :: ListWorkflowsResponse -> TestTree
responseListWorkflows =
  res
    "ListWorkflowsResponse"
    "fixture/ListWorkflowsResponse.proto"
    glueService
    (Proxy :: Proxy ListWorkflows)

responseBatchStopJobRun :: BatchStopJobRunResponse -> TestTree
responseBatchStopJobRun =
  res
    "BatchStopJobRunResponse"
    "fixture/BatchStopJobRunResponse.proto"
    glueService
    (Proxy :: Proxy BatchStopJobRun)

responseGetDevEndpoint :: GetDevEndpointResponse -> TestTree
responseGetDevEndpoint =
  res
    "GetDevEndpointResponse"
    "fixture/GetDevEndpointResponse.proto"
    glueService
    (Proxy :: Proxy GetDevEndpoint)

responsePutWorkflowRunProperties :: PutWorkflowRunPropertiesResponse -> TestTree
responsePutWorkflowRunProperties =
  res
    "PutWorkflowRunPropertiesResponse"
    "fixture/PutWorkflowRunPropertiesResponse.proto"
    glueService
    (Proxy :: Proxy PutWorkflowRunProperties)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable =
  res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    glueService
    (Proxy :: Proxy CreateTable)

responseListCrawlers :: ListCrawlersResponse -> TestTree
responseListCrawlers =
  res
    "ListCrawlersResponse"
    "fixture/ListCrawlersResponse.proto"
    glueService
    (Proxy :: Proxy ListCrawlers)

responseGetCrawlerMetrics :: GetCrawlerMetricsResponse -> TestTree
responseGetCrawlerMetrics =
  res
    "GetCrawlerMetricsResponse"
    "fixture/GetCrawlerMetricsResponse.proto"
    glueService
    (Proxy :: Proxy GetCrawlerMetrics)

responseGetSchemaVersion :: GetSchemaVersionResponse -> TestTree
responseGetSchemaVersion =
  res
    "GetSchemaVersionResponse"
    "fixture/GetSchemaVersionResponse.proto"
    glueService
    (Proxy :: Proxy GetSchemaVersion)

responseGetPlan :: GetPlanResponse -> TestTree
responseGetPlan =
  res
    "GetPlanResponse"
    "fixture/GetPlanResponse.proto"
    glueService
    (Proxy :: Proxy GetPlan)

responseGetTriggers :: GetTriggersResponse -> TestTree
responseGetTriggers =
  res
    "GetTriggersResponse"
    "fixture/GetTriggersResponse.proto"
    glueService
    (Proxy :: Proxy GetTriggers)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    glueService
    (Proxy :: Proxy CreateSchema)

responseListDevEndpoints :: ListDevEndpointsResponse -> TestTree
responseListDevEndpoints =
  res
    "ListDevEndpointsResponse"
    "fixture/ListDevEndpointsResponse.proto"
    glueService
    (Proxy :: Proxy ListDevEndpoints)

responseStartTrigger :: StartTriggerResponse -> TestTree
responseStartTrigger =
  res
    "StartTriggerResponse"
    "fixture/StartTriggerResponse.proto"
    glueService
    (Proxy :: Proxy StartTrigger)

responseGetDataflowGraph :: GetDataflowGraphResponse -> TestTree
responseGetDataflowGraph =
  res
    "GetDataflowGraphResponse"
    "fixture/GetDataflowGraphResponse.proto"
    glueService
    (Proxy :: Proxy GetDataflowGraph)

responseGetDatabases :: GetDatabasesResponse -> TestTree
responseGetDatabases =
  res
    "GetDatabasesResponse"
    "fixture/GetDatabasesResponse.proto"
    glueService
    (Proxy :: Proxy GetDatabases)

responseGetTable :: GetTableResponse -> TestTree
responseGetTable =
  res
    "GetTableResponse"
    "fixture/GetTableResponse.proto"
    glueService
    (Proxy :: Proxy GetTable)

responseCreateCrawler :: CreateCrawlerResponse -> TestTree
responseCreateCrawler =
  res
    "CreateCrawlerResponse"
    "fixture/CreateCrawlerResponse.proto"
    glueService
    (Proxy :: Proxy CreateCrawler)

responseGetJobRun :: GetJobRunResponse -> TestTree
responseGetJobRun =
  res
    "GetJobRunResponse"
    "fixture/GetJobRunResponse.proto"
    glueService
    (Proxy :: Proxy GetJobRun)

responseCreateDevEndpoint :: CreateDevEndpointResponse -> TestTree
responseCreateDevEndpoint =
  res
    "CreateDevEndpointResponse"
    "fixture/CreateDevEndpointResponse.proto"
    glueService
    (Proxy :: Proxy CreateDevEndpoint)

responseGetMLTaskRuns :: GetMLTaskRunsResponse -> TestTree
responseGetMLTaskRuns =
  res
    "GetMLTaskRunsResponse"
    "fixture/GetMLTaskRunsResponse.proto"
    glueService
    (Proxy :: Proxy GetMLTaskRuns)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    glueService
    (Proxy :: Proxy TagResource)

responsePutDataCatalogEncryptionSettings :: PutDataCatalogEncryptionSettingsResponse -> TestTree
responsePutDataCatalogEncryptionSettings =
  res
    "PutDataCatalogEncryptionSettingsResponse"
    "fixture/PutDataCatalogEncryptionSettingsResponse.proto"
    glueService
    (Proxy :: Proxy PutDataCatalogEncryptionSettings)

responseGetMLTransforms :: GetMLTransformsResponse -> TestTree
responseGetMLTransforms =
  res
    "GetMLTransformsResponse"
    "fixture/GetMLTransformsResponse.proto"
    glueService
    (Proxy :: Proxy GetMLTransforms)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    glueService
    (Proxy :: Proxy UpdateSchema)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    glueService
    (Proxy :: Proxy DeleteSchema)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    glueService
    (Proxy :: Proxy GetDatabase)

responseDeleteColumnStatisticsForPartition :: DeleteColumnStatisticsForPartitionResponse -> TestTree
responseDeleteColumnStatisticsForPartition =
  res
    "DeleteColumnStatisticsForPartitionResponse"
    "fixture/DeleteColumnStatisticsForPartitionResponse.proto"
    glueService
    (Proxy :: Proxy DeleteColumnStatisticsForPartition)

responseUpdateColumnStatisticsForPartition :: UpdateColumnStatisticsForPartitionResponse -> TestTree
responseUpdateColumnStatisticsForPartition =
  res
    "UpdateColumnStatisticsForPartitionResponse"
    "fixture/UpdateColumnStatisticsForPartitionResponse.proto"
    glueService
    (Proxy :: Proxy UpdateColumnStatisticsForPartition)

responseGetMLTaskRun :: GetMLTaskRunResponse -> TestTree
responseGetMLTaskRun =
  res
    "GetMLTaskRunResponse"
    "fixture/GetMLTaskRunResponse.proto"
    glueService
    (Proxy :: Proxy GetMLTaskRun)

responseDeletePartition :: DeletePartitionResponse -> TestTree
responseDeletePartition =
  res
    "DeletePartitionResponse"
    "fixture/DeletePartitionResponse.proto"
    glueService
    (Proxy :: Proxy DeletePartition)

responseUpdatePartition :: UpdatePartitionResponse -> TestTree
responseUpdatePartition =
  res
    "UpdatePartitionResponse"
    "fixture/UpdatePartitionResponse.proto"
    glueService
    (Proxy :: Proxy UpdatePartition)

responseGetMLTransform :: GetMLTransformResponse -> TestTree
responseGetMLTransform =
  res
    "GetMLTransformResponse"
    "fixture/GetMLTransformResponse.proto"
    glueService
    (Proxy :: Proxy GetMLTransform)

responseCreateScript :: CreateScriptResponse -> TestTree
responseCreateScript =
  res
    "CreateScriptResponse"
    "fixture/CreateScriptResponse.proto"
    glueService
    (Proxy :: Proxy CreateScript)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    glueService
    (Proxy :: Proxy PutResourcePolicy)

responseGetSecurityConfigurations :: GetSecurityConfigurationsResponse -> TestTree
responseGetSecurityConfigurations =
  res
    "GetSecurityConfigurationsResponse"
    "fixture/GetSecurityConfigurationsResponse.proto"
    glueService
    (Proxy :: Proxy GetSecurityConfigurations)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    glueService
    (Proxy :: Proxy DeleteResourcePolicy)

responseGetConnections :: GetConnectionsResponse -> TestTree
responseGetConnections =
  res
    "GetConnectionsResponse"
    "fixture/GetConnectionsResponse.proto"
    glueService
    (Proxy :: Proxy GetConnections)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    glueService
    (Proxy :: Proxy UntagResource)

responseGetSchemaVersionsDiff :: GetSchemaVersionsDiffResponse -> TestTree
responseGetSchemaVersionsDiff =
  res
    "GetSchemaVersionsDiffResponse"
    "fixture/GetSchemaVersionsDiffResponse.proto"
    glueService
    (Proxy :: Proxy GetSchemaVersionsDiff)

responseSearchTables :: SearchTablesResponse -> TestTree
responseSearchTables =
  res
    "SearchTablesResponse"
    "fixture/SearchTablesResponse.proto"
    glueService
    (Proxy :: Proxy SearchTables)

responseGetTrigger :: GetTriggerResponse -> TestTree
responseGetTrigger =
  res
    "GetTriggerResponse"
    "fixture/GetTriggerResponse.proto"
    glueService
    (Proxy :: Proxy GetTrigger)

responseBatchGetJobs :: BatchGetJobsResponse -> TestTree
responseBatchGetJobs =
  res
    "BatchGetJobsResponse"
    "fixture/BatchGetJobsResponse.proto"
    glueService
    (Proxy :: Proxy BatchGetJobs)

responseImportCatalogToGlue :: ImportCatalogToGlueResponse -> TestTree
responseImportCatalogToGlue =
  res
    "ImportCatalogToGlueResponse"
    "fixture/ImportCatalogToGlueResponse.proto"
    glueService
    (Proxy :: Proxy ImportCatalogToGlue)

responseDeleteClassifier :: DeleteClassifierResponse -> TestTree
responseDeleteClassifier =
  res
    "DeleteClassifierResponse"
    "fixture/DeleteClassifierResponse.proto"
    glueService
    (Proxy :: Proxy DeleteClassifier)

responseUpdateClassifier :: UpdateClassifierResponse -> TestTree
responseUpdateClassifier =
  res
    "UpdateClassifierResponse"
    "fixture/UpdateClassifierResponse.proto"
    glueService
    (Proxy :: Proxy UpdateClassifier)

responseStartJobRun :: StartJobRunResponse -> TestTree
responseStartJobRun =
  res
    "StartJobRunResponse"
    "fixture/StartJobRunResponse.proto"
    glueService
    (Proxy :: Proxy StartJobRun)

responseCreatePartition :: CreatePartitionResponse -> TestTree
responseCreatePartition =
  res
    "CreatePartitionResponse"
    "fixture/CreatePartitionResponse.proto"
    glueService
    (Proxy :: Proxy CreatePartition)

responseBatchGetTriggers :: BatchGetTriggersResponse -> TestTree
responseBatchGetTriggers =
  res
    "BatchGetTriggersResponse"
    "fixture/BatchGetTriggersResponse.proto"
    glueService
    (Proxy :: Proxy BatchGetTriggers)

responseStopCrawlerSchedule :: StopCrawlerScheduleResponse -> TestTree
responseStopCrawlerSchedule =
  res
    "StopCrawlerScheduleResponse"
    "fixture/StopCrawlerScheduleResponse.proto"
    glueService
    (Proxy :: Proxy StopCrawlerSchedule)

responseGetSchemaByDefinition :: GetSchemaByDefinitionResponse -> TestTree
responseGetSchemaByDefinition =
  res
    "GetSchemaByDefinitionResponse"
    "fixture/GetSchemaByDefinitionResponse.proto"
    glueService
    (Proxy :: Proxy GetSchemaByDefinition)

responseCreateDatabase :: CreateDatabaseResponse -> TestTree
responseCreateDatabase =
  res
    "CreateDatabaseResponse"
    "fixture/CreateDatabaseResponse.proto"
    glueService
    (Proxy :: Proxy CreateDatabase)

responseGetTableVersions :: GetTableVersionsResponse -> TestTree
responseGetTableVersions =
  res
    "GetTableVersionsResponse"
    "fixture/GetTableVersionsResponse.proto"
    glueService
    (Proxy :: Proxy GetTableVersions)

responseCreateMLTransform :: CreateMLTransformResponse -> TestTree
responseCreateMLTransform =
  res
    "CreateMLTransformResponse"
    "fixture/CreateMLTransformResponse.proto"
    glueService
    (Proxy :: Proxy CreateMLTransform)

responseDeleteSchemaVersions :: DeleteSchemaVersionsResponse -> TestTree
responseDeleteSchemaVersions =
  res
    "DeleteSchemaVersionsResponse"
    "fixture/DeleteSchemaVersionsResponse.proto"
    glueService
    (Proxy :: Proxy DeleteSchemaVersions)

responseDeleteTrigger :: DeleteTriggerResponse -> TestTree
responseDeleteTrigger =
  res
    "DeleteTriggerResponse"
    "fixture/DeleteTriggerResponse.proto"
    glueService
    (Proxy :: Proxy DeleteTrigger)

responseUpdateTrigger :: UpdateTriggerResponse -> TestTree
responseUpdateTrigger =
  res
    "UpdateTriggerResponse"
    "fixture/UpdateTriggerResponse.proto"
    glueService
    (Proxy :: Proxy UpdateTrigger)
