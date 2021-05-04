{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Glue
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestGetDataCatalogEncryptionSettings $
--             newGetDataCatalogEncryptionSettings
--
--         , requestUpdateColumnStatisticsForTable $
--             newUpdateColumnStatisticsForTable
--
--         , requestStartMLLabelingSetGenerationTaskRun $
--             newStartMLLabelingSetGenerationTaskRun
--
--         , requestDeleteColumnStatisticsForTable $
--             newDeleteColumnStatisticsForTable
--
--         , requestGetSchema $
--             newGetSchema
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestUpdateConnection $
--             newUpdateConnection
--
--         , requestCheckSchemaVersionValidity $
--             newCheckSchemaVersionValidity
--
--         , requestCreateWorkflow $
--             newCreateWorkflow
--
--         , requestGetPartitions $
--             newGetPartitions
--
--         , requestDeleteSecurityConfiguration $
--             newDeleteSecurityConfiguration
--
--         , requestGetPartition $
--             newGetPartition
--
--         , requestUpdateRegistry $
--             newUpdateRegistry
--
--         , requestListMLTransforms $
--             newListMLTransforms
--
--         , requestStopCrawler $
--             newStopCrawler
--
--         , requestStartImportLabelsTaskRun $
--             newStartImportLabelsTaskRun
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestQuerySchemaVersionMetadata $
--             newQuerySchemaVersionMetadata
--
--         , requestDeleteRegistry $
--             newDeleteRegistry
--
--         , requestGetPartitionIndexes $
--             newGetPartitionIndexes
--
--         , requestStartCrawler $
--             newStartCrawler
--
--         , requestGetCatalogImportStatus $
--             newGetCatalogImportStatus
--
--         , requestGetColumnStatisticsForPartition $
--             newGetColumnStatisticsForPartition
--
--         , requestCreateRegistry $
--             newCreateRegistry
--
--         , requestListTriggers $
--             newListTriggers
--
--         , requestCreateMLTransform $
--             newCreateMLTransform
--
--         , requestStopCrawlerSchedule $
--             newStopCrawlerSchedule
--
--         , requestUpdateTrigger $
--             newUpdateTrigger
--
--         , requestGetSchemaByDefinition $
--             newGetSchemaByDefinition
--
--         , requestListRegistries $
--             newListRegistries
--
--         , requestStartCrawlerSchedule $
--             newStartCrawlerSchedule
--
--         , requestDeleteTrigger $
--             newDeleteTrigger
--
--         , requestGetJob $
--             newGetJob
--
--         , requestUpdateClassifier $
--             newUpdateClassifier
--
--         , requestDeleteClassifier $
--             newDeleteClassifier
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestUpdateJob $
--             newUpdateJob
--
--         , requestCreateUserDefinedFunction $
--             newCreateUserDefinedFunction
--
--         , requestGetTrigger $
--             newGetTrigger
--
--         , requestBatchGetJobs $
--             newBatchGetJobs
--
--         , requestCreateClassifier $
--             newCreateClassifier
--
--         , requestGetSecurityConfigurations $
--             newGetSecurityConfigurations
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestUpdatePartition $
--             newUpdatePartition
--
--         , requestGetSchemaVersionsDiff $
--             newGetSchemaVersionsDiff
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestBatchDeleteTable $
--             newBatchDeleteTable
--
--         , requestStartMLEvaluationTaskRun $
--             newStartMLEvaluationTaskRun
--
--         , requestGetDatabase $
--             newGetDatabase
--
--         , requestDeletePartition $
--             newDeletePartition
--
--         , requestGetJobRuns $
--             newGetJobRuns
--
--         , requestGetMLTransforms $
--             newGetMLTransforms
--
--         , requestGetJobRun $
--             newGetJobRun
--
--         , requestCreateDevEndpoint $
--             newCreateDevEndpoint
--
--         , requestCreatePartitionIndex $
--             newCreatePartitionIndex
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetSecurityConfiguration $
--             newGetSecurityConfiguration
--
--         , requestCreateCrawler $
--             newCreateCrawler
--
--         , requestGetMLTaskRuns $
--             newGetMLTaskRuns
--
--         , requestListCrawlers $
--             newListCrawlers
--
--         , requestUpdateDevEndpoint $
--             newUpdateDevEndpoint
--
--         , requestCreateSchema $
--             newCreateSchema
--
--         , requestListDevEndpoints $
--             newListDevEndpoints
--
--         , requestDeleteCrawler $
--             newDeleteCrawler
--
--         , requestDeleteDevEndpoint $
--             newDeleteDevEndpoint
--
--         , requestGetWorkflow $
--             newGetWorkflow
--
--         , requestGetSchemaVersion $
--             newGetSchemaVersion
--
--         , requestUpdateCrawler $
--             newUpdateCrawler
--
--         , requestDeleteWorkflow $
--             newDeleteWorkflow
--
--         , requestRegisterSchemaVersion $
--             newRegisterSchemaVersion
--
--         , requestGetMapping $
--             newGetMapping
--
--         , requestStopWorkflowRun $
--             newStopWorkflowRun
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestBatchCreatePartition $
--             newBatchCreatePartition
--
--         , requestCreateTable $
--             newCreateTable
--
--         , requestUpdateWorkflow $
--             newUpdateWorkflow
--
--         , requestGetClassifiers $
--             newGetClassifiers
--
--         , requestBatchStopJobRun $
--             newBatchStopJobRun
--
--         , requestStartWorkflowRun $
--             newStartWorkflowRun
--
--         , requestListWorkflows $
--             newListWorkflows
--
--         , requestListSchemaVersions $
--             newListSchemaVersions
--
--         , requestBatchDeletePartition $
--             newBatchDeletePartition
--
--         , requestPutSchemaVersionMetadata $
--             newPutSchemaVersionMetadata
--
--         , requestGetWorkflowRuns $
--             newGetWorkflowRuns
--
--         , requestGetTags $
--             newGetTags
--
--         , requestBatchUpdatePartition $
--             newBatchUpdatePartition
--
--         , requestGetUserDefinedFunctions $
--             newGetUserDefinedFunctions
--
--         , requestUpdateTable $
--             newUpdateTable
--
--         , requestDeleteTable $
--             newDeleteTable
--
--         , requestDeleteDatabase $
--             newDeleteDatabase
--
--         , requestUpdateDatabase $
--             newUpdateDatabase
--
--         , requestGetUserDefinedFunction $
--             newGetUserDefinedFunction
--
--         , requestUpdateMLTransform $
--             newUpdateMLTransform
--
--         , requestGetWorkflowRun $
--             newGetWorkflowRun
--
--         , requestDeleteMLTransform $
--             newDeleteMLTransform
--
--         , requestCreateTrigger $
--             newCreateTrigger
--
--         , requestCreateDatabase $
--             newCreateDatabase
--
--         , requestGetClassifier $
--             newGetClassifier
--
--         , requestDeleteSchemaVersions $
--             newDeleteSchemaVersions
--
--         , requestBatchGetTriggers $
--             newBatchGetTriggers
--
--         , requestBatchDeleteTableVersion $
--             newBatchDeleteTableVersion
--
--         , requestGetTableVersions $
--             newGetTableVersions
--
--         , requestGetDevEndpoints $
--             newGetDevEndpoints
--
--         , requestGetCrawlers $
--             newGetCrawlers
--
--         , requestStartJobRun $
--             newStartJobRun
--
--         , requestImportCatalogToGlue $
--             newImportCatalogToGlue
--
--         , requestCreatePartition $
--             newCreatePartition
--
--         , requestResetJobBookmark $
--             newResetJobBookmark
--
--         , requestListJobs $
--             newListJobs
--
--         , requestBatchDeleteConnection $
--             newBatchDeleteConnection
--
--         , requestGetTables $
--             newGetTables
--
--         , requestDeleteColumnStatisticsForPartition $
--             newDeleteColumnStatisticsForPartition
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestGetRegistry $
--             newGetRegistry
--
--         , requestResumeWorkflowRun $
--             newResumeWorkflowRun
--
--         , requestCancelMLTaskRun $
--             newCancelMLTaskRun
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestSearchTables $
--             newSearchTables
--
--         , requestUpdateUserDefinedFunction $
--             newUpdateUserDefinedFunction
--
--         , requestUpdateColumnStatisticsForPartition $
--             newUpdateColumnStatisticsForPartition
--
--         , requestGetConnections $
--             newGetConnections
--
--         , requestGetMLTransform $
--             newGetMLTransform
--
--         , requestCreateScript $
--             newCreateScript
--
--         , requestGetMLTaskRun $
--             newGetMLTaskRun
--
--         , requestDeleteUserDefinedFunction $
--             newDeleteUserDefinedFunction
--
--         , requestStartTrigger $
--             newStartTrigger
--
--         , requestPutDataCatalogEncryptionSettings $
--             newPutDataCatalogEncryptionSettings
--
--         , requestRemoveSchemaVersionMetadata $
--             newRemoveSchemaVersionMetadata
--
--         , requestBatchGetPartition $
--             newBatchGetPartition
--
--         , requestGetTable $
--             newGetTable
--
--         , requestUpdateCrawlerSchedule $
--             newUpdateCrawlerSchedule
--
--         , requestGetColumnStatisticsForTable $
--             newGetColumnStatisticsForTable
--
--         , requestStopTrigger $
--             newStopTrigger
--
--         , requestListSchemas $
--             newListSchemas
--
--         , requestGetConnection $
--             newGetConnection
--
--         , requestGetDatabases $
--             newGetDatabases
--
--         , requestDeleteSchema $
--             newDeleteSchema
--
--         , requestUpdateSchema $
--             newUpdateSchema
--
--         , requestGetDataflowGraph $
--             newGetDataflowGraph
--
--         , requestBatchGetDevEndpoints $
--             newBatchGetDevEndpoints
--
--         , requestStartExportLabelsTaskRun $
--             newStartExportLabelsTaskRun
--
--         , requestGetTriggers $
--             newGetTriggers
--
--         , requestBatchGetCrawlers $
--             newBatchGetCrawlers
--
--         , requestGetPlan $
--             newGetPlan
--
--         , requestGetCrawlerMetrics $
--             newGetCrawlerMetrics
--
--         , requestGetWorkflowRunProperties $
--             newGetWorkflowRunProperties
--
--         , requestDeletePartitionIndex $
--             newDeletePartitionIndex
--
--         , requestGetJobBookmark $
--             newGetJobBookmark
--
--         , requestDeleteTableVersion $
--             newDeleteTableVersion
--
--         , requestGetTableVersion $
--             newGetTableVersion
--
--         , requestPutWorkflowRunProperties $
--             newPutWorkflowRunProperties
--
--         , requestBatchGetWorkflows $
--             newBatchGetWorkflows
--
--         , requestGetResourcePolicies $
--             newGetResourcePolicies
--
--         , requestGetJobs $
--             newGetJobs
--
--         , requestGetDevEndpoint $
--             newGetDevEndpoint
--
--         , requestGetCrawler $
--             newGetCrawler
--
--         , requestCreateSecurityConfiguration $
--             newCreateSecurityConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseGetDataCatalogEncryptionSettings $
--             newGetDataCatalogEncryptionSettingsResponse
--
--         , responseUpdateColumnStatisticsForTable $
--             newUpdateColumnStatisticsForTableResponse
--
--         , responseStartMLLabelingSetGenerationTaskRun $
--             newStartMLLabelingSetGenerationTaskRunResponse
--
--         , responseDeleteColumnStatisticsForTable $
--             newDeleteColumnStatisticsForTableResponse
--
--         , responseGetSchema $
--             newGetSchemaResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseUpdateConnection $
--             newUpdateConnectionResponse
--
--         , responseCheckSchemaVersionValidity $
--             newCheckSchemaVersionValidityResponse
--
--         , responseCreateWorkflow $
--             newCreateWorkflowResponse
--
--         , responseGetPartitions $
--             newGetPartitionsResponse
--
--         , responseDeleteSecurityConfiguration $
--             newDeleteSecurityConfigurationResponse
--
--         , responseGetPartition $
--             newGetPartitionResponse
--
--         , responseUpdateRegistry $
--             newUpdateRegistryResponse
--
--         , responseListMLTransforms $
--             newListMLTransformsResponse
--
--         , responseStopCrawler $
--             newStopCrawlerResponse
--
--         , responseStartImportLabelsTaskRun $
--             newStartImportLabelsTaskRunResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseQuerySchemaVersionMetadata $
--             newQuerySchemaVersionMetadataResponse
--
--         , responseDeleteRegistry $
--             newDeleteRegistryResponse
--
--         , responseGetPartitionIndexes $
--             newGetPartitionIndexesResponse
--
--         , responseStartCrawler $
--             newStartCrawlerResponse
--
--         , responseGetCatalogImportStatus $
--             newGetCatalogImportStatusResponse
--
--         , responseGetColumnStatisticsForPartition $
--             newGetColumnStatisticsForPartitionResponse
--
--         , responseCreateRegistry $
--             newCreateRegistryResponse
--
--         , responseListTriggers $
--             newListTriggersResponse
--
--         , responseCreateMLTransform $
--             newCreateMLTransformResponse
--
--         , responseStopCrawlerSchedule $
--             newStopCrawlerScheduleResponse
--
--         , responseUpdateTrigger $
--             newUpdateTriggerResponse
--
--         , responseGetSchemaByDefinition $
--             newGetSchemaByDefinitionResponse
--
--         , responseListRegistries $
--             newListRegistriesResponse
--
--         , responseStartCrawlerSchedule $
--             newStartCrawlerScheduleResponse
--
--         , responseDeleteTrigger $
--             newDeleteTriggerResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseUpdateClassifier $
--             newUpdateClassifierResponse
--
--         , responseDeleteClassifier $
--             newDeleteClassifierResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseUpdateJob $
--             newUpdateJobResponse
--
--         , responseCreateUserDefinedFunction $
--             newCreateUserDefinedFunctionResponse
--
--         , responseGetTrigger $
--             newGetTriggerResponse
--
--         , responseBatchGetJobs $
--             newBatchGetJobsResponse
--
--         , responseCreateClassifier $
--             newCreateClassifierResponse
--
--         , responseGetSecurityConfigurations $
--             newGetSecurityConfigurationsResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseUpdatePartition $
--             newUpdatePartitionResponse
--
--         , responseGetSchemaVersionsDiff $
--             newGetSchemaVersionsDiffResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseBatchDeleteTable $
--             newBatchDeleteTableResponse
--
--         , responseStartMLEvaluationTaskRun $
--             newStartMLEvaluationTaskRunResponse
--
--         , responseGetDatabase $
--             newGetDatabaseResponse
--
--         , responseDeletePartition $
--             newDeletePartitionResponse
--
--         , responseGetJobRuns $
--             newGetJobRunsResponse
--
--         , responseGetMLTransforms $
--             newGetMLTransformsResponse
--
--         , responseGetJobRun $
--             newGetJobRunResponse
--
--         , responseCreateDevEndpoint $
--             newCreateDevEndpointResponse
--
--         , responseCreatePartitionIndex $
--             newCreatePartitionIndexResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetSecurityConfiguration $
--             newGetSecurityConfigurationResponse
--
--         , responseCreateCrawler $
--             newCreateCrawlerResponse
--
--         , responseGetMLTaskRuns $
--             newGetMLTaskRunsResponse
--
--         , responseListCrawlers $
--             newListCrawlersResponse
--
--         , responseUpdateDevEndpoint $
--             newUpdateDevEndpointResponse
--
--         , responseCreateSchema $
--             newCreateSchemaResponse
--
--         , responseListDevEndpoints $
--             newListDevEndpointsResponse
--
--         , responseDeleteCrawler $
--             newDeleteCrawlerResponse
--
--         , responseDeleteDevEndpoint $
--             newDeleteDevEndpointResponse
--
--         , responseGetWorkflow $
--             newGetWorkflowResponse
--
--         , responseGetSchemaVersion $
--             newGetSchemaVersionResponse
--
--         , responseUpdateCrawler $
--             newUpdateCrawlerResponse
--
--         , responseDeleteWorkflow $
--             newDeleteWorkflowResponse
--
--         , responseRegisterSchemaVersion $
--             newRegisterSchemaVersionResponse
--
--         , responseGetMapping $
--             newGetMappingResponse
--
--         , responseStopWorkflowRun $
--             newStopWorkflowRunResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseBatchCreatePartition $
--             newBatchCreatePartitionResponse
--
--         , responseCreateTable $
--             newCreateTableResponse
--
--         , responseUpdateWorkflow $
--             newUpdateWorkflowResponse
--
--         , responseGetClassifiers $
--             newGetClassifiersResponse
--
--         , responseBatchStopJobRun $
--             newBatchStopJobRunResponse
--
--         , responseStartWorkflowRun $
--             newStartWorkflowRunResponse
--
--         , responseListWorkflows $
--             newListWorkflowsResponse
--
--         , responseListSchemaVersions $
--             newListSchemaVersionsResponse
--
--         , responseBatchDeletePartition $
--             newBatchDeletePartitionResponse
--
--         , responsePutSchemaVersionMetadata $
--             newPutSchemaVersionMetadataResponse
--
--         , responseGetWorkflowRuns $
--             newGetWorkflowRunsResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseBatchUpdatePartition $
--             newBatchUpdatePartitionResponse
--
--         , responseGetUserDefinedFunctions $
--             newGetUserDefinedFunctionsResponse
--
--         , responseUpdateTable $
--             newUpdateTableResponse
--
--         , responseDeleteTable $
--             newDeleteTableResponse
--
--         , responseDeleteDatabase $
--             newDeleteDatabaseResponse
--
--         , responseUpdateDatabase $
--             newUpdateDatabaseResponse
--
--         , responseGetUserDefinedFunction $
--             newGetUserDefinedFunctionResponse
--
--         , responseUpdateMLTransform $
--             newUpdateMLTransformResponse
--
--         , responseGetWorkflowRun $
--             newGetWorkflowRunResponse
--
--         , responseDeleteMLTransform $
--             newDeleteMLTransformResponse
--
--         , responseCreateTrigger $
--             newCreateTriggerResponse
--
--         , responseCreateDatabase $
--             newCreateDatabaseResponse
--
--         , responseGetClassifier $
--             newGetClassifierResponse
--
--         , responseDeleteSchemaVersions $
--             newDeleteSchemaVersionsResponse
--
--         , responseBatchGetTriggers $
--             newBatchGetTriggersResponse
--
--         , responseBatchDeleteTableVersion $
--             newBatchDeleteTableVersionResponse
--
--         , responseGetTableVersions $
--             newGetTableVersionsResponse
--
--         , responseGetDevEndpoints $
--             newGetDevEndpointsResponse
--
--         , responseGetCrawlers $
--             newGetCrawlersResponse
--
--         , responseStartJobRun $
--             newStartJobRunResponse
--
--         , responseImportCatalogToGlue $
--             newImportCatalogToGlueResponse
--
--         , responseCreatePartition $
--             newCreatePartitionResponse
--
--         , responseResetJobBookmark $
--             newResetJobBookmarkResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseBatchDeleteConnection $
--             newBatchDeleteConnectionResponse
--
--         , responseGetTables $
--             newGetTablesResponse
--
--         , responseDeleteColumnStatisticsForPartition $
--             newDeleteColumnStatisticsForPartitionResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseGetRegistry $
--             newGetRegistryResponse
--
--         , responseResumeWorkflowRun $
--             newResumeWorkflowRunResponse
--
--         , responseCancelMLTaskRun $
--             newCancelMLTaskRunResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseSearchTables $
--             newSearchTablesResponse
--
--         , responseUpdateUserDefinedFunction $
--             newUpdateUserDefinedFunctionResponse
--
--         , responseUpdateColumnStatisticsForPartition $
--             newUpdateColumnStatisticsForPartitionResponse
--
--         , responseGetConnections $
--             newGetConnectionsResponse
--
--         , responseGetMLTransform $
--             newGetMLTransformResponse
--
--         , responseCreateScript $
--             newCreateScriptResponse
--
--         , responseGetMLTaskRun $
--             newGetMLTaskRunResponse
--
--         , responseDeleteUserDefinedFunction $
--             newDeleteUserDefinedFunctionResponse
--
--         , responseStartTrigger $
--             newStartTriggerResponse
--
--         , responsePutDataCatalogEncryptionSettings $
--             newPutDataCatalogEncryptionSettingsResponse
--
--         , responseRemoveSchemaVersionMetadata $
--             newRemoveSchemaVersionMetadataResponse
--
--         , responseBatchGetPartition $
--             newBatchGetPartitionResponse
--
--         , responseGetTable $
--             newGetTableResponse
--
--         , responseUpdateCrawlerSchedule $
--             newUpdateCrawlerScheduleResponse
--
--         , responseGetColumnStatisticsForTable $
--             newGetColumnStatisticsForTableResponse
--
--         , responseStopTrigger $
--             newStopTriggerResponse
--
--         , responseListSchemas $
--             newListSchemasResponse
--
--         , responseGetConnection $
--             newGetConnectionResponse
--
--         , responseGetDatabases $
--             newGetDatabasesResponse
--
--         , responseDeleteSchema $
--             newDeleteSchemaResponse
--
--         , responseUpdateSchema $
--             newUpdateSchemaResponse
--
--         , responseGetDataflowGraph $
--             newGetDataflowGraphResponse
--
--         , responseBatchGetDevEndpoints $
--             newBatchGetDevEndpointsResponse
--
--         , responseStartExportLabelsTaskRun $
--             newStartExportLabelsTaskRunResponse
--
--         , responseGetTriggers $
--             newGetTriggersResponse
--
--         , responseBatchGetCrawlers $
--             newBatchGetCrawlersResponse
--
--         , responseGetPlan $
--             newGetPlanResponse
--
--         , responseGetCrawlerMetrics $
--             newGetCrawlerMetricsResponse
--
--         , responseGetWorkflowRunProperties $
--             newGetWorkflowRunPropertiesResponse
--
--         , responseDeletePartitionIndex $
--             newDeletePartitionIndexResponse
--
--         , responseGetJobBookmark $
--             newGetJobBookmarkResponse
--
--         , responseDeleteTableVersion $
--             newDeleteTableVersionResponse
--
--         , responseGetTableVersion $
--             newGetTableVersionResponse
--
--         , responsePutWorkflowRunProperties $
--             newPutWorkflowRunPropertiesResponse
--
--         , responseBatchGetWorkflows $
--             newBatchGetWorkflowsResponse
--
--         , responseGetResourcePolicies $
--             newGetResourcePoliciesResponse
--
--         , responseGetJobs $
--             newGetJobsResponse
--
--         , responseGetDevEndpoint $
--             newGetDevEndpointResponse
--
--         , responseGetCrawler $
--             newGetCrawlerResponse
--
--         , responseCreateSecurityConfiguration $
--             newCreateSecurityConfigurationResponse
--
--           ]
--     ]

-- Requests

requestGetDataCatalogEncryptionSettings :: GetDataCatalogEncryptionSettings -> TestTree
requestGetDataCatalogEncryptionSettings =
  req
    "GetDataCatalogEncryptionSettings"
    "fixture/GetDataCatalogEncryptionSettings.yaml"

requestUpdateColumnStatisticsForTable :: UpdateColumnStatisticsForTable -> TestTree
requestUpdateColumnStatisticsForTable =
  req
    "UpdateColumnStatisticsForTable"
    "fixture/UpdateColumnStatisticsForTable.yaml"

requestStartMLLabelingSetGenerationTaskRun :: StartMLLabelingSetGenerationTaskRun -> TestTree
requestStartMLLabelingSetGenerationTaskRun =
  req
    "StartMLLabelingSetGenerationTaskRun"
    "fixture/StartMLLabelingSetGenerationTaskRun.yaml"

requestDeleteColumnStatisticsForTable :: DeleteColumnStatisticsForTable -> TestTree
requestDeleteColumnStatisticsForTable =
  req
    "DeleteColumnStatisticsForTable"
    "fixture/DeleteColumnStatisticsForTable.yaml"

requestGetSchema :: GetSchema -> TestTree
requestGetSchema =
  req
    "GetSchema"
    "fixture/GetSchema.yaml"

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

requestCheckSchemaVersionValidity :: CheckSchemaVersionValidity -> TestTree
requestCheckSchemaVersionValidity =
  req
    "CheckSchemaVersionValidity"
    "fixture/CheckSchemaVersionValidity.yaml"

requestCreateWorkflow :: CreateWorkflow -> TestTree
requestCreateWorkflow =
  req
    "CreateWorkflow"
    "fixture/CreateWorkflow.yaml"

requestGetPartitions :: GetPartitions -> TestTree
requestGetPartitions =
  req
    "GetPartitions"
    "fixture/GetPartitions.yaml"

requestDeleteSecurityConfiguration :: DeleteSecurityConfiguration -> TestTree
requestDeleteSecurityConfiguration =
  req
    "DeleteSecurityConfiguration"
    "fixture/DeleteSecurityConfiguration.yaml"

requestGetPartition :: GetPartition -> TestTree
requestGetPartition =
  req
    "GetPartition"
    "fixture/GetPartition.yaml"

requestUpdateRegistry :: UpdateRegistry -> TestTree
requestUpdateRegistry =
  req
    "UpdateRegistry"
    "fixture/UpdateRegistry.yaml"

requestListMLTransforms :: ListMLTransforms -> TestTree
requestListMLTransforms =
  req
    "ListMLTransforms"
    "fixture/ListMLTransforms.yaml"

requestStopCrawler :: StopCrawler -> TestTree
requestStopCrawler =
  req
    "StopCrawler"
    "fixture/StopCrawler.yaml"

requestStartImportLabelsTaskRun :: StartImportLabelsTaskRun -> TestTree
requestStartImportLabelsTaskRun =
  req
    "StartImportLabelsTaskRun"
    "fixture/StartImportLabelsTaskRun.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestQuerySchemaVersionMetadata :: QuerySchemaVersionMetadata -> TestTree
requestQuerySchemaVersionMetadata =
  req
    "QuerySchemaVersionMetadata"
    "fixture/QuerySchemaVersionMetadata.yaml"

requestDeleteRegistry :: DeleteRegistry -> TestTree
requestDeleteRegistry =
  req
    "DeleteRegistry"
    "fixture/DeleteRegistry.yaml"

requestGetPartitionIndexes :: GetPartitionIndexes -> TestTree
requestGetPartitionIndexes =
  req
    "GetPartitionIndexes"
    "fixture/GetPartitionIndexes.yaml"

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

requestGetColumnStatisticsForPartition :: GetColumnStatisticsForPartition -> TestTree
requestGetColumnStatisticsForPartition =
  req
    "GetColumnStatisticsForPartition"
    "fixture/GetColumnStatisticsForPartition.yaml"

requestCreateRegistry :: CreateRegistry -> TestTree
requestCreateRegistry =
  req
    "CreateRegistry"
    "fixture/CreateRegistry.yaml"

requestListTriggers :: ListTriggers -> TestTree
requestListTriggers =
  req
    "ListTriggers"
    "fixture/ListTriggers.yaml"

requestCreateMLTransform :: CreateMLTransform -> TestTree
requestCreateMLTransform =
  req
    "CreateMLTransform"
    "fixture/CreateMLTransform.yaml"

requestStopCrawlerSchedule :: StopCrawlerSchedule -> TestTree
requestStopCrawlerSchedule =
  req
    "StopCrawlerSchedule"
    "fixture/StopCrawlerSchedule.yaml"

requestUpdateTrigger :: UpdateTrigger -> TestTree
requestUpdateTrigger =
  req
    "UpdateTrigger"
    "fixture/UpdateTrigger.yaml"

requestGetSchemaByDefinition :: GetSchemaByDefinition -> TestTree
requestGetSchemaByDefinition =
  req
    "GetSchemaByDefinition"
    "fixture/GetSchemaByDefinition.yaml"

requestListRegistries :: ListRegistries -> TestTree
requestListRegistries =
  req
    "ListRegistries"
    "fixture/ListRegistries.yaml"

requestStartCrawlerSchedule :: StartCrawlerSchedule -> TestTree
requestStartCrawlerSchedule =
  req
    "StartCrawlerSchedule"
    "fixture/StartCrawlerSchedule.yaml"

requestDeleteTrigger :: DeleteTrigger -> TestTree
requestDeleteTrigger =
  req
    "DeleteTrigger"
    "fixture/DeleteTrigger.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestUpdateClassifier :: UpdateClassifier -> TestTree
requestUpdateClassifier =
  req
    "UpdateClassifier"
    "fixture/UpdateClassifier.yaml"

requestDeleteClassifier :: DeleteClassifier -> TestTree
requestDeleteClassifier =
  req
    "DeleteClassifier"
    "fixture/DeleteClassifier.yaml"

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

requestCreateUserDefinedFunction :: CreateUserDefinedFunction -> TestTree
requestCreateUserDefinedFunction =
  req
    "CreateUserDefinedFunction"
    "fixture/CreateUserDefinedFunction.yaml"

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

requestCreateClassifier :: CreateClassifier -> TestTree
requestCreateClassifier =
  req
    "CreateClassifier"
    "fixture/CreateClassifier.yaml"

requestGetSecurityConfigurations :: GetSecurityConfigurations -> TestTree
requestGetSecurityConfigurations =
  req
    "GetSecurityConfigurations"
    "fixture/GetSecurityConfigurations.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestUpdatePartition :: UpdatePartition -> TestTree
requestUpdatePartition =
  req
    "UpdatePartition"
    "fixture/UpdatePartition.yaml"

requestGetSchemaVersionsDiff :: GetSchemaVersionsDiff -> TestTree
requestGetSchemaVersionsDiff =
  req
    "GetSchemaVersionsDiff"
    "fixture/GetSchemaVersionsDiff.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestBatchDeleteTable :: BatchDeleteTable -> TestTree
requestBatchDeleteTable =
  req
    "BatchDeleteTable"
    "fixture/BatchDeleteTable.yaml"

requestStartMLEvaluationTaskRun :: StartMLEvaluationTaskRun -> TestTree
requestStartMLEvaluationTaskRun =
  req
    "StartMLEvaluationTaskRun"
    "fixture/StartMLEvaluationTaskRun.yaml"

requestGetDatabase :: GetDatabase -> TestTree
requestGetDatabase =
  req
    "GetDatabase"
    "fixture/GetDatabase.yaml"

requestDeletePartition :: DeletePartition -> TestTree
requestDeletePartition =
  req
    "DeletePartition"
    "fixture/DeletePartition.yaml"

requestGetJobRuns :: GetJobRuns -> TestTree
requestGetJobRuns =
  req
    "GetJobRuns"
    "fixture/GetJobRuns.yaml"

requestGetMLTransforms :: GetMLTransforms -> TestTree
requestGetMLTransforms =
  req
    "GetMLTransforms"
    "fixture/GetMLTransforms.yaml"

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

requestCreatePartitionIndex :: CreatePartitionIndex -> TestTree
requestCreatePartitionIndex =
  req
    "CreatePartitionIndex"
    "fixture/CreatePartitionIndex.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetSecurityConfiguration :: GetSecurityConfiguration -> TestTree
requestGetSecurityConfiguration =
  req
    "GetSecurityConfiguration"
    "fixture/GetSecurityConfiguration.yaml"

requestCreateCrawler :: CreateCrawler -> TestTree
requestCreateCrawler =
  req
    "CreateCrawler"
    "fixture/CreateCrawler.yaml"

requestGetMLTaskRuns :: GetMLTaskRuns -> TestTree
requestGetMLTaskRuns =
  req
    "GetMLTaskRuns"
    "fixture/GetMLTaskRuns.yaml"

requestListCrawlers :: ListCrawlers -> TestTree
requestListCrawlers =
  req
    "ListCrawlers"
    "fixture/ListCrawlers.yaml"

requestUpdateDevEndpoint :: UpdateDevEndpoint -> TestTree
requestUpdateDevEndpoint =
  req
    "UpdateDevEndpoint"
    "fixture/UpdateDevEndpoint.yaml"

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

requestDeleteCrawler :: DeleteCrawler -> TestTree
requestDeleteCrawler =
  req
    "DeleteCrawler"
    "fixture/DeleteCrawler.yaml"

requestDeleteDevEndpoint :: DeleteDevEndpoint -> TestTree
requestDeleteDevEndpoint =
  req
    "DeleteDevEndpoint"
    "fixture/DeleteDevEndpoint.yaml"

requestGetWorkflow :: GetWorkflow -> TestTree
requestGetWorkflow =
  req
    "GetWorkflow"
    "fixture/GetWorkflow.yaml"

requestGetSchemaVersion :: GetSchemaVersion -> TestTree
requestGetSchemaVersion =
  req
    "GetSchemaVersion"
    "fixture/GetSchemaVersion.yaml"

requestUpdateCrawler :: UpdateCrawler -> TestTree
requestUpdateCrawler =
  req
    "UpdateCrawler"
    "fixture/UpdateCrawler.yaml"

requestDeleteWorkflow :: DeleteWorkflow -> TestTree
requestDeleteWorkflow =
  req
    "DeleteWorkflow"
    "fixture/DeleteWorkflow.yaml"

requestRegisterSchemaVersion :: RegisterSchemaVersion -> TestTree
requestRegisterSchemaVersion =
  req
    "RegisterSchemaVersion"
    "fixture/RegisterSchemaVersion.yaml"

requestGetMapping :: GetMapping -> TestTree
requestGetMapping =
  req
    "GetMapping"
    "fixture/GetMapping.yaml"

requestStopWorkflowRun :: StopWorkflowRun -> TestTree
requestStopWorkflowRun =
  req
    "StopWorkflowRun"
    "fixture/StopWorkflowRun.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestBatchCreatePartition :: BatchCreatePartition -> TestTree
requestBatchCreatePartition =
  req
    "BatchCreatePartition"
    "fixture/BatchCreatePartition.yaml"

requestCreateTable :: CreateTable -> TestTree
requestCreateTable =
  req
    "CreateTable"
    "fixture/CreateTable.yaml"

requestUpdateWorkflow :: UpdateWorkflow -> TestTree
requestUpdateWorkflow =
  req
    "UpdateWorkflow"
    "fixture/UpdateWorkflow.yaml"

requestGetClassifiers :: GetClassifiers -> TestTree
requestGetClassifiers =
  req
    "GetClassifiers"
    "fixture/GetClassifiers.yaml"

requestBatchStopJobRun :: BatchStopJobRun -> TestTree
requestBatchStopJobRun =
  req
    "BatchStopJobRun"
    "fixture/BatchStopJobRun.yaml"

requestStartWorkflowRun :: StartWorkflowRun -> TestTree
requestStartWorkflowRun =
  req
    "StartWorkflowRun"
    "fixture/StartWorkflowRun.yaml"

requestListWorkflows :: ListWorkflows -> TestTree
requestListWorkflows =
  req
    "ListWorkflows"
    "fixture/ListWorkflows.yaml"

requestListSchemaVersions :: ListSchemaVersions -> TestTree
requestListSchemaVersions =
  req
    "ListSchemaVersions"
    "fixture/ListSchemaVersions.yaml"

requestBatchDeletePartition :: BatchDeletePartition -> TestTree
requestBatchDeletePartition =
  req
    "BatchDeletePartition"
    "fixture/BatchDeletePartition.yaml"

requestPutSchemaVersionMetadata :: PutSchemaVersionMetadata -> TestTree
requestPutSchemaVersionMetadata =
  req
    "PutSchemaVersionMetadata"
    "fixture/PutSchemaVersionMetadata.yaml"

requestGetWorkflowRuns :: GetWorkflowRuns -> TestTree
requestGetWorkflowRuns =
  req
    "GetWorkflowRuns"
    "fixture/GetWorkflowRuns.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags =
  req
    "GetTags"
    "fixture/GetTags.yaml"

requestBatchUpdatePartition :: BatchUpdatePartition -> TestTree
requestBatchUpdatePartition =
  req
    "BatchUpdatePartition"
    "fixture/BatchUpdatePartition.yaml"

requestGetUserDefinedFunctions :: GetUserDefinedFunctions -> TestTree
requestGetUserDefinedFunctions =
  req
    "GetUserDefinedFunctions"
    "fixture/GetUserDefinedFunctions.yaml"

requestUpdateTable :: UpdateTable -> TestTree
requestUpdateTable =
  req
    "UpdateTable"
    "fixture/UpdateTable.yaml"

requestDeleteTable :: DeleteTable -> TestTree
requestDeleteTable =
  req
    "DeleteTable"
    "fixture/DeleteTable.yaml"

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

requestGetUserDefinedFunction :: GetUserDefinedFunction -> TestTree
requestGetUserDefinedFunction =
  req
    "GetUserDefinedFunction"
    "fixture/GetUserDefinedFunction.yaml"

requestUpdateMLTransform :: UpdateMLTransform -> TestTree
requestUpdateMLTransform =
  req
    "UpdateMLTransform"
    "fixture/UpdateMLTransform.yaml"

requestGetWorkflowRun :: GetWorkflowRun -> TestTree
requestGetWorkflowRun =
  req
    "GetWorkflowRun"
    "fixture/GetWorkflowRun.yaml"

requestDeleteMLTransform :: DeleteMLTransform -> TestTree
requestDeleteMLTransform =
  req
    "DeleteMLTransform"
    "fixture/DeleteMLTransform.yaml"

requestCreateTrigger :: CreateTrigger -> TestTree
requestCreateTrigger =
  req
    "CreateTrigger"
    "fixture/CreateTrigger.yaml"

requestCreateDatabase :: CreateDatabase -> TestTree
requestCreateDatabase =
  req
    "CreateDatabase"
    "fixture/CreateDatabase.yaml"

requestGetClassifier :: GetClassifier -> TestTree
requestGetClassifier =
  req
    "GetClassifier"
    "fixture/GetClassifier.yaml"

requestDeleteSchemaVersions :: DeleteSchemaVersions -> TestTree
requestDeleteSchemaVersions =
  req
    "DeleteSchemaVersions"
    "fixture/DeleteSchemaVersions.yaml"

requestBatchGetTriggers :: BatchGetTriggers -> TestTree
requestBatchGetTriggers =
  req
    "BatchGetTriggers"
    "fixture/BatchGetTriggers.yaml"

requestBatchDeleteTableVersion :: BatchDeleteTableVersion -> TestTree
requestBatchDeleteTableVersion =
  req
    "BatchDeleteTableVersion"
    "fixture/BatchDeleteTableVersion.yaml"

requestGetTableVersions :: GetTableVersions -> TestTree
requestGetTableVersions =
  req
    "GetTableVersions"
    "fixture/GetTableVersions.yaml"

requestGetDevEndpoints :: GetDevEndpoints -> TestTree
requestGetDevEndpoints =
  req
    "GetDevEndpoints"
    "fixture/GetDevEndpoints.yaml"

requestGetCrawlers :: GetCrawlers -> TestTree
requestGetCrawlers =
  req
    "GetCrawlers"
    "fixture/GetCrawlers.yaml"

requestStartJobRun :: StartJobRun -> TestTree
requestStartJobRun =
  req
    "StartJobRun"
    "fixture/StartJobRun.yaml"

requestImportCatalogToGlue :: ImportCatalogToGlue -> TestTree
requestImportCatalogToGlue =
  req
    "ImportCatalogToGlue"
    "fixture/ImportCatalogToGlue.yaml"

requestCreatePartition :: CreatePartition -> TestTree
requestCreatePartition =
  req
    "CreatePartition"
    "fixture/CreatePartition.yaml"

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

requestBatchDeleteConnection :: BatchDeleteConnection -> TestTree
requestBatchDeleteConnection =
  req
    "BatchDeleteConnection"
    "fixture/BatchDeleteConnection.yaml"

requestGetTables :: GetTables -> TestTree
requestGetTables =
  req
    "GetTables"
    "fixture/GetTables.yaml"

requestDeleteColumnStatisticsForPartition :: DeleteColumnStatisticsForPartition -> TestTree
requestDeleteColumnStatisticsForPartition =
  req
    "DeleteColumnStatisticsForPartition"
    "fixture/DeleteColumnStatisticsForPartition.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestGetRegistry :: GetRegistry -> TestTree
requestGetRegistry =
  req
    "GetRegistry"
    "fixture/GetRegistry.yaml"

requestResumeWorkflowRun :: ResumeWorkflowRun -> TestTree
requestResumeWorkflowRun =
  req
    "ResumeWorkflowRun"
    "fixture/ResumeWorkflowRun.yaml"

requestCancelMLTaskRun :: CancelMLTaskRun -> TestTree
requestCancelMLTaskRun =
  req
    "CancelMLTaskRun"
    "fixture/CancelMLTaskRun.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestSearchTables :: SearchTables -> TestTree
requestSearchTables =
  req
    "SearchTables"
    "fixture/SearchTables.yaml"

requestUpdateUserDefinedFunction :: UpdateUserDefinedFunction -> TestTree
requestUpdateUserDefinedFunction =
  req
    "UpdateUserDefinedFunction"
    "fixture/UpdateUserDefinedFunction.yaml"

requestUpdateColumnStatisticsForPartition :: UpdateColumnStatisticsForPartition -> TestTree
requestUpdateColumnStatisticsForPartition =
  req
    "UpdateColumnStatisticsForPartition"
    "fixture/UpdateColumnStatisticsForPartition.yaml"

requestGetConnections :: GetConnections -> TestTree
requestGetConnections =
  req
    "GetConnections"
    "fixture/GetConnections.yaml"

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

requestGetMLTaskRun :: GetMLTaskRun -> TestTree
requestGetMLTaskRun =
  req
    "GetMLTaskRun"
    "fixture/GetMLTaskRun.yaml"

requestDeleteUserDefinedFunction :: DeleteUserDefinedFunction -> TestTree
requestDeleteUserDefinedFunction =
  req
    "DeleteUserDefinedFunction"
    "fixture/DeleteUserDefinedFunction.yaml"

requestStartTrigger :: StartTrigger -> TestTree
requestStartTrigger =
  req
    "StartTrigger"
    "fixture/StartTrigger.yaml"

requestPutDataCatalogEncryptionSettings :: PutDataCatalogEncryptionSettings -> TestTree
requestPutDataCatalogEncryptionSettings =
  req
    "PutDataCatalogEncryptionSettings"
    "fixture/PutDataCatalogEncryptionSettings.yaml"

requestRemoveSchemaVersionMetadata :: RemoveSchemaVersionMetadata -> TestTree
requestRemoveSchemaVersionMetadata =
  req
    "RemoveSchemaVersionMetadata"
    "fixture/RemoveSchemaVersionMetadata.yaml"

requestBatchGetPartition :: BatchGetPartition -> TestTree
requestBatchGetPartition =
  req
    "BatchGetPartition"
    "fixture/BatchGetPartition.yaml"

requestGetTable :: GetTable -> TestTree
requestGetTable =
  req
    "GetTable"
    "fixture/GetTable.yaml"

requestUpdateCrawlerSchedule :: UpdateCrawlerSchedule -> TestTree
requestUpdateCrawlerSchedule =
  req
    "UpdateCrawlerSchedule"
    "fixture/UpdateCrawlerSchedule.yaml"

requestGetColumnStatisticsForTable :: GetColumnStatisticsForTable -> TestTree
requestGetColumnStatisticsForTable =
  req
    "GetColumnStatisticsForTable"
    "fixture/GetColumnStatisticsForTable.yaml"

requestStopTrigger :: StopTrigger -> TestTree
requestStopTrigger =
  req
    "StopTrigger"
    "fixture/StopTrigger.yaml"

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

requestGetDatabases :: GetDatabases -> TestTree
requestGetDatabases =
  req
    "GetDatabases"
    "fixture/GetDatabases.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema =
  req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestUpdateSchema :: UpdateSchema -> TestTree
requestUpdateSchema =
  req
    "UpdateSchema"
    "fixture/UpdateSchema.yaml"

requestGetDataflowGraph :: GetDataflowGraph -> TestTree
requestGetDataflowGraph =
  req
    "GetDataflowGraph"
    "fixture/GetDataflowGraph.yaml"

requestBatchGetDevEndpoints :: BatchGetDevEndpoints -> TestTree
requestBatchGetDevEndpoints =
  req
    "BatchGetDevEndpoints"
    "fixture/BatchGetDevEndpoints.yaml"

requestStartExportLabelsTaskRun :: StartExportLabelsTaskRun -> TestTree
requestStartExportLabelsTaskRun =
  req
    "StartExportLabelsTaskRun"
    "fixture/StartExportLabelsTaskRun.yaml"

requestGetTriggers :: GetTriggers -> TestTree
requestGetTriggers =
  req
    "GetTriggers"
    "fixture/GetTriggers.yaml"

requestBatchGetCrawlers :: BatchGetCrawlers -> TestTree
requestBatchGetCrawlers =
  req
    "BatchGetCrawlers"
    "fixture/BatchGetCrawlers.yaml"

requestGetPlan :: GetPlan -> TestTree
requestGetPlan =
  req
    "GetPlan"
    "fixture/GetPlan.yaml"

requestGetCrawlerMetrics :: GetCrawlerMetrics -> TestTree
requestGetCrawlerMetrics =
  req
    "GetCrawlerMetrics"
    "fixture/GetCrawlerMetrics.yaml"

requestGetWorkflowRunProperties :: GetWorkflowRunProperties -> TestTree
requestGetWorkflowRunProperties =
  req
    "GetWorkflowRunProperties"
    "fixture/GetWorkflowRunProperties.yaml"

requestDeletePartitionIndex :: DeletePartitionIndex -> TestTree
requestDeletePartitionIndex =
  req
    "DeletePartitionIndex"
    "fixture/DeletePartitionIndex.yaml"

requestGetJobBookmark :: GetJobBookmark -> TestTree
requestGetJobBookmark =
  req
    "GetJobBookmark"
    "fixture/GetJobBookmark.yaml"

requestDeleteTableVersion :: DeleteTableVersion -> TestTree
requestDeleteTableVersion =
  req
    "DeleteTableVersion"
    "fixture/DeleteTableVersion.yaml"

requestGetTableVersion :: GetTableVersion -> TestTree
requestGetTableVersion =
  req
    "GetTableVersion"
    "fixture/GetTableVersion.yaml"

requestPutWorkflowRunProperties :: PutWorkflowRunProperties -> TestTree
requestPutWorkflowRunProperties =
  req
    "PutWorkflowRunProperties"
    "fixture/PutWorkflowRunProperties.yaml"

requestBatchGetWorkflows :: BatchGetWorkflows -> TestTree
requestBatchGetWorkflows =
  req
    "BatchGetWorkflows"
    "fixture/BatchGetWorkflows.yaml"

requestGetResourcePolicies :: GetResourcePolicies -> TestTree
requestGetResourcePolicies =
  req
    "GetResourcePolicies"
    "fixture/GetResourcePolicies.yaml"

requestGetJobs :: GetJobs -> TestTree
requestGetJobs =
  req
    "GetJobs"
    "fixture/GetJobs.yaml"

requestGetDevEndpoint :: GetDevEndpoint -> TestTree
requestGetDevEndpoint =
  req
    "GetDevEndpoint"
    "fixture/GetDevEndpoint.yaml"

requestGetCrawler :: GetCrawler -> TestTree
requestGetCrawler =
  req
    "GetCrawler"
    "fixture/GetCrawler.yaml"

requestCreateSecurityConfiguration :: CreateSecurityConfiguration -> TestTree
requestCreateSecurityConfiguration =
  req
    "CreateSecurityConfiguration"
    "fixture/CreateSecurityConfiguration.yaml"

-- Responses

responseGetDataCatalogEncryptionSettings :: GetDataCatalogEncryptionSettingsResponse -> TestTree
responseGetDataCatalogEncryptionSettings =
  res
    "GetDataCatalogEncryptionSettingsResponse"
    "fixture/GetDataCatalogEncryptionSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDataCatalogEncryptionSettings)

responseUpdateColumnStatisticsForTable :: UpdateColumnStatisticsForTableResponse -> TestTree
responseUpdateColumnStatisticsForTable =
  res
    "UpdateColumnStatisticsForTableResponse"
    "fixture/UpdateColumnStatisticsForTableResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateColumnStatisticsForTable)

responseStartMLLabelingSetGenerationTaskRun :: StartMLLabelingSetGenerationTaskRunResponse -> TestTree
responseStartMLLabelingSetGenerationTaskRun =
  res
    "StartMLLabelingSetGenerationTaskRunResponse"
    "fixture/StartMLLabelingSetGenerationTaskRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartMLLabelingSetGenerationTaskRun)

responseDeleteColumnStatisticsForTable :: DeleteColumnStatisticsForTableResponse -> TestTree
responseDeleteColumnStatisticsForTable =
  res
    "DeleteColumnStatisticsForTableResponse"
    "fixture/DeleteColumnStatisticsForTableResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteColumnStatisticsForTable)

responseGetSchema :: GetSchemaResponse -> TestTree
responseGetSchema =
  res
    "GetSchemaResponse"
    "fixture/GetSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy GetSchema)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnection)

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConnection)

responseCheckSchemaVersionValidity :: CheckSchemaVersionValidityResponse -> TestTree
responseCheckSchemaVersionValidity =
  res
    "CheckSchemaVersionValidityResponse"
    "fixture/CheckSchemaVersionValidityResponse.proto"
    defaultService
    (Proxy :: Proxy CheckSchemaVersionValidity)

responseCreateWorkflow :: CreateWorkflowResponse -> TestTree
responseCreateWorkflow =
  res
    "CreateWorkflowResponse"
    "fixture/CreateWorkflowResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkflow)

responseGetPartitions :: GetPartitionsResponse -> TestTree
responseGetPartitions =
  res
    "GetPartitionsResponse"
    "fixture/GetPartitionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetPartitions)

responseDeleteSecurityConfiguration :: DeleteSecurityConfigurationResponse -> TestTree
responseDeleteSecurityConfiguration =
  res
    "DeleteSecurityConfigurationResponse"
    "fixture/DeleteSecurityConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSecurityConfiguration)

responseGetPartition :: GetPartitionResponse -> TestTree
responseGetPartition =
  res
    "GetPartitionResponse"
    "fixture/GetPartitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetPartition)

responseUpdateRegistry :: UpdateRegistryResponse -> TestTree
responseUpdateRegistry =
  res
    "UpdateRegistryResponse"
    "fixture/UpdateRegistryResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRegistry)

responseListMLTransforms :: ListMLTransformsResponse -> TestTree
responseListMLTransforms =
  res
    "ListMLTransformsResponse"
    "fixture/ListMLTransformsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMLTransforms)

responseStopCrawler :: StopCrawlerResponse -> TestTree
responseStopCrawler =
  res
    "StopCrawlerResponse"
    "fixture/StopCrawlerResponse.proto"
    defaultService
    (Proxy :: Proxy StopCrawler)

responseStartImportLabelsTaskRun :: StartImportLabelsTaskRunResponse -> TestTree
responseStartImportLabelsTaskRun =
  res
    "StartImportLabelsTaskRunResponse"
    "fixture/StartImportLabelsTaskRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartImportLabelsTaskRun)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourcePolicy)

responseQuerySchemaVersionMetadata :: QuerySchemaVersionMetadataResponse -> TestTree
responseQuerySchemaVersionMetadata =
  res
    "QuerySchemaVersionMetadataResponse"
    "fixture/QuerySchemaVersionMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy QuerySchemaVersionMetadata)

responseDeleteRegistry :: DeleteRegistryResponse -> TestTree
responseDeleteRegistry =
  res
    "DeleteRegistryResponse"
    "fixture/DeleteRegistryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRegistry)

responseGetPartitionIndexes :: GetPartitionIndexesResponse -> TestTree
responseGetPartitionIndexes =
  res
    "GetPartitionIndexesResponse"
    "fixture/GetPartitionIndexesResponse.proto"
    defaultService
    (Proxy :: Proxy GetPartitionIndexes)

responseStartCrawler :: StartCrawlerResponse -> TestTree
responseStartCrawler =
  res
    "StartCrawlerResponse"
    "fixture/StartCrawlerResponse.proto"
    defaultService
    (Proxy :: Proxy StartCrawler)

responseGetCatalogImportStatus :: GetCatalogImportStatusResponse -> TestTree
responseGetCatalogImportStatus =
  res
    "GetCatalogImportStatusResponse"
    "fixture/GetCatalogImportStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetCatalogImportStatus)

responseGetColumnStatisticsForPartition :: GetColumnStatisticsForPartitionResponse -> TestTree
responseGetColumnStatisticsForPartition =
  res
    "GetColumnStatisticsForPartitionResponse"
    "fixture/GetColumnStatisticsForPartitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetColumnStatisticsForPartition)

responseCreateRegistry :: CreateRegistryResponse -> TestTree
responseCreateRegistry =
  res
    "CreateRegistryResponse"
    "fixture/CreateRegistryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRegistry)

responseListTriggers :: ListTriggersResponse -> TestTree
responseListTriggers =
  res
    "ListTriggersResponse"
    "fixture/ListTriggersResponse.proto"
    defaultService
    (Proxy :: Proxy ListTriggers)

responseCreateMLTransform :: CreateMLTransformResponse -> TestTree
responseCreateMLTransform =
  res
    "CreateMLTransformResponse"
    "fixture/CreateMLTransformResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMLTransform)

responseStopCrawlerSchedule :: StopCrawlerScheduleResponse -> TestTree
responseStopCrawlerSchedule =
  res
    "StopCrawlerScheduleResponse"
    "fixture/StopCrawlerScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy StopCrawlerSchedule)

responseUpdateTrigger :: UpdateTriggerResponse -> TestTree
responseUpdateTrigger =
  res
    "UpdateTriggerResponse"
    "fixture/UpdateTriggerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTrigger)

responseGetSchemaByDefinition :: GetSchemaByDefinitionResponse -> TestTree
responseGetSchemaByDefinition =
  res
    "GetSchemaByDefinitionResponse"
    "fixture/GetSchemaByDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetSchemaByDefinition)

responseListRegistries :: ListRegistriesResponse -> TestTree
responseListRegistries =
  res
    "ListRegistriesResponse"
    "fixture/ListRegistriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRegistries)

responseStartCrawlerSchedule :: StartCrawlerScheduleResponse -> TestTree
responseStartCrawlerSchedule =
  res
    "StartCrawlerScheduleResponse"
    "fixture/StartCrawlerScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy StartCrawlerSchedule)

responseDeleteTrigger :: DeleteTriggerResponse -> TestTree
responseDeleteTrigger =
  res
    "DeleteTriggerResponse"
    "fixture/DeleteTriggerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrigger)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetJob)

responseUpdateClassifier :: UpdateClassifierResponse -> TestTree
responseUpdateClassifier =
  res
    "UpdateClassifierResponse"
    "fixture/UpdateClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateClassifier)

responseDeleteClassifier :: DeleteClassifierResponse -> TestTree
responseDeleteClassifier =
  res
    "DeleteClassifierResponse"
    "fixture/DeleteClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClassifier)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteJob)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateJob)

responseCreateUserDefinedFunction :: CreateUserDefinedFunctionResponse -> TestTree
responseCreateUserDefinedFunction =
  res
    "CreateUserDefinedFunctionResponse"
    "fixture/CreateUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserDefinedFunction)

responseGetTrigger :: GetTriggerResponse -> TestTree
responseGetTrigger =
  res
    "GetTriggerResponse"
    "fixture/GetTriggerResponse.proto"
    defaultService
    (Proxy :: Proxy GetTrigger)

responseBatchGetJobs :: BatchGetJobsResponse -> TestTree
responseBatchGetJobs =
  res
    "BatchGetJobsResponse"
    "fixture/BatchGetJobsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetJobs)

responseCreateClassifier :: CreateClassifierResponse -> TestTree
responseCreateClassifier =
  res
    "CreateClassifierResponse"
    "fixture/CreateClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy CreateClassifier)

responseGetSecurityConfigurations :: GetSecurityConfigurationsResponse -> TestTree
responseGetSecurityConfigurations =
  res
    "GetSecurityConfigurationsResponse"
    "fixture/GetSecurityConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSecurityConfigurations)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutResourcePolicy)

responseUpdatePartition :: UpdatePartitionResponse -> TestTree
responseUpdatePartition =
  res
    "UpdatePartitionResponse"
    "fixture/UpdatePartitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePartition)

responseGetSchemaVersionsDiff :: GetSchemaVersionsDiffResponse -> TestTree
responseGetSchemaVersionsDiff =
  res
    "GetSchemaVersionsDiffResponse"
    "fixture/GetSchemaVersionsDiffResponse.proto"
    defaultService
    (Proxy :: Proxy GetSchemaVersionsDiff)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseBatchDeleteTable :: BatchDeleteTableResponse -> TestTree
responseBatchDeleteTable =
  res
    "BatchDeleteTableResponse"
    "fixture/BatchDeleteTableResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteTable)

responseStartMLEvaluationTaskRun :: StartMLEvaluationTaskRunResponse -> TestTree
responseStartMLEvaluationTaskRun =
  res
    "StartMLEvaluationTaskRunResponse"
    "fixture/StartMLEvaluationTaskRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartMLEvaluationTaskRun)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy GetDatabase)

responseDeletePartition :: DeletePartitionResponse -> TestTree
responseDeletePartition =
  res
    "DeletePartitionResponse"
    "fixture/DeletePartitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePartition)

responseGetJobRuns :: GetJobRunsResponse -> TestTree
responseGetJobRuns =
  res
    "GetJobRunsResponse"
    "fixture/GetJobRunsResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobRuns)

responseGetMLTransforms :: GetMLTransformsResponse -> TestTree
responseGetMLTransforms =
  res
    "GetMLTransformsResponse"
    "fixture/GetMLTransformsResponse.proto"
    defaultService
    (Proxy :: Proxy GetMLTransforms)

responseGetJobRun :: GetJobRunResponse -> TestTree
responseGetJobRun =
  res
    "GetJobRunResponse"
    "fixture/GetJobRunResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobRun)

responseCreateDevEndpoint :: CreateDevEndpointResponse -> TestTree
responseCreateDevEndpoint =
  res
    "CreateDevEndpointResponse"
    "fixture/CreateDevEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDevEndpoint)

responseCreatePartitionIndex :: CreatePartitionIndexResponse -> TestTree
responseCreatePartitionIndex =
  res
    "CreatePartitionIndexResponse"
    "fixture/CreatePartitionIndexResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePartitionIndex)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetSecurityConfiguration :: GetSecurityConfigurationResponse -> TestTree
responseGetSecurityConfiguration =
  res
    "GetSecurityConfigurationResponse"
    "fixture/GetSecurityConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetSecurityConfiguration)

responseCreateCrawler :: CreateCrawlerResponse -> TestTree
responseCreateCrawler =
  res
    "CreateCrawlerResponse"
    "fixture/CreateCrawlerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCrawler)

responseGetMLTaskRuns :: GetMLTaskRunsResponse -> TestTree
responseGetMLTaskRuns =
  res
    "GetMLTaskRunsResponse"
    "fixture/GetMLTaskRunsResponse.proto"
    defaultService
    (Proxy :: Proxy GetMLTaskRuns)

responseListCrawlers :: ListCrawlersResponse -> TestTree
responseListCrawlers =
  res
    "ListCrawlersResponse"
    "fixture/ListCrawlersResponse.proto"
    defaultService
    (Proxy :: Proxy ListCrawlers)

responseUpdateDevEndpoint :: UpdateDevEndpointResponse -> TestTree
responseUpdateDevEndpoint =
  res
    "UpdateDevEndpointResponse"
    "fixture/UpdateDevEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDevEndpoint)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSchema)

responseListDevEndpoints :: ListDevEndpointsResponse -> TestTree
responseListDevEndpoints =
  res
    "ListDevEndpointsResponse"
    "fixture/ListDevEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevEndpoints)

responseDeleteCrawler :: DeleteCrawlerResponse -> TestTree
responseDeleteCrawler =
  res
    "DeleteCrawlerResponse"
    "fixture/DeleteCrawlerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCrawler)

responseDeleteDevEndpoint :: DeleteDevEndpointResponse -> TestTree
responseDeleteDevEndpoint =
  res
    "DeleteDevEndpointResponse"
    "fixture/DeleteDevEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDevEndpoint)

responseGetWorkflow :: GetWorkflowResponse -> TestTree
responseGetWorkflow =
  res
    "GetWorkflowResponse"
    "fixture/GetWorkflowResponse.proto"
    defaultService
    (Proxy :: Proxy GetWorkflow)

responseGetSchemaVersion :: GetSchemaVersionResponse -> TestTree
responseGetSchemaVersion =
  res
    "GetSchemaVersionResponse"
    "fixture/GetSchemaVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetSchemaVersion)

responseUpdateCrawler :: UpdateCrawlerResponse -> TestTree
responseUpdateCrawler =
  res
    "UpdateCrawlerResponse"
    "fixture/UpdateCrawlerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCrawler)

responseDeleteWorkflow :: DeleteWorkflowResponse -> TestTree
responseDeleteWorkflow =
  res
    "DeleteWorkflowResponse"
    "fixture/DeleteWorkflowResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkflow)

responseRegisterSchemaVersion :: RegisterSchemaVersionResponse -> TestTree
responseRegisterSchemaVersion =
  res
    "RegisterSchemaVersionResponse"
    "fixture/RegisterSchemaVersionResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterSchemaVersion)

responseGetMapping :: GetMappingResponse -> TestTree
responseGetMapping =
  res
    "GetMappingResponse"
    "fixture/GetMappingResponse.proto"
    defaultService
    (Proxy :: Proxy GetMapping)

responseStopWorkflowRun :: StopWorkflowRunResponse -> TestTree
responseStopWorkflowRun =
  res
    "StopWorkflowRunResponse"
    "fixture/StopWorkflowRunResponse.proto"
    defaultService
    (Proxy :: Proxy StopWorkflowRun)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnection)

responseBatchCreatePartition :: BatchCreatePartitionResponse -> TestTree
responseBatchCreatePartition =
  res
    "BatchCreatePartitionResponse"
    "fixture/BatchCreatePartitionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchCreatePartition)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable =
  res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTable)

responseUpdateWorkflow :: UpdateWorkflowResponse -> TestTree
responseUpdateWorkflow =
  res
    "UpdateWorkflowResponse"
    "fixture/UpdateWorkflowResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkflow)

responseGetClassifiers :: GetClassifiersResponse -> TestTree
responseGetClassifiers =
  res
    "GetClassifiersResponse"
    "fixture/GetClassifiersResponse.proto"
    defaultService
    (Proxy :: Proxy GetClassifiers)

responseBatchStopJobRun :: BatchStopJobRunResponse -> TestTree
responseBatchStopJobRun =
  res
    "BatchStopJobRunResponse"
    "fixture/BatchStopJobRunResponse.proto"
    defaultService
    (Proxy :: Proxy BatchStopJobRun)

responseStartWorkflowRun :: StartWorkflowRunResponse -> TestTree
responseStartWorkflowRun =
  res
    "StartWorkflowRunResponse"
    "fixture/StartWorkflowRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartWorkflowRun)

responseListWorkflows :: ListWorkflowsResponse -> TestTree
responseListWorkflows =
  res
    "ListWorkflowsResponse"
    "fixture/ListWorkflowsResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkflows)

responseListSchemaVersions :: ListSchemaVersionsResponse -> TestTree
responseListSchemaVersions =
  res
    "ListSchemaVersionsResponse"
    "fixture/ListSchemaVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSchemaVersions)

responseBatchDeletePartition :: BatchDeletePartitionResponse -> TestTree
responseBatchDeletePartition =
  res
    "BatchDeletePartitionResponse"
    "fixture/BatchDeletePartitionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeletePartition)

responsePutSchemaVersionMetadata :: PutSchemaVersionMetadataResponse -> TestTree
responsePutSchemaVersionMetadata =
  res
    "PutSchemaVersionMetadataResponse"
    "fixture/PutSchemaVersionMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy PutSchemaVersionMetadata)

responseGetWorkflowRuns :: GetWorkflowRunsResponse -> TestTree
responseGetWorkflowRuns =
  res
    "GetWorkflowRunsResponse"
    "fixture/GetWorkflowRunsResponse.proto"
    defaultService
    (Proxy :: Proxy GetWorkflowRuns)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTags)

responseBatchUpdatePartition :: BatchUpdatePartitionResponse -> TestTree
responseBatchUpdatePartition =
  res
    "BatchUpdatePartitionResponse"
    "fixture/BatchUpdatePartitionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchUpdatePartition)

responseGetUserDefinedFunctions :: GetUserDefinedFunctionsResponse -> TestTree
responseGetUserDefinedFunctions =
  res
    "GetUserDefinedFunctionsResponse"
    "fixture/GetUserDefinedFunctionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetUserDefinedFunctions)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable =
  res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTable)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable =
  res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTable)

responseDeleteDatabase :: DeleteDatabaseResponse -> TestTree
responseDeleteDatabase =
  res
    "DeleteDatabaseResponse"
    "fixture/DeleteDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDatabase)

responseUpdateDatabase :: UpdateDatabaseResponse -> TestTree
responseUpdateDatabase =
  res
    "UpdateDatabaseResponse"
    "fixture/UpdateDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDatabase)

responseGetUserDefinedFunction :: GetUserDefinedFunctionResponse -> TestTree
responseGetUserDefinedFunction =
  res
    "GetUserDefinedFunctionResponse"
    "fixture/GetUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy GetUserDefinedFunction)

responseUpdateMLTransform :: UpdateMLTransformResponse -> TestTree
responseUpdateMLTransform =
  res
    "UpdateMLTransformResponse"
    "fixture/UpdateMLTransformResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMLTransform)

responseGetWorkflowRun :: GetWorkflowRunResponse -> TestTree
responseGetWorkflowRun =
  res
    "GetWorkflowRunResponse"
    "fixture/GetWorkflowRunResponse.proto"
    defaultService
    (Proxy :: Proxy GetWorkflowRun)

responseDeleteMLTransform :: DeleteMLTransformResponse -> TestTree
responseDeleteMLTransform =
  res
    "DeleteMLTransformResponse"
    "fixture/DeleteMLTransformResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMLTransform)

responseCreateTrigger :: CreateTriggerResponse -> TestTree
responseCreateTrigger =
  res
    "CreateTriggerResponse"
    "fixture/CreateTriggerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrigger)

responseCreateDatabase :: CreateDatabaseResponse -> TestTree
responseCreateDatabase =
  res
    "CreateDatabaseResponse"
    "fixture/CreateDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDatabase)

responseGetClassifier :: GetClassifierResponse -> TestTree
responseGetClassifier =
  res
    "GetClassifierResponse"
    "fixture/GetClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy GetClassifier)

responseDeleteSchemaVersions :: DeleteSchemaVersionsResponse -> TestTree
responseDeleteSchemaVersions =
  res
    "DeleteSchemaVersionsResponse"
    "fixture/DeleteSchemaVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSchemaVersions)

responseBatchGetTriggers :: BatchGetTriggersResponse -> TestTree
responseBatchGetTriggers =
  res
    "BatchGetTriggersResponse"
    "fixture/BatchGetTriggersResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetTriggers)

responseBatchDeleteTableVersion :: BatchDeleteTableVersionResponse -> TestTree
responseBatchDeleteTableVersion =
  res
    "BatchDeleteTableVersionResponse"
    "fixture/BatchDeleteTableVersionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteTableVersion)

responseGetTableVersions :: GetTableVersionsResponse -> TestTree
responseGetTableVersions =
  res
    "GetTableVersionsResponse"
    "fixture/GetTableVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTableVersions)

responseGetDevEndpoints :: GetDevEndpointsResponse -> TestTree
responseGetDevEndpoints =
  res
    "GetDevEndpointsResponse"
    "fixture/GetDevEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevEndpoints)

responseGetCrawlers :: GetCrawlersResponse -> TestTree
responseGetCrawlers =
  res
    "GetCrawlersResponse"
    "fixture/GetCrawlersResponse.proto"
    defaultService
    (Proxy :: Proxy GetCrawlers)

responseStartJobRun :: StartJobRunResponse -> TestTree
responseStartJobRun =
  res
    "StartJobRunResponse"
    "fixture/StartJobRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartJobRun)

responseImportCatalogToGlue :: ImportCatalogToGlueResponse -> TestTree
responseImportCatalogToGlue =
  res
    "ImportCatalogToGlueResponse"
    "fixture/ImportCatalogToGlueResponse.proto"
    defaultService
    (Proxy :: Proxy ImportCatalogToGlue)

responseCreatePartition :: CreatePartitionResponse -> TestTree
responseCreatePartition =
  res
    "CreatePartitionResponse"
    "fixture/CreatePartitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePartition)

responseResetJobBookmark :: ResetJobBookmarkResponse -> TestTree
responseResetJobBookmark =
  res
    "ResetJobBookmarkResponse"
    "fixture/ResetJobBookmarkResponse.proto"
    defaultService
    (Proxy :: Proxy ResetJobBookmark)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobs)

responseBatchDeleteConnection :: BatchDeleteConnectionResponse -> TestTree
responseBatchDeleteConnection =
  res
    "BatchDeleteConnectionResponse"
    "fixture/BatchDeleteConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteConnection)

responseGetTables :: GetTablesResponse -> TestTree
responseGetTables =
  res
    "GetTablesResponse"
    "fixture/GetTablesResponse.proto"
    defaultService
    (Proxy :: Proxy GetTables)

responseDeleteColumnStatisticsForPartition :: DeleteColumnStatisticsForPartitionResponse -> TestTree
responseDeleteColumnStatisticsForPartition =
  res
    "DeleteColumnStatisticsForPartitionResponse"
    "fixture/DeleteColumnStatisticsForPartitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteColumnStatisticsForPartition)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourcePolicy)

responseGetRegistry :: GetRegistryResponse -> TestTree
responseGetRegistry =
  res
    "GetRegistryResponse"
    "fixture/GetRegistryResponse.proto"
    defaultService
    (Proxy :: Proxy GetRegistry)

responseResumeWorkflowRun :: ResumeWorkflowRunResponse -> TestTree
responseResumeWorkflowRun =
  res
    "ResumeWorkflowRunResponse"
    "fixture/ResumeWorkflowRunResponse.proto"
    defaultService
    (Proxy :: Proxy ResumeWorkflowRun)

responseCancelMLTaskRun :: CancelMLTaskRunResponse -> TestTree
responseCancelMLTaskRun =
  res
    "CancelMLTaskRunResponse"
    "fixture/CancelMLTaskRunResponse.proto"
    defaultService
    (Proxy :: Proxy CancelMLTaskRun)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJob)

responseSearchTables :: SearchTablesResponse -> TestTree
responseSearchTables =
  res
    "SearchTablesResponse"
    "fixture/SearchTablesResponse.proto"
    defaultService
    (Proxy :: Proxy SearchTables)

responseUpdateUserDefinedFunction :: UpdateUserDefinedFunctionResponse -> TestTree
responseUpdateUserDefinedFunction =
  res
    "UpdateUserDefinedFunctionResponse"
    "fixture/UpdateUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserDefinedFunction)

responseUpdateColumnStatisticsForPartition :: UpdateColumnStatisticsForPartitionResponse -> TestTree
responseUpdateColumnStatisticsForPartition =
  res
    "UpdateColumnStatisticsForPartitionResponse"
    "fixture/UpdateColumnStatisticsForPartitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateColumnStatisticsForPartition)

responseGetConnections :: GetConnectionsResponse -> TestTree
responseGetConnections =
  res
    "GetConnectionsResponse"
    "fixture/GetConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnections)

responseGetMLTransform :: GetMLTransformResponse -> TestTree
responseGetMLTransform =
  res
    "GetMLTransformResponse"
    "fixture/GetMLTransformResponse.proto"
    defaultService
    (Proxy :: Proxy GetMLTransform)

responseCreateScript :: CreateScriptResponse -> TestTree
responseCreateScript =
  res
    "CreateScriptResponse"
    "fixture/CreateScriptResponse.proto"
    defaultService
    (Proxy :: Proxy CreateScript)

responseGetMLTaskRun :: GetMLTaskRunResponse -> TestTree
responseGetMLTaskRun =
  res
    "GetMLTaskRunResponse"
    "fixture/GetMLTaskRunResponse.proto"
    defaultService
    (Proxy :: Proxy GetMLTaskRun)

responseDeleteUserDefinedFunction :: DeleteUserDefinedFunctionResponse -> TestTree
responseDeleteUserDefinedFunction =
  res
    "DeleteUserDefinedFunctionResponse"
    "fixture/DeleteUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserDefinedFunction)

responseStartTrigger :: StartTriggerResponse -> TestTree
responseStartTrigger =
  res
    "StartTriggerResponse"
    "fixture/StartTriggerResponse.proto"
    defaultService
    (Proxy :: Proxy StartTrigger)

responsePutDataCatalogEncryptionSettings :: PutDataCatalogEncryptionSettingsResponse -> TestTree
responsePutDataCatalogEncryptionSettings =
  res
    "PutDataCatalogEncryptionSettingsResponse"
    "fixture/PutDataCatalogEncryptionSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy PutDataCatalogEncryptionSettings)

responseRemoveSchemaVersionMetadata :: RemoveSchemaVersionMetadataResponse -> TestTree
responseRemoveSchemaVersionMetadata =
  res
    "RemoveSchemaVersionMetadataResponse"
    "fixture/RemoveSchemaVersionMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveSchemaVersionMetadata)

responseBatchGetPartition :: BatchGetPartitionResponse -> TestTree
responseBatchGetPartition =
  res
    "BatchGetPartitionResponse"
    "fixture/BatchGetPartitionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetPartition)

responseGetTable :: GetTableResponse -> TestTree
responseGetTable =
  res
    "GetTableResponse"
    "fixture/GetTableResponse.proto"
    defaultService
    (Proxy :: Proxy GetTable)

responseUpdateCrawlerSchedule :: UpdateCrawlerScheduleResponse -> TestTree
responseUpdateCrawlerSchedule =
  res
    "UpdateCrawlerScheduleResponse"
    "fixture/UpdateCrawlerScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCrawlerSchedule)

responseGetColumnStatisticsForTable :: GetColumnStatisticsForTableResponse -> TestTree
responseGetColumnStatisticsForTable =
  res
    "GetColumnStatisticsForTableResponse"
    "fixture/GetColumnStatisticsForTableResponse.proto"
    defaultService
    (Proxy :: Proxy GetColumnStatisticsForTable)

responseStopTrigger :: StopTriggerResponse -> TestTree
responseStopTrigger =
  res
    "StopTriggerResponse"
    "fixture/StopTriggerResponse.proto"
    defaultService
    (Proxy :: Proxy StopTrigger)

responseListSchemas :: ListSchemasResponse -> TestTree
responseListSchemas =
  res
    "ListSchemasResponse"
    "fixture/ListSchemasResponse.proto"
    defaultService
    (Proxy :: Proxy ListSchemas)

responseGetConnection :: GetConnectionResponse -> TestTree
responseGetConnection =
  res
    "GetConnectionResponse"
    "fixture/GetConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnection)

responseGetDatabases :: GetDatabasesResponse -> TestTree
responseGetDatabases =
  res
    "GetDatabasesResponse"
    "fixture/GetDatabasesResponse.proto"
    defaultService
    (Proxy :: Proxy GetDatabases)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSchema)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSchema)

responseGetDataflowGraph :: GetDataflowGraphResponse -> TestTree
responseGetDataflowGraph =
  res
    "GetDataflowGraphResponse"
    "fixture/GetDataflowGraphResponse.proto"
    defaultService
    (Proxy :: Proxy GetDataflowGraph)

responseBatchGetDevEndpoints :: BatchGetDevEndpointsResponse -> TestTree
responseBatchGetDevEndpoints =
  res
    "BatchGetDevEndpointsResponse"
    "fixture/BatchGetDevEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetDevEndpoints)

responseStartExportLabelsTaskRun :: StartExportLabelsTaskRunResponse -> TestTree
responseStartExportLabelsTaskRun =
  res
    "StartExportLabelsTaskRunResponse"
    "fixture/StartExportLabelsTaskRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartExportLabelsTaskRun)

responseGetTriggers :: GetTriggersResponse -> TestTree
responseGetTriggers =
  res
    "GetTriggersResponse"
    "fixture/GetTriggersResponse.proto"
    defaultService
    (Proxy :: Proxy GetTriggers)

responseBatchGetCrawlers :: BatchGetCrawlersResponse -> TestTree
responseBatchGetCrawlers =
  res
    "BatchGetCrawlersResponse"
    "fixture/BatchGetCrawlersResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetCrawlers)

responseGetPlan :: GetPlanResponse -> TestTree
responseGetPlan =
  res
    "GetPlanResponse"
    "fixture/GetPlanResponse.proto"
    defaultService
    (Proxy :: Proxy GetPlan)

responseGetCrawlerMetrics :: GetCrawlerMetricsResponse -> TestTree
responseGetCrawlerMetrics =
  res
    "GetCrawlerMetricsResponse"
    "fixture/GetCrawlerMetricsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCrawlerMetrics)

responseGetWorkflowRunProperties :: GetWorkflowRunPropertiesResponse -> TestTree
responseGetWorkflowRunProperties =
  res
    "GetWorkflowRunPropertiesResponse"
    "fixture/GetWorkflowRunPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy GetWorkflowRunProperties)

responseDeletePartitionIndex :: DeletePartitionIndexResponse -> TestTree
responseDeletePartitionIndex =
  res
    "DeletePartitionIndexResponse"
    "fixture/DeletePartitionIndexResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePartitionIndex)

responseGetJobBookmark :: GetJobBookmarkResponse -> TestTree
responseGetJobBookmark =
  res
    "GetJobBookmarkResponse"
    "fixture/GetJobBookmarkResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobBookmark)

responseDeleteTableVersion :: DeleteTableVersionResponse -> TestTree
responseDeleteTableVersion =
  res
    "DeleteTableVersionResponse"
    "fixture/DeleteTableVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTableVersion)

responseGetTableVersion :: GetTableVersionResponse -> TestTree
responseGetTableVersion =
  res
    "GetTableVersionResponse"
    "fixture/GetTableVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetTableVersion)

responsePutWorkflowRunProperties :: PutWorkflowRunPropertiesResponse -> TestTree
responsePutWorkflowRunProperties =
  res
    "PutWorkflowRunPropertiesResponse"
    "fixture/PutWorkflowRunPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy PutWorkflowRunProperties)

responseBatchGetWorkflows :: BatchGetWorkflowsResponse -> TestTree
responseBatchGetWorkflows =
  res
    "BatchGetWorkflowsResponse"
    "fixture/BatchGetWorkflowsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetWorkflows)

responseGetResourcePolicies :: GetResourcePoliciesResponse -> TestTree
responseGetResourcePolicies =
  res
    "GetResourcePoliciesResponse"
    "fixture/GetResourcePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourcePolicies)

responseGetJobs :: GetJobsResponse -> TestTree
responseGetJobs =
  res
    "GetJobsResponse"
    "fixture/GetJobsResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobs)

responseGetDevEndpoint :: GetDevEndpointResponse -> TestTree
responseGetDevEndpoint =
  res
    "GetDevEndpointResponse"
    "fixture/GetDevEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevEndpoint)

responseGetCrawler :: GetCrawlerResponse -> TestTree
responseGetCrawler =
  res
    "GetCrawlerResponse"
    "fixture/GetCrawlerResponse.proto"
    defaultService
    (Proxy :: Proxy GetCrawler)

responseCreateSecurityConfiguration :: CreateSecurityConfigurationResponse -> TestTree
responseCreateSecurityConfiguration =
  res
    "CreateSecurityConfigurationResponse"
    "fixture/CreateSecurityConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSecurityConfiguration)
