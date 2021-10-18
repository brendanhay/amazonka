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
--         [ requestUpdateConnection $
--             newUpdateConnection
--
--         , requestUpdateColumnStatisticsForTable $
--             newUpdateColumnStatisticsForTable
--
--         , requestDeleteSecurityConfiguration $
--             newDeleteSecurityConfiguration
--
--         , requestCreateWorkflow $
--             newCreateWorkflow
--
--         , requestStartMLLabelingSetGenerationTaskRun $
--             newStartMLLabelingSetGenerationTaskRun
--
--         , requestGetPartitions $
--             newGetPartitions
--
--         , requestDeleteColumnStatisticsForTable $
--             newDeleteColumnStatisticsForTable
--
--         , requestGetDataCatalogEncryptionSettings $
--             newGetDataCatalogEncryptionSettings
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestGetSchema $
--             newGetSchema
--
--         , requestCheckSchemaVersionValidity $
--             newCheckSchemaVersionValidity
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestStartImportLabelsTaskRun $
--             newStartImportLabelsTaskRun
--
--         , requestGetPartitionIndexes $
--             newGetPartitionIndexes
--
--         , requestGetCatalogImportStatus $
--             newGetCatalogImportStatus
--
--         , requestStopCrawler $
--             newStopCrawler
--
--         , requestUpdateRegistry $
--             newUpdateRegistry
--
--         , requestQuerySchemaVersionMetadata $
--             newQuerySchemaVersionMetadata
--
--         , requestDeleteRegistry $
--             newDeleteRegistry
--
--         , requestGetPartition $
--             newGetPartition
--
--         , requestListMLTransforms $
--             newListMLTransforms
--
--         , requestStartCrawler $
--             newStartCrawler
--
--         , requestGetColumnStatisticsForPartition $
--             newGetColumnStatisticsForPartition
--
--         , requestListRegistries $
--             newListRegistries
--
--         , requestCreateRegistry $
--             newCreateRegistry
--
--         , requestStartCrawlerSchedule $
--             newStartCrawlerSchedule
--
--         , requestGetJob $
--             newGetJob
--
--         , requestDeleteTrigger $
--             newDeleteTrigger
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
--         , requestListTriggers $
--             newListTriggers
--
--         , requestGetSchemaByDefinition $
--             newGetSchemaByDefinition
--
--         , requestDeleteClassifier $
--             newDeleteClassifier
--
--         , requestStartBlueprintRun $
--             newStartBlueprintRun
--
--         , requestUpdateJob $
--             newUpdateJob
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestUpdateClassifier $
--             newUpdateClassifier
--
--         , requestListBlueprints $
--             newListBlueprints
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
--         , requestGetDatabase $
--             newGetDatabase
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestStartMLEvaluationTaskRun $
--             newStartMLEvaluationTaskRun
--
--         , requestDeletePartition $
--             newDeletePartition
--
--         , requestCreateBlueprint $
--             newCreateBlueprint
--
--         , requestBatchDeleteTable $
--             newBatchDeleteTable
--
--         , requestUpdatePartition $
--             newUpdatePartition
--
--         , requestGetSchemaVersionsDiff $
--             newGetSchemaVersionsDiff
--
--         , requestGetJobRuns $
--             newGetJobRuns
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetSecurityConfigurations $
--             newGetSecurityConfigurations
--
--         , requestCreateClassifier $
--             newCreateClassifier
--
--         , requestCreatePartitionIndex $
--             newCreatePartitionIndex
--
--         , requestGetSecurityConfiguration $
--             newGetSecurityConfiguration
--
--         , requestGetMLTransforms $
--             newGetMLTransforms
--
--         , requestGetJobRun $
--             newGetJobRun
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateCrawler $
--             newCreateCrawler
--
--         , requestCreateDevEndpoint $
--             newCreateDevEndpoint
--
--         , requestGetMLTaskRuns $
--             newGetMLTaskRuns
--
--         , requestDeleteCrawler $
--             newDeleteCrawler
--
--         , requestListDevEndpoints $
--             newListDevEndpoints
--
--         , requestDeleteDevEndpoint $
--             newDeleteDevEndpoint
--
--         , requestUpdateDevEndpoint $
--             newUpdateDevEndpoint
--
--         , requestUpdateCrawler $
--             newUpdateCrawler
--
--         , requestGetSchemaVersion $
--             newGetSchemaVersion
--
--         , requestCreateSchema $
--             newCreateSchema
--
--         , requestGetWorkflow $
--             newGetWorkflow
--
--         , requestListCrawlers $
--             newListCrawlers
--
--         , requestCreateTable $
--             newCreateTable
--
--         , requestGetMapping $
--             newGetMapping
--
--         , requestBatchStopJobRun $
--             newBatchStopJobRun
--
--         , requestStartWorkflowRun $
--             newStartWorkflowRun
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestGetClassifiers $
--             newGetClassifiers
--
--         , requestBatchCreatePartition $
--             newBatchCreatePartition
--
--         , requestListWorkflows $
--             newListWorkflows
--
--         , requestUpdateWorkflow $
--             newUpdateWorkflow
--
--         , requestRegisterSchemaVersion $
--             newRegisterSchemaVersion
--
--         , requestStopWorkflowRun $
--             newStopWorkflowRun
--
--         , requestDeleteWorkflow $
--             newDeleteWorkflow
--
--         , requestListSchemaVersions $
--             newListSchemaVersions
--
--         , requestBatchUpdatePartition $
--             newBatchUpdatePartition
--
--         , requestBatchDeletePartition $
--             newBatchDeletePartition
--
--         , requestGetWorkflowRuns $
--             newGetWorkflowRuns
--
--         , requestGetTags $
--             newGetTags
--
--         , requestUpdateTable $
--             newUpdateTable
--
--         , requestDeleteTable $
--             newDeleteTable
--
--         , requestGetUserDefinedFunctions $
--             newGetUserDefinedFunctions
--
--         , requestPutSchemaVersionMetadata $
--             newPutSchemaVersionMetadata
--
--         , requestUpdateDatabase $
--             newUpdateDatabase
--
--         , requestGetUserDefinedFunction $
--             newGetUserDefinedFunction
--
--         , requestCreateTrigger $
--             newCreateTrigger
--
--         , requestDeleteDatabase $
--             newDeleteDatabase
--
--         , requestUpdateMLTransform $
--             newUpdateMLTransform
--
--         , requestDeleteMLTransform $
--             newDeleteMLTransform
--
--         , requestGetWorkflowRun $
--             newGetWorkflowRun
--
--         , requestGetTableVersions $
--             newGetTableVersions
--
--         , requestDeleteSchemaVersions $
--             newDeleteSchemaVersions
--
--         , requestBatchGetTriggers $
--             newBatchGetTriggers
--
--         , requestGetClassifier $
--             newGetClassifier
--
--         , requestCreateDatabase $
--             newCreateDatabase
--
--         , requestGetCrawlers $
--             newGetCrawlers
--
--         , requestGetBlueprint $
--             newGetBlueprint
--
--         , requestGetDevEndpoints $
--             newGetDevEndpoints
--
--         , requestBatchDeleteTableVersion $
--             newBatchDeleteTableVersion
--
--         , requestDeleteBlueprint $
--             newDeleteBlueprint
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
--         , requestUpdateBlueprint $
--             newUpdateBlueprint
--
--         , requestListJobs $
--             newListJobs
--
--         , requestResetJobBookmark $
--             newResetJobBookmark
--
--         , requestBatchGetBlueprints $
--             newBatchGetBlueprints
--
--         , requestGetMLTransform $
--             newGetMLTransform
--
--         , requestCreateScript $
--             newCreateScript
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestGetRegistry $
--             newGetRegistry
--
--         , requestGetMLTaskRun $
--             newGetMLTaskRun
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestUpdateUserDefinedFunction $
--             newUpdateUserDefinedFunction
--
--         , requestDeleteColumnStatisticsForPartition $
--             newDeleteColumnStatisticsForPartition
--
--         , requestDeleteUserDefinedFunction $
--             newDeleteUserDefinedFunction
--
--         , requestCancelMLTaskRun $
--             newCancelMLTaskRun
--
--         , requestSearchTables $
--             newSearchTables
--
--         , requestUpdateColumnStatisticsForPartition $
--             newUpdateColumnStatisticsForPartition
--
--         , requestGetTables $
--             newGetTables
--
--         , requestBatchDeleteConnection $
--             newBatchDeleteConnection
--
--         , requestGetConnections $
--             newGetConnections
--
--         , requestGetBlueprintRuns $
--             newGetBlueprintRuns
--
--         , requestResumeWorkflowRun $
--             newResumeWorkflowRun
--
--         , requestUpdateCrawlerSchedule $
--             newUpdateCrawlerSchedule
--
--         , requestStartTrigger $
--             newStartTrigger
--
--         , requestGetConnection $
--             newGetConnection
--
--         , requestRemoveSchemaVersionMetadata $
--             newRemoveSchemaVersionMetadata
--
--         , requestGetTable $
--             newGetTable
--
--         , requestDeleteSchema $
--             newDeleteSchema
--
--         , requestPutDataCatalogEncryptionSettings $
--             newPutDataCatalogEncryptionSettings
--
--         , requestGetBlueprintRun $
--             newGetBlueprintRun
--
--         , requestUpdateSchema $
--             newUpdateSchema
--
--         , requestStopTrigger $
--             newStopTrigger
--
--         , requestListSchemas $
--             newListSchemas
--
--         , requestGetDataflowGraph $
--             newGetDataflowGraph
--
--         , requestBatchGetPartition $
--             newBatchGetPartition
--
--         , requestGetColumnStatisticsForTable $
--             newGetColumnStatisticsForTable
--
--         , requestGetDatabases $
--             newGetDatabases
--
--         , requestDeleteTableVersion $
--             newDeleteTableVersion
--
--         , requestGetTriggers $
--             newGetTriggers
--
--         , requestBatchGetCrawlers $
--             newBatchGetCrawlers
--
--         , requestGetJobBookmark $
--             newGetJobBookmark
--
--         , requestStartExportLabelsTaskRun $
--             newStartExportLabelsTaskRun
--
--         , requestGetWorkflowRunProperties $
--             newGetWorkflowRunProperties
--
--         , requestGetCrawlerMetrics $
--             newGetCrawlerMetrics
--
--         , requestDeletePartitionIndex $
--             newDeletePartitionIndex
--
--         , requestBatchGetDevEndpoints $
--             newBatchGetDevEndpoints
--
--         , requestGetPlan $
--             newGetPlan
--
--         , requestCreateSecurityConfiguration $
--             newCreateSecurityConfiguration
--
--         , requestGetResourcePolicies $
--             newGetResourcePolicies
--
--         , requestGetDevEndpoint $
--             newGetDevEndpoint
--
--         , requestPutWorkflowRunProperties $
--             newPutWorkflowRunProperties
--
--         , requestBatchGetWorkflows $
--             newBatchGetWorkflows
--
--         , requestGetTableVersion $
--             newGetTableVersion
--
--         , requestGetJobs $
--             newGetJobs
--
--         , requestGetCrawler $
--             newGetCrawler
--
--           ]

--     , testGroup "response"
--         [ responseUpdateConnection $
--             newUpdateConnectionResponse
--
--         , responseUpdateColumnStatisticsForTable $
--             newUpdateColumnStatisticsForTableResponse
--
--         , responseDeleteSecurityConfiguration $
--             newDeleteSecurityConfigurationResponse
--
--         , responseCreateWorkflow $
--             newCreateWorkflowResponse
--
--         , responseStartMLLabelingSetGenerationTaskRun $
--             newStartMLLabelingSetGenerationTaskRunResponse
--
--         , responseGetPartitions $
--             newGetPartitionsResponse
--
--         , responseDeleteColumnStatisticsForTable $
--             newDeleteColumnStatisticsForTableResponse
--
--         , responseGetDataCatalogEncryptionSettings $
--             newGetDataCatalogEncryptionSettingsResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseGetSchema $
--             newGetSchemaResponse
--
--         , responseCheckSchemaVersionValidity $
--             newCheckSchemaVersionValidityResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseStartImportLabelsTaskRun $
--             newStartImportLabelsTaskRunResponse
--
--         , responseGetPartitionIndexes $
--             newGetPartitionIndexesResponse
--
--         , responseGetCatalogImportStatus $
--             newGetCatalogImportStatusResponse
--
--         , responseStopCrawler $
--             newStopCrawlerResponse
--
--         , responseUpdateRegistry $
--             newUpdateRegistryResponse
--
--         , responseQuerySchemaVersionMetadata $
--             newQuerySchemaVersionMetadataResponse
--
--         , responseDeleteRegistry $
--             newDeleteRegistryResponse
--
--         , responseGetPartition $
--             newGetPartitionResponse
--
--         , responseListMLTransforms $
--             newListMLTransformsResponse
--
--         , responseStartCrawler $
--             newStartCrawlerResponse
--
--         , responseGetColumnStatisticsForPartition $
--             newGetColumnStatisticsForPartitionResponse
--
--         , responseListRegistries $
--             newListRegistriesResponse
--
--         , responseCreateRegistry $
--             newCreateRegistryResponse
--
--         , responseStartCrawlerSchedule $
--             newStartCrawlerScheduleResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseDeleteTrigger $
--             newDeleteTriggerResponse
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
--         , responseListTriggers $
--             newListTriggersResponse
--
--         , responseGetSchemaByDefinition $
--             newGetSchemaByDefinitionResponse
--
--         , responseDeleteClassifier $
--             newDeleteClassifierResponse
--
--         , responseStartBlueprintRun $
--             newStartBlueprintRunResponse
--
--         , responseUpdateJob $
--             newUpdateJobResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseUpdateClassifier $
--             newUpdateClassifierResponse
--
--         , responseListBlueprints $
--             newListBlueprintsResponse
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
--         , responseGetDatabase $
--             newGetDatabaseResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseStartMLEvaluationTaskRun $
--             newStartMLEvaluationTaskRunResponse
--
--         , responseDeletePartition $
--             newDeletePartitionResponse
--
--         , responseCreateBlueprint $
--             newCreateBlueprintResponse
--
--         , responseBatchDeleteTable $
--             newBatchDeleteTableResponse
--
--         , responseUpdatePartition $
--             newUpdatePartitionResponse
--
--         , responseGetSchemaVersionsDiff $
--             newGetSchemaVersionsDiffResponse
--
--         , responseGetJobRuns $
--             newGetJobRunsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetSecurityConfigurations $
--             newGetSecurityConfigurationsResponse
--
--         , responseCreateClassifier $
--             newCreateClassifierResponse
--
--         , responseCreatePartitionIndex $
--             newCreatePartitionIndexResponse
--
--         , responseGetSecurityConfiguration $
--             newGetSecurityConfigurationResponse
--
--         , responseGetMLTransforms $
--             newGetMLTransformsResponse
--
--         , responseGetJobRun $
--             newGetJobRunResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateCrawler $
--             newCreateCrawlerResponse
--
--         , responseCreateDevEndpoint $
--             newCreateDevEndpointResponse
--
--         , responseGetMLTaskRuns $
--             newGetMLTaskRunsResponse
--
--         , responseDeleteCrawler $
--             newDeleteCrawlerResponse
--
--         , responseListDevEndpoints $
--             newListDevEndpointsResponse
--
--         , responseDeleteDevEndpoint $
--             newDeleteDevEndpointResponse
--
--         , responseUpdateDevEndpoint $
--             newUpdateDevEndpointResponse
--
--         , responseUpdateCrawler $
--             newUpdateCrawlerResponse
--
--         , responseGetSchemaVersion $
--             newGetSchemaVersionResponse
--
--         , responseCreateSchema $
--             newCreateSchemaResponse
--
--         , responseGetWorkflow $
--             newGetWorkflowResponse
--
--         , responseListCrawlers $
--             newListCrawlersResponse
--
--         , responseCreateTable $
--             newCreateTableResponse
--
--         , responseGetMapping $
--             newGetMappingResponse
--
--         , responseBatchStopJobRun $
--             newBatchStopJobRunResponse
--
--         , responseStartWorkflowRun $
--             newStartWorkflowRunResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseGetClassifiers $
--             newGetClassifiersResponse
--
--         , responseBatchCreatePartition $
--             newBatchCreatePartitionResponse
--
--         , responseListWorkflows $
--             newListWorkflowsResponse
--
--         , responseUpdateWorkflow $
--             newUpdateWorkflowResponse
--
--         , responseRegisterSchemaVersion $
--             newRegisterSchemaVersionResponse
--
--         , responseStopWorkflowRun $
--             newStopWorkflowRunResponse
--
--         , responseDeleteWorkflow $
--             newDeleteWorkflowResponse
--
--         , responseListSchemaVersions $
--             newListSchemaVersionsResponse
--
--         , responseBatchUpdatePartition $
--             newBatchUpdatePartitionResponse
--
--         , responseBatchDeletePartition $
--             newBatchDeletePartitionResponse
--
--         , responseGetWorkflowRuns $
--             newGetWorkflowRunsResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseUpdateTable $
--             newUpdateTableResponse
--
--         , responseDeleteTable $
--             newDeleteTableResponse
--
--         , responseGetUserDefinedFunctions $
--             newGetUserDefinedFunctionsResponse
--
--         , responsePutSchemaVersionMetadata $
--             newPutSchemaVersionMetadataResponse
--
--         , responseUpdateDatabase $
--             newUpdateDatabaseResponse
--
--         , responseGetUserDefinedFunction $
--             newGetUserDefinedFunctionResponse
--
--         , responseCreateTrigger $
--             newCreateTriggerResponse
--
--         , responseDeleteDatabase $
--             newDeleteDatabaseResponse
--
--         , responseUpdateMLTransform $
--             newUpdateMLTransformResponse
--
--         , responseDeleteMLTransform $
--             newDeleteMLTransformResponse
--
--         , responseGetWorkflowRun $
--             newGetWorkflowRunResponse
--
--         , responseGetTableVersions $
--             newGetTableVersionsResponse
--
--         , responseDeleteSchemaVersions $
--             newDeleteSchemaVersionsResponse
--
--         , responseBatchGetTriggers $
--             newBatchGetTriggersResponse
--
--         , responseGetClassifier $
--             newGetClassifierResponse
--
--         , responseCreateDatabase $
--             newCreateDatabaseResponse
--
--         , responseGetCrawlers $
--             newGetCrawlersResponse
--
--         , responseGetBlueprint $
--             newGetBlueprintResponse
--
--         , responseGetDevEndpoints $
--             newGetDevEndpointsResponse
--
--         , responseBatchDeleteTableVersion $
--             newBatchDeleteTableVersionResponse
--
--         , responseDeleteBlueprint $
--             newDeleteBlueprintResponse
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
--         , responseUpdateBlueprint $
--             newUpdateBlueprintResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseResetJobBookmark $
--             newResetJobBookmarkResponse
--
--         , responseBatchGetBlueprints $
--             newBatchGetBlueprintsResponse
--
--         , responseGetMLTransform $
--             newGetMLTransformResponse
--
--         , responseCreateScript $
--             newCreateScriptResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseGetRegistry $
--             newGetRegistryResponse
--
--         , responseGetMLTaskRun $
--             newGetMLTaskRunResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseUpdateUserDefinedFunction $
--             newUpdateUserDefinedFunctionResponse
--
--         , responseDeleteColumnStatisticsForPartition $
--             newDeleteColumnStatisticsForPartitionResponse
--
--         , responseDeleteUserDefinedFunction $
--             newDeleteUserDefinedFunctionResponse
--
--         , responseCancelMLTaskRun $
--             newCancelMLTaskRunResponse
--
--         , responseSearchTables $
--             newSearchTablesResponse
--
--         , responseUpdateColumnStatisticsForPartition $
--             newUpdateColumnStatisticsForPartitionResponse
--
--         , responseGetTables $
--             newGetTablesResponse
--
--         , responseBatchDeleteConnection $
--             newBatchDeleteConnectionResponse
--
--         , responseGetConnections $
--             newGetConnectionsResponse
--
--         , responseGetBlueprintRuns $
--             newGetBlueprintRunsResponse
--
--         , responseResumeWorkflowRun $
--             newResumeWorkflowRunResponse
--
--         , responseUpdateCrawlerSchedule $
--             newUpdateCrawlerScheduleResponse
--
--         , responseStartTrigger $
--             newStartTriggerResponse
--
--         , responseGetConnection $
--             newGetConnectionResponse
--
--         , responseRemoveSchemaVersionMetadata $
--             newRemoveSchemaVersionMetadataResponse
--
--         , responseGetTable $
--             newGetTableResponse
--
--         , responseDeleteSchema $
--             newDeleteSchemaResponse
--
--         , responsePutDataCatalogEncryptionSettings $
--             newPutDataCatalogEncryptionSettingsResponse
--
--         , responseGetBlueprintRun $
--             newGetBlueprintRunResponse
--
--         , responseUpdateSchema $
--             newUpdateSchemaResponse
--
--         , responseStopTrigger $
--             newStopTriggerResponse
--
--         , responseListSchemas $
--             newListSchemasResponse
--
--         , responseGetDataflowGraph $
--             newGetDataflowGraphResponse
--
--         , responseBatchGetPartition $
--             newBatchGetPartitionResponse
--
--         , responseGetColumnStatisticsForTable $
--             newGetColumnStatisticsForTableResponse
--
--         , responseGetDatabases $
--             newGetDatabasesResponse
--
--         , responseDeleteTableVersion $
--             newDeleteTableVersionResponse
--
--         , responseGetTriggers $
--             newGetTriggersResponse
--
--         , responseBatchGetCrawlers $
--             newBatchGetCrawlersResponse
--
--         , responseGetJobBookmark $
--             newGetJobBookmarkResponse
--
--         , responseStartExportLabelsTaskRun $
--             newStartExportLabelsTaskRunResponse
--
--         , responseGetWorkflowRunProperties $
--             newGetWorkflowRunPropertiesResponse
--
--         , responseGetCrawlerMetrics $
--             newGetCrawlerMetricsResponse
--
--         , responseDeletePartitionIndex $
--             newDeletePartitionIndexResponse
--
--         , responseBatchGetDevEndpoints $
--             newBatchGetDevEndpointsResponse
--
--         , responseGetPlan $
--             newGetPlanResponse
--
--         , responseCreateSecurityConfiguration $
--             newCreateSecurityConfigurationResponse
--
--         , responseGetResourcePolicies $
--             newGetResourcePoliciesResponse
--
--         , responseGetDevEndpoint $
--             newGetDevEndpointResponse
--
--         , responsePutWorkflowRunProperties $
--             newPutWorkflowRunPropertiesResponse
--
--         , responseBatchGetWorkflows $
--             newBatchGetWorkflowsResponse
--
--         , responseGetTableVersion $
--             newGetTableVersionResponse
--
--         , responseGetJobs $
--             newGetJobsResponse
--
--         , responseGetCrawler $
--             newGetCrawlerResponse
--
--           ]
--     ]

-- Requests

requestUpdateConnection :: UpdateConnection -> TestTree
requestUpdateConnection =
  req
    "UpdateConnection"
    "fixture/UpdateConnection.yaml"

requestUpdateColumnStatisticsForTable :: UpdateColumnStatisticsForTable -> TestTree
requestUpdateColumnStatisticsForTable =
  req
    "UpdateColumnStatisticsForTable"
    "fixture/UpdateColumnStatisticsForTable.yaml"

requestDeleteSecurityConfiguration :: DeleteSecurityConfiguration -> TestTree
requestDeleteSecurityConfiguration =
  req
    "DeleteSecurityConfiguration"
    "fixture/DeleteSecurityConfiguration.yaml"

requestCreateWorkflow :: CreateWorkflow -> TestTree
requestCreateWorkflow =
  req
    "CreateWorkflow"
    "fixture/CreateWorkflow.yaml"

requestStartMLLabelingSetGenerationTaskRun :: StartMLLabelingSetGenerationTaskRun -> TestTree
requestStartMLLabelingSetGenerationTaskRun =
  req
    "StartMLLabelingSetGenerationTaskRun"
    "fixture/StartMLLabelingSetGenerationTaskRun.yaml"

requestGetPartitions :: GetPartitions -> TestTree
requestGetPartitions =
  req
    "GetPartitions"
    "fixture/GetPartitions.yaml"

requestDeleteColumnStatisticsForTable :: DeleteColumnStatisticsForTable -> TestTree
requestDeleteColumnStatisticsForTable =
  req
    "DeleteColumnStatisticsForTable"
    "fixture/DeleteColumnStatisticsForTable.yaml"

requestGetDataCatalogEncryptionSettings :: GetDataCatalogEncryptionSettings -> TestTree
requestGetDataCatalogEncryptionSettings =
  req
    "GetDataCatalogEncryptionSettings"
    "fixture/GetDataCatalogEncryptionSettings.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestGetSchema :: GetSchema -> TestTree
requestGetSchema =
  req
    "GetSchema"
    "fixture/GetSchema.yaml"

requestCheckSchemaVersionValidity :: CheckSchemaVersionValidity -> TestTree
requestCheckSchemaVersionValidity =
  req
    "CheckSchemaVersionValidity"
    "fixture/CheckSchemaVersionValidity.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestStartImportLabelsTaskRun :: StartImportLabelsTaskRun -> TestTree
requestStartImportLabelsTaskRun =
  req
    "StartImportLabelsTaskRun"
    "fixture/StartImportLabelsTaskRun.yaml"

requestGetPartitionIndexes :: GetPartitionIndexes -> TestTree
requestGetPartitionIndexes =
  req
    "GetPartitionIndexes"
    "fixture/GetPartitionIndexes.yaml"

requestGetCatalogImportStatus :: GetCatalogImportStatus -> TestTree
requestGetCatalogImportStatus =
  req
    "GetCatalogImportStatus"
    "fixture/GetCatalogImportStatus.yaml"

requestStopCrawler :: StopCrawler -> TestTree
requestStopCrawler =
  req
    "StopCrawler"
    "fixture/StopCrawler.yaml"

requestUpdateRegistry :: UpdateRegistry -> TestTree
requestUpdateRegistry =
  req
    "UpdateRegistry"
    "fixture/UpdateRegistry.yaml"

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

requestGetPartition :: GetPartition -> TestTree
requestGetPartition =
  req
    "GetPartition"
    "fixture/GetPartition.yaml"

requestListMLTransforms :: ListMLTransforms -> TestTree
requestListMLTransforms =
  req
    "ListMLTransforms"
    "fixture/ListMLTransforms.yaml"

requestStartCrawler :: StartCrawler -> TestTree
requestStartCrawler =
  req
    "StartCrawler"
    "fixture/StartCrawler.yaml"

requestGetColumnStatisticsForPartition :: GetColumnStatisticsForPartition -> TestTree
requestGetColumnStatisticsForPartition =
  req
    "GetColumnStatisticsForPartition"
    "fixture/GetColumnStatisticsForPartition.yaml"

requestListRegistries :: ListRegistries -> TestTree
requestListRegistries =
  req
    "ListRegistries"
    "fixture/ListRegistries.yaml"

requestCreateRegistry :: CreateRegistry -> TestTree
requestCreateRegistry =
  req
    "CreateRegistry"
    "fixture/CreateRegistry.yaml"

requestStartCrawlerSchedule :: StartCrawlerSchedule -> TestTree
requestStartCrawlerSchedule =
  req
    "StartCrawlerSchedule"
    "fixture/StartCrawlerSchedule.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestDeleteTrigger :: DeleteTrigger -> TestTree
requestDeleteTrigger =
  req
    "DeleteTrigger"
    "fixture/DeleteTrigger.yaml"

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

requestListTriggers :: ListTriggers -> TestTree
requestListTriggers =
  req
    "ListTriggers"
    "fixture/ListTriggers.yaml"

requestGetSchemaByDefinition :: GetSchemaByDefinition -> TestTree
requestGetSchemaByDefinition =
  req
    "GetSchemaByDefinition"
    "fixture/GetSchemaByDefinition.yaml"

requestDeleteClassifier :: DeleteClassifier -> TestTree
requestDeleteClassifier =
  req
    "DeleteClassifier"
    "fixture/DeleteClassifier.yaml"

requestStartBlueprintRun :: StartBlueprintRun -> TestTree
requestStartBlueprintRun =
  req
    "StartBlueprintRun"
    "fixture/StartBlueprintRun.yaml"

requestUpdateJob :: UpdateJob -> TestTree
requestUpdateJob =
  req
    "UpdateJob"
    "fixture/UpdateJob.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestUpdateClassifier :: UpdateClassifier -> TestTree
requestUpdateClassifier =
  req
    "UpdateClassifier"
    "fixture/UpdateClassifier.yaml"

requestListBlueprints :: ListBlueprints -> TestTree
requestListBlueprints =
  req
    "ListBlueprints"
    "fixture/ListBlueprints.yaml"

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

requestGetDatabase :: GetDatabase -> TestTree
requestGetDatabase =
  req
    "GetDatabase"
    "fixture/GetDatabase.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestStartMLEvaluationTaskRun :: StartMLEvaluationTaskRun -> TestTree
requestStartMLEvaluationTaskRun =
  req
    "StartMLEvaluationTaskRun"
    "fixture/StartMLEvaluationTaskRun.yaml"

requestDeletePartition :: DeletePartition -> TestTree
requestDeletePartition =
  req
    "DeletePartition"
    "fixture/DeletePartition.yaml"

requestCreateBlueprint :: CreateBlueprint -> TestTree
requestCreateBlueprint =
  req
    "CreateBlueprint"
    "fixture/CreateBlueprint.yaml"

requestBatchDeleteTable :: BatchDeleteTable -> TestTree
requestBatchDeleteTable =
  req
    "BatchDeleteTable"
    "fixture/BatchDeleteTable.yaml"

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

requestGetJobRuns :: GetJobRuns -> TestTree
requestGetJobRuns =
  req
    "GetJobRuns"
    "fixture/GetJobRuns.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetSecurityConfigurations :: GetSecurityConfigurations -> TestTree
requestGetSecurityConfigurations =
  req
    "GetSecurityConfigurations"
    "fixture/GetSecurityConfigurations.yaml"

requestCreateClassifier :: CreateClassifier -> TestTree
requestCreateClassifier =
  req
    "CreateClassifier"
    "fixture/CreateClassifier.yaml"

requestCreatePartitionIndex :: CreatePartitionIndex -> TestTree
requestCreatePartitionIndex =
  req
    "CreatePartitionIndex"
    "fixture/CreatePartitionIndex.yaml"

requestGetSecurityConfiguration :: GetSecurityConfiguration -> TestTree
requestGetSecurityConfiguration =
  req
    "GetSecurityConfiguration"
    "fixture/GetSecurityConfiguration.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateCrawler :: CreateCrawler -> TestTree
requestCreateCrawler =
  req
    "CreateCrawler"
    "fixture/CreateCrawler.yaml"

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

requestDeleteCrawler :: DeleteCrawler -> TestTree
requestDeleteCrawler =
  req
    "DeleteCrawler"
    "fixture/DeleteCrawler.yaml"

requestListDevEndpoints :: ListDevEndpoints -> TestTree
requestListDevEndpoints =
  req
    "ListDevEndpoints"
    "fixture/ListDevEndpoints.yaml"

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

requestUpdateCrawler :: UpdateCrawler -> TestTree
requestUpdateCrawler =
  req
    "UpdateCrawler"
    "fixture/UpdateCrawler.yaml"

requestGetSchemaVersion :: GetSchemaVersion -> TestTree
requestGetSchemaVersion =
  req
    "GetSchemaVersion"
    "fixture/GetSchemaVersion.yaml"

requestCreateSchema :: CreateSchema -> TestTree
requestCreateSchema =
  req
    "CreateSchema"
    "fixture/CreateSchema.yaml"

requestGetWorkflow :: GetWorkflow -> TestTree
requestGetWorkflow =
  req
    "GetWorkflow"
    "fixture/GetWorkflow.yaml"

requestListCrawlers :: ListCrawlers -> TestTree
requestListCrawlers =
  req
    "ListCrawlers"
    "fixture/ListCrawlers.yaml"

requestCreateTable :: CreateTable -> TestTree
requestCreateTable =
  req
    "CreateTable"
    "fixture/CreateTable.yaml"

requestGetMapping :: GetMapping -> TestTree
requestGetMapping =
  req
    "GetMapping"
    "fixture/GetMapping.yaml"

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

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestGetClassifiers :: GetClassifiers -> TestTree
requestGetClassifiers =
  req
    "GetClassifiers"
    "fixture/GetClassifiers.yaml"

requestBatchCreatePartition :: BatchCreatePartition -> TestTree
requestBatchCreatePartition =
  req
    "BatchCreatePartition"
    "fixture/BatchCreatePartition.yaml"

requestListWorkflows :: ListWorkflows -> TestTree
requestListWorkflows =
  req
    "ListWorkflows"
    "fixture/ListWorkflows.yaml"

requestUpdateWorkflow :: UpdateWorkflow -> TestTree
requestUpdateWorkflow =
  req
    "UpdateWorkflow"
    "fixture/UpdateWorkflow.yaml"

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

requestDeleteWorkflow :: DeleteWorkflow -> TestTree
requestDeleteWorkflow =
  req
    "DeleteWorkflow"
    "fixture/DeleteWorkflow.yaml"

requestListSchemaVersions :: ListSchemaVersions -> TestTree
requestListSchemaVersions =
  req
    "ListSchemaVersions"
    "fixture/ListSchemaVersions.yaml"

requestBatchUpdatePartition :: BatchUpdatePartition -> TestTree
requestBatchUpdatePartition =
  req
    "BatchUpdatePartition"
    "fixture/BatchUpdatePartition.yaml"

requestBatchDeletePartition :: BatchDeletePartition -> TestTree
requestBatchDeletePartition =
  req
    "BatchDeletePartition"
    "fixture/BatchDeletePartition.yaml"

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

requestGetUserDefinedFunctions :: GetUserDefinedFunctions -> TestTree
requestGetUserDefinedFunctions =
  req
    "GetUserDefinedFunctions"
    "fixture/GetUserDefinedFunctions.yaml"

requestPutSchemaVersionMetadata :: PutSchemaVersionMetadata -> TestTree
requestPutSchemaVersionMetadata =
  req
    "PutSchemaVersionMetadata"
    "fixture/PutSchemaVersionMetadata.yaml"

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

requestCreateTrigger :: CreateTrigger -> TestTree
requestCreateTrigger =
  req
    "CreateTrigger"
    "fixture/CreateTrigger.yaml"

requestDeleteDatabase :: DeleteDatabase -> TestTree
requestDeleteDatabase =
  req
    "DeleteDatabase"
    "fixture/DeleteDatabase.yaml"

requestUpdateMLTransform :: UpdateMLTransform -> TestTree
requestUpdateMLTransform =
  req
    "UpdateMLTransform"
    "fixture/UpdateMLTransform.yaml"

requestDeleteMLTransform :: DeleteMLTransform -> TestTree
requestDeleteMLTransform =
  req
    "DeleteMLTransform"
    "fixture/DeleteMLTransform.yaml"

requestGetWorkflowRun :: GetWorkflowRun -> TestTree
requestGetWorkflowRun =
  req
    "GetWorkflowRun"
    "fixture/GetWorkflowRun.yaml"

requestGetTableVersions :: GetTableVersions -> TestTree
requestGetTableVersions =
  req
    "GetTableVersions"
    "fixture/GetTableVersions.yaml"

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

requestGetClassifier :: GetClassifier -> TestTree
requestGetClassifier =
  req
    "GetClassifier"
    "fixture/GetClassifier.yaml"

requestCreateDatabase :: CreateDatabase -> TestTree
requestCreateDatabase =
  req
    "CreateDatabase"
    "fixture/CreateDatabase.yaml"

requestGetCrawlers :: GetCrawlers -> TestTree
requestGetCrawlers =
  req
    "GetCrawlers"
    "fixture/GetCrawlers.yaml"

requestGetBlueprint :: GetBlueprint -> TestTree
requestGetBlueprint =
  req
    "GetBlueprint"
    "fixture/GetBlueprint.yaml"

requestGetDevEndpoints :: GetDevEndpoints -> TestTree
requestGetDevEndpoints =
  req
    "GetDevEndpoints"
    "fixture/GetDevEndpoints.yaml"

requestBatchDeleteTableVersion :: BatchDeleteTableVersion -> TestTree
requestBatchDeleteTableVersion =
  req
    "BatchDeleteTableVersion"
    "fixture/BatchDeleteTableVersion.yaml"

requestDeleteBlueprint :: DeleteBlueprint -> TestTree
requestDeleteBlueprint =
  req
    "DeleteBlueprint"
    "fixture/DeleteBlueprint.yaml"

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

requestUpdateBlueprint :: UpdateBlueprint -> TestTree
requestUpdateBlueprint =
  req
    "UpdateBlueprint"
    "fixture/UpdateBlueprint.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestResetJobBookmark :: ResetJobBookmark -> TestTree
requestResetJobBookmark =
  req
    "ResetJobBookmark"
    "fixture/ResetJobBookmark.yaml"

requestBatchGetBlueprints :: BatchGetBlueprints -> TestTree
requestBatchGetBlueprints =
  req
    "BatchGetBlueprints"
    "fixture/BatchGetBlueprints.yaml"

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

requestGetMLTaskRun :: GetMLTaskRun -> TestTree
requestGetMLTaskRun =
  req
    "GetMLTaskRun"
    "fixture/GetMLTaskRun.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestUpdateUserDefinedFunction :: UpdateUserDefinedFunction -> TestTree
requestUpdateUserDefinedFunction =
  req
    "UpdateUserDefinedFunction"
    "fixture/UpdateUserDefinedFunction.yaml"

requestDeleteColumnStatisticsForPartition :: DeleteColumnStatisticsForPartition -> TestTree
requestDeleteColumnStatisticsForPartition =
  req
    "DeleteColumnStatisticsForPartition"
    "fixture/DeleteColumnStatisticsForPartition.yaml"

requestDeleteUserDefinedFunction :: DeleteUserDefinedFunction -> TestTree
requestDeleteUserDefinedFunction =
  req
    "DeleteUserDefinedFunction"
    "fixture/DeleteUserDefinedFunction.yaml"

requestCancelMLTaskRun :: CancelMLTaskRun -> TestTree
requestCancelMLTaskRun =
  req
    "CancelMLTaskRun"
    "fixture/CancelMLTaskRun.yaml"

requestSearchTables :: SearchTables -> TestTree
requestSearchTables =
  req
    "SearchTables"
    "fixture/SearchTables.yaml"

requestUpdateColumnStatisticsForPartition :: UpdateColumnStatisticsForPartition -> TestTree
requestUpdateColumnStatisticsForPartition =
  req
    "UpdateColumnStatisticsForPartition"
    "fixture/UpdateColumnStatisticsForPartition.yaml"

requestGetTables :: GetTables -> TestTree
requestGetTables =
  req
    "GetTables"
    "fixture/GetTables.yaml"

requestBatchDeleteConnection :: BatchDeleteConnection -> TestTree
requestBatchDeleteConnection =
  req
    "BatchDeleteConnection"
    "fixture/BatchDeleteConnection.yaml"

requestGetConnections :: GetConnections -> TestTree
requestGetConnections =
  req
    "GetConnections"
    "fixture/GetConnections.yaml"

requestGetBlueprintRuns :: GetBlueprintRuns -> TestTree
requestGetBlueprintRuns =
  req
    "GetBlueprintRuns"
    "fixture/GetBlueprintRuns.yaml"

requestResumeWorkflowRun :: ResumeWorkflowRun -> TestTree
requestResumeWorkflowRun =
  req
    "ResumeWorkflowRun"
    "fixture/ResumeWorkflowRun.yaml"

requestUpdateCrawlerSchedule :: UpdateCrawlerSchedule -> TestTree
requestUpdateCrawlerSchedule =
  req
    "UpdateCrawlerSchedule"
    "fixture/UpdateCrawlerSchedule.yaml"

requestStartTrigger :: StartTrigger -> TestTree
requestStartTrigger =
  req
    "StartTrigger"
    "fixture/StartTrigger.yaml"

requestGetConnection :: GetConnection -> TestTree
requestGetConnection =
  req
    "GetConnection"
    "fixture/GetConnection.yaml"

requestRemoveSchemaVersionMetadata :: RemoveSchemaVersionMetadata -> TestTree
requestRemoveSchemaVersionMetadata =
  req
    "RemoveSchemaVersionMetadata"
    "fixture/RemoveSchemaVersionMetadata.yaml"

requestGetTable :: GetTable -> TestTree
requestGetTable =
  req
    "GetTable"
    "fixture/GetTable.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema =
  req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestPutDataCatalogEncryptionSettings :: PutDataCatalogEncryptionSettings -> TestTree
requestPutDataCatalogEncryptionSettings =
  req
    "PutDataCatalogEncryptionSettings"
    "fixture/PutDataCatalogEncryptionSettings.yaml"

requestGetBlueprintRun :: GetBlueprintRun -> TestTree
requestGetBlueprintRun =
  req
    "GetBlueprintRun"
    "fixture/GetBlueprintRun.yaml"

requestUpdateSchema :: UpdateSchema -> TestTree
requestUpdateSchema =
  req
    "UpdateSchema"
    "fixture/UpdateSchema.yaml"

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

requestGetDataflowGraph :: GetDataflowGraph -> TestTree
requestGetDataflowGraph =
  req
    "GetDataflowGraph"
    "fixture/GetDataflowGraph.yaml"

requestBatchGetPartition :: BatchGetPartition -> TestTree
requestBatchGetPartition =
  req
    "BatchGetPartition"
    "fixture/BatchGetPartition.yaml"

requestGetColumnStatisticsForTable :: GetColumnStatisticsForTable -> TestTree
requestGetColumnStatisticsForTable =
  req
    "GetColumnStatisticsForTable"
    "fixture/GetColumnStatisticsForTable.yaml"

requestGetDatabases :: GetDatabases -> TestTree
requestGetDatabases =
  req
    "GetDatabases"
    "fixture/GetDatabases.yaml"

requestDeleteTableVersion :: DeleteTableVersion -> TestTree
requestDeleteTableVersion =
  req
    "DeleteTableVersion"
    "fixture/DeleteTableVersion.yaml"

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

requestGetJobBookmark :: GetJobBookmark -> TestTree
requestGetJobBookmark =
  req
    "GetJobBookmark"
    "fixture/GetJobBookmark.yaml"

requestStartExportLabelsTaskRun :: StartExportLabelsTaskRun -> TestTree
requestStartExportLabelsTaskRun =
  req
    "StartExportLabelsTaskRun"
    "fixture/StartExportLabelsTaskRun.yaml"

requestGetWorkflowRunProperties :: GetWorkflowRunProperties -> TestTree
requestGetWorkflowRunProperties =
  req
    "GetWorkflowRunProperties"
    "fixture/GetWorkflowRunProperties.yaml"

requestGetCrawlerMetrics :: GetCrawlerMetrics -> TestTree
requestGetCrawlerMetrics =
  req
    "GetCrawlerMetrics"
    "fixture/GetCrawlerMetrics.yaml"

requestDeletePartitionIndex :: DeletePartitionIndex -> TestTree
requestDeletePartitionIndex =
  req
    "DeletePartitionIndex"
    "fixture/DeletePartitionIndex.yaml"

requestBatchGetDevEndpoints :: BatchGetDevEndpoints -> TestTree
requestBatchGetDevEndpoints =
  req
    "BatchGetDevEndpoints"
    "fixture/BatchGetDevEndpoints.yaml"

requestGetPlan :: GetPlan -> TestTree
requestGetPlan =
  req
    "GetPlan"
    "fixture/GetPlan.yaml"

requestCreateSecurityConfiguration :: CreateSecurityConfiguration -> TestTree
requestCreateSecurityConfiguration =
  req
    "CreateSecurityConfiguration"
    "fixture/CreateSecurityConfiguration.yaml"

requestGetResourcePolicies :: GetResourcePolicies -> TestTree
requestGetResourcePolicies =
  req
    "GetResourcePolicies"
    "fixture/GetResourcePolicies.yaml"

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

requestBatchGetWorkflows :: BatchGetWorkflows -> TestTree
requestBatchGetWorkflows =
  req
    "BatchGetWorkflows"
    "fixture/BatchGetWorkflows.yaml"

requestGetTableVersion :: GetTableVersion -> TestTree
requestGetTableVersion =
  req
    "GetTableVersion"
    "fixture/GetTableVersion.yaml"

requestGetJobs :: GetJobs -> TestTree
requestGetJobs =
  req
    "GetJobs"
    "fixture/GetJobs.yaml"

requestGetCrawler :: GetCrawler -> TestTree
requestGetCrawler =
  req
    "GetCrawler"
    "fixture/GetCrawler.yaml"

-- Responses

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConnection)

responseUpdateColumnStatisticsForTable :: UpdateColumnStatisticsForTableResponse -> TestTree
responseUpdateColumnStatisticsForTable =
  res
    "UpdateColumnStatisticsForTableResponse"
    "fixture/UpdateColumnStatisticsForTableResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateColumnStatisticsForTable)

responseDeleteSecurityConfiguration :: DeleteSecurityConfigurationResponse -> TestTree
responseDeleteSecurityConfiguration =
  res
    "DeleteSecurityConfigurationResponse"
    "fixture/DeleteSecurityConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSecurityConfiguration)

responseCreateWorkflow :: CreateWorkflowResponse -> TestTree
responseCreateWorkflow =
  res
    "CreateWorkflowResponse"
    "fixture/CreateWorkflowResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkflow)

responseStartMLLabelingSetGenerationTaskRun :: StartMLLabelingSetGenerationTaskRunResponse -> TestTree
responseStartMLLabelingSetGenerationTaskRun =
  res
    "StartMLLabelingSetGenerationTaskRunResponse"
    "fixture/StartMLLabelingSetGenerationTaskRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartMLLabelingSetGenerationTaskRun)

responseGetPartitions :: GetPartitionsResponse -> TestTree
responseGetPartitions =
  res
    "GetPartitionsResponse"
    "fixture/GetPartitionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetPartitions)

responseDeleteColumnStatisticsForTable :: DeleteColumnStatisticsForTableResponse -> TestTree
responseDeleteColumnStatisticsForTable =
  res
    "DeleteColumnStatisticsForTableResponse"
    "fixture/DeleteColumnStatisticsForTableResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteColumnStatisticsForTable)

responseGetDataCatalogEncryptionSettings :: GetDataCatalogEncryptionSettingsResponse -> TestTree
responseGetDataCatalogEncryptionSettings =
  res
    "GetDataCatalogEncryptionSettingsResponse"
    "fixture/GetDataCatalogEncryptionSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDataCatalogEncryptionSettings)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnection)

responseGetSchema :: GetSchemaResponse -> TestTree
responseGetSchema =
  res
    "GetSchemaResponse"
    "fixture/GetSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy GetSchema)

responseCheckSchemaVersionValidity :: CheckSchemaVersionValidityResponse -> TestTree
responseCheckSchemaVersionValidity =
  res
    "CheckSchemaVersionValidityResponse"
    "fixture/CheckSchemaVersionValidityResponse.proto"
    defaultService
    (Proxy :: Proxy CheckSchemaVersionValidity)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourcePolicy)

responseStartImportLabelsTaskRun :: StartImportLabelsTaskRunResponse -> TestTree
responseStartImportLabelsTaskRun =
  res
    "StartImportLabelsTaskRunResponse"
    "fixture/StartImportLabelsTaskRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartImportLabelsTaskRun)

responseGetPartitionIndexes :: GetPartitionIndexesResponse -> TestTree
responseGetPartitionIndexes =
  res
    "GetPartitionIndexesResponse"
    "fixture/GetPartitionIndexesResponse.proto"
    defaultService
    (Proxy :: Proxy GetPartitionIndexes)

responseGetCatalogImportStatus :: GetCatalogImportStatusResponse -> TestTree
responseGetCatalogImportStatus =
  res
    "GetCatalogImportStatusResponse"
    "fixture/GetCatalogImportStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetCatalogImportStatus)

responseStopCrawler :: StopCrawlerResponse -> TestTree
responseStopCrawler =
  res
    "StopCrawlerResponse"
    "fixture/StopCrawlerResponse.proto"
    defaultService
    (Proxy :: Proxy StopCrawler)

responseUpdateRegistry :: UpdateRegistryResponse -> TestTree
responseUpdateRegistry =
  res
    "UpdateRegistryResponse"
    "fixture/UpdateRegistryResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRegistry)

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

responseGetPartition :: GetPartitionResponse -> TestTree
responseGetPartition =
  res
    "GetPartitionResponse"
    "fixture/GetPartitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetPartition)

responseListMLTransforms :: ListMLTransformsResponse -> TestTree
responseListMLTransforms =
  res
    "ListMLTransformsResponse"
    "fixture/ListMLTransformsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMLTransforms)

responseStartCrawler :: StartCrawlerResponse -> TestTree
responseStartCrawler =
  res
    "StartCrawlerResponse"
    "fixture/StartCrawlerResponse.proto"
    defaultService
    (Proxy :: Proxy StartCrawler)

responseGetColumnStatisticsForPartition :: GetColumnStatisticsForPartitionResponse -> TestTree
responseGetColumnStatisticsForPartition =
  res
    "GetColumnStatisticsForPartitionResponse"
    "fixture/GetColumnStatisticsForPartitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetColumnStatisticsForPartition)

responseListRegistries :: ListRegistriesResponse -> TestTree
responseListRegistries =
  res
    "ListRegistriesResponse"
    "fixture/ListRegistriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRegistries)

responseCreateRegistry :: CreateRegistryResponse -> TestTree
responseCreateRegistry =
  res
    "CreateRegistryResponse"
    "fixture/CreateRegistryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRegistry)

responseStartCrawlerSchedule :: StartCrawlerScheduleResponse -> TestTree
responseStartCrawlerSchedule =
  res
    "StartCrawlerScheduleResponse"
    "fixture/StartCrawlerScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy StartCrawlerSchedule)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetJob)

responseDeleteTrigger :: DeleteTriggerResponse -> TestTree
responseDeleteTrigger =
  res
    "DeleteTriggerResponse"
    "fixture/DeleteTriggerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrigger)

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

responseListTriggers :: ListTriggersResponse -> TestTree
responseListTriggers =
  res
    "ListTriggersResponse"
    "fixture/ListTriggersResponse.proto"
    defaultService
    (Proxy :: Proxy ListTriggers)

responseGetSchemaByDefinition :: GetSchemaByDefinitionResponse -> TestTree
responseGetSchemaByDefinition =
  res
    "GetSchemaByDefinitionResponse"
    "fixture/GetSchemaByDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetSchemaByDefinition)

responseDeleteClassifier :: DeleteClassifierResponse -> TestTree
responseDeleteClassifier =
  res
    "DeleteClassifierResponse"
    "fixture/DeleteClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClassifier)

responseStartBlueprintRun :: StartBlueprintRunResponse -> TestTree
responseStartBlueprintRun =
  res
    "StartBlueprintRunResponse"
    "fixture/StartBlueprintRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartBlueprintRun)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateJob)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteJob)

responseUpdateClassifier :: UpdateClassifierResponse -> TestTree
responseUpdateClassifier =
  res
    "UpdateClassifierResponse"
    "fixture/UpdateClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateClassifier)

responseListBlueprints :: ListBlueprintsResponse -> TestTree
responseListBlueprints =
  res
    "ListBlueprintsResponse"
    "fixture/ListBlueprintsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBlueprints)

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

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy GetDatabase)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutResourcePolicy)

responseStartMLEvaluationTaskRun :: StartMLEvaluationTaskRunResponse -> TestTree
responseStartMLEvaluationTaskRun =
  res
    "StartMLEvaluationTaskRunResponse"
    "fixture/StartMLEvaluationTaskRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartMLEvaluationTaskRun)

responseDeletePartition :: DeletePartitionResponse -> TestTree
responseDeletePartition =
  res
    "DeletePartitionResponse"
    "fixture/DeletePartitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePartition)

responseCreateBlueprint :: CreateBlueprintResponse -> TestTree
responseCreateBlueprint =
  res
    "CreateBlueprintResponse"
    "fixture/CreateBlueprintResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBlueprint)

responseBatchDeleteTable :: BatchDeleteTableResponse -> TestTree
responseBatchDeleteTable =
  res
    "BatchDeleteTableResponse"
    "fixture/BatchDeleteTableResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteTable)

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

responseGetJobRuns :: GetJobRunsResponse -> TestTree
responseGetJobRuns =
  res
    "GetJobRunsResponse"
    "fixture/GetJobRunsResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobRuns)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseGetSecurityConfigurations :: GetSecurityConfigurationsResponse -> TestTree
responseGetSecurityConfigurations =
  res
    "GetSecurityConfigurationsResponse"
    "fixture/GetSecurityConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSecurityConfigurations)

responseCreateClassifier :: CreateClassifierResponse -> TestTree
responseCreateClassifier =
  res
    "CreateClassifierResponse"
    "fixture/CreateClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy CreateClassifier)

responseCreatePartitionIndex :: CreatePartitionIndexResponse -> TestTree
responseCreatePartitionIndex =
  res
    "CreatePartitionIndexResponse"
    "fixture/CreatePartitionIndexResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePartitionIndex)

responseGetSecurityConfiguration :: GetSecurityConfigurationResponse -> TestTree
responseGetSecurityConfiguration =
  res
    "GetSecurityConfigurationResponse"
    "fixture/GetSecurityConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetSecurityConfiguration)

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

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseCreateCrawler :: CreateCrawlerResponse -> TestTree
responseCreateCrawler =
  res
    "CreateCrawlerResponse"
    "fixture/CreateCrawlerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCrawler)

responseCreateDevEndpoint :: CreateDevEndpointResponse -> TestTree
responseCreateDevEndpoint =
  res
    "CreateDevEndpointResponse"
    "fixture/CreateDevEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDevEndpoint)

responseGetMLTaskRuns :: GetMLTaskRunsResponse -> TestTree
responseGetMLTaskRuns =
  res
    "GetMLTaskRunsResponse"
    "fixture/GetMLTaskRunsResponse.proto"
    defaultService
    (Proxy :: Proxy GetMLTaskRuns)

responseDeleteCrawler :: DeleteCrawlerResponse -> TestTree
responseDeleteCrawler =
  res
    "DeleteCrawlerResponse"
    "fixture/DeleteCrawlerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCrawler)

responseListDevEndpoints :: ListDevEndpointsResponse -> TestTree
responseListDevEndpoints =
  res
    "ListDevEndpointsResponse"
    "fixture/ListDevEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevEndpoints)

responseDeleteDevEndpoint :: DeleteDevEndpointResponse -> TestTree
responseDeleteDevEndpoint =
  res
    "DeleteDevEndpointResponse"
    "fixture/DeleteDevEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDevEndpoint)

responseUpdateDevEndpoint :: UpdateDevEndpointResponse -> TestTree
responseUpdateDevEndpoint =
  res
    "UpdateDevEndpointResponse"
    "fixture/UpdateDevEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDevEndpoint)

responseUpdateCrawler :: UpdateCrawlerResponse -> TestTree
responseUpdateCrawler =
  res
    "UpdateCrawlerResponse"
    "fixture/UpdateCrawlerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCrawler)

responseGetSchemaVersion :: GetSchemaVersionResponse -> TestTree
responseGetSchemaVersion =
  res
    "GetSchemaVersionResponse"
    "fixture/GetSchemaVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetSchemaVersion)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSchema)

responseGetWorkflow :: GetWorkflowResponse -> TestTree
responseGetWorkflow =
  res
    "GetWorkflowResponse"
    "fixture/GetWorkflowResponse.proto"
    defaultService
    (Proxy :: Proxy GetWorkflow)

responseListCrawlers :: ListCrawlersResponse -> TestTree
responseListCrawlers =
  res
    "ListCrawlersResponse"
    "fixture/ListCrawlersResponse.proto"
    defaultService
    (Proxy :: Proxy ListCrawlers)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable =
  res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTable)

responseGetMapping :: GetMappingResponse -> TestTree
responseGetMapping =
  res
    "GetMappingResponse"
    "fixture/GetMappingResponse.proto"
    defaultService
    (Proxy :: Proxy GetMapping)

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

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnection)

responseGetClassifiers :: GetClassifiersResponse -> TestTree
responseGetClassifiers =
  res
    "GetClassifiersResponse"
    "fixture/GetClassifiersResponse.proto"
    defaultService
    (Proxy :: Proxy GetClassifiers)

responseBatchCreatePartition :: BatchCreatePartitionResponse -> TestTree
responseBatchCreatePartition =
  res
    "BatchCreatePartitionResponse"
    "fixture/BatchCreatePartitionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchCreatePartition)

responseListWorkflows :: ListWorkflowsResponse -> TestTree
responseListWorkflows =
  res
    "ListWorkflowsResponse"
    "fixture/ListWorkflowsResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkflows)

responseUpdateWorkflow :: UpdateWorkflowResponse -> TestTree
responseUpdateWorkflow =
  res
    "UpdateWorkflowResponse"
    "fixture/UpdateWorkflowResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkflow)

responseRegisterSchemaVersion :: RegisterSchemaVersionResponse -> TestTree
responseRegisterSchemaVersion =
  res
    "RegisterSchemaVersionResponse"
    "fixture/RegisterSchemaVersionResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterSchemaVersion)

responseStopWorkflowRun :: StopWorkflowRunResponse -> TestTree
responseStopWorkflowRun =
  res
    "StopWorkflowRunResponse"
    "fixture/StopWorkflowRunResponse.proto"
    defaultService
    (Proxy :: Proxy StopWorkflowRun)

responseDeleteWorkflow :: DeleteWorkflowResponse -> TestTree
responseDeleteWorkflow =
  res
    "DeleteWorkflowResponse"
    "fixture/DeleteWorkflowResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkflow)

responseListSchemaVersions :: ListSchemaVersionsResponse -> TestTree
responseListSchemaVersions =
  res
    "ListSchemaVersionsResponse"
    "fixture/ListSchemaVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSchemaVersions)

responseBatchUpdatePartition :: BatchUpdatePartitionResponse -> TestTree
responseBatchUpdatePartition =
  res
    "BatchUpdatePartitionResponse"
    "fixture/BatchUpdatePartitionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchUpdatePartition)

responseBatchDeletePartition :: BatchDeletePartitionResponse -> TestTree
responseBatchDeletePartition =
  res
    "BatchDeletePartitionResponse"
    "fixture/BatchDeletePartitionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeletePartition)

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

responseGetUserDefinedFunctions :: GetUserDefinedFunctionsResponse -> TestTree
responseGetUserDefinedFunctions =
  res
    "GetUserDefinedFunctionsResponse"
    "fixture/GetUserDefinedFunctionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetUserDefinedFunctions)

responsePutSchemaVersionMetadata :: PutSchemaVersionMetadataResponse -> TestTree
responsePutSchemaVersionMetadata =
  res
    "PutSchemaVersionMetadataResponse"
    "fixture/PutSchemaVersionMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy PutSchemaVersionMetadata)

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

responseCreateTrigger :: CreateTriggerResponse -> TestTree
responseCreateTrigger =
  res
    "CreateTriggerResponse"
    "fixture/CreateTriggerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrigger)

responseDeleteDatabase :: DeleteDatabaseResponse -> TestTree
responseDeleteDatabase =
  res
    "DeleteDatabaseResponse"
    "fixture/DeleteDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDatabase)

responseUpdateMLTransform :: UpdateMLTransformResponse -> TestTree
responseUpdateMLTransform =
  res
    "UpdateMLTransformResponse"
    "fixture/UpdateMLTransformResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMLTransform)

responseDeleteMLTransform :: DeleteMLTransformResponse -> TestTree
responseDeleteMLTransform =
  res
    "DeleteMLTransformResponse"
    "fixture/DeleteMLTransformResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMLTransform)

responseGetWorkflowRun :: GetWorkflowRunResponse -> TestTree
responseGetWorkflowRun =
  res
    "GetWorkflowRunResponse"
    "fixture/GetWorkflowRunResponse.proto"
    defaultService
    (Proxy :: Proxy GetWorkflowRun)

responseGetTableVersions :: GetTableVersionsResponse -> TestTree
responseGetTableVersions =
  res
    "GetTableVersionsResponse"
    "fixture/GetTableVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTableVersions)

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

responseGetClassifier :: GetClassifierResponse -> TestTree
responseGetClassifier =
  res
    "GetClassifierResponse"
    "fixture/GetClassifierResponse.proto"
    defaultService
    (Proxy :: Proxy GetClassifier)

responseCreateDatabase :: CreateDatabaseResponse -> TestTree
responseCreateDatabase =
  res
    "CreateDatabaseResponse"
    "fixture/CreateDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDatabase)

responseGetCrawlers :: GetCrawlersResponse -> TestTree
responseGetCrawlers =
  res
    "GetCrawlersResponse"
    "fixture/GetCrawlersResponse.proto"
    defaultService
    (Proxy :: Proxy GetCrawlers)

responseGetBlueprint :: GetBlueprintResponse -> TestTree
responseGetBlueprint =
  res
    "GetBlueprintResponse"
    "fixture/GetBlueprintResponse.proto"
    defaultService
    (Proxy :: Proxy GetBlueprint)

responseGetDevEndpoints :: GetDevEndpointsResponse -> TestTree
responseGetDevEndpoints =
  res
    "GetDevEndpointsResponse"
    "fixture/GetDevEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevEndpoints)

responseBatchDeleteTableVersion :: BatchDeleteTableVersionResponse -> TestTree
responseBatchDeleteTableVersion =
  res
    "BatchDeleteTableVersionResponse"
    "fixture/BatchDeleteTableVersionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteTableVersion)

responseDeleteBlueprint :: DeleteBlueprintResponse -> TestTree
responseDeleteBlueprint =
  res
    "DeleteBlueprintResponse"
    "fixture/DeleteBlueprintResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBlueprint)

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

responseUpdateBlueprint :: UpdateBlueprintResponse -> TestTree
responseUpdateBlueprint =
  res
    "UpdateBlueprintResponse"
    "fixture/UpdateBlueprintResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBlueprint)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobs)

responseResetJobBookmark :: ResetJobBookmarkResponse -> TestTree
responseResetJobBookmark =
  res
    "ResetJobBookmarkResponse"
    "fixture/ResetJobBookmarkResponse.proto"
    defaultService
    (Proxy :: Proxy ResetJobBookmark)

responseBatchGetBlueprints :: BatchGetBlueprintsResponse -> TestTree
responseBatchGetBlueprints =
  res
    "BatchGetBlueprintsResponse"
    "fixture/BatchGetBlueprintsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetBlueprints)

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

responseGetMLTaskRun :: GetMLTaskRunResponse -> TestTree
responseGetMLTaskRun =
  res
    "GetMLTaskRunResponse"
    "fixture/GetMLTaskRunResponse.proto"
    defaultService
    (Proxy :: Proxy GetMLTaskRun)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJob)

responseUpdateUserDefinedFunction :: UpdateUserDefinedFunctionResponse -> TestTree
responseUpdateUserDefinedFunction =
  res
    "UpdateUserDefinedFunctionResponse"
    "fixture/UpdateUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserDefinedFunction)

responseDeleteColumnStatisticsForPartition :: DeleteColumnStatisticsForPartitionResponse -> TestTree
responseDeleteColumnStatisticsForPartition =
  res
    "DeleteColumnStatisticsForPartitionResponse"
    "fixture/DeleteColumnStatisticsForPartitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteColumnStatisticsForPartition)

responseDeleteUserDefinedFunction :: DeleteUserDefinedFunctionResponse -> TestTree
responseDeleteUserDefinedFunction =
  res
    "DeleteUserDefinedFunctionResponse"
    "fixture/DeleteUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserDefinedFunction)

responseCancelMLTaskRun :: CancelMLTaskRunResponse -> TestTree
responseCancelMLTaskRun =
  res
    "CancelMLTaskRunResponse"
    "fixture/CancelMLTaskRunResponse.proto"
    defaultService
    (Proxy :: Proxy CancelMLTaskRun)

responseSearchTables :: SearchTablesResponse -> TestTree
responseSearchTables =
  res
    "SearchTablesResponse"
    "fixture/SearchTablesResponse.proto"
    defaultService
    (Proxy :: Proxy SearchTables)

responseUpdateColumnStatisticsForPartition :: UpdateColumnStatisticsForPartitionResponse -> TestTree
responseUpdateColumnStatisticsForPartition =
  res
    "UpdateColumnStatisticsForPartitionResponse"
    "fixture/UpdateColumnStatisticsForPartitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateColumnStatisticsForPartition)

responseGetTables :: GetTablesResponse -> TestTree
responseGetTables =
  res
    "GetTablesResponse"
    "fixture/GetTablesResponse.proto"
    defaultService
    (Proxy :: Proxy GetTables)

responseBatchDeleteConnection :: BatchDeleteConnectionResponse -> TestTree
responseBatchDeleteConnection =
  res
    "BatchDeleteConnectionResponse"
    "fixture/BatchDeleteConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteConnection)

responseGetConnections :: GetConnectionsResponse -> TestTree
responseGetConnections =
  res
    "GetConnectionsResponse"
    "fixture/GetConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnections)

responseGetBlueprintRuns :: GetBlueprintRunsResponse -> TestTree
responseGetBlueprintRuns =
  res
    "GetBlueprintRunsResponse"
    "fixture/GetBlueprintRunsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBlueprintRuns)

responseResumeWorkflowRun :: ResumeWorkflowRunResponse -> TestTree
responseResumeWorkflowRun =
  res
    "ResumeWorkflowRunResponse"
    "fixture/ResumeWorkflowRunResponse.proto"
    defaultService
    (Proxy :: Proxy ResumeWorkflowRun)

responseUpdateCrawlerSchedule :: UpdateCrawlerScheduleResponse -> TestTree
responseUpdateCrawlerSchedule =
  res
    "UpdateCrawlerScheduleResponse"
    "fixture/UpdateCrawlerScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCrawlerSchedule)

responseStartTrigger :: StartTriggerResponse -> TestTree
responseStartTrigger =
  res
    "StartTriggerResponse"
    "fixture/StartTriggerResponse.proto"
    defaultService
    (Proxy :: Proxy StartTrigger)

responseGetConnection :: GetConnectionResponse -> TestTree
responseGetConnection =
  res
    "GetConnectionResponse"
    "fixture/GetConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnection)

responseRemoveSchemaVersionMetadata :: RemoveSchemaVersionMetadataResponse -> TestTree
responseRemoveSchemaVersionMetadata =
  res
    "RemoveSchemaVersionMetadataResponse"
    "fixture/RemoveSchemaVersionMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveSchemaVersionMetadata)

responseGetTable :: GetTableResponse -> TestTree
responseGetTable =
  res
    "GetTableResponse"
    "fixture/GetTableResponse.proto"
    defaultService
    (Proxy :: Proxy GetTable)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSchema)

responsePutDataCatalogEncryptionSettings :: PutDataCatalogEncryptionSettingsResponse -> TestTree
responsePutDataCatalogEncryptionSettings =
  res
    "PutDataCatalogEncryptionSettingsResponse"
    "fixture/PutDataCatalogEncryptionSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy PutDataCatalogEncryptionSettings)

responseGetBlueprintRun :: GetBlueprintRunResponse -> TestTree
responseGetBlueprintRun =
  res
    "GetBlueprintRunResponse"
    "fixture/GetBlueprintRunResponse.proto"
    defaultService
    (Proxy :: Proxy GetBlueprintRun)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSchema)

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

responseGetDataflowGraph :: GetDataflowGraphResponse -> TestTree
responseGetDataflowGraph =
  res
    "GetDataflowGraphResponse"
    "fixture/GetDataflowGraphResponse.proto"
    defaultService
    (Proxy :: Proxy GetDataflowGraph)

responseBatchGetPartition :: BatchGetPartitionResponse -> TestTree
responseBatchGetPartition =
  res
    "BatchGetPartitionResponse"
    "fixture/BatchGetPartitionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetPartition)

responseGetColumnStatisticsForTable :: GetColumnStatisticsForTableResponse -> TestTree
responseGetColumnStatisticsForTable =
  res
    "GetColumnStatisticsForTableResponse"
    "fixture/GetColumnStatisticsForTableResponse.proto"
    defaultService
    (Proxy :: Proxy GetColumnStatisticsForTable)

responseGetDatabases :: GetDatabasesResponse -> TestTree
responseGetDatabases =
  res
    "GetDatabasesResponse"
    "fixture/GetDatabasesResponse.proto"
    defaultService
    (Proxy :: Proxy GetDatabases)

responseDeleteTableVersion :: DeleteTableVersionResponse -> TestTree
responseDeleteTableVersion =
  res
    "DeleteTableVersionResponse"
    "fixture/DeleteTableVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTableVersion)

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

responseGetJobBookmark :: GetJobBookmarkResponse -> TestTree
responseGetJobBookmark =
  res
    "GetJobBookmarkResponse"
    "fixture/GetJobBookmarkResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobBookmark)

responseStartExportLabelsTaskRun :: StartExportLabelsTaskRunResponse -> TestTree
responseStartExportLabelsTaskRun =
  res
    "StartExportLabelsTaskRunResponse"
    "fixture/StartExportLabelsTaskRunResponse.proto"
    defaultService
    (Proxy :: Proxy StartExportLabelsTaskRun)

responseGetWorkflowRunProperties :: GetWorkflowRunPropertiesResponse -> TestTree
responseGetWorkflowRunProperties =
  res
    "GetWorkflowRunPropertiesResponse"
    "fixture/GetWorkflowRunPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy GetWorkflowRunProperties)

responseGetCrawlerMetrics :: GetCrawlerMetricsResponse -> TestTree
responseGetCrawlerMetrics =
  res
    "GetCrawlerMetricsResponse"
    "fixture/GetCrawlerMetricsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCrawlerMetrics)

responseDeletePartitionIndex :: DeletePartitionIndexResponse -> TestTree
responseDeletePartitionIndex =
  res
    "DeletePartitionIndexResponse"
    "fixture/DeletePartitionIndexResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePartitionIndex)

responseBatchGetDevEndpoints :: BatchGetDevEndpointsResponse -> TestTree
responseBatchGetDevEndpoints =
  res
    "BatchGetDevEndpointsResponse"
    "fixture/BatchGetDevEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetDevEndpoints)

responseGetPlan :: GetPlanResponse -> TestTree
responseGetPlan =
  res
    "GetPlanResponse"
    "fixture/GetPlanResponse.proto"
    defaultService
    (Proxy :: Proxy GetPlan)

responseCreateSecurityConfiguration :: CreateSecurityConfigurationResponse -> TestTree
responseCreateSecurityConfiguration =
  res
    "CreateSecurityConfigurationResponse"
    "fixture/CreateSecurityConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSecurityConfiguration)

responseGetResourcePolicies :: GetResourcePoliciesResponse -> TestTree
responseGetResourcePolicies =
  res
    "GetResourcePoliciesResponse"
    "fixture/GetResourcePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourcePolicies)

responseGetDevEndpoint :: GetDevEndpointResponse -> TestTree
responseGetDevEndpoint =
  res
    "GetDevEndpointResponse"
    "fixture/GetDevEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevEndpoint)

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

responseGetTableVersion :: GetTableVersionResponse -> TestTree
responseGetTableVersion =
  res
    "GetTableVersionResponse"
    "fixture/GetTableVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetTableVersion)

responseGetJobs :: GetJobsResponse -> TestTree
responseGetJobs =
  res
    "GetJobsResponse"
    "fixture/GetJobsResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobs)

responseGetCrawler :: GetCrawlerResponse -> TestTree
responseGetCrawler =
  res
    "GetCrawlerResponse"
    "fixture/GetCrawlerResponse.proto"
    defaultService
    (Proxy :: Proxy GetCrawler)
