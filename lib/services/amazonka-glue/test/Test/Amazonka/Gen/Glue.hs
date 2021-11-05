{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Glue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Glue where

import Amazonka.Glue
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Glue.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStartImportLabelsTaskRun $
--             newStartImportLabelsTaskRun
--
--         , requestUpdateMLTransform $
--             newUpdateMLTransform
--
--         , requestUpdateRegistry $
--             newUpdateRegistry
--
--         , requestDeleteRegistry $
--             newDeleteRegistry
--
--         , requestDeleteMLTransform $
--             newDeleteMLTransform
--
--         , requestStartCrawler $
--             newStartCrawler
--
--         , requestGetCatalogImportStatus $
--             newGetCatalogImportStatus
--
--         , requestListMLTransforms $
--             newListMLTransforms
--
--         , requestGetPartition $
--             newGetPartition
--
--         , requestQuerySchemaVersionMetadata $
--             newQuerySchemaVersionMetadata
--
--         , requestCreateTrigger $
--             newCreateTrigger
--
--         , requestCheckSchemaVersionValidity $
--             newCheckSchemaVersionValidity
--
--         , requestDeleteTable $
--             newDeleteTable
--
--         , requestUpdateTable $
--             newUpdateTable
--
--         , requestGetWorkflowRuns $
--             newGetWorkflowRuns
--
--         , requestCreateWorkflow $
--             newCreateWorkflow
--
--         , requestUpdateColumnStatisticsForTable $
--             newUpdateColumnStatisticsForTable
--
--         , requestDeleteColumnStatisticsForTable $
--             newDeleteColumnStatisticsForTable
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestUpdateConnection $
--             newUpdateConnection
--
--         , requestGetUserDefinedFunctions $
--             newGetUserDefinedFunctions
--
--         , requestGetTags $
--             newGetTags
--
--         , requestGetDataCatalogEncryptionSettings $
--             newGetDataCatalogEncryptionSettings
--
--         , requestBatchCreatePartition $
--             newBatchCreatePartition
--
--         , requestGetMapping $
--             newGetMapping
--
--         , requestDeleteWorkflow $
--             newDeleteWorkflow
--
--         , requestUpdateWorkflow $
--             newUpdateWorkflow
--
--         , requestGetTableVersion $
--             newGetTableVersion
--
--         , requestCreateSecurityConfiguration $
--             newCreateSecurityConfiguration
--
--         , requestStartWorkflowRun $
--             newStartWorkflowRun
--
--         , requestGetJobs $
--             newGetJobs
--
--         , requestBatchGetWorkflows $
--             newBatchGetWorkflows
--
--         , requestGetClassifiers $
--             newGetClassifiers
--
--         , requestGetResourcePolicies $
--             newGetResourcePolicies
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestListSchemaVersions $
--             newListSchemaVersions
--
--         , requestGetWorkflowRunProperties $
--             newGetWorkflowRunProperties
--
--         , requestBatchGetDevEndpoints $
--             newBatchGetDevEndpoints
--
--         , requestDeletePartitionIndex $
--             newDeletePartitionIndex
--
--         , requestDeleteTableVersion $
--             newDeleteTableVersion
--
--         , requestDeleteDevEndpoint $
--             newDeleteDevEndpoint
--
--         , requestUpdateDevEndpoint $
--             newUpdateDevEndpoint
--
--         , requestGetWorkflow $
--             newGetWorkflow
--
--         , requestBatchGetCrawlers $
--             newBatchGetCrawlers
--
--         , requestGetJobBookmark $
--             newGetJobBookmark
--
--         , requestDeleteCrawler $
--             newDeleteCrawler
--
--         , requestUpdateCrawler $
--             newUpdateCrawler
--
--         , requestStartExportLabelsTaskRun $
--             newStartExportLabelsTaskRun
--
--         , requestGetSecurityConfiguration $
--             newGetSecurityConfiguration
--
--         , requestCreatePartitionIndex $
--             newCreatePartitionIndex
--
--         , requestGetBlueprintRun $
--             newGetBlueprintRun
--
--         , requestRemoveSchemaVersionMetadata $
--             newRemoveSchemaVersionMetadata
--
--         , requestListSchemas $
--             newListSchemas
--
--         , requestGetConnection $
--             newGetConnection
--
--         , requestGetColumnStatisticsForTable $
--             newGetColumnStatisticsForTable
--
--         , requestBatchGetPartition $
--             newBatchGetPartition
--
--         , requestStopTrigger $
--             newStopTrigger
--
--         , requestUpdateCrawlerSchedule $
--             newUpdateCrawlerSchedule
--
--         , requestStartMLEvaluationTaskRun $
--             newStartMLEvaluationTaskRun
--
--         , requestDeleteUserDefinedFunction $
--             newDeleteUserDefinedFunction
--
--         , requestUpdateUserDefinedFunction $
--             newUpdateUserDefinedFunction
--
--         , requestGetRegistry $
--             newGetRegistry
--
--         , requestBatchDeleteTable $
--             newBatchDeleteTable
--
--         , requestCancelMLTaskRun $
--             newCancelMLTaskRun
--
--         , requestGetTables $
--             newGetTables
--
--         , requestResumeWorkflowRun $
--             newResumeWorkflowRun
--
--         , requestCreateClassifier $
--             newCreateClassifier
--
--         , requestBatchDeleteConnection $
--             newBatchDeleteConnection
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestGetJobRuns $
--             newGetJobRuns
--
--         , requestCreateUserDefinedFunction $
--             newCreateUserDefinedFunction
--
--         , requestResetJobBookmark $
--             newResetJobBookmark
--
--         , requestListJobs $
--             newListJobs
--
--         , requestStartBlueprintRun $
--             newStartBlueprintRun
--
--         , requestBatchGetBlueprints $
--             newBatchGetBlueprints
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestUpdateJob $
--             newUpdateJob
--
--         , requestCreateRegistry $
--             newCreateRegistry
--
--         , requestGetCrawlers $
--             newGetCrawlers
--
--         , requestListTriggers $
--             newListTriggers
--
--         , requestGetClassifier $
--             newGetClassifier
--
--         , requestGetJob $
--             newGetJob
--
--         , requestListRegistries $
--             newListRegistries
--
--         , requestBatchDeleteTableVersion $
--             newBatchDeleteTableVersion
--
--         , requestGetDevEndpoints $
--             newGetDevEndpoints
--
--         , requestStartCrawlerSchedule $
--             newStartCrawlerSchedule
--
--         , requestGetPartitionIndexes $
--             newGetPartitionIndexes
--
--         , requestGetUserDefinedFunction $
--             newGetUserDefinedFunction
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestGetWorkflowRun $
--             newGetWorkflowRun
--
--         , requestDeleteDatabase $
--             newDeleteDatabase
--
--         , requestUpdateDatabase $
--             newUpdateDatabase
--
--         , requestGetColumnStatisticsForPartition $
--             newGetColumnStatisticsForPartition
--
--         , requestStopCrawler $
--             newStopCrawler
--
--         , requestDeleteSecurityConfiguration $
--             newDeleteSecurityConfiguration
--
--         , requestGetPartitions $
--             newGetPartitions
--
--         , requestPutSchemaVersionMetadata $
--             newPutSchemaVersionMetadata
--
--         , requestGetSchema $
--             newGetSchema
--
--         , requestBatchDeletePartition $
--             newBatchDeletePartition
--
--         , requestStartMLLabelingSetGenerationTaskRun $
--             newStartMLLabelingSetGenerationTaskRun
--
--         , requestBatchUpdatePartition $
--             newBatchUpdatePartition
--
--         , requestRegisterSchemaVersion $
--             newRegisterSchemaVersion
--
--         , requestStopWorkflowRun $
--             newStopWorkflowRun
--
--         , requestGetCrawler $
--             newGetCrawler
--
--         , requestListWorkflows $
--             newListWorkflows
--
--         , requestBatchStopJobRun $
--             newBatchStopJobRun
--
--         , requestGetDevEndpoint $
--             newGetDevEndpoint
--
--         , requestPutWorkflowRunProperties $
--             newPutWorkflowRunProperties
--
--         , requestCreateTable $
--             newCreateTable
--
--         , requestListCrawlers $
--             newListCrawlers
--
--         , requestGetCrawlerMetrics $
--             newGetCrawlerMetrics
--
--         , requestGetSchemaVersion $
--             newGetSchemaVersion
--
--         , requestGetPlan $
--             newGetPlan
--
--         , requestGetTriggers $
--             newGetTriggers
--
--         , requestCreateSchema $
--             newCreateSchema
--
--         , requestListDevEndpoints $
--             newListDevEndpoints
--
--         , requestStartTrigger $
--             newStartTrigger
--
--         , requestGetDataflowGraph $
--             newGetDataflowGraph
--
--         , requestGetDatabases $
--             newGetDatabases
--
--         , requestGetTable $
--             newGetTable
--
--         , requestCreateCrawler $
--             newCreateCrawler
--
--         , requestGetJobRun $
--             newGetJobRun
--
--         , requestCreateDevEndpoint $
--             newCreateDevEndpoint
--
--         , requestGetMLTaskRuns $
--             newGetMLTaskRuns
--
--         , requestTagResource $
--             newTagResource
--
--         , requestPutDataCatalogEncryptionSettings $
--             newPutDataCatalogEncryptionSettings
--
--         , requestGetMLTransforms $
--             newGetMLTransforms
--
--         , requestUpdateSchema $
--             newUpdateSchema
--
--         , requestDeleteSchema $
--             newDeleteSchema
--
--         , requestGetDatabase $
--             newGetDatabase
--
--         , requestDeleteColumnStatisticsForPartition $
--             newDeleteColumnStatisticsForPartition
--
--         , requestUpdateColumnStatisticsForPartition $
--             newUpdateColumnStatisticsForPartition
--
--         , requestCreateBlueprint $
--             newCreateBlueprint
--
--         , requestGetMLTaskRun $
--             newGetMLTaskRun
--
--         , requestDeletePartition $
--             newDeletePartition
--
--         , requestUpdatePartition $
--             newUpdatePartition
--
--         , requestGetMLTransform $
--             newGetMLTransform
--
--         , requestCreateScript $
--             newCreateScript
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestGetBlueprintRuns $
--             newGetBlueprintRuns
--
--         , requestGetSecurityConfigurations $
--             newGetSecurityConfigurations
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestGetConnections $
--             newGetConnections
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetSchemaVersionsDiff $
--             newGetSchemaVersionsDiff
--
--         , requestSearchTables $
--             newSearchTables
--
--         , requestGetTrigger $
--             newGetTrigger
--
--         , requestBatchGetJobs $
--             newBatchGetJobs
--
--         , requestImportCatalogToGlue $
--             newImportCatalogToGlue
--
--         , requestDeleteClassifier $
--             newDeleteClassifier
--
--         , requestUpdateClassifier $
--             newUpdateClassifier
--
--         , requestStartJobRun $
--             newStartJobRun
--
--         , requestDeleteBlueprint $
--             newDeleteBlueprint
--
--         , requestUpdateBlueprint $
--             newUpdateBlueprint
--
--         , requestListBlueprints $
--             newListBlueprints
--
--         , requestCreatePartition $
--             newCreatePartition
--
--         , requestBatchGetTriggers $
--             newBatchGetTriggers
--
--         , requestGetBlueprint $
--             newGetBlueprint
--
--         , requestStopCrawlerSchedule $
--             newStopCrawlerSchedule
--
--         , requestGetSchemaByDefinition $
--             newGetSchemaByDefinition
--
--         , requestCreateDatabase $
--             newCreateDatabase
--
--         , requestGetTableVersions $
--             newGetTableVersions
--
--         , requestCreateMLTransform $
--             newCreateMLTransform
--
--         , requestDeleteSchemaVersions $
--             newDeleteSchemaVersions
--
--         , requestDeleteTrigger $
--             newDeleteTrigger
--
--         , requestUpdateTrigger $
--             newUpdateTrigger
--
--           ]

--     , testGroup "response"
--         [ responseStartImportLabelsTaskRun $
--             newStartImportLabelsTaskRunResponse
--
--         , responseUpdateMLTransform $
--             newUpdateMLTransformResponse
--
--         , responseUpdateRegistry $
--             newUpdateRegistryResponse
--
--         , responseDeleteRegistry $
--             newDeleteRegistryResponse
--
--         , responseDeleteMLTransform $
--             newDeleteMLTransformResponse
--
--         , responseStartCrawler $
--             newStartCrawlerResponse
--
--         , responseGetCatalogImportStatus $
--             newGetCatalogImportStatusResponse
--
--         , responseListMLTransforms $
--             newListMLTransformsResponse
--
--         , responseGetPartition $
--             newGetPartitionResponse
--
--         , responseQuerySchemaVersionMetadata $
--             newQuerySchemaVersionMetadataResponse
--
--         , responseCreateTrigger $
--             newCreateTriggerResponse
--
--         , responseCheckSchemaVersionValidity $
--             newCheckSchemaVersionValidityResponse
--
--         , responseDeleteTable $
--             newDeleteTableResponse
--
--         , responseUpdateTable $
--             newUpdateTableResponse
--
--         , responseGetWorkflowRuns $
--             newGetWorkflowRunsResponse
--
--         , responseCreateWorkflow $
--             newCreateWorkflowResponse
--
--         , responseUpdateColumnStatisticsForTable $
--             newUpdateColumnStatisticsForTableResponse
--
--         , responseDeleteColumnStatisticsForTable $
--             newDeleteColumnStatisticsForTableResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseUpdateConnection $
--             newUpdateConnectionResponse
--
--         , responseGetUserDefinedFunctions $
--             newGetUserDefinedFunctionsResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseGetDataCatalogEncryptionSettings $
--             newGetDataCatalogEncryptionSettingsResponse
--
--         , responseBatchCreatePartition $
--             newBatchCreatePartitionResponse
--
--         , responseGetMapping $
--             newGetMappingResponse
--
--         , responseDeleteWorkflow $
--             newDeleteWorkflowResponse
--
--         , responseUpdateWorkflow $
--             newUpdateWorkflowResponse
--
--         , responseGetTableVersion $
--             newGetTableVersionResponse
--
--         , responseCreateSecurityConfiguration $
--             newCreateSecurityConfigurationResponse
--
--         , responseStartWorkflowRun $
--             newStartWorkflowRunResponse
--
--         , responseGetJobs $
--             newGetJobsResponse
--
--         , responseBatchGetWorkflows $
--             newBatchGetWorkflowsResponse
--
--         , responseGetClassifiers $
--             newGetClassifiersResponse
--
--         , responseGetResourcePolicies $
--             newGetResourcePoliciesResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseListSchemaVersions $
--             newListSchemaVersionsResponse
--
--         , responseGetWorkflowRunProperties $
--             newGetWorkflowRunPropertiesResponse
--
--         , responseBatchGetDevEndpoints $
--             newBatchGetDevEndpointsResponse
--
--         , responseDeletePartitionIndex $
--             newDeletePartitionIndexResponse
--
--         , responseDeleteTableVersion $
--             newDeleteTableVersionResponse
--
--         , responseDeleteDevEndpoint $
--             newDeleteDevEndpointResponse
--
--         , responseUpdateDevEndpoint $
--             newUpdateDevEndpointResponse
--
--         , responseGetWorkflow $
--             newGetWorkflowResponse
--
--         , responseBatchGetCrawlers $
--             newBatchGetCrawlersResponse
--
--         , responseGetJobBookmark $
--             newGetJobBookmarkResponse
--
--         , responseDeleteCrawler $
--             newDeleteCrawlerResponse
--
--         , responseUpdateCrawler $
--             newUpdateCrawlerResponse
--
--         , responseStartExportLabelsTaskRun $
--             newStartExportLabelsTaskRunResponse
--
--         , responseGetSecurityConfiguration $
--             newGetSecurityConfigurationResponse
--
--         , responseCreatePartitionIndex $
--             newCreatePartitionIndexResponse
--
--         , responseGetBlueprintRun $
--             newGetBlueprintRunResponse
--
--         , responseRemoveSchemaVersionMetadata $
--             newRemoveSchemaVersionMetadataResponse
--
--         , responseListSchemas $
--             newListSchemasResponse
--
--         , responseGetConnection $
--             newGetConnectionResponse
--
--         , responseGetColumnStatisticsForTable $
--             newGetColumnStatisticsForTableResponse
--
--         , responseBatchGetPartition $
--             newBatchGetPartitionResponse
--
--         , responseStopTrigger $
--             newStopTriggerResponse
--
--         , responseUpdateCrawlerSchedule $
--             newUpdateCrawlerScheduleResponse
--
--         , responseStartMLEvaluationTaskRun $
--             newStartMLEvaluationTaskRunResponse
--
--         , responseDeleteUserDefinedFunction $
--             newDeleteUserDefinedFunctionResponse
--
--         , responseUpdateUserDefinedFunction $
--             newUpdateUserDefinedFunctionResponse
--
--         , responseGetRegistry $
--             newGetRegistryResponse
--
--         , responseBatchDeleteTable $
--             newBatchDeleteTableResponse
--
--         , responseCancelMLTaskRun $
--             newCancelMLTaskRunResponse
--
--         , responseGetTables $
--             newGetTablesResponse
--
--         , responseResumeWorkflowRun $
--             newResumeWorkflowRunResponse
--
--         , responseCreateClassifier $
--             newCreateClassifierResponse
--
--         , responseBatchDeleteConnection $
--             newBatchDeleteConnectionResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseGetJobRuns $
--             newGetJobRunsResponse
--
--         , responseCreateUserDefinedFunction $
--             newCreateUserDefinedFunctionResponse
--
--         , responseResetJobBookmark $
--             newResetJobBookmarkResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseStartBlueprintRun $
--             newStartBlueprintRunResponse
--
--         , responseBatchGetBlueprints $
--             newBatchGetBlueprintsResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseUpdateJob $
--             newUpdateJobResponse
--
--         , responseCreateRegistry $
--             newCreateRegistryResponse
--
--         , responseGetCrawlers $
--             newGetCrawlersResponse
--
--         , responseListTriggers $
--             newListTriggersResponse
--
--         , responseGetClassifier $
--             newGetClassifierResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseListRegistries $
--             newListRegistriesResponse
--
--         , responseBatchDeleteTableVersion $
--             newBatchDeleteTableVersionResponse
--
--         , responseGetDevEndpoints $
--             newGetDevEndpointsResponse
--
--         , responseStartCrawlerSchedule $
--             newStartCrawlerScheduleResponse
--
--         , responseGetPartitionIndexes $
--             newGetPartitionIndexesResponse
--
--         , responseGetUserDefinedFunction $
--             newGetUserDefinedFunctionResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseGetWorkflowRun $
--             newGetWorkflowRunResponse
--
--         , responseDeleteDatabase $
--             newDeleteDatabaseResponse
--
--         , responseUpdateDatabase $
--             newUpdateDatabaseResponse
--
--         , responseGetColumnStatisticsForPartition $
--             newGetColumnStatisticsForPartitionResponse
--
--         , responseStopCrawler $
--             newStopCrawlerResponse
--
--         , responseDeleteSecurityConfiguration $
--             newDeleteSecurityConfigurationResponse
--
--         , responseGetPartitions $
--             newGetPartitionsResponse
--
--         , responsePutSchemaVersionMetadata $
--             newPutSchemaVersionMetadataResponse
--
--         , responseGetSchema $
--             newGetSchemaResponse
--
--         , responseBatchDeletePartition $
--             newBatchDeletePartitionResponse
--
--         , responseStartMLLabelingSetGenerationTaskRun $
--             newStartMLLabelingSetGenerationTaskRunResponse
--
--         , responseBatchUpdatePartition $
--             newBatchUpdatePartitionResponse
--
--         , responseRegisterSchemaVersion $
--             newRegisterSchemaVersionResponse
--
--         , responseStopWorkflowRun $
--             newStopWorkflowRunResponse
--
--         , responseGetCrawler $
--             newGetCrawlerResponse
--
--         , responseListWorkflows $
--             newListWorkflowsResponse
--
--         , responseBatchStopJobRun $
--             newBatchStopJobRunResponse
--
--         , responseGetDevEndpoint $
--             newGetDevEndpointResponse
--
--         , responsePutWorkflowRunProperties $
--             newPutWorkflowRunPropertiesResponse
--
--         , responseCreateTable $
--             newCreateTableResponse
--
--         , responseListCrawlers $
--             newListCrawlersResponse
--
--         , responseGetCrawlerMetrics $
--             newGetCrawlerMetricsResponse
--
--         , responseGetSchemaVersion $
--             newGetSchemaVersionResponse
--
--         , responseGetPlan $
--             newGetPlanResponse
--
--         , responseGetTriggers $
--             newGetTriggersResponse
--
--         , responseCreateSchema $
--             newCreateSchemaResponse
--
--         , responseListDevEndpoints $
--             newListDevEndpointsResponse
--
--         , responseStartTrigger $
--             newStartTriggerResponse
--
--         , responseGetDataflowGraph $
--             newGetDataflowGraphResponse
--
--         , responseGetDatabases $
--             newGetDatabasesResponse
--
--         , responseGetTable $
--             newGetTableResponse
--
--         , responseCreateCrawler $
--             newCreateCrawlerResponse
--
--         , responseGetJobRun $
--             newGetJobRunResponse
--
--         , responseCreateDevEndpoint $
--             newCreateDevEndpointResponse
--
--         , responseGetMLTaskRuns $
--             newGetMLTaskRunsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responsePutDataCatalogEncryptionSettings $
--             newPutDataCatalogEncryptionSettingsResponse
--
--         , responseGetMLTransforms $
--             newGetMLTransformsResponse
--
--         , responseUpdateSchema $
--             newUpdateSchemaResponse
--
--         , responseDeleteSchema $
--             newDeleteSchemaResponse
--
--         , responseGetDatabase $
--             newGetDatabaseResponse
--
--         , responseDeleteColumnStatisticsForPartition $
--             newDeleteColumnStatisticsForPartitionResponse
--
--         , responseUpdateColumnStatisticsForPartition $
--             newUpdateColumnStatisticsForPartitionResponse
--
--         , responseCreateBlueprint $
--             newCreateBlueprintResponse
--
--         , responseGetMLTaskRun $
--             newGetMLTaskRunResponse
--
--         , responseDeletePartition $
--             newDeletePartitionResponse
--
--         , responseUpdatePartition $
--             newUpdatePartitionResponse
--
--         , responseGetMLTransform $
--             newGetMLTransformResponse
--
--         , responseCreateScript $
--             newCreateScriptResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseGetBlueprintRuns $
--             newGetBlueprintRunsResponse
--
--         , responseGetSecurityConfigurations $
--             newGetSecurityConfigurationsResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseGetConnections $
--             newGetConnectionsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetSchemaVersionsDiff $
--             newGetSchemaVersionsDiffResponse
--
--         , responseSearchTables $
--             newSearchTablesResponse
--
--         , responseGetTrigger $
--             newGetTriggerResponse
--
--         , responseBatchGetJobs $
--             newBatchGetJobsResponse
--
--         , responseImportCatalogToGlue $
--             newImportCatalogToGlueResponse
--
--         , responseDeleteClassifier $
--             newDeleteClassifierResponse
--
--         , responseUpdateClassifier $
--             newUpdateClassifierResponse
--
--         , responseStartJobRun $
--             newStartJobRunResponse
--
--         , responseDeleteBlueprint $
--             newDeleteBlueprintResponse
--
--         , responseUpdateBlueprint $
--             newUpdateBlueprintResponse
--
--         , responseListBlueprints $
--             newListBlueprintsResponse
--
--         , responseCreatePartition $
--             newCreatePartitionResponse
--
--         , responseBatchGetTriggers $
--             newBatchGetTriggersResponse
--
--         , responseGetBlueprint $
--             newGetBlueprintResponse
--
--         , responseStopCrawlerSchedule $
--             newStopCrawlerScheduleResponse
--
--         , responseGetSchemaByDefinition $
--             newGetSchemaByDefinitionResponse
--
--         , responseCreateDatabase $
--             newCreateDatabaseResponse
--
--         , responseGetTableVersions $
--             newGetTableVersionsResponse
--
--         , responseCreateMLTransform $
--             newCreateMLTransformResponse
--
--         , responseDeleteSchemaVersions $
--             newDeleteSchemaVersionsResponse
--
--         , responseDeleteTrigger $
--             newDeleteTriggerResponse
--
--         , responseUpdateTrigger $
--             newUpdateTriggerResponse
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

requestGetBlueprintRun :: GetBlueprintRun -> TestTree
requestGetBlueprintRun =
  req
    "GetBlueprintRun"
    "fixture/GetBlueprintRun.yaml"

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

requestStartBlueprintRun :: StartBlueprintRun -> TestTree
requestStartBlueprintRun =
  req
    "StartBlueprintRun"
    "fixture/StartBlueprintRun.yaml"

requestBatchGetBlueprints :: BatchGetBlueprints -> TestTree
requestBatchGetBlueprints =
  req
    "BatchGetBlueprints"
    "fixture/BatchGetBlueprints.yaml"

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

requestCreateBlueprint :: CreateBlueprint -> TestTree
requestCreateBlueprint =
  req
    "CreateBlueprint"
    "fixture/CreateBlueprint.yaml"

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

requestGetBlueprintRuns :: GetBlueprintRuns -> TestTree
requestGetBlueprintRuns =
  req
    "GetBlueprintRuns"
    "fixture/GetBlueprintRuns.yaml"

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

requestDeleteBlueprint :: DeleteBlueprint -> TestTree
requestDeleteBlueprint =
  req
    "DeleteBlueprint"
    "fixture/DeleteBlueprint.yaml"

requestUpdateBlueprint :: UpdateBlueprint -> TestTree
requestUpdateBlueprint =
  req
    "UpdateBlueprint"
    "fixture/UpdateBlueprint.yaml"

requestListBlueprints :: ListBlueprints -> TestTree
requestListBlueprints =
  req
    "ListBlueprints"
    "fixture/ListBlueprints.yaml"

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

requestGetBlueprint :: GetBlueprint -> TestTree
requestGetBlueprint =
  req
    "GetBlueprint"
    "fixture/GetBlueprint.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartImportLabelsTaskRun)

responseUpdateMLTransform :: UpdateMLTransformResponse -> TestTree
responseUpdateMLTransform =
  res
    "UpdateMLTransformResponse"
    "fixture/UpdateMLTransformResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMLTransform)

responseUpdateRegistry :: UpdateRegistryResponse -> TestTree
responseUpdateRegistry =
  res
    "UpdateRegistryResponse"
    "fixture/UpdateRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRegistry)

responseDeleteRegistry :: DeleteRegistryResponse -> TestTree
responseDeleteRegistry =
  res
    "DeleteRegistryResponse"
    "fixture/DeleteRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRegistry)

responseDeleteMLTransform :: DeleteMLTransformResponse -> TestTree
responseDeleteMLTransform =
  res
    "DeleteMLTransformResponse"
    "fixture/DeleteMLTransformResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMLTransform)

responseStartCrawler :: StartCrawlerResponse -> TestTree
responseStartCrawler =
  res
    "StartCrawlerResponse"
    "fixture/StartCrawlerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCrawler)

responseGetCatalogImportStatus :: GetCatalogImportStatusResponse -> TestTree
responseGetCatalogImportStatus =
  res
    "GetCatalogImportStatusResponse"
    "fixture/GetCatalogImportStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCatalogImportStatus)

responseListMLTransforms :: ListMLTransformsResponse -> TestTree
responseListMLTransforms =
  res
    "ListMLTransformsResponse"
    "fixture/ListMLTransformsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMLTransforms)

responseGetPartition :: GetPartitionResponse -> TestTree
responseGetPartition =
  res
    "GetPartitionResponse"
    "fixture/GetPartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPartition)

responseQuerySchemaVersionMetadata :: QuerySchemaVersionMetadataResponse -> TestTree
responseQuerySchemaVersionMetadata =
  res
    "QuerySchemaVersionMetadataResponse"
    "fixture/QuerySchemaVersionMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy QuerySchemaVersionMetadata)

responseCreateTrigger :: CreateTriggerResponse -> TestTree
responseCreateTrigger =
  res
    "CreateTriggerResponse"
    "fixture/CreateTriggerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrigger)

responseCheckSchemaVersionValidity :: CheckSchemaVersionValidityResponse -> TestTree
responseCheckSchemaVersionValidity =
  res
    "CheckSchemaVersionValidityResponse"
    "fixture/CheckSchemaVersionValidityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckSchemaVersionValidity)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable =
  res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTable)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable =
  res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTable)

responseGetWorkflowRuns :: GetWorkflowRunsResponse -> TestTree
responseGetWorkflowRuns =
  res
    "GetWorkflowRunsResponse"
    "fixture/GetWorkflowRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowRuns)

responseCreateWorkflow :: CreateWorkflowResponse -> TestTree
responseCreateWorkflow =
  res
    "CreateWorkflowResponse"
    "fixture/CreateWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkflow)

responseUpdateColumnStatisticsForTable :: UpdateColumnStatisticsForTableResponse -> TestTree
responseUpdateColumnStatisticsForTable =
  res
    "UpdateColumnStatisticsForTableResponse"
    "fixture/UpdateColumnStatisticsForTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateColumnStatisticsForTable)

responseDeleteColumnStatisticsForTable :: DeleteColumnStatisticsForTableResponse -> TestTree
responseDeleteColumnStatisticsForTable =
  res
    "DeleteColumnStatisticsForTableResponse"
    "fixture/DeleteColumnStatisticsForTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteColumnStatisticsForTable)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnection)

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnection)

responseGetUserDefinedFunctions :: GetUserDefinedFunctionsResponse -> TestTree
responseGetUserDefinedFunctions =
  res
    "GetUserDefinedFunctionsResponse"
    "fixture/GetUserDefinedFunctionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserDefinedFunctions)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTags)

responseGetDataCatalogEncryptionSettings :: GetDataCatalogEncryptionSettingsResponse -> TestTree
responseGetDataCatalogEncryptionSettings =
  res
    "GetDataCatalogEncryptionSettingsResponse"
    "fixture/GetDataCatalogEncryptionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataCatalogEncryptionSettings)

responseBatchCreatePartition :: BatchCreatePartitionResponse -> TestTree
responseBatchCreatePartition =
  res
    "BatchCreatePartitionResponse"
    "fixture/BatchCreatePartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreatePartition)

responseGetMapping :: GetMappingResponse -> TestTree
responseGetMapping =
  res
    "GetMappingResponse"
    "fixture/GetMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMapping)

responseDeleteWorkflow :: DeleteWorkflowResponse -> TestTree
responseDeleteWorkflow =
  res
    "DeleteWorkflowResponse"
    "fixture/DeleteWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkflow)

responseUpdateWorkflow :: UpdateWorkflowResponse -> TestTree
responseUpdateWorkflow =
  res
    "UpdateWorkflowResponse"
    "fixture/UpdateWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkflow)

responseGetTableVersion :: GetTableVersionResponse -> TestTree
responseGetTableVersion =
  res
    "GetTableVersionResponse"
    "fixture/GetTableVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTableVersion)

responseCreateSecurityConfiguration :: CreateSecurityConfigurationResponse -> TestTree
responseCreateSecurityConfiguration =
  res
    "CreateSecurityConfigurationResponse"
    "fixture/CreateSecurityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecurityConfiguration)

responseStartWorkflowRun :: StartWorkflowRunResponse -> TestTree
responseStartWorkflowRun =
  res
    "StartWorkflowRunResponse"
    "fixture/StartWorkflowRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartWorkflowRun)

responseGetJobs :: GetJobsResponse -> TestTree
responseGetJobs =
  res
    "GetJobsResponse"
    "fixture/GetJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobs)

responseBatchGetWorkflows :: BatchGetWorkflowsResponse -> TestTree
responseBatchGetWorkflows =
  res
    "BatchGetWorkflowsResponse"
    "fixture/BatchGetWorkflowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetWorkflows)

responseGetClassifiers :: GetClassifiersResponse -> TestTree
responseGetClassifiers =
  res
    "GetClassifiersResponse"
    "fixture/GetClassifiersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClassifiers)

responseGetResourcePolicies :: GetResourcePoliciesResponse -> TestTree
responseGetResourcePolicies =
  res
    "GetResourcePoliciesResponse"
    "fixture/GetResourcePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicies)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnection)

responseListSchemaVersions :: ListSchemaVersionsResponse -> TestTree
responseListSchemaVersions =
  res
    "ListSchemaVersionsResponse"
    "fixture/ListSchemaVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchemaVersions)

responseGetWorkflowRunProperties :: GetWorkflowRunPropertiesResponse -> TestTree
responseGetWorkflowRunProperties =
  res
    "GetWorkflowRunPropertiesResponse"
    "fixture/GetWorkflowRunPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowRunProperties)

responseBatchGetDevEndpoints :: BatchGetDevEndpointsResponse -> TestTree
responseBatchGetDevEndpoints =
  res
    "BatchGetDevEndpointsResponse"
    "fixture/BatchGetDevEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDevEndpoints)

responseDeletePartitionIndex :: DeletePartitionIndexResponse -> TestTree
responseDeletePartitionIndex =
  res
    "DeletePartitionIndexResponse"
    "fixture/DeletePartitionIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePartitionIndex)

responseDeleteTableVersion :: DeleteTableVersionResponse -> TestTree
responseDeleteTableVersion =
  res
    "DeleteTableVersionResponse"
    "fixture/DeleteTableVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTableVersion)

responseDeleteDevEndpoint :: DeleteDevEndpointResponse -> TestTree
responseDeleteDevEndpoint =
  res
    "DeleteDevEndpointResponse"
    "fixture/DeleteDevEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDevEndpoint)

responseUpdateDevEndpoint :: UpdateDevEndpointResponse -> TestTree
responseUpdateDevEndpoint =
  res
    "UpdateDevEndpointResponse"
    "fixture/UpdateDevEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDevEndpoint)

responseGetWorkflow :: GetWorkflowResponse -> TestTree
responseGetWorkflow =
  res
    "GetWorkflowResponse"
    "fixture/GetWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflow)

responseBatchGetCrawlers :: BatchGetCrawlersResponse -> TestTree
responseBatchGetCrawlers =
  res
    "BatchGetCrawlersResponse"
    "fixture/BatchGetCrawlersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetCrawlers)

responseGetJobBookmark :: GetJobBookmarkResponse -> TestTree
responseGetJobBookmark =
  res
    "GetJobBookmarkResponse"
    "fixture/GetJobBookmarkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobBookmark)

responseDeleteCrawler :: DeleteCrawlerResponse -> TestTree
responseDeleteCrawler =
  res
    "DeleteCrawlerResponse"
    "fixture/DeleteCrawlerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCrawler)

responseUpdateCrawler :: UpdateCrawlerResponse -> TestTree
responseUpdateCrawler =
  res
    "UpdateCrawlerResponse"
    "fixture/UpdateCrawlerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCrawler)

responseStartExportLabelsTaskRun :: StartExportLabelsTaskRunResponse -> TestTree
responseStartExportLabelsTaskRun =
  res
    "StartExportLabelsTaskRunResponse"
    "fixture/StartExportLabelsTaskRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartExportLabelsTaskRun)

responseGetSecurityConfiguration :: GetSecurityConfigurationResponse -> TestTree
responseGetSecurityConfiguration =
  res
    "GetSecurityConfigurationResponse"
    "fixture/GetSecurityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSecurityConfiguration)

responseCreatePartitionIndex :: CreatePartitionIndexResponse -> TestTree
responseCreatePartitionIndex =
  res
    "CreatePartitionIndexResponse"
    "fixture/CreatePartitionIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePartitionIndex)

responseGetBlueprintRun :: GetBlueprintRunResponse -> TestTree
responseGetBlueprintRun =
  res
    "GetBlueprintRunResponse"
    "fixture/GetBlueprintRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlueprintRun)

responseRemoveSchemaVersionMetadata :: RemoveSchemaVersionMetadataResponse -> TestTree
responseRemoveSchemaVersionMetadata =
  res
    "RemoveSchemaVersionMetadataResponse"
    "fixture/RemoveSchemaVersionMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveSchemaVersionMetadata)

responseListSchemas :: ListSchemasResponse -> TestTree
responseListSchemas =
  res
    "ListSchemasResponse"
    "fixture/ListSchemasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchemas)

responseGetConnection :: GetConnectionResponse -> TestTree
responseGetConnection =
  res
    "GetConnectionResponse"
    "fixture/GetConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnection)

responseGetColumnStatisticsForTable :: GetColumnStatisticsForTableResponse -> TestTree
responseGetColumnStatisticsForTable =
  res
    "GetColumnStatisticsForTableResponse"
    "fixture/GetColumnStatisticsForTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetColumnStatisticsForTable)

responseBatchGetPartition :: BatchGetPartitionResponse -> TestTree
responseBatchGetPartition =
  res
    "BatchGetPartitionResponse"
    "fixture/BatchGetPartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetPartition)

responseStopTrigger :: StopTriggerResponse -> TestTree
responseStopTrigger =
  res
    "StopTriggerResponse"
    "fixture/StopTriggerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTrigger)

responseUpdateCrawlerSchedule :: UpdateCrawlerScheduleResponse -> TestTree
responseUpdateCrawlerSchedule =
  res
    "UpdateCrawlerScheduleResponse"
    "fixture/UpdateCrawlerScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCrawlerSchedule)

responseStartMLEvaluationTaskRun :: StartMLEvaluationTaskRunResponse -> TestTree
responseStartMLEvaluationTaskRun =
  res
    "StartMLEvaluationTaskRunResponse"
    "fixture/StartMLEvaluationTaskRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMLEvaluationTaskRun)

responseDeleteUserDefinedFunction :: DeleteUserDefinedFunctionResponse -> TestTree
responseDeleteUserDefinedFunction =
  res
    "DeleteUserDefinedFunctionResponse"
    "fixture/DeleteUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserDefinedFunction)

responseUpdateUserDefinedFunction :: UpdateUserDefinedFunctionResponse -> TestTree
responseUpdateUserDefinedFunction =
  res
    "UpdateUserDefinedFunctionResponse"
    "fixture/UpdateUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserDefinedFunction)

responseGetRegistry :: GetRegistryResponse -> TestTree
responseGetRegistry =
  res
    "GetRegistryResponse"
    "fixture/GetRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegistry)

responseBatchDeleteTable :: BatchDeleteTableResponse -> TestTree
responseBatchDeleteTable =
  res
    "BatchDeleteTableResponse"
    "fixture/BatchDeleteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteTable)

responseCancelMLTaskRun :: CancelMLTaskRunResponse -> TestTree
responseCancelMLTaskRun =
  res
    "CancelMLTaskRunResponse"
    "fixture/CancelMLTaskRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelMLTaskRun)

responseGetTables :: GetTablesResponse -> TestTree
responseGetTables =
  res
    "GetTablesResponse"
    "fixture/GetTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTables)

responseResumeWorkflowRun :: ResumeWorkflowRunResponse -> TestTree
responseResumeWorkflowRun =
  res
    "ResumeWorkflowRunResponse"
    "fixture/ResumeWorkflowRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeWorkflowRun)

responseCreateClassifier :: CreateClassifierResponse -> TestTree
responseCreateClassifier =
  res
    "CreateClassifierResponse"
    "fixture/CreateClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClassifier)

responseBatchDeleteConnection :: BatchDeleteConnectionResponse -> TestTree
responseBatchDeleteConnection =
  res
    "BatchDeleteConnectionResponse"
    "fixture/BatchDeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteConnection)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJob)

responseGetJobRuns :: GetJobRunsResponse -> TestTree
responseGetJobRuns =
  res
    "GetJobRunsResponse"
    "fixture/GetJobRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobRuns)

responseCreateUserDefinedFunction :: CreateUserDefinedFunctionResponse -> TestTree
responseCreateUserDefinedFunction =
  res
    "CreateUserDefinedFunctionResponse"
    "fixture/CreateUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserDefinedFunction)

responseResetJobBookmark :: ResetJobBookmarkResponse -> TestTree
responseResetJobBookmark =
  res
    "ResetJobBookmarkResponse"
    "fixture/ResetJobBookmarkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetJobBookmark)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseStartBlueprintRun :: StartBlueprintRunResponse -> TestTree
responseStartBlueprintRun =
  res
    "StartBlueprintRunResponse"
    "fixture/StartBlueprintRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBlueprintRun)

responseBatchGetBlueprints :: BatchGetBlueprintsResponse -> TestTree
responseBatchGetBlueprints =
  res
    "BatchGetBlueprintsResponse"
    "fixture/BatchGetBlueprintsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetBlueprints)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJob)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJob)

responseCreateRegistry :: CreateRegistryResponse -> TestTree
responseCreateRegistry =
  res
    "CreateRegistryResponse"
    "fixture/CreateRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRegistry)

responseGetCrawlers :: GetCrawlersResponse -> TestTree
responseGetCrawlers =
  res
    "GetCrawlersResponse"
    "fixture/GetCrawlersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCrawlers)

responseListTriggers :: ListTriggersResponse -> TestTree
responseListTriggers =
  res
    "ListTriggersResponse"
    "fixture/ListTriggersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTriggers)

responseGetClassifier :: GetClassifierResponse -> TestTree
responseGetClassifier =
  res
    "GetClassifierResponse"
    "fixture/GetClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClassifier)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJob)

responseListRegistries :: ListRegistriesResponse -> TestTree
responseListRegistries =
  res
    "ListRegistriesResponse"
    "fixture/ListRegistriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRegistries)

responseBatchDeleteTableVersion :: BatchDeleteTableVersionResponse -> TestTree
responseBatchDeleteTableVersion =
  res
    "BatchDeleteTableVersionResponse"
    "fixture/BatchDeleteTableVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteTableVersion)

responseGetDevEndpoints :: GetDevEndpointsResponse -> TestTree
responseGetDevEndpoints =
  res
    "GetDevEndpointsResponse"
    "fixture/GetDevEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevEndpoints)

responseStartCrawlerSchedule :: StartCrawlerScheduleResponse -> TestTree
responseStartCrawlerSchedule =
  res
    "StartCrawlerScheduleResponse"
    "fixture/StartCrawlerScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCrawlerSchedule)

responseGetPartitionIndexes :: GetPartitionIndexesResponse -> TestTree
responseGetPartitionIndexes =
  res
    "GetPartitionIndexesResponse"
    "fixture/GetPartitionIndexesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPartitionIndexes)

responseGetUserDefinedFunction :: GetUserDefinedFunctionResponse -> TestTree
responseGetUserDefinedFunction =
  res
    "GetUserDefinedFunctionResponse"
    "fixture/GetUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserDefinedFunction)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicy)

responseGetWorkflowRun :: GetWorkflowRunResponse -> TestTree
responseGetWorkflowRun =
  res
    "GetWorkflowRunResponse"
    "fixture/GetWorkflowRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowRun)

responseDeleteDatabase :: DeleteDatabaseResponse -> TestTree
responseDeleteDatabase =
  res
    "DeleteDatabaseResponse"
    "fixture/DeleteDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatabase)

responseUpdateDatabase :: UpdateDatabaseResponse -> TestTree
responseUpdateDatabase =
  res
    "UpdateDatabaseResponse"
    "fixture/UpdateDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDatabase)

responseGetColumnStatisticsForPartition :: GetColumnStatisticsForPartitionResponse -> TestTree
responseGetColumnStatisticsForPartition =
  res
    "GetColumnStatisticsForPartitionResponse"
    "fixture/GetColumnStatisticsForPartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetColumnStatisticsForPartition)

responseStopCrawler :: StopCrawlerResponse -> TestTree
responseStopCrawler =
  res
    "StopCrawlerResponse"
    "fixture/StopCrawlerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopCrawler)

responseDeleteSecurityConfiguration :: DeleteSecurityConfigurationResponse -> TestTree
responseDeleteSecurityConfiguration =
  res
    "DeleteSecurityConfigurationResponse"
    "fixture/DeleteSecurityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecurityConfiguration)

responseGetPartitions :: GetPartitionsResponse -> TestTree
responseGetPartitions =
  res
    "GetPartitionsResponse"
    "fixture/GetPartitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPartitions)

responsePutSchemaVersionMetadata :: PutSchemaVersionMetadataResponse -> TestTree
responsePutSchemaVersionMetadata =
  res
    "PutSchemaVersionMetadataResponse"
    "fixture/PutSchemaVersionMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSchemaVersionMetadata)

responseGetSchema :: GetSchemaResponse -> TestTree
responseGetSchema =
  res
    "GetSchemaResponse"
    "fixture/GetSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchema)

responseBatchDeletePartition :: BatchDeletePartitionResponse -> TestTree
responseBatchDeletePartition =
  res
    "BatchDeletePartitionResponse"
    "fixture/BatchDeletePartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeletePartition)

responseStartMLLabelingSetGenerationTaskRun :: StartMLLabelingSetGenerationTaskRunResponse -> TestTree
responseStartMLLabelingSetGenerationTaskRun =
  res
    "StartMLLabelingSetGenerationTaskRunResponse"
    "fixture/StartMLLabelingSetGenerationTaskRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMLLabelingSetGenerationTaskRun)

responseBatchUpdatePartition :: BatchUpdatePartitionResponse -> TestTree
responseBatchUpdatePartition =
  res
    "BatchUpdatePartitionResponse"
    "fixture/BatchUpdatePartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdatePartition)

responseRegisterSchemaVersion :: RegisterSchemaVersionResponse -> TestTree
responseRegisterSchemaVersion =
  res
    "RegisterSchemaVersionResponse"
    "fixture/RegisterSchemaVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterSchemaVersion)

responseStopWorkflowRun :: StopWorkflowRunResponse -> TestTree
responseStopWorkflowRun =
  res
    "StopWorkflowRunResponse"
    "fixture/StopWorkflowRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopWorkflowRun)

responseGetCrawler :: GetCrawlerResponse -> TestTree
responseGetCrawler =
  res
    "GetCrawlerResponse"
    "fixture/GetCrawlerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCrawler)

responseListWorkflows :: ListWorkflowsResponse -> TestTree
responseListWorkflows =
  res
    "ListWorkflowsResponse"
    "fixture/ListWorkflowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflows)

responseBatchStopJobRun :: BatchStopJobRunResponse -> TestTree
responseBatchStopJobRun =
  res
    "BatchStopJobRunResponse"
    "fixture/BatchStopJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchStopJobRun)

responseGetDevEndpoint :: GetDevEndpointResponse -> TestTree
responseGetDevEndpoint =
  res
    "GetDevEndpointResponse"
    "fixture/GetDevEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevEndpoint)

responsePutWorkflowRunProperties :: PutWorkflowRunPropertiesResponse -> TestTree
responsePutWorkflowRunProperties =
  res
    "PutWorkflowRunPropertiesResponse"
    "fixture/PutWorkflowRunPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutWorkflowRunProperties)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable =
  res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTable)

responseListCrawlers :: ListCrawlersResponse -> TestTree
responseListCrawlers =
  res
    "ListCrawlersResponse"
    "fixture/ListCrawlersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCrawlers)

responseGetCrawlerMetrics :: GetCrawlerMetricsResponse -> TestTree
responseGetCrawlerMetrics =
  res
    "GetCrawlerMetricsResponse"
    "fixture/GetCrawlerMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCrawlerMetrics)

responseGetSchemaVersion :: GetSchemaVersionResponse -> TestTree
responseGetSchemaVersion =
  res
    "GetSchemaVersionResponse"
    "fixture/GetSchemaVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchemaVersion)

responseGetPlan :: GetPlanResponse -> TestTree
responseGetPlan =
  res
    "GetPlanResponse"
    "fixture/GetPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPlan)

responseGetTriggers :: GetTriggersResponse -> TestTree
responseGetTriggers =
  res
    "GetTriggersResponse"
    "fixture/GetTriggersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTriggers)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSchema)

responseListDevEndpoints :: ListDevEndpointsResponse -> TestTree
responseListDevEndpoints =
  res
    "ListDevEndpointsResponse"
    "fixture/ListDevEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevEndpoints)

responseStartTrigger :: StartTriggerResponse -> TestTree
responseStartTrigger =
  res
    "StartTriggerResponse"
    "fixture/StartTriggerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTrigger)

responseGetDataflowGraph :: GetDataflowGraphResponse -> TestTree
responseGetDataflowGraph =
  res
    "GetDataflowGraphResponse"
    "fixture/GetDataflowGraphResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataflowGraph)

responseGetDatabases :: GetDatabasesResponse -> TestTree
responseGetDatabases =
  res
    "GetDatabasesResponse"
    "fixture/GetDatabasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatabases)

responseGetTable :: GetTableResponse -> TestTree
responseGetTable =
  res
    "GetTableResponse"
    "fixture/GetTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTable)

responseCreateCrawler :: CreateCrawlerResponse -> TestTree
responseCreateCrawler =
  res
    "CreateCrawlerResponse"
    "fixture/CreateCrawlerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCrawler)

responseGetJobRun :: GetJobRunResponse -> TestTree
responseGetJobRun =
  res
    "GetJobRunResponse"
    "fixture/GetJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobRun)

responseCreateDevEndpoint :: CreateDevEndpointResponse -> TestTree
responseCreateDevEndpoint =
  res
    "CreateDevEndpointResponse"
    "fixture/CreateDevEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDevEndpoint)

responseGetMLTaskRuns :: GetMLTaskRunsResponse -> TestTree
responseGetMLTaskRuns =
  res
    "GetMLTaskRunsResponse"
    "fixture/GetMLTaskRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMLTaskRuns)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responsePutDataCatalogEncryptionSettings :: PutDataCatalogEncryptionSettingsResponse -> TestTree
responsePutDataCatalogEncryptionSettings =
  res
    "PutDataCatalogEncryptionSettingsResponse"
    "fixture/PutDataCatalogEncryptionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDataCatalogEncryptionSettings)

responseGetMLTransforms :: GetMLTransformsResponse -> TestTree
responseGetMLTransforms =
  res
    "GetMLTransformsResponse"
    "fixture/GetMLTransformsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMLTransforms)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSchema)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchema)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatabase)

responseDeleteColumnStatisticsForPartition :: DeleteColumnStatisticsForPartitionResponse -> TestTree
responseDeleteColumnStatisticsForPartition =
  res
    "DeleteColumnStatisticsForPartitionResponse"
    "fixture/DeleteColumnStatisticsForPartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteColumnStatisticsForPartition)

responseUpdateColumnStatisticsForPartition :: UpdateColumnStatisticsForPartitionResponse -> TestTree
responseUpdateColumnStatisticsForPartition =
  res
    "UpdateColumnStatisticsForPartitionResponse"
    "fixture/UpdateColumnStatisticsForPartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateColumnStatisticsForPartition)

responseCreateBlueprint :: CreateBlueprintResponse -> TestTree
responseCreateBlueprint =
  res
    "CreateBlueprintResponse"
    "fixture/CreateBlueprintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBlueprint)

responseGetMLTaskRun :: GetMLTaskRunResponse -> TestTree
responseGetMLTaskRun =
  res
    "GetMLTaskRunResponse"
    "fixture/GetMLTaskRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMLTaskRun)

responseDeletePartition :: DeletePartitionResponse -> TestTree
responseDeletePartition =
  res
    "DeletePartitionResponse"
    "fixture/DeletePartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePartition)

responseUpdatePartition :: UpdatePartitionResponse -> TestTree
responseUpdatePartition =
  res
    "UpdatePartitionResponse"
    "fixture/UpdatePartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePartition)

responseGetMLTransform :: GetMLTransformResponse -> TestTree
responseGetMLTransform =
  res
    "GetMLTransformResponse"
    "fixture/GetMLTransformResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMLTransform)

responseCreateScript :: CreateScriptResponse -> TestTree
responseCreateScript =
  res
    "CreateScriptResponse"
    "fixture/CreateScriptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateScript)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseGetBlueprintRuns :: GetBlueprintRunsResponse -> TestTree
responseGetBlueprintRuns =
  res
    "GetBlueprintRunsResponse"
    "fixture/GetBlueprintRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlueprintRuns)

responseGetSecurityConfigurations :: GetSecurityConfigurationsResponse -> TestTree
responseGetSecurityConfigurations =
  res
    "GetSecurityConfigurationsResponse"
    "fixture/GetSecurityConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSecurityConfigurations)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseGetConnections :: GetConnectionsResponse -> TestTree
responseGetConnections =
  res
    "GetConnectionsResponse"
    "fixture/GetConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnections)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseGetSchemaVersionsDiff :: GetSchemaVersionsDiffResponse -> TestTree
responseGetSchemaVersionsDiff =
  res
    "GetSchemaVersionsDiffResponse"
    "fixture/GetSchemaVersionsDiffResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchemaVersionsDiff)

responseSearchTables :: SearchTablesResponse -> TestTree
responseSearchTables =
  res
    "SearchTablesResponse"
    "fixture/SearchTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchTables)

responseGetTrigger :: GetTriggerResponse -> TestTree
responseGetTrigger =
  res
    "GetTriggerResponse"
    "fixture/GetTriggerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrigger)

responseBatchGetJobs :: BatchGetJobsResponse -> TestTree
responseBatchGetJobs =
  res
    "BatchGetJobsResponse"
    "fixture/BatchGetJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetJobs)

responseImportCatalogToGlue :: ImportCatalogToGlueResponse -> TestTree
responseImportCatalogToGlue =
  res
    "ImportCatalogToGlueResponse"
    "fixture/ImportCatalogToGlueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportCatalogToGlue)

responseDeleteClassifier :: DeleteClassifierResponse -> TestTree
responseDeleteClassifier =
  res
    "DeleteClassifierResponse"
    "fixture/DeleteClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClassifier)

responseUpdateClassifier :: UpdateClassifierResponse -> TestTree
responseUpdateClassifier =
  res
    "UpdateClassifierResponse"
    "fixture/UpdateClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClassifier)

responseStartJobRun :: StartJobRunResponse -> TestTree
responseStartJobRun =
  res
    "StartJobRunResponse"
    "fixture/StartJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartJobRun)

responseDeleteBlueprint :: DeleteBlueprintResponse -> TestTree
responseDeleteBlueprint =
  res
    "DeleteBlueprintResponse"
    "fixture/DeleteBlueprintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBlueprint)

responseUpdateBlueprint :: UpdateBlueprintResponse -> TestTree
responseUpdateBlueprint =
  res
    "UpdateBlueprintResponse"
    "fixture/UpdateBlueprintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBlueprint)

responseListBlueprints :: ListBlueprintsResponse -> TestTree
responseListBlueprints =
  res
    "ListBlueprintsResponse"
    "fixture/ListBlueprintsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBlueprints)

responseCreatePartition :: CreatePartitionResponse -> TestTree
responseCreatePartition =
  res
    "CreatePartitionResponse"
    "fixture/CreatePartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePartition)

responseBatchGetTriggers :: BatchGetTriggersResponse -> TestTree
responseBatchGetTriggers =
  res
    "BatchGetTriggersResponse"
    "fixture/BatchGetTriggersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetTriggers)

responseGetBlueprint :: GetBlueprintResponse -> TestTree
responseGetBlueprint =
  res
    "GetBlueprintResponse"
    "fixture/GetBlueprintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlueprint)

responseStopCrawlerSchedule :: StopCrawlerScheduleResponse -> TestTree
responseStopCrawlerSchedule =
  res
    "StopCrawlerScheduleResponse"
    "fixture/StopCrawlerScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopCrawlerSchedule)

responseGetSchemaByDefinition :: GetSchemaByDefinitionResponse -> TestTree
responseGetSchemaByDefinition =
  res
    "GetSchemaByDefinitionResponse"
    "fixture/GetSchemaByDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchemaByDefinition)

responseCreateDatabase :: CreateDatabaseResponse -> TestTree
responseCreateDatabase =
  res
    "CreateDatabaseResponse"
    "fixture/CreateDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatabase)

responseGetTableVersions :: GetTableVersionsResponse -> TestTree
responseGetTableVersions =
  res
    "GetTableVersionsResponse"
    "fixture/GetTableVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTableVersions)

responseCreateMLTransform :: CreateMLTransformResponse -> TestTree
responseCreateMLTransform =
  res
    "CreateMLTransformResponse"
    "fixture/CreateMLTransformResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMLTransform)

responseDeleteSchemaVersions :: DeleteSchemaVersionsResponse -> TestTree
responseDeleteSchemaVersions =
  res
    "DeleteSchemaVersionsResponse"
    "fixture/DeleteSchemaVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchemaVersions)

responseDeleteTrigger :: DeleteTriggerResponse -> TestTree
responseDeleteTrigger =
  res
    "DeleteTriggerResponse"
    "fixture/DeleteTriggerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrigger)

responseUpdateTrigger :: UpdateTriggerResponse -> TestTree
responseUpdateTrigger =
  res
    "UpdateTriggerResponse"
    "fixture/UpdateTriggerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrigger)
