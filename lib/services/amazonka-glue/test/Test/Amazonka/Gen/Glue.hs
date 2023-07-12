{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Glue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestBatchCreatePartition $
--             newBatchCreatePartition
--
--         , requestBatchDeleteConnection $
--             newBatchDeleteConnection
--
--         , requestBatchDeletePartition $
--             newBatchDeletePartition
--
--         , requestBatchDeleteTable $
--             newBatchDeleteTable
--
--         , requestBatchDeleteTableVersion $
--             newBatchDeleteTableVersion
--
--         , requestBatchGetBlueprints $
--             newBatchGetBlueprints
--
--         , requestBatchGetCrawlers $
--             newBatchGetCrawlers
--
--         , requestBatchGetCustomEntityTypes $
--             newBatchGetCustomEntityTypes
--
--         , requestBatchGetDataQualityResult $
--             newBatchGetDataQualityResult
--
--         , requestBatchGetDevEndpoints $
--             newBatchGetDevEndpoints
--
--         , requestBatchGetJobs $
--             newBatchGetJobs
--
--         , requestBatchGetPartition $
--             newBatchGetPartition
--
--         , requestBatchGetTriggers $
--             newBatchGetTriggers
--
--         , requestBatchGetWorkflows $
--             newBatchGetWorkflows
--
--         , requestBatchStopJobRun $
--             newBatchStopJobRun
--
--         , requestBatchUpdatePartition $
--             newBatchUpdatePartition
--
--         , requestCancelDataQualityRuleRecommendationRun $
--             newCancelDataQualityRuleRecommendationRun
--
--         , requestCancelDataQualityRulesetEvaluationRun $
--             newCancelDataQualityRulesetEvaluationRun
--
--         , requestCancelMLTaskRun $
--             newCancelMLTaskRun
--
--         , requestCancelStatement $
--             newCancelStatement
--
--         , requestCheckSchemaVersionValidity $
--             newCheckSchemaVersionValidity
--
--         , requestCreateBlueprint $
--             newCreateBlueprint
--
--         , requestCreateClassifier $
--             newCreateClassifier
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestCreateCrawler $
--             newCreateCrawler
--
--         , requestCreateCustomEntityType $
--             newCreateCustomEntityType
--
--         , requestCreateDataQualityRuleset $
--             newCreateDataQualityRuleset
--
--         , requestCreateDatabase $
--             newCreateDatabase
--
--         , requestCreateDevEndpoint $
--             newCreateDevEndpoint
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestCreateMLTransform $
--             newCreateMLTransform
--
--         , requestCreatePartition $
--             newCreatePartition
--
--         , requestCreatePartitionIndex $
--             newCreatePartitionIndex
--
--         , requestCreateRegistry $
--             newCreateRegistry
--
--         , requestCreateSchema $
--             newCreateSchema
--
--         , requestCreateScript $
--             newCreateScript
--
--         , requestCreateSecurityConfiguration $
--             newCreateSecurityConfiguration
--
--         , requestCreateSession $
--             newCreateSession
--
--         , requestCreateTable $
--             newCreateTable
--
--         , requestCreateTrigger $
--             newCreateTrigger
--
--         , requestCreateUserDefinedFunction $
--             newCreateUserDefinedFunction
--
--         , requestCreateWorkflow $
--             newCreateWorkflow
--
--         , requestDeleteBlueprint $
--             newDeleteBlueprint
--
--         , requestDeleteClassifier $
--             newDeleteClassifier
--
--         , requestDeleteColumnStatisticsForPartition $
--             newDeleteColumnStatisticsForPartition
--
--         , requestDeleteColumnStatisticsForTable $
--             newDeleteColumnStatisticsForTable
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestDeleteCrawler $
--             newDeleteCrawler
--
--         , requestDeleteCustomEntityType $
--             newDeleteCustomEntityType
--
--         , requestDeleteDataQualityRuleset $
--             newDeleteDataQualityRuleset
--
--         , requestDeleteDatabase $
--             newDeleteDatabase
--
--         , requestDeleteDevEndpoint $
--             newDeleteDevEndpoint
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestDeleteMLTransform $
--             newDeleteMLTransform
--
--         , requestDeletePartition $
--             newDeletePartition
--
--         , requestDeletePartitionIndex $
--             newDeletePartitionIndex
--
--         , requestDeleteRegistry $
--             newDeleteRegistry
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteSchema $
--             newDeleteSchema
--
--         , requestDeleteSchemaVersions $
--             newDeleteSchemaVersions
--
--         , requestDeleteSecurityConfiguration $
--             newDeleteSecurityConfiguration
--
--         , requestDeleteSession $
--             newDeleteSession
--
--         , requestDeleteTable $
--             newDeleteTable
--
--         , requestDeleteTableVersion $
--             newDeleteTableVersion
--
--         , requestDeleteTrigger $
--             newDeleteTrigger
--
--         , requestDeleteUserDefinedFunction $
--             newDeleteUserDefinedFunction
--
--         , requestDeleteWorkflow $
--             newDeleteWorkflow
--
--         , requestGetBlueprint $
--             newGetBlueprint
--
--         , requestGetBlueprintRun $
--             newGetBlueprintRun
--
--         , requestGetBlueprintRuns $
--             newGetBlueprintRuns
--
--         , requestGetCatalogImportStatus $
--             newGetCatalogImportStatus
--
--         , requestGetClassifier $
--             newGetClassifier
--
--         , requestGetClassifiers $
--             newGetClassifiers
--
--         , requestGetColumnStatisticsForPartition $
--             newGetColumnStatisticsForPartition
--
--         , requestGetColumnStatisticsForTable $
--             newGetColumnStatisticsForTable
--
--         , requestGetConnection $
--             newGetConnection
--
--         , requestGetConnections $
--             newGetConnections
--
--         , requestGetCrawler $
--             newGetCrawler
--
--         , requestGetCrawlerMetrics $
--             newGetCrawlerMetrics
--
--         , requestGetCrawlers $
--             newGetCrawlers
--
--         , requestGetCustomEntityType $
--             newGetCustomEntityType
--
--         , requestGetDataCatalogEncryptionSettings $
--             newGetDataCatalogEncryptionSettings
--
--         , requestGetDataQualityResult $
--             newGetDataQualityResult
--
--         , requestGetDataQualityRuleRecommendationRun $
--             newGetDataQualityRuleRecommendationRun
--
--         , requestGetDataQualityRuleset $
--             newGetDataQualityRuleset
--
--         , requestGetDataQualityRulesetEvaluationRun $
--             newGetDataQualityRulesetEvaluationRun
--
--         , requestGetDatabase $
--             newGetDatabase
--
--         , requestGetDatabases $
--             newGetDatabases
--
--         , requestGetDataflowGraph $
--             newGetDataflowGraph
--
--         , requestGetDevEndpoint $
--             newGetDevEndpoint
--
--         , requestGetDevEndpoints $
--             newGetDevEndpoints
--
--         , requestGetJob $
--             newGetJob
--
--         , requestGetJobBookmark $
--             newGetJobBookmark
--
--         , requestGetJobRun $
--             newGetJobRun
--
--         , requestGetJobRuns $
--             newGetJobRuns
--
--         , requestGetJobs $
--             newGetJobs
--
--         , requestGetMLTaskRun $
--             newGetMLTaskRun
--
--         , requestGetMLTaskRuns $
--             newGetMLTaskRuns
--
--         , requestGetMLTransform $
--             newGetMLTransform
--
--         , requestGetMLTransforms $
--             newGetMLTransforms
--
--         , requestGetMapping $
--             newGetMapping
--
--         , requestGetPartition $
--             newGetPartition
--
--         , requestGetPartitionIndexes $
--             newGetPartitionIndexes
--
--         , requestGetPartitions $
--             newGetPartitions
--
--         , requestGetPlan $
--             newGetPlan
--
--         , requestGetRegistry $
--             newGetRegistry
--
--         , requestGetResourcePolicies $
--             newGetResourcePolicies
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestGetSchema $
--             newGetSchema
--
--         , requestGetSchemaByDefinition $
--             newGetSchemaByDefinition
--
--         , requestGetSchemaVersion $
--             newGetSchemaVersion
--
--         , requestGetSchemaVersionsDiff $
--             newGetSchemaVersionsDiff
--
--         , requestGetSecurityConfiguration $
--             newGetSecurityConfiguration
--
--         , requestGetSecurityConfigurations $
--             newGetSecurityConfigurations
--
--         , requestGetSession $
--             newGetSession
--
--         , requestGetStatement $
--             newGetStatement
--
--         , requestGetTable $
--             newGetTable
--
--         , requestGetTableVersion $
--             newGetTableVersion
--
--         , requestGetTableVersions $
--             newGetTableVersions
--
--         , requestGetTables $
--             newGetTables
--
--         , requestGetTags $
--             newGetTags
--
--         , requestGetTrigger $
--             newGetTrigger
--
--         , requestGetTriggers $
--             newGetTriggers
--
--         , requestGetUnfilteredPartitionMetadata $
--             newGetUnfilteredPartitionMetadata
--
--         , requestGetUnfilteredPartitionsMetadata $
--             newGetUnfilteredPartitionsMetadata
--
--         , requestGetUnfilteredTableMetadata $
--             newGetUnfilteredTableMetadata
--
--         , requestGetUserDefinedFunction $
--             newGetUserDefinedFunction
--
--         , requestGetUserDefinedFunctions $
--             newGetUserDefinedFunctions
--
--         , requestGetWorkflow $
--             newGetWorkflow
--
--         , requestGetWorkflowRun $
--             newGetWorkflowRun
--
--         , requestGetWorkflowRunProperties $
--             newGetWorkflowRunProperties
--
--         , requestGetWorkflowRuns $
--             newGetWorkflowRuns
--
--         , requestImportCatalogToGlue $
--             newImportCatalogToGlue
--
--         , requestListBlueprints $
--             newListBlueprints
--
--         , requestListCrawlers $
--             newListCrawlers
--
--         , requestListCrawls $
--             newListCrawls
--
--         , requestListCustomEntityTypes $
--             newListCustomEntityTypes
--
--         , requestListDataQualityResults $
--             newListDataQualityResults
--
--         , requestListDataQualityRuleRecommendationRuns $
--             newListDataQualityRuleRecommendationRuns
--
--         , requestListDataQualityRulesetEvaluationRuns $
--             newListDataQualityRulesetEvaluationRuns
--
--         , requestListDataQualityRulesets $
--             newListDataQualityRulesets
--
--         , requestListDevEndpoints $
--             newListDevEndpoints
--
--         , requestListJobs $
--             newListJobs
--
--         , requestListMLTransforms $
--             newListMLTransforms
--
--         , requestListRegistries $
--             newListRegistries
--
--         , requestListSchemaVersions $
--             newListSchemaVersions
--
--         , requestListSchemas $
--             newListSchemas
--
--         , requestListSessions $
--             newListSessions
--
--         , requestListStatements $
--             newListStatements
--
--         , requestListTriggers $
--             newListTriggers
--
--         , requestListWorkflows $
--             newListWorkflows
--
--         , requestPutDataCatalogEncryptionSettings $
--             newPutDataCatalogEncryptionSettings
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestPutSchemaVersionMetadata $
--             newPutSchemaVersionMetadata
--
--         , requestPutWorkflowRunProperties $
--             newPutWorkflowRunProperties
--
--         , requestQuerySchemaVersionMetadata $
--             newQuerySchemaVersionMetadata
--
--         , requestRegisterSchemaVersion $
--             newRegisterSchemaVersion
--
--         , requestRemoveSchemaVersionMetadata $
--             newRemoveSchemaVersionMetadata
--
--         , requestResetJobBookmark $
--             newResetJobBookmark
--
--         , requestResumeWorkflowRun $
--             newResumeWorkflowRun
--
--         , requestRunStatement $
--             newRunStatement
--
--         , requestSearchTables $
--             newSearchTables
--
--         , requestStartBlueprintRun $
--             newStartBlueprintRun
--
--         , requestStartCrawler $
--             newStartCrawler
--
--         , requestStartCrawlerSchedule $
--             newStartCrawlerSchedule
--
--         , requestStartDataQualityRuleRecommendationRun $
--             newStartDataQualityRuleRecommendationRun
--
--         , requestStartDataQualityRulesetEvaluationRun $
--             newStartDataQualityRulesetEvaluationRun
--
--         , requestStartExportLabelsTaskRun $
--             newStartExportLabelsTaskRun
--
--         , requestStartImportLabelsTaskRun $
--             newStartImportLabelsTaskRun
--
--         , requestStartJobRun $
--             newStartJobRun
--
--         , requestStartMLEvaluationTaskRun $
--             newStartMLEvaluationTaskRun
--
--         , requestStartMLLabelingSetGenerationTaskRun $
--             newStartMLLabelingSetGenerationTaskRun
--
--         , requestStartTrigger $
--             newStartTrigger
--
--         , requestStartWorkflowRun $
--             newStartWorkflowRun
--
--         , requestStopCrawler $
--             newStopCrawler
--
--         , requestStopCrawlerSchedule $
--             newStopCrawlerSchedule
--
--         , requestStopSession $
--             newStopSession
--
--         , requestStopTrigger $
--             newStopTrigger
--
--         , requestStopWorkflowRun $
--             newStopWorkflowRun
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateBlueprint $
--             newUpdateBlueprint
--
--         , requestUpdateClassifier $
--             newUpdateClassifier
--
--         , requestUpdateColumnStatisticsForPartition $
--             newUpdateColumnStatisticsForPartition
--
--         , requestUpdateColumnStatisticsForTable $
--             newUpdateColumnStatisticsForTable
--
--         , requestUpdateConnection $
--             newUpdateConnection
--
--         , requestUpdateCrawler $
--             newUpdateCrawler
--
--         , requestUpdateCrawlerSchedule $
--             newUpdateCrawlerSchedule
--
--         , requestUpdateDataQualityRuleset $
--             newUpdateDataQualityRuleset
--
--         , requestUpdateDatabase $
--             newUpdateDatabase
--
--         , requestUpdateDevEndpoint $
--             newUpdateDevEndpoint
--
--         , requestUpdateJob $
--             newUpdateJob
--
--         , requestUpdateJobFromSourceControl $
--             newUpdateJobFromSourceControl
--
--         , requestUpdateMLTransform $
--             newUpdateMLTransform
--
--         , requestUpdatePartition $
--             newUpdatePartition
--
--         , requestUpdateRegistry $
--             newUpdateRegistry
--
--         , requestUpdateSchema $
--             newUpdateSchema
--
--         , requestUpdateSourceControlFromJob $
--             newUpdateSourceControlFromJob
--
--         , requestUpdateTable $
--             newUpdateTable
--
--         , requestUpdateTrigger $
--             newUpdateTrigger
--
--         , requestUpdateUserDefinedFunction $
--             newUpdateUserDefinedFunction
--
--         , requestUpdateWorkflow $
--             newUpdateWorkflow
--
--           ]

--     , testGroup "response"
--         [ responseBatchCreatePartition $
--             newBatchCreatePartitionResponse
--
--         , responseBatchDeleteConnection $
--             newBatchDeleteConnectionResponse
--
--         , responseBatchDeletePartition $
--             newBatchDeletePartitionResponse
--
--         , responseBatchDeleteTable $
--             newBatchDeleteTableResponse
--
--         , responseBatchDeleteTableVersion $
--             newBatchDeleteTableVersionResponse
--
--         , responseBatchGetBlueprints $
--             newBatchGetBlueprintsResponse
--
--         , responseBatchGetCrawlers $
--             newBatchGetCrawlersResponse
--
--         , responseBatchGetCustomEntityTypes $
--             newBatchGetCustomEntityTypesResponse
--
--         , responseBatchGetDataQualityResult $
--             newBatchGetDataQualityResultResponse
--
--         , responseBatchGetDevEndpoints $
--             newBatchGetDevEndpointsResponse
--
--         , responseBatchGetJobs $
--             newBatchGetJobsResponse
--
--         , responseBatchGetPartition $
--             newBatchGetPartitionResponse
--
--         , responseBatchGetTriggers $
--             newBatchGetTriggersResponse
--
--         , responseBatchGetWorkflows $
--             newBatchGetWorkflowsResponse
--
--         , responseBatchStopJobRun $
--             newBatchStopJobRunResponse
--
--         , responseBatchUpdatePartition $
--             newBatchUpdatePartitionResponse
--
--         , responseCancelDataQualityRuleRecommendationRun $
--             newCancelDataQualityRuleRecommendationRunResponse
--
--         , responseCancelDataQualityRulesetEvaluationRun $
--             newCancelDataQualityRulesetEvaluationRunResponse
--
--         , responseCancelMLTaskRun $
--             newCancelMLTaskRunResponse
--
--         , responseCancelStatement $
--             newCancelStatementResponse
--
--         , responseCheckSchemaVersionValidity $
--             newCheckSchemaVersionValidityResponse
--
--         , responseCreateBlueprint $
--             newCreateBlueprintResponse
--
--         , responseCreateClassifier $
--             newCreateClassifierResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseCreateCrawler $
--             newCreateCrawlerResponse
--
--         , responseCreateCustomEntityType $
--             newCreateCustomEntityTypeResponse
--
--         , responseCreateDataQualityRuleset $
--             newCreateDataQualityRulesetResponse
--
--         , responseCreateDatabase $
--             newCreateDatabaseResponse
--
--         , responseCreateDevEndpoint $
--             newCreateDevEndpointResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseCreateMLTransform $
--             newCreateMLTransformResponse
--
--         , responseCreatePartition $
--             newCreatePartitionResponse
--
--         , responseCreatePartitionIndex $
--             newCreatePartitionIndexResponse
--
--         , responseCreateRegistry $
--             newCreateRegistryResponse
--
--         , responseCreateSchema $
--             newCreateSchemaResponse
--
--         , responseCreateScript $
--             newCreateScriptResponse
--
--         , responseCreateSecurityConfiguration $
--             newCreateSecurityConfigurationResponse
--
--         , responseCreateSession $
--             newCreateSessionResponse
--
--         , responseCreateTable $
--             newCreateTableResponse
--
--         , responseCreateTrigger $
--             newCreateTriggerResponse
--
--         , responseCreateUserDefinedFunction $
--             newCreateUserDefinedFunctionResponse
--
--         , responseCreateWorkflow $
--             newCreateWorkflowResponse
--
--         , responseDeleteBlueprint $
--             newDeleteBlueprintResponse
--
--         , responseDeleteClassifier $
--             newDeleteClassifierResponse
--
--         , responseDeleteColumnStatisticsForPartition $
--             newDeleteColumnStatisticsForPartitionResponse
--
--         , responseDeleteColumnStatisticsForTable $
--             newDeleteColumnStatisticsForTableResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseDeleteCrawler $
--             newDeleteCrawlerResponse
--
--         , responseDeleteCustomEntityType $
--             newDeleteCustomEntityTypeResponse
--
--         , responseDeleteDataQualityRuleset $
--             newDeleteDataQualityRulesetResponse
--
--         , responseDeleteDatabase $
--             newDeleteDatabaseResponse
--
--         , responseDeleteDevEndpoint $
--             newDeleteDevEndpointResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseDeleteMLTransform $
--             newDeleteMLTransformResponse
--
--         , responseDeletePartition $
--             newDeletePartitionResponse
--
--         , responseDeletePartitionIndex $
--             newDeletePartitionIndexResponse
--
--         , responseDeleteRegistry $
--             newDeleteRegistryResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteSchema $
--             newDeleteSchemaResponse
--
--         , responseDeleteSchemaVersions $
--             newDeleteSchemaVersionsResponse
--
--         , responseDeleteSecurityConfiguration $
--             newDeleteSecurityConfigurationResponse
--
--         , responseDeleteSession $
--             newDeleteSessionResponse
--
--         , responseDeleteTable $
--             newDeleteTableResponse
--
--         , responseDeleteTableVersion $
--             newDeleteTableVersionResponse
--
--         , responseDeleteTrigger $
--             newDeleteTriggerResponse
--
--         , responseDeleteUserDefinedFunction $
--             newDeleteUserDefinedFunctionResponse
--
--         , responseDeleteWorkflow $
--             newDeleteWorkflowResponse
--
--         , responseGetBlueprint $
--             newGetBlueprintResponse
--
--         , responseGetBlueprintRun $
--             newGetBlueprintRunResponse
--
--         , responseGetBlueprintRuns $
--             newGetBlueprintRunsResponse
--
--         , responseGetCatalogImportStatus $
--             newGetCatalogImportStatusResponse
--
--         , responseGetClassifier $
--             newGetClassifierResponse
--
--         , responseGetClassifiers $
--             newGetClassifiersResponse
--
--         , responseGetColumnStatisticsForPartition $
--             newGetColumnStatisticsForPartitionResponse
--
--         , responseGetColumnStatisticsForTable $
--             newGetColumnStatisticsForTableResponse
--
--         , responseGetConnection $
--             newGetConnectionResponse
--
--         , responseGetConnections $
--             newGetConnectionsResponse
--
--         , responseGetCrawler $
--             newGetCrawlerResponse
--
--         , responseGetCrawlerMetrics $
--             newGetCrawlerMetricsResponse
--
--         , responseGetCrawlers $
--             newGetCrawlersResponse
--
--         , responseGetCustomEntityType $
--             newGetCustomEntityTypeResponse
--
--         , responseGetDataCatalogEncryptionSettings $
--             newGetDataCatalogEncryptionSettingsResponse
--
--         , responseGetDataQualityResult $
--             newGetDataQualityResultResponse
--
--         , responseGetDataQualityRuleRecommendationRun $
--             newGetDataQualityRuleRecommendationRunResponse
--
--         , responseGetDataQualityRuleset $
--             newGetDataQualityRulesetResponse
--
--         , responseGetDataQualityRulesetEvaluationRun $
--             newGetDataQualityRulesetEvaluationRunResponse
--
--         , responseGetDatabase $
--             newGetDatabaseResponse
--
--         , responseGetDatabases $
--             newGetDatabasesResponse
--
--         , responseGetDataflowGraph $
--             newGetDataflowGraphResponse
--
--         , responseGetDevEndpoint $
--             newGetDevEndpointResponse
--
--         , responseGetDevEndpoints $
--             newGetDevEndpointsResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseGetJobBookmark $
--             newGetJobBookmarkResponse
--
--         , responseGetJobRun $
--             newGetJobRunResponse
--
--         , responseGetJobRuns $
--             newGetJobRunsResponse
--
--         , responseGetJobs $
--             newGetJobsResponse
--
--         , responseGetMLTaskRun $
--             newGetMLTaskRunResponse
--
--         , responseGetMLTaskRuns $
--             newGetMLTaskRunsResponse
--
--         , responseGetMLTransform $
--             newGetMLTransformResponse
--
--         , responseGetMLTransforms $
--             newGetMLTransformsResponse
--
--         , responseGetMapping $
--             newGetMappingResponse
--
--         , responseGetPartition $
--             newGetPartitionResponse
--
--         , responseGetPartitionIndexes $
--             newGetPartitionIndexesResponse
--
--         , responseGetPartitions $
--             newGetPartitionsResponse
--
--         , responseGetPlan $
--             newGetPlanResponse
--
--         , responseGetRegistry $
--             newGetRegistryResponse
--
--         , responseGetResourcePolicies $
--             newGetResourcePoliciesResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseGetSchema $
--             newGetSchemaResponse
--
--         , responseGetSchemaByDefinition $
--             newGetSchemaByDefinitionResponse
--
--         , responseGetSchemaVersion $
--             newGetSchemaVersionResponse
--
--         , responseGetSchemaVersionsDiff $
--             newGetSchemaVersionsDiffResponse
--
--         , responseGetSecurityConfiguration $
--             newGetSecurityConfigurationResponse
--
--         , responseGetSecurityConfigurations $
--             newGetSecurityConfigurationsResponse
--
--         , responseGetSession $
--             newGetSessionResponse
--
--         , responseGetStatement $
--             newGetStatementResponse
--
--         , responseGetTable $
--             newGetTableResponse
--
--         , responseGetTableVersion $
--             newGetTableVersionResponse
--
--         , responseGetTableVersions $
--             newGetTableVersionsResponse
--
--         , responseGetTables $
--             newGetTablesResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseGetTrigger $
--             newGetTriggerResponse
--
--         , responseGetTriggers $
--             newGetTriggersResponse
--
--         , responseGetUnfilteredPartitionMetadata $
--             newGetUnfilteredPartitionMetadataResponse
--
--         , responseGetUnfilteredPartitionsMetadata $
--             newGetUnfilteredPartitionsMetadataResponse
--
--         , responseGetUnfilteredTableMetadata $
--             newGetUnfilteredTableMetadataResponse
--
--         , responseGetUserDefinedFunction $
--             newGetUserDefinedFunctionResponse
--
--         , responseGetUserDefinedFunctions $
--             newGetUserDefinedFunctionsResponse
--
--         , responseGetWorkflow $
--             newGetWorkflowResponse
--
--         , responseGetWorkflowRun $
--             newGetWorkflowRunResponse
--
--         , responseGetWorkflowRunProperties $
--             newGetWorkflowRunPropertiesResponse
--
--         , responseGetWorkflowRuns $
--             newGetWorkflowRunsResponse
--
--         , responseImportCatalogToGlue $
--             newImportCatalogToGlueResponse
--
--         , responseListBlueprints $
--             newListBlueprintsResponse
--
--         , responseListCrawlers $
--             newListCrawlersResponse
--
--         , responseListCrawls $
--             newListCrawlsResponse
--
--         , responseListCustomEntityTypes $
--             newListCustomEntityTypesResponse
--
--         , responseListDataQualityResults $
--             newListDataQualityResultsResponse
--
--         , responseListDataQualityRuleRecommendationRuns $
--             newListDataQualityRuleRecommendationRunsResponse
--
--         , responseListDataQualityRulesetEvaluationRuns $
--             newListDataQualityRulesetEvaluationRunsResponse
--
--         , responseListDataQualityRulesets $
--             newListDataQualityRulesetsResponse
--
--         , responseListDevEndpoints $
--             newListDevEndpointsResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseListMLTransforms $
--             newListMLTransformsResponse
--
--         , responseListRegistries $
--             newListRegistriesResponse
--
--         , responseListSchemaVersions $
--             newListSchemaVersionsResponse
--
--         , responseListSchemas $
--             newListSchemasResponse
--
--         , responseListSessions $
--             newListSessionsResponse
--
--         , responseListStatements $
--             newListStatementsResponse
--
--         , responseListTriggers $
--             newListTriggersResponse
--
--         , responseListWorkflows $
--             newListWorkflowsResponse
--
--         , responsePutDataCatalogEncryptionSettings $
--             newPutDataCatalogEncryptionSettingsResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responsePutSchemaVersionMetadata $
--             newPutSchemaVersionMetadataResponse
--
--         , responsePutWorkflowRunProperties $
--             newPutWorkflowRunPropertiesResponse
--
--         , responseQuerySchemaVersionMetadata $
--             newQuerySchemaVersionMetadataResponse
--
--         , responseRegisterSchemaVersion $
--             newRegisterSchemaVersionResponse
--
--         , responseRemoveSchemaVersionMetadata $
--             newRemoveSchemaVersionMetadataResponse
--
--         , responseResetJobBookmark $
--             newResetJobBookmarkResponse
--
--         , responseResumeWorkflowRun $
--             newResumeWorkflowRunResponse
--
--         , responseRunStatement $
--             newRunStatementResponse
--
--         , responseSearchTables $
--             newSearchTablesResponse
--
--         , responseStartBlueprintRun $
--             newStartBlueprintRunResponse
--
--         , responseStartCrawler $
--             newStartCrawlerResponse
--
--         , responseStartCrawlerSchedule $
--             newStartCrawlerScheduleResponse
--
--         , responseStartDataQualityRuleRecommendationRun $
--             newStartDataQualityRuleRecommendationRunResponse
--
--         , responseStartDataQualityRulesetEvaluationRun $
--             newStartDataQualityRulesetEvaluationRunResponse
--
--         , responseStartExportLabelsTaskRun $
--             newStartExportLabelsTaskRunResponse
--
--         , responseStartImportLabelsTaskRun $
--             newStartImportLabelsTaskRunResponse
--
--         , responseStartJobRun $
--             newStartJobRunResponse
--
--         , responseStartMLEvaluationTaskRun $
--             newStartMLEvaluationTaskRunResponse
--
--         , responseStartMLLabelingSetGenerationTaskRun $
--             newStartMLLabelingSetGenerationTaskRunResponse
--
--         , responseStartTrigger $
--             newStartTriggerResponse
--
--         , responseStartWorkflowRun $
--             newStartWorkflowRunResponse
--
--         , responseStopCrawler $
--             newStopCrawlerResponse
--
--         , responseStopCrawlerSchedule $
--             newStopCrawlerScheduleResponse
--
--         , responseStopSession $
--             newStopSessionResponse
--
--         , responseStopTrigger $
--             newStopTriggerResponse
--
--         , responseStopWorkflowRun $
--             newStopWorkflowRunResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateBlueprint $
--             newUpdateBlueprintResponse
--
--         , responseUpdateClassifier $
--             newUpdateClassifierResponse
--
--         , responseUpdateColumnStatisticsForPartition $
--             newUpdateColumnStatisticsForPartitionResponse
--
--         , responseUpdateColumnStatisticsForTable $
--             newUpdateColumnStatisticsForTableResponse
--
--         , responseUpdateConnection $
--             newUpdateConnectionResponse
--
--         , responseUpdateCrawler $
--             newUpdateCrawlerResponse
--
--         , responseUpdateCrawlerSchedule $
--             newUpdateCrawlerScheduleResponse
--
--         , responseUpdateDataQualityRuleset $
--             newUpdateDataQualityRulesetResponse
--
--         , responseUpdateDatabase $
--             newUpdateDatabaseResponse
--
--         , responseUpdateDevEndpoint $
--             newUpdateDevEndpointResponse
--
--         , responseUpdateJob $
--             newUpdateJobResponse
--
--         , responseUpdateJobFromSourceControl $
--             newUpdateJobFromSourceControlResponse
--
--         , responseUpdateMLTransform $
--             newUpdateMLTransformResponse
--
--         , responseUpdatePartition $
--             newUpdatePartitionResponse
--
--         , responseUpdateRegistry $
--             newUpdateRegistryResponse
--
--         , responseUpdateSchema $
--             newUpdateSchemaResponse
--
--         , responseUpdateSourceControlFromJob $
--             newUpdateSourceControlFromJobResponse
--
--         , responseUpdateTable $
--             newUpdateTableResponse
--
--         , responseUpdateTrigger $
--             newUpdateTriggerResponse
--
--         , responseUpdateUserDefinedFunction $
--             newUpdateUserDefinedFunctionResponse
--
--         , responseUpdateWorkflow $
--             newUpdateWorkflowResponse
--
--           ]
--     ]

-- Requests

requestBatchCreatePartition :: BatchCreatePartition -> TestTree
requestBatchCreatePartition =
  req
    "BatchCreatePartition"
    "fixture/BatchCreatePartition.yaml"

requestBatchDeleteConnection :: BatchDeleteConnection -> TestTree
requestBatchDeleteConnection =
  req
    "BatchDeleteConnection"
    "fixture/BatchDeleteConnection.yaml"

requestBatchDeletePartition :: BatchDeletePartition -> TestTree
requestBatchDeletePartition =
  req
    "BatchDeletePartition"
    "fixture/BatchDeletePartition.yaml"

requestBatchDeleteTable :: BatchDeleteTable -> TestTree
requestBatchDeleteTable =
  req
    "BatchDeleteTable"
    "fixture/BatchDeleteTable.yaml"

requestBatchDeleteTableVersion :: BatchDeleteTableVersion -> TestTree
requestBatchDeleteTableVersion =
  req
    "BatchDeleteTableVersion"
    "fixture/BatchDeleteTableVersion.yaml"

requestBatchGetBlueprints :: BatchGetBlueprints -> TestTree
requestBatchGetBlueprints =
  req
    "BatchGetBlueprints"
    "fixture/BatchGetBlueprints.yaml"

requestBatchGetCrawlers :: BatchGetCrawlers -> TestTree
requestBatchGetCrawlers =
  req
    "BatchGetCrawlers"
    "fixture/BatchGetCrawlers.yaml"

requestBatchGetCustomEntityTypes :: BatchGetCustomEntityTypes -> TestTree
requestBatchGetCustomEntityTypes =
  req
    "BatchGetCustomEntityTypes"
    "fixture/BatchGetCustomEntityTypes.yaml"

requestBatchGetDataQualityResult :: BatchGetDataQualityResult -> TestTree
requestBatchGetDataQualityResult =
  req
    "BatchGetDataQualityResult"
    "fixture/BatchGetDataQualityResult.yaml"

requestBatchGetDevEndpoints :: BatchGetDevEndpoints -> TestTree
requestBatchGetDevEndpoints =
  req
    "BatchGetDevEndpoints"
    "fixture/BatchGetDevEndpoints.yaml"

requestBatchGetJobs :: BatchGetJobs -> TestTree
requestBatchGetJobs =
  req
    "BatchGetJobs"
    "fixture/BatchGetJobs.yaml"

requestBatchGetPartition :: BatchGetPartition -> TestTree
requestBatchGetPartition =
  req
    "BatchGetPartition"
    "fixture/BatchGetPartition.yaml"

requestBatchGetTriggers :: BatchGetTriggers -> TestTree
requestBatchGetTriggers =
  req
    "BatchGetTriggers"
    "fixture/BatchGetTriggers.yaml"

requestBatchGetWorkflows :: BatchGetWorkflows -> TestTree
requestBatchGetWorkflows =
  req
    "BatchGetWorkflows"
    "fixture/BatchGetWorkflows.yaml"

requestBatchStopJobRun :: BatchStopJobRun -> TestTree
requestBatchStopJobRun =
  req
    "BatchStopJobRun"
    "fixture/BatchStopJobRun.yaml"

requestBatchUpdatePartition :: BatchUpdatePartition -> TestTree
requestBatchUpdatePartition =
  req
    "BatchUpdatePartition"
    "fixture/BatchUpdatePartition.yaml"

requestCancelDataQualityRuleRecommendationRun :: CancelDataQualityRuleRecommendationRun -> TestTree
requestCancelDataQualityRuleRecommendationRun =
  req
    "CancelDataQualityRuleRecommendationRun"
    "fixture/CancelDataQualityRuleRecommendationRun.yaml"

requestCancelDataQualityRulesetEvaluationRun :: CancelDataQualityRulesetEvaluationRun -> TestTree
requestCancelDataQualityRulesetEvaluationRun =
  req
    "CancelDataQualityRulesetEvaluationRun"
    "fixture/CancelDataQualityRulesetEvaluationRun.yaml"

requestCancelMLTaskRun :: CancelMLTaskRun -> TestTree
requestCancelMLTaskRun =
  req
    "CancelMLTaskRun"
    "fixture/CancelMLTaskRun.yaml"

requestCancelStatement :: CancelStatement -> TestTree
requestCancelStatement =
  req
    "CancelStatement"
    "fixture/CancelStatement.yaml"

requestCheckSchemaVersionValidity :: CheckSchemaVersionValidity -> TestTree
requestCheckSchemaVersionValidity =
  req
    "CheckSchemaVersionValidity"
    "fixture/CheckSchemaVersionValidity.yaml"

requestCreateBlueprint :: CreateBlueprint -> TestTree
requestCreateBlueprint =
  req
    "CreateBlueprint"
    "fixture/CreateBlueprint.yaml"

requestCreateClassifier :: CreateClassifier -> TestTree
requestCreateClassifier =
  req
    "CreateClassifier"
    "fixture/CreateClassifier.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestCreateCrawler :: CreateCrawler -> TestTree
requestCreateCrawler =
  req
    "CreateCrawler"
    "fixture/CreateCrawler.yaml"

requestCreateCustomEntityType :: CreateCustomEntityType -> TestTree
requestCreateCustomEntityType =
  req
    "CreateCustomEntityType"
    "fixture/CreateCustomEntityType.yaml"

requestCreateDataQualityRuleset :: CreateDataQualityRuleset -> TestTree
requestCreateDataQualityRuleset =
  req
    "CreateDataQualityRuleset"
    "fixture/CreateDataQualityRuleset.yaml"

requestCreateDatabase :: CreateDatabase -> TestTree
requestCreateDatabase =
  req
    "CreateDatabase"
    "fixture/CreateDatabase.yaml"

requestCreateDevEndpoint :: CreateDevEndpoint -> TestTree
requestCreateDevEndpoint =
  req
    "CreateDevEndpoint"
    "fixture/CreateDevEndpoint.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestCreateMLTransform :: CreateMLTransform -> TestTree
requestCreateMLTransform =
  req
    "CreateMLTransform"
    "fixture/CreateMLTransform.yaml"

requestCreatePartition :: CreatePartition -> TestTree
requestCreatePartition =
  req
    "CreatePartition"
    "fixture/CreatePartition.yaml"

requestCreatePartitionIndex :: CreatePartitionIndex -> TestTree
requestCreatePartitionIndex =
  req
    "CreatePartitionIndex"
    "fixture/CreatePartitionIndex.yaml"

requestCreateRegistry :: CreateRegistry -> TestTree
requestCreateRegistry =
  req
    "CreateRegistry"
    "fixture/CreateRegistry.yaml"

requestCreateSchema :: CreateSchema -> TestTree
requestCreateSchema =
  req
    "CreateSchema"
    "fixture/CreateSchema.yaml"

requestCreateScript :: CreateScript -> TestTree
requestCreateScript =
  req
    "CreateScript"
    "fixture/CreateScript.yaml"

requestCreateSecurityConfiguration :: CreateSecurityConfiguration -> TestTree
requestCreateSecurityConfiguration =
  req
    "CreateSecurityConfiguration"
    "fixture/CreateSecurityConfiguration.yaml"

requestCreateSession :: CreateSession -> TestTree
requestCreateSession =
  req
    "CreateSession"
    "fixture/CreateSession.yaml"

requestCreateTable :: CreateTable -> TestTree
requestCreateTable =
  req
    "CreateTable"
    "fixture/CreateTable.yaml"

requestCreateTrigger :: CreateTrigger -> TestTree
requestCreateTrigger =
  req
    "CreateTrigger"
    "fixture/CreateTrigger.yaml"

requestCreateUserDefinedFunction :: CreateUserDefinedFunction -> TestTree
requestCreateUserDefinedFunction =
  req
    "CreateUserDefinedFunction"
    "fixture/CreateUserDefinedFunction.yaml"

requestCreateWorkflow :: CreateWorkflow -> TestTree
requestCreateWorkflow =
  req
    "CreateWorkflow"
    "fixture/CreateWorkflow.yaml"

requestDeleteBlueprint :: DeleteBlueprint -> TestTree
requestDeleteBlueprint =
  req
    "DeleteBlueprint"
    "fixture/DeleteBlueprint.yaml"

requestDeleteClassifier :: DeleteClassifier -> TestTree
requestDeleteClassifier =
  req
    "DeleteClassifier"
    "fixture/DeleteClassifier.yaml"

requestDeleteColumnStatisticsForPartition :: DeleteColumnStatisticsForPartition -> TestTree
requestDeleteColumnStatisticsForPartition =
  req
    "DeleteColumnStatisticsForPartition"
    "fixture/DeleteColumnStatisticsForPartition.yaml"

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

requestDeleteCrawler :: DeleteCrawler -> TestTree
requestDeleteCrawler =
  req
    "DeleteCrawler"
    "fixture/DeleteCrawler.yaml"

requestDeleteCustomEntityType :: DeleteCustomEntityType -> TestTree
requestDeleteCustomEntityType =
  req
    "DeleteCustomEntityType"
    "fixture/DeleteCustomEntityType.yaml"

requestDeleteDataQualityRuleset :: DeleteDataQualityRuleset -> TestTree
requestDeleteDataQualityRuleset =
  req
    "DeleteDataQualityRuleset"
    "fixture/DeleteDataQualityRuleset.yaml"

requestDeleteDatabase :: DeleteDatabase -> TestTree
requestDeleteDatabase =
  req
    "DeleteDatabase"
    "fixture/DeleteDatabase.yaml"

requestDeleteDevEndpoint :: DeleteDevEndpoint -> TestTree
requestDeleteDevEndpoint =
  req
    "DeleteDevEndpoint"
    "fixture/DeleteDevEndpoint.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestDeleteMLTransform :: DeleteMLTransform -> TestTree
requestDeleteMLTransform =
  req
    "DeleteMLTransform"
    "fixture/DeleteMLTransform.yaml"

requestDeletePartition :: DeletePartition -> TestTree
requestDeletePartition =
  req
    "DeletePartition"
    "fixture/DeletePartition.yaml"

requestDeletePartitionIndex :: DeletePartitionIndex -> TestTree
requestDeletePartitionIndex =
  req
    "DeletePartitionIndex"
    "fixture/DeletePartitionIndex.yaml"

requestDeleteRegistry :: DeleteRegistry -> TestTree
requestDeleteRegistry =
  req
    "DeleteRegistry"
    "fixture/DeleteRegistry.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema =
  req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestDeleteSchemaVersions :: DeleteSchemaVersions -> TestTree
requestDeleteSchemaVersions =
  req
    "DeleteSchemaVersions"
    "fixture/DeleteSchemaVersions.yaml"

requestDeleteSecurityConfiguration :: DeleteSecurityConfiguration -> TestTree
requestDeleteSecurityConfiguration =
  req
    "DeleteSecurityConfiguration"
    "fixture/DeleteSecurityConfiguration.yaml"

requestDeleteSession :: DeleteSession -> TestTree
requestDeleteSession =
  req
    "DeleteSession"
    "fixture/DeleteSession.yaml"

requestDeleteTable :: DeleteTable -> TestTree
requestDeleteTable =
  req
    "DeleteTable"
    "fixture/DeleteTable.yaml"

requestDeleteTableVersion :: DeleteTableVersion -> TestTree
requestDeleteTableVersion =
  req
    "DeleteTableVersion"
    "fixture/DeleteTableVersion.yaml"

requestDeleteTrigger :: DeleteTrigger -> TestTree
requestDeleteTrigger =
  req
    "DeleteTrigger"
    "fixture/DeleteTrigger.yaml"

requestDeleteUserDefinedFunction :: DeleteUserDefinedFunction -> TestTree
requestDeleteUserDefinedFunction =
  req
    "DeleteUserDefinedFunction"
    "fixture/DeleteUserDefinedFunction.yaml"

requestDeleteWorkflow :: DeleteWorkflow -> TestTree
requestDeleteWorkflow =
  req
    "DeleteWorkflow"
    "fixture/DeleteWorkflow.yaml"

requestGetBlueprint :: GetBlueprint -> TestTree
requestGetBlueprint =
  req
    "GetBlueprint"
    "fixture/GetBlueprint.yaml"

requestGetBlueprintRun :: GetBlueprintRun -> TestTree
requestGetBlueprintRun =
  req
    "GetBlueprintRun"
    "fixture/GetBlueprintRun.yaml"

requestGetBlueprintRuns :: GetBlueprintRuns -> TestTree
requestGetBlueprintRuns =
  req
    "GetBlueprintRuns"
    "fixture/GetBlueprintRuns.yaml"

requestGetCatalogImportStatus :: GetCatalogImportStatus -> TestTree
requestGetCatalogImportStatus =
  req
    "GetCatalogImportStatus"
    "fixture/GetCatalogImportStatus.yaml"

requestGetClassifier :: GetClassifier -> TestTree
requestGetClassifier =
  req
    "GetClassifier"
    "fixture/GetClassifier.yaml"

requestGetClassifiers :: GetClassifiers -> TestTree
requestGetClassifiers =
  req
    "GetClassifiers"
    "fixture/GetClassifiers.yaml"

requestGetColumnStatisticsForPartition :: GetColumnStatisticsForPartition -> TestTree
requestGetColumnStatisticsForPartition =
  req
    "GetColumnStatisticsForPartition"
    "fixture/GetColumnStatisticsForPartition.yaml"

requestGetColumnStatisticsForTable :: GetColumnStatisticsForTable -> TestTree
requestGetColumnStatisticsForTable =
  req
    "GetColumnStatisticsForTable"
    "fixture/GetColumnStatisticsForTable.yaml"

requestGetConnection :: GetConnection -> TestTree
requestGetConnection =
  req
    "GetConnection"
    "fixture/GetConnection.yaml"

requestGetConnections :: GetConnections -> TestTree
requestGetConnections =
  req
    "GetConnections"
    "fixture/GetConnections.yaml"

requestGetCrawler :: GetCrawler -> TestTree
requestGetCrawler =
  req
    "GetCrawler"
    "fixture/GetCrawler.yaml"

requestGetCrawlerMetrics :: GetCrawlerMetrics -> TestTree
requestGetCrawlerMetrics =
  req
    "GetCrawlerMetrics"
    "fixture/GetCrawlerMetrics.yaml"

requestGetCrawlers :: GetCrawlers -> TestTree
requestGetCrawlers =
  req
    "GetCrawlers"
    "fixture/GetCrawlers.yaml"

requestGetCustomEntityType :: GetCustomEntityType -> TestTree
requestGetCustomEntityType =
  req
    "GetCustomEntityType"
    "fixture/GetCustomEntityType.yaml"

requestGetDataCatalogEncryptionSettings :: GetDataCatalogEncryptionSettings -> TestTree
requestGetDataCatalogEncryptionSettings =
  req
    "GetDataCatalogEncryptionSettings"
    "fixture/GetDataCatalogEncryptionSettings.yaml"

requestGetDataQualityResult :: GetDataQualityResult -> TestTree
requestGetDataQualityResult =
  req
    "GetDataQualityResult"
    "fixture/GetDataQualityResult.yaml"

requestGetDataQualityRuleRecommendationRun :: GetDataQualityRuleRecommendationRun -> TestTree
requestGetDataQualityRuleRecommendationRun =
  req
    "GetDataQualityRuleRecommendationRun"
    "fixture/GetDataQualityRuleRecommendationRun.yaml"

requestGetDataQualityRuleset :: GetDataQualityRuleset -> TestTree
requestGetDataQualityRuleset =
  req
    "GetDataQualityRuleset"
    "fixture/GetDataQualityRuleset.yaml"

requestGetDataQualityRulesetEvaluationRun :: GetDataQualityRulesetEvaluationRun -> TestTree
requestGetDataQualityRulesetEvaluationRun =
  req
    "GetDataQualityRulesetEvaluationRun"
    "fixture/GetDataQualityRulesetEvaluationRun.yaml"

requestGetDatabase :: GetDatabase -> TestTree
requestGetDatabase =
  req
    "GetDatabase"
    "fixture/GetDatabase.yaml"

requestGetDatabases :: GetDatabases -> TestTree
requestGetDatabases =
  req
    "GetDatabases"
    "fixture/GetDatabases.yaml"

requestGetDataflowGraph :: GetDataflowGraph -> TestTree
requestGetDataflowGraph =
  req
    "GetDataflowGraph"
    "fixture/GetDataflowGraph.yaml"

requestGetDevEndpoint :: GetDevEndpoint -> TestTree
requestGetDevEndpoint =
  req
    "GetDevEndpoint"
    "fixture/GetDevEndpoint.yaml"

requestGetDevEndpoints :: GetDevEndpoints -> TestTree
requestGetDevEndpoints =
  req
    "GetDevEndpoints"
    "fixture/GetDevEndpoints.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestGetJobBookmark :: GetJobBookmark -> TestTree
requestGetJobBookmark =
  req
    "GetJobBookmark"
    "fixture/GetJobBookmark.yaml"

requestGetJobRun :: GetJobRun -> TestTree
requestGetJobRun =
  req
    "GetJobRun"
    "fixture/GetJobRun.yaml"

requestGetJobRuns :: GetJobRuns -> TestTree
requestGetJobRuns =
  req
    "GetJobRuns"
    "fixture/GetJobRuns.yaml"

requestGetJobs :: GetJobs -> TestTree
requestGetJobs =
  req
    "GetJobs"
    "fixture/GetJobs.yaml"

requestGetMLTaskRun :: GetMLTaskRun -> TestTree
requestGetMLTaskRun =
  req
    "GetMLTaskRun"
    "fixture/GetMLTaskRun.yaml"

requestGetMLTaskRuns :: GetMLTaskRuns -> TestTree
requestGetMLTaskRuns =
  req
    "GetMLTaskRuns"
    "fixture/GetMLTaskRuns.yaml"

requestGetMLTransform :: GetMLTransform -> TestTree
requestGetMLTransform =
  req
    "GetMLTransform"
    "fixture/GetMLTransform.yaml"

requestGetMLTransforms :: GetMLTransforms -> TestTree
requestGetMLTransforms =
  req
    "GetMLTransforms"
    "fixture/GetMLTransforms.yaml"

requestGetMapping :: GetMapping -> TestTree
requestGetMapping =
  req
    "GetMapping"
    "fixture/GetMapping.yaml"

requestGetPartition :: GetPartition -> TestTree
requestGetPartition =
  req
    "GetPartition"
    "fixture/GetPartition.yaml"

requestGetPartitionIndexes :: GetPartitionIndexes -> TestTree
requestGetPartitionIndexes =
  req
    "GetPartitionIndexes"
    "fixture/GetPartitionIndexes.yaml"

requestGetPartitions :: GetPartitions -> TestTree
requestGetPartitions =
  req
    "GetPartitions"
    "fixture/GetPartitions.yaml"

requestGetPlan :: GetPlan -> TestTree
requestGetPlan =
  req
    "GetPlan"
    "fixture/GetPlan.yaml"

requestGetRegistry :: GetRegistry -> TestTree
requestGetRegistry =
  req
    "GetRegistry"
    "fixture/GetRegistry.yaml"

requestGetResourcePolicies :: GetResourcePolicies -> TestTree
requestGetResourcePolicies =
  req
    "GetResourcePolicies"
    "fixture/GetResourcePolicies.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestGetSchema :: GetSchema -> TestTree
requestGetSchema =
  req
    "GetSchema"
    "fixture/GetSchema.yaml"

requestGetSchemaByDefinition :: GetSchemaByDefinition -> TestTree
requestGetSchemaByDefinition =
  req
    "GetSchemaByDefinition"
    "fixture/GetSchemaByDefinition.yaml"

requestGetSchemaVersion :: GetSchemaVersion -> TestTree
requestGetSchemaVersion =
  req
    "GetSchemaVersion"
    "fixture/GetSchemaVersion.yaml"

requestGetSchemaVersionsDiff :: GetSchemaVersionsDiff -> TestTree
requestGetSchemaVersionsDiff =
  req
    "GetSchemaVersionsDiff"
    "fixture/GetSchemaVersionsDiff.yaml"

requestGetSecurityConfiguration :: GetSecurityConfiguration -> TestTree
requestGetSecurityConfiguration =
  req
    "GetSecurityConfiguration"
    "fixture/GetSecurityConfiguration.yaml"

requestGetSecurityConfigurations :: GetSecurityConfigurations -> TestTree
requestGetSecurityConfigurations =
  req
    "GetSecurityConfigurations"
    "fixture/GetSecurityConfigurations.yaml"

requestGetSession :: GetSession -> TestTree
requestGetSession =
  req
    "GetSession"
    "fixture/GetSession.yaml"

requestGetStatement :: GetStatement -> TestTree
requestGetStatement =
  req
    "GetStatement"
    "fixture/GetStatement.yaml"

requestGetTable :: GetTable -> TestTree
requestGetTable =
  req
    "GetTable"
    "fixture/GetTable.yaml"

requestGetTableVersion :: GetTableVersion -> TestTree
requestGetTableVersion =
  req
    "GetTableVersion"
    "fixture/GetTableVersion.yaml"

requestGetTableVersions :: GetTableVersions -> TestTree
requestGetTableVersions =
  req
    "GetTableVersions"
    "fixture/GetTableVersions.yaml"

requestGetTables :: GetTables -> TestTree
requestGetTables =
  req
    "GetTables"
    "fixture/GetTables.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags =
  req
    "GetTags"
    "fixture/GetTags.yaml"

requestGetTrigger :: GetTrigger -> TestTree
requestGetTrigger =
  req
    "GetTrigger"
    "fixture/GetTrigger.yaml"

requestGetTriggers :: GetTriggers -> TestTree
requestGetTriggers =
  req
    "GetTriggers"
    "fixture/GetTriggers.yaml"

requestGetUnfilteredPartitionMetadata :: GetUnfilteredPartitionMetadata -> TestTree
requestGetUnfilteredPartitionMetadata =
  req
    "GetUnfilteredPartitionMetadata"
    "fixture/GetUnfilteredPartitionMetadata.yaml"

requestGetUnfilteredPartitionsMetadata :: GetUnfilteredPartitionsMetadata -> TestTree
requestGetUnfilteredPartitionsMetadata =
  req
    "GetUnfilteredPartitionsMetadata"
    "fixture/GetUnfilteredPartitionsMetadata.yaml"

requestGetUnfilteredTableMetadata :: GetUnfilteredTableMetadata -> TestTree
requestGetUnfilteredTableMetadata =
  req
    "GetUnfilteredTableMetadata"
    "fixture/GetUnfilteredTableMetadata.yaml"

requestGetUserDefinedFunction :: GetUserDefinedFunction -> TestTree
requestGetUserDefinedFunction =
  req
    "GetUserDefinedFunction"
    "fixture/GetUserDefinedFunction.yaml"

requestGetUserDefinedFunctions :: GetUserDefinedFunctions -> TestTree
requestGetUserDefinedFunctions =
  req
    "GetUserDefinedFunctions"
    "fixture/GetUserDefinedFunctions.yaml"

requestGetWorkflow :: GetWorkflow -> TestTree
requestGetWorkflow =
  req
    "GetWorkflow"
    "fixture/GetWorkflow.yaml"

requestGetWorkflowRun :: GetWorkflowRun -> TestTree
requestGetWorkflowRun =
  req
    "GetWorkflowRun"
    "fixture/GetWorkflowRun.yaml"

requestGetWorkflowRunProperties :: GetWorkflowRunProperties -> TestTree
requestGetWorkflowRunProperties =
  req
    "GetWorkflowRunProperties"
    "fixture/GetWorkflowRunProperties.yaml"

requestGetWorkflowRuns :: GetWorkflowRuns -> TestTree
requestGetWorkflowRuns =
  req
    "GetWorkflowRuns"
    "fixture/GetWorkflowRuns.yaml"

requestImportCatalogToGlue :: ImportCatalogToGlue -> TestTree
requestImportCatalogToGlue =
  req
    "ImportCatalogToGlue"
    "fixture/ImportCatalogToGlue.yaml"

requestListBlueprints :: ListBlueprints -> TestTree
requestListBlueprints =
  req
    "ListBlueprints"
    "fixture/ListBlueprints.yaml"

requestListCrawlers :: ListCrawlers -> TestTree
requestListCrawlers =
  req
    "ListCrawlers"
    "fixture/ListCrawlers.yaml"

requestListCrawls :: ListCrawls -> TestTree
requestListCrawls =
  req
    "ListCrawls"
    "fixture/ListCrawls.yaml"

requestListCustomEntityTypes :: ListCustomEntityTypes -> TestTree
requestListCustomEntityTypes =
  req
    "ListCustomEntityTypes"
    "fixture/ListCustomEntityTypes.yaml"

requestListDataQualityResults :: ListDataQualityResults -> TestTree
requestListDataQualityResults =
  req
    "ListDataQualityResults"
    "fixture/ListDataQualityResults.yaml"

requestListDataQualityRuleRecommendationRuns :: ListDataQualityRuleRecommendationRuns -> TestTree
requestListDataQualityRuleRecommendationRuns =
  req
    "ListDataQualityRuleRecommendationRuns"
    "fixture/ListDataQualityRuleRecommendationRuns.yaml"

requestListDataQualityRulesetEvaluationRuns :: ListDataQualityRulesetEvaluationRuns -> TestTree
requestListDataQualityRulesetEvaluationRuns =
  req
    "ListDataQualityRulesetEvaluationRuns"
    "fixture/ListDataQualityRulesetEvaluationRuns.yaml"

requestListDataQualityRulesets :: ListDataQualityRulesets -> TestTree
requestListDataQualityRulesets =
  req
    "ListDataQualityRulesets"
    "fixture/ListDataQualityRulesets.yaml"

requestListDevEndpoints :: ListDevEndpoints -> TestTree
requestListDevEndpoints =
  req
    "ListDevEndpoints"
    "fixture/ListDevEndpoints.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListMLTransforms :: ListMLTransforms -> TestTree
requestListMLTransforms =
  req
    "ListMLTransforms"
    "fixture/ListMLTransforms.yaml"

requestListRegistries :: ListRegistries -> TestTree
requestListRegistries =
  req
    "ListRegistries"
    "fixture/ListRegistries.yaml"

requestListSchemaVersions :: ListSchemaVersions -> TestTree
requestListSchemaVersions =
  req
    "ListSchemaVersions"
    "fixture/ListSchemaVersions.yaml"

requestListSchemas :: ListSchemas -> TestTree
requestListSchemas =
  req
    "ListSchemas"
    "fixture/ListSchemas.yaml"

requestListSessions :: ListSessions -> TestTree
requestListSessions =
  req
    "ListSessions"
    "fixture/ListSessions.yaml"

requestListStatements :: ListStatements -> TestTree
requestListStatements =
  req
    "ListStatements"
    "fixture/ListStatements.yaml"

requestListTriggers :: ListTriggers -> TestTree
requestListTriggers =
  req
    "ListTriggers"
    "fixture/ListTriggers.yaml"

requestListWorkflows :: ListWorkflows -> TestTree
requestListWorkflows =
  req
    "ListWorkflows"
    "fixture/ListWorkflows.yaml"

requestPutDataCatalogEncryptionSettings :: PutDataCatalogEncryptionSettings -> TestTree
requestPutDataCatalogEncryptionSettings =
  req
    "PutDataCatalogEncryptionSettings"
    "fixture/PutDataCatalogEncryptionSettings.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestPutSchemaVersionMetadata :: PutSchemaVersionMetadata -> TestTree
requestPutSchemaVersionMetadata =
  req
    "PutSchemaVersionMetadata"
    "fixture/PutSchemaVersionMetadata.yaml"

requestPutWorkflowRunProperties :: PutWorkflowRunProperties -> TestTree
requestPutWorkflowRunProperties =
  req
    "PutWorkflowRunProperties"
    "fixture/PutWorkflowRunProperties.yaml"

requestQuerySchemaVersionMetadata :: QuerySchemaVersionMetadata -> TestTree
requestQuerySchemaVersionMetadata =
  req
    "QuerySchemaVersionMetadata"
    "fixture/QuerySchemaVersionMetadata.yaml"

requestRegisterSchemaVersion :: RegisterSchemaVersion -> TestTree
requestRegisterSchemaVersion =
  req
    "RegisterSchemaVersion"
    "fixture/RegisterSchemaVersion.yaml"

requestRemoveSchemaVersionMetadata :: RemoveSchemaVersionMetadata -> TestTree
requestRemoveSchemaVersionMetadata =
  req
    "RemoveSchemaVersionMetadata"
    "fixture/RemoveSchemaVersionMetadata.yaml"

requestResetJobBookmark :: ResetJobBookmark -> TestTree
requestResetJobBookmark =
  req
    "ResetJobBookmark"
    "fixture/ResetJobBookmark.yaml"

requestResumeWorkflowRun :: ResumeWorkflowRun -> TestTree
requestResumeWorkflowRun =
  req
    "ResumeWorkflowRun"
    "fixture/ResumeWorkflowRun.yaml"

requestRunStatement :: RunStatement -> TestTree
requestRunStatement =
  req
    "RunStatement"
    "fixture/RunStatement.yaml"

requestSearchTables :: SearchTables -> TestTree
requestSearchTables =
  req
    "SearchTables"
    "fixture/SearchTables.yaml"

requestStartBlueprintRun :: StartBlueprintRun -> TestTree
requestStartBlueprintRun =
  req
    "StartBlueprintRun"
    "fixture/StartBlueprintRun.yaml"

requestStartCrawler :: StartCrawler -> TestTree
requestStartCrawler =
  req
    "StartCrawler"
    "fixture/StartCrawler.yaml"

requestStartCrawlerSchedule :: StartCrawlerSchedule -> TestTree
requestStartCrawlerSchedule =
  req
    "StartCrawlerSchedule"
    "fixture/StartCrawlerSchedule.yaml"

requestStartDataQualityRuleRecommendationRun :: StartDataQualityRuleRecommendationRun -> TestTree
requestStartDataQualityRuleRecommendationRun =
  req
    "StartDataQualityRuleRecommendationRun"
    "fixture/StartDataQualityRuleRecommendationRun.yaml"

requestStartDataQualityRulesetEvaluationRun :: StartDataQualityRulesetEvaluationRun -> TestTree
requestStartDataQualityRulesetEvaluationRun =
  req
    "StartDataQualityRulesetEvaluationRun"
    "fixture/StartDataQualityRulesetEvaluationRun.yaml"

requestStartExportLabelsTaskRun :: StartExportLabelsTaskRun -> TestTree
requestStartExportLabelsTaskRun =
  req
    "StartExportLabelsTaskRun"
    "fixture/StartExportLabelsTaskRun.yaml"

requestStartImportLabelsTaskRun :: StartImportLabelsTaskRun -> TestTree
requestStartImportLabelsTaskRun =
  req
    "StartImportLabelsTaskRun"
    "fixture/StartImportLabelsTaskRun.yaml"

requestStartJobRun :: StartJobRun -> TestTree
requestStartJobRun =
  req
    "StartJobRun"
    "fixture/StartJobRun.yaml"

requestStartMLEvaluationTaskRun :: StartMLEvaluationTaskRun -> TestTree
requestStartMLEvaluationTaskRun =
  req
    "StartMLEvaluationTaskRun"
    "fixture/StartMLEvaluationTaskRun.yaml"

requestStartMLLabelingSetGenerationTaskRun :: StartMLLabelingSetGenerationTaskRun -> TestTree
requestStartMLLabelingSetGenerationTaskRun =
  req
    "StartMLLabelingSetGenerationTaskRun"
    "fixture/StartMLLabelingSetGenerationTaskRun.yaml"

requestStartTrigger :: StartTrigger -> TestTree
requestStartTrigger =
  req
    "StartTrigger"
    "fixture/StartTrigger.yaml"

requestStartWorkflowRun :: StartWorkflowRun -> TestTree
requestStartWorkflowRun =
  req
    "StartWorkflowRun"
    "fixture/StartWorkflowRun.yaml"

requestStopCrawler :: StopCrawler -> TestTree
requestStopCrawler =
  req
    "StopCrawler"
    "fixture/StopCrawler.yaml"

requestStopCrawlerSchedule :: StopCrawlerSchedule -> TestTree
requestStopCrawlerSchedule =
  req
    "StopCrawlerSchedule"
    "fixture/StopCrawlerSchedule.yaml"

requestStopSession :: StopSession -> TestTree
requestStopSession =
  req
    "StopSession"
    "fixture/StopSession.yaml"

requestStopTrigger :: StopTrigger -> TestTree
requestStopTrigger =
  req
    "StopTrigger"
    "fixture/StopTrigger.yaml"

requestStopWorkflowRun :: StopWorkflowRun -> TestTree
requestStopWorkflowRun =
  req
    "StopWorkflowRun"
    "fixture/StopWorkflowRun.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateBlueprint :: UpdateBlueprint -> TestTree
requestUpdateBlueprint =
  req
    "UpdateBlueprint"
    "fixture/UpdateBlueprint.yaml"

requestUpdateClassifier :: UpdateClassifier -> TestTree
requestUpdateClassifier =
  req
    "UpdateClassifier"
    "fixture/UpdateClassifier.yaml"

requestUpdateColumnStatisticsForPartition :: UpdateColumnStatisticsForPartition -> TestTree
requestUpdateColumnStatisticsForPartition =
  req
    "UpdateColumnStatisticsForPartition"
    "fixture/UpdateColumnStatisticsForPartition.yaml"

requestUpdateColumnStatisticsForTable :: UpdateColumnStatisticsForTable -> TestTree
requestUpdateColumnStatisticsForTable =
  req
    "UpdateColumnStatisticsForTable"
    "fixture/UpdateColumnStatisticsForTable.yaml"

requestUpdateConnection :: UpdateConnection -> TestTree
requestUpdateConnection =
  req
    "UpdateConnection"
    "fixture/UpdateConnection.yaml"

requestUpdateCrawler :: UpdateCrawler -> TestTree
requestUpdateCrawler =
  req
    "UpdateCrawler"
    "fixture/UpdateCrawler.yaml"

requestUpdateCrawlerSchedule :: UpdateCrawlerSchedule -> TestTree
requestUpdateCrawlerSchedule =
  req
    "UpdateCrawlerSchedule"
    "fixture/UpdateCrawlerSchedule.yaml"

requestUpdateDataQualityRuleset :: UpdateDataQualityRuleset -> TestTree
requestUpdateDataQualityRuleset =
  req
    "UpdateDataQualityRuleset"
    "fixture/UpdateDataQualityRuleset.yaml"

requestUpdateDatabase :: UpdateDatabase -> TestTree
requestUpdateDatabase =
  req
    "UpdateDatabase"
    "fixture/UpdateDatabase.yaml"

requestUpdateDevEndpoint :: UpdateDevEndpoint -> TestTree
requestUpdateDevEndpoint =
  req
    "UpdateDevEndpoint"
    "fixture/UpdateDevEndpoint.yaml"

requestUpdateJob :: UpdateJob -> TestTree
requestUpdateJob =
  req
    "UpdateJob"
    "fixture/UpdateJob.yaml"

requestUpdateJobFromSourceControl :: UpdateJobFromSourceControl -> TestTree
requestUpdateJobFromSourceControl =
  req
    "UpdateJobFromSourceControl"
    "fixture/UpdateJobFromSourceControl.yaml"

requestUpdateMLTransform :: UpdateMLTransform -> TestTree
requestUpdateMLTransform =
  req
    "UpdateMLTransform"
    "fixture/UpdateMLTransform.yaml"

requestUpdatePartition :: UpdatePartition -> TestTree
requestUpdatePartition =
  req
    "UpdatePartition"
    "fixture/UpdatePartition.yaml"

requestUpdateRegistry :: UpdateRegistry -> TestTree
requestUpdateRegistry =
  req
    "UpdateRegistry"
    "fixture/UpdateRegistry.yaml"

requestUpdateSchema :: UpdateSchema -> TestTree
requestUpdateSchema =
  req
    "UpdateSchema"
    "fixture/UpdateSchema.yaml"

requestUpdateSourceControlFromJob :: UpdateSourceControlFromJob -> TestTree
requestUpdateSourceControlFromJob =
  req
    "UpdateSourceControlFromJob"
    "fixture/UpdateSourceControlFromJob.yaml"

requestUpdateTable :: UpdateTable -> TestTree
requestUpdateTable =
  req
    "UpdateTable"
    "fixture/UpdateTable.yaml"

requestUpdateTrigger :: UpdateTrigger -> TestTree
requestUpdateTrigger =
  req
    "UpdateTrigger"
    "fixture/UpdateTrigger.yaml"

requestUpdateUserDefinedFunction :: UpdateUserDefinedFunction -> TestTree
requestUpdateUserDefinedFunction =
  req
    "UpdateUserDefinedFunction"
    "fixture/UpdateUserDefinedFunction.yaml"

requestUpdateWorkflow :: UpdateWorkflow -> TestTree
requestUpdateWorkflow =
  req
    "UpdateWorkflow"
    "fixture/UpdateWorkflow.yaml"

-- Responses

responseBatchCreatePartition :: BatchCreatePartitionResponse -> TestTree
responseBatchCreatePartition =
  res
    "BatchCreatePartitionResponse"
    "fixture/BatchCreatePartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreatePartition)

responseBatchDeleteConnection :: BatchDeleteConnectionResponse -> TestTree
responseBatchDeleteConnection =
  res
    "BatchDeleteConnectionResponse"
    "fixture/BatchDeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteConnection)

responseBatchDeletePartition :: BatchDeletePartitionResponse -> TestTree
responseBatchDeletePartition =
  res
    "BatchDeletePartitionResponse"
    "fixture/BatchDeletePartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeletePartition)

responseBatchDeleteTable :: BatchDeleteTableResponse -> TestTree
responseBatchDeleteTable =
  res
    "BatchDeleteTableResponse"
    "fixture/BatchDeleteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteTable)

responseBatchDeleteTableVersion :: BatchDeleteTableVersionResponse -> TestTree
responseBatchDeleteTableVersion =
  res
    "BatchDeleteTableVersionResponse"
    "fixture/BatchDeleteTableVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteTableVersion)

responseBatchGetBlueprints :: BatchGetBlueprintsResponse -> TestTree
responseBatchGetBlueprints =
  res
    "BatchGetBlueprintsResponse"
    "fixture/BatchGetBlueprintsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetBlueprints)

responseBatchGetCrawlers :: BatchGetCrawlersResponse -> TestTree
responseBatchGetCrawlers =
  res
    "BatchGetCrawlersResponse"
    "fixture/BatchGetCrawlersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetCrawlers)

responseBatchGetCustomEntityTypes :: BatchGetCustomEntityTypesResponse -> TestTree
responseBatchGetCustomEntityTypes =
  res
    "BatchGetCustomEntityTypesResponse"
    "fixture/BatchGetCustomEntityTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetCustomEntityTypes)

responseBatchGetDataQualityResult :: BatchGetDataQualityResultResponse -> TestTree
responseBatchGetDataQualityResult =
  res
    "BatchGetDataQualityResultResponse"
    "fixture/BatchGetDataQualityResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDataQualityResult)

responseBatchGetDevEndpoints :: BatchGetDevEndpointsResponse -> TestTree
responseBatchGetDevEndpoints =
  res
    "BatchGetDevEndpointsResponse"
    "fixture/BatchGetDevEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDevEndpoints)

responseBatchGetJobs :: BatchGetJobsResponse -> TestTree
responseBatchGetJobs =
  res
    "BatchGetJobsResponse"
    "fixture/BatchGetJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetJobs)

responseBatchGetPartition :: BatchGetPartitionResponse -> TestTree
responseBatchGetPartition =
  res
    "BatchGetPartitionResponse"
    "fixture/BatchGetPartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetPartition)

responseBatchGetTriggers :: BatchGetTriggersResponse -> TestTree
responseBatchGetTriggers =
  res
    "BatchGetTriggersResponse"
    "fixture/BatchGetTriggersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetTriggers)

responseBatchGetWorkflows :: BatchGetWorkflowsResponse -> TestTree
responseBatchGetWorkflows =
  res
    "BatchGetWorkflowsResponse"
    "fixture/BatchGetWorkflowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetWorkflows)

responseBatchStopJobRun :: BatchStopJobRunResponse -> TestTree
responseBatchStopJobRun =
  res
    "BatchStopJobRunResponse"
    "fixture/BatchStopJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchStopJobRun)

responseBatchUpdatePartition :: BatchUpdatePartitionResponse -> TestTree
responseBatchUpdatePartition =
  res
    "BatchUpdatePartitionResponse"
    "fixture/BatchUpdatePartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdatePartition)

responseCancelDataQualityRuleRecommendationRun :: CancelDataQualityRuleRecommendationRunResponse -> TestTree
responseCancelDataQualityRuleRecommendationRun =
  res
    "CancelDataQualityRuleRecommendationRunResponse"
    "fixture/CancelDataQualityRuleRecommendationRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelDataQualityRuleRecommendationRun)

responseCancelDataQualityRulesetEvaluationRun :: CancelDataQualityRulesetEvaluationRunResponse -> TestTree
responseCancelDataQualityRulesetEvaluationRun =
  res
    "CancelDataQualityRulesetEvaluationRunResponse"
    "fixture/CancelDataQualityRulesetEvaluationRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelDataQualityRulesetEvaluationRun)

responseCancelMLTaskRun :: CancelMLTaskRunResponse -> TestTree
responseCancelMLTaskRun =
  res
    "CancelMLTaskRunResponse"
    "fixture/CancelMLTaskRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelMLTaskRun)

responseCancelStatement :: CancelStatementResponse -> TestTree
responseCancelStatement =
  res
    "CancelStatementResponse"
    "fixture/CancelStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelStatement)

responseCheckSchemaVersionValidity :: CheckSchemaVersionValidityResponse -> TestTree
responseCheckSchemaVersionValidity =
  res
    "CheckSchemaVersionValidityResponse"
    "fixture/CheckSchemaVersionValidityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckSchemaVersionValidity)

responseCreateBlueprint :: CreateBlueprintResponse -> TestTree
responseCreateBlueprint =
  res
    "CreateBlueprintResponse"
    "fixture/CreateBlueprintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBlueprint)

responseCreateClassifier :: CreateClassifierResponse -> TestTree
responseCreateClassifier =
  res
    "CreateClassifierResponse"
    "fixture/CreateClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClassifier)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnection)

responseCreateCrawler :: CreateCrawlerResponse -> TestTree
responseCreateCrawler =
  res
    "CreateCrawlerResponse"
    "fixture/CreateCrawlerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCrawler)

responseCreateCustomEntityType :: CreateCustomEntityTypeResponse -> TestTree
responseCreateCustomEntityType =
  res
    "CreateCustomEntityTypeResponse"
    "fixture/CreateCustomEntityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomEntityType)

responseCreateDataQualityRuleset :: CreateDataQualityRulesetResponse -> TestTree
responseCreateDataQualityRuleset =
  res
    "CreateDataQualityRulesetResponse"
    "fixture/CreateDataQualityRulesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataQualityRuleset)

responseCreateDatabase :: CreateDatabaseResponse -> TestTree
responseCreateDatabase =
  res
    "CreateDatabaseResponse"
    "fixture/CreateDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatabase)

responseCreateDevEndpoint :: CreateDevEndpointResponse -> TestTree
responseCreateDevEndpoint =
  res
    "CreateDevEndpointResponse"
    "fixture/CreateDevEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDevEndpoint)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJob)

responseCreateMLTransform :: CreateMLTransformResponse -> TestTree
responseCreateMLTransform =
  res
    "CreateMLTransformResponse"
    "fixture/CreateMLTransformResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMLTransform)

responseCreatePartition :: CreatePartitionResponse -> TestTree
responseCreatePartition =
  res
    "CreatePartitionResponse"
    "fixture/CreatePartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePartition)

responseCreatePartitionIndex :: CreatePartitionIndexResponse -> TestTree
responseCreatePartitionIndex =
  res
    "CreatePartitionIndexResponse"
    "fixture/CreatePartitionIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePartitionIndex)

responseCreateRegistry :: CreateRegistryResponse -> TestTree
responseCreateRegistry =
  res
    "CreateRegistryResponse"
    "fixture/CreateRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRegistry)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSchema)

responseCreateScript :: CreateScriptResponse -> TestTree
responseCreateScript =
  res
    "CreateScriptResponse"
    "fixture/CreateScriptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateScript)

responseCreateSecurityConfiguration :: CreateSecurityConfigurationResponse -> TestTree
responseCreateSecurityConfiguration =
  res
    "CreateSecurityConfigurationResponse"
    "fixture/CreateSecurityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecurityConfiguration)

responseCreateSession :: CreateSessionResponse -> TestTree
responseCreateSession =
  res
    "CreateSessionResponse"
    "fixture/CreateSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSession)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable =
  res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTable)

responseCreateTrigger :: CreateTriggerResponse -> TestTree
responseCreateTrigger =
  res
    "CreateTriggerResponse"
    "fixture/CreateTriggerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrigger)

responseCreateUserDefinedFunction :: CreateUserDefinedFunctionResponse -> TestTree
responseCreateUserDefinedFunction =
  res
    "CreateUserDefinedFunctionResponse"
    "fixture/CreateUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserDefinedFunction)

responseCreateWorkflow :: CreateWorkflowResponse -> TestTree
responseCreateWorkflow =
  res
    "CreateWorkflowResponse"
    "fixture/CreateWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkflow)

responseDeleteBlueprint :: DeleteBlueprintResponse -> TestTree
responseDeleteBlueprint =
  res
    "DeleteBlueprintResponse"
    "fixture/DeleteBlueprintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBlueprint)

responseDeleteClassifier :: DeleteClassifierResponse -> TestTree
responseDeleteClassifier =
  res
    "DeleteClassifierResponse"
    "fixture/DeleteClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClassifier)

responseDeleteColumnStatisticsForPartition :: DeleteColumnStatisticsForPartitionResponse -> TestTree
responseDeleteColumnStatisticsForPartition =
  res
    "DeleteColumnStatisticsForPartitionResponse"
    "fixture/DeleteColumnStatisticsForPartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteColumnStatisticsForPartition)

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

responseDeleteCrawler :: DeleteCrawlerResponse -> TestTree
responseDeleteCrawler =
  res
    "DeleteCrawlerResponse"
    "fixture/DeleteCrawlerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCrawler)

responseDeleteCustomEntityType :: DeleteCustomEntityTypeResponse -> TestTree
responseDeleteCustomEntityType =
  res
    "DeleteCustomEntityTypeResponse"
    "fixture/DeleteCustomEntityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomEntityType)

responseDeleteDataQualityRuleset :: DeleteDataQualityRulesetResponse -> TestTree
responseDeleteDataQualityRuleset =
  res
    "DeleteDataQualityRulesetResponse"
    "fixture/DeleteDataQualityRulesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataQualityRuleset)

responseDeleteDatabase :: DeleteDatabaseResponse -> TestTree
responseDeleteDatabase =
  res
    "DeleteDatabaseResponse"
    "fixture/DeleteDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatabase)

responseDeleteDevEndpoint :: DeleteDevEndpointResponse -> TestTree
responseDeleteDevEndpoint =
  res
    "DeleteDevEndpointResponse"
    "fixture/DeleteDevEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDevEndpoint)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJob)

responseDeleteMLTransform :: DeleteMLTransformResponse -> TestTree
responseDeleteMLTransform =
  res
    "DeleteMLTransformResponse"
    "fixture/DeleteMLTransformResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMLTransform)

responseDeletePartition :: DeletePartitionResponse -> TestTree
responseDeletePartition =
  res
    "DeletePartitionResponse"
    "fixture/DeletePartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePartition)

responseDeletePartitionIndex :: DeletePartitionIndexResponse -> TestTree
responseDeletePartitionIndex =
  res
    "DeletePartitionIndexResponse"
    "fixture/DeletePartitionIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePartitionIndex)

responseDeleteRegistry :: DeleteRegistryResponse -> TestTree
responseDeleteRegistry =
  res
    "DeleteRegistryResponse"
    "fixture/DeleteRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRegistry)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchema)

responseDeleteSchemaVersions :: DeleteSchemaVersionsResponse -> TestTree
responseDeleteSchemaVersions =
  res
    "DeleteSchemaVersionsResponse"
    "fixture/DeleteSchemaVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchemaVersions)

responseDeleteSecurityConfiguration :: DeleteSecurityConfigurationResponse -> TestTree
responseDeleteSecurityConfiguration =
  res
    "DeleteSecurityConfigurationResponse"
    "fixture/DeleteSecurityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecurityConfiguration)

responseDeleteSession :: DeleteSessionResponse -> TestTree
responseDeleteSession =
  res
    "DeleteSessionResponse"
    "fixture/DeleteSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSession)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable =
  res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTable)

responseDeleteTableVersion :: DeleteTableVersionResponse -> TestTree
responseDeleteTableVersion =
  res
    "DeleteTableVersionResponse"
    "fixture/DeleteTableVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTableVersion)

responseDeleteTrigger :: DeleteTriggerResponse -> TestTree
responseDeleteTrigger =
  res
    "DeleteTriggerResponse"
    "fixture/DeleteTriggerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrigger)

responseDeleteUserDefinedFunction :: DeleteUserDefinedFunctionResponse -> TestTree
responseDeleteUserDefinedFunction =
  res
    "DeleteUserDefinedFunctionResponse"
    "fixture/DeleteUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserDefinedFunction)

responseDeleteWorkflow :: DeleteWorkflowResponse -> TestTree
responseDeleteWorkflow =
  res
    "DeleteWorkflowResponse"
    "fixture/DeleteWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkflow)

responseGetBlueprint :: GetBlueprintResponse -> TestTree
responseGetBlueprint =
  res
    "GetBlueprintResponse"
    "fixture/GetBlueprintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlueprint)

responseGetBlueprintRun :: GetBlueprintRunResponse -> TestTree
responseGetBlueprintRun =
  res
    "GetBlueprintRunResponse"
    "fixture/GetBlueprintRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlueprintRun)

responseGetBlueprintRuns :: GetBlueprintRunsResponse -> TestTree
responseGetBlueprintRuns =
  res
    "GetBlueprintRunsResponse"
    "fixture/GetBlueprintRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlueprintRuns)

responseGetCatalogImportStatus :: GetCatalogImportStatusResponse -> TestTree
responseGetCatalogImportStatus =
  res
    "GetCatalogImportStatusResponse"
    "fixture/GetCatalogImportStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCatalogImportStatus)

responseGetClassifier :: GetClassifierResponse -> TestTree
responseGetClassifier =
  res
    "GetClassifierResponse"
    "fixture/GetClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClassifier)

responseGetClassifiers :: GetClassifiersResponse -> TestTree
responseGetClassifiers =
  res
    "GetClassifiersResponse"
    "fixture/GetClassifiersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClassifiers)

responseGetColumnStatisticsForPartition :: GetColumnStatisticsForPartitionResponse -> TestTree
responseGetColumnStatisticsForPartition =
  res
    "GetColumnStatisticsForPartitionResponse"
    "fixture/GetColumnStatisticsForPartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetColumnStatisticsForPartition)

responseGetColumnStatisticsForTable :: GetColumnStatisticsForTableResponse -> TestTree
responseGetColumnStatisticsForTable =
  res
    "GetColumnStatisticsForTableResponse"
    "fixture/GetColumnStatisticsForTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetColumnStatisticsForTable)

responseGetConnection :: GetConnectionResponse -> TestTree
responseGetConnection =
  res
    "GetConnectionResponse"
    "fixture/GetConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnection)

responseGetConnections :: GetConnectionsResponse -> TestTree
responseGetConnections =
  res
    "GetConnectionsResponse"
    "fixture/GetConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnections)

responseGetCrawler :: GetCrawlerResponse -> TestTree
responseGetCrawler =
  res
    "GetCrawlerResponse"
    "fixture/GetCrawlerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCrawler)

responseGetCrawlerMetrics :: GetCrawlerMetricsResponse -> TestTree
responseGetCrawlerMetrics =
  res
    "GetCrawlerMetricsResponse"
    "fixture/GetCrawlerMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCrawlerMetrics)

responseGetCrawlers :: GetCrawlersResponse -> TestTree
responseGetCrawlers =
  res
    "GetCrawlersResponse"
    "fixture/GetCrawlersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCrawlers)

responseGetCustomEntityType :: GetCustomEntityTypeResponse -> TestTree
responseGetCustomEntityType =
  res
    "GetCustomEntityTypeResponse"
    "fixture/GetCustomEntityTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCustomEntityType)

responseGetDataCatalogEncryptionSettings :: GetDataCatalogEncryptionSettingsResponse -> TestTree
responseGetDataCatalogEncryptionSettings =
  res
    "GetDataCatalogEncryptionSettingsResponse"
    "fixture/GetDataCatalogEncryptionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataCatalogEncryptionSettings)

responseGetDataQualityResult :: GetDataQualityResultResponse -> TestTree
responseGetDataQualityResult =
  res
    "GetDataQualityResultResponse"
    "fixture/GetDataQualityResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataQualityResult)

responseGetDataQualityRuleRecommendationRun :: GetDataQualityRuleRecommendationRunResponse -> TestTree
responseGetDataQualityRuleRecommendationRun =
  res
    "GetDataQualityRuleRecommendationRunResponse"
    "fixture/GetDataQualityRuleRecommendationRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataQualityRuleRecommendationRun)

responseGetDataQualityRuleset :: GetDataQualityRulesetResponse -> TestTree
responseGetDataQualityRuleset =
  res
    "GetDataQualityRulesetResponse"
    "fixture/GetDataQualityRulesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataQualityRuleset)

responseGetDataQualityRulesetEvaluationRun :: GetDataQualityRulesetEvaluationRunResponse -> TestTree
responseGetDataQualityRulesetEvaluationRun =
  res
    "GetDataQualityRulesetEvaluationRunResponse"
    "fixture/GetDataQualityRulesetEvaluationRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataQualityRulesetEvaluationRun)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatabase)

responseGetDatabases :: GetDatabasesResponse -> TestTree
responseGetDatabases =
  res
    "GetDatabasesResponse"
    "fixture/GetDatabasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatabases)

responseGetDataflowGraph :: GetDataflowGraphResponse -> TestTree
responseGetDataflowGraph =
  res
    "GetDataflowGraphResponse"
    "fixture/GetDataflowGraphResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataflowGraph)

responseGetDevEndpoint :: GetDevEndpointResponse -> TestTree
responseGetDevEndpoint =
  res
    "GetDevEndpointResponse"
    "fixture/GetDevEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevEndpoint)

responseGetDevEndpoints :: GetDevEndpointsResponse -> TestTree
responseGetDevEndpoints =
  res
    "GetDevEndpointsResponse"
    "fixture/GetDevEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevEndpoints)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJob)

responseGetJobBookmark :: GetJobBookmarkResponse -> TestTree
responseGetJobBookmark =
  res
    "GetJobBookmarkResponse"
    "fixture/GetJobBookmarkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobBookmark)

responseGetJobRun :: GetJobRunResponse -> TestTree
responseGetJobRun =
  res
    "GetJobRunResponse"
    "fixture/GetJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobRun)

responseGetJobRuns :: GetJobRunsResponse -> TestTree
responseGetJobRuns =
  res
    "GetJobRunsResponse"
    "fixture/GetJobRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobRuns)

responseGetJobs :: GetJobsResponse -> TestTree
responseGetJobs =
  res
    "GetJobsResponse"
    "fixture/GetJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobs)

responseGetMLTaskRun :: GetMLTaskRunResponse -> TestTree
responseGetMLTaskRun =
  res
    "GetMLTaskRunResponse"
    "fixture/GetMLTaskRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMLTaskRun)

responseGetMLTaskRuns :: GetMLTaskRunsResponse -> TestTree
responseGetMLTaskRuns =
  res
    "GetMLTaskRunsResponse"
    "fixture/GetMLTaskRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMLTaskRuns)

responseGetMLTransform :: GetMLTransformResponse -> TestTree
responseGetMLTransform =
  res
    "GetMLTransformResponse"
    "fixture/GetMLTransformResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMLTransform)

responseGetMLTransforms :: GetMLTransformsResponse -> TestTree
responseGetMLTransforms =
  res
    "GetMLTransformsResponse"
    "fixture/GetMLTransformsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMLTransforms)

responseGetMapping :: GetMappingResponse -> TestTree
responseGetMapping =
  res
    "GetMappingResponse"
    "fixture/GetMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMapping)

responseGetPartition :: GetPartitionResponse -> TestTree
responseGetPartition =
  res
    "GetPartitionResponse"
    "fixture/GetPartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPartition)

responseGetPartitionIndexes :: GetPartitionIndexesResponse -> TestTree
responseGetPartitionIndexes =
  res
    "GetPartitionIndexesResponse"
    "fixture/GetPartitionIndexesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPartitionIndexes)

responseGetPartitions :: GetPartitionsResponse -> TestTree
responseGetPartitions =
  res
    "GetPartitionsResponse"
    "fixture/GetPartitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPartitions)

responseGetPlan :: GetPlanResponse -> TestTree
responseGetPlan =
  res
    "GetPlanResponse"
    "fixture/GetPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPlan)

responseGetRegistry :: GetRegistryResponse -> TestTree
responseGetRegistry =
  res
    "GetRegistryResponse"
    "fixture/GetRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegistry)

responseGetResourcePolicies :: GetResourcePoliciesResponse -> TestTree
responseGetResourcePolicies =
  res
    "GetResourcePoliciesResponse"
    "fixture/GetResourcePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicies)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicy)

responseGetSchema :: GetSchemaResponse -> TestTree
responseGetSchema =
  res
    "GetSchemaResponse"
    "fixture/GetSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchema)

responseGetSchemaByDefinition :: GetSchemaByDefinitionResponse -> TestTree
responseGetSchemaByDefinition =
  res
    "GetSchemaByDefinitionResponse"
    "fixture/GetSchemaByDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchemaByDefinition)

responseGetSchemaVersion :: GetSchemaVersionResponse -> TestTree
responseGetSchemaVersion =
  res
    "GetSchemaVersionResponse"
    "fixture/GetSchemaVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchemaVersion)

responseGetSchemaVersionsDiff :: GetSchemaVersionsDiffResponse -> TestTree
responseGetSchemaVersionsDiff =
  res
    "GetSchemaVersionsDiffResponse"
    "fixture/GetSchemaVersionsDiffResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchemaVersionsDiff)

responseGetSecurityConfiguration :: GetSecurityConfigurationResponse -> TestTree
responseGetSecurityConfiguration =
  res
    "GetSecurityConfigurationResponse"
    "fixture/GetSecurityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSecurityConfiguration)

responseGetSecurityConfigurations :: GetSecurityConfigurationsResponse -> TestTree
responseGetSecurityConfigurations =
  res
    "GetSecurityConfigurationsResponse"
    "fixture/GetSecurityConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSecurityConfigurations)

responseGetSession :: GetSessionResponse -> TestTree
responseGetSession =
  res
    "GetSessionResponse"
    "fixture/GetSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSession)

responseGetStatement :: GetStatementResponse -> TestTree
responseGetStatement =
  res
    "GetStatementResponse"
    "fixture/GetStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStatement)

responseGetTable :: GetTableResponse -> TestTree
responseGetTable =
  res
    "GetTableResponse"
    "fixture/GetTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTable)

responseGetTableVersion :: GetTableVersionResponse -> TestTree
responseGetTableVersion =
  res
    "GetTableVersionResponse"
    "fixture/GetTableVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTableVersion)

responseGetTableVersions :: GetTableVersionsResponse -> TestTree
responseGetTableVersions =
  res
    "GetTableVersionsResponse"
    "fixture/GetTableVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTableVersions)

responseGetTables :: GetTablesResponse -> TestTree
responseGetTables =
  res
    "GetTablesResponse"
    "fixture/GetTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTables)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTags)

responseGetTrigger :: GetTriggerResponse -> TestTree
responseGetTrigger =
  res
    "GetTriggerResponse"
    "fixture/GetTriggerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrigger)

responseGetTriggers :: GetTriggersResponse -> TestTree
responseGetTriggers =
  res
    "GetTriggersResponse"
    "fixture/GetTriggersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTriggers)

responseGetUnfilteredPartitionMetadata :: GetUnfilteredPartitionMetadataResponse -> TestTree
responseGetUnfilteredPartitionMetadata =
  res
    "GetUnfilteredPartitionMetadataResponse"
    "fixture/GetUnfilteredPartitionMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUnfilteredPartitionMetadata)

responseGetUnfilteredPartitionsMetadata :: GetUnfilteredPartitionsMetadataResponse -> TestTree
responseGetUnfilteredPartitionsMetadata =
  res
    "GetUnfilteredPartitionsMetadataResponse"
    "fixture/GetUnfilteredPartitionsMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUnfilteredPartitionsMetadata)

responseGetUnfilteredTableMetadata :: GetUnfilteredTableMetadataResponse -> TestTree
responseGetUnfilteredTableMetadata =
  res
    "GetUnfilteredTableMetadataResponse"
    "fixture/GetUnfilteredTableMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUnfilteredTableMetadata)

responseGetUserDefinedFunction :: GetUserDefinedFunctionResponse -> TestTree
responseGetUserDefinedFunction =
  res
    "GetUserDefinedFunctionResponse"
    "fixture/GetUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserDefinedFunction)

responseGetUserDefinedFunctions :: GetUserDefinedFunctionsResponse -> TestTree
responseGetUserDefinedFunctions =
  res
    "GetUserDefinedFunctionsResponse"
    "fixture/GetUserDefinedFunctionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserDefinedFunctions)

responseGetWorkflow :: GetWorkflowResponse -> TestTree
responseGetWorkflow =
  res
    "GetWorkflowResponse"
    "fixture/GetWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflow)

responseGetWorkflowRun :: GetWorkflowRunResponse -> TestTree
responseGetWorkflowRun =
  res
    "GetWorkflowRunResponse"
    "fixture/GetWorkflowRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowRun)

responseGetWorkflowRunProperties :: GetWorkflowRunPropertiesResponse -> TestTree
responseGetWorkflowRunProperties =
  res
    "GetWorkflowRunPropertiesResponse"
    "fixture/GetWorkflowRunPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowRunProperties)

responseGetWorkflowRuns :: GetWorkflowRunsResponse -> TestTree
responseGetWorkflowRuns =
  res
    "GetWorkflowRunsResponse"
    "fixture/GetWorkflowRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowRuns)

responseImportCatalogToGlue :: ImportCatalogToGlueResponse -> TestTree
responseImportCatalogToGlue =
  res
    "ImportCatalogToGlueResponse"
    "fixture/ImportCatalogToGlueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportCatalogToGlue)

responseListBlueprints :: ListBlueprintsResponse -> TestTree
responseListBlueprints =
  res
    "ListBlueprintsResponse"
    "fixture/ListBlueprintsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBlueprints)

responseListCrawlers :: ListCrawlersResponse -> TestTree
responseListCrawlers =
  res
    "ListCrawlersResponse"
    "fixture/ListCrawlersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCrawlers)

responseListCrawls :: ListCrawlsResponse -> TestTree
responseListCrawls =
  res
    "ListCrawlsResponse"
    "fixture/ListCrawlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCrawls)

responseListCustomEntityTypes :: ListCustomEntityTypesResponse -> TestTree
responseListCustomEntityTypes =
  res
    "ListCustomEntityTypesResponse"
    "fixture/ListCustomEntityTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomEntityTypes)

responseListDataQualityResults :: ListDataQualityResultsResponse -> TestTree
responseListDataQualityResults =
  res
    "ListDataQualityResultsResponse"
    "fixture/ListDataQualityResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataQualityResults)

responseListDataQualityRuleRecommendationRuns :: ListDataQualityRuleRecommendationRunsResponse -> TestTree
responseListDataQualityRuleRecommendationRuns =
  res
    "ListDataQualityRuleRecommendationRunsResponse"
    "fixture/ListDataQualityRuleRecommendationRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataQualityRuleRecommendationRuns)

responseListDataQualityRulesetEvaluationRuns :: ListDataQualityRulesetEvaluationRunsResponse -> TestTree
responseListDataQualityRulesetEvaluationRuns =
  res
    "ListDataQualityRulesetEvaluationRunsResponse"
    "fixture/ListDataQualityRulesetEvaluationRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataQualityRulesetEvaluationRuns)

responseListDataQualityRulesets :: ListDataQualityRulesetsResponse -> TestTree
responseListDataQualityRulesets =
  res
    "ListDataQualityRulesetsResponse"
    "fixture/ListDataQualityRulesetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataQualityRulesets)

responseListDevEndpoints :: ListDevEndpointsResponse -> TestTree
responseListDevEndpoints =
  res
    "ListDevEndpointsResponse"
    "fixture/ListDevEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevEndpoints)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseListMLTransforms :: ListMLTransformsResponse -> TestTree
responseListMLTransforms =
  res
    "ListMLTransformsResponse"
    "fixture/ListMLTransformsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMLTransforms)

responseListRegistries :: ListRegistriesResponse -> TestTree
responseListRegistries =
  res
    "ListRegistriesResponse"
    "fixture/ListRegistriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRegistries)

responseListSchemaVersions :: ListSchemaVersionsResponse -> TestTree
responseListSchemaVersions =
  res
    "ListSchemaVersionsResponse"
    "fixture/ListSchemaVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchemaVersions)

responseListSchemas :: ListSchemasResponse -> TestTree
responseListSchemas =
  res
    "ListSchemasResponse"
    "fixture/ListSchemasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchemas)

responseListSessions :: ListSessionsResponse -> TestTree
responseListSessions =
  res
    "ListSessionsResponse"
    "fixture/ListSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSessions)

responseListStatements :: ListStatementsResponse -> TestTree
responseListStatements =
  res
    "ListStatementsResponse"
    "fixture/ListStatementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStatements)

responseListTriggers :: ListTriggersResponse -> TestTree
responseListTriggers =
  res
    "ListTriggersResponse"
    "fixture/ListTriggersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTriggers)

responseListWorkflows :: ListWorkflowsResponse -> TestTree
responseListWorkflows =
  res
    "ListWorkflowsResponse"
    "fixture/ListWorkflowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflows)

responsePutDataCatalogEncryptionSettings :: PutDataCatalogEncryptionSettingsResponse -> TestTree
responsePutDataCatalogEncryptionSettings =
  res
    "PutDataCatalogEncryptionSettingsResponse"
    "fixture/PutDataCatalogEncryptionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDataCatalogEncryptionSettings)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responsePutSchemaVersionMetadata :: PutSchemaVersionMetadataResponse -> TestTree
responsePutSchemaVersionMetadata =
  res
    "PutSchemaVersionMetadataResponse"
    "fixture/PutSchemaVersionMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSchemaVersionMetadata)

responsePutWorkflowRunProperties :: PutWorkflowRunPropertiesResponse -> TestTree
responsePutWorkflowRunProperties =
  res
    "PutWorkflowRunPropertiesResponse"
    "fixture/PutWorkflowRunPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutWorkflowRunProperties)

responseQuerySchemaVersionMetadata :: QuerySchemaVersionMetadataResponse -> TestTree
responseQuerySchemaVersionMetadata =
  res
    "QuerySchemaVersionMetadataResponse"
    "fixture/QuerySchemaVersionMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy QuerySchemaVersionMetadata)

responseRegisterSchemaVersion :: RegisterSchemaVersionResponse -> TestTree
responseRegisterSchemaVersion =
  res
    "RegisterSchemaVersionResponse"
    "fixture/RegisterSchemaVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterSchemaVersion)

responseRemoveSchemaVersionMetadata :: RemoveSchemaVersionMetadataResponse -> TestTree
responseRemoveSchemaVersionMetadata =
  res
    "RemoveSchemaVersionMetadataResponse"
    "fixture/RemoveSchemaVersionMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveSchemaVersionMetadata)

responseResetJobBookmark :: ResetJobBookmarkResponse -> TestTree
responseResetJobBookmark =
  res
    "ResetJobBookmarkResponse"
    "fixture/ResetJobBookmarkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetJobBookmark)

responseResumeWorkflowRun :: ResumeWorkflowRunResponse -> TestTree
responseResumeWorkflowRun =
  res
    "ResumeWorkflowRunResponse"
    "fixture/ResumeWorkflowRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeWorkflowRun)

responseRunStatement :: RunStatementResponse -> TestTree
responseRunStatement =
  res
    "RunStatementResponse"
    "fixture/RunStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RunStatement)

responseSearchTables :: SearchTablesResponse -> TestTree
responseSearchTables =
  res
    "SearchTablesResponse"
    "fixture/SearchTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchTables)

responseStartBlueprintRun :: StartBlueprintRunResponse -> TestTree
responseStartBlueprintRun =
  res
    "StartBlueprintRunResponse"
    "fixture/StartBlueprintRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBlueprintRun)

responseStartCrawler :: StartCrawlerResponse -> TestTree
responseStartCrawler =
  res
    "StartCrawlerResponse"
    "fixture/StartCrawlerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCrawler)

responseStartCrawlerSchedule :: StartCrawlerScheduleResponse -> TestTree
responseStartCrawlerSchedule =
  res
    "StartCrawlerScheduleResponse"
    "fixture/StartCrawlerScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCrawlerSchedule)

responseStartDataQualityRuleRecommendationRun :: StartDataQualityRuleRecommendationRunResponse -> TestTree
responseStartDataQualityRuleRecommendationRun =
  res
    "StartDataQualityRuleRecommendationRunResponse"
    "fixture/StartDataQualityRuleRecommendationRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDataQualityRuleRecommendationRun)

responseStartDataQualityRulesetEvaluationRun :: StartDataQualityRulesetEvaluationRunResponse -> TestTree
responseStartDataQualityRulesetEvaluationRun =
  res
    "StartDataQualityRulesetEvaluationRunResponse"
    "fixture/StartDataQualityRulesetEvaluationRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDataQualityRulesetEvaluationRun)

responseStartExportLabelsTaskRun :: StartExportLabelsTaskRunResponse -> TestTree
responseStartExportLabelsTaskRun =
  res
    "StartExportLabelsTaskRunResponse"
    "fixture/StartExportLabelsTaskRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartExportLabelsTaskRun)

responseStartImportLabelsTaskRun :: StartImportLabelsTaskRunResponse -> TestTree
responseStartImportLabelsTaskRun =
  res
    "StartImportLabelsTaskRunResponse"
    "fixture/StartImportLabelsTaskRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartImportLabelsTaskRun)

responseStartJobRun :: StartJobRunResponse -> TestTree
responseStartJobRun =
  res
    "StartJobRunResponse"
    "fixture/StartJobRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartJobRun)

responseStartMLEvaluationTaskRun :: StartMLEvaluationTaskRunResponse -> TestTree
responseStartMLEvaluationTaskRun =
  res
    "StartMLEvaluationTaskRunResponse"
    "fixture/StartMLEvaluationTaskRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMLEvaluationTaskRun)

responseStartMLLabelingSetGenerationTaskRun :: StartMLLabelingSetGenerationTaskRunResponse -> TestTree
responseStartMLLabelingSetGenerationTaskRun =
  res
    "StartMLLabelingSetGenerationTaskRunResponse"
    "fixture/StartMLLabelingSetGenerationTaskRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMLLabelingSetGenerationTaskRun)

responseStartTrigger :: StartTriggerResponse -> TestTree
responseStartTrigger =
  res
    "StartTriggerResponse"
    "fixture/StartTriggerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTrigger)

responseStartWorkflowRun :: StartWorkflowRunResponse -> TestTree
responseStartWorkflowRun =
  res
    "StartWorkflowRunResponse"
    "fixture/StartWorkflowRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartWorkflowRun)

responseStopCrawler :: StopCrawlerResponse -> TestTree
responseStopCrawler =
  res
    "StopCrawlerResponse"
    "fixture/StopCrawlerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopCrawler)

responseStopCrawlerSchedule :: StopCrawlerScheduleResponse -> TestTree
responseStopCrawlerSchedule =
  res
    "StopCrawlerScheduleResponse"
    "fixture/StopCrawlerScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopCrawlerSchedule)

responseStopSession :: StopSessionResponse -> TestTree
responseStopSession =
  res
    "StopSessionResponse"
    "fixture/StopSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopSession)

responseStopTrigger :: StopTriggerResponse -> TestTree
responseStopTrigger =
  res
    "StopTriggerResponse"
    "fixture/StopTriggerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTrigger)

responseStopWorkflowRun :: StopWorkflowRunResponse -> TestTree
responseStopWorkflowRun =
  res
    "StopWorkflowRunResponse"
    "fixture/StopWorkflowRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopWorkflowRun)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateBlueprint :: UpdateBlueprintResponse -> TestTree
responseUpdateBlueprint =
  res
    "UpdateBlueprintResponse"
    "fixture/UpdateBlueprintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBlueprint)

responseUpdateClassifier :: UpdateClassifierResponse -> TestTree
responseUpdateClassifier =
  res
    "UpdateClassifierResponse"
    "fixture/UpdateClassifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClassifier)

responseUpdateColumnStatisticsForPartition :: UpdateColumnStatisticsForPartitionResponse -> TestTree
responseUpdateColumnStatisticsForPartition =
  res
    "UpdateColumnStatisticsForPartitionResponse"
    "fixture/UpdateColumnStatisticsForPartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateColumnStatisticsForPartition)

responseUpdateColumnStatisticsForTable :: UpdateColumnStatisticsForTableResponse -> TestTree
responseUpdateColumnStatisticsForTable =
  res
    "UpdateColumnStatisticsForTableResponse"
    "fixture/UpdateColumnStatisticsForTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateColumnStatisticsForTable)

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnection)

responseUpdateCrawler :: UpdateCrawlerResponse -> TestTree
responseUpdateCrawler =
  res
    "UpdateCrawlerResponse"
    "fixture/UpdateCrawlerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCrawler)

responseUpdateCrawlerSchedule :: UpdateCrawlerScheduleResponse -> TestTree
responseUpdateCrawlerSchedule =
  res
    "UpdateCrawlerScheduleResponse"
    "fixture/UpdateCrawlerScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCrawlerSchedule)

responseUpdateDataQualityRuleset :: UpdateDataQualityRulesetResponse -> TestTree
responseUpdateDataQualityRuleset =
  res
    "UpdateDataQualityRulesetResponse"
    "fixture/UpdateDataQualityRulesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataQualityRuleset)

responseUpdateDatabase :: UpdateDatabaseResponse -> TestTree
responseUpdateDatabase =
  res
    "UpdateDatabaseResponse"
    "fixture/UpdateDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDatabase)

responseUpdateDevEndpoint :: UpdateDevEndpointResponse -> TestTree
responseUpdateDevEndpoint =
  res
    "UpdateDevEndpointResponse"
    "fixture/UpdateDevEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDevEndpoint)

responseUpdateJob :: UpdateJobResponse -> TestTree
responseUpdateJob =
  res
    "UpdateJobResponse"
    "fixture/UpdateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJob)

responseUpdateJobFromSourceControl :: UpdateJobFromSourceControlResponse -> TestTree
responseUpdateJobFromSourceControl =
  res
    "UpdateJobFromSourceControlResponse"
    "fixture/UpdateJobFromSourceControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJobFromSourceControl)

responseUpdateMLTransform :: UpdateMLTransformResponse -> TestTree
responseUpdateMLTransform =
  res
    "UpdateMLTransformResponse"
    "fixture/UpdateMLTransformResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMLTransform)

responseUpdatePartition :: UpdatePartitionResponse -> TestTree
responseUpdatePartition =
  res
    "UpdatePartitionResponse"
    "fixture/UpdatePartitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePartition)

responseUpdateRegistry :: UpdateRegistryResponse -> TestTree
responseUpdateRegistry =
  res
    "UpdateRegistryResponse"
    "fixture/UpdateRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRegistry)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSchema)

responseUpdateSourceControlFromJob :: UpdateSourceControlFromJobResponse -> TestTree
responseUpdateSourceControlFromJob =
  res
    "UpdateSourceControlFromJobResponse"
    "fixture/UpdateSourceControlFromJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSourceControlFromJob)

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable =
  res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTable)

responseUpdateTrigger :: UpdateTriggerResponse -> TestTree
responseUpdateTrigger =
  res
    "UpdateTriggerResponse"
    "fixture/UpdateTriggerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrigger)

responseUpdateUserDefinedFunction :: UpdateUserDefinedFunctionResponse -> TestTree
responseUpdateUserDefinedFunction =
  res
    "UpdateUserDefinedFunctionResponse"
    "fixture/UpdateUserDefinedFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserDefinedFunction)

responseUpdateWorkflow :: UpdateWorkflowResponse -> TestTree
responseUpdateWorkflow =
  res
    "UpdateWorkflowResponse"
    "fixture/UpdateWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkflow)
