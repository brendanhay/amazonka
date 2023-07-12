{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Config
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Config where

import Amazonka.Config
import qualified Data.Proxy as Proxy
import Test.Amazonka.Config.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchGetAggregateResourceConfig $
--             newBatchGetAggregateResourceConfig
--
--         , requestBatchGetResourceConfig $
--             newBatchGetResourceConfig
--
--         , requestDeleteAggregationAuthorization $
--             newDeleteAggregationAuthorization
--
--         , requestDeleteConfigRule $
--             newDeleteConfigRule
--
--         , requestDeleteConfigurationAggregator $
--             newDeleteConfigurationAggregator
--
--         , requestDeleteConfigurationRecorder $
--             newDeleteConfigurationRecorder
--
--         , requestDeleteConformancePack $
--             newDeleteConformancePack
--
--         , requestDeleteDeliveryChannel $
--             newDeleteDeliveryChannel
--
--         , requestDeleteEvaluationResults $
--             newDeleteEvaluationResults
--
--         , requestDeleteOrganizationConfigRule $
--             newDeleteOrganizationConfigRule
--
--         , requestDeleteOrganizationConformancePack $
--             newDeleteOrganizationConformancePack
--
--         , requestDeletePendingAggregationRequest $
--             newDeletePendingAggregationRequest
--
--         , requestDeleteRemediationConfiguration $
--             newDeleteRemediationConfiguration
--
--         , requestDeleteRemediationExceptions $
--             newDeleteRemediationExceptions
--
--         , requestDeleteResourceConfig $
--             newDeleteResourceConfig
--
--         , requestDeleteRetentionConfiguration $
--             newDeleteRetentionConfiguration
--
--         , requestDeleteStoredQuery $
--             newDeleteStoredQuery
--
--         , requestDeliverConfigSnapshot $
--             newDeliverConfigSnapshot
--
--         , requestDescribeAggregateComplianceByConfigRules $
--             newDescribeAggregateComplianceByConfigRules
--
--         , requestDescribeAggregateComplianceByConformancePacks $
--             newDescribeAggregateComplianceByConformancePacks
--
--         , requestDescribeAggregationAuthorizations $
--             newDescribeAggregationAuthorizations
--
--         , requestDescribeComplianceByConfigRule $
--             newDescribeComplianceByConfigRule
--
--         , requestDescribeComplianceByResource $
--             newDescribeComplianceByResource
--
--         , requestDescribeConfigRuleEvaluationStatus $
--             newDescribeConfigRuleEvaluationStatus
--
--         , requestDescribeConfigRules $
--             newDescribeConfigRules
--
--         , requestDescribeConfigurationAggregatorSourcesStatus $
--             newDescribeConfigurationAggregatorSourcesStatus
--
--         , requestDescribeConfigurationAggregators $
--             newDescribeConfigurationAggregators
--
--         , requestDescribeConfigurationRecorderStatus $
--             newDescribeConfigurationRecorderStatus
--
--         , requestDescribeConfigurationRecorders $
--             newDescribeConfigurationRecorders
--
--         , requestDescribeConformancePackCompliance $
--             newDescribeConformancePackCompliance
--
--         , requestDescribeConformancePackStatus $
--             newDescribeConformancePackStatus
--
--         , requestDescribeConformancePacks $
--             newDescribeConformancePacks
--
--         , requestDescribeDeliveryChannelStatus $
--             newDescribeDeliveryChannelStatus
--
--         , requestDescribeDeliveryChannels $
--             newDescribeDeliveryChannels
--
--         , requestDescribeOrganizationConfigRuleStatuses $
--             newDescribeOrganizationConfigRuleStatuses
--
--         , requestDescribeOrganizationConfigRules $
--             newDescribeOrganizationConfigRules
--
--         , requestDescribeOrganizationConformancePackStatuses $
--             newDescribeOrganizationConformancePackStatuses
--
--         , requestDescribeOrganizationConformancePacks $
--             newDescribeOrganizationConformancePacks
--
--         , requestDescribePendingAggregationRequests $
--             newDescribePendingAggregationRequests
--
--         , requestDescribeRemediationConfigurations $
--             newDescribeRemediationConfigurations
--
--         , requestDescribeRemediationExceptions $
--             newDescribeRemediationExceptions
--
--         , requestDescribeRemediationExecutionStatus $
--             newDescribeRemediationExecutionStatus
--
--         , requestDescribeRetentionConfigurations $
--             newDescribeRetentionConfigurations
--
--         , requestGetAggregateComplianceDetailsByConfigRule $
--             newGetAggregateComplianceDetailsByConfigRule
--
--         , requestGetAggregateConfigRuleComplianceSummary $
--             newGetAggregateConfigRuleComplianceSummary
--
--         , requestGetAggregateConformancePackComplianceSummary $
--             newGetAggregateConformancePackComplianceSummary
--
--         , requestGetAggregateDiscoveredResourceCounts $
--             newGetAggregateDiscoveredResourceCounts
--
--         , requestGetAggregateResourceConfig $
--             newGetAggregateResourceConfig
--
--         , requestGetComplianceDetailsByConfigRule $
--             newGetComplianceDetailsByConfigRule
--
--         , requestGetComplianceDetailsByResource $
--             newGetComplianceDetailsByResource
--
--         , requestGetComplianceSummaryByConfigRule $
--             newGetComplianceSummaryByConfigRule
--
--         , requestGetComplianceSummaryByResourceType $
--             newGetComplianceSummaryByResourceType
--
--         , requestGetConformancePackComplianceDetails $
--             newGetConformancePackComplianceDetails
--
--         , requestGetConformancePackComplianceSummary $
--             newGetConformancePackComplianceSummary
--
--         , requestGetCustomRulePolicy $
--             newGetCustomRulePolicy
--
--         , requestGetDiscoveredResourceCounts $
--             newGetDiscoveredResourceCounts
--
--         , requestGetOrganizationConfigRuleDetailedStatus $
--             newGetOrganizationConfigRuleDetailedStatus
--
--         , requestGetOrganizationConformancePackDetailedStatus $
--             newGetOrganizationConformancePackDetailedStatus
--
--         , requestGetOrganizationCustomRulePolicy $
--             newGetOrganizationCustomRulePolicy
--
--         , requestGetResourceConfigHistory $
--             newGetResourceConfigHistory
--
--         , requestGetResourceEvaluationSummary $
--             newGetResourceEvaluationSummary
--
--         , requestGetStoredQuery $
--             newGetStoredQuery
--
--         , requestListAggregateDiscoveredResources $
--             newListAggregateDiscoveredResources
--
--         , requestListConformancePackComplianceScores $
--             newListConformancePackComplianceScores
--
--         , requestListDiscoveredResources $
--             newListDiscoveredResources
--
--         , requestListResourceEvaluations $
--             newListResourceEvaluations
--
--         , requestListStoredQueries $
--             newListStoredQueries
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutAggregationAuthorization $
--             newPutAggregationAuthorization
--
--         , requestPutConfigRule $
--             newPutConfigRule
--
--         , requestPutConfigurationAggregator $
--             newPutConfigurationAggregator
--
--         , requestPutConfigurationRecorder $
--             newPutConfigurationRecorder
--
--         , requestPutConformancePack $
--             newPutConformancePack
--
--         , requestPutDeliveryChannel $
--             newPutDeliveryChannel
--
--         , requestPutEvaluations $
--             newPutEvaluations
--
--         , requestPutExternalEvaluation $
--             newPutExternalEvaluation
--
--         , requestPutOrganizationConfigRule $
--             newPutOrganizationConfigRule
--
--         , requestPutOrganizationConformancePack $
--             newPutOrganizationConformancePack
--
--         , requestPutRemediationConfigurations $
--             newPutRemediationConfigurations
--
--         , requestPutRemediationExceptions $
--             newPutRemediationExceptions
--
--         , requestPutResourceConfig $
--             newPutResourceConfig
--
--         , requestPutRetentionConfiguration $
--             newPutRetentionConfiguration
--
--         , requestPutStoredQuery $
--             newPutStoredQuery
--
--         , requestSelectAggregateResourceConfig $
--             newSelectAggregateResourceConfig
--
--         , requestSelectResourceConfig $
--             newSelectResourceConfig
--
--         , requestStartConfigRulesEvaluation $
--             newStartConfigRulesEvaluation
--
--         , requestStartConfigurationRecorder $
--             newStartConfigurationRecorder
--
--         , requestStartRemediationExecution $
--             newStartRemediationExecution
--
--         , requestStartResourceEvaluation $
--             newStartResourceEvaluation
--
--         , requestStopConfigurationRecorder $
--             newStopConfigurationRecorder
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseBatchGetAggregateResourceConfig $
--             newBatchGetAggregateResourceConfigResponse
--
--         , responseBatchGetResourceConfig $
--             newBatchGetResourceConfigResponse
--
--         , responseDeleteAggregationAuthorization $
--             newDeleteAggregationAuthorizationResponse
--
--         , responseDeleteConfigRule $
--             newDeleteConfigRuleResponse
--
--         , responseDeleteConfigurationAggregator $
--             newDeleteConfigurationAggregatorResponse
--
--         , responseDeleteConfigurationRecorder $
--             newDeleteConfigurationRecorderResponse
--
--         , responseDeleteConformancePack $
--             newDeleteConformancePackResponse
--
--         , responseDeleteDeliveryChannel $
--             newDeleteDeliveryChannelResponse
--
--         , responseDeleteEvaluationResults $
--             newDeleteEvaluationResultsResponse
--
--         , responseDeleteOrganizationConfigRule $
--             newDeleteOrganizationConfigRuleResponse
--
--         , responseDeleteOrganizationConformancePack $
--             newDeleteOrganizationConformancePackResponse
--
--         , responseDeletePendingAggregationRequest $
--             newDeletePendingAggregationRequestResponse
--
--         , responseDeleteRemediationConfiguration $
--             newDeleteRemediationConfigurationResponse
--
--         , responseDeleteRemediationExceptions $
--             newDeleteRemediationExceptionsResponse
--
--         , responseDeleteResourceConfig $
--             newDeleteResourceConfigResponse
--
--         , responseDeleteRetentionConfiguration $
--             newDeleteRetentionConfigurationResponse
--
--         , responseDeleteStoredQuery $
--             newDeleteStoredQueryResponse
--
--         , responseDeliverConfigSnapshot $
--             newDeliverConfigSnapshotResponse
--
--         , responseDescribeAggregateComplianceByConfigRules $
--             newDescribeAggregateComplianceByConfigRulesResponse
--
--         , responseDescribeAggregateComplianceByConformancePacks $
--             newDescribeAggregateComplianceByConformancePacksResponse
--
--         , responseDescribeAggregationAuthorizations $
--             newDescribeAggregationAuthorizationsResponse
--
--         , responseDescribeComplianceByConfigRule $
--             newDescribeComplianceByConfigRuleResponse
--
--         , responseDescribeComplianceByResource $
--             newDescribeComplianceByResourceResponse
--
--         , responseDescribeConfigRuleEvaluationStatus $
--             newDescribeConfigRuleEvaluationStatusResponse
--
--         , responseDescribeConfigRules $
--             newDescribeConfigRulesResponse
--
--         , responseDescribeConfigurationAggregatorSourcesStatus $
--             newDescribeConfigurationAggregatorSourcesStatusResponse
--
--         , responseDescribeConfigurationAggregators $
--             newDescribeConfigurationAggregatorsResponse
--
--         , responseDescribeConfigurationRecorderStatus $
--             newDescribeConfigurationRecorderStatusResponse
--
--         , responseDescribeConfigurationRecorders $
--             newDescribeConfigurationRecordersResponse
--
--         , responseDescribeConformancePackCompliance $
--             newDescribeConformancePackComplianceResponse
--
--         , responseDescribeConformancePackStatus $
--             newDescribeConformancePackStatusResponse
--
--         , responseDescribeConformancePacks $
--             newDescribeConformancePacksResponse
--
--         , responseDescribeDeliveryChannelStatus $
--             newDescribeDeliveryChannelStatusResponse
--
--         , responseDescribeDeliveryChannels $
--             newDescribeDeliveryChannelsResponse
--
--         , responseDescribeOrganizationConfigRuleStatuses $
--             newDescribeOrganizationConfigRuleStatusesResponse
--
--         , responseDescribeOrganizationConfigRules $
--             newDescribeOrganizationConfigRulesResponse
--
--         , responseDescribeOrganizationConformancePackStatuses $
--             newDescribeOrganizationConformancePackStatusesResponse
--
--         , responseDescribeOrganizationConformancePacks $
--             newDescribeOrganizationConformancePacksResponse
--
--         , responseDescribePendingAggregationRequests $
--             newDescribePendingAggregationRequestsResponse
--
--         , responseDescribeRemediationConfigurations $
--             newDescribeRemediationConfigurationsResponse
--
--         , responseDescribeRemediationExceptions $
--             newDescribeRemediationExceptionsResponse
--
--         , responseDescribeRemediationExecutionStatus $
--             newDescribeRemediationExecutionStatusResponse
--
--         , responseDescribeRetentionConfigurations $
--             newDescribeRetentionConfigurationsResponse
--
--         , responseGetAggregateComplianceDetailsByConfigRule $
--             newGetAggregateComplianceDetailsByConfigRuleResponse
--
--         , responseGetAggregateConfigRuleComplianceSummary $
--             newGetAggregateConfigRuleComplianceSummaryResponse
--
--         , responseGetAggregateConformancePackComplianceSummary $
--             newGetAggregateConformancePackComplianceSummaryResponse
--
--         , responseGetAggregateDiscoveredResourceCounts $
--             newGetAggregateDiscoveredResourceCountsResponse
--
--         , responseGetAggregateResourceConfig $
--             newGetAggregateResourceConfigResponse
--
--         , responseGetComplianceDetailsByConfigRule $
--             newGetComplianceDetailsByConfigRuleResponse
--
--         , responseGetComplianceDetailsByResource $
--             newGetComplianceDetailsByResourceResponse
--
--         , responseGetComplianceSummaryByConfigRule $
--             newGetComplianceSummaryByConfigRuleResponse
--
--         , responseGetComplianceSummaryByResourceType $
--             newGetComplianceSummaryByResourceTypeResponse
--
--         , responseGetConformancePackComplianceDetails $
--             newGetConformancePackComplianceDetailsResponse
--
--         , responseGetConformancePackComplianceSummary $
--             newGetConformancePackComplianceSummaryResponse
--
--         , responseGetCustomRulePolicy $
--             newGetCustomRulePolicyResponse
--
--         , responseGetDiscoveredResourceCounts $
--             newGetDiscoveredResourceCountsResponse
--
--         , responseGetOrganizationConfigRuleDetailedStatus $
--             newGetOrganizationConfigRuleDetailedStatusResponse
--
--         , responseGetOrganizationConformancePackDetailedStatus $
--             newGetOrganizationConformancePackDetailedStatusResponse
--
--         , responseGetOrganizationCustomRulePolicy $
--             newGetOrganizationCustomRulePolicyResponse
--
--         , responseGetResourceConfigHistory $
--             newGetResourceConfigHistoryResponse
--
--         , responseGetResourceEvaluationSummary $
--             newGetResourceEvaluationSummaryResponse
--
--         , responseGetStoredQuery $
--             newGetStoredQueryResponse
--
--         , responseListAggregateDiscoveredResources $
--             newListAggregateDiscoveredResourcesResponse
--
--         , responseListConformancePackComplianceScores $
--             newListConformancePackComplianceScoresResponse
--
--         , responseListDiscoveredResources $
--             newListDiscoveredResourcesResponse
--
--         , responseListResourceEvaluations $
--             newListResourceEvaluationsResponse
--
--         , responseListStoredQueries $
--             newListStoredQueriesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutAggregationAuthorization $
--             newPutAggregationAuthorizationResponse
--
--         , responsePutConfigRule $
--             newPutConfigRuleResponse
--
--         , responsePutConfigurationAggregator $
--             newPutConfigurationAggregatorResponse
--
--         , responsePutConfigurationRecorder $
--             newPutConfigurationRecorderResponse
--
--         , responsePutConformancePack $
--             newPutConformancePackResponse
--
--         , responsePutDeliveryChannel $
--             newPutDeliveryChannelResponse
--
--         , responsePutEvaluations $
--             newPutEvaluationsResponse
--
--         , responsePutExternalEvaluation $
--             newPutExternalEvaluationResponse
--
--         , responsePutOrganizationConfigRule $
--             newPutOrganizationConfigRuleResponse
--
--         , responsePutOrganizationConformancePack $
--             newPutOrganizationConformancePackResponse
--
--         , responsePutRemediationConfigurations $
--             newPutRemediationConfigurationsResponse
--
--         , responsePutRemediationExceptions $
--             newPutRemediationExceptionsResponse
--
--         , responsePutResourceConfig $
--             newPutResourceConfigResponse
--
--         , responsePutRetentionConfiguration $
--             newPutRetentionConfigurationResponse
--
--         , responsePutStoredQuery $
--             newPutStoredQueryResponse
--
--         , responseSelectAggregateResourceConfig $
--             newSelectAggregateResourceConfigResponse
--
--         , responseSelectResourceConfig $
--             newSelectResourceConfigResponse
--
--         , responseStartConfigRulesEvaluation $
--             newStartConfigRulesEvaluationResponse
--
--         , responseStartConfigurationRecorder $
--             newStartConfigurationRecorderResponse
--
--         , responseStartRemediationExecution $
--             newStartRemediationExecutionResponse
--
--         , responseStartResourceEvaluation $
--             newStartResourceEvaluationResponse
--
--         , responseStopConfigurationRecorder $
--             newStopConfigurationRecorderResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestBatchGetAggregateResourceConfig :: BatchGetAggregateResourceConfig -> TestTree
requestBatchGetAggregateResourceConfig =
  req
    "BatchGetAggregateResourceConfig"
    "fixture/BatchGetAggregateResourceConfig.yaml"

requestBatchGetResourceConfig :: BatchGetResourceConfig -> TestTree
requestBatchGetResourceConfig =
  req
    "BatchGetResourceConfig"
    "fixture/BatchGetResourceConfig.yaml"

requestDeleteAggregationAuthorization :: DeleteAggregationAuthorization -> TestTree
requestDeleteAggregationAuthorization =
  req
    "DeleteAggregationAuthorization"
    "fixture/DeleteAggregationAuthorization.yaml"

requestDeleteConfigRule :: DeleteConfigRule -> TestTree
requestDeleteConfigRule =
  req
    "DeleteConfigRule"
    "fixture/DeleteConfigRule.yaml"

requestDeleteConfigurationAggregator :: DeleteConfigurationAggregator -> TestTree
requestDeleteConfigurationAggregator =
  req
    "DeleteConfigurationAggregator"
    "fixture/DeleteConfigurationAggregator.yaml"

requestDeleteConfigurationRecorder :: DeleteConfigurationRecorder -> TestTree
requestDeleteConfigurationRecorder =
  req
    "DeleteConfigurationRecorder"
    "fixture/DeleteConfigurationRecorder.yaml"

requestDeleteConformancePack :: DeleteConformancePack -> TestTree
requestDeleteConformancePack =
  req
    "DeleteConformancePack"
    "fixture/DeleteConformancePack.yaml"

requestDeleteDeliveryChannel :: DeleteDeliveryChannel -> TestTree
requestDeleteDeliveryChannel =
  req
    "DeleteDeliveryChannel"
    "fixture/DeleteDeliveryChannel.yaml"

requestDeleteEvaluationResults :: DeleteEvaluationResults -> TestTree
requestDeleteEvaluationResults =
  req
    "DeleteEvaluationResults"
    "fixture/DeleteEvaluationResults.yaml"

requestDeleteOrganizationConfigRule :: DeleteOrganizationConfigRule -> TestTree
requestDeleteOrganizationConfigRule =
  req
    "DeleteOrganizationConfigRule"
    "fixture/DeleteOrganizationConfigRule.yaml"

requestDeleteOrganizationConformancePack :: DeleteOrganizationConformancePack -> TestTree
requestDeleteOrganizationConformancePack =
  req
    "DeleteOrganizationConformancePack"
    "fixture/DeleteOrganizationConformancePack.yaml"

requestDeletePendingAggregationRequest :: DeletePendingAggregationRequest -> TestTree
requestDeletePendingAggregationRequest =
  req
    "DeletePendingAggregationRequest"
    "fixture/DeletePendingAggregationRequest.yaml"

requestDeleteRemediationConfiguration :: DeleteRemediationConfiguration -> TestTree
requestDeleteRemediationConfiguration =
  req
    "DeleteRemediationConfiguration"
    "fixture/DeleteRemediationConfiguration.yaml"

requestDeleteRemediationExceptions :: DeleteRemediationExceptions -> TestTree
requestDeleteRemediationExceptions =
  req
    "DeleteRemediationExceptions"
    "fixture/DeleteRemediationExceptions.yaml"

requestDeleteResourceConfig :: DeleteResourceConfig -> TestTree
requestDeleteResourceConfig =
  req
    "DeleteResourceConfig"
    "fixture/DeleteResourceConfig.yaml"

requestDeleteRetentionConfiguration :: DeleteRetentionConfiguration -> TestTree
requestDeleteRetentionConfiguration =
  req
    "DeleteRetentionConfiguration"
    "fixture/DeleteRetentionConfiguration.yaml"

requestDeleteStoredQuery :: DeleteStoredQuery -> TestTree
requestDeleteStoredQuery =
  req
    "DeleteStoredQuery"
    "fixture/DeleteStoredQuery.yaml"

requestDeliverConfigSnapshot :: DeliverConfigSnapshot -> TestTree
requestDeliverConfigSnapshot =
  req
    "DeliverConfigSnapshot"
    "fixture/DeliverConfigSnapshot.yaml"

requestDescribeAggregateComplianceByConfigRules :: DescribeAggregateComplianceByConfigRules -> TestTree
requestDescribeAggregateComplianceByConfigRules =
  req
    "DescribeAggregateComplianceByConfigRules"
    "fixture/DescribeAggregateComplianceByConfigRules.yaml"

requestDescribeAggregateComplianceByConformancePacks :: DescribeAggregateComplianceByConformancePacks -> TestTree
requestDescribeAggregateComplianceByConformancePacks =
  req
    "DescribeAggregateComplianceByConformancePacks"
    "fixture/DescribeAggregateComplianceByConformancePacks.yaml"

requestDescribeAggregationAuthorizations :: DescribeAggregationAuthorizations -> TestTree
requestDescribeAggregationAuthorizations =
  req
    "DescribeAggregationAuthorizations"
    "fixture/DescribeAggregationAuthorizations.yaml"

requestDescribeComplianceByConfigRule :: DescribeComplianceByConfigRule -> TestTree
requestDescribeComplianceByConfigRule =
  req
    "DescribeComplianceByConfigRule"
    "fixture/DescribeComplianceByConfigRule.yaml"

requestDescribeComplianceByResource :: DescribeComplianceByResource -> TestTree
requestDescribeComplianceByResource =
  req
    "DescribeComplianceByResource"
    "fixture/DescribeComplianceByResource.yaml"

requestDescribeConfigRuleEvaluationStatus :: DescribeConfigRuleEvaluationStatus -> TestTree
requestDescribeConfigRuleEvaluationStatus =
  req
    "DescribeConfigRuleEvaluationStatus"
    "fixture/DescribeConfigRuleEvaluationStatus.yaml"

requestDescribeConfigRules :: DescribeConfigRules -> TestTree
requestDescribeConfigRules =
  req
    "DescribeConfigRules"
    "fixture/DescribeConfigRules.yaml"

requestDescribeConfigurationAggregatorSourcesStatus :: DescribeConfigurationAggregatorSourcesStatus -> TestTree
requestDescribeConfigurationAggregatorSourcesStatus =
  req
    "DescribeConfigurationAggregatorSourcesStatus"
    "fixture/DescribeConfigurationAggregatorSourcesStatus.yaml"

requestDescribeConfigurationAggregators :: DescribeConfigurationAggregators -> TestTree
requestDescribeConfigurationAggregators =
  req
    "DescribeConfigurationAggregators"
    "fixture/DescribeConfigurationAggregators.yaml"

requestDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatus -> TestTree
requestDescribeConfigurationRecorderStatus =
  req
    "DescribeConfigurationRecorderStatus"
    "fixture/DescribeConfigurationRecorderStatus.yaml"

requestDescribeConfigurationRecorders :: DescribeConfigurationRecorders -> TestTree
requestDescribeConfigurationRecorders =
  req
    "DescribeConfigurationRecorders"
    "fixture/DescribeConfigurationRecorders.yaml"

requestDescribeConformancePackCompliance :: DescribeConformancePackCompliance -> TestTree
requestDescribeConformancePackCompliance =
  req
    "DescribeConformancePackCompliance"
    "fixture/DescribeConformancePackCompliance.yaml"

requestDescribeConformancePackStatus :: DescribeConformancePackStatus -> TestTree
requestDescribeConformancePackStatus =
  req
    "DescribeConformancePackStatus"
    "fixture/DescribeConformancePackStatus.yaml"

requestDescribeConformancePacks :: DescribeConformancePacks -> TestTree
requestDescribeConformancePacks =
  req
    "DescribeConformancePacks"
    "fixture/DescribeConformancePacks.yaml"

requestDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatus -> TestTree
requestDescribeDeliveryChannelStatus =
  req
    "DescribeDeliveryChannelStatus"
    "fixture/DescribeDeliveryChannelStatus.yaml"

requestDescribeDeliveryChannels :: DescribeDeliveryChannels -> TestTree
requestDescribeDeliveryChannels =
  req
    "DescribeDeliveryChannels"
    "fixture/DescribeDeliveryChannels.yaml"

requestDescribeOrganizationConfigRuleStatuses :: DescribeOrganizationConfigRuleStatuses -> TestTree
requestDescribeOrganizationConfigRuleStatuses =
  req
    "DescribeOrganizationConfigRuleStatuses"
    "fixture/DescribeOrganizationConfigRuleStatuses.yaml"

requestDescribeOrganizationConfigRules :: DescribeOrganizationConfigRules -> TestTree
requestDescribeOrganizationConfigRules =
  req
    "DescribeOrganizationConfigRules"
    "fixture/DescribeOrganizationConfigRules.yaml"

requestDescribeOrganizationConformancePackStatuses :: DescribeOrganizationConformancePackStatuses -> TestTree
requestDescribeOrganizationConformancePackStatuses =
  req
    "DescribeOrganizationConformancePackStatuses"
    "fixture/DescribeOrganizationConformancePackStatuses.yaml"

requestDescribeOrganizationConformancePacks :: DescribeOrganizationConformancePacks -> TestTree
requestDescribeOrganizationConformancePacks =
  req
    "DescribeOrganizationConformancePacks"
    "fixture/DescribeOrganizationConformancePacks.yaml"

requestDescribePendingAggregationRequests :: DescribePendingAggregationRequests -> TestTree
requestDescribePendingAggregationRequests =
  req
    "DescribePendingAggregationRequests"
    "fixture/DescribePendingAggregationRequests.yaml"

requestDescribeRemediationConfigurations :: DescribeRemediationConfigurations -> TestTree
requestDescribeRemediationConfigurations =
  req
    "DescribeRemediationConfigurations"
    "fixture/DescribeRemediationConfigurations.yaml"

requestDescribeRemediationExceptions :: DescribeRemediationExceptions -> TestTree
requestDescribeRemediationExceptions =
  req
    "DescribeRemediationExceptions"
    "fixture/DescribeRemediationExceptions.yaml"

requestDescribeRemediationExecutionStatus :: DescribeRemediationExecutionStatus -> TestTree
requestDescribeRemediationExecutionStatus =
  req
    "DescribeRemediationExecutionStatus"
    "fixture/DescribeRemediationExecutionStatus.yaml"

requestDescribeRetentionConfigurations :: DescribeRetentionConfigurations -> TestTree
requestDescribeRetentionConfigurations =
  req
    "DescribeRetentionConfigurations"
    "fixture/DescribeRetentionConfigurations.yaml"

requestGetAggregateComplianceDetailsByConfigRule :: GetAggregateComplianceDetailsByConfigRule -> TestTree
requestGetAggregateComplianceDetailsByConfigRule =
  req
    "GetAggregateComplianceDetailsByConfigRule"
    "fixture/GetAggregateComplianceDetailsByConfigRule.yaml"

requestGetAggregateConfigRuleComplianceSummary :: GetAggregateConfigRuleComplianceSummary -> TestTree
requestGetAggregateConfigRuleComplianceSummary =
  req
    "GetAggregateConfigRuleComplianceSummary"
    "fixture/GetAggregateConfigRuleComplianceSummary.yaml"

requestGetAggregateConformancePackComplianceSummary :: GetAggregateConformancePackComplianceSummary -> TestTree
requestGetAggregateConformancePackComplianceSummary =
  req
    "GetAggregateConformancePackComplianceSummary"
    "fixture/GetAggregateConformancePackComplianceSummary.yaml"

requestGetAggregateDiscoveredResourceCounts :: GetAggregateDiscoveredResourceCounts -> TestTree
requestGetAggregateDiscoveredResourceCounts =
  req
    "GetAggregateDiscoveredResourceCounts"
    "fixture/GetAggregateDiscoveredResourceCounts.yaml"

requestGetAggregateResourceConfig :: GetAggregateResourceConfig -> TestTree
requestGetAggregateResourceConfig =
  req
    "GetAggregateResourceConfig"
    "fixture/GetAggregateResourceConfig.yaml"

requestGetComplianceDetailsByConfigRule :: GetComplianceDetailsByConfigRule -> TestTree
requestGetComplianceDetailsByConfigRule =
  req
    "GetComplianceDetailsByConfigRule"
    "fixture/GetComplianceDetailsByConfigRule.yaml"

requestGetComplianceDetailsByResource :: GetComplianceDetailsByResource -> TestTree
requestGetComplianceDetailsByResource =
  req
    "GetComplianceDetailsByResource"
    "fixture/GetComplianceDetailsByResource.yaml"

requestGetComplianceSummaryByConfigRule :: GetComplianceSummaryByConfigRule -> TestTree
requestGetComplianceSummaryByConfigRule =
  req
    "GetComplianceSummaryByConfigRule"
    "fixture/GetComplianceSummaryByConfigRule.yaml"

requestGetComplianceSummaryByResourceType :: GetComplianceSummaryByResourceType -> TestTree
requestGetComplianceSummaryByResourceType =
  req
    "GetComplianceSummaryByResourceType"
    "fixture/GetComplianceSummaryByResourceType.yaml"

requestGetConformancePackComplianceDetails :: GetConformancePackComplianceDetails -> TestTree
requestGetConformancePackComplianceDetails =
  req
    "GetConformancePackComplianceDetails"
    "fixture/GetConformancePackComplianceDetails.yaml"

requestGetConformancePackComplianceSummary :: GetConformancePackComplianceSummary -> TestTree
requestGetConformancePackComplianceSummary =
  req
    "GetConformancePackComplianceSummary"
    "fixture/GetConformancePackComplianceSummary.yaml"

requestGetCustomRulePolicy :: GetCustomRulePolicy -> TestTree
requestGetCustomRulePolicy =
  req
    "GetCustomRulePolicy"
    "fixture/GetCustomRulePolicy.yaml"

requestGetDiscoveredResourceCounts :: GetDiscoveredResourceCounts -> TestTree
requestGetDiscoveredResourceCounts =
  req
    "GetDiscoveredResourceCounts"
    "fixture/GetDiscoveredResourceCounts.yaml"

requestGetOrganizationConfigRuleDetailedStatus :: GetOrganizationConfigRuleDetailedStatus -> TestTree
requestGetOrganizationConfigRuleDetailedStatus =
  req
    "GetOrganizationConfigRuleDetailedStatus"
    "fixture/GetOrganizationConfigRuleDetailedStatus.yaml"

requestGetOrganizationConformancePackDetailedStatus :: GetOrganizationConformancePackDetailedStatus -> TestTree
requestGetOrganizationConformancePackDetailedStatus =
  req
    "GetOrganizationConformancePackDetailedStatus"
    "fixture/GetOrganizationConformancePackDetailedStatus.yaml"

requestGetOrganizationCustomRulePolicy :: GetOrganizationCustomRulePolicy -> TestTree
requestGetOrganizationCustomRulePolicy =
  req
    "GetOrganizationCustomRulePolicy"
    "fixture/GetOrganizationCustomRulePolicy.yaml"

requestGetResourceConfigHistory :: GetResourceConfigHistory -> TestTree
requestGetResourceConfigHistory =
  req
    "GetResourceConfigHistory"
    "fixture/GetResourceConfigHistory.yaml"

requestGetResourceEvaluationSummary :: GetResourceEvaluationSummary -> TestTree
requestGetResourceEvaluationSummary =
  req
    "GetResourceEvaluationSummary"
    "fixture/GetResourceEvaluationSummary.yaml"

requestGetStoredQuery :: GetStoredQuery -> TestTree
requestGetStoredQuery =
  req
    "GetStoredQuery"
    "fixture/GetStoredQuery.yaml"

requestListAggregateDiscoveredResources :: ListAggregateDiscoveredResources -> TestTree
requestListAggregateDiscoveredResources =
  req
    "ListAggregateDiscoveredResources"
    "fixture/ListAggregateDiscoveredResources.yaml"

requestListConformancePackComplianceScores :: ListConformancePackComplianceScores -> TestTree
requestListConformancePackComplianceScores =
  req
    "ListConformancePackComplianceScores"
    "fixture/ListConformancePackComplianceScores.yaml"

requestListDiscoveredResources :: ListDiscoveredResources -> TestTree
requestListDiscoveredResources =
  req
    "ListDiscoveredResources"
    "fixture/ListDiscoveredResources.yaml"

requestListResourceEvaluations :: ListResourceEvaluations -> TestTree
requestListResourceEvaluations =
  req
    "ListResourceEvaluations"
    "fixture/ListResourceEvaluations.yaml"

requestListStoredQueries :: ListStoredQueries -> TestTree
requestListStoredQueries =
  req
    "ListStoredQueries"
    "fixture/ListStoredQueries.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutAggregationAuthorization :: PutAggregationAuthorization -> TestTree
requestPutAggregationAuthorization =
  req
    "PutAggregationAuthorization"
    "fixture/PutAggregationAuthorization.yaml"

requestPutConfigRule :: PutConfigRule -> TestTree
requestPutConfigRule =
  req
    "PutConfigRule"
    "fixture/PutConfigRule.yaml"

requestPutConfigurationAggregator :: PutConfigurationAggregator -> TestTree
requestPutConfigurationAggregator =
  req
    "PutConfigurationAggregator"
    "fixture/PutConfigurationAggregator.yaml"

requestPutConfigurationRecorder :: PutConfigurationRecorder -> TestTree
requestPutConfigurationRecorder =
  req
    "PutConfigurationRecorder"
    "fixture/PutConfigurationRecorder.yaml"

requestPutConformancePack :: PutConformancePack -> TestTree
requestPutConformancePack =
  req
    "PutConformancePack"
    "fixture/PutConformancePack.yaml"

requestPutDeliveryChannel :: PutDeliveryChannel -> TestTree
requestPutDeliveryChannel =
  req
    "PutDeliveryChannel"
    "fixture/PutDeliveryChannel.yaml"

requestPutEvaluations :: PutEvaluations -> TestTree
requestPutEvaluations =
  req
    "PutEvaluations"
    "fixture/PutEvaluations.yaml"

requestPutExternalEvaluation :: PutExternalEvaluation -> TestTree
requestPutExternalEvaluation =
  req
    "PutExternalEvaluation"
    "fixture/PutExternalEvaluation.yaml"

requestPutOrganizationConfigRule :: PutOrganizationConfigRule -> TestTree
requestPutOrganizationConfigRule =
  req
    "PutOrganizationConfigRule"
    "fixture/PutOrganizationConfigRule.yaml"

requestPutOrganizationConformancePack :: PutOrganizationConformancePack -> TestTree
requestPutOrganizationConformancePack =
  req
    "PutOrganizationConformancePack"
    "fixture/PutOrganizationConformancePack.yaml"

requestPutRemediationConfigurations :: PutRemediationConfigurations -> TestTree
requestPutRemediationConfigurations =
  req
    "PutRemediationConfigurations"
    "fixture/PutRemediationConfigurations.yaml"

requestPutRemediationExceptions :: PutRemediationExceptions -> TestTree
requestPutRemediationExceptions =
  req
    "PutRemediationExceptions"
    "fixture/PutRemediationExceptions.yaml"

requestPutResourceConfig :: PutResourceConfig -> TestTree
requestPutResourceConfig =
  req
    "PutResourceConfig"
    "fixture/PutResourceConfig.yaml"

requestPutRetentionConfiguration :: PutRetentionConfiguration -> TestTree
requestPutRetentionConfiguration =
  req
    "PutRetentionConfiguration"
    "fixture/PutRetentionConfiguration.yaml"

requestPutStoredQuery :: PutStoredQuery -> TestTree
requestPutStoredQuery =
  req
    "PutStoredQuery"
    "fixture/PutStoredQuery.yaml"

requestSelectAggregateResourceConfig :: SelectAggregateResourceConfig -> TestTree
requestSelectAggregateResourceConfig =
  req
    "SelectAggregateResourceConfig"
    "fixture/SelectAggregateResourceConfig.yaml"

requestSelectResourceConfig :: SelectResourceConfig -> TestTree
requestSelectResourceConfig =
  req
    "SelectResourceConfig"
    "fixture/SelectResourceConfig.yaml"

requestStartConfigRulesEvaluation :: StartConfigRulesEvaluation -> TestTree
requestStartConfigRulesEvaluation =
  req
    "StartConfigRulesEvaluation"
    "fixture/StartConfigRulesEvaluation.yaml"

requestStartConfigurationRecorder :: StartConfigurationRecorder -> TestTree
requestStartConfigurationRecorder =
  req
    "StartConfigurationRecorder"
    "fixture/StartConfigurationRecorder.yaml"

requestStartRemediationExecution :: StartRemediationExecution -> TestTree
requestStartRemediationExecution =
  req
    "StartRemediationExecution"
    "fixture/StartRemediationExecution.yaml"

requestStartResourceEvaluation :: StartResourceEvaluation -> TestTree
requestStartResourceEvaluation =
  req
    "StartResourceEvaluation"
    "fixture/StartResourceEvaluation.yaml"

requestStopConfigurationRecorder :: StopConfigurationRecorder -> TestTree
requestStopConfigurationRecorder =
  req
    "StopConfigurationRecorder"
    "fixture/StopConfigurationRecorder.yaml"

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

-- Responses

responseBatchGetAggregateResourceConfig :: BatchGetAggregateResourceConfigResponse -> TestTree
responseBatchGetAggregateResourceConfig =
  res
    "BatchGetAggregateResourceConfigResponse"
    "fixture/BatchGetAggregateResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetAggregateResourceConfig)

responseBatchGetResourceConfig :: BatchGetResourceConfigResponse -> TestTree
responseBatchGetResourceConfig =
  res
    "BatchGetResourceConfigResponse"
    "fixture/BatchGetResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetResourceConfig)

responseDeleteAggregationAuthorization :: DeleteAggregationAuthorizationResponse -> TestTree
responseDeleteAggregationAuthorization =
  res
    "DeleteAggregationAuthorizationResponse"
    "fixture/DeleteAggregationAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAggregationAuthorization)

responseDeleteConfigRule :: DeleteConfigRuleResponse -> TestTree
responseDeleteConfigRule =
  res
    "DeleteConfigRuleResponse"
    "fixture/DeleteConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigRule)

responseDeleteConfigurationAggregator :: DeleteConfigurationAggregatorResponse -> TestTree
responseDeleteConfigurationAggregator =
  res
    "DeleteConfigurationAggregatorResponse"
    "fixture/DeleteConfigurationAggregatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationAggregator)

responseDeleteConfigurationRecorder :: DeleteConfigurationRecorderResponse -> TestTree
responseDeleteConfigurationRecorder =
  res
    "DeleteConfigurationRecorderResponse"
    "fixture/DeleteConfigurationRecorderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationRecorder)

responseDeleteConformancePack :: DeleteConformancePackResponse -> TestTree
responseDeleteConformancePack =
  res
    "DeleteConformancePackResponse"
    "fixture/DeleteConformancePackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConformancePack)

responseDeleteDeliveryChannel :: DeleteDeliveryChannelResponse -> TestTree
responseDeleteDeliveryChannel =
  res
    "DeleteDeliveryChannelResponse"
    "fixture/DeleteDeliveryChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeliveryChannel)

responseDeleteEvaluationResults :: DeleteEvaluationResultsResponse -> TestTree
responseDeleteEvaluationResults =
  res
    "DeleteEvaluationResultsResponse"
    "fixture/DeleteEvaluationResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEvaluationResults)

responseDeleteOrganizationConfigRule :: DeleteOrganizationConfigRuleResponse -> TestTree
responseDeleteOrganizationConfigRule =
  res
    "DeleteOrganizationConfigRuleResponse"
    "fixture/DeleteOrganizationConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOrganizationConfigRule)

responseDeleteOrganizationConformancePack :: DeleteOrganizationConformancePackResponse -> TestTree
responseDeleteOrganizationConformancePack =
  res
    "DeleteOrganizationConformancePackResponse"
    "fixture/DeleteOrganizationConformancePackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOrganizationConformancePack)

responseDeletePendingAggregationRequest :: DeletePendingAggregationRequestResponse -> TestTree
responseDeletePendingAggregationRequest =
  res
    "DeletePendingAggregationRequestResponse"
    "fixture/DeletePendingAggregationRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePendingAggregationRequest)

responseDeleteRemediationConfiguration :: DeleteRemediationConfigurationResponse -> TestTree
responseDeleteRemediationConfiguration =
  res
    "DeleteRemediationConfigurationResponse"
    "fixture/DeleteRemediationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRemediationConfiguration)

responseDeleteRemediationExceptions :: DeleteRemediationExceptionsResponse -> TestTree
responseDeleteRemediationExceptions =
  res
    "DeleteRemediationExceptionsResponse"
    "fixture/DeleteRemediationExceptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRemediationExceptions)

responseDeleteResourceConfig :: DeleteResourceConfigResponse -> TestTree
responseDeleteResourceConfig =
  res
    "DeleteResourceConfigResponse"
    "fixture/DeleteResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceConfig)

responseDeleteRetentionConfiguration :: DeleteRetentionConfigurationResponse -> TestTree
responseDeleteRetentionConfiguration =
  res
    "DeleteRetentionConfigurationResponse"
    "fixture/DeleteRetentionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRetentionConfiguration)

responseDeleteStoredQuery :: DeleteStoredQueryResponse -> TestTree
responseDeleteStoredQuery =
  res
    "DeleteStoredQueryResponse"
    "fixture/DeleteStoredQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStoredQuery)

responseDeliverConfigSnapshot :: DeliverConfigSnapshotResponse -> TestTree
responseDeliverConfigSnapshot =
  res
    "DeliverConfigSnapshotResponse"
    "fixture/DeliverConfigSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeliverConfigSnapshot)

responseDescribeAggregateComplianceByConfigRules :: DescribeAggregateComplianceByConfigRulesResponse -> TestTree
responseDescribeAggregateComplianceByConfigRules =
  res
    "DescribeAggregateComplianceByConfigRulesResponse"
    "fixture/DescribeAggregateComplianceByConfigRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAggregateComplianceByConfigRules)

responseDescribeAggregateComplianceByConformancePacks :: DescribeAggregateComplianceByConformancePacksResponse -> TestTree
responseDescribeAggregateComplianceByConformancePacks =
  res
    "DescribeAggregateComplianceByConformancePacksResponse"
    "fixture/DescribeAggregateComplianceByConformancePacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAggregateComplianceByConformancePacks)

responseDescribeAggregationAuthorizations :: DescribeAggregationAuthorizationsResponse -> TestTree
responseDescribeAggregationAuthorizations =
  res
    "DescribeAggregationAuthorizationsResponse"
    "fixture/DescribeAggregationAuthorizationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAggregationAuthorizations)

responseDescribeComplianceByConfigRule :: DescribeComplianceByConfigRuleResponse -> TestTree
responseDescribeComplianceByConfigRule =
  res
    "DescribeComplianceByConfigRuleResponse"
    "fixture/DescribeComplianceByConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComplianceByConfigRule)

responseDescribeComplianceByResource :: DescribeComplianceByResourceResponse -> TestTree
responseDescribeComplianceByResource =
  res
    "DescribeComplianceByResourceResponse"
    "fixture/DescribeComplianceByResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComplianceByResource)

responseDescribeConfigRuleEvaluationStatus :: DescribeConfigRuleEvaluationStatusResponse -> TestTree
responseDescribeConfigRuleEvaluationStatus =
  res
    "DescribeConfigRuleEvaluationStatusResponse"
    "fixture/DescribeConfigRuleEvaluationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigRuleEvaluationStatus)

responseDescribeConfigRules :: DescribeConfigRulesResponse -> TestTree
responseDescribeConfigRules =
  res
    "DescribeConfigRulesResponse"
    "fixture/DescribeConfigRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigRules)

responseDescribeConfigurationAggregatorSourcesStatus :: DescribeConfigurationAggregatorSourcesStatusResponse -> TestTree
responseDescribeConfigurationAggregatorSourcesStatus =
  res
    "DescribeConfigurationAggregatorSourcesStatusResponse"
    "fixture/DescribeConfigurationAggregatorSourcesStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationAggregatorSourcesStatus)

responseDescribeConfigurationAggregators :: DescribeConfigurationAggregatorsResponse -> TestTree
responseDescribeConfigurationAggregators =
  res
    "DescribeConfigurationAggregatorsResponse"
    "fixture/DescribeConfigurationAggregatorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationAggregators)

responseDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatusResponse -> TestTree
responseDescribeConfigurationRecorderStatus =
  res
    "DescribeConfigurationRecorderStatusResponse"
    "fixture/DescribeConfigurationRecorderStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationRecorderStatus)

responseDescribeConfigurationRecorders :: DescribeConfigurationRecordersResponse -> TestTree
responseDescribeConfigurationRecorders =
  res
    "DescribeConfigurationRecordersResponse"
    "fixture/DescribeConfigurationRecordersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationRecorders)

responseDescribeConformancePackCompliance :: DescribeConformancePackComplianceResponse -> TestTree
responseDescribeConformancePackCompliance =
  res
    "DescribeConformancePackComplianceResponse"
    "fixture/DescribeConformancePackComplianceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConformancePackCompliance)

responseDescribeConformancePackStatus :: DescribeConformancePackStatusResponse -> TestTree
responseDescribeConformancePackStatus =
  res
    "DescribeConformancePackStatusResponse"
    "fixture/DescribeConformancePackStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConformancePackStatus)

responseDescribeConformancePacks :: DescribeConformancePacksResponse -> TestTree
responseDescribeConformancePacks =
  res
    "DescribeConformancePacksResponse"
    "fixture/DescribeConformancePacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConformancePacks)

responseDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatusResponse -> TestTree
responseDescribeDeliveryChannelStatus =
  res
    "DescribeDeliveryChannelStatusResponse"
    "fixture/DescribeDeliveryChannelStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeliveryChannelStatus)

responseDescribeDeliveryChannels :: DescribeDeliveryChannelsResponse -> TestTree
responseDescribeDeliveryChannels =
  res
    "DescribeDeliveryChannelsResponse"
    "fixture/DescribeDeliveryChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeliveryChannels)

responseDescribeOrganizationConfigRuleStatuses :: DescribeOrganizationConfigRuleStatusesResponse -> TestTree
responseDescribeOrganizationConfigRuleStatuses =
  res
    "DescribeOrganizationConfigRuleStatusesResponse"
    "fixture/DescribeOrganizationConfigRuleStatusesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConfigRuleStatuses)

responseDescribeOrganizationConfigRules :: DescribeOrganizationConfigRulesResponse -> TestTree
responseDescribeOrganizationConfigRules =
  res
    "DescribeOrganizationConfigRulesResponse"
    "fixture/DescribeOrganizationConfigRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConfigRules)

responseDescribeOrganizationConformancePackStatuses :: DescribeOrganizationConformancePackStatusesResponse -> TestTree
responseDescribeOrganizationConformancePackStatuses =
  res
    "DescribeOrganizationConformancePackStatusesResponse"
    "fixture/DescribeOrganizationConformancePackStatusesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConformancePackStatuses)

responseDescribeOrganizationConformancePacks :: DescribeOrganizationConformancePacksResponse -> TestTree
responseDescribeOrganizationConformancePacks =
  res
    "DescribeOrganizationConformancePacksResponse"
    "fixture/DescribeOrganizationConformancePacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConformancePacks)

responseDescribePendingAggregationRequests :: DescribePendingAggregationRequestsResponse -> TestTree
responseDescribePendingAggregationRequests =
  res
    "DescribePendingAggregationRequestsResponse"
    "fixture/DescribePendingAggregationRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePendingAggregationRequests)

responseDescribeRemediationConfigurations :: DescribeRemediationConfigurationsResponse -> TestTree
responseDescribeRemediationConfigurations =
  res
    "DescribeRemediationConfigurationsResponse"
    "fixture/DescribeRemediationConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRemediationConfigurations)

responseDescribeRemediationExceptions :: DescribeRemediationExceptionsResponse -> TestTree
responseDescribeRemediationExceptions =
  res
    "DescribeRemediationExceptionsResponse"
    "fixture/DescribeRemediationExceptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRemediationExceptions)

responseDescribeRemediationExecutionStatus :: DescribeRemediationExecutionStatusResponse -> TestTree
responseDescribeRemediationExecutionStatus =
  res
    "DescribeRemediationExecutionStatusResponse"
    "fixture/DescribeRemediationExecutionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRemediationExecutionStatus)

responseDescribeRetentionConfigurations :: DescribeRetentionConfigurationsResponse -> TestTree
responseDescribeRetentionConfigurations =
  res
    "DescribeRetentionConfigurationsResponse"
    "fixture/DescribeRetentionConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRetentionConfigurations)

responseGetAggregateComplianceDetailsByConfigRule :: GetAggregateComplianceDetailsByConfigRuleResponse -> TestTree
responseGetAggregateComplianceDetailsByConfigRule =
  res
    "GetAggregateComplianceDetailsByConfigRuleResponse"
    "fixture/GetAggregateComplianceDetailsByConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAggregateComplianceDetailsByConfigRule)

responseGetAggregateConfigRuleComplianceSummary :: GetAggregateConfigRuleComplianceSummaryResponse -> TestTree
responseGetAggregateConfigRuleComplianceSummary =
  res
    "GetAggregateConfigRuleComplianceSummaryResponse"
    "fixture/GetAggregateConfigRuleComplianceSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAggregateConfigRuleComplianceSummary)

responseGetAggregateConformancePackComplianceSummary :: GetAggregateConformancePackComplianceSummaryResponse -> TestTree
responseGetAggregateConformancePackComplianceSummary =
  res
    "GetAggregateConformancePackComplianceSummaryResponse"
    "fixture/GetAggregateConformancePackComplianceSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAggregateConformancePackComplianceSummary)

responseGetAggregateDiscoveredResourceCounts :: GetAggregateDiscoveredResourceCountsResponse -> TestTree
responseGetAggregateDiscoveredResourceCounts =
  res
    "GetAggregateDiscoveredResourceCountsResponse"
    "fixture/GetAggregateDiscoveredResourceCountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAggregateDiscoveredResourceCounts)

responseGetAggregateResourceConfig :: GetAggregateResourceConfigResponse -> TestTree
responseGetAggregateResourceConfig =
  res
    "GetAggregateResourceConfigResponse"
    "fixture/GetAggregateResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAggregateResourceConfig)

responseGetComplianceDetailsByConfigRule :: GetComplianceDetailsByConfigRuleResponse -> TestTree
responseGetComplianceDetailsByConfigRule =
  res
    "GetComplianceDetailsByConfigRuleResponse"
    "fixture/GetComplianceDetailsByConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComplianceDetailsByConfigRule)

responseGetComplianceDetailsByResource :: GetComplianceDetailsByResourceResponse -> TestTree
responseGetComplianceDetailsByResource =
  res
    "GetComplianceDetailsByResourceResponse"
    "fixture/GetComplianceDetailsByResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComplianceDetailsByResource)

responseGetComplianceSummaryByConfigRule :: GetComplianceSummaryByConfigRuleResponse -> TestTree
responseGetComplianceSummaryByConfigRule =
  res
    "GetComplianceSummaryByConfigRuleResponse"
    "fixture/GetComplianceSummaryByConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComplianceSummaryByConfigRule)

responseGetComplianceSummaryByResourceType :: GetComplianceSummaryByResourceTypeResponse -> TestTree
responseGetComplianceSummaryByResourceType =
  res
    "GetComplianceSummaryByResourceTypeResponse"
    "fixture/GetComplianceSummaryByResourceTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComplianceSummaryByResourceType)

responseGetConformancePackComplianceDetails :: GetConformancePackComplianceDetailsResponse -> TestTree
responseGetConformancePackComplianceDetails =
  res
    "GetConformancePackComplianceDetailsResponse"
    "fixture/GetConformancePackComplianceDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConformancePackComplianceDetails)

responseGetConformancePackComplianceSummary :: GetConformancePackComplianceSummaryResponse -> TestTree
responseGetConformancePackComplianceSummary =
  res
    "GetConformancePackComplianceSummaryResponse"
    "fixture/GetConformancePackComplianceSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConformancePackComplianceSummary)

responseGetCustomRulePolicy :: GetCustomRulePolicyResponse -> TestTree
responseGetCustomRulePolicy =
  res
    "GetCustomRulePolicyResponse"
    "fixture/GetCustomRulePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCustomRulePolicy)

responseGetDiscoveredResourceCounts :: GetDiscoveredResourceCountsResponse -> TestTree
responseGetDiscoveredResourceCounts =
  res
    "GetDiscoveredResourceCountsResponse"
    "fixture/GetDiscoveredResourceCountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDiscoveredResourceCounts)

responseGetOrganizationConfigRuleDetailedStatus :: GetOrganizationConfigRuleDetailedStatusResponse -> TestTree
responseGetOrganizationConfigRuleDetailedStatus =
  res
    "GetOrganizationConfigRuleDetailedStatusResponse"
    "fixture/GetOrganizationConfigRuleDetailedStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOrganizationConfigRuleDetailedStatus)

responseGetOrganizationConformancePackDetailedStatus :: GetOrganizationConformancePackDetailedStatusResponse -> TestTree
responseGetOrganizationConformancePackDetailedStatus =
  res
    "GetOrganizationConformancePackDetailedStatusResponse"
    "fixture/GetOrganizationConformancePackDetailedStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOrganizationConformancePackDetailedStatus)

responseGetOrganizationCustomRulePolicy :: GetOrganizationCustomRulePolicyResponse -> TestTree
responseGetOrganizationCustomRulePolicy =
  res
    "GetOrganizationCustomRulePolicyResponse"
    "fixture/GetOrganizationCustomRulePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOrganizationCustomRulePolicy)

responseGetResourceConfigHistory :: GetResourceConfigHistoryResponse -> TestTree
responseGetResourceConfigHistory =
  res
    "GetResourceConfigHistoryResponse"
    "fixture/GetResourceConfigHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceConfigHistory)

responseGetResourceEvaluationSummary :: GetResourceEvaluationSummaryResponse -> TestTree
responseGetResourceEvaluationSummary =
  res
    "GetResourceEvaluationSummaryResponse"
    "fixture/GetResourceEvaluationSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceEvaluationSummary)

responseGetStoredQuery :: GetStoredQueryResponse -> TestTree
responseGetStoredQuery =
  res
    "GetStoredQueryResponse"
    "fixture/GetStoredQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStoredQuery)

responseListAggregateDiscoveredResources :: ListAggregateDiscoveredResourcesResponse -> TestTree
responseListAggregateDiscoveredResources =
  res
    "ListAggregateDiscoveredResourcesResponse"
    "fixture/ListAggregateDiscoveredResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAggregateDiscoveredResources)

responseListConformancePackComplianceScores :: ListConformancePackComplianceScoresResponse -> TestTree
responseListConformancePackComplianceScores =
  res
    "ListConformancePackComplianceScoresResponse"
    "fixture/ListConformancePackComplianceScoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConformancePackComplianceScores)

responseListDiscoveredResources :: ListDiscoveredResourcesResponse -> TestTree
responseListDiscoveredResources =
  res
    "ListDiscoveredResourcesResponse"
    "fixture/ListDiscoveredResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDiscoveredResources)

responseListResourceEvaluations :: ListResourceEvaluationsResponse -> TestTree
responseListResourceEvaluations =
  res
    "ListResourceEvaluationsResponse"
    "fixture/ListResourceEvaluationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceEvaluations)

responseListStoredQueries :: ListStoredQueriesResponse -> TestTree
responseListStoredQueries =
  res
    "ListStoredQueriesResponse"
    "fixture/ListStoredQueriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStoredQueries)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutAggregationAuthorization :: PutAggregationAuthorizationResponse -> TestTree
responsePutAggregationAuthorization =
  res
    "PutAggregationAuthorizationResponse"
    "fixture/PutAggregationAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAggregationAuthorization)

responsePutConfigRule :: PutConfigRuleResponse -> TestTree
responsePutConfigRule =
  res
    "PutConfigRuleResponse"
    "fixture/PutConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigRule)

responsePutConfigurationAggregator :: PutConfigurationAggregatorResponse -> TestTree
responsePutConfigurationAggregator =
  res
    "PutConfigurationAggregatorResponse"
    "fixture/PutConfigurationAggregatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigurationAggregator)

responsePutConfigurationRecorder :: PutConfigurationRecorderResponse -> TestTree
responsePutConfigurationRecorder =
  res
    "PutConfigurationRecorderResponse"
    "fixture/PutConfigurationRecorderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigurationRecorder)

responsePutConformancePack :: PutConformancePackResponse -> TestTree
responsePutConformancePack =
  res
    "PutConformancePackResponse"
    "fixture/PutConformancePackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConformancePack)

responsePutDeliveryChannel :: PutDeliveryChannelResponse -> TestTree
responsePutDeliveryChannel =
  res
    "PutDeliveryChannelResponse"
    "fixture/PutDeliveryChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDeliveryChannel)

responsePutEvaluations :: PutEvaluationsResponse -> TestTree
responsePutEvaluations =
  res
    "PutEvaluationsResponse"
    "fixture/PutEvaluationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEvaluations)

responsePutExternalEvaluation :: PutExternalEvaluationResponse -> TestTree
responsePutExternalEvaluation =
  res
    "PutExternalEvaluationResponse"
    "fixture/PutExternalEvaluationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutExternalEvaluation)

responsePutOrganizationConfigRule :: PutOrganizationConfigRuleResponse -> TestTree
responsePutOrganizationConfigRule =
  res
    "PutOrganizationConfigRuleResponse"
    "fixture/PutOrganizationConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutOrganizationConfigRule)

responsePutOrganizationConformancePack :: PutOrganizationConformancePackResponse -> TestTree
responsePutOrganizationConformancePack =
  res
    "PutOrganizationConformancePackResponse"
    "fixture/PutOrganizationConformancePackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutOrganizationConformancePack)

responsePutRemediationConfigurations :: PutRemediationConfigurationsResponse -> TestTree
responsePutRemediationConfigurations =
  res
    "PutRemediationConfigurationsResponse"
    "fixture/PutRemediationConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRemediationConfigurations)

responsePutRemediationExceptions :: PutRemediationExceptionsResponse -> TestTree
responsePutRemediationExceptions =
  res
    "PutRemediationExceptionsResponse"
    "fixture/PutRemediationExceptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRemediationExceptions)

responsePutResourceConfig :: PutResourceConfigResponse -> TestTree
responsePutResourceConfig =
  res
    "PutResourceConfigResponse"
    "fixture/PutResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourceConfig)

responsePutRetentionConfiguration :: PutRetentionConfigurationResponse -> TestTree
responsePutRetentionConfiguration =
  res
    "PutRetentionConfigurationResponse"
    "fixture/PutRetentionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRetentionConfiguration)

responsePutStoredQuery :: PutStoredQueryResponse -> TestTree
responsePutStoredQuery =
  res
    "PutStoredQueryResponse"
    "fixture/PutStoredQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutStoredQuery)

responseSelectAggregateResourceConfig :: SelectAggregateResourceConfigResponse -> TestTree
responseSelectAggregateResourceConfig =
  res
    "SelectAggregateResourceConfigResponse"
    "fixture/SelectAggregateResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SelectAggregateResourceConfig)

responseSelectResourceConfig :: SelectResourceConfigResponse -> TestTree
responseSelectResourceConfig =
  res
    "SelectResourceConfigResponse"
    "fixture/SelectResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SelectResourceConfig)

responseStartConfigRulesEvaluation :: StartConfigRulesEvaluationResponse -> TestTree
responseStartConfigRulesEvaluation =
  res
    "StartConfigRulesEvaluationResponse"
    "fixture/StartConfigRulesEvaluationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartConfigRulesEvaluation)

responseStartConfigurationRecorder :: StartConfigurationRecorderResponse -> TestTree
responseStartConfigurationRecorder =
  res
    "StartConfigurationRecorderResponse"
    "fixture/StartConfigurationRecorderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartConfigurationRecorder)

responseStartRemediationExecution :: StartRemediationExecutionResponse -> TestTree
responseStartRemediationExecution =
  res
    "StartRemediationExecutionResponse"
    "fixture/StartRemediationExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRemediationExecution)

responseStartResourceEvaluation :: StartResourceEvaluationResponse -> TestTree
responseStartResourceEvaluation =
  res
    "StartResourceEvaluationResponse"
    "fixture/StartResourceEvaluationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartResourceEvaluation)

responseStopConfigurationRecorder :: StopConfigurationRecorderResponse -> TestTree
responseStopConfigurationRecorder =
  res
    "StopConfigurationRecorderResponse"
    "fixture/StopConfigurationRecorderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopConfigurationRecorder)

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
