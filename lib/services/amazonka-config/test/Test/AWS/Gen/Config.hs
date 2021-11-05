{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Config
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Config where

import Amazonka.Config
import qualified Data.Proxy as Proxy
import Test.AWS.Config.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribePendingAggregationRequests $
--             newDescribePendingAggregationRequests
--
--         , requestDescribeRemediationExecutionStatus $
--             newDescribeRemediationExecutionStatus
--
--         , requestGetResourceConfigHistory $
--             newGetResourceConfigHistory
--
--         , requestGetStoredQuery $
--             newGetStoredQuery
--
--         , requestGetAggregateResourceConfig $
--             newGetAggregateResourceConfig
--
--         , requestDescribeConfigurationAggregators $
--             newDescribeConfigurationAggregators
--
--         , requestDescribeComplianceByConfigRule $
--             newDescribeComplianceByConfigRule
--
--         , requestDescribeRetentionConfigurations $
--             newDescribeRetentionConfigurations
--
--         , requestStopConfigurationRecorder $
--             newStopConfigurationRecorder
--
--         , requestGetAggregateConfigRuleComplianceSummary $
--             newGetAggregateConfigRuleComplianceSummary
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestBatchGetResourceConfig $
--             newBatchGetResourceConfig
--
--         , requestDescribeConfigRules $
--             newDescribeConfigRules
--
--         , requestPutRetentionConfiguration $
--             newPutRetentionConfiguration
--
--         , requestDescribeAggregateComplianceByConformancePacks $
--             newDescribeAggregateComplianceByConformancePacks
--
--         , requestGetOrganizationConformancePackDetailedStatus $
--             newGetOrganizationConformancePackDetailedStatus
--
--         , requestDescribeAggregateComplianceByConfigRules $
--             newDescribeAggregateComplianceByConfigRules
--
--         , requestDeleteEvaluationResults $
--             newDeleteEvaluationResults
--
--         , requestPutConfigRule $
--             newPutConfigRule
--
--         , requestGetConformancePackComplianceDetails $
--             newGetConformancePackComplianceDetails
--
--         , requestDeleteConfigRule $
--             newDeleteConfigRule
--
--         , requestDeleteRetentionConfiguration $
--             newDeleteRetentionConfiguration
--
--         , requestListStoredQueries $
--             newListStoredQueries
--
--         , requestSelectResourceConfig $
--             newSelectResourceConfig
--
--         , requestListAggregateDiscoveredResources $
--             newListAggregateDiscoveredResources
--
--         , requestDescribeOrganizationConfigRuleStatuses $
--             newDescribeOrganizationConfigRuleStatuses
--
--         , requestDescribeOrganizationConformancePackStatuses $
--             newDescribeOrganizationConformancePackStatuses
--
--         , requestGetComplianceDetailsByResource $
--             newGetComplianceDetailsByResource
--
--         , requestDeletePendingAggregationRequest $
--             newDeletePendingAggregationRequest
--
--         , requestDeliverConfigSnapshot $
--             newDeliverConfigSnapshot
--
--         , requestBatchGetAggregateResourceConfig $
--             newBatchGetAggregateResourceConfig
--
--         , requestDescribeConfigRuleEvaluationStatus $
--             newDescribeConfigRuleEvaluationStatus
--
--         , requestGetDiscoveredResourceCounts $
--             newGetDiscoveredResourceCounts
--
--         , requestDescribeRemediationExceptions $
--             newDescribeRemediationExceptions
--
--         , requestDeleteOrganizationConformancePack $
--             newDeleteOrganizationConformancePack
--
--         , requestPutOrganizationConfigRule $
--             newPutOrganizationConfigRule
--
--         , requestPutOrganizationConformancePack $
--             newPutOrganizationConformancePack
--
--         , requestDeleteOrganizationConfigRule $
--             newDeleteOrganizationConfigRule
--
--         , requestPutResourceConfig $
--             newPutResourceConfig
--
--         , requestStartConfigRulesEvaluation $
--             newStartConfigRulesEvaluation
--
--         , requestDescribeOrganizationConfigRules $
--             newDescribeOrganizationConfigRules
--
--         , requestSelectAggregateResourceConfig $
--             newSelectAggregateResourceConfig
--
--         , requestDescribeComplianceByResource $
--             newDescribeComplianceByResource
--
--         , requestDescribeOrganizationConformancePacks $
--             newDescribeOrganizationConformancePacks
--
--         , requestDeleteResourceConfig $
--             newDeleteResourceConfig
--
--         , requestPutEvaluations $
--             newPutEvaluations
--
--         , requestDescribeConfigurationRecorders $
--             newDescribeConfigurationRecorders
--
--         , requestDescribeConformancePackCompliance $
--             newDescribeConformancePackCompliance
--
--         , requestGetAggregateComplianceDetailsByConfigRule $
--             newGetAggregateComplianceDetailsByConfigRule
--
--         , requestGetAggregateDiscoveredResourceCounts $
--             newGetAggregateDiscoveredResourceCounts
--
--         , requestGetAggregateConformancePackComplianceSummary $
--             newGetAggregateConformancePackComplianceSummary
--
--         , requestStartConfigurationRecorder $
--             newStartConfigurationRecorder
--
--         , requestDescribeConformancePacks $
--             newDescribeConformancePacks
--
--         , requestPutExternalEvaluation $
--             newPutExternalEvaluation
--
--         , requestDeleteRemediationExceptions $
--             newDeleteRemediationExceptions
--
--         , requestPutRemediationExceptions $
--             newPutRemediationExceptions
--
--         , requestGetOrganizationConfigRuleDetailedStatus $
--             newGetOrganizationConfigRuleDetailedStatus
--
--         , requestPutRemediationConfigurations $
--             newPutRemediationConfigurations
--
--         , requestDeleteConformancePack $
--             newDeleteConformancePack
--
--         , requestPutConformancePack $
--             newPutConformancePack
--
--         , requestStartRemediationExecution $
--             newStartRemediationExecution
--
--         , requestDescribeConformancePackStatus $
--             newDescribeConformancePackStatus
--
--         , requestGetComplianceSummaryByConfigRule $
--             newGetComplianceSummaryByConfigRule
--
--         , requestPutStoredQuery $
--             newPutStoredQuery
--
--         , requestPutConfigurationAggregator $
--             newPutConfigurationAggregator
--
--         , requestDeleteStoredQuery $
--             newDeleteStoredQuery
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteConfigurationAggregator $
--             newDeleteConfigurationAggregator
--
--         , requestDescribeConfigurationRecorderStatus $
--             newDescribeConfigurationRecorderStatus
--
--         , requestPutConfigurationRecorder $
--             newPutConfigurationRecorder
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteConfigurationRecorder $
--             newDeleteConfigurationRecorder
--
--         , requestGetConformancePackComplianceSummary $
--             newGetConformancePackComplianceSummary
--
--         , requestGetComplianceSummaryByResourceType $
--             newGetComplianceSummaryByResourceType
--
--         , requestDescribeDeliveryChannelStatus $
--             newDescribeDeliveryChannelStatus
--
--         , requestPutDeliveryChannel $
--             newPutDeliveryChannel
--
--         , requestGetComplianceDetailsByConfigRule $
--             newGetComplianceDetailsByConfigRule
--
--         , requestDeleteAggregationAuthorization $
--             newDeleteAggregationAuthorization
--
--         , requestDeleteDeliveryChannel $
--             newDeleteDeliveryChannel
--
--         , requestDeleteRemediationConfiguration $
--             newDeleteRemediationConfiguration
--
--         , requestPutAggregationAuthorization $
--             newPutAggregationAuthorization
--
--         , requestDescribeConfigurationAggregatorSourcesStatus $
--             newDescribeConfigurationAggregatorSourcesStatus
--
--         , requestListDiscoveredResources $
--             newListDiscoveredResources
--
--         , requestDescribeRemediationConfigurations $
--             newDescribeRemediationConfigurations
--
--         , requestDescribeDeliveryChannels $
--             newDescribeDeliveryChannels
--
--         , requestDescribeAggregationAuthorizations $
--             newDescribeAggregationAuthorizations
--
--           ]

--     , testGroup "response"
--         [ responseDescribePendingAggregationRequests $
--             newDescribePendingAggregationRequestsResponse
--
--         , responseDescribeRemediationExecutionStatus $
--             newDescribeRemediationExecutionStatusResponse
--
--         , responseGetResourceConfigHistory $
--             newGetResourceConfigHistoryResponse
--
--         , responseGetStoredQuery $
--             newGetStoredQueryResponse
--
--         , responseGetAggregateResourceConfig $
--             newGetAggregateResourceConfigResponse
--
--         , responseDescribeConfigurationAggregators $
--             newDescribeConfigurationAggregatorsResponse
--
--         , responseDescribeComplianceByConfigRule $
--             newDescribeComplianceByConfigRuleResponse
--
--         , responseDescribeRetentionConfigurations $
--             newDescribeRetentionConfigurationsResponse
--
--         , responseStopConfigurationRecorder $
--             newStopConfigurationRecorderResponse
--
--         , responseGetAggregateConfigRuleComplianceSummary $
--             newGetAggregateConfigRuleComplianceSummaryResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseBatchGetResourceConfig $
--             newBatchGetResourceConfigResponse
--
--         , responseDescribeConfigRules $
--             newDescribeConfigRulesResponse
--
--         , responsePutRetentionConfiguration $
--             newPutRetentionConfigurationResponse
--
--         , responseDescribeAggregateComplianceByConformancePacks $
--             newDescribeAggregateComplianceByConformancePacksResponse
--
--         , responseGetOrganizationConformancePackDetailedStatus $
--             newGetOrganizationConformancePackDetailedStatusResponse
--
--         , responseDescribeAggregateComplianceByConfigRules $
--             newDescribeAggregateComplianceByConfigRulesResponse
--
--         , responseDeleteEvaluationResults $
--             newDeleteEvaluationResultsResponse
--
--         , responsePutConfigRule $
--             newPutConfigRuleResponse
--
--         , responseGetConformancePackComplianceDetails $
--             newGetConformancePackComplianceDetailsResponse
--
--         , responseDeleteConfigRule $
--             newDeleteConfigRuleResponse
--
--         , responseDeleteRetentionConfiguration $
--             newDeleteRetentionConfigurationResponse
--
--         , responseListStoredQueries $
--             newListStoredQueriesResponse
--
--         , responseSelectResourceConfig $
--             newSelectResourceConfigResponse
--
--         , responseListAggregateDiscoveredResources $
--             newListAggregateDiscoveredResourcesResponse
--
--         , responseDescribeOrganizationConfigRuleStatuses $
--             newDescribeOrganizationConfigRuleStatusesResponse
--
--         , responseDescribeOrganizationConformancePackStatuses $
--             newDescribeOrganizationConformancePackStatusesResponse
--
--         , responseGetComplianceDetailsByResource $
--             newGetComplianceDetailsByResourceResponse
--
--         , responseDeletePendingAggregationRequest $
--             newDeletePendingAggregationRequestResponse
--
--         , responseDeliverConfigSnapshot $
--             newDeliverConfigSnapshotResponse
--
--         , responseBatchGetAggregateResourceConfig $
--             newBatchGetAggregateResourceConfigResponse
--
--         , responseDescribeConfigRuleEvaluationStatus $
--             newDescribeConfigRuleEvaluationStatusResponse
--
--         , responseGetDiscoveredResourceCounts $
--             newGetDiscoveredResourceCountsResponse
--
--         , responseDescribeRemediationExceptions $
--             newDescribeRemediationExceptionsResponse
--
--         , responseDeleteOrganizationConformancePack $
--             newDeleteOrganizationConformancePackResponse
--
--         , responsePutOrganizationConfigRule $
--             newPutOrganizationConfigRuleResponse
--
--         , responsePutOrganizationConformancePack $
--             newPutOrganizationConformancePackResponse
--
--         , responseDeleteOrganizationConfigRule $
--             newDeleteOrganizationConfigRuleResponse
--
--         , responsePutResourceConfig $
--             newPutResourceConfigResponse
--
--         , responseStartConfigRulesEvaluation $
--             newStartConfigRulesEvaluationResponse
--
--         , responseDescribeOrganizationConfigRules $
--             newDescribeOrganizationConfigRulesResponse
--
--         , responseSelectAggregateResourceConfig $
--             newSelectAggregateResourceConfigResponse
--
--         , responseDescribeComplianceByResource $
--             newDescribeComplianceByResourceResponse
--
--         , responseDescribeOrganizationConformancePacks $
--             newDescribeOrganizationConformancePacksResponse
--
--         , responseDeleteResourceConfig $
--             newDeleteResourceConfigResponse
--
--         , responsePutEvaluations $
--             newPutEvaluationsResponse
--
--         , responseDescribeConfigurationRecorders $
--             newDescribeConfigurationRecordersResponse
--
--         , responseDescribeConformancePackCompliance $
--             newDescribeConformancePackComplianceResponse
--
--         , responseGetAggregateComplianceDetailsByConfigRule $
--             newGetAggregateComplianceDetailsByConfigRuleResponse
--
--         , responseGetAggregateDiscoveredResourceCounts $
--             newGetAggregateDiscoveredResourceCountsResponse
--
--         , responseGetAggregateConformancePackComplianceSummary $
--             newGetAggregateConformancePackComplianceSummaryResponse
--
--         , responseStartConfigurationRecorder $
--             newStartConfigurationRecorderResponse
--
--         , responseDescribeConformancePacks $
--             newDescribeConformancePacksResponse
--
--         , responsePutExternalEvaluation $
--             newPutExternalEvaluationResponse
--
--         , responseDeleteRemediationExceptions $
--             newDeleteRemediationExceptionsResponse
--
--         , responsePutRemediationExceptions $
--             newPutRemediationExceptionsResponse
--
--         , responseGetOrganizationConfigRuleDetailedStatus $
--             newGetOrganizationConfigRuleDetailedStatusResponse
--
--         , responsePutRemediationConfigurations $
--             newPutRemediationConfigurationsResponse
--
--         , responseDeleteConformancePack $
--             newDeleteConformancePackResponse
--
--         , responsePutConformancePack $
--             newPutConformancePackResponse
--
--         , responseStartRemediationExecution $
--             newStartRemediationExecutionResponse
--
--         , responseDescribeConformancePackStatus $
--             newDescribeConformancePackStatusResponse
--
--         , responseGetComplianceSummaryByConfigRule $
--             newGetComplianceSummaryByConfigRuleResponse
--
--         , responsePutStoredQuery $
--             newPutStoredQueryResponse
--
--         , responsePutConfigurationAggregator $
--             newPutConfigurationAggregatorResponse
--
--         , responseDeleteStoredQuery $
--             newDeleteStoredQueryResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteConfigurationAggregator $
--             newDeleteConfigurationAggregatorResponse
--
--         , responseDescribeConfigurationRecorderStatus $
--             newDescribeConfigurationRecorderStatusResponse
--
--         , responsePutConfigurationRecorder $
--             newPutConfigurationRecorderResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteConfigurationRecorder $
--             newDeleteConfigurationRecorderResponse
--
--         , responseGetConformancePackComplianceSummary $
--             newGetConformancePackComplianceSummaryResponse
--
--         , responseGetComplianceSummaryByResourceType $
--             newGetComplianceSummaryByResourceTypeResponse
--
--         , responseDescribeDeliveryChannelStatus $
--             newDescribeDeliveryChannelStatusResponse
--
--         , responsePutDeliveryChannel $
--             newPutDeliveryChannelResponse
--
--         , responseGetComplianceDetailsByConfigRule $
--             newGetComplianceDetailsByConfigRuleResponse
--
--         , responseDeleteAggregationAuthorization $
--             newDeleteAggregationAuthorizationResponse
--
--         , responseDeleteDeliveryChannel $
--             newDeleteDeliveryChannelResponse
--
--         , responseDeleteRemediationConfiguration $
--             newDeleteRemediationConfigurationResponse
--
--         , responsePutAggregationAuthorization $
--             newPutAggregationAuthorizationResponse
--
--         , responseDescribeConfigurationAggregatorSourcesStatus $
--             newDescribeConfigurationAggregatorSourcesStatusResponse
--
--         , responseListDiscoveredResources $
--             newListDiscoveredResourcesResponse
--
--         , responseDescribeRemediationConfigurations $
--             newDescribeRemediationConfigurationsResponse
--
--         , responseDescribeDeliveryChannels $
--             newDescribeDeliveryChannelsResponse
--
--         , responseDescribeAggregationAuthorizations $
--             newDescribeAggregationAuthorizationsResponse
--
--           ]
--     ]

-- Requests

requestDescribePendingAggregationRequests :: DescribePendingAggregationRequests -> TestTree
requestDescribePendingAggregationRequests =
  req
    "DescribePendingAggregationRequests"
    "fixture/DescribePendingAggregationRequests.yaml"

requestDescribeRemediationExecutionStatus :: DescribeRemediationExecutionStatus -> TestTree
requestDescribeRemediationExecutionStatus =
  req
    "DescribeRemediationExecutionStatus"
    "fixture/DescribeRemediationExecutionStatus.yaml"

requestGetResourceConfigHistory :: GetResourceConfigHistory -> TestTree
requestGetResourceConfigHistory =
  req
    "GetResourceConfigHistory"
    "fixture/GetResourceConfigHistory.yaml"

requestGetStoredQuery :: GetStoredQuery -> TestTree
requestGetStoredQuery =
  req
    "GetStoredQuery"
    "fixture/GetStoredQuery.yaml"

requestGetAggregateResourceConfig :: GetAggregateResourceConfig -> TestTree
requestGetAggregateResourceConfig =
  req
    "GetAggregateResourceConfig"
    "fixture/GetAggregateResourceConfig.yaml"

requestDescribeConfigurationAggregators :: DescribeConfigurationAggregators -> TestTree
requestDescribeConfigurationAggregators =
  req
    "DescribeConfigurationAggregators"
    "fixture/DescribeConfigurationAggregators.yaml"

requestDescribeComplianceByConfigRule :: DescribeComplianceByConfigRule -> TestTree
requestDescribeComplianceByConfigRule =
  req
    "DescribeComplianceByConfigRule"
    "fixture/DescribeComplianceByConfigRule.yaml"

requestDescribeRetentionConfigurations :: DescribeRetentionConfigurations -> TestTree
requestDescribeRetentionConfigurations =
  req
    "DescribeRetentionConfigurations"
    "fixture/DescribeRetentionConfigurations.yaml"

requestStopConfigurationRecorder :: StopConfigurationRecorder -> TestTree
requestStopConfigurationRecorder =
  req
    "StopConfigurationRecorder"
    "fixture/StopConfigurationRecorder.yaml"

requestGetAggregateConfigRuleComplianceSummary :: GetAggregateConfigRuleComplianceSummary -> TestTree
requestGetAggregateConfigRuleComplianceSummary =
  req
    "GetAggregateConfigRuleComplianceSummary"
    "fixture/GetAggregateConfigRuleComplianceSummary.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestBatchGetResourceConfig :: BatchGetResourceConfig -> TestTree
requestBatchGetResourceConfig =
  req
    "BatchGetResourceConfig"
    "fixture/BatchGetResourceConfig.yaml"

requestDescribeConfigRules :: DescribeConfigRules -> TestTree
requestDescribeConfigRules =
  req
    "DescribeConfigRules"
    "fixture/DescribeConfigRules.yaml"

requestPutRetentionConfiguration :: PutRetentionConfiguration -> TestTree
requestPutRetentionConfiguration =
  req
    "PutRetentionConfiguration"
    "fixture/PutRetentionConfiguration.yaml"

requestDescribeAggregateComplianceByConformancePacks :: DescribeAggregateComplianceByConformancePacks -> TestTree
requestDescribeAggregateComplianceByConformancePacks =
  req
    "DescribeAggregateComplianceByConformancePacks"
    "fixture/DescribeAggregateComplianceByConformancePacks.yaml"

requestGetOrganizationConformancePackDetailedStatus :: GetOrganizationConformancePackDetailedStatus -> TestTree
requestGetOrganizationConformancePackDetailedStatus =
  req
    "GetOrganizationConformancePackDetailedStatus"
    "fixture/GetOrganizationConformancePackDetailedStatus.yaml"

requestDescribeAggregateComplianceByConfigRules :: DescribeAggregateComplianceByConfigRules -> TestTree
requestDescribeAggregateComplianceByConfigRules =
  req
    "DescribeAggregateComplianceByConfigRules"
    "fixture/DescribeAggregateComplianceByConfigRules.yaml"

requestDeleteEvaluationResults :: DeleteEvaluationResults -> TestTree
requestDeleteEvaluationResults =
  req
    "DeleteEvaluationResults"
    "fixture/DeleteEvaluationResults.yaml"

requestPutConfigRule :: PutConfigRule -> TestTree
requestPutConfigRule =
  req
    "PutConfigRule"
    "fixture/PutConfigRule.yaml"

requestGetConformancePackComplianceDetails :: GetConformancePackComplianceDetails -> TestTree
requestGetConformancePackComplianceDetails =
  req
    "GetConformancePackComplianceDetails"
    "fixture/GetConformancePackComplianceDetails.yaml"

requestDeleteConfigRule :: DeleteConfigRule -> TestTree
requestDeleteConfigRule =
  req
    "DeleteConfigRule"
    "fixture/DeleteConfigRule.yaml"

requestDeleteRetentionConfiguration :: DeleteRetentionConfiguration -> TestTree
requestDeleteRetentionConfiguration =
  req
    "DeleteRetentionConfiguration"
    "fixture/DeleteRetentionConfiguration.yaml"

requestListStoredQueries :: ListStoredQueries -> TestTree
requestListStoredQueries =
  req
    "ListStoredQueries"
    "fixture/ListStoredQueries.yaml"

requestSelectResourceConfig :: SelectResourceConfig -> TestTree
requestSelectResourceConfig =
  req
    "SelectResourceConfig"
    "fixture/SelectResourceConfig.yaml"

requestListAggregateDiscoveredResources :: ListAggregateDiscoveredResources -> TestTree
requestListAggregateDiscoveredResources =
  req
    "ListAggregateDiscoveredResources"
    "fixture/ListAggregateDiscoveredResources.yaml"

requestDescribeOrganizationConfigRuleStatuses :: DescribeOrganizationConfigRuleStatuses -> TestTree
requestDescribeOrganizationConfigRuleStatuses =
  req
    "DescribeOrganizationConfigRuleStatuses"
    "fixture/DescribeOrganizationConfigRuleStatuses.yaml"

requestDescribeOrganizationConformancePackStatuses :: DescribeOrganizationConformancePackStatuses -> TestTree
requestDescribeOrganizationConformancePackStatuses =
  req
    "DescribeOrganizationConformancePackStatuses"
    "fixture/DescribeOrganizationConformancePackStatuses.yaml"

requestGetComplianceDetailsByResource :: GetComplianceDetailsByResource -> TestTree
requestGetComplianceDetailsByResource =
  req
    "GetComplianceDetailsByResource"
    "fixture/GetComplianceDetailsByResource.yaml"

requestDeletePendingAggregationRequest :: DeletePendingAggregationRequest -> TestTree
requestDeletePendingAggregationRequest =
  req
    "DeletePendingAggregationRequest"
    "fixture/DeletePendingAggregationRequest.yaml"

requestDeliverConfigSnapshot :: DeliverConfigSnapshot -> TestTree
requestDeliverConfigSnapshot =
  req
    "DeliverConfigSnapshot"
    "fixture/DeliverConfigSnapshot.yaml"

requestBatchGetAggregateResourceConfig :: BatchGetAggregateResourceConfig -> TestTree
requestBatchGetAggregateResourceConfig =
  req
    "BatchGetAggregateResourceConfig"
    "fixture/BatchGetAggregateResourceConfig.yaml"

requestDescribeConfigRuleEvaluationStatus :: DescribeConfigRuleEvaluationStatus -> TestTree
requestDescribeConfigRuleEvaluationStatus =
  req
    "DescribeConfigRuleEvaluationStatus"
    "fixture/DescribeConfigRuleEvaluationStatus.yaml"

requestGetDiscoveredResourceCounts :: GetDiscoveredResourceCounts -> TestTree
requestGetDiscoveredResourceCounts =
  req
    "GetDiscoveredResourceCounts"
    "fixture/GetDiscoveredResourceCounts.yaml"

requestDescribeRemediationExceptions :: DescribeRemediationExceptions -> TestTree
requestDescribeRemediationExceptions =
  req
    "DescribeRemediationExceptions"
    "fixture/DescribeRemediationExceptions.yaml"

requestDeleteOrganizationConformancePack :: DeleteOrganizationConformancePack -> TestTree
requestDeleteOrganizationConformancePack =
  req
    "DeleteOrganizationConformancePack"
    "fixture/DeleteOrganizationConformancePack.yaml"

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

requestDeleteOrganizationConfigRule :: DeleteOrganizationConfigRule -> TestTree
requestDeleteOrganizationConfigRule =
  req
    "DeleteOrganizationConfigRule"
    "fixture/DeleteOrganizationConfigRule.yaml"

requestPutResourceConfig :: PutResourceConfig -> TestTree
requestPutResourceConfig =
  req
    "PutResourceConfig"
    "fixture/PutResourceConfig.yaml"

requestStartConfigRulesEvaluation :: StartConfigRulesEvaluation -> TestTree
requestStartConfigRulesEvaluation =
  req
    "StartConfigRulesEvaluation"
    "fixture/StartConfigRulesEvaluation.yaml"

requestDescribeOrganizationConfigRules :: DescribeOrganizationConfigRules -> TestTree
requestDescribeOrganizationConfigRules =
  req
    "DescribeOrganizationConfigRules"
    "fixture/DescribeOrganizationConfigRules.yaml"

requestSelectAggregateResourceConfig :: SelectAggregateResourceConfig -> TestTree
requestSelectAggregateResourceConfig =
  req
    "SelectAggregateResourceConfig"
    "fixture/SelectAggregateResourceConfig.yaml"

requestDescribeComplianceByResource :: DescribeComplianceByResource -> TestTree
requestDescribeComplianceByResource =
  req
    "DescribeComplianceByResource"
    "fixture/DescribeComplianceByResource.yaml"

requestDescribeOrganizationConformancePacks :: DescribeOrganizationConformancePacks -> TestTree
requestDescribeOrganizationConformancePacks =
  req
    "DescribeOrganizationConformancePacks"
    "fixture/DescribeOrganizationConformancePacks.yaml"

requestDeleteResourceConfig :: DeleteResourceConfig -> TestTree
requestDeleteResourceConfig =
  req
    "DeleteResourceConfig"
    "fixture/DeleteResourceConfig.yaml"

requestPutEvaluations :: PutEvaluations -> TestTree
requestPutEvaluations =
  req
    "PutEvaluations"
    "fixture/PutEvaluations.yaml"

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

requestGetAggregateComplianceDetailsByConfigRule :: GetAggregateComplianceDetailsByConfigRule -> TestTree
requestGetAggregateComplianceDetailsByConfigRule =
  req
    "GetAggregateComplianceDetailsByConfigRule"
    "fixture/GetAggregateComplianceDetailsByConfigRule.yaml"

requestGetAggregateDiscoveredResourceCounts :: GetAggregateDiscoveredResourceCounts -> TestTree
requestGetAggregateDiscoveredResourceCounts =
  req
    "GetAggregateDiscoveredResourceCounts"
    "fixture/GetAggregateDiscoveredResourceCounts.yaml"

requestGetAggregateConformancePackComplianceSummary :: GetAggregateConformancePackComplianceSummary -> TestTree
requestGetAggregateConformancePackComplianceSummary =
  req
    "GetAggregateConformancePackComplianceSummary"
    "fixture/GetAggregateConformancePackComplianceSummary.yaml"

requestStartConfigurationRecorder :: StartConfigurationRecorder -> TestTree
requestStartConfigurationRecorder =
  req
    "StartConfigurationRecorder"
    "fixture/StartConfigurationRecorder.yaml"

requestDescribeConformancePacks :: DescribeConformancePacks -> TestTree
requestDescribeConformancePacks =
  req
    "DescribeConformancePacks"
    "fixture/DescribeConformancePacks.yaml"

requestPutExternalEvaluation :: PutExternalEvaluation -> TestTree
requestPutExternalEvaluation =
  req
    "PutExternalEvaluation"
    "fixture/PutExternalEvaluation.yaml"

requestDeleteRemediationExceptions :: DeleteRemediationExceptions -> TestTree
requestDeleteRemediationExceptions =
  req
    "DeleteRemediationExceptions"
    "fixture/DeleteRemediationExceptions.yaml"

requestPutRemediationExceptions :: PutRemediationExceptions -> TestTree
requestPutRemediationExceptions =
  req
    "PutRemediationExceptions"
    "fixture/PutRemediationExceptions.yaml"

requestGetOrganizationConfigRuleDetailedStatus :: GetOrganizationConfigRuleDetailedStatus -> TestTree
requestGetOrganizationConfigRuleDetailedStatus =
  req
    "GetOrganizationConfigRuleDetailedStatus"
    "fixture/GetOrganizationConfigRuleDetailedStatus.yaml"

requestPutRemediationConfigurations :: PutRemediationConfigurations -> TestTree
requestPutRemediationConfigurations =
  req
    "PutRemediationConfigurations"
    "fixture/PutRemediationConfigurations.yaml"

requestDeleteConformancePack :: DeleteConformancePack -> TestTree
requestDeleteConformancePack =
  req
    "DeleteConformancePack"
    "fixture/DeleteConformancePack.yaml"

requestPutConformancePack :: PutConformancePack -> TestTree
requestPutConformancePack =
  req
    "PutConformancePack"
    "fixture/PutConformancePack.yaml"

requestStartRemediationExecution :: StartRemediationExecution -> TestTree
requestStartRemediationExecution =
  req
    "StartRemediationExecution"
    "fixture/StartRemediationExecution.yaml"

requestDescribeConformancePackStatus :: DescribeConformancePackStatus -> TestTree
requestDescribeConformancePackStatus =
  req
    "DescribeConformancePackStatus"
    "fixture/DescribeConformancePackStatus.yaml"

requestGetComplianceSummaryByConfigRule :: GetComplianceSummaryByConfigRule -> TestTree
requestGetComplianceSummaryByConfigRule =
  req
    "GetComplianceSummaryByConfigRule"
    "fixture/GetComplianceSummaryByConfigRule.yaml"

requestPutStoredQuery :: PutStoredQuery -> TestTree
requestPutStoredQuery =
  req
    "PutStoredQuery"
    "fixture/PutStoredQuery.yaml"

requestPutConfigurationAggregator :: PutConfigurationAggregator -> TestTree
requestPutConfigurationAggregator =
  req
    "PutConfigurationAggregator"
    "fixture/PutConfigurationAggregator.yaml"

requestDeleteStoredQuery :: DeleteStoredQuery -> TestTree
requestDeleteStoredQuery =
  req
    "DeleteStoredQuery"
    "fixture/DeleteStoredQuery.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteConfigurationAggregator :: DeleteConfigurationAggregator -> TestTree
requestDeleteConfigurationAggregator =
  req
    "DeleteConfigurationAggregator"
    "fixture/DeleteConfigurationAggregator.yaml"

requestDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatus -> TestTree
requestDescribeConfigurationRecorderStatus =
  req
    "DescribeConfigurationRecorderStatus"
    "fixture/DescribeConfigurationRecorderStatus.yaml"

requestPutConfigurationRecorder :: PutConfigurationRecorder -> TestTree
requestPutConfigurationRecorder =
  req
    "PutConfigurationRecorder"
    "fixture/PutConfigurationRecorder.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteConfigurationRecorder :: DeleteConfigurationRecorder -> TestTree
requestDeleteConfigurationRecorder =
  req
    "DeleteConfigurationRecorder"
    "fixture/DeleteConfigurationRecorder.yaml"

requestGetConformancePackComplianceSummary :: GetConformancePackComplianceSummary -> TestTree
requestGetConformancePackComplianceSummary =
  req
    "GetConformancePackComplianceSummary"
    "fixture/GetConformancePackComplianceSummary.yaml"

requestGetComplianceSummaryByResourceType :: GetComplianceSummaryByResourceType -> TestTree
requestGetComplianceSummaryByResourceType =
  req
    "GetComplianceSummaryByResourceType"
    "fixture/GetComplianceSummaryByResourceType.yaml"

requestDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatus -> TestTree
requestDescribeDeliveryChannelStatus =
  req
    "DescribeDeliveryChannelStatus"
    "fixture/DescribeDeliveryChannelStatus.yaml"

requestPutDeliveryChannel :: PutDeliveryChannel -> TestTree
requestPutDeliveryChannel =
  req
    "PutDeliveryChannel"
    "fixture/PutDeliveryChannel.yaml"

requestGetComplianceDetailsByConfigRule :: GetComplianceDetailsByConfigRule -> TestTree
requestGetComplianceDetailsByConfigRule =
  req
    "GetComplianceDetailsByConfigRule"
    "fixture/GetComplianceDetailsByConfigRule.yaml"

requestDeleteAggregationAuthorization :: DeleteAggregationAuthorization -> TestTree
requestDeleteAggregationAuthorization =
  req
    "DeleteAggregationAuthorization"
    "fixture/DeleteAggregationAuthorization.yaml"

requestDeleteDeliveryChannel :: DeleteDeliveryChannel -> TestTree
requestDeleteDeliveryChannel =
  req
    "DeleteDeliveryChannel"
    "fixture/DeleteDeliveryChannel.yaml"

requestDeleteRemediationConfiguration :: DeleteRemediationConfiguration -> TestTree
requestDeleteRemediationConfiguration =
  req
    "DeleteRemediationConfiguration"
    "fixture/DeleteRemediationConfiguration.yaml"

requestPutAggregationAuthorization :: PutAggregationAuthorization -> TestTree
requestPutAggregationAuthorization =
  req
    "PutAggregationAuthorization"
    "fixture/PutAggregationAuthorization.yaml"

requestDescribeConfigurationAggregatorSourcesStatus :: DescribeConfigurationAggregatorSourcesStatus -> TestTree
requestDescribeConfigurationAggregatorSourcesStatus =
  req
    "DescribeConfigurationAggregatorSourcesStatus"
    "fixture/DescribeConfigurationAggregatorSourcesStatus.yaml"

requestListDiscoveredResources :: ListDiscoveredResources -> TestTree
requestListDiscoveredResources =
  req
    "ListDiscoveredResources"
    "fixture/ListDiscoveredResources.yaml"

requestDescribeRemediationConfigurations :: DescribeRemediationConfigurations -> TestTree
requestDescribeRemediationConfigurations =
  req
    "DescribeRemediationConfigurations"
    "fixture/DescribeRemediationConfigurations.yaml"

requestDescribeDeliveryChannels :: DescribeDeliveryChannels -> TestTree
requestDescribeDeliveryChannels =
  req
    "DescribeDeliveryChannels"
    "fixture/DescribeDeliveryChannels.yaml"

requestDescribeAggregationAuthorizations :: DescribeAggregationAuthorizations -> TestTree
requestDescribeAggregationAuthorizations =
  req
    "DescribeAggregationAuthorizations"
    "fixture/DescribeAggregationAuthorizations.yaml"

-- Responses

responseDescribePendingAggregationRequests :: DescribePendingAggregationRequestsResponse -> TestTree
responseDescribePendingAggregationRequests =
  res
    "DescribePendingAggregationRequestsResponse"
    "fixture/DescribePendingAggregationRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePendingAggregationRequests)

responseDescribeRemediationExecutionStatus :: DescribeRemediationExecutionStatusResponse -> TestTree
responseDescribeRemediationExecutionStatus =
  res
    "DescribeRemediationExecutionStatusResponse"
    "fixture/DescribeRemediationExecutionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRemediationExecutionStatus)

responseGetResourceConfigHistory :: GetResourceConfigHistoryResponse -> TestTree
responseGetResourceConfigHistory =
  res
    "GetResourceConfigHistoryResponse"
    "fixture/GetResourceConfigHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceConfigHistory)

responseGetStoredQuery :: GetStoredQueryResponse -> TestTree
responseGetStoredQuery =
  res
    "GetStoredQueryResponse"
    "fixture/GetStoredQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStoredQuery)

responseGetAggregateResourceConfig :: GetAggregateResourceConfigResponse -> TestTree
responseGetAggregateResourceConfig =
  res
    "GetAggregateResourceConfigResponse"
    "fixture/GetAggregateResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAggregateResourceConfig)

responseDescribeConfigurationAggregators :: DescribeConfigurationAggregatorsResponse -> TestTree
responseDescribeConfigurationAggregators =
  res
    "DescribeConfigurationAggregatorsResponse"
    "fixture/DescribeConfigurationAggregatorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationAggregators)

responseDescribeComplianceByConfigRule :: DescribeComplianceByConfigRuleResponse -> TestTree
responseDescribeComplianceByConfigRule =
  res
    "DescribeComplianceByConfigRuleResponse"
    "fixture/DescribeComplianceByConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComplianceByConfigRule)

responseDescribeRetentionConfigurations :: DescribeRetentionConfigurationsResponse -> TestTree
responseDescribeRetentionConfigurations =
  res
    "DescribeRetentionConfigurationsResponse"
    "fixture/DescribeRetentionConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRetentionConfigurations)

responseStopConfigurationRecorder :: StopConfigurationRecorderResponse -> TestTree
responseStopConfigurationRecorder =
  res
    "StopConfigurationRecorderResponse"
    "fixture/StopConfigurationRecorderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopConfigurationRecorder)

responseGetAggregateConfigRuleComplianceSummary :: GetAggregateConfigRuleComplianceSummaryResponse -> TestTree
responseGetAggregateConfigRuleComplianceSummary =
  res
    "GetAggregateConfigRuleComplianceSummaryResponse"
    "fixture/GetAggregateConfigRuleComplianceSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAggregateConfigRuleComplianceSummary)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseBatchGetResourceConfig :: BatchGetResourceConfigResponse -> TestTree
responseBatchGetResourceConfig =
  res
    "BatchGetResourceConfigResponse"
    "fixture/BatchGetResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetResourceConfig)

responseDescribeConfigRules :: DescribeConfigRulesResponse -> TestTree
responseDescribeConfigRules =
  res
    "DescribeConfigRulesResponse"
    "fixture/DescribeConfigRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigRules)

responsePutRetentionConfiguration :: PutRetentionConfigurationResponse -> TestTree
responsePutRetentionConfiguration =
  res
    "PutRetentionConfigurationResponse"
    "fixture/PutRetentionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRetentionConfiguration)

responseDescribeAggregateComplianceByConformancePacks :: DescribeAggregateComplianceByConformancePacksResponse -> TestTree
responseDescribeAggregateComplianceByConformancePacks =
  res
    "DescribeAggregateComplianceByConformancePacksResponse"
    "fixture/DescribeAggregateComplianceByConformancePacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAggregateComplianceByConformancePacks)

responseGetOrganizationConformancePackDetailedStatus :: GetOrganizationConformancePackDetailedStatusResponse -> TestTree
responseGetOrganizationConformancePackDetailedStatus =
  res
    "GetOrganizationConformancePackDetailedStatusResponse"
    "fixture/GetOrganizationConformancePackDetailedStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOrganizationConformancePackDetailedStatus)

responseDescribeAggregateComplianceByConfigRules :: DescribeAggregateComplianceByConfigRulesResponse -> TestTree
responseDescribeAggregateComplianceByConfigRules =
  res
    "DescribeAggregateComplianceByConfigRulesResponse"
    "fixture/DescribeAggregateComplianceByConfigRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAggregateComplianceByConfigRules)

responseDeleteEvaluationResults :: DeleteEvaluationResultsResponse -> TestTree
responseDeleteEvaluationResults =
  res
    "DeleteEvaluationResultsResponse"
    "fixture/DeleteEvaluationResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEvaluationResults)

responsePutConfigRule :: PutConfigRuleResponse -> TestTree
responsePutConfigRule =
  res
    "PutConfigRuleResponse"
    "fixture/PutConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigRule)

responseGetConformancePackComplianceDetails :: GetConformancePackComplianceDetailsResponse -> TestTree
responseGetConformancePackComplianceDetails =
  res
    "GetConformancePackComplianceDetailsResponse"
    "fixture/GetConformancePackComplianceDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConformancePackComplianceDetails)

responseDeleteConfigRule :: DeleteConfigRuleResponse -> TestTree
responseDeleteConfigRule =
  res
    "DeleteConfigRuleResponse"
    "fixture/DeleteConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigRule)

responseDeleteRetentionConfiguration :: DeleteRetentionConfigurationResponse -> TestTree
responseDeleteRetentionConfiguration =
  res
    "DeleteRetentionConfigurationResponse"
    "fixture/DeleteRetentionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRetentionConfiguration)

responseListStoredQueries :: ListStoredQueriesResponse -> TestTree
responseListStoredQueries =
  res
    "ListStoredQueriesResponse"
    "fixture/ListStoredQueriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStoredQueries)

responseSelectResourceConfig :: SelectResourceConfigResponse -> TestTree
responseSelectResourceConfig =
  res
    "SelectResourceConfigResponse"
    "fixture/SelectResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SelectResourceConfig)

responseListAggregateDiscoveredResources :: ListAggregateDiscoveredResourcesResponse -> TestTree
responseListAggregateDiscoveredResources =
  res
    "ListAggregateDiscoveredResourcesResponse"
    "fixture/ListAggregateDiscoveredResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAggregateDiscoveredResources)

responseDescribeOrganizationConfigRuleStatuses :: DescribeOrganizationConfigRuleStatusesResponse -> TestTree
responseDescribeOrganizationConfigRuleStatuses =
  res
    "DescribeOrganizationConfigRuleStatusesResponse"
    "fixture/DescribeOrganizationConfigRuleStatusesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConfigRuleStatuses)

responseDescribeOrganizationConformancePackStatuses :: DescribeOrganizationConformancePackStatusesResponse -> TestTree
responseDescribeOrganizationConformancePackStatuses =
  res
    "DescribeOrganizationConformancePackStatusesResponse"
    "fixture/DescribeOrganizationConformancePackStatusesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConformancePackStatuses)

responseGetComplianceDetailsByResource :: GetComplianceDetailsByResourceResponse -> TestTree
responseGetComplianceDetailsByResource =
  res
    "GetComplianceDetailsByResourceResponse"
    "fixture/GetComplianceDetailsByResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComplianceDetailsByResource)

responseDeletePendingAggregationRequest :: DeletePendingAggregationRequestResponse -> TestTree
responseDeletePendingAggregationRequest =
  res
    "DeletePendingAggregationRequestResponse"
    "fixture/DeletePendingAggregationRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePendingAggregationRequest)

responseDeliverConfigSnapshot :: DeliverConfigSnapshotResponse -> TestTree
responseDeliverConfigSnapshot =
  res
    "DeliverConfigSnapshotResponse"
    "fixture/DeliverConfigSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeliverConfigSnapshot)

responseBatchGetAggregateResourceConfig :: BatchGetAggregateResourceConfigResponse -> TestTree
responseBatchGetAggregateResourceConfig =
  res
    "BatchGetAggregateResourceConfigResponse"
    "fixture/BatchGetAggregateResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetAggregateResourceConfig)

responseDescribeConfigRuleEvaluationStatus :: DescribeConfigRuleEvaluationStatusResponse -> TestTree
responseDescribeConfigRuleEvaluationStatus =
  res
    "DescribeConfigRuleEvaluationStatusResponse"
    "fixture/DescribeConfigRuleEvaluationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigRuleEvaluationStatus)

responseGetDiscoveredResourceCounts :: GetDiscoveredResourceCountsResponse -> TestTree
responseGetDiscoveredResourceCounts =
  res
    "GetDiscoveredResourceCountsResponse"
    "fixture/GetDiscoveredResourceCountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDiscoveredResourceCounts)

responseDescribeRemediationExceptions :: DescribeRemediationExceptionsResponse -> TestTree
responseDescribeRemediationExceptions =
  res
    "DescribeRemediationExceptionsResponse"
    "fixture/DescribeRemediationExceptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRemediationExceptions)

responseDeleteOrganizationConformancePack :: DeleteOrganizationConformancePackResponse -> TestTree
responseDeleteOrganizationConformancePack =
  res
    "DeleteOrganizationConformancePackResponse"
    "fixture/DeleteOrganizationConformancePackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOrganizationConformancePack)

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

responseDeleteOrganizationConfigRule :: DeleteOrganizationConfigRuleResponse -> TestTree
responseDeleteOrganizationConfigRule =
  res
    "DeleteOrganizationConfigRuleResponse"
    "fixture/DeleteOrganizationConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOrganizationConfigRule)

responsePutResourceConfig :: PutResourceConfigResponse -> TestTree
responsePutResourceConfig =
  res
    "PutResourceConfigResponse"
    "fixture/PutResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourceConfig)

responseStartConfigRulesEvaluation :: StartConfigRulesEvaluationResponse -> TestTree
responseStartConfigRulesEvaluation =
  res
    "StartConfigRulesEvaluationResponse"
    "fixture/StartConfigRulesEvaluationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartConfigRulesEvaluation)

responseDescribeOrganizationConfigRules :: DescribeOrganizationConfigRulesResponse -> TestTree
responseDescribeOrganizationConfigRules =
  res
    "DescribeOrganizationConfigRulesResponse"
    "fixture/DescribeOrganizationConfigRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConfigRules)

responseSelectAggregateResourceConfig :: SelectAggregateResourceConfigResponse -> TestTree
responseSelectAggregateResourceConfig =
  res
    "SelectAggregateResourceConfigResponse"
    "fixture/SelectAggregateResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SelectAggregateResourceConfig)

responseDescribeComplianceByResource :: DescribeComplianceByResourceResponse -> TestTree
responseDescribeComplianceByResource =
  res
    "DescribeComplianceByResourceResponse"
    "fixture/DescribeComplianceByResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComplianceByResource)

responseDescribeOrganizationConformancePacks :: DescribeOrganizationConformancePacksResponse -> TestTree
responseDescribeOrganizationConformancePacks =
  res
    "DescribeOrganizationConformancePacksResponse"
    "fixture/DescribeOrganizationConformancePacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationConformancePacks)

responseDeleteResourceConfig :: DeleteResourceConfigResponse -> TestTree
responseDeleteResourceConfig =
  res
    "DeleteResourceConfigResponse"
    "fixture/DeleteResourceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceConfig)

responsePutEvaluations :: PutEvaluationsResponse -> TestTree
responsePutEvaluations =
  res
    "PutEvaluationsResponse"
    "fixture/PutEvaluationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEvaluations)

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

responseGetAggregateComplianceDetailsByConfigRule :: GetAggregateComplianceDetailsByConfigRuleResponse -> TestTree
responseGetAggregateComplianceDetailsByConfigRule =
  res
    "GetAggregateComplianceDetailsByConfigRuleResponse"
    "fixture/GetAggregateComplianceDetailsByConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAggregateComplianceDetailsByConfigRule)

responseGetAggregateDiscoveredResourceCounts :: GetAggregateDiscoveredResourceCountsResponse -> TestTree
responseGetAggregateDiscoveredResourceCounts =
  res
    "GetAggregateDiscoveredResourceCountsResponse"
    "fixture/GetAggregateDiscoveredResourceCountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAggregateDiscoveredResourceCounts)

responseGetAggregateConformancePackComplianceSummary :: GetAggregateConformancePackComplianceSummaryResponse -> TestTree
responseGetAggregateConformancePackComplianceSummary =
  res
    "GetAggregateConformancePackComplianceSummaryResponse"
    "fixture/GetAggregateConformancePackComplianceSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAggregateConformancePackComplianceSummary)

responseStartConfigurationRecorder :: StartConfigurationRecorderResponse -> TestTree
responseStartConfigurationRecorder =
  res
    "StartConfigurationRecorderResponse"
    "fixture/StartConfigurationRecorderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartConfigurationRecorder)

responseDescribeConformancePacks :: DescribeConformancePacksResponse -> TestTree
responseDescribeConformancePacks =
  res
    "DescribeConformancePacksResponse"
    "fixture/DescribeConformancePacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConformancePacks)

responsePutExternalEvaluation :: PutExternalEvaluationResponse -> TestTree
responsePutExternalEvaluation =
  res
    "PutExternalEvaluationResponse"
    "fixture/PutExternalEvaluationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutExternalEvaluation)

responseDeleteRemediationExceptions :: DeleteRemediationExceptionsResponse -> TestTree
responseDeleteRemediationExceptions =
  res
    "DeleteRemediationExceptionsResponse"
    "fixture/DeleteRemediationExceptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRemediationExceptions)

responsePutRemediationExceptions :: PutRemediationExceptionsResponse -> TestTree
responsePutRemediationExceptions =
  res
    "PutRemediationExceptionsResponse"
    "fixture/PutRemediationExceptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRemediationExceptions)

responseGetOrganizationConfigRuleDetailedStatus :: GetOrganizationConfigRuleDetailedStatusResponse -> TestTree
responseGetOrganizationConfigRuleDetailedStatus =
  res
    "GetOrganizationConfigRuleDetailedStatusResponse"
    "fixture/GetOrganizationConfigRuleDetailedStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOrganizationConfigRuleDetailedStatus)

responsePutRemediationConfigurations :: PutRemediationConfigurationsResponse -> TestTree
responsePutRemediationConfigurations =
  res
    "PutRemediationConfigurationsResponse"
    "fixture/PutRemediationConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRemediationConfigurations)

responseDeleteConformancePack :: DeleteConformancePackResponse -> TestTree
responseDeleteConformancePack =
  res
    "DeleteConformancePackResponse"
    "fixture/DeleteConformancePackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConformancePack)

responsePutConformancePack :: PutConformancePackResponse -> TestTree
responsePutConformancePack =
  res
    "PutConformancePackResponse"
    "fixture/PutConformancePackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConformancePack)

responseStartRemediationExecution :: StartRemediationExecutionResponse -> TestTree
responseStartRemediationExecution =
  res
    "StartRemediationExecutionResponse"
    "fixture/StartRemediationExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRemediationExecution)

responseDescribeConformancePackStatus :: DescribeConformancePackStatusResponse -> TestTree
responseDescribeConformancePackStatus =
  res
    "DescribeConformancePackStatusResponse"
    "fixture/DescribeConformancePackStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConformancePackStatus)

responseGetComplianceSummaryByConfigRule :: GetComplianceSummaryByConfigRuleResponse -> TestTree
responseGetComplianceSummaryByConfigRule =
  res
    "GetComplianceSummaryByConfigRuleResponse"
    "fixture/GetComplianceSummaryByConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComplianceSummaryByConfigRule)

responsePutStoredQuery :: PutStoredQueryResponse -> TestTree
responsePutStoredQuery =
  res
    "PutStoredQueryResponse"
    "fixture/PutStoredQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutStoredQuery)

responsePutConfigurationAggregator :: PutConfigurationAggregatorResponse -> TestTree
responsePutConfigurationAggregator =
  res
    "PutConfigurationAggregatorResponse"
    "fixture/PutConfigurationAggregatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigurationAggregator)

responseDeleteStoredQuery :: DeleteStoredQueryResponse -> TestTree
responseDeleteStoredQuery =
  res
    "DeleteStoredQueryResponse"
    "fixture/DeleteStoredQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStoredQuery)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDeleteConfigurationAggregator :: DeleteConfigurationAggregatorResponse -> TestTree
responseDeleteConfigurationAggregator =
  res
    "DeleteConfigurationAggregatorResponse"
    "fixture/DeleteConfigurationAggregatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationAggregator)

responseDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatusResponse -> TestTree
responseDescribeConfigurationRecorderStatus =
  res
    "DescribeConfigurationRecorderStatusResponse"
    "fixture/DescribeConfigurationRecorderStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationRecorderStatus)

responsePutConfigurationRecorder :: PutConfigurationRecorderResponse -> TestTree
responsePutConfigurationRecorder =
  res
    "PutConfigurationRecorderResponse"
    "fixture/PutConfigurationRecorderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigurationRecorder)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteConfigurationRecorder :: DeleteConfigurationRecorderResponse -> TestTree
responseDeleteConfigurationRecorder =
  res
    "DeleteConfigurationRecorderResponse"
    "fixture/DeleteConfigurationRecorderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationRecorder)

responseGetConformancePackComplianceSummary :: GetConformancePackComplianceSummaryResponse -> TestTree
responseGetConformancePackComplianceSummary =
  res
    "GetConformancePackComplianceSummaryResponse"
    "fixture/GetConformancePackComplianceSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConformancePackComplianceSummary)

responseGetComplianceSummaryByResourceType :: GetComplianceSummaryByResourceTypeResponse -> TestTree
responseGetComplianceSummaryByResourceType =
  res
    "GetComplianceSummaryByResourceTypeResponse"
    "fixture/GetComplianceSummaryByResourceTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComplianceSummaryByResourceType)

responseDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatusResponse -> TestTree
responseDescribeDeliveryChannelStatus =
  res
    "DescribeDeliveryChannelStatusResponse"
    "fixture/DescribeDeliveryChannelStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeliveryChannelStatus)

responsePutDeliveryChannel :: PutDeliveryChannelResponse -> TestTree
responsePutDeliveryChannel =
  res
    "PutDeliveryChannelResponse"
    "fixture/PutDeliveryChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDeliveryChannel)

responseGetComplianceDetailsByConfigRule :: GetComplianceDetailsByConfigRuleResponse -> TestTree
responseGetComplianceDetailsByConfigRule =
  res
    "GetComplianceDetailsByConfigRuleResponse"
    "fixture/GetComplianceDetailsByConfigRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComplianceDetailsByConfigRule)

responseDeleteAggregationAuthorization :: DeleteAggregationAuthorizationResponse -> TestTree
responseDeleteAggregationAuthorization =
  res
    "DeleteAggregationAuthorizationResponse"
    "fixture/DeleteAggregationAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAggregationAuthorization)

responseDeleteDeliveryChannel :: DeleteDeliveryChannelResponse -> TestTree
responseDeleteDeliveryChannel =
  res
    "DeleteDeliveryChannelResponse"
    "fixture/DeleteDeliveryChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeliveryChannel)

responseDeleteRemediationConfiguration :: DeleteRemediationConfigurationResponse -> TestTree
responseDeleteRemediationConfiguration =
  res
    "DeleteRemediationConfigurationResponse"
    "fixture/DeleteRemediationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRemediationConfiguration)

responsePutAggregationAuthorization :: PutAggregationAuthorizationResponse -> TestTree
responsePutAggregationAuthorization =
  res
    "PutAggregationAuthorizationResponse"
    "fixture/PutAggregationAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAggregationAuthorization)

responseDescribeConfigurationAggregatorSourcesStatus :: DescribeConfigurationAggregatorSourcesStatusResponse -> TestTree
responseDescribeConfigurationAggregatorSourcesStatus =
  res
    "DescribeConfigurationAggregatorSourcesStatusResponse"
    "fixture/DescribeConfigurationAggregatorSourcesStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationAggregatorSourcesStatus)

responseListDiscoveredResources :: ListDiscoveredResourcesResponse -> TestTree
responseListDiscoveredResources =
  res
    "ListDiscoveredResourcesResponse"
    "fixture/ListDiscoveredResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDiscoveredResources)

responseDescribeRemediationConfigurations :: DescribeRemediationConfigurationsResponse -> TestTree
responseDescribeRemediationConfigurations =
  res
    "DescribeRemediationConfigurationsResponse"
    "fixture/DescribeRemediationConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRemediationConfigurations)

responseDescribeDeliveryChannels :: DescribeDeliveryChannelsResponse -> TestTree
responseDescribeDeliveryChannels =
  res
    "DescribeDeliveryChannelsResponse"
    "fixture/DescribeDeliveryChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeliveryChannels)

responseDescribeAggregationAuthorizations :: DescribeAggregationAuthorizationsResponse -> TestTree
responseDescribeAggregationAuthorizations =
  res
    "DescribeAggregationAuthorizationsResponse"
    "fixture/DescribeAggregationAuthorizationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAggregationAuthorizations)
