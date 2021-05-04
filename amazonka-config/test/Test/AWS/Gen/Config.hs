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

import Data.Proxy
import Network.AWS.Config
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
--         [ requestDescribeComplianceByConfigRule $
--             newDescribeComplianceByConfigRule
--
--         , requestGetAggregateComplianceDetailsByConfigRule $
--             newGetAggregateComplianceDetailsByConfigRule
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
--         , requestDescribePendingAggregationRequests $
--             newDescribePendingAggregationRequests
--
--         , requestDescribeRemediationExecutionStatus $
--             newDescribeRemediationExecutionStatus
--
--         , requestGetResourceConfigHistory $
--             newGetResourceConfigHistory
--
--         , requestDescribeConformancePackCompliance $
--             newDescribeConformancePackCompliance
--
--         , requestDescribeOrganizationConformancePacks $
--             newDescribeOrganizationConformancePacks
--
--         , requestListDiscoveredResources $
--             newListDiscoveredResources
--
--         , requestDescribeAggregationAuthorizations $
--             newDescribeAggregationAuthorizations
--
--         , requestDescribeRemediationConfigurations $
--             newDescribeRemediationConfigurations
--
--         , requestDeleteResourceConfig $
--             newDeleteResourceConfig
--
--         , requestDescribeComplianceByResource $
--             newDescribeComplianceByResource
--
--         , requestStartConfigRulesEvaluation $
--             newStartConfigRulesEvaluation
--
--         , requestDescribeRemediationExceptions $
--             newDescribeRemediationExceptions
--
--         , requestDeleteOrganizationConformancePack $
--             newDeleteOrganizationConformancePack
--
--         , requestGetComplianceDetailsByConfigRule $
--             newGetComplianceDetailsByConfigRule
--
--         , requestPutOrganizationConfigRule $
--             newPutOrganizationConfigRule
--
--         , requestGetDiscoveredResourceCounts $
--             newGetDiscoveredResourceCounts
--
--         , requestGetComplianceSummaryByResourceType $
--             newGetComplianceSummaryByResourceType
--
--         , requestPutDeliveryChannel $
--             newPutDeliveryChannel
--
--         , requestDescribeConfigurationAggregatorSourcesStatus $
--             newDescribeConfigurationAggregatorSourcesStatus
--
--         , requestDeleteRemediationConfiguration $
--             newDeleteRemediationConfiguration
--
--         , requestDeleteAggregationAuthorization $
--             newDeleteAggregationAuthorization
--
--         , requestDescribeConfigurationRecorderStatus $
--             newDescribeConfigurationRecorderStatus
--
--         , requestDeleteConfigurationRecorder $
--             newDeleteConfigurationRecorder
--
--         , requestGetConformancePackComplianceSummary $
--             newGetConformancePackComplianceSummary
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeConfigRuleEvaluationStatus $
--             newDescribeConfigRuleEvaluationStatus
--
--         , requestDeleteConfigurationAggregator $
--             newDeleteConfigurationAggregator
--
--         , requestDeleteStoredQuery $
--             newDeleteStoredQuery
--
--         , requestDescribeOrganizationConfigRuleStatuses $
--             newDescribeOrganizationConfigRuleStatuses
--
--         , requestGetComplianceDetailsByResource $
--             newGetComplianceDetailsByResource
--
--         , requestListAggregateDiscoveredResources $
--             newListAggregateDiscoveredResources
--
--         , requestSelectResourceConfig $
--             newSelectResourceConfig
--
--         , requestTagResource $
--             newTagResource
--
--         , requestPutRetentionConfiguration $
--             newPutRetentionConfiguration
--
--         , requestGetConformancePackComplianceDetails $
--             newGetConformancePackComplianceDetails
--
--         , requestDeleteEvaluationResults $
--             newDeleteEvaluationResults
--
--         , requestPutRemediationConfigurations $
--             newPutRemediationConfigurations
--
--         , requestPutConfigRule $
--             newPutConfigRule
--
--         , requestPutConformancePack $
--             newPutConformancePack
--
--         , requestListStoredQueries $
--             newListStoredQueries
--
--         , requestDeleteRemediationExceptions $
--             newDeleteRemediationExceptions
--
--         , requestPutExternalEvaluation $
--             newPutExternalEvaluation
--
--         , requestBatchGetResourceConfig $
--             newBatchGetResourceConfig
--
--         , requestGetAggregateDiscoveredResourceCounts $
--             newGetAggregateDiscoveredResourceCounts
--
--         , requestPutEvaluations $
--             newPutEvaluations
--
--         , requestDescribeConfigurationRecorders $
--             newDescribeConfigurationRecorders
--
--         , requestDescribeOrganizationConfigRules $
--             newDescribeOrganizationConfigRules
--
--         , requestSelectAggregateResourceConfig $
--             newSelectAggregateResourceConfig
--
--         , requestPutResourceConfig $
--             newPutResourceConfig
--
--         , requestDescribeDeliveryChannels $
--             newDescribeDeliveryChannels
--
--         , requestDeleteDeliveryChannel $
--             newDeleteDeliveryChannel
--
--         , requestDescribeDeliveryChannelStatus $
--             newDescribeDeliveryChannelStatus
--
--         , requestDeleteOrganizationConfigRule $
--             newDeleteOrganizationConfigRule
--
--         , requestPutAggregationAuthorization $
--             newPutAggregationAuthorization
--
--         , requestPutOrganizationConformancePack $
--             newPutOrganizationConformancePack
--
--         , requestPutConfigurationRecorder $
--             newPutConfigurationRecorder
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
--         , requestGetComplianceSummaryByConfigRule $
--             newGetComplianceSummaryByConfigRule
--
--         , requestDescribeOrganizationConformancePackStatuses $
--             newDescribeOrganizationConformancePackStatuses
--
--         , requestPutStoredQuery $
--             newPutStoredQuery
--
--         , requestPutConfigurationAggregator $
--             newPutConfigurationAggregator
--
--         , requestGetOrganizationConfigRuleDetailedStatus $
--             newGetOrganizationConfigRuleDetailedStatus
--
--         , requestStartRemediationExecution $
--             newStartRemediationExecution
--
--         , requestDescribeConformancePackStatus $
--             newDescribeConformancePackStatus
--
--         , requestDeleteConfigRule $
--             newDeleteConfigRule
--
--         , requestDeleteRetentionConfiguration $
--             newDeleteRetentionConfiguration
--
--         , requestDeleteConformancePack $
--             newDeleteConformancePack
--
--         , requestGetOrganizationConformancePackDetailedStatus $
--             newGetOrganizationConformancePackDetailedStatus
--
--         , requestDescribeAggregateComplianceByConfigRules $
--             newDescribeAggregateComplianceByConfigRules
--
--         , requestStartConfigurationRecorder $
--             newStartConfigurationRecorder
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetAggregateConfigRuleComplianceSummary $
--             newGetAggregateConfigRuleComplianceSummary
--
--         , requestPutRemediationExceptions $
--             newPutRemediationExceptions
--
--         , requestDescribeConfigRules $
--             newDescribeConfigRules
--
--         , requestStopConfigurationRecorder $
--             newStopConfigurationRecorder
--
--         , requestDescribeConformancePacks $
--             newDescribeConformancePacks
--
--         , requestDescribeRetentionConfigurations $
--             newDescribeRetentionConfigurations
--
--           ]

--     , testGroup "response"
--         [ responseDescribeComplianceByConfigRule $
--             newDescribeComplianceByConfigRuleResponse
--
--         , responseGetAggregateComplianceDetailsByConfigRule $
--             newGetAggregateComplianceDetailsByConfigRuleResponse
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
--         , responseDescribePendingAggregationRequests $
--             newDescribePendingAggregationRequestsResponse
--
--         , responseDescribeRemediationExecutionStatus $
--             newDescribeRemediationExecutionStatusResponse
--
--         , responseGetResourceConfigHistory $
--             newGetResourceConfigHistoryResponse
--
--         , responseDescribeConformancePackCompliance $
--             newDescribeConformancePackComplianceResponse
--
--         , responseDescribeOrganizationConformancePacks $
--             newDescribeOrganizationConformancePacksResponse
--
--         , responseListDiscoveredResources $
--             newListDiscoveredResourcesResponse
--
--         , responseDescribeAggregationAuthorizations $
--             newDescribeAggregationAuthorizationsResponse
--
--         , responseDescribeRemediationConfigurations $
--             newDescribeRemediationConfigurationsResponse
--
--         , responseDeleteResourceConfig $
--             newDeleteResourceConfigResponse
--
--         , responseDescribeComplianceByResource $
--             newDescribeComplianceByResourceResponse
--
--         , responseStartConfigRulesEvaluation $
--             newStartConfigRulesEvaluationResponse
--
--         , responseDescribeRemediationExceptions $
--             newDescribeRemediationExceptionsResponse
--
--         , responseDeleteOrganizationConformancePack $
--             newDeleteOrganizationConformancePackResponse
--
--         , responseGetComplianceDetailsByConfigRule $
--             newGetComplianceDetailsByConfigRuleResponse
--
--         , responsePutOrganizationConfigRule $
--             newPutOrganizationConfigRuleResponse
--
--         , responseGetDiscoveredResourceCounts $
--             newGetDiscoveredResourceCountsResponse
--
--         , responseGetComplianceSummaryByResourceType $
--             newGetComplianceSummaryByResourceTypeResponse
--
--         , responsePutDeliveryChannel $
--             newPutDeliveryChannelResponse
--
--         , responseDescribeConfigurationAggregatorSourcesStatus $
--             newDescribeConfigurationAggregatorSourcesStatusResponse
--
--         , responseDeleteRemediationConfiguration $
--             newDeleteRemediationConfigurationResponse
--
--         , responseDeleteAggregationAuthorization $
--             newDeleteAggregationAuthorizationResponse
--
--         , responseDescribeConfigurationRecorderStatus $
--             newDescribeConfigurationRecorderStatusResponse
--
--         , responseDeleteConfigurationRecorder $
--             newDeleteConfigurationRecorderResponse
--
--         , responseGetConformancePackComplianceSummary $
--             newGetConformancePackComplianceSummaryResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeConfigRuleEvaluationStatus $
--             newDescribeConfigRuleEvaluationStatusResponse
--
--         , responseDeleteConfigurationAggregator $
--             newDeleteConfigurationAggregatorResponse
--
--         , responseDeleteStoredQuery $
--             newDeleteStoredQueryResponse
--
--         , responseDescribeOrganizationConfigRuleStatuses $
--             newDescribeOrganizationConfigRuleStatusesResponse
--
--         , responseGetComplianceDetailsByResource $
--             newGetComplianceDetailsByResourceResponse
--
--         , responseListAggregateDiscoveredResources $
--             newListAggregateDiscoveredResourcesResponse
--
--         , responseSelectResourceConfig $
--             newSelectResourceConfigResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responsePutRetentionConfiguration $
--             newPutRetentionConfigurationResponse
--
--         , responseGetConformancePackComplianceDetails $
--             newGetConformancePackComplianceDetailsResponse
--
--         , responseDeleteEvaluationResults $
--             newDeleteEvaluationResultsResponse
--
--         , responsePutRemediationConfigurations $
--             newPutRemediationConfigurationsResponse
--
--         , responsePutConfigRule $
--             newPutConfigRuleResponse
--
--         , responsePutConformancePack $
--             newPutConformancePackResponse
--
--         , responseListStoredQueries $
--             newListStoredQueriesResponse
--
--         , responseDeleteRemediationExceptions $
--             newDeleteRemediationExceptionsResponse
--
--         , responsePutExternalEvaluation $
--             newPutExternalEvaluationResponse
--
--         , responseBatchGetResourceConfig $
--             newBatchGetResourceConfigResponse
--
--         , responseGetAggregateDiscoveredResourceCounts $
--             newGetAggregateDiscoveredResourceCountsResponse
--
--         , responsePutEvaluations $
--             newPutEvaluationsResponse
--
--         , responseDescribeConfigurationRecorders $
--             newDescribeConfigurationRecordersResponse
--
--         , responseDescribeOrganizationConfigRules $
--             newDescribeOrganizationConfigRulesResponse
--
--         , responseSelectAggregateResourceConfig $
--             newSelectAggregateResourceConfigResponse
--
--         , responsePutResourceConfig $
--             newPutResourceConfigResponse
--
--         , responseDescribeDeliveryChannels $
--             newDescribeDeliveryChannelsResponse
--
--         , responseDeleteDeliveryChannel $
--             newDeleteDeliveryChannelResponse
--
--         , responseDescribeDeliveryChannelStatus $
--             newDescribeDeliveryChannelStatusResponse
--
--         , responseDeleteOrganizationConfigRule $
--             newDeleteOrganizationConfigRuleResponse
--
--         , responsePutAggregationAuthorization $
--             newPutAggregationAuthorizationResponse
--
--         , responsePutOrganizationConformancePack $
--             newPutOrganizationConformancePackResponse
--
--         , responsePutConfigurationRecorder $
--             newPutConfigurationRecorderResponse
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
--         , responseGetComplianceSummaryByConfigRule $
--             newGetComplianceSummaryByConfigRuleResponse
--
--         , responseDescribeOrganizationConformancePackStatuses $
--             newDescribeOrganizationConformancePackStatusesResponse
--
--         , responsePutStoredQuery $
--             newPutStoredQueryResponse
--
--         , responsePutConfigurationAggregator $
--             newPutConfigurationAggregatorResponse
--
--         , responseGetOrganizationConfigRuleDetailedStatus $
--             newGetOrganizationConfigRuleDetailedStatusResponse
--
--         , responseStartRemediationExecution $
--             newStartRemediationExecutionResponse
--
--         , responseDescribeConformancePackStatus $
--             newDescribeConformancePackStatusResponse
--
--         , responseDeleteConfigRule $
--             newDeleteConfigRuleResponse
--
--         , responseDeleteRetentionConfiguration $
--             newDeleteRetentionConfigurationResponse
--
--         , responseDeleteConformancePack $
--             newDeleteConformancePackResponse
--
--         , responseGetOrganizationConformancePackDetailedStatus $
--             newGetOrganizationConformancePackDetailedStatusResponse
--
--         , responseDescribeAggregateComplianceByConfigRules $
--             newDescribeAggregateComplianceByConfigRulesResponse
--
--         , responseStartConfigurationRecorder $
--             newStartConfigurationRecorderResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetAggregateConfigRuleComplianceSummary $
--             newGetAggregateConfigRuleComplianceSummaryResponse
--
--         , responsePutRemediationExceptions $
--             newPutRemediationExceptionsResponse
--
--         , responseDescribeConfigRules $
--             newDescribeConfigRulesResponse
--
--         , responseStopConfigurationRecorder $
--             newStopConfigurationRecorderResponse
--
--         , responseDescribeConformancePacks $
--             newDescribeConformancePacksResponse
--
--         , responseDescribeRetentionConfigurations $
--             newDescribeRetentionConfigurationsResponse
--
--           ]
--     ]

-- Requests

requestDescribeComplianceByConfigRule :: DescribeComplianceByConfigRule -> TestTree
requestDescribeComplianceByConfigRule =
  req
    "DescribeComplianceByConfigRule"
    "fixture/DescribeComplianceByConfigRule.yaml"

requestGetAggregateComplianceDetailsByConfigRule :: GetAggregateComplianceDetailsByConfigRule -> TestTree
requestGetAggregateComplianceDetailsByConfigRule =
  req
    "GetAggregateComplianceDetailsByConfigRule"
    "fixture/GetAggregateComplianceDetailsByConfigRule.yaml"

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

requestDescribeConformancePackCompliance :: DescribeConformancePackCompliance -> TestTree
requestDescribeConformancePackCompliance =
  req
    "DescribeConformancePackCompliance"
    "fixture/DescribeConformancePackCompliance.yaml"

requestDescribeOrganizationConformancePacks :: DescribeOrganizationConformancePacks -> TestTree
requestDescribeOrganizationConformancePacks =
  req
    "DescribeOrganizationConformancePacks"
    "fixture/DescribeOrganizationConformancePacks.yaml"

requestListDiscoveredResources :: ListDiscoveredResources -> TestTree
requestListDiscoveredResources =
  req
    "ListDiscoveredResources"
    "fixture/ListDiscoveredResources.yaml"

requestDescribeAggregationAuthorizations :: DescribeAggregationAuthorizations -> TestTree
requestDescribeAggregationAuthorizations =
  req
    "DescribeAggregationAuthorizations"
    "fixture/DescribeAggregationAuthorizations.yaml"

requestDescribeRemediationConfigurations :: DescribeRemediationConfigurations -> TestTree
requestDescribeRemediationConfigurations =
  req
    "DescribeRemediationConfigurations"
    "fixture/DescribeRemediationConfigurations.yaml"

requestDeleteResourceConfig :: DeleteResourceConfig -> TestTree
requestDeleteResourceConfig =
  req
    "DeleteResourceConfig"
    "fixture/DeleteResourceConfig.yaml"

requestDescribeComplianceByResource :: DescribeComplianceByResource -> TestTree
requestDescribeComplianceByResource =
  req
    "DescribeComplianceByResource"
    "fixture/DescribeComplianceByResource.yaml"

requestStartConfigRulesEvaluation :: StartConfigRulesEvaluation -> TestTree
requestStartConfigRulesEvaluation =
  req
    "StartConfigRulesEvaluation"
    "fixture/StartConfigRulesEvaluation.yaml"

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

requestGetComplianceDetailsByConfigRule :: GetComplianceDetailsByConfigRule -> TestTree
requestGetComplianceDetailsByConfigRule =
  req
    "GetComplianceDetailsByConfigRule"
    "fixture/GetComplianceDetailsByConfigRule.yaml"

requestPutOrganizationConfigRule :: PutOrganizationConfigRule -> TestTree
requestPutOrganizationConfigRule =
  req
    "PutOrganizationConfigRule"
    "fixture/PutOrganizationConfigRule.yaml"

requestGetDiscoveredResourceCounts :: GetDiscoveredResourceCounts -> TestTree
requestGetDiscoveredResourceCounts =
  req
    "GetDiscoveredResourceCounts"
    "fixture/GetDiscoveredResourceCounts.yaml"

requestGetComplianceSummaryByResourceType :: GetComplianceSummaryByResourceType -> TestTree
requestGetComplianceSummaryByResourceType =
  req
    "GetComplianceSummaryByResourceType"
    "fixture/GetComplianceSummaryByResourceType.yaml"

requestPutDeliveryChannel :: PutDeliveryChannel -> TestTree
requestPutDeliveryChannel =
  req
    "PutDeliveryChannel"
    "fixture/PutDeliveryChannel.yaml"

requestDescribeConfigurationAggregatorSourcesStatus :: DescribeConfigurationAggregatorSourcesStatus -> TestTree
requestDescribeConfigurationAggregatorSourcesStatus =
  req
    "DescribeConfigurationAggregatorSourcesStatus"
    "fixture/DescribeConfigurationAggregatorSourcesStatus.yaml"

requestDeleteRemediationConfiguration :: DeleteRemediationConfiguration -> TestTree
requestDeleteRemediationConfiguration =
  req
    "DeleteRemediationConfiguration"
    "fixture/DeleteRemediationConfiguration.yaml"

requestDeleteAggregationAuthorization :: DeleteAggregationAuthorization -> TestTree
requestDeleteAggregationAuthorization =
  req
    "DeleteAggregationAuthorization"
    "fixture/DeleteAggregationAuthorization.yaml"

requestDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatus -> TestTree
requestDescribeConfigurationRecorderStatus =
  req
    "DescribeConfigurationRecorderStatus"
    "fixture/DescribeConfigurationRecorderStatus.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeConfigRuleEvaluationStatus :: DescribeConfigRuleEvaluationStatus -> TestTree
requestDescribeConfigRuleEvaluationStatus =
  req
    "DescribeConfigRuleEvaluationStatus"
    "fixture/DescribeConfigRuleEvaluationStatus.yaml"

requestDeleteConfigurationAggregator :: DeleteConfigurationAggregator -> TestTree
requestDeleteConfigurationAggregator =
  req
    "DeleteConfigurationAggregator"
    "fixture/DeleteConfigurationAggregator.yaml"

requestDeleteStoredQuery :: DeleteStoredQuery -> TestTree
requestDeleteStoredQuery =
  req
    "DeleteStoredQuery"
    "fixture/DeleteStoredQuery.yaml"

requestDescribeOrganizationConfigRuleStatuses :: DescribeOrganizationConfigRuleStatuses -> TestTree
requestDescribeOrganizationConfigRuleStatuses =
  req
    "DescribeOrganizationConfigRuleStatuses"
    "fixture/DescribeOrganizationConfigRuleStatuses.yaml"

requestGetComplianceDetailsByResource :: GetComplianceDetailsByResource -> TestTree
requestGetComplianceDetailsByResource =
  req
    "GetComplianceDetailsByResource"
    "fixture/GetComplianceDetailsByResource.yaml"

requestListAggregateDiscoveredResources :: ListAggregateDiscoveredResources -> TestTree
requestListAggregateDiscoveredResources =
  req
    "ListAggregateDiscoveredResources"
    "fixture/ListAggregateDiscoveredResources.yaml"

requestSelectResourceConfig :: SelectResourceConfig -> TestTree
requestSelectResourceConfig =
  req
    "SelectResourceConfig"
    "fixture/SelectResourceConfig.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestPutRetentionConfiguration :: PutRetentionConfiguration -> TestTree
requestPutRetentionConfiguration =
  req
    "PutRetentionConfiguration"
    "fixture/PutRetentionConfiguration.yaml"

requestGetConformancePackComplianceDetails :: GetConformancePackComplianceDetails -> TestTree
requestGetConformancePackComplianceDetails =
  req
    "GetConformancePackComplianceDetails"
    "fixture/GetConformancePackComplianceDetails.yaml"

requestDeleteEvaluationResults :: DeleteEvaluationResults -> TestTree
requestDeleteEvaluationResults =
  req
    "DeleteEvaluationResults"
    "fixture/DeleteEvaluationResults.yaml"

requestPutRemediationConfigurations :: PutRemediationConfigurations -> TestTree
requestPutRemediationConfigurations =
  req
    "PutRemediationConfigurations"
    "fixture/PutRemediationConfigurations.yaml"

requestPutConfigRule :: PutConfigRule -> TestTree
requestPutConfigRule =
  req
    "PutConfigRule"
    "fixture/PutConfigRule.yaml"

requestPutConformancePack :: PutConformancePack -> TestTree
requestPutConformancePack =
  req
    "PutConformancePack"
    "fixture/PutConformancePack.yaml"

requestListStoredQueries :: ListStoredQueries -> TestTree
requestListStoredQueries =
  req
    "ListStoredQueries"
    "fixture/ListStoredQueries.yaml"

requestDeleteRemediationExceptions :: DeleteRemediationExceptions -> TestTree
requestDeleteRemediationExceptions =
  req
    "DeleteRemediationExceptions"
    "fixture/DeleteRemediationExceptions.yaml"

requestPutExternalEvaluation :: PutExternalEvaluation -> TestTree
requestPutExternalEvaluation =
  req
    "PutExternalEvaluation"
    "fixture/PutExternalEvaluation.yaml"

requestBatchGetResourceConfig :: BatchGetResourceConfig -> TestTree
requestBatchGetResourceConfig =
  req
    "BatchGetResourceConfig"
    "fixture/BatchGetResourceConfig.yaml"

requestGetAggregateDiscoveredResourceCounts :: GetAggregateDiscoveredResourceCounts -> TestTree
requestGetAggregateDiscoveredResourceCounts =
  req
    "GetAggregateDiscoveredResourceCounts"
    "fixture/GetAggregateDiscoveredResourceCounts.yaml"

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

requestPutResourceConfig :: PutResourceConfig -> TestTree
requestPutResourceConfig =
  req
    "PutResourceConfig"
    "fixture/PutResourceConfig.yaml"

requestDescribeDeliveryChannels :: DescribeDeliveryChannels -> TestTree
requestDescribeDeliveryChannels =
  req
    "DescribeDeliveryChannels"
    "fixture/DescribeDeliveryChannels.yaml"

requestDeleteDeliveryChannel :: DeleteDeliveryChannel -> TestTree
requestDeleteDeliveryChannel =
  req
    "DeleteDeliveryChannel"
    "fixture/DeleteDeliveryChannel.yaml"

requestDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatus -> TestTree
requestDescribeDeliveryChannelStatus =
  req
    "DescribeDeliveryChannelStatus"
    "fixture/DescribeDeliveryChannelStatus.yaml"

requestDeleteOrganizationConfigRule :: DeleteOrganizationConfigRule -> TestTree
requestDeleteOrganizationConfigRule =
  req
    "DeleteOrganizationConfigRule"
    "fixture/DeleteOrganizationConfigRule.yaml"

requestPutAggregationAuthorization :: PutAggregationAuthorization -> TestTree
requestPutAggregationAuthorization =
  req
    "PutAggregationAuthorization"
    "fixture/PutAggregationAuthorization.yaml"

requestPutOrganizationConformancePack :: PutOrganizationConformancePack -> TestTree
requestPutOrganizationConformancePack =
  req
    "PutOrganizationConformancePack"
    "fixture/PutOrganizationConformancePack.yaml"

requestPutConfigurationRecorder :: PutConfigurationRecorder -> TestTree
requestPutConfigurationRecorder =
  req
    "PutConfigurationRecorder"
    "fixture/PutConfigurationRecorder.yaml"

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

requestGetComplianceSummaryByConfigRule :: GetComplianceSummaryByConfigRule -> TestTree
requestGetComplianceSummaryByConfigRule =
  req
    "GetComplianceSummaryByConfigRule"
    "fixture/GetComplianceSummaryByConfigRule.yaml"

requestDescribeOrganizationConformancePackStatuses :: DescribeOrganizationConformancePackStatuses -> TestTree
requestDescribeOrganizationConformancePackStatuses =
  req
    "DescribeOrganizationConformancePackStatuses"
    "fixture/DescribeOrganizationConformancePackStatuses.yaml"

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

requestGetOrganizationConfigRuleDetailedStatus :: GetOrganizationConfigRuleDetailedStatus -> TestTree
requestGetOrganizationConfigRuleDetailedStatus =
  req
    "GetOrganizationConfigRuleDetailedStatus"
    "fixture/GetOrganizationConfigRuleDetailedStatus.yaml"

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

requestDeleteConformancePack :: DeleteConformancePack -> TestTree
requestDeleteConformancePack =
  req
    "DeleteConformancePack"
    "fixture/DeleteConformancePack.yaml"

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

requestStartConfigurationRecorder :: StartConfigurationRecorder -> TestTree
requestStartConfigurationRecorder =
  req
    "StartConfigurationRecorder"
    "fixture/StartConfigurationRecorder.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetAggregateConfigRuleComplianceSummary :: GetAggregateConfigRuleComplianceSummary -> TestTree
requestGetAggregateConfigRuleComplianceSummary =
  req
    "GetAggregateConfigRuleComplianceSummary"
    "fixture/GetAggregateConfigRuleComplianceSummary.yaml"

requestPutRemediationExceptions :: PutRemediationExceptions -> TestTree
requestPutRemediationExceptions =
  req
    "PutRemediationExceptions"
    "fixture/PutRemediationExceptions.yaml"

requestDescribeConfigRules :: DescribeConfigRules -> TestTree
requestDescribeConfigRules =
  req
    "DescribeConfigRules"
    "fixture/DescribeConfigRules.yaml"

requestStopConfigurationRecorder :: StopConfigurationRecorder -> TestTree
requestStopConfigurationRecorder =
  req
    "StopConfigurationRecorder"
    "fixture/StopConfigurationRecorder.yaml"

requestDescribeConformancePacks :: DescribeConformancePacks -> TestTree
requestDescribeConformancePacks =
  req
    "DescribeConformancePacks"
    "fixture/DescribeConformancePacks.yaml"

requestDescribeRetentionConfigurations :: DescribeRetentionConfigurations -> TestTree
requestDescribeRetentionConfigurations =
  req
    "DescribeRetentionConfigurations"
    "fixture/DescribeRetentionConfigurations.yaml"

-- Responses

responseDescribeComplianceByConfigRule :: DescribeComplianceByConfigRuleResponse -> TestTree
responseDescribeComplianceByConfigRule =
  res
    "DescribeComplianceByConfigRuleResponse"
    "fixture/DescribeComplianceByConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeComplianceByConfigRule)

responseGetAggregateComplianceDetailsByConfigRule :: GetAggregateComplianceDetailsByConfigRuleResponse -> TestTree
responseGetAggregateComplianceDetailsByConfigRule =
  res
    "GetAggregateComplianceDetailsByConfigRuleResponse"
    "fixture/GetAggregateComplianceDetailsByConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetAggregateComplianceDetailsByConfigRule)

responseGetStoredQuery :: GetStoredQueryResponse -> TestTree
responseGetStoredQuery =
  res
    "GetStoredQueryResponse"
    "fixture/GetStoredQueryResponse.proto"
    defaultService
    (Proxy :: Proxy GetStoredQuery)

responseGetAggregateResourceConfig :: GetAggregateResourceConfigResponse -> TestTree
responseGetAggregateResourceConfig =
  res
    "GetAggregateResourceConfigResponse"
    "fixture/GetAggregateResourceConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetAggregateResourceConfig)

responseDescribeConfigurationAggregators :: DescribeConfigurationAggregatorsResponse -> TestTree
responseDescribeConfigurationAggregators =
  res
    "DescribeConfigurationAggregatorsResponse"
    "fixture/DescribeConfigurationAggregatorsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationAggregators)

responseDescribePendingAggregationRequests :: DescribePendingAggregationRequestsResponse -> TestTree
responseDescribePendingAggregationRequests =
  res
    "DescribePendingAggregationRequestsResponse"
    "fixture/DescribePendingAggregationRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePendingAggregationRequests)

responseDescribeRemediationExecutionStatus :: DescribeRemediationExecutionStatusResponse -> TestTree
responseDescribeRemediationExecutionStatus =
  res
    "DescribeRemediationExecutionStatusResponse"
    "fixture/DescribeRemediationExecutionStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRemediationExecutionStatus)

responseGetResourceConfigHistory :: GetResourceConfigHistoryResponse -> TestTree
responseGetResourceConfigHistory =
  res
    "GetResourceConfigHistoryResponse"
    "fixture/GetResourceConfigHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourceConfigHistory)

responseDescribeConformancePackCompliance :: DescribeConformancePackComplianceResponse -> TestTree
responseDescribeConformancePackCompliance =
  res
    "DescribeConformancePackComplianceResponse"
    "fixture/DescribeConformancePackComplianceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConformancePackCompliance)

responseDescribeOrganizationConformancePacks :: DescribeOrganizationConformancePacksResponse -> TestTree
responseDescribeOrganizationConformancePacks =
  res
    "DescribeOrganizationConformancePacksResponse"
    "fixture/DescribeOrganizationConformancePacksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrganizationConformancePacks)

responseListDiscoveredResources :: ListDiscoveredResourcesResponse -> TestTree
responseListDiscoveredResources =
  res
    "ListDiscoveredResourcesResponse"
    "fixture/ListDiscoveredResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDiscoveredResources)

responseDescribeAggregationAuthorizations :: DescribeAggregationAuthorizationsResponse -> TestTree
responseDescribeAggregationAuthorizations =
  res
    "DescribeAggregationAuthorizationsResponse"
    "fixture/DescribeAggregationAuthorizationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAggregationAuthorizations)

responseDescribeRemediationConfigurations :: DescribeRemediationConfigurationsResponse -> TestTree
responseDescribeRemediationConfigurations =
  res
    "DescribeRemediationConfigurationsResponse"
    "fixture/DescribeRemediationConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRemediationConfigurations)

responseDeleteResourceConfig :: DeleteResourceConfigResponse -> TestTree
responseDeleteResourceConfig =
  res
    "DeleteResourceConfigResponse"
    "fixture/DeleteResourceConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourceConfig)

responseDescribeComplianceByResource :: DescribeComplianceByResourceResponse -> TestTree
responseDescribeComplianceByResource =
  res
    "DescribeComplianceByResourceResponse"
    "fixture/DescribeComplianceByResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeComplianceByResource)

responseStartConfigRulesEvaluation :: StartConfigRulesEvaluationResponse -> TestTree
responseStartConfigRulesEvaluation =
  res
    "StartConfigRulesEvaluationResponse"
    "fixture/StartConfigRulesEvaluationResponse.proto"
    defaultService
    (Proxy :: Proxy StartConfigRulesEvaluation)

responseDescribeRemediationExceptions :: DescribeRemediationExceptionsResponse -> TestTree
responseDescribeRemediationExceptions =
  res
    "DescribeRemediationExceptionsResponse"
    "fixture/DescribeRemediationExceptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRemediationExceptions)

responseDeleteOrganizationConformancePack :: DeleteOrganizationConformancePackResponse -> TestTree
responseDeleteOrganizationConformancePack =
  res
    "DeleteOrganizationConformancePackResponse"
    "fixture/DeleteOrganizationConformancePackResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOrganizationConformancePack)

responseGetComplianceDetailsByConfigRule :: GetComplianceDetailsByConfigRuleResponse -> TestTree
responseGetComplianceDetailsByConfigRule =
  res
    "GetComplianceDetailsByConfigRuleResponse"
    "fixture/GetComplianceDetailsByConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetComplianceDetailsByConfigRule)

responsePutOrganizationConfigRule :: PutOrganizationConfigRuleResponse -> TestTree
responsePutOrganizationConfigRule =
  res
    "PutOrganizationConfigRuleResponse"
    "fixture/PutOrganizationConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy PutOrganizationConfigRule)

responseGetDiscoveredResourceCounts :: GetDiscoveredResourceCountsResponse -> TestTree
responseGetDiscoveredResourceCounts =
  res
    "GetDiscoveredResourceCountsResponse"
    "fixture/GetDiscoveredResourceCountsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDiscoveredResourceCounts)

responseGetComplianceSummaryByResourceType :: GetComplianceSummaryByResourceTypeResponse -> TestTree
responseGetComplianceSummaryByResourceType =
  res
    "GetComplianceSummaryByResourceTypeResponse"
    "fixture/GetComplianceSummaryByResourceTypeResponse.proto"
    defaultService
    (Proxy :: Proxy GetComplianceSummaryByResourceType)

responsePutDeliveryChannel :: PutDeliveryChannelResponse -> TestTree
responsePutDeliveryChannel =
  res
    "PutDeliveryChannelResponse"
    "fixture/PutDeliveryChannelResponse.proto"
    defaultService
    (Proxy :: Proxy PutDeliveryChannel)

responseDescribeConfigurationAggregatorSourcesStatus :: DescribeConfigurationAggregatorSourcesStatusResponse -> TestTree
responseDescribeConfigurationAggregatorSourcesStatus =
  res
    "DescribeConfigurationAggregatorSourcesStatusResponse"
    "fixture/DescribeConfigurationAggregatorSourcesStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationAggregatorSourcesStatus)

responseDeleteRemediationConfiguration :: DeleteRemediationConfigurationResponse -> TestTree
responseDeleteRemediationConfiguration =
  res
    "DeleteRemediationConfigurationResponse"
    "fixture/DeleteRemediationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRemediationConfiguration)

responseDeleteAggregationAuthorization :: DeleteAggregationAuthorizationResponse -> TestTree
responseDeleteAggregationAuthorization =
  res
    "DeleteAggregationAuthorizationResponse"
    "fixture/DeleteAggregationAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAggregationAuthorization)

responseDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatusResponse -> TestTree
responseDescribeConfigurationRecorderStatus =
  res
    "DescribeConfigurationRecorderStatusResponse"
    "fixture/DescribeConfigurationRecorderStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationRecorderStatus)

responseDeleteConfigurationRecorder :: DeleteConfigurationRecorderResponse -> TestTree
responseDeleteConfigurationRecorder =
  res
    "DeleteConfigurationRecorderResponse"
    "fixture/DeleteConfigurationRecorderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationRecorder)

responseGetConformancePackComplianceSummary :: GetConformancePackComplianceSummaryResponse -> TestTree
responseGetConformancePackComplianceSummary =
  res
    "GetConformancePackComplianceSummaryResponse"
    "fixture/GetConformancePackComplianceSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy GetConformancePackComplianceSummary)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDescribeConfigRuleEvaluationStatus :: DescribeConfigRuleEvaluationStatusResponse -> TestTree
responseDescribeConfigRuleEvaluationStatus =
  res
    "DescribeConfigRuleEvaluationStatusResponse"
    "fixture/DescribeConfigRuleEvaluationStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigRuleEvaluationStatus)

responseDeleteConfigurationAggregator :: DeleteConfigurationAggregatorResponse -> TestTree
responseDeleteConfigurationAggregator =
  res
    "DeleteConfigurationAggregatorResponse"
    "fixture/DeleteConfigurationAggregatorResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationAggregator)

responseDeleteStoredQuery :: DeleteStoredQueryResponse -> TestTree
responseDeleteStoredQuery =
  res
    "DeleteStoredQueryResponse"
    "fixture/DeleteStoredQueryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStoredQuery)

responseDescribeOrganizationConfigRuleStatuses :: DescribeOrganizationConfigRuleStatusesResponse -> TestTree
responseDescribeOrganizationConfigRuleStatuses =
  res
    "DescribeOrganizationConfigRuleStatusesResponse"
    "fixture/DescribeOrganizationConfigRuleStatusesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrganizationConfigRuleStatuses)

responseGetComplianceDetailsByResource :: GetComplianceDetailsByResourceResponse -> TestTree
responseGetComplianceDetailsByResource =
  res
    "GetComplianceDetailsByResourceResponse"
    "fixture/GetComplianceDetailsByResourceResponse.proto"
    defaultService
    (Proxy :: Proxy GetComplianceDetailsByResource)

responseListAggregateDiscoveredResources :: ListAggregateDiscoveredResourcesResponse -> TestTree
responseListAggregateDiscoveredResources =
  res
    "ListAggregateDiscoveredResourcesResponse"
    "fixture/ListAggregateDiscoveredResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAggregateDiscoveredResources)

responseSelectResourceConfig :: SelectResourceConfigResponse -> TestTree
responseSelectResourceConfig =
  res
    "SelectResourceConfigResponse"
    "fixture/SelectResourceConfigResponse.proto"
    defaultService
    (Proxy :: Proxy SelectResourceConfig)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responsePutRetentionConfiguration :: PutRetentionConfigurationResponse -> TestTree
responsePutRetentionConfiguration =
  res
    "PutRetentionConfigurationResponse"
    "fixture/PutRetentionConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutRetentionConfiguration)

responseGetConformancePackComplianceDetails :: GetConformancePackComplianceDetailsResponse -> TestTree
responseGetConformancePackComplianceDetails =
  res
    "GetConformancePackComplianceDetailsResponse"
    "fixture/GetConformancePackComplianceDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetConformancePackComplianceDetails)

responseDeleteEvaluationResults :: DeleteEvaluationResultsResponse -> TestTree
responseDeleteEvaluationResults =
  res
    "DeleteEvaluationResultsResponse"
    "fixture/DeleteEvaluationResultsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEvaluationResults)

responsePutRemediationConfigurations :: PutRemediationConfigurationsResponse -> TestTree
responsePutRemediationConfigurations =
  res
    "PutRemediationConfigurationsResponse"
    "fixture/PutRemediationConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy PutRemediationConfigurations)

responsePutConfigRule :: PutConfigRuleResponse -> TestTree
responsePutConfigRule =
  res
    "PutConfigRuleResponse"
    "fixture/PutConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigRule)

responsePutConformancePack :: PutConformancePackResponse -> TestTree
responsePutConformancePack =
  res
    "PutConformancePackResponse"
    "fixture/PutConformancePackResponse.proto"
    defaultService
    (Proxy :: Proxy PutConformancePack)

responseListStoredQueries :: ListStoredQueriesResponse -> TestTree
responseListStoredQueries =
  res
    "ListStoredQueriesResponse"
    "fixture/ListStoredQueriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListStoredQueries)

responseDeleteRemediationExceptions :: DeleteRemediationExceptionsResponse -> TestTree
responseDeleteRemediationExceptions =
  res
    "DeleteRemediationExceptionsResponse"
    "fixture/DeleteRemediationExceptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRemediationExceptions)

responsePutExternalEvaluation :: PutExternalEvaluationResponse -> TestTree
responsePutExternalEvaluation =
  res
    "PutExternalEvaluationResponse"
    "fixture/PutExternalEvaluationResponse.proto"
    defaultService
    (Proxy :: Proxy PutExternalEvaluation)

responseBatchGetResourceConfig :: BatchGetResourceConfigResponse -> TestTree
responseBatchGetResourceConfig =
  res
    "BatchGetResourceConfigResponse"
    "fixture/BatchGetResourceConfigResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetResourceConfig)

responseGetAggregateDiscoveredResourceCounts :: GetAggregateDiscoveredResourceCountsResponse -> TestTree
responseGetAggregateDiscoveredResourceCounts =
  res
    "GetAggregateDiscoveredResourceCountsResponse"
    "fixture/GetAggregateDiscoveredResourceCountsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAggregateDiscoveredResourceCounts)

responsePutEvaluations :: PutEvaluationsResponse -> TestTree
responsePutEvaluations =
  res
    "PutEvaluationsResponse"
    "fixture/PutEvaluationsResponse.proto"
    defaultService
    (Proxy :: Proxy PutEvaluations)

responseDescribeConfigurationRecorders :: DescribeConfigurationRecordersResponse -> TestTree
responseDescribeConfigurationRecorders =
  res
    "DescribeConfigurationRecordersResponse"
    "fixture/DescribeConfigurationRecordersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationRecorders)

responseDescribeOrganizationConfigRules :: DescribeOrganizationConfigRulesResponse -> TestTree
responseDescribeOrganizationConfigRules =
  res
    "DescribeOrganizationConfigRulesResponse"
    "fixture/DescribeOrganizationConfigRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrganizationConfigRules)

responseSelectAggregateResourceConfig :: SelectAggregateResourceConfigResponse -> TestTree
responseSelectAggregateResourceConfig =
  res
    "SelectAggregateResourceConfigResponse"
    "fixture/SelectAggregateResourceConfigResponse.proto"
    defaultService
    (Proxy :: Proxy SelectAggregateResourceConfig)

responsePutResourceConfig :: PutResourceConfigResponse -> TestTree
responsePutResourceConfig =
  res
    "PutResourceConfigResponse"
    "fixture/PutResourceConfigResponse.proto"
    defaultService
    (Proxy :: Proxy PutResourceConfig)

responseDescribeDeliveryChannels :: DescribeDeliveryChannelsResponse -> TestTree
responseDescribeDeliveryChannels =
  res
    "DescribeDeliveryChannelsResponse"
    "fixture/DescribeDeliveryChannelsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDeliveryChannels)

responseDeleteDeliveryChannel :: DeleteDeliveryChannelResponse -> TestTree
responseDeleteDeliveryChannel =
  res
    "DeleteDeliveryChannelResponse"
    "fixture/DeleteDeliveryChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeliveryChannel)

responseDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatusResponse -> TestTree
responseDescribeDeliveryChannelStatus =
  res
    "DescribeDeliveryChannelStatusResponse"
    "fixture/DescribeDeliveryChannelStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDeliveryChannelStatus)

responseDeleteOrganizationConfigRule :: DeleteOrganizationConfigRuleResponse -> TestTree
responseDeleteOrganizationConfigRule =
  res
    "DeleteOrganizationConfigRuleResponse"
    "fixture/DeleteOrganizationConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOrganizationConfigRule)

responsePutAggregationAuthorization :: PutAggregationAuthorizationResponse -> TestTree
responsePutAggregationAuthorization =
  res
    "PutAggregationAuthorizationResponse"
    "fixture/PutAggregationAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy PutAggregationAuthorization)

responsePutOrganizationConformancePack :: PutOrganizationConformancePackResponse -> TestTree
responsePutOrganizationConformancePack =
  res
    "PutOrganizationConformancePackResponse"
    "fixture/PutOrganizationConformancePackResponse.proto"
    defaultService
    (Proxy :: Proxy PutOrganizationConformancePack)

responsePutConfigurationRecorder :: PutConfigurationRecorderResponse -> TestTree
responsePutConfigurationRecorder =
  res
    "PutConfigurationRecorderResponse"
    "fixture/PutConfigurationRecorderResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationRecorder)

responseDeletePendingAggregationRequest :: DeletePendingAggregationRequestResponse -> TestTree
responseDeletePendingAggregationRequest =
  res
    "DeletePendingAggregationRequestResponse"
    "fixture/DeletePendingAggregationRequestResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePendingAggregationRequest)

responseDeliverConfigSnapshot :: DeliverConfigSnapshotResponse -> TestTree
responseDeliverConfigSnapshot =
  res
    "DeliverConfigSnapshotResponse"
    "fixture/DeliverConfigSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeliverConfigSnapshot)

responseBatchGetAggregateResourceConfig :: BatchGetAggregateResourceConfigResponse -> TestTree
responseBatchGetAggregateResourceConfig =
  res
    "BatchGetAggregateResourceConfigResponse"
    "fixture/BatchGetAggregateResourceConfigResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetAggregateResourceConfig)

responseGetComplianceSummaryByConfigRule :: GetComplianceSummaryByConfigRuleResponse -> TestTree
responseGetComplianceSummaryByConfigRule =
  res
    "GetComplianceSummaryByConfigRuleResponse"
    "fixture/GetComplianceSummaryByConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetComplianceSummaryByConfigRule)

responseDescribeOrganizationConformancePackStatuses :: DescribeOrganizationConformancePackStatusesResponse -> TestTree
responseDescribeOrganizationConformancePackStatuses =
  res
    "DescribeOrganizationConformancePackStatusesResponse"
    "fixture/DescribeOrganizationConformancePackStatusesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrganizationConformancePackStatuses)

responsePutStoredQuery :: PutStoredQueryResponse -> TestTree
responsePutStoredQuery =
  res
    "PutStoredQueryResponse"
    "fixture/PutStoredQueryResponse.proto"
    defaultService
    (Proxy :: Proxy PutStoredQuery)

responsePutConfigurationAggregator :: PutConfigurationAggregatorResponse -> TestTree
responsePutConfigurationAggregator =
  res
    "PutConfigurationAggregatorResponse"
    "fixture/PutConfigurationAggregatorResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationAggregator)

responseGetOrganizationConfigRuleDetailedStatus :: GetOrganizationConfigRuleDetailedStatusResponse -> TestTree
responseGetOrganizationConfigRuleDetailedStatus =
  res
    "GetOrganizationConfigRuleDetailedStatusResponse"
    "fixture/GetOrganizationConfigRuleDetailedStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetOrganizationConfigRuleDetailedStatus)

responseStartRemediationExecution :: StartRemediationExecutionResponse -> TestTree
responseStartRemediationExecution =
  res
    "StartRemediationExecutionResponse"
    "fixture/StartRemediationExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartRemediationExecution)

responseDescribeConformancePackStatus :: DescribeConformancePackStatusResponse -> TestTree
responseDescribeConformancePackStatus =
  res
    "DescribeConformancePackStatusResponse"
    "fixture/DescribeConformancePackStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConformancePackStatus)

responseDeleteConfigRule :: DeleteConfigRuleResponse -> TestTree
responseDeleteConfigRule =
  res
    "DeleteConfigRuleResponse"
    "fixture/DeleteConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigRule)

responseDeleteRetentionConfiguration :: DeleteRetentionConfigurationResponse -> TestTree
responseDeleteRetentionConfiguration =
  res
    "DeleteRetentionConfigurationResponse"
    "fixture/DeleteRetentionConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRetentionConfiguration)

responseDeleteConformancePack :: DeleteConformancePackResponse -> TestTree
responseDeleteConformancePack =
  res
    "DeleteConformancePackResponse"
    "fixture/DeleteConformancePackResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConformancePack)

responseGetOrganizationConformancePackDetailedStatus :: GetOrganizationConformancePackDetailedStatusResponse -> TestTree
responseGetOrganizationConformancePackDetailedStatus =
  res
    "GetOrganizationConformancePackDetailedStatusResponse"
    "fixture/GetOrganizationConformancePackDetailedStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetOrganizationConformancePackDetailedStatus)

responseDescribeAggregateComplianceByConfigRules :: DescribeAggregateComplianceByConfigRulesResponse -> TestTree
responseDescribeAggregateComplianceByConfigRules =
  res
    "DescribeAggregateComplianceByConfigRulesResponse"
    "fixture/DescribeAggregateComplianceByConfigRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAggregateComplianceByConfigRules)

responseStartConfigurationRecorder :: StartConfigurationRecorderResponse -> TestTree
responseStartConfigurationRecorder =
  res
    "StartConfigurationRecorderResponse"
    "fixture/StartConfigurationRecorderResponse.proto"
    defaultService
    (Proxy :: Proxy StartConfigurationRecorder)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetAggregateConfigRuleComplianceSummary :: GetAggregateConfigRuleComplianceSummaryResponse -> TestTree
responseGetAggregateConfigRuleComplianceSummary =
  res
    "GetAggregateConfigRuleComplianceSummaryResponse"
    "fixture/GetAggregateConfigRuleComplianceSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy GetAggregateConfigRuleComplianceSummary)

responsePutRemediationExceptions :: PutRemediationExceptionsResponse -> TestTree
responsePutRemediationExceptions =
  res
    "PutRemediationExceptionsResponse"
    "fixture/PutRemediationExceptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutRemediationExceptions)

responseDescribeConfigRules :: DescribeConfigRulesResponse -> TestTree
responseDescribeConfigRules =
  res
    "DescribeConfigRulesResponse"
    "fixture/DescribeConfigRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigRules)

responseStopConfigurationRecorder :: StopConfigurationRecorderResponse -> TestTree
responseStopConfigurationRecorder =
  res
    "StopConfigurationRecorderResponse"
    "fixture/StopConfigurationRecorderResponse.proto"
    defaultService
    (Proxy :: Proxy StopConfigurationRecorder)

responseDescribeConformancePacks :: DescribeConformancePacksResponse -> TestTree
responseDescribeConformancePacks =
  res
    "DescribeConformancePacksResponse"
    "fixture/DescribeConformancePacksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConformancePacks)

responseDescribeRetentionConfigurations :: DescribeRetentionConfigurationsResponse -> TestTree
responseDescribeRetentionConfigurations =
  res
    "DescribeRetentionConfigurationsResponse"
    "fixture/DescribeRetentionConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRetentionConfigurations)
