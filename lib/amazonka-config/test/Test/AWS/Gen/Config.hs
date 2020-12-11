{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Config
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestDescribePendingAggregationRequests $
--             mkDescribePendingAggregationRequests
--
--         , requestDescribeRemediationExecutionStatus $
--             mkDescribeRemediationExecutionStatus
--
--         , requestGetResourceConfigHistory $
--             mkGetResourceConfigHistory
--
--         , requestGetAggregateResourceConfig $
--             mkGetAggregateResourceConfig
--
--         , requestDescribeConfigurationAggregators $
--             mkDescribeConfigurationAggregators
--
--         , requestDescribeComplianceByConfigRule $
--             mkDescribeComplianceByConfigRule
--
--         , requestDescribeRetentionConfigurations $
--             mkDescribeRetentionConfigurations
--
--         , requestStopConfigurationRecorder $
--             mkStopConfigurationRecorder
--
--         , requestGetAggregateConfigRuleComplianceSummary $
--             mkGetAggregateConfigRuleComplianceSummary
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestBatchGetResourceConfig $
--             mkBatchGetResourceConfig
--
--         , requestDescribeConfigRules $
--             mkDescribeConfigRules
--
--         , requestPutRetentionConfiguration $
--             mkPutRetentionConfiguration
--
--         , requestGetOrganizationConformancePackDetailedStatus $
--             mkGetOrganizationConformancePackDetailedStatus
--
--         , requestDescribeAggregateComplianceByConfigRules $
--             mkDescribeAggregateComplianceByConfigRules
--
--         , requestDeleteEvaluationResults $
--             mkDeleteEvaluationResults
--
--         , requestPutConfigRule $
--             mkPutConfigRule
--
--         , requestGetConformancePackComplianceDetails $
--             mkGetConformancePackComplianceDetails
--
--         , requestDeleteConfigRule $
--             mkDeleteConfigRule
--
--         , requestDeleteRetentionConfiguration $
--             mkDeleteRetentionConfiguration
--
--         , requestSelectResourceConfig $
--             mkSelectResourceConfig
--
--         , requestListAggregateDiscoveredResources $
--             mkListAggregateDiscoveredResources
--
--         , requestDescribeOrganizationConfigRuleStatuses $
--             mkDescribeOrganizationConfigRuleStatuses
--
--         , requestDescribeOrganizationConformancePackStatuses $
--             mkDescribeOrganizationConformancePackStatuses
--
--         , requestGetComplianceDetailsByResource $
--             mkGetComplianceDetailsByResource
--
--         , requestDeletePendingAggregationRequest $
--             mkDeletePendingAggregationRequest
--
--         , requestDeliverConfigSnapshot $
--             mkDeliverConfigSnapshot
--
--         , requestBatchGetAggregateResourceConfig $
--             mkBatchGetAggregateResourceConfig
--
--         , requestDescribeConfigRuleEvaluationStatus $
--             mkDescribeConfigRuleEvaluationStatus
--
--         , requestGetDiscoveredResourceCounts $
--             mkGetDiscoveredResourceCounts
--
--         , requestDescribeRemediationExceptions $
--             mkDescribeRemediationExceptions
--
--         , requestDeleteOrganizationConformancePack $
--             mkDeleteOrganizationConformancePack
--
--         , requestPutOrganizationConfigRule $
--             mkPutOrganizationConfigRule
--
--         , requestPutOrganizationConformancePack $
--             mkPutOrganizationConformancePack
--
--         , requestDeleteOrganizationConfigRule $
--             mkDeleteOrganizationConfigRule
--
--         , requestPutResourceConfig $
--             mkPutResourceConfig
--
--         , requestStartConfigRulesEvaluation $
--             mkStartConfigRulesEvaluation
--
--         , requestDescribeOrganizationConfigRules $
--             mkDescribeOrganizationConfigRules
--
--         , requestSelectAggregateResourceConfig $
--             mkSelectAggregateResourceConfig
--
--         , requestDescribeComplianceByResource $
--             mkDescribeComplianceByResource
--
--         , requestDescribeOrganizationConformancePacks $
--             mkDescribeOrganizationConformancePacks
--
--         , requestDeleteResourceConfig $
--             mkDeleteResourceConfig
--
--         , requestPutEvaluations $
--             mkPutEvaluations
--
--         , requestDescribeConfigurationRecorders $
--             mkDescribeConfigurationRecorders
--
--         , requestDescribeConformancePackCompliance $
--             mkDescribeConformancePackCompliance
--
--         , requestGetAggregateComplianceDetailsByConfigRule $
--             mkGetAggregateComplianceDetailsByConfigRule
--
--         , requestGetAggregateDiscoveredResourceCounts $
--             mkGetAggregateDiscoveredResourceCounts
--
--         , requestStartConfigurationRecorder $
--             mkStartConfigurationRecorder
--
--         , requestDescribeConformancePacks $
--             mkDescribeConformancePacks
--
--         , requestDeleteRemediationExceptions $
--             mkDeleteRemediationExceptions
--
--         , requestPutRemediationExceptions $
--             mkPutRemediationExceptions
--
--         , requestGetOrganizationConfigRuleDetailedStatus $
--             mkGetOrganizationConfigRuleDetailedStatus
--
--         , requestPutRemediationConfigurations $
--             mkPutRemediationConfigurations
--
--         , requestDeleteConformancePack $
--             mkDeleteConformancePack
--
--         , requestPutConformancePack $
--             mkPutConformancePack
--
--         , requestStartRemediationExecution $
--             mkStartRemediationExecution
--
--         , requestDescribeConformancePackStatus $
--             mkDescribeConformancePackStatus
--
--         , requestGetComplianceSummaryByConfigRule $
--             mkGetComplianceSummaryByConfigRule
--
--         , requestPutConfigurationAggregator $
--             mkPutConfigurationAggregator
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestDeleteConfigurationAggregator $
--             mkDeleteConfigurationAggregator
--
--         , requestDescribeConfigurationRecorderStatus $
--             mkDescribeConfigurationRecorderStatus
--
--         , requestPutConfigurationRecorder $
--             mkPutConfigurationRecorder
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDeleteConfigurationRecorder $
--             mkDeleteConfigurationRecorder
--
--         , requestGetConformancePackComplianceSummary $
--             mkGetConformancePackComplianceSummary
--
--         , requestGetComplianceSummaryByResourceType $
--             mkGetComplianceSummaryByResourceType
--
--         , requestDescribeDeliveryChannelStatus $
--             mkDescribeDeliveryChannelStatus
--
--         , requestPutDeliveryChannel $
--             mkPutDeliveryChannel
--
--         , requestGetComplianceDetailsByConfigRule $
--             mkGetComplianceDetailsByConfigRule
--
--         , requestDeleteAggregationAuthorization $
--             mkDeleteAggregationAuthorization
--
--         , requestDeleteDeliveryChannel $
--             mkDeleteDeliveryChannel
--
--         , requestDeleteRemediationConfiguration $
--             mkDeleteRemediationConfiguration
--
--         , requestPutAggregationAuthorization $
--             mkPutAggregationAuthorization
--
--         , requestDescribeConfigurationAggregatorSourcesStatus $
--             mkDescribeConfigurationAggregatorSourcesStatus
--
--         , requestListDiscoveredResources $
--             mkListDiscoveredResources
--
--         , requestDescribeRemediationConfigurations $
--             mkDescribeRemediationConfigurations
--
--         , requestDescribeDeliveryChannels $
--             mkDescribeDeliveryChannels
--
--         , requestDescribeAggregationAuthorizations $
--             mkDescribeAggregationAuthorizations
--
--           ]

--     , testGroup "response"
--         [ responseDescribePendingAggregationRequests $
--             mkDescribePendingAggregationRequestsResponse
--
--         , responseDescribeRemediationExecutionStatus $
--             mkDescribeRemediationExecutionStatusResponse
--
--         , responseGetResourceConfigHistory $
--             mkGetResourceConfigHistoryResponse
--
--         , responseGetAggregateResourceConfig $
--             mkGetAggregateResourceConfigResponse
--
--         , responseDescribeConfigurationAggregators $
--             mkDescribeConfigurationAggregatorsResponse
--
--         , responseDescribeComplianceByConfigRule $
--             mkDescribeComplianceByConfigRuleResponse
--
--         , responseDescribeRetentionConfigurations $
--             mkDescribeRetentionConfigurationsResponse
--
--         , responseStopConfigurationRecorder $
--             mkStopConfigurationRecorderResponse
--
--         , responseGetAggregateConfigRuleComplianceSummary $
--             mkGetAggregateConfigRuleComplianceSummaryResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseBatchGetResourceConfig $
--             mkBatchGetResourceConfigResponse
--
--         , responseDescribeConfigRules $
--             mkDescribeConfigRulesResponse
--
--         , responsePutRetentionConfiguration $
--             mkPutRetentionConfigurationResponse
--
--         , responseGetOrganizationConformancePackDetailedStatus $
--             mkGetOrganizationConformancePackDetailedStatusResponse
--
--         , responseDescribeAggregateComplianceByConfigRules $
--             mkDescribeAggregateComplianceByConfigRulesResponse
--
--         , responseDeleteEvaluationResults $
--             mkDeleteEvaluationResultsResponse
--
--         , responsePutConfigRule $
--             mkPutConfigRuleResponse
--
--         , responseGetConformancePackComplianceDetails $
--             mkGetConformancePackComplianceDetailsResponse
--
--         , responseDeleteConfigRule $
--             mkDeleteConfigRuleResponse
--
--         , responseDeleteRetentionConfiguration $
--             mkDeleteRetentionConfigurationResponse
--
--         , responseSelectResourceConfig $
--             mkSelectResourceConfigResponse
--
--         , responseListAggregateDiscoveredResources $
--             mkListAggregateDiscoveredResourcesResponse
--
--         , responseDescribeOrganizationConfigRuleStatuses $
--             mkDescribeOrganizationConfigRuleStatusesResponse
--
--         , responseDescribeOrganizationConformancePackStatuses $
--             mkDescribeOrganizationConformancePackStatusesResponse
--
--         , responseGetComplianceDetailsByResource $
--             mkGetComplianceDetailsByResourceResponse
--
--         , responseDeletePendingAggregationRequest $
--             mkDeletePendingAggregationRequestResponse
--
--         , responseDeliverConfigSnapshot $
--             mkDeliverConfigSnapshotResponse
--
--         , responseBatchGetAggregateResourceConfig $
--             mkBatchGetAggregateResourceConfigResponse
--
--         , responseDescribeConfigRuleEvaluationStatus $
--             mkDescribeConfigRuleEvaluationStatusResponse
--
--         , responseGetDiscoveredResourceCounts $
--             mkGetDiscoveredResourceCountsResponse
--
--         , responseDescribeRemediationExceptions $
--             mkDescribeRemediationExceptionsResponse
--
--         , responseDeleteOrganizationConformancePack $
--             mkDeleteOrganizationConformancePackResponse
--
--         , responsePutOrganizationConfigRule $
--             mkPutOrganizationConfigRuleResponse
--
--         , responsePutOrganizationConformancePack $
--             mkPutOrganizationConformancePackResponse
--
--         , responseDeleteOrganizationConfigRule $
--             mkDeleteOrganizationConfigRuleResponse
--
--         , responsePutResourceConfig $
--             mkPutResourceConfigResponse
--
--         , responseStartConfigRulesEvaluation $
--             mkStartConfigRulesEvaluationResponse
--
--         , responseDescribeOrganizationConfigRules $
--             mkDescribeOrganizationConfigRulesResponse
--
--         , responseSelectAggregateResourceConfig $
--             mkSelectAggregateResourceConfigResponse
--
--         , responseDescribeComplianceByResource $
--             mkDescribeComplianceByResourceResponse
--
--         , responseDescribeOrganizationConformancePacks $
--             mkDescribeOrganizationConformancePacksResponse
--
--         , responseDeleteResourceConfig $
--             mkDeleteResourceConfigResponse
--
--         , responsePutEvaluations $
--             mkPutEvaluationsResponse
--
--         , responseDescribeConfigurationRecorders $
--             mkDescribeConfigurationRecordersResponse
--
--         , responseDescribeConformancePackCompliance $
--             mkDescribeConformancePackComplianceResponse
--
--         , responseGetAggregateComplianceDetailsByConfigRule $
--             mkGetAggregateComplianceDetailsByConfigRuleResponse
--
--         , responseGetAggregateDiscoveredResourceCounts $
--             mkGetAggregateDiscoveredResourceCountsResponse
--
--         , responseStartConfigurationRecorder $
--             mkStartConfigurationRecorderResponse
--
--         , responseDescribeConformancePacks $
--             mkDescribeConformancePacksResponse
--
--         , responseDeleteRemediationExceptions $
--             mkDeleteRemediationExceptionsResponse
--
--         , responsePutRemediationExceptions $
--             mkPutRemediationExceptionsResponse
--
--         , responseGetOrganizationConfigRuleDetailedStatus $
--             mkGetOrganizationConfigRuleDetailedStatusResponse
--
--         , responsePutRemediationConfigurations $
--             mkPutRemediationConfigurationsResponse
--
--         , responseDeleteConformancePack $
--             mkDeleteConformancePackResponse
--
--         , responsePutConformancePack $
--             mkPutConformancePackResponse
--
--         , responseStartRemediationExecution $
--             mkStartRemediationExecutionResponse
--
--         , responseDescribeConformancePackStatus $
--             mkDescribeConformancePackStatusResponse
--
--         , responseGetComplianceSummaryByConfigRule $
--             mkGetComplianceSummaryByConfigRuleResponse
--
--         , responsePutConfigurationAggregator $
--             mkPutConfigurationAggregatorResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseDeleteConfigurationAggregator $
--             mkDeleteConfigurationAggregatorResponse
--
--         , responseDescribeConfigurationRecorderStatus $
--             mkDescribeConfigurationRecorderStatusResponse
--
--         , responsePutConfigurationRecorder $
--             mkPutConfigurationRecorderResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDeleteConfigurationRecorder $
--             mkDeleteConfigurationRecorderResponse
--
--         , responseGetConformancePackComplianceSummary $
--             mkGetConformancePackComplianceSummaryResponse
--
--         , responseGetComplianceSummaryByResourceType $
--             mkGetComplianceSummaryByResourceTypeResponse
--
--         , responseDescribeDeliveryChannelStatus $
--             mkDescribeDeliveryChannelStatusResponse
--
--         , responsePutDeliveryChannel $
--             mkPutDeliveryChannelResponse
--
--         , responseGetComplianceDetailsByConfigRule $
--             mkGetComplianceDetailsByConfigRuleResponse
--
--         , responseDeleteAggregationAuthorization $
--             mkDeleteAggregationAuthorizationResponse
--
--         , responseDeleteDeliveryChannel $
--             mkDeleteDeliveryChannelResponse
--
--         , responseDeleteRemediationConfiguration $
--             mkDeleteRemediationConfigurationResponse
--
--         , responsePutAggregationAuthorization $
--             mkPutAggregationAuthorizationResponse
--
--         , responseDescribeConfigurationAggregatorSourcesStatus $
--             mkDescribeConfigurationAggregatorSourcesStatusResponse
--
--         , responseListDiscoveredResources $
--             mkListDiscoveredResourcesResponse
--
--         , responseDescribeRemediationConfigurations $
--             mkDescribeRemediationConfigurationsResponse
--
--         , responseDescribeDeliveryChannels $
--             mkDescribeDeliveryChannelsResponse
--
--         , responseDescribeAggregationAuthorizations $
--             mkDescribeAggregationAuthorizationsResponse
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

requestPutConfigurationAggregator :: PutConfigurationAggregator -> TestTree
requestPutConfigurationAggregator =
  req
    "PutConfigurationAggregator"
    "fixture/PutConfigurationAggregator.yaml"

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
    configService
    (Proxy :: Proxy DescribePendingAggregationRequests)

responseDescribeRemediationExecutionStatus :: DescribeRemediationExecutionStatusResponse -> TestTree
responseDescribeRemediationExecutionStatus =
  res
    "DescribeRemediationExecutionStatusResponse"
    "fixture/DescribeRemediationExecutionStatusResponse.proto"
    configService
    (Proxy :: Proxy DescribeRemediationExecutionStatus)

responseGetResourceConfigHistory :: GetResourceConfigHistoryResponse -> TestTree
responseGetResourceConfigHistory =
  res
    "GetResourceConfigHistoryResponse"
    "fixture/GetResourceConfigHistoryResponse.proto"
    configService
    (Proxy :: Proxy GetResourceConfigHistory)

responseGetAggregateResourceConfig :: GetAggregateResourceConfigResponse -> TestTree
responseGetAggregateResourceConfig =
  res
    "GetAggregateResourceConfigResponse"
    "fixture/GetAggregateResourceConfigResponse.proto"
    configService
    (Proxy :: Proxy GetAggregateResourceConfig)

responseDescribeConfigurationAggregators :: DescribeConfigurationAggregatorsResponse -> TestTree
responseDescribeConfigurationAggregators =
  res
    "DescribeConfigurationAggregatorsResponse"
    "fixture/DescribeConfigurationAggregatorsResponse.proto"
    configService
    (Proxy :: Proxy DescribeConfigurationAggregators)

responseDescribeComplianceByConfigRule :: DescribeComplianceByConfigRuleResponse -> TestTree
responseDescribeComplianceByConfigRule =
  res
    "DescribeComplianceByConfigRuleResponse"
    "fixture/DescribeComplianceByConfigRuleResponse.proto"
    configService
    (Proxy :: Proxy DescribeComplianceByConfigRule)

responseDescribeRetentionConfigurations :: DescribeRetentionConfigurationsResponse -> TestTree
responseDescribeRetentionConfigurations =
  res
    "DescribeRetentionConfigurationsResponse"
    "fixture/DescribeRetentionConfigurationsResponse.proto"
    configService
    (Proxy :: Proxy DescribeRetentionConfigurations)

responseStopConfigurationRecorder :: StopConfigurationRecorderResponse -> TestTree
responseStopConfigurationRecorder =
  res
    "StopConfigurationRecorderResponse"
    "fixture/StopConfigurationRecorderResponse.proto"
    configService
    (Proxy :: Proxy StopConfigurationRecorder)

responseGetAggregateConfigRuleComplianceSummary :: GetAggregateConfigRuleComplianceSummaryResponse -> TestTree
responseGetAggregateConfigRuleComplianceSummary =
  res
    "GetAggregateConfigRuleComplianceSummaryResponse"
    "fixture/GetAggregateConfigRuleComplianceSummaryResponse.proto"
    configService
    (Proxy :: Proxy GetAggregateConfigRuleComplianceSummary)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    configService
    (Proxy :: Proxy ListTagsForResource)

responseBatchGetResourceConfig :: BatchGetResourceConfigResponse -> TestTree
responseBatchGetResourceConfig =
  res
    "BatchGetResourceConfigResponse"
    "fixture/BatchGetResourceConfigResponse.proto"
    configService
    (Proxy :: Proxy BatchGetResourceConfig)

responseDescribeConfigRules :: DescribeConfigRulesResponse -> TestTree
responseDescribeConfigRules =
  res
    "DescribeConfigRulesResponse"
    "fixture/DescribeConfigRulesResponse.proto"
    configService
    (Proxy :: Proxy DescribeConfigRules)

responsePutRetentionConfiguration :: PutRetentionConfigurationResponse -> TestTree
responsePutRetentionConfiguration =
  res
    "PutRetentionConfigurationResponse"
    "fixture/PutRetentionConfigurationResponse.proto"
    configService
    (Proxy :: Proxy PutRetentionConfiguration)

responseGetOrganizationConformancePackDetailedStatus :: GetOrganizationConformancePackDetailedStatusResponse -> TestTree
responseGetOrganizationConformancePackDetailedStatus =
  res
    "GetOrganizationConformancePackDetailedStatusResponse"
    "fixture/GetOrganizationConformancePackDetailedStatusResponse.proto"
    configService
    (Proxy :: Proxy GetOrganizationConformancePackDetailedStatus)

responseDescribeAggregateComplianceByConfigRules :: DescribeAggregateComplianceByConfigRulesResponse -> TestTree
responseDescribeAggregateComplianceByConfigRules =
  res
    "DescribeAggregateComplianceByConfigRulesResponse"
    "fixture/DescribeAggregateComplianceByConfigRulesResponse.proto"
    configService
    (Proxy :: Proxy DescribeAggregateComplianceByConfigRules)

responseDeleteEvaluationResults :: DeleteEvaluationResultsResponse -> TestTree
responseDeleteEvaluationResults =
  res
    "DeleteEvaluationResultsResponse"
    "fixture/DeleteEvaluationResultsResponse.proto"
    configService
    (Proxy :: Proxy DeleteEvaluationResults)

responsePutConfigRule :: PutConfigRuleResponse -> TestTree
responsePutConfigRule =
  res
    "PutConfigRuleResponse"
    "fixture/PutConfigRuleResponse.proto"
    configService
    (Proxy :: Proxy PutConfigRule)

responseGetConformancePackComplianceDetails :: GetConformancePackComplianceDetailsResponse -> TestTree
responseGetConformancePackComplianceDetails =
  res
    "GetConformancePackComplianceDetailsResponse"
    "fixture/GetConformancePackComplianceDetailsResponse.proto"
    configService
    (Proxy :: Proxy GetConformancePackComplianceDetails)

responseDeleteConfigRule :: DeleteConfigRuleResponse -> TestTree
responseDeleteConfigRule =
  res
    "DeleteConfigRuleResponse"
    "fixture/DeleteConfigRuleResponse.proto"
    configService
    (Proxy :: Proxy DeleteConfigRule)

responseDeleteRetentionConfiguration :: DeleteRetentionConfigurationResponse -> TestTree
responseDeleteRetentionConfiguration =
  res
    "DeleteRetentionConfigurationResponse"
    "fixture/DeleteRetentionConfigurationResponse.proto"
    configService
    (Proxy :: Proxy DeleteRetentionConfiguration)

responseSelectResourceConfig :: SelectResourceConfigResponse -> TestTree
responseSelectResourceConfig =
  res
    "SelectResourceConfigResponse"
    "fixture/SelectResourceConfigResponse.proto"
    configService
    (Proxy :: Proxy SelectResourceConfig)

responseListAggregateDiscoveredResources :: ListAggregateDiscoveredResourcesResponse -> TestTree
responseListAggregateDiscoveredResources =
  res
    "ListAggregateDiscoveredResourcesResponse"
    "fixture/ListAggregateDiscoveredResourcesResponse.proto"
    configService
    (Proxy :: Proxy ListAggregateDiscoveredResources)

responseDescribeOrganizationConfigRuleStatuses :: DescribeOrganizationConfigRuleStatusesResponse -> TestTree
responseDescribeOrganizationConfigRuleStatuses =
  res
    "DescribeOrganizationConfigRuleStatusesResponse"
    "fixture/DescribeOrganizationConfigRuleStatusesResponse.proto"
    configService
    (Proxy :: Proxy DescribeOrganizationConfigRuleStatuses)

responseDescribeOrganizationConformancePackStatuses :: DescribeOrganizationConformancePackStatusesResponse -> TestTree
responseDescribeOrganizationConformancePackStatuses =
  res
    "DescribeOrganizationConformancePackStatusesResponse"
    "fixture/DescribeOrganizationConformancePackStatusesResponse.proto"
    configService
    (Proxy :: Proxy DescribeOrganizationConformancePackStatuses)

responseGetComplianceDetailsByResource :: GetComplianceDetailsByResourceResponse -> TestTree
responseGetComplianceDetailsByResource =
  res
    "GetComplianceDetailsByResourceResponse"
    "fixture/GetComplianceDetailsByResourceResponse.proto"
    configService
    (Proxy :: Proxy GetComplianceDetailsByResource)

responseDeletePendingAggregationRequest :: DeletePendingAggregationRequestResponse -> TestTree
responseDeletePendingAggregationRequest =
  res
    "DeletePendingAggregationRequestResponse"
    "fixture/DeletePendingAggregationRequestResponse.proto"
    configService
    (Proxy :: Proxy DeletePendingAggregationRequest)

responseDeliverConfigSnapshot :: DeliverConfigSnapshotResponse -> TestTree
responseDeliverConfigSnapshot =
  res
    "DeliverConfigSnapshotResponse"
    "fixture/DeliverConfigSnapshotResponse.proto"
    configService
    (Proxy :: Proxy DeliverConfigSnapshot)

responseBatchGetAggregateResourceConfig :: BatchGetAggregateResourceConfigResponse -> TestTree
responseBatchGetAggregateResourceConfig =
  res
    "BatchGetAggregateResourceConfigResponse"
    "fixture/BatchGetAggregateResourceConfigResponse.proto"
    configService
    (Proxy :: Proxy BatchGetAggregateResourceConfig)

responseDescribeConfigRuleEvaluationStatus :: DescribeConfigRuleEvaluationStatusResponse -> TestTree
responseDescribeConfigRuleEvaluationStatus =
  res
    "DescribeConfigRuleEvaluationStatusResponse"
    "fixture/DescribeConfigRuleEvaluationStatusResponse.proto"
    configService
    (Proxy :: Proxy DescribeConfigRuleEvaluationStatus)

responseGetDiscoveredResourceCounts :: GetDiscoveredResourceCountsResponse -> TestTree
responseGetDiscoveredResourceCounts =
  res
    "GetDiscoveredResourceCountsResponse"
    "fixture/GetDiscoveredResourceCountsResponse.proto"
    configService
    (Proxy :: Proxy GetDiscoveredResourceCounts)

responseDescribeRemediationExceptions :: DescribeRemediationExceptionsResponse -> TestTree
responseDescribeRemediationExceptions =
  res
    "DescribeRemediationExceptionsResponse"
    "fixture/DescribeRemediationExceptionsResponse.proto"
    configService
    (Proxy :: Proxy DescribeRemediationExceptions)

responseDeleteOrganizationConformancePack :: DeleteOrganizationConformancePackResponse -> TestTree
responseDeleteOrganizationConformancePack =
  res
    "DeleteOrganizationConformancePackResponse"
    "fixture/DeleteOrganizationConformancePackResponse.proto"
    configService
    (Proxy :: Proxy DeleteOrganizationConformancePack)

responsePutOrganizationConfigRule :: PutOrganizationConfigRuleResponse -> TestTree
responsePutOrganizationConfigRule =
  res
    "PutOrganizationConfigRuleResponse"
    "fixture/PutOrganizationConfigRuleResponse.proto"
    configService
    (Proxy :: Proxy PutOrganizationConfigRule)

responsePutOrganizationConformancePack :: PutOrganizationConformancePackResponse -> TestTree
responsePutOrganizationConformancePack =
  res
    "PutOrganizationConformancePackResponse"
    "fixture/PutOrganizationConformancePackResponse.proto"
    configService
    (Proxy :: Proxy PutOrganizationConformancePack)

responseDeleteOrganizationConfigRule :: DeleteOrganizationConfigRuleResponse -> TestTree
responseDeleteOrganizationConfigRule =
  res
    "DeleteOrganizationConfigRuleResponse"
    "fixture/DeleteOrganizationConfigRuleResponse.proto"
    configService
    (Proxy :: Proxy DeleteOrganizationConfigRule)

responsePutResourceConfig :: PutResourceConfigResponse -> TestTree
responsePutResourceConfig =
  res
    "PutResourceConfigResponse"
    "fixture/PutResourceConfigResponse.proto"
    configService
    (Proxy :: Proxy PutResourceConfig)

responseStartConfigRulesEvaluation :: StartConfigRulesEvaluationResponse -> TestTree
responseStartConfigRulesEvaluation =
  res
    "StartConfigRulesEvaluationResponse"
    "fixture/StartConfigRulesEvaluationResponse.proto"
    configService
    (Proxy :: Proxy StartConfigRulesEvaluation)

responseDescribeOrganizationConfigRules :: DescribeOrganizationConfigRulesResponse -> TestTree
responseDescribeOrganizationConfigRules =
  res
    "DescribeOrganizationConfigRulesResponse"
    "fixture/DescribeOrganizationConfigRulesResponse.proto"
    configService
    (Proxy :: Proxy DescribeOrganizationConfigRules)

responseSelectAggregateResourceConfig :: SelectAggregateResourceConfigResponse -> TestTree
responseSelectAggregateResourceConfig =
  res
    "SelectAggregateResourceConfigResponse"
    "fixture/SelectAggregateResourceConfigResponse.proto"
    configService
    (Proxy :: Proxy SelectAggregateResourceConfig)

responseDescribeComplianceByResource :: DescribeComplianceByResourceResponse -> TestTree
responseDescribeComplianceByResource =
  res
    "DescribeComplianceByResourceResponse"
    "fixture/DescribeComplianceByResourceResponse.proto"
    configService
    (Proxy :: Proxy DescribeComplianceByResource)

responseDescribeOrganizationConformancePacks :: DescribeOrganizationConformancePacksResponse -> TestTree
responseDescribeOrganizationConformancePacks =
  res
    "DescribeOrganizationConformancePacksResponse"
    "fixture/DescribeOrganizationConformancePacksResponse.proto"
    configService
    (Proxy :: Proxy DescribeOrganizationConformancePacks)

responseDeleteResourceConfig :: DeleteResourceConfigResponse -> TestTree
responseDeleteResourceConfig =
  res
    "DeleteResourceConfigResponse"
    "fixture/DeleteResourceConfigResponse.proto"
    configService
    (Proxy :: Proxy DeleteResourceConfig)

responsePutEvaluations :: PutEvaluationsResponse -> TestTree
responsePutEvaluations =
  res
    "PutEvaluationsResponse"
    "fixture/PutEvaluationsResponse.proto"
    configService
    (Proxy :: Proxy PutEvaluations)

responseDescribeConfigurationRecorders :: DescribeConfigurationRecordersResponse -> TestTree
responseDescribeConfigurationRecorders =
  res
    "DescribeConfigurationRecordersResponse"
    "fixture/DescribeConfigurationRecordersResponse.proto"
    configService
    (Proxy :: Proxy DescribeConfigurationRecorders)

responseDescribeConformancePackCompliance :: DescribeConformancePackComplianceResponse -> TestTree
responseDescribeConformancePackCompliance =
  res
    "DescribeConformancePackComplianceResponse"
    "fixture/DescribeConformancePackComplianceResponse.proto"
    configService
    (Proxy :: Proxy DescribeConformancePackCompliance)

responseGetAggregateComplianceDetailsByConfigRule :: GetAggregateComplianceDetailsByConfigRuleResponse -> TestTree
responseGetAggregateComplianceDetailsByConfigRule =
  res
    "GetAggregateComplianceDetailsByConfigRuleResponse"
    "fixture/GetAggregateComplianceDetailsByConfigRuleResponse.proto"
    configService
    (Proxy :: Proxy GetAggregateComplianceDetailsByConfigRule)

responseGetAggregateDiscoveredResourceCounts :: GetAggregateDiscoveredResourceCountsResponse -> TestTree
responseGetAggregateDiscoveredResourceCounts =
  res
    "GetAggregateDiscoveredResourceCountsResponse"
    "fixture/GetAggregateDiscoveredResourceCountsResponse.proto"
    configService
    (Proxy :: Proxy GetAggregateDiscoveredResourceCounts)

responseStartConfigurationRecorder :: StartConfigurationRecorderResponse -> TestTree
responseStartConfigurationRecorder =
  res
    "StartConfigurationRecorderResponse"
    "fixture/StartConfigurationRecorderResponse.proto"
    configService
    (Proxy :: Proxy StartConfigurationRecorder)

responseDescribeConformancePacks :: DescribeConformancePacksResponse -> TestTree
responseDescribeConformancePacks =
  res
    "DescribeConformancePacksResponse"
    "fixture/DescribeConformancePacksResponse.proto"
    configService
    (Proxy :: Proxy DescribeConformancePacks)

responseDeleteRemediationExceptions :: DeleteRemediationExceptionsResponse -> TestTree
responseDeleteRemediationExceptions =
  res
    "DeleteRemediationExceptionsResponse"
    "fixture/DeleteRemediationExceptionsResponse.proto"
    configService
    (Proxy :: Proxy DeleteRemediationExceptions)

responsePutRemediationExceptions :: PutRemediationExceptionsResponse -> TestTree
responsePutRemediationExceptions =
  res
    "PutRemediationExceptionsResponse"
    "fixture/PutRemediationExceptionsResponse.proto"
    configService
    (Proxy :: Proxy PutRemediationExceptions)

responseGetOrganizationConfigRuleDetailedStatus :: GetOrganizationConfigRuleDetailedStatusResponse -> TestTree
responseGetOrganizationConfigRuleDetailedStatus =
  res
    "GetOrganizationConfigRuleDetailedStatusResponse"
    "fixture/GetOrganizationConfigRuleDetailedStatusResponse.proto"
    configService
    (Proxy :: Proxy GetOrganizationConfigRuleDetailedStatus)

responsePutRemediationConfigurations :: PutRemediationConfigurationsResponse -> TestTree
responsePutRemediationConfigurations =
  res
    "PutRemediationConfigurationsResponse"
    "fixture/PutRemediationConfigurationsResponse.proto"
    configService
    (Proxy :: Proxy PutRemediationConfigurations)

responseDeleteConformancePack :: DeleteConformancePackResponse -> TestTree
responseDeleteConformancePack =
  res
    "DeleteConformancePackResponse"
    "fixture/DeleteConformancePackResponse.proto"
    configService
    (Proxy :: Proxy DeleteConformancePack)

responsePutConformancePack :: PutConformancePackResponse -> TestTree
responsePutConformancePack =
  res
    "PutConformancePackResponse"
    "fixture/PutConformancePackResponse.proto"
    configService
    (Proxy :: Proxy PutConformancePack)

responseStartRemediationExecution :: StartRemediationExecutionResponse -> TestTree
responseStartRemediationExecution =
  res
    "StartRemediationExecutionResponse"
    "fixture/StartRemediationExecutionResponse.proto"
    configService
    (Proxy :: Proxy StartRemediationExecution)

responseDescribeConformancePackStatus :: DescribeConformancePackStatusResponse -> TestTree
responseDescribeConformancePackStatus =
  res
    "DescribeConformancePackStatusResponse"
    "fixture/DescribeConformancePackStatusResponse.proto"
    configService
    (Proxy :: Proxy DescribeConformancePackStatus)

responseGetComplianceSummaryByConfigRule :: GetComplianceSummaryByConfigRuleResponse -> TestTree
responseGetComplianceSummaryByConfigRule =
  res
    "GetComplianceSummaryByConfigRuleResponse"
    "fixture/GetComplianceSummaryByConfigRuleResponse.proto"
    configService
    (Proxy :: Proxy GetComplianceSummaryByConfigRule)

responsePutConfigurationAggregator :: PutConfigurationAggregatorResponse -> TestTree
responsePutConfigurationAggregator =
  res
    "PutConfigurationAggregatorResponse"
    "fixture/PutConfigurationAggregatorResponse.proto"
    configService
    (Proxy :: Proxy PutConfigurationAggregator)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    configService
    (Proxy :: Proxy TagResource)

responseDeleteConfigurationAggregator :: DeleteConfigurationAggregatorResponse -> TestTree
responseDeleteConfigurationAggregator =
  res
    "DeleteConfigurationAggregatorResponse"
    "fixture/DeleteConfigurationAggregatorResponse.proto"
    configService
    (Proxy :: Proxy DeleteConfigurationAggregator)

responseDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatusResponse -> TestTree
responseDescribeConfigurationRecorderStatus =
  res
    "DescribeConfigurationRecorderStatusResponse"
    "fixture/DescribeConfigurationRecorderStatusResponse.proto"
    configService
    (Proxy :: Proxy DescribeConfigurationRecorderStatus)

responsePutConfigurationRecorder :: PutConfigurationRecorderResponse -> TestTree
responsePutConfigurationRecorder =
  res
    "PutConfigurationRecorderResponse"
    "fixture/PutConfigurationRecorderResponse.proto"
    configService
    (Proxy :: Proxy PutConfigurationRecorder)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    configService
    (Proxy :: Proxy UntagResource)

responseDeleteConfigurationRecorder :: DeleteConfigurationRecorderResponse -> TestTree
responseDeleteConfigurationRecorder =
  res
    "DeleteConfigurationRecorderResponse"
    "fixture/DeleteConfigurationRecorderResponse.proto"
    configService
    (Proxy :: Proxy DeleteConfigurationRecorder)

responseGetConformancePackComplianceSummary :: GetConformancePackComplianceSummaryResponse -> TestTree
responseGetConformancePackComplianceSummary =
  res
    "GetConformancePackComplianceSummaryResponse"
    "fixture/GetConformancePackComplianceSummaryResponse.proto"
    configService
    (Proxy :: Proxy GetConformancePackComplianceSummary)

responseGetComplianceSummaryByResourceType :: GetComplianceSummaryByResourceTypeResponse -> TestTree
responseGetComplianceSummaryByResourceType =
  res
    "GetComplianceSummaryByResourceTypeResponse"
    "fixture/GetComplianceSummaryByResourceTypeResponse.proto"
    configService
    (Proxy :: Proxy GetComplianceSummaryByResourceType)

responseDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatusResponse -> TestTree
responseDescribeDeliveryChannelStatus =
  res
    "DescribeDeliveryChannelStatusResponse"
    "fixture/DescribeDeliveryChannelStatusResponse.proto"
    configService
    (Proxy :: Proxy DescribeDeliveryChannelStatus)

responsePutDeliveryChannel :: PutDeliveryChannelResponse -> TestTree
responsePutDeliveryChannel =
  res
    "PutDeliveryChannelResponse"
    "fixture/PutDeliveryChannelResponse.proto"
    configService
    (Proxy :: Proxy PutDeliveryChannel)

responseGetComplianceDetailsByConfigRule :: GetComplianceDetailsByConfigRuleResponse -> TestTree
responseGetComplianceDetailsByConfigRule =
  res
    "GetComplianceDetailsByConfigRuleResponse"
    "fixture/GetComplianceDetailsByConfigRuleResponse.proto"
    configService
    (Proxy :: Proxy GetComplianceDetailsByConfigRule)

responseDeleteAggregationAuthorization :: DeleteAggregationAuthorizationResponse -> TestTree
responseDeleteAggregationAuthorization =
  res
    "DeleteAggregationAuthorizationResponse"
    "fixture/DeleteAggregationAuthorizationResponse.proto"
    configService
    (Proxy :: Proxy DeleteAggregationAuthorization)

responseDeleteDeliveryChannel :: DeleteDeliveryChannelResponse -> TestTree
responseDeleteDeliveryChannel =
  res
    "DeleteDeliveryChannelResponse"
    "fixture/DeleteDeliveryChannelResponse.proto"
    configService
    (Proxy :: Proxy DeleteDeliveryChannel)

responseDeleteRemediationConfiguration :: DeleteRemediationConfigurationResponse -> TestTree
responseDeleteRemediationConfiguration =
  res
    "DeleteRemediationConfigurationResponse"
    "fixture/DeleteRemediationConfigurationResponse.proto"
    configService
    (Proxy :: Proxy DeleteRemediationConfiguration)

responsePutAggregationAuthorization :: PutAggregationAuthorizationResponse -> TestTree
responsePutAggregationAuthorization =
  res
    "PutAggregationAuthorizationResponse"
    "fixture/PutAggregationAuthorizationResponse.proto"
    configService
    (Proxy :: Proxy PutAggregationAuthorization)

responseDescribeConfigurationAggregatorSourcesStatus :: DescribeConfigurationAggregatorSourcesStatusResponse -> TestTree
responseDescribeConfigurationAggregatorSourcesStatus =
  res
    "DescribeConfigurationAggregatorSourcesStatusResponse"
    "fixture/DescribeConfigurationAggregatorSourcesStatusResponse.proto"
    configService
    (Proxy :: Proxy DescribeConfigurationAggregatorSourcesStatus)

responseListDiscoveredResources :: ListDiscoveredResourcesResponse -> TestTree
responseListDiscoveredResources =
  res
    "ListDiscoveredResourcesResponse"
    "fixture/ListDiscoveredResourcesResponse.proto"
    configService
    (Proxy :: Proxy ListDiscoveredResources)

responseDescribeRemediationConfigurations :: DescribeRemediationConfigurationsResponse -> TestTree
responseDescribeRemediationConfigurations =
  res
    "DescribeRemediationConfigurationsResponse"
    "fixture/DescribeRemediationConfigurationsResponse.proto"
    configService
    (Proxy :: Proxy DescribeRemediationConfigurations)

responseDescribeDeliveryChannels :: DescribeDeliveryChannelsResponse -> TestTree
responseDescribeDeliveryChannels =
  res
    "DescribeDeliveryChannelsResponse"
    "fixture/DescribeDeliveryChannelsResponse.proto"
    configService
    (Proxy :: Proxy DescribeDeliveryChannels)

responseDescribeAggregationAuthorizations :: DescribeAggregationAuthorizationsResponse -> TestTree
responseDescribeAggregationAuthorizations =
  res
    "DescribeAggregationAuthorizationsResponse"
    "fixture/DescribeAggregationAuthorizationsResponse.proto"
    configService
    (Proxy :: Proxy DescribeAggregationAuthorizations)
