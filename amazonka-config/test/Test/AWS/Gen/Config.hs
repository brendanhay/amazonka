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
--         , requestGetAggregateResourceConfig $
--             newGetAggregateResourceConfig
--
--         , requestGetStoredQuery $
--             newGetStoredQuery
--
--         , requestDescribeConfigurationAggregators $
--             newDescribeConfigurationAggregators
--
--         , requestGetAggregateComplianceDetailsByConfigRule $
--             newGetAggregateComplianceDetailsByConfigRule
--
--         , requestGetResourceConfigHistory $
--             newGetResourceConfigHistory
--
--         , requestDescribeRemediationExecutionStatus $
--             newDescribeRemediationExecutionStatus
--
--         , requestDescribePendingAggregationRequests $
--             newDescribePendingAggregationRequests
--
--         , requestDescribeConformancePackCompliance $
--             newDescribeConformancePackCompliance
--
--         , requestStartConfigRulesEvaluation $
--             newStartConfigRulesEvaluation
--
--         , requestListDiscoveredResources $
--             newListDiscoveredResources
--
--         , requestDescribeAggregationAuthorizations $
--             newDescribeAggregationAuthorizations
--
--         , requestDescribeComplianceByResource $
--             newDescribeComplianceByResource
--
--         , requestDescribeOrganizationConformancePacks $
--             newDescribeOrganizationConformancePacks
--
--         , requestDescribeRemediationConfigurations $
--             newDescribeRemediationConfigurations
--
--         , requestDeleteResourceConfig $
--             newDeleteResourceConfig
--
--         , requestDescribeConfigurationAggregatorSourcesStatus $
--             newDescribeConfigurationAggregatorSourcesStatus
--
--         , requestDeleteOrganizationConformancePack $
--             newDeleteOrganizationConformancePack
--
--         , requestDeleteAggregationAuthorization $
--             newDeleteAggregationAuthorization
--
--         , requestDescribeRemediationExceptions $
--             newDescribeRemediationExceptions
--
--         , requestDeleteRemediationConfiguration $
--             newDeleteRemediationConfiguration
--
--         , requestGetComplianceSummaryByResourceType $
--             newGetComplianceSummaryByResourceType
--
--         , requestGetComplianceDetailsByConfigRule $
--             newGetComplianceDetailsByConfigRule
--
--         , requestGetDiscoveredResourceCounts $
--             newGetDiscoveredResourceCounts
--
--         , requestPutDeliveryChannel $
--             newPutDeliveryChannel
--
--         , requestPutOrganizationConfigRule $
--             newPutOrganizationConfigRule
--
--         , requestDeleteConfigurationRecorder $
--             newDeleteConfigurationRecorder
--
--         , requestGetConformancePackComplianceSummary $
--             newGetConformancePackComplianceSummary
--
--         , requestDescribeConfigurationRecorderStatus $
--             newDescribeConfigurationRecorderStatus
--
--         , requestDescribeConfigRuleEvaluationStatus $
--             newDescribeConfigRuleEvaluationStatus
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteConfigurationAggregator $
--             newDeleteConfigurationAggregator
--
--         , requestListAggregateDiscoveredResources $
--             newListAggregateDiscoveredResources
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeOrganizationConfigRuleStatuses $
--             newDescribeOrganizationConfigRuleStatuses
--
--         , requestSelectResourceConfig $
--             newSelectResourceConfig
--
--         , requestDeleteStoredQuery $
--             newDeleteStoredQuery
--
--         , requestGetComplianceDetailsByResource $
--             newGetComplianceDetailsByResource
--
--         , requestListStoredQueries $
--             newListStoredQueries
--
--         , requestDescribeAggregateComplianceByConformancePacks $
--             newDescribeAggregateComplianceByConformancePacks
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
--         , requestPutRetentionConfiguration $
--             newPutRetentionConfiguration
--
--         , requestPutConformancePack $
--             newPutConformancePack
--
--         , requestGetConformancePackComplianceDetails $
--             newGetConformancePackComplianceDetails
--
--         , requestPutExternalEvaluation $
--             newPutExternalEvaluation
--
--         , requestBatchGetResourceConfig $
--             newBatchGetResourceConfig
--
--         , requestDeleteRemediationExceptions $
--             newDeleteRemediationExceptions
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
--         , requestSelectAggregateResourceConfig $
--             newSelectAggregateResourceConfig
--
--         , requestDescribeDeliveryChannels $
--             newDescribeDeliveryChannels
--
--         , requestPutResourceConfig $
--             newPutResourceConfig
--
--         , requestDescribeOrganizationConfigRules $
--             newDescribeOrganizationConfigRules
--
--         , requestDeleteDeliveryChannel $
--             newDeleteDeliveryChannel
--
--         , requestPutOrganizationConformancePack $
--             newPutOrganizationConformancePack
--
--         , requestPutAggregationAuthorization $
--             newPutAggregationAuthorization
--
--         , requestDeleteOrganizationConfigRule $
--             newDeleteOrganizationConfigRule
--
--         , requestDescribeDeliveryChannelStatus $
--             newDescribeDeliveryChannelStatus
--
--         , requestBatchGetAggregateResourceConfig $
--             newBatchGetAggregateResourceConfig
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
--         , requestPutConfigurationAggregator $
--             newPutConfigurationAggregator
--
--         , requestPutStoredQuery $
--             newPutStoredQuery
--
--         , requestGetComplianceSummaryByConfigRule $
--             newGetComplianceSummaryByConfigRule
--
--         , requestDescribeOrganizationConformancePackStatuses $
--             newDescribeOrganizationConformancePackStatuses
--
--         , requestGetOrganizationConfigRuleDetailedStatus $
--             newGetOrganizationConfigRuleDetailedStatus
--
--         , requestDescribeAggregateComplianceByConfigRules $
--             newDescribeAggregateComplianceByConfigRules
--
--         , requestDeleteConfigRule $
--             newDeleteConfigRule
--
--         , requestDescribeConformancePackStatus $
--             newDescribeConformancePackStatus
--
--         , requestDeleteConformancePack $
--             newDeleteConformancePack
--
--         , requestStartRemediationExecution $
--             newStartRemediationExecution
--
--         , requestGetOrganizationConformancePackDetailedStatus $
--             newGetOrganizationConformancePackDetailedStatus
--
--         , requestDeleteRetentionConfiguration $
--             newDeleteRetentionConfiguration
--
--         , requestGetAggregateConformancePackComplianceSummary $
--             newGetAggregateConformancePackComplianceSummary
--
--         , requestPutRemediationExceptions $
--             newPutRemediationExceptions
--
--         , requestStopConfigurationRecorder $
--             newStopConfigurationRecorder
--
--         , requestDescribeConfigRules $
--             newDescribeConfigRules
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeRetentionConfigurations $
--             newDescribeRetentionConfigurations
--
--         , requestStartConfigurationRecorder $
--             newStartConfigurationRecorder
--
--         , requestGetAggregateConfigRuleComplianceSummary $
--             newGetAggregateConfigRuleComplianceSummary
--
--         , requestDescribeConformancePacks $
--             newDescribeConformancePacks
--
--           ]

--     , testGroup "response"
--         [ responseDescribeComplianceByConfigRule $
--             newDescribeComplianceByConfigRuleResponse
--
--         , responseGetAggregateResourceConfig $
--             newGetAggregateResourceConfigResponse
--
--         , responseGetStoredQuery $
--             newGetStoredQueryResponse
--
--         , responseDescribeConfigurationAggregators $
--             newDescribeConfigurationAggregatorsResponse
--
--         , responseGetAggregateComplianceDetailsByConfigRule $
--             newGetAggregateComplianceDetailsByConfigRuleResponse
--
--         , responseGetResourceConfigHistory $
--             newGetResourceConfigHistoryResponse
--
--         , responseDescribeRemediationExecutionStatus $
--             newDescribeRemediationExecutionStatusResponse
--
--         , responseDescribePendingAggregationRequests $
--             newDescribePendingAggregationRequestsResponse
--
--         , responseDescribeConformancePackCompliance $
--             newDescribeConformancePackComplianceResponse
--
--         , responseStartConfigRulesEvaluation $
--             newStartConfigRulesEvaluationResponse
--
--         , responseListDiscoveredResources $
--             newListDiscoveredResourcesResponse
--
--         , responseDescribeAggregationAuthorizations $
--             newDescribeAggregationAuthorizationsResponse
--
--         , responseDescribeComplianceByResource $
--             newDescribeComplianceByResourceResponse
--
--         , responseDescribeOrganizationConformancePacks $
--             newDescribeOrganizationConformancePacksResponse
--
--         , responseDescribeRemediationConfigurations $
--             newDescribeRemediationConfigurationsResponse
--
--         , responseDeleteResourceConfig $
--             newDeleteResourceConfigResponse
--
--         , responseDescribeConfigurationAggregatorSourcesStatus $
--             newDescribeConfigurationAggregatorSourcesStatusResponse
--
--         , responseDeleteOrganizationConformancePack $
--             newDeleteOrganizationConformancePackResponse
--
--         , responseDeleteAggregationAuthorization $
--             newDeleteAggregationAuthorizationResponse
--
--         , responseDescribeRemediationExceptions $
--             newDescribeRemediationExceptionsResponse
--
--         , responseDeleteRemediationConfiguration $
--             newDeleteRemediationConfigurationResponse
--
--         , responseGetComplianceSummaryByResourceType $
--             newGetComplianceSummaryByResourceTypeResponse
--
--         , responseGetComplianceDetailsByConfigRule $
--             newGetComplianceDetailsByConfigRuleResponse
--
--         , responseGetDiscoveredResourceCounts $
--             newGetDiscoveredResourceCountsResponse
--
--         , responsePutDeliveryChannel $
--             newPutDeliveryChannelResponse
--
--         , responsePutOrganizationConfigRule $
--             newPutOrganizationConfigRuleResponse
--
--         , responseDeleteConfigurationRecorder $
--             newDeleteConfigurationRecorderResponse
--
--         , responseGetConformancePackComplianceSummary $
--             newGetConformancePackComplianceSummaryResponse
--
--         , responseDescribeConfigurationRecorderStatus $
--             newDescribeConfigurationRecorderStatusResponse
--
--         , responseDescribeConfigRuleEvaluationStatus $
--             newDescribeConfigRuleEvaluationStatusResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteConfigurationAggregator $
--             newDeleteConfigurationAggregatorResponse
--
--         , responseListAggregateDiscoveredResources $
--             newListAggregateDiscoveredResourcesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeOrganizationConfigRuleStatuses $
--             newDescribeOrganizationConfigRuleStatusesResponse
--
--         , responseSelectResourceConfig $
--             newSelectResourceConfigResponse
--
--         , responseDeleteStoredQuery $
--             newDeleteStoredQueryResponse
--
--         , responseGetComplianceDetailsByResource $
--             newGetComplianceDetailsByResourceResponse
--
--         , responseListStoredQueries $
--             newListStoredQueriesResponse
--
--         , responseDescribeAggregateComplianceByConformancePacks $
--             newDescribeAggregateComplianceByConformancePacksResponse
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
--         , responsePutRetentionConfiguration $
--             newPutRetentionConfigurationResponse
--
--         , responsePutConformancePack $
--             newPutConformancePackResponse
--
--         , responseGetConformancePackComplianceDetails $
--             newGetConformancePackComplianceDetailsResponse
--
--         , responsePutExternalEvaluation $
--             newPutExternalEvaluationResponse
--
--         , responseBatchGetResourceConfig $
--             newBatchGetResourceConfigResponse
--
--         , responseDeleteRemediationExceptions $
--             newDeleteRemediationExceptionsResponse
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
--         , responseSelectAggregateResourceConfig $
--             newSelectAggregateResourceConfigResponse
--
--         , responseDescribeDeliveryChannels $
--             newDescribeDeliveryChannelsResponse
--
--         , responsePutResourceConfig $
--             newPutResourceConfigResponse
--
--         , responseDescribeOrganizationConfigRules $
--             newDescribeOrganizationConfigRulesResponse
--
--         , responseDeleteDeliveryChannel $
--             newDeleteDeliveryChannelResponse
--
--         , responsePutOrganizationConformancePack $
--             newPutOrganizationConformancePackResponse
--
--         , responsePutAggregationAuthorization $
--             newPutAggregationAuthorizationResponse
--
--         , responseDeleteOrganizationConfigRule $
--             newDeleteOrganizationConfigRuleResponse
--
--         , responseDescribeDeliveryChannelStatus $
--             newDescribeDeliveryChannelStatusResponse
--
--         , responseBatchGetAggregateResourceConfig $
--             newBatchGetAggregateResourceConfigResponse
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
--         , responsePutConfigurationAggregator $
--             newPutConfigurationAggregatorResponse
--
--         , responsePutStoredQuery $
--             newPutStoredQueryResponse
--
--         , responseGetComplianceSummaryByConfigRule $
--             newGetComplianceSummaryByConfigRuleResponse
--
--         , responseDescribeOrganizationConformancePackStatuses $
--             newDescribeOrganizationConformancePackStatusesResponse
--
--         , responseGetOrganizationConfigRuleDetailedStatus $
--             newGetOrganizationConfigRuleDetailedStatusResponse
--
--         , responseDescribeAggregateComplianceByConfigRules $
--             newDescribeAggregateComplianceByConfigRulesResponse
--
--         , responseDeleteConfigRule $
--             newDeleteConfigRuleResponse
--
--         , responseDescribeConformancePackStatus $
--             newDescribeConformancePackStatusResponse
--
--         , responseDeleteConformancePack $
--             newDeleteConformancePackResponse
--
--         , responseStartRemediationExecution $
--             newStartRemediationExecutionResponse
--
--         , responseGetOrganizationConformancePackDetailedStatus $
--             newGetOrganizationConformancePackDetailedStatusResponse
--
--         , responseDeleteRetentionConfiguration $
--             newDeleteRetentionConfigurationResponse
--
--         , responseGetAggregateConformancePackComplianceSummary $
--             newGetAggregateConformancePackComplianceSummaryResponse
--
--         , responsePutRemediationExceptions $
--             newPutRemediationExceptionsResponse
--
--         , responseStopConfigurationRecorder $
--             newStopConfigurationRecorderResponse
--
--         , responseDescribeConfigRules $
--             newDescribeConfigRulesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeRetentionConfigurations $
--             newDescribeRetentionConfigurationsResponse
--
--         , responseStartConfigurationRecorder $
--             newStartConfigurationRecorderResponse
--
--         , responseGetAggregateConfigRuleComplianceSummary $
--             newGetAggregateConfigRuleComplianceSummaryResponse
--
--         , responseDescribeConformancePacks $
--             newDescribeConformancePacksResponse
--
--           ]
--     ]

-- Requests

requestDescribeComplianceByConfigRule :: DescribeComplianceByConfigRule -> TestTree
requestDescribeComplianceByConfigRule =
  req
    "DescribeComplianceByConfigRule"
    "fixture/DescribeComplianceByConfigRule.yaml"

requestGetAggregateResourceConfig :: GetAggregateResourceConfig -> TestTree
requestGetAggregateResourceConfig =
  req
    "GetAggregateResourceConfig"
    "fixture/GetAggregateResourceConfig.yaml"

requestGetStoredQuery :: GetStoredQuery -> TestTree
requestGetStoredQuery =
  req
    "GetStoredQuery"
    "fixture/GetStoredQuery.yaml"

requestDescribeConfigurationAggregators :: DescribeConfigurationAggregators -> TestTree
requestDescribeConfigurationAggregators =
  req
    "DescribeConfigurationAggregators"
    "fixture/DescribeConfigurationAggregators.yaml"

requestGetAggregateComplianceDetailsByConfigRule :: GetAggregateComplianceDetailsByConfigRule -> TestTree
requestGetAggregateComplianceDetailsByConfigRule =
  req
    "GetAggregateComplianceDetailsByConfigRule"
    "fixture/GetAggregateComplianceDetailsByConfigRule.yaml"

requestGetResourceConfigHistory :: GetResourceConfigHistory -> TestTree
requestGetResourceConfigHistory =
  req
    "GetResourceConfigHistory"
    "fixture/GetResourceConfigHistory.yaml"

requestDescribeRemediationExecutionStatus :: DescribeRemediationExecutionStatus -> TestTree
requestDescribeRemediationExecutionStatus =
  req
    "DescribeRemediationExecutionStatus"
    "fixture/DescribeRemediationExecutionStatus.yaml"

requestDescribePendingAggregationRequests :: DescribePendingAggregationRequests -> TestTree
requestDescribePendingAggregationRequests =
  req
    "DescribePendingAggregationRequests"
    "fixture/DescribePendingAggregationRequests.yaml"

requestDescribeConformancePackCompliance :: DescribeConformancePackCompliance -> TestTree
requestDescribeConformancePackCompliance =
  req
    "DescribeConformancePackCompliance"
    "fixture/DescribeConformancePackCompliance.yaml"

requestStartConfigRulesEvaluation :: StartConfigRulesEvaluation -> TestTree
requestStartConfigRulesEvaluation =
  req
    "StartConfigRulesEvaluation"
    "fixture/StartConfigRulesEvaluation.yaml"

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

requestDescribeConfigurationAggregatorSourcesStatus :: DescribeConfigurationAggregatorSourcesStatus -> TestTree
requestDescribeConfigurationAggregatorSourcesStatus =
  req
    "DescribeConfigurationAggregatorSourcesStatus"
    "fixture/DescribeConfigurationAggregatorSourcesStatus.yaml"

requestDeleteOrganizationConformancePack :: DeleteOrganizationConformancePack -> TestTree
requestDeleteOrganizationConformancePack =
  req
    "DeleteOrganizationConformancePack"
    "fixture/DeleteOrganizationConformancePack.yaml"

requestDeleteAggregationAuthorization :: DeleteAggregationAuthorization -> TestTree
requestDeleteAggregationAuthorization =
  req
    "DeleteAggregationAuthorization"
    "fixture/DeleteAggregationAuthorization.yaml"

requestDescribeRemediationExceptions :: DescribeRemediationExceptions -> TestTree
requestDescribeRemediationExceptions =
  req
    "DescribeRemediationExceptions"
    "fixture/DescribeRemediationExceptions.yaml"

requestDeleteRemediationConfiguration :: DeleteRemediationConfiguration -> TestTree
requestDeleteRemediationConfiguration =
  req
    "DeleteRemediationConfiguration"
    "fixture/DeleteRemediationConfiguration.yaml"

requestGetComplianceSummaryByResourceType :: GetComplianceSummaryByResourceType -> TestTree
requestGetComplianceSummaryByResourceType =
  req
    "GetComplianceSummaryByResourceType"
    "fixture/GetComplianceSummaryByResourceType.yaml"

requestGetComplianceDetailsByConfigRule :: GetComplianceDetailsByConfigRule -> TestTree
requestGetComplianceDetailsByConfigRule =
  req
    "GetComplianceDetailsByConfigRule"
    "fixture/GetComplianceDetailsByConfigRule.yaml"

requestGetDiscoveredResourceCounts :: GetDiscoveredResourceCounts -> TestTree
requestGetDiscoveredResourceCounts =
  req
    "GetDiscoveredResourceCounts"
    "fixture/GetDiscoveredResourceCounts.yaml"

requestPutDeliveryChannel :: PutDeliveryChannel -> TestTree
requestPutDeliveryChannel =
  req
    "PutDeliveryChannel"
    "fixture/PutDeliveryChannel.yaml"

requestPutOrganizationConfigRule :: PutOrganizationConfigRule -> TestTree
requestPutOrganizationConfigRule =
  req
    "PutOrganizationConfigRule"
    "fixture/PutOrganizationConfigRule.yaml"

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

requestDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatus -> TestTree
requestDescribeConfigurationRecorderStatus =
  req
    "DescribeConfigurationRecorderStatus"
    "fixture/DescribeConfigurationRecorderStatus.yaml"

requestDescribeConfigRuleEvaluationStatus :: DescribeConfigRuleEvaluationStatus -> TestTree
requestDescribeConfigRuleEvaluationStatus =
  req
    "DescribeConfigRuleEvaluationStatus"
    "fixture/DescribeConfigRuleEvaluationStatus.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteConfigurationAggregator :: DeleteConfigurationAggregator -> TestTree
requestDeleteConfigurationAggregator =
  req
    "DeleteConfigurationAggregator"
    "fixture/DeleteConfigurationAggregator.yaml"

requestListAggregateDiscoveredResources :: ListAggregateDiscoveredResources -> TestTree
requestListAggregateDiscoveredResources =
  req
    "ListAggregateDiscoveredResources"
    "fixture/ListAggregateDiscoveredResources.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeOrganizationConfigRuleStatuses :: DescribeOrganizationConfigRuleStatuses -> TestTree
requestDescribeOrganizationConfigRuleStatuses =
  req
    "DescribeOrganizationConfigRuleStatuses"
    "fixture/DescribeOrganizationConfigRuleStatuses.yaml"

requestSelectResourceConfig :: SelectResourceConfig -> TestTree
requestSelectResourceConfig =
  req
    "SelectResourceConfig"
    "fixture/SelectResourceConfig.yaml"

requestDeleteStoredQuery :: DeleteStoredQuery -> TestTree
requestDeleteStoredQuery =
  req
    "DeleteStoredQuery"
    "fixture/DeleteStoredQuery.yaml"

requestGetComplianceDetailsByResource :: GetComplianceDetailsByResource -> TestTree
requestGetComplianceDetailsByResource =
  req
    "GetComplianceDetailsByResource"
    "fixture/GetComplianceDetailsByResource.yaml"

requestListStoredQueries :: ListStoredQueries -> TestTree
requestListStoredQueries =
  req
    "ListStoredQueries"
    "fixture/ListStoredQueries.yaml"

requestDescribeAggregateComplianceByConformancePacks :: DescribeAggregateComplianceByConformancePacks -> TestTree
requestDescribeAggregateComplianceByConformancePacks =
  req
    "DescribeAggregateComplianceByConformancePacks"
    "fixture/DescribeAggregateComplianceByConformancePacks.yaml"

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

requestPutRetentionConfiguration :: PutRetentionConfiguration -> TestTree
requestPutRetentionConfiguration =
  req
    "PutRetentionConfiguration"
    "fixture/PutRetentionConfiguration.yaml"

requestPutConformancePack :: PutConformancePack -> TestTree
requestPutConformancePack =
  req
    "PutConformancePack"
    "fixture/PutConformancePack.yaml"

requestGetConformancePackComplianceDetails :: GetConformancePackComplianceDetails -> TestTree
requestGetConformancePackComplianceDetails =
  req
    "GetConformancePackComplianceDetails"
    "fixture/GetConformancePackComplianceDetails.yaml"

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

requestDeleteRemediationExceptions :: DeleteRemediationExceptions -> TestTree
requestDeleteRemediationExceptions =
  req
    "DeleteRemediationExceptions"
    "fixture/DeleteRemediationExceptions.yaml"

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

requestSelectAggregateResourceConfig :: SelectAggregateResourceConfig -> TestTree
requestSelectAggregateResourceConfig =
  req
    "SelectAggregateResourceConfig"
    "fixture/SelectAggregateResourceConfig.yaml"

requestDescribeDeliveryChannels :: DescribeDeliveryChannels -> TestTree
requestDescribeDeliveryChannels =
  req
    "DescribeDeliveryChannels"
    "fixture/DescribeDeliveryChannels.yaml"

requestPutResourceConfig :: PutResourceConfig -> TestTree
requestPutResourceConfig =
  req
    "PutResourceConfig"
    "fixture/PutResourceConfig.yaml"

requestDescribeOrganizationConfigRules :: DescribeOrganizationConfigRules -> TestTree
requestDescribeOrganizationConfigRules =
  req
    "DescribeOrganizationConfigRules"
    "fixture/DescribeOrganizationConfigRules.yaml"

requestDeleteDeliveryChannel :: DeleteDeliveryChannel -> TestTree
requestDeleteDeliveryChannel =
  req
    "DeleteDeliveryChannel"
    "fixture/DeleteDeliveryChannel.yaml"

requestPutOrganizationConformancePack :: PutOrganizationConformancePack -> TestTree
requestPutOrganizationConformancePack =
  req
    "PutOrganizationConformancePack"
    "fixture/PutOrganizationConformancePack.yaml"

requestPutAggregationAuthorization :: PutAggregationAuthorization -> TestTree
requestPutAggregationAuthorization =
  req
    "PutAggregationAuthorization"
    "fixture/PutAggregationAuthorization.yaml"

requestDeleteOrganizationConfigRule :: DeleteOrganizationConfigRule -> TestTree
requestDeleteOrganizationConfigRule =
  req
    "DeleteOrganizationConfigRule"
    "fixture/DeleteOrganizationConfigRule.yaml"

requestDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatus -> TestTree
requestDescribeDeliveryChannelStatus =
  req
    "DescribeDeliveryChannelStatus"
    "fixture/DescribeDeliveryChannelStatus.yaml"

requestBatchGetAggregateResourceConfig :: BatchGetAggregateResourceConfig -> TestTree
requestBatchGetAggregateResourceConfig =
  req
    "BatchGetAggregateResourceConfig"
    "fixture/BatchGetAggregateResourceConfig.yaml"

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

requestPutConfigurationAggregator :: PutConfigurationAggregator -> TestTree
requestPutConfigurationAggregator =
  req
    "PutConfigurationAggregator"
    "fixture/PutConfigurationAggregator.yaml"

requestPutStoredQuery :: PutStoredQuery -> TestTree
requestPutStoredQuery =
  req
    "PutStoredQuery"
    "fixture/PutStoredQuery.yaml"

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

requestGetOrganizationConfigRuleDetailedStatus :: GetOrganizationConfigRuleDetailedStatus -> TestTree
requestGetOrganizationConfigRuleDetailedStatus =
  req
    "GetOrganizationConfigRuleDetailedStatus"
    "fixture/GetOrganizationConfigRuleDetailedStatus.yaml"

requestDescribeAggregateComplianceByConfigRules :: DescribeAggregateComplianceByConfigRules -> TestTree
requestDescribeAggregateComplianceByConfigRules =
  req
    "DescribeAggregateComplianceByConfigRules"
    "fixture/DescribeAggregateComplianceByConfigRules.yaml"

requestDeleteConfigRule :: DeleteConfigRule -> TestTree
requestDeleteConfigRule =
  req
    "DeleteConfigRule"
    "fixture/DeleteConfigRule.yaml"

requestDescribeConformancePackStatus :: DescribeConformancePackStatus -> TestTree
requestDescribeConformancePackStatus =
  req
    "DescribeConformancePackStatus"
    "fixture/DescribeConformancePackStatus.yaml"

requestDeleteConformancePack :: DeleteConformancePack -> TestTree
requestDeleteConformancePack =
  req
    "DeleteConformancePack"
    "fixture/DeleteConformancePack.yaml"

requestStartRemediationExecution :: StartRemediationExecution -> TestTree
requestStartRemediationExecution =
  req
    "StartRemediationExecution"
    "fixture/StartRemediationExecution.yaml"

requestGetOrganizationConformancePackDetailedStatus :: GetOrganizationConformancePackDetailedStatus -> TestTree
requestGetOrganizationConformancePackDetailedStatus =
  req
    "GetOrganizationConformancePackDetailedStatus"
    "fixture/GetOrganizationConformancePackDetailedStatus.yaml"

requestDeleteRetentionConfiguration :: DeleteRetentionConfiguration -> TestTree
requestDeleteRetentionConfiguration =
  req
    "DeleteRetentionConfiguration"
    "fixture/DeleteRetentionConfiguration.yaml"

requestGetAggregateConformancePackComplianceSummary :: GetAggregateConformancePackComplianceSummary -> TestTree
requestGetAggregateConformancePackComplianceSummary =
  req
    "GetAggregateConformancePackComplianceSummary"
    "fixture/GetAggregateConformancePackComplianceSummary.yaml"

requestPutRemediationExceptions :: PutRemediationExceptions -> TestTree
requestPutRemediationExceptions =
  req
    "PutRemediationExceptions"
    "fixture/PutRemediationExceptions.yaml"

requestStopConfigurationRecorder :: StopConfigurationRecorder -> TestTree
requestStopConfigurationRecorder =
  req
    "StopConfigurationRecorder"
    "fixture/StopConfigurationRecorder.yaml"

requestDescribeConfigRules :: DescribeConfigRules -> TestTree
requestDescribeConfigRules =
  req
    "DescribeConfigRules"
    "fixture/DescribeConfigRules.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeRetentionConfigurations :: DescribeRetentionConfigurations -> TestTree
requestDescribeRetentionConfigurations =
  req
    "DescribeRetentionConfigurations"
    "fixture/DescribeRetentionConfigurations.yaml"

requestStartConfigurationRecorder :: StartConfigurationRecorder -> TestTree
requestStartConfigurationRecorder =
  req
    "StartConfigurationRecorder"
    "fixture/StartConfigurationRecorder.yaml"

requestGetAggregateConfigRuleComplianceSummary :: GetAggregateConfigRuleComplianceSummary -> TestTree
requestGetAggregateConfigRuleComplianceSummary =
  req
    "GetAggregateConfigRuleComplianceSummary"
    "fixture/GetAggregateConfigRuleComplianceSummary.yaml"

requestDescribeConformancePacks :: DescribeConformancePacks -> TestTree
requestDescribeConformancePacks =
  req
    "DescribeConformancePacks"
    "fixture/DescribeConformancePacks.yaml"

-- Responses

responseDescribeComplianceByConfigRule :: DescribeComplianceByConfigRuleResponse -> TestTree
responseDescribeComplianceByConfigRule =
  res
    "DescribeComplianceByConfigRuleResponse"
    "fixture/DescribeComplianceByConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeComplianceByConfigRule)

responseGetAggregateResourceConfig :: GetAggregateResourceConfigResponse -> TestTree
responseGetAggregateResourceConfig =
  res
    "GetAggregateResourceConfigResponse"
    "fixture/GetAggregateResourceConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetAggregateResourceConfig)

responseGetStoredQuery :: GetStoredQueryResponse -> TestTree
responseGetStoredQuery =
  res
    "GetStoredQueryResponse"
    "fixture/GetStoredQueryResponse.proto"
    defaultService
    (Proxy :: Proxy GetStoredQuery)

responseDescribeConfigurationAggregators :: DescribeConfigurationAggregatorsResponse -> TestTree
responseDescribeConfigurationAggregators =
  res
    "DescribeConfigurationAggregatorsResponse"
    "fixture/DescribeConfigurationAggregatorsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationAggregators)

responseGetAggregateComplianceDetailsByConfigRule :: GetAggregateComplianceDetailsByConfigRuleResponse -> TestTree
responseGetAggregateComplianceDetailsByConfigRule =
  res
    "GetAggregateComplianceDetailsByConfigRuleResponse"
    "fixture/GetAggregateComplianceDetailsByConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetAggregateComplianceDetailsByConfigRule)

responseGetResourceConfigHistory :: GetResourceConfigHistoryResponse -> TestTree
responseGetResourceConfigHistory =
  res
    "GetResourceConfigHistoryResponse"
    "fixture/GetResourceConfigHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourceConfigHistory)

responseDescribeRemediationExecutionStatus :: DescribeRemediationExecutionStatusResponse -> TestTree
responseDescribeRemediationExecutionStatus =
  res
    "DescribeRemediationExecutionStatusResponse"
    "fixture/DescribeRemediationExecutionStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRemediationExecutionStatus)

responseDescribePendingAggregationRequests :: DescribePendingAggregationRequestsResponse -> TestTree
responseDescribePendingAggregationRequests =
  res
    "DescribePendingAggregationRequestsResponse"
    "fixture/DescribePendingAggregationRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePendingAggregationRequests)

responseDescribeConformancePackCompliance :: DescribeConformancePackComplianceResponse -> TestTree
responseDescribeConformancePackCompliance =
  res
    "DescribeConformancePackComplianceResponse"
    "fixture/DescribeConformancePackComplianceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConformancePackCompliance)

responseStartConfigRulesEvaluation :: StartConfigRulesEvaluationResponse -> TestTree
responseStartConfigRulesEvaluation =
  res
    "StartConfigRulesEvaluationResponse"
    "fixture/StartConfigRulesEvaluationResponse.proto"
    defaultService
    (Proxy :: Proxy StartConfigRulesEvaluation)

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

responseDescribeComplianceByResource :: DescribeComplianceByResourceResponse -> TestTree
responseDescribeComplianceByResource =
  res
    "DescribeComplianceByResourceResponse"
    "fixture/DescribeComplianceByResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeComplianceByResource)

responseDescribeOrganizationConformancePacks :: DescribeOrganizationConformancePacksResponse -> TestTree
responseDescribeOrganizationConformancePacks =
  res
    "DescribeOrganizationConformancePacksResponse"
    "fixture/DescribeOrganizationConformancePacksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrganizationConformancePacks)

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

responseDescribeConfigurationAggregatorSourcesStatus :: DescribeConfigurationAggregatorSourcesStatusResponse -> TestTree
responseDescribeConfigurationAggregatorSourcesStatus =
  res
    "DescribeConfigurationAggregatorSourcesStatusResponse"
    "fixture/DescribeConfigurationAggregatorSourcesStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationAggregatorSourcesStatus)

responseDeleteOrganizationConformancePack :: DeleteOrganizationConformancePackResponse -> TestTree
responseDeleteOrganizationConformancePack =
  res
    "DeleteOrganizationConformancePackResponse"
    "fixture/DeleteOrganizationConformancePackResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOrganizationConformancePack)

responseDeleteAggregationAuthorization :: DeleteAggregationAuthorizationResponse -> TestTree
responseDeleteAggregationAuthorization =
  res
    "DeleteAggregationAuthorizationResponse"
    "fixture/DeleteAggregationAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAggregationAuthorization)

responseDescribeRemediationExceptions :: DescribeRemediationExceptionsResponse -> TestTree
responseDescribeRemediationExceptions =
  res
    "DescribeRemediationExceptionsResponse"
    "fixture/DescribeRemediationExceptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRemediationExceptions)

responseDeleteRemediationConfiguration :: DeleteRemediationConfigurationResponse -> TestTree
responseDeleteRemediationConfiguration =
  res
    "DeleteRemediationConfigurationResponse"
    "fixture/DeleteRemediationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRemediationConfiguration)

responseGetComplianceSummaryByResourceType :: GetComplianceSummaryByResourceTypeResponse -> TestTree
responseGetComplianceSummaryByResourceType =
  res
    "GetComplianceSummaryByResourceTypeResponse"
    "fixture/GetComplianceSummaryByResourceTypeResponse.proto"
    defaultService
    (Proxy :: Proxy GetComplianceSummaryByResourceType)

responseGetComplianceDetailsByConfigRule :: GetComplianceDetailsByConfigRuleResponse -> TestTree
responseGetComplianceDetailsByConfigRule =
  res
    "GetComplianceDetailsByConfigRuleResponse"
    "fixture/GetComplianceDetailsByConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetComplianceDetailsByConfigRule)

responseGetDiscoveredResourceCounts :: GetDiscoveredResourceCountsResponse -> TestTree
responseGetDiscoveredResourceCounts =
  res
    "GetDiscoveredResourceCountsResponse"
    "fixture/GetDiscoveredResourceCountsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDiscoveredResourceCounts)

responsePutDeliveryChannel :: PutDeliveryChannelResponse -> TestTree
responsePutDeliveryChannel =
  res
    "PutDeliveryChannelResponse"
    "fixture/PutDeliveryChannelResponse.proto"
    defaultService
    (Proxy :: Proxy PutDeliveryChannel)

responsePutOrganizationConfigRule :: PutOrganizationConfigRuleResponse -> TestTree
responsePutOrganizationConfigRule =
  res
    "PutOrganizationConfigRuleResponse"
    "fixture/PutOrganizationConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy PutOrganizationConfigRule)

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

responseDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatusResponse -> TestTree
responseDescribeConfigurationRecorderStatus =
  res
    "DescribeConfigurationRecorderStatusResponse"
    "fixture/DescribeConfigurationRecorderStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationRecorderStatus)

responseDescribeConfigRuleEvaluationStatus :: DescribeConfigRuleEvaluationStatusResponse -> TestTree
responseDescribeConfigRuleEvaluationStatus =
  res
    "DescribeConfigRuleEvaluationStatusResponse"
    "fixture/DescribeConfigRuleEvaluationStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigRuleEvaluationStatus)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteConfigurationAggregator :: DeleteConfigurationAggregatorResponse -> TestTree
responseDeleteConfigurationAggregator =
  res
    "DeleteConfigurationAggregatorResponse"
    "fixture/DeleteConfigurationAggregatorResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationAggregator)

responseListAggregateDiscoveredResources :: ListAggregateDiscoveredResourcesResponse -> TestTree
responseListAggregateDiscoveredResources =
  res
    "ListAggregateDiscoveredResourcesResponse"
    "fixture/ListAggregateDiscoveredResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAggregateDiscoveredResources)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDescribeOrganizationConfigRuleStatuses :: DescribeOrganizationConfigRuleStatusesResponse -> TestTree
responseDescribeOrganizationConfigRuleStatuses =
  res
    "DescribeOrganizationConfigRuleStatusesResponse"
    "fixture/DescribeOrganizationConfigRuleStatusesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrganizationConfigRuleStatuses)

responseSelectResourceConfig :: SelectResourceConfigResponse -> TestTree
responseSelectResourceConfig =
  res
    "SelectResourceConfigResponse"
    "fixture/SelectResourceConfigResponse.proto"
    defaultService
    (Proxy :: Proxy SelectResourceConfig)

responseDeleteStoredQuery :: DeleteStoredQueryResponse -> TestTree
responseDeleteStoredQuery =
  res
    "DeleteStoredQueryResponse"
    "fixture/DeleteStoredQueryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStoredQuery)

responseGetComplianceDetailsByResource :: GetComplianceDetailsByResourceResponse -> TestTree
responseGetComplianceDetailsByResource =
  res
    "GetComplianceDetailsByResourceResponse"
    "fixture/GetComplianceDetailsByResourceResponse.proto"
    defaultService
    (Proxy :: Proxy GetComplianceDetailsByResource)

responseListStoredQueries :: ListStoredQueriesResponse -> TestTree
responseListStoredQueries =
  res
    "ListStoredQueriesResponse"
    "fixture/ListStoredQueriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListStoredQueries)

responseDescribeAggregateComplianceByConformancePacks :: DescribeAggregateComplianceByConformancePacksResponse -> TestTree
responseDescribeAggregateComplianceByConformancePacks =
  res
    "DescribeAggregateComplianceByConformancePacksResponse"
    "fixture/DescribeAggregateComplianceByConformancePacksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAggregateComplianceByConformancePacks)

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

responsePutRetentionConfiguration :: PutRetentionConfigurationResponse -> TestTree
responsePutRetentionConfiguration =
  res
    "PutRetentionConfigurationResponse"
    "fixture/PutRetentionConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutRetentionConfiguration)

responsePutConformancePack :: PutConformancePackResponse -> TestTree
responsePutConformancePack =
  res
    "PutConformancePackResponse"
    "fixture/PutConformancePackResponse.proto"
    defaultService
    (Proxy :: Proxy PutConformancePack)

responseGetConformancePackComplianceDetails :: GetConformancePackComplianceDetailsResponse -> TestTree
responseGetConformancePackComplianceDetails =
  res
    "GetConformancePackComplianceDetailsResponse"
    "fixture/GetConformancePackComplianceDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetConformancePackComplianceDetails)

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

responseDeleteRemediationExceptions :: DeleteRemediationExceptionsResponse -> TestTree
responseDeleteRemediationExceptions =
  res
    "DeleteRemediationExceptionsResponse"
    "fixture/DeleteRemediationExceptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRemediationExceptions)

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

responseSelectAggregateResourceConfig :: SelectAggregateResourceConfigResponse -> TestTree
responseSelectAggregateResourceConfig =
  res
    "SelectAggregateResourceConfigResponse"
    "fixture/SelectAggregateResourceConfigResponse.proto"
    defaultService
    (Proxy :: Proxy SelectAggregateResourceConfig)

responseDescribeDeliveryChannels :: DescribeDeliveryChannelsResponse -> TestTree
responseDescribeDeliveryChannels =
  res
    "DescribeDeliveryChannelsResponse"
    "fixture/DescribeDeliveryChannelsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDeliveryChannels)

responsePutResourceConfig :: PutResourceConfigResponse -> TestTree
responsePutResourceConfig =
  res
    "PutResourceConfigResponse"
    "fixture/PutResourceConfigResponse.proto"
    defaultService
    (Proxy :: Proxy PutResourceConfig)

responseDescribeOrganizationConfigRules :: DescribeOrganizationConfigRulesResponse -> TestTree
responseDescribeOrganizationConfigRules =
  res
    "DescribeOrganizationConfigRulesResponse"
    "fixture/DescribeOrganizationConfigRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrganizationConfigRules)

responseDeleteDeliveryChannel :: DeleteDeliveryChannelResponse -> TestTree
responseDeleteDeliveryChannel =
  res
    "DeleteDeliveryChannelResponse"
    "fixture/DeleteDeliveryChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeliveryChannel)

responsePutOrganizationConformancePack :: PutOrganizationConformancePackResponse -> TestTree
responsePutOrganizationConformancePack =
  res
    "PutOrganizationConformancePackResponse"
    "fixture/PutOrganizationConformancePackResponse.proto"
    defaultService
    (Proxy :: Proxy PutOrganizationConformancePack)

responsePutAggregationAuthorization :: PutAggregationAuthorizationResponse -> TestTree
responsePutAggregationAuthorization =
  res
    "PutAggregationAuthorizationResponse"
    "fixture/PutAggregationAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy PutAggregationAuthorization)

responseDeleteOrganizationConfigRule :: DeleteOrganizationConfigRuleResponse -> TestTree
responseDeleteOrganizationConfigRule =
  res
    "DeleteOrganizationConfigRuleResponse"
    "fixture/DeleteOrganizationConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOrganizationConfigRule)

responseDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatusResponse -> TestTree
responseDescribeDeliveryChannelStatus =
  res
    "DescribeDeliveryChannelStatusResponse"
    "fixture/DescribeDeliveryChannelStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDeliveryChannelStatus)

responseBatchGetAggregateResourceConfig :: BatchGetAggregateResourceConfigResponse -> TestTree
responseBatchGetAggregateResourceConfig =
  res
    "BatchGetAggregateResourceConfigResponse"
    "fixture/BatchGetAggregateResourceConfigResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetAggregateResourceConfig)

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

responsePutConfigurationAggregator :: PutConfigurationAggregatorResponse -> TestTree
responsePutConfigurationAggregator =
  res
    "PutConfigurationAggregatorResponse"
    "fixture/PutConfigurationAggregatorResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationAggregator)

responsePutStoredQuery :: PutStoredQueryResponse -> TestTree
responsePutStoredQuery =
  res
    "PutStoredQueryResponse"
    "fixture/PutStoredQueryResponse.proto"
    defaultService
    (Proxy :: Proxy PutStoredQuery)

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

responseGetOrganizationConfigRuleDetailedStatus :: GetOrganizationConfigRuleDetailedStatusResponse -> TestTree
responseGetOrganizationConfigRuleDetailedStatus =
  res
    "GetOrganizationConfigRuleDetailedStatusResponse"
    "fixture/GetOrganizationConfigRuleDetailedStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetOrganizationConfigRuleDetailedStatus)

responseDescribeAggregateComplianceByConfigRules :: DescribeAggregateComplianceByConfigRulesResponse -> TestTree
responseDescribeAggregateComplianceByConfigRules =
  res
    "DescribeAggregateComplianceByConfigRulesResponse"
    "fixture/DescribeAggregateComplianceByConfigRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAggregateComplianceByConfigRules)

responseDeleteConfigRule :: DeleteConfigRuleResponse -> TestTree
responseDeleteConfigRule =
  res
    "DeleteConfigRuleResponse"
    "fixture/DeleteConfigRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigRule)

responseDescribeConformancePackStatus :: DescribeConformancePackStatusResponse -> TestTree
responseDescribeConformancePackStatus =
  res
    "DescribeConformancePackStatusResponse"
    "fixture/DescribeConformancePackStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConformancePackStatus)

responseDeleteConformancePack :: DeleteConformancePackResponse -> TestTree
responseDeleteConformancePack =
  res
    "DeleteConformancePackResponse"
    "fixture/DeleteConformancePackResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConformancePack)

responseStartRemediationExecution :: StartRemediationExecutionResponse -> TestTree
responseStartRemediationExecution =
  res
    "StartRemediationExecutionResponse"
    "fixture/StartRemediationExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartRemediationExecution)

responseGetOrganizationConformancePackDetailedStatus :: GetOrganizationConformancePackDetailedStatusResponse -> TestTree
responseGetOrganizationConformancePackDetailedStatus =
  res
    "GetOrganizationConformancePackDetailedStatusResponse"
    "fixture/GetOrganizationConformancePackDetailedStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetOrganizationConformancePackDetailedStatus)

responseDeleteRetentionConfiguration :: DeleteRetentionConfigurationResponse -> TestTree
responseDeleteRetentionConfiguration =
  res
    "DeleteRetentionConfigurationResponse"
    "fixture/DeleteRetentionConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRetentionConfiguration)

responseGetAggregateConformancePackComplianceSummary :: GetAggregateConformancePackComplianceSummaryResponse -> TestTree
responseGetAggregateConformancePackComplianceSummary =
  res
    "GetAggregateConformancePackComplianceSummaryResponse"
    "fixture/GetAggregateConformancePackComplianceSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy GetAggregateConformancePackComplianceSummary)

responsePutRemediationExceptions :: PutRemediationExceptionsResponse -> TestTree
responsePutRemediationExceptions =
  res
    "PutRemediationExceptionsResponse"
    "fixture/PutRemediationExceptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutRemediationExceptions)

responseStopConfigurationRecorder :: StopConfigurationRecorderResponse -> TestTree
responseStopConfigurationRecorder =
  res
    "StopConfigurationRecorderResponse"
    "fixture/StopConfigurationRecorderResponse.proto"
    defaultService
    (Proxy :: Proxy StopConfigurationRecorder)

responseDescribeConfigRules :: DescribeConfigRulesResponse -> TestTree
responseDescribeConfigRules =
  res
    "DescribeConfigRulesResponse"
    "fixture/DescribeConfigRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigRules)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeRetentionConfigurations :: DescribeRetentionConfigurationsResponse -> TestTree
responseDescribeRetentionConfigurations =
  res
    "DescribeRetentionConfigurationsResponse"
    "fixture/DescribeRetentionConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRetentionConfigurations)

responseStartConfigurationRecorder :: StartConfigurationRecorderResponse -> TestTree
responseStartConfigurationRecorder =
  res
    "StartConfigurationRecorderResponse"
    "fixture/StartConfigurationRecorderResponse.proto"
    defaultService
    (Proxy :: Proxy StartConfigurationRecorder)

responseGetAggregateConfigRuleComplianceSummary :: GetAggregateConfigRuleComplianceSummaryResponse -> TestTree
responseGetAggregateConfigRuleComplianceSummary =
  res
    "GetAggregateConfigRuleComplianceSummaryResponse"
    "fixture/GetAggregateConfigRuleComplianceSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy GetAggregateConfigRuleComplianceSummary)

responseDescribeConformancePacks :: DescribeConformancePacksResponse -> TestTree
responseDescribeConformancePacks =
  res
    "DescribeConformancePacksResponse"
    "fixture/DescribeConformancePacksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConformancePacks)
