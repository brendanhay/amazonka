{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Config
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             describePendingAggregationRequests
--
--         , requestDescribeRemediationExecutionStatus $
--             describeRemediationExecutionStatus
--
--         , requestGetResourceConfigHistory $
--             getResourceConfigHistory
--
--         , requestGetAggregateResourceConfig $
--             getAggregateResourceConfig
--
--         , requestDescribeConfigurationAggregators $
--             describeConfigurationAggregators
--
--         , requestDescribeComplianceByConfigRule $
--             describeComplianceByConfigRule
--
--         , requestDescribeRetentionConfigurations $
--             describeRetentionConfigurations
--
--         , requestStopConfigurationRecorder $
--             stopConfigurationRecorder
--
--         , requestGetAggregateConfigRuleComplianceSummary $
--             getAggregateConfigRuleComplianceSummary
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestBatchGetResourceConfig $
--             batchGetResourceConfig
--
--         , requestDescribeConfigRules $
--             describeConfigRules
--
--         , requestPutRetentionConfiguration $
--             putRetentionConfiguration
--
--         , requestDescribeAggregateComplianceByConfigRules $
--             describeAggregateComplianceByConfigRules
--
--         , requestDeleteEvaluationResults $
--             deleteEvaluationResults
--
--         , requestPutConfigRule $
--             putConfigRule
--
--         , requestDeleteConfigRule $
--             deleteConfigRule
--
--         , requestDeleteRetentionConfiguration $
--             deleteRetentionConfiguration
--
--         , requestSelectResourceConfig $
--             selectResourceConfig
--
--         , requestListAggregateDiscoveredResources $
--             listAggregateDiscoveredResources
--
--         , requestGetComplianceDetailsByResource $
--             getComplianceDetailsByResource
--
--         , requestDeletePendingAggregationRequest $
--             deletePendingAggregationRequest
--
--         , requestDeliverConfigSnapshot $
--             deliverConfigSnapshot
--
--         , requestBatchGetAggregateResourceConfig $
--             batchGetAggregateResourceConfig
--
--         , requestDescribeConfigRuleEvaluationStatus $
--             describeConfigRuleEvaluationStatus
--
--         , requestGetDiscoveredResourceCounts $
--             getDiscoveredResourceCounts
--
--         , requestStartConfigRulesEvaluation $
--             startConfigRulesEvaluation
--
--         , requestDescribeComplianceByResource $
--             describeComplianceByResource
--
--         , requestPutEvaluations $
--             putEvaluations
--
--         , requestDescribeConfigurationRecorders $
--             describeConfigurationRecorders
--
--         , requestGetAggregateComplianceDetailsByConfigRule $
--             getAggregateComplianceDetailsByConfigRule
--
--         , requestGetAggregateDiscoveredResourceCounts $
--             getAggregateDiscoveredResourceCounts
--
--         , requestStartConfigurationRecorder $
--             startConfigurationRecorder
--
--         , requestPutRemediationConfigurations $
--             putRemediationConfigurations
--
--         , requestStartRemediationExecution $
--             startRemediationExecution
--
--         , requestGetComplianceSummaryByConfigRule $
--             getComplianceSummaryByConfigRule
--
--         , requestPutConfigurationAggregator $
--             putConfigurationAggregator
--
--         , requestTagResource $
--             tagResource
--
--         , requestDeleteConfigurationAggregator $
--             deleteConfigurationAggregator
--
--         , requestDescribeConfigurationRecorderStatus $
--             describeConfigurationRecorderStatus
--
--         , requestPutConfigurationRecorder $
--             putConfigurationRecorder
--
--         , requestUntagResource $
--             untagResource
--
--         , requestDeleteConfigurationRecorder $
--             deleteConfigurationRecorder
--
--         , requestGetComplianceSummaryByResourceType $
--             getComplianceSummaryByResourceType
--
--         , requestDescribeDeliveryChannelStatus $
--             describeDeliveryChannelStatus
--
--         , requestPutDeliveryChannel $
--             putDeliveryChannel
--
--         , requestGetComplianceDetailsByConfigRule $
--             getComplianceDetailsByConfigRule
--
--         , requestDeleteAggregationAuthorization $
--             deleteAggregationAuthorization
--
--         , requestDeleteDeliveryChannel $
--             deleteDeliveryChannel
--
--         , requestDeleteRemediationConfiguration $
--             deleteRemediationConfiguration
--
--         , requestPutAggregationAuthorization $
--             putAggregationAuthorization
--
--         , requestDescribeConfigurationAggregatorSourcesStatus $
--             describeConfigurationAggregatorSourcesStatus
--
--         , requestListDiscoveredResources $
--             listDiscoveredResources
--
--         , requestDescribeRemediationConfigurations $
--             describeRemediationConfigurations
--
--         , requestDescribeDeliveryChannels $
--             describeDeliveryChannels
--
--         , requestDescribeAggregationAuthorizations $
--             describeAggregationAuthorizations
--
--           ]

--     , testGroup "response"
--         [ responseDescribePendingAggregationRequests $
--             describePendingAggregationRequestsResponse
--
--         , responseDescribeRemediationExecutionStatus $
--             describeRemediationExecutionStatusResponse
--
--         , responseGetResourceConfigHistory $
--             getResourceConfigHistoryResponse
--
--         , responseGetAggregateResourceConfig $
--             getAggregateResourceConfigResponse
--
--         , responseDescribeConfigurationAggregators $
--             describeConfigurationAggregatorsResponse
--
--         , responseDescribeComplianceByConfigRule $
--             describeComplianceByConfigRuleResponse
--
--         , responseDescribeRetentionConfigurations $
--             describeRetentionConfigurationsResponse
--
--         , responseStopConfigurationRecorder $
--             stopConfigurationRecorderResponse
--
--         , responseGetAggregateConfigRuleComplianceSummary $
--             getAggregateConfigRuleComplianceSummaryResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseBatchGetResourceConfig $
--             batchGetResourceConfigResponse
--
--         , responseDescribeConfigRules $
--             describeConfigRulesResponse
--
--         , responsePutRetentionConfiguration $
--             putRetentionConfigurationResponse
--
--         , responseDescribeAggregateComplianceByConfigRules $
--             describeAggregateComplianceByConfigRulesResponse
--
--         , responseDeleteEvaluationResults $
--             deleteEvaluationResultsResponse
--
--         , responsePutConfigRule $
--             putConfigRuleResponse
--
--         , responseDeleteConfigRule $
--             deleteConfigRuleResponse
--
--         , responseDeleteRetentionConfiguration $
--             deleteRetentionConfigurationResponse
--
--         , responseSelectResourceConfig $
--             selectResourceConfigResponse
--
--         , responseListAggregateDiscoveredResources $
--             listAggregateDiscoveredResourcesResponse
--
--         , responseGetComplianceDetailsByResource $
--             getComplianceDetailsByResourceResponse
--
--         , responseDeletePendingAggregationRequest $
--             deletePendingAggregationRequestResponse
--
--         , responseDeliverConfigSnapshot $
--             deliverConfigSnapshotResponse
--
--         , responseBatchGetAggregateResourceConfig $
--             batchGetAggregateResourceConfigResponse
--
--         , responseDescribeConfigRuleEvaluationStatus $
--             describeConfigRuleEvaluationStatusResponse
--
--         , responseGetDiscoveredResourceCounts $
--             getDiscoveredResourceCountsResponse
--
--         , responseStartConfigRulesEvaluation $
--             startConfigRulesEvaluationResponse
--
--         , responseDescribeComplianceByResource $
--             describeComplianceByResourceResponse
--
--         , responsePutEvaluations $
--             putEvaluationsResponse
--
--         , responseDescribeConfigurationRecorders $
--             describeConfigurationRecordersResponse
--
--         , responseGetAggregateComplianceDetailsByConfigRule $
--             getAggregateComplianceDetailsByConfigRuleResponse
--
--         , responseGetAggregateDiscoveredResourceCounts $
--             getAggregateDiscoveredResourceCountsResponse
--
--         , responseStartConfigurationRecorder $
--             startConfigurationRecorderResponse
--
--         , responsePutRemediationConfigurations $
--             putRemediationConfigurationsResponse
--
--         , responseStartRemediationExecution $
--             startRemediationExecutionResponse
--
--         , responseGetComplianceSummaryByConfigRule $
--             getComplianceSummaryByConfigRuleResponse
--
--         , responsePutConfigurationAggregator $
--             putConfigurationAggregatorResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseDeleteConfigurationAggregator $
--             deleteConfigurationAggregatorResponse
--
--         , responseDescribeConfigurationRecorderStatus $
--             describeConfigurationRecorderStatusResponse
--
--         , responsePutConfigurationRecorder $
--             putConfigurationRecorderResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseDeleteConfigurationRecorder $
--             deleteConfigurationRecorderResponse
--
--         , responseGetComplianceSummaryByResourceType $
--             getComplianceSummaryByResourceTypeResponse
--
--         , responseDescribeDeliveryChannelStatus $
--             describeDeliveryChannelStatusResponse
--
--         , responsePutDeliveryChannel $
--             putDeliveryChannelResponse
--
--         , responseGetComplianceDetailsByConfigRule $
--             getComplianceDetailsByConfigRuleResponse
--
--         , responseDeleteAggregationAuthorization $
--             deleteAggregationAuthorizationResponse
--
--         , responseDeleteDeliveryChannel $
--             deleteDeliveryChannelResponse
--
--         , responseDeleteRemediationConfiguration $
--             deleteRemediationConfigurationResponse
--
--         , responsePutAggregationAuthorization $
--             putAggregationAuthorizationResponse
--
--         , responseDescribeConfigurationAggregatorSourcesStatus $
--             describeConfigurationAggregatorSourcesStatusResponse
--
--         , responseListDiscoveredResources $
--             listDiscoveredResourcesResponse
--
--         , responseDescribeRemediationConfigurations $
--             describeRemediationConfigurationsResponse
--
--         , responseDescribeDeliveryChannels $
--             describeDeliveryChannelsResponse
--
--         , responseDescribeAggregationAuthorizations $
--             describeAggregationAuthorizationsResponse
--
--           ]
--     ]

-- Requests

requestDescribePendingAggregationRequests :: DescribePendingAggregationRequests -> TestTree
requestDescribePendingAggregationRequests = req
    "DescribePendingAggregationRequests"
    "fixture/DescribePendingAggregationRequests.yaml"

requestDescribeRemediationExecutionStatus :: DescribeRemediationExecutionStatus -> TestTree
requestDescribeRemediationExecutionStatus = req
    "DescribeRemediationExecutionStatus"
    "fixture/DescribeRemediationExecutionStatus.yaml"

requestGetResourceConfigHistory :: GetResourceConfigHistory -> TestTree
requestGetResourceConfigHistory = req
    "GetResourceConfigHistory"
    "fixture/GetResourceConfigHistory.yaml"

requestGetAggregateResourceConfig :: GetAggregateResourceConfig -> TestTree
requestGetAggregateResourceConfig = req
    "GetAggregateResourceConfig"
    "fixture/GetAggregateResourceConfig.yaml"

requestDescribeConfigurationAggregators :: DescribeConfigurationAggregators -> TestTree
requestDescribeConfigurationAggregators = req
    "DescribeConfigurationAggregators"
    "fixture/DescribeConfigurationAggregators.yaml"

requestDescribeComplianceByConfigRule :: DescribeComplianceByConfigRule -> TestTree
requestDescribeComplianceByConfigRule = req
    "DescribeComplianceByConfigRule"
    "fixture/DescribeComplianceByConfigRule.yaml"

requestDescribeRetentionConfigurations :: DescribeRetentionConfigurations -> TestTree
requestDescribeRetentionConfigurations = req
    "DescribeRetentionConfigurations"
    "fixture/DescribeRetentionConfigurations.yaml"

requestStopConfigurationRecorder :: StopConfigurationRecorder -> TestTree
requestStopConfigurationRecorder = req
    "StopConfigurationRecorder"
    "fixture/StopConfigurationRecorder.yaml"

requestGetAggregateConfigRuleComplianceSummary :: GetAggregateConfigRuleComplianceSummary -> TestTree
requestGetAggregateConfigRuleComplianceSummary = req
    "GetAggregateConfigRuleComplianceSummary"
    "fixture/GetAggregateConfigRuleComplianceSummary.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestBatchGetResourceConfig :: BatchGetResourceConfig -> TestTree
requestBatchGetResourceConfig = req
    "BatchGetResourceConfig"
    "fixture/BatchGetResourceConfig.yaml"

requestDescribeConfigRules :: DescribeConfigRules -> TestTree
requestDescribeConfigRules = req
    "DescribeConfigRules"
    "fixture/DescribeConfigRules.yaml"

requestPutRetentionConfiguration :: PutRetentionConfiguration -> TestTree
requestPutRetentionConfiguration = req
    "PutRetentionConfiguration"
    "fixture/PutRetentionConfiguration.yaml"

requestDescribeAggregateComplianceByConfigRules :: DescribeAggregateComplianceByConfigRules -> TestTree
requestDescribeAggregateComplianceByConfigRules = req
    "DescribeAggregateComplianceByConfigRules"
    "fixture/DescribeAggregateComplianceByConfigRules.yaml"

requestDeleteEvaluationResults :: DeleteEvaluationResults -> TestTree
requestDeleteEvaluationResults = req
    "DeleteEvaluationResults"
    "fixture/DeleteEvaluationResults.yaml"

requestPutConfigRule :: PutConfigRule -> TestTree
requestPutConfigRule = req
    "PutConfigRule"
    "fixture/PutConfigRule.yaml"

requestDeleteConfigRule :: DeleteConfigRule -> TestTree
requestDeleteConfigRule = req
    "DeleteConfigRule"
    "fixture/DeleteConfigRule.yaml"

requestDeleteRetentionConfiguration :: DeleteRetentionConfiguration -> TestTree
requestDeleteRetentionConfiguration = req
    "DeleteRetentionConfiguration"
    "fixture/DeleteRetentionConfiguration.yaml"

requestSelectResourceConfig :: SelectResourceConfig -> TestTree
requestSelectResourceConfig = req
    "SelectResourceConfig"
    "fixture/SelectResourceConfig.yaml"

requestListAggregateDiscoveredResources :: ListAggregateDiscoveredResources -> TestTree
requestListAggregateDiscoveredResources = req
    "ListAggregateDiscoveredResources"
    "fixture/ListAggregateDiscoveredResources.yaml"

requestGetComplianceDetailsByResource :: GetComplianceDetailsByResource -> TestTree
requestGetComplianceDetailsByResource = req
    "GetComplianceDetailsByResource"
    "fixture/GetComplianceDetailsByResource.yaml"

requestDeletePendingAggregationRequest :: DeletePendingAggregationRequest -> TestTree
requestDeletePendingAggregationRequest = req
    "DeletePendingAggregationRequest"
    "fixture/DeletePendingAggregationRequest.yaml"

requestDeliverConfigSnapshot :: DeliverConfigSnapshot -> TestTree
requestDeliverConfigSnapshot = req
    "DeliverConfigSnapshot"
    "fixture/DeliverConfigSnapshot.yaml"

requestBatchGetAggregateResourceConfig :: BatchGetAggregateResourceConfig -> TestTree
requestBatchGetAggregateResourceConfig = req
    "BatchGetAggregateResourceConfig"
    "fixture/BatchGetAggregateResourceConfig.yaml"

requestDescribeConfigRuleEvaluationStatus :: DescribeConfigRuleEvaluationStatus -> TestTree
requestDescribeConfigRuleEvaluationStatus = req
    "DescribeConfigRuleEvaluationStatus"
    "fixture/DescribeConfigRuleEvaluationStatus.yaml"

requestGetDiscoveredResourceCounts :: GetDiscoveredResourceCounts -> TestTree
requestGetDiscoveredResourceCounts = req
    "GetDiscoveredResourceCounts"
    "fixture/GetDiscoveredResourceCounts.yaml"

requestStartConfigRulesEvaluation :: StartConfigRulesEvaluation -> TestTree
requestStartConfigRulesEvaluation = req
    "StartConfigRulesEvaluation"
    "fixture/StartConfigRulesEvaluation.yaml"

requestDescribeComplianceByResource :: DescribeComplianceByResource -> TestTree
requestDescribeComplianceByResource = req
    "DescribeComplianceByResource"
    "fixture/DescribeComplianceByResource.yaml"

requestPutEvaluations :: PutEvaluations -> TestTree
requestPutEvaluations = req
    "PutEvaluations"
    "fixture/PutEvaluations.yaml"

requestDescribeConfigurationRecorders :: DescribeConfigurationRecorders -> TestTree
requestDescribeConfigurationRecorders = req
    "DescribeConfigurationRecorders"
    "fixture/DescribeConfigurationRecorders.yaml"

requestGetAggregateComplianceDetailsByConfigRule :: GetAggregateComplianceDetailsByConfigRule -> TestTree
requestGetAggregateComplianceDetailsByConfigRule = req
    "GetAggregateComplianceDetailsByConfigRule"
    "fixture/GetAggregateComplianceDetailsByConfigRule.yaml"

requestGetAggregateDiscoveredResourceCounts :: GetAggregateDiscoveredResourceCounts -> TestTree
requestGetAggregateDiscoveredResourceCounts = req
    "GetAggregateDiscoveredResourceCounts"
    "fixture/GetAggregateDiscoveredResourceCounts.yaml"

requestStartConfigurationRecorder :: StartConfigurationRecorder -> TestTree
requestStartConfigurationRecorder = req
    "StartConfigurationRecorder"
    "fixture/StartConfigurationRecorder.yaml"

requestPutRemediationConfigurations :: PutRemediationConfigurations -> TestTree
requestPutRemediationConfigurations = req
    "PutRemediationConfigurations"
    "fixture/PutRemediationConfigurations.yaml"

requestStartRemediationExecution :: StartRemediationExecution -> TestTree
requestStartRemediationExecution = req
    "StartRemediationExecution"
    "fixture/StartRemediationExecution.yaml"

requestGetComplianceSummaryByConfigRule :: GetComplianceSummaryByConfigRule -> TestTree
requestGetComplianceSummaryByConfigRule = req
    "GetComplianceSummaryByConfigRule"
    "fixture/GetComplianceSummaryByConfigRule.yaml"

requestPutConfigurationAggregator :: PutConfigurationAggregator -> TestTree
requestPutConfigurationAggregator = req
    "PutConfigurationAggregator"
    "fixture/PutConfigurationAggregator.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteConfigurationAggregator :: DeleteConfigurationAggregator -> TestTree
requestDeleteConfigurationAggregator = req
    "DeleteConfigurationAggregator"
    "fixture/DeleteConfigurationAggregator.yaml"

requestDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatus -> TestTree
requestDescribeConfigurationRecorderStatus = req
    "DescribeConfigurationRecorderStatus"
    "fixture/DescribeConfigurationRecorderStatus.yaml"

requestPutConfigurationRecorder :: PutConfigurationRecorder -> TestTree
requestPutConfigurationRecorder = req
    "PutConfigurationRecorder"
    "fixture/PutConfigurationRecorder.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteConfigurationRecorder :: DeleteConfigurationRecorder -> TestTree
requestDeleteConfigurationRecorder = req
    "DeleteConfigurationRecorder"
    "fixture/DeleteConfigurationRecorder.yaml"

requestGetComplianceSummaryByResourceType :: GetComplianceSummaryByResourceType -> TestTree
requestGetComplianceSummaryByResourceType = req
    "GetComplianceSummaryByResourceType"
    "fixture/GetComplianceSummaryByResourceType.yaml"

requestDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatus -> TestTree
requestDescribeDeliveryChannelStatus = req
    "DescribeDeliveryChannelStatus"
    "fixture/DescribeDeliveryChannelStatus.yaml"

requestPutDeliveryChannel :: PutDeliveryChannel -> TestTree
requestPutDeliveryChannel = req
    "PutDeliveryChannel"
    "fixture/PutDeliveryChannel.yaml"

requestGetComplianceDetailsByConfigRule :: GetComplianceDetailsByConfigRule -> TestTree
requestGetComplianceDetailsByConfigRule = req
    "GetComplianceDetailsByConfigRule"
    "fixture/GetComplianceDetailsByConfigRule.yaml"

requestDeleteAggregationAuthorization :: DeleteAggregationAuthorization -> TestTree
requestDeleteAggregationAuthorization = req
    "DeleteAggregationAuthorization"
    "fixture/DeleteAggregationAuthorization.yaml"

requestDeleteDeliveryChannel :: DeleteDeliveryChannel -> TestTree
requestDeleteDeliveryChannel = req
    "DeleteDeliveryChannel"
    "fixture/DeleteDeliveryChannel.yaml"

requestDeleteRemediationConfiguration :: DeleteRemediationConfiguration -> TestTree
requestDeleteRemediationConfiguration = req
    "DeleteRemediationConfiguration"
    "fixture/DeleteRemediationConfiguration.yaml"

requestPutAggregationAuthorization :: PutAggregationAuthorization -> TestTree
requestPutAggregationAuthorization = req
    "PutAggregationAuthorization"
    "fixture/PutAggregationAuthorization.yaml"

requestDescribeConfigurationAggregatorSourcesStatus :: DescribeConfigurationAggregatorSourcesStatus -> TestTree
requestDescribeConfigurationAggregatorSourcesStatus = req
    "DescribeConfigurationAggregatorSourcesStatus"
    "fixture/DescribeConfigurationAggregatorSourcesStatus.yaml"

requestListDiscoveredResources :: ListDiscoveredResources -> TestTree
requestListDiscoveredResources = req
    "ListDiscoveredResources"
    "fixture/ListDiscoveredResources.yaml"

requestDescribeRemediationConfigurations :: DescribeRemediationConfigurations -> TestTree
requestDescribeRemediationConfigurations = req
    "DescribeRemediationConfigurations"
    "fixture/DescribeRemediationConfigurations.yaml"

requestDescribeDeliveryChannels :: DescribeDeliveryChannels -> TestTree
requestDescribeDeliveryChannels = req
    "DescribeDeliveryChannels"
    "fixture/DescribeDeliveryChannels.yaml"

requestDescribeAggregationAuthorizations :: DescribeAggregationAuthorizations -> TestTree
requestDescribeAggregationAuthorizations = req
    "DescribeAggregationAuthorizations"
    "fixture/DescribeAggregationAuthorizations.yaml"

-- Responses

responseDescribePendingAggregationRequests :: DescribePendingAggregationRequestsResponse -> TestTree
responseDescribePendingAggregationRequests = res
    "DescribePendingAggregationRequestsResponse"
    "fixture/DescribePendingAggregationRequestsResponse.proto"
    config
    (Proxy :: Proxy DescribePendingAggregationRequests)

responseDescribeRemediationExecutionStatus :: DescribeRemediationExecutionStatusResponse -> TestTree
responseDescribeRemediationExecutionStatus = res
    "DescribeRemediationExecutionStatusResponse"
    "fixture/DescribeRemediationExecutionStatusResponse.proto"
    config
    (Proxy :: Proxy DescribeRemediationExecutionStatus)

responseGetResourceConfigHistory :: GetResourceConfigHistoryResponse -> TestTree
responseGetResourceConfigHistory = res
    "GetResourceConfigHistoryResponse"
    "fixture/GetResourceConfigHistoryResponse.proto"
    config
    (Proxy :: Proxy GetResourceConfigHistory)

responseGetAggregateResourceConfig :: GetAggregateResourceConfigResponse -> TestTree
responseGetAggregateResourceConfig = res
    "GetAggregateResourceConfigResponse"
    "fixture/GetAggregateResourceConfigResponse.proto"
    config
    (Proxy :: Proxy GetAggregateResourceConfig)

responseDescribeConfigurationAggregators :: DescribeConfigurationAggregatorsResponse -> TestTree
responseDescribeConfigurationAggregators = res
    "DescribeConfigurationAggregatorsResponse"
    "fixture/DescribeConfigurationAggregatorsResponse.proto"
    config
    (Proxy :: Proxy DescribeConfigurationAggregators)

responseDescribeComplianceByConfigRule :: DescribeComplianceByConfigRuleResponse -> TestTree
responseDescribeComplianceByConfigRule = res
    "DescribeComplianceByConfigRuleResponse"
    "fixture/DescribeComplianceByConfigRuleResponse.proto"
    config
    (Proxy :: Proxy DescribeComplianceByConfigRule)

responseDescribeRetentionConfigurations :: DescribeRetentionConfigurationsResponse -> TestTree
responseDescribeRetentionConfigurations = res
    "DescribeRetentionConfigurationsResponse"
    "fixture/DescribeRetentionConfigurationsResponse.proto"
    config
    (Proxy :: Proxy DescribeRetentionConfigurations)

responseStopConfigurationRecorder :: StopConfigurationRecorderResponse -> TestTree
responseStopConfigurationRecorder = res
    "StopConfigurationRecorderResponse"
    "fixture/StopConfigurationRecorderResponse.proto"
    config
    (Proxy :: Proxy StopConfigurationRecorder)

responseGetAggregateConfigRuleComplianceSummary :: GetAggregateConfigRuleComplianceSummaryResponse -> TestTree
responseGetAggregateConfigRuleComplianceSummary = res
    "GetAggregateConfigRuleComplianceSummaryResponse"
    "fixture/GetAggregateConfigRuleComplianceSummaryResponse.proto"
    config
    (Proxy :: Proxy GetAggregateConfigRuleComplianceSummary)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    config
    (Proxy :: Proxy ListTagsForResource)

responseBatchGetResourceConfig :: BatchGetResourceConfigResponse -> TestTree
responseBatchGetResourceConfig = res
    "BatchGetResourceConfigResponse"
    "fixture/BatchGetResourceConfigResponse.proto"
    config
    (Proxy :: Proxy BatchGetResourceConfig)

responseDescribeConfigRules :: DescribeConfigRulesResponse -> TestTree
responseDescribeConfigRules = res
    "DescribeConfigRulesResponse"
    "fixture/DescribeConfigRulesResponse.proto"
    config
    (Proxy :: Proxy DescribeConfigRules)

responsePutRetentionConfiguration :: PutRetentionConfigurationResponse -> TestTree
responsePutRetentionConfiguration = res
    "PutRetentionConfigurationResponse"
    "fixture/PutRetentionConfigurationResponse.proto"
    config
    (Proxy :: Proxy PutRetentionConfiguration)

responseDescribeAggregateComplianceByConfigRules :: DescribeAggregateComplianceByConfigRulesResponse -> TestTree
responseDescribeAggregateComplianceByConfigRules = res
    "DescribeAggregateComplianceByConfigRulesResponse"
    "fixture/DescribeAggregateComplianceByConfigRulesResponse.proto"
    config
    (Proxy :: Proxy DescribeAggregateComplianceByConfigRules)

responseDeleteEvaluationResults :: DeleteEvaluationResultsResponse -> TestTree
responseDeleteEvaluationResults = res
    "DeleteEvaluationResultsResponse"
    "fixture/DeleteEvaluationResultsResponse.proto"
    config
    (Proxy :: Proxy DeleteEvaluationResults)

responsePutConfigRule :: PutConfigRuleResponse -> TestTree
responsePutConfigRule = res
    "PutConfigRuleResponse"
    "fixture/PutConfigRuleResponse.proto"
    config
    (Proxy :: Proxy PutConfigRule)

responseDeleteConfigRule :: DeleteConfigRuleResponse -> TestTree
responseDeleteConfigRule = res
    "DeleteConfigRuleResponse"
    "fixture/DeleteConfigRuleResponse.proto"
    config
    (Proxy :: Proxy DeleteConfigRule)

responseDeleteRetentionConfiguration :: DeleteRetentionConfigurationResponse -> TestTree
responseDeleteRetentionConfiguration = res
    "DeleteRetentionConfigurationResponse"
    "fixture/DeleteRetentionConfigurationResponse.proto"
    config
    (Proxy :: Proxy DeleteRetentionConfiguration)

responseSelectResourceConfig :: SelectResourceConfigResponse -> TestTree
responseSelectResourceConfig = res
    "SelectResourceConfigResponse"
    "fixture/SelectResourceConfigResponse.proto"
    config
    (Proxy :: Proxy SelectResourceConfig)

responseListAggregateDiscoveredResources :: ListAggregateDiscoveredResourcesResponse -> TestTree
responseListAggregateDiscoveredResources = res
    "ListAggregateDiscoveredResourcesResponse"
    "fixture/ListAggregateDiscoveredResourcesResponse.proto"
    config
    (Proxy :: Proxy ListAggregateDiscoveredResources)

responseGetComplianceDetailsByResource :: GetComplianceDetailsByResourceResponse -> TestTree
responseGetComplianceDetailsByResource = res
    "GetComplianceDetailsByResourceResponse"
    "fixture/GetComplianceDetailsByResourceResponse.proto"
    config
    (Proxy :: Proxy GetComplianceDetailsByResource)

responseDeletePendingAggregationRequest :: DeletePendingAggregationRequestResponse -> TestTree
responseDeletePendingAggregationRequest = res
    "DeletePendingAggregationRequestResponse"
    "fixture/DeletePendingAggregationRequestResponse.proto"
    config
    (Proxy :: Proxy DeletePendingAggregationRequest)

responseDeliverConfigSnapshot :: DeliverConfigSnapshotResponse -> TestTree
responseDeliverConfigSnapshot = res
    "DeliverConfigSnapshotResponse"
    "fixture/DeliverConfigSnapshotResponse.proto"
    config
    (Proxy :: Proxy DeliverConfigSnapshot)

responseBatchGetAggregateResourceConfig :: BatchGetAggregateResourceConfigResponse -> TestTree
responseBatchGetAggregateResourceConfig = res
    "BatchGetAggregateResourceConfigResponse"
    "fixture/BatchGetAggregateResourceConfigResponse.proto"
    config
    (Proxy :: Proxy BatchGetAggregateResourceConfig)

responseDescribeConfigRuleEvaluationStatus :: DescribeConfigRuleEvaluationStatusResponse -> TestTree
responseDescribeConfigRuleEvaluationStatus = res
    "DescribeConfigRuleEvaluationStatusResponse"
    "fixture/DescribeConfigRuleEvaluationStatusResponse.proto"
    config
    (Proxy :: Proxy DescribeConfigRuleEvaluationStatus)

responseGetDiscoveredResourceCounts :: GetDiscoveredResourceCountsResponse -> TestTree
responseGetDiscoveredResourceCounts = res
    "GetDiscoveredResourceCountsResponse"
    "fixture/GetDiscoveredResourceCountsResponse.proto"
    config
    (Proxy :: Proxy GetDiscoveredResourceCounts)

responseStartConfigRulesEvaluation :: StartConfigRulesEvaluationResponse -> TestTree
responseStartConfigRulesEvaluation = res
    "StartConfigRulesEvaluationResponse"
    "fixture/StartConfigRulesEvaluationResponse.proto"
    config
    (Proxy :: Proxy StartConfigRulesEvaluation)

responseDescribeComplianceByResource :: DescribeComplianceByResourceResponse -> TestTree
responseDescribeComplianceByResource = res
    "DescribeComplianceByResourceResponse"
    "fixture/DescribeComplianceByResourceResponse.proto"
    config
    (Proxy :: Proxy DescribeComplianceByResource)

responsePutEvaluations :: PutEvaluationsResponse -> TestTree
responsePutEvaluations = res
    "PutEvaluationsResponse"
    "fixture/PutEvaluationsResponse.proto"
    config
    (Proxy :: Proxy PutEvaluations)

responseDescribeConfigurationRecorders :: DescribeConfigurationRecordersResponse -> TestTree
responseDescribeConfigurationRecorders = res
    "DescribeConfigurationRecordersResponse"
    "fixture/DescribeConfigurationRecordersResponse.proto"
    config
    (Proxy :: Proxy DescribeConfigurationRecorders)

responseGetAggregateComplianceDetailsByConfigRule :: GetAggregateComplianceDetailsByConfigRuleResponse -> TestTree
responseGetAggregateComplianceDetailsByConfigRule = res
    "GetAggregateComplianceDetailsByConfigRuleResponse"
    "fixture/GetAggregateComplianceDetailsByConfigRuleResponse.proto"
    config
    (Proxy :: Proxy GetAggregateComplianceDetailsByConfigRule)

responseGetAggregateDiscoveredResourceCounts :: GetAggregateDiscoveredResourceCountsResponse -> TestTree
responseGetAggregateDiscoveredResourceCounts = res
    "GetAggregateDiscoveredResourceCountsResponse"
    "fixture/GetAggregateDiscoveredResourceCountsResponse.proto"
    config
    (Proxy :: Proxy GetAggregateDiscoveredResourceCounts)

responseStartConfigurationRecorder :: StartConfigurationRecorderResponse -> TestTree
responseStartConfigurationRecorder = res
    "StartConfigurationRecorderResponse"
    "fixture/StartConfigurationRecorderResponse.proto"
    config
    (Proxy :: Proxy StartConfigurationRecorder)

responsePutRemediationConfigurations :: PutRemediationConfigurationsResponse -> TestTree
responsePutRemediationConfigurations = res
    "PutRemediationConfigurationsResponse"
    "fixture/PutRemediationConfigurationsResponse.proto"
    config
    (Proxy :: Proxy PutRemediationConfigurations)

responseStartRemediationExecution :: StartRemediationExecutionResponse -> TestTree
responseStartRemediationExecution = res
    "StartRemediationExecutionResponse"
    "fixture/StartRemediationExecutionResponse.proto"
    config
    (Proxy :: Proxy StartRemediationExecution)

responseGetComplianceSummaryByConfigRule :: GetComplianceSummaryByConfigRuleResponse -> TestTree
responseGetComplianceSummaryByConfigRule = res
    "GetComplianceSummaryByConfigRuleResponse"
    "fixture/GetComplianceSummaryByConfigRuleResponse.proto"
    config
    (Proxy :: Proxy GetComplianceSummaryByConfigRule)

responsePutConfigurationAggregator :: PutConfigurationAggregatorResponse -> TestTree
responsePutConfigurationAggregator = res
    "PutConfigurationAggregatorResponse"
    "fixture/PutConfigurationAggregatorResponse.proto"
    config
    (Proxy :: Proxy PutConfigurationAggregator)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    config
    (Proxy :: Proxy TagResource)

responseDeleteConfigurationAggregator :: DeleteConfigurationAggregatorResponse -> TestTree
responseDeleteConfigurationAggregator = res
    "DeleteConfigurationAggregatorResponse"
    "fixture/DeleteConfigurationAggregatorResponse.proto"
    config
    (Proxy :: Proxy DeleteConfigurationAggregator)

responseDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatusResponse -> TestTree
responseDescribeConfigurationRecorderStatus = res
    "DescribeConfigurationRecorderStatusResponse"
    "fixture/DescribeConfigurationRecorderStatusResponse.proto"
    config
    (Proxy :: Proxy DescribeConfigurationRecorderStatus)

responsePutConfigurationRecorder :: PutConfigurationRecorderResponse -> TestTree
responsePutConfigurationRecorder = res
    "PutConfigurationRecorderResponse"
    "fixture/PutConfigurationRecorderResponse.proto"
    config
    (Proxy :: Proxy PutConfigurationRecorder)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    config
    (Proxy :: Proxy UntagResource)

responseDeleteConfigurationRecorder :: DeleteConfigurationRecorderResponse -> TestTree
responseDeleteConfigurationRecorder = res
    "DeleteConfigurationRecorderResponse"
    "fixture/DeleteConfigurationRecorderResponse.proto"
    config
    (Proxy :: Proxy DeleteConfigurationRecorder)

responseGetComplianceSummaryByResourceType :: GetComplianceSummaryByResourceTypeResponse -> TestTree
responseGetComplianceSummaryByResourceType = res
    "GetComplianceSummaryByResourceTypeResponse"
    "fixture/GetComplianceSummaryByResourceTypeResponse.proto"
    config
    (Proxy :: Proxy GetComplianceSummaryByResourceType)

responseDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatusResponse -> TestTree
responseDescribeDeliveryChannelStatus = res
    "DescribeDeliveryChannelStatusResponse"
    "fixture/DescribeDeliveryChannelStatusResponse.proto"
    config
    (Proxy :: Proxy DescribeDeliveryChannelStatus)

responsePutDeliveryChannel :: PutDeliveryChannelResponse -> TestTree
responsePutDeliveryChannel = res
    "PutDeliveryChannelResponse"
    "fixture/PutDeliveryChannelResponse.proto"
    config
    (Proxy :: Proxy PutDeliveryChannel)

responseGetComplianceDetailsByConfigRule :: GetComplianceDetailsByConfigRuleResponse -> TestTree
responseGetComplianceDetailsByConfigRule = res
    "GetComplianceDetailsByConfigRuleResponse"
    "fixture/GetComplianceDetailsByConfigRuleResponse.proto"
    config
    (Proxy :: Proxy GetComplianceDetailsByConfigRule)

responseDeleteAggregationAuthorization :: DeleteAggregationAuthorizationResponse -> TestTree
responseDeleteAggregationAuthorization = res
    "DeleteAggregationAuthorizationResponse"
    "fixture/DeleteAggregationAuthorizationResponse.proto"
    config
    (Proxy :: Proxy DeleteAggregationAuthorization)

responseDeleteDeliveryChannel :: DeleteDeliveryChannelResponse -> TestTree
responseDeleteDeliveryChannel = res
    "DeleteDeliveryChannelResponse"
    "fixture/DeleteDeliveryChannelResponse.proto"
    config
    (Proxy :: Proxy DeleteDeliveryChannel)

responseDeleteRemediationConfiguration :: DeleteRemediationConfigurationResponse -> TestTree
responseDeleteRemediationConfiguration = res
    "DeleteRemediationConfigurationResponse"
    "fixture/DeleteRemediationConfigurationResponse.proto"
    config
    (Proxy :: Proxy DeleteRemediationConfiguration)

responsePutAggregationAuthorization :: PutAggregationAuthorizationResponse -> TestTree
responsePutAggregationAuthorization = res
    "PutAggregationAuthorizationResponse"
    "fixture/PutAggregationAuthorizationResponse.proto"
    config
    (Proxy :: Proxy PutAggregationAuthorization)

responseDescribeConfigurationAggregatorSourcesStatus :: DescribeConfigurationAggregatorSourcesStatusResponse -> TestTree
responseDescribeConfigurationAggregatorSourcesStatus = res
    "DescribeConfigurationAggregatorSourcesStatusResponse"
    "fixture/DescribeConfigurationAggregatorSourcesStatusResponse.proto"
    config
    (Proxy :: Proxy DescribeConfigurationAggregatorSourcesStatus)

responseListDiscoveredResources :: ListDiscoveredResourcesResponse -> TestTree
responseListDiscoveredResources = res
    "ListDiscoveredResourcesResponse"
    "fixture/ListDiscoveredResourcesResponse.proto"
    config
    (Proxy :: Proxy ListDiscoveredResources)

responseDescribeRemediationConfigurations :: DescribeRemediationConfigurationsResponse -> TestTree
responseDescribeRemediationConfigurations = res
    "DescribeRemediationConfigurationsResponse"
    "fixture/DescribeRemediationConfigurationsResponse.proto"
    config
    (Proxy :: Proxy DescribeRemediationConfigurations)

responseDescribeDeliveryChannels :: DescribeDeliveryChannelsResponse -> TestTree
responseDescribeDeliveryChannels = res
    "DescribeDeliveryChannelsResponse"
    "fixture/DescribeDeliveryChannelsResponse.proto"
    config
    (Proxy :: Proxy DescribeDeliveryChannels)

responseDescribeAggregationAuthorizations :: DescribeAggregationAuthorizationsResponse -> TestTree
responseDescribeAggregationAuthorizations = res
    "DescribeAggregationAuthorizationsResponse"
    "fixture/DescribeAggregationAuthorizationsResponse.proto"
    config
    (Proxy :: Proxy DescribeAggregationAuthorizations)
