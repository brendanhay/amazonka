{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Config
-- Copyright   : (c) 2013-2017 Brendan Hay
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
--         [ requestGetResourceConfigHistory $
--             getResourceConfigHistory
--
--         , requestDescribeComplianceByConfigRule $
--             describeComplianceByConfigRule
--
--         , requestStopConfigurationRecorder $
--             stopConfigurationRecorder
--
--         , requestDescribeConfigRules $
--             describeConfigRules
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
--         , requestGetComplianceDetailsByResource $
--             getComplianceDetailsByResource
--
--         , requestDeliverConfigSnapshot $
--             deliverConfigSnapshot
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
--         , requestStartConfigurationRecorder $
--             startConfigurationRecorder
--
--         , requestGetComplianceSummaryByConfigRule $
--             getComplianceSummaryByConfigRule
--
--         , requestDescribeConfigurationRecorderStatus $
--             describeConfigurationRecorderStatus
--
--         , requestPutConfigurationRecorder $
--             putConfigurationRecorder
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
--         , requestDeleteDeliveryChannel $
--             deleteDeliveryChannel
--
--         , requestListDiscoveredResources $
--             listDiscoveredResources
--
--         , requestDescribeDeliveryChannels $
--             describeDeliveryChannels
--
--           ]

--     , testGroup "response"
--         [ responseGetResourceConfigHistory $
--             getResourceConfigHistoryResponse
--
--         , responseDescribeComplianceByConfigRule $
--             describeComplianceByConfigRuleResponse
--
--         , responseStopConfigurationRecorder $
--             stopConfigurationRecorderResponse
--
--         , responseDescribeConfigRules $
--             describeConfigRulesResponse
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
--         , responseGetComplianceDetailsByResource $
--             getComplianceDetailsByResourceResponse
--
--         , responseDeliverConfigSnapshot $
--             deliverConfigSnapshotResponse
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
--         , responseStartConfigurationRecorder $
--             startConfigurationRecorderResponse
--
--         , responseGetComplianceSummaryByConfigRule $
--             getComplianceSummaryByConfigRuleResponse
--
--         , responseDescribeConfigurationRecorderStatus $
--             describeConfigurationRecorderStatusResponse
--
--         , responsePutConfigurationRecorder $
--             putConfigurationRecorderResponse
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
--         , responseDeleteDeliveryChannel $
--             deleteDeliveryChannelResponse
--
--         , responseListDiscoveredResources $
--             listDiscoveredResourcesResponse
--
--         , responseDescribeDeliveryChannels $
--             describeDeliveryChannelsResponse
--
--           ]
--     ]

-- Requests

requestGetResourceConfigHistory :: GetResourceConfigHistory -> TestTree
requestGetResourceConfigHistory = req
    "GetResourceConfigHistory"
    "fixture/GetResourceConfigHistory.yaml"

requestDescribeComplianceByConfigRule :: DescribeComplianceByConfigRule -> TestTree
requestDescribeComplianceByConfigRule = req
    "DescribeComplianceByConfigRule"
    "fixture/DescribeComplianceByConfigRule.yaml"

requestStopConfigurationRecorder :: StopConfigurationRecorder -> TestTree
requestStopConfigurationRecorder = req
    "StopConfigurationRecorder"
    "fixture/StopConfigurationRecorder.yaml"

requestDescribeConfigRules :: DescribeConfigRules -> TestTree
requestDescribeConfigRules = req
    "DescribeConfigRules"
    "fixture/DescribeConfigRules.yaml"

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

requestGetComplianceDetailsByResource :: GetComplianceDetailsByResource -> TestTree
requestGetComplianceDetailsByResource = req
    "GetComplianceDetailsByResource"
    "fixture/GetComplianceDetailsByResource.yaml"

requestDeliverConfigSnapshot :: DeliverConfigSnapshot -> TestTree
requestDeliverConfigSnapshot = req
    "DeliverConfigSnapshot"
    "fixture/DeliverConfigSnapshot.yaml"

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

requestStartConfigurationRecorder :: StartConfigurationRecorder -> TestTree
requestStartConfigurationRecorder = req
    "StartConfigurationRecorder"
    "fixture/StartConfigurationRecorder.yaml"

requestGetComplianceSummaryByConfigRule :: GetComplianceSummaryByConfigRule -> TestTree
requestGetComplianceSummaryByConfigRule = req
    "GetComplianceSummaryByConfigRule"
    "fixture/GetComplianceSummaryByConfigRule.yaml"

requestDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatus -> TestTree
requestDescribeConfigurationRecorderStatus = req
    "DescribeConfigurationRecorderStatus"
    "fixture/DescribeConfigurationRecorderStatus.yaml"

requestPutConfigurationRecorder :: PutConfigurationRecorder -> TestTree
requestPutConfigurationRecorder = req
    "PutConfigurationRecorder"
    "fixture/PutConfigurationRecorder.yaml"

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

requestDeleteDeliveryChannel :: DeleteDeliveryChannel -> TestTree
requestDeleteDeliveryChannel = req
    "DeleteDeliveryChannel"
    "fixture/DeleteDeliveryChannel.yaml"

requestListDiscoveredResources :: ListDiscoveredResources -> TestTree
requestListDiscoveredResources = req
    "ListDiscoveredResources"
    "fixture/ListDiscoveredResources.yaml"

requestDescribeDeliveryChannels :: DescribeDeliveryChannels -> TestTree
requestDescribeDeliveryChannels = req
    "DescribeDeliveryChannels"
    "fixture/DescribeDeliveryChannels.yaml"

-- Responses

responseGetResourceConfigHistory :: GetResourceConfigHistoryResponse -> TestTree
responseGetResourceConfigHistory = res
    "GetResourceConfigHistoryResponse"
    "fixture/GetResourceConfigHistoryResponse.proto"
    config
    (Proxy :: Proxy GetResourceConfigHistory)

responseDescribeComplianceByConfigRule :: DescribeComplianceByConfigRuleResponse -> TestTree
responseDescribeComplianceByConfigRule = res
    "DescribeComplianceByConfigRuleResponse"
    "fixture/DescribeComplianceByConfigRuleResponse.proto"
    config
    (Proxy :: Proxy DescribeComplianceByConfigRule)

responseStopConfigurationRecorder :: StopConfigurationRecorderResponse -> TestTree
responseStopConfigurationRecorder = res
    "StopConfigurationRecorderResponse"
    "fixture/StopConfigurationRecorderResponse.proto"
    config
    (Proxy :: Proxy StopConfigurationRecorder)

responseDescribeConfigRules :: DescribeConfigRulesResponse -> TestTree
responseDescribeConfigRules = res
    "DescribeConfigRulesResponse"
    "fixture/DescribeConfigRulesResponse.proto"
    config
    (Proxy :: Proxy DescribeConfigRules)

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

responseGetComplianceDetailsByResource :: GetComplianceDetailsByResourceResponse -> TestTree
responseGetComplianceDetailsByResource = res
    "GetComplianceDetailsByResourceResponse"
    "fixture/GetComplianceDetailsByResourceResponse.proto"
    config
    (Proxy :: Proxy GetComplianceDetailsByResource)

responseDeliverConfigSnapshot :: DeliverConfigSnapshotResponse -> TestTree
responseDeliverConfigSnapshot = res
    "DeliverConfigSnapshotResponse"
    "fixture/DeliverConfigSnapshotResponse.proto"
    config
    (Proxy :: Proxy DeliverConfigSnapshot)

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

responseStartConfigurationRecorder :: StartConfigurationRecorderResponse -> TestTree
responseStartConfigurationRecorder = res
    "StartConfigurationRecorderResponse"
    "fixture/StartConfigurationRecorderResponse.proto"
    config
    (Proxy :: Proxy StartConfigurationRecorder)

responseGetComplianceSummaryByConfigRule :: GetComplianceSummaryByConfigRuleResponse -> TestTree
responseGetComplianceSummaryByConfigRule = res
    "GetComplianceSummaryByConfigRuleResponse"
    "fixture/GetComplianceSummaryByConfigRuleResponse.proto"
    config
    (Proxy :: Proxy GetComplianceSummaryByConfigRule)

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

responseDeleteDeliveryChannel :: DeleteDeliveryChannelResponse -> TestTree
responseDeleteDeliveryChannel = res
    "DeleteDeliveryChannelResponse"
    "fixture/DeleteDeliveryChannelResponse.proto"
    config
    (Proxy :: Proxy DeleteDeliveryChannel)

responseListDiscoveredResources :: ListDiscoveredResourcesResponse -> TestTree
responseListDiscoveredResources = res
    "ListDiscoveredResourcesResponse"
    "fixture/ListDiscoveredResourcesResponse.proto"
    config
    (Proxy :: Proxy ListDiscoveredResources)

responseDescribeDeliveryChannels :: DescribeDeliveryChannelsResponse -> TestTree
responseDescribeDeliveryChannels = res
    "DescribeDeliveryChannelsResponse"
    "fixture/DescribeDeliveryChannelsResponse.proto"
    config
    (Proxy :: Proxy DescribeDeliveryChannels)
