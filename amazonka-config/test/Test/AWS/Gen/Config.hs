{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Config
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Config where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Config
import Test.AWS.Config.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetResourceConfigHistory $
--             getResourceConfigHistory
--
--         , testDescribeComplianceByConfigRule $
--             describeComplianceByConfigRule
--
--         , testStopConfigurationRecorder $
--             stopConfigurationRecorder
--
--         , testDescribeConfigRules $
--             describeConfigRules
--
--         , testPutConfigRule $
--             putConfigRule
--
--         , testDeleteConfigRule $
--             deleteConfigRule
--
--         , testGetComplianceDetailsByResource $
--             getComplianceDetailsByResource
--
--         , testDeliverConfigSnapshot $
--             deliverConfigSnapshot
--
--         , testDescribeConfigRuleEvaluationStatus $
--             describeConfigRuleEvaluationStatus
--
--         , testDescribeComplianceByResource $
--             describeComplianceByResource
--
--         , testPutEvaluations $
--             putEvaluations
--
--         , testDescribeConfigurationRecorders $
--             describeConfigurationRecorders
--
--         , testStartConfigurationRecorder $
--             startConfigurationRecorder
--
--         , testGetComplianceSummaryByConfigRule $
--             getComplianceSummaryByConfigRule
--
--         , testDescribeConfigurationRecorderStatus $
--             describeConfigurationRecorderStatus
--
--         , testPutConfigurationRecorder $
--             putConfigurationRecorder
--
--         , testGetComplianceSummaryByResourceType $
--             getComplianceSummaryByResourceType
--
--         , testDescribeDeliveryChannelStatus $
--             describeDeliveryChannelStatus
--
--         , testPutDeliveryChannel $
--             putDeliveryChannel
--
--         , testGetComplianceDetailsByConfigRule $
--             getComplianceDetailsByConfigRule
--
--         , testDeleteDeliveryChannel $
--             deleteDeliveryChannel
--
--         , testListDiscoveredResources $
--             listDiscoveredResources
--
--         , testDescribeDeliveryChannels $
--             describeDeliveryChannels
--
--           ]

--     , testGroup "response"
--         [ testGetResourceConfigHistoryResponse $
--             getResourceConfigHistoryResponse
--
--         , testDescribeComplianceByConfigRuleResponse $
--             describeComplianceByConfigRuleResponse
--
--         , testStopConfigurationRecorderResponse $
--             stopConfigurationRecorderResponse
--
--         , testDescribeConfigRulesResponse $
--             describeConfigRulesResponse
--
--         , testPutConfigRuleResponse $
--             putConfigRuleResponse
--
--         , testDeleteConfigRuleResponse $
--             deleteConfigRuleResponse
--
--         , testGetComplianceDetailsByResourceResponse $
--             getComplianceDetailsByResourceResponse
--
--         , testDeliverConfigSnapshotResponse $
--             deliverConfigSnapshotResponse
--
--         , testDescribeConfigRuleEvaluationStatusResponse $
--             describeConfigRuleEvaluationStatusResponse
--
--         , testDescribeComplianceByResourceResponse $
--             describeComplianceByResourceResponse
--
--         , testPutEvaluationsResponse $
--             putEvaluationsResponse
--
--         , testDescribeConfigurationRecordersResponse $
--             describeConfigurationRecordersResponse
--
--         , testStartConfigurationRecorderResponse $
--             startConfigurationRecorderResponse
--
--         , testGetComplianceSummaryByConfigRuleResponse $
--             getComplianceSummaryByConfigRuleResponse
--
--         , testDescribeConfigurationRecorderStatusResponse $
--             describeConfigurationRecorderStatusResponse
--
--         , testPutConfigurationRecorderResponse $
--             putConfigurationRecorderResponse
--
--         , testGetComplianceSummaryByResourceTypeResponse $
--             getComplianceSummaryByResourceTypeResponse
--
--         , testDescribeDeliveryChannelStatusResponse $
--             describeDeliveryChannelStatusResponse
--
--         , testPutDeliveryChannelResponse $
--             putDeliveryChannelResponse
--
--         , testGetComplianceDetailsByConfigRuleResponse $
--             getComplianceDetailsByConfigRuleResponse
--
--         , testDeleteDeliveryChannelResponse $
--             deleteDeliveryChannelResponse
--
--         , testListDiscoveredResourcesResponse $
--             listDiscoveredResourcesResponse
--
--         , testDescribeDeliveryChannelsResponse $
--             describeDeliveryChannelsResponse
--
--           ]
--     ]

-- Requests

testGetResourceConfigHistory :: GetResourceConfigHistory -> TestTree
testGetResourceConfigHistory = req
    "GetResourceConfigHistory"
    "fixture/GetResourceConfigHistory.yaml"

testDescribeComplianceByConfigRule :: DescribeComplianceByConfigRule -> TestTree
testDescribeComplianceByConfigRule = req
    "DescribeComplianceByConfigRule"
    "fixture/DescribeComplianceByConfigRule.yaml"

testStopConfigurationRecorder :: StopConfigurationRecorder -> TestTree
testStopConfigurationRecorder = req
    "StopConfigurationRecorder"
    "fixture/StopConfigurationRecorder.yaml"

testDescribeConfigRules :: DescribeConfigRules -> TestTree
testDescribeConfigRules = req
    "DescribeConfigRules"
    "fixture/DescribeConfigRules.yaml"

testPutConfigRule :: PutConfigRule -> TestTree
testPutConfigRule = req
    "PutConfigRule"
    "fixture/PutConfigRule.yaml"

testDeleteConfigRule :: DeleteConfigRule -> TestTree
testDeleteConfigRule = req
    "DeleteConfigRule"
    "fixture/DeleteConfigRule.yaml"

testGetComplianceDetailsByResource :: GetComplianceDetailsByResource -> TestTree
testGetComplianceDetailsByResource = req
    "GetComplianceDetailsByResource"
    "fixture/GetComplianceDetailsByResource.yaml"

testDeliverConfigSnapshot :: DeliverConfigSnapshot -> TestTree
testDeliverConfigSnapshot = req
    "DeliverConfigSnapshot"
    "fixture/DeliverConfigSnapshot.yaml"

testDescribeConfigRuleEvaluationStatus :: DescribeConfigRuleEvaluationStatus -> TestTree
testDescribeConfigRuleEvaluationStatus = req
    "DescribeConfigRuleEvaluationStatus"
    "fixture/DescribeConfigRuleEvaluationStatus.yaml"

testDescribeComplianceByResource :: DescribeComplianceByResource -> TestTree
testDescribeComplianceByResource = req
    "DescribeComplianceByResource"
    "fixture/DescribeComplianceByResource.yaml"

testPutEvaluations :: PutEvaluations -> TestTree
testPutEvaluations = req
    "PutEvaluations"
    "fixture/PutEvaluations.yaml"

testDescribeConfigurationRecorders :: DescribeConfigurationRecorders -> TestTree
testDescribeConfigurationRecorders = req
    "DescribeConfigurationRecorders"
    "fixture/DescribeConfigurationRecorders.yaml"

testStartConfigurationRecorder :: StartConfigurationRecorder -> TestTree
testStartConfigurationRecorder = req
    "StartConfigurationRecorder"
    "fixture/StartConfigurationRecorder.yaml"

testGetComplianceSummaryByConfigRule :: GetComplianceSummaryByConfigRule -> TestTree
testGetComplianceSummaryByConfigRule = req
    "GetComplianceSummaryByConfigRule"
    "fixture/GetComplianceSummaryByConfigRule.yaml"

testDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatus -> TestTree
testDescribeConfigurationRecorderStatus = req
    "DescribeConfigurationRecorderStatus"
    "fixture/DescribeConfigurationRecorderStatus.yaml"

testPutConfigurationRecorder :: PutConfigurationRecorder -> TestTree
testPutConfigurationRecorder = req
    "PutConfigurationRecorder"
    "fixture/PutConfigurationRecorder.yaml"

testGetComplianceSummaryByResourceType :: GetComplianceSummaryByResourceType -> TestTree
testGetComplianceSummaryByResourceType = req
    "GetComplianceSummaryByResourceType"
    "fixture/GetComplianceSummaryByResourceType.yaml"

testDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatus -> TestTree
testDescribeDeliveryChannelStatus = req
    "DescribeDeliveryChannelStatus"
    "fixture/DescribeDeliveryChannelStatus.yaml"

testPutDeliveryChannel :: PutDeliveryChannel -> TestTree
testPutDeliveryChannel = req
    "PutDeliveryChannel"
    "fixture/PutDeliveryChannel.yaml"

testGetComplianceDetailsByConfigRule :: GetComplianceDetailsByConfigRule -> TestTree
testGetComplianceDetailsByConfigRule = req
    "GetComplianceDetailsByConfigRule"
    "fixture/GetComplianceDetailsByConfigRule.yaml"

testDeleteDeliveryChannel :: DeleteDeliveryChannel -> TestTree
testDeleteDeliveryChannel = req
    "DeleteDeliveryChannel"
    "fixture/DeleteDeliveryChannel.yaml"

testListDiscoveredResources :: ListDiscoveredResources -> TestTree
testListDiscoveredResources = req
    "ListDiscoveredResources"
    "fixture/ListDiscoveredResources.yaml"

testDescribeDeliveryChannels :: DescribeDeliveryChannels -> TestTree
testDescribeDeliveryChannels = req
    "DescribeDeliveryChannels"
    "fixture/DescribeDeliveryChannels.yaml"

-- Responses

testGetResourceConfigHistoryResponse :: GetResourceConfigHistoryResponse -> TestTree
testGetResourceConfigHistoryResponse = res
    "GetResourceConfigHistoryResponse"
    "fixture/GetResourceConfigHistoryResponse.proto"
    config
    (Proxy :: Proxy GetResourceConfigHistory)

testDescribeComplianceByConfigRuleResponse :: DescribeComplianceByConfigRuleResponse -> TestTree
testDescribeComplianceByConfigRuleResponse = res
    "DescribeComplianceByConfigRuleResponse"
    "fixture/DescribeComplianceByConfigRuleResponse.proto"
    config
    (Proxy :: Proxy DescribeComplianceByConfigRule)

testStopConfigurationRecorderResponse :: StopConfigurationRecorderResponse -> TestTree
testStopConfigurationRecorderResponse = res
    "StopConfigurationRecorderResponse"
    "fixture/StopConfigurationRecorderResponse.proto"
    config
    (Proxy :: Proxy StopConfigurationRecorder)

testDescribeConfigRulesResponse :: DescribeConfigRulesResponse -> TestTree
testDescribeConfigRulesResponse = res
    "DescribeConfigRulesResponse"
    "fixture/DescribeConfigRulesResponse.proto"
    config
    (Proxy :: Proxy DescribeConfigRules)

testPutConfigRuleResponse :: PutConfigRuleResponse -> TestTree
testPutConfigRuleResponse = res
    "PutConfigRuleResponse"
    "fixture/PutConfigRuleResponse.proto"
    config
    (Proxy :: Proxy PutConfigRule)

testDeleteConfigRuleResponse :: DeleteConfigRuleResponse -> TestTree
testDeleteConfigRuleResponse = res
    "DeleteConfigRuleResponse"
    "fixture/DeleteConfigRuleResponse.proto"
    config
    (Proxy :: Proxy DeleteConfigRule)

testGetComplianceDetailsByResourceResponse :: GetComplianceDetailsByResourceResponse -> TestTree
testGetComplianceDetailsByResourceResponse = res
    "GetComplianceDetailsByResourceResponse"
    "fixture/GetComplianceDetailsByResourceResponse.proto"
    config
    (Proxy :: Proxy GetComplianceDetailsByResource)

testDeliverConfigSnapshotResponse :: DeliverConfigSnapshotResponse -> TestTree
testDeliverConfigSnapshotResponse = res
    "DeliverConfigSnapshotResponse"
    "fixture/DeliverConfigSnapshotResponse.proto"
    config
    (Proxy :: Proxy DeliverConfigSnapshot)

testDescribeConfigRuleEvaluationStatusResponse :: DescribeConfigRuleEvaluationStatusResponse -> TestTree
testDescribeConfigRuleEvaluationStatusResponse = res
    "DescribeConfigRuleEvaluationStatusResponse"
    "fixture/DescribeConfigRuleEvaluationStatusResponse.proto"
    config
    (Proxy :: Proxy DescribeConfigRuleEvaluationStatus)

testDescribeComplianceByResourceResponse :: DescribeComplianceByResourceResponse -> TestTree
testDescribeComplianceByResourceResponse = res
    "DescribeComplianceByResourceResponse"
    "fixture/DescribeComplianceByResourceResponse.proto"
    config
    (Proxy :: Proxy DescribeComplianceByResource)

testPutEvaluationsResponse :: PutEvaluationsResponse -> TestTree
testPutEvaluationsResponse = res
    "PutEvaluationsResponse"
    "fixture/PutEvaluationsResponse.proto"
    config
    (Proxy :: Proxy PutEvaluations)

testDescribeConfigurationRecordersResponse :: DescribeConfigurationRecordersResponse -> TestTree
testDescribeConfigurationRecordersResponse = res
    "DescribeConfigurationRecordersResponse"
    "fixture/DescribeConfigurationRecordersResponse.proto"
    config
    (Proxy :: Proxy DescribeConfigurationRecorders)

testStartConfigurationRecorderResponse :: StartConfigurationRecorderResponse -> TestTree
testStartConfigurationRecorderResponse = res
    "StartConfigurationRecorderResponse"
    "fixture/StartConfigurationRecorderResponse.proto"
    config
    (Proxy :: Proxy StartConfigurationRecorder)

testGetComplianceSummaryByConfigRuleResponse :: GetComplianceSummaryByConfigRuleResponse -> TestTree
testGetComplianceSummaryByConfigRuleResponse = res
    "GetComplianceSummaryByConfigRuleResponse"
    "fixture/GetComplianceSummaryByConfigRuleResponse.proto"
    config
    (Proxy :: Proxy GetComplianceSummaryByConfigRule)

testDescribeConfigurationRecorderStatusResponse :: DescribeConfigurationRecorderStatusResponse -> TestTree
testDescribeConfigurationRecorderStatusResponse = res
    "DescribeConfigurationRecorderStatusResponse"
    "fixture/DescribeConfigurationRecorderStatusResponse.proto"
    config
    (Proxy :: Proxy DescribeConfigurationRecorderStatus)

testPutConfigurationRecorderResponse :: PutConfigurationRecorderResponse -> TestTree
testPutConfigurationRecorderResponse = res
    "PutConfigurationRecorderResponse"
    "fixture/PutConfigurationRecorderResponse.proto"
    config
    (Proxy :: Proxy PutConfigurationRecorder)

testGetComplianceSummaryByResourceTypeResponse :: GetComplianceSummaryByResourceTypeResponse -> TestTree
testGetComplianceSummaryByResourceTypeResponse = res
    "GetComplianceSummaryByResourceTypeResponse"
    "fixture/GetComplianceSummaryByResourceTypeResponse.proto"
    config
    (Proxy :: Proxy GetComplianceSummaryByResourceType)

testDescribeDeliveryChannelStatusResponse :: DescribeDeliveryChannelStatusResponse -> TestTree
testDescribeDeliveryChannelStatusResponse = res
    "DescribeDeliveryChannelStatusResponse"
    "fixture/DescribeDeliveryChannelStatusResponse.proto"
    config
    (Proxy :: Proxy DescribeDeliveryChannelStatus)

testPutDeliveryChannelResponse :: PutDeliveryChannelResponse -> TestTree
testPutDeliveryChannelResponse = res
    "PutDeliveryChannelResponse"
    "fixture/PutDeliveryChannelResponse.proto"
    config
    (Proxy :: Proxy PutDeliveryChannel)

testGetComplianceDetailsByConfigRuleResponse :: GetComplianceDetailsByConfigRuleResponse -> TestTree
testGetComplianceDetailsByConfigRuleResponse = res
    "GetComplianceDetailsByConfigRuleResponse"
    "fixture/GetComplianceDetailsByConfigRuleResponse.proto"
    config
    (Proxy :: Proxy GetComplianceDetailsByConfigRule)

testDeleteDeliveryChannelResponse :: DeleteDeliveryChannelResponse -> TestTree
testDeleteDeliveryChannelResponse = res
    "DeleteDeliveryChannelResponse"
    "fixture/DeleteDeliveryChannelResponse.proto"
    config
    (Proxy :: Proxy DeleteDeliveryChannel)

testListDiscoveredResourcesResponse :: ListDiscoveredResourcesResponse -> TestTree
testListDiscoveredResourcesResponse = res
    "ListDiscoveredResourcesResponse"
    "fixture/ListDiscoveredResourcesResponse.proto"
    config
    (Proxy :: Proxy ListDiscoveredResources)

testDescribeDeliveryChannelsResponse :: DescribeDeliveryChannelsResponse -> TestTree
testDescribeDeliveryChannelsResponse = res
    "DescribeDeliveryChannelsResponse"
    "fixture/DescribeDeliveryChannelsResponse.proto"
    config
    (Proxy :: Proxy DescribeDeliveryChannels)
