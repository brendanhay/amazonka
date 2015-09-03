{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Config
-- Copyright   : (c) 2013-2015 Brendan Hay
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
--         , testStopConfigurationRecorder $
--             stopConfigurationRecorder
--
--         , testDeliverConfigSnapshot $
--             deliverConfigSnapshot
--
--         , testDescribeConfigurationRecorders $
--             describeConfigurationRecorders
--
--         , testStartConfigurationRecorder $
--             startConfigurationRecorder
--
--         , testDescribeConfigurationRecorderStatus $
--             describeConfigurationRecorderStatus
--
--         , testPutConfigurationRecorder $
--             putConfigurationRecorder
--
--         , testDescribeDeliveryChannelStatus $
--             describeDeliveryChannelStatus
--
--         , testPutDeliveryChannel $
--             putDeliveryChannel
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
--         , testStopConfigurationRecorderResponse $
--             stopConfigurationRecorderResponse
--
--         , testDeliverConfigSnapshotResponse $
--             deliverConfigSnapshotResponse
--
--         , testDescribeConfigurationRecordersResponse $
--             describeConfigurationRecordersResponse
--
--         , testStartConfigurationRecorderResponse $
--             startConfigurationRecorderResponse
--
--         , testDescribeConfigurationRecorderStatusResponse $
--             describeConfigurationRecorderStatusResponse
--
--         , testPutConfigurationRecorderResponse $
--             putConfigurationRecorderResponse
--
--         , testDescribeDeliveryChannelStatusResponse $
--             describeDeliveryChannelStatusResponse
--
--         , testPutDeliveryChannelResponse $
--             putDeliveryChannelResponse
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

testStopConfigurationRecorder :: StopConfigurationRecorder -> TestTree
testStopConfigurationRecorder = req
    "StopConfigurationRecorder"
    "fixture/StopConfigurationRecorder.yaml"

testDeliverConfigSnapshot :: DeliverConfigSnapshot -> TestTree
testDeliverConfigSnapshot = req
    "DeliverConfigSnapshot"
    "fixture/DeliverConfigSnapshot.yaml"

testDescribeConfigurationRecorders :: DescribeConfigurationRecorders -> TestTree
testDescribeConfigurationRecorders = req
    "DescribeConfigurationRecorders"
    "fixture/DescribeConfigurationRecorders.yaml"

testStartConfigurationRecorder :: StartConfigurationRecorder -> TestTree
testStartConfigurationRecorder = req
    "StartConfigurationRecorder"
    "fixture/StartConfigurationRecorder.yaml"

testDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatus -> TestTree
testDescribeConfigurationRecorderStatus = req
    "DescribeConfigurationRecorderStatus"
    "fixture/DescribeConfigurationRecorderStatus.yaml"

testPutConfigurationRecorder :: PutConfigurationRecorder -> TestTree
testPutConfigurationRecorder = req
    "PutConfigurationRecorder"
    "fixture/PutConfigurationRecorder.yaml"

testDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatus -> TestTree
testDescribeDeliveryChannelStatus = req
    "DescribeDeliveryChannelStatus"
    "fixture/DescribeDeliveryChannelStatus.yaml"

testPutDeliveryChannel :: PutDeliveryChannel -> TestTree
testPutDeliveryChannel = req
    "PutDeliveryChannel"
    "fixture/PutDeliveryChannel.yaml"

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

testStopConfigurationRecorderResponse :: StopConfigurationRecorderResponse -> TestTree
testStopConfigurationRecorderResponse = res
    "StopConfigurationRecorderResponse"
    "fixture/StopConfigurationRecorderResponse.proto"
    config
    (Proxy :: Proxy StopConfigurationRecorder)

testDeliverConfigSnapshotResponse :: DeliverConfigSnapshotResponse -> TestTree
testDeliverConfigSnapshotResponse = res
    "DeliverConfigSnapshotResponse"
    "fixture/DeliverConfigSnapshotResponse.proto"
    config
    (Proxy :: Proxy DeliverConfigSnapshot)

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
