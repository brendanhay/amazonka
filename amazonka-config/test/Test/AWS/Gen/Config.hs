{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Config
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
--         , testDeleteDeliveryChannel $
--             deleteDeliveryChannel
--
--         , testPutDeliveryChannel $
--             putDeliveryChannel
--
--         , testDescribeDeliveryChannelStatus $
--             describeDeliveryChannelStatus
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
--         , testDeleteDeliveryChannelResponse $
--             deleteDeliveryChannelResponse
--
--         , testPutDeliveryChannelResponse $
--             putDeliveryChannelResponse
--
--         , testDescribeDeliveryChannelStatusResponse $
--             describeDeliveryChannelStatusResponse
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
    "fixture/GetResourceConfigHistory"

testStopConfigurationRecorder :: StopConfigurationRecorder -> TestTree
testStopConfigurationRecorder = req
    "StopConfigurationRecorder"
    "fixture/StopConfigurationRecorder"

testDeliverConfigSnapshot :: DeliverConfigSnapshot -> TestTree
testDeliverConfigSnapshot = req
    "DeliverConfigSnapshot"
    "fixture/DeliverConfigSnapshot"

testDescribeConfigurationRecorders :: DescribeConfigurationRecorders -> TestTree
testDescribeConfigurationRecorders = req
    "DescribeConfigurationRecorders"
    "fixture/DescribeConfigurationRecorders"

testStartConfigurationRecorder :: StartConfigurationRecorder -> TestTree
testStartConfigurationRecorder = req
    "StartConfigurationRecorder"
    "fixture/StartConfigurationRecorder"

testDescribeConfigurationRecorderStatus :: DescribeConfigurationRecorderStatus -> TestTree
testDescribeConfigurationRecorderStatus = req
    "DescribeConfigurationRecorderStatus"
    "fixture/DescribeConfigurationRecorderStatus"

testPutConfigurationRecorder :: PutConfigurationRecorder -> TestTree
testPutConfigurationRecorder = req
    "PutConfigurationRecorder"
    "fixture/PutConfigurationRecorder"

testDeleteDeliveryChannel :: DeleteDeliveryChannel -> TestTree
testDeleteDeliveryChannel = req
    "DeleteDeliveryChannel"
    "fixture/DeleteDeliveryChannel"

testPutDeliveryChannel :: PutDeliveryChannel -> TestTree
testPutDeliveryChannel = req
    "PutDeliveryChannel"
    "fixture/PutDeliveryChannel"

testDescribeDeliveryChannelStatus :: DescribeDeliveryChannelStatus -> TestTree
testDescribeDeliveryChannelStatus = req
    "DescribeDeliveryChannelStatus"
    "fixture/DescribeDeliveryChannelStatus"

testDescribeDeliveryChannels :: DescribeDeliveryChannels -> TestTree
testDescribeDeliveryChannels = req
    "DescribeDeliveryChannels"
    "fixture/DescribeDeliveryChannels"

-- Responses

testGetResourceConfigHistoryResponse :: GetResourceConfigHistoryResponse -> TestTree
testGetResourceConfigHistoryResponse = res
    "GetResourceConfigHistoryResponse"
    "fixture/GetResourceConfigHistoryResponse"
    (Proxy :: Proxy GetResourceConfigHistory)

testStopConfigurationRecorderResponse :: StopConfigurationRecorderResponse -> TestTree
testStopConfigurationRecorderResponse = res
    "StopConfigurationRecorderResponse"
    "fixture/StopConfigurationRecorderResponse"
    (Proxy :: Proxy StopConfigurationRecorder)

testDeliverConfigSnapshotResponse :: DeliverConfigSnapshotResponse -> TestTree
testDeliverConfigSnapshotResponse = res
    "DeliverConfigSnapshotResponse"
    "fixture/DeliverConfigSnapshotResponse"
    (Proxy :: Proxy DeliverConfigSnapshot)

testDescribeConfigurationRecordersResponse :: DescribeConfigurationRecordersResponse -> TestTree
testDescribeConfigurationRecordersResponse = res
    "DescribeConfigurationRecordersResponse"
    "fixture/DescribeConfigurationRecordersResponse"
    (Proxy :: Proxy DescribeConfigurationRecorders)

testStartConfigurationRecorderResponse :: StartConfigurationRecorderResponse -> TestTree
testStartConfigurationRecorderResponse = res
    "StartConfigurationRecorderResponse"
    "fixture/StartConfigurationRecorderResponse"
    (Proxy :: Proxy StartConfigurationRecorder)

testDescribeConfigurationRecorderStatusResponse :: DescribeConfigurationRecorderStatusResponse -> TestTree
testDescribeConfigurationRecorderStatusResponse = res
    "DescribeConfigurationRecorderStatusResponse"
    "fixture/DescribeConfigurationRecorderStatusResponse"
    (Proxy :: Proxy DescribeConfigurationRecorderStatus)

testPutConfigurationRecorderResponse :: PutConfigurationRecorderResponse -> TestTree
testPutConfigurationRecorderResponse = res
    "PutConfigurationRecorderResponse"
    "fixture/PutConfigurationRecorderResponse"
    (Proxy :: Proxy PutConfigurationRecorder)

testDeleteDeliveryChannelResponse :: DeleteDeliveryChannelResponse -> TestTree
testDeleteDeliveryChannelResponse = res
    "DeleteDeliveryChannelResponse"
    "fixture/DeleteDeliveryChannelResponse"
    (Proxy :: Proxy DeleteDeliveryChannel)

testPutDeliveryChannelResponse :: PutDeliveryChannelResponse -> TestTree
testPutDeliveryChannelResponse = res
    "PutDeliveryChannelResponse"
    "fixture/PutDeliveryChannelResponse"
    (Proxy :: Proxy PutDeliveryChannel)

testDescribeDeliveryChannelStatusResponse :: DescribeDeliveryChannelStatusResponse -> TestTree
testDescribeDeliveryChannelStatusResponse = res
    "DescribeDeliveryChannelStatusResponse"
    "fixture/DescribeDeliveryChannelStatusResponse"
    (Proxy :: Proxy DescribeDeliveryChannelStatus)

testDescribeDeliveryChannelsResponse :: DescribeDeliveryChannelsResponse -> TestTree
testDescribeDeliveryChannelsResponse = res
    "DescribeDeliveryChannelsResponse"
    "fixture/DescribeDeliveryChannelsResponse"
    (Proxy :: Proxy DescribeDeliveryChannels)
