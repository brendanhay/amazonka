-- Module      : Test.AWS.Gen.Config
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.Config where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.Config

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ getResourceConfigHistoryTest $
--             getResourceConfigHistory
--
--         , stopConfigurationRecorderTest $
--             stopConfigurationRecorder
--
--         , deliverConfigSnapshotTest $
--             deliverConfigSnapshot
--
--         , describeConfigurationRecordersTest $
--             describeConfigurationRecorders
--
--         , startConfigurationRecorderTest $
--             startConfigurationRecorder
--
--         , describeConfigurationRecorderStatusTest $
--             describeConfigurationRecorderStatus
--
--         , putConfigurationRecorderTest $
--             putConfigurationRecorder
--
--         , deleteDeliveryChannelTest $
--             deleteDeliveryChannel
--
--         , putDeliveryChannelTest $
--             putDeliveryChannel
--
--         , describeDeliveryChannelStatusTest $
--             describeDeliveryChannelStatus
--
--         , describeDeliveryChannelsTest $
--             describeDeliveryChannels
--
--           ]

--     , testGroup "response"
--         [ getResourceConfigHistoryResponseTest $
--             getResourceConfigHistoryResponse
--
--         , stopConfigurationRecorderResponseTest $
--             stopConfigurationRecorderResponse
--
--         , deliverConfigSnapshotResponseTest $
--             deliverConfigSnapshotResponse
--
--         , describeConfigurationRecordersResponseTest $
--             describeConfigurationRecordersResponse
--
--         , startConfigurationRecorderResponseTest $
--             startConfigurationRecorderResponse
--
--         , describeConfigurationRecorderStatusResponseTest $
--             describeConfigurationRecorderStatusResponse
--
--         , putConfigurationRecorderResponseTest $
--             putConfigurationRecorderResponse
--
--         , deleteDeliveryChannelResponseTest $
--             deleteDeliveryChannelResponse
--
--         , putDeliveryChannelResponseTest $
--             putDeliveryChannelResponse
--
--         , describeDeliveryChannelStatusResponseTest $
--             describeDeliveryChannelStatusResponse
--
--         , describeDeliveryChannelsResponseTest $
--             describeDeliveryChannelsResponse
--
--           ]
--     ]

-- Requests

getResourceConfigHistoryTest :: GetResourceConfigHistory -> TestTree
getResourceConfigHistoryTest = undefined

stopConfigurationRecorderTest :: StopConfigurationRecorder -> TestTree
stopConfigurationRecorderTest = undefined

deliverConfigSnapshotTest :: DeliverConfigSnapshot -> TestTree
deliverConfigSnapshotTest = undefined

describeConfigurationRecordersTest :: DescribeConfigurationRecorders -> TestTree
describeConfigurationRecordersTest = undefined

startConfigurationRecorderTest :: StartConfigurationRecorder -> TestTree
startConfigurationRecorderTest = undefined

describeConfigurationRecorderStatusTest :: DescribeConfigurationRecorderStatus -> TestTree
describeConfigurationRecorderStatusTest = undefined

putConfigurationRecorderTest :: PutConfigurationRecorder -> TestTree
putConfigurationRecorderTest = undefined

deleteDeliveryChannelTest :: DeleteDeliveryChannel -> TestTree
deleteDeliveryChannelTest = undefined

putDeliveryChannelTest :: PutDeliveryChannel -> TestTree
putDeliveryChannelTest = undefined

describeDeliveryChannelStatusTest :: DescribeDeliveryChannelStatus -> TestTree
describeDeliveryChannelStatusTest = undefined

describeDeliveryChannelsTest :: DescribeDeliveryChannels -> TestTree
describeDeliveryChannelsTest = undefined

-- Responses

getResourceConfigHistoryResponseTest :: GetResourceConfigHistoryResponse -> TestTree
getResourceConfigHistoryResponseTest = resp
    "GetResourceConfigHistoryResponse"
    "fixture/GetResourceConfigHistoryResponse"
    (Proxy :: Proxy GetResourceConfigHistory)

stopConfigurationRecorderResponseTest :: StopConfigurationRecorderResponse -> TestTree
stopConfigurationRecorderResponseTest = resp
    "StopConfigurationRecorderResponse"
    "fixture/StopConfigurationRecorderResponse"
    (Proxy :: Proxy StopConfigurationRecorder)

deliverConfigSnapshotResponseTest :: DeliverConfigSnapshotResponse -> TestTree
deliverConfigSnapshotResponseTest = resp
    "DeliverConfigSnapshotResponse"
    "fixture/DeliverConfigSnapshotResponse"
    (Proxy :: Proxy DeliverConfigSnapshot)

describeConfigurationRecordersResponseTest :: DescribeConfigurationRecordersResponse -> TestTree
describeConfigurationRecordersResponseTest = resp
    "DescribeConfigurationRecordersResponse"
    "fixture/DescribeConfigurationRecordersResponse"
    (Proxy :: Proxy DescribeConfigurationRecorders)

startConfigurationRecorderResponseTest :: StartConfigurationRecorderResponse -> TestTree
startConfigurationRecorderResponseTest = resp
    "StartConfigurationRecorderResponse"
    "fixture/StartConfigurationRecorderResponse"
    (Proxy :: Proxy StartConfigurationRecorder)

describeConfigurationRecorderStatusResponseTest :: DescribeConfigurationRecorderStatusResponse -> TestTree
describeConfigurationRecorderStatusResponseTest = resp
    "DescribeConfigurationRecorderStatusResponse"
    "fixture/DescribeConfigurationRecorderStatusResponse"
    (Proxy :: Proxy DescribeConfigurationRecorderStatus)

putConfigurationRecorderResponseTest :: PutConfigurationRecorderResponse -> TestTree
putConfigurationRecorderResponseTest = resp
    "PutConfigurationRecorderResponse"
    "fixture/PutConfigurationRecorderResponse"
    (Proxy :: Proxy PutConfigurationRecorder)

deleteDeliveryChannelResponseTest :: DeleteDeliveryChannelResponse -> TestTree
deleteDeliveryChannelResponseTest = resp
    "DeleteDeliveryChannelResponse"
    "fixture/DeleteDeliveryChannelResponse"
    (Proxy :: Proxy DeleteDeliveryChannel)

putDeliveryChannelResponseTest :: PutDeliveryChannelResponse -> TestTree
putDeliveryChannelResponseTest = resp
    "PutDeliveryChannelResponse"
    "fixture/PutDeliveryChannelResponse"
    (Proxy :: Proxy PutDeliveryChannel)

describeDeliveryChannelStatusResponseTest :: DescribeDeliveryChannelStatusResponse -> TestTree
describeDeliveryChannelStatusResponseTest = resp
    "DescribeDeliveryChannelStatusResponse"
    "fixture/DescribeDeliveryChannelStatusResponse"
    (Proxy :: Proxy DescribeDeliveryChannelStatus)

describeDeliveryChannelsResponseTest :: DescribeDeliveryChannelsResponse -> TestTree
describeDeliveryChannelsResponseTest = resp
    "DescribeDeliveryChannelsResponse"
    "fixture/DescribeDeliveryChannelsResponse"
    (Proxy :: Proxy DescribeDeliveryChannels)
