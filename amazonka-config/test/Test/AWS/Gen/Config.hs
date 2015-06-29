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
--         [ deleteDeliveryChannelTest $
--             deleteDeliveryChannel
--
--         , deliverConfigSnapshotTest $
--             deliverConfigSnapshot
--
--         , describeConfigurationRecorderStatusTest $
--             describeConfigurationRecorderStatus
--
--         , describeConfigurationRecordersTest $
--             describeConfigurationRecorders
--
--         , describeDeliveryChannelStatusTest $
--             describeDeliveryChannelStatus
--
--         , describeDeliveryChannelsTest $
--             describeDeliveryChannels
--
--         , getResourceConfigHistoryTest $
--             getResourceConfigHistory
--
--         , putConfigurationRecorderTest $
--             putConfigurationRecorder
--
--         , putDeliveryChannelTest $
--             putDeliveryChannel
--
--         , startConfigurationRecorderTest $
--             startConfigurationRecorder
--
--         , stopConfigurationRecorderTest $
--             stopConfigurationRecorder
--
--           ]

--     , testGroup "response"
--         [ deleteDeliveryChannelResponseTest $
--             deleteDeliveryChannelResponse
--
--         , deliverConfigSnapshotResponseTest $
--             deliverConfigSnapshotResponse
--
--         , describeConfigurationRecorderStatusResponseTest $
--             describeConfigurationRecorderStatusResponse
--
--         , describeConfigurationRecordersResponseTest $
--             describeConfigurationRecordersResponse
--
--         , describeDeliveryChannelStatusResponseTest $
--             describeDeliveryChannelStatusResponse
--
--         , describeDeliveryChannelsResponseTest $
--             describeDeliveryChannelsResponse
--
--         , getResourceConfigHistoryResponseTest $
--             getResourceConfigHistoryResponse
--
--         , putConfigurationRecorderResponseTest $
--             putConfigurationRecorderResponse
--
--         , putDeliveryChannelResponseTest $
--             putDeliveryChannelResponse
--
--         , startConfigurationRecorderResponseTest $
--             startConfigurationRecorderResponse
--
--         , stopConfigurationRecorderResponseTest $
--             stopConfigurationRecorderResponse
--
--           ]
--     ]

-- Requests

deleteDeliveryChannelTest :: DeleteDeliveryChannel -> TestTree
deleteDeliveryChannelTest = undefined

deliverConfigSnapshotTest :: DeliverConfigSnapshot -> TestTree
deliverConfigSnapshotTest = undefined

describeConfigurationRecorderStatusTest :: DescribeConfigurationRecorderStatus -> TestTree
describeConfigurationRecorderStatusTest = undefined

describeConfigurationRecordersTest :: DescribeConfigurationRecorders -> TestTree
describeConfigurationRecordersTest = undefined

describeDeliveryChannelStatusTest :: DescribeDeliveryChannelStatus -> TestTree
describeDeliveryChannelStatusTest = undefined

describeDeliveryChannelsTest :: DescribeDeliveryChannels -> TestTree
describeDeliveryChannelsTest = undefined

getResourceConfigHistoryTest :: GetResourceConfigHistory -> TestTree
getResourceConfigHistoryTest = undefined

putConfigurationRecorderTest :: PutConfigurationRecorder -> TestTree
putConfigurationRecorderTest = undefined

putDeliveryChannelTest :: PutDeliveryChannel -> TestTree
putDeliveryChannelTest = undefined

startConfigurationRecorderTest :: StartConfigurationRecorder -> TestTree
startConfigurationRecorderTest = undefined

stopConfigurationRecorderTest :: StopConfigurationRecorder -> TestTree
stopConfigurationRecorderTest = undefined

-- Responses

deleteDeliveryChannelResponseTest :: DeleteDeliveryChannelResponse -> TestTree
deleteDeliveryChannelResponseTest = resp
    "deleteDeliveryChannelResponse"
    "fixture/DeleteDeliveryChannelResponse"
    (Proxy :: Proxy DeleteDeliveryChannel)

deliverConfigSnapshotResponseTest :: DeliverConfigSnapshotResponse -> TestTree
deliverConfigSnapshotResponseTest = resp
    "deliverConfigSnapshotResponse"
    "fixture/DeliverConfigSnapshotResponse"
    (Proxy :: Proxy DeliverConfigSnapshot)

describeConfigurationRecorderStatusResponseTest :: DescribeConfigurationRecorderStatusResponse -> TestTree
describeConfigurationRecorderStatusResponseTest = resp
    "describeConfigurationRecorderStatusResponse"
    "fixture/DescribeConfigurationRecorderStatusResponse"
    (Proxy :: Proxy DescribeConfigurationRecorderStatus)

describeConfigurationRecordersResponseTest :: DescribeConfigurationRecordersResponse -> TestTree
describeConfigurationRecordersResponseTest = resp
    "describeConfigurationRecordersResponse"
    "fixture/DescribeConfigurationRecordersResponse"
    (Proxy :: Proxy DescribeConfigurationRecorders)

describeDeliveryChannelStatusResponseTest :: DescribeDeliveryChannelStatusResponse -> TestTree
describeDeliveryChannelStatusResponseTest = resp
    "describeDeliveryChannelStatusResponse"
    "fixture/DescribeDeliveryChannelStatusResponse"
    (Proxy :: Proxy DescribeDeliveryChannelStatus)

describeDeliveryChannelsResponseTest :: DescribeDeliveryChannelsResponse -> TestTree
describeDeliveryChannelsResponseTest = resp
    "describeDeliveryChannelsResponse"
    "fixture/DescribeDeliveryChannelsResponse"
    (Proxy :: Proxy DescribeDeliveryChannels)

getResourceConfigHistoryResponseTest :: GetResourceConfigHistoryResponse -> TestTree
getResourceConfigHistoryResponseTest = resp
    "getResourceConfigHistoryResponse"
    "fixture/GetResourceConfigHistoryResponse"
    (Proxy :: Proxy GetResourceConfigHistory)

putConfigurationRecorderResponseTest :: PutConfigurationRecorderResponse -> TestTree
putConfigurationRecorderResponseTest = resp
    "putConfigurationRecorderResponse"
    "fixture/PutConfigurationRecorderResponse"
    (Proxy :: Proxy PutConfigurationRecorder)

putDeliveryChannelResponseTest :: PutDeliveryChannelResponse -> TestTree
putDeliveryChannelResponseTest = resp
    "putDeliveryChannelResponse"
    "fixture/PutDeliveryChannelResponse"
    (Proxy :: Proxy PutDeliveryChannel)

startConfigurationRecorderResponseTest :: StartConfigurationRecorderResponse -> TestTree
startConfigurationRecorderResponseTest = resp
    "startConfigurationRecorderResponse"
    "fixture/StartConfigurationRecorderResponse"
    (Proxy :: Proxy StartConfigurationRecorder)

stopConfigurationRecorderResponseTest :: StopConfigurationRecorderResponse -> TestTree
stopConfigurationRecorderResponseTest = resp
    "stopConfigurationRecorderResponse"
    "fixture/StopConfigurationRecorderResponse"
    (Proxy :: Proxy StopConfigurationRecorder)
