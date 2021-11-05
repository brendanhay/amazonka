{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTEventsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.IoTEventsData where

import Amazonka.IoTEventsData
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.IoTEventsData.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchSnoozeAlarm $
--             newBatchSnoozeAlarm
--
--         , requestBatchDisableAlarm $
--             newBatchDisableAlarm
--
--         , requestDescribeAlarm $
--             newDescribeAlarm
--
--         , requestBatchPutMessage $
--             newBatchPutMessage
--
--         , requestDescribeDetector $
--             newDescribeDetector
--
--         , requestBatchUpdateDetector $
--             newBatchUpdateDetector
--
--         , requestBatchAcknowledgeAlarm $
--             newBatchAcknowledgeAlarm
--
--         , requestListAlarms $
--             newListAlarms
--
--         , requestBatchResetAlarm $
--             newBatchResetAlarm
--
--         , requestListDetectors $
--             newListDetectors
--
--         , requestBatchEnableAlarm $
--             newBatchEnableAlarm
--
--           ]

--     , testGroup "response"
--         [ responseBatchSnoozeAlarm $
--             newBatchSnoozeAlarmResponse
--
--         , responseBatchDisableAlarm $
--             newBatchDisableAlarmResponse
--
--         , responseDescribeAlarm $
--             newDescribeAlarmResponse
--
--         , responseBatchPutMessage $
--             newBatchPutMessageResponse
--
--         , responseDescribeDetector $
--             newDescribeDetectorResponse
--
--         , responseBatchUpdateDetector $
--             newBatchUpdateDetectorResponse
--
--         , responseBatchAcknowledgeAlarm $
--             newBatchAcknowledgeAlarmResponse
--
--         , responseListAlarms $
--             newListAlarmsResponse
--
--         , responseBatchResetAlarm $
--             newBatchResetAlarmResponse
--
--         , responseListDetectors $
--             newListDetectorsResponse
--
--         , responseBatchEnableAlarm $
--             newBatchEnableAlarmResponse
--
--           ]
--     ]

-- Requests

requestBatchSnoozeAlarm :: BatchSnoozeAlarm -> TestTree
requestBatchSnoozeAlarm =
  req
    "BatchSnoozeAlarm"
    "fixture/BatchSnoozeAlarm.yaml"

requestBatchDisableAlarm :: BatchDisableAlarm -> TestTree
requestBatchDisableAlarm =
  req
    "BatchDisableAlarm"
    "fixture/BatchDisableAlarm.yaml"

requestDescribeAlarm :: DescribeAlarm -> TestTree
requestDescribeAlarm =
  req
    "DescribeAlarm"
    "fixture/DescribeAlarm.yaml"

requestBatchPutMessage :: BatchPutMessage -> TestTree
requestBatchPutMessage =
  req
    "BatchPutMessage"
    "fixture/BatchPutMessage.yaml"

requestDescribeDetector :: DescribeDetector -> TestTree
requestDescribeDetector =
  req
    "DescribeDetector"
    "fixture/DescribeDetector.yaml"

requestBatchUpdateDetector :: BatchUpdateDetector -> TestTree
requestBatchUpdateDetector =
  req
    "BatchUpdateDetector"
    "fixture/BatchUpdateDetector.yaml"

requestBatchAcknowledgeAlarm :: BatchAcknowledgeAlarm -> TestTree
requestBatchAcknowledgeAlarm =
  req
    "BatchAcknowledgeAlarm"
    "fixture/BatchAcknowledgeAlarm.yaml"

requestListAlarms :: ListAlarms -> TestTree
requestListAlarms =
  req
    "ListAlarms"
    "fixture/ListAlarms.yaml"

requestBatchResetAlarm :: BatchResetAlarm -> TestTree
requestBatchResetAlarm =
  req
    "BatchResetAlarm"
    "fixture/BatchResetAlarm.yaml"

requestListDetectors :: ListDetectors -> TestTree
requestListDetectors =
  req
    "ListDetectors"
    "fixture/ListDetectors.yaml"

requestBatchEnableAlarm :: BatchEnableAlarm -> TestTree
requestBatchEnableAlarm =
  req
    "BatchEnableAlarm"
    "fixture/BatchEnableAlarm.yaml"

-- Responses

responseBatchSnoozeAlarm :: BatchSnoozeAlarmResponse -> TestTree
responseBatchSnoozeAlarm =
  res
    "BatchSnoozeAlarmResponse"
    "fixture/BatchSnoozeAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchSnoozeAlarm)

responseBatchDisableAlarm :: BatchDisableAlarmResponse -> TestTree
responseBatchDisableAlarm =
  res
    "BatchDisableAlarmResponse"
    "fixture/BatchDisableAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisableAlarm)

responseDescribeAlarm :: DescribeAlarmResponse -> TestTree
responseDescribeAlarm =
  res
    "DescribeAlarmResponse"
    "fixture/DescribeAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlarm)

responseBatchPutMessage :: BatchPutMessageResponse -> TestTree
responseBatchPutMessage =
  res
    "BatchPutMessageResponse"
    "fixture/BatchPutMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutMessage)

responseDescribeDetector :: DescribeDetectorResponse -> TestTree
responseDescribeDetector =
  res
    "DescribeDetectorResponse"
    "fixture/DescribeDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDetector)

responseBatchUpdateDetector :: BatchUpdateDetectorResponse -> TestTree
responseBatchUpdateDetector =
  res
    "BatchUpdateDetectorResponse"
    "fixture/BatchUpdateDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateDetector)

responseBatchAcknowledgeAlarm :: BatchAcknowledgeAlarmResponse -> TestTree
responseBatchAcknowledgeAlarm =
  res
    "BatchAcknowledgeAlarmResponse"
    "fixture/BatchAcknowledgeAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAcknowledgeAlarm)

responseListAlarms :: ListAlarmsResponse -> TestTree
responseListAlarms =
  res
    "ListAlarmsResponse"
    "fixture/ListAlarmsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAlarms)

responseBatchResetAlarm :: BatchResetAlarmResponse -> TestTree
responseBatchResetAlarm =
  res
    "BatchResetAlarmResponse"
    "fixture/BatchResetAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchResetAlarm)

responseListDetectors :: ListDetectorsResponse -> TestTree
responseListDetectors =
  res
    "ListDetectorsResponse"
    "fixture/ListDetectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDetectors)

responseBatchEnableAlarm :: BatchEnableAlarmResponse -> TestTree
responseBatchEnableAlarm =
  res
    "BatchEnableAlarmResponse"
    "fixture/BatchEnableAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchEnableAlarm)
