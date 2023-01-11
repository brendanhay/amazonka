{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoTEventsData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoTEventsData where

import Amazonka.IoTEventsData
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoTEventsData.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchAcknowledgeAlarm $
--             newBatchAcknowledgeAlarm
--
--         , requestBatchDeleteDetector $
--             newBatchDeleteDetector
--
--         , requestBatchDisableAlarm $
--             newBatchDisableAlarm
--
--         , requestBatchEnableAlarm $
--             newBatchEnableAlarm
--
--         , requestBatchPutMessage $
--             newBatchPutMessage
--
--         , requestBatchResetAlarm $
--             newBatchResetAlarm
--
--         , requestBatchSnoozeAlarm $
--             newBatchSnoozeAlarm
--
--         , requestBatchUpdateDetector $
--             newBatchUpdateDetector
--
--         , requestDescribeAlarm $
--             newDescribeAlarm
--
--         , requestDescribeDetector $
--             newDescribeDetector
--
--         , requestListAlarms $
--             newListAlarms
--
--         , requestListDetectors $
--             newListDetectors
--
--           ]

--     , testGroup "response"
--         [ responseBatchAcknowledgeAlarm $
--             newBatchAcknowledgeAlarmResponse
--
--         , responseBatchDeleteDetector $
--             newBatchDeleteDetectorResponse
--
--         , responseBatchDisableAlarm $
--             newBatchDisableAlarmResponse
--
--         , responseBatchEnableAlarm $
--             newBatchEnableAlarmResponse
--
--         , responseBatchPutMessage $
--             newBatchPutMessageResponse
--
--         , responseBatchResetAlarm $
--             newBatchResetAlarmResponse
--
--         , responseBatchSnoozeAlarm $
--             newBatchSnoozeAlarmResponse
--
--         , responseBatchUpdateDetector $
--             newBatchUpdateDetectorResponse
--
--         , responseDescribeAlarm $
--             newDescribeAlarmResponse
--
--         , responseDescribeDetector $
--             newDescribeDetectorResponse
--
--         , responseListAlarms $
--             newListAlarmsResponse
--
--         , responseListDetectors $
--             newListDetectorsResponse
--
--           ]
--     ]

-- Requests

requestBatchAcknowledgeAlarm :: BatchAcknowledgeAlarm -> TestTree
requestBatchAcknowledgeAlarm =
  req
    "BatchAcknowledgeAlarm"
    "fixture/BatchAcknowledgeAlarm.yaml"

requestBatchDeleteDetector :: BatchDeleteDetector -> TestTree
requestBatchDeleteDetector =
  req
    "BatchDeleteDetector"
    "fixture/BatchDeleteDetector.yaml"

requestBatchDisableAlarm :: BatchDisableAlarm -> TestTree
requestBatchDisableAlarm =
  req
    "BatchDisableAlarm"
    "fixture/BatchDisableAlarm.yaml"

requestBatchEnableAlarm :: BatchEnableAlarm -> TestTree
requestBatchEnableAlarm =
  req
    "BatchEnableAlarm"
    "fixture/BatchEnableAlarm.yaml"

requestBatchPutMessage :: BatchPutMessage -> TestTree
requestBatchPutMessage =
  req
    "BatchPutMessage"
    "fixture/BatchPutMessage.yaml"

requestBatchResetAlarm :: BatchResetAlarm -> TestTree
requestBatchResetAlarm =
  req
    "BatchResetAlarm"
    "fixture/BatchResetAlarm.yaml"

requestBatchSnoozeAlarm :: BatchSnoozeAlarm -> TestTree
requestBatchSnoozeAlarm =
  req
    "BatchSnoozeAlarm"
    "fixture/BatchSnoozeAlarm.yaml"

requestBatchUpdateDetector :: BatchUpdateDetector -> TestTree
requestBatchUpdateDetector =
  req
    "BatchUpdateDetector"
    "fixture/BatchUpdateDetector.yaml"

requestDescribeAlarm :: DescribeAlarm -> TestTree
requestDescribeAlarm =
  req
    "DescribeAlarm"
    "fixture/DescribeAlarm.yaml"

requestDescribeDetector :: DescribeDetector -> TestTree
requestDescribeDetector =
  req
    "DescribeDetector"
    "fixture/DescribeDetector.yaml"

requestListAlarms :: ListAlarms -> TestTree
requestListAlarms =
  req
    "ListAlarms"
    "fixture/ListAlarms.yaml"

requestListDetectors :: ListDetectors -> TestTree
requestListDetectors =
  req
    "ListDetectors"
    "fixture/ListDetectors.yaml"

-- Responses

responseBatchAcknowledgeAlarm :: BatchAcknowledgeAlarmResponse -> TestTree
responseBatchAcknowledgeAlarm =
  res
    "BatchAcknowledgeAlarmResponse"
    "fixture/BatchAcknowledgeAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAcknowledgeAlarm)

responseBatchDeleteDetector :: BatchDeleteDetectorResponse -> TestTree
responseBatchDeleteDetector =
  res
    "BatchDeleteDetectorResponse"
    "fixture/BatchDeleteDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteDetector)

responseBatchDisableAlarm :: BatchDisableAlarmResponse -> TestTree
responseBatchDisableAlarm =
  res
    "BatchDisableAlarmResponse"
    "fixture/BatchDisableAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisableAlarm)

responseBatchEnableAlarm :: BatchEnableAlarmResponse -> TestTree
responseBatchEnableAlarm =
  res
    "BatchEnableAlarmResponse"
    "fixture/BatchEnableAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchEnableAlarm)

responseBatchPutMessage :: BatchPutMessageResponse -> TestTree
responseBatchPutMessage =
  res
    "BatchPutMessageResponse"
    "fixture/BatchPutMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutMessage)

responseBatchResetAlarm :: BatchResetAlarmResponse -> TestTree
responseBatchResetAlarm =
  res
    "BatchResetAlarmResponse"
    "fixture/BatchResetAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchResetAlarm)

responseBatchSnoozeAlarm :: BatchSnoozeAlarmResponse -> TestTree
responseBatchSnoozeAlarm =
  res
    "BatchSnoozeAlarmResponse"
    "fixture/BatchSnoozeAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchSnoozeAlarm)

responseBatchUpdateDetector :: BatchUpdateDetectorResponse -> TestTree
responseBatchUpdateDetector =
  res
    "BatchUpdateDetectorResponse"
    "fixture/BatchUpdateDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateDetector)

responseDescribeAlarm :: DescribeAlarmResponse -> TestTree
responseDescribeAlarm =
  res
    "DescribeAlarmResponse"
    "fixture/DescribeAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlarm)

responseDescribeDetector :: DescribeDetectorResponse -> TestTree
responseDescribeDetector =
  res
    "DescribeDetectorResponse"
    "fixture/DescribeDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDetector)

responseListAlarms :: ListAlarmsResponse -> TestTree
responseListAlarms =
  res
    "ListAlarmsResponse"
    "fixture/ListAlarmsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAlarms)

responseListDetectors :: ListDetectorsResponse -> TestTree
responseListDetectors =
  res
    "ListDetectorsResponse"
    "fixture/ListDetectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDetectors)
