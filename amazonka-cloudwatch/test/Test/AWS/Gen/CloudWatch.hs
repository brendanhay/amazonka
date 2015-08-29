{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudWatch where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CloudWatch
import Test.AWS.CloudWatch.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testEnableAlarmActions $
--             enableAlarmActions
--
--         , testPutMetricData $
--             putMetricData
--
--         , testDescribeAlarms $
--             describeAlarms
--
--         , testListMetrics $
--             listMetrics
--
--         , testDeleteAlarms $
--             deleteAlarms
--
--         , testDescribeAlarmHistory $
--             describeAlarmHistory
--
--         , testGetMetricStatistics $
--             getMetricStatistics
--
--         , testDisableAlarmActions $
--             disableAlarmActions
--
--         , testDescribeAlarmsForMetric $
--             describeAlarmsForMetric
--
--         , testSetAlarmState $
--             setAlarmState
--
--         , testPutMetricAlarm $
--             putMetricAlarm
--
--           ]

--     , testGroup "response"
--         [ testEnableAlarmActionsResponse $
--             enableAlarmActionsResponse
--
--         , testPutMetricDataResponse $
--             putMetricDataResponse
--
--         , testDescribeAlarmsResponse $
--             describeAlarmsResponse
--
--         , testListMetricsResponse $
--             listMetricsResponse
--
--         , testDeleteAlarmsResponse $
--             deleteAlarmsResponse
--
--         , testDescribeAlarmHistoryResponse $
--             describeAlarmHistoryResponse
--
--         , testGetMetricStatisticsResponse $
--             getMetricStatisticsResponse
--
--         , testDisableAlarmActionsResponse $
--             disableAlarmActionsResponse
--
--         , testDescribeAlarmsForMetricResponse $
--             describeAlarmsForMetricResponse
--
--         , testSetAlarmStateResponse $
--             setAlarmStateResponse
--
--         , testPutMetricAlarmResponse $
--             putMetricAlarmResponse
--
--           ]
--     ]

-- Requests

testEnableAlarmActions :: EnableAlarmActions -> TestTree
testEnableAlarmActions = req
    "EnableAlarmActions"
    "fixture/EnableAlarmActions.yaml"

testPutMetricData :: PutMetricData -> TestTree
testPutMetricData = req
    "PutMetricData"
    "fixture/PutMetricData.yaml"

testDescribeAlarms :: DescribeAlarms -> TestTree
testDescribeAlarms = req
    "DescribeAlarms"
    "fixture/DescribeAlarms.yaml"

testListMetrics :: ListMetrics -> TestTree
testListMetrics = req
    "ListMetrics"
    "fixture/ListMetrics.yaml"

testDeleteAlarms :: DeleteAlarms -> TestTree
testDeleteAlarms = req
    "DeleteAlarms"
    "fixture/DeleteAlarms.yaml"

testDescribeAlarmHistory :: DescribeAlarmHistory -> TestTree
testDescribeAlarmHistory = req
    "DescribeAlarmHistory"
    "fixture/DescribeAlarmHistory.yaml"

testGetMetricStatistics :: GetMetricStatistics -> TestTree
testGetMetricStatistics = req
    "GetMetricStatistics"
    "fixture/GetMetricStatistics.yaml"

testDisableAlarmActions :: DisableAlarmActions -> TestTree
testDisableAlarmActions = req
    "DisableAlarmActions"
    "fixture/DisableAlarmActions.yaml"

testDescribeAlarmsForMetric :: DescribeAlarmsForMetric -> TestTree
testDescribeAlarmsForMetric = req
    "DescribeAlarmsForMetric"
    "fixture/DescribeAlarmsForMetric.yaml"

testSetAlarmState :: SetAlarmState -> TestTree
testSetAlarmState = req
    "SetAlarmState"
    "fixture/SetAlarmState.yaml"

testPutMetricAlarm :: PutMetricAlarm -> TestTree
testPutMetricAlarm = req
    "PutMetricAlarm"
    "fixture/PutMetricAlarm.yaml"

-- Responses

testEnableAlarmActionsResponse :: EnableAlarmActionsResponse -> TestTree
testEnableAlarmActionsResponse = res
    "EnableAlarmActionsResponse"
    "fixture/EnableAlarmActionsResponse.proto"
    cloudWatch
    (Proxy :: Proxy EnableAlarmActions)

testPutMetricDataResponse :: PutMetricDataResponse -> TestTree
testPutMetricDataResponse = res
    "PutMetricDataResponse"
    "fixture/PutMetricDataResponse.proto"
    cloudWatch
    (Proxy :: Proxy PutMetricData)

testDescribeAlarmsResponse :: DescribeAlarmsResponse -> TestTree
testDescribeAlarmsResponse = res
    "DescribeAlarmsResponse"
    "fixture/DescribeAlarmsResponse.proto"
    cloudWatch
    (Proxy :: Proxy DescribeAlarms)

testListMetricsResponse :: ListMetricsResponse -> TestTree
testListMetricsResponse = res
    "ListMetricsResponse"
    "fixture/ListMetricsResponse.proto"
    cloudWatch
    (Proxy :: Proxy ListMetrics)

testDeleteAlarmsResponse :: DeleteAlarmsResponse -> TestTree
testDeleteAlarmsResponse = res
    "DeleteAlarmsResponse"
    "fixture/DeleteAlarmsResponse.proto"
    cloudWatch
    (Proxy :: Proxy DeleteAlarms)

testDescribeAlarmHistoryResponse :: DescribeAlarmHistoryResponse -> TestTree
testDescribeAlarmHistoryResponse = res
    "DescribeAlarmHistoryResponse"
    "fixture/DescribeAlarmHistoryResponse.proto"
    cloudWatch
    (Proxy :: Proxy DescribeAlarmHistory)

testGetMetricStatisticsResponse :: GetMetricStatisticsResponse -> TestTree
testGetMetricStatisticsResponse = res
    "GetMetricStatisticsResponse"
    "fixture/GetMetricStatisticsResponse.proto"
    cloudWatch
    (Proxy :: Proxy GetMetricStatistics)

testDisableAlarmActionsResponse :: DisableAlarmActionsResponse -> TestTree
testDisableAlarmActionsResponse = res
    "DisableAlarmActionsResponse"
    "fixture/DisableAlarmActionsResponse.proto"
    cloudWatch
    (Proxy :: Proxy DisableAlarmActions)

testDescribeAlarmsForMetricResponse :: DescribeAlarmsForMetricResponse -> TestTree
testDescribeAlarmsForMetricResponse = res
    "DescribeAlarmsForMetricResponse"
    "fixture/DescribeAlarmsForMetricResponse.proto"
    cloudWatch
    (Proxy :: Proxy DescribeAlarmsForMetric)

testSetAlarmStateResponse :: SetAlarmStateResponse -> TestTree
testSetAlarmStateResponse = res
    "SetAlarmStateResponse"
    "fixture/SetAlarmStateResponse.proto"
    cloudWatch
    (Proxy :: Proxy SetAlarmState)

testPutMetricAlarmResponse :: PutMetricAlarmResponse -> TestTree
testPutMetricAlarmResponse = res
    "PutMetricAlarmResponse"
    "fixture/PutMetricAlarmResponse.proto"
    cloudWatch
    (Proxy :: Proxy PutMetricAlarm)
