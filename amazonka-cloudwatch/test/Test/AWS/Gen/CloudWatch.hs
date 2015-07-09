{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudWatch where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CloudWatch

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
testEnableAlarmActions = undefined

testPutMetricData :: PutMetricData -> TestTree
testPutMetricData = undefined

testDescribeAlarms :: DescribeAlarms -> TestTree
testDescribeAlarms = undefined

testListMetrics :: ListMetrics -> TestTree
testListMetrics = undefined

testDeleteAlarms :: DeleteAlarms -> TestTree
testDeleteAlarms = undefined

testDescribeAlarmHistory :: DescribeAlarmHistory -> TestTree
testDescribeAlarmHistory = undefined

testGetMetricStatistics :: GetMetricStatistics -> TestTree
testGetMetricStatistics = undefined

testDisableAlarmActions :: DisableAlarmActions -> TestTree
testDisableAlarmActions = undefined

testDescribeAlarmsForMetric :: DescribeAlarmsForMetric -> TestTree
testDescribeAlarmsForMetric = undefined

testSetAlarmState :: SetAlarmState -> TestTree
testSetAlarmState = undefined

testPutMetricAlarm :: PutMetricAlarm -> TestTree
testPutMetricAlarm = undefined

-- Responses

testEnableAlarmActionsResponse :: EnableAlarmActionsResponse -> TestTree
testEnableAlarmActionsResponse = resp
    "EnableAlarmActionsResponse"
    "fixture/EnableAlarmActionsResponse"
    (Proxy :: Proxy EnableAlarmActions)

testPutMetricDataResponse :: PutMetricDataResponse -> TestTree
testPutMetricDataResponse = resp
    "PutMetricDataResponse"
    "fixture/PutMetricDataResponse"
    (Proxy :: Proxy PutMetricData)

testDescribeAlarmsResponse :: DescribeAlarmsResponse -> TestTree
testDescribeAlarmsResponse = resp
    "DescribeAlarmsResponse"
    "fixture/DescribeAlarmsResponse"
    (Proxy :: Proxy DescribeAlarms)

testListMetricsResponse :: ListMetricsResponse -> TestTree
testListMetricsResponse = resp
    "ListMetricsResponse"
    "fixture/ListMetricsResponse"
    (Proxy :: Proxy ListMetrics)

testDeleteAlarmsResponse :: DeleteAlarmsResponse -> TestTree
testDeleteAlarmsResponse = resp
    "DeleteAlarmsResponse"
    "fixture/DeleteAlarmsResponse"
    (Proxy :: Proxy DeleteAlarms)

testDescribeAlarmHistoryResponse :: DescribeAlarmHistoryResponse -> TestTree
testDescribeAlarmHistoryResponse = resp
    "DescribeAlarmHistoryResponse"
    "fixture/DescribeAlarmHistoryResponse"
    (Proxy :: Proxy DescribeAlarmHistory)

testGetMetricStatisticsResponse :: GetMetricStatisticsResponse -> TestTree
testGetMetricStatisticsResponse = resp
    "GetMetricStatisticsResponse"
    "fixture/GetMetricStatisticsResponse"
    (Proxy :: Proxy GetMetricStatistics)

testDisableAlarmActionsResponse :: DisableAlarmActionsResponse -> TestTree
testDisableAlarmActionsResponse = resp
    "DisableAlarmActionsResponse"
    "fixture/DisableAlarmActionsResponse"
    (Proxy :: Proxy DisableAlarmActions)

testDescribeAlarmsForMetricResponse :: DescribeAlarmsForMetricResponse -> TestTree
testDescribeAlarmsForMetricResponse = resp
    "DescribeAlarmsForMetricResponse"
    "fixture/DescribeAlarmsForMetricResponse"
    (Proxy :: Proxy DescribeAlarmsForMetric)

testSetAlarmStateResponse :: SetAlarmStateResponse -> TestTree
testSetAlarmStateResponse = resp
    "SetAlarmStateResponse"
    "fixture/SetAlarmStateResponse"
    (Proxy :: Proxy SetAlarmState)

testPutMetricAlarmResponse :: PutMetricAlarmResponse -> TestTree
testPutMetricAlarmResponse = resp
    "PutMetricAlarmResponse"
    "fixture/PutMetricAlarmResponse"
    (Proxy :: Proxy PutMetricAlarm)

instance Out AlarmHistoryItem
instance Out ComparisonOperator
instance Out Datapoint
instance Out DeleteAlarms
instance Out DeleteAlarmsResponse
instance Out DescribeAlarmHistory
instance Out DescribeAlarmHistoryResponse
instance Out DescribeAlarms
instance Out DescribeAlarmsForMetric
instance Out DescribeAlarmsForMetricResponse
instance Out DescribeAlarmsResponse
instance Out Dimension
instance Out DimensionFilter
instance Out DisableAlarmActions
instance Out DisableAlarmActionsResponse
instance Out EnableAlarmActions
instance Out EnableAlarmActionsResponse
instance Out GetMetricStatistics
instance Out GetMetricStatisticsResponse
instance Out HistoryItemType
instance Out ListMetrics
instance Out ListMetricsResponse
instance Out Metric
instance Out MetricAlarm
instance Out MetricDatum
instance Out PutMetricAlarm
instance Out PutMetricAlarmResponse
instance Out PutMetricData
instance Out PutMetricDataResponse
instance Out SetAlarmState
instance Out SetAlarmStateResponse
instance Out StandardUnit
instance Out StateValue
instance Out Statistic
instance Out StatisticSet
