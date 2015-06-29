-- Module      : Test.AWS.Gen.CloudWatch
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

module Test.AWS.Gen.CloudWatch where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.CloudWatch

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ deleteAlarmsTest $
--             deleteAlarms
--
--         , describeAlarmHistoryTest $
--             describeAlarmHistory
--
--         , describeAlarmsTest $
--             describeAlarms
--
--         , describeAlarmsForMetricTest $
--             describeAlarmsForMetric
--
--         , disableAlarmActionsTest $
--             disableAlarmActions
--
--         , enableAlarmActionsTest $
--             enableAlarmActions
--
--         , getMetricStatisticsTest $
--             getMetricStatistics
--
--         , listMetricsTest $
--             listMetrics
--
--         , putMetricAlarmTest $
--             putMetricAlarm
--
--         , putMetricDataTest $
--             putMetricData
--
--         , setAlarmStateTest $
--             setAlarmState
--
--           ]

--     , testGroup "response"
--         [ deleteAlarmsResponseTest $
--             deleteAlarmsResponse
--
--         , describeAlarmHistoryResponseTest $
--             describeAlarmHistoryResponse
--
--         , describeAlarmsResponseTest $
--             describeAlarmsResponse
--
--         , describeAlarmsForMetricResponseTest $
--             describeAlarmsForMetricResponse
--
--         , disableAlarmActionsResponseTest $
--             disableAlarmActionsResponse
--
--         , enableAlarmActionsResponseTest $
--             enableAlarmActionsResponse
--
--         , getMetricStatisticsResponseTest $
--             getMetricStatisticsResponse
--
--         , listMetricsResponseTest $
--             listMetricsResponse
--
--         , putMetricAlarmResponseTest $
--             putMetricAlarmResponse
--
--         , putMetricDataResponseTest $
--             putMetricDataResponse
--
--         , setAlarmStateResponseTest $
--             setAlarmStateResponse
--
--           ]
--     ]

-- Requests

deleteAlarmsTest :: DeleteAlarms -> TestTree
deleteAlarmsTest = undefined

describeAlarmHistoryTest :: DescribeAlarmHistory -> TestTree
describeAlarmHistoryTest = undefined

describeAlarmsTest :: DescribeAlarms -> TestTree
describeAlarmsTest = undefined

describeAlarmsForMetricTest :: DescribeAlarmsForMetric -> TestTree
describeAlarmsForMetricTest = undefined

disableAlarmActionsTest :: DisableAlarmActions -> TestTree
disableAlarmActionsTest = undefined

enableAlarmActionsTest :: EnableAlarmActions -> TestTree
enableAlarmActionsTest = undefined

getMetricStatisticsTest :: GetMetricStatistics -> TestTree
getMetricStatisticsTest = undefined

listMetricsTest :: ListMetrics -> TestTree
listMetricsTest = undefined

putMetricAlarmTest :: PutMetricAlarm -> TestTree
putMetricAlarmTest = undefined

putMetricDataTest :: PutMetricData -> TestTree
putMetricDataTest = undefined

setAlarmStateTest :: SetAlarmState -> TestTree
setAlarmStateTest = undefined

-- Responses

deleteAlarmsResponseTest :: DeleteAlarmsResponse -> TestTree
deleteAlarmsResponseTest = resp
    "deleteAlarmsResponse"
    "fixture/DeleteAlarmsResponse"
    (Proxy :: Proxy DeleteAlarms)

describeAlarmHistoryResponseTest :: DescribeAlarmHistoryResponse -> TestTree
describeAlarmHistoryResponseTest = resp
    "describeAlarmHistoryResponse"
    "fixture/DescribeAlarmHistoryResponse"
    (Proxy :: Proxy DescribeAlarmHistory)

describeAlarmsResponseTest :: DescribeAlarmsResponse -> TestTree
describeAlarmsResponseTest = resp
    "describeAlarmsResponse"
    "fixture/DescribeAlarmsResponse"
    (Proxy :: Proxy DescribeAlarms)

describeAlarmsForMetricResponseTest :: DescribeAlarmsForMetricResponse -> TestTree
describeAlarmsForMetricResponseTest = resp
    "describeAlarmsForMetricResponse"
    "fixture/DescribeAlarmsForMetricResponse"
    (Proxy :: Proxy DescribeAlarmsForMetric)

disableAlarmActionsResponseTest :: DisableAlarmActionsResponse -> TestTree
disableAlarmActionsResponseTest = resp
    "disableAlarmActionsResponse"
    "fixture/DisableAlarmActionsResponse"
    (Proxy :: Proxy DisableAlarmActions)

enableAlarmActionsResponseTest :: EnableAlarmActionsResponse -> TestTree
enableAlarmActionsResponseTest = resp
    "enableAlarmActionsResponse"
    "fixture/EnableAlarmActionsResponse"
    (Proxy :: Proxy EnableAlarmActions)

getMetricStatisticsResponseTest :: GetMetricStatisticsResponse -> TestTree
getMetricStatisticsResponseTest = resp
    "getMetricStatisticsResponse"
    "fixture/GetMetricStatisticsResponse"
    (Proxy :: Proxy GetMetricStatistics)

listMetricsResponseTest :: ListMetricsResponse -> TestTree
listMetricsResponseTest = resp
    "listMetricsResponse"
    "fixture/ListMetricsResponse"
    (Proxy :: Proxy ListMetrics)

putMetricAlarmResponseTest :: PutMetricAlarmResponse -> TestTree
putMetricAlarmResponseTest = resp
    "putMetricAlarmResponse"
    "fixture/PutMetricAlarmResponse"
    (Proxy :: Proxy PutMetricAlarm)

putMetricDataResponseTest :: PutMetricDataResponse -> TestTree
putMetricDataResponseTest = resp
    "putMetricDataResponse"
    "fixture/PutMetricDataResponse"
    (Proxy :: Proxy PutMetricData)

setAlarmStateResponseTest :: SetAlarmStateResponse -> TestTree
setAlarmStateResponseTest = resp
    "setAlarmStateResponse"
    "fixture/SetAlarmStateResponse"
    (Proxy :: Proxy SetAlarmState)
