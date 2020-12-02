{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudWatch where

import Data.Proxy
import Network.AWS.CloudWatch
import Test.AWS.CloudWatch.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestEnableAlarmActions $
--             enableAlarmActions
--
--         , requestGetDashboard $
--             getDashboard
--
--         , requestGetMetricData $
--             getMetricData
--
--         , requestPutMetricData $
--             putMetricData
--
--         , requestListDashboards $
--             listDashboards
--
--         , requestDescribeAlarms $
--             describeAlarms
--
--         , requestListMetrics $
--             listMetrics
--
--         , requestDeleteDashboards $
--             deleteDashboards
--
--         , requestDeleteAlarms $
--             deleteAlarms
--
--         , requestDescribeAlarmHistory $
--             describeAlarmHistory
--
--         , requestGetMetricStatistics $
--             getMetricStatistics
--
--         , requestDescribeAlarmsForMetric $
--             describeAlarmsForMetric
--
--         , requestDisableAlarmActions $
--             disableAlarmActions
--
--         , requestPutDashboard $
--             putDashboard
--
--         , requestPutMetricAlarm $
--             putMetricAlarm
--
--         , requestSetAlarmState $
--             setAlarmState
--
--           ]

--     , testGroup "response"
--         [ responseEnableAlarmActions $
--             enableAlarmActionsResponse
--
--         , responseGetDashboard $
--             getDashboardResponse
--
--         , responseGetMetricData $
--             getMetricDataResponse
--
--         , responsePutMetricData $
--             putMetricDataResponse
--
--         , responseListDashboards $
--             listDashboardsResponse
--
--         , responseDescribeAlarms $
--             describeAlarmsResponse
--
--         , responseListMetrics $
--             listMetricsResponse
--
--         , responseDeleteDashboards $
--             deleteDashboardsResponse
--
--         , responseDeleteAlarms $
--             deleteAlarmsResponse
--
--         , responseDescribeAlarmHistory $
--             describeAlarmHistoryResponse
--
--         , responseGetMetricStatistics $
--             getMetricStatisticsResponse
--
--         , responseDescribeAlarmsForMetric $
--             describeAlarmsForMetricResponse
--
--         , responseDisableAlarmActions $
--             disableAlarmActionsResponse
--
--         , responsePutDashboard $
--             putDashboardResponse
--
--         , responsePutMetricAlarm $
--             putMetricAlarmResponse
--
--         , responseSetAlarmState $
--             setAlarmStateResponse
--
--           ]
--     ]

-- Requests

requestEnableAlarmActions :: EnableAlarmActions -> TestTree
requestEnableAlarmActions = req
    "EnableAlarmActions"
    "fixture/EnableAlarmActions.yaml"

requestGetDashboard :: GetDashboard -> TestTree
requestGetDashboard = req
    "GetDashboard"
    "fixture/GetDashboard.yaml"

requestGetMetricData :: GetMetricData -> TestTree
requestGetMetricData = req
    "GetMetricData"
    "fixture/GetMetricData.yaml"

requestPutMetricData :: PutMetricData -> TestTree
requestPutMetricData = req
    "PutMetricData"
    "fixture/PutMetricData.yaml"

requestListDashboards :: ListDashboards -> TestTree
requestListDashboards = req
    "ListDashboards"
    "fixture/ListDashboards.yaml"

requestDescribeAlarms :: DescribeAlarms -> TestTree
requestDescribeAlarms = req
    "DescribeAlarms"
    "fixture/DescribeAlarms.yaml"

requestListMetrics :: ListMetrics -> TestTree
requestListMetrics = req
    "ListMetrics"
    "fixture/ListMetrics.yaml"

requestDeleteDashboards :: DeleteDashboards -> TestTree
requestDeleteDashboards = req
    "DeleteDashboards"
    "fixture/DeleteDashboards.yaml"

requestDeleteAlarms :: DeleteAlarms -> TestTree
requestDeleteAlarms = req
    "DeleteAlarms"
    "fixture/DeleteAlarms.yaml"

requestDescribeAlarmHistory :: DescribeAlarmHistory -> TestTree
requestDescribeAlarmHistory = req
    "DescribeAlarmHistory"
    "fixture/DescribeAlarmHistory.yaml"

requestGetMetricStatistics :: GetMetricStatistics -> TestTree
requestGetMetricStatistics = req
    "GetMetricStatistics"
    "fixture/GetMetricStatistics.yaml"

requestDescribeAlarmsForMetric :: DescribeAlarmsForMetric -> TestTree
requestDescribeAlarmsForMetric = req
    "DescribeAlarmsForMetric"
    "fixture/DescribeAlarmsForMetric.yaml"

requestDisableAlarmActions :: DisableAlarmActions -> TestTree
requestDisableAlarmActions = req
    "DisableAlarmActions"
    "fixture/DisableAlarmActions.yaml"

requestPutDashboard :: PutDashboard -> TestTree
requestPutDashboard = req
    "PutDashboard"
    "fixture/PutDashboard.yaml"

requestPutMetricAlarm :: PutMetricAlarm -> TestTree
requestPutMetricAlarm = req
    "PutMetricAlarm"
    "fixture/PutMetricAlarm.yaml"

requestSetAlarmState :: SetAlarmState -> TestTree
requestSetAlarmState = req
    "SetAlarmState"
    "fixture/SetAlarmState.yaml"

-- Responses

responseEnableAlarmActions :: EnableAlarmActionsResponse -> TestTree
responseEnableAlarmActions = res
    "EnableAlarmActionsResponse"
    "fixture/EnableAlarmActionsResponse.proto"
    cloudWatch
    (Proxy :: Proxy EnableAlarmActions)

responseGetDashboard :: GetDashboardResponse -> TestTree
responseGetDashboard = res
    "GetDashboardResponse"
    "fixture/GetDashboardResponse.proto"
    cloudWatch
    (Proxy :: Proxy GetDashboard)

responseGetMetricData :: GetMetricDataResponse -> TestTree
responseGetMetricData = res
    "GetMetricDataResponse"
    "fixture/GetMetricDataResponse.proto"
    cloudWatch
    (Proxy :: Proxy GetMetricData)

responsePutMetricData :: PutMetricDataResponse -> TestTree
responsePutMetricData = res
    "PutMetricDataResponse"
    "fixture/PutMetricDataResponse.proto"
    cloudWatch
    (Proxy :: Proxy PutMetricData)

responseListDashboards :: ListDashboardsResponse -> TestTree
responseListDashboards = res
    "ListDashboardsResponse"
    "fixture/ListDashboardsResponse.proto"
    cloudWatch
    (Proxy :: Proxy ListDashboards)

responseDescribeAlarms :: DescribeAlarmsResponse -> TestTree
responseDescribeAlarms = res
    "DescribeAlarmsResponse"
    "fixture/DescribeAlarmsResponse.proto"
    cloudWatch
    (Proxy :: Proxy DescribeAlarms)

responseListMetrics :: ListMetricsResponse -> TestTree
responseListMetrics = res
    "ListMetricsResponse"
    "fixture/ListMetricsResponse.proto"
    cloudWatch
    (Proxy :: Proxy ListMetrics)

responseDeleteDashboards :: DeleteDashboardsResponse -> TestTree
responseDeleteDashboards = res
    "DeleteDashboardsResponse"
    "fixture/DeleteDashboardsResponse.proto"
    cloudWatch
    (Proxy :: Proxy DeleteDashboards)

responseDeleteAlarms :: DeleteAlarmsResponse -> TestTree
responseDeleteAlarms = res
    "DeleteAlarmsResponse"
    "fixture/DeleteAlarmsResponse.proto"
    cloudWatch
    (Proxy :: Proxy DeleteAlarms)

responseDescribeAlarmHistory :: DescribeAlarmHistoryResponse -> TestTree
responseDescribeAlarmHistory = res
    "DescribeAlarmHistoryResponse"
    "fixture/DescribeAlarmHistoryResponse.proto"
    cloudWatch
    (Proxy :: Proxy DescribeAlarmHistory)

responseGetMetricStatistics :: GetMetricStatisticsResponse -> TestTree
responseGetMetricStatistics = res
    "GetMetricStatisticsResponse"
    "fixture/GetMetricStatisticsResponse.proto"
    cloudWatch
    (Proxy :: Proxy GetMetricStatistics)

responseDescribeAlarmsForMetric :: DescribeAlarmsForMetricResponse -> TestTree
responseDescribeAlarmsForMetric = res
    "DescribeAlarmsForMetricResponse"
    "fixture/DescribeAlarmsForMetricResponse.proto"
    cloudWatch
    (Proxy :: Proxy DescribeAlarmsForMetric)

responseDisableAlarmActions :: DisableAlarmActionsResponse -> TestTree
responseDisableAlarmActions = res
    "DisableAlarmActionsResponse"
    "fixture/DisableAlarmActionsResponse.proto"
    cloudWatch
    (Proxy :: Proxy DisableAlarmActions)

responsePutDashboard :: PutDashboardResponse -> TestTree
responsePutDashboard = res
    "PutDashboardResponse"
    "fixture/PutDashboardResponse.proto"
    cloudWatch
    (Proxy :: Proxy PutDashboard)

responsePutMetricAlarm :: PutMetricAlarmResponse -> TestTree
responsePutMetricAlarm = res
    "PutMetricAlarmResponse"
    "fixture/PutMetricAlarmResponse.proto"
    cloudWatch
    (Proxy :: Proxy PutMetricAlarm)

responseSetAlarmState :: SetAlarmStateResponse -> TestTree
responseSetAlarmState = res
    "SetAlarmStateResponse"
    "fixture/SetAlarmStateResponse.proto"
    cloudWatch
    (Proxy :: Proxy SetAlarmState)
