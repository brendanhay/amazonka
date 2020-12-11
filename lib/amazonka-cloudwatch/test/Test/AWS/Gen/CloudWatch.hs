{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--             mkEnableAlarmActions
--
--         , requestDisableInsightRules $
--             mkDisableInsightRules
--
--         , requestPutCompositeAlarm $
--             mkPutCompositeAlarm
--
--         , requestDeleteAnomalyDetector $
--             mkDeleteAnomalyDetector
--
--         , requestDeleteInsightRules $
--             mkDeleteInsightRules
--
--         , requestGetDashboard $
--             mkGetDashboard
--
--         , requestPutAnomalyDetector $
--             mkPutAnomalyDetector
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestGetMetricData $
--             mkGetMetricData
--
--         , requestPutMetricData $
--             mkPutMetricData
--
--         , requestListDashboards $
--             mkListDashboards
--
--         , requestDescribeAlarms $
--             mkDescribeAlarms
--
--         , requestListMetrics $
--             mkListMetrics
--
--         , requestGetInsightRuleReport $
--             mkGetInsightRuleReport
--
--         , requestDeleteDashboards $
--             mkDeleteDashboards
--
--         , requestPutInsightRule $
--             mkPutInsightRule
--
--         , requestGetMetricWidgetImage $
--             mkGetMetricWidgetImage
--
--         , requestDeleteAlarms $
--             mkDeleteAlarms
--
--         , requestDescribeAlarmHistory $
--             mkDescribeAlarmHistory
--
--         , requestGetMetricStatistics $
--             mkGetMetricStatistics
--
--         , requestDescribeAlarmsForMetric $
--             mkDescribeAlarmsForMetric
--
--         , requestEnableInsightRules $
--             mkEnableInsightRules
--
--         , requestDisableAlarmActions $
--             mkDisableAlarmActions
--
--         , requestDescribeAnomalyDetectors $
--             mkDescribeAnomalyDetectors
--
--         , requestPutDashboard $
--             mkPutDashboard
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestPutMetricAlarm $
--             mkPutMetricAlarm
--
--         , requestSetAlarmState $
--             mkSetAlarmState
--
--         , requestDescribeInsightRules $
--             mkDescribeInsightRules
--
--           ]

--     , testGroup "response"
--         [ responseEnableAlarmActions $
--             mkEnableAlarmActionsResponse
--
--         , responseDisableInsightRules $
--             mkDisableInsightRulesResponse
--
--         , responsePutCompositeAlarm $
--             mkPutCompositeAlarmResponse
--
--         , responseDeleteAnomalyDetector $
--             mkDeleteAnomalyDetectorResponse
--
--         , responseDeleteInsightRules $
--             mkDeleteInsightRulesResponse
--
--         , responseGetDashboard $
--             mkGetDashboardResponse
--
--         , responsePutAnomalyDetector $
--             mkPutAnomalyDetectorResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseGetMetricData $
--             mkGetMetricDataResponse
--
--         , responsePutMetricData $
--             mkPutMetricDataResponse
--
--         , responseListDashboards $
--             mkListDashboardsResponse
--
--         , responseDescribeAlarms $
--             mkDescribeAlarmsResponse
--
--         , responseListMetrics $
--             mkListMetricsResponse
--
--         , responseGetInsightRuleReport $
--             mkGetInsightRuleReportResponse
--
--         , responseDeleteDashboards $
--             mkDeleteDashboardsResponse
--
--         , responsePutInsightRule $
--             mkPutInsightRuleResponse
--
--         , responseGetMetricWidgetImage $
--             mkGetMetricWidgetImageResponse
--
--         , responseDeleteAlarms $
--             mkDeleteAlarmsResponse
--
--         , responseDescribeAlarmHistory $
--             mkDescribeAlarmHistoryResponse
--
--         , responseGetMetricStatistics $
--             mkGetMetricStatisticsResponse
--
--         , responseDescribeAlarmsForMetric $
--             mkDescribeAlarmsForMetricResponse
--
--         , responseEnableInsightRules $
--             mkEnableInsightRulesResponse
--
--         , responseDisableAlarmActions $
--             mkDisableAlarmActionsResponse
--
--         , responseDescribeAnomalyDetectors $
--             mkDescribeAnomalyDetectorsResponse
--
--         , responsePutDashboard $
--             mkPutDashboardResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responsePutMetricAlarm $
--             mkPutMetricAlarmResponse
--
--         , responseSetAlarmState $
--             mkSetAlarmStateResponse
--
--         , responseDescribeInsightRules $
--             mkDescribeInsightRulesResponse
--
--           ]
--     ]

-- Requests

requestEnableAlarmActions :: EnableAlarmActions -> TestTree
requestEnableAlarmActions =
  req
    "EnableAlarmActions"
    "fixture/EnableAlarmActions.yaml"

requestDisableInsightRules :: DisableInsightRules -> TestTree
requestDisableInsightRules =
  req
    "DisableInsightRules"
    "fixture/DisableInsightRules.yaml"

requestPutCompositeAlarm :: PutCompositeAlarm -> TestTree
requestPutCompositeAlarm =
  req
    "PutCompositeAlarm"
    "fixture/PutCompositeAlarm.yaml"

requestDeleteAnomalyDetector :: DeleteAnomalyDetector -> TestTree
requestDeleteAnomalyDetector =
  req
    "DeleteAnomalyDetector"
    "fixture/DeleteAnomalyDetector.yaml"

requestDeleteInsightRules :: DeleteInsightRules -> TestTree
requestDeleteInsightRules =
  req
    "DeleteInsightRules"
    "fixture/DeleteInsightRules.yaml"

requestGetDashboard :: GetDashboard -> TestTree
requestGetDashboard =
  req
    "GetDashboard"
    "fixture/GetDashboard.yaml"

requestPutAnomalyDetector :: PutAnomalyDetector -> TestTree
requestPutAnomalyDetector =
  req
    "PutAnomalyDetector"
    "fixture/PutAnomalyDetector.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetMetricData :: GetMetricData -> TestTree
requestGetMetricData =
  req
    "GetMetricData"
    "fixture/GetMetricData.yaml"

requestPutMetricData :: PutMetricData -> TestTree
requestPutMetricData =
  req
    "PutMetricData"
    "fixture/PutMetricData.yaml"

requestListDashboards :: ListDashboards -> TestTree
requestListDashboards =
  req
    "ListDashboards"
    "fixture/ListDashboards.yaml"

requestDescribeAlarms :: DescribeAlarms -> TestTree
requestDescribeAlarms =
  req
    "DescribeAlarms"
    "fixture/DescribeAlarms.yaml"

requestListMetrics :: ListMetrics -> TestTree
requestListMetrics =
  req
    "ListMetrics"
    "fixture/ListMetrics.yaml"

requestGetInsightRuleReport :: GetInsightRuleReport -> TestTree
requestGetInsightRuleReport =
  req
    "GetInsightRuleReport"
    "fixture/GetInsightRuleReport.yaml"

requestDeleteDashboards :: DeleteDashboards -> TestTree
requestDeleteDashboards =
  req
    "DeleteDashboards"
    "fixture/DeleteDashboards.yaml"

requestPutInsightRule :: PutInsightRule -> TestTree
requestPutInsightRule =
  req
    "PutInsightRule"
    "fixture/PutInsightRule.yaml"

requestGetMetricWidgetImage :: GetMetricWidgetImage -> TestTree
requestGetMetricWidgetImage =
  req
    "GetMetricWidgetImage"
    "fixture/GetMetricWidgetImage.yaml"

requestDeleteAlarms :: DeleteAlarms -> TestTree
requestDeleteAlarms =
  req
    "DeleteAlarms"
    "fixture/DeleteAlarms.yaml"

requestDescribeAlarmHistory :: DescribeAlarmHistory -> TestTree
requestDescribeAlarmHistory =
  req
    "DescribeAlarmHistory"
    "fixture/DescribeAlarmHistory.yaml"

requestGetMetricStatistics :: GetMetricStatistics -> TestTree
requestGetMetricStatistics =
  req
    "GetMetricStatistics"
    "fixture/GetMetricStatistics.yaml"

requestDescribeAlarmsForMetric :: DescribeAlarmsForMetric -> TestTree
requestDescribeAlarmsForMetric =
  req
    "DescribeAlarmsForMetric"
    "fixture/DescribeAlarmsForMetric.yaml"

requestEnableInsightRules :: EnableInsightRules -> TestTree
requestEnableInsightRules =
  req
    "EnableInsightRules"
    "fixture/EnableInsightRules.yaml"

requestDisableAlarmActions :: DisableAlarmActions -> TestTree
requestDisableAlarmActions =
  req
    "DisableAlarmActions"
    "fixture/DisableAlarmActions.yaml"

requestDescribeAnomalyDetectors :: DescribeAnomalyDetectors -> TestTree
requestDescribeAnomalyDetectors =
  req
    "DescribeAnomalyDetectors"
    "fixture/DescribeAnomalyDetectors.yaml"

requestPutDashboard :: PutDashboard -> TestTree
requestPutDashboard =
  req
    "PutDashboard"
    "fixture/PutDashboard.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestPutMetricAlarm :: PutMetricAlarm -> TestTree
requestPutMetricAlarm =
  req
    "PutMetricAlarm"
    "fixture/PutMetricAlarm.yaml"

requestSetAlarmState :: SetAlarmState -> TestTree
requestSetAlarmState =
  req
    "SetAlarmState"
    "fixture/SetAlarmState.yaml"

requestDescribeInsightRules :: DescribeInsightRules -> TestTree
requestDescribeInsightRules =
  req
    "DescribeInsightRules"
    "fixture/DescribeInsightRules.yaml"

-- Responses

responseEnableAlarmActions :: EnableAlarmActionsResponse -> TestTree
responseEnableAlarmActions =
  res
    "EnableAlarmActionsResponse"
    "fixture/EnableAlarmActionsResponse.proto"
    cloudWatchService
    (Proxy :: Proxy EnableAlarmActions)

responseDisableInsightRules :: DisableInsightRulesResponse -> TestTree
responseDisableInsightRules =
  res
    "DisableInsightRulesResponse"
    "fixture/DisableInsightRulesResponse.proto"
    cloudWatchService
    (Proxy :: Proxy DisableInsightRules)

responsePutCompositeAlarm :: PutCompositeAlarmResponse -> TestTree
responsePutCompositeAlarm =
  res
    "PutCompositeAlarmResponse"
    "fixture/PutCompositeAlarmResponse.proto"
    cloudWatchService
    (Proxy :: Proxy PutCompositeAlarm)

responseDeleteAnomalyDetector :: DeleteAnomalyDetectorResponse -> TestTree
responseDeleteAnomalyDetector =
  res
    "DeleteAnomalyDetectorResponse"
    "fixture/DeleteAnomalyDetectorResponse.proto"
    cloudWatchService
    (Proxy :: Proxy DeleteAnomalyDetector)

responseDeleteInsightRules :: DeleteInsightRulesResponse -> TestTree
responseDeleteInsightRules =
  res
    "DeleteInsightRulesResponse"
    "fixture/DeleteInsightRulesResponse.proto"
    cloudWatchService
    (Proxy :: Proxy DeleteInsightRules)

responseGetDashboard :: GetDashboardResponse -> TestTree
responseGetDashboard =
  res
    "GetDashboardResponse"
    "fixture/GetDashboardResponse.proto"
    cloudWatchService
    (Proxy :: Proxy GetDashboard)

responsePutAnomalyDetector :: PutAnomalyDetectorResponse -> TestTree
responsePutAnomalyDetector =
  res
    "PutAnomalyDetectorResponse"
    "fixture/PutAnomalyDetectorResponse.proto"
    cloudWatchService
    (Proxy :: Proxy PutAnomalyDetector)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    cloudWatchService
    (Proxy :: Proxy ListTagsForResource)

responseGetMetricData :: GetMetricDataResponse -> TestTree
responseGetMetricData =
  res
    "GetMetricDataResponse"
    "fixture/GetMetricDataResponse.proto"
    cloudWatchService
    (Proxy :: Proxy GetMetricData)

responsePutMetricData :: PutMetricDataResponse -> TestTree
responsePutMetricData =
  res
    "PutMetricDataResponse"
    "fixture/PutMetricDataResponse.proto"
    cloudWatchService
    (Proxy :: Proxy PutMetricData)

responseListDashboards :: ListDashboardsResponse -> TestTree
responseListDashboards =
  res
    "ListDashboardsResponse"
    "fixture/ListDashboardsResponse.proto"
    cloudWatchService
    (Proxy :: Proxy ListDashboards)

responseDescribeAlarms :: DescribeAlarmsResponse -> TestTree
responseDescribeAlarms =
  res
    "DescribeAlarmsResponse"
    "fixture/DescribeAlarmsResponse.proto"
    cloudWatchService
    (Proxy :: Proxy DescribeAlarms)

responseListMetrics :: ListMetricsResponse -> TestTree
responseListMetrics =
  res
    "ListMetricsResponse"
    "fixture/ListMetricsResponse.proto"
    cloudWatchService
    (Proxy :: Proxy ListMetrics)

responseGetInsightRuleReport :: GetInsightRuleReportResponse -> TestTree
responseGetInsightRuleReport =
  res
    "GetInsightRuleReportResponse"
    "fixture/GetInsightRuleReportResponse.proto"
    cloudWatchService
    (Proxy :: Proxy GetInsightRuleReport)

responseDeleteDashboards :: DeleteDashboardsResponse -> TestTree
responseDeleteDashboards =
  res
    "DeleteDashboardsResponse"
    "fixture/DeleteDashboardsResponse.proto"
    cloudWatchService
    (Proxy :: Proxy DeleteDashboards)

responsePutInsightRule :: PutInsightRuleResponse -> TestTree
responsePutInsightRule =
  res
    "PutInsightRuleResponse"
    "fixture/PutInsightRuleResponse.proto"
    cloudWatchService
    (Proxy :: Proxy PutInsightRule)

responseGetMetricWidgetImage :: GetMetricWidgetImageResponse -> TestTree
responseGetMetricWidgetImage =
  res
    "GetMetricWidgetImageResponse"
    "fixture/GetMetricWidgetImageResponse.proto"
    cloudWatchService
    (Proxy :: Proxy GetMetricWidgetImage)

responseDeleteAlarms :: DeleteAlarmsResponse -> TestTree
responseDeleteAlarms =
  res
    "DeleteAlarmsResponse"
    "fixture/DeleteAlarmsResponse.proto"
    cloudWatchService
    (Proxy :: Proxy DeleteAlarms)

responseDescribeAlarmHistory :: DescribeAlarmHistoryResponse -> TestTree
responseDescribeAlarmHistory =
  res
    "DescribeAlarmHistoryResponse"
    "fixture/DescribeAlarmHistoryResponse.proto"
    cloudWatchService
    (Proxy :: Proxy DescribeAlarmHistory)

responseGetMetricStatistics :: GetMetricStatisticsResponse -> TestTree
responseGetMetricStatistics =
  res
    "GetMetricStatisticsResponse"
    "fixture/GetMetricStatisticsResponse.proto"
    cloudWatchService
    (Proxy :: Proxy GetMetricStatistics)

responseDescribeAlarmsForMetric :: DescribeAlarmsForMetricResponse -> TestTree
responseDescribeAlarmsForMetric =
  res
    "DescribeAlarmsForMetricResponse"
    "fixture/DescribeAlarmsForMetricResponse.proto"
    cloudWatchService
    (Proxy :: Proxy DescribeAlarmsForMetric)

responseEnableInsightRules :: EnableInsightRulesResponse -> TestTree
responseEnableInsightRules =
  res
    "EnableInsightRulesResponse"
    "fixture/EnableInsightRulesResponse.proto"
    cloudWatchService
    (Proxy :: Proxy EnableInsightRules)

responseDisableAlarmActions :: DisableAlarmActionsResponse -> TestTree
responseDisableAlarmActions =
  res
    "DisableAlarmActionsResponse"
    "fixture/DisableAlarmActionsResponse.proto"
    cloudWatchService
    (Proxy :: Proxy DisableAlarmActions)

responseDescribeAnomalyDetectors :: DescribeAnomalyDetectorsResponse -> TestTree
responseDescribeAnomalyDetectors =
  res
    "DescribeAnomalyDetectorsResponse"
    "fixture/DescribeAnomalyDetectorsResponse.proto"
    cloudWatchService
    (Proxy :: Proxy DescribeAnomalyDetectors)

responsePutDashboard :: PutDashboardResponse -> TestTree
responsePutDashboard =
  res
    "PutDashboardResponse"
    "fixture/PutDashboardResponse.proto"
    cloudWatchService
    (Proxy :: Proxy PutDashboard)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    cloudWatchService
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    cloudWatchService
    (Proxy :: Proxy UntagResource)

responsePutMetricAlarm :: PutMetricAlarmResponse -> TestTree
responsePutMetricAlarm =
  res
    "PutMetricAlarmResponse"
    "fixture/PutMetricAlarmResponse.proto"
    cloudWatchService
    (Proxy :: Proxy PutMetricAlarm)

responseSetAlarmState :: SetAlarmStateResponse -> TestTree
responseSetAlarmState =
  res
    "SetAlarmStateResponse"
    "fixture/SetAlarmStateResponse.proto"
    cloudWatchService
    (Proxy :: Proxy SetAlarmState)

responseDescribeInsightRules :: DescribeInsightRulesResponse -> TestTree
responseDescribeInsightRules =
  res
    "DescribeInsightRulesResponse"
    "fixture/DescribeInsightRulesResponse.proto"
    cloudWatchService
    (Proxy :: Proxy DescribeInsightRules)
