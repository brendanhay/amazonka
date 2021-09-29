{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatch
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestGetMetricStatistics $
--             newGetMetricStatistics
--
--         , requestEnableAlarmActions $
--             newEnableAlarmActions
--
--         , requestGetMetricWidgetImage $
--             newGetMetricWidgetImage
--
--         , requestPutInsightRule $
--             newPutInsightRule
--
--         , requestDeleteMetricStream $
--             newDeleteMetricStream
--
--         , requestDeleteAlarms $
--             newDeleteAlarms
--
--         , requestPutMetricAlarm $
--             newPutMetricAlarm
--
--         , requestDescribeInsightRules $
--             newDescribeInsightRules
--
--         , requestStartMetricStreams $
--             newStartMetricStreams
--
--         , requestStopMetricStreams $
--             newStopMetricStreams
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetInsightRuleReport $
--             newGetInsightRuleReport
--
--         , requestDescribeAnomalyDetectors $
--             newDescribeAnomalyDetectors
--
--         , requestPutMetricData $
--             newPutMetricData
--
--         , requestListMetrics $
--             newListMetrics
--
--         , requestPutDashboard $
--             newPutDashboard
--
--         , requestDeleteInsightRules $
--             newDeleteInsightRules
--
--         , requestGetDashboard $
--             newGetDashboard
--
--         , requestDescribeAlarmsForMetric $
--             newDescribeAlarmsForMetric
--
--         , requestDisableAlarmActions $
--             newDisableAlarmActions
--
--         , requestPutAnomalyDetector $
--             newPutAnomalyDetector
--
--         , requestDisableInsightRules $
--             newDisableInsightRules
--
--         , requestPutCompositeAlarm $
--             newPutCompositeAlarm
--
--         , requestDescribeAlarmHistory $
--             newDescribeAlarmHistory
--
--         , requestPutMetricStream $
--             newPutMetricStream
--
--         , requestListMetricStreams $
--             newListMetricStreams
--
--         , requestGetMetricStream $
--             newGetMetricStream
--
--         , requestSetAlarmState $
--             newSetAlarmState
--
--         , requestDeleteDashboards $
--             newDeleteDashboards
--
--         , requestListDashboards $
--             newListDashboards
--
--         , requestDescribeAlarms $
--             newDescribeAlarms
--
--         , requestEnableInsightRules $
--             newEnableInsightRules
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteAnomalyDetector $
--             newDeleteAnomalyDetector
--
--         , requestGetMetricData $
--             newGetMetricData
--
--           ]

--     , testGroup "response"
--         [ responseGetMetricStatistics $
--             newGetMetricStatisticsResponse
--
--         , responseEnableAlarmActions $
--             newEnableAlarmActionsResponse
--
--         , responseGetMetricWidgetImage $
--             newGetMetricWidgetImageResponse
--
--         , responsePutInsightRule $
--             newPutInsightRuleResponse
--
--         , responseDeleteMetricStream $
--             newDeleteMetricStreamResponse
--
--         , responseDeleteAlarms $
--             newDeleteAlarmsResponse
--
--         , responsePutMetricAlarm $
--             newPutMetricAlarmResponse
--
--         , responseDescribeInsightRules $
--             newDescribeInsightRulesResponse
--
--         , responseStartMetricStreams $
--             newStartMetricStreamsResponse
--
--         , responseStopMetricStreams $
--             newStopMetricStreamsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetInsightRuleReport $
--             newGetInsightRuleReportResponse
--
--         , responseDescribeAnomalyDetectors $
--             newDescribeAnomalyDetectorsResponse
--
--         , responsePutMetricData $
--             newPutMetricDataResponse
--
--         , responseListMetrics $
--             newListMetricsResponse
--
--         , responsePutDashboard $
--             newPutDashboardResponse
--
--         , responseDeleteInsightRules $
--             newDeleteInsightRulesResponse
--
--         , responseGetDashboard $
--             newGetDashboardResponse
--
--         , responseDescribeAlarmsForMetric $
--             newDescribeAlarmsForMetricResponse
--
--         , responseDisableAlarmActions $
--             newDisableAlarmActionsResponse
--
--         , responsePutAnomalyDetector $
--             newPutAnomalyDetectorResponse
--
--         , responseDisableInsightRules $
--             newDisableInsightRulesResponse
--
--         , responsePutCompositeAlarm $
--             newPutCompositeAlarmResponse
--
--         , responseDescribeAlarmHistory $
--             newDescribeAlarmHistoryResponse
--
--         , responsePutMetricStream $
--             newPutMetricStreamResponse
--
--         , responseListMetricStreams $
--             newListMetricStreamsResponse
--
--         , responseGetMetricStream $
--             newGetMetricStreamResponse
--
--         , responseSetAlarmState $
--             newSetAlarmStateResponse
--
--         , responseDeleteDashboards $
--             newDeleteDashboardsResponse
--
--         , responseListDashboards $
--             newListDashboardsResponse
--
--         , responseDescribeAlarms $
--             newDescribeAlarmsResponse
--
--         , responseEnableInsightRules $
--             newEnableInsightRulesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteAnomalyDetector $
--             newDeleteAnomalyDetectorResponse
--
--         , responseGetMetricData $
--             newGetMetricDataResponse
--
--           ]
--     ]

-- Requests

requestGetMetricStatistics :: GetMetricStatistics -> TestTree
requestGetMetricStatistics =
  req
    "GetMetricStatistics"
    "fixture/GetMetricStatistics.yaml"

requestEnableAlarmActions :: EnableAlarmActions -> TestTree
requestEnableAlarmActions =
  req
    "EnableAlarmActions"
    "fixture/EnableAlarmActions.yaml"

requestGetMetricWidgetImage :: GetMetricWidgetImage -> TestTree
requestGetMetricWidgetImage =
  req
    "GetMetricWidgetImage"
    "fixture/GetMetricWidgetImage.yaml"

requestPutInsightRule :: PutInsightRule -> TestTree
requestPutInsightRule =
  req
    "PutInsightRule"
    "fixture/PutInsightRule.yaml"

requestDeleteMetricStream :: DeleteMetricStream -> TestTree
requestDeleteMetricStream =
  req
    "DeleteMetricStream"
    "fixture/DeleteMetricStream.yaml"

requestDeleteAlarms :: DeleteAlarms -> TestTree
requestDeleteAlarms =
  req
    "DeleteAlarms"
    "fixture/DeleteAlarms.yaml"

requestPutMetricAlarm :: PutMetricAlarm -> TestTree
requestPutMetricAlarm =
  req
    "PutMetricAlarm"
    "fixture/PutMetricAlarm.yaml"

requestDescribeInsightRules :: DescribeInsightRules -> TestTree
requestDescribeInsightRules =
  req
    "DescribeInsightRules"
    "fixture/DescribeInsightRules.yaml"

requestStartMetricStreams :: StartMetricStreams -> TestTree
requestStartMetricStreams =
  req
    "StartMetricStreams"
    "fixture/StartMetricStreams.yaml"

requestStopMetricStreams :: StopMetricStreams -> TestTree
requestStopMetricStreams =
  req
    "StopMetricStreams"
    "fixture/StopMetricStreams.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetInsightRuleReport :: GetInsightRuleReport -> TestTree
requestGetInsightRuleReport =
  req
    "GetInsightRuleReport"
    "fixture/GetInsightRuleReport.yaml"

requestDescribeAnomalyDetectors :: DescribeAnomalyDetectors -> TestTree
requestDescribeAnomalyDetectors =
  req
    "DescribeAnomalyDetectors"
    "fixture/DescribeAnomalyDetectors.yaml"

requestPutMetricData :: PutMetricData -> TestTree
requestPutMetricData =
  req
    "PutMetricData"
    "fixture/PutMetricData.yaml"

requestListMetrics :: ListMetrics -> TestTree
requestListMetrics =
  req
    "ListMetrics"
    "fixture/ListMetrics.yaml"

requestPutDashboard :: PutDashboard -> TestTree
requestPutDashboard =
  req
    "PutDashboard"
    "fixture/PutDashboard.yaml"

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

requestDescribeAlarmsForMetric :: DescribeAlarmsForMetric -> TestTree
requestDescribeAlarmsForMetric =
  req
    "DescribeAlarmsForMetric"
    "fixture/DescribeAlarmsForMetric.yaml"

requestDisableAlarmActions :: DisableAlarmActions -> TestTree
requestDisableAlarmActions =
  req
    "DisableAlarmActions"
    "fixture/DisableAlarmActions.yaml"

requestPutAnomalyDetector :: PutAnomalyDetector -> TestTree
requestPutAnomalyDetector =
  req
    "PutAnomalyDetector"
    "fixture/PutAnomalyDetector.yaml"

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

requestDescribeAlarmHistory :: DescribeAlarmHistory -> TestTree
requestDescribeAlarmHistory =
  req
    "DescribeAlarmHistory"
    "fixture/DescribeAlarmHistory.yaml"

requestPutMetricStream :: PutMetricStream -> TestTree
requestPutMetricStream =
  req
    "PutMetricStream"
    "fixture/PutMetricStream.yaml"

requestListMetricStreams :: ListMetricStreams -> TestTree
requestListMetricStreams =
  req
    "ListMetricStreams"
    "fixture/ListMetricStreams.yaml"

requestGetMetricStream :: GetMetricStream -> TestTree
requestGetMetricStream =
  req
    "GetMetricStream"
    "fixture/GetMetricStream.yaml"

requestSetAlarmState :: SetAlarmState -> TestTree
requestSetAlarmState =
  req
    "SetAlarmState"
    "fixture/SetAlarmState.yaml"

requestDeleteDashboards :: DeleteDashboards -> TestTree
requestDeleteDashboards =
  req
    "DeleteDashboards"
    "fixture/DeleteDashboards.yaml"

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

requestEnableInsightRules :: EnableInsightRules -> TestTree
requestEnableInsightRules =
  req
    "EnableInsightRules"
    "fixture/EnableInsightRules.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteAnomalyDetector :: DeleteAnomalyDetector -> TestTree
requestDeleteAnomalyDetector =
  req
    "DeleteAnomalyDetector"
    "fixture/DeleteAnomalyDetector.yaml"

requestGetMetricData :: GetMetricData -> TestTree
requestGetMetricData =
  req
    "GetMetricData"
    "fixture/GetMetricData.yaml"

-- Responses

responseGetMetricStatistics :: GetMetricStatisticsResponse -> TestTree
responseGetMetricStatistics =
  res
    "GetMetricStatisticsResponse"
    "fixture/GetMetricStatisticsResponse.proto"
    defaultService
    (Proxy :: Proxy GetMetricStatistics)

responseEnableAlarmActions :: EnableAlarmActionsResponse -> TestTree
responseEnableAlarmActions =
  res
    "EnableAlarmActionsResponse"
    "fixture/EnableAlarmActionsResponse.proto"
    defaultService
    (Proxy :: Proxy EnableAlarmActions)

responseGetMetricWidgetImage :: GetMetricWidgetImageResponse -> TestTree
responseGetMetricWidgetImage =
  res
    "GetMetricWidgetImageResponse"
    "fixture/GetMetricWidgetImageResponse.proto"
    defaultService
    (Proxy :: Proxy GetMetricWidgetImage)

responsePutInsightRule :: PutInsightRuleResponse -> TestTree
responsePutInsightRule =
  res
    "PutInsightRuleResponse"
    "fixture/PutInsightRuleResponse.proto"
    defaultService
    (Proxy :: Proxy PutInsightRule)

responseDeleteMetricStream :: DeleteMetricStreamResponse -> TestTree
responseDeleteMetricStream =
  res
    "DeleteMetricStreamResponse"
    "fixture/DeleteMetricStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMetricStream)

responseDeleteAlarms :: DeleteAlarmsResponse -> TestTree
responseDeleteAlarms =
  res
    "DeleteAlarmsResponse"
    "fixture/DeleteAlarmsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAlarms)

responsePutMetricAlarm :: PutMetricAlarmResponse -> TestTree
responsePutMetricAlarm =
  res
    "PutMetricAlarmResponse"
    "fixture/PutMetricAlarmResponse.proto"
    defaultService
    (Proxy :: Proxy PutMetricAlarm)

responseDescribeInsightRules :: DescribeInsightRulesResponse -> TestTree
responseDescribeInsightRules =
  res
    "DescribeInsightRulesResponse"
    "fixture/DescribeInsightRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInsightRules)

responseStartMetricStreams :: StartMetricStreamsResponse -> TestTree
responseStartMetricStreams =
  res
    "StartMetricStreamsResponse"
    "fixture/StartMetricStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy StartMetricStreams)

responseStopMetricStreams :: StopMetricStreamsResponse -> TestTree
responseStopMetricStreams =
  res
    "StopMetricStreamsResponse"
    "fixture/StopMetricStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy StopMetricStreams)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetInsightRuleReport :: GetInsightRuleReportResponse -> TestTree
responseGetInsightRuleReport =
  res
    "GetInsightRuleReportResponse"
    "fixture/GetInsightRuleReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetInsightRuleReport)

responseDescribeAnomalyDetectors :: DescribeAnomalyDetectorsResponse -> TestTree
responseDescribeAnomalyDetectors =
  res
    "DescribeAnomalyDetectorsResponse"
    "fixture/DescribeAnomalyDetectorsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAnomalyDetectors)

responsePutMetricData :: PutMetricDataResponse -> TestTree
responsePutMetricData =
  res
    "PutMetricDataResponse"
    "fixture/PutMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy PutMetricData)

responseListMetrics :: ListMetricsResponse -> TestTree
responseListMetrics =
  res
    "ListMetricsResponse"
    "fixture/ListMetricsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMetrics)

responsePutDashboard :: PutDashboardResponse -> TestTree
responsePutDashboard =
  res
    "PutDashboardResponse"
    "fixture/PutDashboardResponse.proto"
    defaultService
    (Proxy :: Proxy PutDashboard)

responseDeleteInsightRules :: DeleteInsightRulesResponse -> TestTree
responseDeleteInsightRules =
  res
    "DeleteInsightRulesResponse"
    "fixture/DeleteInsightRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInsightRules)

responseGetDashboard :: GetDashboardResponse -> TestTree
responseGetDashboard =
  res
    "GetDashboardResponse"
    "fixture/GetDashboardResponse.proto"
    defaultService
    (Proxy :: Proxy GetDashboard)

responseDescribeAlarmsForMetric :: DescribeAlarmsForMetricResponse -> TestTree
responseDescribeAlarmsForMetric =
  res
    "DescribeAlarmsForMetricResponse"
    "fixture/DescribeAlarmsForMetricResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAlarmsForMetric)

responseDisableAlarmActions :: DisableAlarmActionsResponse -> TestTree
responseDisableAlarmActions =
  res
    "DisableAlarmActionsResponse"
    "fixture/DisableAlarmActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DisableAlarmActions)

responsePutAnomalyDetector :: PutAnomalyDetectorResponse -> TestTree
responsePutAnomalyDetector =
  res
    "PutAnomalyDetectorResponse"
    "fixture/PutAnomalyDetectorResponse.proto"
    defaultService
    (Proxy :: Proxy PutAnomalyDetector)

responseDisableInsightRules :: DisableInsightRulesResponse -> TestTree
responseDisableInsightRules =
  res
    "DisableInsightRulesResponse"
    "fixture/DisableInsightRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DisableInsightRules)

responsePutCompositeAlarm :: PutCompositeAlarmResponse -> TestTree
responsePutCompositeAlarm =
  res
    "PutCompositeAlarmResponse"
    "fixture/PutCompositeAlarmResponse.proto"
    defaultService
    (Proxy :: Proxy PutCompositeAlarm)

responseDescribeAlarmHistory :: DescribeAlarmHistoryResponse -> TestTree
responseDescribeAlarmHistory =
  res
    "DescribeAlarmHistoryResponse"
    "fixture/DescribeAlarmHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAlarmHistory)

responsePutMetricStream :: PutMetricStreamResponse -> TestTree
responsePutMetricStream =
  res
    "PutMetricStreamResponse"
    "fixture/PutMetricStreamResponse.proto"
    defaultService
    (Proxy :: Proxy PutMetricStream)

responseListMetricStreams :: ListMetricStreamsResponse -> TestTree
responseListMetricStreams =
  res
    "ListMetricStreamsResponse"
    "fixture/ListMetricStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMetricStreams)

responseGetMetricStream :: GetMetricStreamResponse -> TestTree
responseGetMetricStream =
  res
    "GetMetricStreamResponse"
    "fixture/GetMetricStreamResponse.proto"
    defaultService
    (Proxy :: Proxy GetMetricStream)

responseSetAlarmState :: SetAlarmStateResponse -> TestTree
responseSetAlarmState =
  res
    "SetAlarmStateResponse"
    "fixture/SetAlarmStateResponse.proto"
    defaultService
    (Proxy :: Proxy SetAlarmState)

responseDeleteDashboards :: DeleteDashboardsResponse -> TestTree
responseDeleteDashboards =
  res
    "DeleteDashboardsResponse"
    "fixture/DeleteDashboardsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDashboards)

responseListDashboards :: ListDashboardsResponse -> TestTree
responseListDashboards =
  res
    "ListDashboardsResponse"
    "fixture/ListDashboardsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDashboards)

responseDescribeAlarms :: DescribeAlarmsResponse -> TestTree
responseDescribeAlarms =
  res
    "DescribeAlarmsResponse"
    "fixture/DescribeAlarmsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAlarms)

responseEnableInsightRules :: EnableInsightRulesResponse -> TestTree
responseEnableInsightRules =
  res
    "EnableInsightRulesResponse"
    "fixture/EnableInsightRulesResponse.proto"
    defaultService
    (Proxy :: Proxy EnableInsightRules)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDeleteAnomalyDetector :: DeleteAnomalyDetectorResponse -> TestTree
responseDeleteAnomalyDetector =
  res
    "DeleteAnomalyDetectorResponse"
    "fixture/DeleteAnomalyDetectorResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAnomalyDetector)

responseGetMetricData :: GetMetricDataResponse -> TestTree
responseGetMetricData =
  res
    "GetMetricDataResponse"
    "fixture/GetMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetMetricData)
