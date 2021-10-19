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
--         [ requestEnableAlarmActions $
--             newEnableAlarmActions
--
--         , requestDisableInsightRules $
--             newDisableInsightRules
--
--         , requestPutCompositeAlarm $
--             newPutCompositeAlarm
--
--         , requestDeleteAnomalyDetector $
--             newDeleteAnomalyDetector
--
--         , requestDeleteInsightRules $
--             newDeleteInsightRules
--
--         , requestGetDashboard $
--             newGetDashboard
--
--         , requestPutAnomalyDetector $
--             newPutAnomalyDetector
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetMetricData $
--             newGetMetricData
--
--         , requestPutMetricData $
--             newPutMetricData
--
--         , requestListDashboards $
--             newListDashboards
--
--         , requestDescribeAlarms $
--             newDescribeAlarms
--
--         , requestListMetrics $
--             newListMetrics
--
--         , requestGetInsightRuleReport $
--             newGetInsightRuleReport
--
--         , requestStartMetricStreams $
--             newStartMetricStreams
--
--         , requestDeleteDashboards $
--             newDeleteDashboards
--
--         , requestPutInsightRule $
--             newPutInsightRule
--
--         , requestListMetricStreams $
--             newListMetricStreams
--
--         , requestGetMetricWidgetImage $
--             newGetMetricWidgetImage
--
--         , requestDeleteMetricStream $
--             newDeleteMetricStream
--
--         , requestDeleteAlarms $
--             newDeleteAlarms
--
--         , requestPutMetricStream $
--             newPutMetricStream
--
--         , requestDescribeAlarmHistory $
--             newDescribeAlarmHistory
--
--         , requestGetMetricStatistics $
--             newGetMetricStatistics
--
--         , requestDescribeAlarmsForMetric $
--             newDescribeAlarmsForMetric
--
--         , requestEnableInsightRules $
--             newEnableInsightRules
--
--         , requestDisableAlarmActions $
--             newDisableAlarmActions
--
--         , requestDescribeAnomalyDetectors $
--             newDescribeAnomalyDetectors
--
--         , requestPutDashboard $
--             newPutDashboard
--
--         , requestTagResource $
--             newTagResource
--
--         , requestStopMetricStreams $
--             newStopMetricStreams
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetMetricStream $
--             newGetMetricStream
--
--         , requestPutMetricAlarm $
--             newPutMetricAlarm
--
--         , requestSetAlarmState $
--             newSetAlarmState
--
--         , requestDescribeInsightRules $
--             newDescribeInsightRules
--
--           ]

--     , testGroup "response"
--         [ responseEnableAlarmActions $
--             newEnableAlarmActionsResponse
--
--         , responseDisableInsightRules $
--             newDisableInsightRulesResponse
--
--         , responsePutCompositeAlarm $
--             newPutCompositeAlarmResponse
--
--         , responseDeleteAnomalyDetector $
--             newDeleteAnomalyDetectorResponse
--
--         , responseDeleteInsightRules $
--             newDeleteInsightRulesResponse
--
--         , responseGetDashboard $
--             newGetDashboardResponse
--
--         , responsePutAnomalyDetector $
--             newPutAnomalyDetectorResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetMetricData $
--             newGetMetricDataResponse
--
--         , responsePutMetricData $
--             newPutMetricDataResponse
--
--         , responseListDashboards $
--             newListDashboardsResponse
--
--         , responseDescribeAlarms $
--             newDescribeAlarmsResponse
--
--         , responseListMetrics $
--             newListMetricsResponse
--
--         , responseGetInsightRuleReport $
--             newGetInsightRuleReportResponse
--
--         , responseStartMetricStreams $
--             newStartMetricStreamsResponse
--
--         , responseDeleteDashboards $
--             newDeleteDashboardsResponse
--
--         , responsePutInsightRule $
--             newPutInsightRuleResponse
--
--         , responseListMetricStreams $
--             newListMetricStreamsResponse
--
--         , responseGetMetricWidgetImage $
--             newGetMetricWidgetImageResponse
--
--         , responseDeleteMetricStream $
--             newDeleteMetricStreamResponse
--
--         , responseDeleteAlarms $
--             newDeleteAlarmsResponse
--
--         , responsePutMetricStream $
--             newPutMetricStreamResponse
--
--         , responseDescribeAlarmHistory $
--             newDescribeAlarmHistoryResponse
--
--         , responseGetMetricStatistics $
--             newGetMetricStatisticsResponse
--
--         , responseDescribeAlarmsForMetric $
--             newDescribeAlarmsForMetricResponse
--
--         , responseEnableInsightRules $
--             newEnableInsightRulesResponse
--
--         , responseDisableAlarmActions $
--             newDisableAlarmActionsResponse
--
--         , responseDescribeAnomalyDetectors $
--             newDescribeAnomalyDetectorsResponse
--
--         , responsePutDashboard $
--             newPutDashboardResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseStopMetricStreams $
--             newStopMetricStreamsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetMetricStream $
--             newGetMetricStreamResponse
--
--         , responsePutMetricAlarm $
--             newPutMetricAlarmResponse
--
--         , responseSetAlarmState $
--             newSetAlarmStateResponse
--
--         , responseDescribeInsightRules $
--             newDescribeInsightRulesResponse
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

requestStartMetricStreams :: StartMetricStreams -> TestTree
requestStartMetricStreams =
  req
    "StartMetricStreams"
    "fixture/StartMetricStreams.yaml"

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

requestListMetricStreams :: ListMetricStreams -> TestTree
requestListMetricStreams =
  req
    "ListMetricStreams"
    "fixture/ListMetricStreams.yaml"

requestGetMetricWidgetImage :: GetMetricWidgetImage -> TestTree
requestGetMetricWidgetImage =
  req
    "GetMetricWidgetImage"
    "fixture/GetMetricWidgetImage.yaml"

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

requestPutMetricStream :: PutMetricStream -> TestTree
requestPutMetricStream =
  req
    "PutMetricStream"
    "fixture/PutMetricStream.yaml"

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

requestGetMetricStream :: GetMetricStream -> TestTree
requestGetMetricStream =
  req
    "GetMetricStream"
    "fixture/GetMetricStream.yaml"

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
    defaultService
    (Proxy :: Proxy EnableAlarmActions)

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

responseDeleteAnomalyDetector :: DeleteAnomalyDetectorResponse -> TestTree
responseDeleteAnomalyDetector =
  res
    "DeleteAnomalyDetectorResponse"
    "fixture/DeleteAnomalyDetectorResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAnomalyDetector)

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

responsePutAnomalyDetector :: PutAnomalyDetectorResponse -> TestTree
responsePutAnomalyDetector =
  res
    "PutAnomalyDetectorResponse"
    "fixture/PutAnomalyDetectorResponse.proto"
    defaultService
    (Proxy :: Proxy PutAnomalyDetector)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetMetricData :: GetMetricDataResponse -> TestTree
responseGetMetricData =
  res
    "GetMetricDataResponse"
    "fixture/GetMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetMetricData)

responsePutMetricData :: PutMetricDataResponse -> TestTree
responsePutMetricData =
  res
    "PutMetricDataResponse"
    "fixture/PutMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy PutMetricData)

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

responseListMetrics :: ListMetricsResponse -> TestTree
responseListMetrics =
  res
    "ListMetricsResponse"
    "fixture/ListMetricsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMetrics)

responseGetInsightRuleReport :: GetInsightRuleReportResponse -> TestTree
responseGetInsightRuleReport =
  res
    "GetInsightRuleReportResponse"
    "fixture/GetInsightRuleReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetInsightRuleReport)

responseStartMetricStreams :: StartMetricStreamsResponse -> TestTree
responseStartMetricStreams =
  res
    "StartMetricStreamsResponse"
    "fixture/StartMetricStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy StartMetricStreams)

responseDeleteDashboards :: DeleteDashboardsResponse -> TestTree
responseDeleteDashboards =
  res
    "DeleteDashboardsResponse"
    "fixture/DeleteDashboardsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDashboards)

responsePutInsightRule :: PutInsightRuleResponse -> TestTree
responsePutInsightRule =
  res
    "PutInsightRuleResponse"
    "fixture/PutInsightRuleResponse.proto"
    defaultService
    (Proxy :: Proxy PutInsightRule)

responseListMetricStreams :: ListMetricStreamsResponse -> TestTree
responseListMetricStreams =
  res
    "ListMetricStreamsResponse"
    "fixture/ListMetricStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMetricStreams)

responseGetMetricWidgetImage :: GetMetricWidgetImageResponse -> TestTree
responseGetMetricWidgetImage =
  res
    "GetMetricWidgetImageResponse"
    "fixture/GetMetricWidgetImageResponse.proto"
    defaultService
    (Proxy :: Proxy GetMetricWidgetImage)

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

responsePutMetricStream :: PutMetricStreamResponse -> TestTree
responsePutMetricStream =
  res
    "PutMetricStreamResponse"
    "fixture/PutMetricStreamResponse.proto"
    defaultService
    (Proxy :: Proxy PutMetricStream)

responseDescribeAlarmHistory :: DescribeAlarmHistoryResponse -> TestTree
responseDescribeAlarmHistory =
  res
    "DescribeAlarmHistoryResponse"
    "fixture/DescribeAlarmHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAlarmHistory)

responseGetMetricStatistics :: GetMetricStatisticsResponse -> TestTree
responseGetMetricStatistics =
  res
    "GetMetricStatisticsResponse"
    "fixture/GetMetricStatisticsResponse.proto"
    defaultService
    (Proxy :: Proxy GetMetricStatistics)

responseDescribeAlarmsForMetric :: DescribeAlarmsForMetricResponse -> TestTree
responseDescribeAlarmsForMetric =
  res
    "DescribeAlarmsForMetricResponse"
    "fixture/DescribeAlarmsForMetricResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAlarmsForMetric)

responseEnableInsightRules :: EnableInsightRulesResponse -> TestTree
responseEnableInsightRules =
  res
    "EnableInsightRulesResponse"
    "fixture/EnableInsightRulesResponse.proto"
    defaultService
    (Proxy :: Proxy EnableInsightRules)

responseDisableAlarmActions :: DisableAlarmActionsResponse -> TestTree
responseDisableAlarmActions =
  res
    "DisableAlarmActionsResponse"
    "fixture/DisableAlarmActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DisableAlarmActions)

responseDescribeAnomalyDetectors :: DescribeAnomalyDetectorsResponse -> TestTree
responseDescribeAnomalyDetectors =
  res
    "DescribeAnomalyDetectorsResponse"
    "fixture/DescribeAnomalyDetectorsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAnomalyDetectors)

responsePutDashboard :: PutDashboardResponse -> TestTree
responsePutDashboard =
  res
    "PutDashboardResponse"
    "fixture/PutDashboardResponse.proto"
    defaultService
    (Proxy :: Proxy PutDashboard)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

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

responseGetMetricStream :: GetMetricStreamResponse -> TestTree
responseGetMetricStream =
  res
    "GetMetricStreamResponse"
    "fixture/GetMetricStreamResponse.proto"
    defaultService
    (Proxy :: Proxy GetMetricStream)

responsePutMetricAlarm :: PutMetricAlarmResponse -> TestTree
responsePutMetricAlarm =
  res
    "PutMetricAlarmResponse"
    "fixture/PutMetricAlarmResponse.proto"
    defaultService
    (Proxy :: Proxy PutMetricAlarm)

responseSetAlarmState :: SetAlarmStateResponse -> TestTree
responseSetAlarmState =
  res
    "SetAlarmStateResponse"
    "fixture/SetAlarmStateResponse.proto"
    defaultService
    (Proxy :: Proxy SetAlarmState)

responseDescribeInsightRules :: DescribeInsightRulesResponse -> TestTree
responseDescribeInsightRules =
  res
    "DescribeInsightRulesResponse"
    "fixture/DescribeInsightRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInsightRules)
