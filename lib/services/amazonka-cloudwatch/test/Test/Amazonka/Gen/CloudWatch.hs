{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudWatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CloudWatch where

import Amazonka.CloudWatch
import qualified Data.Proxy as Proxy
import Test.Amazonka.CloudWatch.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteAlarms $
--             newDeleteAlarms
--
--         , requestDeleteAnomalyDetector $
--             newDeleteAnomalyDetector
--
--         , requestDeleteDashboards $
--             newDeleteDashboards
--
--         , requestDeleteInsightRules $
--             newDeleteInsightRules
--
--         , requestDeleteMetricStream $
--             newDeleteMetricStream
--
--         , requestDescribeAlarmHistory $
--             newDescribeAlarmHistory
--
--         , requestDescribeAlarms $
--             newDescribeAlarms
--
--         , requestDescribeAlarmsForMetric $
--             newDescribeAlarmsForMetric
--
--         , requestDescribeAnomalyDetectors $
--             newDescribeAnomalyDetectors
--
--         , requestDescribeInsightRules $
--             newDescribeInsightRules
--
--         , requestDisableAlarmActions $
--             newDisableAlarmActions
--
--         , requestDisableInsightRules $
--             newDisableInsightRules
--
--         , requestEnableAlarmActions $
--             newEnableAlarmActions
--
--         , requestEnableInsightRules $
--             newEnableInsightRules
--
--         , requestGetDashboard $
--             newGetDashboard
--
--         , requestGetInsightRuleReport $
--             newGetInsightRuleReport
--
--         , requestGetMetricData $
--             newGetMetricData
--
--         , requestGetMetricStatistics $
--             newGetMetricStatistics
--
--         , requestGetMetricStream $
--             newGetMetricStream
--
--         , requestGetMetricWidgetImage $
--             newGetMetricWidgetImage
--
--         , requestListDashboards $
--             newListDashboards
--
--         , requestListManagedInsightRules $
--             newListManagedInsightRules
--
--         , requestListMetricStreams $
--             newListMetricStreams
--
--         , requestListMetrics $
--             newListMetrics
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutAnomalyDetector $
--             newPutAnomalyDetector
--
--         , requestPutCompositeAlarm $
--             newPutCompositeAlarm
--
--         , requestPutDashboard $
--             newPutDashboard
--
--         , requestPutInsightRule $
--             newPutInsightRule
--
--         , requestPutManagedInsightRules $
--             newPutManagedInsightRules
--
--         , requestPutMetricAlarm $
--             newPutMetricAlarm
--
--         , requestPutMetricData $
--             newPutMetricData
--
--         , requestPutMetricStream $
--             newPutMetricStream
--
--         , requestSetAlarmState $
--             newSetAlarmState
--
--         , requestStartMetricStreams $
--             newStartMetricStreams
--
--         , requestStopMetricStreams $
--             newStopMetricStreams
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseDeleteAlarms $
--             newDeleteAlarmsResponse
--
--         , responseDeleteAnomalyDetector $
--             newDeleteAnomalyDetectorResponse
--
--         , responseDeleteDashboards $
--             newDeleteDashboardsResponse
--
--         , responseDeleteInsightRules $
--             newDeleteInsightRulesResponse
--
--         , responseDeleteMetricStream $
--             newDeleteMetricStreamResponse
--
--         , responseDescribeAlarmHistory $
--             newDescribeAlarmHistoryResponse
--
--         , responseDescribeAlarms $
--             newDescribeAlarmsResponse
--
--         , responseDescribeAlarmsForMetric $
--             newDescribeAlarmsForMetricResponse
--
--         , responseDescribeAnomalyDetectors $
--             newDescribeAnomalyDetectorsResponse
--
--         , responseDescribeInsightRules $
--             newDescribeInsightRulesResponse
--
--         , responseDisableAlarmActions $
--             newDisableAlarmActionsResponse
--
--         , responseDisableInsightRules $
--             newDisableInsightRulesResponse
--
--         , responseEnableAlarmActions $
--             newEnableAlarmActionsResponse
--
--         , responseEnableInsightRules $
--             newEnableInsightRulesResponse
--
--         , responseGetDashboard $
--             newGetDashboardResponse
--
--         , responseGetInsightRuleReport $
--             newGetInsightRuleReportResponse
--
--         , responseGetMetricData $
--             newGetMetricDataResponse
--
--         , responseGetMetricStatistics $
--             newGetMetricStatisticsResponse
--
--         , responseGetMetricStream $
--             newGetMetricStreamResponse
--
--         , responseGetMetricWidgetImage $
--             newGetMetricWidgetImageResponse
--
--         , responseListDashboards $
--             newListDashboardsResponse
--
--         , responseListManagedInsightRules $
--             newListManagedInsightRulesResponse
--
--         , responseListMetricStreams $
--             newListMetricStreamsResponse
--
--         , responseListMetrics $
--             newListMetricsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutAnomalyDetector $
--             newPutAnomalyDetectorResponse
--
--         , responsePutCompositeAlarm $
--             newPutCompositeAlarmResponse
--
--         , responsePutDashboard $
--             newPutDashboardResponse
--
--         , responsePutInsightRule $
--             newPutInsightRuleResponse
--
--         , responsePutManagedInsightRules $
--             newPutManagedInsightRulesResponse
--
--         , responsePutMetricAlarm $
--             newPutMetricAlarmResponse
--
--         , responsePutMetricData $
--             newPutMetricDataResponse
--
--         , responsePutMetricStream $
--             newPutMetricStreamResponse
--
--         , responseSetAlarmState $
--             newSetAlarmStateResponse
--
--         , responseStartMetricStreams $
--             newStartMetricStreamsResponse
--
--         , responseStopMetricStreams $
--             newStopMetricStreamsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestDeleteAlarms :: DeleteAlarms -> TestTree
requestDeleteAlarms =
  req
    "DeleteAlarms"
    "fixture/DeleteAlarms.yaml"

requestDeleteAnomalyDetector :: DeleteAnomalyDetector -> TestTree
requestDeleteAnomalyDetector =
  req
    "DeleteAnomalyDetector"
    "fixture/DeleteAnomalyDetector.yaml"

requestDeleteDashboards :: DeleteDashboards -> TestTree
requestDeleteDashboards =
  req
    "DeleteDashboards"
    "fixture/DeleteDashboards.yaml"

requestDeleteInsightRules :: DeleteInsightRules -> TestTree
requestDeleteInsightRules =
  req
    "DeleteInsightRules"
    "fixture/DeleteInsightRules.yaml"

requestDeleteMetricStream :: DeleteMetricStream -> TestTree
requestDeleteMetricStream =
  req
    "DeleteMetricStream"
    "fixture/DeleteMetricStream.yaml"

requestDescribeAlarmHistory :: DescribeAlarmHistory -> TestTree
requestDescribeAlarmHistory =
  req
    "DescribeAlarmHistory"
    "fixture/DescribeAlarmHistory.yaml"

requestDescribeAlarms :: DescribeAlarms -> TestTree
requestDescribeAlarms =
  req
    "DescribeAlarms"
    "fixture/DescribeAlarms.yaml"

requestDescribeAlarmsForMetric :: DescribeAlarmsForMetric -> TestTree
requestDescribeAlarmsForMetric =
  req
    "DescribeAlarmsForMetric"
    "fixture/DescribeAlarmsForMetric.yaml"

requestDescribeAnomalyDetectors :: DescribeAnomalyDetectors -> TestTree
requestDescribeAnomalyDetectors =
  req
    "DescribeAnomalyDetectors"
    "fixture/DescribeAnomalyDetectors.yaml"

requestDescribeInsightRules :: DescribeInsightRules -> TestTree
requestDescribeInsightRules =
  req
    "DescribeInsightRules"
    "fixture/DescribeInsightRules.yaml"

requestDisableAlarmActions :: DisableAlarmActions -> TestTree
requestDisableAlarmActions =
  req
    "DisableAlarmActions"
    "fixture/DisableAlarmActions.yaml"

requestDisableInsightRules :: DisableInsightRules -> TestTree
requestDisableInsightRules =
  req
    "DisableInsightRules"
    "fixture/DisableInsightRules.yaml"

requestEnableAlarmActions :: EnableAlarmActions -> TestTree
requestEnableAlarmActions =
  req
    "EnableAlarmActions"
    "fixture/EnableAlarmActions.yaml"

requestEnableInsightRules :: EnableInsightRules -> TestTree
requestEnableInsightRules =
  req
    "EnableInsightRules"
    "fixture/EnableInsightRules.yaml"

requestGetDashboard :: GetDashboard -> TestTree
requestGetDashboard =
  req
    "GetDashboard"
    "fixture/GetDashboard.yaml"

requestGetInsightRuleReport :: GetInsightRuleReport -> TestTree
requestGetInsightRuleReport =
  req
    "GetInsightRuleReport"
    "fixture/GetInsightRuleReport.yaml"

requestGetMetricData :: GetMetricData -> TestTree
requestGetMetricData =
  req
    "GetMetricData"
    "fixture/GetMetricData.yaml"

requestGetMetricStatistics :: GetMetricStatistics -> TestTree
requestGetMetricStatistics =
  req
    "GetMetricStatistics"
    "fixture/GetMetricStatistics.yaml"

requestGetMetricStream :: GetMetricStream -> TestTree
requestGetMetricStream =
  req
    "GetMetricStream"
    "fixture/GetMetricStream.yaml"

requestGetMetricWidgetImage :: GetMetricWidgetImage -> TestTree
requestGetMetricWidgetImage =
  req
    "GetMetricWidgetImage"
    "fixture/GetMetricWidgetImage.yaml"

requestListDashboards :: ListDashboards -> TestTree
requestListDashboards =
  req
    "ListDashboards"
    "fixture/ListDashboards.yaml"

requestListManagedInsightRules :: ListManagedInsightRules -> TestTree
requestListManagedInsightRules =
  req
    "ListManagedInsightRules"
    "fixture/ListManagedInsightRules.yaml"

requestListMetricStreams :: ListMetricStreams -> TestTree
requestListMetricStreams =
  req
    "ListMetricStreams"
    "fixture/ListMetricStreams.yaml"

requestListMetrics :: ListMetrics -> TestTree
requestListMetrics =
  req
    "ListMetrics"
    "fixture/ListMetrics.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutAnomalyDetector :: PutAnomalyDetector -> TestTree
requestPutAnomalyDetector =
  req
    "PutAnomalyDetector"
    "fixture/PutAnomalyDetector.yaml"

requestPutCompositeAlarm :: PutCompositeAlarm -> TestTree
requestPutCompositeAlarm =
  req
    "PutCompositeAlarm"
    "fixture/PutCompositeAlarm.yaml"

requestPutDashboard :: PutDashboard -> TestTree
requestPutDashboard =
  req
    "PutDashboard"
    "fixture/PutDashboard.yaml"

requestPutInsightRule :: PutInsightRule -> TestTree
requestPutInsightRule =
  req
    "PutInsightRule"
    "fixture/PutInsightRule.yaml"

requestPutManagedInsightRules :: PutManagedInsightRules -> TestTree
requestPutManagedInsightRules =
  req
    "PutManagedInsightRules"
    "fixture/PutManagedInsightRules.yaml"

requestPutMetricAlarm :: PutMetricAlarm -> TestTree
requestPutMetricAlarm =
  req
    "PutMetricAlarm"
    "fixture/PutMetricAlarm.yaml"

requestPutMetricData :: PutMetricData -> TestTree
requestPutMetricData =
  req
    "PutMetricData"
    "fixture/PutMetricData.yaml"

requestPutMetricStream :: PutMetricStream -> TestTree
requestPutMetricStream =
  req
    "PutMetricStream"
    "fixture/PutMetricStream.yaml"

requestSetAlarmState :: SetAlarmState -> TestTree
requestSetAlarmState =
  req
    "SetAlarmState"
    "fixture/SetAlarmState.yaml"

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

-- Responses

responseDeleteAlarms :: DeleteAlarmsResponse -> TestTree
responseDeleteAlarms =
  res
    "DeleteAlarmsResponse"
    "fixture/DeleteAlarmsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlarms)

responseDeleteAnomalyDetector :: DeleteAnomalyDetectorResponse -> TestTree
responseDeleteAnomalyDetector =
  res
    "DeleteAnomalyDetectorResponse"
    "fixture/DeleteAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnomalyDetector)

responseDeleteDashboards :: DeleteDashboardsResponse -> TestTree
responseDeleteDashboards =
  res
    "DeleteDashboardsResponse"
    "fixture/DeleteDashboardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDashboards)

responseDeleteInsightRules :: DeleteInsightRulesResponse -> TestTree
responseDeleteInsightRules =
  res
    "DeleteInsightRulesResponse"
    "fixture/DeleteInsightRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInsightRules)

responseDeleteMetricStream :: DeleteMetricStreamResponse -> TestTree
responseDeleteMetricStream =
  res
    "DeleteMetricStreamResponse"
    "fixture/DeleteMetricStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMetricStream)

responseDescribeAlarmHistory :: DescribeAlarmHistoryResponse -> TestTree
responseDescribeAlarmHistory =
  res
    "DescribeAlarmHistoryResponse"
    "fixture/DescribeAlarmHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlarmHistory)

responseDescribeAlarms :: DescribeAlarmsResponse -> TestTree
responseDescribeAlarms =
  res
    "DescribeAlarmsResponse"
    "fixture/DescribeAlarmsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlarms)

responseDescribeAlarmsForMetric :: DescribeAlarmsForMetricResponse -> TestTree
responseDescribeAlarmsForMetric =
  res
    "DescribeAlarmsForMetricResponse"
    "fixture/DescribeAlarmsForMetricResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlarmsForMetric)

responseDescribeAnomalyDetectors :: DescribeAnomalyDetectorsResponse -> TestTree
responseDescribeAnomalyDetectors =
  res
    "DescribeAnomalyDetectorsResponse"
    "fixture/DescribeAnomalyDetectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnomalyDetectors)

responseDescribeInsightRules :: DescribeInsightRulesResponse -> TestTree
responseDescribeInsightRules =
  res
    "DescribeInsightRulesResponse"
    "fixture/DescribeInsightRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInsightRules)

responseDisableAlarmActions :: DisableAlarmActionsResponse -> TestTree
responseDisableAlarmActions =
  res
    "DisableAlarmActionsResponse"
    "fixture/DisableAlarmActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableAlarmActions)

responseDisableInsightRules :: DisableInsightRulesResponse -> TestTree
responseDisableInsightRules =
  res
    "DisableInsightRulesResponse"
    "fixture/DisableInsightRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableInsightRules)

responseEnableAlarmActions :: EnableAlarmActionsResponse -> TestTree
responseEnableAlarmActions =
  res
    "EnableAlarmActionsResponse"
    "fixture/EnableAlarmActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableAlarmActions)

responseEnableInsightRules :: EnableInsightRulesResponse -> TestTree
responseEnableInsightRules =
  res
    "EnableInsightRulesResponse"
    "fixture/EnableInsightRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableInsightRules)

responseGetDashboard :: GetDashboardResponse -> TestTree
responseGetDashboard =
  res
    "GetDashboardResponse"
    "fixture/GetDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDashboard)

responseGetInsightRuleReport :: GetInsightRuleReportResponse -> TestTree
responseGetInsightRuleReport =
  res
    "GetInsightRuleReportResponse"
    "fixture/GetInsightRuleReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInsightRuleReport)

responseGetMetricData :: GetMetricDataResponse -> TestTree
responseGetMetricData =
  res
    "GetMetricDataResponse"
    "fixture/GetMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMetricData)

responseGetMetricStatistics :: GetMetricStatisticsResponse -> TestTree
responseGetMetricStatistics =
  res
    "GetMetricStatisticsResponse"
    "fixture/GetMetricStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMetricStatistics)

responseGetMetricStream :: GetMetricStreamResponse -> TestTree
responseGetMetricStream =
  res
    "GetMetricStreamResponse"
    "fixture/GetMetricStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMetricStream)

responseGetMetricWidgetImage :: GetMetricWidgetImageResponse -> TestTree
responseGetMetricWidgetImage =
  res
    "GetMetricWidgetImageResponse"
    "fixture/GetMetricWidgetImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMetricWidgetImage)

responseListDashboards :: ListDashboardsResponse -> TestTree
responseListDashboards =
  res
    "ListDashboardsResponse"
    "fixture/ListDashboardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDashboards)

responseListManagedInsightRules :: ListManagedInsightRulesResponse -> TestTree
responseListManagedInsightRules =
  res
    "ListManagedInsightRulesResponse"
    "fixture/ListManagedInsightRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListManagedInsightRules)

responseListMetricStreams :: ListMetricStreamsResponse -> TestTree
responseListMetricStreams =
  res
    "ListMetricStreamsResponse"
    "fixture/ListMetricStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMetricStreams)

responseListMetrics :: ListMetricsResponse -> TestTree
responseListMetrics =
  res
    "ListMetricsResponse"
    "fixture/ListMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMetrics)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutAnomalyDetector :: PutAnomalyDetectorResponse -> TestTree
responsePutAnomalyDetector =
  res
    "PutAnomalyDetectorResponse"
    "fixture/PutAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAnomalyDetector)

responsePutCompositeAlarm :: PutCompositeAlarmResponse -> TestTree
responsePutCompositeAlarm =
  res
    "PutCompositeAlarmResponse"
    "fixture/PutCompositeAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutCompositeAlarm)

responsePutDashboard :: PutDashboardResponse -> TestTree
responsePutDashboard =
  res
    "PutDashboardResponse"
    "fixture/PutDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDashboard)

responsePutInsightRule :: PutInsightRuleResponse -> TestTree
responsePutInsightRule =
  res
    "PutInsightRuleResponse"
    "fixture/PutInsightRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutInsightRule)

responsePutManagedInsightRules :: PutManagedInsightRulesResponse -> TestTree
responsePutManagedInsightRules =
  res
    "PutManagedInsightRulesResponse"
    "fixture/PutManagedInsightRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutManagedInsightRules)

responsePutMetricAlarm :: PutMetricAlarmResponse -> TestTree
responsePutMetricAlarm =
  res
    "PutMetricAlarmResponse"
    "fixture/PutMetricAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMetricAlarm)

responsePutMetricData :: PutMetricDataResponse -> TestTree
responsePutMetricData =
  res
    "PutMetricDataResponse"
    "fixture/PutMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMetricData)

responsePutMetricStream :: PutMetricStreamResponse -> TestTree
responsePutMetricStream =
  res
    "PutMetricStreamResponse"
    "fixture/PutMetricStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMetricStream)

responseSetAlarmState :: SetAlarmStateResponse -> TestTree
responseSetAlarmState =
  res
    "SetAlarmStateResponse"
    "fixture/SetAlarmStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetAlarmState)

responseStartMetricStreams :: StartMetricStreamsResponse -> TestTree
responseStartMetricStreams =
  res
    "StartMetricStreamsResponse"
    "fixture/StartMetricStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMetricStreams)

responseStopMetricStreams :: StopMetricStreamsResponse -> TestTree
responseStopMetricStreams =
  res
    "StopMetricStreamsResponse"
    "fixture/StopMetricStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopMetricStreams)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)
