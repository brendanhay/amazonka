{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LookoutMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.LookoutMetrics where

import Amazonka.LookoutMetrics
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.LookoutMetrics.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestActivateAnomalyDetector $
--             newActivateAnomalyDetector
--
--         , requestBackTestAnomalyDetector $
--             newBackTestAnomalyDetector
--
--         , requestCreateAlert $
--             newCreateAlert
--
--         , requestCreateAnomalyDetector $
--             newCreateAnomalyDetector
--
--         , requestCreateMetricSet $
--             newCreateMetricSet
--
--         , requestDeactivateAnomalyDetector $
--             newDeactivateAnomalyDetector
--
--         , requestDeleteAlert $
--             newDeleteAlert
--
--         , requestDeleteAnomalyDetector $
--             newDeleteAnomalyDetector
--
--         , requestDescribeAlert $
--             newDescribeAlert
--
--         , requestDescribeAnomalyDetectionExecutions $
--             newDescribeAnomalyDetectionExecutions
--
--         , requestDescribeAnomalyDetector $
--             newDescribeAnomalyDetector
--
--         , requestDescribeMetricSet $
--             newDescribeMetricSet
--
--         , requestDetectMetricSetConfig $
--             newDetectMetricSetConfig
--
--         , requestGetAnomalyGroup $
--             newGetAnomalyGroup
--
--         , requestGetDataQualityMetrics $
--             newGetDataQualityMetrics
--
--         , requestGetFeedback $
--             newGetFeedback
--
--         , requestGetSampleData $
--             newGetSampleData
--
--         , requestListAlerts $
--             newListAlerts
--
--         , requestListAnomalyDetectors $
--             newListAnomalyDetectors
--
--         , requestListAnomalyGroupRelatedMetrics $
--             newListAnomalyGroupRelatedMetrics
--
--         , requestListAnomalyGroupSummaries $
--             newListAnomalyGroupSummaries
--
--         , requestListAnomalyGroupTimeSeries $
--             newListAnomalyGroupTimeSeries
--
--         , requestListMetricSets $
--             newListMetricSets
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutFeedback $
--             newPutFeedback
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAlert $
--             newUpdateAlert
--
--         , requestUpdateAnomalyDetector $
--             newUpdateAnomalyDetector
--
--         , requestUpdateMetricSet $
--             newUpdateMetricSet
--
--           ]

--     , testGroup "response"
--         [ responseActivateAnomalyDetector $
--             newActivateAnomalyDetectorResponse
--
--         , responseBackTestAnomalyDetector $
--             newBackTestAnomalyDetectorResponse
--
--         , responseCreateAlert $
--             newCreateAlertResponse
--
--         , responseCreateAnomalyDetector $
--             newCreateAnomalyDetectorResponse
--
--         , responseCreateMetricSet $
--             newCreateMetricSetResponse
--
--         , responseDeactivateAnomalyDetector $
--             newDeactivateAnomalyDetectorResponse
--
--         , responseDeleteAlert $
--             newDeleteAlertResponse
--
--         , responseDeleteAnomalyDetector $
--             newDeleteAnomalyDetectorResponse
--
--         , responseDescribeAlert $
--             newDescribeAlertResponse
--
--         , responseDescribeAnomalyDetectionExecutions $
--             newDescribeAnomalyDetectionExecutionsResponse
--
--         , responseDescribeAnomalyDetector $
--             newDescribeAnomalyDetectorResponse
--
--         , responseDescribeMetricSet $
--             newDescribeMetricSetResponse
--
--         , responseDetectMetricSetConfig $
--             newDetectMetricSetConfigResponse
--
--         , responseGetAnomalyGroup $
--             newGetAnomalyGroupResponse
--
--         , responseGetDataQualityMetrics $
--             newGetDataQualityMetricsResponse
--
--         , responseGetFeedback $
--             newGetFeedbackResponse
--
--         , responseGetSampleData $
--             newGetSampleDataResponse
--
--         , responseListAlerts $
--             newListAlertsResponse
--
--         , responseListAnomalyDetectors $
--             newListAnomalyDetectorsResponse
--
--         , responseListAnomalyGroupRelatedMetrics $
--             newListAnomalyGroupRelatedMetricsResponse
--
--         , responseListAnomalyGroupSummaries $
--             newListAnomalyGroupSummariesResponse
--
--         , responseListAnomalyGroupTimeSeries $
--             newListAnomalyGroupTimeSeriesResponse
--
--         , responseListMetricSets $
--             newListMetricSetsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutFeedback $
--             newPutFeedbackResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAlert $
--             newUpdateAlertResponse
--
--         , responseUpdateAnomalyDetector $
--             newUpdateAnomalyDetectorResponse
--
--         , responseUpdateMetricSet $
--             newUpdateMetricSetResponse
--
--           ]
--     ]

-- Requests

requestActivateAnomalyDetector :: ActivateAnomalyDetector -> TestTree
requestActivateAnomalyDetector =
  req
    "ActivateAnomalyDetector"
    "fixture/ActivateAnomalyDetector.yaml"

requestBackTestAnomalyDetector :: BackTestAnomalyDetector -> TestTree
requestBackTestAnomalyDetector =
  req
    "BackTestAnomalyDetector"
    "fixture/BackTestAnomalyDetector.yaml"

requestCreateAlert :: CreateAlert -> TestTree
requestCreateAlert =
  req
    "CreateAlert"
    "fixture/CreateAlert.yaml"

requestCreateAnomalyDetector :: CreateAnomalyDetector -> TestTree
requestCreateAnomalyDetector =
  req
    "CreateAnomalyDetector"
    "fixture/CreateAnomalyDetector.yaml"

requestCreateMetricSet :: CreateMetricSet -> TestTree
requestCreateMetricSet =
  req
    "CreateMetricSet"
    "fixture/CreateMetricSet.yaml"

requestDeactivateAnomalyDetector :: DeactivateAnomalyDetector -> TestTree
requestDeactivateAnomalyDetector =
  req
    "DeactivateAnomalyDetector"
    "fixture/DeactivateAnomalyDetector.yaml"

requestDeleteAlert :: DeleteAlert -> TestTree
requestDeleteAlert =
  req
    "DeleteAlert"
    "fixture/DeleteAlert.yaml"

requestDeleteAnomalyDetector :: DeleteAnomalyDetector -> TestTree
requestDeleteAnomalyDetector =
  req
    "DeleteAnomalyDetector"
    "fixture/DeleteAnomalyDetector.yaml"

requestDescribeAlert :: DescribeAlert -> TestTree
requestDescribeAlert =
  req
    "DescribeAlert"
    "fixture/DescribeAlert.yaml"

requestDescribeAnomalyDetectionExecutions :: DescribeAnomalyDetectionExecutions -> TestTree
requestDescribeAnomalyDetectionExecutions =
  req
    "DescribeAnomalyDetectionExecutions"
    "fixture/DescribeAnomalyDetectionExecutions.yaml"

requestDescribeAnomalyDetector :: DescribeAnomalyDetector -> TestTree
requestDescribeAnomalyDetector =
  req
    "DescribeAnomalyDetector"
    "fixture/DescribeAnomalyDetector.yaml"

requestDescribeMetricSet :: DescribeMetricSet -> TestTree
requestDescribeMetricSet =
  req
    "DescribeMetricSet"
    "fixture/DescribeMetricSet.yaml"

requestDetectMetricSetConfig :: DetectMetricSetConfig -> TestTree
requestDetectMetricSetConfig =
  req
    "DetectMetricSetConfig"
    "fixture/DetectMetricSetConfig.yaml"

requestGetAnomalyGroup :: GetAnomalyGroup -> TestTree
requestGetAnomalyGroup =
  req
    "GetAnomalyGroup"
    "fixture/GetAnomalyGroup.yaml"

requestGetDataQualityMetrics :: GetDataQualityMetrics -> TestTree
requestGetDataQualityMetrics =
  req
    "GetDataQualityMetrics"
    "fixture/GetDataQualityMetrics.yaml"

requestGetFeedback :: GetFeedback -> TestTree
requestGetFeedback =
  req
    "GetFeedback"
    "fixture/GetFeedback.yaml"

requestGetSampleData :: GetSampleData -> TestTree
requestGetSampleData =
  req
    "GetSampleData"
    "fixture/GetSampleData.yaml"

requestListAlerts :: ListAlerts -> TestTree
requestListAlerts =
  req
    "ListAlerts"
    "fixture/ListAlerts.yaml"

requestListAnomalyDetectors :: ListAnomalyDetectors -> TestTree
requestListAnomalyDetectors =
  req
    "ListAnomalyDetectors"
    "fixture/ListAnomalyDetectors.yaml"

requestListAnomalyGroupRelatedMetrics :: ListAnomalyGroupRelatedMetrics -> TestTree
requestListAnomalyGroupRelatedMetrics =
  req
    "ListAnomalyGroupRelatedMetrics"
    "fixture/ListAnomalyGroupRelatedMetrics.yaml"

requestListAnomalyGroupSummaries :: ListAnomalyGroupSummaries -> TestTree
requestListAnomalyGroupSummaries =
  req
    "ListAnomalyGroupSummaries"
    "fixture/ListAnomalyGroupSummaries.yaml"

requestListAnomalyGroupTimeSeries :: ListAnomalyGroupTimeSeries -> TestTree
requestListAnomalyGroupTimeSeries =
  req
    "ListAnomalyGroupTimeSeries"
    "fixture/ListAnomalyGroupTimeSeries.yaml"

requestListMetricSets :: ListMetricSets -> TestTree
requestListMetricSets =
  req
    "ListMetricSets"
    "fixture/ListMetricSets.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutFeedback :: PutFeedback -> TestTree
requestPutFeedback =
  req
    "PutFeedback"
    "fixture/PutFeedback.yaml"

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

requestUpdateAlert :: UpdateAlert -> TestTree
requestUpdateAlert =
  req
    "UpdateAlert"
    "fixture/UpdateAlert.yaml"

requestUpdateAnomalyDetector :: UpdateAnomalyDetector -> TestTree
requestUpdateAnomalyDetector =
  req
    "UpdateAnomalyDetector"
    "fixture/UpdateAnomalyDetector.yaml"

requestUpdateMetricSet :: UpdateMetricSet -> TestTree
requestUpdateMetricSet =
  req
    "UpdateMetricSet"
    "fixture/UpdateMetricSet.yaml"

-- Responses

responseActivateAnomalyDetector :: ActivateAnomalyDetectorResponse -> TestTree
responseActivateAnomalyDetector =
  res
    "ActivateAnomalyDetectorResponse"
    "fixture/ActivateAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateAnomalyDetector)

responseBackTestAnomalyDetector :: BackTestAnomalyDetectorResponse -> TestTree
responseBackTestAnomalyDetector =
  res
    "BackTestAnomalyDetectorResponse"
    "fixture/BackTestAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BackTestAnomalyDetector)

responseCreateAlert :: CreateAlertResponse -> TestTree
responseCreateAlert =
  res
    "CreateAlertResponse"
    "fixture/CreateAlertResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlert)

responseCreateAnomalyDetector :: CreateAnomalyDetectorResponse -> TestTree
responseCreateAnomalyDetector =
  res
    "CreateAnomalyDetectorResponse"
    "fixture/CreateAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAnomalyDetector)

responseCreateMetricSet :: CreateMetricSetResponse -> TestTree
responseCreateMetricSet =
  res
    "CreateMetricSetResponse"
    "fixture/CreateMetricSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMetricSet)

responseDeactivateAnomalyDetector :: DeactivateAnomalyDetectorResponse -> TestTree
responseDeactivateAnomalyDetector =
  res
    "DeactivateAnomalyDetectorResponse"
    "fixture/DeactivateAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateAnomalyDetector)

responseDeleteAlert :: DeleteAlertResponse -> TestTree
responseDeleteAlert =
  res
    "DeleteAlertResponse"
    "fixture/DeleteAlertResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlert)

responseDeleteAnomalyDetector :: DeleteAnomalyDetectorResponse -> TestTree
responseDeleteAnomalyDetector =
  res
    "DeleteAnomalyDetectorResponse"
    "fixture/DeleteAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnomalyDetector)

responseDescribeAlert :: DescribeAlertResponse -> TestTree
responseDescribeAlert =
  res
    "DescribeAlertResponse"
    "fixture/DescribeAlertResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlert)

responseDescribeAnomalyDetectionExecutions :: DescribeAnomalyDetectionExecutionsResponse -> TestTree
responseDescribeAnomalyDetectionExecutions =
  res
    "DescribeAnomalyDetectionExecutionsResponse"
    "fixture/DescribeAnomalyDetectionExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnomalyDetectionExecutions)

responseDescribeAnomalyDetector :: DescribeAnomalyDetectorResponse -> TestTree
responseDescribeAnomalyDetector =
  res
    "DescribeAnomalyDetectorResponse"
    "fixture/DescribeAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnomalyDetector)

responseDescribeMetricSet :: DescribeMetricSetResponse -> TestTree
responseDescribeMetricSet =
  res
    "DescribeMetricSetResponse"
    "fixture/DescribeMetricSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMetricSet)

responseDetectMetricSetConfig :: DetectMetricSetConfigResponse -> TestTree
responseDetectMetricSetConfig =
  res
    "DetectMetricSetConfigResponse"
    "fixture/DetectMetricSetConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectMetricSetConfig)

responseGetAnomalyGroup :: GetAnomalyGroupResponse -> TestTree
responseGetAnomalyGroup =
  res
    "GetAnomalyGroupResponse"
    "fixture/GetAnomalyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnomalyGroup)

responseGetDataQualityMetrics :: GetDataQualityMetricsResponse -> TestTree
responseGetDataQualityMetrics =
  res
    "GetDataQualityMetricsResponse"
    "fixture/GetDataQualityMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataQualityMetrics)

responseGetFeedback :: GetFeedbackResponse -> TestTree
responseGetFeedback =
  res
    "GetFeedbackResponse"
    "fixture/GetFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFeedback)

responseGetSampleData :: GetSampleDataResponse -> TestTree
responseGetSampleData =
  res
    "GetSampleDataResponse"
    "fixture/GetSampleDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSampleData)

responseListAlerts :: ListAlertsResponse -> TestTree
responseListAlerts =
  res
    "ListAlertsResponse"
    "fixture/ListAlertsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAlerts)

responseListAnomalyDetectors :: ListAnomalyDetectorsResponse -> TestTree
responseListAnomalyDetectors =
  res
    "ListAnomalyDetectorsResponse"
    "fixture/ListAnomalyDetectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnomalyDetectors)

responseListAnomalyGroupRelatedMetrics :: ListAnomalyGroupRelatedMetricsResponse -> TestTree
responseListAnomalyGroupRelatedMetrics =
  res
    "ListAnomalyGroupRelatedMetricsResponse"
    "fixture/ListAnomalyGroupRelatedMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnomalyGroupRelatedMetrics)

responseListAnomalyGroupSummaries :: ListAnomalyGroupSummariesResponse -> TestTree
responseListAnomalyGroupSummaries =
  res
    "ListAnomalyGroupSummariesResponse"
    "fixture/ListAnomalyGroupSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnomalyGroupSummaries)

responseListAnomalyGroupTimeSeries :: ListAnomalyGroupTimeSeriesResponse -> TestTree
responseListAnomalyGroupTimeSeries =
  res
    "ListAnomalyGroupTimeSeriesResponse"
    "fixture/ListAnomalyGroupTimeSeriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnomalyGroupTimeSeries)

responseListMetricSets :: ListMetricSetsResponse -> TestTree
responseListMetricSets =
  res
    "ListMetricSetsResponse"
    "fixture/ListMetricSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMetricSets)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutFeedback :: PutFeedbackResponse -> TestTree
responsePutFeedback =
  res
    "PutFeedbackResponse"
    "fixture/PutFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFeedback)

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

responseUpdateAlert :: UpdateAlertResponse -> TestTree
responseUpdateAlert =
  res
    "UpdateAlertResponse"
    "fixture/UpdateAlertResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAlert)

responseUpdateAnomalyDetector :: UpdateAnomalyDetectorResponse -> TestTree
responseUpdateAnomalyDetector =
  res
    "UpdateAnomalyDetectorResponse"
    "fixture/UpdateAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAnomalyDetector)

responseUpdateMetricSet :: UpdateMetricSetResponse -> TestTree
responseUpdateMetricSet =
  res
    "UpdateMetricSetResponse"
    "fixture/UpdateMetricSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMetricSet)
