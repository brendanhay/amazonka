{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LookoutMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestGetFeedback $
--             newGetFeedback
--
--         , requestListAlerts $
--             newListAlerts
--
--         , requestListMetricSets $
--             newListMetricSets
--
--         , requestDeleteAnomalyDetector $
--             newDeleteAnomalyDetector
--
--         , requestUpdateAnomalyDetector $
--             newUpdateAnomalyDetector
--
--         , requestListAnomalyDetectors $
--             newListAnomalyDetectors
--
--         , requestDescribeAnomalyDetectionExecutions $
--             newDescribeAnomalyDetectionExecutions
--
--         , requestCreateMetricSet $
--             newCreateMetricSet
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateAlert $
--             newCreateAlert
--
--         , requestGetAnomalyGroup $
--             newGetAnomalyGroup
--
--         , requestPutFeedback $
--             newPutFeedback
--
--         , requestBackTestAnomalyDetector $
--             newBackTestAnomalyDetector
--
--         , requestDeleteAlert $
--             newDeleteAlert
--
--         , requestCreateAnomalyDetector $
--             newCreateAnomalyDetector
--
--         , requestUpdateMetricSet $
--             newUpdateMetricSet
--
--         , requestActivateAnomalyDetector $
--             newActivateAnomalyDetector
--
--         , requestListAnomalyGroupTimeSeries $
--             newListAnomalyGroupTimeSeries
--
--         , requestGetSampleData $
--             newGetSampleData
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeMetricSet $
--             newDescribeMetricSet
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeAlert $
--             newDescribeAlert
--
--         , requestListAnomalyGroupSummaries $
--             newListAnomalyGroupSummaries
--
--         , requestDescribeAnomalyDetector $
--             newDescribeAnomalyDetector
--
--           ]

--     , testGroup "response"
--         [ responseGetFeedback $
--             newGetFeedbackResponse
--
--         , responseListAlerts $
--             newListAlertsResponse
--
--         , responseListMetricSets $
--             newListMetricSetsResponse
--
--         , responseDeleteAnomalyDetector $
--             newDeleteAnomalyDetectorResponse
--
--         , responseUpdateAnomalyDetector $
--             newUpdateAnomalyDetectorResponse
--
--         , responseListAnomalyDetectors $
--             newListAnomalyDetectorsResponse
--
--         , responseDescribeAnomalyDetectionExecutions $
--             newDescribeAnomalyDetectionExecutionsResponse
--
--         , responseCreateMetricSet $
--             newCreateMetricSetResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateAlert $
--             newCreateAlertResponse
--
--         , responseGetAnomalyGroup $
--             newGetAnomalyGroupResponse
--
--         , responsePutFeedback $
--             newPutFeedbackResponse
--
--         , responseBackTestAnomalyDetector $
--             newBackTestAnomalyDetectorResponse
--
--         , responseDeleteAlert $
--             newDeleteAlertResponse
--
--         , responseCreateAnomalyDetector $
--             newCreateAnomalyDetectorResponse
--
--         , responseUpdateMetricSet $
--             newUpdateMetricSetResponse
--
--         , responseActivateAnomalyDetector $
--             newActivateAnomalyDetectorResponse
--
--         , responseListAnomalyGroupTimeSeries $
--             newListAnomalyGroupTimeSeriesResponse
--
--         , responseGetSampleData $
--             newGetSampleDataResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeMetricSet $
--             newDescribeMetricSetResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeAlert $
--             newDescribeAlertResponse
--
--         , responseListAnomalyGroupSummaries $
--             newListAnomalyGroupSummariesResponse
--
--         , responseDescribeAnomalyDetector $
--             newDescribeAnomalyDetectorResponse
--
--           ]
--     ]

-- Requests

requestGetFeedback :: GetFeedback -> TestTree
requestGetFeedback =
  req
    "GetFeedback"
    "fixture/GetFeedback.yaml"

requestListAlerts :: ListAlerts -> TestTree
requestListAlerts =
  req
    "ListAlerts"
    "fixture/ListAlerts.yaml"

requestListMetricSets :: ListMetricSets -> TestTree
requestListMetricSets =
  req
    "ListMetricSets"
    "fixture/ListMetricSets.yaml"

requestDeleteAnomalyDetector :: DeleteAnomalyDetector -> TestTree
requestDeleteAnomalyDetector =
  req
    "DeleteAnomalyDetector"
    "fixture/DeleteAnomalyDetector.yaml"

requestUpdateAnomalyDetector :: UpdateAnomalyDetector -> TestTree
requestUpdateAnomalyDetector =
  req
    "UpdateAnomalyDetector"
    "fixture/UpdateAnomalyDetector.yaml"

requestListAnomalyDetectors :: ListAnomalyDetectors -> TestTree
requestListAnomalyDetectors =
  req
    "ListAnomalyDetectors"
    "fixture/ListAnomalyDetectors.yaml"

requestDescribeAnomalyDetectionExecutions :: DescribeAnomalyDetectionExecutions -> TestTree
requestDescribeAnomalyDetectionExecutions =
  req
    "DescribeAnomalyDetectionExecutions"
    "fixture/DescribeAnomalyDetectionExecutions.yaml"

requestCreateMetricSet :: CreateMetricSet -> TestTree
requestCreateMetricSet =
  req
    "CreateMetricSet"
    "fixture/CreateMetricSet.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateAlert :: CreateAlert -> TestTree
requestCreateAlert =
  req
    "CreateAlert"
    "fixture/CreateAlert.yaml"

requestGetAnomalyGroup :: GetAnomalyGroup -> TestTree
requestGetAnomalyGroup =
  req
    "GetAnomalyGroup"
    "fixture/GetAnomalyGroup.yaml"

requestPutFeedback :: PutFeedback -> TestTree
requestPutFeedback =
  req
    "PutFeedback"
    "fixture/PutFeedback.yaml"

requestBackTestAnomalyDetector :: BackTestAnomalyDetector -> TestTree
requestBackTestAnomalyDetector =
  req
    "BackTestAnomalyDetector"
    "fixture/BackTestAnomalyDetector.yaml"

requestDeleteAlert :: DeleteAlert -> TestTree
requestDeleteAlert =
  req
    "DeleteAlert"
    "fixture/DeleteAlert.yaml"

requestCreateAnomalyDetector :: CreateAnomalyDetector -> TestTree
requestCreateAnomalyDetector =
  req
    "CreateAnomalyDetector"
    "fixture/CreateAnomalyDetector.yaml"

requestUpdateMetricSet :: UpdateMetricSet -> TestTree
requestUpdateMetricSet =
  req
    "UpdateMetricSet"
    "fixture/UpdateMetricSet.yaml"

requestActivateAnomalyDetector :: ActivateAnomalyDetector -> TestTree
requestActivateAnomalyDetector =
  req
    "ActivateAnomalyDetector"
    "fixture/ActivateAnomalyDetector.yaml"

requestListAnomalyGroupTimeSeries :: ListAnomalyGroupTimeSeries -> TestTree
requestListAnomalyGroupTimeSeries =
  req
    "ListAnomalyGroupTimeSeries"
    "fixture/ListAnomalyGroupTimeSeries.yaml"

requestGetSampleData :: GetSampleData -> TestTree
requestGetSampleData =
  req
    "GetSampleData"
    "fixture/GetSampleData.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeMetricSet :: DescribeMetricSet -> TestTree
requestDescribeMetricSet =
  req
    "DescribeMetricSet"
    "fixture/DescribeMetricSet.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeAlert :: DescribeAlert -> TestTree
requestDescribeAlert =
  req
    "DescribeAlert"
    "fixture/DescribeAlert.yaml"

requestListAnomalyGroupSummaries :: ListAnomalyGroupSummaries -> TestTree
requestListAnomalyGroupSummaries =
  req
    "ListAnomalyGroupSummaries"
    "fixture/ListAnomalyGroupSummaries.yaml"

requestDescribeAnomalyDetector :: DescribeAnomalyDetector -> TestTree
requestDescribeAnomalyDetector =
  req
    "DescribeAnomalyDetector"
    "fixture/DescribeAnomalyDetector.yaml"

-- Responses

responseGetFeedback :: GetFeedbackResponse -> TestTree
responseGetFeedback =
  res
    "GetFeedbackResponse"
    "fixture/GetFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFeedback)

responseListAlerts :: ListAlertsResponse -> TestTree
responseListAlerts =
  res
    "ListAlertsResponse"
    "fixture/ListAlertsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAlerts)

responseListMetricSets :: ListMetricSetsResponse -> TestTree
responseListMetricSets =
  res
    "ListMetricSetsResponse"
    "fixture/ListMetricSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMetricSets)

responseDeleteAnomalyDetector :: DeleteAnomalyDetectorResponse -> TestTree
responseDeleteAnomalyDetector =
  res
    "DeleteAnomalyDetectorResponse"
    "fixture/DeleteAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnomalyDetector)

responseUpdateAnomalyDetector :: UpdateAnomalyDetectorResponse -> TestTree
responseUpdateAnomalyDetector =
  res
    "UpdateAnomalyDetectorResponse"
    "fixture/UpdateAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAnomalyDetector)

responseListAnomalyDetectors :: ListAnomalyDetectorsResponse -> TestTree
responseListAnomalyDetectors =
  res
    "ListAnomalyDetectorsResponse"
    "fixture/ListAnomalyDetectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnomalyDetectors)

responseDescribeAnomalyDetectionExecutions :: DescribeAnomalyDetectionExecutionsResponse -> TestTree
responseDescribeAnomalyDetectionExecutions =
  res
    "DescribeAnomalyDetectionExecutionsResponse"
    "fixture/DescribeAnomalyDetectionExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnomalyDetectionExecutions)

responseCreateMetricSet :: CreateMetricSetResponse -> TestTree
responseCreateMetricSet =
  res
    "CreateMetricSetResponse"
    "fixture/CreateMetricSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMetricSet)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateAlert :: CreateAlertResponse -> TestTree
responseCreateAlert =
  res
    "CreateAlertResponse"
    "fixture/CreateAlertResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlert)

responseGetAnomalyGroup :: GetAnomalyGroupResponse -> TestTree
responseGetAnomalyGroup =
  res
    "GetAnomalyGroupResponse"
    "fixture/GetAnomalyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnomalyGroup)

responsePutFeedback :: PutFeedbackResponse -> TestTree
responsePutFeedback =
  res
    "PutFeedbackResponse"
    "fixture/PutFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFeedback)

responseBackTestAnomalyDetector :: BackTestAnomalyDetectorResponse -> TestTree
responseBackTestAnomalyDetector =
  res
    "BackTestAnomalyDetectorResponse"
    "fixture/BackTestAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BackTestAnomalyDetector)

responseDeleteAlert :: DeleteAlertResponse -> TestTree
responseDeleteAlert =
  res
    "DeleteAlertResponse"
    "fixture/DeleteAlertResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlert)

responseCreateAnomalyDetector :: CreateAnomalyDetectorResponse -> TestTree
responseCreateAnomalyDetector =
  res
    "CreateAnomalyDetectorResponse"
    "fixture/CreateAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAnomalyDetector)

responseUpdateMetricSet :: UpdateMetricSetResponse -> TestTree
responseUpdateMetricSet =
  res
    "UpdateMetricSetResponse"
    "fixture/UpdateMetricSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMetricSet)

responseActivateAnomalyDetector :: ActivateAnomalyDetectorResponse -> TestTree
responseActivateAnomalyDetector =
  res
    "ActivateAnomalyDetectorResponse"
    "fixture/ActivateAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateAnomalyDetector)

responseListAnomalyGroupTimeSeries :: ListAnomalyGroupTimeSeriesResponse -> TestTree
responseListAnomalyGroupTimeSeries =
  res
    "ListAnomalyGroupTimeSeriesResponse"
    "fixture/ListAnomalyGroupTimeSeriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnomalyGroupTimeSeries)

responseGetSampleData :: GetSampleDataResponse -> TestTree
responseGetSampleData =
  res
    "GetSampleDataResponse"
    "fixture/GetSampleDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSampleData)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDescribeMetricSet :: DescribeMetricSetResponse -> TestTree
responseDescribeMetricSet =
  res
    "DescribeMetricSetResponse"
    "fixture/DescribeMetricSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMetricSet)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDescribeAlert :: DescribeAlertResponse -> TestTree
responseDescribeAlert =
  res
    "DescribeAlertResponse"
    "fixture/DescribeAlertResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlert)

responseListAnomalyGroupSummaries :: ListAnomalyGroupSummariesResponse -> TestTree
responseListAnomalyGroupSummaries =
  res
    "ListAnomalyGroupSummariesResponse"
    "fixture/ListAnomalyGroupSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnomalyGroupSummaries)

responseDescribeAnomalyDetector :: DescribeAnomalyDetectorResponse -> TestTree
responseDescribeAnomalyDetector =
  res
    "DescribeAnomalyDetectorResponse"
    "fixture/DescribeAnomalyDetectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnomalyDetector)
