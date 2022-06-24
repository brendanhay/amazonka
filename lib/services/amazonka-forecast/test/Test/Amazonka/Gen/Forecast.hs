{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Forecast
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Forecast where

import Amazonka.Forecast
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Forecast.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateDataset $
--             newCreateDataset
--
--         , requestCreateDatasetGroup $
--             newCreateDatasetGroup
--
--         , requestCreateDatasetImportJob $
--             newCreateDatasetImportJob
--
--         , requestCreateForecast $
--             newCreateForecast
--
--         , requestCreateForecastExportJob $
--             newCreateForecastExportJob
--
--         , requestCreatePredictor $
--             newCreatePredictor
--
--         , requestCreatePredictorBacktestExportJob $
--             newCreatePredictorBacktestExportJob
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestDeleteDatasetGroup $
--             newDeleteDatasetGroup
--
--         , requestDeleteDatasetImportJob $
--             newDeleteDatasetImportJob
--
--         , requestDeleteForecast $
--             newDeleteForecast
--
--         , requestDeleteForecastExportJob $
--             newDeleteForecastExportJob
--
--         , requestDeletePredictor $
--             newDeletePredictor
--
--         , requestDeletePredictorBacktestExportJob $
--             newDeletePredictorBacktestExportJob
--
--         , requestDeleteResourceTree $
--             newDeleteResourceTree
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestDescribeDatasetGroup $
--             newDescribeDatasetGroup
--
--         , requestDescribeDatasetImportJob $
--             newDescribeDatasetImportJob
--
--         , requestDescribeForecast $
--             newDescribeForecast
--
--         , requestDescribeForecastExportJob $
--             newDescribeForecastExportJob
--
--         , requestDescribePredictor $
--             newDescribePredictor
--
--         , requestDescribePredictorBacktestExportJob $
--             newDescribePredictorBacktestExportJob
--
--         , requestGetAccuracyMetrics $
--             newGetAccuracyMetrics
--
--         , requestListDatasetGroups $
--             newListDatasetGroups
--
--         , requestListDatasetImportJobs $
--             newListDatasetImportJobs
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestListForecastExportJobs $
--             newListForecastExportJobs
--
--         , requestListForecasts $
--             newListForecasts
--
--         , requestListPredictorBacktestExportJobs $
--             newListPredictorBacktestExportJobs
--
--         , requestListPredictors $
--             newListPredictors
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStopResource $
--             newStopResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDatasetGroup $
--             newUpdateDatasetGroup
--
--           ]

--     , testGroup "response"
--         [ responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseCreateDatasetGroup $
--             newCreateDatasetGroupResponse
--
--         , responseCreateDatasetImportJob $
--             newCreateDatasetImportJobResponse
--
--         , responseCreateForecast $
--             newCreateForecastResponse
--
--         , responseCreateForecastExportJob $
--             newCreateForecastExportJobResponse
--
--         , responseCreatePredictor $
--             newCreatePredictorResponse
--
--         , responseCreatePredictorBacktestExportJob $
--             newCreatePredictorBacktestExportJobResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseDeleteDatasetGroup $
--             newDeleteDatasetGroupResponse
--
--         , responseDeleteDatasetImportJob $
--             newDeleteDatasetImportJobResponse
--
--         , responseDeleteForecast $
--             newDeleteForecastResponse
--
--         , responseDeleteForecastExportJob $
--             newDeleteForecastExportJobResponse
--
--         , responseDeletePredictor $
--             newDeletePredictorResponse
--
--         , responseDeletePredictorBacktestExportJob $
--             newDeletePredictorBacktestExportJobResponse
--
--         , responseDeleteResourceTree $
--             newDeleteResourceTreeResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseDescribeDatasetGroup $
--             newDescribeDatasetGroupResponse
--
--         , responseDescribeDatasetImportJob $
--             newDescribeDatasetImportJobResponse
--
--         , responseDescribeForecast $
--             newDescribeForecastResponse
--
--         , responseDescribeForecastExportJob $
--             newDescribeForecastExportJobResponse
--
--         , responseDescribePredictor $
--             newDescribePredictorResponse
--
--         , responseDescribePredictorBacktestExportJob $
--             newDescribePredictorBacktestExportJobResponse
--
--         , responseGetAccuracyMetrics $
--             newGetAccuracyMetricsResponse
--
--         , responseListDatasetGroups $
--             newListDatasetGroupsResponse
--
--         , responseListDatasetImportJobs $
--             newListDatasetImportJobsResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseListForecastExportJobs $
--             newListForecastExportJobsResponse
--
--         , responseListForecasts $
--             newListForecastsResponse
--
--         , responseListPredictorBacktestExportJobs $
--             newListPredictorBacktestExportJobsResponse
--
--         , responseListPredictors $
--             newListPredictorsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStopResource $
--             newStopResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDatasetGroup $
--             newUpdateDatasetGroupResponse
--
--           ]
--     ]

-- Requests

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset =
  req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestCreateDatasetGroup :: CreateDatasetGroup -> TestTree
requestCreateDatasetGroup =
  req
    "CreateDatasetGroup"
    "fixture/CreateDatasetGroup.yaml"

requestCreateDatasetImportJob :: CreateDatasetImportJob -> TestTree
requestCreateDatasetImportJob =
  req
    "CreateDatasetImportJob"
    "fixture/CreateDatasetImportJob.yaml"

requestCreateForecast :: CreateForecast -> TestTree
requestCreateForecast =
  req
    "CreateForecast"
    "fixture/CreateForecast.yaml"

requestCreateForecastExportJob :: CreateForecastExportJob -> TestTree
requestCreateForecastExportJob =
  req
    "CreateForecastExportJob"
    "fixture/CreateForecastExportJob.yaml"

requestCreatePredictor :: CreatePredictor -> TestTree
requestCreatePredictor =
  req
    "CreatePredictor"
    "fixture/CreatePredictor.yaml"

requestCreatePredictorBacktestExportJob :: CreatePredictorBacktestExportJob -> TestTree
requestCreatePredictorBacktestExportJob =
  req
    "CreatePredictorBacktestExportJob"
    "fixture/CreatePredictorBacktestExportJob.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestDeleteDatasetGroup :: DeleteDatasetGroup -> TestTree
requestDeleteDatasetGroup =
  req
    "DeleteDatasetGroup"
    "fixture/DeleteDatasetGroup.yaml"

requestDeleteDatasetImportJob :: DeleteDatasetImportJob -> TestTree
requestDeleteDatasetImportJob =
  req
    "DeleteDatasetImportJob"
    "fixture/DeleteDatasetImportJob.yaml"

requestDeleteForecast :: DeleteForecast -> TestTree
requestDeleteForecast =
  req
    "DeleteForecast"
    "fixture/DeleteForecast.yaml"

requestDeleteForecastExportJob :: DeleteForecastExportJob -> TestTree
requestDeleteForecastExportJob =
  req
    "DeleteForecastExportJob"
    "fixture/DeleteForecastExportJob.yaml"

requestDeletePredictor :: DeletePredictor -> TestTree
requestDeletePredictor =
  req
    "DeletePredictor"
    "fixture/DeletePredictor.yaml"

requestDeletePredictorBacktestExportJob :: DeletePredictorBacktestExportJob -> TestTree
requestDeletePredictorBacktestExportJob =
  req
    "DeletePredictorBacktestExportJob"
    "fixture/DeletePredictorBacktestExportJob.yaml"

requestDeleteResourceTree :: DeleteResourceTree -> TestTree
requestDeleteResourceTree =
  req
    "DeleteResourceTree"
    "fixture/DeleteResourceTree.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestDescribeDatasetGroup :: DescribeDatasetGroup -> TestTree
requestDescribeDatasetGroup =
  req
    "DescribeDatasetGroup"
    "fixture/DescribeDatasetGroup.yaml"

requestDescribeDatasetImportJob :: DescribeDatasetImportJob -> TestTree
requestDescribeDatasetImportJob =
  req
    "DescribeDatasetImportJob"
    "fixture/DescribeDatasetImportJob.yaml"

requestDescribeForecast :: DescribeForecast -> TestTree
requestDescribeForecast =
  req
    "DescribeForecast"
    "fixture/DescribeForecast.yaml"

requestDescribeForecastExportJob :: DescribeForecastExportJob -> TestTree
requestDescribeForecastExportJob =
  req
    "DescribeForecastExportJob"
    "fixture/DescribeForecastExportJob.yaml"

requestDescribePredictor :: DescribePredictor -> TestTree
requestDescribePredictor =
  req
    "DescribePredictor"
    "fixture/DescribePredictor.yaml"

requestDescribePredictorBacktestExportJob :: DescribePredictorBacktestExportJob -> TestTree
requestDescribePredictorBacktestExportJob =
  req
    "DescribePredictorBacktestExportJob"
    "fixture/DescribePredictorBacktestExportJob.yaml"

requestGetAccuracyMetrics :: GetAccuracyMetrics -> TestTree
requestGetAccuracyMetrics =
  req
    "GetAccuracyMetrics"
    "fixture/GetAccuracyMetrics.yaml"

requestListDatasetGroups :: ListDatasetGroups -> TestTree
requestListDatasetGroups =
  req
    "ListDatasetGroups"
    "fixture/ListDatasetGroups.yaml"

requestListDatasetImportJobs :: ListDatasetImportJobs -> TestTree
requestListDatasetImportJobs =
  req
    "ListDatasetImportJobs"
    "fixture/ListDatasetImportJobs.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestListForecastExportJobs :: ListForecastExportJobs -> TestTree
requestListForecastExportJobs =
  req
    "ListForecastExportJobs"
    "fixture/ListForecastExportJobs.yaml"

requestListForecasts :: ListForecasts -> TestTree
requestListForecasts =
  req
    "ListForecasts"
    "fixture/ListForecasts.yaml"

requestListPredictorBacktestExportJobs :: ListPredictorBacktestExportJobs -> TestTree
requestListPredictorBacktestExportJobs =
  req
    "ListPredictorBacktestExportJobs"
    "fixture/ListPredictorBacktestExportJobs.yaml"

requestListPredictors :: ListPredictors -> TestTree
requestListPredictors =
  req
    "ListPredictors"
    "fixture/ListPredictors.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStopResource :: StopResource -> TestTree
requestStopResource =
  req
    "StopResource"
    "fixture/StopResource.yaml"

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

requestUpdateDatasetGroup :: UpdateDatasetGroup -> TestTree
requestUpdateDatasetGroup =
  req
    "UpdateDatasetGroup"
    "fixture/UpdateDatasetGroup.yaml"

-- Responses

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataset)

responseCreateDatasetGroup :: CreateDatasetGroupResponse -> TestTree
responseCreateDatasetGroup =
  res
    "CreateDatasetGroupResponse"
    "fixture/CreateDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatasetGroup)

responseCreateDatasetImportJob :: CreateDatasetImportJobResponse -> TestTree
responseCreateDatasetImportJob =
  res
    "CreateDatasetImportJobResponse"
    "fixture/CreateDatasetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatasetImportJob)

responseCreateForecast :: CreateForecastResponse -> TestTree
responseCreateForecast =
  res
    "CreateForecastResponse"
    "fixture/CreateForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateForecast)

responseCreateForecastExportJob :: CreateForecastExportJobResponse -> TestTree
responseCreateForecastExportJob =
  res
    "CreateForecastExportJobResponse"
    "fixture/CreateForecastExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateForecastExportJob)

responseCreatePredictor :: CreatePredictorResponse -> TestTree
responseCreatePredictor =
  res
    "CreatePredictorResponse"
    "fixture/CreatePredictorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePredictor)

responseCreatePredictorBacktestExportJob :: CreatePredictorBacktestExportJobResponse -> TestTree
responseCreatePredictorBacktestExportJob =
  res
    "CreatePredictorBacktestExportJobResponse"
    "fixture/CreatePredictorBacktestExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePredictorBacktestExportJob)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataset)

responseDeleteDatasetGroup :: DeleteDatasetGroupResponse -> TestTree
responseDeleteDatasetGroup =
  res
    "DeleteDatasetGroupResponse"
    "fixture/DeleteDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatasetGroup)

responseDeleteDatasetImportJob :: DeleteDatasetImportJobResponse -> TestTree
responseDeleteDatasetImportJob =
  res
    "DeleteDatasetImportJobResponse"
    "fixture/DeleteDatasetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatasetImportJob)

responseDeleteForecast :: DeleteForecastResponse -> TestTree
responseDeleteForecast =
  res
    "DeleteForecastResponse"
    "fixture/DeleteForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteForecast)

responseDeleteForecastExportJob :: DeleteForecastExportJobResponse -> TestTree
responseDeleteForecastExportJob =
  res
    "DeleteForecastExportJobResponse"
    "fixture/DeleteForecastExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteForecastExportJob)

responseDeletePredictor :: DeletePredictorResponse -> TestTree
responseDeletePredictor =
  res
    "DeletePredictorResponse"
    "fixture/DeletePredictorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePredictor)

responseDeletePredictorBacktestExportJob :: DeletePredictorBacktestExportJobResponse -> TestTree
responseDeletePredictorBacktestExportJob =
  res
    "DeletePredictorBacktestExportJobResponse"
    "fixture/DeletePredictorBacktestExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePredictorBacktestExportJob)

responseDeleteResourceTree :: DeleteResourceTreeResponse -> TestTree
responseDeleteResourceTree =
  res
    "DeleteResourceTreeResponse"
    "fixture/DeleteResourceTreeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceTree)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataset)

responseDescribeDatasetGroup :: DescribeDatasetGroupResponse -> TestTree
responseDescribeDatasetGroup =
  res
    "DescribeDatasetGroupResponse"
    "fixture/DescribeDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDatasetGroup)

responseDescribeDatasetImportJob :: DescribeDatasetImportJobResponse -> TestTree
responseDescribeDatasetImportJob =
  res
    "DescribeDatasetImportJobResponse"
    "fixture/DescribeDatasetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDatasetImportJob)

responseDescribeForecast :: DescribeForecastResponse -> TestTree
responseDescribeForecast =
  res
    "DescribeForecastResponse"
    "fixture/DescribeForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeForecast)

responseDescribeForecastExportJob :: DescribeForecastExportJobResponse -> TestTree
responseDescribeForecastExportJob =
  res
    "DescribeForecastExportJobResponse"
    "fixture/DescribeForecastExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeForecastExportJob)

responseDescribePredictor :: DescribePredictorResponse -> TestTree
responseDescribePredictor =
  res
    "DescribePredictorResponse"
    "fixture/DescribePredictorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePredictor)

responseDescribePredictorBacktestExportJob :: DescribePredictorBacktestExportJobResponse -> TestTree
responseDescribePredictorBacktestExportJob =
  res
    "DescribePredictorBacktestExportJobResponse"
    "fixture/DescribePredictorBacktestExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePredictorBacktestExportJob)

responseGetAccuracyMetrics :: GetAccuracyMetricsResponse -> TestTree
responseGetAccuracyMetrics =
  res
    "GetAccuracyMetricsResponse"
    "fixture/GetAccuracyMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccuracyMetrics)

responseListDatasetGroups :: ListDatasetGroupsResponse -> TestTree
responseListDatasetGroups =
  res
    "ListDatasetGroupsResponse"
    "fixture/ListDatasetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetGroups)

responseListDatasetImportJobs :: ListDatasetImportJobsResponse -> TestTree
responseListDatasetImportJobs =
  res
    "ListDatasetImportJobsResponse"
    "fixture/ListDatasetImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetImportJobs)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasets)

responseListForecastExportJobs :: ListForecastExportJobsResponse -> TestTree
responseListForecastExportJobs =
  res
    "ListForecastExportJobsResponse"
    "fixture/ListForecastExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListForecastExportJobs)

responseListForecasts :: ListForecastsResponse -> TestTree
responseListForecasts =
  res
    "ListForecastsResponse"
    "fixture/ListForecastsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListForecasts)

responseListPredictorBacktestExportJobs :: ListPredictorBacktestExportJobsResponse -> TestTree
responseListPredictorBacktestExportJobs =
  res
    "ListPredictorBacktestExportJobsResponse"
    "fixture/ListPredictorBacktestExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPredictorBacktestExportJobs)

responseListPredictors :: ListPredictorsResponse -> TestTree
responseListPredictors =
  res
    "ListPredictorsResponse"
    "fixture/ListPredictorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPredictors)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStopResource :: StopResourceResponse -> TestTree
responseStopResource =
  res
    "StopResourceResponse"
    "fixture/StopResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopResource)

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

responseUpdateDatasetGroup :: UpdateDatasetGroupResponse -> TestTree
responseUpdateDatasetGroup =
  res
    "UpdateDatasetGroupResponse"
    "fixture/UpdateDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDatasetGroup)
