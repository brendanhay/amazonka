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
--         [ requestListDatasetGroups $
--             newListDatasetGroups
--
--         , requestCreateDatasetImportJob $
--             newCreateDatasetImportJob
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestListForecasts $
--             newListForecasts
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStopResource $
--             newStopResource
--
--         , requestDescribeDatasetImportJob $
--             newDescribeDatasetImportJob
--
--         , requestDescribeForecastExportJob $
--             newDescribeForecastExportJob
--
--         , requestDescribePredictor $
--             newDescribePredictor
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestDescribeForecast $
--             newDescribeForecast
--
--         , requestCreateDataset $
--             newCreateDataset
--
--         , requestDeleteForecastExportJob $
--             newDeleteForecastExportJob
--
--         , requestDeletePredictor $
--             newDeletePredictor
--
--         , requestListDatasetImportJobs $
--             newListDatasetImportJobs
--
--         , requestDeleteDatasetImportJob $
--             newDeleteDatasetImportJob
--
--         , requestGetAccuracyMetrics $
--             newGetAccuracyMetrics
--
--         , requestDeleteDatasetGroup $
--             newDeleteDatasetGroup
--
--         , requestUpdateDatasetGroup $
--             newUpdateDatasetGroup
--
--         , requestCreateForecastExportJob $
--             newCreateForecastExportJob
--
--         , requestCreatePredictor $
--             newCreatePredictor
--
--         , requestListPredictorBacktestExportJobs $
--             newListPredictorBacktestExportJobs
--
--         , requestDeletePredictorBacktestExportJob $
--             newDeletePredictorBacktestExportJob
--
--         , requestCreateForecast $
--             newCreateForecast
--
--         , requestCreatePredictorBacktestExportJob $
--             newCreatePredictorBacktestExportJob
--
--         , requestDeleteForecast $
--             newDeleteForecast
--
--         , requestDeleteResourceTree $
--             newDeleteResourceTree
--
--         , requestDescribeDatasetGroup $
--             newDescribeDatasetGroup
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestDescribePredictorBacktestExportJob $
--             newDescribePredictorBacktestExportJob
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateDatasetGroup $
--             newCreateDatasetGroup
--
--         , requestListForecastExportJobs $
--             newListForecastExportJobs
--
--         , requestListPredictors $
--             newListPredictors
--
--           ]

--     , testGroup "response"
--         [ responseListDatasetGroups $
--             newListDatasetGroupsResponse
--
--         , responseCreateDatasetImportJob $
--             newCreateDatasetImportJobResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseListForecasts $
--             newListForecastsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStopResource $
--             newStopResourceResponse
--
--         , responseDescribeDatasetImportJob $
--             newDescribeDatasetImportJobResponse
--
--         , responseDescribeForecastExportJob $
--             newDescribeForecastExportJobResponse
--
--         , responseDescribePredictor $
--             newDescribePredictorResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseDescribeForecast $
--             newDescribeForecastResponse
--
--         , responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseDeleteForecastExportJob $
--             newDeleteForecastExportJobResponse
--
--         , responseDeletePredictor $
--             newDeletePredictorResponse
--
--         , responseListDatasetImportJobs $
--             newListDatasetImportJobsResponse
--
--         , responseDeleteDatasetImportJob $
--             newDeleteDatasetImportJobResponse
--
--         , responseGetAccuracyMetrics $
--             newGetAccuracyMetricsResponse
--
--         , responseDeleteDatasetGroup $
--             newDeleteDatasetGroupResponse
--
--         , responseUpdateDatasetGroup $
--             newUpdateDatasetGroupResponse
--
--         , responseCreateForecastExportJob $
--             newCreateForecastExportJobResponse
--
--         , responseCreatePredictor $
--             newCreatePredictorResponse
--
--         , responseListPredictorBacktestExportJobs $
--             newListPredictorBacktestExportJobsResponse
--
--         , responseDeletePredictorBacktestExportJob $
--             newDeletePredictorBacktestExportJobResponse
--
--         , responseCreateForecast $
--             newCreateForecastResponse
--
--         , responseCreatePredictorBacktestExportJob $
--             newCreatePredictorBacktestExportJobResponse
--
--         , responseDeleteForecast $
--             newDeleteForecastResponse
--
--         , responseDeleteResourceTree $
--             newDeleteResourceTreeResponse
--
--         , responseDescribeDatasetGroup $
--             newDescribeDatasetGroupResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseDescribePredictorBacktestExportJob $
--             newDescribePredictorBacktestExportJobResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateDatasetGroup $
--             newCreateDatasetGroupResponse
--
--         , responseListForecastExportJobs $
--             newListForecastExportJobsResponse
--
--         , responseListPredictors $
--             newListPredictorsResponse
--
--           ]
--     ]

-- Requests

requestListDatasetGroups :: ListDatasetGroups -> TestTree
requestListDatasetGroups =
  req
    "ListDatasetGroups"
    "fixture/ListDatasetGroups.yaml"

requestCreateDatasetImportJob :: CreateDatasetImportJob -> TestTree
requestCreateDatasetImportJob =
  req
    "CreateDatasetImportJob"
    "fixture/CreateDatasetImportJob.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestListForecasts :: ListForecasts -> TestTree
requestListForecasts =
  req
    "ListForecasts"
    "fixture/ListForecasts.yaml"

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

requestDescribeDatasetImportJob :: DescribeDatasetImportJob -> TestTree
requestDescribeDatasetImportJob =
  req
    "DescribeDatasetImportJob"
    "fixture/DescribeDatasetImportJob.yaml"

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

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestDescribeForecast :: DescribeForecast -> TestTree
requestDescribeForecast =
  req
    "DescribeForecast"
    "fixture/DescribeForecast.yaml"

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset =
  req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

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

requestListDatasetImportJobs :: ListDatasetImportJobs -> TestTree
requestListDatasetImportJobs =
  req
    "ListDatasetImportJobs"
    "fixture/ListDatasetImportJobs.yaml"

requestDeleteDatasetImportJob :: DeleteDatasetImportJob -> TestTree
requestDeleteDatasetImportJob =
  req
    "DeleteDatasetImportJob"
    "fixture/DeleteDatasetImportJob.yaml"

requestGetAccuracyMetrics :: GetAccuracyMetrics -> TestTree
requestGetAccuracyMetrics =
  req
    "GetAccuracyMetrics"
    "fixture/GetAccuracyMetrics.yaml"

requestDeleteDatasetGroup :: DeleteDatasetGroup -> TestTree
requestDeleteDatasetGroup =
  req
    "DeleteDatasetGroup"
    "fixture/DeleteDatasetGroup.yaml"

requestUpdateDatasetGroup :: UpdateDatasetGroup -> TestTree
requestUpdateDatasetGroup =
  req
    "UpdateDatasetGroup"
    "fixture/UpdateDatasetGroup.yaml"

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

requestListPredictorBacktestExportJobs :: ListPredictorBacktestExportJobs -> TestTree
requestListPredictorBacktestExportJobs =
  req
    "ListPredictorBacktestExportJobs"
    "fixture/ListPredictorBacktestExportJobs.yaml"

requestDeletePredictorBacktestExportJob :: DeletePredictorBacktestExportJob -> TestTree
requestDeletePredictorBacktestExportJob =
  req
    "DeletePredictorBacktestExportJob"
    "fixture/DeletePredictorBacktestExportJob.yaml"

requestCreateForecast :: CreateForecast -> TestTree
requestCreateForecast =
  req
    "CreateForecast"
    "fixture/CreateForecast.yaml"

requestCreatePredictorBacktestExportJob :: CreatePredictorBacktestExportJob -> TestTree
requestCreatePredictorBacktestExportJob =
  req
    "CreatePredictorBacktestExportJob"
    "fixture/CreatePredictorBacktestExportJob.yaml"

requestDeleteForecast :: DeleteForecast -> TestTree
requestDeleteForecast =
  req
    "DeleteForecast"
    "fixture/DeleteForecast.yaml"

requestDeleteResourceTree :: DeleteResourceTree -> TestTree
requestDeleteResourceTree =
  req
    "DeleteResourceTree"
    "fixture/DeleteResourceTree.yaml"

requestDescribeDatasetGroup :: DescribeDatasetGroup -> TestTree
requestDescribeDatasetGroup =
  req
    "DescribeDatasetGroup"
    "fixture/DescribeDatasetGroup.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestDescribePredictorBacktestExportJob :: DescribePredictorBacktestExportJob -> TestTree
requestDescribePredictorBacktestExportJob =
  req
    "DescribePredictorBacktestExportJob"
    "fixture/DescribePredictorBacktestExportJob.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateDatasetGroup :: CreateDatasetGroup -> TestTree
requestCreateDatasetGroup =
  req
    "CreateDatasetGroup"
    "fixture/CreateDatasetGroup.yaml"

requestListForecastExportJobs :: ListForecastExportJobs -> TestTree
requestListForecastExportJobs =
  req
    "ListForecastExportJobs"
    "fixture/ListForecastExportJobs.yaml"

requestListPredictors :: ListPredictors -> TestTree
requestListPredictors =
  req
    "ListPredictors"
    "fixture/ListPredictors.yaml"

-- Responses

responseListDatasetGroups :: ListDatasetGroupsResponse -> TestTree
responseListDatasetGroups =
  res
    "ListDatasetGroupsResponse"
    "fixture/ListDatasetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetGroups)

responseCreateDatasetImportJob :: CreateDatasetImportJobResponse -> TestTree
responseCreateDatasetImportJob =
  res
    "CreateDatasetImportJobResponse"
    "fixture/CreateDatasetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatasetImportJob)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataset)

responseListForecasts :: ListForecastsResponse -> TestTree
responseListForecasts =
  res
    "ListForecastsResponse"
    "fixture/ListForecastsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListForecasts)

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

responseDescribeDatasetImportJob :: DescribeDatasetImportJobResponse -> TestTree
responseDescribeDatasetImportJob =
  res
    "DescribeDatasetImportJobResponse"
    "fixture/DescribeDatasetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDatasetImportJob)

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

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataset)

responseDescribeForecast :: DescribeForecastResponse -> TestTree
responseDescribeForecast =
  res
    "DescribeForecastResponse"
    "fixture/DescribeForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeForecast)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataset)

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

responseListDatasetImportJobs :: ListDatasetImportJobsResponse -> TestTree
responseListDatasetImportJobs =
  res
    "ListDatasetImportJobsResponse"
    "fixture/ListDatasetImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetImportJobs)

responseDeleteDatasetImportJob :: DeleteDatasetImportJobResponse -> TestTree
responseDeleteDatasetImportJob =
  res
    "DeleteDatasetImportJobResponse"
    "fixture/DeleteDatasetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatasetImportJob)

responseGetAccuracyMetrics :: GetAccuracyMetricsResponse -> TestTree
responseGetAccuracyMetrics =
  res
    "GetAccuracyMetricsResponse"
    "fixture/GetAccuracyMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccuracyMetrics)

responseDeleteDatasetGroup :: DeleteDatasetGroupResponse -> TestTree
responseDeleteDatasetGroup =
  res
    "DeleteDatasetGroupResponse"
    "fixture/DeleteDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatasetGroup)

responseUpdateDatasetGroup :: UpdateDatasetGroupResponse -> TestTree
responseUpdateDatasetGroup =
  res
    "UpdateDatasetGroupResponse"
    "fixture/UpdateDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDatasetGroup)

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

responseListPredictorBacktestExportJobs :: ListPredictorBacktestExportJobsResponse -> TestTree
responseListPredictorBacktestExportJobs =
  res
    "ListPredictorBacktestExportJobsResponse"
    "fixture/ListPredictorBacktestExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPredictorBacktestExportJobs)

responseDeletePredictorBacktestExportJob :: DeletePredictorBacktestExportJobResponse -> TestTree
responseDeletePredictorBacktestExportJob =
  res
    "DeletePredictorBacktestExportJobResponse"
    "fixture/DeletePredictorBacktestExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePredictorBacktestExportJob)

responseCreateForecast :: CreateForecastResponse -> TestTree
responseCreateForecast =
  res
    "CreateForecastResponse"
    "fixture/CreateForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateForecast)

responseCreatePredictorBacktestExportJob :: CreatePredictorBacktestExportJobResponse -> TestTree
responseCreatePredictorBacktestExportJob =
  res
    "CreatePredictorBacktestExportJobResponse"
    "fixture/CreatePredictorBacktestExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePredictorBacktestExportJob)

responseDeleteForecast :: DeleteForecastResponse -> TestTree
responseDeleteForecast =
  res
    "DeleteForecastResponse"
    "fixture/DeleteForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteForecast)

responseDeleteResourceTree :: DeleteResourceTreeResponse -> TestTree
responseDeleteResourceTree =
  res
    "DeleteResourceTreeResponse"
    "fixture/DeleteResourceTreeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceTree)

responseDescribeDatasetGroup :: DescribeDatasetGroupResponse -> TestTree
responseDescribeDatasetGroup =
  res
    "DescribeDatasetGroupResponse"
    "fixture/DescribeDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDatasetGroup)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasets)

responseDescribePredictorBacktestExportJob :: DescribePredictorBacktestExportJobResponse -> TestTree
responseDescribePredictorBacktestExportJob =
  res
    "DescribePredictorBacktestExportJobResponse"
    "fixture/DescribePredictorBacktestExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePredictorBacktestExportJob)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseCreateDatasetGroup :: CreateDatasetGroupResponse -> TestTree
responseCreateDatasetGroup =
  res
    "CreateDatasetGroupResponse"
    "fixture/CreateDatasetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatasetGroup)

responseListForecastExportJobs :: ListForecastExportJobsResponse -> TestTree
responseListForecastExportJobs =
  res
    "ListForecastExportJobsResponse"
    "fixture/ListForecastExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListForecastExportJobs)

responseListPredictors :: ListPredictorsResponse -> TestTree
responseListPredictors =
  res
    "ListPredictorsResponse"
    "fixture/ListPredictorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPredictors)
