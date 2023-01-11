{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Forecast
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestCreateAutoPredictor $
--             newCreateAutoPredictor
--
--         , requestCreateDataset $
--             newCreateDataset
--
--         , requestCreateDatasetGroup $
--             newCreateDatasetGroup
--
--         , requestCreateDatasetImportJob $
--             newCreateDatasetImportJob
--
--         , requestCreateExplainability $
--             newCreateExplainability
--
--         , requestCreateExplainabilityExport $
--             newCreateExplainabilityExport
--
--         , requestCreateForecast $
--             newCreateForecast
--
--         , requestCreateForecastExportJob $
--             newCreateForecastExportJob
--
--         , requestCreateMonitor $
--             newCreateMonitor
--
--         , requestCreatePredictor $
--             newCreatePredictor
--
--         , requestCreatePredictorBacktestExportJob $
--             newCreatePredictorBacktestExportJob
--
--         , requestCreateWhatIfAnalysis $
--             newCreateWhatIfAnalysis
--
--         , requestCreateWhatIfForecast $
--             newCreateWhatIfForecast
--
--         , requestCreateWhatIfForecastExport $
--             newCreateWhatIfForecastExport
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
--         , requestDeleteExplainability $
--             newDeleteExplainability
--
--         , requestDeleteExplainabilityExport $
--             newDeleteExplainabilityExport
--
--         , requestDeleteForecast $
--             newDeleteForecast
--
--         , requestDeleteForecastExportJob $
--             newDeleteForecastExportJob
--
--         , requestDeleteMonitor $
--             newDeleteMonitor
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
--         , requestDeleteWhatIfAnalysis $
--             newDeleteWhatIfAnalysis
--
--         , requestDeleteWhatIfForecast $
--             newDeleteWhatIfForecast
--
--         , requestDeleteWhatIfForecastExport $
--             newDeleteWhatIfForecastExport
--
--         , requestDescribeAutoPredictor $
--             newDescribeAutoPredictor
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
--         , requestDescribeExplainability $
--             newDescribeExplainability
--
--         , requestDescribeExplainabilityExport $
--             newDescribeExplainabilityExport
--
--         , requestDescribeForecast $
--             newDescribeForecast
--
--         , requestDescribeForecastExportJob $
--             newDescribeForecastExportJob
--
--         , requestDescribeMonitor $
--             newDescribeMonitor
--
--         , requestDescribePredictor $
--             newDescribePredictor
--
--         , requestDescribePredictorBacktestExportJob $
--             newDescribePredictorBacktestExportJob
--
--         , requestDescribeWhatIfAnalysis $
--             newDescribeWhatIfAnalysis
--
--         , requestDescribeWhatIfForecast $
--             newDescribeWhatIfForecast
--
--         , requestDescribeWhatIfForecastExport $
--             newDescribeWhatIfForecastExport
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
--         , requestListExplainabilities $
--             newListExplainabilities
--
--         , requestListExplainabilityExports $
--             newListExplainabilityExports
--
--         , requestListForecastExportJobs $
--             newListForecastExportJobs
--
--         , requestListForecasts $
--             newListForecasts
--
--         , requestListMonitorEvaluations $
--             newListMonitorEvaluations
--
--         , requestListMonitors $
--             newListMonitors
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
--         , requestListWhatIfAnalyses $
--             newListWhatIfAnalyses
--
--         , requestListWhatIfForecastExports $
--             newListWhatIfForecastExports
--
--         , requestListWhatIfForecasts $
--             newListWhatIfForecasts
--
--         , requestResumeResource $
--             newResumeResource
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
--         [ responseCreateAutoPredictor $
--             newCreateAutoPredictorResponse
--
--         , responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseCreateDatasetGroup $
--             newCreateDatasetGroupResponse
--
--         , responseCreateDatasetImportJob $
--             newCreateDatasetImportJobResponse
--
--         , responseCreateExplainability $
--             newCreateExplainabilityResponse
--
--         , responseCreateExplainabilityExport $
--             newCreateExplainabilityExportResponse
--
--         , responseCreateForecast $
--             newCreateForecastResponse
--
--         , responseCreateForecastExportJob $
--             newCreateForecastExportJobResponse
--
--         , responseCreateMonitor $
--             newCreateMonitorResponse
--
--         , responseCreatePredictor $
--             newCreatePredictorResponse
--
--         , responseCreatePredictorBacktestExportJob $
--             newCreatePredictorBacktestExportJobResponse
--
--         , responseCreateWhatIfAnalysis $
--             newCreateWhatIfAnalysisResponse
--
--         , responseCreateWhatIfForecast $
--             newCreateWhatIfForecastResponse
--
--         , responseCreateWhatIfForecastExport $
--             newCreateWhatIfForecastExportResponse
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
--         , responseDeleteExplainability $
--             newDeleteExplainabilityResponse
--
--         , responseDeleteExplainabilityExport $
--             newDeleteExplainabilityExportResponse
--
--         , responseDeleteForecast $
--             newDeleteForecastResponse
--
--         , responseDeleteForecastExportJob $
--             newDeleteForecastExportJobResponse
--
--         , responseDeleteMonitor $
--             newDeleteMonitorResponse
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
--         , responseDeleteWhatIfAnalysis $
--             newDeleteWhatIfAnalysisResponse
--
--         , responseDeleteWhatIfForecast $
--             newDeleteWhatIfForecastResponse
--
--         , responseDeleteWhatIfForecastExport $
--             newDeleteWhatIfForecastExportResponse
--
--         , responseDescribeAutoPredictor $
--             newDescribeAutoPredictorResponse
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
--         , responseDescribeExplainability $
--             newDescribeExplainabilityResponse
--
--         , responseDescribeExplainabilityExport $
--             newDescribeExplainabilityExportResponse
--
--         , responseDescribeForecast $
--             newDescribeForecastResponse
--
--         , responseDescribeForecastExportJob $
--             newDescribeForecastExportJobResponse
--
--         , responseDescribeMonitor $
--             newDescribeMonitorResponse
--
--         , responseDescribePredictor $
--             newDescribePredictorResponse
--
--         , responseDescribePredictorBacktestExportJob $
--             newDescribePredictorBacktestExportJobResponse
--
--         , responseDescribeWhatIfAnalysis $
--             newDescribeWhatIfAnalysisResponse
--
--         , responseDescribeWhatIfForecast $
--             newDescribeWhatIfForecastResponse
--
--         , responseDescribeWhatIfForecastExport $
--             newDescribeWhatIfForecastExportResponse
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
--         , responseListExplainabilities $
--             newListExplainabilitiesResponse
--
--         , responseListExplainabilityExports $
--             newListExplainabilityExportsResponse
--
--         , responseListForecastExportJobs $
--             newListForecastExportJobsResponse
--
--         , responseListForecasts $
--             newListForecastsResponse
--
--         , responseListMonitorEvaluations $
--             newListMonitorEvaluationsResponse
--
--         , responseListMonitors $
--             newListMonitorsResponse
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
--         , responseListWhatIfAnalyses $
--             newListWhatIfAnalysesResponse
--
--         , responseListWhatIfForecastExports $
--             newListWhatIfForecastExportsResponse
--
--         , responseListWhatIfForecasts $
--             newListWhatIfForecastsResponse
--
--         , responseResumeResource $
--             newResumeResourceResponse
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

requestCreateAutoPredictor :: CreateAutoPredictor -> TestTree
requestCreateAutoPredictor =
  req
    "CreateAutoPredictor"
    "fixture/CreateAutoPredictor.yaml"

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

requestCreateExplainability :: CreateExplainability -> TestTree
requestCreateExplainability =
  req
    "CreateExplainability"
    "fixture/CreateExplainability.yaml"

requestCreateExplainabilityExport :: CreateExplainabilityExport -> TestTree
requestCreateExplainabilityExport =
  req
    "CreateExplainabilityExport"
    "fixture/CreateExplainabilityExport.yaml"

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

requestCreateMonitor :: CreateMonitor -> TestTree
requestCreateMonitor =
  req
    "CreateMonitor"
    "fixture/CreateMonitor.yaml"

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

requestCreateWhatIfAnalysis :: CreateWhatIfAnalysis -> TestTree
requestCreateWhatIfAnalysis =
  req
    "CreateWhatIfAnalysis"
    "fixture/CreateWhatIfAnalysis.yaml"

requestCreateWhatIfForecast :: CreateWhatIfForecast -> TestTree
requestCreateWhatIfForecast =
  req
    "CreateWhatIfForecast"
    "fixture/CreateWhatIfForecast.yaml"

requestCreateWhatIfForecastExport :: CreateWhatIfForecastExport -> TestTree
requestCreateWhatIfForecastExport =
  req
    "CreateWhatIfForecastExport"
    "fixture/CreateWhatIfForecastExport.yaml"

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

requestDeleteExplainability :: DeleteExplainability -> TestTree
requestDeleteExplainability =
  req
    "DeleteExplainability"
    "fixture/DeleteExplainability.yaml"

requestDeleteExplainabilityExport :: DeleteExplainabilityExport -> TestTree
requestDeleteExplainabilityExport =
  req
    "DeleteExplainabilityExport"
    "fixture/DeleteExplainabilityExport.yaml"

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

requestDeleteMonitor :: DeleteMonitor -> TestTree
requestDeleteMonitor =
  req
    "DeleteMonitor"
    "fixture/DeleteMonitor.yaml"

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

requestDeleteWhatIfAnalysis :: DeleteWhatIfAnalysis -> TestTree
requestDeleteWhatIfAnalysis =
  req
    "DeleteWhatIfAnalysis"
    "fixture/DeleteWhatIfAnalysis.yaml"

requestDeleteWhatIfForecast :: DeleteWhatIfForecast -> TestTree
requestDeleteWhatIfForecast =
  req
    "DeleteWhatIfForecast"
    "fixture/DeleteWhatIfForecast.yaml"

requestDeleteWhatIfForecastExport :: DeleteWhatIfForecastExport -> TestTree
requestDeleteWhatIfForecastExport =
  req
    "DeleteWhatIfForecastExport"
    "fixture/DeleteWhatIfForecastExport.yaml"

requestDescribeAutoPredictor :: DescribeAutoPredictor -> TestTree
requestDescribeAutoPredictor =
  req
    "DescribeAutoPredictor"
    "fixture/DescribeAutoPredictor.yaml"

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

requestDescribeExplainability :: DescribeExplainability -> TestTree
requestDescribeExplainability =
  req
    "DescribeExplainability"
    "fixture/DescribeExplainability.yaml"

requestDescribeExplainabilityExport :: DescribeExplainabilityExport -> TestTree
requestDescribeExplainabilityExport =
  req
    "DescribeExplainabilityExport"
    "fixture/DescribeExplainabilityExport.yaml"

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

requestDescribeMonitor :: DescribeMonitor -> TestTree
requestDescribeMonitor =
  req
    "DescribeMonitor"
    "fixture/DescribeMonitor.yaml"

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

requestDescribeWhatIfAnalysis :: DescribeWhatIfAnalysis -> TestTree
requestDescribeWhatIfAnalysis =
  req
    "DescribeWhatIfAnalysis"
    "fixture/DescribeWhatIfAnalysis.yaml"

requestDescribeWhatIfForecast :: DescribeWhatIfForecast -> TestTree
requestDescribeWhatIfForecast =
  req
    "DescribeWhatIfForecast"
    "fixture/DescribeWhatIfForecast.yaml"

requestDescribeWhatIfForecastExport :: DescribeWhatIfForecastExport -> TestTree
requestDescribeWhatIfForecastExport =
  req
    "DescribeWhatIfForecastExport"
    "fixture/DescribeWhatIfForecastExport.yaml"

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

requestListExplainabilities :: ListExplainabilities -> TestTree
requestListExplainabilities =
  req
    "ListExplainabilities"
    "fixture/ListExplainabilities.yaml"

requestListExplainabilityExports :: ListExplainabilityExports -> TestTree
requestListExplainabilityExports =
  req
    "ListExplainabilityExports"
    "fixture/ListExplainabilityExports.yaml"

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

requestListMonitorEvaluations :: ListMonitorEvaluations -> TestTree
requestListMonitorEvaluations =
  req
    "ListMonitorEvaluations"
    "fixture/ListMonitorEvaluations.yaml"

requestListMonitors :: ListMonitors -> TestTree
requestListMonitors =
  req
    "ListMonitors"
    "fixture/ListMonitors.yaml"

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

requestListWhatIfAnalyses :: ListWhatIfAnalyses -> TestTree
requestListWhatIfAnalyses =
  req
    "ListWhatIfAnalyses"
    "fixture/ListWhatIfAnalyses.yaml"

requestListWhatIfForecastExports :: ListWhatIfForecastExports -> TestTree
requestListWhatIfForecastExports =
  req
    "ListWhatIfForecastExports"
    "fixture/ListWhatIfForecastExports.yaml"

requestListWhatIfForecasts :: ListWhatIfForecasts -> TestTree
requestListWhatIfForecasts =
  req
    "ListWhatIfForecasts"
    "fixture/ListWhatIfForecasts.yaml"

requestResumeResource :: ResumeResource -> TestTree
requestResumeResource =
  req
    "ResumeResource"
    "fixture/ResumeResource.yaml"

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

responseCreateAutoPredictor :: CreateAutoPredictorResponse -> TestTree
responseCreateAutoPredictor =
  res
    "CreateAutoPredictorResponse"
    "fixture/CreateAutoPredictorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAutoPredictor)

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

responseCreateExplainability :: CreateExplainabilityResponse -> TestTree
responseCreateExplainability =
  res
    "CreateExplainabilityResponse"
    "fixture/CreateExplainabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExplainability)

responseCreateExplainabilityExport :: CreateExplainabilityExportResponse -> TestTree
responseCreateExplainabilityExport =
  res
    "CreateExplainabilityExportResponse"
    "fixture/CreateExplainabilityExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExplainabilityExport)

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

responseCreateMonitor :: CreateMonitorResponse -> TestTree
responseCreateMonitor =
  res
    "CreateMonitorResponse"
    "fixture/CreateMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMonitor)

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

responseCreateWhatIfAnalysis :: CreateWhatIfAnalysisResponse -> TestTree
responseCreateWhatIfAnalysis =
  res
    "CreateWhatIfAnalysisResponse"
    "fixture/CreateWhatIfAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWhatIfAnalysis)

responseCreateWhatIfForecast :: CreateWhatIfForecastResponse -> TestTree
responseCreateWhatIfForecast =
  res
    "CreateWhatIfForecastResponse"
    "fixture/CreateWhatIfForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWhatIfForecast)

responseCreateWhatIfForecastExport :: CreateWhatIfForecastExportResponse -> TestTree
responseCreateWhatIfForecastExport =
  res
    "CreateWhatIfForecastExportResponse"
    "fixture/CreateWhatIfForecastExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWhatIfForecastExport)

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

responseDeleteExplainability :: DeleteExplainabilityResponse -> TestTree
responseDeleteExplainability =
  res
    "DeleteExplainabilityResponse"
    "fixture/DeleteExplainabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExplainability)

responseDeleteExplainabilityExport :: DeleteExplainabilityExportResponse -> TestTree
responseDeleteExplainabilityExport =
  res
    "DeleteExplainabilityExportResponse"
    "fixture/DeleteExplainabilityExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExplainabilityExport)

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

responseDeleteMonitor :: DeleteMonitorResponse -> TestTree
responseDeleteMonitor =
  res
    "DeleteMonitorResponse"
    "fixture/DeleteMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMonitor)

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

responseDeleteWhatIfAnalysis :: DeleteWhatIfAnalysisResponse -> TestTree
responseDeleteWhatIfAnalysis =
  res
    "DeleteWhatIfAnalysisResponse"
    "fixture/DeleteWhatIfAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWhatIfAnalysis)

responseDeleteWhatIfForecast :: DeleteWhatIfForecastResponse -> TestTree
responseDeleteWhatIfForecast =
  res
    "DeleteWhatIfForecastResponse"
    "fixture/DeleteWhatIfForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWhatIfForecast)

responseDeleteWhatIfForecastExport :: DeleteWhatIfForecastExportResponse -> TestTree
responseDeleteWhatIfForecastExport =
  res
    "DeleteWhatIfForecastExportResponse"
    "fixture/DeleteWhatIfForecastExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWhatIfForecastExport)

responseDescribeAutoPredictor :: DescribeAutoPredictorResponse -> TestTree
responseDescribeAutoPredictor =
  res
    "DescribeAutoPredictorResponse"
    "fixture/DescribeAutoPredictorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutoPredictor)

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

responseDescribeExplainability :: DescribeExplainabilityResponse -> TestTree
responseDescribeExplainability =
  res
    "DescribeExplainabilityResponse"
    "fixture/DescribeExplainabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExplainability)

responseDescribeExplainabilityExport :: DescribeExplainabilityExportResponse -> TestTree
responseDescribeExplainabilityExport =
  res
    "DescribeExplainabilityExportResponse"
    "fixture/DescribeExplainabilityExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExplainabilityExport)

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

responseDescribeMonitor :: DescribeMonitorResponse -> TestTree
responseDescribeMonitor =
  res
    "DescribeMonitorResponse"
    "fixture/DescribeMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMonitor)

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

responseDescribeWhatIfAnalysis :: DescribeWhatIfAnalysisResponse -> TestTree
responseDescribeWhatIfAnalysis =
  res
    "DescribeWhatIfAnalysisResponse"
    "fixture/DescribeWhatIfAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWhatIfAnalysis)

responseDescribeWhatIfForecast :: DescribeWhatIfForecastResponse -> TestTree
responseDescribeWhatIfForecast =
  res
    "DescribeWhatIfForecastResponse"
    "fixture/DescribeWhatIfForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWhatIfForecast)

responseDescribeWhatIfForecastExport :: DescribeWhatIfForecastExportResponse -> TestTree
responseDescribeWhatIfForecastExport =
  res
    "DescribeWhatIfForecastExportResponse"
    "fixture/DescribeWhatIfForecastExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWhatIfForecastExport)

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

responseListExplainabilities :: ListExplainabilitiesResponse -> TestTree
responseListExplainabilities =
  res
    "ListExplainabilitiesResponse"
    "fixture/ListExplainabilitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExplainabilities)

responseListExplainabilityExports :: ListExplainabilityExportsResponse -> TestTree
responseListExplainabilityExports =
  res
    "ListExplainabilityExportsResponse"
    "fixture/ListExplainabilityExportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExplainabilityExports)

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

responseListMonitorEvaluations :: ListMonitorEvaluationsResponse -> TestTree
responseListMonitorEvaluations =
  res
    "ListMonitorEvaluationsResponse"
    "fixture/ListMonitorEvaluationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMonitorEvaluations)

responseListMonitors :: ListMonitorsResponse -> TestTree
responseListMonitors =
  res
    "ListMonitorsResponse"
    "fixture/ListMonitorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMonitors)

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

responseListWhatIfAnalyses :: ListWhatIfAnalysesResponse -> TestTree
responseListWhatIfAnalyses =
  res
    "ListWhatIfAnalysesResponse"
    "fixture/ListWhatIfAnalysesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWhatIfAnalyses)

responseListWhatIfForecastExports :: ListWhatIfForecastExportsResponse -> TestTree
responseListWhatIfForecastExports =
  res
    "ListWhatIfForecastExportsResponse"
    "fixture/ListWhatIfForecastExportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWhatIfForecastExports)

responseListWhatIfForecasts :: ListWhatIfForecastsResponse -> TestTree
responseListWhatIfForecasts =
  res
    "ListWhatIfForecastsResponse"
    "fixture/ListWhatIfForecastsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWhatIfForecasts)

responseResumeResource :: ResumeResourceResponse -> TestTree
responseResumeResource =
  res
    "ResumeResourceResponse"
    "fixture/ResumeResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeResource)

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
