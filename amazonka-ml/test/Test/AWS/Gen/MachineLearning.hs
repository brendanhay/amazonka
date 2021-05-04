{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MachineLearning
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MachineLearning where

import Data.Proxy
import Network.AWS.MachineLearning
import Test.AWS.Fixture
import Test.AWS.MachineLearning.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteMLModel $
--             newDeleteMLModel
--
--         , requestUpdateMLModel $
--             newUpdateMLModel
--
--         , requestCreateDataSourceFromS $
--             newCreateDataSourceFromS
--
--         , requestCreateDataSourceFromRedshift $
--             newCreateDataSourceFromRedshift
--
--         , requestUpdateDataSource $
--             newUpdateDataSource
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDeleteDataSource $
--             newDeleteDataSource
--
--         , requestDescribeDataSources $
--             newDescribeDataSources
--
--         , requestDescribeEvaluations $
--             newDescribeEvaluations
--
--         , requestAddTags $
--             newAddTags
--
--         , requestGetMLModel $
--             newGetMLModel
--
--         , requestGetEvaluation $
--             newGetEvaluation
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDeleteRealtimeEndpoint $
--             newDeleteRealtimeEndpoint
--
--         , requestCreateDataSourceFromRDS $
--             newCreateDataSourceFromRDS
--
--         , requestGetBatchPrediction $
--             newGetBatchPrediction
--
--         , requestDescribeBatchPredictions $
--             newDescribeBatchPredictions
--
--         , requestDeleteEvaluation $
--             newDeleteEvaluation
--
--         , requestUpdateEvaluation $
--             newUpdateEvaluation
--
--         , requestGetDataSource $
--             newGetDataSource
--
--         , requestCreateRealtimeEndpoint $
--             newCreateRealtimeEndpoint
--
--         , requestUpdateBatchPrediction $
--             newUpdateBatchPrediction
--
--         , requestDeleteBatchPrediction $
--             newDeleteBatchPrediction
--
--         , requestDescribeMLModels $
--             newDescribeMLModels
--
--         , requestCreateBatchPrediction $
--             newCreateBatchPrediction
--
--         , requestPredict $
--             newPredict
--
--         , requestCreateMLModel $
--             newCreateMLModel
--
--         , requestCreateEvaluation $
--             newCreateEvaluation
--
--           ]

--     , testGroup "response"
--         [ responseDeleteMLModel $
--             newDeleteMLModelResponse
--
--         , responseUpdateMLModel $
--             newUpdateMLModelResponse
--
--         , responseCreateDataSourceFromS $
--             newCreateDataSourceFromSResponse
--
--         , responseCreateDataSourceFromRedshift $
--             newCreateDataSourceFromRedshiftResponse
--
--         , responseUpdateDataSource $
--             newUpdateDataSourceResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDeleteDataSource $
--             newDeleteDataSourceResponse
--
--         , responseDescribeDataSources $
--             newDescribeDataSourcesResponse
--
--         , responseDescribeEvaluations $
--             newDescribeEvaluationsResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseGetMLModel $
--             newGetMLModelResponse
--
--         , responseGetEvaluation $
--             newGetEvaluationResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDeleteRealtimeEndpoint $
--             newDeleteRealtimeEndpointResponse
--
--         , responseCreateDataSourceFromRDS $
--             newCreateDataSourceFromRDSResponse
--
--         , responseGetBatchPrediction $
--             newGetBatchPredictionResponse
--
--         , responseDescribeBatchPredictions $
--             newDescribeBatchPredictionsResponse
--
--         , responseDeleteEvaluation $
--             newDeleteEvaluationResponse
--
--         , responseUpdateEvaluation $
--             newUpdateEvaluationResponse
--
--         , responseGetDataSource $
--             newGetDataSourceResponse
--
--         , responseCreateRealtimeEndpoint $
--             newCreateRealtimeEndpointResponse
--
--         , responseUpdateBatchPrediction $
--             newUpdateBatchPredictionResponse
--
--         , responseDeleteBatchPrediction $
--             newDeleteBatchPredictionResponse
--
--         , responseDescribeMLModels $
--             newDescribeMLModelsResponse
--
--         , responseCreateBatchPrediction $
--             newCreateBatchPredictionResponse
--
--         , responsePredict $
--             newPredictResponse
--
--         , responseCreateMLModel $
--             newCreateMLModelResponse
--
--         , responseCreateEvaluation $
--             newCreateEvaluationResponse
--
--           ]
--     ]

-- Requests

requestDeleteMLModel :: DeleteMLModel -> TestTree
requestDeleteMLModel =
  req
    "DeleteMLModel"
    "fixture/DeleteMLModel.yaml"

requestUpdateMLModel :: UpdateMLModel -> TestTree
requestUpdateMLModel =
  req
    "UpdateMLModel"
    "fixture/UpdateMLModel.yaml"

requestCreateDataSourceFromS :: CreateDataSourceFromS -> TestTree
requestCreateDataSourceFromS =
  req
    "CreateDataSourceFromS"
    "fixture/CreateDataSourceFromS.yaml"

requestCreateDataSourceFromRedshift :: CreateDataSourceFromRedshift -> TestTree
requestCreateDataSourceFromRedshift =
  req
    "CreateDataSourceFromRedshift"
    "fixture/CreateDataSourceFromRedshift.yaml"

requestUpdateDataSource :: UpdateDataSource -> TestTree
requestUpdateDataSource =
  req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDeleteDataSource :: DeleteDataSource -> TestTree
requestDeleteDataSource =
  req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

requestDescribeDataSources :: DescribeDataSources -> TestTree
requestDescribeDataSources =
  req
    "DescribeDataSources"
    "fixture/DescribeDataSources.yaml"

requestDescribeEvaluations :: DescribeEvaluations -> TestTree
requestDescribeEvaluations =
  req
    "DescribeEvaluations"
    "fixture/DescribeEvaluations.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestGetMLModel :: GetMLModel -> TestTree
requestGetMLModel =
  req
    "GetMLModel"
    "fixture/GetMLModel.yaml"

requestGetEvaluation :: GetEvaluation -> TestTree
requestGetEvaluation =
  req
    "GetEvaluation"
    "fixture/GetEvaluation.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDeleteRealtimeEndpoint :: DeleteRealtimeEndpoint -> TestTree
requestDeleteRealtimeEndpoint =
  req
    "DeleteRealtimeEndpoint"
    "fixture/DeleteRealtimeEndpoint.yaml"

requestCreateDataSourceFromRDS :: CreateDataSourceFromRDS -> TestTree
requestCreateDataSourceFromRDS =
  req
    "CreateDataSourceFromRDS"
    "fixture/CreateDataSourceFromRDS.yaml"

requestGetBatchPrediction :: GetBatchPrediction -> TestTree
requestGetBatchPrediction =
  req
    "GetBatchPrediction"
    "fixture/GetBatchPrediction.yaml"

requestDescribeBatchPredictions :: DescribeBatchPredictions -> TestTree
requestDescribeBatchPredictions =
  req
    "DescribeBatchPredictions"
    "fixture/DescribeBatchPredictions.yaml"

requestDeleteEvaluation :: DeleteEvaluation -> TestTree
requestDeleteEvaluation =
  req
    "DeleteEvaluation"
    "fixture/DeleteEvaluation.yaml"

requestUpdateEvaluation :: UpdateEvaluation -> TestTree
requestUpdateEvaluation =
  req
    "UpdateEvaluation"
    "fixture/UpdateEvaluation.yaml"

requestGetDataSource :: GetDataSource -> TestTree
requestGetDataSource =
  req
    "GetDataSource"
    "fixture/GetDataSource.yaml"

requestCreateRealtimeEndpoint :: CreateRealtimeEndpoint -> TestTree
requestCreateRealtimeEndpoint =
  req
    "CreateRealtimeEndpoint"
    "fixture/CreateRealtimeEndpoint.yaml"

requestUpdateBatchPrediction :: UpdateBatchPrediction -> TestTree
requestUpdateBatchPrediction =
  req
    "UpdateBatchPrediction"
    "fixture/UpdateBatchPrediction.yaml"

requestDeleteBatchPrediction :: DeleteBatchPrediction -> TestTree
requestDeleteBatchPrediction =
  req
    "DeleteBatchPrediction"
    "fixture/DeleteBatchPrediction.yaml"

requestDescribeMLModels :: DescribeMLModels -> TestTree
requestDescribeMLModels =
  req
    "DescribeMLModels"
    "fixture/DescribeMLModels.yaml"

requestCreateBatchPrediction :: CreateBatchPrediction -> TestTree
requestCreateBatchPrediction =
  req
    "CreateBatchPrediction"
    "fixture/CreateBatchPrediction.yaml"

requestPredict :: Predict -> TestTree
requestPredict =
  req
    "Predict"
    "fixture/Predict.yaml"

requestCreateMLModel :: CreateMLModel -> TestTree
requestCreateMLModel =
  req
    "CreateMLModel"
    "fixture/CreateMLModel.yaml"

requestCreateEvaluation :: CreateEvaluation -> TestTree
requestCreateEvaluation =
  req
    "CreateEvaluation"
    "fixture/CreateEvaluation.yaml"

-- Responses

responseDeleteMLModel :: DeleteMLModelResponse -> TestTree
responseDeleteMLModel =
  res
    "DeleteMLModelResponse"
    "fixture/DeleteMLModelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMLModel)

responseUpdateMLModel :: UpdateMLModelResponse -> TestTree
responseUpdateMLModel =
  res
    "UpdateMLModelResponse"
    "fixture/UpdateMLModelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMLModel)

responseCreateDataSourceFromS :: CreateDataSourceFromSResponse -> TestTree
responseCreateDataSourceFromS =
  res
    "CreateDataSourceFromSResponse"
    "fixture/CreateDataSourceFromSResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDataSourceFromS)

responseCreateDataSourceFromRedshift :: CreateDataSourceFromRedshiftResponse -> TestTree
responseCreateDataSourceFromRedshift =
  res
    "CreateDataSourceFromRedshiftResponse"
    "fixture/CreateDataSourceFromRedshiftResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDataSourceFromRedshift)

responseUpdateDataSource :: UpdateDataSourceResponse -> TestTree
responseUpdateDataSource =
  res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDataSource)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDataSource)

responseDescribeDataSources :: DescribeDataSourcesResponse -> TestTree
responseDescribeDataSources =
  res
    "DescribeDataSourcesResponse"
    "fixture/DescribeDataSourcesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDataSources)

responseDescribeEvaluations :: DescribeEvaluationsResponse -> TestTree
responseDescribeEvaluations =
  res
    "DescribeEvaluationsResponse"
    "fixture/DescribeEvaluationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEvaluations)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy :: Proxy AddTags)

responseGetMLModel :: GetMLModelResponse -> TestTree
responseGetMLModel =
  res
    "GetMLModelResponse"
    "fixture/GetMLModelResponse.proto"
    defaultService
    (Proxy :: Proxy GetMLModel)

responseGetEvaluation :: GetEvaluationResponse -> TestTree
responseGetEvaluation =
  res
    "GetEvaluationResponse"
    "fixture/GetEvaluationResponse.proto"
    defaultService
    (Proxy :: Proxy GetEvaluation)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTags)

responseDeleteRealtimeEndpoint :: DeleteRealtimeEndpointResponse -> TestTree
responseDeleteRealtimeEndpoint =
  res
    "DeleteRealtimeEndpointResponse"
    "fixture/DeleteRealtimeEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRealtimeEndpoint)

responseCreateDataSourceFromRDS :: CreateDataSourceFromRDSResponse -> TestTree
responseCreateDataSourceFromRDS =
  res
    "CreateDataSourceFromRDSResponse"
    "fixture/CreateDataSourceFromRDSResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDataSourceFromRDS)

responseGetBatchPrediction :: GetBatchPredictionResponse -> TestTree
responseGetBatchPrediction =
  res
    "GetBatchPredictionResponse"
    "fixture/GetBatchPredictionResponse.proto"
    defaultService
    (Proxy :: Proxy GetBatchPrediction)

responseDescribeBatchPredictions :: DescribeBatchPredictionsResponse -> TestTree
responseDescribeBatchPredictions =
  res
    "DescribeBatchPredictionsResponse"
    "fixture/DescribeBatchPredictionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBatchPredictions)

responseDeleteEvaluation :: DeleteEvaluationResponse -> TestTree
responseDeleteEvaluation =
  res
    "DeleteEvaluationResponse"
    "fixture/DeleteEvaluationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEvaluation)

responseUpdateEvaluation :: UpdateEvaluationResponse -> TestTree
responseUpdateEvaluation =
  res
    "UpdateEvaluationResponse"
    "fixture/UpdateEvaluationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEvaluation)

responseGetDataSource :: GetDataSourceResponse -> TestTree
responseGetDataSource =
  res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse.proto"
    defaultService
    (Proxy :: Proxy GetDataSource)

responseCreateRealtimeEndpoint :: CreateRealtimeEndpointResponse -> TestTree
responseCreateRealtimeEndpoint =
  res
    "CreateRealtimeEndpointResponse"
    "fixture/CreateRealtimeEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRealtimeEndpoint)

responseUpdateBatchPrediction :: UpdateBatchPredictionResponse -> TestTree
responseUpdateBatchPrediction =
  res
    "UpdateBatchPredictionResponse"
    "fixture/UpdateBatchPredictionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBatchPrediction)

responseDeleteBatchPrediction :: DeleteBatchPredictionResponse -> TestTree
responseDeleteBatchPrediction =
  res
    "DeleteBatchPredictionResponse"
    "fixture/DeleteBatchPredictionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBatchPrediction)

responseDescribeMLModels :: DescribeMLModelsResponse -> TestTree
responseDescribeMLModels =
  res
    "DescribeMLModelsResponse"
    "fixture/DescribeMLModelsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMLModels)

responseCreateBatchPrediction :: CreateBatchPredictionResponse -> TestTree
responseCreateBatchPrediction =
  res
    "CreateBatchPredictionResponse"
    "fixture/CreateBatchPredictionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBatchPrediction)

responsePredict :: PredictResponse -> TestTree
responsePredict =
  res
    "PredictResponse"
    "fixture/PredictResponse.proto"
    defaultService
    (Proxy :: Proxy Predict)

responseCreateMLModel :: CreateMLModelResponse -> TestTree
responseCreateMLModel =
  res
    "CreateMLModelResponse"
    "fixture/CreateMLModelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMLModel)

responseCreateEvaluation :: CreateEvaluationResponse -> TestTree
responseCreateEvaluation =
  res
    "CreateEvaluationResponse"
    "fixture/CreateEvaluationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEvaluation)
