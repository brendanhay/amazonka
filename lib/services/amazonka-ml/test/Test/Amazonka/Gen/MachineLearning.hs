{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MachineLearning
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MachineLearning where

import Amazonka.MachineLearning
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MachineLearning.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddTags $
--             newAddTags
--
--         , requestCreateBatchPrediction $
--             newCreateBatchPrediction
--
--         , requestCreateDataSourceFromRDS $
--             newCreateDataSourceFromRDS
--
--         , requestCreateDataSourceFromRedshift $
--             newCreateDataSourceFromRedshift
--
--         , requestCreateDataSourceFromS3 $
--             newCreateDataSourceFromS3
--
--         , requestCreateEvaluation $
--             newCreateEvaluation
--
--         , requestCreateMLModel $
--             newCreateMLModel
--
--         , requestCreateRealtimeEndpoint $
--             newCreateRealtimeEndpoint
--
--         , requestDeleteBatchPrediction $
--             newDeleteBatchPrediction
--
--         , requestDeleteDataSource $
--             newDeleteDataSource
--
--         , requestDeleteEvaluation $
--             newDeleteEvaluation
--
--         , requestDeleteMLModel $
--             newDeleteMLModel
--
--         , requestDeleteRealtimeEndpoint $
--             newDeleteRealtimeEndpoint
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDescribeBatchPredictions $
--             newDescribeBatchPredictions
--
--         , requestDescribeDataSources $
--             newDescribeDataSources
--
--         , requestDescribeEvaluations $
--             newDescribeEvaluations
--
--         , requestDescribeMLModels $
--             newDescribeMLModels
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestGetBatchPrediction $
--             newGetBatchPrediction
--
--         , requestGetDataSource $
--             newGetDataSource
--
--         , requestGetEvaluation $
--             newGetEvaluation
--
--         , requestGetMLModel $
--             newGetMLModel
--
--         , requestPredict $
--             newPredict
--
--         , requestUpdateBatchPrediction $
--             newUpdateBatchPrediction
--
--         , requestUpdateDataSource $
--             newUpdateDataSource
--
--         , requestUpdateEvaluation $
--             newUpdateEvaluation
--
--         , requestUpdateMLModel $
--             newUpdateMLModel
--
--           ]

--     , testGroup "response"
--         [ responseAddTags $
--             newAddTagsResponse
--
--         , responseCreateBatchPrediction $
--             newCreateBatchPredictionResponse
--
--         , responseCreateDataSourceFromRDS $
--             newCreateDataSourceFromRDSResponse
--
--         , responseCreateDataSourceFromRedshift $
--             newCreateDataSourceFromRedshiftResponse
--
--         , responseCreateDataSourceFromS3 $
--             newCreateDataSourceFromS3Response
--
--         , responseCreateEvaluation $
--             newCreateEvaluationResponse
--
--         , responseCreateMLModel $
--             newCreateMLModelResponse
--
--         , responseCreateRealtimeEndpoint $
--             newCreateRealtimeEndpointResponse
--
--         , responseDeleteBatchPrediction $
--             newDeleteBatchPredictionResponse
--
--         , responseDeleteDataSource $
--             newDeleteDataSourceResponse
--
--         , responseDeleteEvaluation $
--             newDeleteEvaluationResponse
--
--         , responseDeleteMLModel $
--             newDeleteMLModelResponse
--
--         , responseDeleteRealtimeEndpoint $
--             newDeleteRealtimeEndpointResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDescribeBatchPredictions $
--             newDescribeBatchPredictionsResponse
--
--         , responseDescribeDataSources $
--             newDescribeDataSourcesResponse
--
--         , responseDescribeEvaluations $
--             newDescribeEvaluationsResponse
--
--         , responseDescribeMLModels $
--             newDescribeMLModelsResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseGetBatchPrediction $
--             newGetBatchPredictionResponse
--
--         , responseGetDataSource $
--             newGetDataSourceResponse
--
--         , responseGetEvaluation $
--             newGetEvaluationResponse
--
--         , responseGetMLModel $
--             newGetMLModelResponse
--
--         , responsePredict $
--             newPredictResponse
--
--         , responseUpdateBatchPrediction $
--             newUpdateBatchPredictionResponse
--
--         , responseUpdateDataSource $
--             newUpdateDataSourceResponse
--
--         , responseUpdateEvaluation $
--             newUpdateEvaluationResponse
--
--         , responseUpdateMLModel $
--             newUpdateMLModelResponse
--
--           ]
--     ]

-- Requests

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestCreateBatchPrediction :: CreateBatchPrediction -> TestTree
requestCreateBatchPrediction =
  req
    "CreateBatchPrediction"
    "fixture/CreateBatchPrediction.yaml"

requestCreateDataSourceFromRDS :: CreateDataSourceFromRDS -> TestTree
requestCreateDataSourceFromRDS =
  req
    "CreateDataSourceFromRDS"
    "fixture/CreateDataSourceFromRDS.yaml"

requestCreateDataSourceFromRedshift :: CreateDataSourceFromRedshift -> TestTree
requestCreateDataSourceFromRedshift =
  req
    "CreateDataSourceFromRedshift"
    "fixture/CreateDataSourceFromRedshift.yaml"

requestCreateDataSourceFromS3 :: CreateDataSourceFromS3 -> TestTree
requestCreateDataSourceFromS3 =
  req
    "CreateDataSourceFromS3"
    "fixture/CreateDataSourceFromS3.yaml"

requestCreateEvaluation :: CreateEvaluation -> TestTree
requestCreateEvaluation =
  req
    "CreateEvaluation"
    "fixture/CreateEvaluation.yaml"

requestCreateMLModel :: CreateMLModel -> TestTree
requestCreateMLModel =
  req
    "CreateMLModel"
    "fixture/CreateMLModel.yaml"

requestCreateRealtimeEndpoint :: CreateRealtimeEndpoint -> TestTree
requestCreateRealtimeEndpoint =
  req
    "CreateRealtimeEndpoint"
    "fixture/CreateRealtimeEndpoint.yaml"

requestDeleteBatchPrediction :: DeleteBatchPrediction -> TestTree
requestDeleteBatchPrediction =
  req
    "DeleteBatchPrediction"
    "fixture/DeleteBatchPrediction.yaml"

requestDeleteDataSource :: DeleteDataSource -> TestTree
requestDeleteDataSource =
  req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

requestDeleteEvaluation :: DeleteEvaluation -> TestTree
requestDeleteEvaluation =
  req
    "DeleteEvaluation"
    "fixture/DeleteEvaluation.yaml"

requestDeleteMLModel :: DeleteMLModel -> TestTree
requestDeleteMLModel =
  req
    "DeleteMLModel"
    "fixture/DeleteMLModel.yaml"

requestDeleteRealtimeEndpoint :: DeleteRealtimeEndpoint -> TestTree
requestDeleteRealtimeEndpoint =
  req
    "DeleteRealtimeEndpoint"
    "fixture/DeleteRealtimeEndpoint.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDescribeBatchPredictions :: DescribeBatchPredictions -> TestTree
requestDescribeBatchPredictions =
  req
    "DescribeBatchPredictions"
    "fixture/DescribeBatchPredictions.yaml"

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

requestDescribeMLModels :: DescribeMLModels -> TestTree
requestDescribeMLModels =
  req
    "DescribeMLModels"
    "fixture/DescribeMLModels.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestGetBatchPrediction :: GetBatchPrediction -> TestTree
requestGetBatchPrediction =
  req
    "GetBatchPrediction"
    "fixture/GetBatchPrediction.yaml"

requestGetDataSource :: GetDataSource -> TestTree
requestGetDataSource =
  req
    "GetDataSource"
    "fixture/GetDataSource.yaml"

requestGetEvaluation :: GetEvaluation -> TestTree
requestGetEvaluation =
  req
    "GetEvaluation"
    "fixture/GetEvaluation.yaml"

requestGetMLModel :: GetMLModel -> TestTree
requestGetMLModel =
  req
    "GetMLModel"
    "fixture/GetMLModel.yaml"

requestPredict :: Predict -> TestTree
requestPredict =
  req
    "Predict"
    "fixture/Predict.yaml"

requestUpdateBatchPrediction :: UpdateBatchPrediction -> TestTree
requestUpdateBatchPrediction =
  req
    "UpdateBatchPrediction"
    "fixture/UpdateBatchPrediction.yaml"

requestUpdateDataSource :: UpdateDataSource -> TestTree
requestUpdateDataSource =
  req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

requestUpdateEvaluation :: UpdateEvaluation -> TestTree
requestUpdateEvaluation =
  req
    "UpdateEvaluation"
    "fixture/UpdateEvaluation.yaml"

requestUpdateMLModel :: UpdateMLModel -> TestTree
requestUpdateMLModel =
  req
    "UpdateMLModel"
    "fixture/UpdateMLModel.yaml"

-- Responses

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseCreateBatchPrediction :: CreateBatchPredictionResponse -> TestTree
responseCreateBatchPrediction =
  res
    "CreateBatchPredictionResponse"
    "fixture/CreateBatchPredictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBatchPrediction)

responseCreateDataSourceFromRDS :: CreateDataSourceFromRDSResponse -> TestTree
responseCreateDataSourceFromRDS =
  res
    "CreateDataSourceFromRDSResponse"
    "fixture/CreateDataSourceFromRDSResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSourceFromRDS)

responseCreateDataSourceFromRedshift :: CreateDataSourceFromRedshiftResponse -> TestTree
responseCreateDataSourceFromRedshift =
  res
    "CreateDataSourceFromRedshiftResponse"
    "fixture/CreateDataSourceFromRedshiftResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSourceFromRedshift)

responseCreateDataSourceFromS3 :: CreateDataSourceFromS3Response -> TestTree
responseCreateDataSourceFromS3 =
  res
    "CreateDataSourceFromS3Response"
    "fixture/CreateDataSourceFromS3Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSourceFromS3)

responseCreateEvaluation :: CreateEvaluationResponse -> TestTree
responseCreateEvaluation =
  res
    "CreateEvaluationResponse"
    "fixture/CreateEvaluationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEvaluation)

responseCreateMLModel :: CreateMLModelResponse -> TestTree
responseCreateMLModel =
  res
    "CreateMLModelResponse"
    "fixture/CreateMLModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMLModel)

responseCreateRealtimeEndpoint :: CreateRealtimeEndpointResponse -> TestTree
responseCreateRealtimeEndpoint =
  res
    "CreateRealtimeEndpointResponse"
    "fixture/CreateRealtimeEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRealtimeEndpoint)

responseDeleteBatchPrediction :: DeleteBatchPredictionResponse -> TestTree
responseDeleteBatchPrediction =
  res
    "DeleteBatchPredictionResponse"
    "fixture/DeleteBatchPredictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBatchPrediction)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSource)

responseDeleteEvaluation :: DeleteEvaluationResponse -> TestTree
responseDeleteEvaluation =
  res
    "DeleteEvaluationResponse"
    "fixture/DeleteEvaluationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEvaluation)

responseDeleteMLModel :: DeleteMLModelResponse -> TestTree
responseDeleteMLModel =
  res
    "DeleteMLModelResponse"
    "fixture/DeleteMLModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMLModel)

responseDeleteRealtimeEndpoint :: DeleteRealtimeEndpointResponse -> TestTree
responseDeleteRealtimeEndpoint =
  res
    "DeleteRealtimeEndpointResponse"
    "fixture/DeleteRealtimeEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRealtimeEndpoint)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseDescribeBatchPredictions :: DescribeBatchPredictionsResponse -> TestTree
responseDescribeBatchPredictions =
  res
    "DescribeBatchPredictionsResponse"
    "fixture/DescribeBatchPredictionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBatchPredictions)

responseDescribeDataSources :: DescribeDataSourcesResponse -> TestTree
responseDescribeDataSources =
  res
    "DescribeDataSourcesResponse"
    "fixture/DescribeDataSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSources)

responseDescribeEvaluations :: DescribeEvaluationsResponse -> TestTree
responseDescribeEvaluations =
  res
    "DescribeEvaluationsResponse"
    "fixture/DescribeEvaluationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvaluations)

responseDescribeMLModels :: DescribeMLModelsResponse -> TestTree
responseDescribeMLModels =
  res
    "DescribeMLModelsResponse"
    "fixture/DescribeMLModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMLModels)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseGetBatchPrediction :: GetBatchPredictionResponse -> TestTree
responseGetBatchPrediction =
  res
    "GetBatchPredictionResponse"
    "fixture/GetBatchPredictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBatchPrediction)

responseGetDataSource :: GetDataSourceResponse -> TestTree
responseGetDataSource =
  res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataSource)

responseGetEvaluation :: GetEvaluationResponse -> TestTree
responseGetEvaluation =
  res
    "GetEvaluationResponse"
    "fixture/GetEvaluationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvaluation)

responseGetMLModel :: GetMLModelResponse -> TestTree
responseGetMLModel =
  res
    "GetMLModelResponse"
    "fixture/GetMLModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMLModel)

responsePredict :: PredictResponse -> TestTree
responsePredict =
  res
    "PredictResponse"
    "fixture/PredictResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Predict)

responseUpdateBatchPrediction :: UpdateBatchPredictionResponse -> TestTree
responseUpdateBatchPrediction =
  res
    "UpdateBatchPredictionResponse"
    "fixture/UpdateBatchPredictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBatchPrediction)

responseUpdateDataSource :: UpdateDataSourceResponse -> TestTree
responseUpdateDataSource =
  res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSource)

responseUpdateEvaluation :: UpdateEvaluationResponse -> TestTree
responseUpdateEvaluation =
  res
    "UpdateEvaluationResponse"
    "fixture/UpdateEvaluationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEvaluation)

responseUpdateMLModel :: UpdateMLModelResponse -> TestTree
responseUpdateMLModel =
  res
    "UpdateMLModelResponse"
    "fixture/UpdateMLModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMLModel)
