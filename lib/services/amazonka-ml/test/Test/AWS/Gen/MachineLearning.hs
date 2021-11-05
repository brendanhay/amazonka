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

import Amazonka.MachineLearning
import qualified Data.Proxy as Proxy
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
--         [ requestUpdateDataSource $
--             newUpdateDataSource
--
--         , requestDeleteDataSource $
--             newDeleteDataSource
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestCreateDataSourceFromRedshift $
--             newCreateDataSourceFromRedshift
--
--         , requestCreateDataSourceFromS3 $
--             newCreateDataSourceFromS3
--
--         , requestCreateMLModel $
--             newCreateMLModel
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDeleteBatchPrediction $
--             newDeleteBatchPrediction
--
--         , requestUpdateBatchPrediction $
--             newUpdateBatchPrediction
--
--         , requestGetMLModel $
--             newGetMLModel
--
--         , requestGetDataSource $
--             newGetDataSource
--
--         , requestUpdateEvaluation $
--             newUpdateEvaluation
--
--         , requestDeleteEvaluation $
--             newDeleteEvaluation
--
--         , requestDeleteMLModel $
--             newDeleteMLModel
--
--         , requestUpdateMLModel $
--             newUpdateMLModel
--
--         , requestGetBatchPrediction $
--             newGetBatchPrediction
--
--         , requestDescribeBatchPredictions $
--             newDescribeBatchPredictions
--
--         , requestCreateDataSourceFromRDS $
--             newCreateDataSourceFromRDS
--
--         , requestCreateEvaluation $
--             newCreateEvaluation
--
--         , requestPredict $
--             newPredict
--
--         , requestDeleteRealtimeEndpoint $
--             newDeleteRealtimeEndpoint
--
--         , requestCreateBatchPrediction $
--             newCreateBatchPrediction
--
--         , requestGetEvaluation $
--             newGetEvaluation
--
--         , requestDescribeEvaluations $
--             newDescribeEvaluations
--
--         , requestCreateRealtimeEndpoint $
--             newCreateRealtimeEndpoint
--
--         , requestAddTags $
--             newAddTags
--
--         , requestDescribeMLModels $
--             newDescribeMLModels
--
--         , requestDescribeDataSources $
--             newDescribeDataSources
--
--           ]

--     , testGroup "response"
--         [ responseUpdateDataSource $
--             newUpdateDataSourceResponse
--
--         , responseDeleteDataSource $
--             newDeleteDataSourceResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseCreateDataSourceFromRedshift $
--             newCreateDataSourceFromRedshiftResponse
--
--         , responseCreateDataSourceFromS3 $
--             newCreateDataSourceFromS3Response
--
--         , responseCreateMLModel $
--             newCreateMLModelResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDeleteBatchPrediction $
--             newDeleteBatchPredictionResponse
--
--         , responseUpdateBatchPrediction $
--             newUpdateBatchPredictionResponse
--
--         , responseGetMLModel $
--             newGetMLModelResponse
--
--         , responseGetDataSource $
--             newGetDataSourceResponse
--
--         , responseUpdateEvaluation $
--             newUpdateEvaluationResponse
--
--         , responseDeleteEvaluation $
--             newDeleteEvaluationResponse
--
--         , responseDeleteMLModel $
--             newDeleteMLModelResponse
--
--         , responseUpdateMLModel $
--             newUpdateMLModelResponse
--
--         , responseGetBatchPrediction $
--             newGetBatchPredictionResponse
--
--         , responseDescribeBatchPredictions $
--             newDescribeBatchPredictionsResponse
--
--         , responseCreateDataSourceFromRDS $
--             newCreateDataSourceFromRDSResponse
--
--         , responseCreateEvaluation $
--             newCreateEvaluationResponse
--
--         , responsePredict $
--             newPredictResponse
--
--         , responseDeleteRealtimeEndpoint $
--             newDeleteRealtimeEndpointResponse
--
--         , responseCreateBatchPrediction $
--             newCreateBatchPredictionResponse
--
--         , responseGetEvaluation $
--             newGetEvaluationResponse
--
--         , responseDescribeEvaluations $
--             newDescribeEvaluationsResponse
--
--         , responseCreateRealtimeEndpoint $
--             newCreateRealtimeEndpointResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseDescribeMLModels $
--             newDescribeMLModelsResponse
--
--         , responseDescribeDataSources $
--             newDescribeDataSourcesResponse
--
--           ]
--     ]

-- Requests

requestUpdateDataSource :: UpdateDataSource -> TestTree
requestUpdateDataSource =
  req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

requestDeleteDataSource :: DeleteDataSource -> TestTree
requestDeleteDataSource =
  req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

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

requestCreateMLModel :: CreateMLModel -> TestTree
requestCreateMLModel =
  req
    "CreateMLModel"
    "fixture/CreateMLModel.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDeleteBatchPrediction :: DeleteBatchPrediction -> TestTree
requestDeleteBatchPrediction =
  req
    "DeleteBatchPrediction"
    "fixture/DeleteBatchPrediction.yaml"

requestUpdateBatchPrediction :: UpdateBatchPrediction -> TestTree
requestUpdateBatchPrediction =
  req
    "UpdateBatchPrediction"
    "fixture/UpdateBatchPrediction.yaml"

requestGetMLModel :: GetMLModel -> TestTree
requestGetMLModel =
  req
    "GetMLModel"
    "fixture/GetMLModel.yaml"

requestGetDataSource :: GetDataSource -> TestTree
requestGetDataSource =
  req
    "GetDataSource"
    "fixture/GetDataSource.yaml"

requestUpdateEvaluation :: UpdateEvaluation -> TestTree
requestUpdateEvaluation =
  req
    "UpdateEvaluation"
    "fixture/UpdateEvaluation.yaml"

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

requestUpdateMLModel :: UpdateMLModel -> TestTree
requestUpdateMLModel =
  req
    "UpdateMLModel"
    "fixture/UpdateMLModel.yaml"

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

requestCreateDataSourceFromRDS :: CreateDataSourceFromRDS -> TestTree
requestCreateDataSourceFromRDS =
  req
    "CreateDataSourceFromRDS"
    "fixture/CreateDataSourceFromRDS.yaml"

requestCreateEvaluation :: CreateEvaluation -> TestTree
requestCreateEvaluation =
  req
    "CreateEvaluation"
    "fixture/CreateEvaluation.yaml"

requestPredict :: Predict -> TestTree
requestPredict =
  req
    "Predict"
    "fixture/Predict.yaml"

requestDeleteRealtimeEndpoint :: DeleteRealtimeEndpoint -> TestTree
requestDeleteRealtimeEndpoint =
  req
    "DeleteRealtimeEndpoint"
    "fixture/DeleteRealtimeEndpoint.yaml"

requestCreateBatchPrediction :: CreateBatchPrediction -> TestTree
requestCreateBatchPrediction =
  req
    "CreateBatchPrediction"
    "fixture/CreateBatchPrediction.yaml"

requestGetEvaluation :: GetEvaluation -> TestTree
requestGetEvaluation =
  req
    "GetEvaluation"
    "fixture/GetEvaluation.yaml"

requestDescribeEvaluations :: DescribeEvaluations -> TestTree
requestDescribeEvaluations =
  req
    "DescribeEvaluations"
    "fixture/DescribeEvaluations.yaml"

requestCreateRealtimeEndpoint :: CreateRealtimeEndpoint -> TestTree
requestCreateRealtimeEndpoint =
  req
    "CreateRealtimeEndpoint"
    "fixture/CreateRealtimeEndpoint.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestDescribeMLModels :: DescribeMLModels -> TestTree
requestDescribeMLModels =
  req
    "DescribeMLModels"
    "fixture/DescribeMLModels.yaml"

requestDescribeDataSources :: DescribeDataSources -> TestTree
requestDescribeDataSources =
  req
    "DescribeDataSources"
    "fixture/DescribeDataSources.yaml"

-- Responses

responseUpdateDataSource :: UpdateDataSourceResponse -> TestTree
responseUpdateDataSource =
  res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSource)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSource)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

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

responseCreateMLModel :: CreateMLModelResponse -> TestTree
responseCreateMLModel =
  res
    "CreateMLModelResponse"
    "fixture/CreateMLModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMLModel)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseDeleteBatchPrediction :: DeleteBatchPredictionResponse -> TestTree
responseDeleteBatchPrediction =
  res
    "DeleteBatchPredictionResponse"
    "fixture/DeleteBatchPredictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBatchPrediction)

responseUpdateBatchPrediction :: UpdateBatchPredictionResponse -> TestTree
responseUpdateBatchPrediction =
  res
    "UpdateBatchPredictionResponse"
    "fixture/UpdateBatchPredictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBatchPrediction)

responseGetMLModel :: GetMLModelResponse -> TestTree
responseGetMLModel =
  res
    "GetMLModelResponse"
    "fixture/GetMLModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMLModel)

responseGetDataSource :: GetDataSourceResponse -> TestTree
responseGetDataSource =
  res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataSource)

responseUpdateEvaluation :: UpdateEvaluationResponse -> TestTree
responseUpdateEvaluation =
  res
    "UpdateEvaluationResponse"
    "fixture/UpdateEvaluationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEvaluation)

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

responseUpdateMLModel :: UpdateMLModelResponse -> TestTree
responseUpdateMLModel =
  res
    "UpdateMLModelResponse"
    "fixture/UpdateMLModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMLModel)

responseGetBatchPrediction :: GetBatchPredictionResponse -> TestTree
responseGetBatchPrediction =
  res
    "GetBatchPredictionResponse"
    "fixture/GetBatchPredictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBatchPrediction)

responseDescribeBatchPredictions :: DescribeBatchPredictionsResponse -> TestTree
responseDescribeBatchPredictions =
  res
    "DescribeBatchPredictionsResponse"
    "fixture/DescribeBatchPredictionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBatchPredictions)

responseCreateDataSourceFromRDS :: CreateDataSourceFromRDSResponse -> TestTree
responseCreateDataSourceFromRDS =
  res
    "CreateDataSourceFromRDSResponse"
    "fixture/CreateDataSourceFromRDSResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSourceFromRDS)

responseCreateEvaluation :: CreateEvaluationResponse -> TestTree
responseCreateEvaluation =
  res
    "CreateEvaluationResponse"
    "fixture/CreateEvaluationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEvaluation)

responsePredict :: PredictResponse -> TestTree
responsePredict =
  res
    "PredictResponse"
    "fixture/PredictResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Predict)

responseDeleteRealtimeEndpoint :: DeleteRealtimeEndpointResponse -> TestTree
responseDeleteRealtimeEndpoint =
  res
    "DeleteRealtimeEndpointResponse"
    "fixture/DeleteRealtimeEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRealtimeEndpoint)

responseCreateBatchPrediction :: CreateBatchPredictionResponse -> TestTree
responseCreateBatchPrediction =
  res
    "CreateBatchPredictionResponse"
    "fixture/CreateBatchPredictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBatchPrediction)

responseGetEvaluation :: GetEvaluationResponse -> TestTree
responseGetEvaluation =
  res
    "GetEvaluationResponse"
    "fixture/GetEvaluationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEvaluation)

responseDescribeEvaluations :: DescribeEvaluationsResponse -> TestTree
responseDescribeEvaluations =
  res
    "DescribeEvaluationsResponse"
    "fixture/DescribeEvaluationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvaluations)

responseCreateRealtimeEndpoint :: CreateRealtimeEndpointResponse -> TestTree
responseCreateRealtimeEndpoint =
  res
    "CreateRealtimeEndpointResponse"
    "fixture/CreateRealtimeEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRealtimeEndpoint)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseDescribeMLModels :: DescribeMLModelsResponse -> TestTree
responseDescribeMLModels =
  res
    "DescribeMLModelsResponse"
    "fixture/DescribeMLModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMLModels)

responseDescribeDataSources :: DescribeDataSourcesResponse -> TestTree
responseDescribeDataSources =
  res
    "DescribeDataSourcesResponse"
    "fixture/DescribeDataSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSources)
