{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MachineLearning
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestUpdateDataSource $
--             updateDataSource
--
--         , requestDeleteDataSource $
--             deleteDataSource
--
--         , requestDescribeTags $
--             describeTags
--
--         , requestCreateDataSourceFromRedshift $
--             createDataSourceFromRedshift
--
--         , requestCreateDataSourceFromS3 $
--             createDataSourceFromS3
--
--         , requestCreateMLModel $
--             createMLModel
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestDeleteBatchPrediction $
--             deleteBatchPrediction
--
--         , requestUpdateBatchPrediction $
--             updateBatchPrediction
--
--         , requestGetMLModel $
--             getMLModel
--
--         , requestGetDataSource $
--             getDataSource
--
--         , requestUpdateEvaluation $
--             updateEvaluation
--
--         , requestDeleteEvaluation $
--             deleteEvaluation
--
--         , requestDeleteMLModel $
--             deleteMLModel
--
--         , requestUpdateMLModel $
--             updateMLModel
--
--         , requestGetBatchPrediction $
--             getBatchPrediction
--
--         , requestDescribeBatchPredictions $
--             describeBatchPredictions
--
--         , requestCreateDataSourceFromRDS $
--             createDataSourceFromRDS
--
--         , requestCreateEvaluation $
--             createEvaluation
--
--         , requestPredict $
--             predict
--
--         , requestDeleteRealtimeEndpoint $
--             deleteRealtimeEndpoint
--
--         , requestCreateBatchPrediction $
--             createBatchPrediction
--
--         , requestGetEvaluation $
--             getEvaluation
--
--         , requestDescribeEvaluations $
--             describeEvaluations
--
--         , requestCreateRealtimeEndpoint $
--             createRealtimeEndpoint
--
--         , requestAddTags $
--             addTags
--
--         , requestDescribeMLModels $
--             describeMLModels
--
--         , requestDescribeDataSources $
--             describeDataSources
--
--           ]

--     , testGroup "response"
--         [ responseUpdateDataSource $
--             updateDataSourceResponse
--
--         , responseDeleteDataSource $
--             deleteDataSourceResponse
--
--         , responseDescribeTags $
--             describeTagsResponse
--
--         , responseCreateDataSourceFromRedshift $
--             createDataSourceFromRedshiftResponse
--
--         , responseCreateDataSourceFromS3 $
--             createDataSourceFromS3Response
--
--         , responseCreateMLModel $
--             createMLModelResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseDeleteBatchPrediction $
--             deleteBatchPredictionResponse
--
--         , responseUpdateBatchPrediction $
--             updateBatchPredictionResponse
--
--         , responseGetMLModel $
--             getMLModelResponse
--
--         , responseGetDataSource $
--             getDataSourceResponse
--
--         , responseUpdateEvaluation $
--             updateEvaluationResponse
--
--         , responseDeleteEvaluation $
--             deleteEvaluationResponse
--
--         , responseDeleteMLModel $
--             deleteMLModelResponse
--
--         , responseUpdateMLModel $
--             updateMLModelResponse
--
--         , responseGetBatchPrediction $
--             getBatchPredictionResponse
--
--         , responseDescribeBatchPredictions $
--             describeBatchPredictionsResponse
--
--         , responseCreateDataSourceFromRDS $
--             createDataSourceFromRDSResponse
--
--         , responseCreateEvaluation $
--             createEvaluationResponse
--
--         , responsePredict $
--             predictResponse
--
--         , responseDeleteRealtimeEndpoint $
--             deleteRealtimeEndpointResponse
--
--         , responseCreateBatchPrediction $
--             createBatchPredictionResponse
--
--         , responseGetEvaluation $
--             getEvaluationResponse
--
--         , responseDescribeEvaluations $
--             describeEvaluationsResponse
--
--         , responseCreateRealtimeEndpoint $
--             createRealtimeEndpointResponse
--
--         , responseAddTags $
--             addTagsResponse
--
--         , responseDescribeMLModels $
--             describeMLModelsResponse
--
--         , responseDescribeDataSources $
--             describeDataSourcesResponse
--
--           ]
--     ]

-- Requests

requestUpdateDataSource :: UpdateDataSource -> TestTree
requestUpdateDataSource = req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

requestDeleteDataSource :: DeleteDataSource -> TestTree
requestDeleteDataSource = req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestCreateDataSourceFromRedshift :: CreateDataSourceFromRedshift -> TestTree
requestCreateDataSourceFromRedshift = req
    "CreateDataSourceFromRedshift"
    "fixture/CreateDataSourceFromRedshift.yaml"

requestCreateDataSourceFromS3 :: CreateDataSourceFromS3 -> TestTree
requestCreateDataSourceFromS3 = req
    "CreateDataSourceFromS3"
    "fixture/CreateDataSourceFromS3.yaml"

requestCreateMLModel :: CreateMLModel -> TestTree
requestCreateMLModel = req
    "CreateMLModel"
    "fixture/CreateMLModel.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDeleteBatchPrediction :: DeleteBatchPrediction -> TestTree
requestDeleteBatchPrediction = req
    "DeleteBatchPrediction"
    "fixture/DeleteBatchPrediction.yaml"

requestUpdateBatchPrediction :: UpdateBatchPrediction -> TestTree
requestUpdateBatchPrediction = req
    "UpdateBatchPrediction"
    "fixture/UpdateBatchPrediction.yaml"

requestGetMLModel :: GetMLModel -> TestTree
requestGetMLModel = req
    "GetMLModel"
    "fixture/GetMLModel.yaml"

requestGetDataSource :: GetDataSource -> TestTree
requestGetDataSource = req
    "GetDataSource"
    "fixture/GetDataSource.yaml"

requestUpdateEvaluation :: UpdateEvaluation -> TestTree
requestUpdateEvaluation = req
    "UpdateEvaluation"
    "fixture/UpdateEvaluation.yaml"

requestDeleteEvaluation :: DeleteEvaluation -> TestTree
requestDeleteEvaluation = req
    "DeleteEvaluation"
    "fixture/DeleteEvaluation.yaml"

requestDeleteMLModel :: DeleteMLModel -> TestTree
requestDeleteMLModel = req
    "DeleteMLModel"
    "fixture/DeleteMLModel.yaml"

requestUpdateMLModel :: UpdateMLModel -> TestTree
requestUpdateMLModel = req
    "UpdateMLModel"
    "fixture/UpdateMLModel.yaml"

requestGetBatchPrediction :: GetBatchPrediction -> TestTree
requestGetBatchPrediction = req
    "GetBatchPrediction"
    "fixture/GetBatchPrediction.yaml"

requestDescribeBatchPredictions :: DescribeBatchPredictions -> TestTree
requestDescribeBatchPredictions = req
    "DescribeBatchPredictions"
    "fixture/DescribeBatchPredictions.yaml"

requestCreateDataSourceFromRDS :: CreateDataSourceFromRDS -> TestTree
requestCreateDataSourceFromRDS = req
    "CreateDataSourceFromRDS"
    "fixture/CreateDataSourceFromRDS.yaml"

requestCreateEvaluation :: CreateEvaluation -> TestTree
requestCreateEvaluation = req
    "CreateEvaluation"
    "fixture/CreateEvaluation.yaml"

requestPredict :: Predict -> TestTree
requestPredict = req
    "Predict"
    "fixture/Predict.yaml"

requestDeleteRealtimeEndpoint :: DeleteRealtimeEndpoint -> TestTree
requestDeleteRealtimeEndpoint = req
    "DeleteRealtimeEndpoint"
    "fixture/DeleteRealtimeEndpoint.yaml"

requestCreateBatchPrediction :: CreateBatchPrediction -> TestTree
requestCreateBatchPrediction = req
    "CreateBatchPrediction"
    "fixture/CreateBatchPrediction.yaml"

requestGetEvaluation :: GetEvaluation -> TestTree
requestGetEvaluation = req
    "GetEvaluation"
    "fixture/GetEvaluation.yaml"

requestDescribeEvaluations :: DescribeEvaluations -> TestTree
requestDescribeEvaluations = req
    "DescribeEvaluations"
    "fixture/DescribeEvaluations.yaml"

requestCreateRealtimeEndpoint :: CreateRealtimeEndpoint -> TestTree
requestCreateRealtimeEndpoint = req
    "CreateRealtimeEndpoint"
    "fixture/CreateRealtimeEndpoint.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

requestDescribeMLModels :: DescribeMLModels -> TestTree
requestDescribeMLModels = req
    "DescribeMLModels"
    "fixture/DescribeMLModels.yaml"

requestDescribeDataSources :: DescribeDataSources -> TestTree
requestDescribeDataSources = req
    "DescribeDataSources"
    "fixture/DescribeDataSources.yaml"

-- Responses

responseUpdateDataSource :: UpdateDataSourceResponse -> TestTree
responseUpdateDataSource = res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    machineLearning
    (Proxy :: Proxy UpdateDataSource)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource = res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    machineLearning
    (Proxy :: Proxy DeleteDataSource)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    machineLearning
    (Proxy :: Proxy DescribeTags)

responseCreateDataSourceFromRedshift :: CreateDataSourceFromRedshiftResponse -> TestTree
responseCreateDataSourceFromRedshift = res
    "CreateDataSourceFromRedshiftResponse"
    "fixture/CreateDataSourceFromRedshiftResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateDataSourceFromRedshift)

responseCreateDataSourceFromS3 :: CreateDataSourceFromS3Response -> TestTree
responseCreateDataSourceFromS3 = res
    "CreateDataSourceFromS3Response"
    "fixture/CreateDataSourceFromS3Response.proto"
    machineLearning
    (Proxy :: Proxy CreateDataSourceFromS3)

responseCreateMLModel :: CreateMLModelResponse -> TestTree
responseCreateMLModel = res
    "CreateMLModelResponse"
    "fixture/CreateMLModelResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateMLModel)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    machineLearning
    (Proxy :: Proxy DeleteTags)

responseDeleteBatchPrediction :: DeleteBatchPredictionResponse -> TestTree
responseDeleteBatchPrediction = res
    "DeleteBatchPredictionResponse"
    "fixture/DeleteBatchPredictionResponse.proto"
    machineLearning
    (Proxy :: Proxy DeleteBatchPrediction)

responseUpdateBatchPrediction :: UpdateBatchPredictionResponse -> TestTree
responseUpdateBatchPrediction = res
    "UpdateBatchPredictionResponse"
    "fixture/UpdateBatchPredictionResponse.proto"
    machineLearning
    (Proxy :: Proxy UpdateBatchPrediction)

responseGetMLModel :: GetMLModelResponse -> TestTree
responseGetMLModel = res
    "GetMLModelResponse"
    "fixture/GetMLModelResponse.proto"
    machineLearning
    (Proxy :: Proxy GetMLModel)

responseGetDataSource :: GetDataSourceResponse -> TestTree
responseGetDataSource = res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse.proto"
    machineLearning
    (Proxy :: Proxy GetDataSource)

responseUpdateEvaluation :: UpdateEvaluationResponse -> TestTree
responseUpdateEvaluation = res
    "UpdateEvaluationResponse"
    "fixture/UpdateEvaluationResponse.proto"
    machineLearning
    (Proxy :: Proxy UpdateEvaluation)

responseDeleteEvaluation :: DeleteEvaluationResponse -> TestTree
responseDeleteEvaluation = res
    "DeleteEvaluationResponse"
    "fixture/DeleteEvaluationResponse.proto"
    machineLearning
    (Proxy :: Proxy DeleteEvaluation)

responseDeleteMLModel :: DeleteMLModelResponse -> TestTree
responseDeleteMLModel = res
    "DeleteMLModelResponse"
    "fixture/DeleteMLModelResponse.proto"
    machineLearning
    (Proxy :: Proxy DeleteMLModel)

responseUpdateMLModel :: UpdateMLModelResponse -> TestTree
responseUpdateMLModel = res
    "UpdateMLModelResponse"
    "fixture/UpdateMLModelResponse.proto"
    machineLearning
    (Proxy :: Proxy UpdateMLModel)

responseGetBatchPrediction :: GetBatchPredictionResponse -> TestTree
responseGetBatchPrediction = res
    "GetBatchPredictionResponse"
    "fixture/GetBatchPredictionResponse.proto"
    machineLearning
    (Proxy :: Proxy GetBatchPrediction)

responseDescribeBatchPredictions :: DescribeBatchPredictionsResponse -> TestTree
responseDescribeBatchPredictions = res
    "DescribeBatchPredictionsResponse"
    "fixture/DescribeBatchPredictionsResponse.proto"
    machineLearning
    (Proxy :: Proxy DescribeBatchPredictions)

responseCreateDataSourceFromRDS :: CreateDataSourceFromRDSResponse -> TestTree
responseCreateDataSourceFromRDS = res
    "CreateDataSourceFromRDSResponse"
    "fixture/CreateDataSourceFromRDSResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateDataSourceFromRDS)

responseCreateEvaluation :: CreateEvaluationResponse -> TestTree
responseCreateEvaluation = res
    "CreateEvaluationResponse"
    "fixture/CreateEvaluationResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateEvaluation)

responsePredict :: PredictResponse -> TestTree
responsePredict = res
    "PredictResponse"
    "fixture/PredictResponse.proto"
    machineLearning
    (Proxy :: Proxy Predict)

responseDeleteRealtimeEndpoint :: DeleteRealtimeEndpointResponse -> TestTree
responseDeleteRealtimeEndpoint = res
    "DeleteRealtimeEndpointResponse"
    "fixture/DeleteRealtimeEndpointResponse.proto"
    machineLearning
    (Proxy :: Proxy DeleteRealtimeEndpoint)

responseCreateBatchPrediction :: CreateBatchPredictionResponse -> TestTree
responseCreateBatchPrediction = res
    "CreateBatchPredictionResponse"
    "fixture/CreateBatchPredictionResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateBatchPrediction)

responseGetEvaluation :: GetEvaluationResponse -> TestTree
responseGetEvaluation = res
    "GetEvaluationResponse"
    "fixture/GetEvaluationResponse.proto"
    machineLearning
    (Proxy :: Proxy GetEvaluation)

responseDescribeEvaluations :: DescribeEvaluationsResponse -> TestTree
responseDescribeEvaluations = res
    "DescribeEvaluationsResponse"
    "fixture/DescribeEvaluationsResponse.proto"
    machineLearning
    (Proxy :: Proxy DescribeEvaluations)

responseCreateRealtimeEndpoint :: CreateRealtimeEndpointResponse -> TestTree
responseCreateRealtimeEndpoint = res
    "CreateRealtimeEndpointResponse"
    "fixture/CreateRealtimeEndpointResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateRealtimeEndpoint)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    machineLearning
    (Proxy :: Proxy AddTags)

responseDescribeMLModels :: DescribeMLModelsResponse -> TestTree
responseDescribeMLModels = res
    "DescribeMLModelsResponse"
    "fixture/DescribeMLModelsResponse.proto"
    machineLearning
    (Proxy :: Proxy DescribeMLModels)

responseDescribeDataSources :: DescribeDataSourcesResponse -> TestTree
responseDescribeDataSources = res
    "DescribeDataSourcesResponse"
    "fixture/DescribeDataSourcesResponse.proto"
    machineLearning
    (Proxy :: Proxy DescribeDataSources)
