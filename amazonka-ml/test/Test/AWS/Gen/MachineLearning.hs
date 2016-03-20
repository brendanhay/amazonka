{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MachineLearning
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.MachineLearning where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.MachineLearning
import Test.AWS.MachineLearning.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testUpdateDataSource $
--             updateDataSource
--
--         , testDeleteDataSource $
--             deleteDataSource
--
--         , testCreateDataSourceFromRedshift $
--             createDataSourceFromRedshift
--
--         , testCreateDataSourceFromS3 $
--             createDataSourceFromS3
--
--         , testCreateMLModel $
--             createMLModel
--
--         , testDeleteBatchPrediction $
--             deleteBatchPrediction
--
--         , testUpdateBatchPrediction $
--             updateBatchPrediction
--
--         , testGetMLModel $
--             getMLModel
--
--         , testGetDataSource $
--             getDataSource
--
--         , testUpdateEvaluation $
--             updateEvaluation
--
--         , testDeleteEvaluation $
--             deleteEvaluation
--
--         , testDeleteMLModel $
--             deleteMLModel
--
--         , testUpdateMLModel $
--             updateMLModel
--
--         , testGetBatchPrediction $
--             getBatchPrediction
--
--         , testDescribeBatchPredictions $
--             describeBatchPredictions
--
--         , testCreateDataSourceFromRDS $
--             createDataSourceFromRDS
--
--         , testCreateEvaluation $
--             createEvaluation
--
--         , testPredict $
--             predict
--
--         , testDeleteRealtimeEndpoint $
--             deleteRealtimeEndpoint
--
--         , testCreateBatchPrediction $
--             createBatchPrediction
--
--         , testGetEvaluation $
--             getEvaluation
--
--         , testDescribeEvaluations $
--             describeEvaluations
--
--         , testCreateRealtimeEndpoint $
--             createRealtimeEndpoint
--
--         , testDescribeMLModels $
--             describeMLModels
--
--         , testDescribeDataSources $
--             describeDataSources
--
--           ]

--     , testGroup "response"
--         [ testUpdateDataSourceResponse $
--             updateDataSourceResponse
--
--         , testDeleteDataSourceResponse $
--             deleteDataSourceResponse
--
--         , testCreateDataSourceFromRedshiftResponse $
--             createDataSourceFromRedshiftResponse
--
--         , testCreateDataSourceFromS3Response $
--             createDataSourceFromS3Response
--
--         , testCreateMLModelResponse $
--             createMLModelResponse
--
--         , testDeleteBatchPredictionResponse $
--             deleteBatchPredictionResponse
--
--         , testUpdateBatchPredictionResponse $
--             updateBatchPredictionResponse
--
--         , testGetMLModelResponse $
--             getMLModelResponse
--
--         , testGetDataSourceResponse $
--             getDataSourceResponse
--
--         , testUpdateEvaluationResponse $
--             updateEvaluationResponse
--
--         , testDeleteEvaluationResponse $
--             deleteEvaluationResponse
--
--         , testDeleteMLModelResponse $
--             deleteMLModelResponse
--
--         , testUpdateMLModelResponse $
--             updateMLModelResponse
--
--         , testGetBatchPredictionResponse $
--             getBatchPredictionResponse
--
--         , testDescribeBatchPredictionsResponse $
--             describeBatchPredictionsResponse
--
--         , testCreateDataSourceFromRDSResponse $
--             createDataSourceFromRDSResponse
--
--         , testCreateEvaluationResponse $
--             createEvaluationResponse
--
--         , testPredictResponse $
--             predictResponse
--
--         , testDeleteRealtimeEndpointResponse $
--             deleteRealtimeEndpointResponse
--
--         , testCreateBatchPredictionResponse $
--             createBatchPredictionResponse
--
--         , testGetEvaluationResponse $
--             getEvaluationResponse
--
--         , testDescribeEvaluationsResponse $
--             describeEvaluationsResponse
--
--         , testCreateRealtimeEndpointResponse $
--             createRealtimeEndpointResponse
--
--         , testDescribeMLModelsResponse $
--             describeMLModelsResponse
--
--         , testDescribeDataSourcesResponse $
--             describeDataSourcesResponse
--
--           ]
--     ]

-- Requests

testUpdateDataSource :: UpdateDataSource -> TestTree
testUpdateDataSource = req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

testDeleteDataSource :: DeleteDataSource -> TestTree
testDeleteDataSource = req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

testCreateDataSourceFromRedshift :: CreateDataSourceFromRedshift -> TestTree
testCreateDataSourceFromRedshift = req
    "CreateDataSourceFromRedshift"
    "fixture/CreateDataSourceFromRedshift.yaml"

testCreateDataSourceFromS3 :: CreateDataSourceFromS3 -> TestTree
testCreateDataSourceFromS3 = req
    "CreateDataSourceFromS3"
    "fixture/CreateDataSourceFromS3.yaml"

testCreateMLModel :: CreateMLModel -> TestTree
testCreateMLModel = req
    "CreateMLModel"
    "fixture/CreateMLModel.yaml"

testDeleteBatchPrediction :: DeleteBatchPrediction -> TestTree
testDeleteBatchPrediction = req
    "DeleteBatchPrediction"
    "fixture/DeleteBatchPrediction.yaml"

testUpdateBatchPrediction :: UpdateBatchPrediction -> TestTree
testUpdateBatchPrediction = req
    "UpdateBatchPrediction"
    "fixture/UpdateBatchPrediction.yaml"

testGetMLModel :: GetMLModel -> TestTree
testGetMLModel = req
    "GetMLModel"
    "fixture/GetMLModel.yaml"

testGetDataSource :: GetDataSource -> TestTree
testGetDataSource = req
    "GetDataSource"
    "fixture/GetDataSource.yaml"

testUpdateEvaluation :: UpdateEvaluation -> TestTree
testUpdateEvaluation = req
    "UpdateEvaluation"
    "fixture/UpdateEvaluation.yaml"

testDeleteEvaluation :: DeleteEvaluation -> TestTree
testDeleteEvaluation = req
    "DeleteEvaluation"
    "fixture/DeleteEvaluation.yaml"

testDeleteMLModel :: DeleteMLModel -> TestTree
testDeleteMLModel = req
    "DeleteMLModel"
    "fixture/DeleteMLModel.yaml"

testUpdateMLModel :: UpdateMLModel -> TestTree
testUpdateMLModel = req
    "UpdateMLModel"
    "fixture/UpdateMLModel.yaml"

testGetBatchPrediction :: GetBatchPrediction -> TestTree
testGetBatchPrediction = req
    "GetBatchPrediction"
    "fixture/GetBatchPrediction.yaml"

testDescribeBatchPredictions :: DescribeBatchPredictions -> TestTree
testDescribeBatchPredictions = req
    "DescribeBatchPredictions"
    "fixture/DescribeBatchPredictions.yaml"

testCreateDataSourceFromRDS :: CreateDataSourceFromRDS -> TestTree
testCreateDataSourceFromRDS = req
    "CreateDataSourceFromRDS"
    "fixture/CreateDataSourceFromRDS.yaml"

testCreateEvaluation :: CreateEvaluation -> TestTree
testCreateEvaluation = req
    "CreateEvaluation"
    "fixture/CreateEvaluation.yaml"

testPredict :: Predict -> TestTree
testPredict = req
    "Predict"
    "fixture/Predict.yaml"

testDeleteRealtimeEndpoint :: DeleteRealtimeEndpoint -> TestTree
testDeleteRealtimeEndpoint = req
    "DeleteRealtimeEndpoint"
    "fixture/DeleteRealtimeEndpoint.yaml"

testCreateBatchPrediction :: CreateBatchPrediction -> TestTree
testCreateBatchPrediction = req
    "CreateBatchPrediction"
    "fixture/CreateBatchPrediction.yaml"

testGetEvaluation :: GetEvaluation -> TestTree
testGetEvaluation = req
    "GetEvaluation"
    "fixture/GetEvaluation.yaml"

testDescribeEvaluations :: DescribeEvaluations -> TestTree
testDescribeEvaluations = req
    "DescribeEvaluations"
    "fixture/DescribeEvaluations.yaml"

testCreateRealtimeEndpoint :: CreateRealtimeEndpoint -> TestTree
testCreateRealtimeEndpoint = req
    "CreateRealtimeEndpoint"
    "fixture/CreateRealtimeEndpoint.yaml"

testDescribeMLModels :: DescribeMLModels -> TestTree
testDescribeMLModels = req
    "DescribeMLModels"
    "fixture/DescribeMLModels.yaml"

testDescribeDataSources :: DescribeDataSources -> TestTree
testDescribeDataSources = req
    "DescribeDataSources"
    "fixture/DescribeDataSources.yaml"

-- Responses

testUpdateDataSourceResponse :: UpdateDataSourceResponse -> TestTree
testUpdateDataSourceResponse = res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    machineLearning
    (Proxy :: Proxy UpdateDataSource)

testDeleteDataSourceResponse :: DeleteDataSourceResponse -> TestTree
testDeleteDataSourceResponse = res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    machineLearning
    (Proxy :: Proxy DeleteDataSource)

testCreateDataSourceFromRedshiftResponse :: CreateDataSourceFromRedshiftResponse -> TestTree
testCreateDataSourceFromRedshiftResponse = res
    "CreateDataSourceFromRedshiftResponse"
    "fixture/CreateDataSourceFromRedshiftResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateDataSourceFromRedshift)

testCreateDataSourceFromS3Response :: CreateDataSourceFromS3Response -> TestTree
testCreateDataSourceFromS3Response = res
    "CreateDataSourceFromS3Response"
    "fixture/CreateDataSourceFromS3Response.proto"
    machineLearning
    (Proxy :: Proxy CreateDataSourceFromS3)

testCreateMLModelResponse :: CreateMLModelResponse -> TestTree
testCreateMLModelResponse = res
    "CreateMLModelResponse"
    "fixture/CreateMLModelResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateMLModel)

testDeleteBatchPredictionResponse :: DeleteBatchPredictionResponse -> TestTree
testDeleteBatchPredictionResponse = res
    "DeleteBatchPredictionResponse"
    "fixture/DeleteBatchPredictionResponse.proto"
    machineLearning
    (Proxy :: Proxy DeleteBatchPrediction)

testUpdateBatchPredictionResponse :: UpdateBatchPredictionResponse -> TestTree
testUpdateBatchPredictionResponse = res
    "UpdateBatchPredictionResponse"
    "fixture/UpdateBatchPredictionResponse.proto"
    machineLearning
    (Proxy :: Proxy UpdateBatchPrediction)

testGetMLModelResponse :: GetMLModelResponse -> TestTree
testGetMLModelResponse = res
    "GetMLModelResponse"
    "fixture/GetMLModelResponse.proto"
    machineLearning
    (Proxy :: Proxy GetMLModel)

testGetDataSourceResponse :: GetDataSourceResponse -> TestTree
testGetDataSourceResponse = res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse.proto"
    machineLearning
    (Proxy :: Proxy GetDataSource)

testUpdateEvaluationResponse :: UpdateEvaluationResponse -> TestTree
testUpdateEvaluationResponse = res
    "UpdateEvaluationResponse"
    "fixture/UpdateEvaluationResponse.proto"
    machineLearning
    (Proxy :: Proxy UpdateEvaluation)

testDeleteEvaluationResponse :: DeleteEvaluationResponse -> TestTree
testDeleteEvaluationResponse = res
    "DeleteEvaluationResponse"
    "fixture/DeleteEvaluationResponse.proto"
    machineLearning
    (Proxy :: Proxy DeleteEvaluation)

testDeleteMLModelResponse :: DeleteMLModelResponse -> TestTree
testDeleteMLModelResponse = res
    "DeleteMLModelResponse"
    "fixture/DeleteMLModelResponse.proto"
    machineLearning
    (Proxy :: Proxy DeleteMLModel)

testUpdateMLModelResponse :: UpdateMLModelResponse -> TestTree
testUpdateMLModelResponse = res
    "UpdateMLModelResponse"
    "fixture/UpdateMLModelResponse.proto"
    machineLearning
    (Proxy :: Proxy UpdateMLModel)

testGetBatchPredictionResponse :: GetBatchPredictionResponse -> TestTree
testGetBatchPredictionResponse = res
    "GetBatchPredictionResponse"
    "fixture/GetBatchPredictionResponse.proto"
    machineLearning
    (Proxy :: Proxy GetBatchPrediction)

testDescribeBatchPredictionsResponse :: DescribeBatchPredictionsResponse -> TestTree
testDescribeBatchPredictionsResponse = res
    "DescribeBatchPredictionsResponse"
    "fixture/DescribeBatchPredictionsResponse.proto"
    machineLearning
    (Proxy :: Proxy DescribeBatchPredictions)

testCreateDataSourceFromRDSResponse :: CreateDataSourceFromRDSResponse -> TestTree
testCreateDataSourceFromRDSResponse = res
    "CreateDataSourceFromRDSResponse"
    "fixture/CreateDataSourceFromRDSResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateDataSourceFromRDS)

testCreateEvaluationResponse :: CreateEvaluationResponse -> TestTree
testCreateEvaluationResponse = res
    "CreateEvaluationResponse"
    "fixture/CreateEvaluationResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateEvaluation)

testPredictResponse :: PredictResponse -> TestTree
testPredictResponse = res
    "PredictResponse"
    "fixture/PredictResponse.proto"
    machineLearning
    (Proxy :: Proxy Predict)

testDeleteRealtimeEndpointResponse :: DeleteRealtimeEndpointResponse -> TestTree
testDeleteRealtimeEndpointResponse = res
    "DeleteRealtimeEndpointResponse"
    "fixture/DeleteRealtimeEndpointResponse.proto"
    machineLearning
    (Proxy :: Proxy DeleteRealtimeEndpoint)

testCreateBatchPredictionResponse :: CreateBatchPredictionResponse -> TestTree
testCreateBatchPredictionResponse = res
    "CreateBatchPredictionResponse"
    "fixture/CreateBatchPredictionResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateBatchPrediction)

testGetEvaluationResponse :: GetEvaluationResponse -> TestTree
testGetEvaluationResponse = res
    "GetEvaluationResponse"
    "fixture/GetEvaluationResponse.proto"
    machineLearning
    (Proxy :: Proxy GetEvaluation)

testDescribeEvaluationsResponse :: DescribeEvaluationsResponse -> TestTree
testDescribeEvaluationsResponse = res
    "DescribeEvaluationsResponse"
    "fixture/DescribeEvaluationsResponse.proto"
    machineLearning
    (Proxy :: Proxy DescribeEvaluations)

testCreateRealtimeEndpointResponse :: CreateRealtimeEndpointResponse -> TestTree
testCreateRealtimeEndpointResponse = res
    "CreateRealtimeEndpointResponse"
    "fixture/CreateRealtimeEndpointResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateRealtimeEndpoint)

testDescribeMLModelsResponse :: DescribeMLModelsResponse -> TestTree
testDescribeMLModelsResponse = res
    "DescribeMLModelsResponse"
    "fixture/DescribeMLModelsResponse.proto"
    machineLearning
    (Proxy :: Proxy DescribeMLModels)

testDescribeDataSourcesResponse :: DescribeDataSourcesResponse -> TestTree
testDescribeDataSourcesResponse = res
    "DescribeDataSourcesResponse"
    "fixture/DescribeDataSourcesResponse.proto"
    machineLearning
    (Proxy :: Proxy DescribeDataSources)
