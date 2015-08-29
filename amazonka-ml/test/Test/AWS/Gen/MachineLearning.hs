{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MachineLearning
-- Copyright   : (c) 2013-2015 Brendan Hay
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
--         [ testDeleteDataSource $
--             deleteDataSource
--
--         , testUpdateDataSource $
--             updateDataSource
--
--         , testCreateDataSourceFromRedshift $
--             createDataSourceFromRedshift
--
--         , testCreateDataSourceFromS $
--             createDataSourceFromS
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
--         , testDeleteMLModel $
--             deleteMLModel
--
--         , testUpdateMLModel $
--             updateMLModel
--
--         , testDescribeBatchPredictions $
--             describeBatchPredictions
--
--         , testUpdateEvaluation $
--             updateEvaluation
--
--         , testDeleteEvaluation $
--             deleteEvaluation
--
--         , testGetBatchPrediction $
--             getBatchPrediction
--
--         , testCreateEvaluation $
--             createEvaluation
--
--         , testCreateDataSourceFromRDS $
--             createDataSourceFromRDS
--
--         , testCreateBatchPrediction $
--             createBatchPrediction
--
--         , testPredict $
--             predict
--
--         , testDeleteRealtimeEndpoint $
--             deleteRealtimeEndpoint
--
--         , testDescribeEvaluations $
--             describeEvaluations
--
--         , testGetEvaluation $
--             getEvaluation
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
--         [ testDeleteDataSourceResponse $
--             deleteDataSourceResponse
--
--         , testUpdateDataSourceResponse $
--             updateDataSourceResponse
--
--         , testCreateDataSourceFromRedshiftResponse $
--             createDataSourceFromRedshiftResponse
--
--         , testCreateDataSourceFromSResponse $
--             createDataSourceFromSResponse
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
--         , testDeleteMLModelResponse $
--             deleteMLModelResponse
--
--         , testUpdateMLModelResponse $
--             updateMLModelResponse
--
--         , testDescribeBatchPredictionsResponse $
--             describeBatchPredictionsResponse
--
--         , testUpdateEvaluationResponse $
--             updateEvaluationResponse
--
--         , testDeleteEvaluationResponse $
--             deleteEvaluationResponse
--
--         , testGetBatchPredictionResponse $
--             getBatchPredictionResponse
--
--         , testCreateEvaluationResponse $
--             createEvaluationResponse
--
--         , testCreateDataSourceFromRDSResponse $
--             createDataSourceFromRDSResponse
--
--         , testCreateBatchPredictionResponse $
--             createBatchPredictionResponse
--
--         , testPredictResponse $
--             predictResponse
--
--         , testDeleteRealtimeEndpointResponse $
--             deleteRealtimeEndpointResponse
--
--         , testDescribeEvaluationsResponse $
--             describeEvaluationsResponse
--
--         , testGetEvaluationResponse $
--             getEvaluationResponse
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

testDeleteDataSource :: DeleteDataSource -> TestTree
testDeleteDataSource = req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

testUpdateDataSource :: UpdateDataSource -> TestTree
testUpdateDataSource = req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

testCreateDataSourceFromRedshift :: CreateDataSourceFromRedshift -> TestTree
testCreateDataSourceFromRedshift = req
    "CreateDataSourceFromRedshift"
    "fixture/CreateDataSourceFromRedshift.yaml"

testCreateDataSourceFromS :: CreateDataSourceFromS -> TestTree
testCreateDataSourceFromS = req
    "CreateDataSourceFromS"
    "fixture/CreateDataSourceFromS.yaml"

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

testDeleteMLModel :: DeleteMLModel -> TestTree
testDeleteMLModel = req
    "DeleteMLModel"
    "fixture/DeleteMLModel.yaml"

testUpdateMLModel :: UpdateMLModel -> TestTree
testUpdateMLModel = req
    "UpdateMLModel"
    "fixture/UpdateMLModel.yaml"

testDescribeBatchPredictions :: DescribeBatchPredictions -> TestTree
testDescribeBatchPredictions = req
    "DescribeBatchPredictions"
    "fixture/DescribeBatchPredictions.yaml"

testUpdateEvaluation :: UpdateEvaluation -> TestTree
testUpdateEvaluation = req
    "UpdateEvaluation"
    "fixture/UpdateEvaluation.yaml"

testDeleteEvaluation :: DeleteEvaluation -> TestTree
testDeleteEvaluation = req
    "DeleteEvaluation"
    "fixture/DeleteEvaluation.yaml"

testGetBatchPrediction :: GetBatchPrediction -> TestTree
testGetBatchPrediction = req
    "GetBatchPrediction"
    "fixture/GetBatchPrediction.yaml"

testCreateEvaluation :: CreateEvaluation -> TestTree
testCreateEvaluation = req
    "CreateEvaluation"
    "fixture/CreateEvaluation.yaml"

testCreateDataSourceFromRDS :: CreateDataSourceFromRDS -> TestTree
testCreateDataSourceFromRDS = req
    "CreateDataSourceFromRDS"
    "fixture/CreateDataSourceFromRDS.yaml"

testCreateBatchPrediction :: CreateBatchPrediction -> TestTree
testCreateBatchPrediction = req
    "CreateBatchPrediction"
    "fixture/CreateBatchPrediction.yaml"

testPredict :: Predict -> TestTree
testPredict = req
    "Predict"
    "fixture/Predict.yaml"

testDeleteRealtimeEndpoint :: DeleteRealtimeEndpoint -> TestTree
testDeleteRealtimeEndpoint = req
    "DeleteRealtimeEndpoint"
    "fixture/DeleteRealtimeEndpoint.yaml"

testDescribeEvaluations :: DescribeEvaluations -> TestTree
testDescribeEvaluations = req
    "DescribeEvaluations"
    "fixture/DescribeEvaluations.yaml"

testGetEvaluation :: GetEvaluation -> TestTree
testGetEvaluation = req
    "GetEvaluation"
    "fixture/GetEvaluation.yaml"

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

testDeleteDataSourceResponse :: DeleteDataSourceResponse -> TestTree
testDeleteDataSourceResponse = res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    machineLearning
    (Proxy :: Proxy DeleteDataSource)

testUpdateDataSourceResponse :: UpdateDataSourceResponse -> TestTree
testUpdateDataSourceResponse = res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    machineLearning
    (Proxy :: Proxy UpdateDataSource)

testCreateDataSourceFromRedshiftResponse :: CreateDataSourceFromRedshiftResponse -> TestTree
testCreateDataSourceFromRedshiftResponse = res
    "CreateDataSourceFromRedshiftResponse"
    "fixture/CreateDataSourceFromRedshiftResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateDataSourceFromRedshift)

testCreateDataSourceFromSResponse :: CreateDataSourceFromSResponse -> TestTree
testCreateDataSourceFromSResponse = res
    "CreateDataSourceFromSResponse"
    "fixture/CreateDataSourceFromSResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateDataSourceFromS)

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

testDescribeBatchPredictionsResponse :: DescribeBatchPredictionsResponse -> TestTree
testDescribeBatchPredictionsResponse = res
    "DescribeBatchPredictionsResponse"
    "fixture/DescribeBatchPredictionsResponse.proto"
    machineLearning
    (Proxy :: Proxy DescribeBatchPredictions)

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

testGetBatchPredictionResponse :: GetBatchPredictionResponse -> TestTree
testGetBatchPredictionResponse = res
    "GetBatchPredictionResponse"
    "fixture/GetBatchPredictionResponse.proto"
    machineLearning
    (Proxy :: Proxy GetBatchPrediction)

testCreateEvaluationResponse :: CreateEvaluationResponse -> TestTree
testCreateEvaluationResponse = res
    "CreateEvaluationResponse"
    "fixture/CreateEvaluationResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateEvaluation)

testCreateDataSourceFromRDSResponse :: CreateDataSourceFromRDSResponse -> TestTree
testCreateDataSourceFromRDSResponse = res
    "CreateDataSourceFromRDSResponse"
    "fixture/CreateDataSourceFromRDSResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateDataSourceFromRDS)

testCreateBatchPredictionResponse :: CreateBatchPredictionResponse -> TestTree
testCreateBatchPredictionResponse = res
    "CreateBatchPredictionResponse"
    "fixture/CreateBatchPredictionResponse.proto"
    machineLearning
    (Proxy :: Proxy CreateBatchPrediction)

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

testDescribeEvaluationsResponse :: DescribeEvaluationsResponse -> TestTree
testDescribeEvaluationsResponse = res
    "DescribeEvaluationsResponse"
    "fixture/DescribeEvaluationsResponse.proto"
    machineLearning
    (Proxy :: Proxy DescribeEvaluations)

testGetEvaluationResponse :: GetEvaluationResponse -> TestTree
testGetEvaluationResponse = res
    "GetEvaluationResponse"
    "fixture/GetEvaluationResponse.proto"
    machineLearning
    (Proxy :: Proxy GetEvaluation)

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
