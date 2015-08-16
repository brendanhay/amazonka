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
    "fixture/DeleteDataSource"

testUpdateDataSource :: UpdateDataSource -> TestTree
testUpdateDataSource = req
    "UpdateDataSource"
    "fixture/UpdateDataSource"

testCreateDataSourceFromRedshift :: CreateDataSourceFromRedshift -> TestTree
testCreateDataSourceFromRedshift = req
    "CreateDataSourceFromRedshift"
    "fixture/CreateDataSourceFromRedshift"

testCreateDataSourceFromS :: CreateDataSourceFromS -> TestTree
testCreateDataSourceFromS = req
    "CreateDataSourceFromS"
    "fixture/CreateDataSourceFromS"

testCreateMLModel :: CreateMLModel -> TestTree
testCreateMLModel = req
    "CreateMLModel"
    "fixture/CreateMLModel"

testDeleteBatchPrediction :: DeleteBatchPrediction -> TestTree
testDeleteBatchPrediction = req
    "DeleteBatchPrediction"
    "fixture/DeleteBatchPrediction"

testUpdateBatchPrediction :: UpdateBatchPrediction -> TestTree
testUpdateBatchPrediction = req
    "UpdateBatchPrediction"
    "fixture/UpdateBatchPrediction"

testGetMLModel :: GetMLModel -> TestTree
testGetMLModel = req
    "GetMLModel"
    "fixture/GetMLModel"

testGetDataSource :: GetDataSource -> TestTree
testGetDataSource = req
    "GetDataSource"
    "fixture/GetDataSource"

testDeleteMLModel :: DeleteMLModel -> TestTree
testDeleteMLModel = req
    "DeleteMLModel"
    "fixture/DeleteMLModel"

testUpdateMLModel :: UpdateMLModel -> TestTree
testUpdateMLModel = req
    "UpdateMLModel"
    "fixture/UpdateMLModel"

testDescribeBatchPredictions :: DescribeBatchPredictions -> TestTree
testDescribeBatchPredictions = req
    "DescribeBatchPredictions"
    "fixture/DescribeBatchPredictions"

testUpdateEvaluation :: UpdateEvaluation -> TestTree
testUpdateEvaluation = req
    "UpdateEvaluation"
    "fixture/UpdateEvaluation"

testDeleteEvaluation :: DeleteEvaluation -> TestTree
testDeleteEvaluation = req
    "DeleteEvaluation"
    "fixture/DeleteEvaluation"

testGetBatchPrediction :: GetBatchPrediction -> TestTree
testGetBatchPrediction = req
    "GetBatchPrediction"
    "fixture/GetBatchPrediction"

testCreateEvaluation :: CreateEvaluation -> TestTree
testCreateEvaluation = req
    "CreateEvaluation"
    "fixture/CreateEvaluation"

testCreateDataSourceFromRDS :: CreateDataSourceFromRDS -> TestTree
testCreateDataSourceFromRDS = req
    "CreateDataSourceFromRDS"
    "fixture/CreateDataSourceFromRDS"

testCreateBatchPrediction :: CreateBatchPrediction -> TestTree
testCreateBatchPrediction = req
    "CreateBatchPrediction"
    "fixture/CreateBatchPrediction"

testPredict :: Predict -> TestTree
testPredict = req
    "Predict"
    "fixture/Predict"

testDeleteRealtimeEndpoint :: DeleteRealtimeEndpoint -> TestTree
testDeleteRealtimeEndpoint = req
    "DeleteRealtimeEndpoint"
    "fixture/DeleteRealtimeEndpoint"

testDescribeEvaluations :: DescribeEvaluations -> TestTree
testDescribeEvaluations = req
    "DescribeEvaluations"
    "fixture/DescribeEvaluations"

testGetEvaluation :: GetEvaluation -> TestTree
testGetEvaluation = req
    "GetEvaluation"
    "fixture/GetEvaluation"

testCreateRealtimeEndpoint :: CreateRealtimeEndpoint -> TestTree
testCreateRealtimeEndpoint = req
    "CreateRealtimeEndpoint"
    "fixture/CreateRealtimeEndpoint"

testDescribeMLModels :: DescribeMLModels -> TestTree
testDescribeMLModels = req
    "DescribeMLModels"
    "fixture/DescribeMLModels"

testDescribeDataSources :: DescribeDataSources -> TestTree
testDescribeDataSources = req
    "DescribeDataSources"
    "fixture/DescribeDataSources"

-- Responses

testDeleteDataSourceResponse :: DeleteDataSourceResponse -> TestTree
testDeleteDataSourceResponse = res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse"
    (Proxy :: Proxy DeleteDataSource)

testUpdateDataSourceResponse :: UpdateDataSourceResponse -> TestTree
testUpdateDataSourceResponse = res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse"
    (Proxy :: Proxy UpdateDataSource)

testCreateDataSourceFromRedshiftResponse :: CreateDataSourceFromRedshiftResponse -> TestTree
testCreateDataSourceFromRedshiftResponse = res
    "CreateDataSourceFromRedshiftResponse"
    "fixture/CreateDataSourceFromRedshiftResponse"
    (Proxy :: Proxy CreateDataSourceFromRedshift)

testCreateDataSourceFromSResponse :: CreateDataSourceFromSResponse -> TestTree
testCreateDataSourceFromSResponse = res
    "CreateDataSourceFromSResponse"
    "fixture/CreateDataSourceFromSResponse"
    (Proxy :: Proxy CreateDataSourceFromS)

testCreateMLModelResponse :: CreateMLModelResponse -> TestTree
testCreateMLModelResponse = res
    "CreateMLModelResponse"
    "fixture/CreateMLModelResponse"
    (Proxy :: Proxy CreateMLModel)

testDeleteBatchPredictionResponse :: DeleteBatchPredictionResponse -> TestTree
testDeleteBatchPredictionResponse = res
    "DeleteBatchPredictionResponse"
    "fixture/DeleteBatchPredictionResponse"
    (Proxy :: Proxy DeleteBatchPrediction)

testUpdateBatchPredictionResponse :: UpdateBatchPredictionResponse -> TestTree
testUpdateBatchPredictionResponse = res
    "UpdateBatchPredictionResponse"
    "fixture/UpdateBatchPredictionResponse"
    (Proxy :: Proxy UpdateBatchPrediction)

testGetMLModelResponse :: GetMLModelResponse -> TestTree
testGetMLModelResponse = res
    "GetMLModelResponse"
    "fixture/GetMLModelResponse"
    (Proxy :: Proxy GetMLModel)

testGetDataSourceResponse :: GetDataSourceResponse -> TestTree
testGetDataSourceResponse = res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse"
    (Proxy :: Proxy GetDataSource)

testDeleteMLModelResponse :: DeleteMLModelResponse -> TestTree
testDeleteMLModelResponse = res
    "DeleteMLModelResponse"
    "fixture/DeleteMLModelResponse"
    (Proxy :: Proxy DeleteMLModel)

testUpdateMLModelResponse :: UpdateMLModelResponse -> TestTree
testUpdateMLModelResponse = res
    "UpdateMLModelResponse"
    "fixture/UpdateMLModelResponse"
    (Proxy :: Proxy UpdateMLModel)

testDescribeBatchPredictionsResponse :: DescribeBatchPredictionsResponse -> TestTree
testDescribeBatchPredictionsResponse = res
    "DescribeBatchPredictionsResponse"
    "fixture/DescribeBatchPredictionsResponse"
    (Proxy :: Proxy DescribeBatchPredictions)

testUpdateEvaluationResponse :: UpdateEvaluationResponse -> TestTree
testUpdateEvaluationResponse = res
    "UpdateEvaluationResponse"
    "fixture/UpdateEvaluationResponse"
    (Proxy :: Proxy UpdateEvaluation)

testDeleteEvaluationResponse :: DeleteEvaluationResponse -> TestTree
testDeleteEvaluationResponse = res
    "DeleteEvaluationResponse"
    "fixture/DeleteEvaluationResponse"
    (Proxy :: Proxy DeleteEvaluation)

testGetBatchPredictionResponse :: GetBatchPredictionResponse -> TestTree
testGetBatchPredictionResponse = res
    "GetBatchPredictionResponse"
    "fixture/GetBatchPredictionResponse"
    (Proxy :: Proxy GetBatchPrediction)

testCreateEvaluationResponse :: CreateEvaluationResponse -> TestTree
testCreateEvaluationResponse = res
    "CreateEvaluationResponse"
    "fixture/CreateEvaluationResponse"
    (Proxy :: Proxy CreateEvaluation)

testCreateDataSourceFromRDSResponse :: CreateDataSourceFromRDSResponse -> TestTree
testCreateDataSourceFromRDSResponse = res
    "CreateDataSourceFromRDSResponse"
    "fixture/CreateDataSourceFromRDSResponse"
    (Proxy :: Proxy CreateDataSourceFromRDS)

testCreateBatchPredictionResponse :: CreateBatchPredictionResponse -> TestTree
testCreateBatchPredictionResponse = res
    "CreateBatchPredictionResponse"
    "fixture/CreateBatchPredictionResponse"
    (Proxy :: Proxy CreateBatchPrediction)

testPredictResponse :: PredictResponse -> TestTree
testPredictResponse = res
    "PredictResponse"
    "fixture/PredictResponse"
    (Proxy :: Proxy Predict)

testDeleteRealtimeEndpointResponse :: DeleteRealtimeEndpointResponse -> TestTree
testDeleteRealtimeEndpointResponse = res
    "DeleteRealtimeEndpointResponse"
    "fixture/DeleteRealtimeEndpointResponse"
    (Proxy :: Proxy DeleteRealtimeEndpoint)

testDescribeEvaluationsResponse :: DescribeEvaluationsResponse -> TestTree
testDescribeEvaluationsResponse = res
    "DescribeEvaluationsResponse"
    "fixture/DescribeEvaluationsResponse"
    (Proxy :: Proxy DescribeEvaluations)

testGetEvaluationResponse :: GetEvaluationResponse -> TestTree
testGetEvaluationResponse = res
    "GetEvaluationResponse"
    "fixture/GetEvaluationResponse"
    (Proxy :: Proxy GetEvaluation)

testCreateRealtimeEndpointResponse :: CreateRealtimeEndpointResponse -> TestTree
testCreateRealtimeEndpointResponse = res
    "CreateRealtimeEndpointResponse"
    "fixture/CreateRealtimeEndpointResponse"
    (Proxy :: Proxy CreateRealtimeEndpoint)

testDescribeMLModelsResponse :: DescribeMLModelsResponse -> TestTree
testDescribeMLModelsResponse = res
    "DescribeMLModelsResponse"
    "fixture/DescribeMLModelsResponse"
    (Proxy :: Proxy DescribeMLModels)

testDescribeDataSourcesResponse :: DescribeDataSourcesResponse -> TestTree
testDescribeDataSourcesResponse = res
    "DescribeDataSourcesResponse"
    "fixture/DescribeDataSourcesResponse"
    (Proxy :: Proxy DescribeDataSources)
