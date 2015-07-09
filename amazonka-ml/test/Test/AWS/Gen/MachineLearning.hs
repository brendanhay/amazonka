{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.MachineLearning
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.MachineLearning where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.MachineLearning

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
testDeleteDataSource = undefined

testUpdateDataSource :: UpdateDataSource -> TestTree
testUpdateDataSource = undefined

testCreateDataSourceFromRedshift :: CreateDataSourceFromRedshift -> TestTree
testCreateDataSourceFromRedshift = undefined

testCreateDataSourceFromS :: CreateDataSourceFromS -> TestTree
testCreateDataSourceFromS = undefined

testCreateMLModel :: CreateMLModel -> TestTree
testCreateMLModel = undefined

testDeleteBatchPrediction :: DeleteBatchPrediction -> TestTree
testDeleteBatchPrediction = undefined

testUpdateBatchPrediction :: UpdateBatchPrediction -> TestTree
testUpdateBatchPrediction = undefined

testGetMLModel :: GetMLModel -> TestTree
testGetMLModel = undefined

testGetDataSource :: GetDataSource -> TestTree
testGetDataSource = undefined

testDeleteMLModel :: DeleteMLModel -> TestTree
testDeleteMLModel = undefined

testUpdateMLModel :: UpdateMLModel -> TestTree
testUpdateMLModel = undefined

testDescribeBatchPredictions :: DescribeBatchPredictions -> TestTree
testDescribeBatchPredictions = undefined

testUpdateEvaluation :: UpdateEvaluation -> TestTree
testUpdateEvaluation = undefined

testDeleteEvaluation :: DeleteEvaluation -> TestTree
testDeleteEvaluation = undefined

testGetBatchPrediction :: GetBatchPrediction -> TestTree
testGetBatchPrediction = undefined

testCreateEvaluation :: CreateEvaluation -> TestTree
testCreateEvaluation = undefined

testCreateDataSourceFromRDS :: CreateDataSourceFromRDS -> TestTree
testCreateDataSourceFromRDS = undefined

testCreateBatchPrediction :: CreateBatchPrediction -> TestTree
testCreateBatchPrediction = undefined

testPredict :: Predict -> TestTree
testPredict = undefined

testDeleteRealtimeEndpoint :: DeleteRealtimeEndpoint -> TestTree
testDeleteRealtimeEndpoint = undefined

testDescribeEvaluations :: DescribeEvaluations -> TestTree
testDescribeEvaluations = undefined

testGetEvaluation :: GetEvaluation -> TestTree
testGetEvaluation = undefined

testCreateRealtimeEndpoint :: CreateRealtimeEndpoint -> TestTree
testCreateRealtimeEndpoint = undefined

testDescribeMLModels :: DescribeMLModels -> TestTree
testDescribeMLModels = undefined

testDescribeDataSources :: DescribeDataSources -> TestTree
testDescribeDataSources = undefined

-- Responses

testDeleteDataSourceResponse :: DeleteDataSourceResponse -> TestTree
testDeleteDataSourceResponse = resp
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse"
    (Proxy :: Proxy DeleteDataSource)

testUpdateDataSourceResponse :: UpdateDataSourceResponse -> TestTree
testUpdateDataSourceResponse = resp
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse"
    (Proxy :: Proxy UpdateDataSource)

testCreateDataSourceFromRedshiftResponse :: CreateDataSourceFromRedshiftResponse -> TestTree
testCreateDataSourceFromRedshiftResponse = resp
    "CreateDataSourceFromRedshiftResponse"
    "fixture/CreateDataSourceFromRedshiftResponse"
    (Proxy :: Proxy CreateDataSourceFromRedshift)

testCreateDataSourceFromSResponse :: CreateDataSourceFromSResponse -> TestTree
testCreateDataSourceFromSResponse = resp
    "CreateDataSourceFromSResponse"
    "fixture/CreateDataSourceFromSResponse"
    (Proxy :: Proxy CreateDataSourceFromS)

testCreateMLModelResponse :: CreateMLModelResponse -> TestTree
testCreateMLModelResponse = resp
    "CreateMLModelResponse"
    "fixture/CreateMLModelResponse"
    (Proxy :: Proxy CreateMLModel)

testDeleteBatchPredictionResponse :: DeleteBatchPredictionResponse -> TestTree
testDeleteBatchPredictionResponse = resp
    "DeleteBatchPredictionResponse"
    "fixture/DeleteBatchPredictionResponse"
    (Proxy :: Proxy DeleteBatchPrediction)

testUpdateBatchPredictionResponse :: UpdateBatchPredictionResponse -> TestTree
testUpdateBatchPredictionResponse = resp
    "UpdateBatchPredictionResponse"
    "fixture/UpdateBatchPredictionResponse"
    (Proxy :: Proxy UpdateBatchPrediction)

testGetMLModelResponse :: GetMLModelResponse -> TestTree
testGetMLModelResponse = resp
    "GetMLModelResponse"
    "fixture/GetMLModelResponse"
    (Proxy :: Proxy GetMLModel)

testGetDataSourceResponse :: GetDataSourceResponse -> TestTree
testGetDataSourceResponse = resp
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse"
    (Proxy :: Proxy GetDataSource)

testDeleteMLModelResponse :: DeleteMLModelResponse -> TestTree
testDeleteMLModelResponse = resp
    "DeleteMLModelResponse"
    "fixture/DeleteMLModelResponse"
    (Proxy :: Proxy DeleteMLModel)

testUpdateMLModelResponse :: UpdateMLModelResponse -> TestTree
testUpdateMLModelResponse = resp
    "UpdateMLModelResponse"
    "fixture/UpdateMLModelResponse"
    (Proxy :: Proxy UpdateMLModel)

testDescribeBatchPredictionsResponse :: DescribeBatchPredictionsResponse -> TestTree
testDescribeBatchPredictionsResponse = resp
    "DescribeBatchPredictionsResponse"
    "fixture/DescribeBatchPredictionsResponse"
    (Proxy :: Proxy DescribeBatchPredictions)

testUpdateEvaluationResponse :: UpdateEvaluationResponse -> TestTree
testUpdateEvaluationResponse = resp
    "UpdateEvaluationResponse"
    "fixture/UpdateEvaluationResponse"
    (Proxy :: Proxy UpdateEvaluation)

testDeleteEvaluationResponse :: DeleteEvaluationResponse -> TestTree
testDeleteEvaluationResponse = resp
    "DeleteEvaluationResponse"
    "fixture/DeleteEvaluationResponse"
    (Proxy :: Proxy DeleteEvaluation)

testGetBatchPredictionResponse :: GetBatchPredictionResponse -> TestTree
testGetBatchPredictionResponse = resp
    "GetBatchPredictionResponse"
    "fixture/GetBatchPredictionResponse"
    (Proxy :: Proxy GetBatchPrediction)

testCreateEvaluationResponse :: CreateEvaluationResponse -> TestTree
testCreateEvaluationResponse = resp
    "CreateEvaluationResponse"
    "fixture/CreateEvaluationResponse"
    (Proxy :: Proxy CreateEvaluation)

testCreateDataSourceFromRDSResponse :: CreateDataSourceFromRDSResponse -> TestTree
testCreateDataSourceFromRDSResponse = resp
    "CreateDataSourceFromRDSResponse"
    "fixture/CreateDataSourceFromRDSResponse"
    (Proxy :: Proxy CreateDataSourceFromRDS)

testCreateBatchPredictionResponse :: CreateBatchPredictionResponse -> TestTree
testCreateBatchPredictionResponse = resp
    "CreateBatchPredictionResponse"
    "fixture/CreateBatchPredictionResponse"
    (Proxy :: Proxy CreateBatchPrediction)

testPredictResponse :: PredictResponse -> TestTree
testPredictResponse = resp
    "PredictResponse"
    "fixture/PredictResponse"
    (Proxy :: Proxy Predict)

testDeleteRealtimeEndpointResponse :: DeleteRealtimeEndpointResponse -> TestTree
testDeleteRealtimeEndpointResponse = resp
    "DeleteRealtimeEndpointResponse"
    "fixture/DeleteRealtimeEndpointResponse"
    (Proxy :: Proxy DeleteRealtimeEndpoint)

testDescribeEvaluationsResponse :: DescribeEvaluationsResponse -> TestTree
testDescribeEvaluationsResponse = resp
    "DescribeEvaluationsResponse"
    "fixture/DescribeEvaluationsResponse"
    (Proxy :: Proxy DescribeEvaluations)

testGetEvaluationResponse :: GetEvaluationResponse -> TestTree
testGetEvaluationResponse = resp
    "GetEvaluationResponse"
    "fixture/GetEvaluationResponse"
    (Proxy :: Proxy GetEvaluation)

testCreateRealtimeEndpointResponse :: CreateRealtimeEndpointResponse -> TestTree
testCreateRealtimeEndpointResponse = resp
    "CreateRealtimeEndpointResponse"
    "fixture/CreateRealtimeEndpointResponse"
    (Proxy :: Proxy CreateRealtimeEndpoint)

testDescribeMLModelsResponse :: DescribeMLModelsResponse -> TestTree
testDescribeMLModelsResponse = resp
    "DescribeMLModelsResponse"
    "fixture/DescribeMLModelsResponse"
    (Proxy :: Proxy DescribeMLModels)

testDescribeDataSourcesResponse :: DescribeDataSourcesResponse -> TestTree
testDescribeDataSourcesResponse = resp
    "DescribeDataSourcesResponse"
    "fixture/DescribeDataSourcesResponse"
    (Proxy :: Proxy DescribeDataSources)

instance Out Algorithm
instance Out BatchPrediction
instance Out BatchPredictionFilterVariable
instance Out CreateBatchPrediction
instance Out CreateBatchPredictionResponse
instance Out CreateDataSourceFromRDS
instance Out CreateDataSourceFromRDSResponse
instance Out CreateDataSourceFromRedshift
instance Out CreateDataSourceFromRedshiftResponse
instance Out CreateDataSourceFromS
instance Out CreateDataSourceFromSResponse
instance Out CreateEvaluation
instance Out CreateEvaluationResponse
instance Out CreateMLModel
instance Out CreateMLModelResponse
instance Out CreateRealtimeEndpoint
instance Out CreateRealtimeEndpointResponse
instance Out DataSource
instance Out DataSourceFilterVariable
instance Out DeleteBatchPrediction
instance Out DeleteBatchPredictionResponse
instance Out DeleteDataSource
instance Out DeleteDataSourceResponse
instance Out DeleteEvaluation
instance Out DeleteEvaluationResponse
instance Out DeleteMLModel
instance Out DeleteMLModelResponse
instance Out DeleteRealtimeEndpoint
instance Out DeleteRealtimeEndpointResponse
instance Out DescribeBatchPredictions
instance Out DescribeBatchPredictionsResponse
instance Out DescribeDataSources
instance Out DescribeDataSourcesResponse
instance Out DescribeEvaluations
instance Out DescribeEvaluationsResponse
instance Out DescribeMLModels
instance Out DescribeMLModelsResponse
instance Out DetailsAttributes
instance Out EntityStatus
instance Out Evaluation
instance Out EvaluationFilterVariable
instance Out GetBatchPrediction
instance Out GetBatchPredictionResponse
instance Out GetDataSource
instance Out GetDataSourceResponse
instance Out GetEvaluation
instance Out GetEvaluationResponse
instance Out GetMLModel
instance Out GetMLModelResponse
instance Out MLModel
instance Out MLModelFilterVariable
instance Out MLModelType
instance Out PerformanceMetrics
instance Out Predict
instance Out PredictResponse
instance Out Prediction
instance Out RDSDataSpec
instance Out RDSDatabase
instance Out RDSDatabaseCredentials
instance Out RDSMetadata
instance Out RealtimeEndpointInfo
instance Out RealtimeEndpointStatus
instance Out RedshiftDataSpec
instance Out RedshiftDatabase
instance Out RedshiftDatabaseCredentials
instance Out RedshiftMetadata
instance Out S3DataSpec
instance Out SortOrder
instance Out UpdateBatchPrediction
instance Out UpdateBatchPredictionResponse
instance Out UpdateDataSource
instance Out UpdateDataSourceResponse
instance Out UpdateEvaluation
instance Out UpdateEvaluationResponse
instance Out UpdateMLModel
instance Out UpdateMLModelResponse
