-- Module      : Test.AWS.Gen.MachineLearning
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ deleteDataSourceTest $
--             deleteDataSource
--
--         , updateDataSourceTest $
--             updateDataSource
--
--         , createDataSourceFromRedshiftTest $
--             createDataSourceFromRedshift
--
--         , createDataSourceFromSTest $
--             createDataSourceFromS
--
--         , createMLModelTest $
--             createMLModel
--
--         , deleteBatchPredictionTest $
--             deleteBatchPrediction
--
--         , updateBatchPredictionTest $
--             updateBatchPrediction
--
--         , getMLModelTest $
--             getMLModel
--
--         , getDataSourceTest $
--             getDataSource
--
--         , deleteMLModelTest $
--             deleteMLModel
--
--         , updateMLModelTest $
--             updateMLModel
--
--         , describeBatchPredictionsTest $
--             describeBatchPredictions
--
--         , updateEvaluationTest $
--             updateEvaluation
--
--         , deleteEvaluationTest $
--             deleteEvaluation
--
--         , getBatchPredictionTest $
--             getBatchPrediction
--
--         , createEvaluationTest $
--             createEvaluation
--
--         , createDataSourceFromRDSTest $
--             createDataSourceFromRDS
--
--         , createBatchPredictionTest $
--             createBatchPrediction
--
--         , predictTest $
--             predict
--
--         , deleteRealtimeEndpointTest $
--             deleteRealtimeEndpoint
--
--         , describeEvaluationsTest $
--             describeEvaluations
--
--         , getEvaluationTest $
--             getEvaluation
--
--         , createRealtimeEndpointTest $
--             createRealtimeEndpoint
--
--         , describeMLModelsTest $
--             describeMLModels
--
--         , describeDataSourcesTest $
--             describeDataSources
--
--           ]

--     , testGroup "response"
--         [ deleteDataSourceResponseTest $
--             deleteDataSourceResponse
--
--         , updateDataSourceResponseTest $
--             updateDataSourceResponse
--
--         , createDataSourceFromRedshiftResponseTest $
--             createDataSourceFromRedshiftResponse
--
--         , createDataSourceFromSResponseTest $
--             createDataSourceFromSResponse
--
--         , createMLModelResponseTest $
--             createMLModelResponse
--
--         , deleteBatchPredictionResponseTest $
--             deleteBatchPredictionResponse
--
--         , updateBatchPredictionResponseTest $
--             updateBatchPredictionResponse
--
--         , getMLModelResponseTest $
--             getMLModelResponse
--
--         , getDataSourceResponseTest $
--             getDataSourceResponse
--
--         , deleteMLModelResponseTest $
--             deleteMLModelResponse
--
--         , updateMLModelResponseTest $
--             updateMLModelResponse
--
--         , describeBatchPredictionsResponseTest $
--             describeBatchPredictionsResponse
--
--         , updateEvaluationResponseTest $
--             updateEvaluationResponse
--
--         , deleteEvaluationResponseTest $
--             deleteEvaluationResponse
--
--         , getBatchPredictionResponseTest $
--             getBatchPredictionResponse
--
--         , createEvaluationResponseTest $
--             createEvaluationResponse
--
--         , createDataSourceFromRDSResponseTest $
--             createDataSourceFromRDSResponse
--
--         , createBatchPredictionResponseTest $
--             createBatchPredictionResponse
--
--         , predictResponseTest $
--             predictResponse
--
--         , deleteRealtimeEndpointResponseTest $
--             deleteRealtimeEndpointResponse
--
--         , describeEvaluationsResponseTest $
--             describeEvaluationsResponse
--
--         , getEvaluationResponseTest $
--             getEvaluationResponse
--
--         , createRealtimeEndpointResponseTest $
--             createRealtimeEndpointResponse
--
--         , describeMLModelsResponseTest $
--             describeMLModelsResponse
--
--         , describeDataSourcesResponseTest $
--             describeDataSourcesResponse
--
--           ]
--     ]

-- Requests

deleteDataSourceTest :: DeleteDataSource -> TestTree
deleteDataSourceTest = undefined

updateDataSourceTest :: UpdateDataSource -> TestTree
updateDataSourceTest = undefined

createDataSourceFromRedshiftTest :: CreateDataSourceFromRedshift -> TestTree
createDataSourceFromRedshiftTest = undefined

createDataSourceFromSTest :: CreateDataSourceFromS -> TestTree
createDataSourceFromSTest = undefined

createMLModelTest :: CreateMLModel -> TestTree
createMLModelTest = undefined

deleteBatchPredictionTest :: DeleteBatchPrediction -> TestTree
deleteBatchPredictionTest = undefined

updateBatchPredictionTest :: UpdateBatchPrediction -> TestTree
updateBatchPredictionTest = undefined

getMLModelTest :: GetMLModel -> TestTree
getMLModelTest = undefined

getDataSourceTest :: GetDataSource -> TestTree
getDataSourceTest = undefined

deleteMLModelTest :: DeleteMLModel -> TestTree
deleteMLModelTest = undefined

updateMLModelTest :: UpdateMLModel -> TestTree
updateMLModelTest = undefined

describeBatchPredictionsTest :: DescribeBatchPredictions -> TestTree
describeBatchPredictionsTest = undefined

updateEvaluationTest :: UpdateEvaluation -> TestTree
updateEvaluationTest = undefined

deleteEvaluationTest :: DeleteEvaluation -> TestTree
deleteEvaluationTest = undefined

getBatchPredictionTest :: GetBatchPrediction -> TestTree
getBatchPredictionTest = undefined

createEvaluationTest :: CreateEvaluation -> TestTree
createEvaluationTest = undefined

createDataSourceFromRDSTest :: CreateDataSourceFromRDS -> TestTree
createDataSourceFromRDSTest = undefined

createBatchPredictionTest :: CreateBatchPrediction -> TestTree
createBatchPredictionTest = undefined

predictTest :: Predict -> TestTree
predictTest = undefined

deleteRealtimeEndpointTest :: DeleteRealtimeEndpoint -> TestTree
deleteRealtimeEndpointTest = undefined

describeEvaluationsTest :: DescribeEvaluations -> TestTree
describeEvaluationsTest = undefined

getEvaluationTest :: GetEvaluation -> TestTree
getEvaluationTest = undefined

createRealtimeEndpointTest :: CreateRealtimeEndpoint -> TestTree
createRealtimeEndpointTest = undefined

describeMLModelsTest :: DescribeMLModels -> TestTree
describeMLModelsTest = undefined

describeDataSourcesTest :: DescribeDataSources -> TestTree
describeDataSourcesTest = undefined

-- Responses

deleteDataSourceResponseTest :: DeleteDataSourceResponse -> TestTree
deleteDataSourceResponseTest = resp
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse"
    (Proxy :: Proxy DeleteDataSource)

updateDataSourceResponseTest :: UpdateDataSourceResponse -> TestTree
updateDataSourceResponseTest = resp
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse"
    (Proxy :: Proxy UpdateDataSource)

createDataSourceFromRedshiftResponseTest :: CreateDataSourceFromRedshiftResponse -> TestTree
createDataSourceFromRedshiftResponseTest = resp
    "CreateDataSourceFromRedshiftResponse"
    "fixture/CreateDataSourceFromRedshiftResponse"
    (Proxy :: Proxy CreateDataSourceFromRedshift)

createDataSourceFromSResponseTest :: CreateDataSourceFromSResponse -> TestTree
createDataSourceFromSResponseTest = resp
    "CreateDataSourceFromSResponse"
    "fixture/CreateDataSourceFromSResponse"
    (Proxy :: Proxy CreateDataSourceFromS)

createMLModelResponseTest :: CreateMLModelResponse -> TestTree
createMLModelResponseTest = resp
    "CreateMLModelResponse"
    "fixture/CreateMLModelResponse"
    (Proxy :: Proxy CreateMLModel)

deleteBatchPredictionResponseTest :: DeleteBatchPredictionResponse -> TestTree
deleteBatchPredictionResponseTest = resp
    "DeleteBatchPredictionResponse"
    "fixture/DeleteBatchPredictionResponse"
    (Proxy :: Proxy DeleteBatchPrediction)

updateBatchPredictionResponseTest :: UpdateBatchPredictionResponse -> TestTree
updateBatchPredictionResponseTest = resp
    "UpdateBatchPredictionResponse"
    "fixture/UpdateBatchPredictionResponse"
    (Proxy :: Proxy UpdateBatchPrediction)

getMLModelResponseTest :: GetMLModelResponse -> TestTree
getMLModelResponseTest = resp
    "GetMLModelResponse"
    "fixture/GetMLModelResponse"
    (Proxy :: Proxy GetMLModel)

getDataSourceResponseTest :: GetDataSourceResponse -> TestTree
getDataSourceResponseTest = resp
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse"
    (Proxy :: Proxy GetDataSource)

deleteMLModelResponseTest :: DeleteMLModelResponse -> TestTree
deleteMLModelResponseTest = resp
    "DeleteMLModelResponse"
    "fixture/DeleteMLModelResponse"
    (Proxy :: Proxy DeleteMLModel)

updateMLModelResponseTest :: UpdateMLModelResponse -> TestTree
updateMLModelResponseTest = resp
    "UpdateMLModelResponse"
    "fixture/UpdateMLModelResponse"
    (Proxy :: Proxy UpdateMLModel)

describeBatchPredictionsResponseTest :: DescribeBatchPredictionsResponse -> TestTree
describeBatchPredictionsResponseTest = resp
    "DescribeBatchPredictionsResponse"
    "fixture/DescribeBatchPredictionsResponse"
    (Proxy :: Proxy DescribeBatchPredictions)

updateEvaluationResponseTest :: UpdateEvaluationResponse -> TestTree
updateEvaluationResponseTest = resp
    "UpdateEvaluationResponse"
    "fixture/UpdateEvaluationResponse"
    (Proxy :: Proxy UpdateEvaluation)

deleteEvaluationResponseTest :: DeleteEvaluationResponse -> TestTree
deleteEvaluationResponseTest = resp
    "DeleteEvaluationResponse"
    "fixture/DeleteEvaluationResponse"
    (Proxy :: Proxy DeleteEvaluation)

getBatchPredictionResponseTest :: GetBatchPredictionResponse -> TestTree
getBatchPredictionResponseTest = resp
    "GetBatchPredictionResponse"
    "fixture/GetBatchPredictionResponse"
    (Proxy :: Proxy GetBatchPrediction)

createEvaluationResponseTest :: CreateEvaluationResponse -> TestTree
createEvaluationResponseTest = resp
    "CreateEvaluationResponse"
    "fixture/CreateEvaluationResponse"
    (Proxy :: Proxy CreateEvaluation)

createDataSourceFromRDSResponseTest :: CreateDataSourceFromRDSResponse -> TestTree
createDataSourceFromRDSResponseTest = resp
    "CreateDataSourceFromRDSResponse"
    "fixture/CreateDataSourceFromRDSResponse"
    (Proxy :: Proxy CreateDataSourceFromRDS)

createBatchPredictionResponseTest :: CreateBatchPredictionResponse -> TestTree
createBatchPredictionResponseTest = resp
    "CreateBatchPredictionResponse"
    "fixture/CreateBatchPredictionResponse"
    (Proxy :: Proxy CreateBatchPrediction)

predictResponseTest :: PredictResponse -> TestTree
predictResponseTest = resp
    "PredictResponse"
    "fixture/PredictResponse"
    (Proxy :: Proxy Predict)

deleteRealtimeEndpointResponseTest :: DeleteRealtimeEndpointResponse -> TestTree
deleteRealtimeEndpointResponseTest = resp
    "DeleteRealtimeEndpointResponse"
    "fixture/DeleteRealtimeEndpointResponse"
    (Proxy :: Proxy DeleteRealtimeEndpoint)

describeEvaluationsResponseTest :: DescribeEvaluationsResponse -> TestTree
describeEvaluationsResponseTest = resp
    "DescribeEvaluationsResponse"
    "fixture/DescribeEvaluationsResponse"
    (Proxy :: Proxy DescribeEvaluations)

getEvaluationResponseTest :: GetEvaluationResponse -> TestTree
getEvaluationResponseTest = resp
    "GetEvaluationResponse"
    "fixture/GetEvaluationResponse"
    (Proxy :: Proxy GetEvaluation)

createRealtimeEndpointResponseTest :: CreateRealtimeEndpointResponse -> TestTree
createRealtimeEndpointResponseTest = resp
    "CreateRealtimeEndpointResponse"
    "fixture/CreateRealtimeEndpointResponse"
    (Proxy :: Proxy CreateRealtimeEndpoint)

describeMLModelsResponseTest :: DescribeMLModelsResponse -> TestTree
describeMLModelsResponseTest = resp
    "DescribeMLModelsResponse"
    "fixture/DescribeMLModelsResponse"
    (Proxy :: Proxy DescribeMLModels)

describeDataSourcesResponseTest :: DescribeDataSourcesResponse -> TestTree
describeDataSourcesResponseTest = resp
    "DescribeDataSourcesResponse"
    "fixture/DescribeDataSourcesResponse"
    (Proxy :: Proxy DescribeDataSources)
