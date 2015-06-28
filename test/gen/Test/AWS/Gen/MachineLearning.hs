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

import           Data.Proxy
import           Network.AWS.MachineLearning
import           Test.AWS.Fixture
import           Test.Tasty

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
    "DeleteDataSource"
    "fixture/MachineLearning/DeleteDataSourceResponse"
    (Proxy :: Proxy DeleteDataSource)

updateDataSourceResponseTest :: UpdateDataSourceResponse -> TestTree
updateDataSourceResponseTest = resp
    "UpdateDataSource"
    "fixture/MachineLearning/UpdateDataSourceResponse"
    (Proxy :: Proxy UpdateDataSource)

createDataSourceFromRedshiftResponseTest :: CreateDataSourceFromRedshiftResponse -> TestTree
createDataSourceFromRedshiftResponseTest = resp
    "CreateDataSourceFromRedshift"
    "fixture/MachineLearning/CreateDataSourceFromRedshiftResponse"
    (Proxy :: Proxy CreateDataSourceFromRedshift)

createDataSourceFromSResponseTest :: CreateDataSourceFromSResponse -> TestTree
createDataSourceFromSResponseTest = resp
    "CreateDataSourceFromS"
    "fixture/MachineLearning/CreateDataSourceFromSResponse"
    (Proxy :: Proxy CreateDataSourceFromS)

createMLModelResponseTest :: CreateMLModelResponse -> TestTree
createMLModelResponseTest = resp
    "CreateMLModel"
    "fixture/MachineLearning/CreateMLModelResponse"
    (Proxy :: Proxy CreateMLModel)

deleteBatchPredictionResponseTest :: DeleteBatchPredictionResponse -> TestTree
deleteBatchPredictionResponseTest = resp
    "DeleteBatchPrediction"
    "fixture/MachineLearning/DeleteBatchPredictionResponse"
    (Proxy :: Proxy DeleteBatchPrediction)

updateBatchPredictionResponseTest :: UpdateBatchPredictionResponse -> TestTree
updateBatchPredictionResponseTest = resp
    "UpdateBatchPrediction"
    "fixture/MachineLearning/UpdateBatchPredictionResponse"
    (Proxy :: Proxy UpdateBatchPrediction)

getMLModelResponseTest :: GetMLModelResponse -> TestTree
getMLModelResponseTest = resp
    "GetMLModel"
    "fixture/MachineLearning/GetMLModelResponse"
    (Proxy :: Proxy GetMLModel)

getDataSourceResponseTest :: GetDataSourceResponse -> TestTree
getDataSourceResponseTest = resp
    "GetDataSource"
    "fixture/MachineLearning/GetDataSourceResponse"
    (Proxy :: Proxy GetDataSource)

deleteMLModelResponseTest :: DeleteMLModelResponse -> TestTree
deleteMLModelResponseTest = resp
    "DeleteMLModel"
    "fixture/MachineLearning/DeleteMLModelResponse"
    (Proxy :: Proxy DeleteMLModel)

updateMLModelResponseTest :: UpdateMLModelResponse -> TestTree
updateMLModelResponseTest = resp
    "UpdateMLModel"
    "fixture/MachineLearning/UpdateMLModelResponse"
    (Proxy :: Proxy UpdateMLModel)

describeBatchPredictionsResponseTest :: DescribeBatchPredictionsResponse -> TestTree
describeBatchPredictionsResponseTest = resp
    "DescribeBatchPredictions"
    "fixture/MachineLearning/DescribeBatchPredictionsResponse"
    (Proxy :: Proxy DescribeBatchPredictions)

updateEvaluationResponseTest :: UpdateEvaluationResponse -> TestTree
updateEvaluationResponseTest = resp
    "UpdateEvaluation"
    "fixture/MachineLearning/UpdateEvaluationResponse"
    (Proxy :: Proxy UpdateEvaluation)

deleteEvaluationResponseTest :: DeleteEvaluationResponse -> TestTree
deleteEvaluationResponseTest = resp
    "DeleteEvaluation"
    "fixture/MachineLearning/DeleteEvaluationResponse"
    (Proxy :: Proxy DeleteEvaluation)

getBatchPredictionResponseTest :: GetBatchPredictionResponse -> TestTree
getBatchPredictionResponseTest = resp
    "GetBatchPrediction"
    "fixture/MachineLearning/GetBatchPredictionResponse"
    (Proxy :: Proxy GetBatchPrediction)

createEvaluationResponseTest :: CreateEvaluationResponse -> TestTree
createEvaluationResponseTest = resp
    "CreateEvaluation"
    "fixture/MachineLearning/CreateEvaluationResponse"
    (Proxy :: Proxy CreateEvaluation)

createDataSourceFromRDSResponseTest :: CreateDataSourceFromRDSResponse -> TestTree
createDataSourceFromRDSResponseTest = resp
    "CreateDataSourceFromRDS"
    "fixture/MachineLearning/CreateDataSourceFromRDSResponse"
    (Proxy :: Proxy CreateDataSourceFromRDS)

createBatchPredictionResponseTest :: CreateBatchPredictionResponse -> TestTree
createBatchPredictionResponseTest = resp
    "CreateBatchPrediction"
    "fixture/MachineLearning/CreateBatchPredictionResponse"
    (Proxy :: Proxy CreateBatchPrediction)

predictResponseTest :: PredictResponse -> TestTree
predictResponseTest = resp
    "Predict"
    "fixture/MachineLearning/PredictResponse"
    (Proxy :: Proxy Predict)

deleteRealtimeEndpointResponseTest :: DeleteRealtimeEndpointResponse -> TestTree
deleteRealtimeEndpointResponseTest = resp
    "DeleteRealtimeEndpoint"
    "fixture/MachineLearning/DeleteRealtimeEndpointResponse"
    (Proxy :: Proxy DeleteRealtimeEndpoint)

describeEvaluationsResponseTest :: DescribeEvaluationsResponse -> TestTree
describeEvaluationsResponseTest = resp
    "DescribeEvaluations"
    "fixture/MachineLearning/DescribeEvaluationsResponse"
    (Proxy :: Proxy DescribeEvaluations)

getEvaluationResponseTest :: GetEvaluationResponse -> TestTree
getEvaluationResponseTest = resp
    "GetEvaluation"
    "fixture/MachineLearning/GetEvaluationResponse"
    (Proxy :: Proxy GetEvaluation)

createRealtimeEndpointResponseTest :: CreateRealtimeEndpointResponse -> TestTree
createRealtimeEndpointResponseTest = resp
    "CreateRealtimeEndpoint"
    "fixture/MachineLearning/CreateRealtimeEndpointResponse"
    (Proxy :: Proxy CreateRealtimeEndpoint)

describeMLModelsResponseTest :: DescribeMLModelsResponse -> TestTree
describeMLModelsResponseTest = resp
    "DescribeMLModels"
    "fixture/MachineLearning/DescribeMLModelsResponse"
    (Proxy :: Proxy DescribeMLModels)

describeDataSourcesResponseTest :: DescribeDataSourcesResponse -> TestTree
describeDataSourcesResponseTest = resp
    "DescribeDataSources"
    "fixture/MachineLearning/DescribeDataSourcesResponse"
    (Proxy :: Proxy DescribeDataSources)
