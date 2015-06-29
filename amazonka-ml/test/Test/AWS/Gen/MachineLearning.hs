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
--         [ createBatchPredictionTest $
--             createBatchPrediction
--
--         , createDataSourceFromRDSTest $
--             createDataSourceFromRDS
--
--         , createDataSourceFromRedshiftTest $
--             createDataSourceFromRedshift
--
--         , createDataSourceFromS3Test $
--             createDataSourceFromS
--
--         , createEvaluationTest $
--             createEvaluation
--
--         , createMLModelTest $
--             createMLModel
--
--         , createRealtimeEndpointTest $
--             createRealtimeEndpoint
--
--         , deleteBatchPredictionTest $
--             deleteBatchPrediction
--
--         , deleteDataSourceTest $
--             deleteDataSource
--
--         , deleteEvaluationTest $
--             deleteEvaluation
--
--         , deleteMLModelTest $
--             deleteMLModel
--
--         , deleteRealtimeEndpointTest $
--             deleteRealtimeEndpoint
--
--         , describeBatchPredictionsTest $
--             describeBatchPredictions
--
--         , describeDataSourcesTest $
--             describeDataSources
--
--         , describeEvaluationsTest $
--             describeEvaluations
--
--         , describeMLModelsTest $
--             describeMLModels
--
--         , getBatchPredictionTest $
--             getBatchPrediction
--
--         , getDataSourceTest $
--             getDataSource
--
--         , getEvaluationTest $
--             getEvaluation
--
--         , getMLModelTest $
--             getMLModel
--
--         , predictTest $
--             predict
--
--         , updateBatchPredictionTest $
--             updateBatchPrediction
--
--         , updateDataSourceTest $
--             updateDataSource
--
--         , updateEvaluationTest $
--             updateEvaluation
--
--         , updateMLModelTest $
--             updateMLModel
--
--           ]

--     , testGroup "response"
--         [ createBatchPredictionResponseTest $
--             createBatchPredictionResponse
--
--         , createDataSourceFromRDSResponseTest $
--             createDataSourceFromRDSResponse
--
--         , createDataSourceFromRedshiftResponseTest $
--             createDataSourceFromRedshiftResponse
--
--         , createDataSourceFromS3ResponseTest $
--             createDataSourceFromSResponse
--
--         , createEvaluationResponseTest $
--             createEvaluationResponse
--
--         , createMLModelResponseTest $
--             createMLModelResponse
--
--         , createRealtimeEndpointResponseTest $
--             createRealtimeEndpointResponse
--
--         , deleteBatchPredictionResponseTest $
--             deleteBatchPredictionResponse
--
--         , deleteDataSourceResponseTest $
--             deleteDataSourceResponse
--
--         , deleteEvaluationResponseTest $
--             deleteEvaluationResponse
--
--         , deleteMLModelResponseTest $
--             deleteMLModelResponse
--
--         , deleteRealtimeEndpointResponseTest $
--             deleteRealtimeEndpointResponse
--
--         , describeBatchPredictionsResponseTest $
--             describeBatchPredictionsResponse
--
--         , describeDataSourcesResponseTest $
--             describeDataSourcesResponse
--
--         , describeEvaluationsResponseTest $
--             describeEvaluationsResponse
--
--         , describeMLModelsResponseTest $
--             describeMLModelsResponse
--
--         , getBatchPredictionResponseTest $
--             getBatchPredictionResponse
--
--         , getDataSourceResponseTest $
--             getDataSourceResponse
--
--         , getEvaluationResponseTest $
--             getEvaluationResponse
--
--         , getMLModelResponseTest $
--             getMLModelResponse
--
--         , predictResponseTest $
--             predictResponse
--
--         , updateBatchPredictionResponseTest $
--             updateBatchPredictionResponse
--
--         , updateDataSourceResponseTest $
--             updateDataSourceResponse
--
--         , updateEvaluationResponseTest $
--             updateEvaluationResponse
--
--         , updateMLModelResponseTest $
--             updateMLModelResponse
--
--           ]
--     ]

-- Requests

createBatchPredictionTest :: CreateBatchPrediction -> TestTree
createBatchPredictionTest = undefined

createDataSourceFromRDSTest :: CreateDataSourceFromRDS -> TestTree
createDataSourceFromRDSTest = undefined

createDataSourceFromRedshiftTest :: CreateDataSourceFromRedshift -> TestTree
createDataSourceFromRedshiftTest = undefined

createDataSourceFromS3Test :: CreateDataSourceFromS -> TestTree
createDataSourceFromS3Test = undefined

createEvaluationTest :: CreateEvaluation -> TestTree
createEvaluationTest = undefined

createMLModelTest :: CreateMLModel -> TestTree
createMLModelTest = undefined

createRealtimeEndpointTest :: CreateRealtimeEndpoint -> TestTree
createRealtimeEndpointTest = undefined

deleteBatchPredictionTest :: DeleteBatchPrediction -> TestTree
deleteBatchPredictionTest = undefined

deleteDataSourceTest :: DeleteDataSource -> TestTree
deleteDataSourceTest = undefined

deleteEvaluationTest :: DeleteEvaluation -> TestTree
deleteEvaluationTest = undefined

deleteMLModelTest :: DeleteMLModel -> TestTree
deleteMLModelTest = undefined

deleteRealtimeEndpointTest :: DeleteRealtimeEndpoint -> TestTree
deleteRealtimeEndpointTest = undefined

describeBatchPredictionsTest :: DescribeBatchPredictions -> TestTree
describeBatchPredictionsTest = undefined

describeDataSourcesTest :: DescribeDataSources -> TestTree
describeDataSourcesTest = undefined

describeEvaluationsTest :: DescribeEvaluations -> TestTree
describeEvaluationsTest = undefined

describeMLModelsTest :: DescribeMLModels -> TestTree
describeMLModelsTest = undefined

getBatchPredictionTest :: GetBatchPrediction -> TestTree
getBatchPredictionTest = undefined

getDataSourceTest :: GetDataSource -> TestTree
getDataSourceTest = undefined

getEvaluationTest :: GetEvaluation -> TestTree
getEvaluationTest = undefined

getMLModelTest :: GetMLModel -> TestTree
getMLModelTest = undefined

predictTest :: Predict -> TestTree
predictTest = undefined

updateBatchPredictionTest :: UpdateBatchPrediction -> TestTree
updateBatchPredictionTest = undefined

updateDataSourceTest :: UpdateDataSource -> TestTree
updateDataSourceTest = undefined

updateEvaluationTest :: UpdateEvaluation -> TestTree
updateEvaluationTest = undefined

updateMLModelTest :: UpdateMLModel -> TestTree
updateMLModelTest = undefined

-- Responses

createBatchPredictionResponseTest :: CreateBatchPredictionResponse -> TestTree
createBatchPredictionResponseTest = resp
    "createBatchPredictionResponse"
    "fixture/CreateBatchPredictionResponse"
    (Proxy :: Proxy CreateBatchPrediction)

createDataSourceFromRDSResponseTest :: CreateDataSourceFromRDSResponse -> TestTree
createDataSourceFromRDSResponseTest = resp
    "createDataSourceFromRDSResponse"
    "fixture/CreateDataSourceFromRDSResponse"
    (Proxy :: Proxy CreateDataSourceFromRDS)

createDataSourceFromRedshiftResponseTest :: CreateDataSourceFromRedshiftResponse -> TestTree
createDataSourceFromRedshiftResponseTest = resp
    "createDataSourceFromRedshiftResponse"
    "fixture/CreateDataSourceFromRedshiftResponse"
    (Proxy :: Proxy CreateDataSourceFromRedshift)

createDataSourceFromS3ResponseTest :: CreateDataSourceFromSResponse -> TestTree
createDataSourceFromS3ResponseTest = resp
    "createDataSourceFromS3Response"
    "fixture/CreateDataSourceFromSResponse"
    (Proxy :: Proxy CreateDataSourceFromS)

createEvaluationResponseTest :: CreateEvaluationResponse -> TestTree
createEvaluationResponseTest = resp
    "createEvaluationResponse"
    "fixture/CreateEvaluationResponse"
    (Proxy :: Proxy CreateEvaluation)

createMLModelResponseTest :: CreateMLModelResponse -> TestTree
createMLModelResponseTest = resp
    "createMLModelResponse"
    "fixture/CreateMLModelResponse"
    (Proxy :: Proxy CreateMLModel)

createRealtimeEndpointResponseTest :: CreateRealtimeEndpointResponse -> TestTree
createRealtimeEndpointResponseTest = resp
    "createRealtimeEndpointResponse"
    "fixture/CreateRealtimeEndpointResponse"
    (Proxy :: Proxy CreateRealtimeEndpoint)

deleteBatchPredictionResponseTest :: DeleteBatchPredictionResponse -> TestTree
deleteBatchPredictionResponseTest = resp
    "deleteBatchPredictionResponse"
    "fixture/DeleteBatchPredictionResponse"
    (Proxy :: Proxy DeleteBatchPrediction)

deleteDataSourceResponseTest :: DeleteDataSourceResponse -> TestTree
deleteDataSourceResponseTest = resp
    "deleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse"
    (Proxy :: Proxy DeleteDataSource)

deleteEvaluationResponseTest :: DeleteEvaluationResponse -> TestTree
deleteEvaluationResponseTest = resp
    "deleteEvaluationResponse"
    "fixture/DeleteEvaluationResponse"
    (Proxy :: Proxy DeleteEvaluation)

deleteMLModelResponseTest :: DeleteMLModelResponse -> TestTree
deleteMLModelResponseTest = resp
    "deleteMLModelResponse"
    "fixture/DeleteMLModelResponse"
    (Proxy :: Proxy DeleteMLModel)

deleteRealtimeEndpointResponseTest :: DeleteRealtimeEndpointResponse -> TestTree
deleteRealtimeEndpointResponseTest = resp
    "deleteRealtimeEndpointResponse"
    "fixture/DeleteRealtimeEndpointResponse"
    (Proxy :: Proxy DeleteRealtimeEndpoint)

describeBatchPredictionsResponseTest :: DescribeBatchPredictionsResponse -> TestTree
describeBatchPredictionsResponseTest = resp
    "describeBatchPredictionsResponse"
    "fixture/DescribeBatchPredictionsResponse"
    (Proxy :: Proxy DescribeBatchPredictions)

describeDataSourcesResponseTest :: DescribeDataSourcesResponse -> TestTree
describeDataSourcesResponseTest = resp
    "describeDataSourcesResponse"
    "fixture/DescribeDataSourcesResponse"
    (Proxy :: Proxy DescribeDataSources)

describeEvaluationsResponseTest :: DescribeEvaluationsResponse -> TestTree
describeEvaluationsResponseTest = resp
    "describeEvaluationsResponse"
    "fixture/DescribeEvaluationsResponse"
    (Proxy :: Proxy DescribeEvaluations)

describeMLModelsResponseTest :: DescribeMLModelsResponse -> TestTree
describeMLModelsResponseTest = resp
    "describeMLModelsResponse"
    "fixture/DescribeMLModelsResponse"
    (Proxy :: Proxy DescribeMLModels)

getBatchPredictionResponseTest :: GetBatchPredictionResponse -> TestTree
getBatchPredictionResponseTest = resp
    "getBatchPredictionResponse"
    "fixture/GetBatchPredictionResponse"
    (Proxy :: Proxy GetBatchPrediction)

getDataSourceResponseTest :: GetDataSourceResponse -> TestTree
getDataSourceResponseTest = resp
    "getDataSourceResponse"
    "fixture/GetDataSourceResponse"
    (Proxy :: Proxy GetDataSource)

getEvaluationResponseTest :: GetEvaluationResponse -> TestTree
getEvaluationResponseTest = resp
    "getEvaluationResponse"
    "fixture/GetEvaluationResponse"
    (Proxy :: Proxy GetEvaluation)

getMLModelResponseTest :: GetMLModelResponse -> TestTree
getMLModelResponseTest = resp
    "getMLModelResponse"
    "fixture/GetMLModelResponse"
    (Proxy :: Proxy GetMLModel)

predictResponseTest :: PredictResponse -> TestTree
predictResponseTest = resp
    "predictResponse"
    "fixture/PredictResponse"
    (Proxy :: Proxy Predict)

updateBatchPredictionResponseTest :: UpdateBatchPredictionResponse -> TestTree
updateBatchPredictionResponseTest = resp
    "updateBatchPredictionResponse"
    "fixture/UpdateBatchPredictionResponse"
    (Proxy :: Proxy UpdateBatchPrediction)

updateDataSourceResponseTest :: UpdateDataSourceResponse -> TestTree
updateDataSourceResponseTest = resp
    "updateDataSourceResponse"
    "fixture/UpdateDataSourceResponse"
    (Proxy :: Proxy UpdateDataSource)

updateEvaluationResponseTest :: UpdateEvaluationResponse -> TestTree
updateEvaluationResponseTest = resp
    "updateEvaluationResponse"
    "fixture/UpdateEvaluationResponse"
    (Proxy :: Proxy UpdateEvaluation)

updateMLModelResponseTest :: UpdateMLModelResponse -> TestTree
updateMLModelResponseTest = resp
    "updateMLModelResponse"
    "fixture/UpdateMLModelResponse"
    (Proxy :: Proxy UpdateMLModel)
