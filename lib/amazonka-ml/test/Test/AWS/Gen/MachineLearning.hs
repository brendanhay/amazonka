{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MachineLearning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestUpdateDataSource $
--             mkUpdateDataSource
--
--         , requestDeleteDataSource $
--             mkDeleteDataSource
--
--         , requestDescribeTags $
--             mkDescribeTags
--
--         , requestCreateDataSourceFromRedshift $
--             mkCreateDataSourceFromRedshift
--
--         , requestCreateDataSourceFromS3 $
--             mkCreateDataSourceFromS3
--
--         , requestCreateMLModel $
--             mkCreateMLModel
--
--         , requestDeleteTags $
--             mkDeleteTags
--
--         , requestDeleteBatchPrediction $
--             mkDeleteBatchPrediction
--
--         , requestUpdateBatchPrediction $
--             mkUpdateBatchPrediction
--
--         , requestGetMLModel $
--             mkGetMLModel
--
--         , requestGetDataSource $
--             mkGetDataSource
--
--         , requestUpdateEvaluation $
--             mkUpdateEvaluation
--
--         , requestDeleteEvaluation $
--             mkDeleteEvaluation
--
--         , requestDeleteMLModel $
--             mkDeleteMLModel
--
--         , requestUpdateMLModel $
--             mkUpdateMLModel
--
--         , requestGetBatchPrediction $
--             mkGetBatchPrediction
--
--         , requestDescribeBatchPredictions $
--             mkDescribeBatchPredictions
--
--         , requestCreateDataSourceFromRDS $
--             mkCreateDataSourceFromRDS
--
--         , requestCreateEvaluation $
--             mkCreateEvaluation
--
--         , requestPredict $
--             mkPredict
--
--         , requestDeleteRealtimeEndpoint $
--             mkDeleteRealtimeEndpoint
--
--         , requestCreateBatchPrediction $
--             mkCreateBatchPrediction
--
--         , requestGetEvaluation $
--             mkGetEvaluation
--
--         , requestDescribeEvaluations $
--             mkDescribeEvaluations
--
--         , requestCreateRealtimeEndpoint $
--             mkCreateRealtimeEndpoint
--
--         , requestAddTags $
--             mkAddTags
--
--         , requestDescribeMLModels $
--             mkDescribeMLModels
--
--         , requestDescribeDataSources $
--             mkDescribeDataSources
--
--           ]

--     , testGroup "response"
--         [ responseUpdateDataSource $
--             mkUpdateDataSourceResponse
--
--         , responseDeleteDataSource $
--             mkDeleteDataSourceResponse
--
--         , responseDescribeTags $
--             mkDescribeTagsResponse
--
--         , responseCreateDataSourceFromRedshift $
--             mkCreateDataSourceFromRedshiftResponse
--
--         , responseCreateDataSourceFromS3 $
--             mkCreateDataSourceFromS3Response
--
--         , responseCreateMLModel $
--             mkCreateMLModelResponse
--
--         , responseDeleteTags $
--             mkDeleteTagsResponse
--
--         , responseDeleteBatchPrediction $
--             mkDeleteBatchPredictionResponse
--
--         , responseUpdateBatchPrediction $
--             mkUpdateBatchPredictionResponse
--
--         , responseGetMLModel $
--             mkGetMLModelResponse
--
--         , responseGetDataSource $
--             mkGetDataSourceResponse
--
--         , responseUpdateEvaluation $
--             mkUpdateEvaluationResponse
--
--         , responseDeleteEvaluation $
--             mkDeleteEvaluationResponse
--
--         , responseDeleteMLModel $
--             mkDeleteMLModelResponse
--
--         , responseUpdateMLModel $
--             mkUpdateMLModelResponse
--
--         , responseGetBatchPrediction $
--             mkGetBatchPredictionResponse
--
--         , responseDescribeBatchPredictions $
--             mkDescribeBatchPredictionsResponse
--
--         , responseCreateDataSourceFromRDS $
--             mkCreateDataSourceFromRDSResponse
--
--         , responseCreateEvaluation $
--             mkCreateEvaluationResponse
--
--         , responsePredict $
--             mkPredictResponse
--
--         , responseDeleteRealtimeEndpoint $
--             mkDeleteRealtimeEndpointResponse
--
--         , responseCreateBatchPrediction $
--             mkCreateBatchPredictionResponse
--
--         , responseGetEvaluation $
--             mkGetEvaluationResponse
--
--         , responseDescribeEvaluations $
--             mkDescribeEvaluationsResponse
--
--         , responseCreateRealtimeEndpoint $
--             mkCreateRealtimeEndpointResponse
--
--         , responseAddTags $
--             mkAddTagsResponse
--
--         , responseDescribeMLModels $
--             mkDescribeMLModelsResponse
--
--         , responseDescribeDataSources $
--             mkDescribeDataSourcesResponse
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
    mkServiceConfig
    (Proxy :: Proxy UpdateDataSource)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource = res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDataSource)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTags)

responseCreateDataSourceFromRedshift :: CreateDataSourceFromRedshiftResponse -> TestTree
responseCreateDataSourceFromRedshift = res
    "CreateDataSourceFromRedshiftResponse"
    "fixture/CreateDataSourceFromRedshiftResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDataSourceFromRedshift)

responseCreateDataSourceFromS3 :: CreateDataSourceFromS3Response -> TestTree
responseCreateDataSourceFromS3 = res
    "CreateDataSourceFromS3Response"
    "fixture/CreateDataSourceFromS3Response.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDataSourceFromS3)

responseCreateMLModel :: CreateMLModelResponse -> TestTree
responseCreateMLModel = res
    "CreateMLModelResponse"
    "fixture/CreateMLModelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateMLModel)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTags)

responseDeleteBatchPrediction :: DeleteBatchPredictionResponse -> TestTree
responseDeleteBatchPrediction = res
    "DeleteBatchPredictionResponse"
    "fixture/DeleteBatchPredictionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBatchPrediction)

responseUpdateBatchPrediction :: UpdateBatchPredictionResponse -> TestTree
responseUpdateBatchPrediction = res
    "UpdateBatchPredictionResponse"
    "fixture/UpdateBatchPredictionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateBatchPrediction)

responseGetMLModel :: GetMLModelResponse -> TestTree
responseGetMLModel = res
    "GetMLModelResponse"
    "fixture/GetMLModelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetMLModel)

responseGetDataSource :: GetDataSourceResponse -> TestTree
responseGetDataSource = res
    "GetDataSourceResponse"
    "fixture/GetDataSourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDataSource)

responseUpdateEvaluation :: UpdateEvaluationResponse -> TestTree
responseUpdateEvaluation = res
    "UpdateEvaluationResponse"
    "fixture/UpdateEvaluationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateEvaluation)

responseDeleteEvaluation :: DeleteEvaluationResponse -> TestTree
responseDeleteEvaluation = res
    "DeleteEvaluationResponse"
    "fixture/DeleteEvaluationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteEvaluation)

responseDeleteMLModel :: DeleteMLModelResponse -> TestTree
responseDeleteMLModel = res
    "DeleteMLModelResponse"
    "fixture/DeleteMLModelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteMLModel)

responseUpdateMLModel :: UpdateMLModelResponse -> TestTree
responseUpdateMLModel = res
    "UpdateMLModelResponse"
    "fixture/UpdateMLModelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateMLModel)

responseGetBatchPrediction :: GetBatchPredictionResponse -> TestTree
responseGetBatchPrediction = res
    "GetBatchPredictionResponse"
    "fixture/GetBatchPredictionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBatchPrediction)

responseDescribeBatchPredictions :: DescribeBatchPredictionsResponse -> TestTree
responseDescribeBatchPredictions = res
    "DescribeBatchPredictionsResponse"
    "fixture/DescribeBatchPredictionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeBatchPredictions)

responseCreateDataSourceFromRDS :: CreateDataSourceFromRDSResponse -> TestTree
responseCreateDataSourceFromRDS = res
    "CreateDataSourceFromRDSResponse"
    "fixture/CreateDataSourceFromRDSResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDataSourceFromRDS)

responseCreateEvaluation :: CreateEvaluationResponse -> TestTree
responseCreateEvaluation = res
    "CreateEvaluationResponse"
    "fixture/CreateEvaluationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateEvaluation)

responsePredict :: PredictResponse -> TestTree
responsePredict = res
    "PredictResponse"
    "fixture/PredictResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Predict)

responseDeleteRealtimeEndpoint :: DeleteRealtimeEndpointResponse -> TestTree
responseDeleteRealtimeEndpoint = res
    "DeleteRealtimeEndpointResponse"
    "fixture/DeleteRealtimeEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRealtimeEndpoint)

responseCreateBatchPrediction :: CreateBatchPredictionResponse -> TestTree
responseCreateBatchPrediction = res
    "CreateBatchPredictionResponse"
    "fixture/CreateBatchPredictionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateBatchPrediction)

responseGetEvaluation :: GetEvaluationResponse -> TestTree
responseGetEvaluation = res
    "GetEvaluationResponse"
    "fixture/GetEvaluationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetEvaluation)

responseDescribeEvaluations :: DescribeEvaluationsResponse -> TestTree
responseDescribeEvaluations = res
    "DescribeEvaluationsResponse"
    "fixture/DescribeEvaluationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEvaluations)

responseCreateRealtimeEndpoint :: CreateRealtimeEndpointResponse -> TestTree
responseCreateRealtimeEndpoint = res
    "CreateRealtimeEndpointResponse"
    "fixture/CreateRealtimeEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRealtimeEndpoint)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddTags)

responseDescribeMLModels :: DescribeMLModelsResponse -> TestTree
responseDescribeMLModels = res
    "DescribeMLModelsResponse"
    "fixture/DescribeMLModelsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMLModels)

responseDescribeDataSources :: DescribeDataSourcesResponse -> TestTree
responseDescribeDataSources = res
    "DescribeDataSourcesResponse"
    "fixture/DescribeDataSourcesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDataSources)
