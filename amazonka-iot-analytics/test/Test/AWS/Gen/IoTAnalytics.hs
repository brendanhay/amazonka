{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTAnalytics
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.IoTAnalytics where

import Data.Proxy
import Network.AWS.IoTAnalytics
import Test.AWS.Fixture
import Test.AWS.IoTAnalytics.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribePipeline $
--             describePipeline
--
--         , requestDescribeDataset $
--             describeDataset
--
--         , requestListChannels $
--             listChannels
--
--         , requestPutLoggingOptions $
--             putLoggingOptions
--
--         , requestDeleteChannel $
--             deleteChannel
--
--         , requestUpdateChannel $
--             updateChannel
--
--         , requestSampleChannelData $
--             sampleChannelData
--
--         , requestCancelPipelineReprocessing $
--             cancelPipelineReprocessing
--
--         , requestCreateDatastore $
--             createDatastore
--
--         , requestUpdatePipeline $
--             updatePipeline
--
--         , requestDeletePipeline $
--             deletePipeline
--
--         , requestDeleteDataset $
--             deleteDataset
--
--         , requestUpdateDataset $
--             updateDataset
--
--         , requestListPipelines $
--             listPipelines
--
--         , requestDeleteDatastore $
--             deleteDatastore
--
--         , requestUpdateDatastore $
--             updateDatastore
--
--         , requestCreateDataset $
--             createDataset
--
--         , requestBatchPutMessage $
--             batchPutMessage
--
--         , requestListDatastores $
--             listDatastores
--
--         , requestCreateDatasetContent $
--             createDatasetContent
--
--         , requestCreateChannel $
--             createChannel
--
--         , requestDeleteDatasetContent $
--             deleteDatasetContent
--
--         , requestDescribeDatastore $
--             describeDatastore
--
--         , requestGetDatasetContent $
--             getDatasetContent
--
--         , requestListDatasets $
--             listDatasets
--
--         , requestRunPipelineActivity $
--             runPipelineActivity
--
--         , requestDescribeChannel $
--             describeChannel
--
--         , requestCreatePipeline $
--             createPipeline
--
--         , requestStartPipelineReprocessing $
--             startPipelineReprocessing
--
--         , requestDescribeLoggingOptions $
--             describeLoggingOptions
--
--           ]

--     , testGroup "response"
--         [ responseDescribePipeline $
--             describePipelineResponse
--
--         , responseDescribeDataset $
--             describeDatasetResponse
--
--         , responseListChannels $
--             listChannelsResponse
--
--         , responsePutLoggingOptions $
--             putLoggingOptionsResponse
--
--         , responseDeleteChannel $
--             deleteChannelResponse
--
--         , responseUpdateChannel $
--             updateChannelResponse
--
--         , responseSampleChannelData $
--             sampleChannelDataResponse
--
--         , responseCancelPipelineReprocessing $
--             cancelPipelineReprocessingResponse
--
--         , responseCreateDatastore $
--             createDatastoreResponse
--
--         , responseUpdatePipeline $
--             updatePipelineResponse
--
--         , responseDeletePipeline $
--             deletePipelineResponse
--
--         , responseDeleteDataset $
--             deleteDatasetResponse
--
--         , responseUpdateDataset $
--             updateDatasetResponse
--
--         , responseListPipelines $
--             listPipelinesResponse
--
--         , responseDeleteDatastore $
--             deleteDatastoreResponse
--
--         , responseUpdateDatastore $
--             updateDatastoreResponse
--
--         , responseCreateDataset $
--             createDatasetResponse
--
--         , responseBatchPutMessage $
--             batchPutMessageResponse
--
--         , responseListDatastores $
--             listDatastoresResponse
--
--         , responseCreateDatasetContent $
--             createDatasetContentResponse
--
--         , responseCreateChannel $
--             createChannelResponse
--
--         , responseDeleteDatasetContent $
--             deleteDatasetContentResponse
--
--         , responseDescribeDatastore $
--             describeDatastoreResponse
--
--         , responseGetDatasetContent $
--             getDatasetContentResponse
--
--         , responseListDatasets $
--             listDatasetsResponse
--
--         , responseRunPipelineActivity $
--             runPipelineActivityResponse
--
--         , responseDescribeChannel $
--             describeChannelResponse
--
--         , responseCreatePipeline $
--             createPipelineResponse
--
--         , responseStartPipelineReprocessing $
--             startPipelineReprocessingResponse
--
--         , responseDescribeLoggingOptions $
--             describeLoggingOptionsResponse
--
--           ]
--     ]

-- Requests

requestDescribePipeline :: DescribePipeline -> TestTree
requestDescribePipeline = req
    "DescribePipeline"
    "fixture/DescribePipeline.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset = req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels = req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestPutLoggingOptions :: PutLoggingOptions -> TestTree
requestPutLoggingOptions = req
    "PutLoggingOptions"
    "fixture/PutLoggingOptions.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel = req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestUpdateChannel :: UpdateChannel -> TestTree
requestUpdateChannel = req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestSampleChannelData :: SampleChannelData -> TestTree
requestSampleChannelData = req
    "SampleChannelData"
    "fixture/SampleChannelData.yaml"

requestCancelPipelineReprocessing :: CancelPipelineReprocessing -> TestTree
requestCancelPipelineReprocessing = req
    "CancelPipelineReprocessing"
    "fixture/CancelPipelineReprocessing.yaml"

requestCreateDatastore :: CreateDatastore -> TestTree
requestCreateDatastore = req
    "CreateDatastore"
    "fixture/CreateDatastore.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline = req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline = req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset = req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestUpdateDataset :: UpdateDataset -> TestTree
requestUpdateDataset = req
    "UpdateDataset"
    "fixture/UpdateDataset.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines = req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestDeleteDatastore :: DeleteDatastore -> TestTree
requestDeleteDatastore = req
    "DeleteDatastore"
    "fixture/DeleteDatastore.yaml"

requestUpdateDatastore :: UpdateDatastore -> TestTree
requestUpdateDatastore = req
    "UpdateDatastore"
    "fixture/UpdateDatastore.yaml"

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset = req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestBatchPutMessage :: BatchPutMessage -> TestTree
requestBatchPutMessage = req
    "BatchPutMessage"
    "fixture/BatchPutMessage.yaml"

requestListDatastores :: ListDatastores -> TestTree
requestListDatastores = req
    "ListDatastores"
    "fixture/ListDatastores.yaml"

requestCreateDatasetContent :: CreateDatasetContent -> TestTree
requestCreateDatasetContent = req
    "CreateDatasetContent"
    "fixture/CreateDatasetContent.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel = req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestDeleteDatasetContent :: DeleteDatasetContent -> TestTree
requestDeleteDatasetContent = req
    "DeleteDatasetContent"
    "fixture/DeleteDatasetContent.yaml"

requestDescribeDatastore :: DescribeDatastore -> TestTree
requestDescribeDatastore = req
    "DescribeDatastore"
    "fixture/DescribeDatastore.yaml"

requestGetDatasetContent :: GetDatasetContent -> TestTree
requestGetDatasetContent = req
    "GetDatasetContent"
    "fixture/GetDatasetContent.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets = req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestRunPipelineActivity :: RunPipelineActivity -> TestTree
requestRunPipelineActivity = req
    "RunPipelineActivity"
    "fixture/RunPipelineActivity.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel = req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline = req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestStartPipelineReprocessing :: StartPipelineReprocessing -> TestTree
requestStartPipelineReprocessing = req
    "StartPipelineReprocessing"
    "fixture/StartPipelineReprocessing.yaml"

requestDescribeLoggingOptions :: DescribeLoggingOptions -> TestTree
requestDescribeLoggingOptions = req
    "DescribeLoggingOptions"
    "fixture/DescribeLoggingOptions.yaml"

-- Responses

responseDescribePipeline :: DescribePipelineResponse -> TestTree
responseDescribePipeline = res
    "DescribePipelineResponse"
    "fixture/DescribePipelineResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy DescribePipeline)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset = res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy DescribeDataset)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels = res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy ListChannels)

responsePutLoggingOptions :: PutLoggingOptionsResponse -> TestTree
responsePutLoggingOptions = res
    "PutLoggingOptionsResponse"
    "fixture/PutLoggingOptionsResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy PutLoggingOptions)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel = res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy DeleteChannel)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel = res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy UpdateChannel)

responseSampleChannelData :: SampleChannelDataResponse -> TestTree
responseSampleChannelData = res
    "SampleChannelDataResponse"
    "fixture/SampleChannelDataResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy SampleChannelData)

responseCancelPipelineReprocessing :: CancelPipelineReprocessingResponse -> TestTree
responseCancelPipelineReprocessing = res
    "CancelPipelineReprocessingResponse"
    "fixture/CancelPipelineReprocessingResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy CancelPipelineReprocessing)

responseCreateDatastore :: CreateDatastoreResponse -> TestTree
responseCreateDatastore = res
    "CreateDatastoreResponse"
    "fixture/CreateDatastoreResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy CreateDatastore)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline = res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy UpdatePipeline)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline = res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy DeletePipeline)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset = res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy DeleteDataset)

responseUpdateDataset :: UpdateDatasetResponse -> TestTree
responseUpdateDataset = res
    "UpdateDatasetResponse"
    "fixture/UpdateDatasetResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy UpdateDataset)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines = res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy ListPipelines)

responseDeleteDatastore :: DeleteDatastoreResponse -> TestTree
responseDeleteDatastore = res
    "DeleteDatastoreResponse"
    "fixture/DeleteDatastoreResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy DeleteDatastore)

responseUpdateDatastore :: UpdateDatastoreResponse -> TestTree
responseUpdateDatastore = res
    "UpdateDatastoreResponse"
    "fixture/UpdateDatastoreResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy UpdateDatastore)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset = res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy CreateDataset)

responseBatchPutMessage :: BatchPutMessageResponse -> TestTree
responseBatchPutMessage = res
    "BatchPutMessageResponse"
    "fixture/BatchPutMessageResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy BatchPutMessage)

responseListDatastores :: ListDatastoresResponse -> TestTree
responseListDatastores = res
    "ListDatastoresResponse"
    "fixture/ListDatastoresResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy ListDatastores)

responseCreateDatasetContent :: CreateDatasetContentResponse -> TestTree
responseCreateDatasetContent = res
    "CreateDatasetContentResponse"
    "fixture/CreateDatasetContentResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy CreateDatasetContent)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel = res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy CreateChannel)

responseDeleteDatasetContent :: DeleteDatasetContentResponse -> TestTree
responseDeleteDatasetContent = res
    "DeleteDatasetContentResponse"
    "fixture/DeleteDatasetContentResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy DeleteDatasetContent)

responseDescribeDatastore :: DescribeDatastoreResponse -> TestTree
responseDescribeDatastore = res
    "DescribeDatastoreResponse"
    "fixture/DescribeDatastoreResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy DescribeDatastore)

responseGetDatasetContent :: GetDatasetContentResponse -> TestTree
responseGetDatasetContent = res
    "GetDatasetContentResponse"
    "fixture/GetDatasetContentResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy GetDatasetContent)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets = res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy ListDatasets)

responseRunPipelineActivity :: RunPipelineActivityResponse -> TestTree
responseRunPipelineActivity = res
    "RunPipelineActivityResponse"
    "fixture/RunPipelineActivityResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy RunPipelineActivity)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel = res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy DescribeChannel)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline = res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy CreatePipeline)

responseStartPipelineReprocessing :: StartPipelineReprocessingResponse -> TestTree
responseStartPipelineReprocessing = res
    "StartPipelineReprocessingResponse"
    "fixture/StartPipelineReprocessingResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy StartPipelineReprocessing)

responseDescribeLoggingOptions :: DescribeLoggingOptionsResponse -> TestTree
responseDescribeLoggingOptions = res
    "DescribeLoggingOptionsResponse"
    "fixture/DescribeLoggingOptionsResponse.proto"
    ioTAnalytics
    (Proxy :: Proxy DescribeLoggingOptions)
