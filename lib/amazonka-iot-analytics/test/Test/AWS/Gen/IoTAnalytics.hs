{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTAnalytics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--             mkDescribePipeline
--
--         , requestDescribeDataset $
--             mkDescribeDataset
--
--         , requestListChannels $
--             mkListChannels
--
--         , requestListDatasetContents $
--             mkListDatasetContents
--
--         , requestPutLoggingOptions $
--             mkPutLoggingOptions
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestDeleteChannel $
--             mkDeleteChannel
--
--         , requestUpdateChannel $
--             mkUpdateChannel
--
--         , requestSampleChannelData $
--             mkSampleChannelData
--
--         , requestCancelPipelineReprocessing $
--             mkCancelPipelineReprocessing
--
--         , requestCreateDatastore $
--             mkCreateDatastore
--
--         , requestUpdatePipeline $
--             mkUpdatePipeline
--
--         , requestDeletePipeline $
--             mkDeletePipeline
--
--         , requestDeleteDataset $
--             mkDeleteDataset
--
--         , requestUpdateDataset $
--             mkUpdateDataset
--
--         , requestListPipelines $
--             mkListPipelines
--
--         , requestDeleteDatastore $
--             mkDeleteDatastore
--
--         , requestUpdateDatastore $
--             mkUpdateDatastore
--
--         , requestCreateDataset $
--             mkCreateDataset
--
--         , requestBatchPutMessage $
--             mkBatchPutMessage
--
--         , requestListDatastores $
--             mkListDatastores
--
--         , requestCreateDatasetContent $
--             mkCreateDatasetContent
--
--         , requestCreateChannel $
--             mkCreateChannel
--
--         , requestDeleteDatasetContent $
--             mkDeleteDatasetContent
--
--         , requestDescribeDatastore $
--             mkDescribeDatastore
--
--         , requestGetDatasetContent $
--             mkGetDatasetContent
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestListDatasets $
--             mkListDatasets
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestRunPipelineActivity $
--             mkRunPipelineActivity
--
--         , requestDescribeChannel $
--             mkDescribeChannel
--
--         , requestCreatePipeline $
--             mkCreatePipeline
--
--         , requestStartPipelineReprocessing $
--             mkStartPipelineReprocessing
--
--         , requestDescribeLoggingOptions $
--             mkDescribeLoggingOptions
--
--           ]

--     , testGroup "response"
--         [ responseDescribePipeline $
--             mkDescribePipelineResponse
--
--         , responseDescribeDataset $
--             mkDescribeDatasetResponse
--
--         , responseListChannels $
--             mkListChannelsResponse
--
--         , responseListDatasetContents $
--             mkListDatasetContentsResponse
--
--         , responsePutLoggingOptions $
--             mkPutLoggingOptionsResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseDeleteChannel $
--             mkDeleteChannelResponse
--
--         , responseUpdateChannel $
--             mkUpdateChannelResponse
--
--         , responseSampleChannelData $
--             mkSampleChannelDataResponse
--
--         , responseCancelPipelineReprocessing $
--             mkCancelPipelineReprocessingResponse
--
--         , responseCreateDatastore $
--             mkCreateDatastoreResponse
--
--         , responseUpdatePipeline $
--             mkUpdatePipelineResponse
--
--         , responseDeletePipeline $
--             mkDeletePipelineResponse
--
--         , responseDeleteDataset $
--             mkDeleteDatasetResponse
--
--         , responseUpdateDataset $
--             mkUpdateDatasetResponse
--
--         , responseListPipelines $
--             mkListPipelinesResponse
--
--         , responseDeleteDatastore $
--             mkDeleteDatastoreResponse
--
--         , responseUpdateDatastore $
--             mkUpdateDatastoreResponse
--
--         , responseCreateDataset $
--             mkCreateDatasetResponse
--
--         , responseBatchPutMessage $
--             mkBatchPutMessageResponse
--
--         , responseListDatastores $
--             mkListDatastoresResponse
--
--         , responseCreateDatasetContent $
--             mkCreateDatasetContentResponse
--
--         , responseCreateChannel $
--             mkCreateChannelResponse
--
--         , responseDeleteDatasetContent $
--             mkDeleteDatasetContentResponse
--
--         , responseDescribeDatastore $
--             mkDescribeDatastoreResponse
--
--         , responseGetDatasetContent $
--             mkGetDatasetContentResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseListDatasets $
--             mkListDatasetsResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseRunPipelineActivity $
--             mkRunPipelineActivityResponse
--
--         , responseDescribeChannel $
--             mkDescribeChannelResponse
--
--         , responseCreatePipeline $
--             mkCreatePipelineResponse
--
--         , responseStartPipelineReprocessing $
--             mkStartPipelineReprocessingResponse
--
--         , responseDescribeLoggingOptions $
--             mkDescribeLoggingOptionsResponse
--
--           ]
--     ]

-- Requests

requestDescribePipeline :: DescribePipeline -> TestTree
requestDescribePipeline =
  req
    "DescribePipeline"
    "fixture/DescribePipeline.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestListDatasetContents :: ListDatasetContents -> TestTree
requestListDatasetContents =
  req
    "ListDatasetContents"
    "fixture/ListDatasetContents.yaml"

requestPutLoggingOptions :: PutLoggingOptions -> TestTree
requestPutLoggingOptions =
  req
    "PutLoggingOptions"
    "fixture/PutLoggingOptions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel =
  req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestUpdateChannel :: UpdateChannel -> TestTree
requestUpdateChannel =
  req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestSampleChannelData :: SampleChannelData -> TestTree
requestSampleChannelData =
  req
    "SampleChannelData"
    "fixture/SampleChannelData.yaml"

requestCancelPipelineReprocessing :: CancelPipelineReprocessing -> TestTree
requestCancelPipelineReprocessing =
  req
    "CancelPipelineReprocessing"
    "fixture/CancelPipelineReprocessing.yaml"

requestCreateDatastore :: CreateDatastore -> TestTree
requestCreateDatastore =
  req
    "CreateDatastore"
    "fixture/CreateDatastore.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline =
  req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestUpdateDataset :: UpdateDataset -> TestTree
requestUpdateDataset =
  req
    "UpdateDataset"
    "fixture/UpdateDataset.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestDeleteDatastore :: DeleteDatastore -> TestTree
requestDeleteDatastore =
  req
    "DeleteDatastore"
    "fixture/DeleteDatastore.yaml"

requestUpdateDatastore :: UpdateDatastore -> TestTree
requestUpdateDatastore =
  req
    "UpdateDatastore"
    "fixture/UpdateDatastore.yaml"

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset =
  req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestBatchPutMessage :: BatchPutMessage -> TestTree
requestBatchPutMessage =
  req
    "BatchPutMessage"
    "fixture/BatchPutMessage.yaml"

requestListDatastores :: ListDatastores -> TestTree
requestListDatastores =
  req
    "ListDatastores"
    "fixture/ListDatastores.yaml"

requestCreateDatasetContent :: CreateDatasetContent -> TestTree
requestCreateDatasetContent =
  req
    "CreateDatasetContent"
    "fixture/CreateDatasetContent.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestDeleteDatasetContent :: DeleteDatasetContent -> TestTree
requestDeleteDatasetContent =
  req
    "DeleteDatasetContent"
    "fixture/DeleteDatasetContent.yaml"

requestDescribeDatastore :: DescribeDatastore -> TestTree
requestDescribeDatastore =
  req
    "DescribeDatastore"
    "fixture/DescribeDatastore.yaml"

requestGetDatasetContent :: GetDatasetContent -> TestTree
requestGetDatasetContent =
  req
    "GetDatasetContent"
    "fixture/GetDatasetContent.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestRunPipelineActivity :: RunPipelineActivity -> TestTree
requestRunPipelineActivity =
  req
    "RunPipelineActivity"
    "fixture/RunPipelineActivity.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel =
  req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestStartPipelineReprocessing :: StartPipelineReprocessing -> TestTree
requestStartPipelineReprocessing =
  req
    "StartPipelineReprocessing"
    "fixture/StartPipelineReprocessing.yaml"

requestDescribeLoggingOptions :: DescribeLoggingOptions -> TestTree
requestDescribeLoggingOptions =
  req
    "DescribeLoggingOptions"
    "fixture/DescribeLoggingOptions.yaml"

-- Responses

responseDescribePipeline :: DescribePipelineResponse -> TestTree
responseDescribePipeline =
  res
    "DescribePipelineResponse"
    "fixture/DescribePipelineResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribePipeline)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDataset)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListChannels)

responseListDatasetContents :: ListDatasetContentsResponse -> TestTree
responseListDatasetContents =
  res
    "ListDatasetContentsResponse"
    "fixture/ListDatasetContentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDatasetContents)

responsePutLoggingOptions :: PutLoggingOptionsResponse -> TestTree
responsePutLoggingOptions =
  res
    "PutLoggingOptionsResponse"
    "fixture/PutLoggingOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutLoggingOptions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteChannel)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateChannel)

responseSampleChannelData :: SampleChannelDataResponse -> TestTree
responseSampleChannelData =
  res
    "SampleChannelDataResponse"
    "fixture/SampleChannelDataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SampleChannelData)

responseCancelPipelineReprocessing :: CancelPipelineReprocessingResponse -> TestTree
responseCancelPipelineReprocessing =
  res
    "CancelPipelineReprocessingResponse"
    "fixture/CancelPipelineReprocessingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelPipelineReprocessing)

responseCreateDatastore :: CreateDatastoreResponse -> TestTree
responseCreateDatastore =
  res
    "CreateDatastoreResponse"
    "fixture/CreateDatastoreResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDatastore)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdatePipeline)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePipeline)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDataset)

responseUpdateDataset :: UpdateDatasetResponse -> TestTree
responseUpdateDataset =
  res
    "UpdateDatasetResponse"
    "fixture/UpdateDatasetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDataset)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPipelines)

responseDeleteDatastore :: DeleteDatastoreResponse -> TestTree
responseDeleteDatastore =
  res
    "DeleteDatastoreResponse"
    "fixture/DeleteDatastoreResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDatastore)

responseUpdateDatastore :: UpdateDatastoreResponse -> TestTree
responseUpdateDatastore =
  res
    "UpdateDatastoreResponse"
    "fixture/UpdateDatastoreResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDatastore)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDataset)

responseBatchPutMessage :: BatchPutMessageResponse -> TestTree
responseBatchPutMessage =
  res
    "BatchPutMessageResponse"
    "fixture/BatchPutMessageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchPutMessage)

responseListDatastores :: ListDatastoresResponse -> TestTree
responseListDatastores =
  res
    "ListDatastoresResponse"
    "fixture/ListDatastoresResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDatastores)

responseCreateDatasetContent :: CreateDatasetContentResponse -> TestTree
responseCreateDatasetContent =
  res
    "CreateDatasetContentResponse"
    "fixture/CreateDatasetContentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDatasetContent)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateChannel)

responseDeleteDatasetContent :: DeleteDatasetContentResponse -> TestTree
responseDeleteDatasetContent =
  res
    "DeleteDatasetContentResponse"
    "fixture/DeleteDatasetContentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDatasetContent)

responseDescribeDatastore :: DescribeDatastoreResponse -> TestTree
responseDescribeDatastore =
  res
    "DescribeDatastoreResponse"
    "fixture/DescribeDatastoreResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDatastore)

responseGetDatasetContent :: GetDatasetContentResponse -> TestTree
responseGetDatasetContent =
  res
    "GetDatasetContentResponse"
    "fixture/GetDatasetContentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDatasetContent)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDatasets)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseRunPipelineActivity :: RunPipelineActivityResponse -> TestTree
responseRunPipelineActivity =
  res
    "RunPipelineActivityResponse"
    "fixture/RunPipelineActivityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RunPipelineActivity)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeChannel)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePipeline)

responseStartPipelineReprocessing :: StartPipelineReprocessingResponse -> TestTree
responseStartPipelineReprocessing =
  res
    "StartPipelineReprocessingResponse"
    "fixture/StartPipelineReprocessingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartPipelineReprocessing)

responseDescribeLoggingOptions :: DescribeLoggingOptionsResponse -> TestTree
responseDescribeLoggingOptions =
  res
    "DescribeLoggingOptionsResponse"
    "fixture/DescribeLoggingOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLoggingOptions)
