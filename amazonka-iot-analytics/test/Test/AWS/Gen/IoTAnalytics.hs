{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTAnalytics
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestCreateChannel $
--             newCreateChannel
--
--         , requestDescribePipeline $
--             newDescribePipeline
--
--         , requestBatchPutMessage $
--             newBatchPutMessage
--
--         , requestDescribeLoggingOptions $
--             newDescribeLoggingOptions
--
--         , requestDeleteDatastore $
--             newDeleteDatastore
--
--         , requestUpdateDatastore $
--             newUpdateDatastore
--
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestCreateDataset $
--             newCreateDataset
--
--         , requestUpdatePipeline $
--             newUpdatePipeline
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestCancelPipelineReprocessing $
--             newCancelPipelineReprocessing
--
--         , requestTagResource $
--             newTagResource
--
--         , requestSampleChannelData $
--             newSampleChannelData
--
--         , requestDescribeDatastore $
--             newDescribeDatastore
--
--         , requestListChannels $
--             newListChannels
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestCreateDatasetContent $
--             newCreateDatasetContent
--
--         , requestDescribeChannel $
--             newDescribeChannel
--
--         , requestListDatastores $
--             newListDatastores
--
--         , requestStartPipelineReprocessing $
--             newStartPipelineReprocessing
--
--         , requestRunPipelineActivity $
--             newRunPipelineActivity
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestCreateDatastore $
--             newCreateDatastore
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestUpdateDataset $
--             newUpdateDataset
--
--         , requestGetDatasetContent $
--             newGetDatasetContent
--
--         , requestListDatasetContents $
--             newListDatasetContents
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteChannel $
--             newDeleteChannel
--
--         , requestUpdateChannel $
--             newUpdateChannel
--
--         , requestPutLoggingOptions $
--             newPutLoggingOptions
--
--         , requestDeleteDatasetContent $
--             newDeleteDatasetContent
--
--           ]

--     , testGroup "response"
--         [ responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseDescribePipeline $
--             newDescribePipelineResponse
--
--         , responseBatchPutMessage $
--             newBatchPutMessageResponse
--
--         , responseDescribeLoggingOptions $
--             newDescribeLoggingOptionsResponse
--
--         , responseDeleteDatastore $
--             newDeleteDatastoreResponse
--
--         , responseUpdateDatastore $
--             newUpdateDatastoreResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseUpdatePipeline $
--             newUpdatePipelineResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseCancelPipelineReprocessing $
--             newCancelPipelineReprocessingResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseSampleChannelData $
--             newSampleChannelDataResponse
--
--         , responseDescribeDatastore $
--             newDescribeDatastoreResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseCreateDatasetContent $
--             newCreateDatasetContentResponse
--
--         , responseDescribeChannel $
--             newDescribeChannelResponse
--
--         , responseListDatastores $
--             newListDatastoresResponse
--
--         , responseStartPipelineReprocessing $
--             newStartPipelineReprocessingResponse
--
--         , responseRunPipelineActivity $
--             newRunPipelineActivityResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseCreateDatastore $
--             newCreateDatastoreResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responseUpdateDataset $
--             newUpdateDatasetResponse
--
--         , responseGetDatasetContent $
--             newGetDatasetContentResponse
--
--         , responseListDatasetContents $
--             newListDatasetContentsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteChannel $
--             newDeleteChannelResponse
--
--         , responseUpdateChannel $
--             newUpdateChannelResponse
--
--         , responsePutLoggingOptions $
--             newPutLoggingOptionsResponse
--
--         , responseDeleteDatasetContent $
--             newDeleteDatasetContentResponse
--
--           ]
--     ]

-- Requests

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestDescribePipeline :: DescribePipeline -> TestTree
requestDescribePipeline =
  req
    "DescribePipeline"
    "fixture/DescribePipeline.yaml"

requestBatchPutMessage :: BatchPutMessage -> TestTree
requestBatchPutMessage =
  req
    "BatchPutMessage"
    "fixture/BatchPutMessage.yaml"

requestDescribeLoggingOptions :: DescribeLoggingOptions -> TestTree
requestDescribeLoggingOptions =
  req
    "DescribeLoggingOptions"
    "fixture/DescribeLoggingOptions.yaml"

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

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset =
  req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline =
  req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestCancelPipelineReprocessing :: CancelPipelineReprocessing -> TestTree
requestCancelPipelineReprocessing =
  req
    "CancelPipelineReprocessing"
    "fixture/CancelPipelineReprocessing.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestSampleChannelData :: SampleChannelData -> TestTree
requestSampleChannelData =
  req
    "SampleChannelData"
    "fixture/SampleChannelData.yaml"

requestDescribeDatastore :: DescribeDatastore -> TestTree
requestDescribeDatastore =
  req
    "DescribeDatastore"
    "fixture/DescribeDatastore.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestCreateDatasetContent :: CreateDatasetContent -> TestTree
requestCreateDatasetContent =
  req
    "CreateDatasetContent"
    "fixture/CreateDatasetContent.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel =
  req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestListDatastores :: ListDatastores -> TestTree
requestListDatastores =
  req
    "ListDatastores"
    "fixture/ListDatastores.yaml"

requestStartPipelineReprocessing :: StartPipelineReprocessing -> TestTree
requestStartPipelineReprocessing =
  req
    "StartPipelineReprocessing"
    "fixture/StartPipelineReprocessing.yaml"

requestRunPipelineActivity :: RunPipelineActivity -> TestTree
requestRunPipelineActivity =
  req
    "RunPipelineActivity"
    "fixture/RunPipelineActivity.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestCreateDatastore :: CreateDatastore -> TestTree
requestCreateDatastore =
  req
    "CreateDatastore"
    "fixture/CreateDatastore.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestUpdateDataset :: UpdateDataset -> TestTree
requestUpdateDataset =
  req
    "UpdateDataset"
    "fixture/UpdateDataset.yaml"

requestGetDatasetContent :: GetDatasetContent -> TestTree
requestGetDatasetContent =
  req
    "GetDatasetContent"
    "fixture/GetDatasetContent.yaml"

requestListDatasetContents :: ListDatasetContents -> TestTree
requestListDatasetContents =
  req
    "ListDatasetContents"
    "fixture/ListDatasetContents.yaml"

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

requestPutLoggingOptions :: PutLoggingOptions -> TestTree
requestPutLoggingOptions =
  req
    "PutLoggingOptions"
    "fixture/PutLoggingOptions.yaml"

requestDeleteDatasetContent :: DeleteDatasetContent -> TestTree
requestDeleteDatasetContent =
  req
    "DeleteDatasetContent"
    "fixture/DeleteDatasetContent.yaml"

-- Responses

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateChannel)

responseDescribePipeline :: DescribePipelineResponse -> TestTree
responseDescribePipeline =
  res
    "DescribePipelineResponse"
    "fixture/DescribePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePipeline)

responseBatchPutMessage :: BatchPutMessageResponse -> TestTree
responseBatchPutMessage =
  res
    "BatchPutMessageResponse"
    "fixture/BatchPutMessageResponse.proto"
    defaultService
    (Proxy :: Proxy BatchPutMessage)

responseDescribeLoggingOptions :: DescribeLoggingOptionsResponse -> TestTree
responseDescribeLoggingOptions =
  res
    "DescribeLoggingOptionsResponse"
    "fixture/DescribeLoggingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoggingOptions)

responseDeleteDatastore :: DeleteDatastoreResponse -> TestTree
responseDeleteDatastore =
  res
    "DeleteDatastoreResponse"
    "fixture/DeleteDatastoreResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDatastore)

responseUpdateDatastore :: UpdateDatastoreResponse -> TestTree
responseUpdateDatastore =
  res
    "UpdateDatastoreResponse"
    "fixture/UpdateDatastoreResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDatastore)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePipeline)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDataset)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePipeline)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePipeline)

responseCancelPipelineReprocessing :: CancelPipelineReprocessingResponse -> TestTree
responseCancelPipelineReprocessing =
  res
    "CancelPipelineReprocessingResponse"
    "fixture/CancelPipelineReprocessingResponse.proto"
    defaultService
    (Proxy :: Proxy CancelPipelineReprocessing)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseSampleChannelData :: SampleChannelDataResponse -> TestTree
responseSampleChannelData =
  res
    "SampleChannelDataResponse"
    "fixture/SampleChannelDataResponse.proto"
    defaultService
    (Proxy :: Proxy SampleChannelData)

responseDescribeDatastore :: DescribeDatastoreResponse -> TestTree
responseDescribeDatastore =
  res
    "DescribeDatastoreResponse"
    "fixture/DescribeDatastoreResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDatastore)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListChannels)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDataset)

responseCreateDatasetContent :: CreateDatasetContentResponse -> TestTree
responseCreateDatasetContent =
  res
    "CreateDatasetContentResponse"
    "fixture/CreateDatasetContentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDatasetContent)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeChannel)

responseListDatastores :: ListDatastoresResponse -> TestTree
responseListDatastores =
  res
    "ListDatastoresResponse"
    "fixture/ListDatastoresResponse.proto"
    defaultService
    (Proxy :: Proxy ListDatastores)

responseStartPipelineReprocessing :: StartPipelineReprocessingResponse -> TestTree
responseStartPipelineReprocessing =
  res
    "StartPipelineReprocessingResponse"
    "fixture/StartPipelineReprocessingResponse.proto"
    defaultService
    (Proxy :: Proxy StartPipelineReprocessing)

responseRunPipelineActivity :: RunPipelineActivityResponse -> TestTree
responseRunPipelineActivity =
  res
    "RunPipelineActivityResponse"
    "fixture/RunPipelineActivityResponse.proto"
    defaultService
    (Proxy :: Proxy RunPipelineActivity)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDataset)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDatasets)

responseCreateDatastore :: CreateDatastoreResponse -> TestTree
responseCreateDatastore =
  res
    "CreateDatastoreResponse"
    "fixture/CreateDatastoreResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDatastore)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPipelines)

responseUpdateDataset :: UpdateDatasetResponse -> TestTree
responseUpdateDataset =
  res
    "UpdateDatasetResponse"
    "fixture/UpdateDatasetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDataset)

responseGetDatasetContent :: GetDatasetContentResponse -> TestTree
responseGetDatasetContent =
  res
    "GetDatasetContentResponse"
    "fixture/GetDatasetContentResponse.proto"
    defaultService
    (Proxy :: Proxy GetDatasetContent)

responseListDatasetContents :: ListDatasetContentsResponse -> TestTree
responseListDatasetContents =
  res
    "ListDatasetContentsResponse"
    "fixture/ListDatasetContentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDatasetContents)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteChannel)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateChannel)

responsePutLoggingOptions :: PutLoggingOptionsResponse -> TestTree
responsePutLoggingOptions =
  res
    "PutLoggingOptionsResponse"
    "fixture/PutLoggingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutLoggingOptions)

responseDeleteDatasetContent :: DeleteDatasetContentResponse -> TestTree
responseDeleteDatasetContent =
  res
    "DeleteDatasetContentResponse"
    "fixture/DeleteDatasetContentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDatasetContent)
