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

import qualified Data.Proxy as Proxy
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
--             newDescribePipeline
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestListChannels $
--             newListChannels
--
--         , requestListDatasetContents $
--             newListDatasetContents
--
--         , requestPutLoggingOptions $
--             newPutLoggingOptions
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
--         , requestSampleChannelData $
--             newSampleChannelData
--
--         , requestCancelPipelineReprocessing $
--             newCancelPipelineReprocessing
--
--         , requestCreateDatastore $
--             newCreateDatastore
--
--         , requestUpdatePipeline $
--             newUpdatePipeline
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestUpdateDataset $
--             newUpdateDataset
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestDeleteDatastore $
--             newDeleteDatastore
--
--         , requestUpdateDatastore $
--             newUpdateDatastore
--
--         , requestCreateDataset $
--             newCreateDataset
--
--         , requestBatchPutMessage $
--             newBatchPutMessage
--
--         , requestListDatastores $
--             newListDatastores
--
--         , requestCreateDatasetContent $
--             newCreateDatasetContent
--
--         , requestCreateChannel $
--             newCreateChannel
--
--         , requestDeleteDatasetContent $
--             newDeleteDatasetContent
--
--         , requestDescribeDatastore $
--             newDescribeDatastore
--
--         , requestGetDatasetContent $
--             newGetDatasetContent
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestRunPipelineActivity $
--             newRunPipelineActivity
--
--         , requestDescribeChannel $
--             newDescribeChannel
--
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestStartPipelineReprocessing $
--             newStartPipelineReprocessing
--
--         , requestDescribeLoggingOptions $
--             newDescribeLoggingOptions
--
--           ]

--     , testGroup "response"
--         [ responseDescribePipeline $
--             newDescribePipelineResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseListDatasetContents $
--             newListDatasetContentsResponse
--
--         , responsePutLoggingOptions $
--             newPutLoggingOptionsResponse
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
--         , responseSampleChannelData $
--             newSampleChannelDataResponse
--
--         , responseCancelPipelineReprocessing $
--             newCancelPipelineReprocessingResponse
--
--         , responseCreateDatastore $
--             newCreateDatastoreResponse
--
--         , responseUpdatePipeline $
--             newUpdatePipelineResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseUpdateDataset $
--             newUpdateDatasetResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responseDeleteDatastore $
--             newDeleteDatastoreResponse
--
--         , responseUpdateDatastore $
--             newUpdateDatastoreResponse
--
--         , responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseBatchPutMessage $
--             newBatchPutMessageResponse
--
--         , responseListDatastores $
--             newListDatastoresResponse
--
--         , responseCreateDatasetContent $
--             newCreateDatasetContentResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseDeleteDatasetContent $
--             newDeleteDatasetContentResponse
--
--         , responseDescribeDatastore $
--             newDescribeDatastoreResponse
--
--         , responseGetDatasetContent $
--             newGetDatasetContentResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseRunPipelineActivity $
--             newRunPipelineActivityResponse
--
--         , responseDescribeChannel $
--             newDescribeChannelResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseStartPipelineReprocessing $
--             newStartPipelineReprocessingResponse
--
--         , responseDescribeLoggingOptions $
--             newDescribeLoggingOptionsResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePipeline)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataset)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannels)

responseListDatasetContents :: ListDatasetContentsResponse -> TestTree
responseListDatasetContents =
  res
    "ListDatasetContentsResponse"
    "fixture/ListDatasetContentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetContents)

responsePutLoggingOptions :: PutLoggingOptionsResponse -> TestTree
responsePutLoggingOptions =
  res
    "PutLoggingOptionsResponse"
    "fixture/PutLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLoggingOptions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannel)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannel)

responseSampleChannelData :: SampleChannelDataResponse -> TestTree
responseSampleChannelData =
  res
    "SampleChannelDataResponse"
    "fixture/SampleChannelDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SampleChannelData)

responseCancelPipelineReprocessing :: CancelPipelineReprocessingResponse -> TestTree
responseCancelPipelineReprocessing =
  res
    "CancelPipelineReprocessingResponse"
    "fixture/CancelPipelineReprocessingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelPipelineReprocessing)

responseCreateDatastore :: CreateDatastoreResponse -> TestTree
responseCreateDatastore =
  res
    "CreateDatastoreResponse"
    "fixture/CreateDatastoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatastore)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePipeline)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePipeline)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataset)

responseUpdateDataset :: UpdateDatasetResponse -> TestTree
responseUpdateDataset =
  res
    "UpdateDatasetResponse"
    "fixture/UpdateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataset)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelines)

responseDeleteDatastore :: DeleteDatastoreResponse -> TestTree
responseDeleteDatastore =
  res
    "DeleteDatastoreResponse"
    "fixture/DeleteDatastoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatastore)

responseUpdateDatastore :: UpdateDatastoreResponse -> TestTree
responseUpdateDatastore =
  res
    "UpdateDatastoreResponse"
    "fixture/UpdateDatastoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDatastore)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataset)

responseBatchPutMessage :: BatchPutMessageResponse -> TestTree
responseBatchPutMessage =
  res
    "BatchPutMessageResponse"
    "fixture/BatchPutMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutMessage)

responseListDatastores :: ListDatastoresResponse -> TestTree
responseListDatastores =
  res
    "ListDatastoresResponse"
    "fixture/ListDatastoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatastores)

responseCreateDatasetContent :: CreateDatasetContentResponse -> TestTree
responseCreateDatasetContent =
  res
    "CreateDatasetContentResponse"
    "fixture/CreateDatasetContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDatasetContent)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannel)

responseDeleteDatasetContent :: DeleteDatasetContentResponse -> TestTree
responseDeleteDatasetContent =
  res
    "DeleteDatasetContentResponse"
    "fixture/DeleteDatasetContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDatasetContent)

responseDescribeDatastore :: DescribeDatastoreResponse -> TestTree
responseDescribeDatastore =
  res
    "DescribeDatastoreResponse"
    "fixture/DescribeDatastoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDatastore)

responseGetDatasetContent :: GetDatasetContentResponse -> TestTree
responseGetDatasetContent =
  res
    "GetDatasetContentResponse"
    "fixture/GetDatasetContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatasetContent)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasets)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseRunPipelineActivity :: RunPipelineActivityResponse -> TestTree
responseRunPipelineActivity =
  res
    "RunPipelineActivityResponse"
    "fixture/RunPipelineActivityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RunPipelineActivity)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannel)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePipeline)

responseStartPipelineReprocessing :: StartPipelineReprocessingResponse -> TestTree
responseStartPipelineReprocessing =
  res
    "StartPipelineReprocessingResponse"
    "fixture/StartPipelineReprocessingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPipelineReprocessing)

responseDescribeLoggingOptions :: DescribeLoggingOptionsResponse -> TestTree
responseDescribeLoggingOptions =
  res
    "DescribeLoggingOptionsResponse"
    "fixture/DescribeLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoggingOptions)
