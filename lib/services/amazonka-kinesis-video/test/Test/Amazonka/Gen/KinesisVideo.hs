{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.KinesisVideo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.KinesisVideo where

import Amazonka.KinesisVideo
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.KinesisVideo.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateSignalingChannel $
--             newCreateSignalingChannel
--
--         , requestCreateStream $
--             newCreateStream
--
--         , requestDeleteSignalingChannel $
--             newDeleteSignalingChannel
--
--         , requestDeleteStream $
--             newDeleteStream
--
--         , requestDescribeEdgeConfiguration $
--             newDescribeEdgeConfiguration
--
--         , requestDescribeImageGenerationConfiguration $
--             newDescribeImageGenerationConfiguration
--
--         , requestDescribeMappedResourceConfiguration $
--             newDescribeMappedResourceConfiguration
--
--         , requestDescribeMediaStorageConfiguration $
--             newDescribeMediaStorageConfiguration
--
--         , requestDescribeNotificationConfiguration $
--             newDescribeNotificationConfiguration
--
--         , requestDescribeSignalingChannel $
--             newDescribeSignalingChannel
--
--         , requestDescribeStream $
--             newDescribeStream
--
--         , requestGetDataEndpoint $
--             newGetDataEndpoint
--
--         , requestGetSignalingChannelEndpoint $
--             newGetSignalingChannelEndpoint
--
--         , requestListSignalingChannels $
--             newListSignalingChannels
--
--         , requestListStreams $
--             newListStreams
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTagsForStream $
--             newListTagsForStream
--
--         , requestStartEdgeConfigurationUpdate $
--             newStartEdgeConfigurationUpdate
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTagStream $
--             newTagStream
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUntagStream $
--             newUntagStream
--
--         , requestUpdateDataRetention $
--             newUpdateDataRetention
--
--         , requestUpdateImageGenerationConfiguration $
--             newUpdateImageGenerationConfiguration
--
--         , requestUpdateMediaStorageConfiguration $
--             newUpdateMediaStorageConfiguration
--
--         , requestUpdateNotificationConfiguration $
--             newUpdateNotificationConfiguration
--
--         , requestUpdateSignalingChannel $
--             newUpdateSignalingChannel
--
--         , requestUpdateStream $
--             newUpdateStream
--
--           ]

--     , testGroup "response"
--         [ responseCreateSignalingChannel $
--             newCreateSignalingChannelResponse
--
--         , responseCreateStream $
--             newCreateStreamResponse
--
--         , responseDeleteSignalingChannel $
--             newDeleteSignalingChannelResponse
--
--         , responseDeleteStream $
--             newDeleteStreamResponse
--
--         , responseDescribeEdgeConfiguration $
--             newDescribeEdgeConfigurationResponse
--
--         , responseDescribeImageGenerationConfiguration $
--             newDescribeImageGenerationConfigurationResponse
--
--         , responseDescribeMappedResourceConfiguration $
--             newDescribeMappedResourceConfigurationResponse
--
--         , responseDescribeMediaStorageConfiguration $
--             newDescribeMediaStorageConfigurationResponse
--
--         , responseDescribeNotificationConfiguration $
--             newDescribeNotificationConfigurationResponse
--
--         , responseDescribeSignalingChannel $
--             newDescribeSignalingChannelResponse
--
--         , responseDescribeStream $
--             newDescribeStreamResponse
--
--         , responseGetDataEndpoint $
--             newGetDataEndpointResponse
--
--         , responseGetSignalingChannelEndpoint $
--             newGetSignalingChannelEndpointResponse
--
--         , responseListSignalingChannels $
--             newListSignalingChannelsResponse
--
--         , responseListStreams $
--             newListStreamsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTagsForStream $
--             newListTagsForStreamResponse
--
--         , responseStartEdgeConfigurationUpdate $
--             newStartEdgeConfigurationUpdateResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTagStream $
--             newTagStreamResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUntagStream $
--             newUntagStreamResponse
--
--         , responseUpdateDataRetention $
--             newUpdateDataRetentionResponse
--
--         , responseUpdateImageGenerationConfiguration $
--             newUpdateImageGenerationConfigurationResponse
--
--         , responseUpdateMediaStorageConfiguration $
--             newUpdateMediaStorageConfigurationResponse
--
--         , responseUpdateNotificationConfiguration $
--             newUpdateNotificationConfigurationResponse
--
--         , responseUpdateSignalingChannel $
--             newUpdateSignalingChannelResponse
--
--         , responseUpdateStream $
--             newUpdateStreamResponse
--
--           ]
--     ]

-- Requests

requestCreateSignalingChannel :: CreateSignalingChannel -> TestTree
requestCreateSignalingChannel =
  req
    "CreateSignalingChannel"
    "fixture/CreateSignalingChannel.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream =
  req
    "CreateStream"
    "fixture/CreateStream.yaml"

requestDeleteSignalingChannel :: DeleteSignalingChannel -> TestTree
requestDeleteSignalingChannel =
  req
    "DeleteSignalingChannel"
    "fixture/DeleteSignalingChannel.yaml"

requestDeleteStream :: DeleteStream -> TestTree
requestDeleteStream =
  req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

requestDescribeEdgeConfiguration :: DescribeEdgeConfiguration -> TestTree
requestDescribeEdgeConfiguration =
  req
    "DescribeEdgeConfiguration"
    "fixture/DescribeEdgeConfiguration.yaml"

requestDescribeImageGenerationConfiguration :: DescribeImageGenerationConfiguration -> TestTree
requestDescribeImageGenerationConfiguration =
  req
    "DescribeImageGenerationConfiguration"
    "fixture/DescribeImageGenerationConfiguration.yaml"

requestDescribeMappedResourceConfiguration :: DescribeMappedResourceConfiguration -> TestTree
requestDescribeMappedResourceConfiguration =
  req
    "DescribeMappedResourceConfiguration"
    "fixture/DescribeMappedResourceConfiguration.yaml"

requestDescribeMediaStorageConfiguration :: DescribeMediaStorageConfiguration -> TestTree
requestDescribeMediaStorageConfiguration =
  req
    "DescribeMediaStorageConfiguration"
    "fixture/DescribeMediaStorageConfiguration.yaml"

requestDescribeNotificationConfiguration :: DescribeNotificationConfiguration -> TestTree
requestDescribeNotificationConfiguration =
  req
    "DescribeNotificationConfiguration"
    "fixture/DescribeNotificationConfiguration.yaml"

requestDescribeSignalingChannel :: DescribeSignalingChannel -> TestTree
requestDescribeSignalingChannel =
  req
    "DescribeSignalingChannel"
    "fixture/DescribeSignalingChannel.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream =
  req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

requestGetDataEndpoint :: GetDataEndpoint -> TestTree
requestGetDataEndpoint =
  req
    "GetDataEndpoint"
    "fixture/GetDataEndpoint.yaml"

requestGetSignalingChannelEndpoint :: GetSignalingChannelEndpoint -> TestTree
requestGetSignalingChannelEndpoint =
  req
    "GetSignalingChannelEndpoint"
    "fixture/GetSignalingChannelEndpoint.yaml"

requestListSignalingChannels :: ListSignalingChannels -> TestTree
requestListSignalingChannels =
  req
    "ListSignalingChannels"
    "fixture/ListSignalingChannels.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams =
  req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTagsForStream :: ListTagsForStream -> TestTree
requestListTagsForStream =
  req
    "ListTagsForStream"
    "fixture/ListTagsForStream.yaml"

requestStartEdgeConfigurationUpdate :: StartEdgeConfigurationUpdate -> TestTree
requestStartEdgeConfigurationUpdate =
  req
    "StartEdgeConfigurationUpdate"
    "fixture/StartEdgeConfigurationUpdate.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTagStream :: TagStream -> TestTree
requestTagStream =
  req
    "TagStream"
    "fixture/TagStream.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUntagStream :: UntagStream -> TestTree
requestUntagStream =
  req
    "UntagStream"
    "fixture/UntagStream.yaml"

requestUpdateDataRetention :: UpdateDataRetention -> TestTree
requestUpdateDataRetention =
  req
    "UpdateDataRetention"
    "fixture/UpdateDataRetention.yaml"

requestUpdateImageGenerationConfiguration :: UpdateImageGenerationConfiguration -> TestTree
requestUpdateImageGenerationConfiguration =
  req
    "UpdateImageGenerationConfiguration"
    "fixture/UpdateImageGenerationConfiguration.yaml"

requestUpdateMediaStorageConfiguration :: UpdateMediaStorageConfiguration -> TestTree
requestUpdateMediaStorageConfiguration =
  req
    "UpdateMediaStorageConfiguration"
    "fixture/UpdateMediaStorageConfiguration.yaml"

requestUpdateNotificationConfiguration :: UpdateNotificationConfiguration -> TestTree
requestUpdateNotificationConfiguration =
  req
    "UpdateNotificationConfiguration"
    "fixture/UpdateNotificationConfiguration.yaml"

requestUpdateSignalingChannel :: UpdateSignalingChannel -> TestTree
requestUpdateSignalingChannel =
  req
    "UpdateSignalingChannel"
    "fixture/UpdateSignalingChannel.yaml"

requestUpdateStream :: UpdateStream -> TestTree
requestUpdateStream =
  req
    "UpdateStream"
    "fixture/UpdateStream.yaml"

-- Responses

responseCreateSignalingChannel :: CreateSignalingChannelResponse -> TestTree
responseCreateSignalingChannel =
  res
    "CreateSignalingChannelResponse"
    "fixture/CreateSignalingChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSignalingChannel)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStream)

responseDeleteSignalingChannel :: DeleteSignalingChannelResponse -> TestTree
responseDeleteSignalingChannel =
  res
    "DeleteSignalingChannelResponse"
    "fixture/DeleteSignalingChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSignalingChannel)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStream)

responseDescribeEdgeConfiguration :: DescribeEdgeConfigurationResponse -> TestTree
responseDescribeEdgeConfiguration =
  res
    "DescribeEdgeConfigurationResponse"
    "fixture/DescribeEdgeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEdgeConfiguration)

responseDescribeImageGenerationConfiguration :: DescribeImageGenerationConfigurationResponse -> TestTree
responseDescribeImageGenerationConfiguration =
  res
    "DescribeImageGenerationConfigurationResponse"
    "fixture/DescribeImageGenerationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImageGenerationConfiguration)

responseDescribeMappedResourceConfiguration :: DescribeMappedResourceConfigurationResponse -> TestTree
responseDescribeMappedResourceConfiguration =
  res
    "DescribeMappedResourceConfigurationResponse"
    "fixture/DescribeMappedResourceConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMappedResourceConfiguration)

responseDescribeMediaStorageConfiguration :: DescribeMediaStorageConfigurationResponse -> TestTree
responseDescribeMediaStorageConfiguration =
  res
    "DescribeMediaStorageConfigurationResponse"
    "fixture/DescribeMediaStorageConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMediaStorageConfiguration)

responseDescribeNotificationConfiguration :: DescribeNotificationConfigurationResponse -> TestTree
responseDescribeNotificationConfiguration =
  res
    "DescribeNotificationConfigurationResponse"
    "fixture/DescribeNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotificationConfiguration)

responseDescribeSignalingChannel :: DescribeSignalingChannelResponse -> TestTree
responseDescribeSignalingChannel =
  res
    "DescribeSignalingChannelResponse"
    "fixture/DescribeSignalingChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSignalingChannel)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStream)

responseGetDataEndpoint :: GetDataEndpointResponse -> TestTree
responseGetDataEndpoint =
  res
    "GetDataEndpointResponse"
    "fixture/GetDataEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataEndpoint)

responseGetSignalingChannelEndpoint :: GetSignalingChannelEndpointResponse -> TestTree
responseGetSignalingChannelEndpoint =
  res
    "GetSignalingChannelEndpointResponse"
    "fixture/GetSignalingChannelEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSignalingChannelEndpoint)

responseListSignalingChannels :: ListSignalingChannelsResponse -> TestTree
responseListSignalingChannels =
  res
    "ListSignalingChannelsResponse"
    "fixture/ListSignalingChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSignalingChannels)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreams)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTagsForStream :: ListTagsForStreamResponse -> TestTree
responseListTagsForStream =
  res
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForStream)

responseStartEdgeConfigurationUpdate :: StartEdgeConfigurationUpdateResponse -> TestTree
responseStartEdgeConfigurationUpdate =
  res
    "StartEdgeConfigurationUpdateResponse"
    "fixture/StartEdgeConfigurationUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartEdgeConfigurationUpdate)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTagStream :: TagStreamResponse -> TestTree
responseTagStream =
  res
    "TagStreamResponse"
    "fixture/TagStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagStream)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUntagStream :: UntagStreamResponse -> TestTree
responseUntagStream =
  res
    "UntagStreamResponse"
    "fixture/UntagStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagStream)

responseUpdateDataRetention :: UpdateDataRetentionResponse -> TestTree
responseUpdateDataRetention =
  res
    "UpdateDataRetentionResponse"
    "fixture/UpdateDataRetentionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataRetention)

responseUpdateImageGenerationConfiguration :: UpdateImageGenerationConfigurationResponse -> TestTree
responseUpdateImageGenerationConfiguration =
  res
    "UpdateImageGenerationConfigurationResponse"
    "fixture/UpdateImageGenerationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateImageGenerationConfiguration)

responseUpdateMediaStorageConfiguration :: UpdateMediaStorageConfigurationResponse -> TestTree
responseUpdateMediaStorageConfiguration =
  res
    "UpdateMediaStorageConfigurationResponse"
    "fixture/UpdateMediaStorageConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMediaStorageConfiguration)

responseUpdateNotificationConfiguration :: UpdateNotificationConfigurationResponse -> TestTree
responseUpdateNotificationConfiguration =
  res
    "UpdateNotificationConfigurationResponse"
    "fixture/UpdateNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNotificationConfiguration)

responseUpdateSignalingChannel :: UpdateSignalingChannelResponse -> TestTree
responseUpdateSignalingChannel =
  res
    "UpdateSignalingChannelResponse"
    "fixture/UpdateSignalingChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSignalingChannel)

responseUpdateStream :: UpdateStreamResponse -> TestTree
responseUpdateStream =
  res
    "UpdateStreamResponse"
    "fixture/UpdateStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStream)
