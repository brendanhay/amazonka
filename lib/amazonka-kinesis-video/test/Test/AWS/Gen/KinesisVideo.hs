{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.KinesisVideo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.KinesisVideo where

import Data.Proxy
import Network.AWS.KinesisVideo
import Test.AWS.Fixture
import Test.AWS.KinesisVideo.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListSignalingChannels $
--             mkListSignalingChannels
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestUntagStream $
--             mkUntagStream
--
--         , requestUpdateDataRetention $
--             mkUpdateDataRetention
--
--         , requestGetDataEndpoint $
--             mkGetDataEndpoint
--
--         , requestGetSignalingChannelEndpoint $
--             mkGetSignalingChannelEndpoint
--
--         , requestListTagsForStream $
--             mkListTagsForStream
--
--         , requestDeleteSignalingChannel $
--             mkDeleteSignalingChannel
--
--         , requestUpdateSignalingChannel $
--             mkUpdateSignalingChannel
--
--         , requestUpdateStream $
--             mkUpdateStream
--
--         , requestDeleteStream $
--             mkDeleteStream
--
--         , requestListStreams $
--             mkListStreams
--
--         , requestCreateStream $
--             mkCreateStream
--
--         , requestDescribeSignalingChannel $
--             mkDescribeSignalingChannel
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestTagStream $
--             mkTagStream
--
--         , requestCreateSignalingChannel $
--             mkCreateSignalingChannel
--
--         , requestDescribeStream $
--             mkDescribeStream
--
--           ]

--     , testGroup "response"
--         [ responseListSignalingChannels $
--             mkListSignalingChannelsResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseUntagStream $
--             mkUntagStreamResponse
--
--         , responseUpdateDataRetention $
--             mkUpdateDataRetentionResponse
--
--         , responseGetDataEndpoint $
--             mkGetDataEndpointResponse
--
--         , responseGetSignalingChannelEndpoint $
--             mkGetSignalingChannelEndpointResponse
--
--         , responseListTagsForStream $
--             mkListTagsForStreamResponse
--
--         , responseDeleteSignalingChannel $
--             mkDeleteSignalingChannelResponse
--
--         , responseUpdateSignalingChannel $
--             mkUpdateSignalingChannelResponse
--
--         , responseUpdateStream $
--             mkUpdateStreamResponse
--
--         , responseDeleteStream $
--             mkDeleteStreamResponse
--
--         , responseListStreams $
--             mkListStreamsResponse
--
--         , responseCreateStream $
--             mkCreateStreamResponse
--
--         , responseDescribeSignalingChannel $
--             mkDescribeSignalingChannelResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseTagStream $
--             mkTagStreamResponse
--
--         , responseCreateSignalingChannel $
--             mkCreateSignalingChannelResponse
--
--         , responseDescribeStream $
--             mkDescribeStreamResponse
--
--           ]
--     ]

-- Requests

requestListSignalingChannels :: ListSignalingChannels -> TestTree
requestListSignalingChannels =
  req
    "ListSignalingChannels"
    "fixture/ListSignalingChannels.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestListTagsForStream :: ListTagsForStream -> TestTree
requestListTagsForStream =
  req
    "ListTagsForStream"
    "fixture/ListTagsForStream.yaml"

requestDeleteSignalingChannel :: DeleteSignalingChannel -> TestTree
requestDeleteSignalingChannel =
  req
    "DeleteSignalingChannel"
    "fixture/DeleteSignalingChannel.yaml"

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

requestDeleteStream :: DeleteStream -> TestTree
requestDeleteStream =
  req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams =
  req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream =
  req
    "CreateStream"
    "fixture/CreateStream.yaml"

requestDescribeSignalingChannel :: DescribeSignalingChannel -> TestTree
requestDescribeSignalingChannel =
  req
    "DescribeSignalingChannel"
    "fixture/DescribeSignalingChannel.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestTagStream :: TagStream -> TestTree
requestTagStream =
  req
    "TagStream"
    "fixture/TagStream.yaml"

requestCreateSignalingChannel :: CreateSignalingChannel -> TestTree
requestCreateSignalingChannel =
  req
    "CreateSignalingChannel"
    "fixture/CreateSignalingChannel.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream =
  req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

-- Responses

responseListSignalingChannels :: ListSignalingChannelsResponse -> TestTree
responseListSignalingChannels =
  res
    "ListSignalingChannelsResponse"
    "fixture/ListSignalingChannelsResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy ListSignalingChannels)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy ListTagsForResource)

responseUntagStream :: UntagStreamResponse -> TestTree
responseUntagStream =
  res
    "UntagStreamResponse"
    "fixture/UntagStreamResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy UntagStream)

responseUpdateDataRetention :: UpdateDataRetentionResponse -> TestTree
responseUpdateDataRetention =
  res
    "UpdateDataRetentionResponse"
    "fixture/UpdateDataRetentionResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy UpdateDataRetention)

responseGetDataEndpoint :: GetDataEndpointResponse -> TestTree
responseGetDataEndpoint =
  res
    "GetDataEndpointResponse"
    "fixture/GetDataEndpointResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy GetDataEndpoint)

responseGetSignalingChannelEndpoint :: GetSignalingChannelEndpointResponse -> TestTree
responseGetSignalingChannelEndpoint =
  res
    "GetSignalingChannelEndpointResponse"
    "fixture/GetSignalingChannelEndpointResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy GetSignalingChannelEndpoint)

responseListTagsForStream :: ListTagsForStreamResponse -> TestTree
responseListTagsForStream =
  res
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy ListTagsForStream)

responseDeleteSignalingChannel :: DeleteSignalingChannelResponse -> TestTree
responseDeleteSignalingChannel =
  res
    "DeleteSignalingChannelResponse"
    "fixture/DeleteSignalingChannelResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy DeleteSignalingChannel)

responseUpdateSignalingChannel :: UpdateSignalingChannelResponse -> TestTree
responseUpdateSignalingChannel =
  res
    "UpdateSignalingChannelResponse"
    "fixture/UpdateSignalingChannelResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy UpdateSignalingChannel)

responseUpdateStream :: UpdateStreamResponse -> TestTree
responseUpdateStream =
  res
    "UpdateStreamResponse"
    "fixture/UpdateStreamResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy UpdateStream)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy DeleteStream)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy ListStreams)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy CreateStream)

responseDescribeSignalingChannel :: DescribeSignalingChannelResponse -> TestTree
responseDescribeSignalingChannel =
  res
    "DescribeSignalingChannelResponse"
    "fixture/DescribeSignalingChannelResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy DescribeSignalingChannel)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy UntagResource)

responseTagStream :: TagStreamResponse -> TestTree
responseTagStream =
  res
    "TagStreamResponse"
    "fixture/TagStreamResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy TagStream)

responseCreateSignalingChannel :: CreateSignalingChannelResponse -> TestTree
responseCreateSignalingChannel =
  res
    "CreateSignalingChannelResponse"
    "fixture/CreateSignalingChannelResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy CreateSignalingChannel)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    kinesisVideoService
    (Proxy :: Proxy DescribeStream)
