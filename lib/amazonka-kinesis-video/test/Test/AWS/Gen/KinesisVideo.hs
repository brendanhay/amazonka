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
--             listSignalingChannels
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestUntagStream $
--             untagStream
--
--         , requestUpdateDataRetention $
--             updateDataRetention
--
--         , requestGetDataEndpoint $
--             getDataEndpoint
--
--         , requestGetSignalingChannelEndpoint $
--             getSignalingChannelEndpoint
--
--         , requestListTagsForStream $
--             listTagsForStream
--
--         , requestDeleteSignalingChannel $
--             deleteSignalingChannel
--
--         , requestUpdateSignalingChannel $
--             updateSignalingChannel
--
--         , requestUpdateStream $
--             updateStream
--
--         , requestDeleteStream $
--             deleteStream
--
--         , requestListStreams $
--             listStreams
--
--         , requestCreateStream $
--             createStream
--
--         , requestDescribeSignalingChannel $
--             describeSignalingChannel
--
--         , requestTagResource $
--             tagResource
--
--         , requestUntagResource $
--             untagResource
--
--         , requestTagStream $
--             tagStream
--
--         , requestCreateSignalingChannel $
--             createSignalingChannel
--
--         , requestDescribeStream $
--             describeStream
--
--           ]

--     , testGroup "response"
--         [ responseListSignalingChannels $
--             listSignalingChannelsResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseUntagStream $
--             untagStreamResponse
--
--         , responseUpdateDataRetention $
--             updateDataRetentionResponse
--
--         , responseGetDataEndpoint $
--             getDataEndpointResponse
--
--         , responseGetSignalingChannelEndpoint $
--             getSignalingChannelEndpointResponse
--
--         , responseListTagsForStream $
--             listTagsForStreamResponse
--
--         , responseDeleteSignalingChannel $
--             deleteSignalingChannelResponse
--
--         , responseUpdateSignalingChannel $
--             updateSignalingChannelResponse
--
--         , responseUpdateStream $
--             updateStreamResponse
--
--         , responseDeleteStream $
--             deleteStreamResponse
--
--         , responseListStreams $
--             listStreamsResponse
--
--         , responseCreateStream $
--             createStreamResponse
--
--         , responseDescribeSignalingChannel $
--             describeSignalingChannelResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseTagStream $
--             tagStreamResponse
--
--         , responseCreateSignalingChannel $
--             createSignalingChannelResponse
--
--         , responseDescribeStream $
--             describeStreamResponse
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
    kinesisVideo
    (Proxy :: Proxy ListSignalingChannels)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    kinesisVideo
    (Proxy :: Proxy ListTagsForResource)

responseUntagStream :: UntagStreamResponse -> TestTree
responseUntagStream =
  res
    "UntagStreamResponse"
    "fixture/UntagStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy UntagStream)

responseUpdateDataRetention :: UpdateDataRetentionResponse -> TestTree
responseUpdateDataRetention =
  res
    "UpdateDataRetentionResponse"
    "fixture/UpdateDataRetentionResponse.proto"
    kinesisVideo
    (Proxy :: Proxy UpdateDataRetention)

responseGetDataEndpoint :: GetDataEndpointResponse -> TestTree
responseGetDataEndpoint =
  res
    "GetDataEndpointResponse"
    "fixture/GetDataEndpointResponse.proto"
    kinesisVideo
    (Proxy :: Proxy GetDataEndpoint)

responseGetSignalingChannelEndpoint :: GetSignalingChannelEndpointResponse -> TestTree
responseGetSignalingChannelEndpoint =
  res
    "GetSignalingChannelEndpointResponse"
    "fixture/GetSignalingChannelEndpointResponse.proto"
    kinesisVideo
    (Proxy :: Proxy GetSignalingChannelEndpoint)

responseListTagsForStream :: ListTagsForStreamResponse -> TestTree
responseListTagsForStream =
  res
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy ListTagsForStream)

responseDeleteSignalingChannel :: DeleteSignalingChannelResponse -> TestTree
responseDeleteSignalingChannel =
  res
    "DeleteSignalingChannelResponse"
    "fixture/DeleteSignalingChannelResponse.proto"
    kinesisVideo
    (Proxy :: Proxy DeleteSignalingChannel)

responseUpdateSignalingChannel :: UpdateSignalingChannelResponse -> TestTree
responseUpdateSignalingChannel =
  res
    "UpdateSignalingChannelResponse"
    "fixture/UpdateSignalingChannelResponse.proto"
    kinesisVideo
    (Proxy :: Proxy UpdateSignalingChannel)

responseUpdateStream :: UpdateStreamResponse -> TestTree
responseUpdateStream =
  res
    "UpdateStreamResponse"
    "fixture/UpdateStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy UpdateStream)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy DeleteStream)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    kinesisVideo
    (Proxy :: Proxy ListStreams)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy CreateStream)

responseDescribeSignalingChannel :: DescribeSignalingChannelResponse -> TestTree
responseDescribeSignalingChannel =
  res
    "DescribeSignalingChannelResponse"
    "fixture/DescribeSignalingChannelResponse.proto"
    kinesisVideo
    (Proxy :: Proxy DescribeSignalingChannel)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    kinesisVideo
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    kinesisVideo
    (Proxy :: Proxy UntagResource)

responseTagStream :: TagStreamResponse -> TestTree
responseTagStream =
  res
    "TagStreamResponse"
    "fixture/TagStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy TagStream)

responseCreateSignalingChannel :: CreateSignalingChannelResponse -> TestTree
responseCreateSignalingChannel =
  res
    "CreateSignalingChannelResponse"
    "fixture/CreateSignalingChannelResponse.proto"
    kinesisVideo
    (Proxy :: Proxy CreateSignalingChannel)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy DescribeStream)
