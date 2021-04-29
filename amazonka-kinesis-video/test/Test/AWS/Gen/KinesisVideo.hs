{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.KinesisVideo
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestListTagsForStream $
--             newListTagsForStream
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetDataEndpoint $
--             newGetDataEndpoint
--
--         , requestUpdateStream $
--             newUpdateStream
--
--         , requestDeleteStream $
--             newDeleteStream
--
--         , requestUntagStream $
--             newUntagStream
--
--         , requestUpdateDataRetention $
--             newUpdateDataRetention
--
--         , requestUpdateSignalingChannel $
--             newUpdateSignalingChannel
--
--         , requestDeleteSignalingChannel $
--             newDeleteSignalingChannel
--
--         , requestListSignalingChannels $
--             newListSignalingChannels
--
--         , requestCreateSignalingChannel $
--             newCreateSignalingChannel
--
--         , requestDescribeStream $
--             newDescribeStream
--
--         , requestTagStream $
--             newTagStream
--
--         , requestGetSignalingChannelEndpoint $
--             newGetSignalingChannelEndpoint
--
--         , requestDescribeSignalingChannel $
--             newDescribeSignalingChannel
--
--         , requestCreateStream $
--             newCreateStream
--
--         , requestListStreams $
--             newListStreams
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseListTagsForStream $
--             newListTagsForStreamResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetDataEndpoint $
--             newGetDataEndpointResponse
--
--         , responseUpdateStream $
--             newUpdateStreamResponse
--
--         , responseDeleteStream $
--             newDeleteStreamResponse
--
--         , responseUntagStream $
--             newUntagStreamResponse
--
--         , responseUpdateDataRetention $
--             newUpdateDataRetentionResponse
--
--         , responseUpdateSignalingChannel $
--             newUpdateSignalingChannelResponse
--
--         , responseDeleteSignalingChannel $
--             newDeleteSignalingChannelResponse
--
--         , responseListSignalingChannels $
--             newListSignalingChannelsResponse
--
--         , responseCreateSignalingChannel $
--             newCreateSignalingChannelResponse
--
--         , responseDescribeStream $
--             newDescribeStreamResponse
--
--         , responseTagStream $
--             newTagStreamResponse
--
--         , responseGetSignalingChannelEndpoint $
--             newGetSignalingChannelEndpointResponse
--
--         , responseDescribeSignalingChannel $
--             newDescribeSignalingChannelResponse
--
--         , responseCreateStream $
--             newCreateStreamResponse
--
--         , responseListStreams $
--             newListStreamsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestListTagsForStream :: ListTagsForStream -> TestTree
requestListTagsForStream =
  req
    "ListTagsForStream"
    "fixture/ListTagsForStream.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetDataEndpoint :: GetDataEndpoint -> TestTree
requestGetDataEndpoint =
  req
    "GetDataEndpoint"
    "fixture/GetDataEndpoint.yaml"

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

requestUpdateSignalingChannel :: UpdateSignalingChannel -> TestTree
requestUpdateSignalingChannel =
  req
    "UpdateSignalingChannel"
    "fixture/UpdateSignalingChannel.yaml"

requestDeleteSignalingChannel :: DeleteSignalingChannel -> TestTree
requestDeleteSignalingChannel =
  req
    "DeleteSignalingChannel"
    "fixture/DeleteSignalingChannel.yaml"

requestListSignalingChannels :: ListSignalingChannels -> TestTree
requestListSignalingChannels =
  req
    "ListSignalingChannels"
    "fixture/ListSignalingChannels.yaml"

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

requestTagStream :: TagStream -> TestTree
requestTagStream =
  req
    "TagStream"
    "fixture/TagStream.yaml"

requestGetSignalingChannelEndpoint :: GetSignalingChannelEndpoint -> TestTree
requestGetSignalingChannelEndpoint =
  req
    "GetSignalingChannelEndpoint"
    "fixture/GetSignalingChannelEndpoint.yaml"

requestDescribeSignalingChannel :: DescribeSignalingChannel -> TestTree
requestDescribeSignalingChannel =
  req
    "DescribeSignalingChannel"
    "fixture/DescribeSignalingChannel.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream =
  req
    "CreateStream"
    "fixture/CreateStream.yaml"

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

-- Responses

responseListTagsForStream :: ListTagsForStreamResponse -> TestTree
responseListTagsForStream =
  res
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForStream)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetDataEndpoint :: GetDataEndpointResponse -> TestTree
responseGetDataEndpoint =
  res
    "GetDataEndpointResponse"
    "fixture/GetDataEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy GetDataEndpoint)

responseUpdateStream :: UpdateStreamResponse -> TestTree
responseUpdateStream =
  res
    "UpdateStreamResponse"
    "fixture/UpdateStreamResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStream)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream =
  res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStream)

responseUntagStream :: UntagStreamResponse -> TestTree
responseUntagStream =
  res
    "UntagStreamResponse"
    "fixture/UntagStreamResponse.proto"
    defaultService
    (Proxy :: Proxy UntagStream)

responseUpdateDataRetention :: UpdateDataRetentionResponse -> TestTree
responseUpdateDataRetention =
  res
    "UpdateDataRetentionResponse"
    "fixture/UpdateDataRetentionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDataRetention)

responseUpdateSignalingChannel :: UpdateSignalingChannelResponse -> TestTree
responseUpdateSignalingChannel =
  res
    "UpdateSignalingChannelResponse"
    "fixture/UpdateSignalingChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSignalingChannel)

responseDeleteSignalingChannel :: DeleteSignalingChannelResponse -> TestTree
responseDeleteSignalingChannel =
  res
    "DeleteSignalingChannelResponse"
    "fixture/DeleteSignalingChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSignalingChannel)

responseListSignalingChannels :: ListSignalingChannelsResponse -> TestTree
responseListSignalingChannels =
  res
    "ListSignalingChannelsResponse"
    "fixture/ListSignalingChannelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSignalingChannels)

responseCreateSignalingChannel :: CreateSignalingChannelResponse -> TestTree
responseCreateSignalingChannel =
  res
    "CreateSignalingChannelResponse"
    "fixture/CreateSignalingChannelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSignalingChannel)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream =
  res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStream)

responseTagStream :: TagStreamResponse -> TestTree
responseTagStream =
  res
    "TagStreamResponse"
    "fixture/TagStreamResponse.proto"
    defaultService
    (Proxy :: Proxy TagStream)

responseGetSignalingChannelEndpoint :: GetSignalingChannelEndpointResponse -> TestTree
responseGetSignalingChannelEndpoint =
  res
    "GetSignalingChannelEndpointResponse"
    "fixture/GetSignalingChannelEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy GetSignalingChannelEndpoint)

responseDescribeSignalingChannel :: DescribeSignalingChannelResponse -> TestTree
responseDescribeSignalingChannel =
  res
    "DescribeSignalingChannelResponse"
    "fixture/DescribeSignalingChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSignalingChannel)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream =
  res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStream)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStreams)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)
