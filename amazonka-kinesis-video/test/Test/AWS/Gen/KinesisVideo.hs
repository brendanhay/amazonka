{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.KinesisVideo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestUntagStream $
--             untagStream
--
--         , requestUpdateDataRetention $
--             updateDataRetention
--
--         , requestGetDataEndpoint $
--             getDataEndpoint
--
--         , requestListTagsForStream $
--             listTagsForStream
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
--         , requestTagStream $
--             tagStream
--
--         , requestDescribeStream $
--             describeStream
--
--           ]

--     , testGroup "response"
--         [ responseUntagStream $
--             untagStreamResponse
--
--         , responseUpdateDataRetention $
--             updateDataRetentionResponse
--
--         , responseGetDataEndpoint $
--             getDataEndpointResponse
--
--         , responseListTagsForStream $
--             listTagsForStreamResponse
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
--         , responseTagStream $
--             tagStreamResponse
--
--         , responseDescribeStream $
--             describeStreamResponse
--
--           ]
--     ]

-- Requests

requestUntagStream :: UntagStream -> TestTree
requestUntagStream = req
    "UntagStream"
    "fixture/UntagStream.yaml"

requestUpdateDataRetention :: UpdateDataRetention -> TestTree
requestUpdateDataRetention = req
    "UpdateDataRetention"
    "fixture/UpdateDataRetention.yaml"

requestGetDataEndpoint :: GetDataEndpoint -> TestTree
requestGetDataEndpoint = req
    "GetDataEndpoint"
    "fixture/GetDataEndpoint.yaml"

requestListTagsForStream :: ListTagsForStream -> TestTree
requestListTagsForStream = req
    "ListTagsForStream"
    "fixture/ListTagsForStream.yaml"

requestUpdateStream :: UpdateStream -> TestTree
requestUpdateStream = req
    "UpdateStream"
    "fixture/UpdateStream.yaml"

requestDeleteStream :: DeleteStream -> TestTree
requestDeleteStream = req
    "DeleteStream"
    "fixture/DeleteStream.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams = req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestCreateStream :: CreateStream -> TestTree
requestCreateStream = req
    "CreateStream"
    "fixture/CreateStream.yaml"

requestTagStream :: TagStream -> TestTree
requestTagStream = req
    "TagStream"
    "fixture/TagStream.yaml"

requestDescribeStream :: DescribeStream -> TestTree
requestDescribeStream = req
    "DescribeStream"
    "fixture/DescribeStream.yaml"

-- Responses

responseUntagStream :: UntagStreamResponse -> TestTree
responseUntagStream = res
    "UntagStreamResponse"
    "fixture/UntagStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy UntagStream)

responseUpdateDataRetention :: UpdateDataRetentionResponse -> TestTree
responseUpdateDataRetention = res
    "UpdateDataRetentionResponse"
    "fixture/UpdateDataRetentionResponse.proto"
    kinesisVideo
    (Proxy :: Proxy UpdateDataRetention)

responseGetDataEndpoint :: GetDataEndpointResponse -> TestTree
responseGetDataEndpoint = res
    "GetDataEndpointResponse"
    "fixture/GetDataEndpointResponse.proto"
    kinesisVideo
    (Proxy :: Proxy GetDataEndpoint)

responseListTagsForStream :: ListTagsForStreamResponse -> TestTree
responseListTagsForStream = res
    "ListTagsForStreamResponse"
    "fixture/ListTagsForStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy ListTagsForStream)

responseUpdateStream :: UpdateStreamResponse -> TestTree
responseUpdateStream = res
    "UpdateStreamResponse"
    "fixture/UpdateStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy UpdateStream)

responseDeleteStream :: DeleteStreamResponse -> TestTree
responseDeleteStream = res
    "DeleteStreamResponse"
    "fixture/DeleteStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy DeleteStream)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams = res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    kinesisVideo
    (Proxy :: Proxy ListStreams)

responseCreateStream :: CreateStreamResponse -> TestTree
responseCreateStream = res
    "CreateStreamResponse"
    "fixture/CreateStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy CreateStream)

responseTagStream :: TagStreamResponse -> TestTree
responseTagStream = res
    "TagStreamResponse"
    "fixture/TagStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy TagStream)

responseDescribeStream :: DescribeStreamResponse -> TestTree
responseDescribeStream = res
    "DescribeStreamResponse"
    "fixture/DescribeStreamResponse.proto"
    kinesisVideo
    (Proxy :: Proxy DescribeStream)
