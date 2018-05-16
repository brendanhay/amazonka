{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaPackage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.MediaPackage where

import Data.Proxy
import Network.AWS.MediaPackage
import Test.AWS.Fixture
import Test.AWS.MediaPackage.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestRotateChannelCredentials $
--             rotateChannelCredentials
--
--         , requestDescribeOriginEndpoint $
--             describeOriginEndpoint
--
--         , requestListChannels $
--             listChannels
--
--         , requestDeleteChannel $
--             deleteChannel
--
--         , requestUpdateChannel $
--             updateChannel
--
--         , requestCreateOriginEndpoint $
--             createOriginEndpoint
--
--         , requestListOriginEndpoints $
--             listOriginEndpoints
--
--         , requestCreateChannel $
--             createChannel
--
--         , requestDescribeChannel $
--             describeChannel
--
--         , requestDeleteOriginEndpoint $
--             deleteOriginEndpoint
--
--         , requestUpdateOriginEndpoint $
--             updateOriginEndpoint
--
--           ]

--     , testGroup "response"
--         [ responseRotateChannelCredentials $
--             rotateChannelCredentialsResponse
--
--         , responseDescribeOriginEndpoint $
--             describeOriginEndpointResponse
--
--         , responseListChannels $
--             listChannelsResponse
--
--         , responseDeleteChannel $
--             deleteChannelResponse
--
--         , responseUpdateChannel $
--             updateChannelResponse
--
--         , responseCreateOriginEndpoint $
--             createOriginEndpointResponse
--
--         , responseListOriginEndpoints $
--             listOriginEndpointsResponse
--
--         , responseCreateChannel $
--             createChannelResponse
--
--         , responseDescribeChannel $
--             describeChannelResponse
--
--         , responseDeleteOriginEndpoint $
--             deleteOriginEndpointResponse
--
--         , responseUpdateOriginEndpoint $
--             updateOriginEndpointResponse
--
--           ]
--     ]

-- Requests

requestRotateChannelCredentials :: RotateChannelCredentials -> TestTree
requestRotateChannelCredentials = req
    "RotateChannelCredentials"
    "fixture/RotateChannelCredentials.yaml"

requestDescribeOriginEndpoint :: DescribeOriginEndpoint -> TestTree
requestDescribeOriginEndpoint = req
    "DescribeOriginEndpoint"
    "fixture/DescribeOriginEndpoint.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels = req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel = req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestUpdateChannel :: UpdateChannel -> TestTree
requestUpdateChannel = req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestCreateOriginEndpoint :: CreateOriginEndpoint -> TestTree
requestCreateOriginEndpoint = req
    "CreateOriginEndpoint"
    "fixture/CreateOriginEndpoint.yaml"

requestListOriginEndpoints :: ListOriginEndpoints -> TestTree
requestListOriginEndpoints = req
    "ListOriginEndpoints"
    "fixture/ListOriginEndpoints.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel = req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel = req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestDeleteOriginEndpoint :: DeleteOriginEndpoint -> TestTree
requestDeleteOriginEndpoint = req
    "DeleteOriginEndpoint"
    "fixture/DeleteOriginEndpoint.yaml"

requestUpdateOriginEndpoint :: UpdateOriginEndpoint -> TestTree
requestUpdateOriginEndpoint = req
    "UpdateOriginEndpoint"
    "fixture/UpdateOriginEndpoint.yaml"

-- Responses

responseRotateChannelCredentials :: RotateChannelCredentialsResponse -> TestTree
responseRotateChannelCredentials = res
    "RotateChannelCredentialsResponse"
    "fixture/RotateChannelCredentialsResponse.proto"
    mediaPackage
    (Proxy :: Proxy RotateChannelCredentials)

responseDescribeOriginEndpoint :: DescribeOriginEndpointResponse -> TestTree
responseDescribeOriginEndpoint = res
    "DescribeOriginEndpointResponse"
    "fixture/DescribeOriginEndpointResponse.proto"
    mediaPackage
    (Proxy :: Proxy DescribeOriginEndpoint)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels = res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    mediaPackage
    (Proxy :: Proxy ListChannels)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel = res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    mediaPackage
    (Proxy :: Proxy DeleteChannel)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel = res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    mediaPackage
    (Proxy :: Proxy UpdateChannel)

responseCreateOriginEndpoint :: CreateOriginEndpointResponse -> TestTree
responseCreateOriginEndpoint = res
    "CreateOriginEndpointResponse"
    "fixture/CreateOriginEndpointResponse.proto"
    mediaPackage
    (Proxy :: Proxy CreateOriginEndpoint)

responseListOriginEndpoints :: ListOriginEndpointsResponse -> TestTree
responseListOriginEndpoints = res
    "ListOriginEndpointsResponse"
    "fixture/ListOriginEndpointsResponse.proto"
    mediaPackage
    (Proxy :: Proxy ListOriginEndpoints)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel = res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    mediaPackage
    (Proxy :: Proxy CreateChannel)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel = res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    mediaPackage
    (Proxy :: Proxy DescribeChannel)

responseDeleteOriginEndpoint :: DeleteOriginEndpointResponse -> TestTree
responseDeleteOriginEndpoint = res
    "DeleteOriginEndpointResponse"
    "fixture/DeleteOriginEndpointResponse.proto"
    mediaPackage
    (Proxy :: Proxy DeleteOriginEndpoint)

responseUpdateOriginEndpoint :: UpdateOriginEndpointResponse -> TestTree
responseUpdateOriginEndpoint = res
    "UpdateOriginEndpointResponse"
    "fixture/UpdateOriginEndpointResponse.proto"
    mediaPackage
    (Proxy :: Proxy UpdateOriginEndpoint)
