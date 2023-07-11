{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoTSecureTunneling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoTSecureTunneling where

import Amazonka.IoTSecureTunneling
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoTSecureTunneling.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCloseTunnel $
--             newCloseTunnel
--
--         , requestDescribeTunnel $
--             newDescribeTunnel
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTunnels $
--             newListTunnels
--
--         , requestOpenTunnel $
--             newOpenTunnel
--
--         , requestRotateTunnelAccessToken $
--             newRotateTunnelAccessToken
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCloseTunnel $
--             newCloseTunnelResponse
--
--         , responseDescribeTunnel $
--             newDescribeTunnelResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTunnels $
--             newListTunnelsResponse
--
--         , responseOpenTunnel $
--             newOpenTunnelResponse
--
--         , responseRotateTunnelAccessToken $
--             newRotateTunnelAccessTokenResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestCloseTunnel :: CloseTunnel -> TestTree
requestCloseTunnel =
  req
    "CloseTunnel"
    "fixture/CloseTunnel.yaml"

requestDescribeTunnel :: DescribeTunnel -> TestTree
requestDescribeTunnel =
  req
    "DescribeTunnel"
    "fixture/DescribeTunnel.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTunnels :: ListTunnels -> TestTree
requestListTunnels =
  req
    "ListTunnels"
    "fixture/ListTunnels.yaml"

requestOpenTunnel :: OpenTunnel -> TestTree
requestOpenTunnel =
  req
    "OpenTunnel"
    "fixture/OpenTunnel.yaml"

requestRotateTunnelAccessToken :: RotateTunnelAccessToken -> TestTree
requestRotateTunnelAccessToken =
  req
    "RotateTunnelAccessToken"
    "fixture/RotateTunnelAccessToken.yaml"

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

-- Responses

responseCloseTunnel :: CloseTunnelResponse -> TestTree
responseCloseTunnel =
  res
    "CloseTunnelResponse"
    "fixture/CloseTunnelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CloseTunnel)

responseDescribeTunnel :: DescribeTunnelResponse -> TestTree
responseDescribeTunnel =
  res
    "DescribeTunnelResponse"
    "fixture/DescribeTunnelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTunnel)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTunnels :: ListTunnelsResponse -> TestTree
responseListTunnels =
  res
    "ListTunnelsResponse"
    "fixture/ListTunnelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTunnels)

responseOpenTunnel :: OpenTunnelResponse -> TestTree
responseOpenTunnel =
  res
    "OpenTunnelResponse"
    "fixture/OpenTunnelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy OpenTunnel)

responseRotateTunnelAccessToken :: RotateTunnelAccessTokenResponse -> TestTree
responseRotateTunnelAccessToken =
  res
    "RotateTunnelAccessTokenResponse"
    "fixture/RotateTunnelAccessTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RotateTunnelAccessToken)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)
