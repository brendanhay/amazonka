{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTSecureTunneling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.IoTSecureTunneling where

import Data.Proxy
import Network.AWS.IoTSecureTunneling
import Test.AWS.Fixture
import Test.AWS.IoTSecureTunneling.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCloseTunnel $
--             newCloseTunnel
--
--         , requestOpenTunnel $
--             newOpenTunnel
--
--         , requestDescribeTunnel $
--             newDescribeTunnel
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListTunnels $
--             newListTunnels
--
--           ]

--     , testGroup "response"
--         [ responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCloseTunnel $
--             newCloseTunnelResponse
--
--         , responseOpenTunnel $
--             newOpenTunnelResponse
--
--         , responseDescribeTunnel $
--             newDescribeTunnelResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListTunnels $
--             newListTunnelsResponse
--
--           ]
--     ]

-- Requests

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCloseTunnel :: CloseTunnel -> TestTree
requestCloseTunnel =
  req
    "CloseTunnel"
    "fixture/CloseTunnel.yaml"

requestOpenTunnel :: OpenTunnel -> TestTree
requestOpenTunnel =
  req
    "OpenTunnel"
    "fixture/OpenTunnel.yaml"

requestDescribeTunnel :: DescribeTunnel -> TestTree
requestDescribeTunnel =
  req
    "DescribeTunnel"
    "fixture/DescribeTunnel.yaml"

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

requestListTunnels :: ListTunnels -> TestTree
requestListTunnels =
  req
    "ListTunnels"
    "fixture/ListTunnels.yaml"

-- Responses

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCloseTunnel :: CloseTunnelResponse -> TestTree
responseCloseTunnel =
  res
    "CloseTunnelResponse"
    "fixture/CloseTunnelResponse.proto"
    defaultService
    (Proxy :: Proxy CloseTunnel)

responseOpenTunnel :: OpenTunnelResponse -> TestTree
responseOpenTunnel =
  res
    "OpenTunnelResponse"
    "fixture/OpenTunnelResponse.proto"
    defaultService
    (Proxy :: Proxy OpenTunnel)

responseDescribeTunnel :: DescribeTunnelResponse -> TestTree
responseDescribeTunnel =
  res
    "DescribeTunnelResponse"
    "fixture/DescribeTunnelResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTunnel)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListTunnels :: ListTunnelsResponse -> TestTree
responseListTunnels =
  res
    "ListTunnelsResponse"
    "fixture/ListTunnelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTunnels)
