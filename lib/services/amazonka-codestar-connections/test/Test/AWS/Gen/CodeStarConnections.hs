{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeStarConnections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CodeStarConnections where

import qualified Data.Proxy as Proxy
import Network.AWS.CodeStarConnections
import Test.AWS.CodeStarConnections.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateHost $
--             newCreateHost
--
--         , requestListConnections $
--             newListConnections
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestGetConnection $
--             newGetConnection
--
--         , requestDeleteHost $
--             newDeleteHost
--
--         , requestUpdateHost $
--             newUpdateHost
--
--         , requestListHosts $
--             newListHosts
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetHost $
--             newGetHost
--
--           ]

--     , testGroup "response"
--         [ responseCreateHost $
--             newCreateHostResponse
--
--         , responseListConnections $
--             newListConnectionsResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseGetConnection $
--             newGetConnectionResponse
--
--         , responseDeleteHost $
--             newDeleteHostResponse
--
--         , responseUpdateHost $
--             newUpdateHostResponse
--
--         , responseListHosts $
--             newListHostsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetHost $
--             newGetHostResponse
--
--           ]
--     ]

-- Requests

requestCreateHost :: CreateHost -> TestTree
requestCreateHost =
  req
    "CreateHost"
    "fixture/CreateHost.yaml"

requestListConnections :: ListConnections -> TestTree
requestListConnections =
  req
    "ListConnections"
    "fixture/ListConnections.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestGetConnection :: GetConnection -> TestTree
requestGetConnection =
  req
    "GetConnection"
    "fixture/GetConnection.yaml"

requestDeleteHost :: DeleteHost -> TestTree
requestDeleteHost =
  req
    "DeleteHost"
    "fixture/DeleteHost.yaml"

requestUpdateHost :: UpdateHost -> TestTree
requestUpdateHost =
  req
    "UpdateHost"
    "fixture/UpdateHost.yaml"

requestListHosts :: ListHosts -> TestTree
requestListHosts =
  req
    "ListHosts"
    "fixture/ListHosts.yaml"

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

requestGetHost :: GetHost -> TestTree
requestGetHost =
  req
    "GetHost"
    "fixture/GetHost.yaml"

-- Responses

responseCreateHost :: CreateHostResponse -> TestTree
responseCreateHost =
  res
    "CreateHostResponse"
    "fixture/CreateHostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHost)

responseListConnections :: ListConnectionsResponse -> TestTree
responseListConnections =
  res
    "ListConnectionsResponse"
    "fixture/ListConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnections)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnection)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnection)

responseGetConnection :: GetConnectionResponse -> TestTree
responseGetConnection =
  res
    "GetConnectionResponse"
    "fixture/GetConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnection)

responseDeleteHost :: DeleteHostResponse -> TestTree
responseDeleteHost =
  res
    "DeleteHostResponse"
    "fixture/DeleteHostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHost)

responseUpdateHost :: UpdateHostResponse -> TestTree
responseUpdateHost =
  res
    "UpdateHostResponse"
    "fixture/UpdateHostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHost)

responseListHosts :: ListHostsResponse -> TestTree
responseListHosts =
  res
    "ListHostsResponse"
    "fixture/ListHostsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHosts)

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

responseGetHost :: GetHostResponse -> TestTree
responseGetHost =
  res
    "GetHostResponse"
    "fixture/GetHostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHost)
