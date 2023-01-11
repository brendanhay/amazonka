{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodeStarConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CodeStarConnections where

import Amazonka.CodeStarConnections
import qualified Data.Proxy as Proxy
import Test.Amazonka.CodeStarConnections.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateConnection $
--             newCreateConnection
--
--         , requestCreateHost $
--             newCreateHost
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestDeleteHost $
--             newDeleteHost
--
--         , requestGetConnection $
--             newGetConnection
--
--         , requestGetHost $
--             newGetHost
--
--         , requestListConnections $
--             newListConnections
--
--         , requestListHosts $
--             newListHosts
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateHost $
--             newUpdateHost
--
--           ]

--     , testGroup "response"
--         [ responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseCreateHost $
--             newCreateHostResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseDeleteHost $
--             newDeleteHostResponse
--
--         , responseGetConnection $
--             newGetConnectionResponse
--
--         , responseGetHost $
--             newGetHostResponse
--
--         , responseListConnections $
--             newListConnectionsResponse
--
--         , responseListHosts $
--             newListHostsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateHost $
--             newUpdateHostResponse
--
--           ]
--     ]

-- Requests

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestCreateHost :: CreateHost -> TestTree
requestCreateHost =
  req
    "CreateHost"
    "fixture/CreateHost.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestDeleteHost :: DeleteHost -> TestTree
requestDeleteHost =
  req
    "DeleteHost"
    "fixture/DeleteHost.yaml"

requestGetConnection :: GetConnection -> TestTree
requestGetConnection =
  req
    "GetConnection"
    "fixture/GetConnection.yaml"

requestGetHost :: GetHost -> TestTree
requestGetHost =
  req
    "GetHost"
    "fixture/GetHost.yaml"

requestListConnections :: ListConnections -> TestTree
requestListConnections =
  req
    "ListConnections"
    "fixture/ListConnections.yaml"

requestListHosts :: ListHosts -> TestTree
requestListHosts =
  req
    "ListHosts"
    "fixture/ListHosts.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestUpdateHost :: UpdateHost -> TestTree
requestUpdateHost =
  req
    "UpdateHost"
    "fixture/UpdateHost.yaml"

-- Responses

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnection)

responseCreateHost :: CreateHostResponse -> TestTree
responseCreateHost =
  res
    "CreateHostResponse"
    "fixture/CreateHostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHost)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnection)

responseDeleteHost :: DeleteHostResponse -> TestTree
responseDeleteHost =
  res
    "DeleteHostResponse"
    "fixture/DeleteHostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHost)

responseGetConnection :: GetConnectionResponse -> TestTree
responseGetConnection =
  res
    "GetConnectionResponse"
    "fixture/GetConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnection)

responseGetHost :: GetHostResponse -> TestTree
responseGetHost =
  res
    "GetHostResponse"
    "fixture/GetHostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHost)

responseListConnections :: ListConnectionsResponse -> TestTree
responseListConnections =
  res
    "ListConnectionsResponse"
    "fixture/ListConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnections)

responseListHosts :: ListHostsResponse -> TestTree
responseListHosts =
  res
    "ListHostsResponse"
    "fixture/ListHostsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHosts)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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

responseUpdateHost :: UpdateHostResponse -> TestTree
responseUpdateHost =
  res
    "UpdateHostResponse"
    "fixture/UpdateHostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHost)
