{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.S3Outposts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.S3Outposts where

import Amazonka.S3Outposts
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.S3Outposts.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateEndpoint $
--             newCreateEndpoint
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestListEndpoints $
--             newListEndpoints
--
--         , requestListSharedEndpoints $
--             newListSharedEndpoints
--
--           ]

--     , testGroup "response"
--         [ responseCreateEndpoint $
--             newCreateEndpointResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseListEndpoints $
--             newListEndpointsResponse
--
--         , responseListSharedEndpoints $
--             newListSharedEndpointsResponse
--
--           ]
--     ]

-- Requests

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint =
  req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestListEndpoints :: ListEndpoints -> TestTree
requestListEndpoints =
  req
    "ListEndpoints"
    "fixture/ListEndpoints.yaml"

requestListSharedEndpoints :: ListSharedEndpoints -> TestTree
requestListSharedEndpoints =
  req
    "ListSharedEndpoints"
    "fixture/ListSharedEndpoints.yaml"

-- Responses

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpoint)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpoint)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpoints)

responseListSharedEndpoints :: ListSharedEndpointsResponse -> TestTree
responseListSharedEndpoints =
  res
    "ListSharedEndpointsResponse"
    "fixture/ListSharedEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSharedEndpoints)
