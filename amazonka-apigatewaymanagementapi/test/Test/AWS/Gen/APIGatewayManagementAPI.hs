{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.APIGatewayManagementAPI
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.APIGatewayManagementAPI where

import Data.Proxy
import Network.AWS.APIGatewayManagementAPI
import Test.AWS.APIGatewayManagementAPI.Internal
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
--         [ requestDeleteConnection $
--             newDeleteConnection
--
--         , requestPostToConnection $
--             newPostToConnection
--
--         , requestGetConnection $
--             newGetConnection
--
--           ]

--     , testGroup "response"
--         [ responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responsePostToConnection $
--             newPostToConnectionResponse
--
--         , responseGetConnection $
--             newGetConnectionResponse
--
--           ]
--     ]

-- Requests

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestPostToConnection :: PostToConnection -> TestTree
requestPostToConnection =
  req
    "PostToConnection"
    "fixture/PostToConnection.yaml"

requestGetConnection :: GetConnection -> TestTree
requestGetConnection =
  req
    "GetConnection"
    "fixture/GetConnection.yaml"

-- Responses

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnection)

responsePostToConnection :: PostToConnectionResponse -> TestTree
responsePostToConnection =
  res
    "PostToConnectionResponse"
    "fixture/PostToConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy PostToConnection)

responseGetConnection :: GetConnectionResponse -> TestTree
responseGetConnection =
  res
    "GetConnectionResponse"
    "fixture/GetConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnection)
