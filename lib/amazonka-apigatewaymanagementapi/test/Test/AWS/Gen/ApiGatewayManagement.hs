{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ApiGatewayManagement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ApiGatewayManagement where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ApiGatewayManagement
import Test.AWS.ApiGatewayManagement.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteConnection $
--             mkDeleteConnection
--
--         , requestGetConnection $
--             mkGetConnection
--
--         , requestPostToConnection $
--             mkPostToConnection
--
--           ]

--     , testGroup "response"
--         [ responseDeleteConnection $
--             mkDeleteConnectionResponse
--
--         , responseGetConnection $
--             mkGetConnectionResponse
--
--         , responsePostToConnection $
--             mkPostToConnectionResponse
--
--           ]
--     ]

-- Requests

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection = req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestGetConnection :: GetConnection -> TestTree
requestGetConnection = req
    "GetConnection"
    "fixture/GetConnection.yaml"

requestPostToConnection :: PostToConnection -> TestTree
requestPostToConnection = req
    "PostToConnection"
    "fixture/PostToConnection.yaml"

-- Responses

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection = res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteConnection)

responseGetConnection :: GetConnectionResponse -> TestTree
responseGetConnection = res
    "GetConnectionResponse"
    "fixture/GetConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetConnection)

responsePostToConnection :: PostToConnectionResponse -> TestTree
responsePostToConnection = res
    "PostToConnectionResponse"
    "fixture/PostToConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PostToConnection)
