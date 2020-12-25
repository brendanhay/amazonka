{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.LexRuntime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.LexRuntime where

import Data.Proxy
import Network.AWS.LexRuntime
import Test.AWS.Fixture
import Test.AWS.LexRuntime.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutSession $
--             mkPutSession
--
--         , requestDeleteSession $
--             mkDeleteSession
--
--         , requestPostText $
--             mkPostText
--
--         , requestPostContent $
--             mkPostContent
--
--         , requestGetSession $
--             mkGetSession
--
--           ]

--     , testGroup "response"
--         [ responsePutSession $
--             mkPutSessionResponse
--
--         , responseDeleteSession $
--             mkDeleteSessionResponse
--
--         , responsePostText $
--             mkPostTextResponse
--
--         , responsePostContent $
--             mkPostContentResponse
--
--         , responseGetSession $
--             mkGetSessionResponse
--
--           ]
--     ]

-- Requests

requestPutSession :: PutSession -> TestTree
requestPutSession =
  req
    "PutSession"
    "fixture/PutSession.yaml"

requestDeleteSession :: DeleteSession -> TestTree
requestDeleteSession =
  req
    "DeleteSession"
    "fixture/DeleteSession.yaml"

requestPostText :: PostText -> TestTree
requestPostText =
  req
    "PostText"
    "fixture/PostText.yaml"

requestGetSession :: GetSession -> TestTree
requestGetSession =
  req
    "GetSession"
    "fixture/GetSession.yaml"

-- Responses

responseDeleteSession :: DeleteSessionResponse -> TestTree
responseDeleteSession =
  res
    "DeleteSessionResponse"
    "fixture/DeleteSessionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSession)

responsePostText :: PostTextResponse -> TestTree
responsePostText =
  res
    "PostTextResponse"
    "fixture/PostTextResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PostText)

responseGetSession :: GetSessionResponse -> TestTree
responseGetSession =
  res
    "GetSessionResponse"
    "fixture/GetSessionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSession)
