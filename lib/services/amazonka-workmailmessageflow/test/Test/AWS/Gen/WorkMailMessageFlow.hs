{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkMailMessageFlow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.WorkMailMessageFlow where

import qualified Data.Proxy as Proxy
import Network.AWS.WorkMailMessageFlow
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.WorkMailMessageFlow.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutRawMessageContent $
--             newPutRawMessageContent
--
--         , requestGetRawMessageContent $
--             newGetRawMessageContent
--
--           ]

--     , testGroup "response"
--         [ responsePutRawMessageContent $
--             newPutRawMessageContentResponse
--
--         , responseGetRawMessageContent $
--             newGetRawMessageContentResponse
--
--           ]
--     ]

-- Requests

requestPutRawMessageContent :: PutRawMessageContent -> TestTree
requestPutRawMessageContent =
  req
    "PutRawMessageContent"
    "fixture/PutRawMessageContent.yaml"

requestGetRawMessageContent :: GetRawMessageContent -> TestTree
requestGetRawMessageContent =
  req
    "GetRawMessageContent"
    "fixture/GetRawMessageContent.yaml"

-- Responses

responsePutRawMessageContent :: PutRawMessageContentResponse -> TestTree
responsePutRawMessageContent =
  res
    "PutRawMessageContentResponse"
    "fixture/PutRawMessageContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRawMessageContent)
