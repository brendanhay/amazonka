{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.LexRuntime
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestPostText $
--             postText
--
--         , requestPostContent $
--             postContent
--
--           ]

--     , testGroup "response"
--         [ responsePostText $
--             postTextResponse
--
--         , responsePostContent $
--             postContentResponse
--
--           ]
--     ]

-- Requests

requestPostText :: PostText -> TestTree
requestPostText = req
    "PostText"
    "fixture/PostText.yaml"

-- Responses

responsePostText :: PostTextResponse -> TestTree
responsePostText = res
    "PostTextResponse"
    "fixture/PostTextResponse.proto"
    lexRuntime
    (Proxy :: Proxy PostText)
