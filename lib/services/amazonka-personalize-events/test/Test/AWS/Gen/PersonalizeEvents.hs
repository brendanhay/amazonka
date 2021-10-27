{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.PersonalizeEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.PersonalizeEvents where

import Data.Proxy
import Network.AWS.PersonalizeEvents
import Test.AWS.Fixture
import Test.AWS.PersonalizeEvents.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutUsers $
--             newPutUsers
--
--         , requestPutItems $
--             newPutItems
--
--         , requestPutEvents $
--             newPutEvents
--
--           ]

--     , testGroup "response"
--         [ responsePutUsers $
--             newPutUsersResponse
--
--         , responsePutItems $
--             newPutItemsResponse
--
--         , responsePutEvents $
--             newPutEventsResponse
--
--           ]
--     ]

-- Requests

requestPutUsers :: PutUsers -> TestTree
requestPutUsers =
  req
    "PutUsers"
    "fixture/PutUsers.yaml"

requestPutItems :: PutItems -> TestTree
requestPutItems =
  req
    "PutItems"
    "fixture/PutItems.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents =
  req
    "PutEvents"
    "fixture/PutEvents.yaml"

-- Responses

responsePutUsers :: PutUsersResponse -> TestTree
responsePutUsers =
  res
    "PutUsersResponse"
    "fixture/PutUsersResponse.proto"
    defaultService
    (Proxy :: Proxy PutUsers)

responsePutItems :: PutItemsResponse -> TestTree
responsePutItems =
  res
    "PutItemsResponse"
    "fixture/PutItemsResponse.proto"
    defaultService
    (Proxy :: Proxy PutItems)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    defaultService
    (Proxy :: Proxy PutEvents)
