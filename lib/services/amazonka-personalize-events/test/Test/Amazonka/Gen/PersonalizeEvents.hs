{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.PersonalizeEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.PersonalizeEvents where

import Amazonka.PersonalizeEvents
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.PersonalizeEvents.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutEvents $
--             newPutEvents
--
--         , requestPutItems $
--             newPutItems
--
--         , requestPutUsers $
--             newPutUsers
--
--           ]

--     , testGroup "response"
--         [ responsePutEvents $
--             newPutEventsResponse
--
--         , responsePutItems $
--             newPutItemsResponse
--
--         , responsePutUsers $
--             newPutUsersResponse
--
--           ]
--     ]

-- Requests

requestPutEvents :: PutEvents -> TestTree
requestPutEvents =
  req
    "PutEvents"
    "fixture/PutEvents.yaml"

requestPutItems :: PutItems -> TestTree
requestPutItems =
  req
    "PutItems"
    "fixture/PutItems.yaml"

requestPutUsers :: PutUsers -> TestTree
requestPutUsers =
  req
    "PutUsers"
    "fixture/PutUsers.yaml"

-- Responses

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEvents)

responsePutItems :: PutItemsResponse -> TestTree
responsePutItems =
  res
    "PutItemsResponse"
    "fixture/PutItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutItems)

responsePutUsers :: PutUsersResponse -> TestTree
responsePutUsers =
  res
    "PutUsersResponse"
    "fixture/PutUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutUsers)
