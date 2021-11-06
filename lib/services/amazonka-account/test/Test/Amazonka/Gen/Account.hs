{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Account
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Account where

import Amazonka.Account
import qualified Data.Proxy as Proxy
import Test.Amazonka.Account.Internal
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
--         [ requestDeleteAlternateContact $
--             newDeleteAlternateContact
--
--         , requestPutAlternateContact $
--             newPutAlternateContact
--
--         , requestGetAlternateContact $
--             newGetAlternateContact
--
--           ]

--     , testGroup "response"
--         [ responseDeleteAlternateContact $
--             newDeleteAlternateContactResponse
--
--         , responsePutAlternateContact $
--             newPutAlternateContactResponse
--
--         , responseGetAlternateContact $
--             newGetAlternateContactResponse
--
--           ]
--     ]

-- Requests

requestDeleteAlternateContact :: DeleteAlternateContact -> TestTree
requestDeleteAlternateContact =
  req
    "DeleteAlternateContact"
    "fixture/DeleteAlternateContact.yaml"

requestPutAlternateContact :: PutAlternateContact -> TestTree
requestPutAlternateContact =
  req
    "PutAlternateContact"
    "fixture/PutAlternateContact.yaml"

requestGetAlternateContact :: GetAlternateContact -> TestTree
requestGetAlternateContact =
  req
    "GetAlternateContact"
    "fixture/GetAlternateContact.yaml"

-- Responses

responseDeleteAlternateContact :: DeleteAlternateContactResponse -> TestTree
responseDeleteAlternateContact =
  res
    "DeleteAlternateContactResponse"
    "fixture/DeleteAlternateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlternateContact)

responsePutAlternateContact :: PutAlternateContactResponse -> TestTree
responsePutAlternateContact =
  res
    "PutAlternateContactResponse"
    "fixture/PutAlternateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAlternateContact)

responseGetAlternateContact :: GetAlternateContactResponse -> TestTree
responseGetAlternateContact =
  res
    "GetAlternateContactResponse"
    "fixture/GetAlternateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAlternateContact)
