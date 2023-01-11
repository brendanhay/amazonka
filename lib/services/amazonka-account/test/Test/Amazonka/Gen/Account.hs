{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Account
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         , requestGetAlternateContact $
--             newGetAlternateContact
--
--         , requestGetContactInformation $
--             newGetContactInformation
--
--         , requestPutAlternateContact $
--             newPutAlternateContact
--
--         , requestPutContactInformation $
--             newPutContactInformation
--
--           ]

--     , testGroup "response"
--         [ responseDeleteAlternateContact $
--             newDeleteAlternateContactResponse
--
--         , responseGetAlternateContact $
--             newGetAlternateContactResponse
--
--         , responseGetContactInformation $
--             newGetContactInformationResponse
--
--         , responsePutAlternateContact $
--             newPutAlternateContactResponse
--
--         , responsePutContactInformation $
--             newPutContactInformationResponse
--
--           ]
--     ]

-- Requests

requestDeleteAlternateContact :: DeleteAlternateContact -> TestTree
requestDeleteAlternateContact =
  req
    "DeleteAlternateContact"
    "fixture/DeleteAlternateContact.yaml"

requestGetAlternateContact :: GetAlternateContact -> TestTree
requestGetAlternateContact =
  req
    "GetAlternateContact"
    "fixture/GetAlternateContact.yaml"

requestGetContactInformation :: GetContactInformation -> TestTree
requestGetContactInformation =
  req
    "GetContactInformation"
    "fixture/GetContactInformation.yaml"

requestPutAlternateContact :: PutAlternateContact -> TestTree
requestPutAlternateContact =
  req
    "PutAlternateContact"
    "fixture/PutAlternateContact.yaml"

requestPutContactInformation :: PutContactInformation -> TestTree
requestPutContactInformation =
  req
    "PutContactInformation"
    "fixture/PutContactInformation.yaml"

-- Responses

responseDeleteAlternateContact :: DeleteAlternateContactResponse -> TestTree
responseDeleteAlternateContact =
  res
    "DeleteAlternateContactResponse"
    "fixture/DeleteAlternateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlternateContact)

responseGetAlternateContact :: GetAlternateContactResponse -> TestTree
responseGetAlternateContact =
  res
    "GetAlternateContactResponse"
    "fixture/GetAlternateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAlternateContact)

responseGetContactInformation :: GetContactInformationResponse -> TestTree
responseGetContactInformation =
  res
    "GetContactInformationResponse"
    "fixture/GetContactInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContactInformation)

responsePutAlternateContact :: PutAlternateContactResponse -> TestTree
responsePutAlternateContact =
  res
    "PutAlternateContactResponse"
    "fixture/PutAlternateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAlternateContact)

responsePutContactInformation :: PutContactInformationResponse -> TestTree
responsePutContactInformation =
  res
    "PutContactInformationResponse"
    "fixture/PutContactInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutContactInformation)
