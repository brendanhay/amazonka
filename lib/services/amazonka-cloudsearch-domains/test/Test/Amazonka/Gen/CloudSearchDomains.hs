{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudSearchDomains
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CloudSearchDomains where

import Amazonka.CloudSearchDomains
import qualified Data.Proxy as Proxy
import Test.Amazonka.CloudSearchDomains.Internal
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
--         [ requestSearch $
--             newSearch
--
--         , requestSuggest $
--             newSuggest
--
--         , requestUploadDocuments $
--             newUploadDocuments
--
--           ]

--     , testGroup "response"
--         [ responseSearch $
--             newSearchResponse
--
--         , responseSuggest $
--             newSuggestResponse
--
--         , responseUploadDocuments $
--             newUploadDocumentsResponse
--
--           ]
--     ]

-- Requests

requestSearch :: Search -> TestTree
requestSearch =
  req
    "Search"
    "fixture/Search.yaml"

requestSuggest :: Suggest -> TestTree
requestSuggest =
  req
    "Suggest"
    "fixture/Suggest.yaml"

-- Responses

responseSearch :: SearchResponse -> TestTree
responseSearch =
  res
    "SearchResponse"
    "fixture/SearchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Search)

responseSuggest :: SuggestResponse -> TestTree
responseSuggest =
  res
    "SuggestResponse"
    "fixture/SuggestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Suggest)

responseUploadDocuments :: UploadDocumentsResponse -> TestTree
responseUploadDocuments =
  res
    "UploadDocumentsResponse"
    "fixture/UploadDocumentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadDocuments)
