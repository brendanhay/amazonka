{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudSearchDomains
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CloudSearchDomains where

import Data.Proxy
import Network.AWS.CloudSearchDomains
import Test.AWS.CloudSearchDomains.Internal
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
--         [ requestUploadDocuments $
--             newUploadDocuments
--
--         , requestSearch $
--             newSearch
--
--         , requestSuggest $
--             newSuggest
--
--           ]

--     , testGroup "response"
--         [ responseUploadDocuments $
--             newUploadDocumentsResponse
--
--         , responseSearch $
--             newSearchResponse
--
--         , responseSuggest $
--             newSuggestResponse
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

responseUploadDocuments :: UploadDocumentsResponse -> TestTree
responseUploadDocuments =
  res
    "UploadDocumentsResponse"
    "fixture/UploadDocumentsResponse.proto"
    defaultService
    (Proxy :: Proxy UploadDocuments)

responseSearch :: SearchResponse -> TestTree
responseSearch =
  res
    "SearchResponse"
    "fixture/SearchResponse.proto"
    defaultService
    (Proxy :: Proxy Search)

responseSuggest :: SuggestResponse -> TestTree
responseSuggest =
  res
    "SuggestResponse"
    "fixture/SuggestResponse.proto"
    defaultService
    (Proxy :: Proxy Suggest)
