{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudSearchDomains
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudSearchDomains where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CloudSearchDomains
import Test.AWS.CloudSearchDomains.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testSuggest $
--             suggest
--
--         , testUploadDocuments $
--             uploadDocuments
--
--         , testSearch $
--             search
--
--           ]

--     , testGroup "response"
--         [ testSuggestResponse $
--             suggestResponse
--
--         , testUploadDocumentsResponse $
--             uploadDocumentsResponse
--
--         , testSearchResponse $
--             searchResponse
--
--           ]
--     ]

-- Requests

testSuggest :: Suggest -> TestTree
testSuggest = req
    "Suggest"
    "fixture/Suggest.yaml"

testSearch :: Search -> TestTree
testSearch = req
    "Search"
    "fixture/Search.yaml"

-- Responses

testSuggestResponse :: SuggestResponse -> TestTree
testSuggestResponse = res
    "SuggestResponse"
    "fixture/SuggestResponse.proto"
    cloudSearchDomains
    (Proxy :: Proxy Suggest)

testUploadDocumentsResponse :: UploadDocumentsResponse -> TestTree
testUploadDocumentsResponse = res
    "UploadDocumentsResponse"
    "fixture/UploadDocumentsResponse.proto"
    cloudSearchDomains
    (Proxy :: Proxy UploadDocuments)

testSearchResponse :: SearchResponse -> TestTree
testSearchResponse = res
    "SearchResponse"
    "fixture/SearchResponse.proto"
    cloudSearchDomains
    (Proxy :: Proxy Search)
