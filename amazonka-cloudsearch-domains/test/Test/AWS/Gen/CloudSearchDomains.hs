{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.CloudSearchDomains
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.CloudSearchDomains where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.CloudSearchDomains

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
testSuggest = undefined

testSearch :: Search -> TestTree
testSearch = undefined

-- Responses

testSuggestResponse :: SuggestResponse -> TestTree
testSuggestResponse = resp
    "SuggestResponse"
    "fixture/SuggestResponse"
    (Proxy :: Proxy Suggest)

testUploadDocumentsResponse :: UploadDocumentsResponse -> TestTree
testUploadDocumentsResponse = resp
    "UploadDocumentsResponse"
    "fixture/UploadDocumentsResponse"
    (Proxy :: Proxy UploadDocuments)

testSearchResponse :: SearchResponse -> TestTree
testSearchResponse = resp
    "SearchResponse"
    "fixture/SearchResponse"
    (Proxy :: Proxy Search)

instance Out Bucket
instance Out BucketInfo
instance Out ContentType
instance Out DocumentServiceWarning
instance Out Hit
instance Out Hits
instance Out QueryParser
instance Out Search
instance Out SearchResponse
instance Out SearchStatus
instance Out Suggest
instance Out SuggestModel
instance Out SuggestResponse
instance Out SuggestStatus
instance Out SuggestionMatch
instance Out UploadDocuments
instance Out UploadDocumentsResponse
