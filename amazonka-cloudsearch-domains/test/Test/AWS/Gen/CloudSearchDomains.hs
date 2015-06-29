-- Module      : Test.AWS.Gen.CloudSearchDomains
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ suggestTest $
--             suggest
--
--         , uploadDocumentsTest $
--             uploadDocuments
--
--         , searchTest $
--             search
--
--           ]

--     , testGroup "response"
--         [ suggestResponseTest $
--             suggestResponse
--
--         , uploadDocumentsResponseTest $
--             uploadDocumentsResponse
--
--         , searchResponseTest $
--             searchResponse
--
--           ]
--     ]

-- Requests

suggestTest :: Suggest -> TestTree
suggestTest = undefined

uploadDocumentsTest :: UploadDocuments -> TestTree
uploadDocumentsTest = undefined

searchTest :: Search -> TestTree
searchTest = undefined

-- Responses

suggestResponseTest :: SuggestResponse -> TestTree
suggestResponseTest = resp
    "SuggestResponse"
    "fixture/SuggestResponse"
    (Proxy :: Proxy Suggest)

uploadDocumentsResponseTest :: UploadDocumentsResponse -> TestTree
uploadDocumentsResponseTest = resp
    "UploadDocumentsResponse"
    "fixture/UploadDocumentsResponse"
    (Proxy :: Proxy UploadDocuments)

searchResponseTest :: SearchResponse -> TestTree
searchResponseTest = resp
    "SearchResponse"
    "fixture/SearchResponse"
    (Proxy :: Proxy Search)
