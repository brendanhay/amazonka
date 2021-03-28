{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudSearchDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestSuggest $
--             mkSuggest
--
--         , requestUploadDocuments $
--             mkUploadDocuments
--
--         , requestSearch $
--             mkSearch
--
--           ]

--     , testGroup "response"
--         [ responseSuggest $
--             mkSuggestResponse
--
--         , responseUploadDocuments $
--             mkUploadDocumentsResponse
--
--         , responseSearch $
--             mkSearchResponse
--
--           ]
--     ]

-- Requests

requestSuggest :: Suggest -> TestTree
requestSuggest = req
    "Suggest"
    "fixture/Suggest.yaml"

requestSearch :: Search -> TestTree
requestSearch = req
    "Search"
    "fixture/Search.yaml"

-- Responses

responseSuggest :: SuggestResponse -> TestTree
responseSuggest = res
    "SuggestResponse"
    "fixture/SuggestResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Suggest)

responseUploadDocuments :: UploadDocumentsResponse -> TestTree
responseUploadDocuments = res
    "UploadDocumentsResponse"
    "fixture/UploadDocumentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UploadDocuments)

responseSearch :: SearchResponse -> TestTree
responseSearch = res
    "SearchResponse"
    "fixture/SearchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Search)
