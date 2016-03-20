{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticSearch
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ElasticSearch where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ElasticSearch
import Test.AWS.ElasticSearch.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testCreateElasticsearchDomain $
--             createElasticsearchDomain
--
--         , testRemoveTags $
--             removeTags
--
--         , testDescribeElasticsearchDomains $
--             describeElasticsearchDomains
--
--         , testDescribeElasticsearchDomain $
--             describeElasticsearchDomain
--
--         , testListDomainNames $
--             listDomainNames
--
--         , testDescribeElasticsearchDomainConfig $
--             describeElasticsearchDomainConfig
--
--         , testDeleteElasticsearchDomain $
--             deleteElasticsearchDomain
--
--         , testUpdateElasticsearchDomainConfig $
--             updateElasticsearchDomainConfig
--
--         , testAddTags $
--             addTags
--
--         , testListTags $
--             listTags
--
--           ]

--     , testGroup "response"
--         [ testCreateElasticsearchDomainResponse $
--             createElasticsearchDomainResponse
--
--         , testRemoveTagsResponse $
--             removeTagsResponse
--
--         , testDescribeElasticsearchDomainsResponse $
--             describeElasticsearchDomainsResponse
--
--         , testDescribeElasticsearchDomainResponse $
--             describeElasticsearchDomainResponse
--
--         , testListDomainNamesResponse $
--             listDomainNamesResponse
--
--         , testDescribeElasticsearchDomainConfigResponse $
--             describeElasticsearchDomainConfigResponse
--
--         , testDeleteElasticsearchDomainResponse $
--             deleteElasticsearchDomainResponse
--
--         , testUpdateElasticsearchDomainConfigResponse $
--             updateElasticsearchDomainConfigResponse
--
--         , testAddTagsResponse $
--             addTagsResponse
--
--         , testListTagsResponse $
--             listTagsResponse
--
--           ]
--     ]

-- Requests

testCreateElasticsearchDomain :: CreateElasticsearchDomain -> TestTree
testCreateElasticsearchDomain = req
    "CreateElasticsearchDomain"
    "fixture/CreateElasticsearchDomain.yaml"

testRemoveTags :: RemoveTags -> TestTree
testRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

testDescribeElasticsearchDomains :: DescribeElasticsearchDomains -> TestTree
testDescribeElasticsearchDomains = req
    "DescribeElasticsearchDomains"
    "fixture/DescribeElasticsearchDomains.yaml"

testDescribeElasticsearchDomain :: DescribeElasticsearchDomain -> TestTree
testDescribeElasticsearchDomain = req
    "DescribeElasticsearchDomain"
    "fixture/DescribeElasticsearchDomain.yaml"

testListDomainNames :: ListDomainNames -> TestTree
testListDomainNames = req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

testDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfig -> TestTree
testDescribeElasticsearchDomainConfig = req
    "DescribeElasticsearchDomainConfig"
    "fixture/DescribeElasticsearchDomainConfig.yaml"

testDeleteElasticsearchDomain :: DeleteElasticsearchDomain -> TestTree
testDeleteElasticsearchDomain = req
    "DeleteElasticsearchDomain"
    "fixture/DeleteElasticsearchDomain.yaml"

testUpdateElasticsearchDomainConfig :: UpdateElasticsearchDomainConfig -> TestTree
testUpdateElasticsearchDomainConfig = req
    "UpdateElasticsearchDomainConfig"
    "fixture/UpdateElasticsearchDomainConfig.yaml"

testAddTags :: AddTags -> TestTree
testAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

testListTags :: ListTags -> TestTree
testListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

-- Responses

testCreateElasticsearchDomainResponse :: CreateElasticsearchDomainResponse -> TestTree
testCreateElasticsearchDomainResponse = res
    "CreateElasticsearchDomainResponse"
    "fixture/CreateElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy CreateElasticsearchDomain)

testRemoveTagsResponse :: RemoveTagsResponse -> TestTree
testRemoveTagsResponse = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    elasticSearch
    (Proxy :: Proxy RemoveTags)

testDescribeElasticsearchDomainsResponse :: DescribeElasticsearchDomainsResponse -> TestTree
testDescribeElasticsearchDomainsResponse = res
    "DescribeElasticsearchDomainsResponse"
    "fixture/DescribeElasticsearchDomainsResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchDomains)

testDescribeElasticsearchDomainResponse :: DescribeElasticsearchDomainResponse -> TestTree
testDescribeElasticsearchDomainResponse = res
    "DescribeElasticsearchDomainResponse"
    "fixture/DescribeElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchDomain)

testListDomainNamesResponse :: ListDomainNamesResponse -> TestTree
testListDomainNamesResponse = res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListDomainNames)

testDescribeElasticsearchDomainConfigResponse :: DescribeElasticsearchDomainConfigResponse -> TestTree
testDescribeElasticsearchDomainConfigResponse = res
    "DescribeElasticsearchDomainConfigResponse"
    "fixture/DescribeElasticsearchDomainConfigResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchDomainConfig)

testDeleteElasticsearchDomainResponse :: DeleteElasticsearchDomainResponse -> TestTree
testDeleteElasticsearchDomainResponse = res
    "DeleteElasticsearchDomainResponse"
    "fixture/DeleteElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy DeleteElasticsearchDomain)

testUpdateElasticsearchDomainConfigResponse :: UpdateElasticsearchDomainConfigResponse -> TestTree
testUpdateElasticsearchDomainConfigResponse = res
    "UpdateElasticsearchDomainConfigResponse"
    "fixture/UpdateElasticsearchDomainConfigResponse.proto"
    elasticSearch
    (Proxy :: Proxy UpdateElasticsearchDomainConfig)

testAddTagsResponse :: AddTagsResponse -> TestTree
testAddTagsResponse = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    elasticSearch
    (Proxy :: Proxy AddTags)

testListTagsResponse :: ListTagsResponse -> TestTree
testListTagsResponse = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListTags)
