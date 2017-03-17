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
--         [ requestCreateElasticsearchDomain $
--             createElasticsearchDomain
--
--         , requestRemoveTags $
--             removeTags
--
--         , requestDescribeElasticsearchDomains $
--             describeElasticsearchDomains
--
--         , requestListElasticsearchInstanceTypes $
--             listElasticsearchInstanceTypes
--
--         , requestDescribeElasticsearchDomain $
--             describeElasticsearchDomain
--
--         , requestListDomainNames $
--             listDomainNames
--
--         , requestDescribeElasticsearchInstanceTypeLimits $
--             describeElasticsearchInstanceTypeLimits
--
--         , requestDescribeElasticsearchDomainConfig $
--             describeElasticsearchDomainConfig
--
--         , requestDeleteElasticsearchDomain $
--             deleteElasticsearchDomain
--
--         , requestUpdateElasticsearchDomainConfig $
--             updateElasticsearchDomainConfig
--
--         , requestListElasticsearchVersions $
--             listElasticsearchVersions
--
--         , requestAddTags $
--             addTags
--
--         , requestListTags $
--             listTags
--
--           ]

--     , testGroup "response"
--         [ responseCreateElasticsearchDomain $
--             createElasticsearchDomainResponse
--
--         , responseRemoveTags $
--             removeTagsResponse
--
--         , responseDescribeElasticsearchDomains $
--             describeElasticsearchDomainsResponse
--
--         , responseListElasticsearchInstanceTypes $
--             listElasticsearchInstanceTypesResponse
--
--         , responseDescribeElasticsearchDomain $
--             describeElasticsearchDomainResponse
--
--         , responseListDomainNames $
--             listDomainNamesResponse
--
--         , responseDescribeElasticsearchInstanceTypeLimits $
--             describeElasticsearchInstanceTypeLimitsResponse
--
--         , responseDescribeElasticsearchDomainConfig $
--             describeElasticsearchDomainConfigResponse
--
--         , responseDeleteElasticsearchDomain $
--             deleteElasticsearchDomainResponse
--
--         , responseUpdateElasticsearchDomainConfig $
--             updateElasticsearchDomainConfigResponse
--
--         , responseListElasticsearchVersions $
--             listElasticsearchVersionsResponse
--
--         , responseAddTags $
--             addTagsResponse
--
--         , responseListTags $
--             listTagsResponse
--
--           ]
--     ]

-- Requests

requestCreateElasticsearchDomain :: CreateElasticsearchDomain -> TestTree
requestCreateElasticsearchDomain = req
    "CreateElasticsearchDomain"
    "fixture/CreateElasticsearchDomain.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestDescribeElasticsearchDomains :: DescribeElasticsearchDomains -> TestTree
requestDescribeElasticsearchDomains = req
    "DescribeElasticsearchDomains"
    "fixture/DescribeElasticsearchDomains.yaml"

requestListElasticsearchInstanceTypes :: ListElasticsearchInstanceTypes -> TestTree
requestListElasticsearchInstanceTypes = req
    "ListElasticsearchInstanceTypes"
    "fixture/ListElasticsearchInstanceTypes.yaml"

requestDescribeElasticsearchDomain :: DescribeElasticsearchDomain -> TestTree
requestDescribeElasticsearchDomain = req
    "DescribeElasticsearchDomain"
    "fixture/DescribeElasticsearchDomain.yaml"

requestListDomainNames :: ListDomainNames -> TestTree
requestListDomainNames = req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

requestDescribeElasticsearchInstanceTypeLimits :: DescribeElasticsearchInstanceTypeLimits -> TestTree
requestDescribeElasticsearchInstanceTypeLimits = req
    "DescribeElasticsearchInstanceTypeLimits"
    "fixture/DescribeElasticsearchInstanceTypeLimits.yaml"

requestDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfig -> TestTree
requestDescribeElasticsearchDomainConfig = req
    "DescribeElasticsearchDomainConfig"
    "fixture/DescribeElasticsearchDomainConfig.yaml"

requestDeleteElasticsearchDomain :: DeleteElasticsearchDomain -> TestTree
requestDeleteElasticsearchDomain = req
    "DeleteElasticsearchDomain"
    "fixture/DeleteElasticsearchDomain.yaml"

requestUpdateElasticsearchDomainConfig :: UpdateElasticsearchDomainConfig -> TestTree
requestUpdateElasticsearchDomainConfig = req
    "UpdateElasticsearchDomainConfig"
    "fixture/UpdateElasticsearchDomainConfig.yaml"

requestListElasticsearchVersions :: ListElasticsearchVersions -> TestTree
requestListElasticsearchVersions = req
    "ListElasticsearchVersions"
    "fixture/ListElasticsearchVersions.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

-- Responses

responseCreateElasticsearchDomain :: CreateElasticsearchDomainResponse -> TestTree
responseCreateElasticsearchDomain = res
    "CreateElasticsearchDomainResponse"
    "fixture/CreateElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy CreateElasticsearchDomain)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    elasticSearch
    (Proxy :: Proxy RemoveTags)

responseDescribeElasticsearchDomains :: DescribeElasticsearchDomainsResponse -> TestTree
responseDescribeElasticsearchDomains = res
    "DescribeElasticsearchDomainsResponse"
    "fixture/DescribeElasticsearchDomainsResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchDomains)

responseListElasticsearchInstanceTypes :: ListElasticsearchInstanceTypesResponse -> TestTree
responseListElasticsearchInstanceTypes = res
    "ListElasticsearchInstanceTypesResponse"
    "fixture/ListElasticsearchInstanceTypesResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListElasticsearchInstanceTypes)

responseDescribeElasticsearchDomain :: DescribeElasticsearchDomainResponse -> TestTree
responseDescribeElasticsearchDomain = res
    "DescribeElasticsearchDomainResponse"
    "fixture/DescribeElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchDomain)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames = res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListDomainNames)

responseDescribeElasticsearchInstanceTypeLimits :: DescribeElasticsearchInstanceTypeLimitsResponse -> TestTree
responseDescribeElasticsearchInstanceTypeLimits = res
    "DescribeElasticsearchInstanceTypeLimitsResponse"
    "fixture/DescribeElasticsearchInstanceTypeLimitsResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchInstanceTypeLimits)

responseDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfigResponse -> TestTree
responseDescribeElasticsearchDomainConfig = res
    "DescribeElasticsearchDomainConfigResponse"
    "fixture/DescribeElasticsearchDomainConfigResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchDomainConfig)

responseDeleteElasticsearchDomain :: DeleteElasticsearchDomainResponse -> TestTree
responseDeleteElasticsearchDomain = res
    "DeleteElasticsearchDomainResponse"
    "fixture/DeleteElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy DeleteElasticsearchDomain)

responseUpdateElasticsearchDomainConfig :: UpdateElasticsearchDomainConfigResponse -> TestTree
responseUpdateElasticsearchDomainConfig = res
    "UpdateElasticsearchDomainConfigResponse"
    "fixture/UpdateElasticsearchDomainConfigResponse.proto"
    elasticSearch
    (Proxy :: Proxy UpdateElasticsearchDomainConfig)

responseListElasticsearchVersions :: ListElasticsearchVersionsResponse -> TestTree
responseListElasticsearchVersions = res
    "ListElasticsearchVersionsResponse"
    "fixture/ListElasticsearchVersionsResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListElasticsearchVersions)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    elasticSearch
    (Proxy :: Proxy AddTags)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListTags)
