{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SDB
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SDB where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.SDB
import Test.AWS.SDB.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testBatchDeleteAttributes $
--             batchDeleteAttributes
--
--         , testBatchPutAttributes $
--             batchPutAttributes
--
--         , testGetAttributes $
--             getAttributes
--
--         , testCreateDomain $
--             createDomain
--
--         , testDomainMetadata $
--             domainMetadata
--
--         , testSelect $
--             select
--
--         , testPutAttributes $
--             putAttributes
--
--         , testDeleteAttributes $
--             deleteAttributes
--
--         , testListDomains $
--             listDomains
--
--         , testDeleteDomain $
--             deleteDomain
--
--           ]

--     , testGroup "response"
--         [ testBatchDeleteAttributesResponse $
--             batchDeleteAttributesResponse
--
--         , testBatchPutAttributesResponse $
--             batchPutAttributesResponse
--
--         , testGetAttributesResponse $
--             getAttributesResponse
--
--         , testCreateDomainResponse $
--             createDomainResponse
--
--         , testDomainMetadataResponse $
--             domainMetadataResponse
--
--         , testSelectResponse $
--             selectResponse
--
--         , testPutAttributesResponse $
--             putAttributesResponse
--
--         , testDeleteAttributesResponse $
--             deleteAttributesResponse
--
--         , testListDomainsResponse $
--             listDomainsResponse
--
--         , testDeleteDomainResponse $
--             deleteDomainResponse
--
--           ]
--     ]

-- Requests

testBatchDeleteAttributes :: BatchDeleteAttributes -> TestTree
testBatchDeleteAttributes = undefined

testBatchPutAttributes :: BatchPutAttributes -> TestTree
testBatchPutAttributes = undefined

testGetAttributes :: GetAttributes -> TestTree
testGetAttributes = undefined

testCreateDomain :: CreateDomain -> TestTree
testCreateDomain = undefined

testDomainMetadata :: DomainMetadata -> TestTree
testDomainMetadata = undefined

testSelect :: Select -> TestTree
testSelect = undefined

testPutAttributes :: PutAttributes -> TestTree
testPutAttributes = undefined

testDeleteAttributes :: DeleteAttributes -> TestTree
testDeleteAttributes = undefined

testListDomains :: ListDomains -> TestTree
testListDomains = undefined

testDeleteDomain :: DeleteDomain -> TestTree
testDeleteDomain = undefined

-- Responses

testBatchDeleteAttributesResponse :: BatchDeleteAttributesResponse -> TestTree
testBatchDeleteAttributesResponse = resp
    "BatchDeleteAttributesResponse"
    "fixture/BatchDeleteAttributesResponse"
    (Proxy :: Proxy BatchDeleteAttributes)

testBatchPutAttributesResponse :: BatchPutAttributesResponse -> TestTree
testBatchPutAttributesResponse = resp
    "BatchPutAttributesResponse"
    "fixture/BatchPutAttributesResponse"
    (Proxy :: Proxy BatchPutAttributes)

testGetAttributesResponse :: GetAttributesResponse -> TestTree
testGetAttributesResponse = resp
    "GetAttributesResponse"
    "fixture/GetAttributesResponse"
    (Proxy :: Proxy GetAttributes)

testCreateDomainResponse :: CreateDomainResponse -> TestTree
testCreateDomainResponse = resp
    "CreateDomainResponse"
    "fixture/CreateDomainResponse"
    (Proxy :: Proxy CreateDomain)

testDomainMetadataResponse :: DomainMetadataResponse -> TestTree
testDomainMetadataResponse = resp
    "DomainMetadataResponse"
    "fixture/DomainMetadataResponse"
    (Proxy :: Proxy DomainMetadata)

testSelectResponse :: SelectResponse -> TestTree
testSelectResponse = resp
    "SelectResponse"
    "fixture/SelectResponse"
    (Proxy :: Proxy Select)

testPutAttributesResponse :: PutAttributesResponse -> TestTree
testPutAttributesResponse = resp
    "PutAttributesResponse"
    "fixture/PutAttributesResponse"
    (Proxy :: Proxy PutAttributes)

testDeleteAttributesResponse :: DeleteAttributesResponse -> TestTree
testDeleteAttributesResponse = resp
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse"
    (Proxy :: Proxy DeleteAttributes)

testListDomainsResponse :: ListDomainsResponse -> TestTree
testListDomainsResponse = resp
    "ListDomainsResponse"
    "fixture/ListDomainsResponse"
    (Proxy :: Proxy ListDomains)

testDeleteDomainResponse :: DeleteDomainResponse -> TestTree
testDeleteDomainResponse = resp
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse"
    (Proxy :: Proxy DeleteDomain)

instance Out Attribute
instance Out BatchDeleteAttributes
instance Out BatchDeleteAttributesResponse
instance Out BatchPutAttributes
instance Out BatchPutAttributesResponse
instance Out CreateDomain
instance Out CreateDomainResponse
instance Out DeletableItem
instance Out DeleteAttributes
instance Out DeleteAttributesResponse
instance Out DeleteDomain
instance Out DeleteDomainResponse
instance Out DomainMetadata
instance Out DomainMetadataResponse
instance Out GetAttributes
instance Out GetAttributesResponse
instance Out Item
instance Out ListDomains
instance Out ListDomainsResponse
instance Out PutAttributes
instance Out PutAttributesResponse
instance Out ReplaceableAttribute
instance Out ReplaceableItem
instance Out Select
instance Out SelectResponse
instance Out UpdateCondition
