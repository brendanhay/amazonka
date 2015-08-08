{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SDB
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
testBatchDeleteAttributes = req
    "BatchDeleteAttributes"
    "fixture/BatchDeleteAttributes"

testBatchPutAttributes :: BatchPutAttributes -> TestTree
testBatchPutAttributes = req
    "BatchPutAttributes"
    "fixture/BatchPutAttributes"

testGetAttributes :: GetAttributes -> TestTree
testGetAttributes = req
    "GetAttributes"
    "fixture/GetAttributes"

testCreateDomain :: CreateDomain -> TestTree
testCreateDomain = req
    "CreateDomain"
    "fixture/CreateDomain"

testDomainMetadata :: DomainMetadata -> TestTree
testDomainMetadata = req
    "DomainMetadata"
    "fixture/DomainMetadata"

testSelect :: Select -> TestTree
testSelect = req
    "Select"
    "fixture/Select"

testPutAttributes :: PutAttributes -> TestTree
testPutAttributes = req
    "PutAttributes"
    "fixture/PutAttributes"

testDeleteAttributes :: DeleteAttributes -> TestTree
testDeleteAttributes = req
    "DeleteAttributes"
    "fixture/DeleteAttributes"

testListDomains :: ListDomains -> TestTree
testListDomains = req
    "ListDomains"
    "fixture/ListDomains"

testDeleteDomain :: DeleteDomain -> TestTree
testDeleteDomain = req
    "DeleteDomain"
    "fixture/DeleteDomain"

-- Responses

testBatchDeleteAttributesResponse :: BatchDeleteAttributesResponse -> TestTree
testBatchDeleteAttributesResponse = res
    "BatchDeleteAttributesResponse"
    "fixture/BatchDeleteAttributesResponse"
    (Proxy :: Proxy BatchDeleteAttributes)

testBatchPutAttributesResponse :: BatchPutAttributesResponse -> TestTree
testBatchPutAttributesResponse = res
    "BatchPutAttributesResponse"
    "fixture/BatchPutAttributesResponse"
    (Proxy :: Proxy BatchPutAttributes)

testGetAttributesResponse :: GetAttributesResponse -> TestTree
testGetAttributesResponse = res
    "GetAttributesResponse"
    "fixture/GetAttributesResponse"
    (Proxy :: Proxy GetAttributes)

testCreateDomainResponse :: CreateDomainResponse -> TestTree
testCreateDomainResponse = res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse"
    (Proxy :: Proxy CreateDomain)

testDomainMetadataResponse :: DomainMetadataResponse -> TestTree
testDomainMetadataResponse = res
    "DomainMetadataResponse"
    "fixture/DomainMetadataResponse"
    (Proxy :: Proxy DomainMetadata)

testSelectResponse :: SelectResponse -> TestTree
testSelectResponse = res
    "SelectResponse"
    "fixture/SelectResponse"
    (Proxy :: Proxy Select)

testPutAttributesResponse :: PutAttributesResponse -> TestTree
testPutAttributesResponse = res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse"
    (Proxy :: Proxy PutAttributes)

testDeleteAttributesResponse :: DeleteAttributesResponse -> TestTree
testDeleteAttributesResponse = res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse"
    (Proxy :: Proxy DeleteAttributes)

testListDomainsResponse :: ListDomainsResponse -> TestTree
testListDomainsResponse = res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse"
    (Proxy :: Proxy ListDomains)

testDeleteDomainResponse :: DeleteDomainResponse -> TestTree
testDeleteDomainResponse = res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse"
    (Proxy :: Proxy DeleteDomain)
