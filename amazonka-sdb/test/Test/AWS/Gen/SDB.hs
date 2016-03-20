{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SDB
-- Copyright   : (c) 2013-2016 Brendan Hay
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
--         , testDeleteAttributes $
--             deleteAttributes
--
--         , testPutAttributes $
--             putAttributes
--
--         , testDeleteDomain $
--             deleteDomain
--
--         , testListDomains $
--             listDomains
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
--         , testDeleteAttributesResponse $
--             deleteAttributesResponse
--
--         , testPutAttributesResponse $
--             putAttributesResponse
--
--         , testDeleteDomainResponse $
--             deleteDomainResponse
--
--         , testListDomainsResponse $
--             listDomainsResponse
--
--           ]
--     ]

-- Requests

testBatchDeleteAttributes :: BatchDeleteAttributes -> TestTree
testBatchDeleteAttributes = req
    "BatchDeleteAttributes"
    "fixture/BatchDeleteAttributes.yaml"

testBatchPutAttributes :: BatchPutAttributes -> TestTree
testBatchPutAttributes = req
    "BatchPutAttributes"
    "fixture/BatchPutAttributes.yaml"

testGetAttributes :: GetAttributes -> TestTree
testGetAttributes = req
    "GetAttributes"
    "fixture/GetAttributes.yaml"

testCreateDomain :: CreateDomain -> TestTree
testCreateDomain = req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

testDomainMetadata :: DomainMetadata -> TestTree
testDomainMetadata = req
    "DomainMetadata"
    "fixture/DomainMetadata.yaml"

testSelect :: Select -> TestTree
testSelect = req
    "Select"
    "fixture/Select.yaml"

testDeleteAttributes :: DeleteAttributes -> TestTree
testDeleteAttributes = req
    "DeleteAttributes"
    "fixture/DeleteAttributes.yaml"

testPutAttributes :: PutAttributes -> TestTree
testPutAttributes = req
    "PutAttributes"
    "fixture/PutAttributes.yaml"

testDeleteDomain :: DeleteDomain -> TestTree
testDeleteDomain = req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

testListDomains :: ListDomains -> TestTree
testListDomains = req
    "ListDomains"
    "fixture/ListDomains.yaml"

-- Responses

testBatchDeleteAttributesResponse :: BatchDeleteAttributesResponse -> TestTree
testBatchDeleteAttributesResponse = res
    "BatchDeleteAttributesResponse"
    "fixture/BatchDeleteAttributesResponse.proto"
    sdb
    (Proxy :: Proxy BatchDeleteAttributes)

testBatchPutAttributesResponse :: BatchPutAttributesResponse -> TestTree
testBatchPutAttributesResponse = res
    "BatchPutAttributesResponse"
    "fixture/BatchPutAttributesResponse.proto"
    sdb
    (Proxy :: Proxy BatchPutAttributes)

testGetAttributesResponse :: GetAttributesResponse -> TestTree
testGetAttributesResponse = res
    "GetAttributesResponse"
    "fixture/GetAttributesResponse.proto"
    sdb
    (Proxy :: Proxy GetAttributes)

testCreateDomainResponse :: CreateDomainResponse -> TestTree
testCreateDomainResponse = res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    sdb
    (Proxy :: Proxy CreateDomain)

testDomainMetadataResponse :: DomainMetadataResponse -> TestTree
testDomainMetadataResponse = res
    "DomainMetadataResponse"
    "fixture/DomainMetadataResponse.proto"
    sdb
    (Proxy :: Proxy DomainMetadata)

testSelectResponse :: SelectResponse -> TestTree
testSelectResponse = res
    "SelectResponse"
    "fixture/SelectResponse.proto"
    sdb
    (Proxy :: Proxy Select)

testDeleteAttributesResponse :: DeleteAttributesResponse -> TestTree
testDeleteAttributesResponse = res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse.proto"
    sdb
    (Proxy :: Proxy DeleteAttributes)

testPutAttributesResponse :: PutAttributesResponse -> TestTree
testPutAttributesResponse = res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse.proto"
    sdb
    (Proxy :: Proxy PutAttributes)

testDeleteDomainResponse :: DeleteDomainResponse -> TestTree
testDeleteDomainResponse = res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    sdb
    (Proxy :: Proxy DeleteDomain)

testListDomainsResponse :: ListDomainsResponse -> TestTree
testListDomainsResponse = res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    sdb
    (Proxy :: Proxy ListDomains)
