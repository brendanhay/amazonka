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

testPutAttributes :: PutAttributes -> TestTree
testPutAttributes = req
    "PutAttributes"
    "fixture/PutAttributes.yaml"

testDeleteAttributes :: DeleteAttributes -> TestTree
testDeleteAttributes = req
    "DeleteAttributes"
    "fixture/DeleteAttributes.yaml"

testListDomains :: ListDomains -> TestTree
testListDomains = req
    "ListDomains"
    "fixture/ListDomains.yaml"

testDeleteDomain :: DeleteDomain -> TestTree
testDeleteDomain = req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

-- Responses

testBatchDeleteAttributesResponse :: BatchDeleteAttributesResponse -> TestTree
testBatchDeleteAttributesResponse = res
    "BatchDeleteAttributesResponse"
    "fixture/BatchDeleteAttributesResponse.proto"
    sDB
    (Proxy :: Proxy BatchDeleteAttributes)

testBatchPutAttributesResponse :: BatchPutAttributesResponse -> TestTree
testBatchPutAttributesResponse = res
    "BatchPutAttributesResponse"
    "fixture/BatchPutAttributesResponse.proto"
    sDB
    (Proxy :: Proxy BatchPutAttributes)

testGetAttributesResponse :: GetAttributesResponse -> TestTree
testGetAttributesResponse = res
    "GetAttributesResponse"
    "fixture/GetAttributesResponse.proto"
    sDB
    (Proxy :: Proxy GetAttributes)

testCreateDomainResponse :: CreateDomainResponse -> TestTree
testCreateDomainResponse = res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    sDB
    (Proxy :: Proxy CreateDomain)

testDomainMetadataResponse :: DomainMetadataResponse -> TestTree
testDomainMetadataResponse = res
    "DomainMetadataResponse"
    "fixture/DomainMetadataResponse.proto"
    sDB
    (Proxy :: Proxy DomainMetadata)

testSelectResponse :: SelectResponse -> TestTree
testSelectResponse = res
    "SelectResponse"
    "fixture/SelectResponse.proto"
    sDB
    (Proxy :: Proxy Select)

testPutAttributesResponse :: PutAttributesResponse -> TestTree
testPutAttributesResponse = res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse.proto"
    sDB
    (Proxy :: Proxy PutAttributes)

testDeleteAttributesResponse :: DeleteAttributesResponse -> TestTree
testDeleteAttributesResponse = res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse.proto"
    sDB
    (Proxy :: Proxy DeleteAttributes)

testListDomainsResponse :: ListDomainsResponse -> TestTree
testListDomainsResponse = res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    sDB
    (Proxy :: Proxy ListDomains)

testDeleteDomainResponse :: DeleteDomainResponse -> TestTree
testDeleteDomainResponse = res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    sDB
    (Proxy :: Proxy DeleteDomain)
