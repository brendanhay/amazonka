-- Module      : Test.AWS.Gen.SDB
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

module Test.AWS.Gen.SDB where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.SDB

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
