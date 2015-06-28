-- Module      : Test.AWS.Gen.SDB
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

module Test.AWS.Gen.SDB where

import           Data.Proxy
import           Network.AWS.SDB
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ batchDeleteAttributesTest $
--             batchDeleteAttributes
--
--         , batchPutAttributesTest $
--             batchPutAttributes
--
--         , getAttributesTest $
--             getAttributes
--
--         , createDomainTest $
--             createDomain
--
--         , domainMetadataTest $
--             domainMetadata
--
--         , selectTest $
--             select
--
--         , putAttributesTest $
--             putAttributes
--
--         , deleteAttributesTest $
--             deleteAttributes
--
--         , listDomainsTest $
--             listDomains
--
--         , deleteDomainTest $
--             deleteDomain
--
--           ]

--     , testGroup "response"
--         [ batchDeleteAttributesResponseTest $
--             batchDeleteAttributesResponse
--
--         , batchPutAttributesResponseTest $
--             batchPutAttributesResponse
--
--         , getAttributesResponseTest $
--             getAttributesResponse
--
--         , createDomainResponseTest $
--             createDomainResponse
--
--         , domainMetadataResponseTest $
--             domainMetadataResponse
--
--         , selectResponseTest $
--             selectResponse
--
--         , putAttributesResponseTest $
--             putAttributesResponse
--
--         , deleteAttributesResponseTest $
--             deleteAttributesResponse
--
--         , listDomainsResponseTest $
--             listDomainsResponse
--
--         , deleteDomainResponseTest $
--             deleteDomainResponse
--
--           ]
--     ]

-- Requests

batchDeleteAttributesTest :: BatchDeleteAttributes -> TestTree
batchDeleteAttributesTest = undefined

batchPutAttributesTest :: BatchPutAttributes -> TestTree
batchPutAttributesTest = undefined

getAttributesTest :: GetAttributes -> TestTree
getAttributesTest = undefined

createDomainTest :: CreateDomain -> TestTree
createDomainTest = undefined

domainMetadataTest :: DomainMetadata -> TestTree
domainMetadataTest = undefined

selectTest :: Select -> TestTree
selectTest = undefined

putAttributesTest :: PutAttributes -> TestTree
putAttributesTest = undefined

deleteAttributesTest :: DeleteAttributes -> TestTree
deleteAttributesTest = undefined

listDomainsTest :: ListDomains -> TestTree
listDomainsTest = undefined

deleteDomainTest :: DeleteDomain -> TestTree
deleteDomainTest = undefined

-- Responses

batchDeleteAttributesResponseTest :: BatchDeleteAttributesResponse -> TestTree
batchDeleteAttributesResponseTest = resp
    "BatchDeleteAttributesResponse"
    "fixture/SDB/BatchDeleteAttributesResponse"
    (Proxy :: Proxy BatchDeleteAttributes)

batchPutAttributesResponseTest :: BatchPutAttributesResponse -> TestTree
batchPutAttributesResponseTest = resp
    "BatchPutAttributesResponse"
    "fixture/SDB/BatchPutAttributesResponse"
    (Proxy :: Proxy BatchPutAttributes)

getAttributesResponseTest :: GetAttributesResponse -> TestTree
getAttributesResponseTest = resp
    "GetAttributesResponse"
    "fixture/SDB/GetAttributesResponse"
    (Proxy :: Proxy GetAttributes)

createDomainResponseTest :: CreateDomainResponse -> TestTree
createDomainResponseTest = resp
    "CreateDomainResponse"
    "fixture/SDB/CreateDomainResponse"
    (Proxy :: Proxy CreateDomain)

domainMetadataResponseTest :: DomainMetadataResponse -> TestTree
domainMetadataResponseTest = resp
    "DomainMetadataResponse"
    "fixture/SDB/DomainMetadataResponse"
    (Proxy :: Proxy DomainMetadata)

selectResponseTest :: SelectResponse -> TestTree
selectResponseTest = resp
    "SelectResponse"
    "fixture/SDB/SelectResponse"
    (Proxy :: Proxy Select)

putAttributesResponseTest :: PutAttributesResponse -> TestTree
putAttributesResponseTest = resp
    "PutAttributesResponse"
    "fixture/SDB/PutAttributesResponse"
    (Proxy :: Proxy PutAttributes)

deleteAttributesResponseTest :: DeleteAttributesResponse -> TestTree
deleteAttributesResponseTest = resp
    "DeleteAttributesResponse"
    "fixture/SDB/DeleteAttributesResponse"
    (Proxy :: Proxy DeleteAttributes)

listDomainsResponseTest :: ListDomainsResponse -> TestTree
listDomainsResponseTest = resp
    "ListDomainsResponse"
    "fixture/SDB/ListDomainsResponse"
    (Proxy :: Proxy ListDomains)

deleteDomainResponseTest :: DeleteDomainResponse -> TestTree
deleteDomainResponseTest = resp
    "DeleteDomainResponse"
    "fixture/SDB/DeleteDomainResponse"
    (Proxy :: Proxy DeleteDomain)
