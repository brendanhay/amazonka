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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.SDB

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
--         , createDomainTest $
--             createDomain
--
--         , deleteAttributesTest $
--             deleteAttributes
--
--         , deleteDomainTest $
--             deleteDomain
--
--         , domainMetadataTest $
--             domainMetadata
--
--         , getAttributesTest $
--             getAttributes
--
--         , listDomainsTest $
--             listDomains
--
--         , putAttributesTest $
--             putAttributes
--
--         , selectTest $
--             select
--
--           ]

--     , testGroup "response"
--         [ batchDeleteAttributesResponseTest $
--             batchDeleteAttributesResponse
--
--         , batchPutAttributesResponseTest $
--             batchPutAttributesResponse
--
--         , createDomainResponseTest $
--             createDomainResponse
--
--         , deleteAttributesResponseTest $
--             deleteAttributesResponse
--
--         , deleteDomainResponseTest $
--             deleteDomainResponse
--
--         , domainMetadataResponseTest $
--             domainMetadataResponse
--
--         , getAttributesResponseTest $
--             getAttributesResponse
--
--         , listDomainsResponseTest $
--             listDomainsResponse
--
--         , putAttributesResponseTest $
--             putAttributesResponse
--
--         , selectResponseTest $
--             selectResponse
--
--           ]
--     ]

-- Requests

batchDeleteAttributesTest :: BatchDeleteAttributes -> TestTree
batchDeleteAttributesTest = undefined

batchPutAttributesTest :: BatchPutAttributes -> TestTree
batchPutAttributesTest = undefined

createDomainTest :: CreateDomain -> TestTree
createDomainTest = undefined

deleteAttributesTest :: DeleteAttributes -> TestTree
deleteAttributesTest = undefined

deleteDomainTest :: DeleteDomain -> TestTree
deleteDomainTest = undefined

domainMetadataTest :: DomainMetadata -> TestTree
domainMetadataTest = undefined

getAttributesTest :: GetAttributes -> TestTree
getAttributesTest = undefined

listDomainsTest :: ListDomains -> TestTree
listDomainsTest = undefined

putAttributesTest :: PutAttributes -> TestTree
putAttributesTest = undefined

selectTest :: Select -> TestTree
selectTest = undefined

-- Responses

batchDeleteAttributesResponseTest :: BatchDeleteAttributesResponse -> TestTree
batchDeleteAttributesResponseTest = resp
    "batchDeleteAttributesResponse"
    "fixture/BatchDeleteAttributesResponse"
    (Proxy :: Proxy BatchDeleteAttributes)

batchPutAttributesResponseTest :: BatchPutAttributesResponse -> TestTree
batchPutAttributesResponseTest = resp
    "batchPutAttributesResponse"
    "fixture/BatchPutAttributesResponse"
    (Proxy :: Proxy BatchPutAttributes)

createDomainResponseTest :: CreateDomainResponse -> TestTree
createDomainResponseTest = resp
    "createDomainResponse"
    "fixture/CreateDomainResponse"
    (Proxy :: Proxy CreateDomain)

deleteAttributesResponseTest :: DeleteAttributesResponse -> TestTree
deleteAttributesResponseTest = resp
    "deleteAttributesResponse"
    "fixture/DeleteAttributesResponse"
    (Proxy :: Proxy DeleteAttributes)

deleteDomainResponseTest :: DeleteDomainResponse -> TestTree
deleteDomainResponseTest = resp
    "deleteDomainResponse"
    "fixture/DeleteDomainResponse"
    (Proxy :: Proxy DeleteDomain)

domainMetadataResponseTest :: DomainMetadataResponse -> TestTree
domainMetadataResponseTest = resp
    "domainMetadataResponse"
    "fixture/DomainMetadataResponse"
    (Proxy :: Proxy DomainMetadata)

getAttributesResponseTest :: GetAttributesResponse -> TestTree
getAttributesResponseTest = resp
    "getAttributesResponse"
    "fixture/GetAttributesResponse"
    (Proxy :: Proxy GetAttributes)

listDomainsResponseTest :: ListDomainsResponse -> TestTree
listDomainsResponseTest = resp
    "listDomainsResponse"
    "fixture/ListDomainsResponse"
    (Proxy :: Proxy ListDomains)

putAttributesResponseTest :: PutAttributesResponse -> TestTree
putAttributesResponseTest = resp
    "putAttributesResponse"
    "fixture/PutAttributesResponse"
    (Proxy :: Proxy PutAttributes)

selectResponseTest :: SelectResponse -> TestTree
selectResponseTest = resp
    "selectResponse"
    "fixture/SelectResponse"
    (Proxy :: Proxy Select)
