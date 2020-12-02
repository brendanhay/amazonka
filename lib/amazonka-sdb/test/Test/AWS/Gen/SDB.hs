{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SDB
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SDB where

import Data.Proxy
import Network.AWS.SDB
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SDB.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchDeleteAttributes $
--             batchDeleteAttributes
--
--         , requestBatchPutAttributes $
--             batchPutAttributes
--
--         , requestGetAttributes $
--             getAttributes
--
--         , requestCreateDomain $
--             createDomain
--
--         , requestDomainMetadata $
--             domainMetadata
--
--         , requestSelect $
--             select
--
--         , requestDeleteAttributes $
--             deleteAttributes
--
--         , requestPutAttributes $
--             putAttributes
--
--         , requestDeleteDomain $
--             deleteDomain
--
--         , requestListDomains $
--             listDomains
--
--           ]

--     , testGroup "response"
--         [ responseBatchDeleteAttributes $
--             batchDeleteAttributesResponse
--
--         , responseBatchPutAttributes $
--             batchPutAttributesResponse
--
--         , responseGetAttributes $
--             getAttributesResponse
--
--         , responseCreateDomain $
--             createDomainResponse
--
--         , responseDomainMetadata $
--             domainMetadataResponse
--
--         , responseSelect $
--             selectResponse
--
--         , responseDeleteAttributes $
--             deleteAttributesResponse
--
--         , responsePutAttributes $
--             putAttributesResponse
--
--         , responseDeleteDomain $
--             deleteDomainResponse
--
--         , responseListDomains $
--             listDomainsResponse
--
--           ]
--     ]

-- Requests

requestBatchDeleteAttributes :: BatchDeleteAttributes -> TestTree
requestBatchDeleteAttributes = req
    "BatchDeleteAttributes"
    "fixture/BatchDeleteAttributes.yaml"

requestBatchPutAttributes :: BatchPutAttributes -> TestTree
requestBatchPutAttributes = req
    "BatchPutAttributes"
    "fixture/BatchPutAttributes.yaml"

requestGetAttributes :: GetAttributes -> TestTree
requestGetAttributes = req
    "GetAttributes"
    "fixture/GetAttributes.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain = req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestDomainMetadata :: DomainMetadata -> TestTree
requestDomainMetadata = req
    "DomainMetadata"
    "fixture/DomainMetadata.yaml"

requestSelect :: Select -> TestTree
requestSelect = req
    "Select"
    "fixture/Select.yaml"

requestDeleteAttributes :: DeleteAttributes -> TestTree
requestDeleteAttributes = req
    "DeleteAttributes"
    "fixture/DeleteAttributes.yaml"

requestPutAttributes :: PutAttributes -> TestTree
requestPutAttributes = req
    "PutAttributes"
    "fixture/PutAttributes.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain = req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains = req
    "ListDomains"
    "fixture/ListDomains.yaml"

-- Responses

responseBatchDeleteAttributes :: BatchDeleteAttributesResponse -> TestTree
responseBatchDeleteAttributes = res
    "BatchDeleteAttributesResponse"
    "fixture/BatchDeleteAttributesResponse.proto"
    sdb
    (Proxy :: Proxy BatchDeleteAttributes)

responseBatchPutAttributes :: BatchPutAttributesResponse -> TestTree
responseBatchPutAttributes = res
    "BatchPutAttributesResponse"
    "fixture/BatchPutAttributesResponse.proto"
    sdb
    (Proxy :: Proxy BatchPutAttributes)

responseGetAttributes :: GetAttributesResponse -> TestTree
responseGetAttributes = res
    "GetAttributesResponse"
    "fixture/GetAttributesResponse.proto"
    sdb
    (Proxy :: Proxy GetAttributes)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain = res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    sdb
    (Proxy :: Proxy CreateDomain)

responseDomainMetadata :: DomainMetadataResponse -> TestTree
responseDomainMetadata = res
    "DomainMetadataResponse"
    "fixture/DomainMetadataResponse.proto"
    sdb
    (Proxy :: Proxy DomainMetadata)

responseSelect :: SelectResponse -> TestTree
responseSelect = res
    "SelectResponse"
    "fixture/SelectResponse.proto"
    sdb
    (Proxy :: Proxy Select)

responseDeleteAttributes :: DeleteAttributesResponse -> TestTree
responseDeleteAttributes = res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse.proto"
    sdb
    (Proxy :: Proxy DeleteAttributes)

responsePutAttributes :: PutAttributesResponse -> TestTree
responsePutAttributes = res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse.proto"
    sdb
    (Proxy :: Proxy PutAttributes)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain = res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    sdb
    (Proxy :: Proxy DeleteDomain)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains = res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    sdb
    (Proxy :: Proxy ListDomains)
