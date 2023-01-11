{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SDB
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SDB where

import Amazonka.SDB
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SDB.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchDeleteAttributes $
--             newBatchDeleteAttributes
--
--         , requestBatchPutAttributes $
--             newBatchPutAttributes
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestDeleteAttributes $
--             newDeleteAttributes
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestDomainMetadata $
--             newDomainMetadata
--
--         , requestGetAttributes $
--             newGetAttributes
--
--         , requestListDomains $
--             newListDomains
--
--         , requestPutAttributes $
--             newPutAttributes
--
--         , requestSelect $
--             newSelect
--
--           ]

--     , testGroup "response"
--         [ responseBatchDeleteAttributes $
--             newBatchDeleteAttributesResponse
--
--         , responseBatchPutAttributes $
--             newBatchPutAttributesResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseDeleteAttributes $
--             newDeleteAttributesResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseDomainMetadata $
--             newDomainMetadataResponse
--
--         , responseGetAttributes $
--             newGetAttributesResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responsePutAttributes $
--             newPutAttributesResponse
--
--         , responseSelect $
--             newSelectResponse
--
--           ]
--     ]

-- Requests

requestBatchDeleteAttributes :: BatchDeleteAttributes -> TestTree
requestBatchDeleteAttributes =
  req
    "BatchDeleteAttributes"
    "fixture/BatchDeleteAttributes.yaml"

requestBatchPutAttributes :: BatchPutAttributes -> TestTree
requestBatchPutAttributes =
  req
    "BatchPutAttributes"
    "fixture/BatchPutAttributes.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestDeleteAttributes :: DeleteAttributes -> TestTree
requestDeleteAttributes =
  req
    "DeleteAttributes"
    "fixture/DeleteAttributes.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestDomainMetadata :: DomainMetadata -> TestTree
requestDomainMetadata =
  req
    "DomainMetadata"
    "fixture/DomainMetadata.yaml"

requestGetAttributes :: GetAttributes -> TestTree
requestGetAttributes =
  req
    "GetAttributes"
    "fixture/GetAttributes.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestPutAttributes :: PutAttributes -> TestTree
requestPutAttributes =
  req
    "PutAttributes"
    "fixture/PutAttributes.yaml"

requestSelect :: Select -> TestTree
requestSelect =
  req
    "Select"
    "fixture/Select.yaml"

-- Responses

responseBatchDeleteAttributes :: BatchDeleteAttributesResponse -> TestTree
responseBatchDeleteAttributes =
  res
    "BatchDeleteAttributesResponse"
    "fixture/BatchDeleteAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteAttributes)

responseBatchPutAttributes :: BatchPutAttributesResponse -> TestTree
responseBatchPutAttributes =
  res
    "BatchPutAttributesResponse"
    "fixture/BatchPutAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutAttributes)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseDeleteAttributes :: DeleteAttributesResponse -> TestTree
responseDeleteAttributes =
  res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAttributes)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseDomainMetadata :: DomainMetadataResponse -> TestTree
responseDomainMetadata =
  res
    "DomainMetadataResponse"
    "fixture/DomainMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DomainMetadata)

responseGetAttributes :: GetAttributesResponse -> TestTree
responseGetAttributes =
  res
    "GetAttributesResponse"
    "fixture/GetAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAttributes)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responsePutAttributes :: PutAttributesResponse -> TestTree
responsePutAttributes =
  res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAttributes)

responseSelect :: SelectResponse -> TestTree
responseSelect =
  res
    "SelectResponse"
    "fixture/SelectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Select)
