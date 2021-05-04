{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SDB
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestListDomains $
--             newListDomains
--
--         , requestDeleteAttributes $
--             newDeleteAttributes
--
--         , requestDomainMetadata $
--             newDomainMetadata
--
--         , requestBatchPutAttributes $
--             newBatchPutAttributes
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestGetAttributes $
--             newGetAttributes
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestPutAttributes $
--             newPutAttributes
--
--         , requestSelect $
--             newSelect
--
--         , requestBatchDeleteAttributes $
--             newBatchDeleteAttributes
--
--           ]

--     , testGroup "response"
--         [ responseListDomains $
--             newListDomainsResponse
--
--         , responseDeleteAttributes $
--             newDeleteAttributesResponse
--
--         , responseDomainMetadata $
--             newDomainMetadataResponse
--
--         , responseBatchPutAttributes $
--             newBatchPutAttributesResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseGetAttributes $
--             newGetAttributesResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responsePutAttributes $
--             newPutAttributesResponse
--
--         , responseSelect $
--             newSelectResponse
--
--         , responseBatchDeleteAttributes $
--             newBatchDeleteAttributesResponse
--
--           ]
--     ]

-- Requests

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestDeleteAttributes :: DeleteAttributes -> TestTree
requestDeleteAttributes =
  req
    "DeleteAttributes"
    "fixture/DeleteAttributes.yaml"

requestDomainMetadata :: DomainMetadata -> TestTree
requestDomainMetadata =
  req
    "DomainMetadata"
    "fixture/DomainMetadata.yaml"

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

requestGetAttributes :: GetAttributes -> TestTree
requestGetAttributes =
  req
    "GetAttributes"
    "fixture/GetAttributes.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

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

requestBatchDeleteAttributes :: BatchDeleteAttributes -> TestTree
requestBatchDeleteAttributes =
  req
    "BatchDeleteAttributes"
    "fixture/BatchDeleteAttributes.yaml"

-- Responses

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomains)

responseDeleteAttributes :: DeleteAttributesResponse -> TestTree
responseDeleteAttributes =
  res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAttributes)

responseDomainMetadata :: DomainMetadataResponse -> TestTree
responseDomainMetadata =
  res
    "DomainMetadataResponse"
    "fixture/DomainMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy DomainMetadata)

responseBatchPutAttributes :: BatchPutAttributesResponse -> TestTree
responseBatchPutAttributes =
  res
    "BatchPutAttributesResponse"
    "fixture/BatchPutAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchPutAttributes)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDomain)

responseGetAttributes :: GetAttributesResponse -> TestTree
responseGetAttributes =
  res
    "GetAttributesResponse"
    "fixture/GetAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetAttributes)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomain)

responsePutAttributes :: PutAttributesResponse -> TestTree
responsePutAttributes =
  res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutAttributes)

responseSelect :: SelectResponse -> TestTree
responseSelect =
  res
    "SelectResponse"
    "fixture/SelectResponse.proto"
    defaultService
    (Proxy :: Proxy Select)

responseBatchDeleteAttributes :: BatchDeleteAttributesResponse -> TestTree
responseBatchDeleteAttributes =
  res
    "BatchDeleteAttributesResponse"
    "fixture/BatchDeleteAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteAttributes)
