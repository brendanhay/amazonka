{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SDB
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestBatchDeleteAttributes $
--             mkBatchDeleteAttributes
--
--         , requestBatchPutAttributes $
--             mkBatchPutAttributes
--
--         , requestGetAttributes $
--             mkGetAttributes
--
--         , requestCreateDomain $
--             mkCreateDomain
--
--         , requestDomainMetadata $
--             mkDomainMetadata
--
--         , requestSelect $
--             mkSelect
--
--         , requestDeleteAttributes $
--             mkDeleteAttributes
--
--         , requestPutAttributes $
--             mkPutAttributes
--
--         , requestDeleteDomain $
--             mkDeleteDomain
--
--         , requestListDomains $
--             mkListDomains
--
--           ]

--     , testGroup "response"
--         [ responseBatchDeleteAttributes $
--             mkBatchDeleteAttributesResponse
--
--         , responseBatchPutAttributes $
--             mkBatchPutAttributesResponse
--
--         , responseGetAttributes $
--             mkGetAttributesResponse
--
--         , responseCreateDomain $
--             mkCreateDomainResponse
--
--         , responseDomainMetadata $
--             mkDomainMetadataResponse
--
--         , responseSelect $
--             mkSelectResponse
--
--         , responseDeleteAttributes $
--             mkDeleteAttributesResponse
--
--         , responsePutAttributes $
--             mkPutAttributesResponse
--
--         , responseDeleteDomain $
--             mkDeleteDomainResponse
--
--         , responseListDomains $
--             mkListDomainsResponse
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

requestGetAttributes :: GetAttributes -> TestTree
requestGetAttributes =
  req
    "GetAttributes"
    "fixture/GetAttributes.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestDomainMetadata :: DomainMetadata -> TestTree
requestDomainMetadata =
  req
    "DomainMetadata"
    "fixture/DomainMetadata.yaml"

requestSelect :: Select -> TestTree
requestSelect =
  req
    "Select"
    "fixture/Select.yaml"

requestDeleteAttributes :: DeleteAttributes -> TestTree
requestDeleteAttributes =
  req
    "DeleteAttributes"
    "fixture/DeleteAttributes.yaml"

requestPutAttributes :: PutAttributes -> TestTree
requestPutAttributes =
  req
    "PutAttributes"
    "fixture/PutAttributes.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

-- Responses

responseBatchDeleteAttributes :: BatchDeleteAttributesResponse -> TestTree
responseBatchDeleteAttributes =
  res
    "BatchDeleteAttributesResponse"
    "fixture/BatchDeleteAttributesResponse.proto"
    sdbService
    (Proxy :: Proxy BatchDeleteAttributes)

responseBatchPutAttributes :: BatchPutAttributesResponse -> TestTree
responseBatchPutAttributes =
  res
    "BatchPutAttributesResponse"
    "fixture/BatchPutAttributesResponse.proto"
    sdbService
    (Proxy :: Proxy BatchPutAttributes)

responseGetAttributes :: GetAttributesResponse -> TestTree
responseGetAttributes =
  res
    "GetAttributesResponse"
    "fixture/GetAttributesResponse.proto"
    sdbService
    (Proxy :: Proxy GetAttributes)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    sdbService
    (Proxy :: Proxy CreateDomain)

responseDomainMetadata :: DomainMetadataResponse -> TestTree
responseDomainMetadata =
  res
    "DomainMetadataResponse"
    "fixture/DomainMetadataResponse.proto"
    sdbService
    (Proxy :: Proxy DomainMetadata)

responseSelect :: SelectResponse -> TestTree
responseSelect =
  res
    "SelectResponse"
    "fixture/SelectResponse.proto"
    sdbService
    (Proxy :: Proxy Select)

responseDeleteAttributes :: DeleteAttributesResponse -> TestTree
responseDeleteAttributes =
  res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse.proto"
    sdbService
    (Proxy :: Proxy DeleteAttributes)

responsePutAttributes :: PutAttributesResponse -> TestTree
responsePutAttributes =
  res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse.proto"
    sdbService
    (Proxy :: Proxy PutAttributes)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    sdbService
    (Proxy :: Proxy DeleteDomain)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    sdbService
    (Proxy :: Proxy ListDomains)
