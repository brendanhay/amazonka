{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ServiceQuotas
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ServiceQuotas where

import Amazonka.ServiceQuotas
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.ServiceQuotas.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateServiceQuotaTemplate $
--             newAssociateServiceQuotaTemplate
--
--         , requestDeleteServiceQuotaIncreaseRequestFromTemplate $
--             newDeleteServiceQuotaIncreaseRequestFromTemplate
--
--         , requestDisassociateServiceQuotaTemplate $
--             newDisassociateServiceQuotaTemplate
--
--         , requestGetAWSDefaultServiceQuota $
--             newGetAWSDefaultServiceQuota
--
--         , requestGetAssociationForServiceQuotaTemplate $
--             newGetAssociationForServiceQuotaTemplate
--
--         , requestGetRequestedServiceQuotaChange $
--             newGetRequestedServiceQuotaChange
--
--         , requestGetServiceQuota $
--             newGetServiceQuota
--
--         , requestGetServiceQuotaIncreaseRequestFromTemplate $
--             newGetServiceQuotaIncreaseRequestFromTemplate
--
--         , requestListAWSDefaultServiceQuotas $
--             newListAWSDefaultServiceQuotas
--
--         , requestListRequestedServiceQuotaChangeHistory $
--             newListRequestedServiceQuotaChangeHistory
--
--         , requestListRequestedServiceQuotaChangeHistoryByQuota $
--             newListRequestedServiceQuotaChangeHistoryByQuota
--
--         , requestListServiceQuotaIncreaseRequestsInTemplate $
--             newListServiceQuotaIncreaseRequestsInTemplate
--
--         , requestListServiceQuotas $
--             newListServiceQuotas
--
--         , requestListServices $
--             newListServices
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutServiceQuotaIncreaseRequestIntoTemplate $
--             newPutServiceQuotaIncreaseRequestIntoTemplate
--
--         , requestRequestServiceQuotaIncrease $
--             newRequestServiceQuotaIncrease
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseAssociateServiceQuotaTemplate $
--             newAssociateServiceQuotaTemplateResponse
--
--         , responseDeleteServiceQuotaIncreaseRequestFromTemplate $
--             newDeleteServiceQuotaIncreaseRequestFromTemplateResponse
--
--         , responseDisassociateServiceQuotaTemplate $
--             newDisassociateServiceQuotaTemplateResponse
--
--         , responseGetAWSDefaultServiceQuota $
--             newGetAWSDefaultServiceQuotaResponse
--
--         , responseGetAssociationForServiceQuotaTemplate $
--             newGetAssociationForServiceQuotaTemplateResponse
--
--         , responseGetRequestedServiceQuotaChange $
--             newGetRequestedServiceQuotaChangeResponse
--
--         , responseGetServiceQuota $
--             newGetServiceQuotaResponse
--
--         , responseGetServiceQuotaIncreaseRequestFromTemplate $
--             newGetServiceQuotaIncreaseRequestFromTemplateResponse
--
--         , responseListAWSDefaultServiceQuotas $
--             newListAWSDefaultServiceQuotasResponse
--
--         , responseListRequestedServiceQuotaChangeHistory $
--             newListRequestedServiceQuotaChangeHistoryResponse
--
--         , responseListRequestedServiceQuotaChangeHistoryByQuota $
--             newListRequestedServiceQuotaChangeHistoryByQuotaResponse
--
--         , responseListServiceQuotaIncreaseRequestsInTemplate $
--             newListServiceQuotaIncreaseRequestsInTemplateResponse
--
--         , responseListServiceQuotas $
--             newListServiceQuotasResponse
--
--         , responseListServices $
--             newListServicesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutServiceQuotaIncreaseRequestIntoTemplate $
--             newPutServiceQuotaIncreaseRequestIntoTemplateResponse
--
--         , responseRequestServiceQuotaIncrease $
--             newRequestServiceQuotaIncreaseResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestAssociateServiceQuotaTemplate :: AssociateServiceQuotaTemplate -> TestTree
requestAssociateServiceQuotaTemplate =
  req
    "AssociateServiceQuotaTemplate"
    "fixture/AssociateServiceQuotaTemplate.yaml"

requestDeleteServiceQuotaIncreaseRequestFromTemplate :: DeleteServiceQuotaIncreaseRequestFromTemplate -> TestTree
requestDeleteServiceQuotaIncreaseRequestFromTemplate =
  req
    "DeleteServiceQuotaIncreaseRequestFromTemplate"
    "fixture/DeleteServiceQuotaIncreaseRequestFromTemplate.yaml"

requestDisassociateServiceQuotaTemplate :: DisassociateServiceQuotaTemplate -> TestTree
requestDisassociateServiceQuotaTemplate =
  req
    "DisassociateServiceQuotaTemplate"
    "fixture/DisassociateServiceQuotaTemplate.yaml"

requestGetAWSDefaultServiceQuota :: GetAWSDefaultServiceQuota -> TestTree
requestGetAWSDefaultServiceQuota =
  req
    "GetAWSDefaultServiceQuota"
    "fixture/GetAWSDefaultServiceQuota.yaml"

requestGetAssociationForServiceQuotaTemplate :: GetAssociationForServiceQuotaTemplate -> TestTree
requestGetAssociationForServiceQuotaTemplate =
  req
    "GetAssociationForServiceQuotaTemplate"
    "fixture/GetAssociationForServiceQuotaTemplate.yaml"

requestGetRequestedServiceQuotaChange :: GetRequestedServiceQuotaChange -> TestTree
requestGetRequestedServiceQuotaChange =
  req
    "GetRequestedServiceQuotaChange"
    "fixture/GetRequestedServiceQuotaChange.yaml"

requestGetServiceQuota :: GetServiceQuota -> TestTree
requestGetServiceQuota =
  req
    "GetServiceQuota"
    "fixture/GetServiceQuota.yaml"

requestGetServiceQuotaIncreaseRequestFromTemplate :: GetServiceQuotaIncreaseRequestFromTemplate -> TestTree
requestGetServiceQuotaIncreaseRequestFromTemplate =
  req
    "GetServiceQuotaIncreaseRequestFromTemplate"
    "fixture/GetServiceQuotaIncreaseRequestFromTemplate.yaml"

requestListAWSDefaultServiceQuotas :: ListAWSDefaultServiceQuotas -> TestTree
requestListAWSDefaultServiceQuotas =
  req
    "ListAWSDefaultServiceQuotas"
    "fixture/ListAWSDefaultServiceQuotas.yaml"

requestListRequestedServiceQuotaChangeHistory :: ListRequestedServiceQuotaChangeHistory -> TestTree
requestListRequestedServiceQuotaChangeHistory =
  req
    "ListRequestedServiceQuotaChangeHistory"
    "fixture/ListRequestedServiceQuotaChangeHistory.yaml"

requestListRequestedServiceQuotaChangeHistoryByQuota :: ListRequestedServiceQuotaChangeHistoryByQuota -> TestTree
requestListRequestedServiceQuotaChangeHistoryByQuota =
  req
    "ListRequestedServiceQuotaChangeHistoryByQuota"
    "fixture/ListRequestedServiceQuotaChangeHistoryByQuota.yaml"

requestListServiceQuotaIncreaseRequestsInTemplate :: ListServiceQuotaIncreaseRequestsInTemplate -> TestTree
requestListServiceQuotaIncreaseRequestsInTemplate =
  req
    "ListServiceQuotaIncreaseRequestsInTemplate"
    "fixture/ListServiceQuotaIncreaseRequestsInTemplate.yaml"

requestListServiceQuotas :: ListServiceQuotas -> TestTree
requestListServiceQuotas =
  req
    "ListServiceQuotas"
    "fixture/ListServiceQuotas.yaml"

requestListServices :: ListServices -> TestTree
requestListServices =
  req
    "ListServices"
    "fixture/ListServices.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutServiceQuotaIncreaseRequestIntoTemplate :: PutServiceQuotaIncreaseRequestIntoTemplate -> TestTree
requestPutServiceQuotaIncreaseRequestIntoTemplate =
  req
    "PutServiceQuotaIncreaseRequestIntoTemplate"
    "fixture/PutServiceQuotaIncreaseRequestIntoTemplate.yaml"

requestRequestServiceQuotaIncrease :: RequestServiceQuotaIncrease -> TestTree
requestRequestServiceQuotaIncrease =
  req
    "RequestServiceQuotaIncrease"
    "fixture/RequestServiceQuotaIncrease.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

-- Responses

responseAssociateServiceQuotaTemplate :: AssociateServiceQuotaTemplateResponse -> TestTree
responseAssociateServiceQuotaTemplate =
  res
    "AssociateServiceQuotaTemplateResponse"
    "fixture/AssociateServiceQuotaTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateServiceQuotaTemplate)

responseDeleteServiceQuotaIncreaseRequestFromTemplate :: DeleteServiceQuotaIncreaseRequestFromTemplateResponse -> TestTree
responseDeleteServiceQuotaIncreaseRequestFromTemplate =
  res
    "DeleteServiceQuotaIncreaseRequestFromTemplateResponse"
    "fixture/DeleteServiceQuotaIncreaseRequestFromTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceQuotaIncreaseRequestFromTemplate)

responseDisassociateServiceQuotaTemplate :: DisassociateServiceQuotaTemplateResponse -> TestTree
responseDisassociateServiceQuotaTemplate =
  res
    "DisassociateServiceQuotaTemplateResponse"
    "fixture/DisassociateServiceQuotaTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateServiceQuotaTemplate)

responseGetAWSDefaultServiceQuota :: GetAWSDefaultServiceQuotaResponse -> TestTree
responseGetAWSDefaultServiceQuota =
  res
    "GetAWSDefaultServiceQuotaResponse"
    "fixture/GetAWSDefaultServiceQuotaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAWSDefaultServiceQuota)

responseGetAssociationForServiceQuotaTemplate :: GetAssociationForServiceQuotaTemplateResponse -> TestTree
responseGetAssociationForServiceQuotaTemplate =
  res
    "GetAssociationForServiceQuotaTemplateResponse"
    "fixture/GetAssociationForServiceQuotaTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssociationForServiceQuotaTemplate)

responseGetRequestedServiceQuotaChange :: GetRequestedServiceQuotaChangeResponse -> TestTree
responseGetRequestedServiceQuotaChange =
  res
    "GetRequestedServiceQuotaChangeResponse"
    "fixture/GetRequestedServiceQuotaChangeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRequestedServiceQuotaChange)

responseGetServiceQuota :: GetServiceQuotaResponse -> TestTree
responseGetServiceQuota =
  res
    "GetServiceQuotaResponse"
    "fixture/GetServiceQuotaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceQuota)

responseGetServiceQuotaIncreaseRequestFromTemplate :: GetServiceQuotaIncreaseRequestFromTemplateResponse -> TestTree
responseGetServiceQuotaIncreaseRequestFromTemplate =
  res
    "GetServiceQuotaIncreaseRequestFromTemplateResponse"
    "fixture/GetServiceQuotaIncreaseRequestFromTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceQuotaIncreaseRequestFromTemplate)

responseListAWSDefaultServiceQuotas :: ListAWSDefaultServiceQuotasResponse -> TestTree
responseListAWSDefaultServiceQuotas =
  res
    "ListAWSDefaultServiceQuotasResponse"
    "fixture/ListAWSDefaultServiceQuotasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAWSDefaultServiceQuotas)

responseListRequestedServiceQuotaChangeHistory :: ListRequestedServiceQuotaChangeHistoryResponse -> TestTree
responseListRequestedServiceQuotaChangeHistory =
  res
    "ListRequestedServiceQuotaChangeHistoryResponse"
    "fixture/ListRequestedServiceQuotaChangeHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRequestedServiceQuotaChangeHistory)

responseListRequestedServiceQuotaChangeHistoryByQuota :: ListRequestedServiceQuotaChangeHistoryByQuotaResponse -> TestTree
responseListRequestedServiceQuotaChangeHistoryByQuota =
  res
    "ListRequestedServiceQuotaChangeHistoryByQuotaResponse"
    "fixture/ListRequestedServiceQuotaChangeHistoryByQuotaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRequestedServiceQuotaChangeHistoryByQuota)

responseListServiceQuotaIncreaseRequestsInTemplate :: ListServiceQuotaIncreaseRequestsInTemplateResponse -> TestTree
responseListServiceQuotaIncreaseRequestsInTemplate =
  res
    "ListServiceQuotaIncreaseRequestsInTemplateResponse"
    "fixture/ListServiceQuotaIncreaseRequestsInTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceQuotaIncreaseRequestsInTemplate)

responseListServiceQuotas :: ListServiceQuotasResponse -> TestTree
responseListServiceQuotas =
  res
    "ListServiceQuotasResponse"
    "fixture/ListServiceQuotasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceQuotas)

responseListServices :: ListServicesResponse -> TestTree
responseListServices =
  res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServices)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutServiceQuotaIncreaseRequestIntoTemplate :: PutServiceQuotaIncreaseRequestIntoTemplateResponse -> TestTree
responsePutServiceQuotaIncreaseRequestIntoTemplate =
  res
    "PutServiceQuotaIncreaseRequestIntoTemplateResponse"
    "fixture/PutServiceQuotaIncreaseRequestIntoTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutServiceQuotaIncreaseRequestIntoTemplate)

responseRequestServiceQuotaIncrease :: RequestServiceQuotaIncreaseResponse -> TestTree
responseRequestServiceQuotaIncrease =
  res
    "RequestServiceQuotaIncreaseResponse"
    "fixture/RequestServiceQuotaIncreaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestServiceQuotaIncrease)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)
