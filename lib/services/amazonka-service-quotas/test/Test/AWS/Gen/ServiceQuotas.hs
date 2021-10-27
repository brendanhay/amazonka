{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ServiceQuotas
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ServiceQuotas where

import Data.Proxy
import Network.AWS.ServiceQuotas
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.ServiceQuotas.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListServices $
--             newListServices
--
--         , requestListAWSDefaultServiceQuotas $
--             newListAWSDefaultServiceQuotas
--
--         , requestGetAssociationForServiceQuotaTemplate $
--             newGetAssociationForServiceQuotaTemplate
--
--         , requestAssociateServiceQuotaTemplate $
--             newAssociateServiceQuotaTemplate
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetServiceQuota $
--             newGetServiceQuota
--
--         , requestPutServiceQuotaIncreaseRequestIntoTemplate $
--             newPutServiceQuotaIncreaseRequestIntoTemplate
--
--         , requestRequestServiceQuotaIncrease $
--             newRequestServiceQuotaIncrease
--
--         , requestGetServiceQuotaIncreaseRequestFromTemplate $
--             newGetServiceQuotaIncreaseRequestFromTemplate
--
--         , requestDisassociateServiceQuotaTemplate $
--             newDisassociateServiceQuotaTemplate
--
--         , requestDeleteServiceQuotaIncreaseRequestFromTemplate $
--             newDeleteServiceQuotaIncreaseRequestFromTemplate
--
--         , requestListServiceQuotas $
--             newListServiceQuotas
--
--         , requestListRequestedServiceQuotaChangeHistory $
--             newListRequestedServiceQuotaChangeHistory
--
--         , requestListServiceQuotaIncreaseRequestsInTemplate $
--             newListServiceQuotaIncreaseRequestsInTemplate
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListRequestedServiceQuotaChangeHistoryByQuota $
--             newListRequestedServiceQuotaChangeHistoryByQuota
--
--         , requestGetRequestedServiceQuotaChange $
--             newGetRequestedServiceQuotaChange
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetAWSDefaultServiceQuota $
--             newGetAWSDefaultServiceQuota
--
--           ]

--     , testGroup "response"
--         [ responseListServices $
--             newListServicesResponse
--
--         , responseListAWSDefaultServiceQuotas $
--             newListAWSDefaultServiceQuotasResponse
--
--         , responseGetAssociationForServiceQuotaTemplate $
--             newGetAssociationForServiceQuotaTemplateResponse
--
--         , responseAssociateServiceQuotaTemplate $
--             newAssociateServiceQuotaTemplateResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetServiceQuota $
--             newGetServiceQuotaResponse
--
--         , responsePutServiceQuotaIncreaseRequestIntoTemplate $
--             newPutServiceQuotaIncreaseRequestIntoTemplateResponse
--
--         , responseRequestServiceQuotaIncrease $
--             newRequestServiceQuotaIncreaseResponse
--
--         , responseGetServiceQuotaIncreaseRequestFromTemplate $
--             newGetServiceQuotaIncreaseRequestFromTemplateResponse
--
--         , responseDisassociateServiceQuotaTemplate $
--             newDisassociateServiceQuotaTemplateResponse
--
--         , responseDeleteServiceQuotaIncreaseRequestFromTemplate $
--             newDeleteServiceQuotaIncreaseRequestFromTemplateResponse
--
--         , responseListServiceQuotas $
--             newListServiceQuotasResponse
--
--         , responseListRequestedServiceQuotaChangeHistory $
--             newListRequestedServiceQuotaChangeHistoryResponse
--
--         , responseListServiceQuotaIncreaseRequestsInTemplate $
--             newListServiceQuotaIncreaseRequestsInTemplateResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListRequestedServiceQuotaChangeHistoryByQuota $
--             newListRequestedServiceQuotaChangeHistoryByQuotaResponse
--
--         , responseGetRequestedServiceQuotaChange $
--             newGetRequestedServiceQuotaChangeResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetAWSDefaultServiceQuota $
--             newGetAWSDefaultServiceQuotaResponse
--
--           ]
--     ]

-- Requests

requestListServices :: ListServices -> TestTree
requestListServices =
  req
    "ListServices"
    "fixture/ListServices.yaml"

requestListAWSDefaultServiceQuotas :: ListAWSDefaultServiceQuotas -> TestTree
requestListAWSDefaultServiceQuotas =
  req
    "ListAWSDefaultServiceQuotas"
    "fixture/ListAWSDefaultServiceQuotas.yaml"

requestGetAssociationForServiceQuotaTemplate :: GetAssociationForServiceQuotaTemplate -> TestTree
requestGetAssociationForServiceQuotaTemplate =
  req
    "GetAssociationForServiceQuotaTemplate"
    "fixture/GetAssociationForServiceQuotaTemplate.yaml"

requestAssociateServiceQuotaTemplate :: AssociateServiceQuotaTemplate -> TestTree
requestAssociateServiceQuotaTemplate =
  req
    "AssociateServiceQuotaTemplate"
    "fixture/AssociateServiceQuotaTemplate.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetServiceQuota :: GetServiceQuota -> TestTree
requestGetServiceQuota =
  req
    "GetServiceQuota"
    "fixture/GetServiceQuota.yaml"

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

requestGetServiceQuotaIncreaseRequestFromTemplate :: GetServiceQuotaIncreaseRequestFromTemplate -> TestTree
requestGetServiceQuotaIncreaseRequestFromTemplate =
  req
    "GetServiceQuotaIncreaseRequestFromTemplate"
    "fixture/GetServiceQuotaIncreaseRequestFromTemplate.yaml"

requestDisassociateServiceQuotaTemplate :: DisassociateServiceQuotaTemplate -> TestTree
requestDisassociateServiceQuotaTemplate =
  req
    "DisassociateServiceQuotaTemplate"
    "fixture/DisassociateServiceQuotaTemplate.yaml"

requestDeleteServiceQuotaIncreaseRequestFromTemplate :: DeleteServiceQuotaIncreaseRequestFromTemplate -> TestTree
requestDeleteServiceQuotaIncreaseRequestFromTemplate =
  req
    "DeleteServiceQuotaIncreaseRequestFromTemplate"
    "fixture/DeleteServiceQuotaIncreaseRequestFromTemplate.yaml"

requestListServiceQuotas :: ListServiceQuotas -> TestTree
requestListServiceQuotas =
  req
    "ListServiceQuotas"
    "fixture/ListServiceQuotas.yaml"

requestListRequestedServiceQuotaChangeHistory :: ListRequestedServiceQuotaChangeHistory -> TestTree
requestListRequestedServiceQuotaChangeHistory =
  req
    "ListRequestedServiceQuotaChangeHistory"
    "fixture/ListRequestedServiceQuotaChangeHistory.yaml"

requestListServiceQuotaIncreaseRequestsInTemplate :: ListServiceQuotaIncreaseRequestsInTemplate -> TestTree
requestListServiceQuotaIncreaseRequestsInTemplate =
  req
    "ListServiceQuotaIncreaseRequestsInTemplate"
    "fixture/ListServiceQuotaIncreaseRequestsInTemplate.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListRequestedServiceQuotaChangeHistoryByQuota :: ListRequestedServiceQuotaChangeHistoryByQuota -> TestTree
requestListRequestedServiceQuotaChangeHistoryByQuota =
  req
    "ListRequestedServiceQuotaChangeHistoryByQuota"
    "fixture/ListRequestedServiceQuotaChangeHistoryByQuota.yaml"

requestGetRequestedServiceQuotaChange :: GetRequestedServiceQuotaChange -> TestTree
requestGetRequestedServiceQuotaChange =
  req
    "GetRequestedServiceQuotaChange"
    "fixture/GetRequestedServiceQuotaChange.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetAWSDefaultServiceQuota :: GetAWSDefaultServiceQuota -> TestTree
requestGetAWSDefaultServiceQuota =
  req
    "GetAWSDefaultServiceQuota"
    "fixture/GetAWSDefaultServiceQuota.yaml"

-- Responses

responseListServices :: ListServicesResponse -> TestTree
responseListServices =
  res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListServices)

responseListAWSDefaultServiceQuotas :: ListAWSDefaultServiceQuotasResponse -> TestTree
responseListAWSDefaultServiceQuotas =
  res
    "ListAWSDefaultServiceQuotasResponse"
    "fixture/ListAWSDefaultServiceQuotasResponse.proto"
    defaultService
    (Proxy :: Proxy ListAWSDefaultServiceQuotas)

responseGetAssociationForServiceQuotaTemplate :: GetAssociationForServiceQuotaTemplateResponse -> TestTree
responseGetAssociationForServiceQuotaTemplate =
  res
    "GetAssociationForServiceQuotaTemplateResponse"
    "fixture/GetAssociationForServiceQuotaTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetAssociationForServiceQuotaTemplate)

responseAssociateServiceQuotaTemplate :: AssociateServiceQuotaTemplateResponse -> TestTree
responseAssociateServiceQuotaTemplate =
  res
    "AssociateServiceQuotaTemplateResponse"
    "fixture/AssociateServiceQuotaTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateServiceQuotaTemplate)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetServiceQuota :: GetServiceQuotaResponse -> TestTree
responseGetServiceQuota =
  res
    "GetServiceQuotaResponse"
    "fixture/GetServiceQuotaResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceQuota)

responsePutServiceQuotaIncreaseRequestIntoTemplate :: PutServiceQuotaIncreaseRequestIntoTemplateResponse -> TestTree
responsePutServiceQuotaIncreaseRequestIntoTemplate =
  res
    "PutServiceQuotaIncreaseRequestIntoTemplateResponse"
    "fixture/PutServiceQuotaIncreaseRequestIntoTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy PutServiceQuotaIncreaseRequestIntoTemplate)

responseRequestServiceQuotaIncrease :: RequestServiceQuotaIncreaseResponse -> TestTree
responseRequestServiceQuotaIncrease =
  res
    "RequestServiceQuotaIncreaseResponse"
    "fixture/RequestServiceQuotaIncreaseResponse.proto"
    defaultService
    (Proxy :: Proxy RequestServiceQuotaIncrease)

responseGetServiceQuotaIncreaseRequestFromTemplate :: GetServiceQuotaIncreaseRequestFromTemplateResponse -> TestTree
responseGetServiceQuotaIncreaseRequestFromTemplate =
  res
    "GetServiceQuotaIncreaseRequestFromTemplateResponse"
    "fixture/GetServiceQuotaIncreaseRequestFromTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceQuotaIncreaseRequestFromTemplate)

responseDisassociateServiceQuotaTemplate :: DisassociateServiceQuotaTemplateResponse -> TestTree
responseDisassociateServiceQuotaTemplate =
  res
    "DisassociateServiceQuotaTemplateResponse"
    "fixture/DisassociateServiceQuotaTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateServiceQuotaTemplate)

responseDeleteServiceQuotaIncreaseRequestFromTemplate :: DeleteServiceQuotaIncreaseRequestFromTemplateResponse -> TestTree
responseDeleteServiceQuotaIncreaseRequestFromTemplate =
  res
    "DeleteServiceQuotaIncreaseRequestFromTemplateResponse"
    "fixture/DeleteServiceQuotaIncreaseRequestFromTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteServiceQuotaIncreaseRequestFromTemplate)

responseListServiceQuotas :: ListServiceQuotasResponse -> TestTree
responseListServiceQuotas =
  res
    "ListServiceQuotasResponse"
    "fixture/ListServiceQuotasResponse.proto"
    defaultService
    (Proxy :: Proxy ListServiceQuotas)

responseListRequestedServiceQuotaChangeHistory :: ListRequestedServiceQuotaChangeHistoryResponse -> TestTree
responseListRequestedServiceQuotaChangeHistory =
  res
    "ListRequestedServiceQuotaChangeHistoryResponse"
    "fixture/ListRequestedServiceQuotaChangeHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy ListRequestedServiceQuotaChangeHistory)

responseListServiceQuotaIncreaseRequestsInTemplate :: ListServiceQuotaIncreaseRequestsInTemplateResponse -> TestTree
responseListServiceQuotaIncreaseRequestsInTemplate =
  res
    "ListServiceQuotaIncreaseRequestsInTemplateResponse"
    "fixture/ListServiceQuotaIncreaseRequestsInTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy ListServiceQuotaIncreaseRequestsInTemplate)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListRequestedServiceQuotaChangeHistoryByQuota :: ListRequestedServiceQuotaChangeHistoryByQuotaResponse -> TestTree
responseListRequestedServiceQuotaChangeHistoryByQuota =
  res
    "ListRequestedServiceQuotaChangeHistoryByQuotaResponse"
    "fixture/ListRequestedServiceQuotaChangeHistoryByQuotaResponse.proto"
    defaultService
    (Proxy :: Proxy ListRequestedServiceQuotaChangeHistoryByQuota)

responseGetRequestedServiceQuotaChange :: GetRequestedServiceQuotaChangeResponse -> TestTree
responseGetRequestedServiceQuotaChange =
  res
    "GetRequestedServiceQuotaChangeResponse"
    "fixture/GetRequestedServiceQuotaChangeResponse.proto"
    defaultService
    (Proxy :: Proxy GetRequestedServiceQuotaChange)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseGetAWSDefaultServiceQuota :: GetAWSDefaultServiceQuotaResponse -> TestTree
responseGetAWSDefaultServiceQuota =
  res
    "GetAWSDefaultServiceQuotaResponse"
    "fixture/GetAWSDefaultServiceQuotaResponse.proto"
    defaultService
    (Proxy :: Proxy GetAWSDefaultServiceQuota)
