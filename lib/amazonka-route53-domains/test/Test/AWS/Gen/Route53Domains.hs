{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53Domains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Route53Domains where

import Data.Proxy
import Network.AWS.Route53Domains
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Route53Domains.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListOperations $
--             listOperations
--
--         , requestGetDomainDetail $
--             getDomainDetail
--
--         , requestCheckDomainTransferability $
--             checkDomainTransferability
--
--         , requestUpdateDomainContactPrivacy $
--             updateDomainContactPrivacy
--
--         , requestGetOperationDetail $
--             getOperationDetail
--
--         , requestRejectDomainTransferFromAnotherAWSAccount $
--             rejectDomainTransferFromAnotherAWSAccount
--
--         , requestEnableDomainAutoRenew $
--             enableDomainAutoRenew
--
--         , requestResendContactReachabilityEmail $
--             resendContactReachabilityEmail
--
--         , requestDisableDomainAutoRenew $
--             disableDomainAutoRenew
--
--         , requestRenewDomain $
--             renewDomain
--
--         , requestViewBilling $
--             viewBilling
--
--         , requestUpdateDomainContact $
--             updateDomainContact
--
--         , requestEnableDomainTransferLock $
--             enableDomainTransferLock
--
--         , requestRegisterDomain $
--             registerDomain
--
--         , requestGetDomainSuggestions $
--             getDomainSuggestions
--
--         , requestDisableDomainTransferLock $
--             disableDomainTransferLock
--
--         , requestCheckDomainAvailability $
--             checkDomainAvailability
--
--         , requestTransferDomainToAnotherAWSAccount $
--             transferDomainToAnotherAWSAccount
--
--         , requestAcceptDomainTransferFromAnotherAWSAccount $
--             acceptDomainTransferFromAnotherAWSAccount
--
--         , requestGetContactReachabilityStatus $
--             getContactReachabilityStatus
--
--         , requestListTagsForDomain $
--             listTagsForDomain
--
--         , requestUpdateDomainNameservers $
--             updateDomainNameservers
--
--         , requestDeleteTagsForDomain $
--             deleteTagsForDomain
--
--         , requestUpdateTagsForDomain $
--             updateTagsForDomain
--
--         , requestRetrieveDomainAuthCode $
--             retrieveDomainAuthCode
--
--         , requestTransferDomain $
--             transferDomain
--
--         , requestListDomains $
--             listDomains
--
--         , requestCancelDomainTransferToAnotherAWSAccount $
--             cancelDomainTransferToAnotherAWSAccount
--
--           ]

--     , testGroup "response"
--         [ responseListOperations $
--             listOperationsResponse
--
--         , responseGetDomainDetail $
--             getDomainDetailResponse
--
--         , responseCheckDomainTransferability $
--             checkDomainTransferabilityResponse
--
--         , responseUpdateDomainContactPrivacy $
--             updateDomainContactPrivacyResponse
--
--         , responseGetOperationDetail $
--             getOperationDetailResponse
--
--         , responseRejectDomainTransferFromAnotherAWSAccount $
--             rejectDomainTransferFromAnotherAWSAccountResponse
--
--         , responseEnableDomainAutoRenew $
--             enableDomainAutoRenewResponse
--
--         , responseResendContactReachabilityEmail $
--             resendContactReachabilityEmailResponse
--
--         , responseDisableDomainAutoRenew $
--             disableDomainAutoRenewResponse
--
--         , responseRenewDomain $
--             renewDomainResponse
--
--         , responseViewBilling $
--             viewBillingResponse
--
--         , responseUpdateDomainContact $
--             updateDomainContactResponse
--
--         , responseEnableDomainTransferLock $
--             enableDomainTransferLockResponse
--
--         , responseRegisterDomain $
--             registerDomainResponse
--
--         , responseGetDomainSuggestions $
--             getDomainSuggestionsResponse
--
--         , responseDisableDomainTransferLock $
--             disableDomainTransferLockResponse
--
--         , responseCheckDomainAvailability $
--             checkDomainAvailabilityResponse
--
--         , responseTransferDomainToAnotherAWSAccount $
--             transferDomainToAnotherAWSAccountResponse
--
--         , responseAcceptDomainTransferFromAnotherAWSAccount $
--             acceptDomainTransferFromAnotherAWSAccountResponse
--
--         , responseGetContactReachabilityStatus $
--             getContactReachabilityStatusResponse
--
--         , responseListTagsForDomain $
--             listTagsForDomainResponse
--
--         , responseUpdateDomainNameservers $
--             updateDomainNameserversResponse
--
--         , responseDeleteTagsForDomain $
--             deleteTagsForDomainResponse
--
--         , responseUpdateTagsForDomain $
--             updateTagsForDomainResponse
--
--         , responseRetrieveDomainAuthCode $
--             retrieveDomainAuthCodeResponse
--
--         , responseTransferDomain $
--             transferDomainResponse
--
--         , responseListDomains $
--             listDomainsResponse
--
--         , responseCancelDomainTransferToAnotherAWSAccount $
--             cancelDomainTransferToAnotherAWSAccountResponse
--
--           ]
--     ]

-- Requests

requestListOperations :: ListOperations -> TestTree
requestListOperations =
  req
    "ListOperations"
    "fixture/ListOperations.yaml"

requestGetDomainDetail :: GetDomainDetail -> TestTree
requestGetDomainDetail =
  req
    "GetDomainDetail"
    "fixture/GetDomainDetail.yaml"

requestCheckDomainTransferability :: CheckDomainTransferability -> TestTree
requestCheckDomainTransferability =
  req
    "CheckDomainTransferability"
    "fixture/CheckDomainTransferability.yaml"

requestUpdateDomainContactPrivacy :: UpdateDomainContactPrivacy -> TestTree
requestUpdateDomainContactPrivacy =
  req
    "UpdateDomainContactPrivacy"
    "fixture/UpdateDomainContactPrivacy.yaml"

requestGetOperationDetail :: GetOperationDetail -> TestTree
requestGetOperationDetail =
  req
    "GetOperationDetail"
    "fixture/GetOperationDetail.yaml"

requestRejectDomainTransferFromAnotherAWSAccount :: RejectDomainTransferFromAnotherAWSAccount -> TestTree
requestRejectDomainTransferFromAnotherAWSAccount =
  req
    "RejectDomainTransferFromAnotherAWSAccount"
    "fixture/RejectDomainTransferFromAnotherAWSAccount.yaml"

requestEnableDomainAutoRenew :: EnableDomainAutoRenew -> TestTree
requestEnableDomainAutoRenew =
  req
    "EnableDomainAutoRenew"
    "fixture/EnableDomainAutoRenew.yaml"

requestResendContactReachabilityEmail :: ResendContactReachabilityEmail -> TestTree
requestResendContactReachabilityEmail =
  req
    "ResendContactReachabilityEmail"
    "fixture/ResendContactReachabilityEmail.yaml"

requestDisableDomainAutoRenew :: DisableDomainAutoRenew -> TestTree
requestDisableDomainAutoRenew =
  req
    "DisableDomainAutoRenew"
    "fixture/DisableDomainAutoRenew.yaml"

requestRenewDomain :: RenewDomain -> TestTree
requestRenewDomain =
  req
    "RenewDomain"
    "fixture/RenewDomain.yaml"

requestViewBilling :: ViewBilling -> TestTree
requestViewBilling =
  req
    "ViewBilling"
    "fixture/ViewBilling.yaml"

requestUpdateDomainContact :: UpdateDomainContact -> TestTree
requestUpdateDomainContact =
  req
    "UpdateDomainContact"
    "fixture/UpdateDomainContact.yaml"

requestEnableDomainTransferLock :: EnableDomainTransferLock -> TestTree
requestEnableDomainTransferLock =
  req
    "EnableDomainTransferLock"
    "fixture/EnableDomainTransferLock.yaml"

requestRegisterDomain :: RegisterDomain -> TestTree
requestRegisterDomain =
  req
    "RegisterDomain"
    "fixture/RegisterDomain.yaml"

requestGetDomainSuggestions :: GetDomainSuggestions -> TestTree
requestGetDomainSuggestions =
  req
    "GetDomainSuggestions"
    "fixture/GetDomainSuggestions.yaml"

requestDisableDomainTransferLock :: DisableDomainTransferLock -> TestTree
requestDisableDomainTransferLock =
  req
    "DisableDomainTransferLock"
    "fixture/DisableDomainTransferLock.yaml"

requestCheckDomainAvailability :: CheckDomainAvailability -> TestTree
requestCheckDomainAvailability =
  req
    "CheckDomainAvailability"
    "fixture/CheckDomainAvailability.yaml"

requestTransferDomainToAnotherAWSAccount :: TransferDomainToAnotherAWSAccount -> TestTree
requestTransferDomainToAnotherAWSAccount =
  req
    "TransferDomainToAnotherAWSAccount"
    "fixture/TransferDomainToAnotherAWSAccount.yaml"

requestAcceptDomainTransferFromAnotherAWSAccount :: AcceptDomainTransferFromAnotherAWSAccount -> TestTree
requestAcceptDomainTransferFromAnotherAWSAccount =
  req
    "AcceptDomainTransferFromAnotherAWSAccount"
    "fixture/AcceptDomainTransferFromAnotherAWSAccount.yaml"

requestGetContactReachabilityStatus :: GetContactReachabilityStatus -> TestTree
requestGetContactReachabilityStatus =
  req
    "GetContactReachabilityStatus"
    "fixture/GetContactReachabilityStatus.yaml"

requestListTagsForDomain :: ListTagsForDomain -> TestTree
requestListTagsForDomain =
  req
    "ListTagsForDomain"
    "fixture/ListTagsForDomain.yaml"

requestUpdateDomainNameservers :: UpdateDomainNameservers -> TestTree
requestUpdateDomainNameservers =
  req
    "UpdateDomainNameservers"
    "fixture/UpdateDomainNameservers.yaml"

requestDeleteTagsForDomain :: DeleteTagsForDomain -> TestTree
requestDeleteTagsForDomain =
  req
    "DeleteTagsForDomain"
    "fixture/DeleteTagsForDomain.yaml"

requestUpdateTagsForDomain :: UpdateTagsForDomain -> TestTree
requestUpdateTagsForDomain =
  req
    "UpdateTagsForDomain"
    "fixture/UpdateTagsForDomain.yaml"

requestRetrieveDomainAuthCode :: RetrieveDomainAuthCode -> TestTree
requestRetrieveDomainAuthCode =
  req
    "RetrieveDomainAuthCode"
    "fixture/RetrieveDomainAuthCode.yaml"

requestTransferDomain :: TransferDomain -> TestTree
requestTransferDomain =
  req
    "TransferDomain"
    "fixture/TransferDomain.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestCancelDomainTransferToAnotherAWSAccount :: CancelDomainTransferToAnotherAWSAccount -> TestTree
requestCancelDomainTransferToAnotherAWSAccount =
  req
    "CancelDomainTransferToAnotherAWSAccount"
    "fixture/CancelDomainTransferToAnotherAWSAccount.yaml"

-- Responses

responseListOperations :: ListOperationsResponse -> TestTree
responseListOperations =
  res
    "ListOperationsResponse"
    "fixture/ListOperationsResponse.proto"
    route53Domains
    (Proxy :: Proxy ListOperations)

responseGetDomainDetail :: GetDomainDetailResponse -> TestTree
responseGetDomainDetail =
  res
    "GetDomainDetailResponse"
    "fixture/GetDomainDetailResponse.proto"
    route53Domains
    (Proxy :: Proxy GetDomainDetail)

responseCheckDomainTransferability :: CheckDomainTransferabilityResponse -> TestTree
responseCheckDomainTransferability =
  res
    "CheckDomainTransferabilityResponse"
    "fixture/CheckDomainTransferabilityResponse.proto"
    route53Domains
    (Proxy :: Proxy CheckDomainTransferability)

responseUpdateDomainContactPrivacy :: UpdateDomainContactPrivacyResponse -> TestTree
responseUpdateDomainContactPrivacy =
  res
    "UpdateDomainContactPrivacyResponse"
    "fixture/UpdateDomainContactPrivacyResponse.proto"
    route53Domains
    (Proxy :: Proxy UpdateDomainContactPrivacy)

responseGetOperationDetail :: GetOperationDetailResponse -> TestTree
responseGetOperationDetail =
  res
    "GetOperationDetailResponse"
    "fixture/GetOperationDetailResponse.proto"
    route53Domains
    (Proxy :: Proxy GetOperationDetail)

responseRejectDomainTransferFromAnotherAWSAccount :: RejectDomainTransferFromAnotherAWSAccountResponse -> TestTree
responseRejectDomainTransferFromAnotherAWSAccount =
  res
    "RejectDomainTransferFromAnotherAWSAccountResponse"
    "fixture/RejectDomainTransferFromAnotherAWSAccountResponse.proto"
    route53Domains
    (Proxy :: Proxy RejectDomainTransferFromAnotherAWSAccount)

responseEnableDomainAutoRenew :: EnableDomainAutoRenewResponse -> TestTree
responseEnableDomainAutoRenew =
  res
    "EnableDomainAutoRenewResponse"
    "fixture/EnableDomainAutoRenewResponse.proto"
    route53Domains
    (Proxy :: Proxy EnableDomainAutoRenew)

responseResendContactReachabilityEmail :: ResendContactReachabilityEmailResponse -> TestTree
responseResendContactReachabilityEmail =
  res
    "ResendContactReachabilityEmailResponse"
    "fixture/ResendContactReachabilityEmailResponse.proto"
    route53Domains
    (Proxy :: Proxy ResendContactReachabilityEmail)

responseDisableDomainAutoRenew :: DisableDomainAutoRenewResponse -> TestTree
responseDisableDomainAutoRenew =
  res
    "DisableDomainAutoRenewResponse"
    "fixture/DisableDomainAutoRenewResponse.proto"
    route53Domains
    (Proxy :: Proxy DisableDomainAutoRenew)

responseRenewDomain :: RenewDomainResponse -> TestTree
responseRenewDomain =
  res
    "RenewDomainResponse"
    "fixture/RenewDomainResponse.proto"
    route53Domains
    (Proxy :: Proxy RenewDomain)

responseViewBilling :: ViewBillingResponse -> TestTree
responseViewBilling =
  res
    "ViewBillingResponse"
    "fixture/ViewBillingResponse.proto"
    route53Domains
    (Proxy :: Proxy ViewBilling)

responseUpdateDomainContact :: UpdateDomainContactResponse -> TestTree
responseUpdateDomainContact =
  res
    "UpdateDomainContactResponse"
    "fixture/UpdateDomainContactResponse.proto"
    route53Domains
    (Proxy :: Proxy UpdateDomainContact)

responseEnableDomainTransferLock :: EnableDomainTransferLockResponse -> TestTree
responseEnableDomainTransferLock =
  res
    "EnableDomainTransferLockResponse"
    "fixture/EnableDomainTransferLockResponse.proto"
    route53Domains
    (Proxy :: Proxy EnableDomainTransferLock)

responseRegisterDomain :: RegisterDomainResponse -> TestTree
responseRegisterDomain =
  res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse.proto"
    route53Domains
    (Proxy :: Proxy RegisterDomain)

responseGetDomainSuggestions :: GetDomainSuggestionsResponse -> TestTree
responseGetDomainSuggestions =
  res
    "GetDomainSuggestionsResponse"
    "fixture/GetDomainSuggestionsResponse.proto"
    route53Domains
    (Proxy :: Proxy GetDomainSuggestions)

responseDisableDomainTransferLock :: DisableDomainTransferLockResponse -> TestTree
responseDisableDomainTransferLock =
  res
    "DisableDomainTransferLockResponse"
    "fixture/DisableDomainTransferLockResponse.proto"
    route53Domains
    (Proxy :: Proxy DisableDomainTransferLock)

responseCheckDomainAvailability :: CheckDomainAvailabilityResponse -> TestTree
responseCheckDomainAvailability =
  res
    "CheckDomainAvailabilityResponse"
    "fixture/CheckDomainAvailabilityResponse.proto"
    route53Domains
    (Proxy :: Proxy CheckDomainAvailability)

responseTransferDomainToAnotherAWSAccount :: TransferDomainToAnotherAWSAccountResponse -> TestTree
responseTransferDomainToAnotherAWSAccount =
  res
    "TransferDomainToAnotherAWSAccountResponse"
    "fixture/TransferDomainToAnotherAWSAccountResponse.proto"
    route53Domains
    (Proxy :: Proxy TransferDomainToAnotherAWSAccount)

responseAcceptDomainTransferFromAnotherAWSAccount :: AcceptDomainTransferFromAnotherAWSAccountResponse -> TestTree
responseAcceptDomainTransferFromAnotherAWSAccount =
  res
    "AcceptDomainTransferFromAnotherAWSAccountResponse"
    "fixture/AcceptDomainTransferFromAnotherAWSAccountResponse.proto"
    route53Domains
    (Proxy :: Proxy AcceptDomainTransferFromAnotherAWSAccount)

responseGetContactReachabilityStatus :: GetContactReachabilityStatusResponse -> TestTree
responseGetContactReachabilityStatus =
  res
    "GetContactReachabilityStatusResponse"
    "fixture/GetContactReachabilityStatusResponse.proto"
    route53Domains
    (Proxy :: Proxy GetContactReachabilityStatus)

responseListTagsForDomain :: ListTagsForDomainResponse -> TestTree
responseListTagsForDomain =
  res
    "ListTagsForDomainResponse"
    "fixture/ListTagsForDomainResponse.proto"
    route53Domains
    (Proxy :: Proxy ListTagsForDomain)

responseUpdateDomainNameservers :: UpdateDomainNameserversResponse -> TestTree
responseUpdateDomainNameservers =
  res
    "UpdateDomainNameserversResponse"
    "fixture/UpdateDomainNameserversResponse.proto"
    route53Domains
    (Proxy :: Proxy UpdateDomainNameservers)

responseDeleteTagsForDomain :: DeleteTagsForDomainResponse -> TestTree
responseDeleteTagsForDomain =
  res
    "DeleteTagsForDomainResponse"
    "fixture/DeleteTagsForDomainResponse.proto"
    route53Domains
    (Proxy :: Proxy DeleteTagsForDomain)

responseUpdateTagsForDomain :: UpdateTagsForDomainResponse -> TestTree
responseUpdateTagsForDomain =
  res
    "UpdateTagsForDomainResponse"
    "fixture/UpdateTagsForDomainResponse.proto"
    route53Domains
    (Proxy :: Proxy UpdateTagsForDomain)

responseRetrieveDomainAuthCode :: RetrieveDomainAuthCodeResponse -> TestTree
responseRetrieveDomainAuthCode =
  res
    "RetrieveDomainAuthCodeResponse"
    "fixture/RetrieveDomainAuthCodeResponse.proto"
    route53Domains
    (Proxy :: Proxy RetrieveDomainAuthCode)

responseTransferDomain :: TransferDomainResponse -> TestTree
responseTransferDomain =
  res
    "TransferDomainResponse"
    "fixture/TransferDomainResponse.proto"
    route53Domains
    (Proxy :: Proxy TransferDomain)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    route53Domains
    (Proxy :: Proxy ListDomains)

responseCancelDomainTransferToAnotherAWSAccount :: CancelDomainTransferToAnotherAWSAccountResponse -> TestTree
responseCancelDomainTransferToAnotherAWSAccount =
  res
    "CancelDomainTransferToAnotherAWSAccountResponse"
    "fixture/CancelDomainTransferToAnotherAWSAccountResponse.proto"
    route53Domains
    (Proxy :: Proxy CancelDomainTransferToAnotherAWSAccount)
