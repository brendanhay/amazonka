{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53Domains
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Route53Domains where

import Amazonka.Route53Domains
import qualified Data.Proxy as Proxy
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
--             newListOperations
--
--         , requestGetDomainDetail $
--             newGetDomainDetail
--
--         , requestCheckDomainTransferability $
--             newCheckDomainTransferability
--
--         , requestUpdateDomainContactPrivacy $
--             newUpdateDomainContactPrivacy
--
--         , requestGetOperationDetail $
--             newGetOperationDetail
--
--         , requestRejectDomainTransferFromAnotherAwsAccount $
--             newRejectDomainTransferFromAnotherAwsAccount
--
--         , requestEnableDomainAutoRenew $
--             newEnableDomainAutoRenew
--
--         , requestResendContactReachabilityEmail $
--             newResendContactReachabilityEmail
--
--         , requestDisableDomainAutoRenew $
--             newDisableDomainAutoRenew
--
--         , requestRenewDomain $
--             newRenewDomain
--
--         , requestViewBilling $
--             newViewBilling
--
--         , requestUpdateDomainContact $
--             newUpdateDomainContact
--
--         , requestEnableDomainTransferLock $
--             newEnableDomainTransferLock
--
--         , requestRegisterDomain $
--             newRegisterDomain
--
--         , requestGetDomainSuggestions $
--             newGetDomainSuggestions
--
--         , requestDisableDomainTransferLock $
--             newDisableDomainTransferLock
--
--         , requestCheckDomainAvailability $
--             newCheckDomainAvailability
--
--         , requestTransferDomainToAnotherAwsAccount $
--             newTransferDomainToAnotherAwsAccount
--
--         , requestAcceptDomainTransferFromAnotherAwsAccount $
--             newAcceptDomainTransferFromAnotherAwsAccount
--
--         , requestGetContactReachabilityStatus $
--             newGetContactReachabilityStatus
--
--         , requestListTagsForDomain $
--             newListTagsForDomain
--
--         , requestUpdateDomainNameservers $
--             newUpdateDomainNameservers
--
--         , requestDeleteTagsForDomain $
--             newDeleteTagsForDomain
--
--         , requestUpdateTagsForDomain $
--             newUpdateTagsForDomain
--
--         , requestRetrieveDomainAuthCode $
--             newRetrieveDomainAuthCode
--
--         , requestTransferDomain $
--             newTransferDomain
--
--         , requestListDomains $
--             newListDomains
--
--         , requestCancelDomainTransferToAnotherAwsAccount $
--             newCancelDomainTransferToAnotherAwsAccount
--
--           ]

--     , testGroup "response"
--         [ responseListOperations $
--             newListOperationsResponse
--
--         , responseGetDomainDetail $
--             newGetDomainDetailResponse
--
--         , responseCheckDomainTransferability $
--             newCheckDomainTransferabilityResponse
--
--         , responseUpdateDomainContactPrivacy $
--             newUpdateDomainContactPrivacyResponse
--
--         , responseGetOperationDetail $
--             newGetOperationDetailResponse
--
--         , responseRejectDomainTransferFromAnotherAwsAccount $
--             newRejectDomainTransferFromAnotherAwsAccountResponse
--
--         , responseEnableDomainAutoRenew $
--             newEnableDomainAutoRenewResponse
--
--         , responseResendContactReachabilityEmail $
--             newResendContactReachabilityEmailResponse
--
--         , responseDisableDomainAutoRenew $
--             newDisableDomainAutoRenewResponse
--
--         , responseRenewDomain $
--             newRenewDomainResponse
--
--         , responseViewBilling $
--             newViewBillingResponse
--
--         , responseUpdateDomainContact $
--             newUpdateDomainContactResponse
--
--         , responseEnableDomainTransferLock $
--             newEnableDomainTransferLockResponse
--
--         , responseRegisterDomain $
--             newRegisterDomainResponse
--
--         , responseGetDomainSuggestions $
--             newGetDomainSuggestionsResponse
--
--         , responseDisableDomainTransferLock $
--             newDisableDomainTransferLockResponse
--
--         , responseCheckDomainAvailability $
--             newCheckDomainAvailabilityResponse
--
--         , responseTransferDomainToAnotherAwsAccount $
--             newTransferDomainToAnotherAwsAccountResponse
--
--         , responseAcceptDomainTransferFromAnotherAwsAccount $
--             newAcceptDomainTransferFromAnotherAwsAccountResponse
--
--         , responseGetContactReachabilityStatus $
--             newGetContactReachabilityStatusResponse
--
--         , responseListTagsForDomain $
--             newListTagsForDomainResponse
--
--         , responseUpdateDomainNameservers $
--             newUpdateDomainNameserversResponse
--
--         , responseDeleteTagsForDomain $
--             newDeleteTagsForDomainResponse
--
--         , responseUpdateTagsForDomain $
--             newUpdateTagsForDomainResponse
--
--         , responseRetrieveDomainAuthCode $
--             newRetrieveDomainAuthCodeResponse
--
--         , responseTransferDomain $
--             newTransferDomainResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseCancelDomainTransferToAnotherAwsAccount $
--             newCancelDomainTransferToAnotherAwsAccountResponse
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

requestRejectDomainTransferFromAnotherAwsAccount :: RejectDomainTransferFromAnotherAwsAccount -> TestTree
requestRejectDomainTransferFromAnotherAwsAccount =
  req
    "RejectDomainTransferFromAnotherAwsAccount"
    "fixture/RejectDomainTransferFromAnotherAwsAccount.yaml"

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

requestTransferDomainToAnotherAwsAccount :: TransferDomainToAnotherAwsAccount -> TestTree
requestTransferDomainToAnotherAwsAccount =
  req
    "TransferDomainToAnotherAwsAccount"
    "fixture/TransferDomainToAnotherAwsAccount.yaml"

requestAcceptDomainTransferFromAnotherAwsAccount :: AcceptDomainTransferFromAnotherAwsAccount -> TestTree
requestAcceptDomainTransferFromAnotherAwsAccount =
  req
    "AcceptDomainTransferFromAnotherAwsAccount"
    "fixture/AcceptDomainTransferFromAnotherAwsAccount.yaml"

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

requestCancelDomainTransferToAnotherAwsAccount :: CancelDomainTransferToAnotherAwsAccount -> TestTree
requestCancelDomainTransferToAnotherAwsAccount =
  req
    "CancelDomainTransferToAnotherAwsAccount"
    "fixture/CancelDomainTransferToAnotherAwsAccount.yaml"

-- Responses

responseListOperations :: ListOperationsResponse -> TestTree
responseListOperations =
  res
    "ListOperationsResponse"
    "fixture/ListOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOperations)

responseGetDomainDetail :: GetDomainDetailResponse -> TestTree
responseGetDomainDetail =
  res
    "GetDomainDetailResponse"
    "fixture/GetDomainDetailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainDetail)

responseCheckDomainTransferability :: CheckDomainTransferabilityResponse -> TestTree
responseCheckDomainTransferability =
  res
    "CheckDomainTransferabilityResponse"
    "fixture/CheckDomainTransferabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckDomainTransferability)

responseUpdateDomainContactPrivacy :: UpdateDomainContactPrivacyResponse -> TestTree
responseUpdateDomainContactPrivacy =
  res
    "UpdateDomainContactPrivacyResponse"
    "fixture/UpdateDomainContactPrivacyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainContactPrivacy)

responseGetOperationDetail :: GetOperationDetailResponse -> TestTree
responseGetOperationDetail =
  res
    "GetOperationDetailResponse"
    "fixture/GetOperationDetailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOperationDetail)

responseRejectDomainTransferFromAnotherAwsAccount :: RejectDomainTransferFromAnotherAwsAccountResponse -> TestTree
responseRejectDomainTransferFromAnotherAwsAccount =
  res
    "RejectDomainTransferFromAnotherAwsAccountResponse"
    "fixture/RejectDomainTransferFromAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectDomainTransferFromAnotherAwsAccount)

responseEnableDomainAutoRenew :: EnableDomainAutoRenewResponse -> TestTree
responseEnableDomainAutoRenew =
  res
    "EnableDomainAutoRenewResponse"
    "fixture/EnableDomainAutoRenewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableDomainAutoRenew)

responseResendContactReachabilityEmail :: ResendContactReachabilityEmailResponse -> TestTree
responseResendContactReachabilityEmail =
  res
    "ResendContactReachabilityEmailResponse"
    "fixture/ResendContactReachabilityEmailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResendContactReachabilityEmail)

responseDisableDomainAutoRenew :: DisableDomainAutoRenewResponse -> TestTree
responseDisableDomainAutoRenew =
  res
    "DisableDomainAutoRenewResponse"
    "fixture/DisableDomainAutoRenewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableDomainAutoRenew)

responseRenewDomain :: RenewDomainResponse -> TestTree
responseRenewDomain =
  res
    "RenewDomainResponse"
    "fixture/RenewDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RenewDomain)

responseViewBilling :: ViewBillingResponse -> TestTree
responseViewBilling =
  res
    "ViewBillingResponse"
    "fixture/ViewBillingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ViewBilling)

responseUpdateDomainContact :: UpdateDomainContactResponse -> TestTree
responseUpdateDomainContact =
  res
    "UpdateDomainContactResponse"
    "fixture/UpdateDomainContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainContact)

responseEnableDomainTransferLock :: EnableDomainTransferLockResponse -> TestTree
responseEnableDomainTransferLock =
  res
    "EnableDomainTransferLockResponse"
    "fixture/EnableDomainTransferLockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableDomainTransferLock)

responseRegisterDomain :: RegisterDomainResponse -> TestTree
responseRegisterDomain =
  res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDomain)

responseGetDomainSuggestions :: GetDomainSuggestionsResponse -> TestTree
responseGetDomainSuggestions =
  res
    "GetDomainSuggestionsResponse"
    "fixture/GetDomainSuggestionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainSuggestions)

responseDisableDomainTransferLock :: DisableDomainTransferLockResponse -> TestTree
responseDisableDomainTransferLock =
  res
    "DisableDomainTransferLockResponse"
    "fixture/DisableDomainTransferLockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableDomainTransferLock)

responseCheckDomainAvailability :: CheckDomainAvailabilityResponse -> TestTree
responseCheckDomainAvailability =
  res
    "CheckDomainAvailabilityResponse"
    "fixture/CheckDomainAvailabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckDomainAvailability)

responseTransferDomainToAnotherAwsAccount :: TransferDomainToAnotherAwsAccountResponse -> TestTree
responseTransferDomainToAnotherAwsAccount =
  res
    "TransferDomainToAnotherAwsAccountResponse"
    "fixture/TransferDomainToAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TransferDomainToAnotherAwsAccount)

responseAcceptDomainTransferFromAnotherAwsAccount :: AcceptDomainTransferFromAnotherAwsAccountResponse -> TestTree
responseAcceptDomainTransferFromAnotherAwsAccount =
  res
    "AcceptDomainTransferFromAnotherAwsAccountResponse"
    "fixture/AcceptDomainTransferFromAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptDomainTransferFromAnotherAwsAccount)

responseGetContactReachabilityStatus :: GetContactReachabilityStatusResponse -> TestTree
responseGetContactReachabilityStatus =
  res
    "GetContactReachabilityStatusResponse"
    "fixture/GetContactReachabilityStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContactReachabilityStatus)

responseListTagsForDomain :: ListTagsForDomainResponse -> TestTree
responseListTagsForDomain =
  res
    "ListTagsForDomainResponse"
    "fixture/ListTagsForDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForDomain)

responseUpdateDomainNameservers :: UpdateDomainNameserversResponse -> TestTree
responseUpdateDomainNameservers =
  res
    "UpdateDomainNameserversResponse"
    "fixture/UpdateDomainNameserversResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainNameservers)

responseDeleteTagsForDomain :: DeleteTagsForDomainResponse -> TestTree
responseDeleteTagsForDomain =
  res
    "DeleteTagsForDomainResponse"
    "fixture/DeleteTagsForDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTagsForDomain)

responseUpdateTagsForDomain :: UpdateTagsForDomainResponse -> TestTree
responseUpdateTagsForDomain =
  res
    "UpdateTagsForDomainResponse"
    "fixture/UpdateTagsForDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTagsForDomain)

responseRetrieveDomainAuthCode :: RetrieveDomainAuthCodeResponse -> TestTree
responseRetrieveDomainAuthCode =
  res
    "RetrieveDomainAuthCodeResponse"
    "fixture/RetrieveDomainAuthCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetrieveDomainAuthCode)

responseTransferDomain :: TransferDomainResponse -> TestTree
responseTransferDomain =
  res
    "TransferDomainResponse"
    "fixture/TransferDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TransferDomain)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseCancelDomainTransferToAnotherAwsAccount :: CancelDomainTransferToAnotherAwsAccountResponse -> TestTree
responseCancelDomainTransferToAnotherAwsAccount =
  res
    "CancelDomainTransferToAnotherAwsAccountResponse"
    "fixture/CancelDomainTransferToAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelDomainTransferToAnotherAwsAccount)
