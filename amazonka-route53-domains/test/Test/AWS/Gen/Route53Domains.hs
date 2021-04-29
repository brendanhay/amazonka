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
--         [ requestTransferDomainToAnotherAwsAccount $
--             newTransferDomainToAnotherAwsAccount
--
--         , requestUpdateDomainContactPrivacy $
--             newUpdateDomainContactPrivacy
--
--         , requestCheckDomainAvailability $
--             newCheckDomainAvailability
--
--         , requestCheckDomainTransferability $
--             newCheckDomainTransferability
--
--         , requestListOperations $
--             newListOperations
--
--         , requestDisableDomainTransferLock $
--             newDisableDomainTransferLock
--
--         , requestRegisterDomain $
--             newRegisterDomain
--
--         , requestGetDomainSuggestions $
--             newGetDomainSuggestions
--
--         , requestListDomains $
--             newListDomains
--
--         , requestCancelDomainTransferToAnotherAwsAccount $
--             newCancelDomainTransferToAnotherAwsAccount
--
--         , requestEnableDomainTransferLock $
--             newEnableDomainTransferLock
--
--         , requestViewBilling $
--             newViewBilling
--
--         , requestDeleteTagsForDomain $
--             newDeleteTagsForDomain
--
--         , requestUpdateTagsForDomain $
--             newUpdateTagsForDomain
--
--         , requestListTagsForDomain $
--             newListTagsForDomain
--
--         , requestResendContactReachabilityEmail $
--             newResendContactReachabilityEmail
--
--         , requestDisableDomainAutoRenew $
--             newDisableDomainAutoRenew
--
--         , requestUpdateDomainNameservers $
--             newUpdateDomainNameservers
--
--         , requestEnableDomainAutoRenew $
--             newEnableDomainAutoRenew
--
--         , requestGetContactReachabilityStatus $
--             newGetContactReachabilityStatus
--
--         , requestRejectDomainTransferFromAnotherAwsAccount $
--             newRejectDomainTransferFromAnotherAwsAccount
--
--         , requestAcceptDomainTransferFromAnotherAwsAccount $
--             newAcceptDomainTransferFromAnotherAwsAccount
--
--         , requestGetOperationDetail $
--             newGetOperationDetail
--
--         , requestGetDomainDetail $
--             newGetDomainDetail
--
--         , requestUpdateDomainContact $
--             newUpdateDomainContact
--
--         , requestTransferDomain $
--             newTransferDomain
--
--         , requestRenewDomain $
--             newRenewDomain
--
--         , requestRetrieveDomainAuthCode $
--             newRetrieveDomainAuthCode
--
--           ]

--     , testGroup "response"
--         [ responseTransferDomainToAnotherAwsAccount $
--             newTransferDomainToAnotherAwsAccountResponse
--
--         , responseUpdateDomainContactPrivacy $
--             newUpdateDomainContactPrivacyResponse
--
--         , responseCheckDomainAvailability $
--             newCheckDomainAvailabilityResponse
--
--         , responseCheckDomainTransferability $
--             newCheckDomainTransferabilityResponse
--
--         , responseListOperations $
--             newListOperationsResponse
--
--         , responseDisableDomainTransferLock $
--             newDisableDomainTransferLockResponse
--
--         , responseRegisterDomain $
--             newRegisterDomainResponse
--
--         , responseGetDomainSuggestions $
--             newGetDomainSuggestionsResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseCancelDomainTransferToAnotherAwsAccount $
--             newCancelDomainTransferToAnotherAwsAccountResponse
--
--         , responseEnableDomainTransferLock $
--             newEnableDomainTransferLockResponse
--
--         , responseViewBilling $
--             newViewBillingResponse
--
--         , responseDeleteTagsForDomain $
--             newDeleteTagsForDomainResponse
--
--         , responseUpdateTagsForDomain $
--             newUpdateTagsForDomainResponse
--
--         , responseListTagsForDomain $
--             newListTagsForDomainResponse
--
--         , responseResendContactReachabilityEmail $
--             newResendContactReachabilityEmailResponse
--
--         , responseDisableDomainAutoRenew $
--             newDisableDomainAutoRenewResponse
--
--         , responseUpdateDomainNameservers $
--             newUpdateDomainNameserversResponse
--
--         , responseEnableDomainAutoRenew $
--             newEnableDomainAutoRenewResponse
--
--         , responseGetContactReachabilityStatus $
--             newGetContactReachabilityStatusResponse
--
--         , responseRejectDomainTransferFromAnotherAwsAccount $
--             newRejectDomainTransferFromAnotherAwsAccountResponse
--
--         , responseAcceptDomainTransferFromAnotherAwsAccount $
--             newAcceptDomainTransferFromAnotherAwsAccountResponse
--
--         , responseGetOperationDetail $
--             newGetOperationDetailResponse
--
--         , responseGetDomainDetail $
--             newGetDomainDetailResponse
--
--         , responseUpdateDomainContact $
--             newUpdateDomainContactResponse
--
--         , responseTransferDomain $
--             newTransferDomainResponse
--
--         , responseRenewDomain $
--             newRenewDomainResponse
--
--         , responseRetrieveDomainAuthCode $
--             newRetrieveDomainAuthCodeResponse
--
--           ]
--     ]

-- Requests

requestTransferDomainToAnotherAwsAccount :: TransferDomainToAnotherAwsAccount -> TestTree
requestTransferDomainToAnotherAwsAccount =
  req
    "TransferDomainToAnotherAwsAccount"
    "fixture/TransferDomainToAnotherAwsAccount.yaml"

requestUpdateDomainContactPrivacy :: UpdateDomainContactPrivacy -> TestTree
requestUpdateDomainContactPrivacy =
  req
    "UpdateDomainContactPrivacy"
    "fixture/UpdateDomainContactPrivacy.yaml"

requestCheckDomainAvailability :: CheckDomainAvailability -> TestTree
requestCheckDomainAvailability =
  req
    "CheckDomainAvailability"
    "fixture/CheckDomainAvailability.yaml"

requestCheckDomainTransferability :: CheckDomainTransferability -> TestTree
requestCheckDomainTransferability =
  req
    "CheckDomainTransferability"
    "fixture/CheckDomainTransferability.yaml"

requestListOperations :: ListOperations -> TestTree
requestListOperations =
  req
    "ListOperations"
    "fixture/ListOperations.yaml"

requestDisableDomainTransferLock :: DisableDomainTransferLock -> TestTree
requestDisableDomainTransferLock =
  req
    "DisableDomainTransferLock"
    "fixture/DisableDomainTransferLock.yaml"

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

requestEnableDomainTransferLock :: EnableDomainTransferLock -> TestTree
requestEnableDomainTransferLock =
  req
    "EnableDomainTransferLock"
    "fixture/EnableDomainTransferLock.yaml"

requestViewBilling :: ViewBilling -> TestTree
requestViewBilling =
  req
    "ViewBilling"
    "fixture/ViewBilling.yaml"

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

requestListTagsForDomain :: ListTagsForDomain -> TestTree
requestListTagsForDomain =
  req
    "ListTagsForDomain"
    "fixture/ListTagsForDomain.yaml"

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

requestUpdateDomainNameservers :: UpdateDomainNameservers -> TestTree
requestUpdateDomainNameservers =
  req
    "UpdateDomainNameservers"
    "fixture/UpdateDomainNameservers.yaml"

requestEnableDomainAutoRenew :: EnableDomainAutoRenew -> TestTree
requestEnableDomainAutoRenew =
  req
    "EnableDomainAutoRenew"
    "fixture/EnableDomainAutoRenew.yaml"

requestGetContactReachabilityStatus :: GetContactReachabilityStatus -> TestTree
requestGetContactReachabilityStatus =
  req
    "GetContactReachabilityStatus"
    "fixture/GetContactReachabilityStatus.yaml"

requestRejectDomainTransferFromAnotherAwsAccount :: RejectDomainTransferFromAnotherAwsAccount -> TestTree
requestRejectDomainTransferFromAnotherAwsAccount =
  req
    "RejectDomainTransferFromAnotherAwsAccount"
    "fixture/RejectDomainTransferFromAnotherAwsAccount.yaml"

requestAcceptDomainTransferFromAnotherAwsAccount :: AcceptDomainTransferFromAnotherAwsAccount -> TestTree
requestAcceptDomainTransferFromAnotherAwsAccount =
  req
    "AcceptDomainTransferFromAnotherAwsAccount"
    "fixture/AcceptDomainTransferFromAnotherAwsAccount.yaml"

requestGetOperationDetail :: GetOperationDetail -> TestTree
requestGetOperationDetail =
  req
    "GetOperationDetail"
    "fixture/GetOperationDetail.yaml"

requestGetDomainDetail :: GetDomainDetail -> TestTree
requestGetDomainDetail =
  req
    "GetDomainDetail"
    "fixture/GetDomainDetail.yaml"

requestUpdateDomainContact :: UpdateDomainContact -> TestTree
requestUpdateDomainContact =
  req
    "UpdateDomainContact"
    "fixture/UpdateDomainContact.yaml"

requestTransferDomain :: TransferDomain -> TestTree
requestTransferDomain =
  req
    "TransferDomain"
    "fixture/TransferDomain.yaml"

requestRenewDomain :: RenewDomain -> TestTree
requestRenewDomain =
  req
    "RenewDomain"
    "fixture/RenewDomain.yaml"

requestRetrieveDomainAuthCode :: RetrieveDomainAuthCode -> TestTree
requestRetrieveDomainAuthCode =
  req
    "RetrieveDomainAuthCode"
    "fixture/RetrieveDomainAuthCode.yaml"

-- Responses

responseTransferDomainToAnotherAwsAccount :: TransferDomainToAnotherAwsAccountResponse -> TestTree
responseTransferDomainToAnotherAwsAccount =
  res
    "TransferDomainToAnotherAwsAccountResponse"
    "fixture/TransferDomainToAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy :: Proxy TransferDomainToAnotherAwsAccount)

responseUpdateDomainContactPrivacy :: UpdateDomainContactPrivacyResponse -> TestTree
responseUpdateDomainContactPrivacy =
  res
    "UpdateDomainContactPrivacyResponse"
    "fixture/UpdateDomainContactPrivacyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomainContactPrivacy)

responseCheckDomainAvailability :: CheckDomainAvailabilityResponse -> TestTree
responseCheckDomainAvailability =
  res
    "CheckDomainAvailabilityResponse"
    "fixture/CheckDomainAvailabilityResponse.proto"
    defaultService
    (Proxy :: Proxy CheckDomainAvailability)

responseCheckDomainTransferability :: CheckDomainTransferabilityResponse -> TestTree
responseCheckDomainTransferability =
  res
    "CheckDomainTransferabilityResponse"
    "fixture/CheckDomainTransferabilityResponse.proto"
    defaultService
    (Proxy :: Proxy CheckDomainTransferability)

responseListOperations :: ListOperationsResponse -> TestTree
responseListOperations =
  res
    "ListOperationsResponse"
    "fixture/ListOperationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOperations)

responseDisableDomainTransferLock :: DisableDomainTransferLockResponse -> TestTree
responseDisableDomainTransferLock =
  res
    "DisableDomainTransferLockResponse"
    "fixture/DisableDomainTransferLockResponse.proto"
    defaultService
    (Proxy :: Proxy DisableDomainTransferLock)

responseRegisterDomain :: RegisterDomainResponse -> TestTree
responseRegisterDomain =
  res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterDomain)

responseGetDomainSuggestions :: GetDomainSuggestionsResponse -> TestTree
responseGetDomainSuggestions =
  res
    "GetDomainSuggestionsResponse"
    "fixture/GetDomainSuggestionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomainSuggestions)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomains)

responseCancelDomainTransferToAnotherAwsAccount :: CancelDomainTransferToAnotherAwsAccountResponse -> TestTree
responseCancelDomainTransferToAnotherAwsAccount =
  res
    "CancelDomainTransferToAnotherAwsAccountResponse"
    "fixture/CancelDomainTransferToAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy :: Proxy CancelDomainTransferToAnotherAwsAccount)

responseEnableDomainTransferLock :: EnableDomainTransferLockResponse -> TestTree
responseEnableDomainTransferLock =
  res
    "EnableDomainTransferLockResponse"
    "fixture/EnableDomainTransferLockResponse.proto"
    defaultService
    (Proxy :: Proxy EnableDomainTransferLock)

responseViewBilling :: ViewBillingResponse -> TestTree
responseViewBilling =
  res
    "ViewBillingResponse"
    "fixture/ViewBillingResponse.proto"
    defaultService
    (Proxy :: Proxy ViewBilling)

responseDeleteTagsForDomain :: DeleteTagsForDomainResponse -> TestTree
responseDeleteTagsForDomain =
  res
    "DeleteTagsForDomainResponse"
    "fixture/DeleteTagsForDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTagsForDomain)

responseUpdateTagsForDomain :: UpdateTagsForDomainResponse -> TestTree
responseUpdateTagsForDomain =
  res
    "UpdateTagsForDomainResponse"
    "fixture/UpdateTagsForDomainResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTagsForDomain)

responseListTagsForDomain :: ListTagsForDomainResponse -> TestTree
responseListTagsForDomain =
  res
    "ListTagsForDomainResponse"
    "fixture/ListTagsForDomainResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForDomain)

responseResendContactReachabilityEmail :: ResendContactReachabilityEmailResponse -> TestTree
responseResendContactReachabilityEmail =
  res
    "ResendContactReachabilityEmailResponse"
    "fixture/ResendContactReachabilityEmailResponse.proto"
    defaultService
    (Proxy :: Proxy ResendContactReachabilityEmail)

responseDisableDomainAutoRenew :: DisableDomainAutoRenewResponse -> TestTree
responseDisableDomainAutoRenew =
  res
    "DisableDomainAutoRenewResponse"
    "fixture/DisableDomainAutoRenewResponse.proto"
    defaultService
    (Proxy :: Proxy DisableDomainAutoRenew)

responseUpdateDomainNameservers :: UpdateDomainNameserversResponse -> TestTree
responseUpdateDomainNameservers =
  res
    "UpdateDomainNameserversResponse"
    "fixture/UpdateDomainNameserversResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomainNameservers)

responseEnableDomainAutoRenew :: EnableDomainAutoRenewResponse -> TestTree
responseEnableDomainAutoRenew =
  res
    "EnableDomainAutoRenewResponse"
    "fixture/EnableDomainAutoRenewResponse.proto"
    defaultService
    (Proxy :: Proxy EnableDomainAutoRenew)

responseGetContactReachabilityStatus :: GetContactReachabilityStatusResponse -> TestTree
responseGetContactReachabilityStatus =
  res
    "GetContactReachabilityStatusResponse"
    "fixture/GetContactReachabilityStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetContactReachabilityStatus)

responseRejectDomainTransferFromAnotherAwsAccount :: RejectDomainTransferFromAnotherAwsAccountResponse -> TestTree
responseRejectDomainTransferFromAnotherAwsAccount =
  res
    "RejectDomainTransferFromAnotherAwsAccountResponse"
    "fixture/RejectDomainTransferFromAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy :: Proxy RejectDomainTransferFromAnotherAwsAccount)

responseAcceptDomainTransferFromAnotherAwsAccount :: AcceptDomainTransferFromAnotherAwsAccountResponse -> TestTree
responseAcceptDomainTransferFromAnotherAwsAccount =
  res
    "AcceptDomainTransferFromAnotherAwsAccountResponse"
    "fixture/AcceptDomainTransferFromAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptDomainTransferFromAnotherAwsAccount)

responseGetOperationDetail :: GetOperationDetailResponse -> TestTree
responseGetOperationDetail =
  res
    "GetOperationDetailResponse"
    "fixture/GetOperationDetailResponse.proto"
    defaultService
    (Proxy :: Proxy GetOperationDetail)

responseGetDomainDetail :: GetDomainDetailResponse -> TestTree
responseGetDomainDetail =
  res
    "GetDomainDetailResponse"
    "fixture/GetDomainDetailResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomainDetail)

responseUpdateDomainContact :: UpdateDomainContactResponse -> TestTree
responseUpdateDomainContact =
  res
    "UpdateDomainContactResponse"
    "fixture/UpdateDomainContactResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomainContact)

responseTransferDomain :: TransferDomainResponse -> TestTree
responseTransferDomain =
  res
    "TransferDomainResponse"
    "fixture/TransferDomainResponse.proto"
    defaultService
    (Proxy :: Proxy TransferDomain)

responseRenewDomain :: RenewDomainResponse -> TestTree
responseRenewDomain =
  res
    "RenewDomainResponse"
    "fixture/RenewDomainResponse.proto"
    defaultService
    (Proxy :: Proxy RenewDomain)

responseRetrieveDomainAuthCode :: RetrieveDomainAuthCodeResponse -> TestTree
responseRetrieveDomainAuthCode =
  res
    "RetrieveDomainAuthCodeResponse"
    "fixture/RetrieveDomainAuthCodeResponse.proto"
    defaultService
    (Proxy :: Proxy RetrieveDomainAuthCode)
