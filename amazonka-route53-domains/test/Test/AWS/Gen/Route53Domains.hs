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
--         [ requestCheckDomainTransferability $
--             newCheckDomainTransferability
--
--         , requestCheckDomainAvailability $
--             newCheckDomainAvailability
--
--         , requestTransferDomainToAnotherAwsAccount $
--             newTransferDomainToAnotherAwsAccount
--
--         , requestUpdateDomainContactPrivacy $
--             newUpdateDomainContactPrivacy
--
--         , requestListOperations $
--             newListOperations
--
--         , requestDisableDomainTransferLock $
--             newDisableDomainTransferLock
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
--         , requestCancelDomainTransferToAnotherAwsAccount $
--             newCancelDomainTransferToAnotherAwsAccount
--
--         , requestListDomains $
--             newListDomains
--
--         , requestViewBilling $
--             newViewBilling
--
--         , requestUpdateTagsForDomain $
--             newUpdateTagsForDomain
--
--         , requestDeleteTagsForDomain $
--             newDeleteTagsForDomain
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
--         , requestGetOperationDetail $
--             newGetOperationDetail
--
--         , requestAcceptDomainTransferFromAnotherAwsAccount $
--             newAcceptDomainTransferFromAnotherAwsAccount
--
--         , requestGetDomainDetail $
--             newGetDomainDetail
--
--         , requestUpdateDomainContact $
--             newUpdateDomainContact
--
--         , requestRenewDomain $
--             newRenewDomain
--
--         , requestTransferDomain $
--             newTransferDomain
--
--         , requestRetrieveDomainAuthCode $
--             newRetrieveDomainAuthCode
--
--           ]

--     , testGroup "response"
--         [ responseCheckDomainTransferability $
--             newCheckDomainTransferabilityResponse
--
--         , responseCheckDomainAvailability $
--             newCheckDomainAvailabilityResponse
--
--         , responseTransferDomainToAnotherAwsAccount $
--             newTransferDomainToAnotherAwsAccountResponse
--
--         , responseUpdateDomainContactPrivacy $
--             newUpdateDomainContactPrivacyResponse
--
--         , responseListOperations $
--             newListOperationsResponse
--
--         , responseDisableDomainTransferLock $
--             newDisableDomainTransferLockResponse
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
--         , responseCancelDomainTransferToAnotherAwsAccount $
--             newCancelDomainTransferToAnotherAwsAccountResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseViewBilling $
--             newViewBillingResponse
--
--         , responseUpdateTagsForDomain $
--             newUpdateTagsForDomainResponse
--
--         , responseDeleteTagsForDomain $
--             newDeleteTagsForDomainResponse
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
--         , responseGetOperationDetail $
--             newGetOperationDetailResponse
--
--         , responseAcceptDomainTransferFromAnotherAwsAccount $
--             newAcceptDomainTransferFromAnotherAwsAccountResponse
--
--         , responseGetDomainDetail $
--             newGetDomainDetailResponse
--
--         , responseUpdateDomainContact $
--             newUpdateDomainContactResponse
--
--         , responseRenewDomain $
--             newRenewDomainResponse
--
--         , responseTransferDomain $
--             newTransferDomainResponse
--
--         , responseRetrieveDomainAuthCode $
--             newRetrieveDomainAuthCodeResponse
--
--           ]
--     ]

-- Requests

requestCheckDomainTransferability :: CheckDomainTransferability -> TestTree
requestCheckDomainTransferability =
  req
    "CheckDomainTransferability"
    "fixture/CheckDomainTransferability.yaml"

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

requestUpdateDomainContactPrivacy :: UpdateDomainContactPrivacy -> TestTree
requestUpdateDomainContactPrivacy =
  req
    "UpdateDomainContactPrivacy"
    "fixture/UpdateDomainContactPrivacy.yaml"

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

requestCancelDomainTransferToAnotherAwsAccount :: CancelDomainTransferToAnotherAwsAccount -> TestTree
requestCancelDomainTransferToAnotherAwsAccount =
  req
    "CancelDomainTransferToAnotherAwsAccount"
    "fixture/CancelDomainTransferToAnotherAwsAccount.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestViewBilling :: ViewBilling -> TestTree
requestViewBilling =
  req
    "ViewBilling"
    "fixture/ViewBilling.yaml"

requestUpdateTagsForDomain :: UpdateTagsForDomain -> TestTree
requestUpdateTagsForDomain =
  req
    "UpdateTagsForDomain"
    "fixture/UpdateTagsForDomain.yaml"

requestDeleteTagsForDomain :: DeleteTagsForDomain -> TestTree
requestDeleteTagsForDomain =
  req
    "DeleteTagsForDomain"
    "fixture/DeleteTagsForDomain.yaml"

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

requestGetOperationDetail :: GetOperationDetail -> TestTree
requestGetOperationDetail =
  req
    "GetOperationDetail"
    "fixture/GetOperationDetail.yaml"

requestAcceptDomainTransferFromAnotherAwsAccount :: AcceptDomainTransferFromAnotherAwsAccount -> TestTree
requestAcceptDomainTransferFromAnotherAwsAccount =
  req
    "AcceptDomainTransferFromAnotherAwsAccount"
    "fixture/AcceptDomainTransferFromAnotherAwsAccount.yaml"

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

requestRenewDomain :: RenewDomain -> TestTree
requestRenewDomain =
  req
    "RenewDomain"
    "fixture/RenewDomain.yaml"

requestTransferDomain :: TransferDomain -> TestTree
requestTransferDomain =
  req
    "TransferDomain"
    "fixture/TransferDomain.yaml"

requestRetrieveDomainAuthCode :: RetrieveDomainAuthCode -> TestTree
requestRetrieveDomainAuthCode =
  req
    "RetrieveDomainAuthCode"
    "fixture/RetrieveDomainAuthCode.yaml"

-- Responses

responseCheckDomainTransferability :: CheckDomainTransferabilityResponse -> TestTree
responseCheckDomainTransferability =
  res
    "CheckDomainTransferabilityResponse"
    "fixture/CheckDomainTransferabilityResponse.proto"
    defaultService
    (Proxy :: Proxy CheckDomainTransferability)

responseCheckDomainAvailability :: CheckDomainAvailabilityResponse -> TestTree
responseCheckDomainAvailability =
  res
    "CheckDomainAvailabilityResponse"
    "fixture/CheckDomainAvailabilityResponse.proto"
    defaultService
    (Proxy :: Proxy CheckDomainAvailability)

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

responseEnableDomainTransferLock :: EnableDomainTransferLockResponse -> TestTree
responseEnableDomainTransferLock =
  res
    "EnableDomainTransferLockResponse"
    "fixture/EnableDomainTransferLockResponse.proto"
    defaultService
    (Proxy :: Proxy EnableDomainTransferLock)

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

responseCancelDomainTransferToAnotherAwsAccount :: CancelDomainTransferToAnotherAwsAccountResponse -> TestTree
responseCancelDomainTransferToAnotherAwsAccount =
  res
    "CancelDomainTransferToAnotherAwsAccountResponse"
    "fixture/CancelDomainTransferToAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy :: Proxy CancelDomainTransferToAnotherAwsAccount)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomains)

responseViewBilling :: ViewBillingResponse -> TestTree
responseViewBilling =
  res
    "ViewBillingResponse"
    "fixture/ViewBillingResponse.proto"
    defaultService
    (Proxy :: Proxy ViewBilling)

responseUpdateTagsForDomain :: UpdateTagsForDomainResponse -> TestTree
responseUpdateTagsForDomain =
  res
    "UpdateTagsForDomainResponse"
    "fixture/UpdateTagsForDomainResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTagsForDomain)

responseDeleteTagsForDomain :: DeleteTagsForDomainResponse -> TestTree
responseDeleteTagsForDomain =
  res
    "DeleteTagsForDomainResponse"
    "fixture/DeleteTagsForDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTagsForDomain)

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

responseGetOperationDetail :: GetOperationDetailResponse -> TestTree
responseGetOperationDetail =
  res
    "GetOperationDetailResponse"
    "fixture/GetOperationDetailResponse.proto"
    defaultService
    (Proxy :: Proxy GetOperationDetail)

responseAcceptDomainTransferFromAnotherAwsAccount :: AcceptDomainTransferFromAnotherAwsAccountResponse -> TestTree
responseAcceptDomainTransferFromAnotherAwsAccount =
  res
    "AcceptDomainTransferFromAnotherAwsAccountResponse"
    "fixture/AcceptDomainTransferFromAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptDomainTransferFromAnotherAwsAccount)

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

responseRenewDomain :: RenewDomainResponse -> TestTree
responseRenewDomain =
  res
    "RenewDomainResponse"
    "fixture/RenewDomainResponse.proto"
    defaultService
    (Proxy :: Proxy RenewDomain)

responseTransferDomain :: TransferDomainResponse -> TestTree
responseTransferDomain =
  res
    "TransferDomainResponse"
    "fixture/TransferDomainResponse.proto"
    defaultService
    (Proxy :: Proxy TransferDomain)

responseRetrieveDomainAuthCode :: RetrieveDomainAuthCodeResponse -> TestTree
responseRetrieveDomainAuthCode =
  res
    "RetrieveDomainAuthCodeResponse"
    "fixture/RetrieveDomainAuthCodeResponse.proto"
    defaultService
    (Proxy :: Proxy RetrieveDomainAuthCode)
