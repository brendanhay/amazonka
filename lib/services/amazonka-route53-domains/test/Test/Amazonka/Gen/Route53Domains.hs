{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Route53Domains
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Route53Domains where

import Amazonka.Route53Domains
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Route53Domains.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptDomainTransferFromAnotherAwsAccount $
--             newAcceptDomainTransferFromAnotherAwsAccount
--
--         , requestAssociateDelegationSignerToDomain $
--             newAssociateDelegationSignerToDomain
--
--         , requestCancelDomainTransferToAnotherAwsAccount $
--             newCancelDomainTransferToAnotherAwsAccount
--
--         , requestCheckDomainAvailability $
--             newCheckDomainAvailability
--
--         , requestCheckDomainTransferability $
--             newCheckDomainTransferability
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestDeleteTagsForDomain $
--             newDeleteTagsForDomain
--
--         , requestDisableDomainAutoRenew $
--             newDisableDomainAutoRenew
--
--         , requestDisableDomainTransferLock $
--             newDisableDomainTransferLock
--
--         , requestDisassociateDelegationSignerFromDomain $
--             newDisassociateDelegationSignerFromDomain
--
--         , requestEnableDomainAutoRenew $
--             newEnableDomainAutoRenew
--
--         , requestEnableDomainTransferLock $
--             newEnableDomainTransferLock
--
--         , requestGetContactReachabilityStatus $
--             newGetContactReachabilityStatus
--
--         , requestGetDomainDetail $
--             newGetDomainDetail
--
--         , requestGetDomainSuggestions $
--             newGetDomainSuggestions
--
--         , requestGetOperationDetail $
--             newGetOperationDetail
--
--         , requestListDomains $
--             newListDomains
--
--         , requestListOperations $
--             newListOperations
--
--         , requestListPrices $
--             newListPrices
--
--         , requestListTagsForDomain $
--             newListTagsForDomain
--
--         , requestPushDomain $
--             newPushDomain
--
--         , requestRegisterDomain $
--             newRegisterDomain
--
--         , requestRejectDomainTransferFromAnotherAwsAccount $
--             newRejectDomainTransferFromAnotherAwsAccount
--
--         , requestRenewDomain $
--             newRenewDomain
--
--         , requestResendContactReachabilityEmail $
--             newResendContactReachabilityEmail
--
--         , requestResendOperationAuthorization $
--             newResendOperationAuthorization
--
--         , requestRetrieveDomainAuthCode $
--             newRetrieveDomainAuthCode
--
--         , requestTransferDomain $
--             newTransferDomain
--
--         , requestTransferDomainToAnotherAwsAccount $
--             newTransferDomainToAnotherAwsAccount
--
--         , requestUpdateDomainContact $
--             newUpdateDomainContact
--
--         , requestUpdateDomainContactPrivacy $
--             newUpdateDomainContactPrivacy
--
--         , requestUpdateDomainNameservers $
--             newUpdateDomainNameservers
--
--         , requestUpdateTagsForDomain $
--             newUpdateTagsForDomain
--
--         , requestViewBilling $
--             newViewBilling
--
--           ]

--     , testGroup "response"
--         [ responseAcceptDomainTransferFromAnotherAwsAccount $
--             newAcceptDomainTransferFromAnotherAwsAccountResponse
--
--         , responseAssociateDelegationSignerToDomain $
--             newAssociateDelegationSignerToDomainResponse
--
--         , responseCancelDomainTransferToAnotherAwsAccount $
--             newCancelDomainTransferToAnotherAwsAccountResponse
--
--         , responseCheckDomainAvailability $
--             newCheckDomainAvailabilityResponse
--
--         , responseCheckDomainTransferability $
--             newCheckDomainTransferabilityResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseDeleteTagsForDomain $
--             newDeleteTagsForDomainResponse
--
--         , responseDisableDomainAutoRenew $
--             newDisableDomainAutoRenewResponse
--
--         , responseDisableDomainTransferLock $
--             newDisableDomainTransferLockResponse
--
--         , responseDisassociateDelegationSignerFromDomain $
--             newDisassociateDelegationSignerFromDomainResponse
--
--         , responseEnableDomainAutoRenew $
--             newEnableDomainAutoRenewResponse
--
--         , responseEnableDomainTransferLock $
--             newEnableDomainTransferLockResponse
--
--         , responseGetContactReachabilityStatus $
--             newGetContactReachabilityStatusResponse
--
--         , responseGetDomainDetail $
--             newGetDomainDetailResponse
--
--         , responseGetDomainSuggestions $
--             newGetDomainSuggestionsResponse
--
--         , responseGetOperationDetail $
--             newGetOperationDetailResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseListOperations $
--             newListOperationsResponse
--
--         , responseListPrices $
--             newListPricesResponse
--
--         , responseListTagsForDomain $
--             newListTagsForDomainResponse
--
--         , responsePushDomain $
--             newPushDomainResponse
--
--         , responseRegisterDomain $
--             newRegisterDomainResponse
--
--         , responseRejectDomainTransferFromAnotherAwsAccount $
--             newRejectDomainTransferFromAnotherAwsAccountResponse
--
--         , responseRenewDomain $
--             newRenewDomainResponse
--
--         , responseResendContactReachabilityEmail $
--             newResendContactReachabilityEmailResponse
--
--         , responseResendOperationAuthorization $
--             newResendOperationAuthorizationResponse
--
--         , responseRetrieveDomainAuthCode $
--             newRetrieveDomainAuthCodeResponse
--
--         , responseTransferDomain $
--             newTransferDomainResponse
--
--         , responseTransferDomainToAnotherAwsAccount $
--             newTransferDomainToAnotherAwsAccountResponse
--
--         , responseUpdateDomainContact $
--             newUpdateDomainContactResponse
--
--         , responseUpdateDomainContactPrivacy $
--             newUpdateDomainContactPrivacyResponse
--
--         , responseUpdateDomainNameservers $
--             newUpdateDomainNameserversResponse
--
--         , responseUpdateTagsForDomain $
--             newUpdateTagsForDomainResponse
--
--         , responseViewBilling $
--             newViewBillingResponse
--
--           ]
--     ]

-- Requests

requestAcceptDomainTransferFromAnotherAwsAccount :: AcceptDomainTransferFromAnotherAwsAccount -> TestTree
requestAcceptDomainTransferFromAnotherAwsAccount =
  req
    "AcceptDomainTransferFromAnotherAwsAccount"
    "fixture/AcceptDomainTransferFromAnotherAwsAccount.yaml"

requestAssociateDelegationSignerToDomain :: AssociateDelegationSignerToDomain -> TestTree
requestAssociateDelegationSignerToDomain =
  req
    "AssociateDelegationSignerToDomain"
    "fixture/AssociateDelegationSignerToDomain.yaml"

requestCancelDomainTransferToAnotherAwsAccount :: CancelDomainTransferToAnotherAwsAccount -> TestTree
requestCancelDomainTransferToAnotherAwsAccount =
  req
    "CancelDomainTransferToAnotherAwsAccount"
    "fixture/CancelDomainTransferToAnotherAwsAccount.yaml"

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

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestDeleteTagsForDomain :: DeleteTagsForDomain -> TestTree
requestDeleteTagsForDomain =
  req
    "DeleteTagsForDomain"
    "fixture/DeleteTagsForDomain.yaml"

requestDisableDomainAutoRenew :: DisableDomainAutoRenew -> TestTree
requestDisableDomainAutoRenew =
  req
    "DisableDomainAutoRenew"
    "fixture/DisableDomainAutoRenew.yaml"

requestDisableDomainTransferLock :: DisableDomainTransferLock -> TestTree
requestDisableDomainTransferLock =
  req
    "DisableDomainTransferLock"
    "fixture/DisableDomainTransferLock.yaml"

requestDisassociateDelegationSignerFromDomain :: DisassociateDelegationSignerFromDomain -> TestTree
requestDisassociateDelegationSignerFromDomain =
  req
    "DisassociateDelegationSignerFromDomain"
    "fixture/DisassociateDelegationSignerFromDomain.yaml"

requestEnableDomainAutoRenew :: EnableDomainAutoRenew -> TestTree
requestEnableDomainAutoRenew =
  req
    "EnableDomainAutoRenew"
    "fixture/EnableDomainAutoRenew.yaml"

requestEnableDomainTransferLock :: EnableDomainTransferLock -> TestTree
requestEnableDomainTransferLock =
  req
    "EnableDomainTransferLock"
    "fixture/EnableDomainTransferLock.yaml"

requestGetContactReachabilityStatus :: GetContactReachabilityStatus -> TestTree
requestGetContactReachabilityStatus =
  req
    "GetContactReachabilityStatus"
    "fixture/GetContactReachabilityStatus.yaml"

requestGetDomainDetail :: GetDomainDetail -> TestTree
requestGetDomainDetail =
  req
    "GetDomainDetail"
    "fixture/GetDomainDetail.yaml"

requestGetDomainSuggestions :: GetDomainSuggestions -> TestTree
requestGetDomainSuggestions =
  req
    "GetDomainSuggestions"
    "fixture/GetDomainSuggestions.yaml"

requestGetOperationDetail :: GetOperationDetail -> TestTree
requestGetOperationDetail =
  req
    "GetOperationDetail"
    "fixture/GetOperationDetail.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestListOperations :: ListOperations -> TestTree
requestListOperations =
  req
    "ListOperations"
    "fixture/ListOperations.yaml"

requestListPrices :: ListPrices -> TestTree
requestListPrices =
  req
    "ListPrices"
    "fixture/ListPrices.yaml"

requestListTagsForDomain :: ListTagsForDomain -> TestTree
requestListTagsForDomain =
  req
    "ListTagsForDomain"
    "fixture/ListTagsForDomain.yaml"

requestPushDomain :: PushDomain -> TestTree
requestPushDomain =
  req
    "PushDomain"
    "fixture/PushDomain.yaml"

requestRegisterDomain :: RegisterDomain -> TestTree
requestRegisterDomain =
  req
    "RegisterDomain"
    "fixture/RegisterDomain.yaml"

requestRejectDomainTransferFromAnotherAwsAccount :: RejectDomainTransferFromAnotherAwsAccount -> TestTree
requestRejectDomainTransferFromAnotherAwsAccount =
  req
    "RejectDomainTransferFromAnotherAwsAccount"
    "fixture/RejectDomainTransferFromAnotherAwsAccount.yaml"

requestRenewDomain :: RenewDomain -> TestTree
requestRenewDomain =
  req
    "RenewDomain"
    "fixture/RenewDomain.yaml"

requestResendContactReachabilityEmail :: ResendContactReachabilityEmail -> TestTree
requestResendContactReachabilityEmail =
  req
    "ResendContactReachabilityEmail"
    "fixture/ResendContactReachabilityEmail.yaml"

requestResendOperationAuthorization :: ResendOperationAuthorization -> TestTree
requestResendOperationAuthorization =
  req
    "ResendOperationAuthorization"
    "fixture/ResendOperationAuthorization.yaml"

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

requestTransferDomainToAnotherAwsAccount :: TransferDomainToAnotherAwsAccount -> TestTree
requestTransferDomainToAnotherAwsAccount =
  req
    "TransferDomainToAnotherAwsAccount"
    "fixture/TransferDomainToAnotherAwsAccount.yaml"

requestUpdateDomainContact :: UpdateDomainContact -> TestTree
requestUpdateDomainContact =
  req
    "UpdateDomainContact"
    "fixture/UpdateDomainContact.yaml"

requestUpdateDomainContactPrivacy :: UpdateDomainContactPrivacy -> TestTree
requestUpdateDomainContactPrivacy =
  req
    "UpdateDomainContactPrivacy"
    "fixture/UpdateDomainContactPrivacy.yaml"

requestUpdateDomainNameservers :: UpdateDomainNameservers -> TestTree
requestUpdateDomainNameservers =
  req
    "UpdateDomainNameservers"
    "fixture/UpdateDomainNameservers.yaml"

requestUpdateTagsForDomain :: UpdateTagsForDomain -> TestTree
requestUpdateTagsForDomain =
  req
    "UpdateTagsForDomain"
    "fixture/UpdateTagsForDomain.yaml"

requestViewBilling :: ViewBilling -> TestTree
requestViewBilling =
  req
    "ViewBilling"
    "fixture/ViewBilling.yaml"

-- Responses

responseAcceptDomainTransferFromAnotherAwsAccount :: AcceptDomainTransferFromAnotherAwsAccountResponse -> TestTree
responseAcceptDomainTransferFromAnotherAwsAccount =
  res
    "AcceptDomainTransferFromAnotherAwsAccountResponse"
    "fixture/AcceptDomainTransferFromAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptDomainTransferFromAnotherAwsAccount)

responseAssociateDelegationSignerToDomain :: AssociateDelegationSignerToDomainResponse -> TestTree
responseAssociateDelegationSignerToDomain =
  res
    "AssociateDelegationSignerToDomainResponse"
    "fixture/AssociateDelegationSignerToDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDelegationSignerToDomain)

responseCancelDomainTransferToAnotherAwsAccount :: CancelDomainTransferToAnotherAwsAccountResponse -> TestTree
responseCancelDomainTransferToAnotherAwsAccount =
  res
    "CancelDomainTransferToAnotherAwsAccountResponse"
    "fixture/CancelDomainTransferToAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelDomainTransferToAnotherAwsAccount)

responseCheckDomainAvailability :: CheckDomainAvailabilityResponse -> TestTree
responseCheckDomainAvailability =
  res
    "CheckDomainAvailabilityResponse"
    "fixture/CheckDomainAvailabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckDomainAvailability)

responseCheckDomainTransferability :: CheckDomainTransferabilityResponse -> TestTree
responseCheckDomainTransferability =
  res
    "CheckDomainTransferabilityResponse"
    "fixture/CheckDomainTransferabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckDomainTransferability)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseDeleteTagsForDomain :: DeleteTagsForDomainResponse -> TestTree
responseDeleteTagsForDomain =
  res
    "DeleteTagsForDomainResponse"
    "fixture/DeleteTagsForDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTagsForDomain)

responseDisableDomainAutoRenew :: DisableDomainAutoRenewResponse -> TestTree
responseDisableDomainAutoRenew =
  res
    "DisableDomainAutoRenewResponse"
    "fixture/DisableDomainAutoRenewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableDomainAutoRenew)

responseDisableDomainTransferLock :: DisableDomainTransferLockResponse -> TestTree
responseDisableDomainTransferLock =
  res
    "DisableDomainTransferLockResponse"
    "fixture/DisableDomainTransferLockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableDomainTransferLock)

responseDisassociateDelegationSignerFromDomain :: DisassociateDelegationSignerFromDomainResponse -> TestTree
responseDisassociateDelegationSignerFromDomain =
  res
    "DisassociateDelegationSignerFromDomainResponse"
    "fixture/DisassociateDelegationSignerFromDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDelegationSignerFromDomain)

responseEnableDomainAutoRenew :: EnableDomainAutoRenewResponse -> TestTree
responseEnableDomainAutoRenew =
  res
    "EnableDomainAutoRenewResponse"
    "fixture/EnableDomainAutoRenewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableDomainAutoRenew)

responseEnableDomainTransferLock :: EnableDomainTransferLockResponse -> TestTree
responseEnableDomainTransferLock =
  res
    "EnableDomainTransferLockResponse"
    "fixture/EnableDomainTransferLockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableDomainTransferLock)

responseGetContactReachabilityStatus :: GetContactReachabilityStatusResponse -> TestTree
responseGetContactReachabilityStatus =
  res
    "GetContactReachabilityStatusResponse"
    "fixture/GetContactReachabilityStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContactReachabilityStatus)

responseGetDomainDetail :: GetDomainDetailResponse -> TestTree
responseGetDomainDetail =
  res
    "GetDomainDetailResponse"
    "fixture/GetDomainDetailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainDetail)

responseGetDomainSuggestions :: GetDomainSuggestionsResponse -> TestTree
responseGetDomainSuggestions =
  res
    "GetDomainSuggestionsResponse"
    "fixture/GetDomainSuggestionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainSuggestions)

responseGetOperationDetail :: GetOperationDetailResponse -> TestTree
responseGetOperationDetail =
  res
    "GetOperationDetailResponse"
    "fixture/GetOperationDetailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOperationDetail)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseListOperations :: ListOperationsResponse -> TestTree
responseListOperations =
  res
    "ListOperationsResponse"
    "fixture/ListOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOperations)

responseListPrices :: ListPricesResponse -> TestTree
responseListPrices =
  res
    "ListPricesResponse"
    "fixture/ListPricesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPrices)

responseListTagsForDomain :: ListTagsForDomainResponse -> TestTree
responseListTagsForDomain =
  res
    "ListTagsForDomainResponse"
    "fixture/ListTagsForDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForDomain)

responsePushDomain :: PushDomainResponse -> TestTree
responsePushDomain =
  res
    "PushDomainResponse"
    "fixture/PushDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PushDomain)

responseRegisterDomain :: RegisterDomainResponse -> TestTree
responseRegisterDomain =
  res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDomain)

responseRejectDomainTransferFromAnotherAwsAccount :: RejectDomainTransferFromAnotherAwsAccountResponse -> TestTree
responseRejectDomainTransferFromAnotherAwsAccount =
  res
    "RejectDomainTransferFromAnotherAwsAccountResponse"
    "fixture/RejectDomainTransferFromAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectDomainTransferFromAnotherAwsAccount)

responseRenewDomain :: RenewDomainResponse -> TestTree
responseRenewDomain =
  res
    "RenewDomainResponse"
    "fixture/RenewDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RenewDomain)

responseResendContactReachabilityEmail :: ResendContactReachabilityEmailResponse -> TestTree
responseResendContactReachabilityEmail =
  res
    "ResendContactReachabilityEmailResponse"
    "fixture/ResendContactReachabilityEmailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResendContactReachabilityEmail)

responseResendOperationAuthorization :: ResendOperationAuthorizationResponse -> TestTree
responseResendOperationAuthorization =
  res
    "ResendOperationAuthorizationResponse"
    "fixture/ResendOperationAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResendOperationAuthorization)

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

responseTransferDomainToAnotherAwsAccount :: TransferDomainToAnotherAwsAccountResponse -> TestTree
responseTransferDomainToAnotherAwsAccount =
  res
    "TransferDomainToAnotherAwsAccountResponse"
    "fixture/TransferDomainToAnotherAwsAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TransferDomainToAnotherAwsAccount)

responseUpdateDomainContact :: UpdateDomainContactResponse -> TestTree
responseUpdateDomainContact =
  res
    "UpdateDomainContactResponse"
    "fixture/UpdateDomainContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainContact)

responseUpdateDomainContactPrivacy :: UpdateDomainContactPrivacyResponse -> TestTree
responseUpdateDomainContactPrivacy =
  res
    "UpdateDomainContactPrivacyResponse"
    "fixture/UpdateDomainContactPrivacyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainContactPrivacy)

responseUpdateDomainNameservers :: UpdateDomainNameserversResponse -> TestTree
responseUpdateDomainNameservers =
  res
    "UpdateDomainNameserversResponse"
    "fixture/UpdateDomainNameserversResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainNameservers)

responseUpdateTagsForDomain :: UpdateTagsForDomainResponse -> TestTree
responseUpdateTagsForDomain =
  res
    "UpdateTagsForDomainResponse"
    "fixture/UpdateTagsForDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTagsForDomain)

responseViewBilling :: ViewBillingResponse -> TestTree
responseViewBilling =
  res
    "ViewBillingResponse"
    "fixture/ViewBillingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ViewBilling)
