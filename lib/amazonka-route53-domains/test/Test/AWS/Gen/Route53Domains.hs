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
--             mkListOperations
--
--         , requestGetDomainDetail $
--             mkGetDomainDetail
--
--         , requestCheckDomainTransferability $
--             mkCheckDomainTransferability
--
--         , requestUpdateDomainContactPrivacy $
--             mkUpdateDomainContactPrivacy
--
--         , requestGetOperationDetail $
--             mkGetOperationDetail
--
--         , requestRejectDomainTransferFromAnotherAwsAccount $
--             mkRejectDomainTransferFromAnotherAwsAccount
--
--         , requestEnableDomainAutoRenew $
--             mkEnableDomainAutoRenew
--
--         , requestResendContactReachabilityEmail $
--             mkResendContactReachabilityEmail
--
--         , requestDisableDomainAutoRenew $
--             mkDisableDomainAutoRenew
--
--         , requestRenewDomain $
--             mkRenewDomain
--
--         , requestViewBilling $
--             mkViewBilling
--
--         , requestUpdateDomainContact $
--             mkUpdateDomainContact
--
--         , requestEnableDomainTransferLock $
--             mkEnableDomainTransferLock
--
--         , requestRegisterDomain $
--             mkRegisterDomain
--
--         , requestGetDomainSuggestions $
--             mkGetDomainSuggestions
--
--         , requestDisableDomainTransferLock $
--             mkDisableDomainTransferLock
--
--         , requestCheckDomainAvailability $
--             mkCheckDomainAvailability
--
--         , requestTransferDomainToAnotherAwsAccount $
--             mkTransferDomainToAnotherAwsAccount
--
--         , requestAcceptDomainTransferFromAnotherAwsAccount $
--             mkAcceptDomainTransferFromAnotherAwsAccount
--
--         , requestGetContactReachabilityStatus $
--             mkGetContactReachabilityStatus
--
--         , requestListTagsForDomain $
--             mkListTagsForDomain
--
--         , requestUpdateDomainNameservers $
--             mkUpdateDomainNameservers
--
--         , requestDeleteTagsForDomain $
--             mkDeleteTagsForDomain
--
--         , requestUpdateTagsForDomain $
--             mkUpdateTagsForDomain
--
--         , requestRetrieveDomainAuthCode $
--             mkRetrieveDomainAuthCode
--
--         , requestTransferDomain $
--             mkTransferDomain
--
--         , requestListDomains $
--             mkListDomains
--
--         , requestCancelDomainTransferToAnotherAwsAccount $
--             mkCancelDomainTransferToAnotherAwsAccount
--
--           ]

--     , testGroup "response"
--         [ responseListOperations $
--             mkListOperationsResponse
--
--         , responseGetDomainDetail $
--             mkGetDomainDetailResponse
--
--         , responseCheckDomainTransferability $
--             mkCheckDomainTransferabilityResponse
--
--         , responseUpdateDomainContactPrivacy $
--             mkUpdateDomainContactPrivacyResponse
--
--         , responseGetOperationDetail $
--             mkGetOperationDetailResponse
--
--         , responseRejectDomainTransferFromAnotherAwsAccount $
--             mkRejectDomainTransferFromAnotherAwsAccountResponse
--
--         , responseEnableDomainAutoRenew $
--             mkEnableDomainAutoRenewResponse
--
--         , responseResendContactReachabilityEmail $
--             mkResendContactReachabilityEmailResponse
--
--         , responseDisableDomainAutoRenew $
--             mkDisableDomainAutoRenewResponse
--
--         , responseRenewDomain $
--             mkRenewDomainResponse
--
--         , responseViewBilling $
--             mkViewBillingResponse
--
--         , responseUpdateDomainContact $
--             mkUpdateDomainContactResponse
--
--         , responseEnableDomainTransferLock $
--             mkEnableDomainTransferLockResponse
--
--         , responseRegisterDomain $
--             mkRegisterDomainResponse
--
--         , responseGetDomainSuggestions $
--             mkGetDomainSuggestionsResponse
--
--         , responseDisableDomainTransferLock $
--             mkDisableDomainTransferLockResponse
--
--         , responseCheckDomainAvailability $
--             mkCheckDomainAvailabilityResponse
--
--         , responseTransferDomainToAnotherAwsAccount $
--             mkTransferDomainToAnotherAwsAccountResponse
--
--         , responseAcceptDomainTransferFromAnotherAwsAccount $
--             mkAcceptDomainTransferFromAnotherAwsAccountResponse
--
--         , responseGetContactReachabilityStatus $
--             mkGetContactReachabilityStatusResponse
--
--         , responseListTagsForDomain $
--             mkListTagsForDomainResponse
--
--         , responseUpdateDomainNameservers $
--             mkUpdateDomainNameserversResponse
--
--         , responseDeleteTagsForDomain $
--             mkDeleteTagsForDomainResponse
--
--         , responseUpdateTagsForDomain $
--             mkUpdateTagsForDomainResponse
--
--         , responseRetrieveDomainAuthCode $
--             mkRetrieveDomainAuthCodeResponse
--
--         , responseTransferDomain $
--             mkTransferDomainResponse
--
--         , responseListDomains $
--             mkListDomainsResponse
--
--         , responseCancelDomainTransferToAnotherAwsAccount $
--             mkCancelDomainTransferToAnotherAwsAccountResponse
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
    mkServiceConfig
    (Proxy :: Proxy ListOperations)

responseGetDomainDetail :: GetDomainDetailResponse -> TestTree
responseGetDomainDetail =
  res
    "GetDomainDetailResponse"
    "fixture/GetDomainDetailResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDomainDetail)

responseCheckDomainTransferability :: CheckDomainTransferabilityResponse -> TestTree
responseCheckDomainTransferability =
  res
    "CheckDomainTransferabilityResponse"
    "fixture/CheckDomainTransferabilityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CheckDomainTransferability)

responseUpdateDomainContactPrivacy :: UpdateDomainContactPrivacyResponse -> TestTree
responseUpdateDomainContactPrivacy =
  res
    "UpdateDomainContactPrivacyResponse"
    "fixture/UpdateDomainContactPrivacyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDomainContactPrivacy)

responseGetOperationDetail :: GetOperationDetailResponse -> TestTree
responseGetOperationDetail =
  res
    "GetOperationDetailResponse"
    "fixture/GetOperationDetailResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetOperationDetail)

responseRejectDomainTransferFromAnotherAwsAccount :: RejectDomainTransferFromAnotherAwsAccountResponse -> TestTree
responseRejectDomainTransferFromAnotherAwsAccount =
  res
    "RejectDomainTransferFromAnotherAwsAccountResponse"
    "fixture/RejectDomainTransferFromAnotherAwsAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RejectDomainTransferFromAnotherAwsAccount)

responseEnableDomainAutoRenew :: EnableDomainAutoRenewResponse -> TestTree
responseEnableDomainAutoRenew =
  res
    "EnableDomainAutoRenewResponse"
    "fixture/EnableDomainAutoRenewResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableDomainAutoRenew)

responseResendContactReachabilityEmail :: ResendContactReachabilityEmailResponse -> TestTree
responseResendContactReachabilityEmail =
  res
    "ResendContactReachabilityEmailResponse"
    "fixture/ResendContactReachabilityEmailResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResendContactReachabilityEmail)

responseDisableDomainAutoRenew :: DisableDomainAutoRenewResponse -> TestTree
responseDisableDomainAutoRenew =
  res
    "DisableDomainAutoRenewResponse"
    "fixture/DisableDomainAutoRenewResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableDomainAutoRenew)

responseRenewDomain :: RenewDomainResponse -> TestTree
responseRenewDomain =
  res
    "RenewDomainResponse"
    "fixture/RenewDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RenewDomain)

responseViewBilling :: ViewBillingResponse -> TestTree
responseViewBilling =
  res
    "ViewBillingResponse"
    "fixture/ViewBillingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ViewBilling)

responseUpdateDomainContact :: UpdateDomainContactResponse -> TestTree
responseUpdateDomainContact =
  res
    "UpdateDomainContactResponse"
    "fixture/UpdateDomainContactResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDomainContact)

responseEnableDomainTransferLock :: EnableDomainTransferLockResponse -> TestTree
responseEnableDomainTransferLock =
  res
    "EnableDomainTransferLockResponse"
    "fixture/EnableDomainTransferLockResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableDomainTransferLock)

responseRegisterDomain :: RegisterDomainResponse -> TestTree
responseRegisterDomain =
  res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterDomain)

responseGetDomainSuggestions :: GetDomainSuggestionsResponse -> TestTree
responseGetDomainSuggestions =
  res
    "GetDomainSuggestionsResponse"
    "fixture/GetDomainSuggestionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDomainSuggestions)

responseDisableDomainTransferLock :: DisableDomainTransferLockResponse -> TestTree
responseDisableDomainTransferLock =
  res
    "DisableDomainTransferLockResponse"
    "fixture/DisableDomainTransferLockResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableDomainTransferLock)

responseCheckDomainAvailability :: CheckDomainAvailabilityResponse -> TestTree
responseCheckDomainAvailability =
  res
    "CheckDomainAvailabilityResponse"
    "fixture/CheckDomainAvailabilityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CheckDomainAvailability)

responseTransferDomainToAnotherAwsAccount :: TransferDomainToAnotherAwsAccountResponse -> TestTree
responseTransferDomainToAnotherAwsAccount =
  res
    "TransferDomainToAnotherAwsAccountResponse"
    "fixture/TransferDomainToAnotherAwsAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TransferDomainToAnotherAwsAccount)

responseAcceptDomainTransferFromAnotherAwsAccount :: AcceptDomainTransferFromAnotherAwsAccountResponse -> TestTree
responseAcceptDomainTransferFromAnotherAwsAccount =
  res
    "AcceptDomainTransferFromAnotherAwsAccountResponse"
    "fixture/AcceptDomainTransferFromAnotherAwsAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AcceptDomainTransferFromAnotherAwsAccount)

responseGetContactReachabilityStatus :: GetContactReachabilityStatusResponse -> TestTree
responseGetContactReachabilityStatus =
  res
    "GetContactReachabilityStatusResponse"
    "fixture/GetContactReachabilityStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContactReachabilityStatus)

responseListTagsForDomain :: ListTagsForDomainResponse -> TestTree
responseListTagsForDomain =
  res
    "ListTagsForDomainResponse"
    "fixture/ListTagsForDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForDomain)

responseUpdateDomainNameservers :: UpdateDomainNameserversResponse -> TestTree
responseUpdateDomainNameservers =
  res
    "UpdateDomainNameserversResponse"
    "fixture/UpdateDomainNameserversResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDomainNameservers)

responseDeleteTagsForDomain :: DeleteTagsForDomainResponse -> TestTree
responseDeleteTagsForDomain =
  res
    "DeleteTagsForDomainResponse"
    "fixture/DeleteTagsForDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTagsForDomain)

responseUpdateTagsForDomain :: UpdateTagsForDomainResponse -> TestTree
responseUpdateTagsForDomain =
  res
    "UpdateTagsForDomainResponse"
    "fixture/UpdateTagsForDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateTagsForDomain)

responseRetrieveDomainAuthCode :: RetrieveDomainAuthCodeResponse -> TestTree
responseRetrieveDomainAuthCode =
  res
    "RetrieveDomainAuthCodeResponse"
    "fixture/RetrieveDomainAuthCodeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RetrieveDomainAuthCode)

responseTransferDomain :: TransferDomainResponse -> TestTree
responseTransferDomain =
  res
    "TransferDomainResponse"
    "fixture/TransferDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TransferDomain)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDomains)

responseCancelDomainTransferToAnotherAwsAccount :: CancelDomainTransferToAnotherAwsAccountResponse -> TestTree
responseCancelDomainTransferToAnotherAwsAccount =
  res
    "CancelDomainTransferToAnotherAwsAccountResponse"
    "fixture/CancelDomainTransferToAnotherAwsAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelDomainTransferToAnotherAwsAccount)
