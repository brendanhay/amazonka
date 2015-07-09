{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.Route53Domains
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.Route53Domains where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.Route53Domains

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testListOperations $
--             listOperations
--
--         , testGetDomainDetail $
--             getDomainDetail
--
--         , testUpdateDomainContactPrivacy $
--             updateDomainContactPrivacy
--
--         , testGetOperationDetail $
--             getOperationDetail
--
--         , testEnableDomainAutoRenew $
--             enableDomainAutoRenew
--
--         , testDisableDomainAutoRenew $
--             disableDomainAutoRenew
--
--         , testUpdateDomainContact $
--             updateDomainContact
--
--         , testEnableDomainTransferLock $
--             enableDomainTransferLock
--
--         , testRegisterDomain $
--             registerDomain
--
--         , testDisableDomainTransferLock $
--             disableDomainTransferLock
--
--         , testCheckDomainAvailability $
--             checkDomainAvailability
--
--         , testListTagsForDomain $
--             listTagsForDomain
--
--         , testUpdateDomainNameservers $
--             updateDomainNameservers
--
--         , testRetrieveDomainAuthCode $
--             retrieveDomainAuthCode
--
--         , testTransferDomain $
--             transferDomain
--
--         , testDeleteTagsForDomain $
--             deleteTagsForDomain
--
--         , testUpdateTagsForDomain $
--             updateTagsForDomain
--
--         , testListDomains $
--             listDomains
--
--           ]

--     , testGroup "response"
--         [ testListOperationsResponse $
--             listOperationsResponse
--
--         , testGetDomainDetailResponse $
--             getDomainDetailResponse
--
--         , testUpdateDomainContactPrivacyResponse $
--             updateDomainContactPrivacyResponse
--
--         , testGetOperationDetailResponse $
--             getOperationDetailResponse
--
--         , testEnableDomainAutoRenewResponse $
--             enableDomainAutoRenewResponse
--
--         , testDisableDomainAutoRenewResponse $
--             disableDomainAutoRenewResponse
--
--         , testUpdateDomainContactResponse $
--             updateDomainContactResponse
--
--         , testEnableDomainTransferLockResponse $
--             enableDomainTransferLockResponse
--
--         , testRegisterDomainResponse $
--             registerDomainResponse
--
--         , testDisableDomainTransferLockResponse $
--             disableDomainTransferLockResponse
--
--         , testCheckDomainAvailabilityResponse $
--             checkDomainAvailabilityResponse
--
--         , testListTagsForDomainResponse $
--             listTagsForDomainResponse
--
--         , testUpdateDomainNameserversResponse $
--             updateDomainNameserversResponse
--
--         , testRetrieveDomainAuthCodeResponse $
--             retrieveDomainAuthCodeResponse
--
--         , testTransferDomainResponse $
--             transferDomainResponse
--
--         , testDeleteTagsForDomainResponse $
--             deleteTagsForDomainResponse
--
--         , testUpdateTagsForDomainResponse $
--             updateTagsForDomainResponse
--
--         , testListDomainsResponse $
--             listDomainsResponse
--
--           ]
--     ]

-- Requests

testListOperations :: ListOperations -> TestTree
testListOperations = undefined

testGetDomainDetail :: GetDomainDetail -> TestTree
testGetDomainDetail = undefined

testUpdateDomainContactPrivacy :: UpdateDomainContactPrivacy -> TestTree
testUpdateDomainContactPrivacy = undefined

testGetOperationDetail :: GetOperationDetail -> TestTree
testGetOperationDetail = undefined

testEnableDomainAutoRenew :: EnableDomainAutoRenew -> TestTree
testEnableDomainAutoRenew = undefined

testDisableDomainAutoRenew :: DisableDomainAutoRenew -> TestTree
testDisableDomainAutoRenew = undefined

testUpdateDomainContact :: UpdateDomainContact -> TestTree
testUpdateDomainContact = undefined

testEnableDomainTransferLock :: EnableDomainTransferLock -> TestTree
testEnableDomainTransferLock = undefined

testRegisterDomain :: RegisterDomain -> TestTree
testRegisterDomain = undefined

testDisableDomainTransferLock :: DisableDomainTransferLock -> TestTree
testDisableDomainTransferLock = undefined

testCheckDomainAvailability :: CheckDomainAvailability -> TestTree
testCheckDomainAvailability = undefined

testListTagsForDomain :: ListTagsForDomain -> TestTree
testListTagsForDomain = undefined

testUpdateDomainNameservers :: UpdateDomainNameservers -> TestTree
testUpdateDomainNameservers = undefined

testRetrieveDomainAuthCode :: RetrieveDomainAuthCode -> TestTree
testRetrieveDomainAuthCode = undefined

testTransferDomain :: TransferDomain -> TestTree
testTransferDomain = undefined

testDeleteTagsForDomain :: DeleteTagsForDomain -> TestTree
testDeleteTagsForDomain = undefined

testUpdateTagsForDomain :: UpdateTagsForDomain -> TestTree
testUpdateTagsForDomain = undefined

testListDomains :: ListDomains -> TestTree
testListDomains = undefined

-- Responses

testListOperationsResponse :: ListOperationsResponse -> TestTree
testListOperationsResponse = resp
    "ListOperationsResponse"
    "fixture/ListOperationsResponse"
    (Proxy :: Proxy ListOperations)

testGetDomainDetailResponse :: GetDomainDetailResponse -> TestTree
testGetDomainDetailResponse = resp
    "GetDomainDetailResponse"
    "fixture/GetDomainDetailResponse"
    (Proxy :: Proxy GetDomainDetail)

testUpdateDomainContactPrivacyResponse :: UpdateDomainContactPrivacyResponse -> TestTree
testUpdateDomainContactPrivacyResponse = resp
    "UpdateDomainContactPrivacyResponse"
    "fixture/UpdateDomainContactPrivacyResponse"
    (Proxy :: Proxy UpdateDomainContactPrivacy)

testGetOperationDetailResponse :: GetOperationDetailResponse -> TestTree
testGetOperationDetailResponse = resp
    "GetOperationDetailResponse"
    "fixture/GetOperationDetailResponse"
    (Proxy :: Proxy GetOperationDetail)

testEnableDomainAutoRenewResponse :: EnableDomainAutoRenewResponse -> TestTree
testEnableDomainAutoRenewResponse = resp
    "EnableDomainAutoRenewResponse"
    "fixture/EnableDomainAutoRenewResponse"
    (Proxy :: Proxy EnableDomainAutoRenew)

testDisableDomainAutoRenewResponse :: DisableDomainAutoRenewResponse -> TestTree
testDisableDomainAutoRenewResponse = resp
    "DisableDomainAutoRenewResponse"
    "fixture/DisableDomainAutoRenewResponse"
    (Proxy :: Proxy DisableDomainAutoRenew)

testUpdateDomainContactResponse :: UpdateDomainContactResponse -> TestTree
testUpdateDomainContactResponse = resp
    "UpdateDomainContactResponse"
    "fixture/UpdateDomainContactResponse"
    (Proxy :: Proxy UpdateDomainContact)

testEnableDomainTransferLockResponse :: EnableDomainTransferLockResponse -> TestTree
testEnableDomainTransferLockResponse = resp
    "EnableDomainTransferLockResponse"
    "fixture/EnableDomainTransferLockResponse"
    (Proxy :: Proxy EnableDomainTransferLock)

testRegisterDomainResponse :: RegisterDomainResponse -> TestTree
testRegisterDomainResponse = resp
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse"
    (Proxy :: Proxy RegisterDomain)

testDisableDomainTransferLockResponse :: DisableDomainTransferLockResponse -> TestTree
testDisableDomainTransferLockResponse = resp
    "DisableDomainTransferLockResponse"
    "fixture/DisableDomainTransferLockResponse"
    (Proxy :: Proxy DisableDomainTransferLock)

testCheckDomainAvailabilityResponse :: CheckDomainAvailabilityResponse -> TestTree
testCheckDomainAvailabilityResponse = resp
    "CheckDomainAvailabilityResponse"
    "fixture/CheckDomainAvailabilityResponse"
    (Proxy :: Proxy CheckDomainAvailability)

testListTagsForDomainResponse :: ListTagsForDomainResponse -> TestTree
testListTagsForDomainResponse = resp
    "ListTagsForDomainResponse"
    "fixture/ListTagsForDomainResponse"
    (Proxy :: Proxy ListTagsForDomain)

testUpdateDomainNameserversResponse :: UpdateDomainNameserversResponse -> TestTree
testUpdateDomainNameserversResponse = resp
    "UpdateDomainNameserversResponse"
    "fixture/UpdateDomainNameserversResponse"
    (Proxy :: Proxy UpdateDomainNameservers)

testRetrieveDomainAuthCodeResponse :: RetrieveDomainAuthCodeResponse -> TestTree
testRetrieveDomainAuthCodeResponse = resp
    "RetrieveDomainAuthCodeResponse"
    "fixture/RetrieveDomainAuthCodeResponse"
    (Proxy :: Proxy RetrieveDomainAuthCode)

testTransferDomainResponse :: TransferDomainResponse -> TestTree
testTransferDomainResponse = resp
    "TransferDomainResponse"
    "fixture/TransferDomainResponse"
    (Proxy :: Proxy TransferDomain)

testDeleteTagsForDomainResponse :: DeleteTagsForDomainResponse -> TestTree
testDeleteTagsForDomainResponse = resp
    "DeleteTagsForDomainResponse"
    "fixture/DeleteTagsForDomainResponse"
    (Proxy :: Proxy DeleteTagsForDomain)

testUpdateTagsForDomainResponse :: UpdateTagsForDomainResponse -> TestTree
testUpdateTagsForDomainResponse = resp
    "UpdateTagsForDomainResponse"
    "fixture/UpdateTagsForDomainResponse"
    (Proxy :: Proxy UpdateTagsForDomain)

testListDomainsResponse :: ListDomainsResponse -> TestTree
testListDomainsResponse = resp
    "ListDomainsResponse"
    "fixture/ListDomainsResponse"
    (Proxy :: Proxy ListDomains)

instance Out CheckDomainAvailability
instance Out CheckDomainAvailabilityResponse
instance Out ContactDetail
instance Out ContactType
instance Out CountryCode
instance Out DeleteTagsForDomain
instance Out DeleteTagsForDomainResponse
instance Out DisableDomainAutoRenew
instance Out DisableDomainAutoRenewResponse
instance Out DisableDomainTransferLock
instance Out DisableDomainTransferLockResponse
instance Out DomainAvailability
instance Out DomainSummary
instance Out EnableDomainAutoRenew
instance Out EnableDomainAutoRenewResponse
instance Out EnableDomainTransferLock
instance Out EnableDomainTransferLockResponse
instance Out ExtraParam
instance Out ExtraParamName
instance Out GetDomainDetail
instance Out GetDomainDetailResponse
instance Out GetOperationDetail
instance Out GetOperationDetailResponse
instance Out ListDomains
instance Out ListDomainsResponse
instance Out ListOperations
instance Out ListOperationsResponse
instance Out ListTagsForDomain
instance Out ListTagsForDomainResponse
instance Out Nameserver
instance Out OperationStatus
instance Out OperationSummary
instance Out OperationType
instance Out RegisterDomain
instance Out RegisterDomainResponse
instance Out RetrieveDomainAuthCode
instance Out RetrieveDomainAuthCodeResponse
instance Out Tag
instance Out TransferDomain
instance Out TransferDomainResponse
instance Out UpdateDomainContact
instance Out UpdateDomainContactPrivacy
instance Out UpdateDomainContactPrivacyResponse
instance Out UpdateDomainContactResponse
instance Out UpdateDomainNameservers
instance Out UpdateDomainNameserversResponse
instance Out UpdateTagsForDomain
instance Out UpdateTagsForDomainResponse
