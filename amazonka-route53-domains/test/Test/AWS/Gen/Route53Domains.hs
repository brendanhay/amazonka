{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53Domains
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Route53Domains where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Route53Domains
import Test.AWS.Route53Domains.Internal

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
testListOperations = req
    "ListOperations"
    "fixture/ListOperations"

testGetDomainDetail :: GetDomainDetail -> TestTree
testGetDomainDetail = req
    "GetDomainDetail"
    "fixture/GetDomainDetail"

testUpdateDomainContactPrivacy :: UpdateDomainContactPrivacy -> TestTree
testUpdateDomainContactPrivacy = req
    "UpdateDomainContactPrivacy"
    "fixture/UpdateDomainContactPrivacy"

testGetOperationDetail :: GetOperationDetail -> TestTree
testGetOperationDetail = req
    "GetOperationDetail"
    "fixture/GetOperationDetail"

testEnableDomainAutoRenew :: EnableDomainAutoRenew -> TestTree
testEnableDomainAutoRenew = req
    "EnableDomainAutoRenew"
    "fixture/EnableDomainAutoRenew"

testDisableDomainAutoRenew :: DisableDomainAutoRenew -> TestTree
testDisableDomainAutoRenew = req
    "DisableDomainAutoRenew"
    "fixture/DisableDomainAutoRenew"

testUpdateDomainContact :: UpdateDomainContact -> TestTree
testUpdateDomainContact = req
    "UpdateDomainContact"
    "fixture/UpdateDomainContact"

testEnableDomainTransferLock :: EnableDomainTransferLock -> TestTree
testEnableDomainTransferLock = req
    "EnableDomainTransferLock"
    "fixture/EnableDomainTransferLock"

testRegisterDomain :: RegisterDomain -> TestTree
testRegisterDomain = req
    "RegisterDomain"
    "fixture/RegisterDomain"

testDisableDomainTransferLock :: DisableDomainTransferLock -> TestTree
testDisableDomainTransferLock = req
    "DisableDomainTransferLock"
    "fixture/DisableDomainTransferLock"

testCheckDomainAvailability :: CheckDomainAvailability -> TestTree
testCheckDomainAvailability = req
    "CheckDomainAvailability"
    "fixture/CheckDomainAvailability"

testListTagsForDomain :: ListTagsForDomain -> TestTree
testListTagsForDomain = req
    "ListTagsForDomain"
    "fixture/ListTagsForDomain"

testUpdateDomainNameservers :: UpdateDomainNameservers -> TestTree
testUpdateDomainNameservers = req
    "UpdateDomainNameservers"
    "fixture/UpdateDomainNameservers"

testRetrieveDomainAuthCode :: RetrieveDomainAuthCode -> TestTree
testRetrieveDomainAuthCode = req
    "RetrieveDomainAuthCode"
    "fixture/RetrieveDomainAuthCode"

testTransferDomain :: TransferDomain -> TestTree
testTransferDomain = req
    "TransferDomain"
    "fixture/TransferDomain"

testDeleteTagsForDomain :: DeleteTagsForDomain -> TestTree
testDeleteTagsForDomain = req
    "DeleteTagsForDomain"
    "fixture/DeleteTagsForDomain"

testUpdateTagsForDomain :: UpdateTagsForDomain -> TestTree
testUpdateTagsForDomain = req
    "UpdateTagsForDomain"
    "fixture/UpdateTagsForDomain"

testListDomains :: ListDomains -> TestTree
testListDomains = req
    "ListDomains"
    "fixture/ListDomains"

-- Responses

testListOperationsResponse :: ListOperationsResponse -> TestTree
testListOperationsResponse = res
    "ListOperationsResponse"
    "fixture/ListOperationsResponse"
    (Proxy :: Proxy ListOperations)

testGetDomainDetailResponse :: GetDomainDetailResponse -> TestTree
testGetDomainDetailResponse = res
    "GetDomainDetailResponse"
    "fixture/GetDomainDetailResponse"
    (Proxy :: Proxy GetDomainDetail)

testUpdateDomainContactPrivacyResponse :: UpdateDomainContactPrivacyResponse -> TestTree
testUpdateDomainContactPrivacyResponse = res
    "UpdateDomainContactPrivacyResponse"
    "fixture/UpdateDomainContactPrivacyResponse"
    (Proxy :: Proxy UpdateDomainContactPrivacy)

testGetOperationDetailResponse :: GetOperationDetailResponse -> TestTree
testGetOperationDetailResponse = res
    "GetOperationDetailResponse"
    "fixture/GetOperationDetailResponse"
    (Proxy :: Proxy GetOperationDetail)

testEnableDomainAutoRenewResponse :: EnableDomainAutoRenewResponse -> TestTree
testEnableDomainAutoRenewResponse = res
    "EnableDomainAutoRenewResponse"
    "fixture/EnableDomainAutoRenewResponse"
    (Proxy :: Proxy EnableDomainAutoRenew)

testDisableDomainAutoRenewResponse :: DisableDomainAutoRenewResponse -> TestTree
testDisableDomainAutoRenewResponse = res
    "DisableDomainAutoRenewResponse"
    "fixture/DisableDomainAutoRenewResponse"
    (Proxy :: Proxy DisableDomainAutoRenew)

testUpdateDomainContactResponse :: UpdateDomainContactResponse -> TestTree
testUpdateDomainContactResponse = res
    "UpdateDomainContactResponse"
    "fixture/UpdateDomainContactResponse"
    (Proxy :: Proxy UpdateDomainContact)

testEnableDomainTransferLockResponse :: EnableDomainTransferLockResponse -> TestTree
testEnableDomainTransferLockResponse = res
    "EnableDomainTransferLockResponse"
    "fixture/EnableDomainTransferLockResponse"
    (Proxy :: Proxy EnableDomainTransferLock)

testRegisterDomainResponse :: RegisterDomainResponse -> TestTree
testRegisterDomainResponse = res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse"
    (Proxy :: Proxy RegisterDomain)

testDisableDomainTransferLockResponse :: DisableDomainTransferLockResponse -> TestTree
testDisableDomainTransferLockResponse = res
    "DisableDomainTransferLockResponse"
    "fixture/DisableDomainTransferLockResponse"
    (Proxy :: Proxy DisableDomainTransferLock)

testCheckDomainAvailabilityResponse :: CheckDomainAvailabilityResponse -> TestTree
testCheckDomainAvailabilityResponse = res
    "CheckDomainAvailabilityResponse"
    "fixture/CheckDomainAvailabilityResponse"
    (Proxy :: Proxy CheckDomainAvailability)

testListTagsForDomainResponse :: ListTagsForDomainResponse -> TestTree
testListTagsForDomainResponse = res
    "ListTagsForDomainResponse"
    "fixture/ListTagsForDomainResponse"
    (Proxy :: Proxy ListTagsForDomain)

testUpdateDomainNameserversResponse :: UpdateDomainNameserversResponse -> TestTree
testUpdateDomainNameserversResponse = res
    "UpdateDomainNameserversResponse"
    "fixture/UpdateDomainNameserversResponse"
    (Proxy :: Proxy UpdateDomainNameservers)

testRetrieveDomainAuthCodeResponse :: RetrieveDomainAuthCodeResponse -> TestTree
testRetrieveDomainAuthCodeResponse = res
    "RetrieveDomainAuthCodeResponse"
    "fixture/RetrieveDomainAuthCodeResponse"
    (Proxy :: Proxy RetrieveDomainAuthCode)

testTransferDomainResponse :: TransferDomainResponse -> TestTree
testTransferDomainResponse = res
    "TransferDomainResponse"
    "fixture/TransferDomainResponse"
    (Proxy :: Proxy TransferDomain)

testDeleteTagsForDomainResponse :: DeleteTagsForDomainResponse -> TestTree
testDeleteTagsForDomainResponse = res
    "DeleteTagsForDomainResponse"
    "fixture/DeleteTagsForDomainResponse"
    (Proxy :: Proxy DeleteTagsForDomain)

testUpdateTagsForDomainResponse :: UpdateTagsForDomainResponse -> TestTree
testUpdateTagsForDomainResponse = res
    "UpdateTagsForDomainResponse"
    "fixture/UpdateTagsForDomainResponse"
    (Proxy :: Proxy UpdateTagsForDomain)

testListDomainsResponse :: ListDomainsResponse -> TestTree
testListDomainsResponse = res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse"
    (Proxy :: Proxy ListDomains)
