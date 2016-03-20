{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53Domains
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
--         , testDeleteTagsForDomain $
--             deleteTagsForDomain
--
--         , testUpdateTagsForDomain $
--             updateTagsForDomain
--
--         , testRetrieveDomainAuthCode $
--             retrieveDomainAuthCode
--
--         , testTransferDomain $
--             transferDomain
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
--         , testDeleteTagsForDomainResponse $
--             deleteTagsForDomainResponse
--
--         , testUpdateTagsForDomainResponse $
--             updateTagsForDomainResponse
--
--         , testRetrieveDomainAuthCodeResponse $
--             retrieveDomainAuthCodeResponse
--
--         , testTransferDomainResponse $
--             transferDomainResponse
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
    "fixture/ListOperations.yaml"

testGetDomainDetail :: GetDomainDetail -> TestTree
testGetDomainDetail = req
    "GetDomainDetail"
    "fixture/GetDomainDetail.yaml"

testUpdateDomainContactPrivacy :: UpdateDomainContactPrivacy -> TestTree
testUpdateDomainContactPrivacy = req
    "UpdateDomainContactPrivacy"
    "fixture/UpdateDomainContactPrivacy.yaml"

testGetOperationDetail :: GetOperationDetail -> TestTree
testGetOperationDetail = req
    "GetOperationDetail"
    "fixture/GetOperationDetail.yaml"

testEnableDomainAutoRenew :: EnableDomainAutoRenew -> TestTree
testEnableDomainAutoRenew = req
    "EnableDomainAutoRenew"
    "fixture/EnableDomainAutoRenew.yaml"

testDisableDomainAutoRenew :: DisableDomainAutoRenew -> TestTree
testDisableDomainAutoRenew = req
    "DisableDomainAutoRenew"
    "fixture/DisableDomainAutoRenew.yaml"

testUpdateDomainContact :: UpdateDomainContact -> TestTree
testUpdateDomainContact = req
    "UpdateDomainContact"
    "fixture/UpdateDomainContact.yaml"

testEnableDomainTransferLock :: EnableDomainTransferLock -> TestTree
testEnableDomainTransferLock = req
    "EnableDomainTransferLock"
    "fixture/EnableDomainTransferLock.yaml"

testRegisterDomain :: RegisterDomain -> TestTree
testRegisterDomain = req
    "RegisterDomain"
    "fixture/RegisterDomain.yaml"

testDisableDomainTransferLock :: DisableDomainTransferLock -> TestTree
testDisableDomainTransferLock = req
    "DisableDomainTransferLock"
    "fixture/DisableDomainTransferLock.yaml"

testCheckDomainAvailability :: CheckDomainAvailability -> TestTree
testCheckDomainAvailability = req
    "CheckDomainAvailability"
    "fixture/CheckDomainAvailability.yaml"

testListTagsForDomain :: ListTagsForDomain -> TestTree
testListTagsForDomain = req
    "ListTagsForDomain"
    "fixture/ListTagsForDomain.yaml"

testUpdateDomainNameservers :: UpdateDomainNameservers -> TestTree
testUpdateDomainNameservers = req
    "UpdateDomainNameservers"
    "fixture/UpdateDomainNameservers.yaml"

testDeleteTagsForDomain :: DeleteTagsForDomain -> TestTree
testDeleteTagsForDomain = req
    "DeleteTagsForDomain"
    "fixture/DeleteTagsForDomain.yaml"

testUpdateTagsForDomain :: UpdateTagsForDomain -> TestTree
testUpdateTagsForDomain = req
    "UpdateTagsForDomain"
    "fixture/UpdateTagsForDomain.yaml"

testRetrieveDomainAuthCode :: RetrieveDomainAuthCode -> TestTree
testRetrieveDomainAuthCode = req
    "RetrieveDomainAuthCode"
    "fixture/RetrieveDomainAuthCode.yaml"

testTransferDomain :: TransferDomain -> TestTree
testTransferDomain = req
    "TransferDomain"
    "fixture/TransferDomain.yaml"

testListDomains :: ListDomains -> TestTree
testListDomains = req
    "ListDomains"
    "fixture/ListDomains.yaml"

-- Responses

testListOperationsResponse :: ListOperationsResponse -> TestTree
testListOperationsResponse = res
    "ListOperationsResponse"
    "fixture/ListOperationsResponse.proto"
    route53Domains
    (Proxy :: Proxy ListOperations)

testGetDomainDetailResponse :: GetDomainDetailResponse -> TestTree
testGetDomainDetailResponse = res
    "GetDomainDetailResponse"
    "fixture/GetDomainDetailResponse.proto"
    route53Domains
    (Proxy :: Proxy GetDomainDetail)

testUpdateDomainContactPrivacyResponse :: UpdateDomainContactPrivacyResponse -> TestTree
testUpdateDomainContactPrivacyResponse = res
    "UpdateDomainContactPrivacyResponse"
    "fixture/UpdateDomainContactPrivacyResponse.proto"
    route53Domains
    (Proxy :: Proxy UpdateDomainContactPrivacy)

testGetOperationDetailResponse :: GetOperationDetailResponse -> TestTree
testGetOperationDetailResponse = res
    "GetOperationDetailResponse"
    "fixture/GetOperationDetailResponse.proto"
    route53Domains
    (Proxy :: Proxy GetOperationDetail)

testEnableDomainAutoRenewResponse :: EnableDomainAutoRenewResponse -> TestTree
testEnableDomainAutoRenewResponse = res
    "EnableDomainAutoRenewResponse"
    "fixture/EnableDomainAutoRenewResponse.proto"
    route53Domains
    (Proxy :: Proxy EnableDomainAutoRenew)

testDisableDomainAutoRenewResponse :: DisableDomainAutoRenewResponse -> TestTree
testDisableDomainAutoRenewResponse = res
    "DisableDomainAutoRenewResponse"
    "fixture/DisableDomainAutoRenewResponse.proto"
    route53Domains
    (Proxy :: Proxy DisableDomainAutoRenew)

testUpdateDomainContactResponse :: UpdateDomainContactResponse -> TestTree
testUpdateDomainContactResponse = res
    "UpdateDomainContactResponse"
    "fixture/UpdateDomainContactResponse.proto"
    route53Domains
    (Proxy :: Proxy UpdateDomainContact)

testEnableDomainTransferLockResponse :: EnableDomainTransferLockResponse -> TestTree
testEnableDomainTransferLockResponse = res
    "EnableDomainTransferLockResponse"
    "fixture/EnableDomainTransferLockResponse.proto"
    route53Domains
    (Proxy :: Proxy EnableDomainTransferLock)

testRegisterDomainResponse :: RegisterDomainResponse -> TestTree
testRegisterDomainResponse = res
    "RegisterDomainResponse"
    "fixture/RegisterDomainResponse.proto"
    route53Domains
    (Proxy :: Proxy RegisterDomain)

testDisableDomainTransferLockResponse :: DisableDomainTransferLockResponse -> TestTree
testDisableDomainTransferLockResponse = res
    "DisableDomainTransferLockResponse"
    "fixture/DisableDomainTransferLockResponse.proto"
    route53Domains
    (Proxy :: Proxy DisableDomainTransferLock)

testCheckDomainAvailabilityResponse :: CheckDomainAvailabilityResponse -> TestTree
testCheckDomainAvailabilityResponse = res
    "CheckDomainAvailabilityResponse"
    "fixture/CheckDomainAvailabilityResponse.proto"
    route53Domains
    (Proxy :: Proxy CheckDomainAvailability)

testListTagsForDomainResponse :: ListTagsForDomainResponse -> TestTree
testListTagsForDomainResponse = res
    "ListTagsForDomainResponse"
    "fixture/ListTagsForDomainResponse.proto"
    route53Domains
    (Proxy :: Proxy ListTagsForDomain)

testUpdateDomainNameserversResponse :: UpdateDomainNameserversResponse -> TestTree
testUpdateDomainNameserversResponse = res
    "UpdateDomainNameserversResponse"
    "fixture/UpdateDomainNameserversResponse.proto"
    route53Domains
    (Proxy :: Proxy UpdateDomainNameservers)

testDeleteTagsForDomainResponse :: DeleteTagsForDomainResponse -> TestTree
testDeleteTagsForDomainResponse = res
    "DeleteTagsForDomainResponse"
    "fixture/DeleteTagsForDomainResponse.proto"
    route53Domains
    (Proxy :: Proxy DeleteTagsForDomain)

testUpdateTagsForDomainResponse :: UpdateTagsForDomainResponse -> TestTree
testUpdateTagsForDomainResponse = res
    "UpdateTagsForDomainResponse"
    "fixture/UpdateTagsForDomainResponse.proto"
    route53Domains
    (Proxy :: Proxy UpdateTagsForDomain)

testRetrieveDomainAuthCodeResponse :: RetrieveDomainAuthCodeResponse -> TestTree
testRetrieveDomainAuthCodeResponse = res
    "RetrieveDomainAuthCodeResponse"
    "fixture/RetrieveDomainAuthCodeResponse.proto"
    route53Domains
    (Proxy :: Proxy RetrieveDomainAuthCode)

testTransferDomainResponse :: TransferDomainResponse -> TestTree
testTransferDomainResponse = res
    "TransferDomainResponse"
    "fixture/TransferDomainResponse.proto"
    route53Domains
    (Proxy :: Proxy TransferDomain)

testListDomainsResponse :: ListDomainsResponse -> TestTree
testListDomainsResponse = res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    route53Domains
    (Proxy :: Proxy ListDomains)
