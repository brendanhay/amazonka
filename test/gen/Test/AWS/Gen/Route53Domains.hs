-- Module      : Test.AWS.Gen.Route53Domains
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

import           Data.Proxy
import           Network.AWS.Route53Domains
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ listOperationsTest $
--             listOperations
--
--         , getDomainDetailTest $
--             getDomainDetail
--
--         , updateDomainContactPrivacyTest $
--             updateDomainContactPrivacy
--
--         , getOperationDetailTest $
--             getOperationDetail
--
--         , enableDomainAutoRenewTest $
--             enableDomainAutoRenew
--
--         , disableDomainAutoRenewTest $
--             disableDomainAutoRenew
--
--         , updateDomainContactTest $
--             updateDomainContact
--
--         , enableDomainTransferLockTest $
--             enableDomainTransferLock
--
--         , registerDomainTest $
--             registerDomain
--
--         , disableDomainTransferLockTest $
--             disableDomainTransferLock
--
--         , checkDomainAvailabilityTest $
--             checkDomainAvailability
--
--         , listTagsForDomainTest $
--             listTagsForDomain
--
--         , updateDomainNameserversTest $
--             updateDomainNameservers
--
--         , retrieveDomainAuthCodeTest $
--             retrieveDomainAuthCode
--
--         , transferDomainTest $
--             transferDomain
--
--         , deleteTagsForDomainTest $
--             deleteTagsForDomain
--
--         , updateTagsForDomainTest $
--             updateTagsForDomain
--
--         , listDomainsTest $
--             listDomains
--
--           ]

--     , testGroup "response"
--         [ listOperationsResponseTest $
--             listOperationsResponse
--
--         , getDomainDetailResponseTest $
--             getDomainDetailResponse
--
--         , updateDomainContactPrivacyResponseTest $
--             updateDomainContactPrivacyResponse
--
--         , getOperationDetailResponseTest $
--             getOperationDetailResponse
--
--         , enableDomainAutoRenewResponseTest $
--             enableDomainAutoRenewResponse
--
--         , disableDomainAutoRenewResponseTest $
--             disableDomainAutoRenewResponse
--
--         , updateDomainContactResponseTest $
--             updateDomainContactResponse
--
--         , enableDomainTransferLockResponseTest $
--             enableDomainTransferLockResponse
--
--         , registerDomainResponseTest $
--             registerDomainResponse
--
--         , disableDomainTransferLockResponseTest $
--             disableDomainTransferLockResponse
--
--         , checkDomainAvailabilityResponseTest $
--             checkDomainAvailabilityResponse
--
--         , listTagsForDomainResponseTest $
--             listTagsForDomainResponse
--
--         , updateDomainNameserversResponseTest $
--             updateDomainNameserversResponse
--
--         , retrieveDomainAuthCodeResponseTest $
--             retrieveDomainAuthCodeResponse
--
--         , transferDomainResponseTest $
--             transferDomainResponse
--
--         , deleteTagsForDomainResponseTest $
--             deleteTagsForDomainResponse
--
--         , updateTagsForDomainResponseTest $
--             updateTagsForDomainResponse
--
--         , listDomainsResponseTest $
--             listDomainsResponse
--
--           ]
--     ]

-- Requests

listOperationsTest :: ListOperations -> TestTree
listOperationsTest = undefined

getDomainDetailTest :: GetDomainDetail -> TestTree
getDomainDetailTest = undefined

updateDomainContactPrivacyTest :: UpdateDomainContactPrivacy -> TestTree
updateDomainContactPrivacyTest = undefined

getOperationDetailTest :: GetOperationDetail -> TestTree
getOperationDetailTest = undefined

enableDomainAutoRenewTest :: EnableDomainAutoRenew -> TestTree
enableDomainAutoRenewTest = undefined

disableDomainAutoRenewTest :: DisableDomainAutoRenew -> TestTree
disableDomainAutoRenewTest = undefined

updateDomainContactTest :: UpdateDomainContact -> TestTree
updateDomainContactTest = undefined

enableDomainTransferLockTest :: EnableDomainTransferLock -> TestTree
enableDomainTransferLockTest = undefined

registerDomainTest :: RegisterDomain -> TestTree
registerDomainTest = undefined

disableDomainTransferLockTest :: DisableDomainTransferLock -> TestTree
disableDomainTransferLockTest = undefined

checkDomainAvailabilityTest :: CheckDomainAvailability -> TestTree
checkDomainAvailabilityTest = undefined

listTagsForDomainTest :: ListTagsForDomain -> TestTree
listTagsForDomainTest = undefined

updateDomainNameserversTest :: UpdateDomainNameservers -> TestTree
updateDomainNameserversTest = undefined

retrieveDomainAuthCodeTest :: RetrieveDomainAuthCode -> TestTree
retrieveDomainAuthCodeTest = undefined

transferDomainTest :: TransferDomain -> TestTree
transferDomainTest = undefined

deleteTagsForDomainTest :: DeleteTagsForDomain -> TestTree
deleteTagsForDomainTest = undefined

updateTagsForDomainTest :: UpdateTagsForDomain -> TestTree
updateTagsForDomainTest = undefined

listDomainsTest :: ListDomains -> TestTree
listDomainsTest = undefined

-- Responses

listOperationsResponseTest :: ListOperationsResponse -> TestTree
listOperationsResponseTest = resp
    "ListOperations"
    "fixture/Route53Domains/ListOperationsResponse"
    (Proxy :: Proxy ListOperations)

getDomainDetailResponseTest :: GetDomainDetailResponse -> TestTree
getDomainDetailResponseTest = resp
    "GetDomainDetail"
    "fixture/Route53Domains/GetDomainDetailResponse"
    (Proxy :: Proxy GetDomainDetail)

updateDomainContactPrivacyResponseTest :: UpdateDomainContactPrivacyResponse -> TestTree
updateDomainContactPrivacyResponseTest = resp
    "UpdateDomainContactPrivacy"
    "fixture/Route53Domains/UpdateDomainContactPrivacyResponse"
    (Proxy :: Proxy UpdateDomainContactPrivacy)

getOperationDetailResponseTest :: GetOperationDetailResponse -> TestTree
getOperationDetailResponseTest = resp
    "GetOperationDetail"
    "fixture/Route53Domains/GetOperationDetailResponse"
    (Proxy :: Proxy GetOperationDetail)

enableDomainAutoRenewResponseTest :: EnableDomainAutoRenewResponse -> TestTree
enableDomainAutoRenewResponseTest = resp
    "EnableDomainAutoRenew"
    "fixture/Route53Domains/EnableDomainAutoRenewResponse"
    (Proxy :: Proxy EnableDomainAutoRenew)

disableDomainAutoRenewResponseTest :: DisableDomainAutoRenewResponse -> TestTree
disableDomainAutoRenewResponseTest = resp
    "DisableDomainAutoRenew"
    "fixture/Route53Domains/DisableDomainAutoRenewResponse"
    (Proxy :: Proxy DisableDomainAutoRenew)

updateDomainContactResponseTest :: UpdateDomainContactResponse -> TestTree
updateDomainContactResponseTest = resp
    "UpdateDomainContact"
    "fixture/Route53Domains/UpdateDomainContactResponse"
    (Proxy :: Proxy UpdateDomainContact)

enableDomainTransferLockResponseTest :: EnableDomainTransferLockResponse -> TestTree
enableDomainTransferLockResponseTest = resp
    "EnableDomainTransferLock"
    "fixture/Route53Domains/EnableDomainTransferLockResponse"
    (Proxy :: Proxy EnableDomainTransferLock)

registerDomainResponseTest :: RegisterDomainResponse -> TestTree
registerDomainResponseTest = resp
    "RegisterDomain"
    "fixture/Route53Domains/RegisterDomainResponse"
    (Proxy :: Proxy RegisterDomain)

disableDomainTransferLockResponseTest :: DisableDomainTransferLockResponse -> TestTree
disableDomainTransferLockResponseTest = resp
    "DisableDomainTransferLock"
    "fixture/Route53Domains/DisableDomainTransferLockResponse"
    (Proxy :: Proxy DisableDomainTransferLock)

checkDomainAvailabilityResponseTest :: CheckDomainAvailabilityResponse -> TestTree
checkDomainAvailabilityResponseTest = resp
    "CheckDomainAvailability"
    "fixture/Route53Domains/CheckDomainAvailabilityResponse"
    (Proxy :: Proxy CheckDomainAvailability)

listTagsForDomainResponseTest :: ListTagsForDomainResponse -> TestTree
listTagsForDomainResponseTest = resp
    "ListTagsForDomain"
    "fixture/Route53Domains/ListTagsForDomainResponse"
    (Proxy :: Proxy ListTagsForDomain)

updateDomainNameserversResponseTest :: UpdateDomainNameserversResponse -> TestTree
updateDomainNameserversResponseTest = resp
    "UpdateDomainNameservers"
    "fixture/Route53Domains/UpdateDomainNameserversResponse"
    (Proxy :: Proxy UpdateDomainNameservers)

retrieveDomainAuthCodeResponseTest :: RetrieveDomainAuthCodeResponse -> TestTree
retrieveDomainAuthCodeResponseTest = resp
    "RetrieveDomainAuthCode"
    "fixture/Route53Domains/RetrieveDomainAuthCodeResponse"
    (Proxy :: Proxy RetrieveDomainAuthCode)

transferDomainResponseTest :: TransferDomainResponse -> TestTree
transferDomainResponseTest = resp
    "TransferDomain"
    "fixture/Route53Domains/TransferDomainResponse"
    (Proxy :: Proxy TransferDomain)

deleteTagsForDomainResponseTest :: DeleteTagsForDomainResponse -> TestTree
deleteTagsForDomainResponseTest = resp
    "DeleteTagsForDomain"
    "fixture/Route53Domains/DeleteTagsForDomainResponse"
    (Proxy :: Proxy DeleteTagsForDomain)

updateTagsForDomainResponseTest :: UpdateTagsForDomainResponse -> TestTree
updateTagsForDomainResponseTest = resp
    "UpdateTagsForDomain"
    "fixture/Route53Domains/UpdateTagsForDomainResponse"
    (Proxy :: Proxy UpdateTagsForDomain)

listDomainsResponseTest :: ListDomainsResponse -> TestTree
listDomainsResponseTest = resp
    "ListDomains"
    "fixture/Route53Domains/ListDomainsResponse"
    (Proxy :: Proxy ListDomains)
