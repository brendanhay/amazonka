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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.Route53Domains

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ checkDomainAvailabilityTest $
--             checkDomainAvailability
--
--         , deleteTagsForDomainTest $
--             deleteTagsForDomain
--
--         , disableDomainAutoRenewTest $
--             disableDomainAutoRenew
--
--         , disableDomainTransferLockTest $
--             disableDomainTransferLock
--
--         , enableDomainAutoRenewTest $
--             enableDomainAutoRenew
--
--         , enableDomainTransferLockTest $
--             enableDomainTransferLock
--
--         , getDomainDetailTest $
--             getDomainDetail
--
--         , getOperationDetailTest $
--             getOperationDetail
--
--         , listDomainsTest $
--             listDomains
--
--         , listOperationsTest $
--             listOperations
--
--         , listTagsForDomainTest $
--             listTagsForDomain
--
--         , registerDomainTest $
--             registerDomain
--
--         , retrieveDomainAuthCodeTest $
--             retrieveDomainAuthCode
--
--         , transferDomainTest $
--             transferDomain
--
--         , updateDomainContactTest $
--             updateDomainContact
--
--         , updateDomainContactPrivacyTest $
--             updateDomainContactPrivacy
--
--         , updateDomainNameserversTest $
--             updateDomainNameservers
--
--         , updateTagsForDomainTest $
--             updateTagsForDomain
--
--           ]

--     , testGroup "response"
--         [ checkDomainAvailabilityResponseTest $
--             checkDomainAvailabilityResponse
--
--         , deleteTagsForDomainResponseTest $
--             deleteTagsForDomainResponse
--
--         , disableDomainAutoRenewResponseTest $
--             disableDomainAutoRenewResponse
--
--         , disableDomainTransferLockResponseTest $
--             disableDomainTransferLockResponse
--
--         , enableDomainAutoRenewResponseTest $
--             enableDomainAutoRenewResponse
--
--         , enableDomainTransferLockResponseTest $
--             enableDomainTransferLockResponse
--
--         , getDomainDetailResponseTest $
--             getDomainDetailResponse
--
--         , getOperationDetailResponseTest $
--             getOperationDetailResponse
--
--         , listDomainsResponseTest $
--             listDomainsResponse
--
--         , listOperationsResponseTest $
--             listOperationsResponse
--
--         , listTagsForDomainResponseTest $
--             listTagsForDomainResponse
--
--         , registerDomainResponseTest $
--             registerDomainResponse
--
--         , retrieveDomainAuthCodeResponseTest $
--             retrieveDomainAuthCodeResponse
--
--         , transferDomainResponseTest $
--             transferDomainResponse
--
--         , updateDomainContactResponseTest $
--             updateDomainContactResponse
--
--         , updateDomainContactPrivacyResponseTest $
--             updateDomainContactPrivacyResponse
--
--         , updateDomainNameserversResponseTest $
--             updateDomainNameserversResponse
--
--         , updateTagsForDomainResponseTest $
--             updateTagsForDomainResponse
--
--           ]
--     ]

-- Requests

checkDomainAvailabilityTest :: CheckDomainAvailability -> TestTree
checkDomainAvailabilityTest = undefined

deleteTagsForDomainTest :: DeleteTagsForDomain -> TestTree
deleteTagsForDomainTest = undefined

disableDomainAutoRenewTest :: DisableDomainAutoRenew -> TestTree
disableDomainAutoRenewTest = undefined

disableDomainTransferLockTest :: DisableDomainTransferLock -> TestTree
disableDomainTransferLockTest = undefined

enableDomainAutoRenewTest :: EnableDomainAutoRenew -> TestTree
enableDomainAutoRenewTest = undefined

enableDomainTransferLockTest :: EnableDomainTransferLock -> TestTree
enableDomainTransferLockTest = undefined

getDomainDetailTest :: GetDomainDetail -> TestTree
getDomainDetailTest = undefined

getOperationDetailTest :: GetOperationDetail -> TestTree
getOperationDetailTest = undefined

listDomainsTest :: ListDomains -> TestTree
listDomainsTest = undefined

listOperationsTest :: ListOperations -> TestTree
listOperationsTest = undefined

listTagsForDomainTest :: ListTagsForDomain -> TestTree
listTagsForDomainTest = undefined

registerDomainTest :: RegisterDomain -> TestTree
registerDomainTest = undefined

retrieveDomainAuthCodeTest :: RetrieveDomainAuthCode -> TestTree
retrieveDomainAuthCodeTest = undefined

transferDomainTest :: TransferDomain -> TestTree
transferDomainTest = undefined

updateDomainContactTest :: UpdateDomainContact -> TestTree
updateDomainContactTest = undefined

updateDomainContactPrivacyTest :: UpdateDomainContactPrivacy -> TestTree
updateDomainContactPrivacyTest = undefined

updateDomainNameserversTest :: UpdateDomainNameservers -> TestTree
updateDomainNameserversTest = undefined

updateTagsForDomainTest :: UpdateTagsForDomain -> TestTree
updateTagsForDomainTest = undefined

-- Responses

checkDomainAvailabilityResponseTest :: CheckDomainAvailabilityResponse -> TestTree
checkDomainAvailabilityResponseTest = resp
    "checkDomainAvailabilityResponse"
    "fixture/CheckDomainAvailabilityResponse"
    (Proxy :: Proxy CheckDomainAvailability)

deleteTagsForDomainResponseTest :: DeleteTagsForDomainResponse -> TestTree
deleteTagsForDomainResponseTest = resp
    "deleteTagsForDomainResponse"
    "fixture/DeleteTagsForDomainResponse"
    (Proxy :: Proxy DeleteTagsForDomain)

disableDomainAutoRenewResponseTest :: DisableDomainAutoRenewResponse -> TestTree
disableDomainAutoRenewResponseTest = resp
    "disableDomainAutoRenewResponse"
    "fixture/DisableDomainAutoRenewResponse"
    (Proxy :: Proxy DisableDomainAutoRenew)

disableDomainTransferLockResponseTest :: DisableDomainTransferLockResponse -> TestTree
disableDomainTransferLockResponseTest = resp
    "disableDomainTransferLockResponse"
    "fixture/DisableDomainTransferLockResponse"
    (Proxy :: Proxy DisableDomainTransferLock)

enableDomainAutoRenewResponseTest :: EnableDomainAutoRenewResponse -> TestTree
enableDomainAutoRenewResponseTest = resp
    "enableDomainAutoRenewResponse"
    "fixture/EnableDomainAutoRenewResponse"
    (Proxy :: Proxy EnableDomainAutoRenew)

enableDomainTransferLockResponseTest :: EnableDomainTransferLockResponse -> TestTree
enableDomainTransferLockResponseTest = resp
    "enableDomainTransferLockResponse"
    "fixture/EnableDomainTransferLockResponse"
    (Proxy :: Proxy EnableDomainTransferLock)

getDomainDetailResponseTest :: GetDomainDetailResponse -> TestTree
getDomainDetailResponseTest = resp
    "getDomainDetailResponse"
    "fixture/GetDomainDetailResponse"
    (Proxy :: Proxy GetDomainDetail)

getOperationDetailResponseTest :: GetOperationDetailResponse -> TestTree
getOperationDetailResponseTest = resp
    "getOperationDetailResponse"
    "fixture/GetOperationDetailResponse"
    (Proxy :: Proxy GetOperationDetail)

listDomainsResponseTest :: ListDomainsResponse -> TestTree
listDomainsResponseTest = resp
    "listDomainsResponse"
    "fixture/ListDomainsResponse"
    (Proxy :: Proxy ListDomains)

listOperationsResponseTest :: ListOperationsResponse -> TestTree
listOperationsResponseTest = resp
    "listOperationsResponse"
    "fixture/ListOperationsResponse"
    (Proxy :: Proxy ListOperations)

listTagsForDomainResponseTest :: ListTagsForDomainResponse -> TestTree
listTagsForDomainResponseTest = resp
    "listTagsForDomainResponse"
    "fixture/ListTagsForDomainResponse"
    (Proxy :: Proxy ListTagsForDomain)

registerDomainResponseTest :: RegisterDomainResponse -> TestTree
registerDomainResponseTest = resp
    "registerDomainResponse"
    "fixture/RegisterDomainResponse"
    (Proxy :: Proxy RegisterDomain)

retrieveDomainAuthCodeResponseTest :: RetrieveDomainAuthCodeResponse -> TestTree
retrieveDomainAuthCodeResponseTest = resp
    "retrieveDomainAuthCodeResponse"
    "fixture/RetrieveDomainAuthCodeResponse"
    (Proxy :: Proxy RetrieveDomainAuthCode)

transferDomainResponseTest :: TransferDomainResponse -> TestTree
transferDomainResponseTest = resp
    "transferDomainResponse"
    "fixture/TransferDomainResponse"
    (Proxy :: Proxy TransferDomain)

updateDomainContactResponseTest :: UpdateDomainContactResponse -> TestTree
updateDomainContactResponseTest = resp
    "updateDomainContactResponse"
    "fixture/UpdateDomainContactResponse"
    (Proxy :: Proxy UpdateDomainContact)

updateDomainContactPrivacyResponseTest :: UpdateDomainContactPrivacyResponse -> TestTree
updateDomainContactPrivacyResponseTest = resp
    "updateDomainContactPrivacyResponse"
    "fixture/UpdateDomainContactPrivacyResponse"
    (Proxy :: Proxy UpdateDomainContactPrivacy)

updateDomainNameserversResponseTest :: UpdateDomainNameserversResponse -> TestTree
updateDomainNameserversResponseTest = resp
    "updateDomainNameserversResponse"
    "fixture/UpdateDomainNameserversResponse"
    (Proxy :: Proxy UpdateDomainNameservers)

updateTagsForDomainResponseTest :: UpdateTagsForDomainResponse -> TestTree
updateTagsForDomainResponseTest = resp
    "updateTagsForDomainResponse"
    "fixture/UpdateTagsForDomainResponse"
    (Proxy :: Proxy UpdateTagsForDomain)
