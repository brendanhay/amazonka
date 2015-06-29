-- Module      : Test.AWS.Gen.SES
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

module Test.AWS.Gen.SES where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.SES

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ deleteIdentityTest $
--             deleteIdentity
--
--         , deleteVerifiedEmailAddressTest $
--             deleteVerifiedEmailAddress
--
--         , getIdentityDkimAttributesTest $
--             getIdentityDkimAttributes
--
--         , getIdentityNotificationAttributesTest $
--             getIdentityNotificationAttributes
--
--         , getIdentityVerificationAttributesTest $
--             getIdentityVerificationAttributes
--
--         , getSendQuotaTest $
--             getSendQuota
--
--         , getSendStatisticsTest $
--             getSendStatistics
--
--         , listIdentitiesTest $
--             listIdentities
--
--         , listVerifiedEmailAddressesTest $
--             listVerifiedEmailAddresses
--
--         , sendEmailTest $
--             sendEmail
--
--         , sendRawEmailTest $
--             sendRawEmail
--
--         , setIdentityDkimEnabledTest $
--             setIdentityDkimEnabled
--
--         , setIdentityFeedbackForwardingEnabledTest $
--             setIdentityFeedbackForwardingEnabled
--
--         , setIdentityNotificationTopicTest $
--             setIdentityNotificationTopic
--
--         , verifyDomainDkimTest $
--             verifyDomainDkim
--
--         , verifyDomainIdentityTest $
--             verifyDomainIdentity
--
--         , verifyEmailAddressTest $
--             verifyEmailAddress
--
--         , verifyEmailIdentityTest $
--             verifyEmailIdentity
--
--           ]

--     , testGroup "response"
--         [ deleteIdentityResponseTest $
--             deleteIdentityResponse
--
--         , deleteVerifiedEmailAddressResponseTest $
--             deleteVerifiedEmailAddressResponse
--
--         , getIdentityDkimAttributesResponseTest $
--             getIdentityDkimAttributesResponse
--
--         , getIdentityNotificationAttributesResponseTest $
--             getIdentityNotificationAttributesResponse
--
--         , getIdentityVerificationAttributesResponseTest $
--             getIdentityVerificationAttributesResponse
--
--         , getSendQuotaResponseTest $
--             getSendQuotaResponse
--
--         , getSendStatisticsResponseTest $
--             getSendStatisticsResponse
--
--         , listIdentitiesResponseTest $
--             listIdentitiesResponse
--
--         , listVerifiedEmailAddressesResponseTest $
--             listVerifiedEmailAddressesResponse
--
--         , sendEmailResponseTest $
--             sendEmailResponse
--
--         , sendRawEmailResponseTest $
--             sendRawEmailResponse
--
--         , setIdentityDkimEnabledResponseTest $
--             setIdentityDkimEnabledResponse
--
--         , setIdentityFeedbackForwardingEnabledResponseTest $
--             setIdentityFeedbackForwardingEnabledResponse
--
--         , setIdentityNotificationTopicResponseTest $
--             setIdentityNotificationTopicResponse
--
--         , verifyDomainDkimResponseTest $
--             verifyDomainDkimResponse
--
--         , verifyDomainIdentityResponseTest $
--             verifyDomainIdentityResponse
--
--         , verifyEmailAddressResponseTest $
--             verifyEmailAddressResponse
--
--         , verifyEmailIdentityResponseTest $
--             verifyEmailIdentityResponse
--
--           ]
--     ]

-- Requests

deleteIdentityTest :: DeleteIdentity -> TestTree
deleteIdentityTest = undefined

deleteVerifiedEmailAddressTest :: DeleteVerifiedEmailAddress -> TestTree
deleteVerifiedEmailAddressTest = undefined

getIdentityDkimAttributesTest :: GetIdentityDkimAttributes -> TestTree
getIdentityDkimAttributesTest = undefined

getIdentityNotificationAttributesTest :: GetIdentityNotificationAttributes -> TestTree
getIdentityNotificationAttributesTest = undefined

getIdentityVerificationAttributesTest :: GetIdentityVerificationAttributes -> TestTree
getIdentityVerificationAttributesTest = undefined

getSendQuotaTest :: GetSendQuota -> TestTree
getSendQuotaTest = undefined

getSendStatisticsTest :: GetSendStatistics -> TestTree
getSendStatisticsTest = undefined

listIdentitiesTest :: ListIdentities -> TestTree
listIdentitiesTest = undefined

listVerifiedEmailAddressesTest :: ListVerifiedEmailAddresses -> TestTree
listVerifiedEmailAddressesTest = undefined

sendEmailTest :: SendEmail -> TestTree
sendEmailTest = undefined

sendRawEmailTest :: SendRawEmail -> TestTree
sendRawEmailTest = undefined

setIdentityDkimEnabledTest :: SetIdentityDkimEnabled -> TestTree
setIdentityDkimEnabledTest = undefined

setIdentityFeedbackForwardingEnabledTest :: SetIdentityFeedbackForwardingEnabled -> TestTree
setIdentityFeedbackForwardingEnabledTest = undefined

setIdentityNotificationTopicTest :: SetIdentityNotificationTopic -> TestTree
setIdentityNotificationTopicTest = undefined

verifyDomainDkimTest :: VerifyDomainDkim -> TestTree
verifyDomainDkimTest = undefined

verifyDomainIdentityTest :: VerifyDomainIdentity -> TestTree
verifyDomainIdentityTest = undefined

verifyEmailAddressTest :: VerifyEmailAddress -> TestTree
verifyEmailAddressTest = undefined

verifyEmailIdentityTest :: VerifyEmailIdentity -> TestTree
verifyEmailIdentityTest = undefined

-- Responses

deleteIdentityResponseTest :: DeleteIdentityResponse -> TestTree
deleteIdentityResponseTest = resp
    "deleteIdentityResponse"
    "fixture/DeleteIdentityResponse"
    (Proxy :: Proxy DeleteIdentity)

deleteVerifiedEmailAddressResponseTest :: DeleteVerifiedEmailAddressResponse -> TestTree
deleteVerifiedEmailAddressResponseTest = resp
    "deleteVerifiedEmailAddressResponse"
    "fixture/DeleteVerifiedEmailAddressResponse"
    (Proxy :: Proxy DeleteVerifiedEmailAddress)

getIdentityDkimAttributesResponseTest :: GetIdentityDkimAttributesResponse -> TestTree
getIdentityDkimAttributesResponseTest = resp
    "getIdentityDkimAttributesResponse"
    "fixture/GetIdentityDkimAttributesResponse"
    (Proxy :: Proxy GetIdentityDkimAttributes)

getIdentityNotificationAttributesResponseTest :: GetIdentityNotificationAttributesResponse -> TestTree
getIdentityNotificationAttributesResponseTest = resp
    "getIdentityNotificationAttributesResponse"
    "fixture/GetIdentityNotificationAttributesResponse"
    (Proxy :: Proxy GetIdentityNotificationAttributes)

getIdentityVerificationAttributesResponseTest :: GetIdentityVerificationAttributesResponse -> TestTree
getIdentityVerificationAttributesResponseTest = resp
    "getIdentityVerificationAttributesResponse"
    "fixture/GetIdentityVerificationAttributesResponse"
    (Proxy :: Proxy GetIdentityVerificationAttributes)

getSendQuotaResponseTest :: GetSendQuotaResponse -> TestTree
getSendQuotaResponseTest = resp
    "getSendQuotaResponse"
    "fixture/GetSendQuotaResponse"
    (Proxy :: Proxy GetSendQuota)

getSendStatisticsResponseTest :: GetSendStatisticsResponse -> TestTree
getSendStatisticsResponseTest = resp
    "getSendStatisticsResponse"
    "fixture/GetSendStatisticsResponse"
    (Proxy :: Proxy GetSendStatistics)

listIdentitiesResponseTest :: ListIdentitiesResponse -> TestTree
listIdentitiesResponseTest = resp
    "listIdentitiesResponse"
    "fixture/ListIdentitiesResponse"
    (Proxy :: Proxy ListIdentities)

listVerifiedEmailAddressesResponseTest :: ListVerifiedEmailAddressesResponse -> TestTree
listVerifiedEmailAddressesResponseTest = resp
    "listVerifiedEmailAddressesResponse"
    "fixture/ListVerifiedEmailAddressesResponse"
    (Proxy :: Proxy ListVerifiedEmailAddresses)

sendEmailResponseTest :: SendEmailResponse -> TestTree
sendEmailResponseTest = resp
    "sendEmailResponse"
    "fixture/SendEmailResponse"
    (Proxy :: Proxy SendEmail)

sendRawEmailResponseTest :: SendRawEmailResponse -> TestTree
sendRawEmailResponseTest = resp
    "sendRawEmailResponse"
    "fixture/SendRawEmailResponse"
    (Proxy :: Proxy SendRawEmail)

setIdentityDkimEnabledResponseTest :: SetIdentityDkimEnabledResponse -> TestTree
setIdentityDkimEnabledResponseTest = resp
    "setIdentityDkimEnabledResponse"
    "fixture/SetIdentityDkimEnabledResponse"
    (Proxy :: Proxy SetIdentityDkimEnabled)

setIdentityFeedbackForwardingEnabledResponseTest :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
setIdentityFeedbackForwardingEnabledResponseTest = resp
    "setIdentityFeedbackForwardingEnabledResponse"
    "fixture/SetIdentityFeedbackForwardingEnabledResponse"
    (Proxy :: Proxy SetIdentityFeedbackForwardingEnabled)

setIdentityNotificationTopicResponseTest :: SetIdentityNotificationTopicResponse -> TestTree
setIdentityNotificationTopicResponseTest = resp
    "setIdentityNotificationTopicResponse"
    "fixture/SetIdentityNotificationTopicResponse"
    (Proxy :: Proxy SetIdentityNotificationTopic)

verifyDomainDkimResponseTest :: VerifyDomainDkimResponse -> TestTree
verifyDomainDkimResponseTest = resp
    "verifyDomainDkimResponse"
    "fixture/VerifyDomainDkimResponse"
    (Proxy :: Proxy VerifyDomainDkim)

verifyDomainIdentityResponseTest :: VerifyDomainIdentityResponse -> TestTree
verifyDomainIdentityResponseTest = resp
    "verifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse"
    (Proxy :: Proxy VerifyDomainIdentity)

verifyEmailAddressResponseTest :: VerifyEmailAddressResponse -> TestTree
verifyEmailAddressResponseTest = resp
    "verifyEmailAddressResponse"
    "fixture/VerifyEmailAddressResponse"
    (Proxy :: Proxy VerifyEmailAddress)

verifyEmailIdentityResponseTest :: VerifyEmailIdentityResponse -> TestTree
verifyEmailIdentityResponseTest = resp
    "verifyEmailIdentityResponse"
    "fixture/VerifyEmailIdentityResponse"
    (Proxy :: Proxy VerifyEmailIdentity)
