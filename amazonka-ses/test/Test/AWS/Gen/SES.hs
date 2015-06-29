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
--         [ getSendQuotaTest $
--             getSendQuota
--
--         , setIdentityDkimEnabledTest $
--             setIdentityDkimEnabled
--
--         , getIdentityNotificationAttributesTest $
--             getIdentityNotificationAttributes
--
--         , setIdentityFeedbackForwardingEnabledTest $
--             setIdentityFeedbackForwardingEnabled
--
--         , getIdentityVerificationAttributesTest $
--             getIdentityVerificationAttributes
--
--         , verifyDomainIdentityTest $
--             verifyDomainIdentity
--
--         , verifyDomainDkimTest $
--             verifyDomainDkim
--
--         , sendRawEmailTest $
--             sendRawEmail
--
--         , getIdentityDkimAttributesTest $
--             getIdentityDkimAttributes
--
--         , deleteIdentityTest $
--             deleteIdentity
--
--         , getSendStatisticsTest $
--             getSendStatistics
--
--         , listIdentitiesTest $
--             listIdentities
--
--         , deleteVerifiedEmailAddressTest $
--             deleteVerifiedEmailAddress
--
--         , verifyEmailAddressTest $
--             verifyEmailAddress
--
--         , verifyEmailIdentityTest $
--             verifyEmailIdentity
--
--         , sendEmailTest $
--             sendEmail
--
--         , listVerifiedEmailAddressesTest $
--             listVerifiedEmailAddresses
--
--         , setIdentityNotificationTopicTest $
--             setIdentityNotificationTopic
--
--           ]

--     , testGroup "response"
--         [ getSendQuotaResponseTest $
--             getSendQuotaResponse
--
--         , setIdentityDkimEnabledResponseTest $
--             setIdentityDkimEnabledResponse
--
--         , getIdentityNotificationAttributesResponseTest $
--             getIdentityNotificationAttributesResponse
--
--         , setIdentityFeedbackForwardingEnabledResponseTest $
--             setIdentityFeedbackForwardingEnabledResponse
--
--         , getIdentityVerificationAttributesResponseTest $
--             getIdentityVerificationAttributesResponse
--
--         , verifyDomainIdentityResponseTest $
--             verifyDomainIdentityResponse
--
--         , verifyDomainDkimResponseTest $
--             verifyDomainDkimResponse
--
--         , sendRawEmailResponseTest $
--             sendRawEmailResponse
--
--         , getIdentityDkimAttributesResponseTest $
--             getIdentityDkimAttributesResponse
--
--         , deleteIdentityResponseTest $
--             deleteIdentityResponse
--
--         , getSendStatisticsResponseTest $
--             getSendStatisticsResponse
--
--         , listIdentitiesResponseTest $
--             listIdentitiesResponse
--
--         , deleteVerifiedEmailAddressResponseTest $
--             deleteVerifiedEmailAddressResponse
--
--         , verifyEmailAddressResponseTest $
--             verifyEmailAddressResponse
--
--         , verifyEmailIdentityResponseTest $
--             verifyEmailIdentityResponse
--
--         , sendEmailResponseTest $
--             sendEmailResponse
--
--         , listVerifiedEmailAddressesResponseTest $
--             listVerifiedEmailAddressesResponse
--
--         , setIdentityNotificationTopicResponseTest $
--             setIdentityNotificationTopicResponse
--
--           ]
--     ]

-- Requests

getSendQuotaTest :: GetSendQuota -> TestTree
getSendQuotaTest = undefined

setIdentityDkimEnabledTest :: SetIdentityDkimEnabled -> TestTree
setIdentityDkimEnabledTest = undefined

getIdentityNotificationAttributesTest :: GetIdentityNotificationAttributes -> TestTree
getIdentityNotificationAttributesTest = undefined

setIdentityFeedbackForwardingEnabledTest :: SetIdentityFeedbackForwardingEnabled -> TestTree
setIdentityFeedbackForwardingEnabledTest = undefined

getIdentityVerificationAttributesTest :: GetIdentityVerificationAttributes -> TestTree
getIdentityVerificationAttributesTest = undefined

verifyDomainIdentityTest :: VerifyDomainIdentity -> TestTree
verifyDomainIdentityTest = undefined

verifyDomainDkimTest :: VerifyDomainDkim -> TestTree
verifyDomainDkimTest = undefined

sendRawEmailTest :: SendRawEmail -> TestTree
sendRawEmailTest = undefined

getIdentityDkimAttributesTest :: GetIdentityDkimAttributes -> TestTree
getIdentityDkimAttributesTest = undefined

deleteIdentityTest :: DeleteIdentity -> TestTree
deleteIdentityTest = undefined

getSendStatisticsTest :: GetSendStatistics -> TestTree
getSendStatisticsTest = undefined

listIdentitiesTest :: ListIdentities -> TestTree
listIdentitiesTest = undefined

deleteVerifiedEmailAddressTest :: DeleteVerifiedEmailAddress -> TestTree
deleteVerifiedEmailAddressTest = undefined

verifyEmailAddressTest :: VerifyEmailAddress -> TestTree
verifyEmailAddressTest = undefined

verifyEmailIdentityTest :: VerifyEmailIdentity -> TestTree
verifyEmailIdentityTest = undefined

sendEmailTest :: SendEmail -> TestTree
sendEmailTest = undefined

listVerifiedEmailAddressesTest :: ListVerifiedEmailAddresses -> TestTree
listVerifiedEmailAddressesTest = undefined

setIdentityNotificationTopicTest :: SetIdentityNotificationTopic -> TestTree
setIdentityNotificationTopicTest = undefined

-- Responses

getSendQuotaResponseTest :: GetSendQuotaResponse -> TestTree
getSendQuotaResponseTest = resp
    "GetSendQuotaResponse"
    "fixture/GetSendQuotaResponse"
    (Proxy :: Proxy GetSendQuota)

setIdentityDkimEnabledResponseTest :: SetIdentityDkimEnabledResponse -> TestTree
setIdentityDkimEnabledResponseTest = resp
    "SetIdentityDkimEnabledResponse"
    "fixture/SetIdentityDkimEnabledResponse"
    (Proxy :: Proxy SetIdentityDkimEnabled)

getIdentityNotificationAttributesResponseTest :: GetIdentityNotificationAttributesResponse -> TestTree
getIdentityNotificationAttributesResponseTest = resp
    "GetIdentityNotificationAttributesResponse"
    "fixture/GetIdentityNotificationAttributesResponse"
    (Proxy :: Proxy GetIdentityNotificationAttributes)

setIdentityFeedbackForwardingEnabledResponseTest :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
setIdentityFeedbackForwardingEnabledResponseTest = resp
    "SetIdentityFeedbackForwardingEnabledResponse"
    "fixture/SetIdentityFeedbackForwardingEnabledResponse"
    (Proxy :: Proxy SetIdentityFeedbackForwardingEnabled)

getIdentityVerificationAttributesResponseTest :: GetIdentityVerificationAttributesResponse -> TestTree
getIdentityVerificationAttributesResponseTest = resp
    "GetIdentityVerificationAttributesResponse"
    "fixture/GetIdentityVerificationAttributesResponse"
    (Proxy :: Proxy GetIdentityVerificationAttributes)

verifyDomainIdentityResponseTest :: VerifyDomainIdentityResponse -> TestTree
verifyDomainIdentityResponseTest = resp
    "VerifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse"
    (Proxy :: Proxy VerifyDomainIdentity)

verifyDomainDkimResponseTest :: VerifyDomainDkimResponse -> TestTree
verifyDomainDkimResponseTest = resp
    "VerifyDomainDkimResponse"
    "fixture/VerifyDomainDkimResponse"
    (Proxy :: Proxy VerifyDomainDkim)

sendRawEmailResponseTest :: SendRawEmailResponse -> TestTree
sendRawEmailResponseTest = resp
    "SendRawEmailResponse"
    "fixture/SendRawEmailResponse"
    (Proxy :: Proxy SendRawEmail)

getIdentityDkimAttributesResponseTest :: GetIdentityDkimAttributesResponse -> TestTree
getIdentityDkimAttributesResponseTest = resp
    "GetIdentityDkimAttributesResponse"
    "fixture/GetIdentityDkimAttributesResponse"
    (Proxy :: Proxy GetIdentityDkimAttributes)

deleteIdentityResponseTest :: DeleteIdentityResponse -> TestTree
deleteIdentityResponseTest = resp
    "DeleteIdentityResponse"
    "fixture/DeleteIdentityResponse"
    (Proxy :: Proxy DeleteIdentity)

getSendStatisticsResponseTest :: GetSendStatisticsResponse -> TestTree
getSendStatisticsResponseTest = resp
    "GetSendStatisticsResponse"
    "fixture/GetSendStatisticsResponse"
    (Proxy :: Proxy GetSendStatistics)

listIdentitiesResponseTest :: ListIdentitiesResponse -> TestTree
listIdentitiesResponseTest = resp
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse"
    (Proxy :: Proxy ListIdentities)

deleteVerifiedEmailAddressResponseTest :: DeleteVerifiedEmailAddressResponse -> TestTree
deleteVerifiedEmailAddressResponseTest = resp
    "DeleteVerifiedEmailAddressResponse"
    "fixture/DeleteVerifiedEmailAddressResponse"
    (Proxy :: Proxy DeleteVerifiedEmailAddress)

verifyEmailAddressResponseTest :: VerifyEmailAddressResponse -> TestTree
verifyEmailAddressResponseTest = resp
    "VerifyEmailAddressResponse"
    "fixture/VerifyEmailAddressResponse"
    (Proxy :: Proxy VerifyEmailAddress)

verifyEmailIdentityResponseTest :: VerifyEmailIdentityResponse -> TestTree
verifyEmailIdentityResponseTest = resp
    "VerifyEmailIdentityResponse"
    "fixture/VerifyEmailIdentityResponse"
    (Proxy :: Proxy VerifyEmailIdentity)

sendEmailResponseTest :: SendEmailResponse -> TestTree
sendEmailResponseTest = resp
    "SendEmailResponse"
    "fixture/SendEmailResponse"
    (Proxy :: Proxy SendEmail)

listVerifiedEmailAddressesResponseTest :: ListVerifiedEmailAddressesResponse -> TestTree
listVerifiedEmailAddressesResponseTest = resp
    "ListVerifiedEmailAddressesResponse"
    "fixture/ListVerifiedEmailAddressesResponse"
    (Proxy :: Proxy ListVerifiedEmailAddresses)

setIdentityNotificationTopicResponseTest :: SetIdentityNotificationTopicResponse -> TestTree
setIdentityNotificationTopicResponseTest = resp
    "SetIdentityNotificationTopicResponse"
    "fixture/SetIdentityNotificationTopicResponse"
    (Proxy :: Proxy SetIdentityNotificationTopic)
