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

import           Data.Proxy
import           Network.AWS.SES
import           Test.AWS.Fixture
import           Test.Tasty

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
    "GetSendQuota"
    "fixture/SES/GetSendQuotaResponse"
    (Proxy :: Proxy GetSendQuota)

setIdentityDkimEnabledResponseTest :: SetIdentityDkimEnabledResponse -> TestTree
setIdentityDkimEnabledResponseTest = resp
    "SetIdentityDkimEnabled"
    "fixture/SES/SetIdentityDkimEnabledResponse"
    (Proxy :: Proxy SetIdentityDkimEnabled)

getIdentityNotificationAttributesResponseTest :: GetIdentityNotificationAttributesResponse -> TestTree
getIdentityNotificationAttributesResponseTest = resp
    "GetIdentityNotificationAttributes"
    "fixture/SES/GetIdentityNotificationAttributesResponse"
    (Proxy :: Proxy GetIdentityNotificationAttributes)

setIdentityFeedbackForwardingEnabledResponseTest :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
setIdentityFeedbackForwardingEnabledResponseTest = resp
    "SetIdentityFeedbackForwardingEnabled"
    "fixture/SES/SetIdentityFeedbackForwardingEnabledResponse"
    (Proxy :: Proxy SetIdentityFeedbackForwardingEnabled)

getIdentityVerificationAttributesResponseTest :: GetIdentityVerificationAttributesResponse -> TestTree
getIdentityVerificationAttributesResponseTest = resp
    "GetIdentityVerificationAttributes"
    "fixture/SES/GetIdentityVerificationAttributesResponse"
    (Proxy :: Proxy GetIdentityVerificationAttributes)

verifyDomainIdentityResponseTest :: VerifyDomainIdentityResponse -> TestTree
verifyDomainIdentityResponseTest = resp
    "VerifyDomainIdentity"
    "fixture/SES/VerifyDomainIdentityResponse"
    (Proxy :: Proxy VerifyDomainIdentity)

verifyDomainDkimResponseTest :: VerifyDomainDkimResponse -> TestTree
verifyDomainDkimResponseTest = resp
    "VerifyDomainDkim"
    "fixture/SES/VerifyDomainDkimResponse"
    (Proxy :: Proxy VerifyDomainDkim)

sendRawEmailResponseTest :: SendRawEmailResponse -> TestTree
sendRawEmailResponseTest = resp
    "SendRawEmail"
    "fixture/SES/SendRawEmailResponse"
    (Proxy :: Proxy SendRawEmail)

getIdentityDkimAttributesResponseTest :: GetIdentityDkimAttributesResponse -> TestTree
getIdentityDkimAttributesResponseTest = resp
    "GetIdentityDkimAttributes"
    "fixture/SES/GetIdentityDkimAttributesResponse"
    (Proxy :: Proxy GetIdentityDkimAttributes)

deleteIdentityResponseTest :: DeleteIdentityResponse -> TestTree
deleteIdentityResponseTest = resp
    "DeleteIdentity"
    "fixture/SES/DeleteIdentityResponse"
    (Proxy :: Proxy DeleteIdentity)

getSendStatisticsResponseTest :: GetSendStatisticsResponse -> TestTree
getSendStatisticsResponseTest = resp
    "GetSendStatistics"
    "fixture/SES/GetSendStatisticsResponse"
    (Proxy :: Proxy GetSendStatistics)

listIdentitiesResponseTest :: ListIdentitiesResponse -> TestTree
listIdentitiesResponseTest = resp
    "ListIdentities"
    "fixture/SES/ListIdentitiesResponse"
    (Proxy :: Proxy ListIdentities)

deleteVerifiedEmailAddressResponseTest :: DeleteVerifiedEmailAddressResponse -> TestTree
deleteVerifiedEmailAddressResponseTest = resp
    "DeleteVerifiedEmailAddress"
    "fixture/SES/DeleteVerifiedEmailAddressResponse"
    (Proxy :: Proxy DeleteVerifiedEmailAddress)

verifyEmailAddressResponseTest :: VerifyEmailAddressResponse -> TestTree
verifyEmailAddressResponseTest = resp
    "VerifyEmailAddress"
    "fixture/SES/VerifyEmailAddressResponse"
    (Proxy :: Proxy VerifyEmailAddress)

verifyEmailIdentityResponseTest :: VerifyEmailIdentityResponse -> TestTree
verifyEmailIdentityResponseTest = resp
    "VerifyEmailIdentity"
    "fixture/SES/VerifyEmailIdentityResponse"
    (Proxy :: Proxy VerifyEmailIdentity)

sendEmailResponseTest :: SendEmailResponse -> TestTree
sendEmailResponseTest = resp
    "SendEmail"
    "fixture/SES/SendEmailResponse"
    (Proxy :: Proxy SendEmail)

listVerifiedEmailAddressesResponseTest :: ListVerifiedEmailAddressesResponse -> TestTree
listVerifiedEmailAddressesResponseTest = resp
    "ListVerifiedEmailAddresses"
    "fixture/SES/ListVerifiedEmailAddressesResponse"
    (Proxy :: Proxy ListVerifiedEmailAddresses)

setIdentityNotificationTopicResponseTest :: SetIdentityNotificationTopicResponse -> TestTree
setIdentityNotificationTopicResponseTest = resp
    "SetIdentityNotificationTopic"
    "fixture/SES/SetIdentityNotificationTopicResponse"
    (Proxy :: Proxy SetIdentityNotificationTopic)
