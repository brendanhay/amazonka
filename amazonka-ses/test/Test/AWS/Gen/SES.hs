{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SES
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SES where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.SES

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetSendQuota $
--             getSendQuota
--
--         , testSetIdentityDkimEnabled $
--             setIdentityDkimEnabled
--
--         , testGetIdentityNotificationAttributes $
--             getIdentityNotificationAttributes
--
--         , testSetIdentityFeedbackForwardingEnabled $
--             setIdentityFeedbackForwardingEnabled
--
--         , testGetIdentityVerificationAttributes $
--             getIdentityVerificationAttributes
--
--         , testVerifyDomainIdentity $
--             verifyDomainIdentity
--
--         , testVerifyDomainDkim $
--             verifyDomainDkim
--
--         , testSendRawEmail $
--             sendRawEmail
--
--         , testGetIdentityDkimAttributes $
--             getIdentityDkimAttributes
--
--         , testDeleteIdentity $
--             deleteIdentity
--
--         , testGetSendStatistics $
--             getSendStatistics
--
--         , testListIdentities $
--             listIdentities
--
--         , testDeleteVerifiedEmailAddress $
--             deleteVerifiedEmailAddress
--
--         , testVerifyEmailAddress $
--             verifyEmailAddress
--
--         , testVerifyEmailIdentity $
--             verifyEmailIdentity
--
--         , testSendEmail $
--             sendEmail
--
--         , testListVerifiedEmailAddresses $
--             listVerifiedEmailAddresses
--
--         , testSetIdentityNotificationTopic $
--             setIdentityNotificationTopic
--
--           ]

--     , testGroup "response"
--         [ testGetSendQuotaResponse $
--             getSendQuotaResponse
--
--         , testSetIdentityDkimEnabledResponse $
--             setIdentityDkimEnabledResponse
--
--         , testGetIdentityNotificationAttributesResponse $
--             getIdentityNotificationAttributesResponse
--
--         , testSetIdentityFeedbackForwardingEnabledResponse $
--             setIdentityFeedbackForwardingEnabledResponse
--
--         , testGetIdentityVerificationAttributesResponse $
--             getIdentityVerificationAttributesResponse
--
--         , testVerifyDomainIdentityResponse $
--             verifyDomainIdentityResponse
--
--         , testVerifyDomainDkimResponse $
--             verifyDomainDkimResponse
--
--         , testSendRawEmailResponse $
--             sendRawEmailResponse
--
--         , testGetIdentityDkimAttributesResponse $
--             getIdentityDkimAttributesResponse
--
--         , testDeleteIdentityResponse $
--             deleteIdentityResponse
--
--         , testGetSendStatisticsResponse $
--             getSendStatisticsResponse
--
--         , testListIdentitiesResponse $
--             listIdentitiesResponse
--
--         , testDeleteVerifiedEmailAddressResponse $
--             deleteVerifiedEmailAddressResponse
--
--         , testVerifyEmailAddressResponse $
--             verifyEmailAddressResponse
--
--         , testVerifyEmailIdentityResponse $
--             verifyEmailIdentityResponse
--
--         , testSendEmailResponse $
--             sendEmailResponse
--
--         , testListVerifiedEmailAddressesResponse $
--             listVerifiedEmailAddressesResponse
--
--         , testSetIdentityNotificationTopicResponse $
--             setIdentityNotificationTopicResponse
--
--           ]
--     ]

-- Requests

testGetSendQuota :: GetSendQuota -> TestTree
testGetSendQuota = undefined

testSetIdentityDkimEnabled :: SetIdentityDkimEnabled -> TestTree
testSetIdentityDkimEnabled = undefined

testGetIdentityNotificationAttributes :: GetIdentityNotificationAttributes -> TestTree
testGetIdentityNotificationAttributes = undefined

testSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabled -> TestTree
testSetIdentityFeedbackForwardingEnabled = undefined

testGetIdentityVerificationAttributes :: GetIdentityVerificationAttributes -> TestTree
testGetIdentityVerificationAttributes = undefined

testVerifyDomainIdentity :: VerifyDomainIdentity -> TestTree
testVerifyDomainIdentity = undefined

testVerifyDomainDkim :: VerifyDomainDkim -> TestTree
testVerifyDomainDkim = undefined

testSendRawEmail :: SendRawEmail -> TestTree
testSendRawEmail = undefined

testGetIdentityDkimAttributes :: GetIdentityDkimAttributes -> TestTree
testGetIdentityDkimAttributes = undefined

testDeleteIdentity :: DeleteIdentity -> TestTree
testDeleteIdentity = undefined

testGetSendStatistics :: GetSendStatistics -> TestTree
testGetSendStatistics = undefined

testListIdentities :: ListIdentities -> TestTree
testListIdentities = undefined

testDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddress -> TestTree
testDeleteVerifiedEmailAddress = undefined

testVerifyEmailAddress :: VerifyEmailAddress -> TestTree
testVerifyEmailAddress = undefined

testVerifyEmailIdentity :: VerifyEmailIdentity -> TestTree
testVerifyEmailIdentity = undefined

testSendEmail :: SendEmail -> TestTree
testSendEmail = undefined

testListVerifiedEmailAddresses :: ListVerifiedEmailAddresses -> TestTree
testListVerifiedEmailAddresses = undefined

testSetIdentityNotificationTopic :: SetIdentityNotificationTopic -> TestTree
testSetIdentityNotificationTopic = undefined

-- Responses

testGetSendQuotaResponse :: GetSendQuotaResponse -> TestTree
testGetSendQuotaResponse = resp
    "GetSendQuotaResponse"
    "fixture/GetSendQuotaResponse"
    (Proxy :: Proxy GetSendQuota)

testSetIdentityDkimEnabledResponse :: SetIdentityDkimEnabledResponse -> TestTree
testSetIdentityDkimEnabledResponse = resp
    "SetIdentityDkimEnabledResponse"
    "fixture/SetIdentityDkimEnabledResponse"
    (Proxy :: Proxy SetIdentityDkimEnabled)

testGetIdentityNotificationAttributesResponse :: GetIdentityNotificationAttributesResponse -> TestTree
testGetIdentityNotificationAttributesResponse = resp
    "GetIdentityNotificationAttributesResponse"
    "fixture/GetIdentityNotificationAttributesResponse"
    (Proxy :: Proxy GetIdentityNotificationAttributes)

testSetIdentityFeedbackForwardingEnabledResponse :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
testSetIdentityFeedbackForwardingEnabledResponse = resp
    "SetIdentityFeedbackForwardingEnabledResponse"
    "fixture/SetIdentityFeedbackForwardingEnabledResponse"
    (Proxy :: Proxy SetIdentityFeedbackForwardingEnabled)

testGetIdentityVerificationAttributesResponse :: GetIdentityVerificationAttributesResponse -> TestTree
testGetIdentityVerificationAttributesResponse = resp
    "GetIdentityVerificationAttributesResponse"
    "fixture/GetIdentityVerificationAttributesResponse"
    (Proxy :: Proxy GetIdentityVerificationAttributes)

testVerifyDomainIdentityResponse :: VerifyDomainIdentityResponse -> TestTree
testVerifyDomainIdentityResponse = resp
    "VerifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse"
    (Proxy :: Proxy VerifyDomainIdentity)

testVerifyDomainDkimResponse :: VerifyDomainDkimResponse -> TestTree
testVerifyDomainDkimResponse = resp
    "VerifyDomainDkimResponse"
    "fixture/VerifyDomainDkimResponse"
    (Proxy :: Proxy VerifyDomainDkim)

testSendRawEmailResponse :: SendRawEmailResponse -> TestTree
testSendRawEmailResponse = resp
    "SendRawEmailResponse"
    "fixture/SendRawEmailResponse"
    (Proxy :: Proxy SendRawEmail)

testGetIdentityDkimAttributesResponse :: GetIdentityDkimAttributesResponse -> TestTree
testGetIdentityDkimAttributesResponse = resp
    "GetIdentityDkimAttributesResponse"
    "fixture/GetIdentityDkimAttributesResponse"
    (Proxy :: Proxy GetIdentityDkimAttributes)

testDeleteIdentityResponse :: DeleteIdentityResponse -> TestTree
testDeleteIdentityResponse = resp
    "DeleteIdentityResponse"
    "fixture/DeleteIdentityResponse"
    (Proxy :: Proxy DeleteIdentity)

testGetSendStatisticsResponse :: GetSendStatisticsResponse -> TestTree
testGetSendStatisticsResponse = resp
    "GetSendStatisticsResponse"
    "fixture/GetSendStatisticsResponse"
    (Proxy :: Proxy GetSendStatistics)

testListIdentitiesResponse :: ListIdentitiesResponse -> TestTree
testListIdentitiesResponse = resp
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse"
    (Proxy :: Proxy ListIdentities)

testDeleteVerifiedEmailAddressResponse :: DeleteVerifiedEmailAddressResponse -> TestTree
testDeleteVerifiedEmailAddressResponse = resp
    "DeleteVerifiedEmailAddressResponse"
    "fixture/DeleteVerifiedEmailAddressResponse"
    (Proxy :: Proxy DeleteVerifiedEmailAddress)

testVerifyEmailAddressResponse :: VerifyEmailAddressResponse -> TestTree
testVerifyEmailAddressResponse = resp
    "VerifyEmailAddressResponse"
    "fixture/VerifyEmailAddressResponse"
    (Proxy :: Proxy VerifyEmailAddress)

testVerifyEmailIdentityResponse :: VerifyEmailIdentityResponse -> TestTree
testVerifyEmailIdentityResponse = resp
    "VerifyEmailIdentityResponse"
    "fixture/VerifyEmailIdentityResponse"
    (Proxy :: Proxy VerifyEmailIdentity)

testSendEmailResponse :: SendEmailResponse -> TestTree
testSendEmailResponse = resp
    "SendEmailResponse"
    "fixture/SendEmailResponse"
    (Proxy :: Proxy SendEmail)

testListVerifiedEmailAddressesResponse :: ListVerifiedEmailAddressesResponse -> TestTree
testListVerifiedEmailAddressesResponse = resp
    "ListVerifiedEmailAddressesResponse"
    "fixture/ListVerifiedEmailAddressesResponse"
    (Proxy :: Proxy ListVerifiedEmailAddresses)

testSetIdentityNotificationTopicResponse :: SetIdentityNotificationTopicResponse -> TestTree
testSetIdentityNotificationTopicResponse = resp
    "SetIdentityNotificationTopicResponse"
    "fixture/SetIdentityNotificationTopicResponse"
    (Proxy :: Proxy SetIdentityNotificationTopic)

instance Out Body
instance Out Content
instance Out DeleteIdentity
instance Out DeleteIdentityResponse
instance Out DeleteVerifiedEmailAddress
instance Out DeleteVerifiedEmailAddressResponse
instance Out Destination
instance Out GetIdentityDkimAttributes
instance Out GetIdentityDkimAttributesResponse
instance Out GetIdentityNotificationAttributes
instance Out GetIdentityNotificationAttributesResponse
instance Out GetIdentityVerificationAttributes
instance Out GetIdentityVerificationAttributesResponse
instance Out GetSendQuota
instance Out GetSendQuotaResponse
instance Out GetSendStatistics
instance Out GetSendStatisticsResponse
instance Out IdentityDkimAttributes
instance Out IdentityNotificationAttributes
instance Out IdentityType
instance Out IdentityVerificationAttributes
instance Out ListIdentities
instance Out ListIdentitiesResponse
instance Out ListVerifiedEmailAddresses
instance Out ListVerifiedEmailAddressesResponse
instance Out Message
instance Out NotificationType
instance Out RawMessage
instance Out SendDataPoint
instance Out SendEmail
instance Out SendEmailResponse
instance Out SendRawEmail
instance Out SendRawEmailResponse
instance Out SetIdentityDkimEnabled
instance Out SetIdentityDkimEnabledResponse
instance Out SetIdentityFeedbackForwardingEnabled
instance Out SetIdentityFeedbackForwardingEnabledResponse
instance Out SetIdentityNotificationTopic
instance Out SetIdentityNotificationTopicResponse
instance Out VerificationStatus
instance Out VerifyDomainDkim
instance Out VerifyDomainDkimResponse
instance Out VerifyDomainIdentity
instance Out VerifyDomainIdentityResponse
instance Out VerifyEmailAddress
instance Out VerifyEmailAddressResponse
instance Out VerifyEmailIdentity
instance Out VerifyEmailIdentityResponse
