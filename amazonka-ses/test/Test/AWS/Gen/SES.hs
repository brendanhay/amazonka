{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

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
import Test.AWS.SES.Internal

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
testGetSendQuota = req
    "GetSendQuota"
    "fixture/GetSendQuota"

testSetIdentityDkimEnabled :: SetIdentityDkimEnabled -> TestTree
testSetIdentityDkimEnabled = req
    "SetIdentityDkimEnabled"
    "fixture/SetIdentityDkimEnabled"

testGetIdentityNotificationAttributes :: GetIdentityNotificationAttributes -> TestTree
testGetIdentityNotificationAttributes = req
    "GetIdentityNotificationAttributes"
    "fixture/GetIdentityNotificationAttributes"

testSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabled -> TestTree
testSetIdentityFeedbackForwardingEnabled = req
    "SetIdentityFeedbackForwardingEnabled"
    "fixture/SetIdentityFeedbackForwardingEnabled"

testGetIdentityVerificationAttributes :: GetIdentityVerificationAttributes -> TestTree
testGetIdentityVerificationAttributes = req
    "GetIdentityVerificationAttributes"
    "fixture/GetIdentityVerificationAttributes"

testVerifyDomainIdentity :: VerifyDomainIdentity -> TestTree
testVerifyDomainIdentity = req
    "VerifyDomainIdentity"
    "fixture/VerifyDomainIdentity"

testVerifyDomainDkim :: VerifyDomainDkim -> TestTree
testVerifyDomainDkim = req
    "VerifyDomainDkim"
    "fixture/VerifyDomainDkim"

testSendRawEmail :: SendRawEmail -> TestTree
testSendRawEmail = req
    "SendRawEmail"
    "fixture/SendRawEmail"

testGetIdentityDkimAttributes :: GetIdentityDkimAttributes -> TestTree
testGetIdentityDkimAttributes = req
    "GetIdentityDkimAttributes"
    "fixture/GetIdentityDkimAttributes"

testDeleteIdentity :: DeleteIdentity -> TestTree
testDeleteIdentity = req
    "DeleteIdentity"
    "fixture/DeleteIdentity"

testGetSendStatistics :: GetSendStatistics -> TestTree
testGetSendStatistics = req
    "GetSendStatistics"
    "fixture/GetSendStatistics"

testListIdentities :: ListIdentities -> TestTree
testListIdentities = req
    "ListIdentities"
    "fixture/ListIdentities"

testDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddress -> TestTree
testDeleteVerifiedEmailAddress = req
    "DeleteVerifiedEmailAddress"
    "fixture/DeleteVerifiedEmailAddress"

testVerifyEmailAddress :: VerifyEmailAddress -> TestTree
testVerifyEmailAddress = req
    "VerifyEmailAddress"
    "fixture/VerifyEmailAddress"

testVerifyEmailIdentity :: VerifyEmailIdentity -> TestTree
testVerifyEmailIdentity = req
    "VerifyEmailIdentity"
    "fixture/VerifyEmailIdentity"

testSendEmail :: SendEmail -> TestTree
testSendEmail = req
    "SendEmail"
    "fixture/SendEmail"

testListVerifiedEmailAddresses :: ListVerifiedEmailAddresses -> TestTree
testListVerifiedEmailAddresses = req
    "ListVerifiedEmailAddresses"
    "fixture/ListVerifiedEmailAddresses"

testSetIdentityNotificationTopic :: SetIdentityNotificationTopic -> TestTree
testSetIdentityNotificationTopic = req
    "SetIdentityNotificationTopic"
    "fixture/SetIdentityNotificationTopic"

-- Responses

testGetSendQuotaResponse :: GetSendQuotaResponse -> TestTree
testGetSendQuotaResponse = res
    "GetSendQuotaResponse"
    "fixture/GetSendQuotaResponse"
    (Proxy :: Proxy GetSendQuota)

testSetIdentityDkimEnabledResponse :: SetIdentityDkimEnabledResponse -> TestTree
testSetIdentityDkimEnabledResponse = res
    "SetIdentityDkimEnabledResponse"
    "fixture/SetIdentityDkimEnabledResponse"
    (Proxy :: Proxy SetIdentityDkimEnabled)

testGetIdentityNotificationAttributesResponse :: GetIdentityNotificationAttributesResponse -> TestTree
testGetIdentityNotificationAttributesResponse = res
    "GetIdentityNotificationAttributesResponse"
    "fixture/GetIdentityNotificationAttributesResponse"
    (Proxy :: Proxy GetIdentityNotificationAttributes)

testSetIdentityFeedbackForwardingEnabledResponse :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
testSetIdentityFeedbackForwardingEnabledResponse = res
    "SetIdentityFeedbackForwardingEnabledResponse"
    "fixture/SetIdentityFeedbackForwardingEnabledResponse"
    (Proxy :: Proxy SetIdentityFeedbackForwardingEnabled)

testGetIdentityVerificationAttributesResponse :: GetIdentityVerificationAttributesResponse -> TestTree
testGetIdentityVerificationAttributesResponse = res
    "GetIdentityVerificationAttributesResponse"
    "fixture/GetIdentityVerificationAttributesResponse"
    (Proxy :: Proxy GetIdentityVerificationAttributes)

testVerifyDomainIdentityResponse :: VerifyDomainIdentityResponse -> TestTree
testVerifyDomainIdentityResponse = res
    "VerifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse"
    (Proxy :: Proxy VerifyDomainIdentity)

testVerifyDomainDkimResponse :: VerifyDomainDkimResponse -> TestTree
testVerifyDomainDkimResponse = res
    "VerifyDomainDkimResponse"
    "fixture/VerifyDomainDkimResponse"
    (Proxy :: Proxy VerifyDomainDkim)

testSendRawEmailResponse :: SendRawEmailResponse -> TestTree
testSendRawEmailResponse = res
    "SendRawEmailResponse"
    "fixture/SendRawEmailResponse"
    (Proxy :: Proxy SendRawEmail)

testGetIdentityDkimAttributesResponse :: GetIdentityDkimAttributesResponse -> TestTree
testGetIdentityDkimAttributesResponse = res
    "GetIdentityDkimAttributesResponse"
    "fixture/GetIdentityDkimAttributesResponse"
    (Proxy :: Proxy GetIdentityDkimAttributes)

testDeleteIdentityResponse :: DeleteIdentityResponse -> TestTree
testDeleteIdentityResponse = res
    "DeleteIdentityResponse"
    "fixture/DeleteIdentityResponse"
    (Proxy :: Proxy DeleteIdentity)

testGetSendStatisticsResponse :: GetSendStatisticsResponse -> TestTree
testGetSendStatisticsResponse = res
    "GetSendStatisticsResponse"
    "fixture/GetSendStatisticsResponse"
    (Proxy :: Proxy GetSendStatistics)

testListIdentitiesResponse :: ListIdentitiesResponse -> TestTree
testListIdentitiesResponse = res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse"
    (Proxy :: Proxy ListIdentities)

testDeleteVerifiedEmailAddressResponse :: DeleteVerifiedEmailAddressResponse -> TestTree
testDeleteVerifiedEmailAddressResponse = res
    "DeleteVerifiedEmailAddressResponse"
    "fixture/DeleteVerifiedEmailAddressResponse"
    (Proxy :: Proxy DeleteVerifiedEmailAddress)

testVerifyEmailAddressResponse :: VerifyEmailAddressResponse -> TestTree
testVerifyEmailAddressResponse = res
    "VerifyEmailAddressResponse"
    "fixture/VerifyEmailAddressResponse"
    (Proxy :: Proxy VerifyEmailAddress)

testVerifyEmailIdentityResponse :: VerifyEmailIdentityResponse -> TestTree
testVerifyEmailIdentityResponse = res
    "VerifyEmailIdentityResponse"
    "fixture/VerifyEmailIdentityResponse"
    (Proxy :: Proxy VerifyEmailIdentity)

testSendEmailResponse :: SendEmailResponse -> TestTree
testSendEmailResponse = res
    "SendEmailResponse"
    "fixture/SendEmailResponse"
    (Proxy :: Proxy SendEmail)

testListVerifiedEmailAddressesResponse :: ListVerifiedEmailAddressesResponse -> TestTree
testListVerifiedEmailAddressesResponse = res
    "ListVerifiedEmailAddressesResponse"
    "fixture/ListVerifiedEmailAddressesResponse"
    (Proxy :: Proxy ListVerifiedEmailAddresses)

testSetIdentityNotificationTopicResponse :: SetIdentityNotificationTopicResponse -> TestTree
testSetIdentityNotificationTopicResponse = res
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
