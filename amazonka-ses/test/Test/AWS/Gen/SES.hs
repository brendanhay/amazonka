{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SES
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
--         , testDeleteIdentityPolicy $
--             deleteIdentityPolicy
--
--         , testPutIdentityPolicy $
--             putIdentityPolicy
--
--         , testSetIdentityDkimEnabled $
--             setIdentityDkimEnabled
--
--         , testGetIdentityNotificationAttributes $
--             getIdentityNotificationAttributes
--
--         , testListIdentityPolicies $
--             listIdentityPolicies
--
--         , testSetIdentityFeedbackForwardingEnabled $
--             setIdentityFeedbackForwardingEnabled
--
--         , testGetIdentityVerificationAttributes $
--             getIdentityVerificationAttributes
--
--         , testGetIdentityPolicies $
--             getIdentityPolicies
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
--         , testDeleteIdentityPolicyResponse $
--             deleteIdentityPolicyResponse
--
--         , testPutIdentityPolicyResponse $
--             putIdentityPolicyResponse
--
--         , testSetIdentityDkimEnabledResponse $
--             setIdentityDkimEnabledResponse
--
--         , testGetIdentityNotificationAttributesResponse $
--             getIdentityNotificationAttributesResponse
--
--         , testListIdentityPoliciesResponse $
--             listIdentityPoliciesResponse
--
--         , testSetIdentityFeedbackForwardingEnabledResponse $
--             setIdentityFeedbackForwardingEnabledResponse
--
--         , testGetIdentityVerificationAttributesResponse $
--             getIdentityVerificationAttributesResponse
--
--         , testGetIdentityPoliciesResponse $
--             getIdentityPoliciesResponse
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

testDeleteIdentityPolicy :: DeleteIdentityPolicy -> TestTree
testDeleteIdentityPolicy = req
    "DeleteIdentityPolicy"
    "fixture/DeleteIdentityPolicy"

testPutIdentityPolicy :: PutIdentityPolicy -> TestTree
testPutIdentityPolicy = req
    "PutIdentityPolicy"
    "fixture/PutIdentityPolicy"

testSetIdentityDkimEnabled :: SetIdentityDkimEnabled -> TestTree
testSetIdentityDkimEnabled = req
    "SetIdentityDkimEnabled"
    "fixture/SetIdentityDkimEnabled"

testGetIdentityNotificationAttributes :: GetIdentityNotificationAttributes -> TestTree
testGetIdentityNotificationAttributes = req
    "GetIdentityNotificationAttributes"
    "fixture/GetIdentityNotificationAttributes"

testListIdentityPolicies :: ListIdentityPolicies -> TestTree
testListIdentityPolicies = req
    "ListIdentityPolicies"
    "fixture/ListIdentityPolicies"

testSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabled -> TestTree
testSetIdentityFeedbackForwardingEnabled = req
    "SetIdentityFeedbackForwardingEnabled"
    "fixture/SetIdentityFeedbackForwardingEnabled"

testGetIdentityVerificationAttributes :: GetIdentityVerificationAttributes -> TestTree
testGetIdentityVerificationAttributes = req
    "GetIdentityVerificationAttributes"
    "fixture/GetIdentityVerificationAttributes"

testGetIdentityPolicies :: GetIdentityPolicies -> TestTree
testGetIdentityPolicies = req
    "GetIdentityPolicies"
    "fixture/GetIdentityPolicies"

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
    sES
    (Proxy :: Proxy GetSendQuota)

testDeleteIdentityPolicyResponse :: DeleteIdentityPolicyResponse -> TestTree
testDeleteIdentityPolicyResponse = res
    "DeleteIdentityPolicyResponse"
    "fixture/DeleteIdentityPolicyResponse"
    sES
    (Proxy :: Proxy DeleteIdentityPolicy)

testPutIdentityPolicyResponse :: PutIdentityPolicyResponse -> TestTree
testPutIdentityPolicyResponse = res
    "PutIdentityPolicyResponse"
    "fixture/PutIdentityPolicyResponse"
    sES
    (Proxy :: Proxy PutIdentityPolicy)

testSetIdentityDkimEnabledResponse :: SetIdentityDkimEnabledResponse -> TestTree
testSetIdentityDkimEnabledResponse = res
    "SetIdentityDkimEnabledResponse"
    "fixture/SetIdentityDkimEnabledResponse"
    sES
    (Proxy :: Proxy SetIdentityDkimEnabled)

testGetIdentityNotificationAttributesResponse :: GetIdentityNotificationAttributesResponse -> TestTree
testGetIdentityNotificationAttributesResponse = res
    "GetIdentityNotificationAttributesResponse"
    "fixture/GetIdentityNotificationAttributesResponse"
    sES
    (Proxy :: Proxy GetIdentityNotificationAttributes)

testListIdentityPoliciesResponse :: ListIdentityPoliciesResponse -> TestTree
testListIdentityPoliciesResponse = res
    "ListIdentityPoliciesResponse"
    "fixture/ListIdentityPoliciesResponse"
    sES
    (Proxy :: Proxy ListIdentityPolicies)

testSetIdentityFeedbackForwardingEnabledResponse :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
testSetIdentityFeedbackForwardingEnabledResponse = res
    "SetIdentityFeedbackForwardingEnabledResponse"
    "fixture/SetIdentityFeedbackForwardingEnabledResponse"
    sES
    (Proxy :: Proxy SetIdentityFeedbackForwardingEnabled)

testGetIdentityVerificationAttributesResponse :: GetIdentityVerificationAttributesResponse -> TestTree
testGetIdentityVerificationAttributesResponse = res
    "GetIdentityVerificationAttributesResponse"
    "fixture/GetIdentityVerificationAttributesResponse"
    sES
    (Proxy :: Proxy GetIdentityVerificationAttributes)

testGetIdentityPoliciesResponse :: GetIdentityPoliciesResponse -> TestTree
testGetIdentityPoliciesResponse = res
    "GetIdentityPoliciesResponse"
    "fixture/GetIdentityPoliciesResponse"
    sES
    (Proxy :: Proxy GetIdentityPolicies)

testVerifyDomainIdentityResponse :: VerifyDomainIdentityResponse -> TestTree
testVerifyDomainIdentityResponse = res
    "VerifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse"
    sES
    (Proxy :: Proxy VerifyDomainIdentity)

testVerifyDomainDkimResponse :: VerifyDomainDkimResponse -> TestTree
testVerifyDomainDkimResponse = res
    "VerifyDomainDkimResponse"
    "fixture/VerifyDomainDkimResponse"
    sES
    (Proxy :: Proxy VerifyDomainDkim)

testSendRawEmailResponse :: SendRawEmailResponse -> TestTree
testSendRawEmailResponse = res
    "SendRawEmailResponse"
    "fixture/SendRawEmailResponse"
    sES
    (Proxy :: Proxy SendRawEmail)

testGetIdentityDkimAttributesResponse :: GetIdentityDkimAttributesResponse -> TestTree
testGetIdentityDkimAttributesResponse = res
    "GetIdentityDkimAttributesResponse"
    "fixture/GetIdentityDkimAttributesResponse"
    sES
    (Proxy :: Proxy GetIdentityDkimAttributes)

testDeleteIdentityResponse :: DeleteIdentityResponse -> TestTree
testDeleteIdentityResponse = res
    "DeleteIdentityResponse"
    "fixture/DeleteIdentityResponse"
    sES
    (Proxy :: Proxy DeleteIdentity)

testGetSendStatisticsResponse :: GetSendStatisticsResponse -> TestTree
testGetSendStatisticsResponse = res
    "GetSendStatisticsResponse"
    "fixture/GetSendStatisticsResponse"
    sES
    (Proxy :: Proxy GetSendStatistics)

testListIdentitiesResponse :: ListIdentitiesResponse -> TestTree
testListIdentitiesResponse = res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse"
    sES
    (Proxy :: Proxy ListIdentities)

testDeleteVerifiedEmailAddressResponse :: DeleteVerifiedEmailAddressResponse -> TestTree
testDeleteVerifiedEmailAddressResponse = res
    "DeleteVerifiedEmailAddressResponse"
    "fixture/DeleteVerifiedEmailAddressResponse"
    sES
    (Proxy :: Proxy DeleteVerifiedEmailAddress)

testVerifyEmailAddressResponse :: VerifyEmailAddressResponse -> TestTree
testVerifyEmailAddressResponse = res
    "VerifyEmailAddressResponse"
    "fixture/VerifyEmailAddressResponse"
    sES
    (Proxy :: Proxy VerifyEmailAddress)

testVerifyEmailIdentityResponse :: VerifyEmailIdentityResponse -> TestTree
testVerifyEmailIdentityResponse = res
    "VerifyEmailIdentityResponse"
    "fixture/VerifyEmailIdentityResponse"
    sES
    (Proxy :: Proxy VerifyEmailIdentity)

testSendEmailResponse :: SendEmailResponse -> TestTree
testSendEmailResponse = res
    "SendEmailResponse"
    "fixture/SendEmailResponse"
    sES
    (Proxy :: Proxy SendEmail)

testListVerifiedEmailAddressesResponse :: ListVerifiedEmailAddressesResponse -> TestTree
testListVerifiedEmailAddressesResponse = res
    "ListVerifiedEmailAddressesResponse"
    "fixture/ListVerifiedEmailAddressesResponse"
    sES
    (Proxy :: Proxy ListVerifiedEmailAddresses)

testSetIdentityNotificationTopicResponse :: SetIdentityNotificationTopicResponse -> TestTree
testSetIdentityNotificationTopicResponse = res
    "SetIdentityNotificationTopicResponse"
    "fixture/SetIdentityNotificationTopicResponse"
    sES
    (Proxy :: Proxy SetIdentityNotificationTopic)
