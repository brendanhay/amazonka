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
--         , testPutIdentityPolicy $
--             putIdentityPolicy
--
--         , testDeleteIdentityPolicy $
--             deleteIdentityPolicy
--
--         , testGetIdentityNotificationAttributes $
--             getIdentityNotificationAttributes
--
--         , testListIdentityPolicies $
--             listIdentityPolicies
--
--         , testSetIdentityDkimEnabled $
--             setIdentityDkimEnabled
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
--         , testGetIdentityDkimAttributes $
--             getIdentityDkimAttributes
--
--         , testVerifyDomainDkim $
--             verifyDomainDkim
--
--         , testSendRawEmail $
--             sendRawEmail
--
--         , testGetSendStatistics $
--             getSendStatistics
--
--         , testDeleteIdentity $
--             deleteIdentity
--
--         , testListIdentities $
--             listIdentities
--
--         , testVerifyEmailIdentity $
--             verifyEmailIdentity
--
--         , testVerifyEmailAddress $
--             verifyEmailAddress
--
--         , testDeleteVerifiedEmailAddress $
--             deleteVerifiedEmailAddress
--
--         , testListVerifiedEmailAddresses $
--             listVerifiedEmailAddresses
--
--         , testSetIdentityNotificationTopic $
--             setIdentityNotificationTopic
--
--         , testSendEmail $
--             sendEmail
--
--           ]

--     , testGroup "response"
--         [ testGetSendQuotaResponse $
--             getSendQuotaResponse
--
--         , testPutIdentityPolicyResponse $
--             putIdentityPolicyResponse
--
--         , testDeleteIdentityPolicyResponse $
--             deleteIdentityPolicyResponse
--
--         , testGetIdentityNotificationAttributesResponse $
--             getIdentityNotificationAttributesResponse
--
--         , testListIdentityPoliciesResponse $
--             listIdentityPoliciesResponse
--
--         , testSetIdentityDkimEnabledResponse $
--             setIdentityDkimEnabledResponse
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
--         , testGetIdentityDkimAttributesResponse $
--             getIdentityDkimAttributesResponse
--
--         , testVerifyDomainDkimResponse $
--             verifyDomainDkimResponse
--
--         , testSendRawEmailResponse $
--             sendRawEmailResponse
--
--         , testGetSendStatisticsResponse $
--             getSendStatisticsResponse
--
--         , testDeleteIdentityResponse $
--             deleteIdentityResponse
--
--         , testListIdentitiesResponse $
--             listIdentitiesResponse
--
--         , testVerifyEmailIdentityResponse $
--             verifyEmailIdentityResponse
--
--         , testVerifyEmailAddressResponse $
--             verifyEmailAddressResponse
--
--         , testDeleteVerifiedEmailAddressResponse $
--             deleteVerifiedEmailAddressResponse
--
--         , testListVerifiedEmailAddressesResponse $
--             listVerifiedEmailAddressesResponse
--
--         , testSetIdentityNotificationTopicResponse $
--             setIdentityNotificationTopicResponse
--
--         , testSendEmailResponse $
--             sendEmailResponse
--
--           ]
--     ]

-- Requests

testGetSendQuota :: GetSendQuota -> TestTree
testGetSendQuota = req
    "GetSendQuota"
    "fixture/GetSendQuota.yaml"

testPutIdentityPolicy :: PutIdentityPolicy -> TestTree
testPutIdentityPolicy = req
    "PutIdentityPolicy"
    "fixture/PutIdentityPolicy.yaml"

testDeleteIdentityPolicy :: DeleteIdentityPolicy -> TestTree
testDeleteIdentityPolicy = req
    "DeleteIdentityPolicy"
    "fixture/DeleteIdentityPolicy.yaml"

testGetIdentityNotificationAttributes :: GetIdentityNotificationAttributes -> TestTree
testGetIdentityNotificationAttributes = req
    "GetIdentityNotificationAttributes"
    "fixture/GetIdentityNotificationAttributes.yaml"

testListIdentityPolicies :: ListIdentityPolicies -> TestTree
testListIdentityPolicies = req
    "ListIdentityPolicies"
    "fixture/ListIdentityPolicies.yaml"

testSetIdentityDkimEnabled :: SetIdentityDkimEnabled -> TestTree
testSetIdentityDkimEnabled = req
    "SetIdentityDkimEnabled"
    "fixture/SetIdentityDkimEnabled.yaml"

testSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabled -> TestTree
testSetIdentityFeedbackForwardingEnabled = req
    "SetIdentityFeedbackForwardingEnabled"
    "fixture/SetIdentityFeedbackForwardingEnabled.yaml"

testGetIdentityVerificationAttributes :: GetIdentityVerificationAttributes -> TestTree
testGetIdentityVerificationAttributes = req
    "GetIdentityVerificationAttributes"
    "fixture/GetIdentityVerificationAttributes.yaml"

testGetIdentityPolicies :: GetIdentityPolicies -> TestTree
testGetIdentityPolicies = req
    "GetIdentityPolicies"
    "fixture/GetIdentityPolicies.yaml"

testVerifyDomainIdentity :: VerifyDomainIdentity -> TestTree
testVerifyDomainIdentity = req
    "VerifyDomainIdentity"
    "fixture/VerifyDomainIdentity.yaml"

testGetIdentityDkimAttributes :: GetIdentityDkimAttributes -> TestTree
testGetIdentityDkimAttributes = req
    "GetIdentityDkimAttributes"
    "fixture/GetIdentityDkimAttributes.yaml"

testVerifyDomainDkim :: VerifyDomainDkim -> TestTree
testVerifyDomainDkim = req
    "VerifyDomainDkim"
    "fixture/VerifyDomainDkim.yaml"

testSendRawEmail :: SendRawEmail -> TestTree
testSendRawEmail = req
    "SendRawEmail"
    "fixture/SendRawEmail.yaml"

testGetSendStatistics :: GetSendStatistics -> TestTree
testGetSendStatistics = req
    "GetSendStatistics"
    "fixture/GetSendStatistics.yaml"

testDeleteIdentity :: DeleteIdentity -> TestTree
testDeleteIdentity = req
    "DeleteIdentity"
    "fixture/DeleteIdentity.yaml"

testListIdentities :: ListIdentities -> TestTree
testListIdentities = req
    "ListIdentities"
    "fixture/ListIdentities.yaml"

testVerifyEmailIdentity :: VerifyEmailIdentity -> TestTree
testVerifyEmailIdentity = req
    "VerifyEmailIdentity"
    "fixture/VerifyEmailIdentity.yaml"

testVerifyEmailAddress :: VerifyEmailAddress -> TestTree
testVerifyEmailAddress = req
    "VerifyEmailAddress"
    "fixture/VerifyEmailAddress.yaml"

testDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddress -> TestTree
testDeleteVerifiedEmailAddress = req
    "DeleteVerifiedEmailAddress"
    "fixture/DeleteVerifiedEmailAddress.yaml"

testListVerifiedEmailAddresses :: ListVerifiedEmailAddresses -> TestTree
testListVerifiedEmailAddresses = req
    "ListVerifiedEmailAddresses"
    "fixture/ListVerifiedEmailAddresses.yaml"

testSetIdentityNotificationTopic :: SetIdentityNotificationTopic -> TestTree
testSetIdentityNotificationTopic = req
    "SetIdentityNotificationTopic"
    "fixture/SetIdentityNotificationTopic.yaml"

testSendEmail :: SendEmail -> TestTree
testSendEmail = req
    "SendEmail"
    "fixture/SendEmail.yaml"

-- Responses

testGetSendQuotaResponse :: GetSendQuotaResponse -> TestTree
testGetSendQuotaResponse = res
    "GetSendQuotaResponse"
    "fixture/GetSendQuotaResponse.proto"
    sES
    (Proxy :: Proxy GetSendQuota)

testPutIdentityPolicyResponse :: PutIdentityPolicyResponse -> TestTree
testPutIdentityPolicyResponse = res
    "PutIdentityPolicyResponse"
    "fixture/PutIdentityPolicyResponse.proto"
    sES
    (Proxy :: Proxy PutIdentityPolicy)

testDeleteIdentityPolicyResponse :: DeleteIdentityPolicyResponse -> TestTree
testDeleteIdentityPolicyResponse = res
    "DeleteIdentityPolicyResponse"
    "fixture/DeleteIdentityPolicyResponse.proto"
    sES
    (Proxy :: Proxy DeleteIdentityPolicy)

testGetIdentityNotificationAttributesResponse :: GetIdentityNotificationAttributesResponse -> TestTree
testGetIdentityNotificationAttributesResponse = res
    "GetIdentityNotificationAttributesResponse"
    "fixture/GetIdentityNotificationAttributesResponse.proto"
    sES
    (Proxy :: Proxy GetIdentityNotificationAttributes)

testListIdentityPoliciesResponse :: ListIdentityPoliciesResponse -> TestTree
testListIdentityPoliciesResponse = res
    "ListIdentityPoliciesResponse"
    "fixture/ListIdentityPoliciesResponse.proto"
    sES
    (Proxy :: Proxy ListIdentityPolicies)

testSetIdentityDkimEnabledResponse :: SetIdentityDkimEnabledResponse -> TestTree
testSetIdentityDkimEnabledResponse = res
    "SetIdentityDkimEnabledResponse"
    "fixture/SetIdentityDkimEnabledResponse.proto"
    sES
    (Proxy :: Proxy SetIdentityDkimEnabled)

testSetIdentityFeedbackForwardingEnabledResponse :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
testSetIdentityFeedbackForwardingEnabledResponse = res
    "SetIdentityFeedbackForwardingEnabledResponse"
    "fixture/SetIdentityFeedbackForwardingEnabledResponse.proto"
    sES
    (Proxy :: Proxy SetIdentityFeedbackForwardingEnabled)

testGetIdentityVerificationAttributesResponse :: GetIdentityVerificationAttributesResponse -> TestTree
testGetIdentityVerificationAttributesResponse = res
    "GetIdentityVerificationAttributesResponse"
    "fixture/GetIdentityVerificationAttributesResponse.proto"
    sES
    (Proxy :: Proxy GetIdentityVerificationAttributes)

testGetIdentityPoliciesResponse :: GetIdentityPoliciesResponse -> TestTree
testGetIdentityPoliciesResponse = res
    "GetIdentityPoliciesResponse"
    "fixture/GetIdentityPoliciesResponse.proto"
    sES
    (Proxy :: Proxy GetIdentityPolicies)

testVerifyDomainIdentityResponse :: VerifyDomainIdentityResponse -> TestTree
testVerifyDomainIdentityResponse = res
    "VerifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse.proto"
    sES
    (Proxy :: Proxy VerifyDomainIdentity)

testGetIdentityDkimAttributesResponse :: GetIdentityDkimAttributesResponse -> TestTree
testGetIdentityDkimAttributesResponse = res
    "GetIdentityDkimAttributesResponse"
    "fixture/GetIdentityDkimAttributesResponse.proto"
    sES
    (Proxy :: Proxy GetIdentityDkimAttributes)

testVerifyDomainDkimResponse :: VerifyDomainDkimResponse -> TestTree
testVerifyDomainDkimResponse = res
    "VerifyDomainDkimResponse"
    "fixture/VerifyDomainDkimResponse.proto"
    sES
    (Proxy :: Proxy VerifyDomainDkim)

testSendRawEmailResponse :: SendRawEmailResponse -> TestTree
testSendRawEmailResponse = res
    "SendRawEmailResponse"
    "fixture/SendRawEmailResponse.proto"
    sES
    (Proxy :: Proxy SendRawEmail)

testGetSendStatisticsResponse :: GetSendStatisticsResponse -> TestTree
testGetSendStatisticsResponse = res
    "GetSendStatisticsResponse"
    "fixture/GetSendStatisticsResponse.proto"
    sES
    (Proxy :: Proxy GetSendStatistics)

testDeleteIdentityResponse :: DeleteIdentityResponse -> TestTree
testDeleteIdentityResponse = res
    "DeleteIdentityResponse"
    "fixture/DeleteIdentityResponse.proto"
    sES
    (Proxy :: Proxy DeleteIdentity)

testListIdentitiesResponse :: ListIdentitiesResponse -> TestTree
testListIdentitiesResponse = res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse.proto"
    sES
    (Proxy :: Proxy ListIdentities)

testVerifyEmailIdentityResponse :: VerifyEmailIdentityResponse -> TestTree
testVerifyEmailIdentityResponse = res
    "VerifyEmailIdentityResponse"
    "fixture/VerifyEmailIdentityResponse.proto"
    sES
    (Proxy :: Proxy VerifyEmailIdentity)

testVerifyEmailAddressResponse :: VerifyEmailAddressResponse -> TestTree
testVerifyEmailAddressResponse = res
    "VerifyEmailAddressResponse"
    "fixture/VerifyEmailAddressResponse.proto"
    sES
    (Proxy :: Proxy VerifyEmailAddress)

testDeleteVerifiedEmailAddressResponse :: DeleteVerifiedEmailAddressResponse -> TestTree
testDeleteVerifiedEmailAddressResponse = res
    "DeleteVerifiedEmailAddressResponse"
    "fixture/DeleteVerifiedEmailAddressResponse.proto"
    sES
    (Proxy :: Proxy DeleteVerifiedEmailAddress)

testListVerifiedEmailAddressesResponse :: ListVerifiedEmailAddressesResponse -> TestTree
testListVerifiedEmailAddressesResponse = res
    "ListVerifiedEmailAddressesResponse"
    "fixture/ListVerifiedEmailAddressesResponse.proto"
    sES
    (Proxy :: Proxy ListVerifiedEmailAddresses)

testSetIdentityNotificationTopicResponse :: SetIdentityNotificationTopicResponse -> TestTree
testSetIdentityNotificationTopicResponse = res
    "SetIdentityNotificationTopicResponse"
    "fixture/SetIdentityNotificationTopicResponse.proto"
    sES
    (Proxy :: Proxy SetIdentityNotificationTopic)

testSendEmailResponse :: SendEmailResponse -> TestTree
testSendEmailResponse = res
    "SendEmailResponse"
    "fixture/SendEmailResponse.proto"
    sES
    (Proxy :: Proxy SendEmail)
