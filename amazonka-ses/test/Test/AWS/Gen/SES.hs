{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SES
-- Copyright   : (c) 2013-2016 Brendan Hay
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
--         [ requestCreateReceiptRuleSet $
--             createReceiptRuleSet
--
--         , requestGetSendQuota $
--             getSendQuota
--
--         , requestPutIdentityPolicy $
--             putIdentityPolicy
--
--         , requestDeleteIdentityPolicy $
--             deleteIdentityPolicy
--
--         , requestGetIdentityNotificationAttributes $
--             getIdentityNotificationAttributes
--
--         , requestListIdentityPolicies $
--             listIdentityPolicies
--
--         , requestSetIdentityDkimEnabled $
--             setIdentityDkimEnabled
--
--         , requestListReceiptFilters $
--             listReceiptFilters
--
--         , requestDescribeReceiptRuleSet $
--             describeReceiptRuleSet
--
--         , requestGetIdentityMailFromDomainAttributes $
--             getIdentityMailFromDomainAttributes
--
--         , requestCreateReceiptFilter $
--             createReceiptFilter
--
--         , requestSetIdentityMailFromDomain $
--             setIdentityMailFromDomain
--
--         , requestSetIdentityFeedbackForwardingEnabled $
--             setIdentityFeedbackForwardingEnabled
--
--         , requestGetIdentityVerificationAttributes $
--             getIdentityVerificationAttributes
--
--         , requestGetIdentityPolicies $
--             getIdentityPolicies
--
--         , requestVerifyDomainIdentity $
--             verifyDomainIdentity
--
--         , requestReorderReceiptRuleSet $
--             reorderReceiptRuleSet
--
--         , requestListReceiptRuleSets $
--             listReceiptRuleSets
--
--         , requestDeleteReceiptRuleSet $
--             deleteReceiptRuleSet
--
--         , requestSetReceiptRulePosition $
--             setReceiptRulePosition
--
--         , requestSendBounce $
--             sendBounce
--
--         , requestGetIdentityDkimAttributes $
--             getIdentityDkimAttributes
--
--         , requestVerifyDomainDkim $
--             verifyDomainDkim
--
--         , requestSendRawEmail $
--             sendRawEmail
--
--         , requestGetSendStatistics $
--             getSendStatistics
--
--         , requestDeleteIdentity $
--             deleteIdentity
--
--         , requestDescribeReceiptRule $
--             describeReceiptRule
--
--         , requestListIdentities $
--             listIdentities
--
--         , requestVerifyEmailIdentity $
--             verifyEmailIdentity
--
--         , requestVerifyEmailAddress $
--             verifyEmailAddress
--
--         , requestDeleteVerifiedEmailAddress $
--             deleteVerifiedEmailAddress
--
--         , requestDeleteReceiptFilter $
--             deleteReceiptFilter
--
--         , requestListVerifiedEmailAddresses $
--             listVerifiedEmailAddresses
--
--         , requestSetIdentityNotificationTopic $
--             setIdentityNotificationTopic
--
--         , requestSendEmail $
--             sendEmail
--
--         , requestDeleteReceiptRule $
--             deleteReceiptRule
--
--         , requestUpdateReceiptRule $
--             updateReceiptRule
--
--         , requestCloneReceiptRuleSet $
--             cloneReceiptRuleSet
--
--         , requestCreateReceiptRule $
--             createReceiptRule
--
--         , requestSetActiveReceiptRuleSet $
--             setActiveReceiptRuleSet
--
--         , requestDescribeActiveReceiptRuleSet $
--             describeActiveReceiptRuleSet
--
--           ]

--     , testGroup "response"
--         [ responseCreateReceiptRuleSet $
--             createReceiptRuleSetResponse
--
--         , responseGetSendQuota $
--             getSendQuotaResponse
--
--         , responsePutIdentityPolicy $
--             putIdentityPolicyResponse
--
--         , responseDeleteIdentityPolicy $
--             deleteIdentityPolicyResponse
--
--         , responseGetIdentityNotificationAttributes $
--             getIdentityNotificationAttributesResponse
--
--         , responseListIdentityPolicies $
--             listIdentityPoliciesResponse
--
--         , responseSetIdentityDkimEnabled $
--             setIdentityDkimEnabledResponse
--
--         , responseListReceiptFilters $
--             listReceiptFiltersResponse
--
--         , responseDescribeReceiptRuleSet $
--             describeReceiptRuleSetResponse
--
--         , responseGetIdentityMailFromDomainAttributes $
--             getIdentityMailFromDomainAttributesResponse
--
--         , responseCreateReceiptFilter $
--             createReceiptFilterResponse
--
--         , responseSetIdentityMailFromDomain $
--             setIdentityMailFromDomainResponse
--
--         , responseSetIdentityFeedbackForwardingEnabled $
--             setIdentityFeedbackForwardingEnabledResponse
--
--         , responseGetIdentityVerificationAttributes $
--             getIdentityVerificationAttributesResponse
--
--         , responseGetIdentityPolicies $
--             getIdentityPoliciesResponse
--
--         , responseVerifyDomainIdentity $
--             verifyDomainIdentityResponse
--
--         , responseReorderReceiptRuleSet $
--             reorderReceiptRuleSetResponse
--
--         , responseListReceiptRuleSets $
--             listReceiptRuleSetsResponse
--
--         , responseDeleteReceiptRuleSet $
--             deleteReceiptRuleSetResponse
--
--         , responseSetReceiptRulePosition $
--             setReceiptRulePositionResponse
--
--         , responseSendBounce $
--             sendBounceResponse
--
--         , responseGetIdentityDkimAttributes $
--             getIdentityDkimAttributesResponse
--
--         , responseVerifyDomainDkim $
--             verifyDomainDkimResponse
--
--         , responseSendRawEmail $
--             sendRawEmailResponse
--
--         , responseGetSendStatistics $
--             getSendStatisticsResponse
--
--         , responseDeleteIdentity $
--             deleteIdentityResponse
--
--         , responseDescribeReceiptRule $
--             describeReceiptRuleResponse
--
--         , responseListIdentities $
--             listIdentitiesResponse
--
--         , responseVerifyEmailIdentity $
--             verifyEmailIdentityResponse
--
--         , responseVerifyEmailAddress $
--             verifyEmailAddressResponse
--
--         , responseDeleteVerifiedEmailAddress $
--             deleteVerifiedEmailAddressResponse
--
--         , responseDeleteReceiptFilter $
--             deleteReceiptFilterResponse
--
--         , responseListVerifiedEmailAddresses $
--             listVerifiedEmailAddressesResponse
--
--         , responseSetIdentityNotificationTopic $
--             setIdentityNotificationTopicResponse
--
--         , responseSendEmail $
--             sendEmailResponse
--
--         , responseDeleteReceiptRule $
--             deleteReceiptRuleResponse
--
--         , responseUpdateReceiptRule $
--             updateReceiptRuleResponse
--
--         , responseCloneReceiptRuleSet $
--             cloneReceiptRuleSetResponse
--
--         , responseCreateReceiptRule $
--             createReceiptRuleResponse
--
--         , responseSetActiveReceiptRuleSet $
--             setActiveReceiptRuleSetResponse
--
--         , responseDescribeActiveReceiptRuleSet $
--             describeActiveReceiptRuleSetResponse
--
--           ]
--     ]

-- Requests

requestCreateReceiptRuleSet :: CreateReceiptRuleSet -> TestTree
requestCreateReceiptRuleSet = req
    "CreateReceiptRuleSet"
    "fixture/CreateReceiptRuleSet.yaml"

requestGetSendQuota :: GetSendQuota -> TestTree
requestGetSendQuota = req
    "GetSendQuota"
    "fixture/GetSendQuota.yaml"

requestPutIdentityPolicy :: PutIdentityPolicy -> TestTree
requestPutIdentityPolicy = req
    "PutIdentityPolicy"
    "fixture/PutIdentityPolicy.yaml"

requestDeleteIdentityPolicy :: DeleteIdentityPolicy -> TestTree
requestDeleteIdentityPolicy = req
    "DeleteIdentityPolicy"
    "fixture/DeleteIdentityPolicy.yaml"

requestGetIdentityNotificationAttributes :: GetIdentityNotificationAttributes -> TestTree
requestGetIdentityNotificationAttributes = req
    "GetIdentityNotificationAttributes"
    "fixture/GetIdentityNotificationAttributes.yaml"

requestListIdentityPolicies :: ListIdentityPolicies -> TestTree
requestListIdentityPolicies = req
    "ListIdentityPolicies"
    "fixture/ListIdentityPolicies.yaml"

requestSetIdentityDkimEnabled :: SetIdentityDkimEnabled -> TestTree
requestSetIdentityDkimEnabled = req
    "SetIdentityDkimEnabled"
    "fixture/SetIdentityDkimEnabled.yaml"

requestListReceiptFilters :: ListReceiptFilters -> TestTree
requestListReceiptFilters = req
    "ListReceiptFilters"
    "fixture/ListReceiptFilters.yaml"

requestDescribeReceiptRuleSet :: DescribeReceiptRuleSet -> TestTree
requestDescribeReceiptRuleSet = req
    "DescribeReceiptRuleSet"
    "fixture/DescribeReceiptRuleSet.yaml"

requestGetIdentityMailFromDomainAttributes :: GetIdentityMailFromDomainAttributes -> TestTree
requestGetIdentityMailFromDomainAttributes = req
    "GetIdentityMailFromDomainAttributes"
    "fixture/GetIdentityMailFromDomainAttributes.yaml"

requestCreateReceiptFilter :: CreateReceiptFilter -> TestTree
requestCreateReceiptFilter = req
    "CreateReceiptFilter"
    "fixture/CreateReceiptFilter.yaml"

requestSetIdentityMailFromDomain :: SetIdentityMailFromDomain -> TestTree
requestSetIdentityMailFromDomain = req
    "SetIdentityMailFromDomain"
    "fixture/SetIdentityMailFromDomain.yaml"

requestSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabled -> TestTree
requestSetIdentityFeedbackForwardingEnabled = req
    "SetIdentityFeedbackForwardingEnabled"
    "fixture/SetIdentityFeedbackForwardingEnabled.yaml"

requestGetIdentityVerificationAttributes :: GetIdentityVerificationAttributes -> TestTree
requestGetIdentityVerificationAttributes = req
    "GetIdentityVerificationAttributes"
    "fixture/GetIdentityVerificationAttributes.yaml"

requestGetIdentityPolicies :: GetIdentityPolicies -> TestTree
requestGetIdentityPolicies = req
    "GetIdentityPolicies"
    "fixture/GetIdentityPolicies.yaml"

requestVerifyDomainIdentity :: VerifyDomainIdentity -> TestTree
requestVerifyDomainIdentity = req
    "VerifyDomainIdentity"
    "fixture/VerifyDomainIdentity.yaml"

requestReorderReceiptRuleSet :: ReorderReceiptRuleSet -> TestTree
requestReorderReceiptRuleSet = req
    "ReorderReceiptRuleSet"
    "fixture/ReorderReceiptRuleSet.yaml"

requestListReceiptRuleSets :: ListReceiptRuleSets -> TestTree
requestListReceiptRuleSets = req
    "ListReceiptRuleSets"
    "fixture/ListReceiptRuleSets.yaml"

requestDeleteReceiptRuleSet :: DeleteReceiptRuleSet -> TestTree
requestDeleteReceiptRuleSet = req
    "DeleteReceiptRuleSet"
    "fixture/DeleteReceiptRuleSet.yaml"

requestSetReceiptRulePosition :: SetReceiptRulePosition -> TestTree
requestSetReceiptRulePosition = req
    "SetReceiptRulePosition"
    "fixture/SetReceiptRulePosition.yaml"

requestSendBounce :: SendBounce -> TestTree
requestSendBounce = req
    "SendBounce"
    "fixture/SendBounce.yaml"

requestGetIdentityDkimAttributes :: GetIdentityDkimAttributes -> TestTree
requestGetIdentityDkimAttributes = req
    "GetIdentityDkimAttributes"
    "fixture/GetIdentityDkimAttributes.yaml"

requestVerifyDomainDkim :: VerifyDomainDkim -> TestTree
requestVerifyDomainDkim = req
    "VerifyDomainDkim"
    "fixture/VerifyDomainDkim.yaml"

requestSendRawEmail :: SendRawEmail -> TestTree
requestSendRawEmail = req
    "SendRawEmail"
    "fixture/SendRawEmail.yaml"

requestGetSendStatistics :: GetSendStatistics -> TestTree
requestGetSendStatistics = req
    "GetSendStatistics"
    "fixture/GetSendStatistics.yaml"

requestDeleteIdentity :: DeleteIdentity -> TestTree
requestDeleteIdentity = req
    "DeleteIdentity"
    "fixture/DeleteIdentity.yaml"

requestDescribeReceiptRule :: DescribeReceiptRule -> TestTree
requestDescribeReceiptRule = req
    "DescribeReceiptRule"
    "fixture/DescribeReceiptRule.yaml"

requestListIdentities :: ListIdentities -> TestTree
requestListIdentities = req
    "ListIdentities"
    "fixture/ListIdentities.yaml"

requestVerifyEmailIdentity :: VerifyEmailIdentity -> TestTree
requestVerifyEmailIdentity = req
    "VerifyEmailIdentity"
    "fixture/VerifyEmailIdentity.yaml"

requestVerifyEmailAddress :: VerifyEmailAddress -> TestTree
requestVerifyEmailAddress = req
    "VerifyEmailAddress"
    "fixture/VerifyEmailAddress.yaml"

requestDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddress -> TestTree
requestDeleteVerifiedEmailAddress = req
    "DeleteVerifiedEmailAddress"
    "fixture/DeleteVerifiedEmailAddress.yaml"

requestDeleteReceiptFilter :: DeleteReceiptFilter -> TestTree
requestDeleteReceiptFilter = req
    "DeleteReceiptFilter"
    "fixture/DeleteReceiptFilter.yaml"

requestListVerifiedEmailAddresses :: ListVerifiedEmailAddresses -> TestTree
requestListVerifiedEmailAddresses = req
    "ListVerifiedEmailAddresses"
    "fixture/ListVerifiedEmailAddresses.yaml"

requestSetIdentityNotificationTopic :: SetIdentityNotificationTopic -> TestTree
requestSetIdentityNotificationTopic = req
    "SetIdentityNotificationTopic"
    "fixture/SetIdentityNotificationTopic.yaml"

requestSendEmail :: SendEmail -> TestTree
requestSendEmail = req
    "SendEmail"
    "fixture/SendEmail.yaml"

requestDeleteReceiptRule :: DeleteReceiptRule -> TestTree
requestDeleteReceiptRule = req
    "DeleteReceiptRule"
    "fixture/DeleteReceiptRule.yaml"

requestUpdateReceiptRule :: UpdateReceiptRule -> TestTree
requestUpdateReceiptRule = req
    "UpdateReceiptRule"
    "fixture/UpdateReceiptRule.yaml"

requestCloneReceiptRuleSet :: CloneReceiptRuleSet -> TestTree
requestCloneReceiptRuleSet = req
    "CloneReceiptRuleSet"
    "fixture/CloneReceiptRuleSet.yaml"

requestCreateReceiptRule :: CreateReceiptRule -> TestTree
requestCreateReceiptRule = req
    "CreateReceiptRule"
    "fixture/CreateReceiptRule.yaml"

requestSetActiveReceiptRuleSet :: SetActiveReceiptRuleSet -> TestTree
requestSetActiveReceiptRuleSet = req
    "SetActiveReceiptRuleSet"
    "fixture/SetActiveReceiptRuleSet.yaml"

requestDescribeActiveReceiptRuleSet :: DescribeActiveReceiptRuleSet -> TestTree
requestDescribeActiveReceiptRuleSet = req
    "DescribeActiveReceiptRuleSet"
    "fixture/DescribeActiveReceiptRuleSet.yaml"

-- Responses

responseCreateReceiptRuleSet :: CreateReceiptRuleSetResponse -> TestTree
responseCreateReceiptRuleSet = res
    "CreateReceiptRuleSetResponse"
    "fixture/CreateReceiptRuleSetResponse.proto"
    ses
    (Proxy :: Proxy CreateReceiptRuleSet)

responseGetSendQuota :: GetSendQuotaResponse -> TestTree
responseGetSendQuota = res
    "GetSendQuotaResponse"
    "fixture/GetSendQuotaResponse.proto"
    ses
    (Proxy :: Proxy GetSendQuota)

responsePutIdentityPolicy :: PutIdentityPolicyResponse -> TestTree
responsePutIdentityPolicy = res
    "PutIdentityPolicyResponse"
    "fixture/PutIdentityPolicyResponse.proto"
    ses
    (Proxy :: Proxy PutIdentityPolicy)

responseDeleteIdentityPolicy :: DeleteIdentityPolicyResponse -> TestTree
responseDeleteIdentityPolicy = res
    "DeleteIdentityPolicyResponse"
    "fixture/DeleteIdentityPolicyResponse.proto"
    ses
    (Proxy :: Proxy DeleteIdentityPolicy)

responseGetIdentityNotificationAttributes :: GetIdentityNotificationAttributesResponse -> TestTree
responseGetIdentityNotificationAttributes = res
    "GetIdentityNotificationAttributesResponse"
    "fixture/GetIdentityNotificationAttributesResponse.proto"
    ses
    (Proxy :: Proxy GetIdentityNotificationAttributes)

responseListIdentityPolicies :: ListIdentityPoliciesResponse -> TestTree
responseListIdentityPolicies = res
    "ListIdentityPoliciesResponse"
    "fixture/ListIdentityPoliciesResponse.proto"
    ses
    (Proxy :: Proxy ListIdentityPolicies)

responseSetIdentityDkimEnabled :: SetIdentityDkimEnabledResponse -> TestTree
responseSetIdentityDkimEnabled = res
    "SetIdentityDkimEnabledResponse"
    "fixture/SetIdentityDkimEnabledResponse.proto"
    ses
    (Proxy :: Proxy SetIdentityDkimEnabled)

responseListReceiptFilters :: ListReceiptFiltersResponse -> TestTree
responseListReceiptFilters = res
    "ListReceiptFiltersResponse"
    "fixture/ListReceiptFiltersResponse.proto"
    ses
    (Proxy :: Proxy ListReceiptFilters)

responseDescribeReceiptRuleSet :: DescribeReceiptRuleSetResponse -> TestTree
responseDescribeReceiptRuleSet = res
    "DescribeReceiptRuleSetResponse"
    "fixture/DescribeReceiptRuleSetResponse.proto"
    ses
    (Proxy :: Proxy DescribeReceiptRuleSet)

responseGetIdentityMailFromDomainAttributes :: GetIdentityMailFromDomainAttributesResponse -> TestTree
responseGetIdentityMailFromDomainAttributes = res
    "GetIdentityMailFromDomainAttributesResponse"
    "fixture/GetIdentityMailFromDomainAttributesResponse.proto"
    ses
    (Proxy :: Proxy GetIdentityMailFromDomainAttributes)

responseCreateReceiptFilter :: CreateReceiptFilterResponse -> TestTree
responseCreateReceiptFilter = res
    "CreateReceiptFilterResponse"
    "fixture/CreateReceiptFilterResponse.proto"
    ses
    (Proxy :: Proxy CreateReceiptFilter)

responseSetIdentityMailFromDomain :: SetIdentityMailFromDomainResponse -> TestTree
responseSetIdentityMailFromDomain = res
    "SetIdentityMailFromDomainResponse"
    "fixture/SetIdentityMailFromDomainResponse.proto"
    ses
    (Proxy :: Proxy SetIdentityMailFromDomain)

responseSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
responseSetIdentityFeedbackForwardingEnabled = res
    "SetIdentityFeedbackForwardingEnabledResponse"
    "fixture/SetIdentityFeedbackForwardingEnabledResponse.proto"
    ses
    (Proxy :: Proxy SetIdentityFeedbackForwardingEnabled)

responseGetIdentityVerificationAttributes :: GetIdentityVerificationAttributesResponse -> TestTree
responseGetIdentityVerificationAttributes = res
    "GetIdentityVerificationAttributesResponse"
    "fixture/GetIdentityVerificationAttributesResponse.proto"
    ses
    (Proxy :: Proxy GetIdentityVerificationAttributes)

responseGetIdentityPolicies :: GetIdentityPoliciesResponse -> TestTree
responseGetIdentityPolicies = res
    "GetIdentityPoliciesResponse"
    "fixture/GetIdentityPoliciesResponse.proto"
    ses
    (Proxy :: Proxy GetIdentityPolicies)

responseVerifyDomainIdentity :: VerifyDomainIdentityResponse -> TestTree
responseVerifyDomainIdentity = res
    "VerifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse.proto"
    ses
    (Proxy :: Proxy VerifyDomainIdentity)

responseReorderReceiptRuleSet :: ReorderReceiptRuleSetResponse -> TestTree
responseReorderReceiptRuleSet = res
    "ReorderReceiptRuleSetResponse"
    "fixture/ReorderReceiptRuleSetResponse.proto"
    ses
    (Proxy :: Proxy ReorderReceiptRuleSet)

responseListReceiptRuleSets :: ListReceiptRuleSetsResponse -> TestTree
responseListReceiptRuleSets = res
    "ListReceiptRuleSetsResponse"
    "fixture/ListReceiptRuleSetsResponse.proto"
    ses
    (Proxy :: Proxy ListReceiptRuleSets)

responseDeleteReceiptRuleSet :: DeleteReceiptRuleSetResponse -> TestTree
responseDeleteReceiptRuleSet = res
    "DeleteReceiptRuleSetResponse"
    "fixture/DeleteReceiptRuleSetResponse.proto"
    ses
    (Proxy :: Proxy DeleteReceiptRuleSet)

responseSetReceiptRulePosition :: SetReceiptRulePositionResponse -> TestTree
responseSetReceiptRulePosition = res
    "SetReceiptRulePositionResponse"
    "fixture/SetReceiptRulePositionResponse.proto"
    ses
    (Proxy :: Proxy SetReceiptRulePosition)

responseSendBounce :: SendBounceResponse -> TestTree
responseSendBounce = res
    "SendBounceResponse"
    "fixture/SendBounceResponse.proto"
    ses
    (Proxy :: Proxy SendBounce)

responseGetIdentityDkimAttributes :: GetIdentityDkimAttributesResponse -> TestTree
responseGetIdentityDkimAttributes = res
    "GetIdentityDkimAttributesResponse"
    "fixture/GetIdentityDkimAttributesResponse.proto"
    ses
    (Proxy :: Proxy GetIdentityDkimAttributes)

responseVerifyDomainDkim :: VerifyDomainDkimResponse -> TestTree
responseVerifyDomainDkim = res
    "VerifyDomainDkimResponse"
    "fixture/VerifyDomainDkimResponse.proto"
    ses
    (Proxy :: Proxy VerifyDomainDkim)

responseSendRawEmail :: SendRawEmailResponse -> TestTree
responseSendRawEmail = res
    "SendRawEmailResponse"
    "fixture/SendRawEmailResponse.proto"
    ses
    (Proxy :: Proxy SendRawEmail)

responseGetSendStatistics :: GetSendStatisticsResponse -> TestTree
responseGetSendStatistics = res
    "GetSendStatisticsResponse"
    "fixture/GetSendStatisticsResponse.proto"
    ses
    (Proxy :: Proxy GetSendStatistics)

responseDeleteIdentity :: DeleteIdentityResponse -> TestTree
responseDeleteIdentity = res
    "DeleteIdentityResponse"
    "fixture/DeleteIdentityResponse.proto"
    ses
    (Proxy :: Proxy DeleteIdentity)

responseDescribeReceiptRule :: DescribeReceiptRuleResponse -> TestTree
responseDescribeReceiptRule = res
    "DescribeReceiptRuleResponse"
    "fixture/DescribeReceiptRuleResponse.proto"
    ses
    (Proxy :: Proxy DescribeReceiptRule)

responseListIdentities :: ListIdentitiesResponse -> TestTree
responseListIdentities = res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse.proto"
    ses
    (Proxy :: Proxy ListIdentities)

responseVerifyEmailIdentity :: VerifyEmailIdentityResponse -> TestTree
responseVerifyEmailIdentity = res
    "VerifyEmailIdentityResponse"
    "fixture/VerifyEmailIdentityResponse.proto"
    ses
    (Proxy :: Proxy VerifyEmailIdentity)

responseVerifyEmailAddress :: VerifyEmailAddressResponse -> TestTree
responseVerifyEmailAddress = res
    "VerifyEmailAddressResponse"
    "fixture/VerifyEmailAddressResponse.proto"
    ses
    (Proxy :: Proxy VerifyEmailAddress)

responseDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddressResponse -> TestTree
responseDeleteVerifiedEmailAddress = res
    "DeleteVerifiedEmailAddressResponse"
    "fixture/DeleteVerifiedEmailAddressResponse.proto"
    ses
    (Proxy :: Proxy DeleteVerifiedEmailAddress)

responseDeleteReceiptFilter :: DeleteReceiptFilterResponse -> TestTree
responseDeleteReceiptFilter = res
    "DeleteReceiptFilterResponse"
    "fixture/DeleteReceiptFilterResponse.proto"
    ses
    (Proxy :: Proxy DeleteReceiptFilter)

responseListVerifiedEmailAddresses :: ListVerifiedEmailAddressesResponse -> TestTree
responseListVerifiedEmailAddresses = res
    "ListVerifiedEmailAddressesResponse"
    "fixture/ListVerifiedEmailAddressesResponse.proto"
    ses
    (Proxy :: Proxy ListVerifiedEmailAddresses)

responseSetIdentityNotificationTopic :: SetIdentityNotificationTopicResponse -> TestTree
responseSetIdentityNotificationTopic = res
    "SetIdentityNotificationTopicResponse"
    "fixture/SetIdentityNotificationTopicResponse.proto"
    ses
    (Proxy :: Proxy SetIdentityNotificationTopic)

responseSendEmail :: SendEmailResponse -> TestTree
responseSendEmail = res
    "SendEmailResponse"
    "fixture/SendEmailResponse.proto"
    ses
    (Proxy :: Proxy SendEmail)

responseDeleteReceiptRule :: DeleteReceiptRuleResponse -> TestTree
responseDeleteReceiptRule = res
    "DeleteReceiptRuleResponse"
    "fixture/DeleteReceiptRuleResponse.proto"
    ses
    (Proxy :: Proxy DeleteReceiptRule)

responseUpdateReceiptRule :: UpdateReceiptRuleResponse -> TestTree
responseUpdateReceiptRule = res
    "UpdateReceiptRuleResponse"
    "fixture/UpdateReceiptRuleResponse.proto"
    ses
    (Proxy :: Proxy UpdateReceiptRule)

responseCloneReceiptRuleSet :: CloneReceiptRuleSetResponse -> TestTree
responseCloneReceiptRuleSet = res
    "CloneReceiptRuleSetResponse"
    "fixture/CloneReceiptRuleSetResponse.proto"
    ses
    (Proxy :: Proxy CloneReceiptRuleSet)

responseCreateReceiptRule :: CreateReceiptRuleResponse -> TestTree
responseCreateReceiptRule = res
    "CreateReceiptRuleResponse"
    "fixture/CreateReceiptRuleResponse.proto"
    ses
    (Proxy :: Proxy CreateReceiptRule)

responseSetActiveReceiptRuleSet :: SetActiveReceiptRuleSetResponse -> TestTree
responseSetActiveReceiptRuleSet = res
    "SetActiveReceiptRuleSetResponse"
    "fixture/SetActiveReceiptRuleSetResponse.proto"
    ses
    (Proxy :: Proxy SetActiveReceiptRuleSet)

responseDescribeActiveReceiptRuleSet :: DescribeActiveReceiptRuleSetResponse -> TestTree
responseDescribeActiveReceiptRuleSet = res
    "DescribeActiveReceiptRuleSetResponse"
    "fixture/DescribeActiveReceiptRuleSetResponse.proto"
    ses
    (Proxy :: Proxy DescribeActiveReceiptRuleSet)
