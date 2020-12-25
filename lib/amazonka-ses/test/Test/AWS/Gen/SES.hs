{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SES
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SES where

import Data.Proxy
import Network.AWS.SES
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SES.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateTemplate $
--             mkCreateTemplate
--
--         , requestDeleteConfigurationSetTrackingOptions $
--             mkDeleteConfigurationSetTrackingOptions
--
--         , requestUpdateConfigurationSetTrackingOptions $
--             mkUpdateConfigurationSetTrackingOptions
--
--         , requestCreateReceiptRuleSet $
--             mkCreateReceiptRuleSet
--
--         , requestSetIdentityHeadersInNotificationsEnabled $
--             mkSetIdentityHeadersInNotificationsEnabled
--
--         , requestGetSendQuota $
--             mkGetSendQuota
--
--         , requestPutConfigurationSetDeliveryOptions $
--             mkPutConfigurationSetDeliveryOptions
--
--         , requestDescribeConfigurationSet $
--             mkDescribeConfigurationSet
--
--         , requestPutIdentityPolicy $
--             mkPutIdentityPolicy
--
--         , requestDeleteCustomVerificationEmailTemplate $
--             mkDeleteCustomVerificationEmailTemplate
--
--         , requestDeleteIdentityPolicy $
--             mkDeleteIdentityPolicy
--
--         , requestUpdateCustomVerificationEmailTemplate $
--             mkUpdateCustomVerificationEmailTemplate
--
--         , requestSendCustomVerificationEmail $
--             mkSendCustomVerificationEmail
--
--         , requestGetIdentityNotificationAttributes $
--             mkGetIdentityNotificationAttributes
--
--         , requestUpdateConfigurationSetReputationMetricsEnabled $
--             mkUpdateConfigurationSetReputationMetricsEnabled
--
--         , requestListIdentityPolicies $
--             mkListIdentityPolicies
--
--         , requestSetIdentityDkimEnabled $
--             mkSetIdentityDkimEnabled
--
--         , requestListReceiptFilters $
--             mkListReceiptFilters
--
--         , requestDescribeReceiptRuleSet $
--             mkDescribeReceiptRuleSet
--
--         , requestGetIdentityMailFromDomainAttributes $
--             mkGetIdentityMailFromDomainAttributes
--
--         , requestCreateReceiptFilter $
--             mkCreateReceiptFilter
--
--         , requestUpdateConfigurationSetEventDestination $
--             mkUpdateConfigurationSetEventDestination
--
--         , requestDeleteConfigurationSetEventDestination $
--             mkDeleteConfigurationSetEventDestination
--
--         , requestSetIdentityMailFromDomain $
--             mkSetIdentityMailFromDomain
--
--         , requestSetIdentityFeedbackForwardingEnabled $
--             mkSetIdentityFeedbackForwardingEnabled
--
--         , requestListConfigurationSets $
--             mkListConfigurationSets
--
--         , requestDeleteConfigurationSet $
--             mkDeleteConfigurationSet
--
--         , requestGetIdentityVerificationAttributes $
--             mkGetIdentityVerificationAttributes
--
--         , requestGetIdentityPolicies $
--             mkGetIdentityPolicies
--
--         , requestListTemplates $
--             mkListTemplates
--
--         , requestVerifyDomainIdentity $
--             mkVerifyDomainIdentity
--
--         , requestUpdateTemplate $
--             mkUpdateTemplate
--
--         , requestDeleteTemplate $
--             mkDeleteTemplate
--
--         , requestReorderReceiptRuleSet $
--             mkReorderReceiptRuleSet
--
--         , requestListReceiptRuleSets $
--             mkListReceiptRuleSets
--
--         , requestDeleteReceiptRuleSet $
--             mkDeleteReceiptRuleSet
--
--         , requestSetReceiptRulePosition $
--             mkSetReceiptRulePosition
--
--         , requestSendBounce $
--             mkSendBounce
--
--         , requestGetIdentityDkimAttributes $
--             mkGetIdentityDkimAttributes
--
--         , requestSendTemplatedEmail $
--             mkSendTemplatedEmail
--
--         , requestVerifyDomainDkim $
--             mkVerifyDomainDkim
--
--         , requestTestRenderTemplate $
--             mkTestRenderTemplate
--
--         , requestSendBulkTemplatedEmail $
--             mkSendBulkTemplatedEmail
--
--         , requestSendRawEmail $
--             mkSendRawEmail
--
--         , requestGetSendStatistics $
--             mkGetSendStatistics
--
--         , requestListCustomVerificationEmailTemplates $
--             mkListCustomVerificationEmailTemplates
--
--         , requestDeleteIdentity $
--             mkDeleteIdentity
--
--         , requestDescribeReceiptRule $
--             mkDescribeReceiptRule
--
--         , requestListIdentities $
--             mkListIdentities
--
--         , requestUpdateConfigurationSetSendingEnabled $
--             mkUpdateConfigurationSetSendingEnabled
--
--         , requestCreateCustomVerificationEmailTemplate $
--             mkCreateCustomVerificationEmailTemplate
--
--         , requestVerifyEmailIdentity $
--             mkVerifyEmailIdentity
--
--         , requestVerifyEmailAddress $
--             mkVerifyEmailAddress
--
--         , requestDeleteVerifiedEmailAddress $
--             mkDeleteVerifiedEmailAddress
--
--         , requestDeleteReceiptFilter $
--             mkDeleteReceiptFilter
--
--         , requestListVerifiedEmailAddresses $
--             mkListVerifiedEmailAddresses
--
--         , requestGetCustomVerificationEmailTemplate $
--             mkGetCustomVerificationEmailTemplate
--
--         , requestSetIdentityNotificationTopic $
--             mkSetIdentityNotificationTopic
--
--         , requestSendEmail $
--             mkSendEmail
--
--         , requestDeleteReceiptRule $
--             mkDeleteReceiptRule
--
--         , requestUpdateReceiptRule $
--             mkUpdateReceiptRule
--
--         , requestCloneReceiptRuleSet $
--             mkCloneReceiptRuleSet
--
--         , requestCreateConfigurationSetEventDestination $
--             mkCreateConfigurationSetEventDestination
--
--         , requestGetAccountSendingEnabled $
--             mkGetAccountSendingEnabled
--
--         , requestCreateReceiptRule $
--             mkCreateReceiptRule
--
--         , requestGetTemplate $
--             mkGetTemplate
--
--         , requestSetActiveReceiptRuleSet $
--             mkSetActiveReceiptRuleSet
--
--         , requestCreateConfigurationSet $
--             mkCreateConfigurationSet
--
--         , requestUpdateAccountSendingEnabled $
--             mkUpdateAccountSendingEnabled
--
--         , requestCreateConfigurationSetTrackingOptions $
--             mkCreateConfigurationSetTrackingOptions
--
--         , requestDescribeActiveReceiptRuleSet $
--             mkDescribeActiveReceiptRuleSet
--
--           ]

--     , testGroup "response"
--         [ responseCreateTemplate $
--             mkCreateTemplateResponse
--
--         , responseDeleteConfigurationSetTrackingOptions $
--             mkDeleteConfigurationSetTrackingOptionsResponse
--
--         , responseUpdateConfigurationSetTrackingOptions $
--             mkUpdateConfigurationSetTrackingOptionsResponse
--
--         , responseCreateReceiptRuleSet $
--             mkCreateReceiptRuleSetResponse
--
--         , responseSetIdentityHeadersInNotificationsEnabled $
--             mkSetIdentityHeadersInNotificationsEnabledResponse
--
--         , responseGetSendQuota $
--             mkGetSendQuotaResponse
--
--         , responsePutConfigurationSetDeliveryOptions $
--             mkPutConfigurationSetDeliveryOptionsResponse
--
--         , responseDescribeConfigurationSet $
--             mkDescribeConfigurationSetResponse
--
--         , responsePutIdentityPolicy $
--             mkPutIdentityPolicyResponse
--
--         , responseDeleteCustomVerificationEmailTemplate $
--             mkDeleteCustomVerificationEmailTemplateResponse
--
--         , responseDeleteIdentityPolicy $
--             mkDeleteIdentityPolicyResponse
--
--         , responseUpdateCustomVerificationEmailTemplate $
--             mkUpdateCustomVerificationEmailTemplateResponse
--
--         , responseSendCustomVerificationEmail $
--             mkSendCustomVerificationEmailResponse
--
--         , responseGetIdentityNotificationAttributes $
--             mkGetIdentityNotificationAttributesResponse
--
--         , responseUpdateConfigurationSetReputationMetricsEnabled $
--             mkUpdateConfigurationSetReputationMetricsEnabledResponse
--
--         , responseListIdentityPolicies $
--             mkListIdentityPoliciesResponse
--
--         , responseSetIdentityDkimEnabled $
--             mkSetIdentityDkimEnabledResponse
--
--         , responseListReceiptFilters $
--             mkListReceiptFiltersResponse
--
--         , responseDescribeReceiptRuleSet $
--             mkDescribeReceiptRuleSetResponse
--
--         , responseGetIdentityMailFromDomainAttributes $
--             mkGetIdentityMailFromDomainAttributesResponse
--
--         , responseCreateReceiptFilter $
--             mkCreateReceiptFilterResponse
--
--         , responseUpdateConfigurationSetEventDestination $
--             mkUpdateConfigurationSetEventDestinationResponse
--
--         , responseDeleteConfigurationSetEventDestination $
--             mkDeleteConfigurationSetEventDestinationResponse
--
--         , responseSetIdentityMailFromDomain $
--             mkSetIdentityMailFromDomainResponse
--
--         , responseSetIdentityFeedbackForwardingEnabled $
--             mkSetIdentityFeedbackForwardingEnabledResponse
--
--         , responseListConfigurationSets $
--             mkListConfigurationSetsResponse
--
--         , responseDeleteConfigurationSet $
--             mkDeleteConfigurationSetResponse
--
--         , responseGetIdentityVerificationAttributes $
--             mkGetIdentityVerificationAttributesResponse
--
--         , responseGetIdentityPolicies $
--             mkGetIdentityPoliciesResponse
--
--         , responseListTemplates $
--             mkListTemplatesResponse
--
--         , responseVerifyDomainIdentity $
--             mkVerifyDomainIdentityResponse
--
--         , responseUpdateTemplate $
--             mkUpdateTemplateResponse
--
--         , responseDeleteTemplate $
--             mkDeleteTemplateResponse
--
--         , responseReorderReceiptRuleSet $
--             mkReorderReceiptRuleSetResponse
--
--         , responseListReceiptRuleSets $
--             mkListReceiptRuleSetsResponse
--
--         , responseDeleteReceiptRuleSet $
--             mkDeleteReceiptRuleSetResponse
--
--         , responseSetReceiptRulePosition $
--             mkSetReceiptRulePositionResponse
--
--         , responseSendBounce $
--             mkSendBounceResponse
--
--         , responseGetIdentityDkimAttributes $
--             mkGetIdentityDkimAttributesResponse
--
--         , responseSendTemplatedEmail $
--             mkSendTemplatedEmailResponse
--
--         , responseVerifyDomainDkim $
--             mkVerifyDomainDkimResponse
--
--         , responseTestRenderTemplate $
--             mkTestRenderTemplateResponse
--
--         , responseSendBulkTemplatedEmail $
--             mkSendBulkTemplatedEmailResponse
--
--         , responseSendRawEmail $
--             mkSendRawEmailResponse
--
--         , responseGetSendStatistics $
--             mkGetSendStatisticsResponse
--
--         , responseListCustomVerificationEmailTemplates $
--             mkListCustomVerificationEmailTemplatesResponse
--
--         , responseDeleteIdentity $
--             mkDeleteIdentityResponse
--
--         , responseDescribeReceiptRule $
--             mkDescribeReceiptRuleResponse
--
--         , responseListIdentities $
--             mkListIdentitiesResponse
--
--         , responseUpdateConfigurationSetSendingEnabled $
--             mkUpdateConfigurationSetSendingEnabledResponse
--
--         , responseCreateCustomVerificationEmailTemplate $
--             mkCreateCustomVerificationEmailTemplateResponse
--
--         , responseVerifyEmailIdentity $
--             mkVerifyEmailIdentityResponse
--
--         , responseVerifyEmailAddress $
--             mkVerifyEmailAddressResponse
--
--         , responseDeleteVerifiedEmailAddress $
--             mkDeleteVerifiedEmailAddressResponse
--
--         , responseDeleteReceiptFilter $
--             mkDeleteReceiptFilterResponse
--
--         , responseListVerifiedEmailAddresses $
--             mkListVerifiedEmailAddressesResponse
--
--         , responseGetCustomVerificationEmailTemplate $
--             mkGetCustomVerificationEmailTemplateResponse
--
--         , responseSetIdentityNotificationTopic $
--             mkSetIdentityNotificationTopicResponse
--
--         , responseSendEmail $
--             mkSendEmailResponse
--
--         , responseDeleteReceiptRule $
--             mkDeleteReceiptRuleResponse
--
--         , responseUpdateReceiptRule $
--             mkUpdateReceiptRuleResponse
--
--         , responseCloneReceiptRuleSet $
--             mkCloneReceiptRuleSetResponse
--
--         , responseCreateConfigurationSetEventDestination $
--             mkCreateConfigurationSetEventDestinationResponse
--
--         , responseGetAccountSendingEnabled $
--             mkGetAccountSendingEnabledResponse
--
--         , responseCreateReceiptRule $
--             mkCreateReceiptRuleResponse
--
--         , responseGetTemplate $
--             mkGetTemplateResponse
--
--         , responseSetActiveReceiptRuleSet $
--             mkSetActiveReceiptRuleSetResponse
--
--         , responseCreateConfigurationSet $
--             mkCreateConfigurationSetResponse
--
--         , responseUpdateAccountSendingEnabled $
--             mkUpdateAccountSendingEnabledResponse
--
--         , responseCreateConfigurationSetTrackingOptions $
--             mkCreateConfigurationSetTrackingOptionsResponse
--
--         , responseDescribeActiveReceiptRuleSet $
--             mkDescribeActiveReceiptRuleSetResponse
--
--           ]
--     ]

-- Requests

requestCreateTemplate :: CreateTemplate -> TestTree
requestCreateTemplate =
  req
    "CreateTemplate"
    "fixture/CreateTemplate.yaml"

requestDeleteConfigurationSetTrackingOptions :: DeleteConfigurationSetTrackingOptions -> TestTree
requestDeleteConfigurationSetTrackingOptions =
  req
    "DeleteConfigurationSetTrackingOptions"
    "fixture/DeleteConfigurationSetTrackingOptions.yaml"

requestUpdateConfigurationSetTrackingOptions :: UpdateConfigurationSetTrackingOptions -> TestTree
requestUpdateConfigurationSetTrackingOptions =
  req
    "UpdateConfigurationSetTrackingOptions"
    "fixture/UpdateConfigurationSetTrackingOptions.yaml"

requestCreateReceiptRuleSet :: CreateReceiptRuleSet -> TestTree
requestCreateReceiptRuleSet =
  req
    "CreateReceiptRuleSet"
    "fixture/CreateReceiptRuleSet.yaml"

requestSetIdentityHeadersInNotificationsEnabled :: SetIdentityHeadersInNotificationsEnabled -> TestTree
requestSetIdentityHeadersInNotificationsEnabled =
  req
    "SetIdentityHeadersInNotificationsEnabled"
    "fixture/SetIdentityHeadersInNotificationsEnabled.yaml"

requestGetSendQuota :: GetSendQuota -> TestTree
requestGetSendQuota =
  req
    "GetSendQuota"
    "fixture/GetSendQuota.yaml"

requestPutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptions -> TestTree
requestPutConfigurationSetDeliveryOptions =
  req
    "PutConfigurationSetDeliveryOptions"
    "fixture/PutConfigurationSetDeliveryOptions.yaml"

requestDescribeConfigurationSet :: DescribeConfigurationSet -> TestTree
requestDescribeConfigurationSet =
  req
    "DescribeConfigurationSet"
    "fixture/DescribeConfigurationSet.yaml"

requestPutIdentityPolicy :: PutIdentityPolicy -> TestTree
requestPutIdentityPolicy =
  req
    "PutIdentityPolicy"
    "fixture/PutIdentityPolicy.yaml"

requestDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplate -> TestTree
requestDeleteCustomVerificationEmailTemplate =
  req
    "DeleteCustomVerificationEmailTemplate"
    "fixture/DeleteCustomVerificationEmailTemplate.yaml"

requestDeleteIdentityPolicy :: DeleteIdentityPolicy -> TestTree
requestDeleteIdentityPolicy =
  req
    "DeleteIdentityPolicy"
    "fixture/DeleteIdentityPolicy.yaml"

requestUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplate -> TestTree
requestUpdateCustomVerificationEmailTemplate =
  req
    "UpdateCustomVerificationEmailTemplate"
    "fixture/UpdateCustomVerificationEmailTemplate.yaml"

requestSendCustomVerificationEmail :: SendCustomVerificationEmail -> TestTree
requestSendCustomVerificationEmail =
  req
    "SendCustomVerificationEmail"
    "fixture/SendCustomVerificationEmail.yaml"

requestGetIdentityNotificationAttributes :: GetIdentityNotificationAttributes -> TestTree
requestGetIdentityNotificationAttributes =
  req
    "GetIdentityNotificationAttributes"
    "fixture/GetIdentityNotificationAttributes.yaml"

requestUpdateConfigurationSetReputationMetricsEnabled :: UpdateConfigurationSetReputationMetricsEnabled -> TestTree
requestUpdateConfigurationSetReputationMetricsEnabled =
  req
    "UpdateConfigurationSetReputationMetricsEnabled"
    "fixture/UpdateConfigurationSetReputationMetricsEnabled.yaml"

requestListIdentityPolicies :: ListIdentityPolicies -> TestTree
requestListIdentityPolicies =
  req
    "ListIdentityPolicies"
    "fixture/ListIdentityPolicies.yaml"

requestSetIdentityDkimEnabled :: SetIdentityDkimEnabled -> TestTree
requestSetIdentityDkimEnabled =
  req
    "SetIdentityDkimEnabled"
    "fixture/SetIdentityDkimEnabled.yaml"

requestListReceiptFilters :: ListReceiptFilters -> TestTree
requestListReceiptFilters =
  req
    "ListReceiptFilters"
    "fixture/ListReceiptFilters.yaml"

requestDescribeReceiptRuleSet :: DescribeReceiptRuleSet -> TestTree
requestDescribeReceiptRuleSet =
  req
    "DescribeReceiptRuleSet"
    "fixture/DescribeReceiptRuleSet.yaml"

requestGetIdentityMailFromDomainAttributes :: GetIdentityMailFromDomainAttributes -> TestTree
requestGetIdentityMailFromDomainAttributes =
  req
    "GetIdentityMailFromDomainAttributes"
    "fixture/GetIdentityMailFromDomainAttributes.yaml"

requestCreateReceiptFilter :: CreateReceiptFilter -> TestTree
requestCreateReceiptFilter =
  req
    "CreateReceiptFilter"
    "fixture/CreateReceiptFilter.yaml"

requestUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestination -> TestTree
requestUpdateConfigurationSetEventDestination =
  req
    "UpdateConfigurationSetEventDestination"
    "fixture/UpdateConfigurationSetEventDestination.yaml"

requestDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestination -> TestTree
requestDeleteConfigurationSetEventDestination =
  req
    "DeleteConfigurationSetEventDestination"
    "fixture/DeleteConfigurationSetEventDestination.yaml"

requestSetIdentityMailFromDomain :: SetIdentityMailFromDomain -> TestTree
requestSetIdentityMailFromDomain =
  req
    "SetIdentityMailFromDomain"
    "fixture/SetIdentityMailFromDomain.yaml"

requestSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabled -> TestTree
requestSetIdentityFeedbackForwardingEnabled =
  req
    "SetIdentityFeedbackForwardingEnabled"
    "fixture/SetIdentityFeedbackForwardingEnabled.yaml"

requestListConfigurationSets :: ListConfigurationSets -> TestTree
requestListConfigurationSets =
  req
    "ListConfigurationSets"
    "fixture/ListConfigurationSets.yaml"

requestDeleteConfigurationSet :: DeleteConfigurationSet -> TestTree
requestDeleteConfigurationSet =
  req
    "DeleteConfigurationSet"
    "fixture/DeleteConfigurationSet.yaml"

requestGetIdentityVerificationAttributes :: GetIdentityVerificationAttributes -> TestTree
requestGetIdentityVerificationAttributes =
  req
    "GetIdentityVerificationAttributes"
    "fixture/GetIdentityVerificationAttributes.yaml"

requestGetIdentityPolicies :: GetIdentityPolicies -> TestTree
requestGetIdentityPolicies =
  req
    "GetIdentityPolicies"
    "fixture/GetIdentityPolicies.yaml"

requestListTemplates :: ListTemplates -> TestTree
requestListTemplates =
  req
    "ListTemplates"
    "fixture/ListTemplates.yaml"

requestVerifyDomainIdentity :: VerifyDomainIdentity -> TestTree
requestVerifyDomainIdentity =
  req
    "VerifyDomainIdentity"
    "fixture/VerifyDomainIdentity.yaml"

requestUpdateTemplate :: UpdateTemplate -> TestTree
requestUpdateTemplate =
  req
    "UpdateTemplate"
    "fixture/UpdateTemplate.yaml"

requestDeleteTemplate :: DeleteTemplate -> TestTree
requestDeleteTemplate =
  req
    "DeleteTemplate"
    "fixture/DeleteTemplate.yaml"

requestReorderReceiptRuleSet :: ReorderReceiptRuleSet -> TestTree
requestReorderReceiptRuleSet =
  req
    "ReorderReceiptRuleSet"
    "fixture/ReorderReceiptRuleSet.yaml"

requestListReceiptRuleSets :: ListReceiptRuleSets -> TestTree
requestListReceiptRuleSets =
  req
    "ListReceiptRuleSets"
    "fixture/ListReceiptRuleSets.yaml"

requestDeleteReceiptRuleSet :: DeleteReceiptRuleSet -> TestTree
requestDeleteReceiptRuleSet =
  req
    "DeleteReceiptRuleSet"
    "fixture/DeleteReceiptRuleSet.yaml"

requestSetReceiptRulePosition :: SetReceiptRulePosition -> TestTree
requestSetReceiptRulePosition =
  req
    "SetReceiptRulePosition"
    "fixture/SetReceiptRulePosition.yaml"

requestSendBounce :: SendBounce -> TestTree
requestSendBounce =
  req
    "SendBounce"
    "fixture/SendBounce.yaml"

requestGetIdentityDkimAttributes :: GetIdentityDkimAttributes -> TestTree
requestGetIdentityDkimAttributes =
  req
    "GetIdentityDkimAttributes"
    "fixture/GetIdentityDkimAttributes.yaml"

requestSendTemplatedEmail :: SendTemplatedEmail -> TestTree
requestSendTemplatedEmail =
  req
    "SendTemplatedEmail"
    "fixture/SendTemplatedEmail.yaml"

requestVerifyDomainDkim :: VerifyDomainDkim -> TestTree
requestVerifyDomainDkim =
  req
    "VerifyDomainDkim"
    "fixture/VerifyDomainDkim.yaml"

requestTestRenderTemplate :: TestRenderTemplate -> TestTree
requestTestRenderTemplate =
  req
    "TestRenderTemplate"
    "fixture/TestRenderTemplate.yaml"

requestSendBulkTemplatedEmail :: SendBulkTemplatedEmail -> TestTree
requestSendBulkTemplatedEmail =
  req
    "SendBulkTemplatedEmail"
    "fixture/SendBulkTemplatedEmail.yaml"

requestSendRawEmail :: SendRawEmail -> TestTree
requestSendRawEmail =
  req
    "SendRawEmail"
    "fixture/SendRawEmail.yaml"

requestGetSendStatistics :: GetSendStatistics -> TestTree
requestGetSendStatistics =
  req
    "GetSendStatistics"
    "fixture/GetSendStatistics.yaml"

requestListCustomVerificationEmailTemplates :: ListCustomVerificationEmailTemplates -> TestTree
requestListCustomVerificationEmailTemplates =
  req
    "ListCustomVerificationEmailTemplates"
    "fixture/ListCustomVerificationEmailTemplates.yaml"

requestDeleteIdentity :: DeleteIdentity -> TestTree
requestDeleteIdentity =
  req
    "DeleteIdentity"
    "fixture/DeleteIdentity.yaml"

requestDescribeReceiptRule :: DescribeReceiptRule -> TestTree
requestDescribeReceiptRule =
  req
    "DescribeReceiptRule"
    "fixture/DescribeReceiptRule.yaml"

requestListIdentities :: ListIdentities -> TestTree
requestListIdentities =
  req
    "ListIdentities"
    "fixture/ListIdentities.yaml"

requestUpdateConfigurationSetSendingEnabled :: UpdateConfigurationSetSendingEnabled -> TestTree
requestUpdateConfigurationSetSendingEnabled =
  req
    "UpdateConfigurationSetSendingEnabled"
    "fixture/UpdateConfigurationSetSendingEnabled.yaml"

requestCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplate -> TestTree
requestCreateCustomVerificationEmailTemplate =
  req
    "CreateCustomVerificationEmailTemplate"
    "fixture/CreateCustomVerificationEmailTemplate.yaml"

requestVerifyEmailIdentity :: VerifyEmailIdentity -> TestTree
requestVerifyEmailIdentity =
  req
    "VerifyEmailIdentity"
    "fixture/VerifyEmailIdentity.yaml"

requestVerifyEmailAddress :: VerifyEmailAddress -> TestTree
requestVerifyEmailAddress =
  req
    "VerifyEmailAddress"
    "fixture/VerifyEmailAddress.yaml"

requestDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddress -> TestTree
requestDeleteVerifiedEmailAddress =
  req
    "DeleteVerifiedEmailAddress"
    "fixture/DeleteVerifiedEmailAddress.yaml"

requestDeleteReceiptFilter :: DeleteReceiptFilter -> TestTree
requestDeleteReceiptFilter =
  req
    "DeleteReceiptFilter"
    "fixture/DeleteReceiptFilter.yaml"

requestListVerifiedEmailAddresses :: ListVerifiedEmailAddresses -> TestTree
requestListVerifiedEmailAddresses =
  req
    "ListVerifiedEmailAddresses"
    "fixture/ListVerifiedEmailAddresses.yaml"

requestGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplate -> TestTree
requestGetCustomVerificationEmailTemplate =
  req
    "GetCustomVerificationEmailTemplate"
    "fixture/GetCustomVerificationEmailTemplate.yaml"

requestSetIdentityNotificationTopic :: SetIdentityNotificationTopic -> TestTree
requestSetIdentityNotificationTopic =
  req
    "SetIdentityNotificationTopic"
    "fixture/SetIdentityNotificationTopic.yaml"

requestSendEmail :: SendEmail -> TestTree
requestSendEmail =
  req
    "SendEmail"
    "fixture/SendEmail.yaml"

requestDeleteReceiptRule :: DeleteReceiptRule -> TestTree
requestDeleteReceiptRule =
  req
    "DeleteReceiptRule"
    "fixture/DeleteReceiptRule.yaml"

requestUpdateReceiptRule :: UpdateReceiptRule -> TestTree
requestUpdateReceiptRule =
  req
    "UpdateReceiptRule"
    "fixture/UpdateReceiptRule.yaml"

requestCloneReceiptRuleSet :: CloneReceiptRuleSet -> TestTree
requestCloneReceiptRuleSet =
  req
    "CloneReceiptRuleSet"
    "fixture/CloneReceiptRuleSet.yaml"

requestCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestination -> TestTree
requestCreateConfigurationSetEventDestination =
  req
    "CreateConfigurationSetEventDestination"
    "fixture/CreateConfigurationSetEventDestination.yaml"

requestGetAccountSendingEnabled :: GetAccountSendingEnabled -> TestTree
requestGetAccountSendingEnabled =
  req
    "GetAccountSendingEnabled"
    "fixture/GetAccountSendingEnabled.yaml"

requestCreateReceiptRule :: CreateReceiptRule -> TestTree
requestCreateReceiptRule =
  req
    "CreateReceiptRule"
    "fixture/CreateReceiptRule.yaml"

requestGetTemplate :: GetTemplate -> TestTree
requestGetTemplate =
  req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

requestSetActiveReceiptRuleSet :: SetActiveReceiptRuleSet -> TestTree
requestSetActiveReceiptRuleSet =
  req
    "SetActiveReceiptRuleSet"
    "fixture/SetActiveReceiptRuleSet.yaml"

requestCreateConfigurationSet :: CreateConfigurationSet -> TestTree
requestCreateConfigurationSet =
  req
    "CreateConfigurationSet"
    "fixture/CreateConfigurationSet.yaml"

requestUpdateAccountSendingEnabled :: UpdateAccountSendingEnabled -> TestTree
requestUpdateAccountSendingEnabled =
  req
    "UpdateAccountSendingEnabled"
    "fixture/UpdateAccountSendingEnabled.yaml"

requestCreateConfigurationSetTrackingOptions :: CreateConfigurationSetTrackingOptions -> TestTree
requestCreateConfigurationSetTrackingOptions =
  req
    "CreateConfigurationSetTrackingOptions"
    "fixture/CreateConfigurationSetTrackingOptions.yaml"

requestDescribeActiveReceiptRuleSet :: DescribeActiveReceiptRuleSet -> TestTree
requestDescribeActiveReceiptRuleSet =
  req
    "DescribeActiveReceiptRuleSet"
    "fixture/DescribeActiveReceiptRuleSet.yaml"

-- Responses

responseCreateTemplate :: CreateTemplateResponse -> TestTree
responseCreateTemplate =
  res
    "CreateTemplateResponse"
    "fixture/CreateTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTemplate)

responseDeleteConfigurationSetTrackingOptions :: DeleteConfigurationSetTrackingOptionsResponse -> TestTree
responseDeleteConfigurationSetTrackingOptions =
  res
    "DeleteConfigurationSetTrackingOptionsResponse"
    "fixture/DeleteConfigurationSetTrackingOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteConfigurationSetTrackingOptions)

responseUpdateConfigurationSetTrackingOptions :: UpdateConfigurationSetTrackingOptionsResponse -> TestTree
responseUpdateConfigurationSetTrackingOptions =
  res
    "UpdateConfigurationSetTrackingOptionsResponse"
    "fixture/UpdateConfigurationSetTrackingOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateConfigurationSetTrackingOptions)

responseCreateReceiptRuleSet :: CreateReceiptRuleSetResponse -> TestTree
responseCreateReceiptRuleSet =
  res
    "CreateReceiptRuleSetResponse"
    "fixture/CreateReceiptRuleSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateReceiptRuleSet)

responseSetIdentityHeadersInNotificationsEnabled :: SetIdentityHeadersInNotificationsEnabledResponse -> TestTree
responseSetIdentityHeadersInNotificationsEnabled =
  res
    "SetIdentityHeadersInNotificationsEnabledResponse"
    "fixture/SetIdentityHeadersInNotificationsEnabledResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetIdentityHeadersInNotificationsEnabled)

responseGetSendQuota :: GetSendQuotaResponse -> TestTree
responseGetSendQuota =
  res
    "GetSendQuotaResponse"
    "fixture/GetSendQuotaResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSendQuota)

responsePutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptionsResponse -> TestTree
responsePutConfigurationSetDeliveryOptions =
  res
    "PutConfigurationSetDeliveryOptionsResponse"
    "fixture/PutConfigurationSetDeliveryOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutConfigurationSetDeliveryOptions)

responseDescribeConfigurationSet :: DescribeConfigurationSetResponse -> TestTree
responseDescribeConfigurationSet =
  res
    "DescribeConfigurationSetResponse"
    "fixture/DescribeConfigurationSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeConfigurationSet)

responsePutIdentityPolicy :: PutIdentityPolicyResponse -> TestTree
responsePutIdentityPolicy =
  res
    "PutIdentityPolicyResponse"
    "fixture/PutIdentityPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutIdentityPolicy)

responseDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplateResponse -> TestTree
responseDeleteCustomVerificationEmailTemplate =
  res
    "DeleteCustomVerificationEmailTemplateResponse"
    "fixture/DeleteCustomVerificationEmailTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCustomVerificationEmailTemplate)

responseDeleteIdentityPolicy :: DeleteIdentityPolicyResponse -> TestTree
responseDeleteIdentityPolicy =
  res
    "DeleteIdentityPolicyResponse"
    "fixture/DeleteIdentityPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteIdentityPolicy)

responseUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplateResponse -> TestTree
responseUpdateCustomVerificationEmailTemplate =
  res
    "UpdateCustomVerificationEmailTemplateResponse"
    "fixture/UpdateCustomVerificationEmailTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateCustomVerificationEmailTemplate)

responseSendCustomVerificationEmail :: SendCustomVerificationEmailResponse -> TestTree
responseSendCustomVerificationEmail =
  res
    "SendCustomVerificationEmailResponse"
    "fixture/SendCustomVerificationEmailResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendCustomVerificationEmail)

responseGetIdentityNotificationAttributes :: GetIdentityNotificationAttributesResponse -> TestTree
responseGetIdentityNotificationAttributes =
  res
    "GetIdentityNotificationAttributesResponse"
    "fixture/GetIdentityNotificationAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIdentityNotificationAttributes)

responseUpdateConfigurationSetReputationMetricsEnabled :: UpdateConfigurationSetReputationMetricsEnabledResponse -> TestTree
responseUpdateConfigurationSetReputationMetricsEnabled =
  res
    "UpdateConfigurationSetReputationMetricsEnabledResponse"
    "fixture/UpdateConfigurationSetReputationMetricsEnabledResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateConfigurationSetReputationMetricsEnabled)

responseListIdentityPolicies :: ListIdentityPoliciesResponse -> TestTree
responseListIdentityPolicies =
  res
    "ListIdentityPoliciesResponse"
    "fixture/ListIdentityPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListIdentityPolicies)

responseSetIdentityDkimEnabled :: SetIdentityDkimEnabledResponse -> TestTree
responseSetIdentityDkimEnabled =
  res
    "SetIdentityDkimEnabledResponse"
    "fixture/SetIdentityDkimEnabledResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetIdentityDkimEnabled)

responseListReceiptFilters :: ListReceiptFiltersResponse -> TestTree
responseListReceiptFilters =
  res
    "ListReceiptFiltersResponse"
    "fixture/ListReceiptFiltersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListReceiptFilters)

responseDescribeReceiptRuleSet :: DescribeReceiptRuleSetResponse -> TestTree
responseDescribeReceiptRuleSet =
  res
    "DescribeReceiptRuleSetResponse"
    "fixture/DescribeReceiptRuleSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReceiptRuleSet)

responseGetIdentityMailFromDomainAttributes :: GetIdentityMailFromDomainAttributesResponse -> TestTree
responseGetIdentityMailFromDomainAttributes =
  res
    "GetIdentityMailFromDomainAttributesResponse"
    "fixture/GetIdentityMailFromDomainAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIdentityMailFromDomainAttributes)

responseCreateReceiptFilter :: CreateReceiptFilterResponse -> TestTree
responseCreateReceiptFilter =
  res
    "CreateReceiptFilterResponse"
    "fixture/CreateReceiptFilterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateReceiptFilter)

responseUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestinationResponse -> TestTree
responseUpdateConfigurationSetEventDestination =
  res
    "UpdateConfigurationSetEventDestinationResponse"
    "fixture/UpdateConfigurationSetEventDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateConfigurationSetEventDestination)

responseDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestinationResponse -> TestTree
responseDeleteConfigurationSetEventDestination =
  res
    "DeleteConfigurationSetEventDestinationResponse"
    "fixture/DeleteConfigurationSetEventDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteConfigurationSetEventDestination)

responseSetIdentityMailFromDomain :: SetIdentityMailFromDomainResponse -> TestTree
responseSetIdentityMailFromDomain =
  res
    "SetIdentityMailFromDomainResponse"
    "fixture/SetIdentityMailFromDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetIdentityMailFromDomain)

responseSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
responseSetIdentityFeedbackForwardingEnabled =
  res
    "SetIdentityFeedbackForwardingEnabledResponse"
    "fixture/SetIdentityFeedbackForwardingEnabledResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetIdentityFeedbackForwardingEnabled)

responseListConfigurationSets :: ListConfigurationSetsResponse -> TestTree
responseListConfigurationSets =
  res
    "ListConfigurationSetsResponse"
    "fixture/ListConfigurationSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListConfigurationSets)

responseDeleteConfigurationSet :: DeleteConfigurationSetResponse -> TestTree
responseDeleteConfigurationSet =
  res
    "DeleteConfigurationSetResponse"
    "fixture/DeleteConfigurationSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteConfigurationSet)

responseGetIdentityVerificationAttributes :: GetIdentityVerificationAttributesResponse -> TestTree
responseGetIdentityVerificationAttributes =
  res
    "GetIdentityVerificationAttributesResponse"
    "fixture/GetIdentityVerificationAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIdentityVerificationAttributes)

responseGetIdentityPolicies :: GetIdentityPoliciesResponse -> TestTree
responseGetIdentityPolicies =
  res
    "GetIdentityPoliciesResponse"
    "fixture/GetIdentityPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIdentityPolicies)

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTemplates)

responseVerifyDomainIdentity :: VerifyDomainIdentityResponse -> TestTree
responseVerifyDomainIdentity =
  res
    "VerifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy VerifyDomainIdentity)

responseUpdateTemplate :: UpdateTemplateResponse -> TestTree
responseUpdateTemplate =
  res
    "UpdateTemplateResponse"
    "fixture/UpdateTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateTemplate)

responseDeleteTemplate :: DeleteTemplateResponse -> TestTree
responseDeleteTemplate =
  res
    "DeleteTemplateResponse"
    "fixture/DeleteTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTemplate)

responseReorderReceiptRuleSet :: ReorderReceiptRuleSetResponse -> TestTree
responseReorderReceiptRuleSet =
  res
    "ReorderReceiptRuleSetResponse"
    "fixture/ReorderReceiptRuleSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReorderReceiptRuleSet)

responseListReceiptRuleSets :: ListReceiptRuleSetsResponse -> TestTree
responseListReceiptRuleSets =
  res
    "ListReceiptRuleSetsResponse"
    "fixture/ListReceiptRuleSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListReceiptRuleSets)

responseDeleteReceiptRuleSet :: DeleteReceiptRuleSetResponse -> TestTree
responseDeleteReceiptRuleSet =
  res
    "DeleteReceiptRuleSetResponse"
    "fixture/DeleteReceiptRuleSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteReceiptRuleSet)

responseSetReceiptRulePosition :: SetReceiptRulePositionResponse -> TestTree
responseSetReceiptRulePosition =
  res
    "SetReceiptRulePositionResponse"
    "fixture/SetReceiptRulePositionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetReceiptRulePosition)

responseSendBounce :: SendBounceResponse -> TestTree
responseSendBounce =
  res
    "SendBounceResponse"
    "fixture/SendBounceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendBounce)

responseGetIdentityDkimAttributes :: GetIdentityDkimAttributesResponse -> TestTree
responseGetIdentityDkimAttributes =
  res
    "GetIdentityDkimAttributesResponse"
    "fixture/GetIdentityDkimAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIdentityDkimAttributes)

responseSendTemplatedEmail :: SendTemplatedEmailResponse -> TestTree
responseSendTemplatedEmail =
  res
    "SendTemplatedEmailResponse"
    "fixture/SendTemplatedEmailResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendTemplatedEmail)

responseVerifyDomainDkim :: VerifyDomainDkimResponse -> TestTree
responseVerifyDomainDkim =
  res
    "VerifyDomainDkimResponse"
    "fixture/VerifyDomainDkimResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy VerifyDomainDkim)

responseTestRenderTemplate :: TestRenderTemplateResponse -> TestTree
responseTestRenderTemplate =
  res
    "TestRenderTemplateResponse"
    "fixture/TestRenderTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TestRenderTemplate)

responseSendBulkTemplatedEmail :: SendBulkTemplatedEmailResponse -> TestTree
responseSendBulkTemplatedEmail =
  res
    "SendBulkTemplatedEmailResponse"
    "fixture/SendBulkTemplatedEmailResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendBulkTemplatedEmail)

responseSendRawEmail :: SendRawEmailResponse -> TestTree
responseSendRawEmail =
  res
    "SendRawEmailResponse"
    "fixture/SendRawEmailResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendRawEmail)

responseGetSendStatistics :: GetSendStatisticsResponse -> TestTree
responseGetSendStatistics =
  res
    "GetSendStatisticsResponse"
    "fixture/GetSendStatisticsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSendStatistics)

responseListCustomVerificationEmailTemplates :: ListCustomVerificationEmailTemplatesResponse -> TestTree
responseListCustomVerificationEmailTemplates =
  res
    "ListCustomVerificationEmailTemplatesResponse"
    "fixture/ListCustomVerificationEmailTemplatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCustomVerificationEmailTemplates)

responseDeleteIdentity :: DeleteIdentityResponse -> TestTree
responseDeleteIdentity =
  res
    "DeleteIdentityResponse"
    "fixture/DeleteIdentityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteIdentity)

responseDescribeReceiptRule :: DescribeReceiptRuleResponse -> TestTree
responseDescribeReceiptRule =
  res
    "DescribeReceiptRuleResponse"
    "fixture/DescribeReceiptRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReceiptRule)

responseListIdentities :: ListIdentitiesResponse -> TestTree
responseListIdentities =
  res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListIdentities)

responseUpdateConfigurationSetSendingEnabled :: UpdateConfigurationSetSendingEnabledResponse -> TestTree
responseUpdateConfigurationSetSendingEnabled =
  res
    "UpdateConfigurationSetSendingEnabledResponse"
    "fixture/UpdateConfigurationSetSendingEnabledResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateConfigurationSetSendingEnabled)

responseCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplateResponse -> TestTree
responseCreateCustomVerificationEmailTemplate =
  res
    "CreateCustomVerificationEmailTemplateResponse"
    "fixture/CreateCustomVerificationEmailTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCustomVerificationEmailTemplate)

responseVerifyEmailIdentity :: VerifyEmailIdentityResponse -> TestTree
responseVerifyEmailIdentity =
  res
    "VerifyEmailIdentityResponse"
    "fixture/VerifyEmailIdentityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy VerifyEmailIdentity)

responseVerifyEmailAddress :: VerifyEmailAddressResponse -> TestTree
responseVerifyEmailAddress =
  res
    "VerifyEmailAddressResponse"
    "fixture/VerifyEmailAddressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy VerifyEmailAddress)

responseDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddressResponse -> TestTree
responseDeleteVerifiedEmailAddress =
  res
    "DeleteVerifiedEmailAddressResponse"
    "fixture/DeleteVerifiedEmailAddressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVerifiedEmailAddress)

responseDeleteReceiptFilter :: DeleteReceiptFilterResponse -> TestTree
responseDeleteReceiptFilter =
  res
    "DeleteReceiptFilterResponse"
    "fixture/DeleteReceiptFilterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteReceiptFilter)

responseListVerifiedEmailAddresses :: ListVerifiedEmailAddressesResponse -> TestTree
responseListVerifiedEmailAddresses =
  res
    "ListVerifiedEmailAddressesResponse"
    "fixture/ListVerifiedEmailAddressesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListVerifiedEmailAddresses)

responseGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplateResponse -> TestTree
responseGetCustomVerificationEmailTemplate =
  res
    "GetCustomVerificationEmailTemplateResponse"
    "fixture/GetCustomVerificationEmailTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCustomVerificationEmailTemplate)

responseSetIdentityNotificationTopic :: SetIdentityNotificationTopicResponse -> TestTree
responseSetIdentityNotificationTopic =
  res
    "SetIdentityNotificationTopicResponse"
    "fixture/SetIdentityNotificationTopicResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetIdentityNotificationTopic)

responseSendEmail :: SendEmailResponse -> TestTree
responseSendEmail =
  res
    "SendEmailResponse"
    "fixture/SendEmailResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendEmail)

responseDeleteReceiptRule :: DeleteReceiptRuleResponse -> TestTree
responseDeleteReceiptRule =
  res
    "DeleteReceiptRuleResponse"
    "fixture/DeleteReceiptRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteReceiptRule)

responseUpdateReceiptRule :: UpdateReceiptRuleResponse -> TestTree
responseUpdateReceiptRule =
  res
    "UpdateReceiptRuleResponse"
    "fixture/UpdateReceiptRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateReceiptRule)

responseCloneReceiptRuleSet :: CloneReceiptRuleSetResponse -> TestTree
responseCloneReceiptRuleSet =
  res
    "CloneReceiptRuleSetResponse"
    "fixture/CloneReceiptRuleSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CloneReceiptRuleSet)

responseCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestinationResponse -> TestTree
responseCreateConfigurationSetEventDestination =
  res
    "CreateConfigurationSetEventDestinationResponse"
    "fixture/CreateConfigurationSetEventDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateConfigurationSetEventDestination)

responseGetAccountSendingEnabled :: GetAccountSendingEnabledResponse -> TestTree
responseGetAccountSendingEnabled =
  res
    "GetAccountSendingEnabledResponse"
    "fixture/GetAccountSendingEnabledResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAccountSendingEnabled)

responseCreateReceiptRule :: CreateReceiptRuleResponse -> TestTree
responseCreateReceiptRule =
  res
    "CreateReceiptRuleResponse"
    "fixture/CreateReceiptRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateReceiptRule)

responseGetTemplate :: GetTemplateResponse -> TestTree
responseGetTemplate =
  res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTemplate)

responseSetActiveReceiptRuleSet :: SetActiveReceiptRuleSetResponse -> TestTree
responseSetActiveReceiptRuleSet =
  res
    "SetActiveReceiptRuleSetResponse"
    "fixture/SetActiveReceiptRuleSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetActiveReceiptRuleSet)

responseCreateConfigurationSet :: CreateConfigurationSetResponse -> TestTree
responseCreateConfigurationSet =
  res
    "CreateConfigurationSetResponse"
    "fixture/CreateConfigurationSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateConfigurationSet)

responseUpdateAccountSendingEnabled :: UpdateAccountSendingEnabledResponse -> TestTree
responseUpdateAccountSendingEnabled =
  res
    "UpdateAccountSendingEnabledResponse"
    "fixture/UpdateAccountSendingEnabledResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAccountSendingEnabled)

responseCreateConfigurationSetTrackingOptions :: CreateConfigurationSetTrackingOptionsResponse -> TestTree
responseCreateConfigurationSetTrackingOptions =
  res
    "CreateConfigurationSetTrackingOptionsResponse"
    "fixture/CreateConfigurationSetTrackingOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateConfigurationSetTrackingOptions)

responseDescribeActiveReceiptRuleSet :: DescribeActiveReceiptRuleSetResponse -> TestTree
responseDescribeActiveReceiptRuleSet =
  res
    "DescribeActiveReceiptRuleSetResponse"
    "fixture/DescribeActiveReceiptRuleSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeActiveReceiptRuleSet)
