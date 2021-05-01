{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SES
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestGetSendStatistics $
--             newGetSendStatistics
--
--         , requestDescribeConfigurationSet $
--             newDescribeConfigurationSet
--
--         , requestPutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptions
--
--         , requestDeleteIdentityPolicy $
--             newDeleteIdentityPolicy
--
--         , requestDescribeReceiptRule $
--             newDescribeReceiptRule
--
--         , requestCreateTemplate $
--             newCreateTemplate
--
--         , requestGetIdentityDkimAttributes $
--             newGetIdentityDkimAttributes
--
--         , requestCreateReceiptRuleSet $
--             newCreateReceiptRuleSet
--
--         , requestGetSendQuota $
--             newGetSendQuota
--
--         , requestSetIdentityHeadersInNotificationsEnabled $
--             newSetIdentityHeadersInNotificationsEnabled
--
--         , requestVerifyDomainIdentity $
--             newVerifyDomainIdentity
--
--         , requestUpdateTemplate $
--             newUpdateTemplate
--
--         , requestDeleteTemplate $
--             newDeleteTemplate
--
--         , requestCreateConfigurationSetTrackingOptions $
--             newCreateConfigurationSetTrackingOptions
--
--         , requestDeleteReceiptRuleSet $
--             newDeleteReceiptRuleSet
--
--         , requestSetReceiptRulePosition $
--             newSetReceiptRulePosition
--
--         , requestUpdateAccountSendingEnabled $
--             newUpdateAccountSendingEnabled
--
--         , requestGetIdentityVerificationAttributes $
--             newGetIdentityVerificationAttributes
--
--         , requestGetIdentityPolicies $
--             newGetIdentityPolicies
--
--         , requestCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestination
--
--         , requestGetAccountSendingEnabled $
--             newGetAccountSendingEnabled
--
--         , requestCreateConfigurationSet $
--             newCreateConfigurationSet
--
--         , requestDeleteConfigurationSet $
--             newDeleteConfigurationSet
--
--         , requestDeleteReceiptRule $
--             newDeleteReceiptRule
--
--         , requestSetIdentityFeedbackForwardingEnabled $
--             newSetIdentityFeedbackForwardingEnabled
--
--         , requestCloneReceiptRuleSet $
--             newCloneReceiptRuleSet
--
--         , requestUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestination
--
--         , requestUpdateReceiptRule $
--             newUpdateReceiptRule
--
--         , requestDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestination
--
--         , requestSendEmail $
--             newSendEmail
--
--         , requestDeleteVerifiedEmailAddress $
--             newDeleteVerifiedEmailAddress
--
--         , requestVerifyEmailAddress $
--             newVerifyEmailAddress
--
--         , requestCreateCustomVerificationEmailTemplate $
--             newCreateCustomVerificationEmailTemplate
--
--         , requestListIdentityPolicies $
--             newListIdentityPolicies
--
--         , requestSetIdentityDkimEnabled $
--             newSetIdentityDkimEnabled
--
--         , requestUpdateConfigurationSetReputationMetricsEnabled $
--             newUpdateConfigurationSetReputationMetricsEnabled
--
--         , requestListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplates
--
--         , requestDeleteIdentity $
--             newDeleteIdentity
--
--         , requestDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplate
--
--         , requestPutIdentityPolicy $
--             newPutIdentityPolicy
--
--         , requestUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplate
--
--         , requestDeleteConfigurationSetTrackingOptions $
--             newDeleteConfigurationSetTrackingOptions
--
--         , requestSendBulkTemplatedEmail $
--             newSendBulkTemplatedEmail
--
--         , requestVerifyDomainDkim $
--             newVerifyDomainDkim
--
--         , requestSendRawEmail $
--             newSendRawEmail
--
--         , requestTestRenderTemplate $
--             newTestRenderTemplate
--
--         , requestSendBounce $
--             newSendBounce
--
--         , requestUpdateConfigurationSetTrackingOptions $
--             newUpdateConfigurationSetTrackingOptions
--
--         , requestSendTemplatedEmail $
--             newSendTemplatedEmail
--
--         , requestListReceiptRuleSets $
--             newListReceiptRuleSets
--
--         , requestReorderReceiptRuleSet $
--             newReorderReceiptRuleSet
--
--         , requestListTemplates $
--             newListTemplates
--
--         , requestDescribeActiveReceiptRuleSet $
--             newDescribeActiveReceiptRuleSet
--
--         , requestCreateReceiptRule $
--             newCreateReceiptRule
--
--         , requestGetTemplate $
--             newGetTemplate
--
--         , requestSetActiveReceiptRuleSet $
--             newSetActiveReceiptRuleSet
--
--         , requestListConfigurationSets $
--             newListConfigurationSets
--
--         , requestSetIdentityMailFromDomain $
--             newSetIdentityMailFromDomain
--
--         , requestGetIdentityMailFromDomainAttributes $
--             newGetIdentityMailFromDomainAttributes
--
--         , requestSetIdentityNotificationTopic $
--             newSetIdentityNotificationTopic
--
--         , requestGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplate
--
--         , requestCreateReceiptFilter $
--             newCreateReceiptFilter
--
--         , requestListVerifiedEmailAddresses $
--             newListVerifiedEmailAddresses
--
--         , requestListReceiptFilters $
--             newListReceiptFilters
--
--         , requestDeleteReceiptFilter $
--             newDeleteReceiptFilter
--
--         , requestDescribeReceiptRuleSet $
--             newDescribeReceiptRuleSet
--
--         , requestVerifyEmailIdentity $
--             newVerifyEmailIdentity
--
--         , requestSendCustomVerificationEmail $
--             newSendCustomVerificationEmail
--
--         , requestListIdentities $
--             newListIdentities
--
--         , requestGetIdentityNotificationAttributes $
--             newGetIdentityNotificationAttributes
--
--         , requestUpdateConfigurationSetSendingEnabled $
--             newUpdateConfigurationSetSendingEnabled
--
--           ]

--     , testGroup "response"
--         [ responseGetSendStatistics $
--             newGetSendStatisticsResponse
--
--         , responseDescribeConfigurationSet $
--             newDescribeConfigurationSetResponse
--
--         , responsePutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptionsResponse
--
--         , responseDeleteIdentityPolicy $
--             newDeleteIdentityPolicyResponse
--
--         , responseDescribeReceiptRule $
--             newDescribeReceiptRuleResponse
--
--         , responseCreateTemplate $
--             newCreateTemplateResponse
--
--         , responseGetIdentityDkimAttributes $
--             newGetIdentityDkimAttributesResponse
--
--         , responseCreateReceiptRuleSet $
--             newCreateReceiptRuleSetResponse
--
--         , responseGetSendQuota $
--             newGetSendQuotaResponse
--
--         , responseSetIdentityHeadersInNotificationsEnabled $
--             newSetIdentityHeadersInNotificationsEnabledResponse
--
--         , responseVerifyDomainIdentity $
--             newVerifyDomainIdentityResponse
--
--         , responseUpdateTemplate $
--             newUpdateTemplateResponse
--
--         , responseDeleteTemplate $
--             newDeleteTemplateResponse
--
--         , responseCreateConfigurationSetTrackingOptions $
--             newCreateConfigurationSetTrackingOptionsResponse
--
--         , responseDeleteReceiptRuleSet $
--             newDeleteReceiptRuleSetResponse
--
--         , responseSetReceiptRulePosition $
--             newSetReceiptRulePositionResponse
--
--         , responseUpdateAccountSendingEnabled $
--             newUpdateAccountSendingEnabledResponse
--
--         , responseGetIdentityVerificationAttributes $
--             newGetIdentityVerificationAttributesResponse
--
--         , responseGetIdentityPolicies $
--             newGetIdentityPoliciesResponse
--
--         , responseCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestinationResponse
--
--         , responseGetAccountSendingEnabled $
--             newGetAccountSendingEnabledResponse
--
--         , responseCreateConfigurationSet $
--             newCreateConfigurationSetResponse
--
--         , responseDeleteConfigurationSet $
--             newDeleteConfigurationSetResponse
--
--         , responseDeleteReceiptRule $
--             newDeleteReceiptRuleResponse
--
--         , responseSetIdentityFeedbackForwardingEnabled $
--             newSetIdentityFeedbackForwardingEnabledResponse
--
--         , responseCloneReceiptRuleSet $
--             newCloneReceiptRuleSetResponse
--
--         , responseUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestinationResponse
--
--         , responseUpdateReceiptRule $
--             newUpdateReceiptRuleResponse
--
--         , responseDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestinationResponse
--
--         , responseSendEmail $
--             newSendEmailResponse
--
--         , responseDeleteVerifiedEmailAddress $
--             newDeleteVerifiedEmailAddressResponse
--
--         , responseVerifyEmailAddress $
--             newVerifyEmailAddressResponse
--
--         , responseCreateCustomVerificationEmailTemplate $
--             newCreateCustomVerificationEmailTemplateResponse
--
--         , responseListIdentityPolicies $
--             newListIdentityPoliciesResponse
--
--         , responseSetIdentityDkimEnabled $
--             newSetIdentityDkimEnabledResponse
--
--         , responseUpdateConfigurationSetReputationMetricsEnabled $
--             newUpdateConfigurationSetReputationMetricsEnabledResponse
--
--         , responseListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplatesResponse
--
--         , responseDeleteIdentity $
--             newDeleteIdentityResponse
--
--         , responseDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplateResponse
--
--         , responsePutIdentityPolicy $
--             newPutIdentityPolicyResponse
--
--         , responseUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplateResponse
--
--         , responseDeleteConfigurationSetTrackingOptions $
--             newDeleteConfigurationSetTrackingOptionsResponse
--
--         , responseSendBulkTemplatedEmail $
--             newSendBulkTemplatedEmailResponse
--
--         , responseVerifyDomainDkim $
--             newVerifyDomainDkimResponse
--
--         , responseSendRawEmail $
--             newSendRawEmailResponse
--
--         , responseTestRenderTemplate $
--             newTestRenderTemplateResponse
--
--         , responseSendBounce $
--             newSendBounceResponse
--
--         , responseUpdateConfigurationSetTrackingOptions $
--             newUpdateConfigurationSetTrackingOptionsResponse
--
--         , responseSendTemplatedEmail $
--             newSendTemplatedEmailResponse
--
--         , responseListReceiptRuleSets $
--             newListReceiptRuleSetsResponse
--
--         , responseReorderReceiptRuleSet $
--             newReorderReceiptRuleSetResponse
--
--         , responseListTemplates $
--             newListTemplatesResponse
--
--         , responseDescribeActiveReceiptRuleSet $
--             newDescribeActiveReceiptRuleSetResponse
--
--         , responseCreateReceiptRule $
--             newCreateReceiptRuleResponse
--
--         , responseGetTemplate $
--             newGetTemplateResponse
--
--         , responseSetActiveReceiptRuleSet $
--             newSetActiveReceiptRuleSetResponse
--
--         , responseListConfigurationSets $
--             newListConfigurationSetsResponse
--
--         , responseSetIdentityMailFromDomain $
--             newSetIdentityMailFromDomainResponse
--
--         , responseGetIdentityMailFromDomainAttributes $
--             newGetIdentityMailFromDomainAttributesResponse
--
--         , responseSetIdentityNotificationTopic $
--             newSetIdentityNotificationTopicResponse
--
--         , responseGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplateResponse
--
--         , responseCreateReceiptFilter $
--             newCreateReceiptFilterResponse
--
--         , responseListVerifiedEmailAddresses $
--             newListVerifiedEmailAddressesResponse
--
--         , responseListReceiptFilters $
--             newListReceiptFiltersResponse
--
--         , responseDeleteReceiptFilter $
--             newDeleteReceiptFilterResponse
--
--         , responseDescribeReceiptRuleSet $
--             newDescribeReceiptRuleSetResponse
--
--         , responseVerifyEmailIdentity $
--             newVerifyEmailIdentityResponse
--
--         , responseSendCustomVerificationEmail $
--             newSendCustomVerificationEmailResponse
--
--         , responseListIdentities $
--             newListIdentitiesResponse
--
--         , responseGetIdentityNotificationAttributes $
--             newGetIdentityNotificationAttributesResponse
--
--         , responseUpdateConfigurationSetSendingEnabled $
--             newUpdateConfigurationSetSendingEnabledResponse
--
--           ]
--     ]

-- Requests

requestGetSendStatistics :: GetSendStatistics -> TestTree
requestGetSendStatistics =
  req
    "GetSendStatistics"
    "fixture/GetSendStatistics.yaml"

requestDescribeConfigurationSet :: DescribeConfigurationSet -> TestTree
requestDescribeConfigurationSet =
  req
    "DescribeConfigurationSet"
    "fixture/DescribeConfigurationSet.yaml"

requestPutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptions -> TestTree
requestPutConfigurationSetDeliveryOptions =
  req
    "PutConfigurationSetDeliveryOptions"
    "fixture/PutConfigurationSetDeliveryOptions.yaml"

requestDeleteIdentityPolicy :: DeleteIdentityPolicy -> TestTree
requestDeleteIdentityPolicy =
  req
    "DeleteIdentityPolicy"
    "fixture/DeleteIdentityPolicy.yaml"

requestDescribeReceiptRule :: DescribeReceiptRule -> TestTree
requestDescribeReceiptRule =
  req
    "DescribeReceiptRule"
    "fixture/DescribeReceiptRule.yaml"

requestCreateTemplate :: CreateTemplate -> TestTree
requestCreateTemplate =
  req
    "CreateTemplate"
    "fixture/CreateTemplate.yaml"

requestGetIdentityDkimAttributes :: GetIdentityDkimAttributes -> TestTree
requestGetIdentityDkimAttributes =
  req
    "GetIdentityDkimAttributes"
    "fixture/GetIdentityDkimAttributes.yaml"

requestCreateReceiptRuleSet :: CreateReceiptRuleSet -> TestTree
requestCreateReceiptRuleSet =
  req
    "CreateReceiptRuleSet"
    "fixture/CreateReceiptRuleSet.yaml"

requestGetSendQuota :: GetSendQuota -> TestTree
requestGetSendQuota =
  req
    "GetSendQuota"
    "fixture/GetSendQuota.yaml"

requestSetIdentityHeadersInNotificationsEnabled :: SetIdentityHeadersInNotificationsEnabled -> TestTree
requestSetIdentityHeadersInNotificationsEnabled =
  req
    "SetIdentityHeadersInNotificationsEnabled"
    "fixture/SetIdentityHeadersInNotificationsEnabled.yaml"

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

requestCreateConfigurationSetTrackingOptions :: CreateConfigurationSetTrackingOptions -> TestTree
requestCreateConfigurationSetTrackingOptions =
  req
    "CreateConfigurationSetTrackingOptions"
    "fixture/CreateConfigurationSetTrackingOptions.yaml"

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

requestUpdateAccountSendingEnabled :: UpdateAccountSendingEnabled -> TestTree
requestUpdateAccountSendingEnabled =
  req
    "UpdateAccountSendingEnabled"
    "fixture/UpdateAccountSendingEnabled.yaml"

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

requestCreateConfigurationSet :: CreateConfigurationSet -> TestTree
requestCreateConfigurationSet =
  req
    "CreateConfigurationSet"
    "fixture/CreateConfigurationSet.yaml"

requestDeleteConfigurationSet :: DeleteConfigurationSet -> TestTree
requestDeleteConfigurationSet =
  req
    "DeleteConfigurationSet"
    "fixture/DeleteConfigurationSet.yaml"

requestDeleteReceiptRule :: DeleteReceiptRule -> TestTree
requestDeleteReceiptRule =
  req
    "DeleteReceiptRule"
    "fixture/DeleteReceiptRule.yaml"

requestSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabled -> TestTree
requestSetIdentityFeedbackForwardingEnabled =
  req
    "SetIdentityFeedbackForwardingEnabled"
    "fixture/SetIdentityFeedbackForwardingEnabled.yaml"

requestCloneReceiptRuleSet :: CloneReceiptRuleSet -> TestTree
requestCloneReceiptRuleSet =
  req
    "CloneReceiptRuleSet"
    "fixture/CloneReceiptRuleSet.yaml"

requestUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestination -> TestTree
requestUpdateConfigurationSetEventDestination =
  req
    "UpdateConfigurationSetEventDestination"
    "fixture/UpdateConfigurationSetEventDestination.yaml"

requestUpdateReceiptRule :: UpdateReceiptRule -> TestTree
requestUpdateReceiptRule =
  req
    "UpdateReceiptRule"
    "fixture/UpdateReceiptRule.yaml"

requestDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestination -> TestTree
requestDeleteConfigurationSetEventDestination =
  req
    "DeleteConfigurationSetEventDestination"
    "fixture/DeleteConfigurationSetEventDestination.yaml"

requestSendEmail :: SendEmail -> TestTree
requestSendEmail =
  req
    "SendEmail"
    "fixture/SendEmail.yaml"

requestDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddress -> TestTree
requestDeleteVerifiedEmailAddress =
  req
    "DeleteVerifiedEmailAddress"
    "fixture/DeleteVerifiedEmailAddress.yaml"

requestVerifyEmailAddress :: VerifyEmailAddress -> TestTree
requestVerifyEmailAddress =
  req
    "VerifyEmailAddress"
    "fixture/VerifyEmailAddress.yaml"

requestCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplate -> TestTree
requestCreateCustomVerificationEmailTemplate =
  req
    "CreateCustomVerificationEmailTemplate"
    "fixture/CreateCustomVerificationEmailTemplate.yaml"

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

requestUpdateConfigurationSetReputationMetricsEnabled :: UpdateConfigurationSetReputationMetricsEnabled -> TestTree
requestUpdateConfigurationSetReputationMetricsEnabled =
  req
    "UpdateConfigurationSetReputationMetricsEnabled"
    "fixture/UpdateConfigurationSetReputationMetricsEnabled.yaml"

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

requestDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplate -> TestTree
requestDeleteCustomVerificationEmailTemplate =
  req
    "DeleteCustomVerificationEmailTemplate"
    "fixture/DeleteCustomVerificationEmailTemplate.yaml"

requestPutIdentityPolicy :: PutIdentityPolicy -> TestTree
requestPutIdentityPolicy =
  req
    "PutIdentityPolicy"
    "fixture/PutIdentityPolicy.yaml"

requestUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplate -> TestTree
requestUpdateCustomVerificationEmailTemplate =
  req
    "UpdateCustomVerificationEmailTemplate"
    "fixture/UpdateCustomVerificationEmailTemplate.yaml"

requestDeleteConfigurationSetTrackingOptions :: DeleteConfigurationSetTrackingOptions -> TestTree
requestDeleteConfigurationSetTrackingOptions =
  req
    "DeleteConfigurationSetTrackingOptions"
    "fixture/DeleteConfigurationSetTrackingOptions.yaml"

requestSendBulkTemplatedEmail :: SendBulkTemplatedEmail -> TestTree
requestSendBulkTemplatedEmail =
  req
    "SendBulkTemplatedEmail"
    "fixture/SendBulkTemplatedEmail.yaml"

requestVerifyDomainDkim :: VerifyDomainDkim -> TestTree
requestVerifyDomainDkim =
  req
    "VerifyDomainDkim"
    "fixture/VerifyDomainDkim.yaml"

requestSendRawEmail :: SendRawEmail -> TestTree
requestSendRawEmail =
  req
    "SendRawEmail"
    "fixture/SendRawEmail.yaml"

requestTestRenderTemplate :: TestRenderTemplate -> TestTree
requestTestRenderTemplate =
  req
    "TestRenderTemplate"
    "fixture/TestRenderTemplate.yaml"

requestSendBounce :: SendBounce -> TestTree
requestSendBounce =
  req
    "SendBounce"
    "fixture/SendBounce.yaml"

requestUpdateConfigurationSetTrackingOptions :: UpdateConfigurationSetTrackingOptions -> TestTree
requestUpdateConfigurationSetTrackingOptions =
  req
    "UpdateConfigurationSetTrackingOptions"
    "fixture/UpdateConfigurationSetTrackingOptions.yaml"

requestSendTemplatedEmail :: SendTemplatedEmail -> TestTree
requestSendTemplatedEmail =
  req
    "SendTemplatedEmail"
    "fixture/SendTemplatedEmail.yaml"

requestListReceiptRuleSets :: ListReceiptRuleSets -> TestTree
requestListReceiptRuleSets =
  req
    "ListReceiptRuleSets"
    "fixture/ListReceiptRuleSets.yaml"

requestReorderReceiptRuleSet :: ReorderReceiptRuleSet -> TestTree
requestReorderReceiptRuleSet =
  req
    "ReorderReceiptRuleSet"
    "fixture/ReorderReceiptRuleSet.yaml"

requestListTemplates :: ListTemplates -> TestTree
requestListTemplates =
  req
    "ListTemplates"
    "fixture/ListTemplates.yaml"

requestDescribeActiveReceiptRuleSet :: DescribeActiveReceiptRuleSet -> TestTree
requestDescribeActiveReceiptRuleSet =
  req
    "DescribeActiveReceiptRuleSet"
    "fixture/DescribeActiveReceiptRuleSet.yaml"

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

requestListConfigurationSets :: ListConfigurationSets -> TestTree
requestListConfigurationSets =
  req
    "ListConfigurationSets"
    "fixture/ListConfigurationSets.yaml"

requestSetIdentityMailFromDomain :: SetIdentityMailFromDomain -> TestTree
requestSetIdentityMailFromDomain =
  req
    "SetIdentityMailFromDomain"
    "fixture/SetIdentityMailFromDomain.yaml"

requestGetIdentityMailFromDomainAttributes :: GetIdentityMailFromDomainAttributes -> TestTree
requestGetIdentityMailFromDomainAttributes =
  req
    "GetIdentityMailFromDomainAttributes"
    "fixture/GetIdentityMailFromDomainAttributes.yaml"

requestSetIdentityNotificationTopic :: SetIdentityNotificationTopic -> TestTree
requestSetIdentityNotificationTopic =
  req
    "SetIdentityNotificationTopic"
    "fixture/SetIdentityNotificationTopic.yaml"

requestGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplate -> TestTree
requestGetCustomVerificationEmailTemplate =
  req
    "GetCustomVerificationEmailTemplate"
    "fixture/GetCustomVerificationEmailTemplate.yaml"

requestCreateReceiptFilter :: CreateReceiptFilter -> TestTree
requestCreateReceiptFilter =
  req
    "CreateReceiptFilter"
    "fixture/CreateReceiptFilter.yaml"

requestListVerifiedEmailAddresses :: ListVerifiedEmailAddresses -> TestTree
requestListVerifiedEmailAddresses =
  req
    "ListVerifiedEmailAddresses"
    "fixture/ListVerifiedEmailAddresses.yaml"

requestListReceiptFilters :: ListReceiptFilters -> TestTree
requestListReceiptFilters =
  req
    "ListReceiptFilters"
    "fixture/ListReceiptFilters.yaml"

requestDeleteReceiptFilter :: DeleteReceiptFilter -> TestTree
requestDeleteReceiptFilter =
  req
    "DeleteReceiptFilter"
    "fixture/DeleteReceiptFilter.yaml"

requestDescribeReceiptRuleSet :: DescribeReceiptRuleSet -> TestTree
requestDescribeReceiptRuleSet =
  req
    "DescribeReceiptRuleSet"
    "fixture/DescribeReceiptRuleSet.yaml"

requestVerifyEmailIdentity :: VerifyEmailIdentity -> TestTree
requestVerifyEmailIdentity =
  req
    "VerifyEmailIdentity"
    "fixture/VerifyEmailIdentity.yaml"

requestSendCustomVerificationEmail :: SendCustomVerificationEmail -> TestTree
requestSendCustomVerificationEmail =
  req
    "SendCustomVerificationEmail"
    "fixture/SendCustomVerificationEmail.yaml"

requestListIdentities :: ListIdentities -> TestTree
requestListIdentities =
  req
    "ListIdentities"
    "fixture/ListIdentities.yaml"

requestGetIdentityNotificationAttributes :: GetIdentityNotificationAttributes -> TestTree
requestGetIdentityNotificationAttributes =
  req
    "GetIdentityNotificationAttributes"
    "fixture/GetIdentityNotificationAttributes.yaml"

requestUpdateConfigurationSetSendingEnabled :: UpdateConfigurationSetSendingEnabled -> TestTree
requestUpdateConfigurationSetSendingEnabled =
  req
    "UpdateConfigurationSetSendingEnabled"
    "fixture/UpdateConfigurationSetSendingEnabled.yaml"

-- Responses

responseGetSendStatistics :: GetSendStatisticsResponse -> TestTree
responseGetSendStatistics =
  res
    "GetSendStatisticsResponse"
    "fixture/GetSendStatisticsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSendStatistics)

responseDescribeConfigurationSet :: DescribeConfigurationSetResponse -> TestTree
responseDescribeConfigurationSet =
  res
    "DescribeConfigurationSetResponse"
    "fixture/DescribeConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationSet)

responsePutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptionsResponse -> TestTree
responsePutConfigurationSetDeliveryOptions =
  res
    "PutConfigurationSetDeliveryOptionsResponse"
    "fixture/PutConfigurationSetDeliveryOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetDeliveryOptions)

responseDeleteIdentityPolicy :: DeleteIdentityPolicyResponse -> TestTree
responseDeleteIdentityPolicy =
  res
    "DeleteIdentityPolicyResponse"
    "fixture/DeleteIdentityPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIdentityPolicy)

responseDescribeReceiptRule :: DescribeReceiptRuleResponse -> TestTree
responseDescribeReceiptRule =
  res
    "DescribeReceiptRuleResponse"
    "fixture/DescribeReceiptRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReceiptRule)

responseCreateTemplate :: CreateTemplateResponse -> TestTree
responseCreateTemplate =
  res
    "CreateTemplateResponse"
    "fixture/CreateTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTemplate)

responseGetIdentityDkimAttributes :: GetIdentityDkimAttributesResponse -> TestTree
responseGetIdentityDkimAttributes =
  res
    "GetIdentityDkimAttributesResponse"
    "fixture/GetIdentityDkimAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityDkimAttributes)

responseCreateReceiptRuleSet :: CreateReceiptRuleSetResponse -> TestTree
responseCreateReceiptRuleSet =
  res
    "CreateReceiptRuleSetResponse"
    "fixture/CreateReceiptRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReceiptRuleSet)

responseGetSendQuota :: GetSendQuotaResponse -> TestTree
responseGetSendQuota =
  res
    "GetSendQuotaResponse"
    "fixture/GetSendQuotaResponse.proto"
    defaultService
    (Proxy :: Proxy GetSendQuota)

responseSetIdentityHeadersInNotificationsEnabled :: SetIdentityHeadersInNotificationsEnabledResponse -> TestTree
responseSetIdentityHeadersInNotificationsEnabled =
  res
    "SetIdentityHeadersInNotificationsEnabledResponse"
    "fixture/SetIdentityHeadersInNotificationsEnabledResponse.proto"
    defaultService
    (Proxy :: Proxy SetIdentityHeadersInNotificationsEnabled)

responseVerifyDomainIdentity :: VerifyDomainIdentityResponse -> TestTree
responseVerifyDomainIdentity =
  res
    "VerifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy VerifyDomainIdentity)

responseUpdateTemplate :: UpdateTemplateResponse -> TestTree
responseUpdateTemplate =
  res
    "UpdateTemplateResponse"
    "fixture/UpdateTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTemplate)

responseDeleteTemplate :: DeleteTemplateResponse -> TestTree
responseDeleteTemplate =
  res
    "DeleteTemplateResponse"
    "fixture/DeleteTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTemplate)

responseCreateConfigurationSetTrackingOptions :: CreateConfigurationSetTrackingOptionsResponse -> TestTree
responseCreateConfigurationSetTrackingOptions =
  res
    "CreateConfigurationSetTrackingOptionsResponse"
    "fixture/CreateConfigurationSetTrackingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfigurationSetTrackingOptions)

responseDeleteReceiptRuleSet :: DeleteReceiptRuleSetResponse -> TestTree
responseDeleteReceiptRuleSet =
  res
    "DeleteReceiptRuleSetResponse"
    "fixture/DeleteReceiptRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReceiptRuleSet)

responseSetReceiptRulePosition :: SetReceiptRulePositionResponse -> TestTree
responseSetReceiptRulePosition =
  res
    "SetReceiptRulePositionResponse"
    "fixture/SetReceiptRulePositionResponse.proto"
    defaultService
    (Proxy :: Proxy SetReceiptRulePosition)

responseUpdateAccountSendingEnabled :: UpdateAccountSendingEnabledResponse -> TestTree
responseUpdateAccountSendingEnabled =
  res
    "UpdateAccountSendingEnabledResponse"
    "fixture/UpdateAccountSendingEnabledResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccountSendingEnabled)

responseGetIdentityVerificationAttributes :: GetIdentityVerificationAttributesResponse -> TestTree
responseGetIdentityVerificationAttributes =
  res
    "GetIdentityVerificationAttributesResponse"
    "fixture/GetIdentityVerificationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityVerificationAttributes)

responseGetIdentityPolicies :: GetIdentityPoliciesResponse -> TestTree
responseGetIdentityPolicies =
  res
    "GetIdentityPoliciesResponse"
    "fixture/GetIdentityPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityPolicies)

responseCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestinationResponse -> TestTree
responseCreateConfigurationSetEventDestination =
  res
    "CreateConfigurationSetEventDestinationResponse"
    "fixture/CreateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfigurationSetEventDestination)

responseGetAccountSendingEnabled :: GetAccountSendingEnabledResponse -> TestTree
responseGetAccountSendingEnabled =
  res
    "GetAccountSendingEnabledResponse"
    "fixture/GetAccountSendingEnabledResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountSendingEnabled)

responseCreateConfigurationSet :: CreateConfigurationSetResponse -> TestTree
responseCreateConfigurationSet =
  res
    "CreateConfigurationSetResponse"
    "fixture/CreateConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfigurationSet)

responseDeleteConfigurationSet :: DeleteConfigurationSetResponse -> TestTree
responseDeleteConfigurationSet =
  res
    "DeleteConfigurationSetResponse"
    "fixture/DeleteConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationSet)

responseDeleteReceiptRule :: DeleteReceiptRuleResponse -> TestTree
responseDeleteReceiptRule =
  res
    "DeleteReceiptRuleResponse"
    "fixture/DeleteReceiptRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReceiptRule)

responseSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
responseSetIdentityFeedbackForwardingEnabled =
  res
    "SetIdentityFeedbackForwardingEnabledResponse"
    "fixture/SetIdentityFeedbackForwardingEnabledResponse.proto"
    defaultService
    (Proxy :: Proxy SetIdentityFeedbackForwardingEnabled)

responseCloneReceiptRuleSet :: CloneReceiptRuleSetResponse -> TestTree
responseCloneReceiptRuleSet =
  res
    "CloneReceiptRuleSetResponse"
    "fixture/CloneReceiptRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy CloneReceiptRuleSet)

responseUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestinationResponse -> TestTree
responseUpdateConfigurationSetEventDestination =
  res
    "UpdateConfigurationSetEventDestinationResponse"
    "fixture/UpdateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfigurationSetEventDestination)

responseUpdateReceiptRule :: UpdateReceiptRuleResponse -> TestTree
responseUpdateReceiptRule =
  res
    "UpdateReceiptRuleResponse"
    "fixture/UpdateReceiptRuleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateReceiptRule)

responseDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestinationResponse -> TestTree
responseDeleteConfigurationSetEventDestination =
  res
    "DeleteConfigurationSetEventDestinationResponse"
    "fixture/DeleteConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationSetEventDestination)

responseSendEmail :: SendEmailResponse -> TestTree
responseSendEmail =
  res
    "SendEmailResponse"
    "fixture/SendEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendEmail)

responseDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddressResponse -> TestTree
responseDeleteVerifiedEmailAddress =
  res
    "DeleteVerifiedEmailAddressResponse"
    "fixture/DeleteVerifiedEmailAddressResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVerifiedEmailAddress)

responseVerifyEmailAddress :: VerifyEmailAddressResponse -> TestTree
responseVerifyEmailAddress =
  res
    "VerifyEmailAddressResponse"
    "fixture/VerifyEmailAddressResponse.proto"
    defaultService
    (Proxy :: Proxy VerifyEmailAddress)

responseCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplateResponse -> TestTree
responseCreateCustomVerificationEmailTemplate =
  res
    "CreateCustomVerificationEmailTemplateResponse"
    "fixture/CreateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCustomVerificationEmailTemplate)

responseListIdentityPolicies :: ListIdentityPoliciesResponse -> TestTree
responseListIdentityPolicies =
  res
    "ListIdentityPoliciesResponse"
    "fixture/ListIdentityPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListIdentityPolicies)

responseSetIdentityDkimEnabled :: SetIdentityDkimEnabledResponse -> TestTree
responseSetIdentityDkimEnabled =
  res
    "SetIdentityDkimEnabledResponse"
    "fixture/SetIdentityDkimEnabledResponse.proto"
    defaultService
    (Proxy :: Proxy SetIdentityDkimEnabled)

responseUpdateConfigurationSetReputationMetricsEnabled :: UpdateConfigurationSetReputationMetricsEnabledResponse -> TestTree
responseUpdateConfigurationSetReputationMetricsEnabled =
  res
    "UpdateConfigurationSetReputationMetricsEnabledResponse"
    "fixture/UpdateConfigurationSetReputationMetricsEnabledResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfigurationSetReputationMetricsEnabled)

responseListCustomVerificationEmailTemplates :: ListCustomVerificationEmailTemplatesResponse -> TestTree
responseListCustomVerificationEmailTemplates =
  res
    "ListCustomVerificationEmailTemplatesResponse"
    "fixture/ListCustomVerificationEmailTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCustomVerificationEmailTemplates)

responseDeleteIdentity :: DeleteIdentityResponse -> TestTree
responseDeleteIdentity =
  res
    "DeleteIdentityResponse"
    "fixture/DeleteIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIdentity)

responseDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplateResponse -> TestTree
responseDeleteCustomVerificationEmailTemplate =
  res
    "DeleteCustomVerificationEmailTemplateResponse"
    "fixture/DeleteCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCustomVerificationEmailTemplate)

responsePutIdentityPolicy :: PutIdentityPolicyResponse -> TestTree
responsePutIdentityPolicy =
  res
    "PutIdentityPolicyResponse"
    "fixture/PutIdentityPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutIdentityPolicy)

responseUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplateResponse -> TestTree
responseUpdateCustomVerificationEmailTemplate =
  res
    "UpdateCustomVerificationEmailTemplateResponse"
    "fixture/UpdateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCustomVerificationEmailTemplate)

responseDeleteConfigurationSetTrackingOptions :: DeleteConfigurationSetTrackingOptionsResponse -> TestTree
responseDeleteConfigurationSetTrackingOptions =
  res
    "DeleteConfigurationSetTrackingOptionsResponse"
    "fixture/DeleteConfigurationSetTrackingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationSetTrackingOptions)

responseSendBulkTemplatedEmail :: SendBulkTemplatedEmailResponse -> TestTree
responseSendBulkTemplatedEmail =
  res
    "SendBulkTemplatedEmailResponse"
    "fixture/SendBulkTemplatedEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendBulkTemplatedEmail)

responseVerifyDomainDkim :: VerifyDomainDkimResponse -> TestTree
responseVerifyDomainDkim =
  res
    "VerifyDomainDkimResponse"
    "fixture/VerifyDomainDkimResponse.proto"
    defaultService
    (Proxy :: Proxy VerifyDomainDkim)

responseSendRawEmail :: SendRawEmailResponse -> TestTree
responseSendRawEmail =
  res
    "SendRawEmailResponse"
    "fixture/SendRawEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendRawEmail)

responseTestRenderTemplate :: TestRenderTemplateResponse -> TestTree
responseTestRenderTemplate =
  res
    "TestRenderTemplateResponse"
    "fixture/TestRenderTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy TestRenderTemplate)

responseSendBounce :: SendBounceResponse -> TestTree
responseSendBounce =
  res
    "SendBounceResponse"
    "fixture/SendBounceResponse.proto"
    defaultService
    (Proxy :: Proxy SendBounce)

responseUpdateConfigurationSetTrackingOptions :: UpdateConfigurationSetTrackingOptionsResponse -> TestTree
responseUpdateConfigurationSetTrackingOptions =
  res
    "UpdateConfigurationSetTrackingOptionsResponse"
    "fixture/UpdateConfigurationSetTrackingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfigurationSetTrackingOptions)

responseSendTemplatedEmail :: SendTemplatedEmailResponse -> TestTree
responseSendTemplatedEmail =
  res
    "SendTemplatedEmailResponse"
    "fixture/SendTemplatedEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendTemplatedEmail)

responseListReceiptRuleSets :: ListReceiptRuleSetsResponse -> TestTree
responseListReceiptRuleSets =
  res
    "ListReceiptRuleSetsResponse"
    "fixture/ListReceiptRuleSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListReceiptRuleSets)

responseReorderReceiptRuleSet :: ReorderReceiptRuleSetResponse -> TestTree
responseReorderReceiptRuleSet =
  res
    "ReorderReceiptRuleSetResponse"
    "fixture/ReorderReceiptRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy ReorderReceiptRuleSet)

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTemplates)

responseDescribeActiveReceiptRuleSet :: DescribeActiveReceiptRuleSetResponse -> TestTree
responseDescribeActiveReceiptRuleSet =
  res
    "DescribeActiveReceiptRuleSetResponse"
    "fixture/DescribeActiveReceiptRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeActiveReceiptRuleSet)

responseCreateReceiptRule :: CreateReceiptRuleResponse -> TestTree
responseCreateReceiptRule =
  res
    "CreateReceiptRuleResponse"
    "fixture/CreateReceiptRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReceiptRule)

responseGetTemplate :: GetTemplateResponse -> TestTree
responseGetTemplate =
  res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetTemplate)

responseSetActiveReceiptRuleSet :: SetActiveReceiptRuleSetResponse -> TestTree
responseSetActiveReceiptRuleSet =
  res
    "SetActiveReceiptRuleSetResponse"
    "fixture/SetActiveReceiptRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy SetActiveReceiptRuleSet)

responseListConfigurationSets :: ListConfigurationSetsResponse -> TestTree
responseListConfigurationSets =
  res
    "ListConfigurationSetsResponse"
    "fixture/ListConfigurationSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConfigurationSets)

responseSetIdentityMailFromDomain :: SetIdentityMailFromDomainResponse -> TestTree
responseSetIdentityMailFromDomain =
  res
    "SetIdentityMailFromDomainResponse"
    "fixture/SetIdentityMailFromDomainResponse.proto"
    defaultService
    (Proxy :: Proxy SetIdentityMailFromDomain)

responseGetIdentityMailFromDomainAttributes :: GetIdentityMailFromDomainAttributesResponse -> TestTree
responseGetIdentityMailFromDomainAttributes =
  res
    "GetIdentityMailFromDomainAttributesResponse"
    "fixture/GetIdentityMailFromDomainAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityMailFromDomainAttributes)

responseSetIdentityNotificationTopic :: SetIdentityNotificationTopicResponse -> TestTree
responseSetIdentityNotificationTopic =
  res
    "SetIdentityNotificationTopicResponse"
    "fixture/SetIdentityNotificationTopicResponse.proto"
    defaultService
    (Proxy :: Proxy SetIdentityNotificationTopic)

responseGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplateResponse -> TestTree
responseGetCustomVerificationEmailTemplate =
  res
    "GetCustomVerificationEmailTemplateResponse"
    "fixture/GetCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetCustomVerificationEmailTemplate)

responseCreateReceiptFilter :: CreateReceiptFilterResponse -> TestTree
responseCreateReceiptFilter =
  res
    "CreateReceiptFilterResponse"
    "fixture/CreateReceiptFilterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReceiptFilter)

responseListVerifiedEmailAddresses :: ListVerifiedEmailAddressesResponse -> TestTree
responseListVerifiedEmailAddresses =
  res
    "ListVerifiedEmailAddressesResponse"
    "fixture/ListVerifiedEmailAddressesResponse.proto"
    defaultService
    (Proxy :: Proxy ListVerifiedEmailAddresses)

responseListReceiptFilters :: ListReceiptFiltersResponse -> TestTree
responseListReceiptFilters =
  res
    "ListReceiptFiltersResponse"
    "fixture/ListReceiptFiltersResponse.proto"
    defaultService
    (Proxy :: Proxy ListReceiptFilters)

responseDeleteReceiptFilter :: DeleteReceiptFilterResponse -> TestTree
responseDeleteReceiptFilter =
  res
    "DeleteReceiptFilterResponse"
    "fixture/DeleteReceiptFilterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReceiptFilter)

responseDescribeReceiptRuleSet :: DescribeReceiptRuleSetResponse -> TestTree
responseDescribeReceiptRuleSet =
  res
    "DescribeReceiptRuleSetResponse"
    "fixture/DescribeReceiptRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReceiptRuleSet)

responseVerifyEmailIdentity :: VerifyEmailIdentityResponse -> TestTree
responseVerifyEmailIdentity =
  res
    "VerifyEmailIdentityResponse"
    "fixture/VerifyEmailIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy VerifyEmailIdentity)

responseSendCustomVerificationEmail :: SendCustomVerificationEmailResponse -> TestTree
responseSendCustomVerificationEmail =
  res
    "SendCustomVerificationEmailResponse"
    "fixture/SendCustomVerificationEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendCustomVerificationEmail)

responseListIdentities :: ListIdentitiesResponse -> TestTree
responseListIdentities =
  res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse.proto"
    defaultService
    (Proxy :: Proxy ListIdentities)

responseGetIdentityNotificationAttributes :: GetIdentityNotificationAttributesResponse -> TestTree
responseGetIdentityNotificationAttributes =
  res
    "GetIdentityNotificationAttributesResponse"
    "fixture/GetIdentityNotificationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityNotificationAttributes)

responseUpdateConfigurationSetSendingEnabled :: UpdateConfigurationSetSendingEnabledResponse -> TestTree
responseUpdateConfigurationSetSendingEnabled =
  res
    "UpdateConfigurationSetSendingEnabledResponse"
    "fixture/UpdateConfigurationSetSendingEnabledResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfigurationSetSendingEnabled)
