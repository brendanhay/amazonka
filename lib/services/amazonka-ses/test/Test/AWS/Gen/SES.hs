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
--         [ requestPutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptions
--
--         , requestDescribeConfigurationSet $
--             newDescribeConfigurationSet
--
--         , requestDescribeReceiptRule $
--             newDescribeReceiptRule
--
--         , requestDeleteIdentityPolicy $
--             newDeleteIdentityPolicy
--
--         , requestGetSendStatistics $
--             newGetSendStatistics
--
--         , requestCreateTemplate $
--             newCreateTemplate
--
--         , requestSetIdentityHeadersInNotificationsEnabled $
--             newSetIdentityHeadersInNotificationsEnabled
--
--         , requestGetIdentityDkimAttributes $
--             newGetIdentityDkimAttributes
--
--         , requestGetSendQuota $
--             newGetSendQuota
--
--         , requestCreateReceiptRuleSet $
--             newCreateReceiptRuleSet
--
--         , requestCreateConfigurationSetTrackingOptions $
--             newCreateConfigurationSetTrackingOptions
--
--         , requestDeleteReceiptRuleSet $
--             newDeleteReceiptRuleSet
--
--         , requestVerifyDomainIdentity $
--             newVerifyDomainIdentity
--
--         , requestUpdateAccountSendingEnabled $
--             newUpdateAccountSendingEnabled
--
--         , requestDeleteTemplate $
--             newDeleteTemplate
--
--         , requestUpdateTemplate $
--             newUpdateTemplate
--
--         , requestSetReceiptRulePosition $
--             newSetReceiptRulePosition
--
--         , requestGetIdentityPolicies $
--             newGetIdentityPolicies
--
--         , requestGetIdentityVerificationAttributes $
--             newGetIdentityVerificationAttributes
--
--         , requestGetAccountSendingEnabled $
--             newGetAccountSendingEnabled
--
--         , requestCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestination
--
--         , requestCreateConfigurationSet $
--             newCreateConfigurationSet
--
--         , requestDeleteConfigurationSet $
--             newDeleteConfigurationSet
--
--         , requestCloneReceiptRuleSet $
--             newCloneReceiptRuleSet
--
--         , requestUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestination
--
--         , requestDeleteReceiptRule $
--             newDeleteReceiptRule
--
--         , requestUpdateReceiptRule $
--             newUpdateReceiptRule
--
--         , requestDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestination
--
--         , requestSetIdentityFeedbackForwardingEnabled $
--             newSetIdentityFeedbackForwardingEnabled
--
--         , requestSendEmail $
--             newSendEmail
--
--         , requestVerifyEmailAddress $
--             newVerifyEmailAddress
--
--         , requestDeleteVerifiedEmailAddress $
--             newDeleteVerifiedEmailAddress
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
--         , requestCreateCustomVerificationEmailTemplate $
--             newCreateCustomVerificationEmailTemplate
--
--         , requestDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplate
--
--         , requestUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplate
--
--         , requestListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplates
--
--         , requestDeleteIdentity $
--             newDeleteIdentity
--
--         , requestPutIdentityPolicy $
--             newPutIdentityPolicy
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
--         , requestSendTemplatedEmail $
--             newSendTemplatedEmail
--
--         , requestSendRawEmail $
--             newSendRawEmail
--
--         , requestSendBounce $
--             newSendBounce
--
--         , requestUpdateConfigurationSetTrackingOptions $
--             newUpdateConfigurationSetTrackingOptions
--
--         , requestTestRenderTemplate $
--             newTestRenderTemplate
--
--         , requestReorderReceiptRuleSet $
--             newReorderReceiptRuleSet
--
--         , requestListReceiptRuleSets $
--             newListReceiptRuleSets
--
--         , requestListTemplates $
--             newListTemplates
--
--         , requestDescribeActiveReceiptRuleSet $
--             newDescribeActiveReceiptRuleSet
--
--         , requestGetTemplate $
--             newGetTemplate
--
--         , requestCreateReceiptRule $
--             newCreateReceiptRule
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
--         , requestGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplate
--
--         , requestSetIdentityNotificationTopic $
--             newSetIdentityNotificationTopic
--
--         , requestCreateReceiptFilter $
--             newCreateReceiptFilter
--
--         , requestListVerifiedEmailAddresses $
--             newListVerifiedEmailAddresses
--
--         , requestDeleteReceiptFilter $
--             newDeleteReceiptFilter
--
--         , requestDescribeReceiptRuleSet $
--             newDescribeReceiptRuleSet
--
--         , requestListReceiptFilters $
--             newListReceiptFilters
--
--         , requestVerifyEmailIdentity $
--             newVerifyEmailIdentity
--
--         , requestUpdateConfigurationSetSendingEnabled $
--             newUpdateConfigurationSetSendingEnabled
--
--         , requestListIdentities $
--             newListIdentities
--
--         , requestGetIdentityNotificationAttributes $
--             newGetIdentityNotificationAttributes
--
--         , requestSendCustomVerificationEmail $
--             newSendCustomVerificationEmail
--
--           ]

--     , testGroup "response"
--         [ responsePutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptionsResponse
--
--         , responseDescribeConfigurationSet $
--             newDescribeConfigurationSetResponse
--
--         , responseDescribeReceiptRule $
--             newDescribeReceiptRuleResponse
--
--         , responseDeleteIdentityPolicy $
--             newDeleteIdentityPolicyResponse
--
--         , responseGetSendStatistics $
--             newGetSendStatisticsResponse
--
--         , responseCreateTemplate $
--             newCreateTemplateResponse
--
--         , responseSetIdentityHeadersInNotificationsEnabled $
--             newSetIdentityHeadersInNotificationsEnabledResponse
--
--         , responseGetIdentityDkimAttributes $
--             newGetIdentityDkimAttributesResponse
--
--         , responseGetSendQuota $
--             newGetSendQuotaResponse
--
--         , responseCreateReceiptRuleSet $
--             newCreateReceiptRuleSetResponse
--
--         , responseCreateConfigurationSetTrackingOptions $
--             newCreateConfigurationSetTrackingOptionsResponse
--
--         , responseDeleteReceiptRuleSet $
--             newDeleteReceiptRuleSetResponse
--
--         , responseVerifyDomainIdentity $
--             newVerifyDomainIdentityResponse
--
--         , responseUpdateAccountSendingEnabled $
--             newUpdateAccountSendingEnabledResponse
--
--         , responseDeleteTemplate $
--             newDeleteTemplateResponse
--
--         , responseUpdateTemplate $
--             newUpdateTemplateResponse
--
--         , responseSetReceiptRulePosition $
--             newSetReceiptRulePositionResponse
--
--         , responseGetIdentityPolicies $
--             newGetIdentityPoliciesResponse
--
--         , responseGetIdentityVerificationAttributes $
--             newGetIdentityVerificationAttributesResponse
--
--         , responseGetAccountSendingEnabled $
--             newGetAccountSendingEnabledResponse
--
--         , responseCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestinationResponse
--
--         , responseCreateConfigurationSet $
--             newCreateConfigurationSetResponse
--
--         , responseDeleteConfigurationSet $
--             newDeleteConfigurationSetResponse
--
--         , responseCloneReceiptRuleSet $
--             newCloneReceiptRuleSetResponse
--
--         , responseUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestinationResponse
--
--         , responseDeleteReceiptRule $
--             newDeleteReceiptRuleResponse
--
--         , responseUpdateReceiptRule $
--             newUpdateReceiptRuleResponse
--
--         , responseDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestinationResponse
--
--         , responseSetIdentityFeedbackForwardingEnabled $
--             newSetIdentityFeedbackForwardingEnabledResponse
--
--         , responseSendEmail $
--             newSendEmailResponse
--
--         , responseVerifyEmailAddress $
--             newVerifyEmailAddressResponse
--
--         , responseDeleteVerifiedEmailAddress $
--             newDeleteVerifiedEmailAddressResponse
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
--         , responseCreateCustomVerificationEmailTemplate $
--             newCreateCustomVerificationEmailTemplateResponse
--
--         , responseDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplateResponse
--
--         , responseUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplateResponse
--
--         , responseListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplatesResponse
--
--         , responseDeleteIdentity $
--             newDeleteIdentityResponse
--
--         , responsePutIdentityPolicy $
--             newPutIdentityPolicyResponse
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
--         , responseSendTemplatedEmail $
--             newSendTemplatedEmailResponse
--
--         , responseSendRawEmail $
--             newSendRawEmailResponse
--
--         , responseSendBounce $
--             newSendBounceResponse
--
--         , responseUpdateConfigurationSetTrackingOptions $
--             newUpdateConfigurationSetTrackingOptionsResponse
--
--         , responseTestRenderTemplate $
--             newTestRenderTemplateResponse
--
--         , responseReorderReceiptRuleSet $
--             newReorderReceiptRuleSetResponse
--
--         , responseListReceiptRuleSets $
--             newListReceiptRuleSetsResponse
--
--         , responseListTemplates $
--             newListTemplatesResponse
--
--         , responseDescribeActiveReceiptRuleSet $
--             newDescribeActiveReceiptRuleSetResponse
--
--         , responseGetTemplate $
--             newGetTemplateResponse
--
--         , responseCreateReceiptRule $
--             newCreateReceiptRuleResponse
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
--         , responseGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplateResponse
--
--         , responseSetIdentityNotificationTopic $
--             newSetIdentityNotificationTopicResponse
--
--         , responseCreateReceiptFilter $
--             newCreateReceiptFilterResponse
--
--         , responseListVerifiedEmailAddresses $
--             newListVerifiedEmailAddressesResponse
--
--         , responseDeleteReceiptFilter $
--             newDeleteReceiptFilterResponse
--
--         , responseDescribeReceiptRuleSet $
--             newDescribeReceiptRuleSetResponse
--
--         , responseListReceiptFilters $
--             newListReceiptFiltersResponse
--
--         , responseVerifyEmailIdentity $
--             newVerifyEmailIdentityResponse
--
--         , responseUpdateConfigurationSetSendingEnabled $
--             newUpdateConfigurationSetSendingEnabledResponse
--
--         , responseListIdentities $
--             newListIdentitiesResponse
--
--         , responseGetIdentityNotificationAttributes $
--             newGetIdentityNotificationAttributesResponse
--
--         , responseSendCustomVerificationEmail $
--             newSendCustomVerificationEmailResponse
--
--           ]
--     ]

-- Requests

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

requestDescribeReceiptRule :: DescribeReceiptRule -> TestTree
requestDescribeReceiptRule =
  req
    "DescribeReceiptRule"
    "fixture/DescribeReceiptRule.yaml"

requestDeleteIdentityPolicy :: DeleteIdentityPolicy -> TestTree
requestDeleteIdentityPolicy =
  req
    "DeleteIdentityPolicy"
    "fixture/DeleteIdentityPolicy.yaml"

requestGetSendStatistics :: GetSendStatistics -> TestTree
requestGetSendStatistics =
  req
    "GetSendStatistics"
    "fixture/GetSendStatistics.yaml"

requestCreateTemplate :: CreateTemplate -> TestTree
requestCreateTemplate =
  req
    "CreateTemplate"
    "fixture/CreateTemplate.yaml"

requestSetIdentityHeadersInNotificationsEnabled :: SetIdentityHeadersInNotificationsEnabled -> TestTree
requestSetIdentityHeadersInNotificationsEnabled =
  req
    "SetIdentityHeadersInNotificationsEnabled"
    "fixture/SetIdentityHeadersInNotificationsEnabled.yaml"

requestGetIdentityDkimAttributes :: GetIdentityDkimAttributes -> TestTree
requestGetIdentityDkimAttributes =
  req
    "GetIdentityDkimAttributes"
    "fixture/GetIdentityDkimAttributes.yaml"

requestGetSendQuota :: GetSendQuota -> TestTree
requestGetSendQuota =
  req
    "GetSendQuota"
    "fixture/GetSendQuota.yaml"

requestCreateReceiptRuleSet :: CreateReceiptRuleSet -> TestTree
requestCreateReceiptRuleSet =
  req
    "CreateReceiptRuleSet"
    "fixture/CreateReceiptRuleSet.yaml"

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

requestVerifyDomainIdentity :: VerifyDomainIdentity -> TestTree
requestVerifyDomainIdentity =
  req
    "VerifyDomainIdentity"
    "fixture/VerifyDomainIdentity.yaml"

requestUpdateAccountSendingEnabled :: UpdateAccountSendingEnabled -> TestTree
requestUpdateAccountSendingEnabled =
  req
    "UpdateAccountSendingEnabled"
    "fixture/UpdateAccountSendingEnabled.yaml"

requestDeleteTemplate :: DeleteTemplate -> TestTree
requestDeleteTemplate =
  req
    "DeleteTemplate"
    "fixture/DeleteTemplate.yaml"

requestUpdateTemplate :: UpdateTemplate -> TestTree
requestUpdateTemplate =
  req
    "UpdateTemplate"
    "fixture/UpdateTemplate.yaml"

requestSetReceiptRulePosition :: SetReceiptRulePosition -> TestTree
requestSetReceiptRulePosition =
  req
    "SetReceiptRulePosition"
    "fixture/SetReceiptRulePosition.yaml"

requestGetIdentityPolicies :: GetIdentityPolicies -> TestTree
requestGetIdentityPolicies =
  req
    "GetIdentityPolicies"
    "fixture/GetIdentityPolicies.yaml"

requestGetIdentityVerificationAttributes :: GetIdentityVerificationAttributes -> TestTree
requestGetIdentityVerificationAttributes =
  req
    "GetIdentityVerificationAttributes"
    "fixture/GetIdentityVerificationAttributes.yaml"

requestGetAccountSendingEnabled :: GetAccountSendingEnabled -> TestTree
requestGetAccountSendingEnabled =
  req
    "GetAccountSendingEnabled"
    "fixture/GetAccountSendingEnabled.yaml"

requestCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestination -> TestTree
requestCreateConfigurationSetEventDestination =
  req
    "CreateConfigurationSetEventDestination"
    "fixture/CreateConfigurationSetEventDestination.yaml"

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

requestDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestination -> TestTree
requestDeleteConfigurationSetEventDestination =
  req
    "DeleteConfigurationSetEventDestination"
    "fixture/DeleteConfigurationSetEventDestination.yaml"

requestSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabled -> TestTree
requestSetIdentityFeedbackForwardingEnabled =
  req
    "SetIdentityFeedbackForwardingEnabled"
    "fixture/SetIdentityFeedbackForwardingEnabled.yaml"

requestSendEmail :: SendEmail -> TestTree
requestSendEmail =
  req
    "SendEmail"
    "fixture/SendEmail.yaml"

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

requestCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplate -> TestTree
requestCreateCustomVerificationEmailTemplate =
  req
    "CreateCustomVerificationEmailTemplate"
    "fixture/CreateCustomVerificationEmailTemplate.yaml"

requestDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplate -> TestTree
requestDeleteCustomVerificationEmailTemplate =
  req
    "DeleteCustomVerificationEmailTemplate"
    "fixture/DeleteCustomVerificationEmailTemplate.yaml"

requestUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplate -> TestTree
requestUpdateCustomVerificationEmailTemplate =
  req
    "UpdateCustomVerificationEmailTemplate"
    "fixture/UpdateCustomVerificationEmailTemplate.yaml"

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

requestPutIdentityPolicy :: PutIdentityPolicy -> TestTree
requestPutIdentityPolicy =
  req
    "PutIdentityPolicy"
    "fixture/PutIdentityPolicy.yaml"

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

requestSendTemplatedEmail :: SendTemplatedEmail -> TestTree
requestSendTemplatedEmail =
  req
    "SendTemplatedEmail"
    "fixture/SendTemplatedEmail.yaml"

requestSendRawEmail :: SendRawEmail -> TestTree
requestSendRawEmail =
  req
    "SendRawEmail"
    "fixture/SendRawEmail.yaml"

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

requestTestRenderTemplate :: TestRenderTemplate -> TestTree
requestTestRenderTemplate =
  req
    "TestRenderTemplate"
    "fixture/TestRenderTemplate.yaml"

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

requestGetTemplate :: GetTemplate -> TestTree
requestGetTemplate =
  req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

requestCreateReceiptRule :: CreateReceiptRule -> TestTree
requestCreateReceiptRule =
  req
    "CreateReceiptRule"
    "fixture/CreateReceiptRule.yaml"

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

requestListReceiptFilters :: ListReceiptFilters -> TestTree
requestListReceiptFilters =
  req
    "ListReceiptFilters"
    "fixture/ListReceiptFilters.yaml"

requestVerifyEmailIdentity :: VerifyEmailIdentity -> TestTree
requestVerifyEmailIdentity =
  req
    "VerifyEmailIdentity"
    "fixture/VerifyEmailIdentity.yaml"

requestUpdateConfigurationSetSendingEnabled :: UpdateConfigurationSetSendingEnabled -> TestTree
requestUpdateConfigurationSetSendingEnabled =
  req
    "UpdateConfigurationSetSendingEnabled"
    "fixture/UpdateConfigurationSetSendingEnabled.yaml"

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

requestSendCustomVerificationEmail :: SendCustomVerificationEmail -> TestTree
requestSendCustomVerificationEmail =
  req
    "SendCustomVerificationEmail"
    "fixture/SendCustomVerificationEmail.yaml"

-- Responses

responsePutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptionsResponse -> TestTree
responsePutConfigurationSetDeliveryOptions =
  res
    "PutConfigurationSetDeliveryOptionsResponse"
    "fixture/PutConfigurationSetDeliveryOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetDeliveryOptions)

responseDescribeConfigurationSet :: DescribeConfigurationSetResponse -> TestTree
responseDescribeConfigurationSet =
  res
    "DescribeConfigurationSetResponse"
    "fixture/DescribeConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationSet)

responseDescribeReceiptRule :: DescribeReceiptRuleResponse -> TestTree
responseDescribeReceiptRule =
  res
    "DescribeReceiptRuleResponse"
    "fixture/DescribeReceiptRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReceiptRule)

responseDeleteIdentityPolicy :: DeleteIdentityPolicyResponse -> TestTree
responseDeleteIdentityPolicy =
  res
    "DeleteIdentityPolicyResponse"
    "fixture/DeleteIdentityPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIdentityPolicy)

responseGetSendStatistics :: GetSendStatisticsResponse -> TestTree
responseGetSendStatistics =
  res
    "GetSendStatisticsResponse"
    "fixture/GetSendStatisticsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSendStatistics)

responseCreateTemplate :: CreateTemplateResponse -> TestTree
responseCreateTemplate =
  res
    "CreateTemplateResponse"
    "fixture/CreateTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTemplate)

responseSetIdentityHeadersInNotificationsEnabled :: SetIdentityHeadersInNotificationsEnabledResponse -> TestTree
responseSetIdentityHeadersInNotificationsEnabled =
  res
    "SetIdentityHeadersInNotificationsEnabledResponse"
    "fixture/SetIdentityHeadersInNotificationsEnabledResponse.proto"
    defaultService
    (Proxy :: Proxy SetIdentityHeadersInNotificationsEnabled)

responseGetIdentityDkimAttributes :: GetIdentityDkimAttributesResponse -> TestTree
responseGetIdentityDkimAttributes =
  res
    "GetIdentityDkimAttributesResponse"
    "fixture/GetIdentityDkimAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityDkimAttributes)

responseGetSendQuota :: GetSendQuotaResponse -> TestTree
responseGetSendQuota =
  res
    "GetSendQuotaResponse"
    "fixture/GetSendQuotaResponse.proto"
    defaultService
    (Proxy :: Proxy GetSendQuota)

responseCreateReceiptRuleSet :: CreateReceiptRuleSetResponse -> TestTree
responseCreateReceiptRuleSet =
  res
    "CreateReceiptRuleSetResponse"
    "fixture/CreateReceiptRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReceiptRuleSet)

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

responseVerifyDomainIdentity :: VerifyDomainIdentityResponse -> TestTree
responseVerifyDomainIdentity =
  res
    "VerifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy VerifyDomainIdentity)

responseUpdateAccountSendingEnabled :: UpdateAccountSendingEnabledResponse -> TestTree
responseUpdateAccountSendingEnabled =
  res
    "UpdateAccountSendingEnabledResponse"
    "fixture/UpdateAccountSendingEnabledResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccountSendingEnabled)

responseDeleteTemplate :: DeleteTemplateResponse -> TestTree
responseDeleteTemplate =
  res
    "DeleteTemplateResponse"
    "fixture/DeleteTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTemplate)

responseUpdateTemplate :: UpdateTemplateResponse -> TestTree
responseUpdateTemplate =
  res
    "UpdateTemplateResponse"
    "fixture/UpdateTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTemplate)

responseSetReceiptRulePosition :: SetReceiptRulePositionResponse -> TestTree
responseSetReceiptRulePosition =
  res
    "SetReceiptRulePositionResponse"
    "fixture/SetReceiptRulePositionResponse.proto"
    defaultService
    (Proxy :: Proxy SetReceiptRulePosition)

responseGetIdentityPolicies :: GetIdentityPoliciesResponse -> TestTree
responseGetIdentityPolicies =
  res
    "GetIdentityPoliciesResponse"
    "fixture/GetIdentityPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityPolicies)

responseGetIdentityVerificationAttributes :: GetIdentityVerificationAttributesResponse -> TestTree
responseGetIdentityVerificationAttributes =
  res
    "GetIdentityVerificationAttributesResponse"
    "fixture/GetIdentityVerificationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityVerificationAttributes)

responseGetAccountSendingEnabled :: GetAccountSendingEnabledResponse -> TestTree
responseGetAccountSendingEnabled =
  res
    "GetAccountSendingEnabledResponse"
    "fixture/GetAccountSendingEnabledResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountSendingEnabled)

responseCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestinationResponse -> TestTree
responseCreateConfigurationSetEventDestination =
  res
    "CreateConfigurationSetEventDestinationResponse"
    "fixture/CreateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfigurationSetEventDestination)

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

responseDeleteReceiptRule :: DeleteReceiptRuleResponse -> TestTree
responseDeleteReceiptRule =
  res
    "DeleteReceiptRuleResponse"
    "fixture/DeleteReceiptRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReceiptRule)

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

responseSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
responseSetIdentityFeedbackForwardingEnabled =
  res
    "SetIdentityFeedbackForwardingEnabledResponse"
    "fixture/SetIdentityFeedbackForwardingEnabledResponse.proto"
    defaultService
    (Proxy :: Proxy SetIdentityFeedbackForwardingEnabled)

responseSendEmail :: SendEmailResponse -> TestTree
responseSendEmail =
  res
    "SendEmailResponse"
    "fixture/SendEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendEmail)

responseVerifyEmailAddress :: VerifyEmailAddressResponse -> TestTree
responseVerifyEmailAddress =
  res
    "VerifyEmailAddressResponse"
    "fixture/VerifyEmailAddressResponse.proto"
    defaultService
    (Proxy :: Proxy VerifyEmailAddress)

responseDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddressResponse -> TestTree
responseDeleteVerifiedEmailAddress =
  res
    "DeleteVerifiedEmailAddressResponse"
    "fixture/DeleteVerifiedEmailAddressResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVerifiedEmailAddress)

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

responseCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplateResponse -> TestTree
responseCreateCustomVerificationEmailTemplate =
  res
    "CreateCustomVerificationEmailTemplateResponse"
    "fixture/CreateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCustomVerificationEmailTemplate)

responseDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplateResponse -> TestTree
responseDeleteCustomVerificationEmailTemplate =
  res
    "DeleteCustomVerificationEmailTemplateResponse"
    "fixture/DeleteCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCustomVerificationEmailTemplate)

responseUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplateResponse -> TestTree
responseUpdateCustomVerificationEmailTemplate =
  res
    "UpdateCustomVerificationEmailTemplateResponse"
    "fixture/UpdateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCustomVerificationEmailTemplate)

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

responsePutIdentityPolicy :: PutIdentityPolicyResponse -> TestTree
responsePutIdentityPolicy =
  res
    "PutIdentityPolicyResponse"
    "fixture/PutIdentityPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutIdentityPolicy)

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

responseSendTemplatedEmail :: SendTemplatedEmailResponse -> TestTree
responseSendTemplatedEmail =
  res
    "SendTemplatedEmailResponse"
    "fixture/SendTemplatedEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendTemplatedEmail)

responseSendRawEmail :: SendRawEmailResponse -> TestTree
responseSendRawEmail =
  res
    "SendRawEmailResponse"
    "fixture/SendRawEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendRawEmail)

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

responseTestRenderTemplate :: TestRenderTemplateResponse -> TestTree
responseTestRenderTemplate =
  res
    "TestRenderTemplateResponse"
    "fixture/TestRenderTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy TestRenderTemplate)

responseReorderReceiptRuleSet :: ReorderReceiptRuleSetResponse -> TestTree
responseReorderReceiptRuleSet =
  res
    "ReorderReceiptRuleSetResponse"
    "fixture/ReorderReceiptRuleSetResponse.proto"
    defaultService
    (Proxy :: Proxy ReorderReceiptRuleSet)

responseListReceiptRuleSets :: ListReceiptRuleSetsResponse -> TestTree
responseListReceiptRuleSets =
  res
    "ListReceiptRuleSetsResponse"
    "fixture/ListReceiptRuleSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListReceiptRuleSets)

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

responseGetTemplate :: GetTemplateResponse -> TestTree
responseGetTemplate =
  res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetTemplate)

responseCreateReceiptRule :: CreateReceiptRuleResponse -> TestTree
responseCreateReceiptRule =
  res
    "CreateReceiptRuleResponse"
    "fixture/CreateReceiptRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReceiptRule)

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

responseGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplateResponse -> TestTree
responseGetCustomVerificationEmailTemplate =
  res
    "GetCustomVerificationEmailTemplateResponse"
    "fixture/GetCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetCustomVerificationEmailTemplate)

responseSetIdentityNotificationTopic :: SetIdentityNotificationTopicResponse -> TestTree
responseSetIdentityNotificationTopic =
  res
    "SetIdentityNotificationTopicResponse"
    "fixture/SetIdentityNotificationTopicResponse.proto"
    defaultService
    (Proxy :: Proxy SetIdentityNotificationTopic)

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

responseListReceiptFilters :: ListReceiptFiltersResponse -> TestTree
responseListReceiptFilters =
  res
    "ListReceiptFiltersResponse"
    "fixture/ListReceiptFiltersResponse.proto"
    defaultService
    (Proxy :: Proxy ListReceiptFilters)

responseVerifyEmailIdentity :: VerifyEmailIdentityResponse -> TestTree
responseVerifyEmailIdentity =
  res
    "VerifyEmailIdentityResponse"
    "fixture/VerifyEmailIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy VerifyEmailIdentity)

responseUpdateConfigurationSetSendingEnabled :: UpdateConfigurationSetSendingEnabledResponse -> TestTree
responseUpdateConfigurationSetSendingEnabled =
  res
    "UpdateConfigurationSetSendingEnabledResponse"
    "fixture/UpdateConfigurationSetSendingEnabledResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfigurationSetSendingEnabled)

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

responseSendCustomVerificationEmail :: SendCustomVerificationEmailResponse -> TestTree
responseSendCustomVerificationEmail =
  res
    "SendCustomVerificationEmailResponse"
    "fixture/SendCustomVerificationEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendCustomVerificationEmail)
