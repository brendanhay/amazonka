{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SES
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SES where

import Amazonka.SES
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SES.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCloneReceiptRuleSet $
--             newCloneReceiptRuleSet
--
--         , requestCreateConfigurationSet $
--             newCreateConfigurationSet
--
--         , requestCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestination
--
--         , requestCreateConfigurationSetTrackingOptions $
--             newCreateConfigurationSetTrackingOptions
--
--         , requestCreateCustomVerificationEmailTemplate $
--             newCreateCustomVerificationEmailTemplate
--
--         , requestCreateReceiptFilter $
--             newCreateReceiptFilter
--
--         , requestCreateReceiptRule $
--             newCreateReceiptRule
--
--         , requestCreateReceiptRuleSet $
--             newCreateReceiptRuleSet
--
--         , requestCreateTemplate $
--             newCreateTemplate
--
--         , requestDeleteConfigurationSet $
--             newDeleteConfigurationSet
--
--         , requestDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestination
--
--         , requestDeleteConfigurationSetTrackingOptions $
--             newDeleteConfigurationSetTrackingOptions
--
--         , requestDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplate
--
--         , requestDeleteIdentity $
--             newDeleteIdentity
--
--         , requestDeleteIdentityPolicy $
--             newDeleteIdentityPolicy
--
--         , requestDeleteReceiptFilter $
--             newDeleteReceiptFilter
--
--         , requestDeleteReceiptRule $
--             newDeleteReceiptRule
--
--         , requestDeleteReceiptRuleSet $
--             newDeleteReceiptRuleSet
--
--         , requestDeleteTemplate $
--             newDeleteTemplate
--
--         , requestDeleteVerifiedEmailAddress $
--             newDeleteVerifiedEmailAddress
--
--         , requestDescribeActiveReceiptRuleSet $
--             newDescribeActiveReceiptRuleSet
--
--         , requestDescribeConfigurationSet $
--             newDescribeConfigurationSet
--
--         , requestDescribeReceiptRule $
--             newDescribeReceiptRule
--
--         , requestDescribeReceiptRuleSet $
--             newDescribeReceiptRuleSet
--
--         , requestGetAccountSendingEnabled $
--             newGetAccountSendingEnabled
--
--         , requestGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplate
--
--         , requestGetIdentityDkimAttributes $
--             newGetIdentityDkimAttributes
--
--         , requestGetIdentityMailFromDomainAttributes $
--             newGetIdentityMailFromDomainAttributes
--
--         , requestGetIdentityNotificationAttributes $
--             newGetIdentityNotificationAttributes
--
--         , requestGetIdentityPolicies $
--             newGetIdentityPolicies
--
--         , requestGetIdentityVerificationAttributes $
--             newGetIdentityVerificationAttributes
--
--         , requestGetSendQuota $
--             newGetSendQuota
--
--         , requestGetSendStatistics $
--             newGetSendStatistics
--
--         , requestGetTemplate $
--             newGetTemplate
--
--         , requestListConfigurationSets $
--             newListConfigurationSets
--
--         , requestListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplates
--
--         , requestListIdentities $
--             newListIdentities
--
--         , requestListIdentityPolicies $
--             newListIdentityPolicies
--
--         , requestListReceiptFilters $
--             newListReceiptFilters
--
--         , requestListReceiptRuleSets $
--             newListReceiptRuleSets
--
--         , requestListTemplates $
--             newListTemplates
--
--         , requestListVerifiedEmailAddresses $
--             newListVerifiedEmailAddresses
--
--         , requestPutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptions
--
--         , requestPutIdentityPolicy $
--             newPutIdentityPolicy
--
--         , requestReorderReceiptRuleSet $
--             newReorderReceiptRuleSet
--
--         , requestSendBounce $
--             newSendBounce
--
--         , requestSendBulkTemplatedEmail $
--             newSendBulkTemplatedEmail
--
--         , requestSendCustomVerificationEmail $
--             newSendCustomVerificationEmail
--
--         , requestSendEmail $
--             newSendEmail
--
--         , requestSendRawEmail $
--             newSendRawEmail
--
--         , requestSendTemplatedEmail $
--             newSendTemplatedEmail
--
--         , requestSetActiveReceiptRuleSet $
--             newSetActiveReceiptRuleSet
--
--         , requestSetIdentityDkimEnabled $
--             newSetIdentityDkimEnabled
--
--         , requestSetIdentityFeedbackForwardingEnabled $
--             newSetIdentityFeedbackForwardingEnabled
--
--         , requestSetIdentityHeadersInNotificationsEnabled $
--             newSetIdentityHeadersInNotificationsEnabled
--
--         , requestSetIdentityMailFromDomain $
--             newSetIdentityMailFromDomain
--
--         , requestSetIdentityNotificationTopic $
--             newSetIdentityNotificationTopic
--
--         , requestSetReceiptRulePosition $
--             newSetReceiptRulePosition
--
--         , requestTestRenderTemplate $
--             newTestRenderTemplate
--
--         , requestUpdateAccountSendingEnabled $
--             newUpdateAccountSendingEnabled
--
--         , requestUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestination
--
--         , requestUpdateConfigurationSetReputationMetricsEnabled $
--             newUpdateConfigurationSetReputationMetricsEnabled
--
--         , requestUpdateConfigurationSetSendingEnabled $
--             newUpdateConfigurationSetSendingEnabled
--
--         , requestUpdateConfigurationSetTrackingOptions $
--             newUpdateConfigurationSetTrackingOptions
--
--         , requestUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplate
--
--         , requestUpdateReceiptRule $
--             newUpdateReceiptRule
--
--         , requestUpdateTemplate $
--             newUpdateTemplate
--
--         , requestVerifyDomainDkim $
--             newVerifyDomainDkim
--
--         , requestVerifyDomainIdentity $
--             newVerifyDomainIdentity
--
--         , requestVerifyEmailAddress $
--             newVerifyEmailAddress
--
--         , requestVerifyEmailIdentity $
--             newVerifyEmailIdentity
--
--           ]

--     , testGroup "response"
--         [ responseCloneReceiptRuleSet $
--             newCloneReceiptRuleSetResponse
--
--         , responseCreateConfigurationSet $
--             newCreateConfigurationSetResponse
--
--         , responseCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestinationResponse
--
--         , responseCreateConfigurationSetTrackingOptions $
--             newCreateConfigurationSetTrackingOptionsResponse
--
--         , responseCreateCustomVerificationEmailTemplate $
--             newCreateCustomVerificationEmailTemplateResponse
--
--         , responseCreateReceiptFilter $
--             newCreateReceiptFilterResponse
--
--         , responseCreateReceiptRule $
--             newCreateReceiptRuleResponse
--
--         , responseCreateReceiptRuleSet $
--             newCreateReceiptRuleSetResponse
--
--         , responseCreateTemplate $
--             newCreateTemplateResponse
--
--         , responseDeleteConfigurationSet $
--             newDeleteConfigurationSetResponse
--
--         , responseDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestinationResponse
--
--         , responseDeleteConfigurationSetTrackingOptions $
--             newDeleteConfigurationSetTrackingOptionsResponse
--
--         , responseDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplateResponse
--
--         , responseDeleteIdentity $
--             newDeleteIdentityResponse
--
--         , responseDeleteIdentityPolicy $
--             newDeleteIdentityPolicyResponse
--
--         , responseDeleteReceiptFilter $
--             newDeleteReceiptFilterResponse
--
--         , responseDeleteReceiptRule $
--             newDeleteReceiptRuleResponse
--
--         , responseDeleteReceiptRuleSet $
--             newDeleteReceiptRuleSetResponse
--
--         , responseDeleteTemplate $
--             newDeleteTemplateResponse
--
--         , responseDeleteVerifiedEmailAddress $
--             newDeleteVerifiedEmailAddressResponse
--
--         , responseDescribeActiveReceiptRuleSet $
--             newDescribeActiveReceiptRuleSetResponse
--
--         , responseDescribeConfigurationSet $
--             newDescribeConfigurationSetResponse
--
--         , responseDescribeReceiptRule $
--             newDescribeReceiptRuleResponse
--
--         , responseDescribeReceiptRuleSet $
--             newDescribeReceiptRuleSetResponse
--
--         , responseGetAccountSendingEnabled $
--             newGetAccountSendingEnabledResponse
--
--         , responseGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplateResponse
--
--         , responseGetIdentityDkimAttributes $
--             newGetIdentityDkimAttributesResponse
--
--         , responseGetIdentityMailFromDomainAttributes $
--             newGetIdentityMailFromDomainAttributesResponse
--
--         , responseGetIdentityNotificationAttributes $
--             newGetIdentityNotificationAttributesResponse
--
--         , responseGetIdentityPolicies $
--             newGetIdentityPoliciesResponse
--
--         , responseGetIdentityVerificationAttributes $
--             newGetIdentityVerificationAttributesResponse
--
--         , responseGetSendQuota $
--             newGetSendQuotaResponse
--
--         , responseGetSendStatistics $
--             newGetSendStatisticsResponse
--
--         , responseGetTemplate $
--             newGetTemplateResponse
--
--         , responseListConfigurationSets $
--             newListConfigurationSetsResponse
--
--         , responseListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplatesResponse
--
--         , responseListIdentities $
--             newListIdentitiesResponse
--
--         , responseListIdentityPolicies $
--             newListIdentityPoliciesResponse
--
--         , responseListReceiptFilters $
--             newListReceiptFiltersResponse
--
--         , responseListReceiptRuleSets $
--             newListReceiptRuleSetsResponse
--
--         , responseListTemplates $
--             newListTemplatesResponse
--
--         , responseListVerifiedEmailAddresses $
--             newListVerifiedEmailAddressesResponse
--
--         , responsePutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptionsResponse
--
--         , responsePutIdentityPolicy $
--             newPutIdentityPolicyResponse
--
--         , responseReorderReceiptRuleSet $
--             newReorderReceiptRuleSetResponse
--
--         , responseSendBounce $
--             newSendBounceResponse
--
--         , responseSendBulkTemplatedEmail $
--             newSendBulkTemplatedEmailResponse
--
--         , responseSendCustomVerificationEmail $
--             newSendCustomVerificationEmailResponse
--
--         , responseSendEmail $
--             newSendEmailResponse
--
--         , responseSendRawEmail $
--             newSendRawEmailResponse
--
--         , responseSendTemplatedEmail $
--             newSendTemplatedEmailResponse
--
--         , responseSetActiveReceiptRuleSet $
--             newSetActiveReceiptRuleSetResponse
--
--         , responseSetIdentityDkimEnabled $
--             newSetIdentityDkimEnabledResponse
--
--         , responseSetIdentityFeedbackForwardingEnabled $
--             newSetIdentityFeedbackForwardingEnabledResponse
--
--         , responseSetIdentityHeadersInNotificationsEnabled $
--             newSetIdentityHeadersInNotificationsEnabledResponse
--
--         , responseSetIdentityMailFromDomain $
--             newSetIdentityMailFromDomainResponse
--
--         , responseSetIdentityNotificationTopic $
--             newSetIdentityNotificationTopicResponse
--
--         , responseSetReceiptRulePosition $
--             newSetReceiptRulePositionResponse
--
--         , responseTestRenderTemplate $
--             newTestRenderTemplateResponse
--
--         , responseUpdateAccountSendingEnabled $
--             newUpdateAccountSendingEnabledResponse
--
--         , responseUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestinationResponse
--
--         , responseUpdateConfigurationSetReputationMetricsEnabled $
--             newUpdateConfigurationSetReputationMetricsEnabledResponse
--
--         , responseUpdateConfigurationSetSendingEnabled $
--             newUpdateConfigurationSetSendingEnabledResponse
--
--         , responseUpdateConfigurationSetTrackingOptions $
--             newUpdateConfigurationSetTrackingOptionsResponse
--
--         , responseUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplateResponse
--
--         , responseUpdateReceiptRule $
--             newUpdateReceiptRuleResponse
--
--         , responseUpdateTemplate $
--             newUpdateTemplateResponse
--
--         , responseVerifyDomainDkim $
--             newVerifyDomainDkimResponse
--
--         , responseVerifyDomainIdentity $
--             newVerifyDomainIdentityResponse
--
--         , responseVerifyEmailAddress $
--             newVerifyEmailAddressResponse
--
--         , responseVerifyEmailIdentity $
--             newVerifyEmailIdentityResponse
--
--           ]
--     ]

-- Requests

requestCloneReceiptRuleSet :: CloneReceiptRuleSet -> TestTree
requestCloneReceiptRuleSet =
  req
    "CloneReceiptRuleSet"
    "fixture/CloneReceiptRuleSet.yaml"

requestCreateConfigurationSet :: CreateConfigurationSet -> TestTree
requestCreateConfigurationSet =
  req
    "CreateConfigurationSet"
    "fixture/CreateConfigurationSet.yaml"

requestCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestination -> TestTree
requestCreateConfigurationSetEventDestination =
  req
    "CreateConfigurationSetEventDestination"
    "fixture/CreateConfigurationSetEventDestination.yaml"

requestCreateConfigurationSetTrackingOptions :: CreateConfigurationSetTrackingOptions -> TestTree
requestCreateConfigurationSetTrackingOptions =
  req
    "CreateConfigurationSetTrackingOptions"
    "fixture/CreateConfigurationSetTrackingOptions.yaml"

requestCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplate -> TestTree
requestCreateCustomVerificationEmailTemplate =
  req
    "CreateCustomVerificationEmailTemplate"
    "fixture/CreateCustomVerificationEmailTemplate.yaml"

requestCreateReceiptFilter :: CreateReceiptFilter -> TestTree
requestCreateReceiptFilter =
  req
    "CreateReceiptFilter"
    "fixture/CreateReceiptFilter.yaml"

requestCreateReceiptRule :: CreateReceiptRule -> TestTree
requestCreateReceiptRule =
  req
    "CreateReceiptRule"
    "fixture/CreateReceiptRule.yaml"

requestCreateReceiptRuleSet :: CreateReceiptRuleSet -> TestTree
requestCreateReceiptRuleSet =
  req
    "CreateReceiptRuleSet"
    "fixture/CreateReceiptRuleSet.yaml"

requestCreateTemplate :: CreateTemplate -> TestTree
requestCreateTemplate =
  req
    "CreateTemplate"
    "fixture/CreateTemplate.yaml"

requestDeleteConfigurationSet :: DeleteConfigurationSet -> TestTree
requestDeleteConfigurationSet =
  req
    "DeleteConfigurationSet"
    "fixture/DeleteConfigurationSet.yaml"

requestDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestination -> TestTree
requestDeleteConfigurationSetEventDestination =
  req
    "DeleteConfigurationSetEventDestination"
    "fixture/DeleteConfigurationSetEventDestination.yaml"

requestDeleteConfigurationSetTrackingOptions :: DeleteConfigurationSetTrackingOptions -> TestTree
requestDeleteConfigurationSetTrackingOptions =
  req
    "DeleteConfigurationSetTrackingOptions"
    "fixture/DeleteConfigurationSetTrackingOptions.yaml"

requestDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplate -> TestTree
requestDeleteCustomVerificationEmailTemplate =
  req
    "DeleteCustomVerificationEmailTemplate"
    "fixture/DeleteCustomVerificationEmailTemplate.yaml"

requestDeleteIdentity :: DeleteIdentity -> TestTree
requestDeleteIdentity =
  req
    "DeleteIdentity"
    "fixture/DeleteIdentity.yaml"

requestDeleteIdentityPolicy :: DeleteIdentityPolicy -> TestTree
requestDeleteIdentityPolicy =
  req
    "DeleteIdentityPolicy"
    "fixture/DeleteIdentityPolicy.yaml"

requestDeleteReceiptFilter :: DeleteReceiptFilter -> TestTree
requestDeleteReceiptFilter =
  req
    "DeleteReceiptFilter"
    "fixture/DeleteReceiptFilter.yaml"

requestDeleteReceiptRule :: DeleteReceiptRule -> TestTree
requestDeleteReceiptRule =
  req
    "DeleteReceiptRule"
    "fixture/DeleteReceiptRule.yaml"

requestDeleteReceiptRuleSet :: DeleteReceiptRuleSet -> TestTree
requestDeleteReceiptRuleSet =
  req
    "DeleteReceiptRuleSet"
    "fixture/DeleteReceiptRuleSet.yaml"

requestDeleteTemplate :: DeleteTemplate -> TestTree
requestDeleteTemplate =
  req
    "DeleteTemplate"
    "fixture/DeleteTemplate.yaml"

requestDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddress -> TestTree
requestDeleteVerifiedEmailAddress =
  req
    "DeleteVerifiedEmailAddress"
    "fixture/DeleteVerifiedEmailAddress.yaml"

requestDescribeActiveReceiptRuleSet :: DescribeActiveReceiptRuleSet -> TestTree
requestDescribeActiveReceiptRuleSet =
  req
    "DescribeActiveReceiptRuleSet"
    "fixture/DescribeActiveReceiptRuleSet.yaml"

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

requestDescribeReceiptRuleSet :: DescribeReceiptRuleSet -> TestTree
requestDescribeReceiptRuleSet =
  req
    "DescribeReceiptRuleSet"
    "fixture/DescribeReceiptRuleSet.yaml"

requestGetAccountSendingEnabled :: GetAccountSendingEnabled -> TestTree
requestGetAccountSendingEnabled =
  req
    "GetAccountSendingEnabled"
    "fixture/GetAccountSendingEnabled.yaml"

requestGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplate -> TestTree
requestGetCustomVerificationEmailTemplate =
  req
    "GetCustomVerificationEmailTemplate"
    "fixture/GetCustomVerificationEmailTemplate.yaml"

requestGetIdentityDkimAttributes :: GetIdentityDkimAttributes -> TestTree
requestGetIdentityDkimAttributes =
  req
    "GetIdentityDkimAttributes"
    "fixture/GetIdentityDkimAttributes.yaml"

requestGetIdentityMailFromDomainAttributes :: GetIdentityMailFromDomainAttributes -> TestTree
requestGetIdentityMailFromDomainAttributes =
  req
    "GetIdentityMailFromDomainAttributes"
    "fixture/GetIdentityMailFromDomainAttributes.yaml"

requestGetIdentityNotificationAttributes :: GetIdentityNotificationAttributes -> TestTree
requestGetIdentityNotificationAttributes =
  req
    "GetIdentityNotificationAttributes"
    "fixture/GetIdentityNotificationAttributes.yaml"

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

requestGetSendQuota :: GetSendQuota -> TestTree
requestGetSendQuota =
  req
    "GetSendQuota"
    "fixture/GetSendQuota.yaml"

requestGetSendStatistics :: GetSendStatistics -> TestTree
requestGetSendStatistics =
  req
    "GetSendStatistics"
    "fixture/GetSendStatistics.yaml"

requestGetTemplate :: GetTemplate -> TestTree
requestGetTemplate =
  req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

requestListConfigurationSets :: ListConfigurationSets -> TestTree
requestListConfigurationSets =
  req
    "ListConfigurationSets"
    "fixture/ListConfigurationSets.yaml"

requestListCustomVerificationEmailTemplates :: ListCustomVerificationEmailTemplates -> TestTree
requestListCustomVerificationEmailTemplates =
  req
    "ListCustomVerificationEmailTemplates"
    "fixture/ListCustomVerificationEmailTemplates.yaml"

requestListIdentities :: ListIdentities -> TestTree
requestListIdentities =
  req
    "ListIdentities"
    "fixture/ListIdentities.yaml"

requestListIdentityPolicies :: ListIdentityPolicies -> TestTree
requestListIdentityPolicies =
  req
    "ListIdentityPolicies"
    "fixture/ListIdentityPolicies.yaml"

requestListReceiptFilters :: ListReceiptFilters -> TestTree
requestListReceiptFilters =
  req
    "ListReceiptFilters"
    "fixture/ListReceiptFilters.yaml"

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

requestListVerifiedEmailAddresses :: ListVerifiedEmailAddresses -> TestTree
requestListVerifiedEmailAddresses =
  req
    "ListVerifiedEmailAddresses"
    "fixture/ListVerifiedEmailAddresses.yaml"

requestPutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptions -> TestTree
requestPutConfigurationSetDeliveryOptions =
  req
    "PutConfigurationSetDeliveryOptions"
    "fixture/PutConfigurationSetDeliveryOptions.yaml"

requestPutIdentityPolicy :: PutIdentityPolicy -> TestTree
requestPutIdentityPolicy =
  req
    "PutIdentityPolicy"
    "fixture/PutIdentityPolicy.yaml"

requestReorderReceiptRuleSet :: ReorderReceiptRuleSet -> TestTree
requestReorderReceiptRuleSet =
  req
    "ReorderReceiptRuleSet"
    "fixture/ReorderReceiptRuleSet.yaml"

requestSendBounce :: SendBounce -> TestTree
requestSendBounce =
  req
    "SendBounce"
    "fixture/SendBounce.yaml"

requestSendBulkTemplatedEmail :: SendBulkTemplatedEmail -> TestTree
requestSendBulkTemplatedEmail =
  req
    "SendBulkTemplatedEmail"
    "fixture/SendBulkTemplatedEmail.yaml"

requestSendCustomVerificationEmail :: SendCustomVerificationEmail -> TestTree
requestSendCustomVerificationEmail =
  req
    "SendCustomVerificationEmail"
    "fixture/SendCustomVerificationEmail.yaml"

requestSendEmail :: SendEmail -> TestTree
requestSendEmail =
  req
    "SendEmail"
    "fixture/SendEmail.yaml"

requestSendRawEmail :: SendRawEmail -> TestTree
requestSendRawEmail =
  req
    "SendRawEmail"
    "fixture/SendRawEmail.yaml"

requestSendTemplatedEmail :: SendTemplatedEmail -> TestTree
requestSendTemplatedEmail =
  req
    "SendTemplatedEmail"
    "fixture/SendTemplatedEmail.yaml"

requestSetActiveReceiptRuleSet :: SetActiveReceiptRuleSet -> TestTree
requestSetActiveReceiptRuleSet =
  req
    "SetActiveReceiptRuleSet"
    "fixture/SetActiveReceiptRuleSet.yaml"

requestSetIdentityDkimEnabled :: SetIdentityDkimEnabled -> TestTree
requestSetIdentityDkimEnabled =
  req
    "SetIdentityDkimEnabled"
    "fixture/SetIdentityDkimEnabled.yaml"

requestSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabled -> TestTree
requestSetIdentityFeedbackForwardingEnabled =
  req
    "SetIdentityFeedbackForwardingEnabled"
    "fixture/SetIdentityFeedbackForwardingEnabled.yaml"

requestSetIdentityHeadersInNotificationsEnabled :: SetIdentityHeadersInNotificationsEnabled -> TestTree
requestSetIdentityHeadersInNotificationsEnabled =
  req
    "SetIdentityHeadersInNotificationsEnabled"
    "fixture/SetIdentityHeadersInNotificationsEnabled.yaml"

requestSetIdentityMailFromDomain :: SetIdentityMailFromDomain -> TestTree
requestSetIdentityMailFromDomain =
  req
    "SetIdentityMailFromDomain"
    "fixture/SetIdentityMailFromDomain.yaml"

requestSetIdentityNotificationTopic :: SetIdentityNotificationTopic -> TestTree
requestSetIdentityNotificationTopic =
  req
    "SetIdentityNotificationTopic"
    "fixture/SetIdentityNotificationTopic.yaml"

requestSetReceiptRulePosition :: SetReceiptRulePosition -> TestTree
requestSetReceiptRulePosition =
  req
    "SetReceiptRulePosition"
    "fixture/SetReceiptRulePosition.yaml"

requestTestRenderTemplate :: TestRenderTemplate -> TestTree
requestTestRenderTemplate =
  req
    "TestRenderTemplate"
    "fixture/TestRenderTemplate.yaml"

requestUpdateAccountSendingEnabled :: UpdateAccountSendingEnabled -> TestTree
requestUpdateAccountSendingEnabled =
  req
    "UpdateAccountSendingEnabled"
    "fixture/UpdateAccountSendingEnabled.yaml"

requestUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestination -> TestTree
requestUpdateConfigurationSetEventDestination =
  req
    "UpdateConfigurationSetEventDestination"
    "fixture/UpdateConfigurationSetEventDestination.yaml"

requestUpdateConfigurationSetReputationMetricsEnabled :: UpdateConfigurationSetReputationMetricsEnabled -> TestTree
requestUpdateConfigurationSetReputationMetricsEnabled =
  req
    "UpdateConfigurationSetReputationMetricsEnabled"
    "fixture/UpdateConfigurationSetReputationMetricsEnabled.yaml"

requestUpdateConfigurationSetSendingEnabled :: UpdateConfigurationSetSendingEnabled -> TestTree
requestUpdateConfigurationSetSendingEnabled =
  req
    "UpdateConfigurationSetSendingEnabled"
    "fixture/UpdateConfigurationSetSendingEnabled.yaml"

requestUpdateConfigurationSetTrackingOptions :: UpdateConfigurationSetTrackingOptions -> TestTree
requestUpdateConfigurationSetTrackingOptions =
  req
    "UpdateConfigurationSetTrackingOptions"
    "fixture/UpdateConfigurationSetTrackingOptions.yaml"

requestUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplate -> TestTree
requestUpdateCustomVerificationEmailTemplate =
  req
    "UpdateCustomVerificationEmailTemplate"
    "fixture/UpdateCustomVerificationEmailTemplate.yaml"

requestUpdateReceiptRule :: UpdateReceiptRule -> TestTree
requestUpdateReceiptRule =
  req
    "UpdateReceiptRule"
    "fixture/UpdateReceiptRule.yaml"

requestUpdateTemplate :: UpdateTemplate -> TestTree
requestUpdateTemplate =
  req
    "UpdateTemplate"
    "fixture/UpdateTemplate.yaml"

requestVerifyDomainDkim :: VerifyDomainDkim -> TestTree
requestVerifyDomainDkim =
  req
    "VerifyDomainDkim"
    "fixture/VerifyDomainDkim.yaml"

requestVerifyDomainIdentity :: VerifyDomainIdentity -> TestTree
requestVerifyDomainIdentity =
  req
    "VerifyDomainIdentity"
    "fixture/VerifyDomainIdentity.yaml"

requestVerifyEmailAddress :: VerifyEmailAddress -> TestTree
requestVerifyEmailAddress =
  req
    "VerifyEmailAddress"
    "fixture/VerifyEmailAddress.yaml"

requestVerifyEmailIdentity :: VerifyEmailIdentity -> TestTree
requestVerifyEmailIdentity =
  req
    "VerifyEmailIdentity"
    "fixture/VerifyEmailIdentity.yaml"

-- Responses

responseCloneReceiptRuleSet :: CloneReceiptRuleSetResponse -> TestTree
responseCloneReceiptRuleSet =
  res
    "CloneReceiptRuleSetResponse"
    "fixture/CloneReceiptRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CloneReceiptRuleSet)

responseCreateConfigurationSet :: CreateConfigurationSetResponse -> TestTree
responseCreateConfigurationSet =
  res
    "CreateConfigurationSetResponse"
    "fixture/CreateConfigurationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfigurationSet)

responseCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestinationResponse -> TestTree
responseCreateConfigurationSetEventDestination =
  res
    "CreateConfigurationSetEventDestinationResponse"
    "fixture/CreateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfigurationSetEventDestination)

responseCreateConfigurationSetTrackingOptions :: CreateConfigurationSetTrackingOptionsResponse -> TestTree
responseCreateConfigurationSetTrackingOptions =
  res
    "CreateConfigurationSetTrackingOptionsResponse"
    "fixture/CreateConfigurationSetTrackingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfigurationSetTrackingOptions)

responseCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplateResponse -> TestTree
responseCreateCustomVerificationEmailTemplate =
  res
    "CreateCustomVerificationEmailTemplateResponse"
    "fixture/CreateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomVerificationEmailTemplate)

responseCreateReceiptFilter :: CreateReceiptFilterResponse -> TestTree
responseCreateReceiptFilter =
  res
    "CreateReceiptFilterResponse"
    "fixture/CreateReceiptFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReceiptFilter)

responseCreateReceiptRule :: CreateReceiptRuleResponse -> TestTree
responseCreateReceiptRule =
  res
    "CreateReceiptRuleResponse"
    "fixture/CreateReceiptRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReceiptRule)

responseCreateReceiptRuleSet :: CreateReceiptRuleSetResponse -> TestTree
responseCreateReceiptRuleSet =
  res
    "CreateReceiptRuleSetResponse"
    "fixture/CreateReceiptRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReceiptRuleSet)

responseCreateTemplate :: CreateTemplateResponse -> TestTree
responseCreateTemplate =
  res
    "CreateTemplateResponse"
    "fixture/CreateTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTemplate)

responseDeleteConfigurationSet :: DeleteConfigurationSetResponse -> TestTree
responseDeleteConfigurationSet =
  res
    "DeleteConfigurationSetResponse"
    "fixture/DeleteConfigurationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationSet)

responseDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestinationResponse -> TestTree
responseDeleteConfigurationSetEventDestination =
  res
    "DeleteConfigurationSetEventDestinationResponse"
    "fixture/DeleteConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationSetEventDestination)

responseDeleteConfigurationSetTrackingOptions :: DeleteConfigurationSetTrackingOptionsResponse -> TestTree
responseDeleteConfigurationSetTrackingOptions =
  res
    "DeleteConfigurationSetTrackingOptionsResponse"
    "fixture/DeleteConfigurationSetTrackingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationSetTrackingOptions)

responseDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplateResponse -> TestTree
responseDeleteCustomVerificationEmailTemplate =
  res
    "DeleteCustomVerificationEmailTemplateResponse"
    "fixture/DeleteCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomVerificationEmailTemplate)

responseDeleteIdentity :: DeleteIdentityResponse -> TestTree
responseDeleteIdentity =
  res
    "DeleteIdentityResponse"
    "fixture/DeleteIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIdentity)

responseDeleteIdentityPolicy :: DeleteIdentityPolicyResponse -> TestTree
responseDeleteIdentityPolicy =
  res
    "DeleteIdentityPolicyResponse"
    "fixture/DeleteIdentityPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIdentityPolicy)

responseDeleteReceiptFilter :: DeleteReceiptFilterResponse -> TestTree
responseDeleteReceiptFilter =
  res
    "DeleteReceiptFilterResponse"
    "fixture/DeleteReceiptFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReceiptFilter)

responseDeleteReceiptRule :: DeleteReceiptRuleResponse -> TestTree
responseDeleteReceiptRule =
  res
    "DeleteReceiptRuleResponse"
    "fixture/DeleteReceiptRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReceiptRule)

responseDeleteReceiptRuleSet :: DeleteReceiptRuleSetResponse -> TestTree
responseDeleteReceiptRuleSet =
  res
    "DeleteReceiptRuleSetResponse"
    "fixture/DeleteReceiptRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReceiptRuleSet)

responseDeleteTemplate :: DeleteTemplateResponse -> TestTree
responseDeleteTemplate =
  res
    "DeleteTemplateResponse"
    "fixture/DeleteTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTemplate)

responseDeleteVerifiedEmailAddress :: DeleteVerifiedEmailAddressResponse -> TestTree
responseDeleteVerifiedEmailAddress =
  res
    "DeleteVerifiedEmailAddressResponse"
    "fixture/DeleteVerifiedEmailAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVerifiedEmailAddress)

responseDescribeActiveReceiptRuleSet :: DescribeActiveReceiptRuleSetResponse -> TestTree
responseDescribeActiveReceiptRuleSet =
  res
    "DescribeActiveReceiptRuleSetResponse"
    "fixture/DescribeActiveReceiptRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeActiveReceiptRuleSet)

responseDescribeConfigurationSet :: DescribeConfigurationSetResponse -> TestTree
responseDescribeConfigurationSet =
  res
    "DescribeConfigurationSetResponse"
    "fixture/DescribeConfigurationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationSet)

responseDescribeReceiptRule :: DescribeReceiptRuleResponse -> TestTree
responseDescribeReceiptRule =
  res
    "DescribeReceiptRuleResponse"
    "fixture/DescribeReceiptRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReceiptRule)

responseDescribeReceiptRuleSet :: DescribeReceiptRuleSetResponse -> TestTree
responseDescribeReceiptRuleSet =
  res
    "DescribeReceiptRuleSetResponse"
    "fixture/DescribeReceiptRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReceiptRuleSet)

responseGetAccountSendingEnabled :: GetAccountSendingEnabledResponse -> TestTree
responseGetAccountSendingEnabled =
  res
    "GetAccountSendingEnabledResponse"
    "fixture/GetAccountSendingEnabledResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountSendingEnabled)

responseGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplateResponse -> TestTree
responseGetCustomVerificationEmailTemplate =
  res
    "GetCustomVerificationEmailTemplateResponse"
    "fixture/GetCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCustomVerificationEmailTemplate)

responseGetIdentityDkimAttributes :: GetIdentityDkimAttributesResponse -> TestTree
responseGetIdentityDkimAttributes =
  res
    "GetIdentityDkimAttributesResponse"
    "fixture/GetIdentityDkimAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentityDkimAttributes)

responseGetIdentityMailFromDomainAttributes :: GetIdentityMailFromDomainAttributesResponse -> TestTree
responseGetIdentityMailFromDomainAttributes =
  res
    "GetIdentityMailFromDomainAttributesResponse"
    "fixture/GetIdentityMailFromDomainAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentityMailFromDomainAttributes)

responseGetIdentityNotificationAttributes :: GetIdentityNotificationAttributesResponse -> TestTree
responseGetIdentityNotificationAttributes =
  res
    "GetIdentityNotificationAttributesResponse"
    "fixture/GetIdentityNotificationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentityNotificationAttributes)

responseGetIdentityPolicies :: GetIdentityPoliciesResponse -> TestTree
responseGetIdentityPolicies =
  res
    "GetIdentityPoliciesResponse"
    "fixture/GetIdentityPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentityPolicies)

responseGetIdentityVerificationAttributes :: GetIdentityVerificationAttributesResponse -> TestTree
responseGetIdentityVerificationAttributes =
  res
    "GetIdentityVerificationAttributesResponse"
    "fixture/GetIdentityVerificationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentityVerificationAttributes)

responseGetSendQuota :: GetSendQuotaResponse -> TestTree
responseGetSendQuota =
  res
    "GetSendQuotaResponse"
    "fixture/GetSendQuotaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSendQuota)

responseGetSendStatistics :: GetSendStatisticsResponse -> TestTree
responseGetSendStatistics =
  res
    "GetSendStatisticsResponse"
    "fixture/GetSendStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSendStatistics)

responseGetTemplate :: GetTemplateResponse -> TestTree
responseGetTemplate =
  res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemplate)

responseListConfigurationSets :: ListConfigurationSetsResponse -> TestTree
responseListConfigurationSets =
  res
    "ListConfigurationSetsResponse"
    "fixture/ListConfigurationSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurationSets)

responseListCustomVerificationEmailTemplates :: ListCustomVerificationEmailTemplatesResponse -> TestTree
responseListCustomVerificationEmailTemplates =
  res
    "ListCustomVerificationEmailTemplatesResponse"
    "fixture/ListCustomVerificationEmailTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomVerificationEmailTemplates)

responseListIdentities :: ListIdentitiesResponse -> TestTree
responseListIdentities =
  res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentities)

responseListIdentityPolicies :: ListIdentityPoliciesResponse -> TestTree
responseListIdentityPolicies =
  res
    "ListIdentityPoliciesResponse"
    "fixture/ListIdentityPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentityPolicies)

responseListReceiptFilters :: ListReceiptFiltersResponse -> TestTree
responseListReceiptFilters =
  res
    "ListReceiptFiltersResponse"
    "fixture/ListReceiptFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReceiptFilters)

responseListReceiptRuleSets :: ListReceiptRuleSetsResponse -> TestTree
responseListReceiptRuleSets =
  res
    "ListReceiptRuleSetsResponse"
    "fixture/ListReceiptRuleSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReceiptRuleSets)

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplates)

responseListVerifiedEmailAddresses :: ListVerifiedEmailAddressesResponse -> TestTree
responseListVerifiedEmailAddresses =
  res
    "ListVerifiedEmailAddressesResponse"
    "fixture/ListVerifiedEmailAddressesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVerifiedEmailAddresses)

responsePutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptionsResponse -> TestTree
responsePutConfigurationSetDeliveryOptions =
  res
    "PutConfigurationSetDeliveryOptionsResponse"
    "fixture/PutConfigurationSetDeliveryOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigurationSetDeliveryOptions)

responsePutIdentityPolicy :: PutIdentityPolicyResponse -> TestTree
responsePutIdentityPolicy =
  res
    "PutIdentityPolicyResponse"
    "fixture/PutIdentityPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutIdentityPolicy)

responseReorderReceiptRuleSet :: ReorderReceiptRuleSetResponse -> TestTree
responseReorderReceiptRuleSet =
  res
    "ReorderReceiptRuleSetResponse"
    "fixture/ReorderReceiptRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReorderReceiptRuleSet)

responseSendBounce :: SendBounceResponse -> TestTree
responseSendBounce =
  res
    "SendBounceResponse"
    "fixture/SendBounceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendBounce)

responseSendBulkTemplatedEmail :: SendBulkTemplatedEmailResponse -> TestTree
responseSendBulkTemplatedEmail =
  res
    "SendBulkTemplatedEmailResponse"
    "fixture/SendBulkTemplatedEmailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendBulkTemplatedEmail)

responseSendCustomVerificationEmail :: SendCustomVerificationEmailResponse -> TestTree
responseSendCustomVerificationEmail =
  res
    "SendCustomVerificationEmailResponse"
    "fixture/SendCustomVerificationEmailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendCustomVerificationEmail)

responseSendEmail :: SendEmailResponse -> TestTree
responseSendEmail =
  res
    "SendEmailResponse"
    "fixture/SendEmailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendEmail)

responseSendRawEmail :: SendRawEmailResponse -> TestTree
responseSendRawEmail =
  res
    "SendRawEmailResponse"
    "fixture/SendRawEmailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendRawEmail)

responseSendTemplatedEmail :: SendTemplatedEmailResponse -> TestTree
responseSendTemplatedEmail =
  res
    "SendTemplatedEmailResponse"
    "fixture/SendTemplatedEmailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendTemplatedEmail)

responseSetActiveReceiptRuleSet :: SetActiveReceiptRuleSetResponse -> TestTree
responseSetActiveReceiptRuleSet =
  res
    "SetActiveReceiptRuleSetResponse"
    "fixture/SetActiveReceiptRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetActiveReceiptRuleSet)

responseSetIdentityDkimEnabled :: SetIdentityDkimEnabledResponse -> TestTree
responseSetIdentityDkimEnabled =
  res
    "SetIdentityDkimEnabledResponse"
    "fixture/SetIdentityDkimEnabledResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetIdentityDkimEnabled)

responseSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabledResponse -> TestTree
responseSetIdentityFeedbackForwardingEnabled =
  res
    "SetIdentityFeedbackForwardingEnabledResponse"
    "fixture/SetIdentityFeedbackForwardingEnabledResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetIdentityFeedbackForwardingEnabled)

responseSetIdentityHeadersInNotificationsEnabled :: SetIdentityHeadersInNotificationsEnabledResponse -> TestTree
responseSetIdentityHeadersInNotificationsEnabled =
  res
    "SetIdentityHeadersInNotificationsEnabledResponse"
    "fixture/SetIdentityHeadersInNotificationsEnabledResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetIdentityHeadersInNotificationsEnabled)

responseSetIdentityMailFromDomain :: SetIdentityMailFromDomainResponse -> TestTree
responseSetIdentityMailFromDomain =
  res
    "SetIdentityMailFromDomainResponse"
    "fixture/SetIdentityMailFromDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetIdentityMailFromDomain)

responseSetIdentityNotificationTopic :: SetIdentityNotificationTopicResponse -> TestTree
responseSetIdentityNotificationTopic =
  res
    "SetIdentityNotificationTopicResponse"
    "fixture/SetIdentityNotificationTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetIdentityNotificationTopic)

responseSetReceiptRulePosition :: SetReceiptRulePositionResponse -> TestTree
responseSetReceiptRulePosition =
  res
    "SetReceiptRulePositionResponse"
    "fixture/SetReceiptRulePositionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetReceiptRulePosition)

responseTestRenderTemplate :: TestRenderTemplateResponse -> TestTree
responseTestRenderTemplate =
  res
    "TestRenderTemplateResponse"
    "fixture/TestRenderTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestRenderTemplate)

responseUpdateAccountSendingEnabled :: UpdateAccountSendingEnabledResponse -> TestTree
responseUpdateAccountSendingEnabled =
  res
    "UpdateAccountSendingEnabledResponse"
    "fixture/UpdateAccountSendingEnabledResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccountSendingEnabled)

responseUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestinationResponse -> TestTree
responseUpdateConfigurationSetEventDestination =
  res
    "UpdateConfigurationSetEventDestinationResponse"
    "fixture/UpdateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfigurationSetEventDestination)

responseUpdateConfigurationSetReputationMetricsEnabled :: UpdateConfigurationSetReputationMetricsEnabledResponse -> TestTree
responseUpdateConfigurationSetReputationMetricsEnabled =
  res
    "UpdateConfigurationSetReputationMetricsEnabledResponse"
    "fixture/UpdateConfigurationSetReputationMetricsEnabledResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfigurationSetReputationMetricsEnabled)

responseUpdateConfigurationSetSendingEnabled :: UpdateConfigurationSetSendingEnabledResponse -> TestTree
responseUpdateConfigurationSetSendingEnabled =
  res
    "UpdateConfigurationSetSendingEnabledResponse"
    "fixture/UpdateConfigurationSetSendingEnabledResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfigurationSetSendingEnabled)

responseUpdateConfigurationSetTrackingOptions :: UpdateConfigurationSetTrackingOptionsResponse -> TestTree
responseUpdateConfigurationSetTrackingOptions =
  res
    "UpdateConfigurationSetTrackingOptionsResponse"
    "fixture/UpdateConfigurationSetTrackingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfigurationSetTrackingOptions)

responseUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplateResponse -> TestTree
responseUpdateCustomVerificationEmailTemplate =
  res
    "UpdateCustomVerificationEmailTemplateResponse"
    "fixture/UpdateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomVerificationEmailTemplate)

responseUpdateReceiptRule :: UpdateReceiptRuleResponse -> TestTree
responseUpdateReceiptRule =
  res
    "UpdateReceiptRuleResponse"
    "fixture/UpdateReceiptRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReceiptRule)

responseUpdateTemplate :: UpdateTemplateResponse -> TestTree
responseUpdateTemplate =
  res
    "UpdateTemplateResponse"
    "fixture/UpdateTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTemplate)

responseVerifyDomainDkim :: VerifyDomainDkimResponse -> TestTree
responseVerifyDomainDkim =
  res
    "VerifyDomainDkimResponse"
    "fixture/VerifyDomainDkimResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyDomainDkim)

responseVerifyDomainIdentity :: VerifyDomainIdentityResponse -> TestTree
responseVerifyDomainIdentity =
  res
    "VerifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyDomainIdentity)

responseVerifyEmailAddress :: VerifyEmailAddressResponse -> TestTree
responseVerifyEmailAddress =
  res
    "VerifyEmailAddressResponse"
    "fixture/VerifyEmailAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyEmailAddress)

responseVerifyEmailIdentity :: VerifyEmailIdentityResponse -> TestTree
responseVerifyEmailIdentity =
  res
    "VerifyEmailIdentityResponse"
    "fixture/VerifyEmailIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyEmailIdentity)
