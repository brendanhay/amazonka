{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SES
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             createTemplate
--
--         , requestDeleteConfigurationSetTrackingOptions $
--             deleteConfigurationSetTrackingOptions
--
--         , requestUpdateConfigurationSetTrackingOptions $
--             updateConfigurationSetTrackingOptions
--
--         , requestCreateReceiptRuleSet $
--             createReceiptRuleSet
--
--         , requestSetIdentityHeadersInNotificationsEnabled $
--             setIdentityHeadersInNotificationsEnabled
--
--         , requestGetSendQuota $
--             getSendQuota
--
--         , requestDescribeConfigurationSet $
--             describeConfigurationSet
--
--         , requestPutIdentityPolicy $
--             putIdentityPolicy
--
--         , requestDeleteCustomVerificationEmailTemplate $
--             deleteCustomVerificationEmailTemplate
--
--         , requestDeleteIdentityPolicy $
--             deleteIdentityPolicy
--
--         , requestUpdateCustomVerificationEmailTemplate $
--             updateCustomVerificationEmailTemplate
--
--         , requestSendCustomVerificationEmail $
--             sendCustomVerificationEmail
--
--         , requestGetIdentityNotificationAttributes $
--             getIdentityNotificationAttributes
--
--         , requestUpdateConfigurationSetReputationMetricsEnabled $
--             updateConfigurationSetReputationMetricsEnabled
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
--         , requestUpdateConfigurationSetEventDestination $
--             updateConfigurationSetEventDestination
--
--         , requestDeleteConfigurationSetEventDestination $
--             deleteConfigurationSetEventDestination
--
--         , requestSetIdentityMailFromDomain $
--             setIdentityMailFromDomain
--
--         , requestSetIdentityFeedbackForwardingEnabled $
--             setIdentityFeedbackForwardingEnabled
--
--         , requestListConfigurationSets $
--             listConfigurationSets
--
--         , requestDeleteConfigurationSet $
--             deleteConfigurationSet
--
--         , requestGetIdentityVerificationAttributes $
--             getIdentityVerificationAttributes
--
--         , requestGetIdentityPolicies $
--             getIdentityPolicies
--
--         , requestListTemplates $
--             listTemplates
--
--         , requestVerifyDomainIdentity $
--             verifyDomainIdentity
--
--         , requestUpdateTemplate $
--             updateTemplate
--
--         , requestDeleteTemplate $
--             deleteTemplate
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
--         , requestSendTemplatedEmail $
--             sendTemplatedEmail
--
--         , requestVerifyDomainDkim $
--             verifyDomainDkim
--
--         , requestTestRenderTemplate $
--             testRenderTemplate
--
--         , requestSendBulkTemplatedEmail $
--             sendBulkTemplatedEmail
--
--         , requestSendRawEmail $
--             sendRawEmail
--
--         , requestGetSendStatistics $
--             getSendStatistics
--
--         , requestListCustomVerificationEmailTemplates $
--             listCustomVerificationEmailTemplates
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
--         , requestUpdateConfigurationSetSendingEnabled $
--             updateConfigurationSetSendingEnabled
--
--         , requestCreateCustomVerificationEmailTemplate $
--             createCustomVerificationEmailTemplate
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
--         , requestGetCustomVerificationEmailTemplate $
--             getCustomVerificationEmailTemplate
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
--         , requestCreateConfigurationSetEventDestination $
--             createConfigurationSetEventDestination
--
--         , requestGetAccountSendingEnabled $
--             getAccountSendingEnabled
--
--         , requestCreateReceiptRule $
--             createReceiptRule
--
--         , requestGetTemplate $
--             getTemplate
--
--         , requestSetActiveReceiptRuleSet $
--             setActiveReceiptRuleSet
--
--         , requestCreateConfigurationSet $
--             createConfigurationSet
--
--         , requestUpdateAccountSendingEnabled $
--             updateAccountSendingEnabled
--
--         , requestCreateConfigurationSetTrackingOptions $
--             createConfigurationSetTrackingOptions
--
--         , requestDescribeActiveReceiptRuleSet $
--             describeActiveReceiptRuleSet
--
--           ]

--     , testGroup "response"
--         [ responseCreateTemplate $
--             createTemplateResponse
--
--         , responseDeleteConfigurationSetTrackingOptions $
--             deleteConfigurationSetTrackingOptionsResponse
--
--         , responseUpdateConfigurationSetTrackingOptions $
--             updateConfigurationSetTrackingOptionsResponse
--
--         , responseCreateReceiptRuleSet $
--             createReceiptRuleSetResponse
--
--         , responseSetIdentityHeadersInNotificationsEnabled $
--             setIdentityHeadersInNotificationsEnabledResponse
--
--         , responseGetSendQuota $
--             getSendQuotaResponse
--
--         , responseDescribeConfigurationSet $
--             describeConfigurationSetResponse
--
--         , responsePutIdentityPolicy $
--             putIdentityPolicyResponse
--
--         , responseDeleteCustomVerificationEmailTemplate $
--             deleteCustomVerificationEmailTemplateResponse
--
--         , responseDeleteIdentityPolicy $
--             deleteIdentityPolicyResponse
--
--         , responseUpdateCustomVerificationEmailTemplate $
--             updateCustomVerificationEmailTemplateResponse
--
--         , responseSendCustomVerificationEmail $
--             sendCustomVerificationEmailResponse
--
--         , responseGetIdentityNotificationAttributes $
--             getIdentityNotificationAttributesResponse
--
--         , responseUpdateConfigurationSetReputationMetricsEnabled $
--             updateConfigurationSetReputationMetricsEnabledResponse
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
--         , responseUpdateConfigurationSetEventDestination $
--             updateConfigurationSetEventDestinationResponse
--
--         , responseDeleteConfigurationSetEventDestination $
--             deleteConfigurationSetEventDestinationResponse
--
--         , responseSetIdentityMailFromDomain $
--             setIdentityMailFromDomainResponse
--
--         , responseSetIdentityFeedbackForwardingEnabled $
--             setIdentityFeedbackForwardingEnabledResponse
--
--         , responseListConfigurationSets $
--             listConfigurationSetsResponse
--
--         , responseDeleteConfigurationSet $
--             deleteConfigurationSetResponse
--
--         , responseGetIdentityVerificationAttributes $
--             getIdentityVerificationAttributesResponse
--
--         , responseGetIdentityPolicies $
--             getIdentityPoliciesResponse
--
--         , responseListTemplates $
--             listTemplatesResponse
--
--         , responseVerifyDomainIdentity $
--             verifyDomainIdentityResponse
--
--         , responseUpdateTemplate $
--             updateTemplateResponse
--
--         , responseDeleteTemplate $
--             deleteTemplateResponse
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
--         , responseSendTemplatedEmail $
--             sendTemplatedEmailResponse
--
--         , responseVerifyDomainDkim $
--             verifyDomainDkimResponse
--
--         , responseTestRenderTemplate $
--             testRenderTemplateResponse
--
--         , responseSendBulkTemplatedEmail $
--             sendBulkTemplatedEmailResponse
--
--         , responseSendRawEmail $
--             sendRawEmailResponse
--
--         , responseGetSendStatistics $
--             getSendStatisticsResponse
--
--         , responseListCustomVerificationEmailTemplates $
--             listCustomVerificationEmailTemplatesResponse
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
--         , responseUpdateConfigurationSetSendingEnabled $
--             updateConfigurationSetSendingEnabledResponse
--
--         , responseCreateCustomVerificationEmailTemplate $
--             createCustomVerificationEmailTemplateResponse
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
--         , responseGetCustomVerificationEmailTemplate $
--             getCustomVerificationEmailTemplateResponse
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
--         , responseCreateConfigurationSetEventDestination $
--             createConfigurationSetEventDestinationResponse
--
--         , responseGetAccountSendingEnabled $
--             getAccountSendingEnabledResponse
--
--         , responseCreateReceiptRule $
--             createReceiptRuleResponse
--
--         , responseGetTemplate $
--             getTemplateResponse
--
--         , responseSetActiveReceiptRuleSet $
--             setActiveReceiptRuleSetResponse
--
--         , responseCreateConfigurationSet $
--             createConfigurationSetResponse
--
--         , responseUpdateAccountSendingEnabled $
--             updateAccountSendingEnabledResponse
--
--         , responseCreateConfigurationSetTrackingOptions $
--             createConfigurationSetTrackingOptionsResponse
--
--         , responseDescribeActiveReceiptRuleSet $
--             describeActiveReceiptRuleSetResponse
--
--           ]
--     ]

-- Requests

requestCreateTemplate :: CreateTemplate -> TestTree
requestCreateTemplate = req
    "CreateTemplate"
    "fixture/CreateTemplate.yaml"

requestDeleteConfigurationSetTrackingOptions :: DeleteConfigurationSetTrackingOptions -> TestTree
requestDeleteConfigurationSetTrackingOptions = req
    "DeleteConfigurationSetTrackingOptions"
    "fixture/DeleteConfigurationSetTrackingOptions.yaml"

requestUpdateConfigurationSetTrackingOptions :: UpdateConfigurationSetTrackingOptions -> TestTree
requestUpdateConfigurationSetTrackingOptions = req
    "UpdateConfigurationSetTrackingOptions"
    "fixture/UpdateConfigurationSetTrackingOptions.yaml"

requestCreateReceiptRuleSet :: CreateReceiptRuleSet -> TestTree
requestCreateReceiptRuleSet = req
    "CreateReceiptRuleSet"
    "fixture/CreateReceiptRuleSet.yaml"

requestSetIdentityHeadersInNotificationsEnabled :: SetIdentityHeadersInNotificationsEnabled -> TestTree
requestSetIdentityHeadersInNotificationsEnabled = req
    "SetIdentityHeadersInNotificationsEnabled"
    "fixture/SetIdentityHeadersInNotificationsEnabled.yaml"

requestGetSendQuota :: GetSendQuota -> TestTree
requestGetSendQuota = req
    "GetSendQuota"
    "fixture/GetSendQuota.yaml"

requestDescribeConfigurationSet :: DescribeConfigurationSet -> TestTree
requestDescribeConfigurationSet = req
    "DescribeConfigurationSet"
    "fixture/DescribeConfigurationSet.yaml"

requestPutIdentityPolicy :: PutIdentityPolicy -> TestTree
requestPutIdentityPolicy = req
    "PutIdentityPolicy"
    "fixture/PutIdentityPolicy.yaml"

requestDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplate -> TestTree
requestDeleteCustomVerificationEmailTemplate = req
    "DeleteCustomVerificationEmailTemplate"
    "fixture/DeleteCustomVerificationEmailTemplate.yaml"

requestDeleteIdentityPolicy :: DeleteIdentityPolicy -> TestTree
requestDeleteIdentityPolicy = req
    "DeleteIdentityPolicy"
    "fixture/DeleteIdentityPolicy.yaml"

requestUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplate -> TestTree
requestUpdateCustomVerificationEmailTemplate = req
    "UpdateCustomVerificationEmailTemplate"
    "fixture/UpdateCustomVerificationEmailTemplate.yaml"

requestSendCustomVerificationEmail :: SendCustomVerificationEmail -> TestTree
requestSendCustomVerificationEmail = req
    "SendCustomVerificationEmail"
    "fixture/SendCustomVerificationEmail.yaml"

requestGetIdentityNotificationAttributes :: GetIdentityNotificationAttributes -> TestTree
requestGetIdentityNotificationAttributes = req
    "GetIdentityNotificationAttributes"
    "fixture/GetIdentityNotificationAttributes.yaml"

requestUpdateConfigurationSetReputationMetricsEnabled :: UpdateConfigurationSetReputationMetricsEnabled -> TestTree
requestUpdateConfigurationSetReputationMetricsEnabled = req
    "UpdateConfigurationSetReputationMetricsEnabled"
    "fixture/UpdateConfigurationSetReputationMetricsEnabled.yaml"

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

requestUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestination -> TestTree
requestUpdateConfigurationSetEventDestination = req
    "UpdateConfigurationSetEventDestination"
    "fixture/UpdateConfigurationSetEventDestination.yaml"

requestDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestination -> TestTree
requestDeleteConfigurationSetEventDestination = req
    "DeleteConfigurationSetEventDestination"
    "fixture/DeleteConfigurationSetEventDestination.yaml"

requestSetIdentityMailFromDomain :: SetIdentityMailFromDomain -> TestTree
requestSetIdentityMailFromDomain = req
    "SetIdentityMailFromDomain"
    "fixture/SetIdentityMailFromDomain.yaml"

requestSetIdentityFeedbackForwardingEnabled :: SetIdentityFeedbackForwardingEnabled -> TestTree
requestSetIdentityFeedbackForwardingEnabled = req
    "SetIdentityFeedbackForwardingEnabled"
    "fixture/SetIdentityFeedbackForwardingEnabled.yaml"

requestListConfigurationSets :: ListConfigurationSets -> TestTree
requestListConfigurationSets = req
    "ListConfigurationSets"
    "fixture/ListConfigurationSets.yaml"

requestDeleteConfigurationSet :: DeleteConfigurationSet -> TestTree
requestDeleteConfigurationSet = req
    "DeleteConfigurationSet"
    "fixture/DeleteConfigurationSet.yaml"

requestGetIdentityVerificationAttributes :: GetIdentityVerificationAttributes -> TestTree
requestGetIdentityVerificationAttributes = req
    "GetIdentityVerificationAttributes"
    "fixture/GetIdentityVerificationAttributes.yaml"

requestGetIdentityPolicies :: GetIdentityPolicies -> TestTree
requestGetIdentityPolicies = req
    "GetIdentityPolicies"
    "fixture/GetIdentityPolicies.yaml"

requestListTemplates :: ListTemplates -> TestTree
requestListTemplates = req
    "ListTemplates"
    "fixture/ListTemplates.yaml"

requestVerifyDomainIdentity :: VerifyDomainIdentity -> TestTree
requestVerifyDomainIdentity = req
    "VerifyDomainIdentity"
    "fixture/VerifyDomainIdentity.yaml"

requestUpdateTemplate :: UpdateTemplate -> TestTree
requestUpdateTemplate = req
    "UpdateTemplate"
    "fixture/UpdateTemplate.yaml"

requestDeleteTemplate :: DeleteTemplate -> TestTree
requestDeleteTemplate = req
    "DeleteTemplate"
    "fixture/DeleteTemplate.yaml"

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

requestSendTemplatedEmail :: SendTemplatedEmail -> TestTree
requestSendTemplatedEmail = req
    "SendTemplatedEmail"
    "fixture/SendTemplatedEmail.yaml"

requestVerifyDomainDkim :: VerifyDomainDkim -> TestTree
requestVerifyDomainDkim = req
    "VerifyDomainDkim"
    "fixture/VerifyDomainDkim.yaml"

requestTestRenderTemplate :: TestRenderTemplate -> TestTree
requestTestRenderTemplate = req
    "TestRenderTemplate"
    "fixture/TestRenderTemplate.yaml"

requestSendBulkTemplatedEmail :: SendBulkTemplatedEmail -> TestTree
requestSendBulkTemplatedEmail = req
    "SendBulkTemplatedEmail"
    "fixture/SendBulkTemplatedEmail.yaml"

requestSendRawEmail :: SendRawEmail -> TestTree
requestSendRawEmail = req
    "SendRawEmail"
    "fixture/SendRawEmail.yaml"

requestGetSendStatistics :: GetSendStatistics -> TestTree
requestGetSendStatistics = req
    "GetSendStatistics"
    "fixture/GetSendStatistics.yaml"

requestListCustomVerificationEmailTemplates :: ListCustomVerificationEmailTemplates -> TestTree
requestListCustomVerificationEmailTemplates = req
    "ListCustomVerificationEmailTemplates"
    "fixture/ListCustomVerificationEmailTemplates.yaml"

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

requestUpdateConfigurationSetSendingEnabled :: UpdateConfigurationSetSendingEnabled -> TestTree
requestUpdateConfigurationSetSendingEnabled = req
    "UpdateConfigurationSetSendingEnabled"
    "fixture/UpdateConfigurationSetSendingEnabled.yaml"

requestCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplate -> TestTree
requestCreateCustomVerificationEmailTemplate = req
    "CreateCustomVerificationEmailTemplate"
    "fixture/CreateCustomVerificationEmailTemplate.yaml"

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

requestGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplate -> TestTree
requestGetCustomVerificationEmailTemplate = req
    "GetCustomVerificationEmailTemplate"
    "fixture/GetCustomVerificationEmailTemplate.yaml"

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

requestCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestination -> TestTree
requestCreateConfigurationSetEventDestination = req
    "CreateConfigurationSetEventDestination"
    "fixture/CreateConfigurationSetEventDestination.yaml"

requestGetAccountSendingEnabled :: GetAccountSendingEnabled -> TestTree
requestGetAccountSendingEnabled = req
    "GetAccountSendingEnabled"
    "fixture/GetAccountSendingEnabled.yaml"

requestCreateReceiptRule :: CreateReceiptRule -> TestTree
requestCreateReceiptRule = req
    "CreateReceiptRule"
    "fixture/CreateReceiptRule.yaml"

requestGetTemplate :: GetTemplate -> TestTree
requestGetTemplate = req
    "GetTemplate"
    "fixture/GetTemplate.yaml"

requestSetActiveReceiptRuleSet :: SetActiveReceiptRuleSet -> TestTree
requestSetActiveReceiptRuleSet = req
    "SetActiveReceiptRuleSet"
    "fixture/SetActiveReceiptRuleSet.yaml"

requestCreateConfigurationSet :: CreateConfigurationSet -> TestTree
requestCreateConfigurationSet = req
    "CreateConfigurationSet"
    "fixture/CreateConfigurationSet.yaml"

requestUpdateAccountSendingEnabled :: UpdateAccountSendingEnabled -> TestTree
requestUpdateAccountSendingEnabled = req
    "UpdateAccountSendingEnabled"
    "fixture/UpdateAccountSendingEnabled.yaml"

requestCreateConfigurationSetTrackingOptions :: CreateConfigurationSetTrackingOptions -> TestTree
requestCreateConfigurationSetTrackingOptions = req
    "CreateConfigurationSetTrackingOptions"
    "fixture/CreateConfigurationSetTrackingOptions.yaml"

requestDescribeActiveReceiptRuleSet :: DescribeActiveReceiptRuleSet -> TestTree
requestDescribeActiveReceiptRuleSet = req
    "DescribeActiveReceiptRuleSet"
    "fixture/DescribeActiveReceiptRuleSet.yaml"

-- Responses

responseCreateTemplate :: CreateTemplateResponse -> TestTree
responseCreateTemplate = res
    "CreateTemplateResponse"
    "fixture/CreateTemplateResponse.proto"
    ses
    (Proxy :: Proxy CreateTemplate)

responseDeleteConfigurationSetTrackingOptions :: DeleteConfigurationSetTrackingOptionsResponse -> TestTree
responseDeleteConfigurationSetTrackingOptions = res
    "DeleteConfigurationSetTrackingOptionsResponse"
    "fixture/DeleteConfigurationSetTrackingOptionsResponse.proto"
    ses
    (Proxy :: Proxy DeleteConfigurationSetTrackingOptions)

responseUpdateConfigurationSetTrackingOptions :: UpdateConfigurationSetTrackingOptionsResponse -> TestTree
responseUpdateConfigurationSetTrackingOptions = res
    "UpdateConfigurationSetTrackingOptionsResponse"
    "fixture/UpdateConfigurationSetTrackingOptionsResponse.proto"
    ses
    (Proxy :: Proxy UpdateConfigurationSetTrackingOptions)

responseCreateReceiptRuleSet :: CreateReceiptRuleSetResponse -> TestTree
responseCreateReceiptRuleSet = res
    "CreateReceiptRuleSetResponse"
    "fixture/CreateReceiptRuleSetResponse.proto"
    ses
    (Proxy :: Proxy CreateReceiptRuleSet)

responseSetIdentityHeadersInNotificationsEnabled :: SetIdentityHeadersInNotificationsEnabledResponse -> TestTree
responseSetIdentityHeadersInNotificationsEnabled = res
    "SetIdentityHeadersInNotificationsEnabledResponse"
    "fixture/SetIdentityHeadersInNotificationsEnabledResponse.proto"
    ses
    (Proxy :: Proxy SetIdentityHeadersInNotificationsEnabled)

responseGetSendQuota :: GetSendQuotaResponse -> TestTree
responseGetSendQuota = res
    "GetSendQuotaResponse"
    "fixture/GetSendQuotaResponse.proto"
    ses
    (Proxy :: Proxy GetSendQuota)

responseDescribeConfigurationSet :: DescribeConfigurationSetResponse -> TestTree
responseDescribeConfigurationSet = res
    "DescribeConfigurationSetResponse"
    "fixture/DescribeConfigurationSetResponse.proto"
    ses
    (Proxy :: Proxy DescribeConfigurationSet)

responsePutIdentityPolicy :: PutIdentityPolicyResponse -> TestTree
responsePutIdentityPolicy = res
    "PutIdentityPolicyResponse"
    "fixture/PutIdentityPolicyResponse.proto"
    ses
    (Proxy :: Proxy PutIdentityPolicy)

responseDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplateResponse -> TestTree
responseDeleteCustomVerificationEmailTemplate = res
    "DeleteCustomVerificationEmailTemplateResponse"
    "fixture/DeleteCustomVerificationEmailTemplateResponse.proto"
    ses
    (Proxy :: Proxy DeleteCustomVerificationEmailTemplate)

responseDeleteIdentityPolicy :: DeleteIdentityPolicyResponse -> TestTree
responseDeleteIdentityPolicy = res
    "DeleteIdentityPolicyResponse"
    "fixture/DeleteIdentityPolicyResponse.proto"
    ses
    (Proxy :: Proxy DeleteIdentityPolicy)

responseUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplateResponse -> TestTree
responseUpdateCustomVerificationEmailTemplate = res
    "UpdateCustomVerificationEmailTemplateResponse"
    "fixture/UpdateCustomVerificationEmailTemplateResponse.proto"
    ses
    (Proxy :: Proxy UpdateCustomVerificationEmailTemplate)

responseSendCustomVerificationEmail :: SendCustomVerificationEmailResponse -> TestTree
responseSendCustomVerificationEmail = res
    "SendCustomVerificationEmailResponse"
    "fixture/SendCustomVerificationEmailResponse.proto"
    ses
    (Proxy :: Proxy SendCustomVerificationEmail)

responseGetIdentityNotificationAttributes :: GetIdentityNotificationAttributesResponse -> TestTree
responseGetIdentityNotificationAttributes = res
    "GetIdentityNotificationAttributesResponse"
    "fixture/GetIdentityNotificationAttributesResponse.proto"
    ses
    (Proxy :: Proxy GetIdentityNotificationAttributes)

responseUpdateConfigurationSetReputationMetricsEnabled :: UpdateConfigurationSetReputationMetricsEnabledResponse -> TestTree
responseUpdateConfigurationSetReputationMetricsEnabled = res
    "UpdateConfigurationSetReputationMetricsEnabledResponse"
    "fixture/UpdateConfigurationSetReputationMetricsEnabledResponse.proto"
    ses
    (Proxy :: Proxy UpdateConfigurationSetReputationMetricsEnabled)

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

responseUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestinationResponse -> TestTree
responseUpdateConfigurationSetEventDestination = res
    "UpdateConfigurationSetEventDestinationResponse"
    "fixture/UpdateConfigurationSetEventDestinationResponse.proto"
    ses
    (Proxy :: Proxy UpdateConfigurationSetEventDestination)

responseDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestinationResponse -> TestTree
responseDeleteConfigurationSetEventDestination = res
    "DeleteConfigurationSetEventDestinationResponse"
    "fixture/DeleteConfigurationSetEventDestinationResponse.proto"
    ses
    (Proxy :: Proxy DeleteConfigurationSetEventDestination)

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

responseListConfigurationSets :: ListConfigurationSetsResponse -> TestTree
responseListConfigurationSets = res
    "ListConfigurationSetsResponse"
    "fixture/ListConfigurationSetsResponse.proto"
    ses
    (Proxy :: Proxy ListConfigurationSets)

responseDeleteConfigurationSet :: DeleteConfigurationSetResponse -> TestTree
responseDeleteConfigurationSet = res
    "DeleteConfigurationSetResponse"
    "fixture/DeleteConfigurationSetResponse.proto"
    ses
    (Proxy :: Proxy DeleteConfigurationSet)

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

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates = res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    ses
    (Proxy :: Proxy ListTemplates)

responseVerifyDomainIdentity :: VerifyDomainIdentityResponse -> TestTree
responseVerifyDomainIdentity = res
    "VerifyDomainIdentityResponse"
    "fixture/VerifyDomainIdentityResponse.proto"
    ses
    (Proxy :: Proxy VerifyDomainIdentity)

responseUpdateTemplate :: UpdateTemplateResponse -> TestTree
responseUpdateTemplate = res
    "UpdateTemplateResponse"
    "fixture/UpdateTemplateResponse.proto"
    ses
    (Proxy :: Proxy UpdateTemplate)

responseDeleteTemplate :: DeleteTemplateResponse -> TestTree
responseDeleteTemplate = res
    "DeleteTemplateResponse"
    "fixture/DeleteTemplateResponse.proto"
    ses
    (Proxy :: Proxy DeleteTemplate)

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

responseSendTemplatedEmail :: SendTemplatedEmailResponse -> TestTree
responseSendTemplatedEmail = res
    "SendTemplatedEmailResponse"
    "fixture/SendTemplatedEmailResponse.proto"
    ses
    (Proxy :: Proxy SendTemplatedEmail)

responseVerifyDomainDkim :: VerifyDomainDkimResponse -> TestTree
responseVerifyDomainDkim = res
    "VerifyDomainDkimResponse"
    "fixture/VerifyDomainDkimResponse.proto"
    ses
    (Proxy :: Proxy VerifyDomainDkim)

responseTestRenderTemplate :: TestRenderTemplateResponse -> TestTree
responseTestRenderTemplate = res
    "TestRenderTemplateResponse"
    "fixture/TestRenderTemplateResponse.proto"
    ses
    (Proxy :: Proxy TestRenderTemplate)

responseSendBulkTemplatedEmail :: SendBulkTemplatedEmailResponse -> TestTree
responseSendBulkTemplatedEmail = res
    "SendBulkTemplatedEmailResponse"
    "fixture/SendBulkTemplatedEmailResponse.proto"
    ses
    (Proxy :: Proxy SendBulkTemplatedEmail)

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

responseListCustomVerificationEmailTemplates :: ListCustomVerificationEmailTemplatesResponse -> TestTree
responseListCustomVerificationEmailTemplates = res
    "ListCustomVerificationEmailTemplatesResponse"
    "fixture/ListCustomVerificationEmailTemplatesResponse.proto"
    ses
    (Proxy :: Proxy ListCustomVerificationEmailTemplates)

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

responseUpdateConfigurationSetSendingEnabled :: UpdateConfigurationSetSendingEnabledResponse -> TestTree
responseUpdateConfigurationSetSendingEnabled = res
    "UpdateConfigurationSetSendingEnabledResponse"
    "fixture/UpdateConfigurationSetSendingEnabledResponse.proto"
    ses
    (Proxy :: Proxy UpdateConfigurationSetSendingEnabled)

responseCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplateResponse -> TestTree
responseCreateCustomVerificationEmailTemplate = res
    "CreateCustomVerificationEmailTemplateResponse"
    "fixture/CreateCustomVerificationEmailTemplateResponse.proto"
    ses
    (Proxy :: Proxy CreateCustomVerificationEmailTemplate)

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

responseGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplateResponse -> TestTree
responseGetCustomVerificationEmailTemplate = res
    "GetCustomVerificationEmailTemplateResponse"
    "fixture/GetCustomVerificationEmailTemplateResponse.proto"
    ses
    (Proxy :: Proxy GetCustomVerificationEmailTemplate)

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

responseCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestinationResponse -> TestTree
responseCreateConfigurationSetEventDestination = res
    "CreateConfigurationSetEventDestinationResponse"
    "fixture/CreateConfigurationSetEventDestinationResponse.proto"
    ses
    (Proxy :: Proxy CreateConfigurationSetEventDestination)

responseGetAccountSendingEnabled :: GetAccountSendingEnabledResponse -> TestTree
responseGetAccountSendingEnabled = res
    "GetAccountSendingEnabledResponse"
    "fixture/GetAccountSendingEnabledResponse.proto"
    ses
    (Proxy :: Proxy GetAccountSendingEnabled)

responseCreateReceiptRule :: CreateReceiptRuleResponse -> TestTree
responseCreateReceiptRule = res
    "CreateReceiptRuleResponse"
    "fixture/CreateReceiptRuleResponse.proto"
    ses
    (Proxy :: Proxy CreateReceiptRule)

responseGetTemplate :: GetTemplateResponse -> TestTree
responseGetTemplate = res
    "GetTemplateResponse"
    "fixture/GetTemplateResponse.proto"
    ses
    (Proxy :: Proxy GetTemplate)

responseSetActiveReceiptRuleSet :: SetActiveReceiptRuleSetResponse -> TestTree
responseSetActiveReceiptRuleSet = res
    "SetActiveReceiptRuleSetResponse"
    "fixture/SetActiveReceiptRuleSetResponse.proto"
    ses
    (Proxy :: Proxy SetActiveReceiptRuleSet)

responseCreateConfigurationSet :: CreateConfigurationSetResponse -> TestTree
responseCreateConfigurationSet = res
    "CreateConfigurationSetResponse"
    "fixture/CreateConfigurationSetResponse.proto"
    ses
    (Proxy :: Proxy CreateConfigurationSet)

responseUpdateAccountSendingEnabled :: UpdateAccountSendingEnabledResponse -> TestTree
responseUpdateAccountSendingEnabled = res
    "UpdateAccountSendingEnabledResponse"
    "fixture/UpdateAccountSendingEnabledResponse.proto"
    ses
    (Proxy :: Proxy UpdateAccountSendingEnabled)

responseCreateConfigurationSetTrackingOptions :: CreateConfigurationSetTrackingOptionsResponse -> TestTree
responseCreateConfigurationSetTrackingOptions = res
    "CreateConfigurationSetTrackingOptionsResponse"
    "fixture/CreateConfigurationSetTrackingOptionsResponse.proto"
    ses
    (Proxy :: Proxy CreateConfigurationSetTrackingOptions)

responseDescribeActiveReceiptRuleSet :: DescribeActiveReceiptRuleSetResponse -> TestTree
responseDescribeActiveReceiptRuleSet = res
    "DescribeActiveReceiptRuleSetResponse"
    "fixture/DescribeActiveReceiptRuleSetResponse.proto"
    ses
    (Proxy :: Proxy DescribeActiveReceiptRuleSet)
