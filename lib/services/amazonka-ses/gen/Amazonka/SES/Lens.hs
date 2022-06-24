{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SES.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Lens
  ( -- * Operations

    -- ** CloneReceiptRuleSet
    cloneReceiptRuleSet_ruleSetName,
    cloneReceiptRuleSet_originalRuleSetName,
    cloneReceiptRuleSetResponse_httpStatus,

    -- ** CreateConfigurationSet
    createConfigurationSet_configurationSet,
    createConfigurationSetResponse_httpStatus,

    -- ** CreateConfigurationSetEventDestination
    createConfigurationSetEventDestination_configurationSetName,
    createConfigurationSetEventDestination_eventDestination,
    createConfigurationSetEventDestinationResponse_httpStatus,

    -- ** CreateConfigurationSetTrackingOptions
    createConfigurationSetTrackingOptions_configurationSetName,
    createConfigurationSetTrackingOptions_trackingOptions,
    createConfigurationSetTrackingOptionsResponse_httpStatus,

    -- ** CreateCustomVerificationEmailTemplate
    createCustomVerificationEmailTemplate_templateName,
    createCustomVerificationEmailTemplate_fromEmailAddress,
    createCustomVerificationEmailTemplate_templateSubject,
    createCustomVerificationEmailTemplate_templateContent,
    createCustomVerificationEmailTemplate_successRedirectionURL,
    createCustomVerificationEmailTemplate_failureRedirectionURL,

    -- ** CreateReceiptFilter
    createReceiptFilter_filter,
    createReceiptFilterResponse_httpStatus,

    -- ** CreateReceiptRule
    createReceiptRule_after,
    createReceiptRule_ruleSetName,
    createReceiptRule_rule,
    createReceiptRuleResponse_httpStatus,

    -- ** CreateReceiptRuleSet
    createReceiptRuleSet_ruleSetName,
    createReceiptRuleSetResponse_httpStatus,

    -- ** CreateTemplate
    createTemplate_template,
    createTemplateResponse_httpStatus,

    -- ** DeleteConfigurationSet
    deleteConfigurationSet_configurationSetName,
    deleteConfigurationSetResponse_httpStatus,

    -- ** DeleteConfigurationSetEventDestination
    deleteConfigurationSetEventDestination_configurationSetName,
    deleteConfigurationSetEventDestination_eventDestinationName,
    deleteConfigurationSetEventDestinationResponse_httpStatus,

    -- ** DeleteConfigurationSetTrackingOptions
    deleteConfigurationSetTrackingOptions_configurationSetName,
    deleteConfigurationSetTrackingOptionsResponse_httpStatus,

    -- ** DeleteCustomVerificationEmailTemplate
    deleteCustomVerificationEmailTemplate_templateName,

    -- ** DeleteIdentity
    deleteIdentity_identity,
    deleteIdentityResponse_httpStatus,

    -- ** DeleteIdentityPolicy
    deleteIdentityPolicy_identity,
    deleteIdentityPolicy_policyName,
    deleteIdentityPolicyResponse_httpStatus,

    -- ** DeleteReceiptFilter
    deleteReceiptFilter_filterName,
    deleteReceiptFilterResponse_httpStatus,

    -- ** DeleteReceiptRule
    deleteReceiptRule_ruleSetName,
    deleteReceiptRule_ruleName,
    deleteReceiptRuleResponse_httpStatus,

    -- ** DeleteReceiptRuleSet
    deleteReceiptRuleSet_ruleSetName,
    deleteReceiptRuleSetResponse_httpStatus,

    -- ** DeleteTemplate
    deleteTemplate_templateName,
    deleteTemplateResponse_httpStatus,

    -- ** DeleteVerifiedEmailAddress
    deleteVerifiedEmailAddress_emailAddress,

    -- ** DescribeActiveReceiptRuleSet
    describeActiveReceiptRuleSetResponse_metadata,
    describeActiveReceiptRuleSetResponse_rules,
    describeActiveReceiptRuleSetResponse_httpStatus,

    -- ** DescribeConfigurationSet
    describeConfigurationSet_configurationSetAttributeNames,
    describeConfigurationSet_configurationSetName,
    describeConfigurationSetResponse_reputationOptions,
    describeConfigurationSetResponse_deliveryOptions,
    describeConfigurationSetResponse_trackingOptions,
    describeConfigurationSetResponse_configurationSet,
    describeConfigurationSetResponse_eventDestinations,
    describeConfigurationSetResponse_httpStatus,

    -- ** DescribeReceiptRule
    describeReceiptRule_ruleSetName,
    describeReceiptRule_ruleName,
    describeReceiptRuleResponse_rule,
    describeReceiptRuleResponse_httpStatus,

    -- ** DescribeReceiptRuleSet
    describeReceiptRuleSet_ruleSetName,
    describeReceiptRuleSetResponse_metadata,
    describeReceiptRuleSetResponse_rules,
    describeReceiptRuleSetResponse_httpStatus,

    -- ** GetAccountSendingEnabled
    getAccountSendingEnabledResponse_enabled,
    getAccountSendingEnabledResponse_httpStatus,

    -- ** GetCustomVerificationEmailTemplate
    getCustomVerificationEmailTemplate_templateName,
    getCustomVerificationEmailTemplateResponse_templateName,
    getCustomVerificationEmailTemplateResponse_successRedirectionURL,
    getCustomVerificationEmailTemplateResponse_fromEmailAddress,
    getCustomVerificationEmailTemplateResponse_templateContent,
    getCustomVerificationEmailTemplateResponse_templateSubject,
    getCustomVerificationEmailTemplateResponse_failureRedirectionURL,
    getCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** GetIdentityDkimAttributes
    getIdentityDkimAttributes_identities,
    getIdentityDkimAttributesResponse_httpStatus,
    getIdentityDkimAttributesResponse_dkimAttributes,

    -- ** GetIdentityMailFromDomainAttributes
    getIdentityMailFromDomainAttributes_identities,
    getIdentityMailFromDomainAttributesResponse_httpStatus,
    getIdentityMailFromDomainAttributesResponse_mailFromDomainAttributes,

    -- ** GetIdentityNotificationAttributes
    getIdentityNotificationAttributes_identities,
    getIdentityNotificationAttributesResponse_httpStatus,
    getIdentityNotificationAttributesResponse_notificationAttributes,

    -- ** GetIdentityPolicies
    getIdentityPolicies_identity,
    getIdentityPolicies_policyNames,
    getIdentityPoliciesResponse_httpStatus,
    getIdentityPoliciesResponse_policies,

    -- ** GetIdentityVerificationAttributes
    getIdentityVerificationAttributes_identities,
    getIdentityVerificationAttributesResponse_httpStatus,
    getIdentityVerificationAttributesResponse_verificationAttributes,

    -- ** GetSendQuota
    getSendQuotaResponse_max24HourSend,
    getSendQuotaResponse_sentLast24Hours,
    getSendQuotaResponse_maxSendRate,
    getSendQuotaResponse_httpStatus,

    -- ** GetSendStatistics
    getSendStatisticsResponse_sendDataPoints,
    getSendStatisticsResponse_httpStatus,

    -- ** GetTemplate
    getTemplate_templateName,
    getTemplateResponse_template,
    getTemplateResponse_httpStatus,

    -- ** ListConfigurationSets
    listConfigurationSets_nextToken,
    listConfigurationSets_maxItems,
    listConfigurationSetsResponse_nextToken,
    listConfigurationSetsResponse_configurationSets,
    listConfigurationSetsResponse_httpStatus,

    -- ** ListCustomVerificationEmailTemplates
    listCustomVerificationEmailTemplates_nextToken,
    listCustomVerificationEmailTemplates_maxResults,
    listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates,
    listCustomVerificationEmailTemplatesResponse_nextToken,
    listCustomVerificationEmailTemplatesResponse_httpStatus,

    -- ** ListIdentities
    listIdentities_nextToken,
    listIdentities_maxItems,
    listIdentities_identityType,
    listIdentitiesResponse_nextToken,
    listIdentitiesResponse_httpStatus,
    listIdentitiesResponse_identities,

    -- ** ListIdentityPolicies
    listIdentityPolicies_identity,
    listIdentityPoliciesResponse_httpStatus,
    listIdentityPoliciesResponse_policyNames,

    -- ** ListReceiptFilters
    listReceiptFiltersResponse_filters,
    listReceiptFiltersResponse_httpStatus,

    -- ** ListReceiptRuleSets
    listReceiptRuleSets_nextToken,
    listReceiptRuleSetsResponse_nextToken,
    listReceiptRuleSetsResponse_ruleSets,
    listReceiptRuleSetsResponse_httpStatus,

    -- ** ListTemplates
    listTemplates_nextToken,
    listTemplates_maxItems,
    listTemplatesResponse_nextToken,
    listTemplatesResponse_templatesMetadata,
    listTemplatesResponse_httpStatus,

    -- ** ListVerifiedEmailAddresses
    listVerifiedEmailAddressesResponse_verifiedEmailAddresses,
    listVerifiedEmailAddressesResponse_httpStatus,

    -- ** PutConfigurationSetDeliveryOptions
    putConfigurationSetDeliveryOptions_deliveryOptions,
    putConfigurationSetDeliveryOptions_configurationSetName,
    putConfigurationSetDeliveryOptionsResponse_httpStatus,

    -- ** PutIdentityPolicy
    putIdentityPolicy_identity,
    putIdentityPolicy_policyName,
    putIdentityPolicy_policy,
    putIdentityPolicyResponse_httpStatus,

    -- ** ReorderReceiptRuleSet
    reorderReceiptRuleSet_ruleSetName,
    reorderReceiptRuleSet_ruleNames,
    reorderReceiptRuleSetResponse_httpStatus,

    -- ** SendBounce
    sendBounce_bounceSenderArn,
    sendBounce_explanation,
    sendBounce_messageDsn,
    sendBounce_originalMessageId,
    sendBounce_bounceSender,
    sendBounce_bouncedRecipientInfoList,
    sendBounceResponse_messageId,
    sendBounceResponse_httpStatus,

    -- ** SendBulkTemplatedEmail
    sendBulkTemplatedEmail_returnPath,
    sendBulkTemplatedEmail_replyToAddresses,
    sendBulkTemplatedEmail_sourceArn,
    sendBulkTemplatedEmail_defaultTags,
    sendBulkTemplatedEmail_configurationSetName,
    sendBulkTemplatedEmail_returnPathArn,
    sendBulkTemplatedEmail_defaultTemplateData,
    sendBulkTemplatedEmail_templateArn,
    sendBulkTemplatedEmail_source,
    sendBulkTemplatedEmail_template,
    sendBulkTemplatedEmail_destinations,
    sendBulkTemplatedEmailResponse_httpStatus,
    sendBulkTemplatedEmailResponse_status,

    -- ** SendCustomVerificationEmail
    sendCustomVerificationEmail_configurationSetName,
    sendCustomVerificationEmail_emailAddress,
    sendCustomVerificationEmail_templateName,
    sendCustomVerificationEmailResponse_messageId,
    sendCustomVerificationEmailResponse_httpStatus,

    -- ** SendEmail
    sendEmail_tags,
    sendEmail_returnPath,
    sendEmail_replyToAddresses,
    sendEmail_sourceArn,
    sendEmail_configurationSetName,
    sendEmail_returnPathArn,
    sendEmail_source,
    sendEmail_destination,
    sendEmail_message,
    sendEmailResponse_httpStatus,
    sendEmailResponse_messageId,

    -- ** SendRawEmail
    sendRawEmail_tags,
    sendRawEmail_fromArn,
    sendRawEmail_sourceArn,
    sendRawEmail_configurationSetName,
    sendRawEmail_returnPathArn,
    sendRawEmail_source,
    sendRawEmail_destinations,
    sendRawEmail_rawMessage,
    sendRawEmailResponse_httpStatus,
    sendRawEmailResponse_messageId,

    -- ** SendTemplatedEmail
    sendTemplatedEmail_tags,
    sendTemplatedEmail_returnPath,
    sendTemplatedEmail_replyToAddresses,
    sendTemplatedEmail_sourceArn,
    sendTemplatedEmail_configurationSetName,
    sendTemplatedEmail_returnPathArn,
    sendTemplatedEmail_templateArn,
    sendTemplatedEmail_source,
    sendTemplatedEmail_destination,
    sendTemplatedEmail_template,
    sendTemplatedEmail_templateData,
    sendTemplatedEmailResponse_httpStatus,
    sendTemplatedEmailResponse_messageId,

    -- ** SetActiveReceiptRuleSet
    setActiveReceiptRuleSet_ruleSetName,
    setActiveReceiptRuleSetResponse_httpStatus,

    -- ** SetIdentityDkimEnabled
    setIdentityDkimEnabled_identity,
    setIdentityDkimEnabled_dkimEnabled,
    setIdentityDkimEnabledResponse_httpStatus,

    -- ** SetIdentityFeedbackForwardingEnabled
    setIdentityFeedbackForwardingEnabled_identity,
    setIdentityFeedbackForwardingEnabled_forwardingEnabled,
    setIdentityFeedbackForwardingEnabledResponse_httpStatus,

    -- ** SetIdentityHeadersInNotificationsEnabled
    setIdentityHeadersInNotificationsEnabled_identity,
    setIdentityHeadersInNotificationsEnabled_notificationType,
    setIdentityHeadersInNotificationsEnabled_enabled,
    setIdentityHeadersInNotificationsEnabledResponse_httpStatus,

    -- ** SetIdentityMailFromDomain
    setIdentityMailFromDomain_mailFromDomain,
    setIdentityMailFromDomain_behaviorOnMXFailure,
    setIdentityMailFromDomain_identity,
    setIdentityMailFromDomainResponse_httpStatus,

    -- ** SetIdentityNotificationTopic
    setIdentityNotificationTopic_snsTopic,
    setIdentityNotificationTopic_identity,
    setIdentityNotificationTopic_notificationType,
    setIdentityNotificationTopicResponse_httpStatus,

    -- ** SetReceiptRulePosition
    setReceiptRulePosition_after,
    setReceiptRulePosition_ruleSetName,
    setReceiptRulePosition_ruleName,
    setReceiptRulePositionResponse_httpStatus,

    -- ** TestRenderTemplate
    testRenderTemplate_templateName,
    testRenderTemplate_templateData,
    testRenderTemplateResponse_renderedTemplate,
    testRenderTemplateResponse_httpStatus,

    -- ** UpdateAccountSendingEnabled
    updateAccountSendingEnabled_enabled,

    -- ** UpdateConfigurationSetEventDestination
    updateConfigurationSetEventDestination_configurationSetName,
    updateConfigurationSetEventDestination_eventDestination,
    updateConfigurationSetEventDestinationResponse_httpStatus,

    -- ** UpdateConfigurationSetReputationMetricsEnabled
    updateConfigurationSetReputationMetricsEnabled_configurationSetName,
    updateConfigurationSetReputationMetricsEnabled_enabled,

    -- ** UpdateConfigurationSetSendingEnabled
    updateConfigurationSetSendingEnabled_configurationSetName,
    updateConfigurationSetSendingEnabled_enabled,

    -- ** UpdateConfigurationSetTrackingOptions
    updateConfigurationSetTrackingOptions_configurationSetName,
    updateConfigurationSetTrackingOptions_trackingOptions,
    updateConfigurationSetTrackingOptionsResponse_httpStatus,

    -- ** UpdateCustomVerificationEmailTemplate
    updateCustomVerificationEmailTemplate_successRedirectionURL,
    updateCustomVerificationEmailTemplate_fromEmailAddress,
    updateCustomVerificationEmailTemplate_templateContent,
    updateCustomVerificationEmailTemplate_templateSubject,
    updateCustomVerificationEmailTemplate_failureRedirectionURL,
    updateCustomVerificationEmailTemplate_templateName,

    -- ** UpdateReceiptRule
    updateReceiptRule_ruleSetName,
    updateReceiptRule_rule,
    updateReceiptRuleResponse_httpStatus,

    -- ** UpdateTemplate
    updateTemplate_template,
    updateTemplateResponse_httpStatus,

    -- ** VerifyDomainDkim
    verifyDomainDkim_domain,
    verifyDomainDkimResponse_httpStatus,
    verifyDomainDkimResponse_dkimTokens,

    -- ** VerifyDomainIdentity
    verifyDomainIdentity_domain,
    verifyDomainIdentityResponse_httpStatus,
    verifyDomainIdentityResponse_verificationToken,

    -- ** VerifyEmailAddress
    verifyEmailAddress_emailAddress,

    -- ** VerifyEmailIdentity
    verifyEmailIdentity_emailAddress,
    verifyEmailIdentityResponse_httpStatus,

    -- * Types

    -- ** AddHeaderAction
    addHeaderAction_headerName,
    addHeaderAction_headerValue,

    -- ** Body
    body_html,
    body_text,

    -- ** BounceAction
    bounceAction_topicArn,
    bounceAction_statusCode,
    bounceAction_smtpReplyCode,
    bounceAction_message,
    bounceAction_sender,

    -- ** BouncedRecipientInfo
    bouncedRecipientInfo_recipientArn,
    bouncedRecipientInfo_bounceType,
    bouncedRecipientInfo_recipientDsnFields,
    bouncedRecipientInfo_recipient,

    -- ** BulkEmailDestination
    bulkEmailDestination_replacementTags,
    bulkEmailDestination_replacementTemplateData,
    bulkEmailDestination_destination,

    -- ** BulkEmailDestinationStatus
    bulkEmailDestinationStatus_messageId,
    bulkEmailDestinationStatus_status,
    bulkEmailDestinationStatus_error,

    -- ** CloudWatchDestination
    cloudWatchDestination_dimensionConfigurations,

    -- ** CloudWatchDimensionConfiguration
    cloudWatchDimensionConfiguration_dimensionName,
    cloudWatchDimensionConfiguration_dimensionValueSource,
    cloudWatchDimensionConfiguration_defaultDimensionValue,

    -- ** ConfigurationSet
    configurationSet_name,

    -- ** Content
    content_charset,
    content_data,

    -- ** CustomVerificationEmailTemplate
    customVerificationEmailTemplate_templateName,
    customVerificationEmailTemplate_successRedirectionURL,
    customVerificationEmailTemplate_fromEmailAddress,
    customVerificationEmailTemplate_templateSubject,
    customVerificationEmailTemplate_failureRedirectionURL,

    -- ** DeliveryOptions
    deliveryOptions_tlsPolicy,

    -- ** Destination
    destination_ccAddresses,
    destination_bccAddresses,
    destination_toAddresses,

    -- ** EventDestination
    eventDestination_sNSDestination,
    eventDestination_enabled,
    eventDestination_cloudWatchDestination,
    eventDestination_kinesisFirehoseDestination,
    eventDestination_name,
    eventDestination_matchingEventTypes,

    -- ** ExtensionField
    extensionField_name,
    extensionField_value,

    -- ** IdentityDkimAttributes
    identityDkimAttributes_dkimTokens,
    identityDkimAttributes_dkimEnabled,
    identityDkimAttributes_dkimVerificationStatus,

    -- ** IdentityMailFromDomainAttributes
    identityMailFromDomainAttributes_mailFromDomain,
    identityMailFromDomainAttributes_mailFromDomainStatus,
    identityMailFromDomainAttributes_behaviorOnMXFailure,

    -- ** IdentityNotificationAttributes
    identityNotificationAttributes_headersInDeliveryNotificationsEnabled,
    identityNotificationAttributes_headersInComplaintNotificationsEnabled,
    identityNotificationAttributes_headersInBounceNotificationsEnabled,
    identityNotificationAttributes_bounceTopic,
    identityNotificationAttributes_complaintTopic,
    identityNotificationAttributes_deliveryTopic,
    identityNotificationAttributes_forwardingEnabled,

    -- ** IdentityVerificationAttributes
    identityVerificationAttributes_verificationToken,
    identityVerificationAttributes_verificationStatus,

    -- ** KinesisFirehoseDestination
    kinesisFirehoseDestination_iAMRoleARN,
    kinesisFirehoseDestination_deliveryStreamARN,

    -- ** LambdaAction
    lambdaAction_topicArn,
    lambdaAction_invocationType,
    lambdaAction_functionArn,

    -- ** Message
    message_subject,
    message_body,

    -- ** MessageDsn
    messageDsn_extensionFields,
    messageDsn_arrivalDate,
    messageDsn_reportingMta,

    -- ** MessageTag
    messageTag_name,
    messageTag_value,

    -- ** RawMessage
    rawMessage_data,

    -- ** ReceiptAction
    receiptAction_addHeaderAction,
    receiptAction_lambdaAction,
    receiptAction_s3Action,
    receiptAction_stopAction,
    receiptAction_sNSAction,
    receiptAction_bounceAction,
    receiptAction_workmailAction,

    -- ** ReceiptFilter
    receiptFilter_name,
    receiptFilter_ipFilter,

    -- ** ReceiptIpFilter
    receiptIpFilter_policy,
    receiptIpFilter_cidr,

    -- ** ReceiptRule
    receiptRule_scanEnabled,
    receiptRule_recipients,
    receiptRule_enabled,
    receiptRule_tlsPolicy,
    receiptRule_actions,
    receiptRule_name,

    -- ** ReceiptRuleSetMetadata
    receiptRuleSetMetadata_name,
    receiptRuleSetMetadata_createdTimestamp,

    -- ** RecipientDsnFields
    recipientDsnFields_extensionFields,
    recipientDsnFields_finalRecipient,
    recipientDsnFields_remoteMta,
    recipientDsnFields_diagnosticCode,
    recipientDsnFields_lastAttemptDate,
    recipientDsnFields_action,
    recipientDsnFields_status,

    -- ** ReputationOptions
    reputationOptions_reputationMetricsEnabled,
    reputationOptions_sendingEnabled,
    reputationOptions_lastFreshStart,

    -- ** S3Action
    s3Action_objectKeyPrefix,
    s3Action_topicArn,
    s3Action_kmsKeyArn,
    s3Action_bucketName,

    -- ** SNSAction
    sNSAction_encoding,
    sNSAction_topicArn,

    -- ** SNSDestination
    sNSDestination_topicARN,

    -- ** SendDataPoint
    sendDataPoint_deliveryAttempts,
    sendDataPoint_rejects,
    sendDataPoint_timestamp,
    sendDataPoint_bounces,
    sendDataPoint_complaints,

    -- ** StopAction
    stopAction_topicArn,
    stopAction_scope,

    -- ** Template
    template_subjectPart,
    template_textPart,
    template_htmlPart,
    template_templateName,

    -- ** TemplateMetadata
    templateMetadata_name,
    templateMetadata_createdTimestamp,

    -- ** TrackingOptions
    trackingOptions_customRedirectDomain,

    -- ** WorkmailAction
    workmailAction_topicArn,
    workmailAction_organizationArn,
  )
where

import Amazonka.SES.CloneReceiptRuleSet
import Amazonka.SES.CreateConfigurationSet
import Amazonka.SES.CreateConfigurationSetEventDestination
import Amazonka.SES.CreateConfigurationSetTrackingOptions
import Amazonka.SES.CreateCustomVerificationEmailTemplate
import Amazonka.SES.CreateReceiptFilter
import Amazonka.SES.CreateReceiptRule
import Amazonka.SES.CreateReceiptRuleSet
import Amazonka.SES.CreateTemplate
import Amazonka.SES.DeleteConfigurationSet
import Amazonka.SES.DeleteConfigurationSetEventDestination
import Amazonka.SES.DeleteConfigurationSetTrackingOptions
import Amazonka.SES.DeleteCustomVerificationEmailTemplate
import Amazonka.SES.DeleteIdentity
import Amazonka.SES.DeleteIdentityPolicy
import Amazonka.SES.DeleteReceiptFilter
import Amazonka.SES.DeleteReceiptRule
import Amazonka.SES.DeleteReceiptRuleSet
import Amazonka.SES.DeleteTemplate
import Amazonka.SES.DeleteVerifiedEmailAddress
import Amazonka.SES.DescribeActiveReceiptRuleSet
import Amazonka.SES.DescribeConfigurationSet
import Amazonka.SES.DescribeReceiptRule
import Amazonka.SES.DescribeReceiptRuleSet
import Amazonka.SES.GetAccountSendingEnabled
import Amazonka.SES.GetCustomVerificationEmailTemplate
import Amazonka.SES.GetIdentityDkimAttributes
import Amazonka.SES.GetIdentityMailFromDomainAttributes
import Amazonka.SES.GetIdentityNotificationAttributes
import Amazonka.SES.GetIdentityPolicies
import Amazonka.SES.GetIdentityVerificationAttributes
import Amazonka.SES.GetSendQuota
import Amazonka.SES.GetSendStatistics
import Amazonka.SES.GetTemplate
import Amazonka.SES.ListConfigurationSets
import Amazonka.SES.ListCustomVerificationEmailTemplates
import Amazonka.SES.ListIdentities
import Amazonka.SES.ListIdentityPolicies
import Amazonka.SES.ListReceiptFilters
import Amazonka.SES.ListReceiptRuleSets
import Amazonka.SES.ListTemplates
import Amazonka.SES.ListVerifiedEmailAddresses
import Amazonka.SES.PutConfigurationSetDeliveryOptions
import Amazonka.SES.PutIdentityPolicy
import Amazonka.SES.ReorderReceiptRuleSet
import Amazonka.SES.SendBounce
import Amazonka.SES.SendBulkTemplatedEmail
import Amazonka.SES.SendCustomVerificationEmail
import Amazonka.SES.SendEmail
import Amazonka.SES.SendRawEmail
import Amazonka.SES.SendTemplatedEmail
import Amazonka.SES.SetActiveReceiptRuleSet
import Amazonka.SES.SetIdentityDkimEnabled
import Amazonka.SES.SetIdentityFeedbackForwardingEnabled
import Amazonka.SES.SetIdentityHeadersInNotificationsEnabled
import Amazonka.SES.SetIdentityMailFromDomain
import Amazonka.SES.SetIdentityNotificationTopic
import Amazonka.SES.SetReceiptRulePosition
import Amazonka.SES.TestRenderTemplate
import Amazonka.SES.Types.AddHeaderAction
import Amazonka.SES.Types.Body
import Amazonka.SES.Types.BounceAction
import Amazonka.SES.Types.BouncedRecipientInfo
import Amazonka.SES.Types.BulkEmailDestination
import Amazonka.SES.Types.BulkEmailDestinationStatus
import Amazonka.SES.Types.CloudWatchDestination
import Amazonka.SES.Types.CloudWatchDimensionConfiguration
import Amazonka.SES.Types.ConfigurationSet
import Amazonka.SES.Types.Content
import Amazonka.SES.Types.CustomVerificationEmailTemplate
import Amazonka.SES.Types.DeliveryOptions
import Amazonka.SES.Types.Destination
import Amazonka.SES.Types.EventDestination
import Amazonka.SES.Types.ExtensionField
import Amazonka.SES.Types.IdentityDkimAttributes
import Amazonka.SES.Types.IdentityMailFromDomainAttributes
import Amazonka.SES.Types.IdentityNotificationAttributes
import Amazonka.SES.Types.IdentityVerificationAttributes
import Amazonka.SES.Types.KinesisFirehoseDestination
import Amazonka.SES.Types.LambdaAction
import Amazonka.SES.Types.Message
import Amazonka.SES.Types.MessageDsn
import Amazonka.SES.Types.MessageTag
import Amazonka.SES.Types.RawMessage
import Amazonka.SES.Types.ReceiptAction
import Amazonka.SES.Types.ReceiptFilter
import Amazonka.SES.Types.ReceiptIpFilter
import Amazonka.SES.Types.ReceiptRule
import Amazonka.SES.Types.ReceiptRuleSetMetadata
import Amazonka.SES.Types.RecipientDsnFields
import Amazonka.SES.Types.ReputationOptions
import Amazonka.SES.Types.S3Action
import Amazonka.SES.Types.SNSAction
import Amazonka.SES.Types.SNSDestination
import Amazonka.SES.Types.SendDataPoint
import Amazonka.SES.Types.StopAction
import Amazonka.SES.Types.Template
import Amazonka.SES.Types.TemplateMetadata
import Amazonka.SES.Types.TrackingOptions
import Amazonka.SES.Types.WorkmailAction
import Amazonka.SES.UpdateAccountSendingEnabled
import Amazonka.SES.UpdateConfigurationSetEventDestination
import Amazonka.SES.UpdateConfigurationSetReputationMetricsEnabled
import Amazonka.SES.UpdateConfigurationSetSendingEnabled
import Amazonka.SES.UpdateConfigurationSetTrackingOptions
import Amazonka.SES.UpdateCustomVerificationEmailTemplate
import Amazonka.SES.UpdateReceiptRule
import Amazonka.SES.UpdateTemplate
import Amazonka.SES.VerifyDomainDkim
import Amazonka.SES.VerifyDomainIdentity
import Amazonka.SES.VerifyEmailAddress
import Amazonka.SES.VerifyEmailIdentity
