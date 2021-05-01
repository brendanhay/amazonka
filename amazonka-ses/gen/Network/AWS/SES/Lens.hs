{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Lens
  ( -- * Operations

    -- ** GetSendStatistics
    getSendStatisticsResponse_sendDataPoints,
    getSendStatisticsResponse_httpStatus,

    -- ** DescribeConfigurationSet
    describeConfigurationSet_configurationSetAttributeNames,
    describeConfigurationSet_configurationSetName,
    describeConfigurationSetResponse_trackingOptions,
    describeConfigurationSetResponse_deliveryOptions,
    describeConfigurationSetResponse_reputationOptions,
    describeConfigurationSetResponse_eventDestinations,
    describeConfigurationSetResponse_configurationSet,
    describeConfigurationSetResponse_httpStatus,

    -- ** PutConfigurationSetDeliveryOptions
    putConfigurationSetDeliveryOptions_deliveryOptions,
    putConfigurationSetDeliveryOptions_configurationSetName,
    putConfigurationSetDeliveryOptionsResponse_httpStatus,

    -- ** DeleteIdentityPolicy
    deleteIdentityPolicy_identity,
    deleteIdentityPolicy_policyName,
    deleteIdentityPolicyResponse_httpStatus,

    -- ** DescribeReceiptRule
    describeReceiptRule_ruleSetName,
    describeReceiptRule_ruleName,
    describeReceiptRuleResponse_rule,
    describeReceiptRuleResponse_httpStatus,

    -- ** CreateTemplate
    createTemplate_template,
    createTemplateResponse_httpStatus,

    -- ** GetIdentityDkimAttributes
    getIdentityDkimAttributes_identities,
    getIdentityDkimAttributesResponse_httpStatus,
    getIdentityDkimAttributesResponse_dkimAttributes,

    -- ** CreateReceiptRuleSet
    createReceiptRuleSet_ruleSetName,
    createReceiptRuleSetResponse_httpStatus,

    -- ** GetSendQuota
    getSendQuotaResponse_max24HourSend,
    getSendQuotaResponse_sentLast24Hours,
    getSendQuotaResponse_maxSendRate,
    getSendQuotaResponse_httpStatus,

    -- ** SetIdentityHeadersInNotificationsEnabled
    setIdentityHeadersInNotificationsEnabled_identity,
    setIdentityHeadersInNotificationsEnabled_notificationType,
    setIdentityHeadersInNotificationsEnabled_enabled,
    setIdentityHeadersInNotificationsEnabledResponse_httpStatus,

    -- ** VerifyDomainIdentity
    verifyDomainIdentity_domain,
    verifyDomainIdentityResponse_httpStatus,
    verifyDomainIdentityResponse_verificationToken,

    -- ** UpdateTemplate
    updateTemplate_template,
    updateTemplateResponse_httpStatus,

    -- ** DeleteTemplate
    deleteTemplate_templateName,
    deleteTemplateResponse_httpStatus,

    -- ** CreateConfigurationSetTrackingOptions
    createConfigurationSetTrackingOptions_configurationSetName,
    createConfigurationSetTrackingOptions_trackingOptions,
    createConfigurationSetTrackingOptionsResponse_httpStatus,

    -- ** DeleteReceiptRuleSet
    deleteReceiptRuleSet_ruleSetName,
    deleteReceiptRuleSetResponse_httpStatus,

    -- ** SetReceiptRulePosition
    setReceiptRulePosition_after,
    setReceiptRulePosition_ruleSetName,
    setReceiptRulePosition_ruleName,
    setReceiptRulePositionResponse_httpStatus,

    -- ** UpdateAccountSendingEnabled
    updateAccountSendingEnabled_enabled,

    -- ** GetIdentityVerificationAttributes
    getIdentityVerificationAttributes_identities,
    getIdentityVerificationAttributesResponse_httpStatus,
    getIdentityVerificationAttributesResponse_verificationAttributes,

    -- ** GetIdentityPolicies
    getIdentityPolicies_identity,
    getIdentityPolicies_policyNames,
    getIdentityPoliciesResponse_httpStatus,
    getIdentityPoliciesResponse_policies,

    -- ** CreateConfigurationSetEventDestination
    createConfigurationSetEventDestination_configurationSetName,
    createConfigurationSetEventDestination_eventDestination,
    createConfigurationSetEventDestinationResponse_httpStatus,

    -- ** GetAccountSendingEnabled
    getAccountSendingEnabledResponse_enabled,
    getAccountSendingEnabledResponse_httpStatus,

    -- ** CreateConfigurationSet
    createConfigurationSet_configurationSet,
    createConfigurationSetResponse_httpStatus,

    -- ** DeleteConfigurationSet
    deleteConfigurationSet_configurationSetName,
    deleteConfigurationSetResponse_httpStatus,

    -- ** DeleteReceiptRule
    deleteReceiptRule_ruleSetName,
    deleteReceiptRule_ruleName,
    deleteReceiptRuleResponse_httpStatus,

    -- ** SetIdentityFeedbackForwardingEnabled
    setIdentityFeedbackForwardingEnabled_identity,
    setIdentityFeedbackForwardingEnabled_forwardingEnabled,
    setIdentityFeedbackForwardingEnabledResponse_httpStatus,

    -- ** CloneReceiptRuleSet
    cloneReceiptRuleSet_ruleSetName,
    cloneReceiptRuleSet_originalRuleSetName,
    cloneReceiptRuleSetResponse_httpStatus,

    -- ** UpdateConfigurationSetEventDestination
    updateConfigurationSetEventDestination_configurationSetName,
    updateConfigurationSetEventDestination_eventDestination,
    updateConfigurationSetEventDestinationResponse_httpStatus,

    -- ** UpdateReceiptRule
    updateReceiptRule_ruleSetName,
    updateReceiptRule_rule,
    updateReceiptRuleResponse_httpStatus,

    -- ** DeleteConfigurationSetEventDestination
    deleteConfigurationSetEventDestination_configurationSetName,
    deleteConfigurationSetEventDestination_eventDestinationName,
    deleteConfigurationSetEventDestinationResponse_httpStatus,

    -- ** SendEmail
    sendEmail_returnPath,
    sendEmail_returnPathArn,
    sendEmail_replyToAddresses,
    sendEmail_tags,
    sendEmail_sourceArn,
    sendEmail_configurationSetName,
    sendEmail_source,
    sendEmail_destination,
    sendEmail_message,
    sendEmailResponse_httpStatus,
    sendEmailResponse_messageId,

    -- ** DeleteVerifiedEmailAddress
    deleteVerifiedEmailAddress_emailAddress,

    -- ** VerifyEmailAddress
    verifyEmailAddress_emailAddress,

    -- ** CreateCustomVerificationEmailTemplate
    createCustomVerificationEmailTemplate_templateName,
    createCustomVerificationEmailTemplate_fromEmailAddress,
    createCustomVerificationEmailTemplate_templateSubject,
    createCustomVerificationEmailTemplate_templateContent,
    createCustomVerificationEmailTemplate_successRedirectionURL,
    createCustomVerificationEmailTemplate_failureRedirectionURL,

    -- ** ListIdentityPolicies
    listIdentityPolicies_identity,
    listIdentityPoliciesResponse_httpStatus,
    listIdentityPoliciesResponse_policyNames,

    -- ** SetIdentityDkimEnabled
    setIdentityDkimEnabled_identity,
    setIdentityDkimEnabled_dkimEnabled,
    setIdentityDkimEnabledResponse_httpStatus,

    -- ** UpdateConfigurationSetReputationMetricsEnabled
    updateConfigurationSetReputationMetricsEnabled_configurationSetName,
    updateConfigurationSetReputationMetricsEnabled_enabled,

    -- ** ListCustomVerificationEmailTemplates
    listCustomVerificationEmailTemplates_nextToken,
    listCustomVerificationEmailTemplates_maxResults,
    listCustomVerificationEmailTemplatesResponse_nextToken,
    listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates,
    listCustomVerificationEmailTemplatesResponse_httpStatus,

    -- ** DeleteIdentity
    deleteIdentity_identity,
    deleteIdentityResponse_httpStatus,

    -- ** DeleteCustomVerificationEmailTemplate
    deleteCustomVerificationEmailTemplate_templateName,

    -- ** PutIdentityPolicy
    putIdentityPolicy_identity,
    putIdentityPolicy_policyName,
    putIdentityPolicy_policy,
    putIdentityPolicyResponse_httpStatus,

    -- ** UpdateCustomVerificationEmailTemplate
    updateCustomVerificationEmailTemplate_templateSubject,
    updateCustomVerificationEmailTemplate_fromEmailAddress,
    updateCustomVerificationEmailTemplate_templateContent,
    updateCustomVerificationEmailTemplate_successRedirectionURL,
    updateCustomVerificationEmailTemplate_failureRedirectionURL,
    updateCustomVerificationEmailTemplate_templateName,

    -- ** DeleteConfigurationSetTrackingOptions
    deleteConfigurationSetTrackingOptions_configurationSetName,
    deleteConfigurationSetTrackingOptionsResponse_httpStatus,

    -- ** SendBulkTemplatedEmail
    sendBulkTemplatedEmail_returnPath,
    sendBulkTemplatedEmail_defaultTemplateData,
    sendBulkTemplatedEmail_returnPathArn,
    sendBulkTemplatedEmail_defaultTags,
    sendBulkTemplatedEmail_replyToAddresses,
    sendBulkTemplatedEmail_sourceArn,
    sendBulkTemplatedEmail_templateArn,
    sendBulkTemplatedEmail_configurationSetName,
    sendBulkTemplatedEmail_source,
    sendBulkTemplatedEmail_template,
    sendBulkTemplatedEmail_destinations,
    sendBulkTemplatedEmailResponse_httpStatus,
    sendBulkTemplatedEmailResponse_status,

    -- ** VerifyDomainDkim
    verifyDomainDkim_domain,
    verifyDomainDkimResponse_httpStatus,
    verifyDomainDkimResponse_dkimTokens,

    -- ** SendRawEmail
    sendRawEmail_fromArn,
    sendRawEmail_source,
    sendRawEmail_returnPathArn,
    sendRawEmail_destinations,
    sendRawEmail_tags,
    sendRawEmail_sourceArn,
    sendRawEmail_configurationSetName,
    sendRawEmail_rawMessage,
    sendRawEmailResponse_httpStatus,
    sendRawEmailResponse_messageId,

    -- ** TestRenderTemplate
    testRenderTemplate_templateName,
    testRenderTemplate_templateData,
    testRenderTemplateResponse_renderedTemplate,
    testRenderTemplateResponse_httpStatus,

    -- ** SendBounce
    sendBounce_bounceSenderArn,
    sendBounce_messageDsn,
    sendBounce_explanation,
    sendBounce_originalMessageId,
    sendBounce_bounceSender,
    sendBounce_bouncedRecipientInfoList,
    sendBounceResponse_messageId,
    sendBounceResponse_httpStatus,

    -- ** UpdateConfigurationSetTrackingOptions
    updateConfigurationSetTrackingOptions_configurationSetName,
    updateConfigurationSetTrackingOptions_trackingOptions,
    updateConfigurationSetTrackingOptionsResponse_httpStatus,

    -- ** SendTemplatedEmail
    sendTemplatedEmail_returnPath,
    sendTemplatedEmail_returnPathArn,
    sendTemplatedEmail_replyToAddresses,
    sendTemplatedEmail_tags,
    sendTemplatedEmail_sourceArn,
    sendTemplatedEmail_templateArn,
    sendTemplatedEmail_configurationSetName,
    sendTemplatedEmail_source,
    sendTemplatedEmail_destination,
    sendTemplatedEmail_template,
    sendTemplatedEmail_templateData,
    sendTemplatedEmailResponse_httpStatus,
    sendTemplatedEmailResponse_messageId,

    -- ** ListReceiptRuleSets
    listReceiptRuleSets_nextToken,
    listReceiptRuleSetsResponse_ruleSets,
    listReceiptRuleSetsResponse_nextToken,
    listReceiptRuleSetsResponse_httpStatus,

    -- ** ReorderReceiptRuleSet
    reorderReceiptRuleSet_ruleSetName,
    reorderReceiptRuleSet_ruleNames,
    reorderReceiptRuleSetResponse_httpStatus,

    -- ** ListTemplates
    listTemplates_nextToken,
    listTemplates_maxItems,
    listTemplatesResponse_nextToken,
    listTemplatesResponse_templatesMetadata,
    listTemplatesResponse_httpStatus,

    -- ** DescribeActiveReceiptRuleSet
    describeActiveReceiptRuleSetResponse_rules,
    describeActiveReceiptRuleSetResponse_metadata,
    describeActiveReceiptRuleSetResponse_httpStatus,

    -- ** CreateReceiptRule
    createReceiptRule_after,
    createReceiptRule_ruleSetName,
    createReceiptRule_rule,
    createReceiptRuleResponse_httpStatus,

    -- ** GetTemplate
    getTemplate_templateName,
    getTemplateResponse_template,
    getTemplateResponse_httpStatus,

    -- ** SetActiveReceiptRuleSet
    setActiveReceiptRuleSet_ruleSetName,
    setActiveReceiptRuleSetResponse_httpStatus,

    -- ** ListConfigurationSets
    listConfigurationSets_nextToken,
    listConfigurationSets_maxItems,
    listConfigurationSetsResponse_nextToken,
    listConfigurationSetsResponse_configurationSets,
    listConfigurationSetsResponse_httpStatus,

    -- ** SetIdentityMailFromDomain
    setIdentityMailFromDomain_mailFromDomain,
    setIdentityMailFromDomain_behaviorOnMXFailure,
    setIdentityMailFromDomain_identity,
    setIdentityMailFromDomainResponse_httpStatus,

    -- ** GetIdentityMailFromDomainAttributes
    getIdentityMailFromDomainAttributes_identities,
    getIdentityMailFromDomainAttributesResponse_httpStatus,
    getIdentityMailFromDomainAttributesResponse_mailFromDomainAttributes,

    -- ** SetIdentityNotificationTopic
    setIdentityNotificationTopic_snsTopic,
    setIdentityNotificationTopic_identity,
    setIdentityNotificationTopic_notificationType,
    setIdentityNotificationTopicResponse_httpStatus,

    -- ** GetCustomVerificationEmailTemplate
    getCustomVerificationEmailTemplate_templateName,
    getCustomVerificationEmailTemplateResponse_templateName,
    getCustomVerificationEmailTemplateResponse_templateSubject,
    getCustomVerificationEmailTemplateResponse_fromEmailAddress,
    getCustomVerificationEmailTemplateResponse_templateContent,
    getCustomVerificationEmailTemplateResponse_successRedirectionURL,
    getCustomVerificationEmailTemplateResponse_failureRedirectionURL,
    getCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** CreateReceiptFilter
    createReceiptFilter_filter,
    createReceiptFilterResponse_httpStatus,

    -- ** ListVerifiedEmailAddresses
    listVerifiedEmailAddressesResponse_verifiedEmailAddresses,
    listVerifiedEmailAddressesResponse_httpStatus,

    -- ** ListReceiptFilters
    listReceiptFiltersResponse_filters,
    listReceiptFiltersResponse_httpStatus,

    -- ** DeleteReceiptFilter
    deleteReceiptFilter_filterName,
    deleteReceiptFilterResponse_httpStatus,

    -- ** DescribeReceiptRuleSet
    describeReceiptRuleSet_ruleSetName,
    describeReceiptRuleSetResponse_rules,
    describeReceiptRuleSetResponse_metadata,
    describeReceiptRuleSetResponse_httpStatus,

    -- ** VerifyEmailIdentity
    verifyEmailIdentity_emailAddress,
    verifyEmailIdentityResponse_httpStatus,

    -- ** SendCustomVerificationEmail
    sendCustomVerificationEmail_configurationSetName,
    sendCustomVerificationEmail_emailAddress,
    sendCustomVerificationEmail_templateName,
    sendCustomVerificationEmailResponse_messageId,
    sendCustomVerificationEmailResponse_httpStatus,

    -- ** ListIdentities
    listIdentities_nextToken,
    listIdentities_identityType,
    listIdentities_maxItems,
    listIdentitiesResponse_nextToken,
    listIdentitiesResponse_httpStatus,
    listIdentitiesResponse_identities,

    -- ** GetIdentityNotificationAttributes
    getIdentityNotificationAttributes_identities,
    getIdentityNotificationAttributesResponse_httpStatus,
    getIdentityNotificationAttributesResponse_notificationAttributes,

    -- ** UpdateConfigurationSetSendingEnabled
    updateConfigurationSetSendingEnabled_configurationSetName,
    updateConfigurationSetSendingEnabled_enabled,

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
    bouncedRecipientInfo_recipientDsnFields,
    bouncedRecipientInfo_bounceType,
    bouncedRecipientInfo_recipient,

    -- ** BulkEmailDestination
    bulkEmailDestination_replacementTags,
    bulkEmailDestination_replacementTemplateData,
    bulkEmailDestination_destination,

    -- ** BulkEmailDestinationStatus
    bulkEmailDestinationStatus_status,
    bulkEmailDestinationStatus_messageId,
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
    customVerificationEmailTemplate_templateSubject,
    customVerificationEmailTemplate_fromEmailAddress,
    customVerificationEmailTemplate_successRedirectionURL,
    customVerificationEmailTemplate_failureRedirectionURL,

    -- ** DeliveryOptions
    deliveryOptions_tlsPolicy,

    -- ** Destination
    destination_toAddresses,
    destination_ccAddresses,
    destination_bccAddresses,

    -- ** EventDestination
    eventDestination_cloudWatchDestination,
    eventDestination_enabled,
    eventDestination_kinesisFirehoseDestination,
    eventDestination_sNSDestination,
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
    identityNotificationAttributes_headersInComplaintNotificationsEnabled,
    identityNotificationAttributes_headersInDeliveryNotificationsEnabled,
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
    lambdaAction_invocationType,
    lambdaAction_topicArn,
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
    receiptAction_lambdaAction,
    receiptAction_stopAction,
    receiptAction_s3Action,
    receiptAction_bounceAction,
    receiptAction_workmailAction,
    receiptAction_addHeaderAction,
    receiptAction_sNSAction,

    -- ** ReceiptFilter
    receiptFilter_name,
    receiptFilter_ipFilter,

    -- ** ReceiptIpFilter
    receiptIpFilter_policy,
    receiptIpFilter_cidr,

    -- ** ReceiptRule
    receiptRule_tlsPolicy,
    receiptRule_enabled,
    receiptRule_actions,
    receiptRule_recipients,
    receiptRule_scanEnabled,
    receiptRule_name,

    -- ** ReceiptRuleSetMetadata
    receiptRuleSetMetadata_createdTimestamp,
    receiptRuleSetMetadata_name,

    -- ** RecipientDsnFields
    recipientDsnFields_remoteMta,
    recipientDsnFields_lastAttemptDate,
    recipientDsnFields_extensionFields,
    recipientDsnFields_diagnosticCode,
    recipientDsnFields_finalRecipient,
    recipientDsnFields_action,
    recipientDsnFields_status,

    -- ** ReputationOptions
    reputationOptions_reputationMetricsEnabled,
    reputationOptions_lastFreshStart,
    reputationOptions_sendingEnabled,

    -- ** S3Action
    s3Action_objectKeyPrefix,
    s3Action_kmsKeyArn,
    s3Action_topicArn,
    s3Action_bucketName,

    -- ** SNSAction
    sNSAction_encoding,
    sNSAction_topicArn,

    -- ** SNSDestination
    sNSDestination_topicARN,

    -- ** SendDataPoint
    sendDataPoint_bounces,
    sendDataPoint_complaints,
    sendDataPoint_rejects,
    sendDataPoint_timestamp,
    sendDataPoint_deliveryAttempts,

    -- ** StopAction
    stopAction_topicArn,
    stopAction_scope,

    -- ** Template
    template_textPart,
    template_subjectPart,
    template_htmlPart,
    template_templateName,

    -- ** TemplateMetadata
    templateMetadata_createdTimestamp,
    templateMetadata_name,

    -- ** TrackingOptions
    trackingOptions_customRedirectDomain,

    -- ** WorkmailAction
    workmailAction_topicArn,
    workmailAction_organizationArn,
  )
where

import Network.AWS.SES.CloneReceiptRuleSet
import Network.AWS.SES.CreateConfigurationSet
import Network.AWS.SES.CreateConfigurationSetEventDestination
import Network.AWS.SES.CreateConfigurationSetTrackingOptions
import Network.AWS.SES.CreateCustomVerificationEmailTemplate
import Network.AWS.SES.CreateReceiptFilter
import Network.AWS.SES.CreateReceiptRule
import Network.AWS.SES.CreateReceiptRuleSet
import Network.AWS.SES.CreateTemplate
import Network.AWS.SES.DeleteConfigurationSet
import Network.AWS.SES.DeleteConfigurationSetEventDestination
import Network.AWS.SES.DeleteConfigurationSetTrackingOptions
import Network.AWS.SES.DeleteCustomVerificationEmailTemplate
import Network.AWS.SES.DeleteIdentity
import Network.AWS.SES.DeleteIdentityPolicy
import Network.AWS.SES.DeleteReceiptFilter
import Network.AWS.SES.DeleteReceiptRule
import Network.AWS.SES.DeleteReceiptRuleSet
import Network.AWS.SES.DeleteTemplate
import Network.AWS.SES.DeleteVerifiedEmailAddress
import Network.AWS.SES.DescribeActiveReceiptRuleSet
import Network.AWS.SES.DescribeConfigurationSet
import Network.AWS.SES.DescribeReceiptRule
import Network.AWS.SES.DescribeReceiptRuleSet
import Network.AWS.SES.GetAccountSendingEnabled
import Network.AWS.SES.GetCustomVerificationEmailTemplate
import Network.AWS.SES.GetIdentityDkimAttributes
import Network.AWS.SES.GetIdentityMailFromDomainAttributes
import Network.AWS.SES.GetIdentityNotificationAttributes
import Network.AWS.SES.GetIdentityPolicies
import Network.AWS.SES.GetIdentityVerificationAttributes
import Network.AWS.SES.GetSendQuota
import Network.AWS.SES.GetSendStatistics
import Network.AWS.SES.GetTemplate
import Network.AWS.SES.ListConfigurationSets
import Network.AWS.SES.ListCustomVerificationEmailTemplates
import Network.AWS.SES.ListIdentities
import Network.AWS.SES.ListIdentityPolicies
import Network.AWS.SES.ListReceiptFilters
import Network.AWS.SES.ListReceiptRuleSets
import Network.AWS.SES.ListTemplates
import Network.AWS.SES.ListVerifiedEmailAddresses
import Network.AWS.SES.PutConfigurationSetDeliveryOptions
import Network.AWS.SES.PutIdentityPolicy
import Network.AWS.SES.ReorderReceiptRuleSet
import Network.AWS.SES.SendBounce
import Network.AWS.SES.SendBulkTemplatedEmail
import Network.AWS.SES.SendCustomVerificationEmail
import Network.AWS.SES.SendEmail
import Network.AWS.SES.SendRawEmail
import Network.AWS.SES.SendTemplatedEmail
import Network.AWS.SES.SetActiveReceiptRuleSet
import Network.AWS.SES.SetIdentityDkimEnabled
import Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
import Network.AWS.SES.SetIdentityHeadersInNotificationsEnabled
import Network.AWS.SES.SetIdentityMailFromDomain
import Network.AWS.SES.SetIdentityNotificationTopic
import Network.AWS.SES.SetReceiptRulePosition
import Network.AWS.SES.TestRenderTemplate
import Network.AWS.SES.Types.AddHeaderAction
import Network.AWS.SES.Types.Body
import Network.AWS.SES.Types.BounceAction
import Network.AWS.SES.Types.BouncedRecipientInfo
import Network.AWS.SES.Types.BulkEmailDestination
import Network.AWS.SES.Types.BulkEmailDestinationStatus
import Network.AWS.SES.Types.CloudWatchDestination
import Network.AWS.SES.Types.CloudWatchDimensionConfiguration
import Network.AWS.SES.Types.ConfigurationSet
import Network.AWS.SES.Types.Content
import Network.AWS.SES.Types.CustomVerificationEmailTemplate
import Network.AWS.SES.Types.DeliveryOptions
import Network.AWS.SES.Types.Destination
import Network.AWS.SES.Types.EventDestination
import Network.AWS.SES.Types.ExtensionField
import Network.AWS.SES.Types.IdentityDkimAttributes
import Network.AWS.SES.Types.IdentityMailFromDomainAttributes
import Network.AWS.SES.Types.IdentityNotificationAttributes
import Network.AWS.SES.Types.IdentityVerificationAttributes
import Network.AWS.SES.Types.KinesisFirehoseDestination
import Network.AWS.SES.Types.LambdaAction
import Network.AWS.SES.Types.Message
import Network.AWS.SES.Types.MessageDsn
import Network.AWS.SES.Types.MessageTag
import Network.AWS.SES.Types.RawMessage
import Network.AWS.SES.Types.ReceiptAction
import Network.AWS.SES.Types.ReceiptFilter
import Network.AWS.SES.Types.ReceiptIpFilter
import Network.AWS.SES.Types.ReceiptRule
import Network.AWS.SES.Types.ReceiptRuleSetMetadata
import Network.AWS.SES.Types.RecipientDsnFields
import Network.AWS.SES.Types.ReputationOptions
import Network.AWS.SES.Types.S3Action
import Network.AWS.SES.Types.SNSAction
import Network.AWS.SES.Types.SNSDestination
import Network.AWS.SES.Types.SendDataPoint
import Network.AWS.SES.Types.StopAction
import Network.AWS.SES.Types.Template
import Network.AWS.SES.Types.TemplateMetadata
import Network.AWS.SES.Types.TrackingOptions
import Network.AWS.SES.Types.WorkmailAction
import Network.AWS.SES.UpdateAccountSendingEnabled
import Network.AWS.SES.UpdateConfigurationSetEventDestination
import Network.AWS.SES.UpdateConfigurationSetReputationMetricsEnabled
import Network.AWS.SES.UpdateConfigurationSetSendingEnabled
import Network.AWS.SES.UpdateConfigurationSetTrackingOptions
import Network.AWS.SES.UpdateCustomVerificationEmailTemplate
import Network.AWS.SES.UpdateReceiptRule
import Network.AWS.SES.UpdateTemplate
import Network.AWS.SES.VerifyDomainDkim
import Network.AWS.SES.VerifyDomainIdentity
import Network.AWS.SES.VerifyEmailAddress
import Network.AWS.SES.VerifyEmailIdentity
