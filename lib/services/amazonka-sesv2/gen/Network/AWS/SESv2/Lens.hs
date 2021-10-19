{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SESv2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Lens
  ( -- * Operations

    -- ** GetConfigurationSet
    getConfigurationSet_configurationSetName,
    getConfigurationSetResponse_sendingOptions,
    getConfigurationSetResponse_configurationSetName,
    getConfigurationSetResponse_deliveryOptions,
    getConfigurationSetResponse_trackingOptions,
    getConfigurationSetResponse_reputationOptions,
    getConfigurationSetResponse_tags,
    getConfigurationSetResponse_suppressionOptions,
    getConfigurationSetResponse_httpStatus,

    -- ** GetImportJob
    getImportJob_jobId,
    getImportJobResponse_processedRecordsCount,
    getImportJobResponse_jobId,
    getImportJobResponse_importDataSource,
    getImportJobResponse_completedTimestamp,
    getImportJobResponse_failureInfo,
    getImportJobResponse_importDestination,
    getImportJobResponse_jobStatus,
    getImportJobResponse_failedRecordsCount,
    getImportJobResponse_createdTimestamp,
    getImportJobResponse_httpStatus,

    -- ** PutConfigurationSetTrackingOptions
    putConfigurationSetTrackingOptions_customRedirectDomain,
    putConfigurationSetTrackingOptions_configurationSetName,
    putConfigurationSetTrackingOptionsResponse_httpStatus,

    -- ** PutEmailIdentityDkimSigningAttributes
    putEmailIdentityDkimSigningAttributes_signingAttributes,
    putEmailIdentityDkimSigningAttributes_emailIdentity,
    putEmailIdentityDkimSigningAttributes_signingAttributesOrigin,
    putEmailIdentityDkimSigningAttributesResponse_dkimStatus,
    putEmailIdentityDkimSigningAttributesResponse_dkimTokens,
    putEmailIdentityDkimSigningAttributesResponse_httpStatus,

    -- ** PutEmailIdentityDkimAttributes
    putEmailIdentityDkimAttributes_signingEnabled,
    putEmailIdentityDkimAttributes_emailIdentity,
    putEmailIdentityDkimAttributesResponse_httpStatus,

    -- ** PutConfigurationSetDeliveryOptions
    putConfigurationSetDeliveryOptions_sendingPoolName,
    putConfigurationSetDeliveryOptions_tlsPolicy,
    putConfigurationSetDeliveryOptions_configurationSetName,
    putConfigurationSetDeliveryOptionsResponse_httpStatus,

    -- ** ListDedicatedIpPools
    listDedicatedIpPools_nextToken,
    listDedicatedIpPools_pageSize,
    listDedicatedIpPoolsResponse_dedicatedIpPools,
    listDedicatedIpPoolsResponse_nextToken,
    listDedicatedIpPoolsResponse_httpStatus,

    -- ** GetDomainDeliverabilityCampaign
    getDomainDeliverabilityCampaign_campaignId,
    getDomainDeliverabilityCampaignResponse_httpStatus,
    getDomainDeliverabilityCampaignResponse_domainDeliverabilityCampaign,

    -- ** GetDedicatedIps
    getDedicatedIps_poolName,
    getDedicatedIps_nextToken,
    getDedicatedIps_pageSize,
    getDedicatedIpsResponse_nextToken,
    getDedicatedIpsResponse_dedicatedIps,
    getDedicatedIpsResponse_httpStatus,

    -- ** PutConfigurationSetSendingOptions
    putConfigurationSetSendingOptions_sendingEnabled,
    putConfigurationSetSendingOptions_configurationSetName,
    putConfigurationSetSendingOptionsResponse_httpStatus,

    -- ** DeleteCustomVerificationEmailTemplate
    deleteCustomVerificationEmailTemplate_templateName,
    deleteCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** UpdateCustomVerificationEmailTemplate
    updateCustomVerificationEmailTemplate_templateName,
    updateCustomVerificationEmailTemplate_fromEmailAddress,
    updateCustomVerificationEmailTemplate_templateSubject,
    updateCustomVerificationEmailTemplate_templateContent,
    updateCustomVerificationEmailTemplate_successRedirectionURL,
    updateCustomVerificationEmailTemplate_failureRedirectionURL,
    updateCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** CreateDedicatedIpPool
    createDedicatedIpPool_tags,
    createDedicatedIpPool_poolName,
    createDedicatedIpPoolResponse_httpStatus,

    -- ** SendCustomVerificationEmail
    sendCustomVerificationEmail_configurationSetName,
    sendCustomVerificationEmail_emailAddress,
    sendCustomVerificationEmail_templateName,
    sendCustomVerificationEmailResponse_messageId,
    sendCustomVerificationEmailResponse_httpStatus,

    -- ** GetSuppressedDestination
    getSuppressedDestination_emailAddress,
    getSuppressedDestinationResponse_httpStatus,
    getSuppressedDestinationResponse_suppressedDestination,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** GetEmailTemplate
    getEmailTemplate_templateName,
    getEmailTemplateResponse_httpStatus,
    getEmailTemplateResponse_templateName,
    getEmailTemplateResponse_templateContent,

    -- ** ListSuppressedDestinations
    listSuppressedDestinations_reasons,
    listSuppressedDestinations_endDate,
    listSuppressedDestinations_startDate,
    listSuppressedDestinations_nextToken,
    listSuppressedDestinations_pageSize,
    listSuppressedDestinationsResponse_nextToken,
    listSuppressedDestinationsResponse_suppressedDestinationSummaries,
    listSuppressedDestinationsResponse_httpStatus,

    -- ** PutEmailIdentityFeedbackAttributes
    putEmailIdentityFeedbackAttributes_emailForwardingEnabled,
    putEmailIdentityFeedbackAttributes_emailIdentity,
    putEmailIdentityFeedbackAttributesResponse_httpStatus,

    -- ** ListEmailTemplates
    listEmailTemplates_nextToken,
    listEmailTemplates_pageSize,
    listEmailTemplatesResponse_templatesMetadata,
    listEmailTemplatesResponse_nextToken,
    listEmailTemplatesResponse_httpStatus,

    -- ** PutConfigurationSetReputationOptions
    putConfigurationSetReputationOptions_reputationMetricsEnabled,
    putConfigurationSetReputationOptions_configurationSetName,
    putConfigurationSetReputationOptionsResponse_httpStatus,

    -- ** PutDedicatedIpInPool
    putDedicatedIpInPool_ip,
    putDedicatedIpInPool_destinationPoolName,
    putDedicatedIpInPoolResponse_httpStatus,

    -- ** CreateEmailTemplate
    createEmailTemplate_templateName,
    createEmailTemplate_templateContent,
    createEmailTemplateResponse_httpStatus,

    -- ** PutAccountSendingAttributes
    putAccountSendingAttributes_sendingEnabled,
    putAccountSendingAttributesResponse_httpStatus,

    -- ** UpdateConfigurationSetEventDestination
    updateConfigurationSetEventDestination_configurationSetName,
    updateConfigurationSetEventDestination_eventDestinationName,
    updateConfigurationSetEventDestination_eventDestination,
    updateConfigurationSetEventDestinationResponse_httpStatus,

    -- ** DeleteConfigurationSetEventDestination
    deleteConfigurationSetEventDestination_configurationSetName,
    deleteConfigurationSetEventDestination_eventDestinationName,
    deleteConfigurationSetEventDestinationResponse_httpStatus,

    -- ** ListConfigurationSets
    listConfigurationSets_nextToken,
    listConfigurationSets_pageSize,
    listConfigurationSetsResponse_configurationSets,
    listConfigurationSetsResponse_nextToken,
    listConfigurationSetsResponse_httpStatus,

    -- ** DeleteEmailIdentity
    deleteEmailIdentity_emailIdentity,
    deleteEmailIdentityResponse_httpStatus,

    -- ** DeleteContactList
    deleteContactList_contactListName,
    deleteContactListResponse_httpStatus,

    -- ** UpdateContactList
    updateContactList_topics,
    updateContactList_description,
    updateContactList_contactListName,
    updateContactListResponse_httpStatus,

    -- ** ListImportJobs
    listImportJobs_nextToken,
    listImportJobs_pageSize,
    listImportJobs_importDestinationType,
    listImportJobsResponse_importJobs,
    listImportJobsResponse_nextToken,
    listImportJobsResponse_httpStatus,

    -- ** DeleteConfigurationSet
    deleteConfigurationSet_configurationSetName,
    deleteConfigurationSetResponse_httpStatus,

    -- ** CreateEmailIdentity
    createEmailIdentity_configurationSetName,
    createEmailIdentity_dkimSigningAttributes,
    createEmailIdentity_tags,
    createEmailIdentity_emailIdentity,
    createEmailIdentityResponse_dkimAttributes,
    createEmailIdentityResponse_verifiedForSendingStatus,
    createEmailIdentityResponse_identityType,
    createEmailIdentityResponse_httpStatus,

    -- ** GetBlacklistReports
    getBlacklistReports_blacklistItemNames,
    getBlacklistReportsResponse_httpStatus,
    getBlacklistReportsResponse_blacklistReport,

    -- ** CreateContactList
    createContactList_topics,
    createContactList_description,
    createContactList_tags,
    createContactList_contactListName,
    createContactListResponse_httpStatus,

    -- ** ListEmailIdentities
    listEmailIdentities_nextToken,
    listEmailIdentities_pageSize,
    listEmailIdentitiesResponse_nextToken,
    listEmailIdentitiesResponse_emailIdentities,
    listEmailIdentitiesResponse_httpStatus,

    -- ** GetContact
    getContact_contactListName,
    getContact_emailAddress,
    getContactResponse_unsubscribeAll,
    getContactResponse_attributesData,
    getContactResponse_topicDefaultPreferences,
    getContactResponse_emailAddress,
    getContactResponse_contactListName,
    getContactResponse_createdTimestamp,
    getContactResponse_lastUpdatedTimestamp,
    getContactResponse_topicPreferences,
    getContactResponse_httpStatus,

    -- ** DeleteContact
    deleteContact_contactListName,
    deleteContact_emailAddress,
    deleteContactResponse_httpStatus,

    -- ** UpdateContact
    updateContact_unsubscribeAll,
    updateContact_attributesData,
    updateContact_topicPreferences,
    updateContact_contactListName,
    updateContact_emailAddress,
    updateContactResponse_httpStatus,

    -- ** GetContactList
    getContactList_contactListName,
    getContactListResponse_topics,
    getContactListResponse_contactListName,
    getContactListResponse_createdTimestamp,
    getContactListResponse_description,
    getContactListResponse_tags,
    getContactListResponse_lastUpdatedTimestamp,
    getContactListResponse_httpStatus,

    -- ** GetDedicatedIp
    getDedicatedIp_ip,
    getDedicatedIpResponse_dedicatedIp,
    getDedicatedIpResponse_httpStatus,

    -- ** CreateContact
    createContact_unsubscribeAll,
    createContact_attributesData,
    createContact_topicPreferences,
    createContact_contactListName,
    createContact_emailAddress,
    createContactResponse_httpStatus,

    -- ** GetEmailIdentity
    getEmailIdentity_emailIdentity,
    getEmailIdentityResponse_dkimAttributes,
    getEmailIdentityResponse_verifiedForSendingStatus,
    getEmailIdentityResponse_configurationSetName,
    getEmailIdentityResponse_identityType,
    getEmailIdentityResponse_mailFromAttributes,
    getEmailIdentityResponse_feedbackForwardingStatus,
    getEmailIdentityResponse_policies,
    getEmailIdentityResponse_tags,
    getEmailIdentityResponse_httpStatus,

    -- ** GetConfigurationSetEventDestinations
    getConfigurationSetEventDestinations_configurationSetName,
    getConfigurationSetEventDestinationsResponse_eventDestinations,
    getConfigurationSetEventDestinationsResponse_httpStatus,

    -- ** ListCustomVerificationEmailTemplates
    listCustomVerificationEmailTemplates_nextToken,
    listCustomVerificationEmailTemplates_pageSize,
    listCustomVerificationEmailTemplatesResponse_nextToken,
    listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates,
    listCustomVerificationEmailTemplatesResponse_httpStatus,

    -- ** GetAccount
    getAccountResponse_enforcementStatus,
    getAccountResponse_dedicatedIpAutoWarmupEnabled,
    getAccountResponse_sendQuota,
    getAccountResponse_productionAccessEnabled,
    getAccountResponse_details,
    getAccountResponse_suppressionAttributes,
    getAccountResponse_sendingEnabled,
    getAccountResponse_httpStatus,

    -- ** DeleteDedicatedIpPool
    deleteDedicatedIpPool_poolName,
    deleteDedicatedIpPoolResponse_httpStatus,

    -- ** GetEmailIdentityPolicies
    getEmailIdentityPolicies_emailIdentity,
    getEmailIdentityPoliciesResponse_policies,
    getEmailIdentityPoliciesResponse_httpStatus,

    -- ** PutConfigurationSetSuppressionOptions
    putConfigurationSetSuppressionOptions_suppressedReasons,
    putConfigurationSetSuppressionOptions_configurationSetName,
    putConfigurationSetSuppressionOptionsResponse_httpStatus,

    -- ** CreateCustomVerificationEmailTemplate
    createCustomVerificationEmailTemplate_templateName,
    createCustomVerificationEmailTemplate_fromEmailAddress,
    createCustomVerificationEmailTemplate_templateSubject,
    createCustomVerificationEmailTemplate_templateContent,
    createCustomVerificationEmailTemplate_successRedirectionURL,
    createCustomVerificationEmailTemplate_failureRedirectionURL,
    createCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** PutAccountDetails
    putAccountDetails_contactLanguage,
    putAccountDetails_productionAccessEnabled,
    putAccountDetails_additionalContactEmailAddresses,
    putAccountDetails_mailType,
    putAccountDetails_websiteURL,
    putAccountDetails_useCaseDescription,
    putAccountDetailsResponse_httpStatus,

    -- ** DeleteSuppressedDestination
    deleteSuppressedDestination_emailAddress,
    deleteSuppressedDestinationResponse_httpStatus,

    -- ** PutSuppressedDestination
    putSuppressedDestination_emailAddress,
    putSuppressedDestination_reason,
    putSuppressedDestinationResponse_httpStatus,

    -- ** GetDomainStatisticsReport
    getDomainStatisticsReport_domain,
    getDomainStatisticsReport_startDate,
    getDomainStatisticsReport_endDate,
    getDomainStatisticsReportResponse_httpStatus,
    getDomainStatisticsReportResponse_overallVolume,
    getDomainStatisticsReportResponse_dailyVolumes,

    -- ** DeleteEmailTemplate
    deleteEmailTemplate_templateName,
    deleteEmailTemplateResponse_httpStatus,

    -- ** UpdateEmailTemplate
    updateEmailTemplate_templateName,
    updateEmailTemplate_templateContent,
    updateEmailTemplateResponse_httpStatus,

    -- ** GetDeliverabilityDashboardOptions
    getDeliverabilityDashboardOptionsResponse_accountStatus,
    getDeliverabilityDashboardOptionsResponse_subscriptionExpiryDate,
    getDeliverabilityDashboardOptionsResponse_activeSubscribedDomains,
    getDeliverabilityDashboardOptionsResponse_pendingExpirationSubscribedDomains,
    getDeliverabilityDashboardOptionsResponse_httpStatus,
    getDeliverabilityDashboardOptionsResponse_dashboardEnabled,

    -- ** GetCustomVerificationEmailTemplate
    getCustomVerificationEmailTemplate_templateName,
    getCustomVerificationEmailTemplateResponse_fromEmailAddress,
    getCustomVerificationEmailTemplateResponse_templateName,
    getCustomVerificationEmailTemplateResponse_failureRedirectionURL,
    getCustomVerificationEmailTemplateResponse_templateSubject,
    getCustomVerificationEmailTemplateResponse_successRedirectionURL,
    getCustomVerificationEmailTemplateResponse_templateContent,
    getCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** ListDomainDeliverabilityCampaigns
    listDomainDeliverabilityCampaigns_nextToken,
    listDomainDeliverabilityCampaigns_pageSize,
    listDomainDeliverabilityCampaigns_startDate,
    listDomainDeliverabilityCampaigns_endDate,
    listDomainDeliverabilityCampaigns_subscribedDomain,
    listDomainDeliverabilityCampaignsResponse_nextToken,
    listDomainDeliverabilityCampaignsResponse_httpStatus,
    listDomainDeliverabilityCampaignsResponse_domainDeliverabilityCampaigns,

    -- ** SendBulkEmail
    sendBulkEmail_fromEmailAddress,
    sendBulkEmail_fromEmailAddressIdentityArn,
    sendBulkEmail_configurationSetName,
    sendBulkEmail_feedbackForwardingEmailAddress,
    sendBulkEmail_feedbackForwardingEmailAddressIdentityArn,
    sendBulkEmail_defaultEmailTags,
    sendBulkEmail_replyToAddresses,
    sendBulkEmail_defaultContent,
    sendBulkEmail_bulkEmailEntries,
    sendBulkEmailResponse_httpStatus,
    sendBulkEmailResponse_bulkEmailEntryResults,

    -- ** TestRenderEmailTemplate
    testRenderEmailTemplate_templateName,
    testRenderEmailTemplate_templateData,
    testRenderEmailTemplateResponse_httpStatus,
    testRenderEmailTemplateResponse_renderedTemplate,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** SendEmail
    sendEmail_fromEmailAddress,
    sendEmail_destination,
    sendEmail_fromEmailAddressIdentityArn,
    sendEmail_configurationSetName,
    sendEmail_listManagementOptions,
    sendEmail_emailTags,
    sendEmail_feedbackForwardingEmailAddress,
    sendEmail_feedbackForwardingEmailAddressIdentityArn,
    sendEmail_replyToAddresses,
    sendEmail_content,
    sendEmailResponse_messageId,
    sendEmailResponse_httpStatus,

    -- ** PutDedicatedIpWarmupAttributes
    putDedicatedIpWarmupAttributes_ip,
    putDedicatedIpWarmupAttributes_warmupPercentage,
    putDedicatedIpWarmupAttributesResponse_httpStatus,

    -- ** DeleteEmailIdentityPolicy
    deleteEmailIdentityPolicy_emailIdentity,
    deleteEmailIdentityPolicy_policyName,
    deleteEmailIdentityPolicyResponse_httpStatus,

    -- ** UpdateEmailIdentityPolicy
    updateEmailIdentityPolicy_emailIdentity,
    updateEmailIdentityPolicy_policyName,
    updateEmailIdentityPolicy_policy,
    updateEmailIdentityPolicyResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateDeliverabilityTestReport
    createDeliverabilityTestReport_reportName,
    createDeliverabilityTestReport_tags,
    createDeliverabilityTestReport_fromEmailAddress,
    createDeliverabilityTestReport_content,
    createDeliverabilityTestReportResponse_httpStatus,
    createDeliverabilityTestReportResponse_reportId,
    createDeliverabilityTestReportResponse_deliverabilityTestStatus,

    -- ** PutEmailIdentityMailFromAttributes
    putEmailIdentityMailFromAttributes_mailFromDomain,
    putEmailIdentityMailFromAttributes_behaviorOnMxFailure,
    putEmailIdentityMailFromAttributes_emailIdentity,
    putEmailIdentityMailFromAttributesResponse_httpStatus,

    -- ** ListContactLists
    listContactLists_nextToken,
    listContactLists_pageSize,
    listContactListsResponse_nextToken,
    listContactListsResponse_contactLists,
    listContactListsResponse_httpStatus,

    -- ** CreateEmailIdentityPolicy
    createEmailIdentityPolicy_emailIdentity,
    createEmailIdentityPolicy_policyName,
    createEmailIdentityPolicy_policy,
    createEmailIdentityPolicyResponse_httpStatus,

    -- ** CreateConfigurationSetEventDestination
    createConfigurationSetEventDestination_configurationSetName,
    createConfigurationSetEventDestination_eventDestinationName,
    createConfigurationSetEventDestination_eventDestination,
    createConfigurationSetEventDestinationResponse_httpStatus,

    -- ** PutEmailIdentityConfigurationSetAttributes
    putEmailIdentityConfigurationSetAttributes_configurationSetName,
    putEmailIdentityConfigurationSetAttributes_emailIdentity,
    putEmailIdentityConfigurationSetAttributesResponse_httpStatus,

    -- ** PutAccountSuppressionAttributes
    putAccountSuppressionAttributes_suppressedReasons,
    putAccountSuppressionAttributesResponse_httpStatus,

    -- ** CreateImportJob
    createImportJob_importDestination,
    createImportJob_importDataSource,
    createImportJobResponse_jobId,
    createImportJobResponse_httpStatus,

    -- ** ListDeliverabilityTestReports
    listDeliverabilityTestReports_nextToken,
    listDeliverabilityTestReports_pageSize,
    listDeliverabilityTestReportsResponse_nextToken,
    listDeliverabilityTestReportsResponse_httpStatus,
    listDeliverabilityTestReportsResponse_deliverabilityTestReports,

    -- ** CreateConfigurationSet
    createConfigurationSet_sendingOptions,
    createConfigurationSet_deliveryOptions,
    createConfigurationSet_trackingOptions,
    createConfigurationSet_reputationOptions,
    createConfigurationSet_tags,
    createConfigurationSet_suppressionOptions,
    createConfigurationSet_configurationSetName,
    createConfigurationSetResponse_httpStatus,

    -- ** GetDeliverabilityTestReport
    getDeliverabilityTestReport_reportId,
    getDeliverabilityTestReportResponse_message,
    getDeliverabilityTestReportResponse_tags,
    getDeliverabilityTestReportResponse_httpStatus,
    getDeliverabilityTestReportResponse_deliverabilityTestReport,
    getDeliverabilityTestReportResponse_overallPlacement,
    getDeliverabilityTestReportResponse_ispPlacements,

    -- ** PutDeliverabilityDashboardOption
    putDeliverabilityDashboardOption_subscribedDomains,
    putDeliverabilityDashboardOption_dashboardEnabled,
    putDeliverabilityDashboardOptionResponse_httpStatus,

    -- ** PutAccountDedicatedIpWarmupAttributes
    putAccountDedicatedIpWarmupAttributes_autoWarmupEnabled,
    putAccountDedicatedIpWarmupAttributesResponse_httpStatus,

    -- ** ListContacts
    listContacts_nextToken,
    listContacts_filter,
    listContacts_pageSize,
    listContacts_contactListName,
    listContactsResponse_nextToken,
    listContactsResponse_contacts,
    listContactsResponse_httpStatus,

    -- * Types

    -- ** AccountDetails
    accountDetails_reviewDetails,
    accountDetails_mailType,
    accountDetails_useCaseDescription,
    accountDetails_contactLanguage,
    accountDetails_additionalContactEmailAddresses,
    accountDetails_websiteURL,

    -- ** BlacklistEntry
    blacklistEntry_listingTime,
    blacklistEntry_rblName,
    blacklistEntry_description,

    -- ** Body
    body_text,
    body_html,

    -- ** BulkEmailContent
    bulkEmailContent_template,

    -- ** BulkEmailEntry
    bulkEmailEntry_replacementEmailContent,
    bulkEmailEntry_replacementTags,
    bulkEmailEntry_destination,

    -- ** BulkEmailEntryResult
    bulkEmailEntryResult_status,
    bulkEmailEntryResult_error,
    bulkEmailEntryResult_messageId,

    -- ** CloudWatchDestination
    cloudWatchDestination_dimensionConfigurations,

    -- ** CloudWatchDimensionConfiguration
    cloudWatchDimensionConfiguration_dimensionName,
    cloudWatchDimensionConfiguration_dimensionValueSource,
    cloudWatchDimensionConfiguration_defaultDimensionValue,

    -- ** Contact
    contact_unsubscribeAll,
    contact_topicDefaultPreferences,
    contact_emailAddress,
    contact_lastUpdatedTimestamp,
    contact_topicPreferences,

    -- ** ContactList
    contactList_contactListName,
    contactList_lastUpdatedTimestamp,

    -- ** ContactListDestination
    contactListDestination_contactListName,
    contactListDestination_contactListImportAction,

    -- ** Content
    content_charset,
    content_data,

    -- ** CustomVerificationEmailTemplateMetadata
    customVerificationEmailTemplateMetadata_fromEmailAddress,
    customVerificationEmailTemplateMetadata_templateName,
    customVerificationEmailTemplateMetadata_failureRedirectionURL,
    customVerificationEmailTemplateMetadata_templateSubject,
    customVerificationEmailTemplateMetadata_successRedirectionURL,

    -- ** DailyVolume
    dailyVolume_domainIspPlacements,
    dailyVolume_startDate,
    dailyVolume_volumeStatistics,

    -- ** DedicatedIp
    dedicatedIp_poolName,
    dedicatedIp_ip,
    dedicatedIp_warmupStatus,
    dedicatedIp_warmupPercentage,

    -- ** DeliverabilityTestReport
    deliverabilityTestReport_subject,
    deliverabilityTestReport_fromEmailAddress,
    deliverabilityTestReport_createDate,
    deliverabilityTestReport_reportId,
    deliverabilityTestReport_reportName,
    deliverabilityTestReport_deliverabilityTestStatus,

    -- ** DeliveryOptions
    deliveryOptions_sendingPoolName,
    deliveryOptions_tlsPolicy,

    -- ** Destination
    destination_bccAddresses,
    destination_ccAddresses,
    destination_toAddresses,

    -- ** DkimAttributes
    dkimAttributes_status,
    dkimAttributes_nextSigningKeyLength,
    dkimAttributes_tokens,
    dkimAttributes_signingEnabled,
    dkimAttributes_currentSigningKeyLength,
    dkimAttributes_lastKeyGenerationTimestamp,
    dkimAttributes_signingAttributesOrigin,

    -- ** DkimSigningAttributes
    dkimSigningAttributes_nextSigningKeyLength,
    dkimSigningAttributes_domainSigningPrivateKey,
    dkimSigningAttributes_domainSigningSelector,

    -- ** DomainDeliverabilityCampaign
    domainDeliverabilityCampaign_spamCount,
    domainDeliverabilityCampaign_subject,
    domainDeliverabilityCampaign_esps,
    domainDeliverabilityCampaign_fromAddress,
    domainDeliverabilityCampaign_deleteRate,
    domainDeliverabilityCampaign_campaignId,
    domainDeliverabilityCampaign_sendingIps,
    domainDeliverabilityCampaign_firstSeenDateTime,
    domainDeliverabilityCampaign_inboxCount,
    domainDeliverabilityCampaign_readDeleteRate,
    domainDeliverabilityCampaign_projectedVolume,
    domainDeliverabilityCampaign_imageUrl,
    domainDeliverabilityCampaign_readRate,
    domainDeliverabilityCampaign_lastSeenDateTime,

    -- ** DomainDeliverabilityTrackingOption
    domainDeliverabilityTrackingOption_domain,
    domainDeliverabilityTrackingOption_subscriptionStartDate,
    domainDeliverabilityTrackingOption_inboxPlacementTrackingOption,

    -- ** DomainIspPlacement
    domainIspPlacement_spamPercentage,
    domainIspPlacement_inboxRawCount,
    domainIspPlacement_ispName,
    domainIspPlacement_inboxPercentage,
    domainIspPlacement_spamRawCount,

    -- ** EmailContent
    emailContent_raw,
    emailContent_simple,
    emailContent_template,

    -- ** EmailTemplateContent
    emailTemplateContent_subject,
    emailTemplateContent_text,
    emailTemplateContent_html,

    -- ** EmailTemplateMetadata
    emailTemplateMetadata_templateName,
    emailTemplateMetadata_createdTimestamp,

    -- ** EventDestination
    eventDestination_pinpointDestination,
    eventDestination_enabled,
    eventDestination_kinesisFirehoseDestination,
    eventDestination_cloudWatchDestination,
    eventDestination_snsDestination,
    eventDestination_name,
    eventDestination_matchingEventTypes,

    -- ** EventDestinationDefinition
    eventDestinationDefinition_matchingEventTypes,
    eventDestinationDefinition_pinpointDestination,
    eventDestinationDefinition_enabled,
    eventDestinationDefinition_kinesisFirehoseDestination,
    eventDestinationDefinition_cloudWatchDestination,
    eventDestinationDefinition_snsDestination,

    -- ** FailureInfo
    failureInfo_failedRecordsS3Url,
    failureInfo_errorMessage,

    -- ** IdentityInfo
    identityInfo_identityType,
    identityInfo_identityName,
    identityInfo_sendingEnabled,

    -- ** ImportDataSource
    importDataSource_s3Url,
    importDataSource_dataFormat,

    -- ** ImportDestination
    importDestination_suppressionListDestination,
    importDestination_contactListDestination,

    -- ** ImportJobSummary
    importJobSummary_jobId,
    importJobSummary_importDestination,
    importJobSummary_jobStatus,
    importJobSummary_createdTimestamp,

    -- ** InboxPlacementTrackingOption
    inboxPlacementTrackingOption_trackedIsps,
    inboxPlacementTrackingOption_global,

    -- ** IspPlacement
    ispPlacement_placementStatistics,
    ispPlacement_ispName,

    -- ** KinesisFirehoseDestination
    kinesisFirehoseDestination_iamRoleArn,
    kinesisFirehoseDestination_deliveryStreamArn,

    -- ** ListContactsFilter
    listContactsFilter_filteredStatus,
    listContactsFilter_topicFilter,

    -- ** ListManagementOptions
    listManagementOptions_topicName,
    listManagementOptions_contactListName,

    -- ** MailFromAttributes
    mailFromAttributes_mailFromDomain,
    mailFromAttributes_mailFromDomainStatus,
    mailFromAttributes_behaviorOnMxFailure,

    -- ** Message
    message_subject,
    message_body,

    -- ** MessageTag
    messageTag_name,
    messageTag_value,

    -- ** OverallVolume
    overallVolume_domainIspPlacements,
    overallVolume_volumeStatistics,
    overallVolume_readRatePercent,

    -- ** PinpointDestination
    pinpointDestination_applicationArn,

    -- ** PlacementStatistics
    placementStatistics_missingPercentage,
    placementStatistics_spamPercentage,
    placementStatistics_spfPercentage,
    placementStatistics_dkimPercentage,
    placementStatistics_inboxPercentage,

    -- ** RawMessage
    rawMessage_data,

    -- ** ReplacementEmailContent
    replacementEmailContent_replacementTemplate,

    -- ** ReplacementTemplate
    replacementTemplate_replacementTemplateData,

    -- ** ReputationOptions
    reputationOptions_lastFreshStart,
    reputationOptions_reputationMetricsEnabled,

    -- ** ReviewDetails
    reviewDetails_status,
    reviewDetails_caseId,

    -- ** SendQuota
    sendQuota_maxSendRate,
    sendQuota_sentLast24Hours,
    sendQuota_max24HourSend,

    -- ** SendingOptions
    sendingOptions_sendingEnabled,

    -- ** SnsDestination
    snsDestination_topicArn,

    -- ** SuppressedDestination
    suppressedDestination_attributes,
    suppressedDestination_emailAddress,
    suppressedDestination_reason,
    suppressedDestination_lastUpdateTime,

    -- ** SuppressedDestinationAttributes
    suppressedDestinationAttributes_feedbackId,
    suppressedDestinationAttributes_messageId,

    -- ** SuppressedDestinationSummary
    suppressedDestinationSummary_emailAddress,
    suppressedDestinationSummary_reason,
    suppressedDestinationSummary_lastUpdateTime,

    -- ** SuppressionAttributes
    suppressionAttributes_suppressedReasons,

    -- ** SuppressionListDestination
    suppressionListDestination_suppressionListImportAction,

    -- ** SuppressionOptions
    suppressionOptions_suppressedReasons,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Template
    template_templateName,
    template_templateArn,
    template_templateData,

    -- ** Topic
    topic_description,
    topic_topicName,
    topic_displayName,
    topic_defaultSubscriptionStatus,

    -- ** TopicFilter
    topicFilter_topicName,
    topicFilter_useDefaultIfPreferenceUnavailable,

    -- ** TopicPreference
    topicPreference_topicName,
    topicPreference_subscriptionStatus,

    -- ** TrackingOptions
    trackingOptions_customRedirectDomain,

    -- ** VolumeStatistics
    volumeStatistics_inboxRawCount,
    volumeStatistics_projectedSpam,
    volumeStatistics_projectedInbox,
    volumeStatistics_spamRawCount,
  )
where

import Network.AWS.SESv2.CreateConfigurationSet
import Network.AWS.SESv2.CreateConfigurationSetEventDestination
import Network.AWS.SESv2.CreateContact
import Network.AWS.SESv2.CreateContactList
import Network.AWS.SESv2.CreateCustomVerificationEmailTemplate
import Network.AWS.SESv2.CreateDedicatedIpPool
import Network.AWS.SESv2.CreateDeliverabilityTestReport
import Network.AWS.SESv2.CreateEmailIdentity
import Network.AWS.SESv2.CreateEmailIdentityPolicy
import Network.AWS.SESv2.CreateEmailTemplate
import Network.AWS.SESv2.CreateImportJob
import Network.AWS.SESv2.DeleteConfigurationSet
import Network.AWS.SESv2.DeleteConfigurationSetEventDestination
import Network.AWS.SESv2.DeleteContact
import Network.AWS.SESv2.DeleteContactList
import Network.AWS.SESv2.DeleteCustomVerificationEmailTemplate
import Network.AWS.SESv2.DeleteDedicatedIpPool
import Network.AWS.SESv2.DeleteEmailIdentity
import Network.AWS.SESv2.DeleteEmailIdentityPolicy
import Network.AWS.SESv2.DeleteEmailTemplate
import Network.AWS.SESv2.DeleteSuppressedDestination
import Network.AWS.SESv2.GetAccount
import Network.AWS.SESv2.GetBlacklistReports
import Network.AWS.SESv2.GetConfigurationSet
import Network.AWS.SESv2.GetConfigurationSetEventDestinations
import Network.AWS.SESv2.GetContact
import Network.AWS.SESv2.GetContactList
import Network.AWS.SESv2.GetCustomVerificationEmailTemplate
import Network.AWS.SESv2.GetDedicatedIp
import Network.AWS.SESv2.GetDedicatedIps
import Network.AWS.SESv2.GetDeliverabilityDashboardOptions
import Network.AWS.SESv2.GetDeliverabilityTestReport
import Network.AWS.SESv2.GetDomainDeliverabilityCampaign
import Network.AWS.SESv2.GetDomainStatisticsReport
import Network.AWS.SESv2.GetEmailIdentity
import Network.AWS.SESv2.GetEmailIdentityPolicies
import Network.AWS.SESv2.GetEmailTemplate
import Network.AWS.SESv2.GetImportJob
import Network.AWS.SESv2.GetSuppressedDestination
import Network.AWS.SESv2.ListConfigurationSets
import Network.AWS.SESv2.ListContactLists
import Network.AWS.SESv2.ListContacts
import Network.AWS.SESv2.ListCustomVerificationEmailTemplates
import Network.AWS.SESv2.ListDedicatedIpPools
import Network.AWS.SESv2.ListDeliverabilityTestReports
import Network.AWS.SESv2.ListDomainDeliverabilityCampaigns
import Network.AWS.SESv2.ListEmailIdentities
import Network.AWS.SESv2.ListEmailTemplates
import Network.AWS.SESv2.ListImportJobs
import Network.AWS.SESv2.ListSuppressedDestinations
import Network.AWS.SESv2.ListTagsForResource
import Network.AWS.SESv2.PutAccountDedicatedIpWarmupAttributes
import Network.AWS.SESv2.PutAccountDetails
import Network.AWS.SESv2.PutAccountSendingAttributes
import Network.AWS.SESv2.PutAccountSuppressionAttributes
import Network.AWS.SESv2.PutConfigurationSetDeliveryOptions
import Network.AWS.SESv2.PutConfigurationSetReputationOptions
import Network.AWS.SESv2.PutConfigurationSetSendingOptions
import Network.AWS.SESv2.PutConfigurationSetSuppressionOptions
import Network.AWS.SESv2.PutConfigurationSetTrackingOptions
import Network.AWS.SESv2.PutDedicatedIpInPool
import Network.AWS.SESv2.PutDedicatedIpWarmupAttributes
import Network.AWS.SESv2.PutDeliverabilityDashboardOption
import Network.AWS.SESv2.PutEmailIdentityConfigurationSetAttributes
import Network.AWS.SESv2.PutEmailIdentityDkimAttributes
import Network.AWS.SESv2.PutEmailIdentityDkimSigningAttributes
import Network.AWS.SESv2.PutEmailIdentityFeedbackAttributes
import Network.AWS.SESv2.PutEmailIdentityMailFromAttributes
import Network.AWS.SESv2.PutSuppressedDestination
import Network.AWS.SESv2.SendBulkEmail
import Network.AWS.SESv2.SendCustomVerificationEmail
import Network.AWS.SESv2.SendEmail
import Network.AWS.SESv2.TagResource
import Network.AWS.SESv2.TestRenderEmailTemplate
import Network.AWS.SESv2.Types.AccountDetails
import Network.AWS.SESv2.Types.BlacklistEntry
import Network.AWS.SESv2.Types.Body
import Network.AWS.SESv2.Types.BulkEmailContent
import Network.AWS.SESv2.Types.BulkEmailEntry
import Network.AWS.SESv2.Types.BulkEmailEntryResult
import Network.AWS.SESv2.Types.CloudWatchDestination
import Network.AWS.SESv2.Types.CloudWatchDimensionConfiguration
import Network.AWS.SESv2.Types.Contact
import Network.AWS.SESv2.Types.ContactList
import Network.AWS.SESv2.Types.ContactListDestination
import Network.AWS.SESv2.Types.Content
import Network.AWS.SESv2.Types.CustomVerificationEmailTemplateMetadata
import Network.AWS.SESv2.Types.DailyVolume
import Network.AWS.SESv2.Types.DedicatedIp
import Network.AWS.SESv2.Types.DeliverabilityTestReport
import Network.AWS.SESv2.Types.DeliveryOptions
import Network.AWS.SESv2.Types.Destination
import Network.AWS.SESv2.Types.DkimAttributes
import Network.AWS.SESv2.Types.DkimSigningAttributes
import Network.AWS.SESv2.Types.DomainDeliverabilityCampaign
import Network.AWS.SESv2.Types.DomainDeliverabilityTrackingOption
import Network.AWS.SESv2.Types.DomainIspPlacement
import Network.AWS.SESv2.Types.EmailContent
import Network.AWS.SESv2.Types.EmailTemplateContent
import Network.AWS.SESv2.Types.EmailTemplateMetadata
import Network.AWS.SESv2.Types.EventDestination
import Network.AWS.SESv2.Types.EventDestinationDefinition
import Network.AWS.SESv2.Types.FailureInfo
import Network.AWS.SESv2.Types.IdentityInfo
import Network.AWS.SESv2.Types.ImportDataSource
import Network.AWS.SESv2.Types.ImportDestination
import Network.AWS.SESv2.Types.ImportJobSummary
import Network.AWS.SESv2.Types.InboxPlacementTrackingOption
import Network.AWS.SESv2.Types.IspPlacement
import Network.AWS.SESv2.Types.KinesisFirehoseDestination
import Network.AWS.SESv2.Types.ListContactsFilter
import Network.AWS.SESv2.Types.ListManagementOptions
import Network.AWS.SESv2.Types.MailFromAttributes
import Network.AWS.SESv2.Types.Message
import Network.AWS.SESv2.Types.MessageTag
import Network.AWS.SESv2.Types.OverallVolume
import Network.AWS.SESv2.Types.PinpointDestination
import Network.AWS.SESv2.Types.PlacementStatistics
import Network.AWS.SESv2.Types.RawMessage
import Network.AWS.SESv2.Types.ReplacementEmailContent
import Network.AWS.SESv2.Types.ReplacementTemplate
import Network.AWS.SESv2.Types.ReputationOptions
import Network.AWS.SESv2.Types.ReviewDetails
import Network.AWS.SESv2.Types.SendQuota
import Network.AWS.SESv2.Types.SendingOptions
import Network.AWS.SESv2.Types.SnsDestination
import Network.AWS.SESv2.Types.SuppressedDestination
import Network.AWS.SESv2.Types.SuppressedDestinationAttributes
import Network.AWS.SESv2.Types.SuppressedDestinationSummary
import Network.AWS.SESv2.Types.SuppressionAttributes
import Network.AWS.SESv2.Types.SuppressionListDestination
import Network.AWS.SESv2.Types.SuppressionOptions
import Network.AWS.SESv2.Types.Tag
import Network.AWS.SESv2.Types.Template
import Network.AWS.SESv2.Types.Topic
import Network.AWS.SESv2.Types.TopicFilter
import Network.AWS.SESv2.Types.TopicPreference
import Network.AWS.SESv2.Types.TrackingOptions
import Network.AWS.SESv2.Types.VolumeStatistics
import Network.AWS.SESv2.UntagResource
import Network.AWS.SESv2.UpdateConfigurationSetEventDestination
import Network.AWS.SESv2.UpdateContact
import Network.AWS.SESv2.UpdateContactList
import Network.AWS.SESv2.UpdateCustomVerificationEmailTemplate
import Network.AWS.SESv2.UpdateEmailIdentityPolicy
import Network.AWS.SESv2.UpdateEmailTemplate
