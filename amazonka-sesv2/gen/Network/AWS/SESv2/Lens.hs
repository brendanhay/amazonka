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
    listDedicatedIpPoolsResponse_nextToken,
    listDedicatedIpPoolsResponse_dedicatedIpPools,
    listDedicatedIpPoolsResponse_httpStatus,

    -- ** DeleteDedicatedIpPool
    deleteDedicatedIpPool_poolName,
    deleteDedicatedIpPoolResponse_httpStatus,

    -- ** GetImportJob
    getImportJob_jobId,
    getImportJobResponse_failedRecordsCount,
    getImportJobResponse_failureInfo,
    getImportJobResponse_createdTimestamp,
    getImportJobResponse_jobStatus,
    getImportJobResponse_completedTimestamp,
    getImportJobResponse_importDataSource,
    getImportJobResponse_processedRecordsCount,
    getImportJobResponse_importDestination,
    getImportJobResponse_jobId,
    getImportJobResponse_httpStatus,

    -- ** CreateContact
    createContact_unsubscribeAll,
    createContact_attributesData,
    createContact_topicPreferences,
    createContact_contactListName,
    createContact_emailAddress,
    createContactResponse_httpStatus,

    -- ** PutConfigurationSetTrackingOptions
    putConfigurationSetTrackingOptions_customRedirectDomain,
    putConfigurationSetTrackingOptions_configurationSetName,
    putConfigurationSetTrackingOptionsResponse_httpStatus,

    -- ** GetDeliverabilityTestReport
    getDeliverabilityTestReport_reportId,
    getDeliverabilityTestReportResponse_message,
    getDeliverabilityTestReportResponse_tags,
    getDeliverabilityTestReportResponse_httpStatus,
    getDeliverabilityTestReportResponse_deliverabilityTestReport,
    getDeliverabilityTestReportResponse_overallPlacement,
    getDeliverabilityTestReportResponse_ispPlacements,

    -- ** PutAccountDedicatedIpWarmupAttributes
    putAccountDedicatedIpWarmupAttributes_autoWarmupEnabled,
    putAccountDedicatedIpWarmupAttributesResponse_httpStatus,

    -- ** CreateEmailIdentity
    createEmailIdentity_dkimSigningAttributes,
    createEmailIdentity_tags,
    createEmailIdentity_configurationSetName,
    createEmailIdentity_emailIdentity,
    createEmailIdentityResponse_dkimAttributes,
    createEmailIdentityResponse_identityType,
    createEmailIdentityResponse_verifiedForSendingStatus,
    createEmailIdentityResponse_httpStatus,

    -- ** GetBlacklistReports
    getBlacklistReports_blacklistItemNames,
    getBlacklistReportsResponse_httpStatus,
    getBlacklistReportsResponse_blacklistReport,

    -- ** CreateConfigurationSetEventDestination
    createConfigurationSetEventDestination_configurationSetName,
    createConfigurationSetEventDestination_eventDestinationName,
    createConfigurationSetEventDestination_eventDestination,
    createConfigurationSetEventDestinationResponse_httpStatus,

    -- ** ListEmailIdentities
    listEmailIdentities_nextToken,
    listEmailIdentities_pageSize,
    listEmailIdentitiesResponse_emailIdentities,
    listEmailIdentitiesResponse_nextToken,
    listEmailIdentitiesResponse_httpStatus,

    -- ** CreateContactList
    createContactList_topics,
    createContactList_tags,
    createContactList_description,
    createContactList_contactListName,
    createContactListResponse_httpStatus,

    -- ** CreateConfigurationSet
    createConfigurationSet_trackingOptions,
    createConfigurationSet_sendingOptions,
    createConfigurationSet_deliveryOptions,
    createConfigurationSet_reputationOptions,
    createConfigurationSet_suppressionOptions,
    createConfigurationSet_tags,
    createConfigurationSet_configurationSetName,
    createConfigurationSetResponse_httpStatus,

    -- ** PutEmailIdentityConfigurationSetAttributes
    putEmailIdentityConfigurationSetAttributes_configurationSetName,
    putEmailIdentityConfigurationSetAttributes_emailIdentity,
    putEmailIdentityConfigurationSetAttributesResponse_httpStatus,

    -- ** DeleteConfigurationSet
    deleteConfigurationSet_configurationSetName,
    deleteConfigurationSetResponse_httpStatus,

    -- ** DeleteEmailIdentityPolicy
    deleteEmailIdentityPolicy_emailIdentity,
    deleteEmailIdentityPolicy_policyName,
    deleteEmailIdentityPolicyResponse_httpStatus,

    -- ** UpdateEmailIdentityPolicy
    updateEmailIdentityPolicy_emailIdentity,
    updateEmailIdentityPolicy_policyName,
    updateEmailIdentityPolicy_policy,
    updateEmailIdentityPolicyResponse_httpStatus,

    -- ** UpdateConfigurationSetEventDestination
    updateConfigurationSetEventDestination_configurationSetName,
    updateConfigurationSetEventDestination_eventDestinationName,
    updateConfigurationSetEventDestination_eventDestination,
    updateConfigurationSetEventDestinationResponse_httpStatus,

    -- ** ListImportJobs
    listImportJobs_nextToken,
    listImportJobs_importDestinationType,
    listImportJobs_pageSize,
    listImportJobsResponse_nextToken,
    listImportJobsResponse_importJobs,
    listImportJobsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteConfigurationSetEventDestination
    deleteConfigurationSetEventDestination_configurationSetName,
    deleteConfigurationSetEventDestination_eventDestinationName,
    deleteConfigurationSetEventDestinationResponse_httpStatus,

    -- ** SendEmail
    sendEmail_fromEmailAddress,
    sendEmail_feedbackForwardingEmailAddressIdentityArn,
    sendEmail_fromEmailAddressIdentityArn,
    sendEmail_destination,
    sendEmail_listManagementOptions,
    sendEmail_replyToAddresses,
    sendEmail_feedbackForwardingEmailAddress,
    sendEmail_configurationSetName,
    sendEmail_emailTags,
    sendEmail_content,
    sendEmailResponse_messageId,
    sendEmailResponse_httpStatus,

    -- ** PutConfigurationSetReputationOptions
    putConfigurationSetReputationOptions_reputationMetricsEnabled,
    putConfigurationSetReputationOptions_configurationSetName,
    putConfigurationSetReputationOptionsResponse_httpStatus,

    -- ** SendBulkEmail
    sendBulkEmail_fromEmailAddress,
    sendBulkEmail_defaultEmailTags,
    sendBulkEmail_feedbackForwardingEmailAddressIdentityArn,
    sendBulkEmail_fromEmailAddressIdentityArn,
    sendBulkEmail_replyToAddresses,
    sendBulkEmail_feedbackForwardingEmailAddress,
    sendBulkEmail_configurationSetName,
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

    -- ** PutDedicatedIpInPool
    putDedicatedIpInPool_ip,
    putDedicatedIpInPool_destinationPoolName,
    putDedicatedIpInPoolResponse_httpStatus,

    -- ** ListDomainDeliverabilityCampaigns
    listDomainDeliverabilityCampaigns_nextToken,
    listDomainDeliverabilityCampaigns_pageSize,
    listDomainDeliverabilityCampaigns_startDate,
    listDomainDeliverabilityCampaigns_endDate,
    listDomainDeliverabilityCampaigns_subscribedDomain,
    listDomainDeliverabilityCampaignsResponse_nextToken,
    listDomainDeliverabilityCampaignsResponse_httpStatus,
    listDomainDeliverabilityCampaignsResponse_domainDeliverabilityCampaigns,

    -- ** PutSuppressedDestination
    putSuppressedDestination_emailAddress,
    putSuppressedDestination_reason,
    putSuppressedDestinationResponse_httpStatus,

    -- ** PutAccountDetails
    putAccountDetails_productionAccessEnabled,
    putAccountDetails_contactLanguage,
    putAccountDetails_additionalContactEmailAddresses,
    putAccountDetails_mailType,
    putAccountDetails_websiteURL,
    putAccountDetails_useCaseDescription,
    putAccountDetailsResponse_httpStatus,

    -- ** CreateCustomVerificationEmailTemplate
    createCustomVerificationEmailTemplate_templateName,
    createCustomVerificationEmailTemplate_fromEmailAddress,
    createCustomVerificationEmailTemplate_templateSubject,
    createCustomVerificationEmailTemplate_templateContent,
    createCustomVerificationEmailTemplate_successRedirectionURL,
    createCustomVerificationEmailTemplate_failureRedirectionURL,
    createCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** GetSuppressedDestination
    getSuppressedDestination_emailAddress,
    getSuppressedDestinationResponse_httpStatus,
    getSuppressedDestinationResponse_suppressedDestination,

    -- ** GetEmailTemplate
    getEmailTemplate_templateName,
    getEmailTemplateResponse_httpStatus,
    getEmailTemplateResponse_templateName,
    getEmailTemplateResponse_templateContent,

    -- ** PutConfigurationSetSuppressionOptions
    putConfigurationSetSuppressionOptions_suppressedReasons,
    putConfigurationSetSuppressionOptions_configurationSetName,
    putConfigurationSetSuppressionOptionsResponse_httpStatus,

    -- ** DeleteCustomVerificationEmailTemplate
    deleteCustomVerificationEmailTemplate_templateName,
    deleteCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** GetDomainDeliverabilityCampaign
    getDomainDeliverabilityCampaign_campaignId,
    getDomainDeliverabilityCampaignResponse_httpStatus,
    getDomainDeliverabilityCampaignResponse_domainDeliverabilityCampaign,

    -- ** UpdateCustomVerificationEmailTemplate
    updateCustomVerificationEmailTemplate_templateName,
    updateCustomVerificationEmailTemplate_fromEmailAddress,
    updateCustomVerificationEmailTemplate_templateSubject,
    updateCustomVerificationEmailTemplate_templateContent,
    updateCustomVerificationEmailTemplate_successRedirectionURL,
    updateCustomVerificationEmailTemplate_failureRedirectionURL,
    updateCustomVerificationEmailTemplateResponse_httpStatus,

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

    -- ** GetDedicatedIps
    getDedicatedIps_nextToken,
    getDedicatedIps_pageSize,
    getDedicatedIps_poolName,
    getDedicatedIpsResponse_nextToken,
    getDedicatedIpsResponse_dedicatedIps,
    getDedicatedIpsResponse_httpStatus,

    -- ** GetAccount
    getAccountResponse_productionAccessEnabled,
    getAccountResponse_details,
    getAccountResponse_sendQuota,
    getAccountResponse_dedicatedIpAutoWarmupEnabled,
    getAccountResponse_sendingEnabled,
    getAccountResponse_suppressionAttributes,
    getAccountResponse_enforcementStatus,
    getAccountResponse_httpStatus,

    -- ** PutConfigurationSetSendingOptions
    putConfigurationSetSendingOptions_sendingEnabled,
    putConfigurationSetSendingOptions_configurationSetName,
    putConfigurationSetSendingOptionsResponse_httpStatus,

    -- ** GetConfigurationSet
    getConfigurationSet_configurationSetName,
    getConfigurationSetResponse_trackingOptions,
    getConfigurationSetResponse_sendingOptions,
    getConfigurationSetResponse_deliveryOptions,
    getConfigurationSetResponse_reputationOptions,
    getConfigurationSetResponse_suppressionOptions,
    getConfigurationSetResponse_tags,
    getConfigurationSetResponse_configurationSetName,
    getConfigurationSetResponse_httpStatus,

    -- ** GetDedicatedIp
    getDedicatedIp_ip,
    getDedicatedIpResponse_dedicatedIp,
    getDedicatedIpResponse_httpStatus,

    -- ** GetEmailIdentity
    getEmailIdentity_emailIdentity,
    getEmailIdentityResponse_dkimAttributes,
    getEmailIdentityResponse_feedbackForwardingStatus,
    getEmailIdentityResponse_policies,
    getEmailIdentityResponse_tags,
    getEmailIdentityResponse_identityType,
    getEmailIdentityResponse_mailFromAttributes,
    getEmailIdentityResponse_configurationSetName,
    getEmailIdentityResponse_verifiedForSendingStatus,
    getEmailIdentityResponse_httpStatus,

    -- ** PutEmailIdentityDkimSigningAttributes
    putEmailIdentityDkimSigningAttributes_signingAttributes,
    putEmailIdentityDkimSigningAttributes_emailIdentity,
    putEmailIdentityDkimSigningAttributes_signingAttributesOrigin,
    putEmailIdentityDkimSigningAttributesResponse_dkimStatus,
    putEmailIdentityDkimSigningAttributesResponse_dkimTokens,
    putEmailIdentityDkimSigningAttributesResponse_httpStatus,

    -- ** GetContactList
    getContactList_contactListName,
    getContactListResponse_createdTimestamp,
    getContactListResponse_topics,
    getContactListResponse_tags,
    getContactListResponse_lastUpdatedTimestamp,
    getContactListResponse_description,
    getContactListResponse_contactListName,
    getContactListResponse_httpStatus,

    -- ** UpdateContact
    updateContact_unsubscribeAll,
    updateContact_attributesData,
    updateContact_topicPreferences,
    updateContact_contactListName,
    updateContact_emailAddress,
    updateContactResponse_httpStatus,

    -- ** ListContacts
    listContacts_nextToken,
    listContacts_pageSize,
    listContacts_filter,
    listContacts_contactListName,
    listContactsResponse_nextToken,
    listContactsResponse_contacts,
    listContactsResponse_httpStatus,

    -- ** DeleteContact
    deleteContact_contactListName,
    deleteContact_emailAddress,
    deleteContactResponse_httpStatus,

    -- ** PutDeliverabilityDashboardOption
    putDeliverabilityDashboardOption_subscribedDomains,
    putDeliverabilityDashboardOption_dashboardEnabled,
    putDeliverabilityDashboardOptionResponse_httpStatus,

    -- ** ListDeliverabilityTestReports
    listDeliverabilityTestReports_nextToken,
    listDeliverabilityTestReports_pageSize,
    listDeliverabilityTestReportsResponse_nextToken,
    listDeliverabilityTestReportsResponse_httpStatus,
    listDeliverabilityTestReportsResponse_deliverabilityTestReports,

    -- ** CreateImportJob
    createImportJob_importDestination,
    createImportJob_importDataSource,
    createImportJobResponse_jobId,
    createImportJobResponse_httpStatus,

    -- ** GetContact
    getContact_contactListName,
    getContact_emailAddress,
    getContactResponse_createdTimestamp,
    getContactResponse_topicDefaultPreferences,
    getContactResponse_unsubscribeAll,
    getContactResponse_attributesData,
    getContactResponse_topicPreferences,
    getContactResponse_lastUpdatedTimestamp,
    getContactResponse_contactListName,
    getContactResponse_emailAddress,
    getContactResponse_httpStatus,

    -- ** PutAccountSuppressionAttributes
    putAccountSuppressionAttributes_suppressedReasons,
    putAccountSuppressionAttributesResponse_httpStatus,

    -- ** CreateEmailIdentityPolicy
    createEmailIdentityPolicy_emailIdentity,
    createEmailIdentityPolicy_policyName,
    createEmailIdentityPolicy_policy,
    createEmailIdentityPolicyResponse_httpStatus,

    -- ** PutAccountSendingAttributes
    putAccountSendingAttributes_sendingEnabled,
    putAccountSendingAttributesResponse_httpStatus,

    -- ** UpdateContactList
    updateContactList_topics,
    updateContactList_description,
    updateContactList_contactListName,
    updateContactListResponse_httpStatus,

    -- ** ListConfigurationSets
    listConfigurationSets_nextToken,
    listConfigurationSets_pageSize,
    listConfigurationSetsResponse_nextToken,
    listConfigurationSetsResponse_configurationSets,
    listConfigurationSetsResponse_httpStatus,

    -- ** ListContactLists
    listContactLists_nextToken,
    listContactLists_pageSize,
    listContactListsResponse_nextToken,
    listContactListsResponse_contactLists,
    listContactListsResponse_httpStatus,

    -- ** DeleteContactList
    deleteContactList_contactListName,
    deleteContactListResponse_httpStatus,

    -- ** CreateDeliverabilityTestReport
    createDeliverabilityTestReport_reportName,
    createDeliverabilityTestReport_tags,
    createDeliverabilityTestReport_fromEmailAddress,
    createDeliverabilityTestReport_content,
    createDeliverabilityTestReportResponse_httpStatus,
    createDeliverabilityTestReportResponse_reportId,
    createDeliverabilityTestReportResponse_deliverabilityTestStatus,

    -- ** DeleteEmailIdentity
    deleteEmailIdentity_emailIdentity,
    deleteEmailIdentityResponse_httpStatus,

    -- ** PutEmailIdentityMailFromAttributes
    putEmailIdentityMailFromAttributes_mailFromDomain,
    putEmailIdentityMailFromAttributes_behaviorOnMxFailure,
    putEmailIdentityMailFromAttributes_emailIdentity,
    putEmailIdentityMailFromAttributesResponse_httpStatus,

    -- ** GetCustomVerificationEmailTemplate
    getCustomVerificationEmailTemplate_templateName,
    getCustomVerificationEmailTemplateResponse_templateName,
    getCustomVerificationEmailTemplateResponse_templateSubject,
    getCustomVerificationEmailTemplateResponse_fromEmailAddress,
    getCustomVerificationEmailTemplateResponse_templateContent,
    getCustomVerificationEmailTemplateResponse_successRedirectionURL,
    getCustomVerificationEmailTemplateResponse_failureRedirectionURL,
    getCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** PutDedicatedIpWarmupAttributes
    putDedicatedIpWarmupAttributes_ip,
    putDedicatedIpWarmupAttributes_warmupPercentage,
    putDedicatedIpWarmupAttributesResponse_httpStatus,

    -- ** CreateEmailTemplate
    createEmailTemplate_templateName,
    createEmailTemplate_templateContent,
    createEmailTemplateResponse_httpStatus,

    -- ** GetDomainStatisticsReport
    getDomainStatisticsReport_domain,
    getDomainStatisticsReport_startDate,
    getDomainStatisticsReport_endDate,
    getDomainStatisticsReportResponse_httpStatus,
    getDomainStatisticsReportResponse_overallVolume,
    getDomainStatisticsReportResponse_dailyVolumes,

    -- ** PutEmailIdentityFeedbackAttributes
    putEmailIdentityFeedbackAttributes_emailForwardingEnabled,
    putEmailIdentityFeedbackAttributes_emailIdentity,
    putEmailIdentityFeedbackAttributesResponse_httpStatus,

    -- ** GetDeliverabilityDashboardOptions
    getDeliverabilityDashboardOptionsResponse_subscriptionExpiryDate,
    getDeliverabilityDashboardOptionsResponse_accountStatus,
    getDeliverabilityDashboardOptionsResponse_activeSubscribedDomains,
    getDeliverabilityDashboardOptionsResponse_pendingExpirationSubscribedDomains,
    getDeliverabilityDashboardOptionsResponse_httpStatus,
    getDeliverabilityDashboardOptionsResponse_dashboardEnabled,

    -- ** ListEmailTemplates
    listEmailTemplates_nextToken,
    listEmailTemplates_pageSize,
    listEmailTemplatesResponse_nextToken,
    listEmailTemplatesResponse_templatesMetadata,
    listEmailTemplatesResponse_httpStatus,

    -- ** DeleteEmailTemplate
    deleteEmailTemplate_templateName,
    deleteEmailTemplateResponse_httpStatus,

    -- ** ListSuppressedDestinations
    listSuppressedDestinations_nextToken,
    listSuppressedDestinations_startDate,
    listSuppressedDestinations_pageSize,
    listSuppressedDestinations_reasons,
    listSuppressedDestinations_endDate,
    listSuppressedDestinationsResponse_nextToken,
    listSuppressedDestinationsResponse_suppressedDestinationSummaries,
    listSuppressedDestinationsResponse_httpStatus,

    -- ** UpdateEmailTemplate
    updateEmailTemplate_templateName,
    updateEmailTemplate_templateContent,
    updateEmailTemplateResponse_httpStatus,

    -- ** DeleteSuppressedDestination
    deleteSuppressedDestination_emailAddress,
    deleteSuppressedDestinationResponse_httpStatus,

    -- ** GetEmailIdentityPolicies
    getEmailIdentityPolicies_emailIdentity,
    getEmailIdentityPoliciesResponse_policies,
    getEmailIdentityPoliciesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** SendCustomVerificationEmail
    sendCustomVerificationEmail_configurationSetName,
    sendCustomVerificationEmail_emailAddress,
    sendCustomVerificationEmail_templateName,
    sendCustomVerificationEmailResponse_messageId,
    sendCustomVerificationEmailResponse_httpStatus,

    -- ** CreateDedicatedIpPool
    createDedicatedIpPool_tags,
    createDedicatedIpPool_poolName,
    createDedicatedIpPoolResponse_httpStatus,

    -- * Types

    -- ** AccountDetails
    accountDetails_useCaseDescription,
    accountDetails_mailType,
    accountDetails_contactLanguage,
    accountDetails_websiteURL,
    accountDetails_reviewDetails,
    accountDetails_additionalContactEmailAddresses,

    -- ** BlacklistEntry
    blacklistEntry_rblName,
    blacklistEntry_listingTime,
    blacklistEntry_description,

    -- ** Body
    body_html,
    body_text,

    -- ** BulkEmailContent
    bulkEmailContent_template,

    -- ** BulkEmailEntry
    bulkEmailEntry_replacementTags,
    bulkEmailEntry_replacementEmailContent,
    bulkEmailEntry_destination,

    -- ** BulkEmailEntryResult
    bulkEmailEntryResult_status,
    bulkEmailEntryResult_messageId,
    bulkEmailEntryResult_error,

    -- ** CloudWatchDestination
    cloudWatchDestination_dimensionConfigurations,

    -- ** CloudWatchDimensionConfiguration
    cloudWatchDimensionConfiguration_dimensionName,
    cloudWatchDimensionConfiguration_dimensionValueSource,
    cloudWatchDimensionConfiguration_defaultDimensionValue,

    -- ** Contact
    contact_topicDefaultPreferences,
    contact_unsubscribeAll,
    contact_topicPreferences,
    contact_lastUpdatedTimestamp,
    contact_emailAddress,

    -- ** ContactList
    contactList_lastUpdatedTimestamp,
    contactList_contactListName,

    -- ** ContactListDestination
    contactListDestination_contactListName,
    contactListDestination_contactListImportAction,

    -- ** Content
    content_charset,
    content_data,

    -- ** CustomVerificationEmailTemplateMetadata
    customVerificationEmailTemplateMetadata_templateName,
    customVerificationEmailTemplateMetadata_templateSubject,
    customVerificationEmailTemplateMetadata_fromEmailAddress,
    customVerificationEmailTemplateMetadata_successRedirectionURL,
    customVerificationEmailTemplateMetadata_failureRedirectionURL,

    -- ** DailyVolume
    dailyVolume_startDate,
    dailyVolume_volumeStatistics,
    dailyVolume_domainIspPlacements,

    -- ** DedicatedIp
    dedicatedIp_poolName,
    dedicatedIp_ip,
    dedicatedIp_warmupStatus,
    dedicatedIp_warmupPercentage,

    -- ** DeliverabilityTestReport
    deliverabilityTestReport_fromEmailAddress,
    deliverabilityTestReport_reportName,
    deliverabilityTestReport_reportId,
    deliverabilityTestReport_createDate,
    deliverabilityTestReport_deliverabilityTestStatus,
    deliverabilityTestReport_subject,

    -- ** DeliveryOptions
    deliveryOptions_sendingPoolName,
    deliveryOptions_tlsPolicy,

    -- ** Destination
    destination_toAddresses,
    destination_ccAddresses,
    destination_bccAddresses,

    -- ** DkimAttributes
    dkimAttributes_status,
    dkimAttributes_signingAttributesOrigin,
    dkimAttributes_tokens,
    dkimAttributes_signingEnabled,

    -- ** DkimSigningAttributes
    dkimSigningAttributes_domainSigningSelector,
    dkimSigningAttributes_domainSigningPrivateKey,

    -- ** DomainDeliverabilityCampaign
    domainDeliverabilityCampaign_projectedVolume,
    domainDeliverabilityCampaign_inboxCount,
    domainDeliverabilityCampaign_readDeleteRate,
    domainDeliverabilityCampaign_firstSeenDateTime,
    domainDeliverabilityCampaign_lastSeenDateTime,
    domainDeliverabilityCampaign_campaignId,
    domainDeliverabilityCampaign_deleteRate,
    domainDeliverabilityCampaign_imageUrl,
    domainDeliverabilityCampaign_spamCount,
    domainDeliverabilityCampaign_subject,
    domainDeliverabilityCampaign_sendingIps,
    domainDeliverabilityCampaign_readRate,
    domainDeliverabilityCampaign_fromAddress,
    domainDeliverabilityCampaign_esps,

    -- ** DomainDeliverabilityTrackingOption
    domainDeliverabilityTrackingOption_subscriptionStartDate,
    domainDeliverabilityTrackingOption_domain,
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
    emailTemplateContent_html,
    emailTemplateContent_subject,
    emailTemplateContent_text,

    -- ** EmailTemplateMetadata
    emailTemplateMetadata_templateName,
    emailTemplateMetadata_createdTimestamp,

    -- ** EventDestination
    eventDestination_cloudWatchDestination,
    eventDestination_enabled,
    eventDestination_pinpointDestination,
    eventDestination_kinesisFirehoseDestination,
    eventDestination_snsDestination,
    eventDestination_name,
    eventDestination_matchingEventTypes,

    -- ** EventDestinationDefinition
    eventDestinationDefinition_matchingEventTypes,
    eventDestinationDefinition_cloudWatchDestination,
    eventDestinationDefinition_enabled,
    eventDestinationDefinition_pinpointDestination,
    eventDestinationDefinition_kinesisFirehoseDestination,
    eventDestinationDefinition_snsDestination,

    -- ** FailureInfo
    failureInfo_failedRecordsS3Url,
    failureInfo_errorMessage,

    -- ** IdentityInfo
    identityInfo_sendingEnabled,
    identityInfo_identityName,
    identityInfo_identityType,

    -- ** ImportDataSource
    importDataSource_s3Url,
    importDataSource_dataFormat,

    -- ** ImportDestination
    importDestination_suppressionListDestination,
    importDestination_contactListDestination,

    -- ** ImportJobSummary
    importJobSummary_createdTimestamp,
    importJobSummary_jobStatus,
    importJobSummary_importDestination,
    importJobSummary_jobId,

    -- ** InboxPlacementTrackingOption
    inboxPlacementTrackingOption_trackedIsps,
    inboxPlacementTrackingOption_global,

    -- ** IspPlacement
    ispPlacement_ispName,
    ispPlacement_placementStatistics,

    -- ** KinesisFirehoseDestination
    kinesisFirehoseDestination_iamRoleArn,
    kinesisFirehoseDestination_deliveryStreamArn,

    -- ** ListContactsFilter
    listContactsFilter_topicFilter,
    listContactsFilter_filteredStatus,

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
    overallVolume_readRatePercent,
    overallVolume_volumeStatistics,
    overallVolume_domainIspPlacements,

    -- ** PinpointDestination
    pinpointDestination_applicationArn,

    -- ** PlacementStatistics
    placementStatistics_spamPercentage,
    placementStatistics_dkimPercentage,
    placementStatistics_spfPercentage,
    placementStatistics_inboxPercentage,
    placementStatistics_missingPercentage,

    -- ** RawMessage
    rawMessage_data,

    -- ** ReplacementEmailContent
    replacementEmailContent_replacementTemplate,

    -- ** ReplacementTemplate
    replacementTemplate_replacementTemplateData,

    -- ** ReputationOptions
    reputationOptions_reputationMetricsEnabled,
    reputationOptions_lastFreshStart,

    -- ** ReviewDetails
    reviewDetails_status,
    reviewDetails_caseId,

    -- ** SendQuota
    sendQuota_max24HourSend,
    sendQuota_sentLast24Hours,
    sendQuota_maxSendRate,

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
    template_templateData,
    template_templateArn,

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
    volumeStatistics_projectedInbox,
    volumeStatistics_projectedSpam,
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
