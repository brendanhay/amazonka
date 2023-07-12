{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SESV2.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Lens
  ( -- * Operations

    -- ** BatchGetMetricData
    batchGetMetricData_queries,
    batchGetMetricDataResponse_errors,
    batchGetMetricDataResponse_results,
    batchGetMetricDataResponse_httpStatus,

    -- ** CreateConfigurationSet
    createConfigurationSet_deliveryOptions,
    createConfigurationSet_reputationOptions,
    createConfigurationSet_sendingOptions,
    createConfigurationSet_suppressionOptions,
    createConfigurationSet_tags,
    createConfigurationSet_trackingOptions,
    createConfigurationSet_vdmOptions,
    createConfigurationSet_configurationSetName,
    createConfigurationSetResponse_httpStatus,

    -- ** CreateConfigurationSetEventDestination
    createConfigurationSetEventDestination_configurationSetName,
    createConfigurationSetEventDestination_eventDestinationName,
    createConfigurationSetEventDestination_eventDestination,
    createConfigurationSetEventDestinationResponse_httpStatus,

    -- ** CreateContact
    createContact_attributesData,
    createContact_topicPreferences,
    createContact_unsubscribeAll,
    createContact_contactListName,
    createContact_emailAddress,
    createContactResponse_httpStatus,

    -- ** CreateContactList
    createContactList_description,
    createContactList_tags,
    createContactList_topics,
    createContactList_contactListName,
    createContactListResponse_httpStatus,

    -- ** CreateCustomVerificationEmailTemplate
    createCustomVerificationEmailTemplate_templateName,
    createCustomVerificationEmailTemplate_fromEmailAddress,
    createCustomVerificationEmailTemplate_templateSubject,
    createCustomVerificationEmailTemplate_templateContent,
    createCustomVerificationEmailTemplate_successRedirectionURL,
    createCustomVerificationEmailTemplate_failureRedirectionURL,
    createCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** CreateDedicatedIpPool
    createDedicatedIpPool_scalingMode,
    createDedicatedIpPool_tags,
    createDedicatedIpPool_poolName,
    createDedicatedIpPoolResponse_httpStatus,

    -- ** CreateDeliverabilityTestReport
    createDeliverabilityTestReport_reportName,
    createDeliverabilityTestReport_tags,
    createDeliverabilityTestReport_fromEmailAddress,
    createDeliverabilityTestReport_content,
    createDeliverabilityTestReportResponse_httpStatus,
    createDeliverabilityTestReportResponse_reportId,
    createDeliverabilityTestReportResponse_deliverabilityTestStatus,

    -- ** CreateEmailIdentity
    createEmailIdentity_configurationSetName,
    createEmailIdentity_dkimSigningAttributes,
    createEmailIdentity_tags,
    createEmailIdentity_emailIdentity,
    createEmailIdentityResponse_dkimAttributes,
    createEmailIdentityResponse_identityType,
    createEmailIdentityResponse_verifiedForSendingStatus,
    createEmailIdentityResponse_httpStatus,

    -- ** CreateEmailIdentityPolicy
    createEmailIdentityPolicy_emailIdentity,
    createEmailIdentityPolicy_policyName,
    createEmailIdentityPolicy_policy,
    createEmailIdentityPolicyResponse_httpStatus,

    -- ** CreateEmailTemplate
    createEmailTemplate_templateName,
    createEmailTemplate_templateContent,
    createEmailTemplateResponse_httpStatus,

    -- ** CreateImportJob
    createImportJob_importDestination,
    createImportJob_importDataSource,
    createImportJobResponse_jobId,
    createImportJobResponse_httpStatus,

    -- ** DeleteConfigurationSet
    deleteConfigurationSet_configurationSetName,
    deleteConfigurationSetResponse_httpStatus,

    -- ** DeleteConfigurationSetEventDestination
    deleteConfigurationSetEventDestination_configurationSetName,
    deleteConfigurationSetEventDestination_eventDestinationName,
    deleteConfigurationSetEventDestinationResponse_httpStatus,

    -- ** DeleteContact
    deleteContact_contactListName,
    deleteContact_emailAddress,
    deleteContactResponse_httpStatus,

    -- ** DeleteContactList
    deleteContactList_contactListName,
    deleteContactListResponse_httpStatus,

    -- ** DeleteCustomVerificationEmailTemplate
    deleteCustomVerificationEmailTemplate_templateName,
    deleteCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** DeleteDedicatedIpPool
    deleteDedicatedIpPool_poolName,
    deleteDedicatedIpPoolResponse_httpStatus,

    -- ** DeleteEmailIdentity
    deleteEmailIdentity_emailIdentity,
    deleteEmailIdentityResponse_httpStatus,

    -- ** DeleteEmailIdentityPolicy
    deleteEmailIdentityPolicy_emailIdentity,
    deleteEmailIdentityPolicy_policyName,
    deleteEmailIdentityPolicyResponse_httpStatus,

    -- ** DeleteEmailTemplate
    deleteEmailTemplate_templateName,
    deleteEmailTemplateResponse_httpStatus,

    -- ** DeleteSuppressedDestination
    deleteSuppressedDestination_emailAddress,
    deleteSuppressedDestinationResponse_httpStatus,

    -- ** GetAccount
    getAccountResponse_dedicatedIpAutoWarmupEnabled,
    getAccountResponse_details,
    getAccountResponse_enforcementStatus,
    getAccountResponse_productionAccessEnabled,
    getAccountResponse_sendQuota,
    getAccountResponse_sendingEnabled,
    getAccountResponse_suppressionAttributes,
    getAccountResponse_vdmAttributes,
    getAccountResponse_httpStatus,

    -- ** GetBlacklistReports
    getBlacklistReports_blacklistItemNames,
    getBlacklistReportsResponse_httpStatus,
    getBlacklistReportsResponse_blacklistReport,

    -- ** GetConfigurationSet
    getConfigurationSet_configurationSetName,
    getConfigurationSetResponse_configurationSetName,
    getConfigurationSetResponse_deliveryOptions,
    getConfigurationSetResponse_reputationOptions,
    getConfigurationSetResponse_sendingOptions,
    getConfigurationSetResponse_suppressionOptions,
    getConfigurationSetResponse_tags,
    getConfigurationSetResponse_trackingOptions,
    getConfigurationSetResponse_vdmOptions,
    getConfigurationSetResponse_httpStatus,

    -- ** GetConfigurationSetEventDestinations
    getConfigurationSetEventDestinations_configurationSetName,
    getConfigurationSetEventDestinationsResponse_eventDestinations,
    getConfigurationSetEventDestinationsResponse_httpStatus,

    -- ** GetContact
    getContact_contactListName,
    getContact_emailAddress,
    getContactResponse_attributesData,
    getContactResponse_contactListName,
    getContactResponse_createdTimestamp,
    getContactResponse_emailAddress,
    getContactResponse_lastUpdatedTimestamp,
    getContactResponse_topicDefaultPreferences,
    getContactResponse_topicPreferences,
    getContactResponse_unsubscribeAll,
    getContactResponse_httpStatus,

    -- ** GetContactList
    getContactList_contactListName,
    getContactListResponse_contactListName,
    getContactListResponse_createdTimestamp,
    getContactListResponse_description,
    getContactListResponse_lastUpdatedTimestamp,
    getContactListResponse_tags,
    getContactListResponse_topics,
    getContactListResponse_httpStatus,

    -- ** GetCustomVerificationEmailTemplate
    getCustomVerificationEmailTemplate_templateName,
    getCustomVerificationEmailTemplateResponse_failureRedirectionURL,
    getCustomVerificationEmailTemplateResponse_fromEmailAddress,
    getCustomVerificationEmailTemplateResponse_successRedirectionURL,
    getCustomVerificationEmailTemplateResponse_templateContent,
    getCustomVerificationEmailTemplateResponse_templateName,
    getCustomVerificationEmailTemplateResponse_templateSubject,
    getCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** GetDedicatedIp
    getDedicatedIp_ip,
    getDedicatedIpResponse_dedicatedIp,
    getDedicatedIpResponse_httpStatus,

    -- ** GetDedicatedIpPool
    getDedicatedIpPool_poolName,
    getDedicatedIpPoolResponse_dedicatedIpPool,
    getDedicatedIpPoolResponse_httpStatus,

    -- ** GetDedicatedIps
    getDedicatedIps_nextToken,
    getDedicatedIps_pageSize,
    getDedicatedIps_poolName,
    getDedicatedIpsResponse_dedicatedIps,
    getDedicatedIpsResponse_nextToken,
    getDedicatedIpsResponse_httpStatus,

    -- ** GetDeliverabilityDashboardOptions
    getDeliverabilityDashboardOptionsResponse_accountStatus,
    getDeliverabilityDashboardOptionsResponse_activeSubscribedDomains,
    getDeliverabilityDashboardOptionsResponse_pendingExpirationSubscribedDomains,
    getDeliverabilityDashboardOptionsResponse_subscriptionExpiryDate,
    getDeliverabilityDashboardOptionsResponse_httpStatus,
    getDeliverabilityDashboardOptionsResponse_dashboardEnabled,

    -- ** GetDeliverabilityTestReport
    getDeliverabilityTestReport_reportId,
    getDeliverabilityTestReportResponse_message,
    getDeliverabilityTestReportResponse_tags,
    getDeliverabilityTestReportResponse_httpStatus,
    getDeliverabilityTestReportResponse_deliverabilityTestReport,
    getDeliverabilityTestReportResponse_overallPlacement,
    getDeliverabilityTestReportResponse_ispPlacements,

    -- ** GetDomainDeliverabilityCampaign
    getDomainDeliverabilityCampaign_campaignId,
    getDomainDeliverabilityCampaignResponse_httpStatus,
    getDomainDeliverabilityCampaignResponse_domainDeliverabilityCampaign,

    -- ** GetDomainStatisticsReport
    getDomainStatisticsReport_domain,
    getDomainStatisticsReport_startDate,
    getDomainStatisticsReport_endDate,
    getDomainStatisticsReportResponse_httpStatus,
    getDomainStatisticsReportResponse_overallVolume,
    getDomainStatisticsReportResponse_dailyVolumes,

    -- ** GetEmailIdentity
    getEmailIdentity_emailIdentity,
    getEmailIdentityResponse_configurationSetName,
    getEmailIdentityResponse_dkimAttributes,
    getEmailIdentityResponse_feedbackForwardingStatus,
    getEmailIdentityResponse_identityType,
    getEmailIdentityResponse_mailFromAttributes,
    getEmailIdentityResponse_policies,
    getEmailIdentityResponse_tags,
    getEmailIdentityResponse_verificationStatus,
    getEmailIdentityResponse_verifiedForSendingStatus,
    getEmailIdentityResponse_httpStatus,

    -- ** GetEmailIdentityPolicies
    getEmailIdentityPolicies_emailIdentity,
    getEmailIdentityPoliciesResponse_policies,
    getEmailIdentityPoliciesResponse_httpStatus,

    -- ** GetEmailTemplate
    getEmailTemplate_templateName,
    getEmailTemplateResponse_httpStatus,
    getEmailTemplateResponse_templateName,
    getEmailTemplateResponse_templateContent,

    -- ** GetImportJob
    getImportJob_jobId,
    getImportJobResponse_completedTimestamp,
    getImportJobResponse_createdTimestamp,
    getImportJobResponse_failedRecordsCount,
    getImportJobResponse_failureInfo,
    getImportJobResponse_importDataSource,
    getImportJobResponse_importDestination,
    getImportJobResponse_jobId,
    getImportJobResponse_jobStatus,
    getImportJobResponse_processedRecordsCount,
    getImportJobResponse_httpStatus,

    -- ** GetSuppressedDestination
    getSuppressedDestination_emailAddress,
    getSuppressedDestinationResponse_httpStatus,
    getSuppressedDestinationResponse_suppressedDestination,

    -- ** ListConfigurationSets
    listConfigurationSets_nextToken,
    listConfigurationSets_pageSize,
    listConfigurationSetsResponse_configurationSets,
    listConfigurationSetsResponse_nextToken,
    listConfigurationSetsResponse_httpStatus,

    -- ** ListContactLists
    listContactLists_nextToken,
    listContactLists_pageSize,
    listContactListsResponse_contactLists,
    listContactListsResponse_nextToken,
    listContactListsResponse_httpStatus,

    -- ** ListContacts
    listContacts_filter,
    listContacts_nextToken,
    listContacts_pageSize,
    listContacts_contactListName,
    listContactsResponse_contacts,
    listContactsResponse_nextToken,
    listContactsResponse_httpStatus,

    -- ** ListCustomVerificationEmailTemplates
    listCustomVerificationEmailTemplates_nextToken,
    listCustomVerificationEmailTemplates_pageSize,
    listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates,
    listCustomVerificationEmailTemplatesResponse_nextToken,
    listCustomVerificationEmailTemplatesResponse_httpStatus,

    -- ** ListDedicatedIpPools
    listDedicatedIpPools_nextToken,
    listDedicatedIpPools_pageSize,
    listDedicatedIpPoolsResponse_dedicatedIpPools,
    listDedicatedIpPoolsResponse_nextToken,
    listDedicatedIpPoolsResponse_httpStatus,

    -- ** ListDeliverabilityTestReports
    listDeliverabilityTestReports_nextToken,
    listDeliverabilityTestReports_pageSize,
    listDeliverabilityTestReportsResponse_nextToken,
    listDeliverabilityTestReportsResponse_httpStatus,
    listDeliverabilityTestReportsResponse_deliverabilityTestReports,

    -- ** ListDomainDeliverabilityCampaigns
    listDomainDeliverabilityCampaigns_nextToken,
    listDomainDeliverabilityCampaigns_pageSize,
    listDomainDeliverabilityCampaigns_startDate,
    listDomainDeliverabilityCampaigns_endDate,
    listDomainDeliverabilityCampaigns_subscribedDomain,
    listDomainDeliverabilityCampaignsResponse_nextToken,
    listDomainDeliverabilityCampaignsResponse_httpStatus,
    listDomainDeliverabilityCampaignsResponse_domainDeliverabilityCampaigns,

    -- ** ListEmailIdentities
    listEmailIdentities_nextToken,
    listEmailIdentities_pageSize,
    listEmailIdentitiesResponse_emailIdentities,
    listEmailIdentitiesResponse_nextToken,
    listEmailIdentitiesResponse_httpStatus,

    -- ** ListEmailTemplates
    listEmailTemplates_nextToken,
    listEmailTemplates_pageSize,
    listEmailTemplatesResponse_nextToken,
    listEmailTemplatesResponse_templatesMetadata,
    listEmailTemplatesResponse_httpStatus,

    -- ** ListImportJobs
    listImportJobs_importDestinationType,
    listImportJobs_nextToken,
    listImportJobs_pageSize,
    listImportJobsResponse_importJobs,
    listImportJobsResponse_nextToken,
    listImportJobsResponse_httpStatus,

    -- ** ListRecommendations
    listRecommendations_filter,
    listRecommendations_nextToken,
    listRecommendations_pageSize,
    listRecommendationsResponse_nextToken,
    listRecommendationsResponse_recommendations,
    listRecommendationsResponse_httpStatus,

    -- ** ListSuppressedDestinations
    listSuppressedDestinations_endDate,
    listSuppressedDestinations_nextToken,
    listSuppressedDestinations_pageSize,
    listSuppressedDestinations_reasons,
    listSuppressedDestinations_startDate,
    listSuppressedDestinationsResponse_nextToken,
    listSuppressedDestinationsResponse_suppressedDestinationSummaries,
    listSuppressedDestinationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** PutAccountDedicatedIpWarmupAttributes
    putAccountDedicatedIpWarmupAttributes_autoWarmupEnabled,
    putAccountDedicatedIpWarmupAttributesResponse_httpStatus,

    -- ** PutAccountDetails
    putAccountDetails_additionalContactEmailAddresses,
    putAccountDetails_contactLanguage,
    putAccountDetails_productionAccessEnabled,
    putAccountDetails_mailType,
    putAccountDetails_websiteURL,
    putAccountDetails_useCaseDescription,
    putAccountDetailsResponse_httpStatus,

    -- ** PutAccountSendingAttributes
    putAccountSendingAttributes_sendingEnabled,
    putAccountSendingAttributesResponse_httpStatus,

    -- ** PutAccountSuppressionAttributes
    putAccountSuppressionAttributes_suppressedReasons,
    putAccountSuppressionAttributesResponse_httpStatus,

    -- ** PutAccountVdmAttributes
    putAccountVdmAttributes_vdmAttributes,
    putAccountVdmAttributesResponse_httpStatus,

    -- ** PutConfigurationSetDeliveryOptions
    putConfigurationSetDeliveryOptions_sendingPoolName,
    putConfigurationSetDeliveryOptions_tlsPolicy,
    putConfigurationSetDeliveryOptions_configurationSetName,
    putConfigurationSetDeliveryOptionsResponse_httpStatus,

    -- ** PutConfigurationSetReputationOptions
    putConfigurationSetReputationOptions_reputationMetricsEnabled,
    putConfigurationSetReputationOptions_configurationSetName,
    putConfigurationSetReputationOptionsResponse_httpStatus,

    -- ** PutConfigurationSetSendingOptions
    putConfigurationSetSendingOptions_sendingEnabled,
    putConfigurationSetSendingOptions_configurationSetName,
    putConfigurationSetSendingOptionsResponse_httpStatus,

    -- ** PutConfigurationSetSuppressionOptions
    putConfigurationSetSuppressionOptions_suppressedReasons,
    putConfigurationSetSuppressionOptions_configurationSetName,
    putConfigurationSetSuppressionOptionsResponse_httpStatus,

    -- ** PutConfigurationSetTrackingOptions
    putConfigurationSetTrackingOptions_customRedirectDomain,
    putConfigurationSetTrackingOptions_configurationSetName,
    putConfigurationSetTrackingOptionsResponse_httpStatus,

    -- ** PutConfigurationSetVdmOptions
    putConfigurationSetVdmOptions_vdmOptions,
    putConfigurationSetVdmOptions_configurationSetName,
    putConfigurationSetVdmOptionsResponse_httpStatus,

    -- ** PutDedicatedIpInPool
    putDedicatedIpInPool_ip,
    putDedicatedIpInPool_destinationPoolName,
    putDedicatedIpInPoolResponse_httpStatus,

    -- ** PutDedicatedIpWarmupAttributes
    putDedicatedIpWarmupAttributes_ip,
    putDedicatedIpWarmupAttributes_warmupPercentage,
    putDedicatedIpWarmupAttributesResponse_httpStatus,

    -- ** PutDeliverabilityDashboardOption
    putDeliverabilityDashboardOption_subscribedDomains,
    putDeliverabilityDashboardOption_dashboardEnabled,
    putDeliverabilityDashboardOptionResponse_httpStatus,

    -- ** PutEmailIdentityConfigurationSetAttributes
    putEmailIdentityConfigurationSetAttributes_configurationSetName,
    putEmailIdentityConfigurationSetAttributes_emailIdentity,
    putEmailIdentityConfigurationSetAttributesResponse_httpStatus,

    -- ** PutEmailIdentityDkimAttributes
    putEmailIdentityDkimAttributes_signingEnabled,
    putEmailIdentityDkimAttributes_emailIdentity,
    putEmailIdentityDkimAttributesResponse_httpStatus,

    -- ** PutEmailIdentityDkimSigningAttributes
    putEmailIdentityDkimSigningAttributes_signingAttributes,
    putEmailIdentityDkimSigningAttributes_emailIdentity,
    putEmailIdentityDkimSigningAttributes_signingAttributesOrigin,
    putEmailIdentityDkimSigningAttributesResponse_dkimStatus,
    putEmailIdentityDkimSigningAttributesResponse_dkimTokens,
    putEmailIdentityDkimSigningAttributesResponse_httpStatus,

    -- ** PutEmailIdentityFeedbackAttributes
    putEmailIdentityFeedbackAttributes_emailForwardingEnabled,
    putEmailIdentityFeedbackAttributes_emailIdentity,
    putEmailIdentityFeedbackAttributesResponse_httpStatus,

    -- ** PutEmailIdentityMailFromAttributes
    putEmailIdentityMailFromAttributes_behaviorOnMxFailure,
    putEmailIdentityMailFromAttributes_mailFromDomain,
    putEmailIdentityMailFromAttributes_emailIdentity,
    putEmailIdentityMailFromAttributesResponse_httpStatus,

    -- ** PutSuppressedDestination
    putSuppressedDestination_emailAddress,
    putSuppressedDestination_reason,
    putSuppressedDestinationResponse_httpStatus,

    -- ** SendBulkEmail
    sendBulkEmail_configurationSetName,
    sendBulkEmail_defaultEmailTags,
    sendBulkEmail_feedbackForwardingEmailAddress,
    sendBulkEmail_feedbackForwardingEmailAddressIdentityArn,
    sendBulkEmail_fromEmailAddress,
    sendBulkEmail_fromEmailAddressIdentityArn,
    sendBulkEmail_replyToAddresses,
    sendBulkEmail_defaultContent,
    sendBulkEmail_bulkEmailEntries,
    sendBulkEmailResponse_httpStatus,
    sendBulkEmailResponse_bulkEmailEntryResults,

    -- ** SendCustomVerificationEmail
    sendCustomVerificationEmail_configurationSetName,
    sendCustomVerificationEmail_emailAddress,
    sendCustomVerificationEmail_templateName,
    sendCustomVerificationEmailResponse_messageId,
    sendCustomVerificationEmailResponse_httpStatus,

    -- ** SendEmail
    sendEmail_configurationSetName,
    sendEmail_destination,
    sendEmail_emailTags,
    sendEmail_feedbackForwardingEmailAddress,
    sendEmail_feedbackForwardingEmailAddressIdentityArn,
    sendEmail_fromEmailAddress,
    sendEmail_fromEmailAddressIdentityArn,
    sendEmail_listManagementOptions,
    sendEmail_replyToAddresses,
    sendEmail_content,
    sendEmailResponse_messageId,
    sendEmailResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TestRenderEmailTemplate
    testRenderEmailTemplate_templateName,
    testRenderEmailTemplate_templateData,
    testRenderEmailTemplateResponse_httpStatus,
    testRenderEmailTemplateResponse_renderedTemplate,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateConfigurationSetEventDestination
    updateConfigurationSetEventDestination_configurationSetName,
    updateConfigurationSetEventDestination_eventDestinationName,
    updateConfigurationSetEventDestination_eventDestination,
    updateConfigurationSetEventDestinationResponse_httpStatus,

    -- ** UpdateContact
    updateContact_attributesData,
    updateContact_topicPreferences,
    updateContact_unsubscribeAll,
    updateContact_contactListName,
    updateContact_emailAddress,
    updateContactResponse_httpStatus,

    -- ** UpdateContactList
    updateContactList_description,
    updateContactList_topics,
    updateContactList_contactListName,
    updateContactListResponse_httpStatus,

    -- ** UpdateCustomVerificationEmailTemplate
    updateCustomVerificationEmailTemplate_templateName,
    updateCustomVerificationEmailTemplate_fromEmailAddress,
    updateCustomVerificationEmailTemplate_templateSubject,
    updateCustomVerificationEmailTemplate_templateContent,
    updateCustomVerificationEmailTemplate_successRedirectionURL,
    updateCustomVerificationEmailTemplate_failureRedirectionURL,
    updateCustomVerificationEmailTemplateResponse_httpStatus,

    -- ** UpdateEmailIdentityPolicy
    updateEmailIdentityPolicy_emailIdentity,
    updateEmailIdentityPolicy_policyName,
    updateEmailIdentityPolicy_policy,
    updateEmailIdentityPolicyResponse_httpStatus,

    -- ** UpdateEmailTemplate
    updateEmailTemplate_templateName,
    updateEmailTemplate_templateContent,
    updateEmailTemplateResponse_httpStatus,

    -- * Types

    -- ** AccountDetails
    accountDetails_additionalContactEmailAddresses,
    accountDetails_contactLanguage,
    accountDetails_mailType,
    accountDetails_reviewDetails,
    accountDetails_useCaseDescription,
    accountDetails_websiteURL,

    -- ** BatchGetMetricDataQuery
    batchGetMetricDataQuery_dimensions,
    batchGetMetricDataQuery_id,
    batchGetMetricDataQuery_namespace,
    batchGetMetricDataQuery_metric,
    batchGetMetricDataQuery_startDate,
    batchGetMetricDataQuery_endDate,

    -- ** BlacklistEntry
    blacklistEntry_description,
    blacklistEntry_listingTime,
    blacklistEntry_rblName,

    -- ** Body
    body_html,
    body_text,

    -- ** BulkEmailContent
    bulkEmailContent_template,

    -- ** BulkEmailEntry
    bulkEmailEntry_replacementEmailContent,
    bulkEmailEntry_replacementTags,
    bulkEmailEntry_destination,

    -- ** BulkEmailEntryResult
    bulkEmailEntryResult_error,
    bulkEmailEntryResult_messageId,
    bulkEmailEntryResult_status,

    -- ** CloudWatchDestination
    cloudWatchDestination_dimensionConfigurations,

    -- ** CloudWatchDimensionConfiguration
    cloudWatchDimensionConfiguration_dimensionName,
    cloudWatchDimensionConfiguration_dimensionValueSource,
    cloudWatchDimensionConfiguration_defaultDimensionValue,

    -- ** Contact
    contact_emailAddress,
    contact_lastUpdatedTimestamp,
    contact_topicDefaultPreferences,
    contact_topicPreferences,
    contact_unsubscribeAll,

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
    customVerificationEmailTemplateMetadata_failureRedirectionURL,
    customVerificationEmailTemplateMetadata_fromEmailAddress,
    customVerificationEmailTemplateMetadata_successRedirectionURL,
    customVerificationEmailTemplateMetadata_templateName,
    customVerificationEmailTemplateMetadata_templateSubject,

    -- ** DailyVolume
    dailyVolume_domainIspPlacements,
    dailyVolume_startDate,
    dailyVolume_volumeStatistics,

    -- ** DashboardAttributes
    dashboardAttributes_engagementMetrics,

    -- ** DashboardOptions
    dashboardOptions_engagementMetrics,

    -- ** DedicatedIp
    dedicatedIp_poolName,
    dedicatedIp_ip,
    dedicatedIp_warmupStatus,
    dedicatedIp_warmupPercentage,

    -- ** DedicatedIpPool
    dedicatedIpPool_poolName,
    dedicatedIpPool_scalingMode,

    -- ** DeliverabilityTestReport
    deliverabilityTestReport_createDate,
    deliverabilityTestReport_deliverabilityTestStatus,
    deliverabilityTestReport_fromEmailAddress,
    deliverabilityTestReport_reportId,
    deliverabilityTestReport_reportName,
    deliverabilityTestReport_subject,

    -- ** DeliveryOptions
    deliveryOptions_sendingPoolName,
    deliveryOptions_tlsPolicy,

    -- ** Destination
    destination_bccAddresses,
    destination_ccAddresses,
    destination_toAddresses,

    -- ** DkimAttributes
    dkimAttributes_currentSigningKeyLength,
    dkimAttributes_lastKeyGenerationTimestamp,
    dkimAttributes_nextSigningKeyLength,
    dkimAttributes_signingAttributesOrigin,
    dkimAttributes_signingEnabled,
    dkimAttributes_status,
    dkimAttributes_tokens,

    -- ** DkimSigningAttributes
    dkimSigningAttributes_domainSigningPrivateKey,
    dkimSigningAttributes_domainSigningSelector,
    dkimSigningAttributes_nextSigningKeyLength,

    -- ** DomainDeliverabilityCampaign
    domainDeliverabilityCampaign_campaignId,
    domainDeliverabilityCampaign_deleteRate,
    domainDeliverabilityCampaign_esps,
    domainDeliverabilityCampaign_firstSeenDateTime,
    domainDeliverabilityCampaign_fromAddress,
    domainDeliverabilityCampaign_imageUrl,
    domainDeliverabilityCampaign_inboxCount,
    domainDeliverabilityCampaign_lastSeenDateTime,
    domainDeliverabilityCampaign_projectedVolume,
    domainDeliverabilityCampaign_readDeleteRate,
    domainDeliverabilityCampaign_readRate,
    domainDeliverabilityCampaign_sendingIps,
    domainDeliverabilityCampaign_spamCount,
    domainDeliverabilityCampaign_subject,

    -- ** DomainDeliverabilityTrackingOption
    domainDeliverabilityTrackingOption_domain,
    domainDeliverabilityTrackingOption_inboxPlacementTrackingOption,
    domainDeliverabilityTrackingOption_subscriptionStartDate,

    -- ** DomainIspPlacement
    domainIspPlacement_inboxPercentage,
    domainIspPlacement_inboxRawCount,
    domainIspPlacement_ispName,
    domainIspPlacement_spamPercentage,
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
    emailTemplateMetadata_createdTimestamp,
    emailTemplateMetadata_templateName,

    -- ** EventDestination
    eventDestination_cloudWatchDestination,
    eventDestination_enabled,
    eventDestination_kinesisFirehoseDestination,
    eventDestination_pinpointDestination,
    eventDestination_snsDestination,
    eventDestination_name,
    eventDestination_matchingEventTypes,

    -- ** EventDestinationDefinition
    eventDestinationDefinition_cloudWatchDestination,
    eventDestinationDefinition_enabled,
    eventDestinationDefinition_kinesisFirehoseDestination,
    eventDestinationDefinition_matchingEventTypes,
    eventDestinationDefinition_pinpointDestination,
    eventDestinationDefinition_snsDestination,

    -- ** FailureInfo
    failureInfo_errorMessage,
    failureInfo_failedRecordsS3Url,

    -- ** GuardianAttributes
    guardianAttributes_optimizedSharedDelivery,

    -- ** GuardianOptions
    guardianOptions_optimizedSharedDelivery,

    -- ** IdentityInfo
    identityInfo_identityName,
    identityInfo_identityType,
    identityInfo_sendingEnabled,
    identityInfo_verificationStatus,

    -- ** ImportDataSource
    importDataSource_s3Url,
    importDataSource_dataFormat,

    -- ** ImportDestination
    importDestination_contactListDestination,
    importDestination_suppressionListDestination,

    -- ** ImportJobSummary
    importJobSummary_createdTimestamp,
    importJobSummary_failedRecordsCount,
    importJobSummary_importDestination,
    importJobSummary_jobId,
    importJobSummary_jobStatus,
    importJobSummary_processedRecordsCount,

    -- ** InboxPlacementTrackingOption
    inboxPlacementTrackingOption_global,
    inboxPlacementTrackingOption_trackedIsps,

    -- ** IspPlacement
    ispPlacement_ispName,
    ispPlacement_placementStatistics,

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

    -- ** MetricDataError
    metricDataError_code,
    metricDataError_id,
    metricDataError_message,

    -- ** MetricDataResult
    metricDataResult_id,
    metricDataResult_timestamps,
    metricDataResult_values,

    -- ** OverallVolume
    overallVolume_domainIspPlacements,
    overallVolume_readRatePercent,
    overallVolume_volumeStatistics,

    -- ** PinpointDestination
    pinpointDestination_applicationArn,

    -- ** PlacementStatistics
    placementStatistics_dkimPercentage,
    placementStatistics_inboxPercentage,
    placementStatistics_missingPercentage,
    placementStatistics_spamPercentage,
    placementStatistics_spfPercentage,

    -- ** RawMessage
    rawMessage_data,

    -- ** Recommendation
    recommendation_createdTimestamp,
    recommendation_description,
    recommendation_impact,
    recommendation_lastUpdatedTimestamp,
    recommendation_resourceArn,
    recommendation_status,
    recommendation_type,

    -- ** ReplacementEmailContent
    replacementEmailContent_replacementTemplate,

    -- ** ReplacementTemplate
    replacementTemplate_replacementTemplateData,

    -- ** ReputationOptions
    reputationOptions_lastFreshStart,
    reputationOptions_reputationMetricsEnabled,

    -- ** ReviewDetails
    reviewDetails_caseId,
    reviewDetails_status,

    -- ** SendQuota
    sendQuota_max24HourSend,
    sendQuota_maxSendRate,
    sendQuota_sentLast24Hours,

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
    template_templateArn,
    template_templateData,
    template_templateName,

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

    -- ** VdmAttributes
    vdmAttributes_dashboardAttributes,
    vdmAttributes_guardianAttributes,
    vdmAttributes_vdmEnabled,

    -- ** VdmOptions
    vdmOptions_dashboardOptions,
    vdmOptions_guardianOptions,

    -- ** VolumeStatistics
    volumeStatistics_inboxRawCount,
    volumeStatistics_projectedInbox,
    volumeStatistics_projectedSpam,
    volumeStatistics_spamRawCount,
  )
where

import Amazonka.SESV2.BatchGetMetricData
import Amazonka.SESV2.CreateConfigurationSet
import Amazonka.SESV2.CreateConfigurationSetEventDestination
import Amazonka.SESV2.CreateContact
import Amazonka.SESV2.CreateContactList
import Amazonka.SESV2.CreateCustomVerificationEmailTemplate
import Amazonka.SESV2.CreateDedicatedIpPool
import Amazonka.SESV2.CreateDeliverabilityTestReport
import Amazonka.SESV2.CreateEmailIdentity
import Amazonka.SESV2.CreateEmailIdentityPolicy
import Amazonka.SESV2.CreateEmailTemplate
import Amazonka.SESV2.CreateImportJob
import Amazonka.SESV2.DeleteConfigurationSet
import Amazonka.SESV2.DeleteConfigurationSetEventDestination
import Amazonka.SESV2.DeleteContact
import Amazonka.SESV2.DeleteContactList
import Amazonka.SESV2.DeleteCustomVerificationEmailTemplate
import Amazonka.SESV2.DeleteDedicatedIpPool
import Amazonka.SESV2.DeleteEmailIdentity
import Amazonka.SESV2.DeleteEmailIdentityPolicy
import Amazonka.SESV2.DeleteEmailTemplate
import Amazonka.SESV2.DeleteSuppressedDestination
import Amazonka.SESV2.GetAccount
import Amazonka.SESV2.GetBlacklistReports
import Amazonka.SESV2.GetConfigurationSet
import Amazonka.SESV2.GetConfigurationSetEventDestinations
import Amazonka.SESV2.GetContact
import Amazonka.SESV2.GetContactList
import Amazonka.SESV2.GetCustomVerificationEmailTemplate
import Amazonka.SESV2.GetDedicatedIp
import Amazonka.SESV2.GetDedicatedIpPool
import Amazonka.SESV2.GetDedicatedIps
import Amazonka.SESV2.GetDeliverabilityDashboardOptions
import Amazonka.SESV2.GetDeliverabilityTestReport
import Amazonka.SESV2.GetDomainDeliverabilityCampaign
import Amazonka.SESV2.GetDomainStatisticsReport
import Amazonka.SESV2.GetEmailIdentity
import Amazonka.SESV2.GetEmailIdentityPolicies
import Amazonka.SESV2.GetEmailTemplate
import Amazonka.SESV2.GetImportJob
import Amazonka.SESV2.GetSuppressedDestination
import Amazonka.SESV2.ListConfigurationSets
import Amazonka.SESV2.ListContactLists
import Amazonka.SESV2.ListContacts
import Amazonka.SESV2.ListCustomVerificationEmailTemplates
import Amazonka.SESV2.ListDedicatedIpPools
import Amazonka.SESV2.ListDeliverabilityTestReports
import Amazonka.SESV2.ListDomainDeliverabilityCampaigns
import Amazonka.SESV2.ListEmailIdentities
import Amazonka.SESV2.ListEmailTemplates
import Amazonka.SESV2.ListImportJobs
import Amazonka.SESV2.ListRecommendations
import Amazonka.SESV2.ListSuppressedDestinations
import Amazonka.SESV2.ListTagsForResource
import Amazonka.SESV2.PutAccountDedicatedIpWarmupAttributes
import Amazonka.SESV2.PutAccountDetails
import Amazonka.SESV2.PutAccountSendingAttributes
import Amazonka.SESV2.PutAccountSuppressionAttributes
import Amazonka.SESV2.PutAccountVdmAttributes
import Amazonka.SESV2.PutConfigurationSetDeliveryOptions
import Amazonka.SESV2.PutConfigurationSetReputationOptions
import Amazonka.SESV2.PutConfigurationSetSendingOptions
import Amazonka.SESV2.PutConfigurationSetSuppressionOptions
import Amazonka.SESV2.PutConfigurationSetTrackingOptions
import Amazonka.SESV2.PutConfigurationSetVdmOptions
import Amazonka.SESV2.PutDedicatedIpInPool
import Amazonka.SESV2.PutDedicatedIpWarmupAttributes
import Amazonka.SESV2.PutDeliverabilityDashboardOption
import Amazonka.SESV2.PutEmailIdentityConfigurationSetAttributes
import Amazonka.SESV2.PutEmailIdentityDkimAttributes
import Amazonka.SESV2.PutEmailIdentityDkimSigningAttributes
import Amazonka.SESV2.PutEmailIdentityFeedbackAttributes
import Amazonka.SESV2.PutEmailIdentityMailFromAttributes
import Amazonka.SESV2.PutSuppressedDestination
import Amazonka.SESV2.SendBulkEmail
import Amazonka.SESV2.SendCustomVerificationEmail
import Amazonka.SESV2.SendEmail
import Amazonka.SESV2.TagResource
import Amazonka.SESV2.TestRenderEmailTemplate
import Amazonka.SESV2.Types.AccountDetails
import Amazonka.SESV2.Types.BatchGetMetricDataQuery
import Amazonka.SESV2.Types.BlacklistEntry
import Amazonka.SESV2.Types.Body
import Amazonka.SESV2.Types.BulkEmailContent
import Amazonka.SESV2.Types.BulkEmailEntry
import Amazonka.SESV2.Types.BulkEmailEntryResult
import Amazonka.SESV2.Types.CloudWatchDestination
import Amazonka.SESV2.Types.CloudWatchDimensionConfiguration
import Amazonka.SESV2.Types.Contact
import Amazonka.SESV2.Types.ContactList
import Amazonka.SESV2.Types.ContactListDestination
import Amazonka.SESV2.Types.Content
import Amazonka.SESV2.Types.CustomVerificationEmailTemplateMetadata
import Amazonka.SESV2.Types.DailyVolume
import Amazonka.SESV2.Types.DashboardAttributes
import Amazonka.SESV2.Types.DashboardOptions
import Amazonka.SESV2.Types.DedicatedIp
import Amazonka.SESV2.Types.DedicatedIpPool
import Amazonka.SESV2.Types.DeliverabilityTestReport
import Amazonka.SESV2.Types.DeliveryOptions
import Amazonka.SESV2.Types.Destination
import Amazonka.SESV2.Types.DkimAttributes
import Amazonka.SESV2.Types.DkimSigningAttributes
import Amazonka.SESV2.Types.DomainDeliverabilityCampaign
import Amazonka.SESV2.Types.DomainDeliverabilityTrackingOption
import Amazonka.SESV2.Types.DomainIspPlacement
import Amazonka.SESV2.Types.EmailContent
import Amazonka.SESV2.Types.EmailTemplateContent
import Amazonka.SESV2.Types.EmailTemplateMetadata
import Amazonka.SESV2.Types.EventDestination
import Amazonka.SESV2.Types.EventDestinationDefinition
import Amazonka.SESV2.Types.FailureInfo
import Amazonka.SESV2.Types.GuardianAttributes
import Amazonka.SESV2.Types.GuardianOptions
import Amazonka.SESV2.Types.IdentityInfo
import Amazonka.SESV2.Types.ImportDataSource
import Amazonka.SESV2.Types.ImportDestination
import Amazonka.SESV2.Types.ImportJobSummary
import Amazonka.SESV2.Types.InboxPlacementTrackingOption
import Amazonka.SESV2.Types.IspPlacement
import Amazonka.SESV2.Types.KinesisFirehoseDestination
import Amazonka.SESV2.Types.ListContactsFilter
import Amazonka.SESV2.Types.ListManagementOptions
import Amazonka.SESV2.Types.MailFromAttributes
import Amazonka.SESV2.Types.Message
import Amazonka.SESV2.Types.MessageTag
import Amazonka.SESV2.Types.MetricDataError
import Amazonka.SESV2.Types.MetricDataResult
import Amazonka.SESV2.Types.OverallVolume
import Amazonka.SESV2.Types.PinpointDestination
import Amazonka.SESV2.Types.PlacementStatistics
import Amazonka.SESV2.Types.RawMessage
import Amazonka.SESV2.Types.Recommendation
import Amazonka.SESV2.Types.ReplacementEmailContent
import Amazonka.SESV2.Types.ReplacementTemplate
import Amazonka.SESV2.Types.ReputationOptions
import Amazonka.SESV2.Types.ReviewDetails
import Amazonka.SESV2.Types.SendQuota
import Amazonka.SESV2.Types.SendingOptions
import Amazonka.SESV2.Types.SnsDestination
import Amazonka.SESV2.Types.SuppressedDestination
import Amazonka.SESV2.Types.SuppressedDestinationAttributes
import Amazonka.SESV2.Types.SuppressedDestinationSummary
import Amazonka.SESV2.Types.SuppressionAttributes
import Amazonka.SESV2.Types.SuppressionListDestination
import Amazonka.SESV2.Types.SuppressionOptions
import Amazonka.SESV2.Types.Tag
import Amazonka.SESV2.Types.Template
import Amazonka.SESV2.Types.Topic
import Amazonka.SESV2.Types.TopicFilter
import Amazonka.SESV2.Types.TopicPreference
import Amazonka.SESV2.Types.TrackingOptions
import Amazonka.SESV2.Types.VdmAttributes
import Amazonka.SESV2.Types.VdmOptions
import Amazonka.SESV2.Types.VolumeStatistics
import Amazonka.SESV2.UntagResource
import Amazonka.SESV2.UpdateConfigurationSetEventDestination
import Amazonka.SESV2.UpdateContact
import Amazonka.SESV2.UpdateContactList
import Amazonka.SESV2.UpdateCustomVerificationEmailTemplate
import Amazonka.SESV2.UpdateEmailIdentityPolicy
import Amazonka.SESV2.UpdateEmailTemplate
