{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CustomerProfiles.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Lens
  ( -- * Operations

    -- ** AddProfileKey
    addProfileKey_profileId,
    addProfileKey_keyName,
    addProfileKey_values,
    addProfileKey_domainName,
    addProfileKeyResponse_keyName,
    addProfileKeyResponse_values,
    addProfileKeyResponse_httpStatus,

    -- ** CreateCalculatedAttributeDefinition
    createCalculatedAttributeDefinition_conditions,
    createCalculatedAttributeDefinition_description,
    createCalculatedAttributeDefinition_displayName,
    createCalculatedAttributeDefinition_tags,
    createCalculatedAttributeDefinition_domainName,
    createCalculatedAttributeDefinition_calculatedAttributeName,
    createCalculatedAttributeDefinition_attributeDetails,
    createCalculatedAttributeDefinition_statistic,
    createCalculatedAttributeDefinitionResponse_attributeDetails,
    createCalculatedAttributeDefinitionResponse_calculatedAttributeName,
    createCalculatedAttributeDefinitionResponse_conditions,
    createCalculatedAttributeDefinitionResponse_createdAt,
    createCalculatedAttributeDefinitionResponse_description,
    createCalculatedAttributeDefinitionResponse_displayName,
    createCalculatedAttributeDefinitionResponse_lastUpdatedAt,
    createCalculatedAttributeDefinitionResponse_statistic,
    createCalculatedAttributeDefinitionResponse_tags,
    createCalculatedAttributeDefinitionResponse_httpStatus,

    -- ** CreateDomain
    createDomain_deadLetterQueueUrl,
    createDomain_defaultEncryptionKey,
    createDomain_matching,
    createDomain_tags,
    createDomain_domainName,
    createDomain_defaultExpirationDays,
    createDomainResponse_deadLetterQueueUrl,
    createDomainResponse_defaultEncryptionKey,
    createDomainResponse_matching,
    createDomainResponse_tags,
    createDomainResponse_httpStatus,
    createDomainResponse_domainName,
    createDomainResponse_defaultExpirationDays,
    createDomainResponse_createdAt,
    createDomainResponse_lastUpdatedAt,

    -- ** CreateEventStream
    createEventStream_tags,
    createEventStream_domainName,
    createEventStream_uri,
    createEventStream_eventStreamName,
    createEventStreamResponse_tags,
    createEventStreamResponse_httpStatus,
    createEventStreamResponse_eventStreamArn,

    -- ** CreateIntegrationWorkflow
    createIntegrationWorkflow_tags,
    createIntegrationWorkflow_domainName,
    createIntegrationWorkflow_workflowType,
    createIntegrationWorkflow_integrationConfig,
    createIntegrationWorkflow_objectTypeName,
    createIntegrationWorkflow_roleArn,
    createIntegrationWorkflowResponse_httpStatus,
    createIntegrationWorkflowResponse_workflowId,
    createIntegrationWorkflowResponse_message,

    -- ** CreateProfile
    createProfile_accountNumber,
    createProfile_additionalInformation,
    createProfile_address,
    createProfile_attributes,
    createProfile_billingAddress,
    createProfile_birthDate,
    createProfile_businessEmailAddress,
    createProfile_businessName,
    createProfile_businessPhoneNumber,
    createProfile_emailAddress,
    createProfile_firstName,
    createProfile_gender,
    createProfile_genderString,
    createProfile_homePhoneNumber,
    createProfile_lastName,
    createProfile_mailingAddress,
    createProfile_middleName,
    createProfile_mobilePhoneNumber,
    createProfile_partyType,
    createProfile_partyTypeString,
    createProfile_personalEmailAddress,
    createProfile_phoneNumber,
    createProfile_shippingAddress,
    createProfile_domainName,
    createProfileResponse_httpStatus,
    createProfileResponse_profileId,

    -- ** DeleteCalculatedAttributeDefinition
    deleteCalculatedAttributeDefinition_domainName,
    deleteCalculatedAttributeDefinition_calculatedAttributeName,
    deleteCalculatedAttributeDefinitionResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_domainName,
    deleteDomainResponse_httpStatus,
    deleteDomainResponse_message,

    -- ** DeleteEventStream
    deleteEventStream_domainName,
    deleteEventStream_eventStreamName,
    deleteEventStreamResponse_httpStatus,

    -- ** DeleteIntegration
    deleteIntegration_domainName,
    deleteIntegration_uri,
    deleteIntegrationResponse_httpStatus,
    deleteIntegrationResponse_message,

    -- ** DeleteProfile
    deleteProfile_profileId,
    deleteProfile_domainName,
    deleteProfileResponse_message,
    deleteProfileResponse_httpStatus,

    -- ** DeleteProfileKey
    deleteProfileKey_profileId,
    deleteProfileKey_keyName,
    deleteProfileKey_values,
    deleteProfileKey_domainName,
    deleteProfileKeyResponse_message,
    deleteProfileKeyResponse_httpStatus,

    -- ** DeleteProfileObject
    deleteProfileObject_profileId,
    deleteProfileObject_profileObjectUniqueKey,
    deleteProfileObject_objectTypeName,
    deleteProfileObject_domainName,
    deleteProfileObjectResponse_message,
    deleteProfileObjectResponse_httpStatus,

    -- ** DeleteProfileObjectType
    deleteProfileObjectType_domainName,
    deleteProfileObjectType_objectTypeName,
    deleteProfileObjectTypeResponse_httpStatus,
    deleteProfileObjectTypeResponse_message,

    -- ** DeleteWorkflow
    deleteWorkflow_domainName,
    deleteWorkflow_workflowId,
    deleteWorkflowResponse_httpStatus,

    -- ** GetAutoMergingPreview
    getAutoMergingPreview_minAllowedConfidenceScoreForMerging,
    getAutoMergingPreview_domainName,
    getAutoMergingPreview_consolidation,
    getAutoMergingPreview_conflictResolution,
    getAutoMergingPreviewResponse_numberOfMatchesInSample,
    getAutoMergingPreviewResponse_numberOfProfilesInSample,
    getAutoMergingPreviewResponse_numberOfProfilesWillBeMerged,
    getAutoMergingPreviewResponse_httpStatus,
    getAutoMergingPreviewResponse_domainName,

    -- ** GetCalculatedAttributeDefinition
    getCalculatedAttributeDefinition_domainName,
    getCalculatedAttributeDefinition_calculatedAttributeName,
    getCalculatedAttributeDefinitionResponse_attributeDetails,
    getCalculatedAttributeDefinitionResponse_calculatedAttributeName,
    getCalculatedAttributeDefinitionResponse_conditions,
    getCalculatedAttributeDefinitionResponse_createdAt,
    getCalculatedAttributeDefinitionResponse_description,
    getCalculatedAttributeDefinitionResponse_displayName,
    getCalculatedAttributeDefinitionResponse_lastUpdatedAt,
    getCalculatedAttributeDefinitionResponse_statistic,
    getCalculatedAttributeDefinitionResponse_tags,
    getCalculatedAttributeDefinitionResponse_httpStatus,

    -- ** GetCalculatedAttributeForProfile
    getCalculatedAttributeForProfile_domainName,
    getCalculatedAttributeForProfile_profileId,
    getCalculatedAttributeForProfile_calculatedAttributeName,
    getCalculatedAttributeForProfileResponse_calculatedAttributeName,
    getCalculatedAttributeForProfileResponse_displayName,
    getCalculatedAttributeForProfileResponse_isDataPartial,
    getCalculatedAttributeForProfileResponse_value,
    getCalculatedAttributeForProfileResponse_httpStatus,

    -- ** GetDomain
    getDomain_domainName,
    getDomainResponse_deadLetterQueueUrl,
    getDomainResponse_defaultEncryptionKey,
    getDomainResponse_defaultExpirationDays,
    getDomainResponse_matching,
    getDomainResponse_stats,
    getDomainResponse_tags,
    getDomainResponse_httpStatus,
    getDomainResponse_domainName,
    getDomainResponse_createdAt,
    getDomainResponse_lastUpdatedAt,

    -- ** GetEventStream
    getEventStream_domainName,
    getEventStream_eventStreamName,
    getEventStreamResponse_stoppedSince,
    getEventStreamResponse_tags,
    getEventStreamResponse_httpStatus,
    getEventStreamResponse_domainName,
    getEventStreamResponse_eventStreamArn,
    getEventStreamResponse_createdAt,
    getEventStreamResponse_state,
    getEventStreamResponse_destinationDetails,

    -- ** GetIdentityResolutionJob
    getIdentityResolutionJob_domainName,
    getIdentityResolutionJob_jobId,
    getIdentityResolutionJobResponse_autoMerging,
    getIdentityResolutionJobResponse_domainName,
    getIdentityResolutionJobResponse_exportingLocation,
    getIdentityResolutionJobResponse_jobEndTime,
    getIdentityResolutionJobResponse_jobExpirationTime,
    getIdentityResolutionJobResponse_jobId,
    getIdentityResolutionJobResponse_jobStartTime,
    getIdentityResolutionJobResponse_jobStats,
    getIdentityResolutionJobResponse_lastUpdatedAt,
    getIdentityResolutionJobResponse_message,
    getIdentityResolutionJobResponse_status,
    getIdentityResolutionJobResponse_httpStatus,

    -- ** GetIntegration
    getIntegration_domainName,
    getIntegration_uri,
    getIntegrationResponse_isUnstructured,
    getIntegrationResponse_objectTypeName,
    getIntegrationResponse_objectTypeNames,
    getIntegrationResponse_tags,
    getIntegrationResponse_workflowId,
    getIntegrationResponse_httpStatus,
    getIntegrationResponse_domainName,
    getIntegrationResponse_uri,
    getIntegrationResponse_createdAt,
    getIntegrationResponse_lastUpdatedAt,

    -- ** GetMatches
    getMatches_maxResults,
    getMatches_nextToken,
    getMatches_domainName,
    getMatchesResponse_matchGenerationDate,
    getMatchesResponse_matches,
    getMatchesResponse_nextToken,
    getMatchesResponse_potentialMatches,
    getMatchesResponse_httpStatus,

    -- ** GetProfileObjectType
    getProfileObjectType_domainName,
    getProfileObjectType_objectTypeName,
    getProfileObjectTypeResponse_allowProfileCreation,
    getProfileObjectTypeResponse_createdAt,
    getProfileObjectTypeResponse_encryptionKey,
    getProfileObjectTypeResponse_expirationDays,
    getProfileObjectTypeResponse_fields,
    getProfileObjectTypeResponse_keys,
    getProfileObjectTypeResponse_lastUpdatedAt,
    getProfileObjectTypeResponse_sourceLastUpdatedTimestampFormat,
    getProfileObjectTypeResponse_tags,
    getProfileObjectTypeResponse_templateId,
    getProfileObjectTypeResponse_httpStatus,
    getProfileObjectTypeResponse_objectTypeName,
    getProfileObjectTypeResponse_description,

    -- ** GetProfileObjectTypeTemplate
    getProfileObjectTypeTemplate_templateId,
    getProfileObjectTypeTemplateResponse_allowProfileCreation,
    getProfileObjectTypeTemplateResponse_fields,
    getProfileObjectTypeTemplateResponse_keys,
    getProfileObjectTypeTemplateResponse_sourceLastUpdatedTimestampFormat,
    getProfileObjectTypeTemplateResponse_sourceName,
    getProfileObjectTypeTemplateResponse_sourceObject,
    getProfileObjectTypeTemplateResponse_templateId,
    getProfileObjectTypeTemplateResponse_httpStatus,

    -- ** GetWorkflow
    getWorkflow_domainName,
    getWorkflow_workflowId,
    getWorkflowResponse_attributes,
    getWorkflowResponse_errorDescription,
    getWorkflowResponse_lastUpdatedAt,
    getWorkflowResponse_metrics,
    getWorkflowResponse_startDate,
    getWorkflowResponse_status,
    getWorkflowResponse_workflowId,
    getWorkflowResponse_workflowType,
    getWorkflowResponse_httpStatus,

    -- ** GetWorkflowSteps
    getWorkflowSteps_maxResults,
    getWorkflowSteps_nextToken,
    getWorkflowSteps_domainName,
    getWorkflowSteps_workflowId,
    getWorkflowStepsResponse_items,
    getWorkflowStepsResponse_nextToken,
    getWorkflowStepsResponse_workflowId,
    getWorkflowStepsResponse_workflowType,
    getWorkflowStepsResponse_httpStatus,

    -- ** ListAccountIntegrations
    listAccountIntegrations_includeHidden,
    listAccountIntegrations_maxResults,
    listAccountIntegrations_nextToken,
    listAccountIntegrations_uri,
    listAccountIntegrationsResponse_items,
    listAccountIntegrationsResponse_nextToken,
    listAccountIntegrationsResponse_httpStatus,

    -- ** ListCalculatedAttributeDefinitions
    listCalculatedAttributeDefinitions_maxResults,
    listCalculatedAttributeDefinitions_nextToken,
    listCalculatedAttributeDefinitions_domainName,
    listCalculatedAttributeDefinitionsResponse_items,
    listCalculatedAttributeDefinitionsResponse_nextToken,
    listCalculatedAttributeDefinitionsResponse_httpStatus,

    -- ** ListCalculatedAttributesForProfile
    listCalculatedAttributesForProfile_maxResults,
    listCalculatedAttributesForProfile_nextToken,
    listCalculatedAttributesForProfile_domainName,
    listCalculatedAttributesForProfile_profileId,
    listCalculatedAttributesForProfileResponse_items,
    listCalculatedAttributesForProfileResponse_nextToken,
    listCalculatedAttributesForProfileResponse_httpStatus,

    -- ** ListDomains
    listDomains_maxResults,
    listDomains_nextToken,
    listDomainsResponse_items,
    listDomainsResponse_nextToken,
    listDomainsResponse_httpStatus,

    -- ** ListEventStreams
    listEventStreams_maxResults,
    listEventStreams_nextToken,
    listEventStreams_domainName,
    listEventStreamsResponse_items,
    listEventStreamsResponse_nextToken,
    listEventStreamsResponse_httpStatus,

    -- ** ListIdentityResolutionJobs
    listIdentityResolutionJobs_maxResults,
    listIdentityResolutionJobs_nextToken,
    listIdentityResolutionJobs_domainName,
    listIdentityResolutionJobsResponse_identityResolutionJobsList,
    listIdentityResolutionJobsResponse_nextToken,
    listIdentityResolutionJobsResponse_httpStatus,

    -- ** ListIntegrations
    listIntegrations_includeHidden,
    listIntegrations_maxResults,
    listIntegrations_nextToken,
    listIntegrations_domainName,
    listIntegrationsResponse_items,
    listIntegrationsResponse_nextToken,
    listIntegrationsResponse_httpStatus,

    -- ** ListProfileObjectTypeTemplates
    listProfileObjectTypeTemplates_maxResults,
    listProfileObjectTypeTemplates_nextToken,
    listProfileObjectTypeTemplatesResponse_items,
    listProfileObjectTypeTemplatesResponse_nextToken,
    listProfileObjectTypeTemplatesResponse_httpStatus,

    -- ** ListProfileObjectTypes
    listProfileObjectTypes_maxResults,
    listProfileObjectTypes_nextToken,
    listProfileObjectTypes_domainName,
    listProfileObjectTypesResponse_items,
    listProfileObjectTypesResponse_nextToken,
    listProfileObjectTypesResponse_httpStatus,

    -- ** ListProfileObjects
    listProfileObjects_maxResults,
    listProfileObjects_nextToken,
    listProfileObjects_objectFilter,
    listProfileObjects_domainName,
    listProfileObjects_objectTypeName,
    listProfileObjects_profileId,
    listProfileObjectsResponse_items,
    listProfileObjectsResponse_nextToken,
    listProfileObjectsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWorkflows
    listWorkflows_maxResults,
    listWorkflows_nextToken,
    listWorkflows_queryEndDate,
    listWorkflows_queryStartDate,
    listWorkflows_status,
    listWorkflows_workflowType,
    listWorkflows_domainName,
    listWorkflowsResponse_items,
    listWorkflowsResponse_nextToken,
    listWorkflowsResponse_httpStatus,

    -- ** MergeProfiles
    mergeProfiles_fieldSourceProfileIds,
    mergeProfiles_domainName,
    mergeProfiles_mainProfileId,
    mergeProfiles_profileIdsToBeMerged,
    mergeProfilesResponse_message,
    mergeProfilesResponse_httpStatus,

    -- ** PutIntegration
    putIntegration_flowDefinition,
    putIntegration_objectTypeName,
    putIntegration_objectTypeNames,
    putIntegration_tags,
    putIntegration_uri,
    putIntegration_domainName,
    putIntegrationResponse_isUnstructured,
    putIntegrationResponse_objectTypeName,
    putIntegrationResponse_objectTypeNames,
    putIntegrationResponse_tags,
    putIntegrationResponse_workflowId,
    putIntegrationResponse_httpStatus,
    putIntegrationResponse_domainName,
    putIntegrationResponse_uri,
    putIntegrationResponse_createdAt,
    putIntegrationResponse_lastUpdatedAt,

    -- ** PutProfileObject
    putProfileObject_objectTypeName,
    putProfileObject_object,
    putProfileObject_domainName,
    putProfileObjectResponse_profileObjectUniqueKey,
    putProfileObjectResponse_httpStatus,

    -- ** PutProfileObjectType
    putProfileObjectType_allowProfileCreation,
    putProfileObjectType_encryptionKey,
    putProfileObjectType_expirationDays,
    putProfileObjectType_fields,
    putProfileObjectType_keys,
    putProfileObjectType_sourceLastUpdatedTimestampFormat,
    putProfileObjectType_tags,
    putProfileObjectType_templateId,
    putProfileObjectType_domainName,
    putProfileObjectType_objectTypeName,
    putProfileObjectType_description,
    putProfileObjectTypeResponse_allowProfileCreation,
    putProfileObjectTypeResponse_createdAt,
    putProfileObjectTypeResponse_encryptionKey,
    putProfileObjectTypeResponse_expirationDays,
    putProfileObjectTypeResponse_fields,
    putProfileObjectTypeResponse_keys,
    putProfileObjectTypeResponse_lastUpdatedAt,
    putProfileObjectTypeResponse_sourceLastUpdatedTimestampFormat,
    putProfileObjectTypeResponse_tags,
    putProfileObjectTypeResponse_templateId,
    putProfileObjectTypeResponse_httpStatus,
    putProfileObjectTypeResponse_objectTypeName,
    putProfileObjectTypeResponse_description,

    -- ** SearchProfiles
    searchProfiles_additionalSearchKeys,
    searchProfiles_logicalOperator,
    searchProfiles_maxResults,
    searchProfiles_nextToken,
    searchProfiles_domainName,
    searchProfiles_keyName,
    searchProfiles_values,
    searchProfilesResponse_items,
    searchProfilesResponse_nextToken,
    searchProfilesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateCalculatedAttributeDefinition
    updateCalculatedAttributeDefinition_conditions,
    updateCalculatedAttributeDefinition_description,
    updateCalculatedAttributeDefinition_displayName,
    updateCalculatedAttributeDefinition_domainName,
    updateCalculatedAttributeDefinition_calculatedAttributeName,
    updateCalculatedAttributeDefinitionResponse_attributeDetails,
    updateCalculatedAttributeDefinitionResponse_calculatedAttributeName,
    updateCalculatedAttributeDefinitionResponse_conditions,
    updateCalculatedAttributeDefinitionResponse_createdAt,
    updateCalculatedAttributeDefinitionResponse_description,
    updateCalculatedAttributeDefinitionResponse_displayName,
    updateCalculatedAttributeDefinitionResponse_lastUpdatedAt,
    updateCalculatedAttributeDefinitionResponse_statistic,
    updateCalculatedAttributeDefinitionResponse_tags,
    updateCalculatedAttributeDefinitionResponse_httpStatus,

    -- ** UpdateDomain
    updateDomain_deadLetterQueueUrl,
    updateDomain_defaultEncryptionKey,
    updateDomain_defaultExpirationDays,
    updateDomain_matching,
    updateDomain_tags,
    updateDomain_domainName,
    updateDomainResponse_deadLetterQueueUrl,
    updateDomainResponse_defaultEncryptionKey,
    updateDomainResponse_defaultExpirationDays,
    updateDomainResponse_matching,
    updateDomainResponse_tags,
    updateDomainResponse_httpStatus,
    updateDomainResponse_domainName,
    updateDomainResponse_createdAt,
    updateDomainResponse_lastUpdatedAt,

    -- ** UpdateProfile
    updateProfile_accountNumber,
    updateProfile_additionalInformation,
    updateProfile_address,
    updateProfile_attributes,
    updateProfile_billingAddress,
    updateProfile_birthDate,
    updateProfile_businessEmailAddress,
    updateProfile_businessName,
    updateProfile_businessPhoneNumber,
    updateProfile_emailAddress,
    updateProfile_firstName,
    updateProfile_gender,
    updateProfile_genderString,
    updateProfile_homePhoneNumber,
    updateProfile_lastName,
    updateProfile_mailingAddress,
    updateProfile_middleName,
    updateProfile_mobilePhoneNumber,
    updateProfile_partyType,
    updateProfile_partyTypeString,
    updateProfile_personalEmailAddress,
    updateProfile_phoneNumber,
    updateProfile_shippingAddress,
    updateProfile_domainName,
    updateProfile_profileId,
    updateProfileResponse_httpStatus,
    updateProfileResponse_profileId,

    -- * Types

    -- ** AdditionalSearchKey
    additionalSearchKey_keyName,
    additionalSearchKey_values,

    -- ** Address
    address_address1,
    address_address2,
    address_address3,
    address_address4,
    address_city,
    address_country,
    address_county,
    address_postalCode,
    address_province,
    address_state,

    -- ** AppflowIntegration
    appflowIntegration_batches,
    appflowIntegration_flowDefinition,

    -- ** AppflowIntegrationWorkflowAttributes
    appflowIntegrationWorkflowAttributes_roleArn,
    appflowIntegrationWorkflowAttributes_sourceConnectorType,
    appflowIntegrationWorkflowAttributes_connectorProfileName,

    -- ** AppflowIntegrationWorkflowMetrics
    appflowIntegrationWorkflowMetrics_recordsProcessed,
    appflowIntegrationWorkflowMetrics_stepsCompleted,
    appflowIntegrationWorkflowMetrics_totalSteps,

    -- ** AppflowIntegrationWorkflowStep
    appflowIntegrationWorkflowStep_flowName,
    appflowIntegrationWorkflowStep_status,
    appflowIntegrationWorkflowStep_executionMessage,
    appflowIntegrationWorkflowStep_recordsProcessed,
    appflowIntegrationWorkflowStep_batchRecordsStartTime,
    appflowIntegrationWorkflowStep_batchRecordsEndTime,
    appflowIntegrationWorkflowStep_createdAt,
    appflowIntegrationWorkflowStep_lastUpdatedAt,

    -- ** AttributeDetails
    attributeDetails_attributes,
    attributeDetails_expression,

    -- ** AttributeItem
    attributeItem_name,

    -- ** AutoMerging
    autoMerging_conflictResolution,
    autoMerging_consolidation,
    autoMerging_minAllowedConfidenceScoreForMerging,
    autoMerging_enabled,

    -- ** Batch
    batch_startTime,
    batch_endTime,

    -- ** Conditions
    conditions_objectCount,
    conditions_range,
    conditions_threshold,

    -- ** ConflictResolution
    conflictResolution_sourceName,
    conflictResolution_conflictResolvingModel,

    -- ** ConnectorOperator
    connectorOperator_marketo,
    connectorOperator_s3,
    connectorOperator_salesforce,
    connectorOperator_serviceNow,
    connectorOperator_zendesk,

    -- ** Consolidation
    consolidation_matchingAttributesList,

    -- ** DestinationSummary
    destinationSummary_unhealthySince,
    destinationSummary_uri,
    destinationSummary_status,

    -- ** DomainStats
    domainStats_meteringProfileCount,
    domainStats_objectCount,
    domainStats_profileCount,
    domainStats_totalSize,

    -- ** EventStreamDestinationDetails
    eventStreamDestinationDetails_message,
    eventStreamDestinationDetails_unhealthySince,
    eventStreamDestinationDetails_uri,
    eventStreamDestinationDetails_status,

    -- ** EventStreamSummary
    eventStreamSummary_destinationSummary,
    eventStreamSummary_stoppedSince,
    eventStreamSummary_tags,
    eventStreamSummary_domainName,
    eventStreamSummary_eventStreamName,
    eventStreamSummary_eventStreamArn,
    eventStreamSummary_state,

    -- ** ExportingConfig
    exportingConfig_s3Exporting,

    -- ** ExportingLocation
    exportingLocation_s3Exporting,

    -- ** FieldSourceProfileIds
    fieldSourceProfileIds_accountNumber,
    fieldSourceProfileIds_additionalInformation,
    fieldSourceProfileIds_address,
    fieldSourceProfileIds_attributes,
    fieldSourceProfileIds_billingAddress,
    fieldSourceProfileIds_birthDate,
    fieldSourceProfileIds_businessEmailAddress,
    fieldSourceProfileIds_businessName,
    fieldSourceProfileIds_businessPhoneNumber,
    fieldSourceProfileIds_emailAddress,
    fieldSourceProfileIds_firstName,
    fieldSourceProfileIds_gender,
    fieldSourceProfileIds_homePhoneNumber,
    fieldSourceProfileIds_lastName,
    fieldSourceProfileIds_mailingAddress,
    fieldSourceProfileIds_middleName,
    fieldSourceProfileIds_mobilePhoneNumber,
    fieldSourceProfileIds_partyType,
    fieldSourceProfileIds_personalEmailAddress,
    fieldSourceProfileIds_phoneNumber,
    fieldSourceProfileIds_shippingAddress,

    -- ** FlowDefinition
    flowDefinition_description,
    flowDefinition_flowName,
    flowDefinition_kmsArn,
    flowDefinition_sourceFlowConfig,
    flowDefinition_tasks,
    flowDefinition_triggerConfig,

    -- ** FoundByKeyValue
    foundByKeyValue_keyName,
    foundByKeyValue_values,

    -- ** IdentityResolutionJob
    identityResolutionJob_domainName,
    identityResolutionJob_exportingLocation,
    identityResolutionJob_jobEndTime,
    identityResolutionJob_jobId,
    identityResolutionJob_jobStartTime,
    identityResolutionJob_jobStats,
    identityResolutionJob_message,
    identityResolutionJob_status,

    -- ** IncrementalPullConfig
    incrementalPullConfig_datetimeTypeFieldName,

    -- ** IntegrationConfig
    integrationConfig_appflowIntegration,

    -- ** JobSchedule
    jobSchedule_dayOfTheWeek,
    jobSchedule_time,

    -- ** JobStats
    jobStats_numberOfMatchesFound,
    jobStats_numberOfMergesDone,
    jobStats_numberOfProfilesReviewed,

    -- ** ListCalculatedAttributeDefinitionItem
    listCalculatedAttributeDefinitionItem_calculatedAttributeName,
    listCalculatedAttributeDefinitionItem_createdAt,
    listCalculatedAttributeDefinitionItem_description,
    listCalculatedAttributeDefinitionItem_displayName,
    listCalculatedAttributeDefinitionItem_lastUpdatedAt,
    listCalculatedAttributeDefinitionItem_tags,

    -- ** ListCalculatedAttributeForProfileItem
    listCalculatedAttributeForProfileItem_calculatedAttributeName,
    listCalculatedAttributeForProfileItem_displayName,
    listCalculatedAttributeForProfileItem_isDataPartial,
    listCalculatedAttributeForProfileItem_value,

    -- ** ListDomainItem
    listDomainItem_tags,
    listDomainItem_domainName,
    listDomainItem_createdAt,
    listDomainItem_lastUpdatedAt,

    -- ** ListIntegrationItem
    listIntegrationItem_isUnstructured,
    listIntegrationItem_objectTypeName,
    listIntegrationItem_objectTypeNames,
    listIntegrationItem_tags,
    listIntegrationItem_workflowId,
    listIntegrationItem_domainName,
    listIntegrationItem_uri,
    listIntegrationItem_createdAt,
    listIntegrationItem_lastUpdatedAt,

    -- ** ListProfileObjectTypeItem
    listProfileObjectTypeItem_createdAt,
    listProfileObjectTypeItem_lastUpdatedAt,
    listProfileObjectTypeItem_tags,
    listProfileObjectTypeItem_objectTypeName,
    listProfileObjectTypeItem_description,

    -- ** ListProfileObjectTypeTemplateItem
    listProfileObjectTypeTemplateItem_sourceName,
    listProfileObjectTypeTemplateItem_sourceObject,
    listProfileObjectTypeTemplateItem_templateId,

    -- ** ListProfileObjectsItem
    listProfileObjectsItem_object,
    listProfileObjectsItem_objectTypeName,
    listProfileObjectsItem_profileObjectUniqueKey,

    -- ** ListWorkflowsItem
    listWorkflowsItem_workflowType,
    listWorkflowsItem_workflowId,
    listWorkflowsItem_status,
    listWorkflowsItem_statusDescription,
    listWorkflowsItem_createdAt,
    listWorkflowsItem_lastUpdatedAt,

    -- ** MarketoSourceProperties
    marketoSourceProperties_object,

    -- ** MatchItem
    matchItem_confidenceScore,
    matchItem_matchId,
    matchItem_profileIds,

    -- ** MatchingRequest
    matchingRequest_autoMerging,
    matchingRequest_exportingConfig,
    matchingRequest_jobSchedule,
    matchingRequest_enabled,

    -- ** MatchingResponse
    matchingResponse_autoMerging,
    matchingResponse_enabled,
    matchingResponse_exportingConfig,
    matchingResponse_jobSchedule,

    -- ** ObjectFilter
    objectFilter_keyName,
    objectFilter_values,

    -- ** ObjectTypeField
    objectTypeField_contentType,
    objectTypeField_source,
    objectTypeField_target,

    -- ** ObjectTypeKey
    objectTypeKey_fieldNames,
    objectTypeKey_standardIdentifiers,

    -- ** Profile
    profile_accountNumber,
    profile_additionalInformation,
    profile_address,
    profile_attributes,
    profile_billingAddress,
    profile_birthDate,
    profile_businessEmailAddress,
    profile_businessName,
    profile_businessPhoneNumber,
    profile_emailAddress,
    profile_firstName,
    profile_foundByItems,
    profile_gender,
    profile_genderString,
    profile_homePhoneNumber,
    profile_lastName,
    profile_mailingAddress,
    profile_middleName,
    profile_mobilePhoneNumber,
    profile_partyType,
    profile_partyTypeString,
    profile_personalEmailAddress,
    profile_phoneNumber,
    profile_profileId,
    profile_shippingAddress,

    -- ** Range
    range_value,
    range_unit,

    -- ** S3ExportingConfig
    s3ExportingConfig_s3KeyName,
    s3ExportingConfig_s3BucketName,

    -- ** S3ExportingLocation
    s3ExportingLocation_s3BucketName,
    s3ExportingLocation_s3KeyName,

    -- ** S3SourceProperties
    s3SourceProperties_bucketPrefix,
    s3SourceProperties_bucketName,

    -- ** SalesforceSourceProperties
    salesforceSourceProperties_enableDynamicFieldUpdate,
    salesforceSourceProperties_includeDeletedRecords,
    salesforceSourceProperties_object,

    -- ** ScheduledTriggerProperties
    scheduledTriggerProperties_dataPullMode,
    scheduledTriggerProperties_firstExecutionFrom,
    scheduledTriggerProperties_scheduleEndTime,
    scheduledTriggerProperties_scheduleOffset,
    scheduledTriggerProperties_scheduleStartTime,
    scheduledTriggerProperties_timezone,
    scheduledTriggerProperties_scheduleExpression,

    -- ** ServiceNowSourceProperties
    serviceNowSourceProperties_object,

    -- ** SourceConnectorProperties
    sourceConnectorProperties_marketo,
    sourceConnectorProperties_s3,
    sourceConnectorProperties_salesforce,
    sourceConnectorProperties_serviceNow,
    sourceConnectorProperties_zendesk,

    -- ** SourceFlowConfig
    sourceFlowConfig_connectorProfileName,
    sourceFlowConfig_incrementalPullConfig,
    sourceFlowConfig_connectorType,
    sourceFlowConfig_sourceConnectorProperties,

    -- ** Task
    task_connectorOperator,
    task_destinationField,
    task_taskProperties,
    task_sourceFields,
    task_taskType,

    -- ** Threshold
    threshold_value,
    threshold_operator,

    -- ** TriggerConfig
    triggerConfig_triggerProperties,
    triggerConfig_triggerType,

    -- ** TriggerProperties
    triggerProperties_scheduled,

    -- ** UpdateAddress
    updateAddress_address1,
    updateAddress_address2,
    updateAddress_address3,
    updateAddress_address4,
    updateAddress_city,
    updateAddress_country,
    updateAddress_county,
    updateAddress_postalCode,
    updateAddress_province,
    updateAddress_state,

    -- ** WorkflowAttributes
    workflowAttributes_appflowIntegration,

    -- ** WorkflowMetrics
    workflowMetrics_appflowIntegration,

    -- ** WorkflowStepItem
    workflowStepItem_appflowIntegration,

    -- ** ZendeskSourceProperties
    zendeskSourceProperties_object,
  )
where

import Amazonka.CustomerProfiles.AddProfileKey
import Amazonka.CustomerProfiles.CreateCalculatedAttributeDefinition
import Amazonka.CustomerProfiles.CreateDomain
import Amazonka.CustomerProfiles.CreateEventStream
import Amazonka.CustomerProfiles.CreateIntegrationWorkflow
import Amazonka.CustomerProfiles.CreateProfile
import Amazonka.CustomerProfiles.DeleteCalculatedAttributeDefinition
import Amazonka.CustomerProfiles.DeleteDomain
import Amazonka.CustomerProfiles.DeleteEventStream
import Amazonka.CustomerProfiles.DeleteIntegration
import Amazonka.CustomerProfiles.DeleteProfile
import Amazonka.CustomerProfiles.DeleteProfileKey
import Amazonka.CustomerProfiles.DeleteProfileObject
import Amazonka.CustomerProfiles.DeleteProfileObjectType
import Amazonka.CustomerProfiles.DeleteWorkflow
import Amazonka.CustomerProfiles.GetAutoMergingPreview
import Amazonka.CustomerProfiles.GetCalculatedAttributeDefinition
import Amazonka.CustomerProfiles.GetCalculatedAttributeForProfile
import Amazonka.CustomerProfiles.GetDomain
import Amazonka.CustomerProfiles.GetEventStream
import Amazonka.CustomerProfiles.GetIdentityResolutionJob
import Amazonka.CustomerProfiles.GetIntegration
import Amazonka.CustomerProfiles.GetMatches
import Amazonka.CustomerProfiles.GetProfileObjectType
import Amazonka.CustomerProfiles.GetProfileObjectTypeTemplate
import Amazonka.CustomerProfiles.GetWorkflow
import Amazonka.CustomerProfiles.GetWorkflowSteps
import Amazonka.CustomerProfiles.ListAccountIntegrations
import Amazonka.CustomerProfiles.ListCalculatedAttributeDefinitions
import Amazonka.CustomerProfiles.ListCalculatedAttributesForProfile
import Amazonka.CustomerProfiles.ListDomains
import Amazonka.CustomerProfiles.ListEventStreams
import Amazonka.CustomerProfiles.ListIdentityResolutionJobs
import Amazonka.CustomerProfiles.ListIntegrations
import Amazonka.CustomerProfiles.ListProfileObjectTypeTemplates
import Amazonka.CustomerProfiles.ListProfileObjectTypes
import Amazonka.CustomerProfiles.ListProfileObjects
import Amazonka.CustomerProfiles.ListTagsForResource
import Amazonka.CustomerProfiles.ListWorkflows
import Amazonka.CustomerProfiles.MergeProfiles
import Amazonka.CustomerProfiles.PutIntegration
import Amazonka.CustomerProfiles.PutProfileObject
import Amazonka.CustomerProfiles.PutProfileObjectType
import Amazonka.CustomerProfiles.SearchProfiles
import Amazonka.CustomerProfiles.TagResource
import Amazonka.CustomerProfiles.Types.AdditionalSearchKey
import Amazonka.CustomerProfiles.Types.Address
import Amazonka.CustomerProfiles.Types.AppflowIntegration
import Amazonka.CustomerProfiles.Types.AppflowIntegrationWorkflowAttributes
import Amazonka.CustomerProfiles.Types.AppflowIntegrationWorkflowMetrics
import Amazonka.CustomerProfiles.Types.AppflowIntegrationWorkflowStep
import Amazonka.CustomerProfiles.Types.AttributeDetails
import Amazonka.CustomerProfiles.Types.AttributeItem
import Amazonka.CustomerProfiles.Types.AutoMerging
import Amazonka.CustomerProfiles.Types.Batch
import Amazonka.CustomerProfiles.Types.Conditions
import Amazonka.CustomerProfiles.Types.ConflictResolution
import Amazonka.CustomerProfiles.Types.ConnectorOperator
import Amazonka.CustomerProfiles.Types.Consolidation
import Amazonka.CustomerProfiles.Types.DestinationSummary
import Amazonka.CustomerProfiles.Types.DomainStats
import Amazonka.CustomerProfiles.Types.EventStreamDestinationDetails
import Amazonka.CustomerProfiles.Types.EventStreamSummary
import Amazonka.CustomerProfiles.Types.ExportingConfig
import Amazonka.CustomerProfiles.Types.ExportingLocation
import Amazonka.CustomerProfiles.Types.FieldSourceProfileIds
import Amazonka.CustomerProfiles.Types.FlowDefinition
import Amazonka.CustomerProfiles.Types.FoundByKeyValue
import Amazonka.CustomerProfiles.Types.IdentityResolutionJob
import Amazonka.CustomerProfiles.Types.IncrementalPullConfig
import Amazonka.CustomerProfiles.Types.IntegrationConfig
import Amazonka.CustomerProfiles.Types.JobSchedule
import Amazonka.CustomerProfiles.Types.JobStats
import Amazonka.CustomerProfiles.Types.ListCalculatedAttributeDefinitionItem
import Amazonka.CustomerProfiles.Types.ListCalculatedAttributeForProfileItem
import Amazonka.CustomerProfiles.Types.ListDomainItem
import Amazonka.CustomerProfiles.Types.ListIntegrationItem
import Amazonka.CustomerProfiles.Types.ListProfileObjectTypeItem
import Amazonka.CustomerProfiles.Types.ListProfileObjectTypeTemplateItem
import Amazonka.CustomerProfiles.Types.ListProfileObjectsItem
import Amazonka.CustomerProfiles.Types.ListWorkflowsItem
import Amazonka.CustomerProfiles.Types.MarketoSourceProperties
import Amazonka.CustomerProfiles.Types.MatchItem
import Amazonka.CustomerProfiles.Types.MatchingRequest
import Amazonka.CustomerProfiles.Types.MatchingResponse
import Amazonka.CustomerProfiles.Types.ObjectFilter
import Amazonka.CustomerProfiles.Types.ObjectTypeField
import Amazonka.CustomerProfiles.Types.ObjectTypeKey
import Amazonka.CustomerProfiles.Types.Profile
import Amazonka.CustomerProfiles.Types.Range
import Amazonka.CustomerProfiles.Types.S3ExportingConfig
import Amazonka.CustomerProfiles.Types.S3ExportingLocation
import Amazonka.CustomerProfiles.Types.S3SourceProperties
import Amazonka.CustomerProfiles.Types.SalesforceSourceProperties
import Amazonka.CustomerProfiles.Types.ScheduledTriggerProperties
import Amazonka.CustomerProfiles.Types.ServiceNowSourceProperties
import Amazonka.CustomerProfiles.Types.SourceConnectorProperties
import Amazonka.CustomerProfiles.Types.SourceFlowConfig
import Amazonka.CustomerProfiles.Types.Task
import Amazonka.CustomerProfiles.Types.Threshold
import Amazonka.CustomerProfiles.Types.TriggerConfig
import Amazonka.CustomerProfiles.Types.TriggerProperties
import Amazonka.CustomerProfiles.Types.UpdateAddress
import Amazonka.CustomerProfiles.Types.WorkflowAttributes
import Amazonka.CustomerProfiles.Types.WorkflowMetrics
import Amazonka.CustomerProfiles.Types.WorkflowStepItem
import Amazonka.CustomerProfiles.Types.ZendeskSourceProperties
import Amazonka.CustomerProfiles.UntagResource
import Amazonka.CustomerProfiles.UpdateCalculatedAttributeDefinition
import Amazonka.CustomerProfiles.UpdateDomain
import Amazonka.CustomerProfiles.UpdateProfile
