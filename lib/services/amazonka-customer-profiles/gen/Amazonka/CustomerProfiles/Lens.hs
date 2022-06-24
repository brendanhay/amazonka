{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CustomerProfiles.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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

    -- ** CreateDomain
    createDomain_tags,
    createDomain_matching,
    createDomain_deadLetterQueueUrl,
    createDomain_defaultEncryptionKey,
    createDomain_domainName,
    createDomain_defaultExpirationDays,
    createDomainResponse_tags,
    createDomainResponse_matching,
    createDomainResponse_deadLetterQueueUrl,
    createDomainResponse_defaultEncryptionKey,
    createDomainResponse_httpStatus,
    createDomainResponse_domainName,
    createDomainResponse_defaultExpirationDays,
    createDomainResponse_createdAt,
    createDomainResponse_lastUpdatedAt,

    -- ** CreateProfile
    createProfile_homePhoneNumber,
    createProfile_mailingAddress,
    createProfile_shippingAddress,
    createProfile_firstName,
    createProfile_businessPhoneNumber,
    createProfile_businessEmailAddress,
    createProfile_businessName,
    createProfile_personalEmailAddress,
    createProfile_billingAddress,
    createProfile_lastName,
    createProfile_birthDate,
    createProfile_address,
    createProfile_partyType,
    createProfile_gender,
    createProfile_mobilePhoneNumber,
    createProfile_middleName,
    createProfile_attributes,
    createProfile_phoneNumber,
    createProfile_additionalInformation,
    createProfile_emailAddress,
    createProfile_accountNumber,
    createProfile_domainName,
    createProfileResponse_httpStatus,
    createProfileResponse_profileId,

    -- ** DeleteDomain
    deleteDomain_domainName,
    deleteDomainResponse_httpStatus,
    deleteDomainResponse_message,

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

    -- ** GetDomain
    getDomain_domainName,
    getDomainResponse_tags,
    getDomainResponse_defaultExpirationDays,
    getDomainResponse_stats,
    getDomainResponse_matching,
    getDomainResponse_deadLetterQueueUrl,
    getDomainResponse_defaultEncryptionKey,
    getDomainResponse_httpStatus,
    getDomainResponse_domainName,
    getDomainResponse_createdAt,
    getDomainResponse_lastUpdatedAt,

    -- ** GetIntegration
    getIntegration_domainName,
    getIntegration_uri,
    getIntegrationResponse_tags,
    getIntegrationResponse_httpStatus,
    getIntegrationResponse_domainName,
    getIntegrationResponse_uri,
    getIntegrationResponse_objectTypeName,
    getIntegrationResponse_createdAt,
    getIntegrationResponse_lastUpdatedAt,

    -- ** GetMatches
    getMatches_nextToken,
    getMatches_maxResults,
    getMatches_domainName,
    getMatchesResponse_nextToken,
    getMatchesResponse_matches,
    getMatchesResponse_potentialMatches,
    getMatchesResponse_matchGenerationDate,
    getMatchesResponse_httpStatus,

    -- ** GetProfileObjectType
    getProfileObjectType_domainName,
    getProfileObjectType_objectTypeName,
    getProfileObjectTypeResponse_tags,
    getProfileObjectTypeResponse_lastUpdatedAt,
    getProfileObjectTypeResponse_fields,
    getProfileObjectTypeResponse_templateId,
    getProfileObjectTypeResponse_expirationDays,
    getProfileObjectTypeResponse_keys,
    getProfileObjectTypeResponse_encryptionKey,
    getProfileObjectTypeResponse_createdAt,
    getProfileObjectTypeResponse_allowProfileCreation,
    getProfileObjectTypeResponse_httpStatus,
    getProfileObjectTypeResponse_objectTypeName,
    getProfileObjectTypeResponse_description,

    -- ** GetProfileObjectTypeTemplate
    getProfileObjectTypeTemplate_templateId,
    getProfileObjectTypeTemplateResponse_sourceName,
    getProfileObjectTypeTemplateResponse_fields,
    getProfileObjectTypeTemplateResponse_templateId,
    getProfileObjectTypeTemplateResponse_sourceObject,
    getProfileObjectTypeTemplateResponse_keys,
    getProfileObjectTypeTemplateResponse_allowProfileCreation,
    getProfileObjectTypeTemplateResponse_httpStatus,

    -- ** ListAccountIntegrations
    listAccountIntegrations_nextToken,
    listAccountIntegrations_maxResults,
    listAccountIntegrations_uri,
    listAccountIntegrationsResponse_items,
    listAccountIntegrationsResponse_nextToken,
    listAccountIntegrationsResponse_httpStatus,

    -- ** ListDomains
    listDomains_nextToken,
    listDomains_maxResults,
    listDomainsResponse_items,
    listDomainsResponse_nextToken,
    listDomainsResponse_httpStatus,

    -- ** ListIntegrations
    listIntegrations_nextToken,
    listIntegrations_maxResults,
    listIntegrations_domainName,
    listIntegrationsResponse_items,
    listIntegrationsResponse_nextToken,
    listIntegrationsResponse_httpStatus,

    -- ** ListProfileObjectTypeTemplates
    listProfileObjectTypeTemplates_nextToken,
    listProfileObjectTypeTemplates_maxResults,
    listProfileObjectTypeTemplatesResponse_items,
    listProfileObjectTypeTemplatesResponse_nextToken,
    listProfileObjectTypeTemplatesResponse_httpStatus,

    -- ** ListProfileObjectTypes
    listProfileObjectTypes_nextToken,
    listProfileObjectTypes_maxResults,
    listProfileObjectTypes_domainName,
    listProfileObjectTypesResponse_items,
    listProfileObjectTypesResponse_nextToken,
    listProfileObjectTypesResponse_httpStatus,

    -- ** ListProfileObjects
    listProfileObjects_nextToken,
    listProfileObjects_objectFilter,
    listProfileObjects_maxResults,
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

    -- ** MergeProfiles
    mergeProfiles_fieldSourceProfileIds,
    mergeProfiles_domainName,
    mergeProfiles_mainProfileId,
    mergeProfiles_profileIdsToBeMerged,
    mergeProfilesResponse_message,
    mergeProfilesResponse_httpStatus,

    -- ** PutIntegration
    putIntegration_tags,
    putIntegration_uri,
    putIntegration_flowDefinition,
    putIntegration_domainName,
    putIntegration_objectTypeName,
    putIntegrationResponse_tags,
    putIntegrationResponse_httpStatus,
    putIntegrationResponse_domainName,
    putIntegrationResponse_uri,
    putIntegrationResponse_objectTypeName,
    putIntegrationResponse_createdAt,
    putIntegrationResponse_lastUpdatedAt,

    -- ** PutProfileObject
    putProfileObject_objectTypeName,
    putProfileObject_object,
    putProfileObject_domainName,
    putProfileObjectResponse_profileObjectUniqueKey,
    putProfileObjectResponse_httpStatus,

    -- ** PutProfileObjectType
    putProfileObjectType_tags,
    putProfileObjectType_fields,
    putProfileObjectType_templateId,
    putProfileObjectType_expirationDays,
    putProfileObjectType_keys,
    putProfileObjectType_encryptionKey,
    putProfileObjectType_allowProfileCreation,
    putProfileObjectType_domainName,
    putProfileObjectType_objectTypeName,
    putProfileObjectType_description,
    putProfileObjectTypeResponse_tags,
    putProfileObjectTypeResponse_lastUpdatedAt,
    putProfileObjectTypeResponse_fields,
    putProfileObjectTypeResponse_templateId,
    putProfileObjectTypeResponse_expirationDays,
    putProfileObjectTypeResponse_keys,
    putProfileObjectTypeResponse_encryptionKey,
    putProfileObjectTypeResponse_createdAt,
    putProfileObjectTypeResponse_allowProfileCreation,
    putProfileObjectTypeResponse_httpStatus,
    putProfileObjectTypeResponse_objectTypeName,
    putProfileObjectTypeResponse_description,

    -- ** SearchProfiles
    searchProfiles_nextToken,
    searchProfiles_maxResults,
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

    -- ** UpdateDomain
    updateDomain_tags,
    updateDomain_defaultExpirationDays,
    updateDomain_matching,
    updateDomain_deadLetterQueueUrl,
    updateDomain_defaultEncryptionKey,
    updateDomain_domainName,
    updateDomainResponse_tags,
    updateDomainResponse_defaultExpirationDays,
    updateDomainResponse_matching,
    updateDomainResponse_deadLetterQueueUrl,
    updateDomainResponse_defaultEncryptionKey,
    updateDomainResponse_httpStatus,
    updateDomainResponse_domainName,
    updateDomainResponse_createdAt,
    updateDomainResponse_lastUpdatedAt,

    -- ** UpdateProfile
    updateProfile_homePhoneNumber,
    updateProfile_mailingAddress,
    updateProfile_shippingAddress,
    updateProfile_firstName,
    updateProfile_businessPhoneNumber,
    updateProfile_businessEmailAddress,
    updateProfile_businessName,
    updateProfile_personalEmailAddress,
    updateProfile_billingAddress,
    updateProfile_lastName,
    updateProfile_birthDate,
    updateProfile_address,
    updateProfile_partyType,
    updateProfile_gender,
    updateProfile_mobilePhoneNumber,
    updateProfile_middleName,
    updateProfile_attributes,
    updateProfile_phoneNumber,
    updateProfile_additionalInformation,
    updateProfile_emailAddress,
    updateProfile_accountNumber,
    updateProfile_domainName,
    updateProfile_profileId,
    updateProfileResponse_httpStatus,
    updateProfileResponse_profileId,

    -- * Types

    -- ** Address
    address_address2,
    address_postalCode,
    address_country,
    address_county,
    address_state,
    address_province,
    address_address3,
    address_city,
    address_address4,
    address_address1,

    -- ** ConnectorOperator
    connectorOperator_zendesk,
    connectorOperator_s3,
    connectorOperator_salesforce,
    connectorOperator_marketo,
    connectorOperator_serviceNow,

    -- ** DomainStats
    domainStats_meteringProfileCount,
    domainStats_profileCount,
    domainStats_objectCount,
    domainStats_totalSize,

    -- ** FieldSourceProfileIds
    fieldSourceProfileIds_homePhoneNumber,
    fieldSourceProfileIds_mailingAddress,
    fieldSourceProfileIds_shippingAddress,
    fieldSourceProfileIds_firstName,
    fieldSourceProfileIds_businessPhoneNumber,
    fieldSourceProfileIds_businessEmailAddress,
    fieldSourceProfileIds_businessName,
    fieldSourceProfileIds_personalEmailAddress,
    fieldSourceProfileIds_billingAddress,
    fieldSourceProfileIds_lastName,
    fieldSourceProfileIds_birthDate,
    fieldSourceProfileIds_address,
    fieldSourceProfileIds_partyType,
    fieldSourceProfileIds_gender,
    fieldSourceProfileIds_mobilePhoneNumber,
    fieldSourceProfileIds_middleName,
    fieldSourceProfileIds_attributes,
    fieldSourceProfileIds_phoneNumber,
    fieldSourceProfileIds_additionalInformation,
    fieldSourceProfileIds_emailAddress,
    fieldSourceProfileIds_accountNumber,

    -- ** FlowDefinition
    flowDefinition_description,
    flowDefinition_flowName,
    flowDefinition_kmsArn,
    flowDefinition_sourceFlowConfig,
    flowDefinition_tasks,
    flowDefinition_triggerConfig,

    -- ** IncrementalPullConfig
    incrementalPullConfig_datetimeTypeFieldName,

    -- ** ListDomainItem
    listDomainItem_tags,
    listDomainItem_domainName,
    listDomainItem_createdAt,
    listDomainItem_lastUpdatedAt,

    -- ** ListIntegrationItem
    listIntegrationItem_tags,
    listIntegrationItem_domainName,
    listIntegrationItem_uri,
    listIntegrationItem_objectTypeName,
    listIntegrationItem_createdAt,
    listIntegrationItem_lastUpdatedAt,

    -- ** ListProfileObjectTypeItem
    listProfileObjectTypeItem_tags,
    listProfileObjectTypeItem_lastUpdatedAt,
    listProfileObjectTypeItem_createdAt,
    listProfileObjectTypeItem_objectTypeName,
    listProfileObjectTypeItem_description,

    -- ** ListProfileObjectTypeTemplateItem
    listProfileObjectTypeTemplateItem_sourceName,
    listProfileObjectTypeTemplateItem_templateId,
    listProfileObjectTypeTemplateItem_sourceObject,

    -- ** ListProfileObjectsItem
    listProfileObjectsItem_object,
    listProfileObjectsItem_profileObjectUniqueKey,
    listProfileObjectsItem_objectTypeName,

    -- ** MarketoSourceProperties
    marketoSourceProperties_object,

    -- ** MatchItem
    matchItem_matchId,
    matchItem_profileIds,

    -- ** MatchingRequest
    matchingRequest_enabled,

    -- ** MatchingResponse
    matchingResponse_enabled,

    -- ** ObjectFilter
    objectFilter_keyName,
    objectFilter_values,

    -- ** ObjectTypeField
    objectTypeField_target,
    objectTypeField_source,
    objectTypeField_contentType,

    -- ** ObjectTypeKey
    objectTypeKey_fieldNames,
    objectTypeKey_standardIdentifiers,

    -- ** Profile
    profile_homePhoneNumber,
    profile_mailingAddress,
    profile_shippingAddress,
    profile_profileId,
    profile_firstName,
    profile_businessPhoneNumber,
    profile_businessEmailAddress,
    profile_businessName,
    profile_personalEmailAddress,
    profile_billingAddress,
    profile_lastName,
    profile_birthDate,
    profile_address,
    profile_partyType,
    profile_gender,
    profile_mobilePhoneNumber,
    profile_middleName,
    profile_attributes,
    profile_phoneNumber,
    profile_additionalInformation,
    profile_emailAddress,
    profile_accountNumber,

    -- ** S3SourceProperties
    s3SourceProperties_bucketPrefix,
    s3SourceProperties_bucketName,

    -- ** SalesforceSourceProperties
    salesforceSourceProperties_includeDeletedRecords,
    salesforceSourceProperties_enableDynamicFieldUpdate,
    salesforceSourceProperties_object,

    -- ** ScheduledTriggerProperties
    scheduledTriggerProperties_scheduleEndTime,
    scheduledTriggerProperties_scheduleStartTime,
    scheduledTriggerProperties_timezone,
    scheduledTriggerProperties_scheduleOffset,
    scheduledTriggerProperties_firstExecutionFrom,
    scheduledTriggerProperties_dataPullMode,
    scheduledTriggerProperties_scheduleExpression,

    -- ** ServiceNowSourceProperties
    serviceNowSourceProperties_object,

    -- ** SourceConnectorProperties
    sourceConnectorProperties_zendesk,
    sourceConnectorProperties_s3,
    sourceConnectorProperties_salesforce,
    sourceConnectorProperties_marketo,
    sourceConnectorProperties_serviceNow,

    -- ** SourceFlowConfig
    sourceFlowConfig_connectorProfileName,
    sourceFlowConfig_incrementalPullConfig,
    sourceFlowConfig_connectorType,
    sourceFlowConfig_sourceConnectorProperties,

    -- ** Task
    task_connectorOperator,
    task_taskProperties,
    task_destinationField,
    task_sourceFields,
    task_taskType,

    -- ** TriggerConfig
    triggerConfig_triggerProperties,
    triggerConfig_triggerType,

    -- ** TriggerProperties
    triggerProperties_scheduled,

    -- ** UpdateAddress
    updateAddress_address2,
    updateAddress_postalCode,
    updateAddress_country,
    updateAddress_county,
    updateAddress_state,
    updateAddress_province,
    updateAddress_address3,
    updateAddress_city,
    updateAddress_address4,
    updateAddress_address1,

    -- ** ZendeskSourceProperties
    zendeskSourceProperties_object,
  )
where

import Amazonka.CustomerProfiles.AddProfileKey
import Amazonka.CustomerProfiles.CreateDomain
import Amazonka.CustomerProfiles.CreateProfile
import Amazonka.CustomerProfiles.DeleteDomain
import Amazonka.CustomerProfiles.DeleteIntegration
import Amazonka.CustomerProfiles.DeleteProfile
import Amazonka.CustomerProfiles.DeleteProfileKey
import Amazonka.CustomerProfiles.DeleteProfileObject
import Amazonka.CustomerProfiles.DeleteProfileObjectType
import Amazonka.CustomerProfiles.GetDomain
import Amazonka.CustomerProfiles.GetIntegration
import Amazonka.CustomerProfiles.GetMatches
import Amazonka.CustomerProfiles.GetProfileObjectType
import Amazonka.CustomerProfiles.GetProfileObjectTypeTemplate
import Amazonka.CustomerProfiles.ListAccountIntegrations
import Amazonka.CustomerProfiles.ListDomains
import Amazonka.CustomerProfiles.ListIntegrations
import Amazonka.CustomerProfiles.ListProfileObjectTypeTemplates
import Amazonka.CustomerProfiles.ListProfileObjectTypes
import Amazonka.CustomerProfiles.ListProfileObjects
import Amazonka.CustomerProfiles.ListTagsForResource
import Amazonka.CustomerProfiles.MergeProfiles
import Amazonka.CustomerProfiles.PutIntegration
import Amazonka.CustomerProfiles.PutProfileObject
import Amazonka.CustomerProfiles.PutProfileObjectType
import Amazonka.CustomerProfiles.SearchProfiles
import Amazonka.CustomerProfiles.TagResource
import Amazonka.CustomerProfiles.Types.Address
import Amazonka.CustomerProfiles.Types.ConnectorOperator
import Amazonka.CustomerProfiles.Types.DomainStats
import Amazonka.CustomerProfiles.Types.FieldSourceProfileIds
import Amazonka.CustomerProfiles.Types.FlowDefinition
import Amazonka.CustomerProfiles.Types.IncrementalPullConfig
import Amazonka.CustomerProfiles.Types.ListDomainItem
import Amazonka.CustomerProfiles.Types.ListIntegrationItem
import Amazonka.CustomerProfiles.Types.ListProfileObjectTypeItem
import Amazonka.CustomerProfiles.Types.ListProfileObjectTypeTemplateItem
import Amazonka.CustomerProfiles.Types.ListProfileObjectsItem
import Amazonka.CustomerProfiles.Types.MarketoSourceProperties
import Amazonka.CustomerProfiles.Types.MatchItem
import Amazonka.CustomerProfiles.Types.MatchingRequest
import Amazonka.CustomerProfiles.Types.MatchingResponse
import Amazonka.CustomerProfiles.Types.ObjectFilter
import Amazonka.CustomerProfiles.Types.ObjectTypeField
import Amazonka.CustomerProfiles.Types.ObjectTypeKey
import Amazonka.CustomerProfiles.Types.Profile
import Amazonka.CustomerProfiles.Types.S3SourceProperties
import Amazonka.CustomerProfiles.Types.SalesforceSourceProperties
import Amazonka.CustomerProfiles.Types.ScheduledTriggerProperties
import Amazonka.CustomerProfiles.Types.ServiceNowSourceProperties
import Amazonka.CustomerProfiles.Types.SourceConnectorProperties
import Amazonka.CustomerProfiles.Types.SourceFlowConfig
import Amazonka.CustomerProfiles.Types.Task
import Amazonka.CustomerProfiles.Types.TriggerConfig
import Amazonka.CustomerProfiles.Types.TriggerProperties
import Amazonka.CustomerProfiles.Types.UpdateAddress
import Amazonka.CustomerProfiles.Types.ZendeskSourceProperties
import Amazonka.CustomerProfiles.UntagResource
import Amazonka.CustomerProfiles.UpdateDomain
import Amazonka.CustomerProfiles.UpdateProfile
