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

    -- ** DeleteProfileObjectType
    deleteProfileObjectType_domainName,
    deleteProfileObjectType_objectTypeName,
    deleteProfileObjectTypeResponse_httpStatus,
    deleteProfileObjectTypeResponse_message,

    -- ** ListIntegrations
    listIntegrations_nextToken,
    listIntegrations_maxResults,
    listIntegrations_domainName,
    listIntegrationsResponse_items,
    listIntegrationsResponse_nextToken,
    listIntegrationsResponse_httpStatus,

    -- ** PutProfileObjectType
    putProfileObjectType_expirationDays,
    putProfileObjectType_templateId,
    putProfileObjectType_keys,
    putProfileObjectType_encryptionKey,
    putProfileObjectType_allowProfileCreation,
    putProfileObjectType_fields,
    putProfileObjectType_tags,
    putProfileObjectType_domainName,
    putProfileObjectType_objectTypeName,
    putProfileObjectType_description,
    putProfileObjectTypeResponse_expirationDays,
    putProfileObjectTypeResponse_lastUpdatedAt,
    putProfileObjectTypeResponse_createdAt,
    putProfileObjectTypeResponse_templateId,
    putProfileObjectTypeResponse_keys,
    putProfileObjectTypeResponse_encryptionKey,
    putProfileObjectTypeResponse_allowProfileCreation,
    putProfileObjectTypeResponse_fields,
    putProfileObjectTypeResponse_tags,
    putProfileObjectTypeResponse_httpStatus,
    putProfileObjectTypeResponse_objectTypeName,
    putProfileObjectTypeResponse_description,

    -- ** ListProfileObjects
    listProfileObjects_objectFilter,
    listProfileObjects_nextToken,
    listProfileObjects_maxResults,
    listProfileObjects_domainName,
    listProfileObjects_objectTypeName,
    listProfileObjects_profileId,
    listProfileObjectsResponse_items,
    listProfileObjectsResponse_nextToken,
    listProfileObjectsResponse_httpStatus,

    -- ** ListProfileObjectTypeTemplates
    listProfileObjectTypeTemplates_nextToken,
    listProfileObjectTypeTemplates_maxResults,
    listProfileObjectTypeTemplatesResponse_items,
    listProfileObjectTypeTemplatesResponse_nextToken,
    listProfileObjectTypeTemplatesResponse_httpStatus,

    -- ** DeleteProfile
    deleteProfile_profileId,
    deleteProfile_domainName,
    deleteProfileResponse_message,
    deleteProfileResponse_httpStatus,

    -- ** UpdateProfile
    updateProfile_shippingAddress,
    updateProfile_mobilePhoneNumber,
    updateProfile_mailingAddress,
    updateProfile_middleName,
    updateProfile_personalEmailAddress,
    updateProfile_lastName,
    updateProfile_additionalInformation,
    updateProfile_homePhoneNumber,
    updateProfile_address,
    updateProfile_partyType,
    updateProfile_businessEmailAddress,
    updateProfile_attributes,
    updateProfile_gender,
    updateProfile_phoneNumber,
    updateProfile_accountNumber,
    updateProfile_emailAddress,
    updateProfile_firstName,
    updateProfile_billingAddress,
    updateProfile_businessPhoneNumber,
    updateProfile_birthDate,
    updateProfile_businessName,
    updateProfile_domainName,
    updateProfile_profileId,
    updateProfileResponse_httpStatus,
    updateProfileResponse_profileId,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** AddProfileKey
    addProfileKey_profileId,
    addProfileKey_keyName,
    addProfileKey_values,
    addProfileKey_domainName,
    addProfileKeyResponse_values,
    addProfileKeyResponse_keyName,
    addProfileKeyResponse_httpStatus,

    -- ** GetProfileObjectTypeTemplate
    getProfileObjectTypeTemplate_templateId,
    getProfileObjectTypeTemplateResponse_sourceName,
    getProfileObjectTypeTemplateResponse_templateId,
    getProfileObjectTypeTemplateResponse_sourceObject,
    getProfileObjectTypeTemplateResponse_keys,
    getProfileObjectTypeTemplateResponse_allowProfileCreation,
    getProfileObjectTypeTemplateResponse_fields,
    getProfileObjectTypeTemplateResponse_httpStatus,

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

    -- ** GetDomain
    getDomain_domainName,
    getDomainResponse_defaultExpirationDays,
    getDomainResponse_defaultEncryptionKey,
    getDomainResponse_matching,
    getDomainResponse_stats,
    getDomainResponse_deadLetterQueueUrl,
    getDomainResponse_tags,
    getDomainResponse_httpStatus,
    getDomainResponse_domainName,
    getDomainResponse_createdAt,
    getDomainResponse_lastUpdatedAt,

    -- ** CreateDomain
    createDomain_defaultEncryptionKey,
    createDomain_matching,
    createDomain_deadLetterQueueUrl,
    createDomain_tags,
    createDomain_domainName,
    createDomain_defaultExpirationDays,
    createDomainResponse_defaultEncryptionKey,
    createDomainResponse_matching,
    createDomainResponse_deadLetterQueueUrl,
    createDomainResponse_tags,
    createDomainResponse_httpStatus,
    createDomainResponse_domainName,
    createDomainResponse_defaultExpirationDays,
    createDomainResponse_createdAt,
    createDomainResponse_lastUpdatedAt,

    -- ** DeleteIntegration
    deleteIntegration_domainName,
    deleteIntegration_uri,
    deleteIntegrationResponse_httpStatus,
    deleteIntegrationResponse_message,

    -- ** CreateProfile
    createProfile_shippingAddress,
    createProfile_mobilePhoneNumber,
    createProfile_mailingAddress,
    createProfile_middleName,
    createProfile_personalEmailAddress,
    createProfile_lastName,
    createProfile_additionalInformation,
    createProfile_homePhoneNumber,
    createProfile_address,
    createProfile_partyType,
    createProfile_businessEmailAddress,
    createProfile_attributes,
    createProfile_gender,
    createProfile_phoneNumber,
    createProfile_accountNumber,
    createProfile_emailAddress,
    createProfile_firstName,
    createProfile_billingAddress,
    createProfile_businessPhoneNumber,
    createProfile_birthDate,
    createProfile_businessName,
    createProfile_domainName,
    createProfileResponse_httpStatus,
    createProfileResponse_profileId,

    -- ** PutProfileObject
    putProfileObject_objectTypeName,
    putProfileObject_object,
    putProfileObject_domainName,
    putProfileObjectResponse_profileObjectUniqueKey,
    putProfileObjectResponse_httpStatus,

    -- ** PutIntegration
    putIntegration_flowDefinition,
    putIntegration_uri,
    putIntegration_tags,
    putIntegration_domainName,
    putIntegration_objectTypeName,
    putIntegrationResponse_tags,
    putIntegrationResponse_httpStatus,
    putIntegrationResponse_domainName,
    putIntegrationResponse_uri,
    putIntegrationResponse_objectTypeName,
    putIntegrationResponse_createdAt,
    putIntegrationResponse_lastUpdatedAt,

    -- ** DeleteProfileObject
    deleteProfileObject_profileId,
    deleteProfileObject_profileObjectUniqueKey,
    deleteProfileObject_objectTypeName,
    deleteProfileObject_domainName,
    deleteProfileObjectResponse_message,
    deleteProfileObjectResponse_httpStatus,

    -- ** ListProfileObjectTypes
    listProfileObjectTypes_nextToken,
    listProfileObjectTypes_maxResults,
    listProfileObjectTypes_domainName,
    listProfileObjectTypesResponse_items,
    listProfileObjectTypesResponse_nextToken,
    listProfileObjectTypesResponse_httpStatus,

    -- ** DeleteProfileKey
    deleteProfileKey_profileId,
    deleteProfileKey_keyName,
    deleteProfileKey_values,
    deleteProfileKey_domainName,
    deleteProfileKeyResponse_message,
    deleteProfileKeyResponse_httpStatus,

    -- ** GetProfileObjectType
    getProfileObjectType_domainName,
    getProfileObjectType_objectTypeName,
    getProfileObjectTypeResponse_expirationDays,
    getProfileObjectTypeResponse_lastUpdatedAt,
    getProfileObjectTypeResponse_createdAt,
    getProfileObjectTypeResponse_templateId,
    getProfileObjectTypeResponse_keys,
    getProfileObjectTypeResponse_encryptionKey,
    getProfileObjectTypeResponse_allowProfileCreation,
    getProfileObjectTypeResponse_fields,
    getProfileObjectTypeResponse_tags,
    getProfileObjectTypeResponse_httpStatus,
    getProfileObjectTypeResponse_objectTypeName,
    getProfileObjectTypeResponse_description,

    -- ** MergeProfiles
    mergeProfiles_fieldSourceProfileIds,
    mergeProfiles_domainName,
    mergeProfiles_mainProfileId,
    mergeProfiles_profileIdsToBeMerged,
    mergeProfilesResponse_message,
    mergeProfilesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetMatches
    getMatches_nextToken,
    getMatches_maxResults,
    getMatches_domainName,
    getMatchesResponse_matches,
    getMatchesResponse_matchGenerationDate,
    getMatchesResponse_nextToken,
    getMatchesResponse_potentialMatches,
    getMatchesResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** SearchProfiles
    searchProfiles_nextToken,
    searchProfiles_maxResults,
    searchProfiles_domainName,
    searchProfiles_keyName,
    searchProfiles_values,
    searchProfilesResponse_items,
    searchProfilesResponse_nextToken,
    searchProfilesResponse_httpStatus,

    -- ** ListAccountIntegrations
    listAccountIntegrations_nextToken,
    listAccountIntegrations_maxResults,
    listAccountIntegrations_uri,
    listAccountIntegrationsResponse_items,
    listAccountIntegrationsResponse_nextToken,
    listAccountIntegrationsResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_domainName,
    deleteDomainResponse_httpStatus,
    deleteDomainResponse_message,

    -- ** UpdateDomain
    updateDomain_defaultExpirationDays,
    updateDomain_defaultEncryptionKey,
    updateDomain_matching,
    updateDomain_deadLetterQueueUrl,
    updateDomain_tags,
    updateDomain_domainName,
    updateDomainResponse_defaultExpirationDays,
    updateDomainResponse_defaultEncryptionKey,
    updateDomainResponse_matching,
    updateDomainResponse_deadLetterQueueUrl,
    updateDomainResponse_tags,
    updateDomainResponse_httpStatus,
    updateDomainResponse_domainName,
    updateDomainResponse_createdAt,
    updateDomainResponse_lastUpdatedAt,

    -- ** ListDomains
    listDomains_nextToken,
    listDomains_maxResults,
    listDomainsResponse_items,
    listDomainsResponse_nextToken,
    listDomainsResponse_httpStatus,

    -- * Types

    -- ** Address
    address_address2,
    address_state,
    address_address4,
    address_county,
    address_address3,
    address_postalCode,
    address_country,
    address_city,
    address_address1,
    address_province,

    -- ** ConnectorOperator
    connectorOperator_serviceNow,
    connectorOperator_marketo,
    connectorOperator_salesforce,
    connectorOperator_zendesk,
    connectorOperator_s3,

    -- ** DomainStats
    domainStats_meteringProfileCount,
    domainStats_totalSize,
    domainStats_profileCount,
    domainStats_objectCount,

    -- ** FieldSourceProfileIds
    fieldSourceProfileIds_shippingAddress,
    fieldSourceProfileIds_mobilePhoneNumber,
    fieldSourceProfileIds_mailingAddress,
    fieldSourceProfileIds_middleName,
    fieldSourceProfileIds_personalEmailAddress,
    fieldSourceProfileIds_lastName,
    fieldSourceProfileIds_additionalInformation,
    fieldSourceProfileIds_homePhoneNumber,
    fieldSourceProfileIds_address,
    fieldSourceProfileIds_partyType,
    fieldSourceProfileIds_businessEmailAddress,
    fieldSourceProfileIds_attributes,
    fieldSourceProfileIds_gender,
    fieldSourceProfileIds_phoneNumber,
    fieldSourceProfileIds_accountNumber,
    fieldSourceProfileIds_emailAddress,
    fieldSourceProfileIds_firstName,
    fieldSourceProfileIds_billingAddress,
    fieldSourceProfileIds_businessPhoneNumber,
    fieldSourceProfileIds_birthDate,
    fieldSourceProfileIds_businessName,

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
    listProfileObjectTypeItem_lastUpdatedAt,
    listProfileObjectTypeItem_createdAt,
    listProfileObjectTypeItem_tags,
    listProfileObjectTypeItem_objectTypeName,
    listProfileObjectTypeItem_description,

    -- ** ListProfileObjectTypeTemplateItem
    listProfileObjectTypeTemplateItem_sourceName,
    listProfileObjectTypeTemplateItem_templateId,
    listProfileObjectTypeTemplateItem_sourceObject,

    -- ** ListProfileObjectsItem
    listProfileObjectsItem_profileObjectUniqueKey,
    listProfileObjectsItem_objectTypeName,
    listProfileObjectsItem_object,

    -- ** MarketoSourceProperties
    marketoSourceProperties_object,

    -- ** MatchItem
    matchItem_profileIds,
    matchItem_matchId,

    -- ** MatchingRequest
    matchingRequest_enabled,

    -- ** MatchingResponse
    matchingResponse_enabled,

    -- ** ObjectFilter
    objectFilter_keyName,
    objectFilter_values,

    -- ** ObjectTypeField
    objectTypeField_source,
    objectTypeField_contentType,
    objectTypeField_target,

    -- ** ObjectTypeKey
    objectTypeKey_fieldNames,
    objectTypeKey_standardIdentifiers,

    -- ** Profile
    profile_shippingAddress,
    profile_mobilePhoneNumber,
    profile_mailingAddress,
    profile_middleName,
    profile_personalEmailAddress,
    profile_lastName,
    profile_additionalInformation,
    profile_homePhoneNumber,
    profile_address,
    profile_partyType,
    profile_profileId,
    profile_businessEmailAddress,
    profile_attributes,
    profile_gender,
    profile_phoneNumber,
    profile_accountNumber,
    profile_emailAddress,
    profile_firstName,
    profile_billingAddress,
    profile_businessPhoneNumber,
    profile_birthDate,
    profile_businessName,

    -- ** S3SourceProperties
    s3SourceProperties_bucketPrefix,
    s3SourceProperties_bucketName,

    -- ** SalesforceSourceProperties
    salesforceSourceProperties_enableDynamicFieldUpdate,
    salesforceSourceProperties_includeDeletedRecords,
    salesforceSourceProperties_object,

    -- ** ScheduledTriggerProperties
    scheduledTriggerProperties_scheduleEndTime,
    scheduledTriggerProperties_scheduleOffset,
    scheduledTriggerProperties_dataPullMode,
    scheduledTriggerProperties_scheduleStartTime,
    scheduledTriggerProperties_timezone,
    scheduledTriggerProperties_firstExecutionFrom,
    scheduledTriggerProperties_scheduleExpression,

    -- ** ServiceNowSourceProperties
    serviceNowSourceProperties_object,

    -- ** SourceConnectorProperties
    sourceConnectorProperties_serviceNow,
    sourceConnectorProperties_marketo,
    sourceConnectorProperties_salesforce,
    sourceConnectorProperties_zendesk,
    sourceConnectorProperties_s3,

    -- ** SourceFlowConfig
    sourceFlowConfig_connectorProfileName,
    sourceFlowConfig_incrementalPullConfig,
    sourceFlowConfig_connectorType,
    sourceFlowConfig_sourceConnectorProperties,

    -- ** Task
    task_taskProperties,
    task_connectorOperator,
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
    updateAddress_state,
    updateAddress_address4,
    updateAddress_county,
    updateAddress_address3,
    updateAddress_postalCode,
    updateAddress_country,
    updateAddress_city,
    updateAddress_address1,
    updateAddress_province,

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
