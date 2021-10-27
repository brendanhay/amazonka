{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CustomerProfiles.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CustomerProfiles.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,
    _BadRequestException,

    -- * DataPullMode
    DataPullMode (..),

    -- * FieldContentType
    FieldContentType (..),

    -- * Gender
    Gender (..),

    -- * MarketoConnectorOperator
    MarketoConnectorOperator (..),

    -- * OperatorPropertiesKeys
    OperatorPropertiesKeys (..),

    -- * PartyType
    PartyType (..),

    -- * S3ConnectorOperator
    S3ConnectorOperator (..),

    -- * SalesforceConnectorOperator
    SalesforceConnectorOperator (..),

    -- * ServiceNowConnectorOperator
    ServiceNowConnectorOperator (..),

    -- * SourceConnectorType
    SourceConnectorType (..),

    -- * StandardIdentifier
    StandardIdentifier (..),

    -- * TaskType
    TaskType (..),

    -- * TriggerType
    TriggerType (..),

    -- * ZendeskConnectorOperator
    ZendeskConnectorOperator (..),

    -- * Address
    Address (..),
    newAddress,
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

    -- * ConnectorOperator
    ConnectorOperator (..),
    newConnectorOperator,
    connectorOperator_serviceNow,
    connectorOperator_marketo,
    connectorOperator_salesforce,
    connectorOperator_zendesk,
    connectorOperator_s3,

    -- * DomainStats
    DomainStats (..),
    newDomainStats,
    domainStats_meteringProfileCount,
    domainStats_totalSize,
    domainStats_profileCount,
    domainStats_objectCount,

    -- * FieldSourceProfileIds
    FieldSourceProfileIds (..),
    newFieldSourceProfileIds,
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

    -- * FlowDefinition
    FlowDefinition (..),
    newFlowDefinition,
    flowDefinition_description,
    flowDefinition_flowName,
    flowDefinition_kmsArn,
    flowDefinition_sourceFlowConfig,
    flowDefinition_tasks,
    flowDefinition_triggerConfig,

    -- * IncrementalPullConfig
    IncrementalPullConfig (..),
    newIncrementalPullConfig,
    incrementalPullConfig_datetimeTypeFieldName,

    -- * ListDomainItem
    ListDomainItem (..),
    newListDomainItem,
    listDomainItem_tags,
    listDomainItem_domainName,
    listDomainItem_createdAt,
    listDomainItem_lastUpdatedAt,

    -- * ListIntegrationItem
    ListIntegrationItem (..),
    newListIntegrationItem,
    listIntegrationItem_tags,
    listIntegrationItem_domainName,
    listIntegrationItem_uri,
    listIntegrationItem_objectTypeName,
    listIntegrationItem_createdAt,
    listIntegrationItem_lastUpdatedAt,

    -- * ListProfileObjectTypeItem
    ListProfileObjectTypeItem (..),
    newListProfileObjectTypeItem,
    listProfileObjectTypeItem_lastUpdatedAt,
    listProfileObjectTypeItem_createdAt,
    listProfileObjectTypeItem_tags,
    listProfileObjectTypeItem_objectTypeName,
    listProfileObjectTypeItem_description,

    -- * ListProfileObjectTypeTemplateItem
    ListProfileObjectTypeTemplateItem (..),
    newListProfileObjectTypeTemplateItem,
    listProfileObjectTypeTemplateItem_sourceName,
    listProfileObjectTypeTemplateItem_templateId,
    listProfileObjectTypeTemplateItem_sourceObject,

    -- * ListProfileObjectsItem
    ListProfileObjectsItem (..),
    newListProfileObjectsItem,
    listProfileObjectsItem_profileObjectUniqueKey,
    listProfileObjectsItem_objectTypeName,
    listProfileObjectsItem_object,

    -- * MarketoSourceProperties
    MarketoSourceProperties (..),
    newMarketoSourceProperties,
    marketoSourceProperties_object,

    -- * MatchItem
    MatchItem (..),
    newMatchItem,
    matchItem_profileIds,
    matchItem_matchId,

    -- * MatchingRequest
    MatchingRequest (..),
    newMatchingRequest,
    matchingRequest_enabled,

    -- * MatchingResponse
    MatchingResponse (..),
    newMatchingResponse,
    matchingResponse_enabled,

    -- * ObjectFilter
    ObjectFilter (..),
    newObjectFilter,
    objectFilter_keyName,
    objectFilter_values,

    -- * ObjectTypeField
    ObjectTypeField (..),
    newObjectTypeField,
    objectTypeField_source,
    objectTypeField_contentType,
    objectTypeField_target,

    -- * ObjectTypeKey
    ObjectTypeKey (..),
    newObjectTypeKey,
    objectTypeKey_fieldNames,
    objectTypeKey_standardIdentifiers,

    -- * Profile
    Profile (..),
    newProfile,
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

    -- * S3SourceProperties
    S3SourceProperties (..),
    newS3SourceProperties,
    s3SourceProperties_bucketPrefix,
    s3SourceProperties_bucketName,

    -- * SalesforceSourceProperties
    SalesforceSourceProperties (..),
    newSalesforceSourceProperties,
    salesforceSourceProperties_enableDynamicFieldUpdate,
    salesforceSourceProperties_includeDeletedRecords,
    salesforceSourceProperties_object,

    -- * ScheduledTriggerProperties
    ScheduledTriggerProperties (..),
    newScheduledTriggerProperties,
    scheduledTriggerProperties_scheduleEndTime,
    scheduledTriggerProperties_scheduleOffset,
    scheduledTriggerProperties_dataPullMode,
    scheduledTriggerProperties_scheduleStartTime,
    scheduledTriggerProperties_timezone,
    scheduledTriggerProperties_firstExecutionFrom,
    scheduledTriggerProperties_scheduleExpression,

    -- * ServiceNowSourceProperties
    ServiceNowSourceProperties (..),
    newServiceNowSourceProperties,
    serviceNowSourceProperties_object,

    -- * SourceConnectorProperties
    SourceConnectorProperties (..),
    newSourceConnectorProperties,
    sourceConnectorProperties_serviceNow,
    sourceConnectorProperties_marketo,
    sourceConnectorProperties_salesforce,
    sourceConnectorProperties_zendesk,
    sourceConnectorProperties_s3,

    -- * SourceFlowConfig
    SourceFlowConfig (..),
    newSourceFlowConfig,
    sourceFlowConfig_connectorProfileName,
    sourceFlowConfig_incrementalPullConfig,
    sourceFlowConfig_connectorType,
    sourceFlowConfig_sourceConnectorProperties,

    -- * Task
    Task (..),
    newTask,
    task_taskProperties,
    task_connectorOperator,
    task_destinationField,
    task_sourceFields,
    task_taskType,

    -- * TriggerConfig
    TriggerConfig (..),
    newTriggerConfig,
    triggerConfig_triggerProperties,
    triggerConfig_triggerType,

    -- * TriggerProperties
    TriggerProperties (..),
    newTriggerProperties,
    triggerProperties_scheduled,

    -- * UpdateAddress
    UpdateAddress (..),
    newUpdateAddress,
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

    -- * ZendeskSourceProperties
    ZendeskSourceProperties (..),
    newZendeskSourceProperties,
    zendeskSourceProperties_object,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CustomerProfiles.Types.Address
import Network.AWS.CustomerProfiles.Types.ConnectorOperator
import Network.AWS.CustomerProfiles.Types.DataPullMode
import Network.AWS.CustomerProfiles.Types.DomainStats
import Network.AWS.CustomerProfiles.Types.FieldContentType
import Network.AWS.CustomerProfiles.Types.FieldSourceProfileIds
import Network.AWS.CustomerProfiles.Types.FlowDefinition
import Network.AWS.CustomerProfiles.Types.Gender
import Network.AWS.CustomerProfiles.Types.IncrementalPullConfig
import Network.AWS.CustomerProfiles.Types.ListDomainItem
import Network.AWS.CustomerProfiles.Types.ListIntegrationItem
import Network.AWS.CustomerProfiles.Types.ListProfileObjectTypeItem
import Network.AWS.CustomerProfiles.Types.ListProfileObjectTypeTemplateItem
import Network.AWS.CustomerProfiles.Types.ListProfileObjectsItem
import Network.AWS.CustomerProfiles.Types.MarketoConnectorOperator
import Network.AWS.CustomerProfiles.Types.MarketoSourceProperties
import Network.AWS.CustomerProfiles.Types.MatchItem
import Network.AWS.CustomerProfiles.Types.MatchingRequest
import Network.AWS.CustomerProfiles.Types.MatchingResponse
import Network.AWS.CustomerProfiles.Types.ObjectFilter
import Network.AWS.CustomerProfiles.Types.ObjectTypeField
import Network.AWS.CustomerProfiles.Types.ObjectTypeKey
import Network.AWS.CustomerProfiles.Types.OperatorPropertiesKeys
import Network.AWS.CustomerProfiles.Types.PartyType
import Network.AWS.CustomerProfiles.Types.Profile
import Network.AWS.CustomerProfiles.Types.S3ConnectorOperator
import Network.AWS.CustomerProfiles.Types.S3SourceProperties
import Network.AWS.CustomerProfiles.Types.SalesforceConnectorOperator
import Network.AWS.CustomerProfiles.Types.SalesforceSourceProperties
import Network.AWS.CustomerProfiles.Types.ScheduledTriggerProperties
import Network.AWS.CustomerProfiles.Types.ServiceNowConnectorOperator
import Network.AWS.CustomerProfiles.Types.ServiceNowSourceProperties
import Network.AWS.CustomerProfiles.Types.SourceConnectorProperties
import Network.AWS.CustomerProfiles.Types.SourceConnectorType
import Network.AWS.CustomerProfiles.Types.SourceFlowConfig
import Network.AWS.CustomerProfiles.Types.StandardIdentifier
import Network.AWS.CustomerProfiles.Types.Task
import Network.AWS.CustomerProfiles.Types.TaskType
import Network.AWS.CustomerProfiles.Types.TriggerConfig
import Network.AWS.CustomerProfiles.Types.TriggerProperties
import Network.AWS.CustomerProfiles.Types.TriggerType
import Network.AWS.CustomerProfiles.Types.UpdateAddress
import Network.AWS.CustomerProfiles.Types.ZendeskConnectorOperator
import Network.AWS.CustomerProfiles.Types.ZendeskSourceProperties
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-08-15@ of the Amazon Connect Customer Profiles SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "CustomerProfiles",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "profile",
      Core._serviceSigningName = "profile",
      Core._serviceVersion = "2020-08-15",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CustomerProfiles",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | You exceeded the maximum number of requests.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | An internal service error occurred.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The requested resource does not exist, or access was denied.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The input you provided is invalid.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
