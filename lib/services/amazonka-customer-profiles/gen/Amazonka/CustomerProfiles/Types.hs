{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CustomerProfiles.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ThrottlingException,
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
    address_postalCode,
    address_country,
    address_county,
    address_state,
    address_province,
    address_address3,
    address_city,
    address_address4,
    address_address1,

    -- * ConnectorOperator
    ConnectorOperator (..),
    newConnectorOperator,
    connectorOperator_zendesk,
    connectorOperator_s3,
    connectorOperator_salesforce,
    connectorOperator_marketo,
    connectorOperator_serviceNow,

    -- * DomainStats
    DomainStats (..),
    newDomainStats,
    domainStats_meteringProfileCount,
    domainStats_profileCount,
    domainStats_objectCount,
    domainStats_totalSize,

    -- * FieldSourceProfileIds
    FieldSourceProfileIds (..),
    newFieldSourceProfileIds,
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
    listProfileObjectTypeItem_tags,
    listProfileObjectTypeItem_lastUpdatedAt,
    listProfileObjectTypeItem_createdAt,
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
    listProfileObjectsItem_object,
    listProfileObjectsItem_profileObjectUniqueKey,
    listProfileObjectsItem_objectTypeName,

    -- * MarketoSourceProperties
    MarketoSourceProperties (..),
    newMarketoSourceProperties,
    marketoSourceProperties_object,

    -- * MatchItem
    MatchItem (..),
    newMatchItem,
    matchItem_matchId,
    matchItem_profileIds,

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
    objectTypeField_target,
    objectTypeField_source,
    objectTypeField_contentType,

    -- * ObjectTypeKey
    ObjectTypeKey (..),
    newObjectTypeKey,
    objectTypeKey_fieldNames,
    objectTypeKey_standardIdentifiers,

    -- * Profile
    Profile (..),
    newProfile,
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

    -- * S3SourceProperties
    S3SourceProperties (..),
    newS3SourceProperties,
    s3SourceProperties_bucketPrefix,
    s3SourceProperties_bucketName,

    -- * SalesforceSourceProperties
    SalesforceSourceProperties (..),
    newSalesforceSourceProperties,
    salesforceSourceProperties_includeDeletedRecords,
    salesforceSourceProperties_enableDynamicFieldUpdate,
    salesforceSourceProperties_object,

    -- * ScheduledTriggerProperties
    ScheduledTriggerProperties (..),
    newScheduledTriggerProperties,
    scheduledTriggerProperties_scheduleEndTime,
    scheduledTriggerProperties_scheduleStartTime,
    scheduledTriggerProperties_timezone,
    scheduledTriggerProperties_scheduleOffset,
    scheduledTriggerProperties_firstExecutionFrom,
    scheduledTriggerProperties_dataPullMode,
    scheduledTriggerProperties_scheduleExpression,

    -- * ServiceNowSourceProperties
    ServiceNowSourceProperties (..),
    newServiceNowSourceProperties,
    serviceNowSourceProperties_object,

    -- * SourceConnectorProperties
    SourceConnectorProperties (..),
    newSourceConnectorProperties,
    sourceConnectorProperties_zendesk,
    sourceConnectorProperties_s3,
    sourceConnectorProperties_salesforce,
    sourceConnectorProperties_marketo,
    sourceConnectorProperties_serviceNow,

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
    task_connectorOperator,
    task_taskProperties,
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
    updateAddress_postalCode,
    updateAddress_country,
    updateAddress_county,
    updateAddress_state,
    updateAddress_province,
    updateAddress_address3,
    updateAddress_city,
    updateAddress_address4,
    updateAddress_address1,

    -- * ZendeskSourceProperties
    ZendeskSourceProperties (..),
    newZendeskSourceProperties,
    zendeskSourceProperties_object,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.CustomerProfiles.Types.Address
import Amazonka.CustomerProfiles.Types.ConnectorOperator
import Amazonka.CustomerProfiles.Types.DataPullMode
import Amazonka.CustomerProfiles.Types.DomainStats
import Amazonka.CustomerProfiles.Types.FieldContentType
import Amazonka.CustomerProfiles.Types.FieldSourceProfileIds
import Amazonka.CustomerProfiles.Types.FlowDefinition
import Amazonka.CustomerProfiles.Types.Gender
import Amazonka.CustomerProfiles.Types.IncrementalPullConfig
import Amazonka.CustomerProfiles.Types.ListDomainItem
import Amazonka.CustomerProfiles.Types.ListIntegrationItem
import Amazonka.CustomerProfiles.Types.ListProfileObjectTypeItem
import Amazonka.CustomerProfiles.Types.ListProfileObjectTypeTemplateItem
import Amazonka.CustomerProfiles.Types.ListProfileObjectsItem
import Amazonka.CustomerProfiles.Types.MarketoConnectorOperator
import Amazonka.CustomerProfiles.Types.MarketoSourceProperties
import Amazonka.CustomerProfiles.Types.MatchItem
import Amazonka.CustomerProfiles.Types.MatchingRequest
import Amazonka.CustomerProfiles.Types.MatchingResponse
import Amazonka.CustomerProfiles.Types.ObjectFilter
import Amazonka.CustomerProfiles.Types.ObjectTypeField
import Amazonka.CustomerProfiles.Types.ObjectTypeKey
import Amazonka.CustomerProfiles.Types.OperatorPropertiesKeys
import Amazonka.CustomerProfiles.Types.PartyType
import Amazonka.CustomerProfiles.Types.Profile
import Amazonka.CustomerProfiles.Types.S3ConnectorOperator
import Amazonka.CustomerProfiles.Types.S3SourceProperties
import Amazonka.CustomerProfiles.Types.SalesforceConnectorOperator
import Amazonka.CustomerProfiles.Types.SalesforceSourceProperties
import Amazonka.CustomerProfiles.Types.ScheduledTriggerProperties
import Amazonka.CustomerProfiles.Types.ServiceNowConnectorOperator
import Amazonka.CustomerProfiles.Types.ServiceNowSourceProperties
import Amazonka.CustomerProfiles.Types.SourceConnectorProperties
import Amazonka.CustomerProfiles.Types.SourceConnectorType
import Amazonka.CustomerProfiles.Types.SourceFlowConfig
import Amazonka.CustomerProfiles.Types.StandardIdentifier
import Amazonka.CustomerProfiles.Types.Task
import Amazonka.CustomerProfiles.Types.TaskType
import Amazonka.CustomerProfiles.Types.TriggerConfig
import Amazonka.CustomerProfiles.Types.TriggerProperties
import Amazonka.CustomerProfiles.Types.TriggerType
import Amazonka.CustomerProfiles.Types.UpdateAddress
import Amazonka.CustomerProfiles.Types.ZendeskConnectorOperator
import Amazonka.CustomerProfiles.Types.ZendeskSourceProperties
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

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

-- | You exceeded the maximum number of requests.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input you provided is invalid.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
