{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ServiceQuotaExceededException,
    _OutboundContactNotPermittedException,
    _ResourceConflictException,
    _ContactFlowNotPublishedException,
    _InternalServiceException,
    _UserNotFoundException,
    _DuplicateResourceException,
    _ThrottlingException,
    _InvalidRequestException,
    _InvalidParameterException,
    _ContactNotFoundException,
    _InvalidContactFlowException,
    _ResourceInUseException,
    _LimitExceededException,
    _DestinationNotAllowedException,
    _ResourceNotFoundException,

    -- * Channel
    Channel (..),

    -- * Comparison
    Comparison (..),

    -- * ContactFlowType
    ContactFlowType (..),

    -- * CurrentMetricName
    CurrentMetricName (..),

    -- * DirectoryType
    DirectoryType (..),

    -- * EncryptionType
    EncryptionType (..),

    -- * Grouping
    Grouping (..),

    -- * HistoricalMetricName
    HistoricalMetricName (..),

    -- * HoursOfOperationDays
    HoursOfOperationDays (..),

    -- * InstanceAttributeType
    InstanceAttributeType (..),

    -- * InstanceStatus
    InstanceStatus (..),

    -- * InstanceStorageResourceType
    InstanceStorageResourceType (..),

    -- * IntegrationType
    IntegrationType (..),

    -- * PhoneNumberCountryCode
    PhoneNumberCountryCode (..),

    -- * PhoneNumberType
    PhoneNumberType (..),

    -- * PhoneType
    PhoneType (..),

    -- * QueueStatus
    QueueStatus (..),

    -- * QueueType
    QueueType (..),

    -- * QuickConnectType
    QuickConnectType (..),

    -- * ReferenceType
    ReferenceType (..),

    -- * SourceType
    SourceType (..),

    -- * Statistic
    Statistic (..),

    -- * StorageType
    StorageType (..),

    -- * Unit
    Unit (..),

    -- * UseCaseType
    UseCaseType (..),

    -- * VoiceRecordingTrack
    VoiceRecordingTrack (..),

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_attributeType,
    attribute_value,

    -- * ChatMessage
    ChatMessage (..),
    newChatMessage,
    chatMessage_contentType,
    chatMessage_content,

    -- * ContactFlow
    ContactFlow (..),
    newContactFlow,
    contactFlow_arn,
    contactFlow_id,
    contactFlow_name,
    contactFlow_content,
    contactFlow_tags,
    contactFlow_description,
    contactFlow_type,

    -- * ContactFlowSummary
    ContactFlowSummary (..),
    newContactFlowSummary,
    contactFlowSummary_arn,
    contactFlowSummary_id,
    contactFlowSummary_contactFlowType,
    contactFlowSummary_name,

    -- * Credentials
    Credentials (..),
    newCredentials,
    credentials_refreshTokenExpiration,
    credentials_accessToken,
    credentials_accessTokenExpiration,
    credentials_refreshToken,

    -- * CurrentMetric
    CurrentMetric (..),
    newCurrentMetric,
    currentMetric_unit,
    currentMetric_name,

    -- * CurrentMetricData
    CurrentMetricData (..),
    newCurrentMetricData,
    currentMetricData_metric,
    currentMetricData_value,

    -- * CurrentMetricResult
    CurrentMetricResult (..),
    newCurrentMetricResult,
    currentMetricResult_collections,
    currentMetricResult_dimensions,

    -- * Dimensions
    Dimensions (..),
    newDimensions,
    dimensions_queue,
    dimensions_channel,

    -- * EncryptionConfig
    EncryptionConfig (..),
    newEncryptionConfig,
    encryptionConfig_encryptionType,
    encryptionConfig_keyId,

    -- * Filters
    Filters (..),
    newFilters,
    filters_channels,
    filters_queues,

    -- * HierarchyGroup
    HierarchyGroup (..),
    newHierarchyGroup,
    hierarchyGroup_levelId,
    hierarchyGroup_arn,
    hierarchyGroup_id,
    hierarchyGroup_hierarchyPath,
    hierarchyGroup_name,

    -- * HierarchyGroupSummary
    HierarchyGroupSummary (..),
    newHierarchyGroupSummary,
    hierarchyGroupSummary_arn,
    hierarchyGroupSummary_id,
    hierarchyGroupSummary_name,

    -- * HierarchyLevel
    HierarchyLevel (..),
    newHierarchyLevel,
    hierarchyLevel_arn,
    hierarchyLevel_id,
    hierarchyLevel_name,

    -- * HierarchyLevelUpdate
    HierarchyLevelUpdate (..),
    newHierarchyLevelUpdate,
    hierarchyLevelUpdate_name,

    -- * HierarchyPath
    HierarchyPath (..),
    newHierarchyPath,
    hierarchyPath_levelThree,
    hierarchyPath_levelFour,
    hierarchyPath_levelTwo,
    hierarchyPath_levelOne,
    hierarchyPath_levelFive,

    -- * HierarchyStructure
    HierarchyStructure (..),
    newHierarchyStructure,
    hierarchyStructure_levelThree,
    hierarchyStructure_levelFour,
    hierarchyStructure_levelTwo,
    hierarchyStructure_levelOne,
    hierarchyStructure_levelFive,

    -- * HierarchyStructureUpdate
    HierarchyStructureUpdate (..),
    newHierarchyStructureUpdate,
    hierarchyStructureUpdate_levelThree,
    hierarchyStructureUpdate_levelFour,
    hierarchyStructureUpdate_levelTwo,
    hierarchyStructureUpdate_levelOne,
    hierarchyStructureUpdate_levelFive,

    -- * HistoricalMetric
    HistoricalMetric (..),
    newHistoricalMetric,
    historicalMetric_threshold,
    historicalMetric_unit,
    historicalMetric_name,
    historicalMetric_statistic,

    -- * HistoricalMetricData
    HistoricalMetricData (..),
    newHistoricalMetricData,
    historicalMetricData_metric,
    historicalMetricData_value,

    -- * HistoricalMetricResult
    HistoricalMetricResult (..),
    newHistoricalMetricResult,
    historicalMetricResult_collections,
    historicalMetricResult_dimensions,

    -- * HoursOfOperation
    HoursOfOperation (..),
    newHoursOfOperation,
    hoursOfOperation_config,
    hoursOfOperation_hoursOfOperationArn,
    hoursOfOperation_name,
    hoursOfOperation_tags,
    hoursOfOperation_description,
    hoursOfOperation_timeZone,
    hoursOfOperation_hoursOfOperationId,

    -- * HoursOfOperationConfig
    HoursOfOperationConfig (..),
    newHoursOfOperationConfig,
    hoursOfOperationConfig_day,
    hoursOfOperationConfig_startTime,
    hoursOfOperationConfig_endTime,

    -- * HoursOfOperationSummary
    HoursOfOperationSummary (..),
    newHoursOfOperationSummary,
    hoursOfOperationSummary_arn,
    hoursOfOperationSummary_id,
    hoursOfOperationSummary_name,

    -- * HoursOfOperationTimeSlice
    HoursOfOperationTimeSlice (..),
    newHoursOfOperationTimeSlice,
    hoursOfOperationTimeSlice_hours,
    hoursOfOperationTimeSlice_minutes,

    -- * Instance
    Instance (..),
    newInstance,
    instance_instanceAlias,
    instance_serviceRole,
    instance_outboundCallsEnabled,
    instance_arn,
    instance_id,
    instance_instanceStatus,
    instance_identityManagementType,
    instance_createdTime,
    instance_inboundCallsEnabled,
    instance_statusReason,

    -- * InstanceStatusReason
    InstanceStatusReason (..),
    newInstanceStatusReason,
    instanceStatusReason_message,

    -- * InstanceStorageConfig
    InstanceStorageConfig (..),
    newInstanceStorageConfig,
    instanceStorageConfig_kinesisStreamConfig,
    instanceStorageConfig_kinesisFirehoseConfig,
    instanceStorageConfig_kinesisVideoStreamConfig,
    instanceStorageConfig_associationId,
    instanceStorageConfig_s3Config,
    instanceStorageConfig_storageType,

    -- * InstanceSummary
    InstanceSummary (..),
    newInstanceSummary,
    instanceSummary_instanceAlias,
    instanceSummary_serviceRole,
    instanceSummary_outboundCallsEnabled,
    instanceSummary_arn,
    instanceSummary_id,
    instanceSummary_instanceStatus,
    instanceSummary_identityManagementType,
    instanceSummary_createdTime,
    instanceSummary_inboundCallsEnabled,

    -- * IntegrationAssociationSummary
    IntegrationAssociationSummary (..),
    newIntegrationAssociationSummary,
    integrationAssociationSummary_instanceId,
    integrationAssociationSummary_sourceApplicationName,
    integrationAssociationSummary_integrationAssociationArn,
    integrationAssociationSummary_sourceApplicationUrl,
    integrationAssociationSummary_integrationType,
    integrationAssociationSummary_integrationArn,
    integrationAssociationSummary_sourceType,
    integrationAssociationSummary_integrationAssociationId,

    -- * KinesisFirehoseConfig
    KinesisFirehoseConfig (..),
    newKinesisFirehoseConfig,
    kinesisFirehoseConfig_firehoseArn,

    -- * KinesisStreamConfig
    KinesisStreamConfig (..),
    newKinesisStreamConfig,
    kinesisStreamConfig_streamArn,

    -- * KinesisVideoStreamConfig
    KinesisVideoStreamConfig (..),
    newKinesisVideoStreamConfig,
    kinesisVideoStreamConfig_prefix,
    kinesisVideoStreamConfig_retentionPeriodHours,
    kinesisVideoStreamConfig_encryptionConfig,

    -- * LexBot
    LexBot (..),
    newLexBot,
    lexBot_name,
    lexBot_lexRegion,

    -- * MediaConcurrency
    MediaConcurrency (..),
    newMediaConcurrency,
    mediaConcurrency_channel,
    mediaConcurrency_concurrency,

    -- * OutboundCallerConfig
    OutboundCallerConfig (..),
    newOutboundCallerConfig,
    outboundCallerConfig_outboundCallerIdNumberId,
    outboundCallerConfig_outboundFlowId,
    outboundCallerConfig_outboundCallerIdName,

    -- * ParticipantDetails
    ParticipantDetails (..),
    newParticipantDetails,
    participantDetails_displayName,

    -- * PhoneNumberQuickConnectConfig
    PhoneNumberQuickConnectConfig (..),
    newPhoneNumberQuickConnectConfig,
    phoneNumberQuickConnectConfig_phoneNumber,

    -- * PhoneNumberSummary
    PhoneNumberSummary (..),
    newPhoneNumberSummary,
    phoneNumberSummary_phoneNumber,
    phoneNumberSummary_arn,
    phoneNumberSummary_id,
    phoneNumberSummary_phoneNumberType,
    phoneNumberSummary_phoneNumberCountryCode,

    -- * PromptSummary
    PromptSummary (..),
    newPromptSummary,
    promptSummary_arn,
    promptSummary_id,
    promptSummary_name,

    -- * Queue
    Queue (..),
    newQueue,
    queue_maxContacts,
    queue_status,
    queue_queueId,
    queue_name,
    queue_queueArn,
    queue_tags,
    queue_description,
    queue_outboundCallerConfig,
    queue_hoursOfOperationId,

    -- * QueueQuickConnectConfig
    QueueQuickConnectConfig (..),
    newQueueQuickConnectConfig,
    queueQuickConnectConfig_queueId,
    queueQuickConnectConfig_contactFlowId,

    -- * QueueReference
    QueueReference (..),
    newQueueReference,
    queueReference_arn,
    queueReference_id,

    -- * QueueSummary
    QueueSummary (..),
    newQueueSummary,
    queueSummary_queueType,
    queueSummary_arn,
    queueSummary_id,
    queueSummary_name,

    -- * QuickConnect
    QuickConnect (..),
    newQuickConnect,
    quickConnect_quickConnectId,
    quickConnect_name,
    quickConnect_tags,
    quickConnect_quickConnectConfig,
    quickConnect_quickConnectARN,
    quickConnect_description,

    -- * QuickConnectConfig
    QuickConnectConfig (..),
    newQuickConnectConfig,
    quickConnectConfig_userConfig,
    quickConnectConfig_phoneConfig,
    quickConnectConfig_queueConfig,
    quickConnectConfig_quickConnectType,

    -- * QuickConnectSummary
    QuickConnectSummary (..),
    newQuickConnectSummary,
    quickConnectSummary_quickConnectType,
    quickConnectSummary_arn,
    quickConnectSummary_id,
    quickConnectSummary_name,

    -- * Reference
    Reference (..),
    newReference,
    reference_value,
    reference_type,

    -- * RoutingProfile
    RoutingProfile (..),
    newRoutingProfile,
    routingProfile_instanceId,
    routingProfile_defaultOutboundQueueId,
    routingProfile_routingProfileId,
    routingProfile_mediaConcurrencies,
    routingProfile_name,
    routingProfile_tags,
    routingProfile_description,
    routingProfile_routingProfileArn,

    -- * RoutingProfileQueueConfig
    RoutingProfileQueueConfig (..),
    newRoutingProfileQueueConfig,
    routingProfileQueueConfig_queueReference,
    routingProfileQueueConfig_priority,
    routingProfileQueueConfig_delay,

    -- * RoutingProfileQueueConfigSummary
    RoutingProfileQueueConfigSummary (..),
    newRoutingProfileQueueConfigSummary,
    routingProfileQueueConfigSummary_queueId,
    routingProfileQueueConfigSummary_queueArn,
    routingProfileQueueConfigSummary_queueName,
    routingProfileQueueConfigSummary_priority,
    routingProfileQueueConfigSummary_delay,
    routingProfileQueueConfigSummary_channel,

    -- * RoutingProfileQueueReference
    RoutingProfileQueueReference (..),
    newRoutingProfileQueueReference,
    routingProfileQueueReference_queueId,
    routingProfileQueueReference_channel,

    -- * RoutingProfileSummary
    RoutingProfileSummary (..),
    newRoutingProfileSummary,
    routingProfileSummary_arn,
    routingProfileSummary_id,
    routingProfileSummary_name,

    -- * S3Config
    S3Config (..),
    newS3Config,
    s3Config_encryptionConfig,
    s3Config_bucketName,
    s3Config_bucketPrefix,

    -- * SecurityKey
    SecurityKey (..),
    newSecurityKey,
    securityKey_key,
    securityKey_creationTime,
    securityKey_associationId,

    -- * SecurityProfileSummary
    SecurityProfileSummary (..),
    newSecurityProfileSummary,
    securityProfileSummary_arn,
    securityProfileSummary_id,
    securityProfileSummary_name,

    -- * Threshold
    Threshold (..),
    newThreshold,
    threshold_thresholdValue,
    threshold_comparison,

    -- * UseCase
    UseCase (..),
    newUseCase,
    useCase_useCaseArn,
    useCase_useCaseType,
    useCase_useCaseId,

    -- * User
    User (..),
    newUser,
    user_securityProfileIds,
    user_identityInfo,
    user_arn,
    user_id,
    user_hierarchyGroupId,
    user_directoryUserId,
    user_routingProfileId,
    user_tags,
    user_phoneConfig,
    user_username,

    -- * UserIdentityInfo
    UserIdentityInfo (..),
    newUserIdentityInfo,
    userIdentityInfo_email,
    userIdentityInfo_firstName,
    userIdentityInfo_lastName,

    -- * UserPhoneConfig
    UserPhoneConfig (..),
    newUserPhoneConfig,
    userPhoneConfig_autoAccept,
    userPhoneConfig_afterContactWorkTimeLimit,
    userPhoneConfig_deskPhoneNumber,
    userPhoneConfig_phoneType,

    -- * UserQuickConnectConfig
    UserQuickConnectConfig (..),
    newUserQuickConnectConfig,
    userQuickConnectConfig_userId,
    userQuickConnectConfig_contactFlowId,

    -- * UserSummary
    UserSummary (..),
    newUserSummary,
    userSummary_arn,
    userSummary_id,
    userSummary_username,

    -- * VoiceRecordingConfiguration
    VoiceRecordingConfiguration (..),
    newVoiceRecordingConfiguration,
    voiceRecordingConfiguration_voiceRecordingTrack,
  )
where

import Network.AWS.Connect.Types.Attribute
import Network.AWS.Connect.Types.Channel
import Network.AWS.Connect.Types.ChatMessage
import Network.AWS.Connect.Types.Comparison
import Network.AWS.Connect.Types.ContactFlow
import Network.AWS.Connect.Types.ContactFlowSummary
import Network.AWS.Connect.Types.ContactFlowType
import Network.AWS.Connect.Types.Credentials
import Network.AWS.Connect.Types.CurrentMetric
import Network.AWS.Connect.Types.CurrentMetricData
import Network.AWS.Connect.Types.CurrentMetricName
import Network.AWS.Connect.Types.CurrentMetricResult
import Network.AWS.Connect.Types.Dimensions
import Network.AWS.Connect.Types.DirectoryType
import Network.AWS.Connect.Types.EncryptionConfig
import Network.AWS.Connect.Types.EncryptionType
import Network.AWS.Connect.Types.Filters
import Network.AWS.Connect.Types.Grouping
import Network.AWS.Connect.Types.HierarchyGroup
import Network.AWS.Connect.Types.HierarchyGroupSummary
import Network.AWS.Connect.Types.HierarchyLevel
import Network.AWS.Connect.Types.HierarchyLevelUpdate
import Network.AWS.Connect.Types.HierarchyPath
import Network.AWS.Connect.Types.HierarchyStructure
import Network.AWS.Connect.Types.HierarchyStructureUpdate
import Network.AWS.Connect.Types.HistoricalMetric
import Network.AWS.Connect.Types.HistoricalMetricData
import Network.AWS.Connect.Types.HistoricalMetricName
import Network.AWS.Connect.Types.HistoricalMetricResult
import Network.AWS.Connect.Types.HoursOfOperation
import Network.AWS.Connect.Types.HoursOfOperationConfig
import Network.AWS.Connect.Types.HoursOfOperationDays
import Network.AWS.Connect.Types.HoursOfOperationSummary
import Network.AWS.Connect.Types.HoursOfOperationTimeSlice
import Network.AWS.Connect.Types.Instance
import Network.AWS.Connect.Types.InstanceAttributeType
import Network.AWS.Connect.Types.InstanceStatus
import Network.AWS.Connect.Types.InstanceStatusReason
import Network.AWS.Connect.Types.InstanceStorageConfig
import Network.AWS.Connect.Types.InstanceStorageResourceType
import Network.AWS.Connect.Types.InstanceSummary
import Network.AWS.Connect.Types.IntegrationAssociationSummary
import Network.AWS.Connect.Types.IntegrationType
import Network.AWS.Connect.Types.KinesisFirehoseConfig
import Network.AWS.Connect.Types.KinesisStreamConfig
import Network.AWS.Connect.Types.KinesisVideoStreamConfig
import Network.AWS.Connect.Types.LexBot
import Network.AWS.Connect.Types.MediaConcurrency
import Network.AWS.Connect.Types.OutboundCallerConfig
import Network.AWS.Connect.Types.ParticipantDetails
import Network.AWS.Connect.Types.PhoneNumberCountryCode
import Network.AWS.Connect.Types.PhoneNumberQuickConnectConfig
import Network.AWS.Connect.Types.PhoneNumberSummary
import Network.AWS.Connect.Types.PhoneNumberType
import Network.AWS.Connect.Types.PhoneType
import Network.AWS.Connect.Types.PromptSummary
import Network.AWS.Connect.Types.Queue
import Network.AWS.Connect.Types.QueueQuickConnectConfig
import Network.AWS.Connect.Types.QueueReference
import Network.AWS.Connect.Types.QueueStatus
import Network.AWS.Connect.Types.QueueSummary
import Network.AWS.Connect.Types.QueueType
import Network.AWS.Connect.Types.QuickConnect
import Network.AWS.Connect.Types.QuickConnectConfig
import Network.AWS.Connect.Types.QuickConnectSummary
import Network.AWS.Connect.Types.QuickConnectType
import Network.AWS.Connect.Types.Reference
import Network.AWS.Connect.Types.ReferenceType
import Network.AWS.Connect.Types.RoutingProfile
import Network.AWS.Connect.Types.RoutingProfileQueueConfig
import Network.AWS.Connect.Types.RoutingProfileQueueConfigSummary
import Network.AWS.Connect.Types.RoutingProfileQueueReference
import Network.AWS.Connect.Types.RoutingProfileSummary
import Network.AWS.Connect.Types.S3Config
import Network.AWS.Connect.Types.SecurityKey
import Network.AWS.Connect.Types.SecurityProfileSummary
import Network.AWS.Connect.Types.SourceType
import Network.AWS.Connect.Types.Statistic
import Network.AWS.Connect.Types.StorageType
import Network.AWS.Connect.Types.Threshold
import Network.AWS.Connect.Types.Unit
import Network.AWS.Connect.Types.UseCase
import Network.AWS.Connect.Types.UseCaseType
import Network.AWS.Connect.Types.User
import Network.AWS.Connect.Types.UserIdentityInfo
import Network.AWS.Connect.Types.UserPhoneConfig
import Network.AWS.Connect.Types.UserQuickConnectConfig
import Network.AWS.Connect.Types.UserSummary
import Network.AWS.Connect.Types.VoiceRecordingConfiguration
import Network.AWS.Connect.Types.VoiceRecordingTrack
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-08-08@ of the Amazon Connect Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Connect",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "connect",
      Core._serviceSigningName = "connect",
      Core._serviceVersion = "2017-08-08",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Connect",
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The service quota has been exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Core.. Core.hasStatus 402

-- | The contact is not permitted.
_OutboundContactNotPermittedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OutboundContactNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OutboundContactNotPermittedException"
    Core.. Core.hasStatus 403

-- | A resource already has that name.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Core.. Core.hasStatus 409

-- | The contact flow has not been published.
_ContactFlowNotPublishedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ContactFlowNotPublishedException =
  Core._MatchServiceError
    defaultService
    "ContactFlowNotPublishedException"
    Core.. Core.hasStatus 404

-- | Request processing failed because of an error or failure with the
-- service.
_InternalServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"
    Core.. Core.hasStatus 500

-- | No user with the specified credentials was found in the Amazon Connect
-- instance.
_UserNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserNotFoundException =
  Core._MatchServiceError
    defaultService
    "UserNotFoundException"
    Core.. Core.hasStatus 404

-- | A resource with the specified name already exists.
_DuplicateResourceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateResourceException =
  Core._MatchServiceError
    defaultService
    "DuplicateResourceException"
    Core.. Core.hasStatus 409

-- | The throttling limit has been exceeded.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Core.. Core.hasStatus 429

-- | The request is not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Core.. Core.hasStatus 400

-- | One or more of the specified parameters are not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Core.. Core.hasStatus 400

-- | The contact with the specified ID is not active or does not exist.
_ContactNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ContactNotFoundException =
  Core._MatchServiceError
    defaultService
    "ContactNotFoundException"
    Core.. Core.hasStatus 410

-- | The contact flow is not valid.
_InvalidContactFlowException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidContactFlowException =
  Core._MatchServiceError
    defaultService
    "InvalidContactFlowException"
    Core.. Core.hasStatus 400

-- | That resource is already in use. Please try another.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Core.. Core.hasStatus 409

-- | The allowed limit for the resource has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Core.. Core.hasStatus 429

-- | Outbound calls to the destination number are not allowed.
_DestinationNotAllowedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DestinationNotAllowedException =
  Core._MatchServiceError
    defaultService
    "DestinationNotAllowedException"
    Core.. Core.hasStatus 403

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Core.. Core.hasStatus 404
