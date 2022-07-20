{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Connect.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidContactFlowException,
    _DuplicateResourceException,
    _UserNotFoundException,
    _ServiceQuotaExceededException,
    _ContactNotFoundException,
    _ResourceNotFoundException,
    _ResourceInUseException,
    _LimitExceededException,
    _OutboundContactNotPermittedException,
    _ThrottlingException,
    _ResourceConflictException,
    _InternalServiceException,
    _InvalidRequestException,
    _DestinationNotAllowedException,
    _ContactFlowNotPublishedException,
    _InvalidParameterException,

    -- * AgentStatusState
    AgentStatusState (..),

    -- * AgentStatusType
    AgentStatusType (..),

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

    -- * LexVersion
    LexVersion (..),

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

    -- * TrafficType
    TrafficType (..),

    -- * Unit
    Unit (..),

    -- * UseCaseType
    UseCaseType (..),

    -- * VoiceRecordingTrack
    VoiceRecordingTrack (..),

    -- * AgentStatus
    AgentStatus (..),
    newAgentStatus,
    agentStatus_tags,
    agentStatus_name,
    agentStatus_type,
    agentStatus_displayOrder,
    agentStatus_agentStatusId,
    agentStatus_state,
    agentStatus_description,
    agentStatus_agentStatusARN,

    -- * AgentStatusSummary
    AgentStatusSummary (..),
    newAgentStatusSummary,
    agentStatusSummary_name,
    agentStatusSummary_type,
    agentStatusSummary_arn,
    agentStatusSummary_id,

    -- * AnswerMachineDetectionConfig
    AnswerMachineDetectionConfig (..),
    newAnswerMachineDetectionConfig,
    answerMachineDetectionConfig_awaitAnswerMachinePrompt,
    answerMachineDetectionConfig_enableAnswerMachineDetection,

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
    contactFlow_tags,
    contactFlow_name,
    contactFlow_type,
    contactFlow_arn,
    contactFlow_id,
    contactFlow_description,
    contactFlow_content,

    -- * ContactFlowSummary
    ContactFlowSummary (..),
    newContactFlowSummary,
    contactFlowSummary_name,
    contactFlowSummary_arn,
    contactFlowSummary_id,
    contactFlowSummary_contactFlowType,

    -- * Credentials
    Credentials (..),
    newCredentials,
    credentials_accessToken,
    credentials_accessTokenExpiration,
    credentials_refreshTokenExpiration,
    credentials_refreshToken,

    -- * CurrentMetric
    CurrentMetric (..),
    newCurrentMetric,
    currentMetric_name,
    currentMetric_unit,

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
    dimensions_channel,
    dimensions_queue,

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
    hierarchyGroup_name,
    hierarchyGroup_arn,
    hierarchyGroup_hierarchyPath,
    hierarchyGroup_id,
    hierarchyGroup_levelId,

    -- * HierarchyGroupSummary
    HierarchyGroupSummary (..),
    newHierarchyGroupSummary,
    hierarchyGroupSummary_name,
    hierarchyGroupSummary_arn,
    hierarchyGroupSummary_id,

    -- * HierarchyLevel
    HierarchyLevel (..),
    newHierarchyLevel,
    hierarchyLevel_name,
    hierarchyLevel_arn,
    hierarchyLevel_id,

    -- * HierarchyLevelUpdate
    HierarchyLevelUpdate (..),
    newHierarchyLevelUpdate,
    hierarchyLevelUpdate_name,

    -- * HierarchyPath
    HierarchyPath (..),
    newHierarchyPath,
    hierarchyPath_levelThree,
    hierarchyPath_levelFour,
    hierarchyPath_levelOne,
    hierarchyPath_levelFive,
    hierarchyPath_levelTwo,

    -- * HierarchyStructure
    HierarchyStructure (..),
    newHierarchyStructure,
    hierarchyStructure_levelThree,
    hierarchyStructure_levelFour,
    hierarchyStructure_levelOne,
    hierarchyStructure_levelFive,
    hierarchyStructure_levelTwo,

    -- * HierarchyStructureUpdate
    HierarchyStructureUpdate (..),
    newHierarchyStructureUpdate,
    hierarchyStructureUpdate_levelThree,
    hierarchyStructureUpdate_levelFour,
    hierarchyStructureUpdate_levelOne,
    hierarchyStructureUpdate_levelFive,
    hierarchyStructureUpdate_levelTwo,

    -- * HistoricalMetric
    HistoricalMetric (..),
    newHistoricalMetric,
    historicalMetric_name,
    historicalMetric_threshold,
    historicalMetric_statistic,
    historicalMetric_unit,

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
    hoursOfOperation_tags,
    hoursOfOperation_name,
    hoursOfOperation_timeZone,
    hoursOfOperation_description,
    hoursOfOperation_hoursOfOperationArn,
    hoursOfOperation_hoursOfOperationId,
    hoursOfOperation_config,

    -- * HoursOfOperationConfig
    HoursOfOperationConfig (..),
    newHoursOfOperationConfig,
    hoursOfOperationConfig_day,
    hoursOfOperationConfig_startTime,
    hoursOfOperationConfig_endTime,

    -- * HoursOfOperationSummary
    HoursOfOperationSummary (..),
    newHoursOfOperationSummary,
    hoursOfOperationSummary_name,
    hoursOfOperationSummary_arn,
    hoursOfOperationSummary_id,

    -- * HoursOfOperationTimeSlice
    HoursOfOperationTimeSlice (..),
    newHoursOfOperationTimeSlice,
    hoursOfOperationTimeSlice_hours,
    hoursOfOperationTimeSlice_minutes,

    -- * Instance
    Instance (..),
    newInstance,
    instance_identityManagementType,
    instance_createdTime,
    instance_instanceStatus,
    instance_arn,
    instance_statusReason,
    instance_id,
    instance_serviceRole,
    instance_inboundCallsEnabled,
    instance_instanceAlias,
    instance_outboundCallsEnabled,

    -- * InstanceStatusReason
    InstanceStatusReason (..),
    newInstanceStatusReason,
    instanceStatusReason_message,

    -- * InstanceStorageConfig
    InstanceStorageConfig (..),
    newInstanceStorageConfig,
    instanceStorageConfig_kinesisVideoStreamConfig,
    instanceStorageConfig_kinesisFirehoseConfig,
    instanceStorageConfig_s3Config,
    instanceStorageConfig_associationId,
    instanceStorageConfig_kinesisStreamConfig,
    instanceStorageConfig_storageType,

    -- * InstanceSummary
    InstanceSummary (..),
    newInstanceSummary,
    instanceSummary_identityManagementType,
    instanceSummary_createdTime,
    instanceSummary_instanceStatus,
    instanceSummary_arn,
    instanceSummary_id,
    instanceSummary_serviceRole,
    instanceSummary_inboundCallsEnabled,
    instanceSummary_instanceAlias,
    instanceSummary_outboundCallsEnabled,

    -- * IntegrationAssociationSummary
    IntegrationAssociationSummary (..),
    newIntegrationAssociationSummary,
    integrationAssociationSummary_integrationAssociationArn,
    integrationAssociationSummary_integrationArn,
    integrationAssociationSummary_sourceType,
    integrationAssociationSummary_instanceId,
    integrationAssociationSummary_integrationType,
    integrationAssociationSummary_sourceApplicationName,
    integrationAssociationSummary_sourceApplicationUrl,
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

    -- * LexBotConfig
    LexBotConfig (..),
    newLexBotConfig,
    lexBotConfig_lexV2Bot,
    lexBotConfig_lexBot,

    -- * LexV2Bot
    LexV2Bot (..),
    newLexV2Bot,
    lexV2Bot_aliasArn,

    -- * MediaConcurrency
    MediaConcurrency (..),
    newMediaConcurrency,
    mediaConcurrency_channel,
    mediaConcurrency_concurrency,

    -- * OutboundCallerConfig
    OutboundCallerConfig (..),
    newOutboundCallerConfig,
    outboundCallerConfig_outboundFlowId,
    outboundCallerConfig_outboundCallerIdNumberId,
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
    phoneNumberSummary_phoneNumberCountryCode,
    phoneNumberSummary_phoneNumberType,
    phoneNumberSummary_arn,
    phoneNumberSummary_id,
    phoneNumberSummary_phoneNumber,

    -- * PromptSummary
    PromptSummary (..),
    newPromptSummary,
    promptSummary_name,
    promptSummary_arn,
    promptSummary_id,

    -- * Queue
    Queue (..),
    newQueue,
    queue_tags,
    queue_name,
    queue_queueArn,
    queue_status,
    queue_description,
    queue_queueId,
    queue_hoursOfOperationId,
    queue_maxContacts,
    queue_outboundCallerConfig,

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
    queueSummary_name,
    queueSummary_arn,
    queueSummary_id,
    queueSummary_queueType,

    -- * QuickConnect
    QuickConnect (..),
    newQuickConnect,
    quickConnect_tags,
    quickConnect_name,
    quickConnect_quickConnectConfig,
    quickConnect_description,
    quickConnect_quickConnectARN,
    quickConnect_quickConnectId,

    -- * QuickConnectConfig
    QuickConnectConfig (..),
    newQuickConnectConfig,
    quickConnectConfig_queueConfig,
    quickConnectConfig_phoneConfig,
    quickConnectConfig_userConfig,
    quickConnectConfig_quickConnectType,

    -- * QuickConnectSummary
    QuickConnectSummary (..),
    newQuickConnectSummary,
    quickConnectSummary_name,
    quickConnectSummary_quickConnectType,
    quickConnectSummary_arn,
    quickConnectSummary_id,

    -- * Reference
    Reference (..),
    newReference,
    reference_value,
    reference_type,

    -- * RoutingProfile
    RoutingProfile (..),
    newRoutingProfile,
    routingProfile_tags,
    routingProfile_name,
    routingProfile_description,
    routingProfile_mediaConcurrencies,
    routingProfile_routingProfileArn,
    routingProfile_instanceId,
    routingProfile_defaultOutboundQueueId,
    routingProfile_routingProfileId,

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
    routingProfileSummary_name,
    routingProfileSummary_arn,
    routingProfileSummary_id,

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
    securityProfileSummary_name,
    securityProfileSummary_arn,
    securityProfileSummary_id,

    -- * Threshold
    Threshold (..),
    newThreshold,
    threshold_thresholdValue,
    threshold_comparison,

    -- * UseCase
    UseCase (..),
    newUseCase,
    useCase_useCaseType,
    useCase_useCaseArn,
    useCase_useCaseId,

    -- * User
    User (..),
    newUser,
    user_tags,
    user_hierarchyGroupId,
    user_identityInfo,
    user_username,
    user_arn,
    user_securityProfileIds,
    user_phoneConfig,
    user_id,
    user_directoryUserId,
    user_routingProfileId,

    -- * UserIdentityInfo
    UserIdentityInfo (..),
    newUserIdentityInfo,
    userIdentityInfo_firstName,
    userIdentityInfo_email,
    userIdentityInfo_lastName,

    -- * UserPhoneConfig
    UserPhoneConfig (..),
    newUserPhoneConfig,
    userPhoneConfig_autoAccept,
    userPhoneConfig_deskPhoneNumber,
    userPhoneConfig_afterContactWorkTimeLimit,
    userPhoneConfig_phoneType,

    -- * UserQuickConnectConfig
    UserQuickConnectConfig (..),
    newUserQuickConnectConfig,
    userQuickConnectConfig_userId,
    userQuickConnectConfig_contactFlowId,

    -- * UserSummary
    UserSummary (..),
    newUserSummary,
    userSummary_username,
    userSummary_arn,
    userSummary_id,

    -- * VoiceRecordingConfiguration
    VoiceRecordingConfiguration (..),
    newVoiceRecordingConfiguration,
    voiceRecordingConfiguration_voiceRecordingTrack,
  )
where

import Amazonka.Connect.Types.AgentStatus
import Amazonka.Connect.Types.AgentStatusState
import Amazonka.Connect.Types.AgentStatusSummary
import Amazonka.Connect.Types.AgentStatusType
import Amazonka.Connect.Types.AnswerMachineDetectionConfig
import Amazonka.Connect.Types.Attribute
import Amazonka.Connect.Types.Channel
import Amazonka.Connect.Types.ChatMessage
import Amazonka.Connect.Types.Comparison
import Amazonka.Connect.Types.ContactFlow
import Amazonka.Connect.Types.ContactFlowSummary
import Amazonka.Connect.Types.ContactFlowType
import Amazonka.Connect.Types.Credentials
import Amazonka.Connect.Types.CurrentMetric
import Amazonka.Connect.Types.CurrentMetricData
import Amazonka.Connect.Types.CurrentMetricName
import Amazonka.Connect.Types.CurrentMetricResult
import Amazonka.Connect.Types.Dimensions
import Amazonka.Connect.Types.DirectoryType
import Amazonka.Connect.Types.EncryptionConfig
import Amazonka.Connect.Types.EncryptionType
import Amazonka.Connect.Types.Filters
import Amazonka.Connect.Types.Grouping
import Amazonka.Connect.Types.HierarchyGroup
import Amazonka.Connect.Types.HierarchyGroupSummary
import Amazonka.Connect.Types.HierarchyLevel
import Amazonka.Connect.Types.HierarchyLevelUpdate
import Amazonka.Connect.Types.HierarchyPath
import Amazonka.Connect.Types.HierarchyStructure
import Amazonka.Connect.Types.HierarchyStructureUpdate
import Amazonka.Connect.Types.HistoricalMetric
import Amazonka.Connect.Types.HistoricalMetricData
import Amazonka.Connect.Types.HistoricalMetricName
import Amazonka.Connect.Types.HistoricalMetricResult
import Amazonka.Connect.Types.HoursOfOperation
import Amazonka.Connect.Types.HoursOfOperationConfig
import Amazonka.Connect.Types.HoursOfOperationDays
import Amazonka.Connect.Types.HoursOfOperationSummary
import Amazonka.Connect.Types.HoursOfOperationTimeSlice
import Amazonka.Connect.Types.Instance
import Amazonka.Connect.Types.InstanceAttributeType
import Amazonka.Connect.Types.InstanceStatus
import Amazonka.Connect.Types.InstanceStatusReason
import Amazonka.Connect.Types.InstanceStorageConfig
import Amazonka.Connect.Types.InstanceStorageResourceType
import Amazonka.Connect.Types.InstanceSummary
import Amazonka.Connect.Types.IntegrationAssociationSummary
import Amazonka.Connect.Types.IntegrationType
import Amazonka.Connect.Types.KinesisFirehoseConfig
import Amazonka.Connect.Types.KinesisStreamConfig
import Amazonka.Connect.Types.KinesisVideoStreamConfig
import Amazonka.Connect.Types.LexBot
import Amazonka.Connect.Types.LexBotConfig
import Amazonka.Connect.Types.LexV2Bot
import Amazonka.Connect.Types.LexVersion
import Amazonka.Connect.Types.MediaConcurrency
import Amazonka.Connect.Types.OutboundCallerConfig
import Amazonka.Connect.Types.ParticipantDetails
import Amazonka.Connect.Types.PhoneNumberCountryCode
import Amazonka.Connect.Types.PhoneNumberQuickConnectConfig
import Amazonka.Connect.Types.PhoneNumberSummary
import Amazonka.Connect.Types.PhoneNumberType
import Amazonka.Connect.Types.PhoneType
import Amazonka.Connect.Types.PromptSummary
import Amazonka.Connect.Types.Queue
import Amazonka.Connect.Types.QueueQuickConnectConfig
import Amazonka.Connect.Types.QueueReference
import Amazonka.Connect.Types.QueueStatus
import Amazonka.Connect.Types.QueueSummary
import Amazonka.Connect.Types.QueueType
import Amazonka.Connect.Types.QuickConnect
import Amazonka.Connect.Types.QuickConnectConfig
import Amazonka.Connect.Types.QuickConnectSummary
import Amazonka.Connect.Types.QuickConnectType
import Amazonka.Connect.Types.Reference
import Amazonka.Connect.Types.ReferenceType
import Amazonka.Connect.Types.RoutingProfile
import Amazonka.Connect.Types.RoutingProfileQueueConfig
import Amazonka.Connect.Types.RoutingProfileQueueConfigSummary
import Amazonka.Connect.Types.RoutingProfileQueueReference
import Amazonka.Connect.Types.RoutingProfileSummary
import Amazonka.Connect.Types.S3Config
import Amazonka.Connect.Types.SecurityKey
import Amazonka.Connect.Types.SecurityProfileSummary
import Amazonka.Connect.Types.SourceType
import Amazonka.Connect.Types.Statistic
import Amazonka.Connect.Types.StorageType
import Amazonka.Connect.Types.Threshold
import Amazonka.Connect.Types.TrafficType
import Amazonka.Connect.Types.Unit
import Amazonka.Connect.Types.UseCase
import Amazonka.Connect.Types.UseCaseType
import Amazonka.Connect.Types.User
import Amazonka.Connect.Types.UserIdentityInfo
import Amazonka.Connect.Types.UserPhoneConfig
import Amazonka.Connect.Types.UserQuickConnectConfig
import Amazonka.Connect.Types.UserSummary
import Amazonka.Connect.Types.VoiceRecordingConfiguration
import Amazonka.Connect.Types.VoiceRecordingTrack
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

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
      Core._serviceTimeout = Prelude.Just 70,
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

-- | The contact flow is not valid.
_InvalidContactFlowException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidContactFlowException =
  Core._MatchServiceError
    defaultService
    "InvalidContactFlowException"
    Prelude.. Core.hasStatus 400

-- | A resource with the specified name already exists.
_DuplicateResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateResourceException =
  Core._MatchServiceError
    defaultService
    "DuplicateResourceException"
    Prelude.. Core.hasStatus 409

-- | No user with the specified credentials was found in the Amazon Connect
-- instance.
_UserNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserNotFoundException =
  Core._MatchServiceError
    defaultService
    "UserNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The service quota has been exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The contact with the specified ID is not active or does not exist.
_ContactNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ContactNotFoundException =
  Core._MatchServiceError
    defaultService
    "ContactNotFoundException"
    Prelude.. Core.hasStatus 410

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | That resource is already in use. Please try another.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 409

-- | The allowed limit for the resource has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | The contact is not permitted.
_OutboundContactNotPermittedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OutboundContactNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OutboundContactNotPermittedException"
    Prelude.. Core.hasStatus 403

-- | The throttling limit has been exceeded.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | A resource already has that name.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Prelude.. Core.hasStatus 409

-- | Request processing failed because of an error or failure with the
-- service.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"
    Prelude.. Core.hasStatus 500

-- | The request is not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | Outbound calls to the destination number are not allowed.
_DestinationNotAllowedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DestinationNotAllowedException =
  Core._MatchServiceError
    defaultService
    "DestinationNotAllowedException"
    Prelude.. Core.hasStatus 403

-- | The contact flow has not been published.
_ContactFlowNotPublishedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ContactFlowNotPublishedException =
  Core._MatchServiceError
    defaultService
    "ContactFlowNotPublishedException"
    Prelude.. Core.hasStatus 404

-- | One or more of the specified parameters are not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400
