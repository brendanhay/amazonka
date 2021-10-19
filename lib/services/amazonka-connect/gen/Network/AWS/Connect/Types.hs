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
    _InvalidContactFlowException,
    _OutboundContactNotPermittedException,
    _InvalidParameterException,
    _InvalidRequestException,
    _DuplicateResourceException,
    _UserNotFoundException,
    _ContactFlowNotPublishedException,
    _DestinationNotAllowedException,
    _ContactNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServiceException,
    _ResourceConflictException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceInUseException,

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
    agentStatus_displayOrder,
    agentStatus_state,
    agentStatus_name,
    agentStatus_agentStatusId,
    agentStatus_type,
    agentStatus_agentStatusARN,
    agentStatus_description,
    agentStatus_tags,

    -- * AgentStatusSummary
    AgentStatusSummary (..),
    newAgentStatusSummary,
    agentStatusSummary_arn,
    agentStatusSummary_name,
    agentStatusSummary_id,
    agentStatusSummary_type,

    -- * AnswerMachineDetectionConfig
    AnswerMachineDetectionConfig (..),
    newAnswerMachineDetectionConfig,
    answerMachineDetectionConfig_enableAnswerMachineDetection,
    answerMachineDetectionConfig_awaitAnswerMachinePrompt,

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_value,
    attribute_attributeType,

    -- * ChatMessage
    ChatMessage (..),
    newChatMessage,
    chatMessage_contentType,
    chatMessage_content,

    -- * ContactFlow
    ContactFlow (..),
    newContactFlow,
    contactFlow_arn,
    contactFlow_content,
    contactFlow_name,
    contactFlow_id,
    contactFlow_type,
    contactFlow_description,
    contactFlow_tags,

    -- * ContactFlowSummary
    ContactFlowSummary (..),
    newContactFlowSummary,
    contactFlowSummary_arn,
    contactFlowSummary_name,
    contactFlowSummary_contactFlowType,
    contactFlowSummary_id,

    -- * Credentials
    Credentials (..),
    newCredentials,
    credentials_accessTokenExpiration,
    credentials_accessToken,
    credentials_refreshToken,
    credentials_refreshTokenExpiration,

    -- * CurrentMetric
    CurrentMetric (..),
    newCurrentMetric,
    currentMetric_name,
    currentMetric_unit,

    -- * CurrentMetricData
    CurrentMetricData (..),
    newCurrentMetricData,
    currentMetricData_value,
    currentMetricData_metric,

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
    filters_queues,
    filters_channels,

    -- * HierarchyGroup
    HierarchyGroup (..),
    newHierarchyGroup,
    hierarchyGroup_arn,
    hierarchyGroup_name,
    hierarchyGroup_hierarchyPath,
    hierarchyGroup_id,
    hierarchyGroup_levelId,

    -- * HierarchyGroupSummary
    HierarchyGroupSummary (..),
    newHierarchyGroupSummary,
    hierarchyGroupSummary_arn,
    hierarchyGroupSummary_name,
    hierarchyGroupSummary_id,

    -- * HierarchyLevel
    HierarchyLevel (..),
    newHierarchyLevel,
    hierarchyLevel_arn,
    hierarchyLevel_name,
    hierarchyLevel_id,

    -- * HierarchyLevelUpdate
    HierarchyLevelUpdate (..),
    newHierarchyLevelUpdate,
    hierarchyLevelUpdate_name,

    -- * HierarchyPath
    HierarchyPath (..),
    newHierarchyPath,
    hierarchyPath_levelFive,
    hierarchyPath_levelThree,
    hierarchyPath_levelFour,
    hierarchyPath_levelTwo,
    hierarchyPath_levelOne,

    -- * HierarchyStructure
    HierarchyStructure (..),
    newHierarchyStructure,
    hierarchyStructure_levelFive,
    hierarchyStructure_levelThree,
    hierarchyStructure_levelFour,
    hierarchyStructure_levelTwo,
    hierarchyStructure_levelOne,

    -- * HierarchyStructureUpdate
    HierarchyStructureUpdate (..),
    newHierarchyStructureUpdate,
    hierarchyStructureUpdate_levelFive,
    hierarchyStructureUpdate_levelThree,
    hierarchyStructureUpdate_levelFour,
    hierarchyStructureUpdate_levelTwo,
    hierarchyStructureUpdate_levelOne,

    -- * HistoricalMetric
    HistoricalMetric (..),
    newHistoricalMetric,
    historicalMetric_name,
    historicalMetric_threshold,
    historicalMetric_unit,
    historicalMetric_statistic,

    -- * HistoricalMetricData
    HistoricalMetricData (..),
    newHistoricalMetricData,
    historicalMetricData_value,
    historicalMetricData_metric,

    -- * HistoricalMetricResult
    HistoricalMetricResult (..),
    newHistoricalMetricResult,
    historicalMetricResult_collections,
    historicalMetricResult_dimensions,

    -- * HoursOfOperation
    HoursOfOperation (..),
    newHoursOfOperation,
    hoursOfOperation_config,
    hoursOfOperation_name,
    hoursOfOperation_hoursOfOperationArn,
    hoursOfOperation_hoursOfOperationId,
    hoursOfOperation_timeZone,
    hoursOfOperation_description,
    hoursOfOperation_tags,

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
    hoursOfOperationSummary_name,
    hoursOfOperationSummary_id,

    -- * HoursOfOperationTimeSlice
    HoursOfOperationTimeSlice (..),
    newHoursOfOperationTimeSlice,
    hoursOfOperationTimeSlice_hours,
    hoursOfOperationTimeSlice_minutes,

    -- * Instance
    Instance (..),
    newInstance,
    instance_arn,
    instance_createdTime,
    instance_outboundCallsEnabled,
    instance_inboundCallsEnabled,
    instance_instanceAlias,
    instance_id,
    instance_instanceStatus,
    instance_identityManagementType,
    instance_statusReason,
    instance_serviceRole,

    -- * InstanceStatusReason
    InstanceStatusReason (..),
    newInstanceStatusReason,
    instanceStatusReason_message,

    -- * InstanceStorageConfig
    InstanceStorageConfig (..),
    newInstanceStorageConfig,
    instanceStorageConfig_associationId,
    instanceStorageConfig_kinesisStreamConfig,
    instanceStorageConfig_kinesisVideoStreamConfig,
    instanceStorageConfig_s3Config,
    instanceStorageConfig_kinesisFirehoseConfig,
    instanceStorageConfig_storageType,

    -- * InstanceSummary
    InstanceSummary (..),
    newInstanceSummary,
    instanceSummary_arn,
    instanceSummary_createdTime,
    instanceSummary_outboundCallsEnabled,
    instanceSummary_inboundCallsEnabled,
    instanceSummary_instanceAlias,
    instanceSummary_id,
    instanceSummary_instanceStatus,
    instanceSummary_identityManagementType,
    instanceSummary_serviceRole,

    -- * IntegrationAssociationSummary
    IntegrationAssociationSummary (..),
    newIntegrationAssociationSummary,
    integrationAssociationSummary_instanceId,
    integrationAssociationSummary_sourceType,
    integrationAssociationSummary_sourceApplicationUrl,
    integrationAssociationSummary_integrationAssociationId,
    integrationAssociationSummary_integrationAssociationArn,
    integrationAssociationSummary_sourceApplicationName,
    integrationAssociationSummary_integrationArn,
    integrationAssociationSummary_integrationType,

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
    lexBot_lexRegion,
    lexBot_name,

    -- * LexBotConfig
    LexBotConfig (..),
    newLexBotConfig,
    lexBotConfig_lexBot,
    lexBotConfig_lexV2Bot,

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
    outboundCallerConfig_outboundCallerIdNumberId,
    outboundCallerConfig_outboundCallerIdName,
    outboundCallerConfig_outboundFlowId,

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
    phoneNumberSummary_phoneNumberType,
    phoneNumberSummary_arn,
    phoneNumberSummary_phoneNumber,
    phoneNumberSummary_phoneNumberCountryCode,
    phoneNumberSummary_id,

    -- * PromptSummary
    PromptSummary (..),
    newPromptSummary,
    promptSummary_arn,
    promptSummary_name,
    promptSummary_id,

    -- * Queue
    Queue (..),
    newQueue,
    queue_status,
    queue_queueArn,
    queue_queueId,
    queue_maxContacts,
    queue_name,
    queue_hoursOfOperationId,
    queue_outboundCallerConfig,
    queue_description,
    queue_tags,

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
    queueSummary_arn,
    queueSummary_name,
    queueSummary_id,
    queueSummary_queueType,

    -- * QuickConnect
    QuickConnect (..),
    newQuickConnect,
    quickConnect_name,
    quickConnect_quickConnectId,
    quickConnect_description,
    quickConnect_quickConnectARN,
    quickConnect_tags,
    quickConnect_quickConnectConfig,

    -- * QuickConnectConfig
    QuickConnectConfig (..),
    newQuickConnectConfig,
    quickConnectConfig_queueConfig,
    quickConnectConfig_userConfig,
    quickConnectConfig_phoneConfig,
    quickConnectConfig_quickConnectType,

    -- * QuickConnectSummary
    QuickConnectSummary (..),
    newQuickConnectSummary,
    quickConnectSummary_arn,
    quickConnectSummary_quickConnectType,
    quickConnectSummary_name,
    quickConnectSummary_id,

    -- * Reference
    Reference (..),
    newReference,
    reference_value,
    reference_type,

    -- * RoutingProfile
    RoutingProfile (..),
    newRoutingProfile,
    routingProfile_instanceId,
    routingProfile_routingProfileArn,
    routingProfile_routingProfileId,
    routingProfile_defaultOutboundQueueId,
    routingProfile_name,
    routingProfile_mediaConcurrencies,
    routingProfile_description,
    routingProfile_tags,

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
    routingProfileSummary_name,
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
    securityKey_creationTime,
    securityKey_associationId,
    securityKey_key,

    -- * SecurityProfileSummary
    SecurityProfileSummary (..),
    newSecurityProfileSummary,
    securityProfileSummary_arn,
    securityProfileSummary_name,
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
    user_routingProfileId,
    user_directoryUserId,
    user_arn,
    user_identityInfo,
    user_securityProfileIds,
    user_username,
    user_id,
    user_hierarchyGroupId,
    user_phoneConfig,
    user_tags,

    -- * UserIdentityInfo
    UserIdentityInfo (..),
    newUserIdentityInfo,
    userIdentityInfo_email,
    userIdentityInfo_lastName,
    userIdentityInfo_firstName,

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
    userSummary_username,
    userSummary_id,

    -- * VoiceRecordingConfiguration
    VoiceRecordingConfiguration (..),
    newVoiceRecordingConfiguration,
    voiceRecordingConfiguration_voiceRecordingTrack,
  )
where

import Network.AWS.Connect.Types.AgentStatus
import Network.AWS.Connect.Types.AgentStatusState
import Network.AWS.Connect.Types.AgentStatusSummary
import Network.AWS.Connect.Types.AgentStatusType
import Network.AWS.Connect.Types.AnswerMachineDetectionConfig
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
import Network.AWS.Connect.Types.LexBotConfig
import Network.AWS.Connect.Types.LexV2Bot
import Network.AWS.Connect.Types.LexVersion
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
import Network.AWS.Connect.Types.TrafficType
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
import qualified Network.AWS.Prelude as Prelude
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

-- | The contact flow is not valid.
_InvalidContactFlowException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidContactFlowException =
  Core._MatchServiceError
    defaultService
    "InvalidContactFlowException"
    Prelude.. Core.hasStatus 400

-- | The contact is not permitted.
_OutboundContactNotPermittedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OutboundContactNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OutboundContactNotPermittedException"
    Prelude.. Core.hasStatus 403

-- | One or more of the specified parameters are not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400

-- | The request is not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
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

-- | The contact flow has not been published.
_ContactFlowNotPublishedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ContactFlowNotPublishedException =
  Core._MatchServiceError
    defaultService
    "ContactFlowNotPublishedException"
    Prelude.. Core.hasStatus 404

-- | Outbound calls to the destination number are not allowed.
_DestinationNotAllowedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DestinationNotAllowedException =
  Core._MatchServiceError
    defaultService
    "DestinationNotAllowedException"
    Prelude.. Core.hasStatus 403

-- | The contact with the specified ID is not active or does not exist.
_ContactNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ContactNotFoundException =
  Core._MatchServiceError
    defaultService
    "ContactNotFoundException"
    Prelude.. Core.hasStatus 410

-- | The service quota has been exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The throttling limit has been exceeded.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Request processing failed because of an error or failure with the
-- service.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"
    Prelude.. Core.hasStatus 500

-- | A resource already has that name.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Prelude.. Core.hasStatus 409

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The allowed limit for the resource has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | That resource is already in use. Please try another.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 409
