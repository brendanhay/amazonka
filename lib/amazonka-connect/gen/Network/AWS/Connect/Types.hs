{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types
  ( -- * Service Configuration
    connect,

    -- * Errors

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

    -- * InstanceAttributeType
    InstanceAttributeType (..),

    -- * InstanceStatus
    InstanceStatus (..),

    -- * InstanceStorageResourceType
    InstanceStorageResourceType (..),

    -- * PhoneNumberCountryCode
    PhoneNumberCountryCode (..),

    -- * PhoneNumberType
    PhoneNumberType (..),

    -- * PhoneType
    PhoneType (..),

    -- * QueueType
    QueueType (..),

    -- * Statistic
    Statistic (..),

    -- * StorageType
    StorageType (..),

    -- * Unit
    Unit (..),

    -- * VoiceRecordingTrack
    VoiceRecordingTrack (..),

    -- * Attribute
    Attribute,
    attribute,
    aValue,
    aAttributeType,

    -- * ChatMessage
    ChatMessage,
    chatMessage,
    cmContentType,
    cmContent,

    -- * ContactFlow
    ContactFlow,
    contactFlow,
    cfARN,
    cfContent,
    cfName,
    cfId,
    cfType,
    cfDescription,
    cfTags,

    -- * ContactFlowSummary
    ContactFlowSummary,
    contactFlowSummary,
    cfsARN,
    cfsName,
    cfsContactFlowType,
    cfsId,

    -- * Credentials
    Credentials,
    credentials,
    cAccessTokenExpiration,
    cAccessToken,
    cRefreshToken,
    cRefreshTokenExpiration,

    -- * CurrentMetric
    CurrentMetric,
    currentMetric,
    cmName,
    cmUnit,

    -- * CurrentMetricData
    CurrentMetricData,
    currentMetricData,
    cmdValue,
    cmdMetric,

    -- * CurrentMetricResult
    CurrentMetricResult,
    currentMetricResult,
    cmrCollections,
    cmrDimensions,

    -- * Dimensions
    Dimensions,
    dimensions,
    dChannel,
    dQueue,

    -- * EncryptionConfig
    EncryptionConfig,
    encryptionConfig,
    ecEncryptionType,
    ecKeyId,

    -- * Filters
    Filters,
    filters,
    fQueues,
    fChannels,

    -- * HierarchyGroup
    HierarchyGroup,
    hierarchyGroup,
    hgARN,
    hgName,
    hgHierarchyPath,
    hgId,
    hgLevelId,

    -- * HierarchyGroupSummary
    HierarchyGroupSummary,
    hierarchyGroupSummary,
    hgsARN,
    hgsName,
    hgsId,

    -- * HierarchyLevel
    HierarchyLevel,
    hierarchyLevel,
    hlARN,
    hlName,
    hlId,

    -- * HierarchyLevelUpdate
    HierarchyLevelUpdate,
    hierarchyLevelUpdate,
    hluName,

    -- * HierarchyPath
    HierarchyPath,
    hierarchyPath,
    hpLevelFive,
    hpLevelThree,
    hpLevelFour,
    hpLevelTwo,
    hpLevelOne,

    -- * HierarchyStructure
    HierarchyStructure,
    hierarchyStructure,
    hsLevelFive,
    hsLevelThree,
    hsLevelFour,
    hsLevelTwo,
    hsLevelOne,

    -- * HierarchyStructureUpdate
    HierarchyStructureUpdate,
    hierarchyStructureUpdate,
    hsuLevelFive,
    hsuLevelThree,
    hsuLevelFour,
    hsuLevelTwo,
    hsuLevelOne,

    -- * HistoricalMetric
    HistoricalMetric,
    historicalMetric,
    hmName,
    hmThreshold,
    hmUnit,
    hmStatistic,

    -- * HistoricalMetricData
    HistoricalMetricData,
    historicalMetricData,
    hmdValue,
    hmdMetric,

    -- * HistoricalMetricResult
    HistoricalMetricResult,
    historicalMetricResult,
    hmrCollections,
    hmrDimensions,

    -- * HoursOfOperationSummary
    HoursOfOperationSummary,
    hoursOfOperationSummary,
    hoosARN,
    hoosName,
    hoosId,

    -- * Instance
    Instance,
    instance',
    iARN,
    iCreatedTime,
    iOutboundCallsEnabled,
    iInboundCallsEnabled,
    iInstanceAlias,
    iId,
    iInstanceStatus,
    iIdentityManagementType,
    iStatusReason,
    iServiceRole,

    -- * InstanceStatusReason
    InstanceStatusReason,
    instanceStatusReason,
    isrMessage,

    -- * InstanceStorageConfig
    InstanceStorageConfig,
    instanceStorageConfig,
    iscAssociationId,
    iscKinesisStreamConfig,
    iscKinesisVideoStreamConfig,
    iscS3Config,
    iscKinesisFirehoseConfig,
    iscStorageType,

    -- * InstanceSummary
    InstanceSummary,
    instanceSummary,
    isARN,
    isCreatedTime,
    isOutboundCallsEnabled,
    isInboundCallsEnabled,
    isInstanceAlias,
    isId,
    isInstanceStatus,
    isIdentityManagementType,
    isServiceRole,

    -- * KinesisFirehoseConfig
    KinesisFirehoseConfig,
    kinesisFirehoseConfig,
    kfcFirehoseARN,

    -- * KinesisStreamConfig
    KinesisStreamConfig,
    kinesisStreamConfig,
    kscStreamARN,

    -- * KinesisVideoStreamConfig
    KinesisVideoStreamConfig,
    kinesisVideoStreamConfig,
    kvscPrefix,
    kvscRetentionPeriodHours,
    kvscEncryptionConfig,

    -- * LexBot
    LexBot,
    lexBot,
    lbLexRegion,
    lbName,

    -- * MediaConcurrency
    MediaConcurrency,
    mediaConcurrency,
    mcChannel,
    mcConcurrency,

    -- * ParticipantDetails
    ParticipantDetails,
    participantDetails,
    pdDisplayName,

    -- * PhoneNumberSummary
    PhoneNumberSummary,
    phoneNumberSummary,
    pnsPhoneNumberType,
    pnsARN,
    pnsPhoneNumber,
    pnsPhoneNumberCountryCode,
    pnsId,

    -- * PromptSummary
    PromptSummary,
    promptSummary,
    psARN,
    psName,
    psId,

    -- * QueueReference
    QueueReference,
    queueReference,
    qrARN,
    qrId,

    -- * QueueSummary
    QueueSummary,
    queueSummary,
    qsARN,
    qsName,
    qsId,
    qsQueueType,

    -- * RoutingProfile
    RoutingProfile,
    routingProfile,
    rpInstanceId,
    rpRoutingProfileARN,
    rpRoutingProfileId,
    rpDefaultOutboundQueueId,
    rpName,
    rpMediaConcurrencies,
    rpDescription,
    rpTags,

    -- * RoutingProfileQueueConfig
    RoutingProfileQueueConfig,
    routingProfileQueueConfig,
    rpqcQueueReference,
    rpqcPriority,
    rpqcDelay,

    -- * RoutingProfileQueueConfigSummary
    RoutingProfileQueueConfigSummary,
    routingProfileQueueConfigSummary,
    rpqcsQueueId,
    rpqcsQueueARN,
    rpqcsQueueName,
    rpqcsPriority,
    rpqcsDelay,
    rpqcsChannel,

    -- * RoutingProfileQueueReference
    RoutingProfileQueueReference,
    routingProfileQueueReference,
    rpqrQueueId,
    rpqrChannel,

    -- * RoutingProfileSummary
    RoutingProfileSummary,
    routingProfileSummary,
    rpsARN,
    rpsName,
    rpsId,

    -- * S3Config
    S3Config,
    s3Config,
    scEncryptionConfig,
    scBucketName,
    scBucketPrefix,

    -- * SecurityKey
    SecurityKey,
    securityKey,
    skCreationTime,
    skAssociationId,
    skKey,

    -- * SecurityProfileSummary
    SecurityProfileSummary,
    securityProfileSummary,
    spsARN,
    spsName,
    spsId,

    -- * Threshold
    Threshold,
    threshold,
    tThresholdValue,
    tComparison,

    -- * User
    User,
    user,
    uRoutingProfileId,
    uDirectoryUserId,
    uARN,
    uIdentityInfo,
    uSecurityProfileIds,
    uUsername,
    uId,
    uHierarchyGroupId,
    uPhoneConfig,
    uTags,

    -- * UserIdentityInfo
    UserIdentityInfo,
    userIdentityInfo,
    uiiEmail,
    uiiLastName,
    uiiFirstName,

    -- * UserPhoneConfig
    UserPhoneConfig,
    userPhoneConfig,
    upcAutoAccept,
    upcAfterContactWorkTimeLimit,
    upcDeskPhoneNumber,
    upcPhoneType,

    -- * UserSummary
    UserSummary,
    userSummary,
    usARN,
    usUsername,
    usId,

    -- * VoiceRecordingConfiguration
    VoiceRecordingConfiguration,
    voiceRecordingConfiguration,
    vrcVoiceRecordingTrack,
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
import Network.AWS.Connect.Types.HoursOfOperationSummary
import Network.AWS.Connect.Types.Instance
import Network.AWS.Connect.Types.InstanceAttributeType
import Network.AWS.Connect.Types.InstanceStatus
import Network.AWS.Connect.Types.InstanceStatusReason
import Network.AWS.Connect.Types.InstanceStorageConfig
import Network.AWS.Connect.Types.InstanceStorageResourceType
import Network.AWS.Connect.Types.InstanceSummary
import Network.AWS.Connect.Types.KinesisFirehoseConfig
import Network.AWS.Connect.Types.KinesisStreamConfig
import Network.AWS.Connect.Types.KinesisVideoStreamConfig
import Network.AWS.Connect.Types.LexBot
import Network.AWS.Connect.Types.MediaConcurrency
import Network.AWS.Connect.Types.ParticipantDetails
import Network.AWS.Connect.Types.PhoneNumberCountryCode
import Network.AWS.Connect.Types.PhoneNumberSummary
import Network.AWS.Connect.Types.PhoneNumberType
import Network.AWS.Connect.Types.PhoneType
import Network.AWS.Connect.Types.PromptSummary
import Network.AWS.Connect.Types.QueueReference
import Network.AWS.Connect.Types.QueueSummary
import Network.AWS.Connect.Types.QueueType
import Network.AWS.Connect.Types.RoutingProfile
import Network.AWS.Connect.Types.RoutingProfileQueueConfig
import Network.AWS.Connect.Types.RoutingProfileQueueConfigSummary
import Network.AWS.Connect.Types.RoutingProfileQueueReference
import Network.AWS.Connect.Types.RoutingProfileSummary
import Network.AWS.Connect.Types.S3Config
import Network.AWS.Connect.Types.SecurityKey
import Network.AWS.Connect.Types.SecurityProfileSummary
import Network.AWS.Connect.Types.Statistic
import Network.AWS.Connect.Types.StorageType
import Network.AWS.Connect.Types.Threshold
import Network.AWS.Connect.Types.Unit
import Network.AWS.Connect.Types.User
import Network.AWS.Connect.Types.UserIdentityInfo
import Network.AWS.Connect.Types.UserPhoneConfig
import Network.AWS.Connect.Types.UserSummary
import Network.AWS.Connect.Types.VoiceRecordingConfiguration
import Network.AWS.Connect.Types.VoiceRecordingTrack
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-08-08@ of the Amazon Connect Service SDK configuration.
connect :: Service
connect =
  Service
    { _svcAbbrev = "Connect",
      _svcSigner = v4,
      _svcPrefix = "connect",
      _svcVersion = "2017-08-08",
      _svcEndpoint = defaultEndpoint connect,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Connect",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
