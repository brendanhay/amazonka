-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types
  ( -- * Service configuration
    connectService,

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
    Attribute (..),
    mkAttribute,
    aValue,
    aAttributeType,

    -- * ChatMessage
    ChatMessage (..),
    mkChatMessage,
    cmContentType,
    cmContent,

    -- * ContactFlow
    ContactFlow (..),
    mkContactFlow,
    cfARN,
    cfContent,
    cfName,
    cfId,
    cfType,
    cfDescription,
    cfTags,

    -- * ContactFlowSummary
    ContactFlowSummary (..),
    mkContactFlowSummary,
    cfsARN,
    cfsName,
    cfsContactFlowType,
    cfsId,

    -- * Credentials
    Credentials (..),
    mkCredentials,
    cAccessTokenExpiration,
    cAccessToken,
    cRefreshToken,
    cRefreshTokenExpiration,

    -- * CurrentMetric
    CurrentMetric (..),
    mkCurrentMetric,
    cmName,
    cmUnit,

    -- * CurrentMetricData
    CurrentMetricData (..),
    mkCurrentMetricData,
    cmdValue,
    cmdMetric,

    -- * CurrentMetricResult
    CurrentMetricResult (..),
    mkCurrentMetricResult,
    cmrCollections,
    cmrDimensions,

    -- * Dimensions
    Dimensions (..),
    mkDimensions,
    dChannel,
    dQueue,

    -- * EncryptionConfig
    EncryptionConfig (..),
    mkEncryptionConfig,
    ecEncryptionType,
    ecKeyId,

    -- * Filters
    Filters (..),
    mkFilters,
    fQueues,
    fChannels,

    -- * HierarchyGroup
    HierarchyGroup (..),
    mkHierarchyGroup,
    hgARN,
    hgName,
    hgHierarchyPath,
    hgId,
    hgLevelId,

    -- * HierarchyGroupSummary
    HierarchyGroupSummary (..),
    mkHierarchyGroupSummary,
    hgsARN,
    hgsName,
    hgsId,

    -- * HierarchyLevel
    HierarchyLevel (..),
    mkHierarchyLevel,
    hlARN,
    hlName,
    hlId,

    -- * HierarchyLevelUpdate
    HierarchyLevelUpdate (..),
    mkHierarchyLevelUpdate,
    hluName,

    -- * HierarchyPath
    HierarchyPath (..),
    mkHierarchyPath,
    hpLevelFive,
    hpLevelThree,
    hpLevelFour,
    hpLevelTwo,
    hpLevelOne,

    -- * HierarchyStructure
    HierarchyStructure (..),
    mkHierarchyStructure,
    hsLevelFive,
    hsLevelThree,
    hsLevelFour,
    hsLevelTwo,
    hsLevelOne,

    -- * HierarchyStructureUpdate
    HierarchyStructureUpdate (..),
    mkHierarchyStructureUpdate,
    hsuLevelFive,
    hsuLevelThree,
    hsuLevelFour,
    hsuLevelTwo,
    hsuLevelOne,

    -- * HistoricalMetric
    HistoricalMetric (..),
    mkHistoricalMetric,
    hmName,
    hmThreshold,
    hmUnit,
    hmStatistic,

    -- * HistoricalMetricData
    HistoricalMetricData (..),
    mkHistoricalMetricData,
    hmdValue,
    hmdMetric,

    -- * HistoricalMetricResult
    HistoricalMetricResult (..),
    mkHistoricalMetricResult,
    hmrCollections,
    hmrDimensions,

    -- * HoursOfOperationSummary
    HoursOfOperationSummary (..),
    mkHoursOfOperationSummary,
    hoosARN,
    hoosName,
    hoosId,

    -- * Instance
    Instance (..),
    mkInstance,
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
    InstanceStatusReason (..),
    mkInstanceStatusReason,
    isrMessage,

    -- * InstanceStorageConfig
    InstanceStorageConfig (..),
    mkInstanceStorageConfig,
    iscAssociationId,
    iscKinesisStreamConfig,
    iscKinesisVideoStreamConfig,
    iscS3Config,
    iscKinesisFirehoseConfig,
    iscStorageType,

    -- * InstanceSummary
    InstanceSummary (..),
    mkInstanceSummary,
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
    KinesisFirehoseConfig (..),
    mkKinesisFirehoseConfig,
    kfcFirehoseARN,

    -- * KinesisStreamConfig
    KinesisStreamConfig (..),
    mkKinesisStreamConfig,
    kscStreamARN,

    -- * KinesisVideoStreamConfig
    KinesisVideoStreamConfig (..),
    mkKinesisVideoStreamConfig,
    kvscPrefix,
    kvscRetentionPeriodHours,
    kvscEncryptionConfig,

    -- * LexBot
    LexBot (..),
    mkLexBot,
    lbLexRegion,
    lbName,

    -- * MediaConcurrency
    MediaConcurrency (..),
    mkMediaConcurrency,
    mcChannel,
    mcConcurrency,

    -- * ParticipantDetails
    ParticipantDetails (..),
    mkParticipantDetails,
    pdDisplayName,

    -- * PhoneNumberSummary
    PhoneNumberSummary (..),
    mkPhoneNumberSummary,
    pnsPhoneNumberType,
    pnsARN,
    pnsPhoneNumber,
    pnsPhoneNumberCountryCode,
    pnsId,

    -- * PromptSummary
    PromptSummary (..),
    mkPromptSummary,
    psARN,
    psName,
    psId,

    -- * QueueReference
    QueueReference (..),
    mkQueueReference,
    qrARN,
    qrId,

    -- * QueueSummary
    QueueSummary (..),
    mkQueueSummary,
    qsARN,
    qsName,
    qsId,
    qsQueueType,

    -- * RoutingProfile
    RoutingProfile (..),
    mkRoutingProfile,
    rpInstanceId,
    rpRoutingProfileARN,
    rpRoutingProfileId,
    rpDefaultOutboundQueueId,
    rpName,
    rpMediaConcurrencies,
    rpDescription,
    rpTags,

    -- * RoutingProfileQueueConfig
    RoutingProfileQueueConfig (..),
    mkRoutingProfileQueueConfig,
    rpqcQueueReference,
    rpqcPriority,
    rpqcDelay,

    -- * RoutingProfileQueueConfigSummary
    RoutingProfileQueueConfigSummary (..),
    mkRoutingProfileQueueConfigSummary,
    rpqcsQueueId,
    rpqcsQueueARN,
    rpqcsQueueName,
    rpqcsPriority,
    rpqcsDelay,
    rpqcsChannel,

    -- * RoutingProfileQueueReference
    RoutingProfileQueueReference (..),
    mkRoutingProfileQueueReference,
    rpqrQueueId,
    rpqrChannel,

    -- * RoutingProfileSummary
    RoutingProfileSummary (..),
    mkRoutingProfileSummary,
    rpsARN,
    rpsName,
    rpsId,

    -- * S3Config
    S3Config (..),
    mkS3Config,
    scEncryptionConfig,
    scBucketName,
    scBucketPrefix,

    -- * SecurityKey
    SecurityKey (..),
    mkSecurityKey,
    skCreationTime,
    skAssociationId,
    skKey,

    -- * SecurityProfileSummary
    SecurityProfileSummary (..),
    mkSecurityProfileSummary,
    spsARN,
    spsName,
    spsId,

    -- * Threshold
    Threshold (..),
    mkThreshold,
    tThresholdValue,
    tComparison,

    -- * User
    User (..),
    mkUser,
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
    UserIdentityInfo (..),
    mkUserIdentityInfo,
    uiiEmail,
    uiiLastName,
    uiiFirstName,

    -- * UserPhoneConfig
    UserPhoneConfig (..),
    mkUserPhoneConfig,
    upcAutoAccept,
    upcAfterContactWorkTimeLimit,
    upcDeskPhoneNumber,
    upcPhoneType,

    -- * UserSummary
    UserSummary (..),
    mkUserSummary,
    usARN,
    usUsername,
    usId,

    -- * VoiceRecordingConfiguration
    VoiceRecordingConfiguration (..),
    mkVoiceRecordingConfiguration,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-08-08@ of the Amazon Connect Service SDK configuration.
connectService :: Lude.Service
connectService =
  Lude.Service
    { Lude._svcAbbrev = "Connect",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "connect",
      Lude._svcVersion = "2017-08-08",
      Lude._svcEndpoint = Lude.defaultEndpoint connectService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Connect",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
