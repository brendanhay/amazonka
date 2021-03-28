-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidContactFlowException
    , _OutboundContactNotPermittedException
    , _InvalidParameterException
    , _InvalidRequestException
    , _DuplicateResourceException
    , _UserNotFoundException
    , _ContactFlowNotPublishedException
    , _DestinationNotAllowedException
    , _ContactNotFoundException
    , _ServiceQuotaExceededException
    , _ThrottlingException
    , _InternalServiceException
    , _ResourceConflictException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _ResourceInUseException

    -- * AgentFirstName
    , AgentFirstName (..)

    -- * AssociationId
    , AssociationId (..)

    -- * ParticipantToken
    , ParticipantToken (..)

    -- * InstanceId
    , InstanceId (..)

    -- * ContactFlowDescription
    , ContactFlowDescription (..)

    -- * Origin
    , Origin (..)

    -- * Attribute
    , Attribute (..)
    , mkAttribute
    , aAttributeType
    , aValue

    -- * HierarchyGroupSummary
    , HierarchyGroupSummary (..)
    , mkHierarchyGroupSummary
    , hgsArn
    , hgsId
    , hgsName

    -- * RoutingProfileQueueConfig
    , RoutingProfileQueueConfig (..)
    , mkRoutingProfileQueueConfig
    , rpqcQueueReference
    , rpqcPriority
    , rpqcDelay

    -- * Email
    , Email (..)

    -- * AgentLastName
    , AgentLastName (..)

    -- * DirectoryId
    , DirectoryId (..)

    -- * PhoneNumberType
    , PhoneNumberType (..)

    -- * EncryptionType
    , EncryptionType (..)

    -- * ChatContentType
    , ChatContentType (..)

    -- * AttributeValue
    , AttributeValue (..)

    -- * HistoricalMetricName
    , HistoricalMetricName (..)

    -- * FunctionArn
    , FunctionArn (..)

    -- * HierarchyStructure
    , HierarchyStructure (..)
    , mkHierarchyStructure
    , hsLevelFive
    , hsLevelFour
    , hsLevelOne
    , hsLevelThree
    , hsLevelTwo

    -- * InstanceStorageConfig
    , InstanceStorageConfig (..)
    , mkInstanceStorageConfig
    , iscStorageType
    , iscAssociationId
    , iscKinesisFirehoseConfig
    , iscKinesisStreamConfig
    , iscKinesisVideoStreamConfig
    , iscS3Config

    -- * KeyId
    , KeyId (..)

    -- * ParticipantId
    , ParticipantId (..)

    -- * InstanceStorageResourceType
    , InstanceStorageResourceType (..)

    -- * ContactFlow
    , ContactFlow (..)
    , mkContactFlow
    , cfArn
    , cfContent
    , cfDescription
    , cfId
    , cfName
    , cfTags
    , cfType

    -- * PhoneNumberId
    , PhoneNumberId (..)

    -- * ClientToken
    , ClientToken (..)

    -- * CurrentMetricName
    , CurrentMetricName (..)

    -- * RoutingProfileId
    , RoutingProfileId (..)

    -- * DirectoryUserId
    , DirectoryUserId (..)

    -- * ChatMessage
    , ChatMessage (..)
    , mkChatMessage
    , cmContentType
    , cmContent

    -- * SecurityToken
    , SecurityToken (..)

    -- * InstanceSummary
    , InstanceSummary (..)
    , mkInstanceSummary
    , isArn
    , isCreatedTime
    , isId
    , isIdentityManagementType
    , isInboundCallsEnabled
    , isInstanceAlias
    , isInstanceStatus
    , isOutboundCallsEnabled
    , isServiceRole

    -- * ARN
    , ARN (..)

    -- * RoutingProfileQueueReference
    , RoutingProfileQueueReference (..)
    , mkRoutingProfileQueueReference
    , rpqrQueueId
    , rpqrChannel

    -- * LexBot
    , LexBot (..)
    , mkLexBot
    , lbLexRegion
    , lbName

    -- * PEM
    , PEM (..)

    -- * Credentials
    , Credentials (..)
    , mkCredentials
    , cAccessToken
    , cAccessTokenExpiration
    , cRefreshToken
    , cRefreshTokenExpiration

    -- * PromptSummary
    , PromptSummary (..)
    , mkPromptSummary
    , psArn
    , psId
    , psName

    -- * HierarchyGroup
    , HierarchyGroup (..)
    , mkHierarchyGroup
    , hgArn
    , hgHierarchyPath
    , hgId
    , hgLevelId
    , hgName

    -- * RoutingProfileName
    , RoutingProfileName (..)

    -- * Prefix
    , Prefix (..)

    -- * RoutingProfileQueueConfigSummary
    , RoutingProfileQueueConfigSummary (..)
    , mkRoutingProfileQueueConfigSummary
    , rpqcsQueueId
    , rpqcsQueueArn
    , rpqcsQueueName
    , rpqcsPriority
    , rpqcsDelay
    , rpqcsChannel

    -- * VoiceRecordingConfiguration
    , VoiceRecordingConfiguration (..)
    , mkVoiceRecordingConfiguration
    , vrcVoiceRecordingTrack

    -- * Filters
    , Filters (..)
    , mkFilters
    , fChannels
    , fQueues

    -- * ContactFlowContent
    , ContactFlowContent (..)

    -- * PhoneNumberSummary
    , PhoneNumberSummary (..)
    , mkPhoneNumberSummary
    , pnsArn
    , pnsId
    , pnsPhoneNumber
    , pnsPhoneNumberCountryCode
    , pnsPhoneNumberType

    -- * RoutingProfileSummary
    , RoutingProfileSummary (..)
    , mkRoutingProfileSummary
    , rpsArn
    , rpsId
    , rpsName

    -- * Channel
    , Channel (..)

    -- * QueueName
    , QueueName (..)

    -- * KinesisStreamConfig
    , KinesisStreamConfig (..)
    , mkKinesisStreamConfig
    , kscStreamArn

    -- * BotName
    , BotName (..)

    -- * HierarchyLevelName
    , HierarchyLevelName (..)

    -- * InstanceAttributeValue
    , InstanceAttributeValue (..)

    -- * HierarchyLevel
    , HierarchyLevel (..)
    , mkHierarchyLevel
    , hlArn
    , hlId
    , hlName

    -- * InstanceStatusReason
    , InstanceStatusReason (..)
    , mkInstanceStatusReason
    , isrMessage

    -- * TagValue
    , TagValue (..)

    -- * RoutingProfileDescription
    , RoutingProfileDescription (..)

    -- * DirectoryType
    , DirectoryType (..)

    -- * User
    , User (..)
    , mkUser
    , uArn
    , uDirectoryUserId
    , uHierarchyGroupId
    , uId
    , uIdentityInfo
    , uPhoneConfig
    , uRoutingProfileId
    , uSecurityProfileIds
    , uTags
    , uUsername

    -- * LexRegion
    , LexRegion (..)

    -- * MediaConcurrency
    , MediaConcurrency (..)
    , mkMediaConcurrency
    , mcChannel
    , mcConcurrency

    -- * Comparison
    , Comparison (..)

    -- * UserIdentityInfo
    , UserIdentityInfo (..)
    , mkUserIdentityInfo
    , uiiEmail
    , uiiFirstName
    , uiiLastName

    -- * BucketName
    , BucketName (..)

    -- * UserId
    , UserId (..)

    -- * NextToken
    , NextToken (..)

    -- * CurrentMetricResult
    , CurrentMetricResult (..)
    , mkCurrentMetricResult
    , cmrCollections
    , cmrDimensions

    -- * VoiceRecordingTrack
    , VoiceRecordingTrack (..)

    -- * PhoneType
    , PhoneType (..)

    -- * QueueId
    , QueueId (..)

    -- * Grouping
    , Grouping (..)

    -- * ContactFlowId
    , ContactFlowId (..)

    -- * DirectoryAlias
    , DirectoryAlias (..)

    -- * KinesisVideoStreamConfig
    , KinesisVideoStreamConfig (..)
    , mkKinesisVideoStreamConfig
    , kvscPrefix
    , kvscRetentionPeriodHours
    , kvscEncryptionConfig

    -- * AgentUsername
    , AgentUsername (..)

    -- * RoutingProfile
    , RoutingProfile (..)
    , mkRoutingProfile
    , rpDefaultOutboundQueueId
    , rpDescription
    , rpInstanceId
    , rpMediaConcurrencies
    , rpName
    , rpRoutingProfileArn
    , rpRoutingProfileId
    , rpTags

    -- * UserPhoneConfig
    , UserPhoneConfig (..)
    , mkUserPhoneConfig
    , upcPhoneType
    , upcAfterContactWorkTimeLimit
    , upcAutoAccept
    , upcDeskPhoneNumber

    -- * PhoneNumber
    , PhoneNumber (..)

    -- * Threshold
    , Threshold (..)
    , mkThreshold
    , tComparison
    , tThresholdValue

    -- * HierarchyGroupName
    , HierarchyGroupName (..)

    -- * Password
    , Password (..)

    -- * PhoneNumberCountryCode
    , PhoneNumberCountryCode (..)

    -- * HierarchyPath
    , HierarchyPath (..)
    , mkHierarchyPath
    , hpLevelFive
    , hpLevelFour
    , hpLevelOne
    , hpLevelThree
    , hpLevelTwo

    -- * ContactFlowType
    , ContactFlowType (..)

    -- * QueueReference
    , QueueReference (..)
    , mkQueueReference
    , qrArn
    , qrId

    -- * SecurityProfileName
    , SecurityProfileName (..)

    -- * S3Config
    , S3Config (..)
    , mkS3Config
    , scBucketName
    , scBucketPrefix
    , scEncryptionConfig

    -- * HoursOfOperationId
    , HoursOfOperationId (..)

    -- * ContactFlowName
    , ContactFlowName (..)

    -- * HierarchyGroupId
    , HierarchyGroupId (..)

    -- * UserSummary
    , UserSummary (..)
    , mkUserSummary
    , usArn
    , usId
    , usUsername

    -- * InstanceStatus
    , InstanceStatus (..)

    -- * DisplayName
    , DisplayName (..)

    -- * ContactId
    , ContactId (..)

    -- * ParticipantDetails
    , ParticipantDetails (..)
    , mkParticipantDetails
    , pdDisplayName

    -- * EncryptionConfig
    , EncryptionConfig (..)
    , mkEncryptionConfig
    , ecEncryptionType
    , ecKeyId

    -- * CurrentMetric
    , CurrentMetric (..)
    , mkCurrentMetric
    , cmName
    , cmUnit

    -- * QueueSummary
    , QueueSummary (..)
    , mkQueueSummary
    , qsArn
    , qsId
    , qsName
    , qsQueueType

    -- * InstanceAttributeType
    , InstanceAttributeType (..)

    -- * CurrentMetricData
    , CurrentMetricData (..)
    , mkCurrentMetricData
    , cmdMetric
    , cmdValue

    -- * TagKey
    , TagKey (..)

    -- * HierarchyLevelUpdate
    , HierarchyLevelUpdate (..)
    , mkHierarchyLevelUpdate
    , hluName

    -- * HistoricalMetric
    , HistoricalMetric (..)
    , mkHistoricalMetric
    , hmName
    , hmStatistic
    , hmThreshold
    , hmUnit

    -- * HistoricalMetricData
    , HistoricalMetricData (..)
    , mkHistoricalMetricData
    , hmdMetric
    , hmdValue

    -- * SecurityProfileId
    , SecurityProfileId (..)

    -- * HoursOfOperationName
    , HoursOfOperationName (..)

    -- * HierarchyStructureUpdate
    , HierarchyStructureUpdate (..)
    , mkHierarchyStructureUpdate
    , hsuLevelFive
    , hsuLevelFour
    , hsuLevelOne
    , hsuLevelThree
    , hsuLevelTwo

    -- * AttributeName
    , AttributeName (..)

    -- * Dimensions
    , Dimensions (..)
    , mkDimensions
    , dChannel
    , dQueue

    -- * KinesisFirehoseConfig
    , KinesisFirehoseConfig (..)
    , mkKinesisFirehoseConfig
    , kfcFirehoseArn

    -- * HoursOfOperationSummary
    , HoursOfOperationSummary (..)
    , mkHoursOfOperationSummary
    , hoosArn
    , hoosId
    , hoosName

    -- * Unit
    , Unit (..)

    -- * Statistic
    , Statistic (..)

    -- * ContactFlowSummary
    , ContactFlowSummary (..)
    , mkContactFlowSummary
    , cfsArn
    , cfsContactFlowType
    , cfsId
    , cfsName

    -- * QueueType
    , QueueType (..)

    -- * Instance
    , Instance (..)
    , mkInstance
    , iArn
    , iCreatedTime
    , iId
    , iIdentityManagementType
    , iInboundCallsEnabled
    , iInstanceAlias
    , iInstanceStatus
    , iOutboundCallsEnabled
    , iServiceRole
    , iStatusReason

    -- * HistoricalMetricResult
    , HistoricalMetricResult (..)
    , mkHistoricalMetricResult
    , hmrCollections
    , hmrDimensions

    -- * SecurityKey
    , SecurityKey (..)
    , mkSecurityKey
    , skAssociationId
    , skCreationTime
    , skKey

    -- * StorageType
    , StorageType (..)

    -- * SecurityProfileSummary
    , SecurityProfileSummary (..)
    , mkSecurityProfileSummary
    , spsArn
    , spsId
    , spsName

    -- * InitialContactId
    , InitialContactId (..)

    -- * Arn
    , Arn (..)

    -- * Id
    , Id (..)

    -- * Name
    , Name (..)

    -- * InstanceAlias
    , InstanceAlias (..)

    -- * Content
    , Content (..)

    -- * Description
    , Description (..)

    -- * DefaultOutboundQueueId
    , DefaultOutboundQueueId (..)

    -- * ServiceRole
    , ServiceRole (..)

    -- * LevelId
    , LevelId (..)

    -- * Username
    , Username (..)

    -- * ParentGroupId
    , ParentGroupId (..)

    -- * DeskPhoneNumber
    , DeskPhoneNumber (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Connect.Types.AgentFirstName
  
import Network.AWS.Connect.Types.AssociationId
  
import Network.AWS.Connect.Types.ParticipantToken
  
import Network.AWS.Connect.Types.InstanceId
  
import Network.AWS.Connect.Types.ContactFlowDescription
  
  
  
import Network.AWS.Connect.Types.Origin
  
import Network.AWS.Connect.Types.Attribute
  
import Network.AWS.Connect.Types.HierarchyGroupSummary
  
import Network.AWS.Connect.Types.RoutingProfileQueueConfig
  
import Network.AWS.Connect.Types.Email
  
import Network.AWS.Connect.Types.AgentLastName
  
import Network.AWS.Connect.Types.DirectoryId
  
import Network.AWS.Connect.Types.PhoneNumberType
  
import Network.AWS.Connect.Types.EncryptionType
  
import Network.AWS.Connect.Types.ChatContentType
  
  
  
import Network.AWS.Connect.Types.AttributeValue
  
import Network.AWS.Connect.Types.HistoricalMetricName
  
import Network.AWS.Connect.Types.FunctionArn
  
  
import Network.AWS.Connect.Types.HierarchyStructure
  
import Network.AWS.Connect.Types.InstanceStorageConfig
  
import Network.AWS.Connect.Types.KeyId
  
import Network.AWS.Connect.Types.ParticipantId
  
import Network.AWS.Connect.Types.InstanceStorageResourceType
  
import Network.AWS.Connect.Types.ContactFlow
  
import Network.AWS.Connect.Types.PhoneNumberId
  
import Network.AWS.Connect.Types.ClientToken
  
import Network.AWS.Connect.Types.CurrentMetricName
  
import Network.AWS.Connect.Types.RoutingProfileId
  
import Network.AWS.Connect.Types.DirectoryUserId
  
import Network.AWS.Connect.Types.ChatMessage
  
import Network.AWS.Connect.Types.SecurityToken
  
import Network.AWS.Connect.Types.InstanceSummary
  
  
import Network.AWS.Connect.Types.ARN
  
import Network.AWS.Connect.Types.RoutingProfileQueueReference
  
import Network.AWS.Connect.Types.LexBot
  
import Network.AWS.Connect.Types.PEM
  
import Network.AWS.Connect.Types.Credentials
  
  
import Network.AWS.Connect.Types.PromptSummary
  
import Network.AWS.Connect.Types.HierarchyGroup
  
import Network.AWS.Connect.Types.RoutingProfileName
  
import Network.AWS.Connect.Types.Prefix
  
import Network.AWS.Connect.Types.RoutingProfileQueueConfigSummary
  
import Network.AWS.Connect.Types.VoiceRecordingConfiguration
  
import Network.AWS.Connect.Types.Filters
  
import Network.AWS.Connect.Types.ContactFlowContent
  
import Network.AWS.Connect.Types.PhoneNumberSummary
  
import Network.AWS.Connect.Types.RoutingProfileSummary
  
import Network.AWS.Connect.Types.Channel
  
import Network.AWS.Connect.Types.QueueName
  
import Network.AWS.Connect.Types.KinesisStreamConfig
  
import Network.AWS.Connect.Types.BotName
  
  
import Network.AWS.Connect.Types.HierarchyLevelName
  
import Network.AWS.Connect.Types.InstanceAttributeValue
  
import Network.AWS.Connect.Types.HierarchyLevel
  
import Network.AWS.Connect.Types.InstanceStatusReason
  
import Network.AWS.Connect.Types.TagValue
  
import Network.AWS.Connect.Types.RoutingProfileDescription
  
import Network.AWS.Connect.Types.DirectoryType
  
import Network.AWS.Connect.Types.User
  
import Network.AWS.Connect.Types.LexRegion
  
import Network.AWS.Connect.Types.MediaConcurrency
  
import Network.AWS.Connect.Types.Comparison
  
import Network.AWS.Connect.Types.UserIdentityInfo
  
import Network.AWS.Connect.Types.BucketName
  
import Network.AWS.Connect.Types.UserId
  
import Network.AWS.Connect.Types.NextToken
  
  
import Network.AWS.Connect.Types.CurrentMetricResult
  
import Network.AWS.Connect.Types.VoiceRecordingTrack
  
import Network.AWS.Connect.Types.PhoneType
  
import Network.AWS.Connect.Types.QueueId
  
import Network.AWS.Connect.Types.Grouping
  
import Network.AWS.Connect.Types.ContactFlowId
  
import Network.AWS.Connect.Types.DirectoryAlias
  
import Network.AWS.Connect.Types.KinesisVideoStreamConfig
  
import Network.AWS.Connect.Types.AgentUsername
  
import Network.AWS.Connect.Types.RoutingProfile
  
import Network.AWS.Connect.Types.UserPhoneConfig
  
import Network.AWS.Connect.Types.PhoneNumber
  
  
import Network.AWS.Connect.Types.Threshold
  
import Network.AWS.Connect.Types.HierarchyGroupName
  
import Network.AWS.Connect.Types.Password
  
  
import Network.AWS.Connect.Types.PhoneNumberCountryCode
  
import Network.AWS.Connect.Types.HierarchyPath
  
import Network.AWS.Connect.Types.ContactFlowType
  
import Network.AWS.Connect.Types.QueueReference
  
import Network.AWS.Connect.Types.SecurityProfileName
  
import Network.AWS.Connect.Types.S3Config
  
import Network.AWS.Connect.Types.HoursOfOperationId
  
import Network.AWS.Connect.Types.ContactFlowName
  
import Network.AWS.Connect.Types.HierarchyGroupId
  
import Network.AWS.Connect.Types.UserSummary
  
import Network.AWS.Connect.Types.InstanceStatus
  
import Network.AWS.Connect.Types.DisplayName
  
import Network.AWS.Connect.Types.ContactId
  
import Network.AWS.Connect.Types.ParticipantDetails
  
import Network.AWS.Connect.Types.EncryptionConfig
  
import Network.AWS.Connect.Types.CurrentMetric
  
  
import Network.AWS.Connect.Types.QueueSummary
  
import Network.AWS.Connect.Types.InstanceAttributeType
  
import Network.AWS.Connect.Types.CurrentMetricData
  
import Network.AWS.Connect.Types.TagKey
  
import Network.AWS.Connect.Types.HierarchyLevelUpdate
  
import Network.AWS.Connect.Types.HistoricalMetric
  
import Network.AWS.Connect.Types.HistoricalMetricData
  
import Network.AWS.Connect.Types.SecurityProfileId
  
import Network.AWS.Connect.Types.HoursOfOperationName
  
import Network.AWS.Connect.Types.HierarchyStructureUpdate
  
  
import Network.AWS.Connect.Types.AttributeName
  
import Network.AWS.Connect.Types.Dimensions
  
import Network.AWS.Connect.Types.KinesisFirehoseConfig
  
import Network.AWS.Connect.Types.HoursOfOperationSummary
  
import Network.AWS.Connect.Types.Unit
  
import Network.AWS.Connect.Types.Statistic
  
import Network.AWS.Connect.Types.ContactFlowSummary
  
  
import Network.AWS.Connect.Types.QueueType
  
import Network.AWS.Connect.Types.Instance
  
import Network.AWS.Connect.Types.HistoricalMetricResult
  
import Network.AWS.Connect.Types.SecurityKey
  
import Network.AWS.Connect.Types.StorageType
  
  
  
import Network.AWS.Connect.Types.SecurityProfileSummary
  
import Network.AWS.Connect.Types.InitialContactId
  
import Network.AWS.Connect.Types.Arn
  
import Network.AWS.Connect.Types.Id
  
import Network.AWS.Connect.Types.Name
  
import Network.AWS.Connect.Types.InstanceAlias
  
import Network.AWS.Connect.Types.Content
  
import Network.AWS.Connect.Types.Description
  
import Network.AWS.Connect.Types.DefaultOutboundQueueId
  
import Network.AWS.Connect.Types.ServiceRole
  
import Network.AWS.Connect.Types.LevelId
  
import Network.AWS.Connect.Types.Username
  
import Network.AWS.Connect.Types.ParentGroupId
  
import Network.AWS.Connect.Types.DeskPhoneNumber
  

-- | API version @2017-08-08@ of the Amazon Connect Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "Connect",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "connect",
                 Core._svcVersion = "2017-08-08", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "Connect",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The contact flow is not valid.
_InvalidContactFlowException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidContactFlowException
  = Core._MatchServiceError mkServiceConfig
      "InvalidContactFlowException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidContactFlowException #-}
{-# DEPRECATED _InvalidContactFlowException "Use generic-lens or generic-optics instead"  #-}

-- | The contact is not permitted.
_OutboundContactNotPermittedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OutboundContactNotPermittedException
  = Core._MatchServiceError mkServiceConfig
      "OutboundContactNotPermittedException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _OutboundContactNotPermittedException #-}
{-# DEPRECATED _OutboundContactNotPermittedException "Use generic-lens or generic-optics instead"  #-}

-- | One or more of the specified parameters are not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidParameterException #-}
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead"  #-}

-- | The request is not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException
  = Core._MatchServiceError mkServiceConfig "InvalidRequestException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidRequestException #-}
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead"  #-}

-- | A resource with the specified name already exists.
_DuplicateResourceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateResourceException
  = Core._MatchServiceError mkServiceConfig
      "DuplicateResourceException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _DuplicateResourceException #-}
{-# DEPRECATED _DuplicateResourceException "Use generic-lens or generic-optics instead"  #-}

-- | No user with the specified credentials was found in the Amazon Connect instance.
_UserNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserNotFoundException
  = Core._MatchServiceError mkServiceConfig "UserNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _UserNotFoundException #-}
{-# DEPRECATED _UserNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The contact flow has not been published.
_ContactFlowNotPublishedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ContactFlowNotPublishedException
  = Core._MatchServiceError mkServiceConfig
      "ContactFlowNotPublishedException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ContactFlowNotPublishedException #-}
{-# DEPRECATED _ContactFlowNotPublishedException "Use generic-lens or generic-optics instead"  #-}

-- | Outbound calls to the destination number are not allowed.
_DestinationNotAllowedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DestinationNotAllowedException
  = Core._MatchServiceError mkServiceConfig
      "DestinationNotAllowedException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _DestinationNotAllowedException #-}
{-# DEPRECATED _DestinationNotAllowedException "Use generic-lens or generic-optics instead"  #-}

-- | The contact with the specified ID is not active or does not exist.
_ContactNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ContactNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ContactNotFoundException"
      Core.. Core.hasStatues 410
{-# INLINEABLE _ContactNotFoundException #-}
{-# DEPRECATED _ContactNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The service quota has been exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException
  = Core._MatchServiceError mkServiceConfig
      "ServiceQuotaExceededException"
      Core.. Core.hasStatues 402
{-# INLINEABLE _ServiceQuotaExceededException #-}
{-# DEPRECATED _ServiceQuotaExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The throttling limit has been exceeded.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException
  = Core._MatchServiceError mkServiceConfig "ThrottlingException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _ThrottlingException #-}
{-# DEPRECATED _ThrottlingException "Use generic-lens or generic-optics instead"  #-}

-- | Request processing failed due to an error or failure with the service.
_InternalServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceException
  = Core._MatchServiceError mkServiceConfig
      "InternalServiceException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalServiceException #-}
{-# DEPRECATED _InternalServiceException "Use generic-lens or generic-optics instead"  #-}

-- | A resource already has that name.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException
  = Core._MatchServiceError mkServiceConfig
      "ResourceConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ResourceConflictException #-}
{-# DEPRECATED _ResourceConflictException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The allowed limit for the resource has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | That resource is already in use. Please try another.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUseException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}
