-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideo.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _AccessDeniedException
    , _InvalidArgumentException
    , _TagsPerResourceExceededLimitException
    , _NotAuthorizedException
    , _ClientLimitExceededException
    , _AccountChannelLimitExceededException
    , _InvalidDeviceException
    , _VersionMismatchException
    , _AccountStreamLimitExceededException
    , _InvalidResourceFormatException
    , _DeviceStreamLimitExceededException
    , _ResourceNotFoundException
    , _ResourceInUseException

    -- * StreamStatus
    , StreamStatus (..)

    -- * MediaType
    , MediaType (..)

    -- * ChannelNameCondition
    , ChannelNameCondition (..)
    , mkChannelNameCondition
    , cncComparisonOperator
    , cncComparisonValue

    -- * APIName
    , APIName (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * StreamInfo
    , StreamInfo (..)
    , mkStreamInfo
    , siCreationTime
    , siDataRetentionInHours
    , siDeviceName
    , siKmsKeyId
    , siMediaType
    , siStatus
    , siStreamARN
    , siStreamName
    , siVersion

    -- * ChannelInfo
    , ChannelInfo (..)
    , mkChannelInfo
    , ciChannelARN
    , ciChannelName
    , ciChannelStatus
    , ciChannelType
    , ciCreationTime
    , ciSingleMasterConfiguration
    , ciVersion

    -- * ResourceEndpointListItem
    , ResourceEndpointListItem (..)
    , mkResourceEndpointListItem
    , reliProtocol
    , reliResourceEndpoint

    -- * SingleMasterConfiguration
    , SingleMasterConfiguration (..)
    , mkSingleMasterConfiguration
    , smcMessageTtlSeconds

    -- * TagValue
    , TagValue (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * ResourceEndpoint
    , ResourceEndpoint (..)

    -- * NextToken
    , NextToken (..)

    -- * ChannelName
    , ChannelName (..)

    -- * ChannelRole
    , ChannelRole (..)

    -- * ResourceARN
    , ResourceARN (..)

    -- * KmsKeyId
    , KmsKeyId (..)

    -- * DeviceName
    , DeviceName (..)

    -- * Version
    , Version (..)

    -- * ChannelProtocol
    , ChannelProtocol (..)

    -- * TagKey
    , TagKey (..)

    -- * StreamNameCondition
    , StreamNameCondition (..)
    , mkStreamNameCondition
    , sncComparisonOperator
    , sncComparisonValue

    -- * UpdateDataRetentionOperation
    , UpdateDataRetentionOperation (..)

    -- * DataEndpoint
    , DataEndpoint (..)

    -- * StreamName
    , StreamName (..)

    -- * ChannelType
    , ChannelType (..)

    -- * SingleMasterChannelEndpointConfiguration
    , SingleMasterChannelEndpointConfiguration (..)
    , mkSingleMasterChannelEndpointConfiguration
    , smcecProtocols
    , smcecRole

    -- * StreamARN
    , StreamARN (..)

    -- * ComparisonValue
    , ComparisonValue (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * ChannelARN
    , ChannelARN (..)

    -- * CurrentVersion
    , CurrentVersion (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.KinesisVideo.Types.StreamStatus
  
import Network.AWS.KinesisVideo.Types.MediaType
  
  
import Network.AWS.KinesisVideo.Types.ChannelNameCondition
  
import Network.AWS.KinesisVideo.Types.APIName
  
import Network.AWS.KinesisVideo.Types.Tag
  
import Network.AWS.KinesisVideo.Types.StreamInfo
  
import Network.AWS.KinesisVideo.Types.ChannelInfo
  
  
  
  
  
  
import Network.AWS.KinesisVideo.Types.ResourceEndpointListItem
  
import Network.AWS.KinesisVideo.Types.SingleMasterConfiguration
  
  
import Network.AWS.KinesisVideo.Types.TagValue
  
import Network.AWS.KinesisVideo.Types.ComparisonOperator
  
import Network.AWS.KinesisVideo.Types.ResourceEndpoint
  
  
import Network.AWS.KinesisVideo.Types.NextToken
  
import Network.AWS.KinesisVideo.Types.ChannelName
  
  
import Network.AWS.KinesisVideo.Types.ChannelRole
  
  
import Network.AWS.KinesisVideo.Types.ResourceARN
  
import Network.AWS.KinesisVideo.Types.KmsKeyId
  
import Network.AWS.KinesisVideo.Types.DeviceName
  
import Network.AWS.KinesisVideo.Types.Version
  
import Network.AWS.KinesisVideo.Types.ChannelProtocol
  
  
import Network.AWS.KinesisVideo.Types.TagKey
  
import Network.AWS.KinesisVideo.Types.StreamNameCondition
  
import Network.AWS.KinesisVideo.Types.UpdateDataRetentionOperation
  
import Network.AWS.KinesisVideo.Types.DataEndpoint
  
import Network.AWS.KinesisVideo.Types.StreamName
  
import Network.AWS.KinesisVideo.Types.ChannelType
  
import Network.AWS.KinesisVideo.Types.SingleMasterChannelEndpointConfiguration
  
  
  
import Network.AWS.KinesisVideo.Types.StreamARN
  
import Network.AWS.KinesisVideo.Types.ComparisonValue
  
import Network.AWS.KinesisVideo.Types.Key
  
import Network.AWS.KinesisVideo.Types.Value
  
import Network.AWS.KinesisVideo.Types.ChannelARN
  
import Network.AWS.KinesisVideo.Types.CurrentVersion
  

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "KinesisVideo",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "kinesisvideo",
                 Core._svcVersion = "2017-09-30", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "KinesisVideo",
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

-- | You do not have required permissions to perform this operation.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException
  = Core._MatchServiceError mkServiceConfig "AccessDeniedException"
      Core.. Core.hasStatues 401
{-# INLINEABLE _AccessDeniedException #-}
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | The value for this input parameter is invalid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException
  = Core._MatchServiceError mkServiceConfig
      "InvalidArgumentException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidArgumentException #-}
{-# DEPRECATED _InvalidArgumentException "Use generic-lens or generic-optics instead"  #-}

-- | You have exceeded the limit of tags that you can associate with the resource. Kinesis video streams support up to 50 tags. 
_TagsPerResourceExceededLimitException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagsPerResourceExceededLimitException
  = Core._MatchServiceError mkServiceConfig
      "TagsPerResourceExceededLimitException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TagsPerResourceExceededLimitException #-}
{-# DEPRECATED _TagsPerResourceExceededLimitException "Use generic-lens or generic-optics instead"  #-}

-- | The caller is not authorized to perform this operation.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException
  = Core._MatchServiceError mkServiceConfig "NotAuthorizedException"
      Core.. Core.hasStatues 401
{-# INLINEABLE _NotAuthorizedException #-}
{-# DEPRECATED _NotAuthorizedException "Use generic-lens or generic-optics instead"  #-}

-- | Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client calls. Try making the call later.
_ClientLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClientLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "ClientLimitExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ClientLimitExceededException #-}
{-# DEPRECATED _ClientLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the maximum limit of active signaling channels for this AWS account in this region.
_AccountChannelLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccountChannelLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "AccountChannelLimitExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _AccountChannelLimitExceededException #-}
{-# DEPRECATED _AccountChannelLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | Not implemented.
_InvalidDeviceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeviceException
  = Core._MatchServiceError mkServiceConfig "InvalidDeviceException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDeviceException #-}
{-# DEPRECATED _InvalidDeviceException "Use generic-lens or generic-optics instead"  #-}

-- | The stream version that you specified is not the latest version. To get the latest version, use the <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_DescribeStream.html DescribeStream> API.
_VersionMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_VersionMismatchException
  = Core._MatchServiceError mkServiceConfig
      "VersionMismatchException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _VersionMismatchException #-}
{-# DEPRECATED _VersionMismatchException "Use generic-lens or generic-optics instead"  #-}

-- | The number of streams created for the account is too high.
_AccountStreamLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccountStreamLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "AccountStreamLimitExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _AccountStreamLimitExceededException #-}
{-# DEPRECATED _AccountStreamLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The format of the @StreamARN@ is invalid.
_InvalidResourceFormatException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResourceFormatException
  = Core._MatchServiceError mkServiceConfig
      "InvalidResourceFormatException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidResourceFormatException #-}
{-# DEPRECATED _InvalidResourceFormatException "Use generic-lens or generic-optics instead"  #-}

-- | Not implemented. 
_DeviceStreamLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeviceStreamLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "DeviceStreamLimitExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DeviceStreamLimitExceededException #-}
{-# DEPRECATED _DeviceStreamLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | Amazon Kinesis Video Streams can't find the stream that you specified.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The signaling channel is currently not available for this operation.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUseException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}
