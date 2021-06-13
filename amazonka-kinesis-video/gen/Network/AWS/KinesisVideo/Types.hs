{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccountStreamLimitExceededException,
    _VersionMismatchException,
    _AccountChannelLimitExceededException,
    _ClientLimitExceededException,
    _DeviceStreamLimitExceededException,
    _TagsPerResourceExceededLimitException,
    _InvalidResourceFormatException,
    _AccessDeniedException,
    _ResourceInUseException,
    _InvalidDeviceException,
    _ResourceNotFoundException,
    _NotAuthorizedException,
    _InvalidArgumentException,

    -- * APIName
    APIName (..),

    -- * ChannelProtocol
    ChannelProtocol (..),

    -- * ChannelRole
    ChannelRole (..),

    -- * ChannelType
    ChannelType (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * StreamStatus
    StreamStatus (..),

    -- * UpdateDataRetentionOperation
    UpdateDataRetentionOperation (..),

    -- * ChannelInfo
    ChannelInfo (..),
    newChannelInfo,
    channelInfo_channelName,
    channelInfo_creationTime,
    channelInfo_singleMasterConfiguration,
    channelInfo_channelType,
    channelInfo_version,
    channelInfo_channelStatus,
    channelInfo_channelARN,

    -- * ChannelNameCondition
    ChannelNameCondition (..),
    newChannelNameCondition,
    channelNameCondition_comparisonOperator,
    channelNameCondition_comparisonValue,

    -- * ResourceEndpointListItem
    ResourceEndpointListItem (..),
    newResourceEndpointListItem,
    resourceEndpointListItem_resourceEndpoint,
    resourceEndpointListItem_protocol,

    -- * SingleMasterChannelEndpointConfiguration
    SingleMasterChannelEndpointConfiguration (..),
    newSingleMasterChannelEndpointConfiguration,
    singleMasterChannelEndpointConfiguration_protocols,
    singleMasterChannelEndpointConfiguration_role,

    -- * SingleMasterConfiguration
    SingleMasterConfiguration (..),
    newSingleMasterConfiguration,
    singleMasterConfiguration_messageTtlSeconds,

    -- * StreamInfo
    StreamInfo (..),
    newStreamInfo,
    streamInfo_status,
    streamInfo_creationTime,
    streamInfo_dataRetentionInHours,
    streamInfo_version,
    streamInfo_kmsKeyId,
    streamInfo_deviceName,
    streamInfo_mediaType,
    streamInfo_streamARN,
    streamInfo_streamName,

    -- * StreamNameCondition
    StreamNameCondition (..),
    newStreamNameCondition,
    streamNameCondition_comparisonOperator,
    streamNameCondition_comparisonValue,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types.APIName
import Network.AWS.KinesisVideo.Types.ChannelInfo
import Network.AWS.KinesisVideo.Types.ChannelNameCondition
import Network.AWS.KinesisVideo.Types.ChannelProtocol
import Network.AWS.KinesisVideo.Types.ChannelRole
import Network.AWS.KinesisVideo.Types.ChannelType
import Network.AWS.KinesisVideo.Types.ComparisonOperator
import Network.AWS.KinesisVideo.Types.ResourceEndpointListItem
import Network.AWS.KinesisVideo.Types.SingleMasterChannelEndpointConfiguration
import Network.AWS.KinesisVideo.Types.SingleMasterConfiguration
import Network.AWS.KinesisVideo.Types.StreamInfo
import Network.AWS.KinesisVideo.Types.StreamNameCondition
import Network.AWS.KinesisVideo.Types.StreamStatus
import Network.AWS.KinesisVideo.Types.Tag
import Network.AWS.KinesisVideo.Types.UpdateDataRetentionOperation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "KinesisVideo",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "kinesisvideo",
      Core._serviceSigningName = "kinesisvideo",
      Core._serviceVersion = "2017-09-30",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "KinesisVideo",
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
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | The number of streams created for the account is too high.
_AccountStreamLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccountStreamLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AccountStreamLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The stream version that you specified is not the latest version. To get
-- the latest version, use the
-- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_DescribeStream.html DescribeStream>
-- API.
_VersionMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_VersionMismatchException =
  Core._MatchServiceError
    defaultService
    "VersionMismatchException"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum limit of active signaling channels for this
-- AWS account in this region.
_AccountChannelLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccountChannelLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AccountChannelLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | Kinesis Video Streams has throttled the request because you have
-- exceeded the limit of allowed client calls. Try making the call later.
_ClientLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClientLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ClientLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | Not implemented.
_DeviceStreamLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeviceStreamLimitExceededException =
  Core._MatchServiceError
    defaultService
    "DeviceStreamLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the limit of tags that you can associate with the
-- resource. Kinesis video streams support up to 50 tags.
_TagsPerResourceExceededLimitException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagsPerResourceExceededLimitException =
  Core._MatchServiceError
    defaultService
    "TagsPerResourceExceededLimitException"
    Prelude.. Core.hasStatus 400

-- | The format of the @StreamARN@ is invalid.
_InvalidResourceFormatException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceFormatException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceFormatException"
    Prelude.. Core.hasStatus 400

-- | You do not have required permissions to perform this operation.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 401

-- | The signaling channel is currently not available for this operation.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 400

-- | Not implemented.
_InvalidDeviceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeviceException =
  Core._MatchServiceError
    defaultService
    "InvalidDeviceException"
    Prelude.. Core.hasStatus 400

-- | Amazon Kinesis Video Streams can\'t find the stream that you specified.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The caller is not authorized to perform this operation.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedException"
    Prelude.. Core.hasStatus 401

-- | The value for this input parameter is invalid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"
    Prelude.. Core.hasStatus 400
