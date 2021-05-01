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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "KinesisVideo",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "kinesisvideo",
      Prelude._svcSigningName = "kinesisvideo",
      Prelude._svcVersion = "2017-09-30",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "KinesisVideo",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The number of streams created for the account is too high.
_AccountStreamLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccountStreamLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "AccountStreamLimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | The stream version that you specified is not the latest version. To get
-- the latest version, use the
-- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_DescribeStream.html DescribeStream>
-- API.
_VersionMismatchException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_VersionMismatchException =
  Prelude._MatchServiceError
    defaultService
    "VersionMismatchException"
    Prelude.. Prelude.hasStatus 400

-- | You have reached the maximum limit of active signaling channels for this
-- AWS account in this region.
_AccountChannelLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccountChannelLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "AccountChannelLimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | Kinesis Video Streams has throttled the request because you have
-- exceeded the limit of allowed client calls. Try making the call later.
_ClientLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClientLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "ClientLimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | Not implemented.
_DeviceStreamLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeviceStreamLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "DeviceStreamLimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | You have exceeded the limit of tags that you can associate with the
-- resource. Kinesis video streams support up to 50 tags.
_TagsPerResourceExceededLimitException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagsPerResourceExceededLimitException =
  Prelude._MatchServiceError
    defaultService
    "TagsPerResourceExceededLimitException"
    Prelude.. Prelude.hasStatus 400

-- | The format of the @StreamARN@ is invalid.
_InvalidResourceFormatException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidResourceFormatException =
  Prelude._MatchServiceError
    defaultService
    "InvalidResourceFormatException"
    Prelude.. Prelude.hasStatus 400

-- | You do not have required permissions to perform this operation.
_AccessDeniedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessDeniedException =
  Prelude._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Prelude.hasStatus 401

-- | The signaling channel is currently not available for this operation.
_ResourceInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceInUseException =
  Prelude._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Prelude.hasStatus 400

-- | Not implemented.
_InvalidDeviceException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeviceException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeviceException"
    Prelude.. Prelude.hasStatus 400

-- | Amazon Kinesis Video Streams can\'t find the stream that you specified.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | The caller is not authorized to perform this operation.
_NotAuthorizedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotAuthorizedException =
  Prelude._MatchServiceError
    defaultService
    "NotAuthorizedException"
    Prelude.. Prelude.hasStatus 401

-- | The value for this input parameter is invalid.
_InvalidArgumentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidArgumentException =
  Prelude._MatchServiceError
    defaultService
    "InvalidArgumentException"
    Prelude.. Prelude.hasStatus 400
