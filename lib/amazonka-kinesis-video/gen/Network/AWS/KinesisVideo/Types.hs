-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types
  ( -- * Service configuration
    kinesisVideoService,

    -- * Errors

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
    mkChannelInfo,
    ciCreationTime,
    ciChannelStatus,
    ciChannelARN,
    ciSingleMasterConfiguration,
    ciChannelName,
    ciVersion,
    ciChannelType,

    -- * ChannelNameCondition
    ChannelNameCondition (..),
    mkChannelNameCondition,
    cncComparisonOperator,
    cncComparisonValue,

    -- * ResourceEndpointListItem
    ResourceEndpointListItem (..),
    mkResourceEndpointListItem,
    reliProtocol,
    reliResourceEndpoint,

    -- * SingleMasterChannelEndpointConfiguration
    SingleMasterChannelEndpointConfiguration (..),
    mkSingleMasterChannelEndpointConfiguration,
    smcecProtocols,
    smcecRole,

    -- * SingleMasterConfiguration
    SingleMasterConfiguration (..),
    mkSingleMasterConfiguration,
    smcMessageTtlSeconds,

    -- * StreamInfo
    StreamInfo (..),
    mkStreamInfo,
    siCreationTime,
    siStatus,
    siMediaType,
    siDataRetentionInHours,
    siStreamARN,
    siKMSKeyId,
    siDeviceName,
    siVersion,
    siStreamName,

    -- * StreamNameCondition
    StreamNameCondition (..),
    mkStreamNameCondition,
    sncComparisonOperator,
    sncComparisonValue,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams SDK configuration.
kinesisVideoService :: Lude.Service
kinesisVideoService =
  Lude.Service
    { Lude._svcAbbrev = "KinesisVideo",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "kinesisvideo",
      Lude._svcVersion = "2017-09-30",
      Lude._svcEndpoint = Lude.defaultEndpoint kinesisVideoService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "KinesisVideo",
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
