{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IVSChat.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSChat.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _PendingVerification,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * ChatTokenCapability
    ChatTokenCapability (..),

    -- * CreateLoggingConfigurationState
    CreateLoggingConfigurationState (..),

    -- * FallbackResult
    FallbackResult (..),

    -- * LoggingConfigurationState
    LoggingConfigurationState (..),

    -- * UpdateLoggingConfigurationState
    UpdateLoggingConfigurationState (..),

    -- * CloudWatchLogsDestinationConfiguration
    CloudWatchLogsDestinationConfiguration (..),
    newCloudWatchLogsDestinationConfiguration,
    cloudWatchLogsDestinationConfiguration_logGroupName,

    -- * DestinationConfiguration
    DestinationConfiguration (..),
    newDestinationConfiguration,
    destinationConfiguration_cloudWatchLogs,
    destinationConfiguration_firehose,
    destinationConfiguration_s3,

    -- * FirehoseDestinationConfiguration
    FirehoseDestinationConfiguration (..),
    newFirehoseDestinationConfiguration,
    firehoseDestinationConfiguration_deliveryStreamName,

    -- * LoggingConfigurationSummary
    LoggingConfigurationSummary (..),
    newLoggingConfigurationSummary,
    loggingConfigurationSummary_arn,
    loggingConfigurationSummary_createTime,
    loggingConfigurationSummary_destinationConfiguration,
    loggingConfigurationSummary_id,
    loggingConfigurationSummary_name,
    loggingConfigurationSummary_state,
    loggingConfigurationSummary_tags,
    loggingConfigurationSummary_updateTime,

    -- * MessageReviewHandler
    MessageReviewHandler (..),
    newMessageReviewHandler,
    messageReviewHandler_fallbackResult,
    messageReviewHandler_uri,

    -- * RoomSummary
    RoomSummary (..),
    newRoomSummary,
    roomSummary_arn,
    roomSummary_createTime,
    roomSummary_id,
    roomSummary_loggingConfigurationIdentifiers,
    roomSummary_messageReviewHandler,
    roomSummary_name,
    roomSummary_tags,
    roomSummary_updateTime,

    -- * S3DestinationConfiguration
    S3DestinationConfiguration (..),
    newS3DestinationConfiguration,
    s3DestinationConfiguration_bucketName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IVSChat.Types.ChatTokenCapability
import Amazonka.IVSChat.Types.CloudWatchLogsDestinationConfiguration
import Amazonka.IVSChat.Types.CreateLoggingConfigurationState
import Amazonka.IVSChat.Types.DestinationConfiguration
import Amazonka.IVSChat.Types.FallbackResult
import Amazonka.IVSChat.Types.FirehoseDestinationConfiguration
import Amazonka.IVSChat.Types.LoggingConfigurationState
import Amazonka.IVSChat.Types.LoggingConfigurationSummary
import Amazonka.IVSChat.Types.MessageReviewHandler
import Amazonka.IVSChat.Types.RoomSummary
import Amazonka.IVSChat.Types.S3DestinationConfiguration
import Amazonka.IVSChat.Types.UpdateLoggingConfigurationState
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-07-14@ of the Amazon Interactive Video Service Chat SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IVSChat",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ivschat",
      Core.signingName = "ivschat",
      Core.version = "2020-07-14",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IVSChat",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- |
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- |
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- |
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- |
_PendingVerification :: Core.AsError a => Lens.Fold a Core.ServiceError
_PendingVerification =
  Core._MatchServiceError
    defaultService
    "PendingVerification"
    Prelude.. Core.hasStatus 403

-- |
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- |
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- |
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- |
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
