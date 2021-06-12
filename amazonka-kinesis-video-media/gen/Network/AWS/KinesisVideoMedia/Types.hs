{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoMedia.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoMedia.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConnectionLimitExceededException,
    _ClientLimitExceededException,
    _InvalidEndpointException,
    _ResourceNotFoundException,
    _NotAuthorizedException,
    _InvalidArgumentException,

    -- * StartSelectorType
    StartSelectorType (..),

    -- * StartSelector
    StartSelector (..),
    newStartSelector,
    startSelector_afterFragmentNumber,
    startSelector_startTimestamp,
    startSelector_continuationToken,
    startSelector_startSelectorType,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideoMedia.Types.StartSelector
import Network.AWS.KinesisVideoMedia.Types.StartSelectorType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams Media SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "KinesisVideoMedia",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "kinesisvideo",
      Core._serviceSigningName = "kinesisvideo",
      Core._serviceVersion = "2017-09-30",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "KinesisVideoMedia",
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
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | Kinesis Video Streams has throttled the request because you have
-- exceeded the limit of allowed client connections.
_ConnectionLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConnectionLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ConnectionLimitExceededException"
    Core.. Core.hasStatus 400

-- | Kinesis Video Streams has throttled the request because you have
-- exceeded the limit of allowed client calls. Try making the call later.
_ClientLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClientLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ClientLimitExceededException"
    Core.. Core.hasStatus 400

-- | Status Code: 400, Caller used wrong endpoint to write data to a stream.
-- On receiving such an exception, the user must call @GetDataEndpoint@
-- with @AccessMode@ set to \"READ\" and use the endpoint Kinesis Video
-- returns in the next @GetMedia@ call.
_InvalidEndpointException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEndpointException =
  Core._MatchServiceError
    defaultService
    "InvalidEndpointException"
    Core.. Core.hasStatus 400

-- | Status Code: 404, The stream with the given name does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Core.. Core.hasStatus 404

-- | Status Code: 403, The caller is not authorized to perform an operation
-- on the given stream, or the token has expired.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedException"
    Core.. Core.hasStatus 401

-- | The value for this input parameter is invalid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"
    Core.. Core.hasStatus 400
