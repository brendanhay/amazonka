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

import Network.AWS.KinesisVideoMedia.Types.StartSelector
import Network.AWS.KinesisVideoMedia.Types.StartSelectorType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams Media SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "KinesisVideoMedia",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "kinesisvideo",
      Prelude._svcSigningName = "kinesisvideo",
      Prelude._svcVersion = "2017-09-30",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "KinesisVideoMedia",
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

-- | Kinesis Video Streams has throttled the request because you have
-- exceeded the limit of allowed client connections.
_ConnectionLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConnectionLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "ConnectionLimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | Kinesis Video Streams has throttled the request because you have
-- exceeded the limit of allowed client calls. Try making the call later.
_ClientLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClientLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "ClientLimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | Status Code: 400, Caller used wrong endpoint to write data to a stream.
-- On receiving such an exception, the user must call @GetDataEndpoint@
-- with @AccessMode@ set to \"READ\" and use the endpoint Kinesis Video
-- returns in the next @GetMedia@ call.
_InvalidEndpointException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidEndpointException =
  Prelude._MatchServiceError
    defaultService
    "InvalidEndpointException"
    Prelude.. Prelude.hasStatus 400

-- | Status Code: 404, The stream with the given name does not exist.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | Status Code: 403, The caller is not authorized to perform an operation
-- on the given stream, or the token has expired.
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
