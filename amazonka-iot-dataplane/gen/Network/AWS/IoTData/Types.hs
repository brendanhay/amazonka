{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTData.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _UnsupportedDocumentEncodingException,
    _UnauthorizedException,
    _ServiceUnavailableException,
    _ThrottlingException,
    _InvalidRequestException,
    _RequestEntityTooLargeException,
    _ConflictException,
    _ResourceNotFoundException,
    _InternalFailureException,
    _MethodNotAllowedException,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-05-28@ of the Amazon IoT Data Plane SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "IoTData",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "data.iot",
      Prelude._svcVersion = "2015-05-28",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "IoTData",
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

-- | The document encoding is not supported.
_UnsupportedDocumentEncodingException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedDocumentEncodingException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedDocumentEncodingException"
    Prelude.. Prelude.hasStatus 415

-- | You are not authorized to perform this operation.
_UnauthorizedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnauthorizedException =
  Prelude._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Prelude.hasStatus 401

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Prelude.hasStatus 503

-- | The rate exceeds the limit.
_ThrottlingException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ThrottlingException =
  Prelude._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Prelude.hasStatus 429

-- | The request is not valid.
_InvalidRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRequestException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Prelude.hasStatus 400

-- | The payload exceeds the maximum size allowed.
_RequestEntityTooLargeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RequestEntityTooLargeException =
  Prelude._MatchServiceError
    defaultService
    "RequestEntityTooLargeException"
    Prelude.. Prelude.hasStatus 413

-- | The specified version does not match the version of the document.
_ConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConflictException =
  Prelude._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Prelude.hasStatus 409

-- | The specified resource does not exist.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | An unexpected error has occurred.
_InternalFailureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalFailureException =
  Prelude._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Prelude.hasStatus 500

-- | The specified combination of HTTP verb and URI is not supported.
_MethodNotAllowedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MethodNotAllowedException =
  Prelude._MatchServiceError
    defaultService
    "MethodNotAllowedException"
    Prelude.. Prelude.hasStatus 405
