-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTData.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidRequestException,
    _ConflictException,
    _RequestEntityTooLargeException,
    _ThrottlingException,
    _MethodNotAllowedException,
    _InternalFailureException,
    _ServiceUnavailableException,
    _UnauthorizedException,
    _ResourceNotFoundException,
    _UnsupportedDocumentEncodingException,

    -- * ShadowName
    ShadowName (..),

    -- * Topic
    Topic (..),

    -- * NextToken
    NextToken (..),

    -- * ThingName
    ThingName (..),
  )
where

import Network.AWS.IoTData.Types.NextToken
import Network.AWS.IoTData.Types.ShadowName
import Network.AWS.IoTData.Types.ThingName
import Network.AWS.IoTData.Types.Topic
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-05-28@ of the Amazon IoT Data Plane SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "IoTData",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "data.iot",
      Core._svcVersion = "2015-05-28",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "IoTData",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The request is not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError mkServiceConfig "InvalidRequestException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead." #-}

-- | The specified version does not match the version of the document.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError mkServiceConfig "ConflictException"
    Core.. Core.hasStatues 409
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead." #-}

-- | The payload exceeds the maximum size allowed.
_RequestEntityTooLargeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestEntityTooLargeException =
  Core._MatchServiceError
    mkServiceConfig
    "RequestEntityTooLargeException"
    Core.. Core.hasStatues 413
{-# DEPRECATED _RequestEntityTooLargeException "Use generic-lens or generic-optics instead." #-}

-- | The rate exceeds the limit.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError mkServiceConfig "ThrottlingException"
    Core.. Core.hasStatues 429
{-# DEPRECATED _ThrottlingException "Use generic-lens or generic-optics instead." #-}

-- | The specified combination of HTTP verb and URI is not supported.
_MethodNotAllowedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MethodNotAllowedException =
  Core._MatchServiceError
    mkServiceConfig
    "MethodNotAllowedException"
    Core.. Core.hasStatues 405
{-# DEPRECATED _MethodNotAllowedException "Use generic-lens or generic-optics instead." #-}

-- | An unexpected error has occurred.
_InternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalFailureException"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalFailureException "Use generic-lens or generic-optics instead." #-}

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "ServiceUnavailableException"
    Core.. Core.hasStatues 503
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | You are not authorized to perform this operation.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError mkServiceConfig "UnauthorizedException"
    Core.. Core.hasStatues 401
{-# DEPRECATED _UnauthorizedException "Use generic-lens or generic-optics instead." #-}

-- | The specified resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The document encoding is not supported.
_UnsupportedDocumentEncodingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedDocumentEncodingException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedDocumentEncodingException"
    Core.. Core.hasStatues 415
{-# DEPRECATED _UnsupportedDocumentEncodingException "Use generic-lens or generic-optics instead." #-}
