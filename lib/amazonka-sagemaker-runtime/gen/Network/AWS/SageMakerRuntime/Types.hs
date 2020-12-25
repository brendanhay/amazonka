-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMakerRuntime.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMakerRuntime.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ServiceUnavailable,
    _ModelError,
    _InternalFailure,
    _ValidationError,

    -- * EndpointName
    EndpointName (..),

    -- * CustomAttributesHeader
    CustomAttributesHeader (..),

    -- * TargetModelHeader
    TargetModelHeader (..),

    -- * Header
    Header (..),

    -- * ContentType
    ContentType (..),

    -- * CustomAttributes
    CustomAttributes (..),

    -- * InvokedProductionVariant
    InvokedProductionVariant (..),

    -- * TargetVariant
    TargetVariant (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import Network.AWS.SageMakerRuntime.Types.ContentType
import Network.AWS.SageMakerRuntime.Types.CustomAttributes
import Network.AWS.SageMakerRuntime.Types.CustomAttributesHeader
import Network.AWS.SageMakerRuntime.Types.EndpointName
import Network.AWS.SageMakerRuntime.Types.Header
import Network.AWS.SageMakerRuntime.Types.InvokedProductionVariant
import Network.AWS.SageMakerRuntime.Types.TargetModelHeader
import Network.AWS.SageMakerRuntime.Types.TargetVariant
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-05-13@ of the Amazon SageMaker Runtime SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "SageMakerRuntime",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "runtime.sagemaker",
      Core._svcVersion = "2017-05-13",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "SageMakerRuntime",
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

-- | The service is unavailable. Try your call again.
_ServiceUnavailable :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailable =
  Core._MatchServiceError mkServiceConfig "ServiceUnavailable"
    Core.. Core.hasStatues 503
{-# DEPRECATED _ServiceUnavailable "Use generic-lens or generic-optics instead." #-}

-- | Model (owned by the customer in the container) returned 4xx or 5xx error code.
_ModelError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ModelError =
  Core._MatchServiceError mkServiceConfig "ModelError"
    Core.. Core.hasStatues 424
{-# DEPRECATED _ModelError "Use generic-lens or generic-optics instead." #-}

-- | An internal failure occurred.
_InternalFailure :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailure =
  Core._MatchServiceError mkServiceConfig "InternalFailure"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalFailure "Use generic-lens or generic-optics instead." #-}

-- | Inspect your request and try again.
_ValidationError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationError =
  Core._MatchServiceError mkServiceConfig "ValidationError"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ValidationError "Use generic-lens or generic-optics instead." #-}
