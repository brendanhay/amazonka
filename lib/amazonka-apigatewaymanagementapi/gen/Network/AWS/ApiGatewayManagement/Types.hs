-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGatewayManagement.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGatewayManagement.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _PayloadTooLargeException
    , _ForbiddenException
    , _GoneException
    , _LimitExceededException

    -- * Identity
    , Identity (..)
    , mkIdentity
    , iSourceIp
    , iUserAgent
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
  
  
import Network.AWS.ApiGatewayManagement.Types.Identity
  
  

-- | API version @2018-11-29@ of the Amazon ApiGatewayManagementApi SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ApiGatewayManagement",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "execute-api",
                 Core._svcVersion = "2018-11-29", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "ApiGatewayManagement",
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

-- | The data has exceeded the maximum size allowed.
_PayloadTooLargeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PayloadTooLargeException
  = Core._MatchServiceError mkServiceConfig
      "PayloadTooLargeException"
      Core.. Core.hasStatues 413
{-# INLINEABLE _PayloadTooLargeException #-}
{-# DEPRECATED _PayloadTooLargeException "Use generic-lens or generic-optics instead"  #-}

-- | The caller is not authorized to invoke this operation.
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException
  = Core._MatchServiceError mkServiceConfig "ForbiddenException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _ForbiddenException #-}
{-# DEPRECATED _ForbiddenException "Use generic-lens or generic-optics instead"  #-}

-- | The connection with the provided id no longer exists.
_GoneException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GoneException
  = Core._MatchServiceError mkServiceConfig "GoneException" Core..
      Core.hasStatues 410
{-# INLINEABLE _GoneException #-}
{-# DEPRECATED _GoneException "Use generic-lens or generic-optics instead"  #-}

-- | The client is sending more than the allowed number of requests per unit of time or the WebSocket client side buffer is full.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
