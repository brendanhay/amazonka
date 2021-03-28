-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStore.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _PolicyNotFoundException
    , _CorsPolicyNotFoundException
    , _ContainerInUseException
    , _InternalServerError
    , _ContainerNotFoundException
    , _LimitExceededException

    -- * Origin
    , Origin (..)

    -- * MetricPolicy
    , MetricPolicy (..)
    , mkMetricPolicy
    , mpContainerLevelMetrics
    , mpMetricPolicyRules

    -- * PaginationToken
    , PaginationToken (..)

    -- * ContainerPolicy
    , ContainerPolicy (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * ObjectGroup
    , ObjectGroup (..)

    -- * ContainerARN
    , ContainerARN (..)

    -- * ContainerName
    , ContainerName (..)

    -- * MethodName
    , MethodName (..)

    -- * CorsRule
    , CorsRule (..)
    , mkCorsRule
    , crAllowedOrigins
    , crAllowedHeaders
    , crAllowedMethods
    , crExposeHeaders
    , crMaxAgeSeconds

    -- * ContainerLevelMetrics
    , ContainerLevelMetrics (..)

    -- * Container
    , Container (..)
    , mkContainer
    , cARN
    , cAccessLoggingEnabled
    , cCreationTime
    , cEndpoint
    , cName
    , cStatus

    -- * Header
    , Header (..)

    -- * MetricPolicyRule
    , MetricPolicyRule (..)
    , mkMetricPolicyRule
    , mprObjectGroup
    , mprObjectGroupName

    -- * ObjectGroupName
    , ObjectGroupName (..)

    -- * TagKey
    , TagKey (..)

    -- * Endpoint
    , Endpoint (..)

    -- * ContainerStatus
    , ContainerStatus (..)

    -- * LifecyclePolicy
    , LifecyclePolicy (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
import Network.AWS.MediaStore.Types.Origin
  
import Network.AWS.MediaStore.Types.MetricPolicy
  
import Network.AWS.MediaStore.Types.PaginationToken
  
import Network.AWS.MediaStore.Types.ContainerPolicy
  
import Network.AWS.MediaStore.Types.Tag
  
import Network.AWS.MediaStore.Types.ObjectGroup
  
import Network.AWS.MediaStore.Types.ContainerARN
  
import Network.AWS.MediaStore.Types.ContainerName
  
import Network.AWS.MediaStore.Types.MethodName
  
import Network.AWS.MediaStore.Types.CorsRule
  
import Network.AWS.MediaStore.Types.ContainerLevelMetrics
  
  
  
import Network.AWS.MediaStore.Types.Container
  
import Network.AWS.MediaStore.Types.Header
  
import Network.AWS.MediaStore.Types.MetricPolicyRule
  
  
import Network.AWS.MediaStore.Types.ObjectGroupName
  
import Network.AWS.MediaStore.Types.TagKey
  
import Network.AWS.MediaStore.Types.Endpoint
  
import Network.AWS.MediaStore.Types.ContainerStatus
  
  
import Network.AWS.MediaStore.Types.LifecyclePolicy
  
  
import Network.AWS.MediaStore.Types.Key
  
import Network.AWS.MediaStore.Types.Value
  

-- | API version @2017-09-01@ of the Amazon Elemental MediaStore SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "MediaStore",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "mediastore",
                 Core._svcVersion = "2017-09-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "MediaStore",
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

-- | The policy that you specified in the request does not exist.
_PolicyNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyNotFoundException
  = Core._MatchServiceError mkServiceConfig "PolicyNotFoundException"
{-# INLINEABLE _PolicyNotFoundException #-}
{-# DEPRECATED _PolicyNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The CORS policy that you specified in the request does not exist.
_CorsPolicyNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CorsPolicyNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "CorsPolicyNotFoundException"
{-# INLINEABLE _CorsPolicyNotFoundException #-}
{-# DEPRECATED _CorsPolicyNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The container that you specified in the request already exists or is being updated.
_ContainerInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ContainerInUseException
  = Core._MatchServiceError mkServiceConfig "ContainerInUseException"
{-# INLINEABLE _ContainerInUseException #-}
{-# DEPRECATED _ContainerInUseException "Use generic-lens or generic-optics instead"  #-}

-- | The service is temporarily unavailable.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError
  = Core._MatchServiceError mkServiceConfig "InternalServerError"
{-# INLINEABLE _InternalServerError #-}
{-# DEPRECATED _InternalServerError "Use generic-lens or generic-optics instead"  #-}

-- | The container that you specified in the request does not exist.
_ContainerNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ContainerNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ContainerNotFoundException"
{-# INLINEABLE _ContainerNotFoundException #-}
{-# DEPRECATED _ContainerNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | A service limit has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
