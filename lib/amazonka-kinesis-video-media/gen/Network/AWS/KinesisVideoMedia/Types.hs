-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoMedia.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoMedia.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ConnectionLimitExceededException
    , _InvalidArgumentException
    , _NotAuthorizedException
    , _ClientLimitExceededException
    , _InvalidEndpointException
    , _ResourceNotFoundException

    -- * ContinuationToken
    , ContinuationToken (..)

    -- * StartSelector
    , StartSelector (..)
    , mkStartSelector
    , ssStartSelectorType
    , ssAfterFragmentNumber
    , ssContinuationToken
    , ssStartTimestamp

    -- * StartSelectorType
    , StartSelectorType (..)

    -- * StreamName
    , StreamName (..)

    -- * ContentType
    , ContentType (..)

    -- * AfterFragmentNumber
    , AfterFragmentNumber (..)

    -- * StreamARN
    , StreamARN (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
import Network.AWS.KinesisVideoMedia.Types.ContinuationToken
  
  
import Network.AWS.KinesisVideoMedia.Types.StartSelector
  
  
  
import Network.AWS.KinesisVideoMedia.Types.StartSelectorType
  
  
import Network.AWS.KinesisVideoMedia.Types.StreamName
  
  
import Network.AWS.KinesisVideoMedia.Types.ContentType
  
import Network.AWS.KinesisVideoMedia.Types.AfterFragmentNumber
  
import Network.AWS.KinesisVideoMedia.Types.StreamARN
  

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams Media SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "KinesisVideoMedia",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "kinesisvideo",
                 Core._svcVersion = "2017-09-30", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "KinesisVideoMedia",
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

-- | Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client connections.
_ConnectionLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConnectionLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "ConnectionLimitExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ConnectionLimitExceededException #-}
{-# DEPRECATED _ConnectionLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The value for this input parameter is invalid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException
  = Core._MatchServiceError mkServiceConfig
      "InvalidArgumentException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidArgumentException #-}
{-# DEPRECATED _InvalidArgumentException "Use generic-lens or generic-optics instead"  #-}

-- | Status Code: 403, The caller is not authorized to perform an operation on the given stream, or the token has expired.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException
  = Core._MatchServiceError mkServiceConfig "NotAuthorizedException"
      Core.. Core.hasStatues 401
{-# INLINEABLE _NotAuthorizedException #-}
{-# DEPRECATED _NotAuthorizedException "Use generic-lens or generic-optics instead"  #-}

-- | Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client calls. Try making the call later.
_ClientLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClientLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "ClientLimitExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ClientLimitExceededException #-}
{-# DEPRECATED _ClientLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | Status Code: 400, Caller used wrong endpoint to write data to a stream. On receiving such an exception, the user must call @GetDataEndpoint@ with @AccessMode@ set to "READ" and use the endpoint Kinesis Video returns in the next @GetMedia@ call. 
_InvalidEndpointException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEndpointException
  = Core._MatchServiceError mkServiceConfig
      "InvalidEndpointException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidEndpointException #-}
{-# DEPRECATED _InvalidEndpointException "Use generic-lens or generic-optics instead"  #-}

-- | Status Code: 404, The stream with the given name does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}
