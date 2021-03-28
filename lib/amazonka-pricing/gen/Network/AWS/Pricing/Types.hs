-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pricing.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidParameterException
    , _InternalErrorException
    , _ExpiredNextTokenException
    , _NotFoundException
    , _InvalidNextTokenException

    -- * AttributeValue
    , AttributeValue (..)
    , mkAttributeValue
    , avValue

    -- * PriceListItemJSON
    , PriceListItemJSON (..)

    -- * PricingService
    , PricingService (..)
    , mkPricingService
    , psAttributeNames
    , psServiceCode

    -- * FilterType
    , FilterType (..)

    -- * Filter
    , Filter (..)
    , mkFilter
    , fType
    , fField
    , fValue
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
import Network.AWS.Pricing.Types.AttributeValue
  
import Network.AWS.Pricing.Types.PriceListItemJSON
  
import Network.AWS.Pricing.Types.PricingService
  
import Network.AWS.Pricing.Types.FilterType
  
  
  
  
  
import Network.AWS.Pricing.Types.Filter
  

-- | API version @2017-10-15@ of the Amazon Price List Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "Pricing",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "api.pricing",
                 Core._svcVersion = "2017-10-15", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "Pricing",
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

-- | One or more parameters had an invalid value.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterException"
{-# INLINEABLE _InvalidParameterException #-}
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead"  #-}

-- | An error on the server occurred during the processing of your request. Try again later.
_InternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalErrorException
  = Core._MatchServiceError mkServiceConfig "InternalErrorException"
{-# INLINEABLE _InternalErrorException #-}
{-# DEPRECATED _InternalErrorException "Use generic-lens or generic-optics instead"  #-}

-- | The pagination token expired. Try again without a pagination token.
_ExpiredNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredNextTokenException
  = Core._MatchServiceError mkServiceConfig
      "ExpiredNextTokenException"
{-# INLINEABLE _ExpiredNextTokenException #-}
{-# DEPRECATED _ExpiredNextTokenException "Use generic-lens or generic-optics instead"  #-}

-- | The requested resource can't be found.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException
  = Core._MatchServiceError mkServiceConfig "NotFoundException"
{-# INLINEABLE _NotFoundException #-}
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The pagination token is invalid. Try again without a pagination token.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidNextTokenException"
{-# INLINEABLE _InvalidNextTokenException #-}
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead"  #-}
