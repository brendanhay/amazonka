-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceEntitlement.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceEntitlement.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidParameterException,
    _ThrottlingException,
    _InternalServiceErrorException,

    -- * EntitlementValue
    EntitlementValue (..),
    mkEntitlementValue,
    evBooleanValue,
    evDoubleValue,
    evIntegerValue,
    evStringValue,

    -- * GetEntitlementFilterName
    GetEntitlementFilterName (..),

    -- * NonEmptyString
    NonEmptyString (..),

    -- * FilterValue
    FilterValue (..),

    -- * ProductCode
    ProductCode (..),

    -- * Entitlement
    Entitlement (..),
    mkEntitlement,
    eCustomerIdentifier,
    eDimension,
    eExpirationDate,
    eProductCode,
    eValue,

    -- * StringValue
    StringValue (..),

    -- * NextToken
    NextToken (..),
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceEntitlement.Types.Entitlement
import Network.AWS.MarketplaceEntitlement.Types.EntitlementValue
import Network.AWS.MarketplaceEntitlement.Types.FilterValue
import Network.AWS.MarketplaceEntitlement.Types.GetEntitlementFilterName
import Network.AWS.MarketplaceEntitlement.Types.NextToken
import Network.AWS.MarketplaceEntitlement.Types.NonEmptyString
import Network.AWS.MarketplaceEntitlement.Types.ProductCode
import Network.AWS.MarketplaceEntitlement.Types.StringValue
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-01-11@ of the Amazon Marketplace Entitlement Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "MarketplaceEntitlement",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "entitlement.marketplace",
      Core._svcVersion = "2017-01-11",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "MarketplaceEntitlement",
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

-- | One or more parameters in your request was invalid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterException"
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead." #-}

-- | The calls to the GetEntitlements API are throttled.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError mkServiceConfig "ThrottlingException"
{-# DEPRECATED _ThrottlingException "Use generic-lens or generic-optics instead." #-}

-- | An internal error has occurred. Retry your request. If the problem persists, post a message with details on the AWS forums.
_InternalServiceErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalServiceErrorException"
{-# DEPRECATED _InternalServiceErrorException "Use generic-lens or generic-optics instead." #-}
