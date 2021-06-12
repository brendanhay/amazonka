{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceEntitlement.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceEntitlement.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ThrottlingException,
    _InvalidParameterException,
    _InternalServiceErrorException,

    -- * GetEntitlementFilterName
    GetEntitlementFilterName (..),

    -- * Entitlement
    Entitlement (..),
    newEntitlement,
    entitlement_expirationDate,
    entitlement_customerIdentifier,
    entitlement_productCode,
    entitlement_value,
    entitlement_dimension,

    -- * EntitlementValue
    EntitlementValue (..),
    newEntitlementValue,
    entitlementValue_doubleValue,
    entitlementValue_stringValue,
    entitlementValue_booleanValue,
    entitlementValue_integerValue,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceEntitlement.Types.Entitlement
import Network.AWS.MarketplaceEntitlement.Types.EntitlementValue
import Network.AWS.MarketplaceEntitlement.Types.GetEntitlementFilterName
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-01-11@ of the Amazon Marketplace Entitlement Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "MarketplaceEntitlement",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix =
        "entitlement.marketplace",
      Core._serviceSigningName = "aws-marketplace",
      Core._serviceVersion = "2017-01-11",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "MarketplaceEntitlement",
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

-- | The calls to the GetEntitlements API are throttled.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | One or more parameters in your request was invalid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | An internal error has occurred. Retry your request. If the problem
-- persists, post a message with details on the AWS forums.
_InternalServiceErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"
