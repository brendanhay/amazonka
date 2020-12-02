{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pricing.Types
  ( -- * Service Configuration
    pricing,

    -- * Errors

    -- * FilterType
    FilterType (..),

    -- * AttributeValue
    AttributeValue,
    attributeValue,
    avValue,

    -- * Filter
    Filter,
    filter',
    fType,
    fField,
    fValue,

    -- * PricingService
    PricingService,
    pricingService,
    psAttributeNames,
    psServiceCode,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Pricing.Types.AttributeValue
import Network.AWS.Pricing.Types.Filter
import Network.AWS.Pricing.Types.FilterType
import Network.AWS.Pricing.Types.PricingService
import Network.AWS.Sign.V4

-- | API version @2017-10-15@ of the Amazon Price List Service SDK configuration.
pricing :: Service
pricing =
  Service
    { _svcAbbrev = "Pricing",
      _svcSigner = v4,
      _svcPrefix = "api.pricing",
      _svcVersion = "2017-10-15",
      _svcEndpoint = defaultEndpoint pricing,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Pricing",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
