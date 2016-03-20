{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MarketplaceMetering.Types
    (
    -- * Service Configuration
      marketplaceMetering

    -- * Errors
    , _InvalidEndpointRegionException
    , _InvalidProductCodeException
    , _InvalidUsageDimensionException
    , _DuplicateRequestException
    , _TimestampOutOfBoundsException
    , _ThrottlingException
    , _InternalServiceErrorException
    ) where

import           Network.AWS.Lens
import           Network.AWS.MarketplaceMetering.Types.Product
import           Network.AWS.MarketplaceMetering.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2016-01-14' of the Amazon Marketplace Metering SDK configuration.
marketplaceMetering :: Service
marketplaceMetering =
    Service
    { _svcAbbrev = "MarketplaceMetering"
    , _svcSigner = v4
    , _svcPrefix = "metering.marketplace"
    , _svcVersion = "2016-01-14"
    , _svcEndpoint = defaultEndpoint marketplaceMetering
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The endpoint being called is in a region different from your EC2
-- instance. The region of the Metering service endpoint and the region of
-- the EC2 instance must match.
_InvalidEndpointRegionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEndpointRegionException =
    _ServiceError . hasCode "InvalidEndpointRegionException"

-- | The product code passed does not match the product code used for
-- publishing the product.
_InvalidProductCodeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidProductCodeException =
    _ServiceError . hasCode "InvalidProductCodeException"

-- | The usage dimension does not match one of the UsageDimensions associated
-- with products.
_InvalidUsageDimensionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidUsageDimensionException =
    _ServiceError . hasCode "InvalidUsageDimensionException"

-- | A metering record has already been emitted by the same EC2 instance for
-- the given {usageDimension, timestamp} with a different usageQuantity.
_DuplicateRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateRequestException =
    _ServiceError . hasCode "DuplicateRequestException"

-- | The timestamp value passed in the meterUsage() is out of allowed range.
_TimestampOutOfBoundsException :: AsError a => Getting (First ServiceError) a ServiceError
_TimestampOutOfBoundsException =
    _ServiceError . hasCode "TimestampOutOfBoundsException"

-- | The calls to the MeterUsage API are throttled.
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException = _ServiceError . hasCode "ThrottlingException"

-- | An internal error has occurred. Retry your request. If the problem
-- persists, post a message with details on the AWS forums.
_InternalServiceErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceErrorException =
    _ServiceError . hasCode "InternalServiceErrorException"
