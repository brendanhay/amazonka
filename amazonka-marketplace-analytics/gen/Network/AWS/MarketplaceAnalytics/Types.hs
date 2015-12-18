{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceAnalytics.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MarketplaceAnalytics.Types
    (
    -- * Service Configuration
      marketplaceAnalytics

    -- * Errors
    , _MarketplaceCommerceAnalyticsException

    -- * DataSetType
    , DataSetType (..)
    ) where

import           Network.AWS.Lens
import           Network.AWS.MarketplaceAnalytics.Types.Product
import           Network.AWS.MarketplaceAnalytics.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-07-01' of the Amazon Marketplace Commerce Analytics SDK configuration.
marketplaceAnalytics :: Service
marketplaceAnalytics =
    Service
    { _svcAbbrev = "MarketplaceAnalytics"
    , _svcSigner = v4
    , _svcPrefix = "marketplacecommerceanalytics"
    , _svcVersion = "2015-07-01"
    , _svcEndpoint = defaultEndpoint marketplaceAnalytics
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
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | This exception is thrown when an internal service error occurs.
_MarketplaceCommerceAnalyticsException :: AsError a => Getting (First ServiceError) a ServiceError
_MarketplaceCommerceAnalyticsException =
    _ServiceError . hasCode "MarketplaceCommerceAnalyticsException"
