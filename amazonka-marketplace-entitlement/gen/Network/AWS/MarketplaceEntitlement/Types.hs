{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceEntitlement.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MarketplaceEntitlement.Types
    (
    -- * Service Configuration
      marketplaceEntitlement

    -- * Errors
    , _InvalidParameterException
    , _ThrottlingException
    , _InternalServiceErrorException

    -- * GetEntitlementFilterName
    , GetEntitlementFilterName (..)

    -- * Entitlement
    , Entitlement
    , entitlement
    , eDimension
    , eValue
    , eExpirationDate
    , eCustomerIdentifier
    , eProductCode

    -- * EntitlementValue
    , EntitlementValue
    , entitlementValue
    , evIntegerValue
    , evDoubleValue
    , evStringValue
    , evBooleanValue
    ) where

import Network.AWS.Lens
import Network.AWS.MarketplaceEntitlement.Types.Product
import Network.AWS.MarketplaceEntitlement.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-01-11@ of the Amazon Marketplace Entitlement Service SDK configuration.
marketplaceEntitlement :: Service
marketplaceEntitlement =
  Service
    { _svcAbbrev = "MarketplaceEntitlement"
    , _svcSigner = v4
    , _svcPrefix = "entitlement.marketplace"
    , _svcVersion = "2017-01-11"
    , _svcEndpoint = defaultEndpoint marketplaceEntitlement
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "MarketplaceEntitlement"
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
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | One or more parameters in your request was invalid.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError marketplaceEntitlement "InvalidParameterException"


-- | The calls to the GetEntitlements API are throttled.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException =
  _MatchServiceError marketplaceEntitlement "ThrottlingException"


-- | An internal error has occurred. Retry your request. If the problem persists, post a message with details on the AWS forums.
--
--
_InternalServiceErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceErrorException =
  _MatchServiceError marketplaceEntitlement "InternalServiceErrorException"

