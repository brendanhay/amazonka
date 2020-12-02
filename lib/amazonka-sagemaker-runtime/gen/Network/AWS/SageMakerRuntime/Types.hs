{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMakerRuntime.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMakerRuntime.Types
    (
    -- * Service Configuration
      sageMakerRuntime

    -- * Errors
    , _ServiceUnavailable
    , _ModelError
    , _InternalFailure
    , _ValidationError
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMakerRuntime.Types.Product
import Network.AWS.SageMakerRuntime.Types.Sum
import Network.AWS.Sign.V4

-- | API version @2017-05-13@ of the Amazon SageMaker Runtime SDK configuration.
sageMakerRuntime :: Service
sageMakerRuntime =
  Service
    { _svcAbbrev = "SageMakerRuntime"
    , _svcSigner = v4
    , _svcPrefix = "runtime.sagemaker"
    , _svcVersion = "2017-05-13"
    , _svcEndpoint = defaultEndpoint sageMakerRuntime
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "SageMakerRuntime"
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


-- | Service is unavailable. Try your call again.
--
--
_ServiceUnavailable :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailable =
  _MatchServiceError sageMakerRuntime "ServiceUnavailable" . hasStatus 503


-- | Model (owned by the customer in the container) returned an error 500.
--
--
_ModelError :: AsError a => Getting (First ServiceError) a ServiceError
_ModelError = _MatchServiceError sageMakerRuntime "ModelError" . hasStatus 424


-- | Internal failure occurred.
--
--
_InternalFailure :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailure =
  _MatchServiceError sageMakerRuntime "InternalFailure" . hasStatus 500


-- | Inspect your request and try again.
--
--
_ValidationError :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationError =
  _MatchServiceError sageMakerRuntime "ValidationError" . hasStatus 400

