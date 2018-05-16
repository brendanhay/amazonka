{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStore.Types
    (
    -- * Service Configuration
      mediaStore

    -- * Errors
    , _PolicyNotFoundException
    , _CORSPolicyNotFoundException
    , _ContainerInUseException
    , _InternalServerError
    , _ContainerNotFoundException
    , _LimitExceededException

    -- * ContainerStatus
    , ContainerStatus (..)

    -- * MethodName
    , MethodName (..)

    -- * CORSRule
    , CORSRule
    , corsRule
    , crAllowedMethods
    , crMaxAgeSeconds
    , crAllowedHeaders
    , crAllowedOrigins
    , crExposeHeaders

    -- * Container
    , Container
    , container
    , cCreationTime
    , cStatus
    , cARN
    , cName
    , cEndpoint
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types.Product
import Network.AWS.MediaStore.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-09-01@ of the Amazon Elemental MediaStore SDK configuration.
mediaStore :: Service
mediaStore =
  Service
    { _svcAbbrev = "MediaStore"
    , _svcSigner = v4
    , _svcPrefix = "mediastore"
    , _svcVersion = "2017-09-01"
    , _svcEndpoint = defaultEndpoint mediaStore
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "MediaStore"
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


-- | Could not perform an operation on a policy that does not exist.
--
--
_PolicyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyNotFoundException =
  _MatchServiceError mediaStore "PolicyNotFoundException"


-- | Could not perform an operation on a policy that does not exist.
--
--
_CORSPolicyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_CORSPolicyNotFoundException =
  _MatchServiceError mediaStore "CorsPolicyNotFoundException"


-- | Resource already exists or is being updated.
--
--
_ContainerInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ContainerInUseException =
  _MatchServiceError mediaStore "ContainerInUseException"


-- | The service is temporarily unavailable.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _MatchServiceError mediaStore "InternalServerError"


-- | Could not perform an operation on a container that does not exist.
--
--
_ContainerNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ContainerNotFoundException =
  _MatchServiceError mediaStore "ContainerNotFoundException"


-- | A service limit has been exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError mediaStore "LimitExceededException"

