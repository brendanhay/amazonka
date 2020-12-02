{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroupsTagging.Types
    (
    -- * Service Configuration
      resourceGroupsTagging

    -- * Errors
    , _InvalidParameterException
    , _ThrottledException
    , _PaginationTokenExpiredException
    , _InternalServiceException

    -- * ResourceErrorCode
    , ResourceErrorCode (..)

    -- * FailureInfo
    , FailureInfo
    , failureInfo
    , fiErrorCode
    , fiErrorMessage
    , fiStatusCode

    -- * ResourceTagMapping
    , ResourceTagMapping
    , resourceTagMapping
    , rtmResourceARN
    , rtmTags

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * TagFilter
    , TagFilter
    , tagFilter
    , tfValues
    , tfKey
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ResourceGroupsTagging.Types.Product
import Network.AWS.ResourceGroupsTagging.Types.Sum
import Network.AWS.Sign.V4

-- | API version @2017-01-26@ of the Amazon Resource Groups Tagging API SDK configuration.
resourceGroupsTagging :: Service
resourceGroupsTagging =
  Service
    { _svcAbbrev = "ResourceGroupsTagging"
    , _svcSigner = v4
    , _svcPrefix = "tagging"
    , _svcVersion = "2017-01-26"
    , _svcEndpoint = defaultEndpoint resourceGroupsTagging
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "ResourceGroupsTagging"
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


-- | A parameter is missing or a malformed string or invalid or out-of-range value was supplied for the request parameter.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError resourceGroupsTagging "InvalidParameterException"


-- | The request was denied to limit the frequency of submitted requests.
--
--
_ThrottledException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottledException =
  _MatchServiceError resourceGroupsTagging "ThrottledException"


-- | A @PaginationToken@ is valid for a maximum of 15 minutes. Your request was denied because the specified @PaginationToken@ has expired.
--
--
_PaginationTokenExpiredException :: AsError a => Getting (First ServiceError) a ServiceError
_PaginationTokenExpiredException =
  _MatchServiceError resourceGroupsTagging "PaginationTokenExpiredException"


-- | The request processing failed because of an unknown error, exception, or failure. You can retry the request.
--
--
_InternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceException =
  _MatchServiceError resourceGroupsTagging "InternalServiceException"

