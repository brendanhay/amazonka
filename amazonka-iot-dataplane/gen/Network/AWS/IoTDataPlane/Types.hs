{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTDataPlane.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTDataPlane.Types
    (
    -- * Service Configuration
      ioTDataPlane

    -- * Errors
    , _InvalidRequestException
    , _ConflictException
    , _RequestEntityTooLargeException
    , _ThrottlingException
    , _MethodNotAllowedException
    , _InternalFailureException
    , _ServiceUnavailableException
    , _UnauthorizedException
    , _ResourceNotFoundException
    , _UnsupportedDocumentEncodingException
    ) where

import           Network.AWS.IoTDataPlane.Types.Product
import           Network.AWS.IoTDataPlane.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-05-28' of the Amazon IoT Data Plane SDK configuration.
ioTDataPlane :: Service
ioTDataPlane =
    Service
    { _svcAbbrev = "IoTDataPlane"
    , _svcSigner = v4
    , _svcPrefix = "data.iot"
    , _svcVersion = "2015-05-28"
    , _svcEndpoint = defaultEndpoint ioTDataPlane
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

-- | The request is not valid.
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
    _ServiceError . hasStatus 400 . hasCode "InvalidRequestException"

-- | The specified version does not match the version of the document.
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException =
    _ServiceError . hasStatus 409 . hasCode "ConflictException"

-- | The payload exceeds the maximum size allowed.
_RequestEntityTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestEntityTooLargeException =
    _ServiceError . hasStatus 413 . hasCode "RequestEntityTooLargeException"

-- | The rate exceeds the limit.
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException =
    _ServiceError . hasStatus 429 . hasCode "ThrottlingException"

-- | The specified combination of HTTP verb and URI is not supported.
_MethodNotAllowedException :: AsError a => Getting (First ServiceError) a ServiceError
_MethodNotAllowedException =
    _ServiceError . hasStatus 405 . hasCode "MethodNotAllowedException"

-- | An unexpected error has occurred.
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException =
    _ServiceError . hasStatus 500 . hasCode "InternalFailureException"

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
    _ServiceError . hasStatus 503 . hasCode "ServiceUnavailableException"

-- | You are not authorized to perform this operation.
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException =
    _ServiceError . hasStatus 401 . hasCode "UnauthorizedException"

-- | The specified resource does not exist.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundException"

-- | The document encoding is not supported.
_UnsupportedDocumentEncodingException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedDocumentEncodingException =
    _ServiceError .
    hasStatus 415 . hasCode "UnsupportedDocumentEncodingException"
