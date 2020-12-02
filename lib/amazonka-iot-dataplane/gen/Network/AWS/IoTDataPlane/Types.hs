{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTDataPlane.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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

import Network.AWS.IoTDataPlane.Types.Product
import Network.AWS.IoTDataPlane.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-05-28@ of the Amazon IoT Data Plane SDK configuration.
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
  , _svcError = parseJSONError "IoTDataPlane"
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
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The request is not valid.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
  _MatchServiceError ioTDataPlane "InvalidRequestException" . hasStatus 400


-- | The specified version does not match the version of the document.
--
--
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException =
  _MatchServiceError ioTDataPlane "ConflictException" . hasStatus 409


-- | The payload exceeds the maximum size allowed.
--
--
_RequestEntityTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestEntityTooLargeException =
  _MatchServiceError ioTDataPlane "RequestEntityTooLargeException" .
  hasStatus 413


-- | The rate exceeds the limit.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException =
  _MatchServiceError ioTDataPlane "ThrottlingException" . hasStatus 429


-- | The specified combination of HTTP verb and URI is not supported.
--
--
_MethodNotAllowedException :: AsError a => Getting (First ServiceError) a ServiceError
_MethodNotAllowedException =
  _MatchServiceError ioTDataPlane "MethodNotAllowedException" . hasStatus 405


-- | An unexpected error has occurred.
--
--
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException =
  _MatchServiceError ioTDataPlane "InternalFailureException" . hasStatus 500


-- | The service is temporarily unavailable.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError ioTDataPlane "ServiceUnavailableException" . hasStatus 503


-- | You are not authorized to perform this operation.
--
--
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException =
  _MatchServiceError ioTDataPlane "UnauthorizedException" . hasStatus 401


-- | The specified resource does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError ioTDataPlane "ResourceNotFoundException" . hasStatus 404


-- | The document encoding is not supported.
--
--
_UnsupportedDocumentEncodingException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedDocumentEncodingException =
  _MatchServiceError ioTDataPlane "UnsupportedDocumentEncodingException" .
  hasStatus 415

