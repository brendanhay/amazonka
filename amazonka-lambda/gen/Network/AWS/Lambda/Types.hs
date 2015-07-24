{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types
    (
    -- * Service
      Lambda

    -- * Errors
    , _PolicyLengthExceededException
    , _UnsupportedMediaTypeException
    , _InvalidRequestContentException
    , _InvalidParameterValueException
    , _RequestTooLargeException
    , _TooManyRequestsException
    , _ServiceException
    , _CodeStorageExceededException
    , _ResourceConflictException
    , _ResourceNotFoundException

    -- * EventSourcePosition
    , EventSourcePosition (..)

    -- * InvocationType
    , InvocationType (..)

    -- * LogType
    , LogType (..)

    -- * Runtime
    , Runtime (..)

    -- * EventSourceMappingConfiguration
    , EventSourceMappingConfiguration
    , eventSourceMappingConfiguration
    , esmcEventSourceARN
    , esmcFunctionARN
    , esmcState
    , esmcUUId
    , esmcLastProcessingResult
    , esmcBatchSize
    , esmcStateTransitionReason
    , esmcLastModified

    -- * FunctionCode
    , FunctionCode
    , functionCode
    , fcS3ObjectVersion
    , fcS3Key
    , fcZipFile
    , fcS3Bucket

    -- * FunctionCodeLocation
    , FunctionCodeLocation
    , functionCodeLocation
    , fclLocation
    , fclRepositoryType

    -- * FunctionConfiguration
    , FunctionConfiguration
    , functionConfiguration
    , fcRuntime
    , fcMemorySize
    , fcFunctionARN
    , fcRole
    , fcFunctionName
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastModified
    , fcDescription
    ) where

import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Lambda.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2015-03-31@ of the Amazon Lambda SDK.
data Lambda

instance AWSService Lambda where
    type Sg Lambda = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "Lambda"
            , _svcPrefix = "lambda"
            , _svcVersion = "2015-03-31"
            , _svcEndpoint = defaultEndpoint svc
            , _svcPreflight = id
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
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

-- | Lambda function access policy is limited to 20 KB.
_PolicyLengthExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_PolicyLengthExceededException =
    _ServiceError . hasStatus 400 . hasCode "PolicyLengthExceededException"

-- | Prism for UnsupportedMediaTypeException' errors.
_UnsupportedMediaTypeException :: AWSError a => Getting (First ServiceError) a ServiceError
_UnsupportedMediaTypeException =
    _ServiceError . hasStatus 415 . hasCode "UnsupportedMediaTypeException"

-- | The request body could not be parsed as JSON.
_InvalidRequestContentException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidRequestContentException =
    _ServiceError . hasStatus 400 . hasCode "InvalidRequestContentException"

-- | One of the parameters in the request is invalid. For example, if you
-- provided an IAM role for AWS Lambda to assume in the @CreateFunction@ or
-- the @UpdateFunctionConfiguration@ API, that AWS Lambda is unable to
-- assume you will get this exception.
_InvalidParameterValueException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterValueException"

-- | Prism for RequestTooLargeException' errors.
_RequestTooLargeException :: AWSError a => Getting (First ServiceError) a ServiceError
_RequestTooLargeException =
    _ServiceError . hasStatus 413 . hasCode "RequestTooLargeException"

-- | Prism for TooManyRequestsException' errors.
_TooManyRequestsException :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
    _ServiceError . hasStatus 429 . hasCode "TooManyRequestsException"

-- | The AWS Lambda service encountered an internal error.
_ServiceException :: AWSError a => Getting (First ServiceError) a ServiceError
_ServiceException = _ServiceError . hasStatus 500 . hasCode "ServiceException"

-- | Prism for CodeStorageExceededException' errors.
_CodeStorageExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_CodeStorageExceededException =
    _ServiceError . hasStatus 400 . hasCode "CodeStorageExceededException"

-- | The resource already exists.
_ResourceConflictException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceConflictException =
    _ServiceError . hasStatus 409 . hasCode "ResourceConflictException"

-- | The resource (for example, a Lambda function or access policy statement)
-- specified in the request does not exist.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundException"
