{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types
    (
    -- * Service Configuration
      lambda

    -- * Errors
    , _EC2ThrottledException
    , _PolicyLengthExceededException
    , _EC2AccessDeniedException
    , _InvalidSubnetIdException
    , _UnsupportedMediaTypeException
    , _InvalidRequestContentException
    , _ENILimitReachedException
    , _InvalidParameterValueException
    , _RequestTooLargeException
    , _TooManyRequestsException
    , _InvalidSecurityGroupIdException
    , _SubnetIPAddressLimitReachedException
    , _ServiceException
    , _CodeStorageExceededException
    , _ResourceConflictException
    , _EC2UnexpectedException
    , _ResourceNotFoundException

    -- * EventSourcePosition
    , EventSourcePosition (..)

    -- * InvocationType
    , InvocationType (..)

    -- * LogType
    , LogType (..)

    -- * Runtime
    , Runtime (..)

    -- * AliasConfiguration
    , AliasConfiguration
    , aliasConfiguration
    , acName
    , acFunctionVersion
    , acAliasARN
    , acDescription

    -- * EventSourceMappingConfiguration
    , EventSourceMappingConfiguration
    , eventSourceMappingConfiguration
    , esmcEventSourceARN
    , esmcState
    , esmcFunctionARN
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
    , fcMemorySize
    , fcRuntime
    , fcFunctionARN
    , fcRole
    , fcVPCConfig
    , fcVersion
    , fcFunctionName
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastModified
    , fcCodeSha256
    , fcDescription

    -- * VPCConfig
    , VPCConfig
    , vpcConfig
    , vpccSecurityGroupIds
    , vpccSubnetIds

    -- * VPCConfigResponse
    , VPCConfigResponse
    , vpcConfigResponse
    , vcSecurityGroupIds
    , vcSubnetIds
    , vcVPCId
    ) where

import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Lambda.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-03-31' of the Amazon Lambda SDK configuration.
lambda :: Service
lambda =
    Service
    { _svcAbbrev = "Lambda"
    , _svcSigner = v4
    , _svcPrefix = "lambda"
    , _svcVersion = "2015-03-31"
    , _svcEndpoint = defaultEndpoint lambda
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Lambda"
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
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | AWS Lambda was throttled by Amazon EC2 during Lambda function initiatization using the execution role provided for the Lambda function.
_EC2ThrottledException :: AsError a => Getting (First ServiceError) a ServiceError
_EC2ThrottledException =
    _ServiceError . hasStatus 502 . hasCode "EC2ThrottledException"

-- | Lambda function access policy is limited to 20 KB.
_PolicyLengthExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyLengthExceededException =
    _ServiceError . hasStatus 400 . hasCode "PolicyLengthExceededException"

-- | Prism for EC2AccessDeniedException' errors.
_EC2AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_EC2AccessDeniedException =
    _ServiceError . hasStatus 502 . hasCode "EC2AccessDeniedException"

-- | The Subnet ID provided in the Lambda function VPC configuration is invalid.
_InvalidSubnetIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnetIdException =
    _ServiceError . hasStatus 502 . hasCode "InvalidSubnetIDException"

-- | The content type of the 'Invoke' request body is not JSON.
_UnsupportedMediaTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedMediaTypeException =
    _ServiceError . hasStatus 415 . hasCode "UnsupportedMediaTypeException"

-- | The request body could not be parsed as JSON.
_InvalidRequestContentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestContentException =
    _ServiceError . hasStatus 400 . hasCode "InvalidRequestContentException"

-- | AWS Lambda was not able to create an Elastic Network Interface (ENI) in the VPC, specified as part of Lambda function configuration, because the limit for network interfaces has been reached.
_ENILimitReachedException :: AsError a => Getting (First ServiceError) a ServiceError
_ENILimitReachedException =
    _ServiceError . hasStatus 502 . hasCode "ENILimitReachedException"

-- | One of the parameters in the request is invalid. For example, if you provided an IAM role for AWS Lambda to assume in the 'CreateFunction' or the 'UpdateFunctionConfiguration' API, that AWS Lambda is unable to assume you will get this exception.
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterValueException"

-- | The request payload exceeded the 'Invoke' request body JSON input limit. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/limits.html Limits>.
_RequestTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestTooLargeException =
    _ServiceError . hasStatus 413 . hasCode "RequestTooLargeException"

-- | Prism for TooManyRequestsException' errors.
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
    _ServiceError . hasStatus 429 . hasCode "TooManyRequestsException"

-- | The Security Group ID provided in the Lambda function VPC configuration is invalid.
_InvalidSecurityGroupIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSecurityGroupIdException =
    _ServiceError . hasStatus 502 . hasCode "InvalidSecurityGroupIDException"

-- | AWS Lambda was not able to set up VPC access for the Lambda function because one or more configured subnets has no available IP addresses.
_SubnetIPAddressLimitReachedException :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetIPAddressLimitReachedException =
    _ServiceError .
    hasStatus 502 . hasCode "SubnetIPAddressLimitReachedException"

-- | The AWS Lambda service encountered an internal error.
_ServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceException = _ServiceError . hasStatus 500 . hasCode "ServiceException"

-- | You have exceeded your maximum total code size per account. <http://docs.aws.amazon.com/lambda/latest/dg/limits.html Limits>
_CodeStorageExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_CodeStorageExceededException =
    _ServiceError . hasStatus 400 . hasCode "CodeStorageExceededException"

-- | The resource already exists.
_ResourceConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceConflictException =
    _ServiceError . hasStatus 409 . hasCode "ResourceConflictException"

-- | AWS Lambda received an unexpected EC2 client exception while setting up for the Lambda function.
_EC2UnexpectedException :: AsError a => Getting (First ServiceError) a ServiceError
_EC2UnexpectedException =
    _ServiceError . hasStatus 502 . hasCode "EC2UnexpectedException"

-- | The resource (for example, a Lambda function or access policy statement) specified in the request does not exist.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundException"
