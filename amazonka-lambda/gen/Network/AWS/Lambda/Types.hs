{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types
    (
    -- * Service Configuration
      lambda

    -- * Errors
    , _KMSInvalidStateException
    , _EC2ThrottledException
    , _InvalidRuntimeException
    , _PolicyLengthExceededException
    , _PreconditionFailedException
    , _EC2AccessDeniedException
    , _InvalidSubnetIdException
    , _UnsupportedMediaTypeException
    , _InvalidRequestContentException
    , _KMSNotFoundException
    , _ENILimitReachedException
    , _InvalidParameterValueException
    , _RequestTooLargeException
    , _TooManyRequestsException
    , _InvalidSecurityGroupIdException
    , _KMSDisabledException
    , _SubnetIPAddressLimitReachedException
    , _ServiceException
    , _CodeStorageExceededException
    , _InvalidZipFileException
    , _ResourceConflictException
    , _EC2UnexpectedException
    , _ResourceNotFoundException
    , _KMSAccessDeniedException

    -- * EventSourcePosition
    , EventSourcePosition (..)

    -- * FunctionVersion
    , FunctionVersion (..)

    -- * InvocationType
    , InvocationType (..)

    -- * LogType
    , LogType (..)

    -- * Runtime
    , Runtime (..)

    -- * TracingMode
    , TracingMode (..)

    -- * AccountLimit
    , AccountLimit
    , accountLimit
    , alConcurrentExecutions
    , alTotalCodeSize
    , alUnreservedConcurrentExecutions
    , alCodeSizeUnzipped
    , alCodeSizeZipped

    -- * AccountUsage
    , AccountUsage
    , accountUsage
    , auTotalCodeSize
    , auFunctionCount

    -- * AliasConfiguration
    , AliasConfiguration
    , aliasConfiguration
    , acRoutingConfig
    , acName
    , acFunctionVersion
    , acAliasARN
    , acDescription
    , acRevisionId

    -- * AliasRoutingConfiguration
    , AliasRoutingConfiguration
    , aliasRoutingConfiguration
    , arcAdditionalVersionWeights

    -- * Concurrency
    , Concurrency
    , concurrency
    , cReservedConcurrentExecutions

    -- * DeadLetterConfig
    , DeadLetterConfig
    , deadLetterConfig
    , dlcTargetARN

    -- * Environment
    , Environment
    , environment
    , eVariables

    -- * EnvironmentError
    , EnvironmentError
    , environmentError
    , eeErrorCode
    , eeMessage

    -- * EnvironmentResponse
    , EnvironmentResponse
    , environmentResponse
    , envVariables
    , envError

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
    , fcKMSKeyARN
    , fcEnvironment
    , fcDeadLetterConfig
    , fcRole
    , fcVPCConfig
    , fcVersion
    , fcFunctionName
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastModified
    , fcCodeSha256
    , fcTracingConfig
    , fcDescription
    , fcRevisionId
    , fcMasterARN

    -- * TracingConfig
    , TracingConfig
    , tracingConfig
    , tMode

    -- * TracingConfigResponse
    , TracingConfigResponse
    , tracingConfigResponse
    , tcMode

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

import Network.AWS.Lambda.Types.Product
import Network.AWS.Lambda.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-03-31@ of the Amazon Lambda SDK configuration.
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


-- | Lambda was unable to decrypt the environment variables because the KMS key used is in an invalid state for Decrypt. Check the function's KMS key settings.
--
--
_KMSInvalidStateException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSInvalidStateException =
  _MatchServiceError lambda "KMSInvalidStateException" . hasStatus 502


-- | AWS Lambda was throttled by Amazon EC2 during Lambda function initialization using the execution role provided for the Lambda function.
--
--
_EC2ThrottledException :: AsError a => Getting (First ServiceError) a ServiceError
_EC2ThrottledException =
  _MatchServiceError lambda "EC2ThrottledException" . hasStatus 502


-- | The runtime or runtime version specified is not supported.
--
--
_InvalidRuntimeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRuntimeException =
  _MatchServiceError lambda "InvalidRuntimeException" . hasStatus 502


-- | Lambda function access policy is limited to 20 KB.
--
--
_PolicyLengthExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyLengthExceededException =
  _MatchServiceError lambda "PolicyLengthExceededException" . hasStatus 400


-- | The RevisionId provided does not match the latest RevisionId for the Lambda function or alias. Call the @GetFunction@ or the @GetAlias@ API to retrieve the latest RevisionId for your resource.
--
--
_PreconditionFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_PreconditionFailedException =
  _MatchServiceError lambda "PreconditionFailedException" . hasStatus 412


-- |
--
--
_EC2AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_EC2AccessDeniedException =
  _MatchServiceError lambda "EC2AccessDeniedException" . hasStatus 502


-- | The Subnet ID provided in the Lambda function VPC configuration is invalid.
--
--
_InvalidSubnetIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnetIdException =
  _MatchServiceError lambda "InvalidSubnetIDException" . hasStatus 502


-- | The content type of the @Invoke@ request body is not JSON.
--
--
_UnsupportedMediaTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedMediaTypeException =
  _MatchServiceError lambda "UnsupportedMediaTypeException" . hasStatus 415


-- | The request body could not be parsed as JSON.
--
--
_InvalidRequestContentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestContentException =
  _MatchServiceError lambda "InvalidRequestContentException" . hasStatus 400


-- | Lambda was unable to decrypt the environment variables because the KMS key was not found. Check the function's KMS key settings.
--
--
_KMSNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSNotFoundException =
  _MatchServiceError lambda "KMSNotFoundException" . hasStatus 502


-- | AWS Lambda was not able to create an Elastic Network Interface (ENI) in the VPC, specified as part of Lambda function configuration, because the limit for network interfaces has been reached.
--
--
_ENILimitReachedException :: AsError a => Getting (First ServiceError) a ServiceError
_ENILimitReachedException =
  _MatchServiceError lambda "ENILimitReachedException" . hasStatus 502


-- | One of the parameters in the request is invalid. For example, if you provided an IAM role for AWS Lambda to assume in the @CreateFunction@ or the @UpdateFunctionConfiguration@ API, that AWS Lambda is unable to assume you will get this exception.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
  _MatchServiceError lambda "InvalidParameterValueException" . hasStatus 400


-- | The request payload exceeded the @Invoke@ request body JSON input limit. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/limits.html Limits> .
--
--
_RequestTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestTooLargeException =
  _MatchServiceError lambda "RequestTooLargeException" . hasStatus 413


-- |
--
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
  _MatchServiceError lambda "TooManyRequestsException" . hasStatus 429


-- | The Security Group ID provided in the Lambda function VPC configuration is invalid.
--
--
_InvalidSecurityGroupIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSecurityGroupIdException =
  _MatchServiceError lambda "InvalidSecurityGroupIDException" . hasStatus 502


-- | Lambda was unable to decrypt the environment variables because the KMS key used is disabled. Check the Lambda function's KMS key settings.
--
--
_KMSDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSDisabledException =
  _MatchServiceError lambda "KMSDisabledException" . hasStatus 502


-- | AWS Lambda was not able to set up VPC access for the Lambda function because one or more configured subnets has no available IP addresses.
--
--
_SubnetIPAddressLimitReachedException :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetIPAddressLimitReachedException =
  _MatchServiceError lambda "SubnetIPAddressLimitReachedException" .
  hasStatus 502


-- | The AWS Lambda service encountered an internal error.
--
--
_ServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceException = _MatchServiceError lambda "ServiceException" . hasStatus 500


-- | You have exceeded your maximum total code size per account. <http://docs.aws.amazon.com/lambda/latest/dg/limits.html Limits>
--
--
_CodeStorageExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_CodeStorageExceededException =
  _MatchServiceError lambda "CodeStorageExceededException" . hasStatus 400


-- | AWS Lambda could not unzip the function zip file.
--
--
_InvalidZipFileException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidZipFileException =
  _MatchServiceError lambda "InvalidZipFileException" . hasStatus 502


-- | The resource already exists.
--
--
_ResourceConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceConflictException =
  _MatchServiceError lambda "ResourceConflictException" . hasStatus 409


-- | AWS Lambda received an unexpected EC2 client exception while setting up for the Lambda function.
--
--
_EC2UnexpectedException :: AsError a => Getting (First ServiceError) a ServiceError
_EC2UnexpectedException =
  _MatchServiceError lambda "EC2UnexpectedException" . hasStatus 502


-- | The resource (for example, a Lambda function or access policy statement) specified in the request does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError lambda "ResourceNotFoundException" . hasStatus 404


-- | Lambda was unable to decrypt the environment variables because KMS access was denied. Check the Lambda function's KMS permissions.
--
--
_KMSAccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSAccessDeniedException =
  _MatchServiceError lambda "KMSAccessDeniedException" . hasStatus 502

