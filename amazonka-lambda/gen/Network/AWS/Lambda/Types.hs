{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidCodeSignatureException,
    _KMSInvalidStateException,
    _KMSNotFoundException,
    _EC2UnexpectedException,
    _UnsupportedMediaTypeException,
    _InvalidZipFileException,
    _ResourceNotReadyException,
    _ResourceConflictException,
    _ProvisionedConcurrencyConfigNotFoundException,
    _EC2AccessDeniedException,
    _InvalidSubnetIDException,
    _EFSMountFailureException,
    _InvalidSecurityGroupIDException,
    _EC2ThrottledException,
    _RequestTooLargeException,
    _EFSMountConnectivityException,
    _InvalidParameterValueException,
    _ENILimitReachedException,
    _ResourceInUseException,
    _EFSMountTimeoutException,
    _EFSIOException,
    _KMSAccessDeniedException,
    _InvalidRequestContentException,
    _ResourceNotFoundException,
    _PolicyLengthExceededException,
    _CodeSigningConfigNotFoundException,
    _CodeVerificationFailedException,
    _CodeStorageExceededException,
    _PreconditionFailedException,
    _KMSDisabledException,
    _SubnetIPAddressLimitReachedException,
    _TooManyRequestsException,
    _ServiceException,
    _InvalidRuntimeException,

    -- * CodeSigningPolicy
    CodeSigningPolicy (..),

    -- * EndPointType
    EndPointType (..),

    -- * EventSourcePosition
    EventSourcePosition (..),

    -- * FunctionResponseType
    FunctionResponseType (..),

    -- * FunctionVersion
    FunctionVersion (..),

    -- * InvocationType
    InvocationType (..),

    -- * LastUpdateStatus
    LastUpdateStatus (..),

    -- * LastUpdateStatusReasonCode
    LastUpdateStatusReasonCode (..),

    -- * LogType
    LogType (..),

    -- * PackageType
    PackageType (..),

    -- * ProvisionedConcurrencyStatusEnum
    ProvisionedConcurrencyStatusEnum (..),

    -- * Runtime
    Runtime (..),

    -- * SourceAccessType
    SourceAccessType (..),

    -- * State
    State (..),

    -- * StateReasonCode
    StateReasonCode (..),

    -- * TracingMode
    TracingMode (..),

    -- * AccountLimit
    AccountLimit (..),
    newAccountLimit,
    accountLimit_codeSizeUnzipped,
    accountLimit_concurrentExecutions,
    accountLimit_unreservedConcurrentExecutions,
    accountLimit_codeSizeZipped,
    accountLimit_totalCodeSize,

    -- * AccountUsage
    AccountUsage (..),
    newAccountUsage,
    accountUsage_functionCount,
    accountUsage_totalCodeSize,

    -- * AliasConfiguration
    AliasConfiguration (..),
    newAliasConfiguration,
    aliasConfiguration_revisionId,
    aliasConfiguration_routingConfig,
    aliasConfiguration_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_description,
    aliasConfiguration_aliasArn,

    -- * AliasRoutingConfiguration
    AliasRoutingConfiguration (..),
    newAliasRoutingConfiguration,
    aliasRoutingConfiguration_additionalVersionWeights,

    -- * AllowedPublishers
    AllowedPublishers (..),
    newAllowedPublishers,
    allowedPublishers_signingProfileVersionArns,

    -- * CodeSigningConfig
    CodeSigningConfig (..),
    newCodeSigningConfig,
    codeSigningConfig_description,
    codeSigningConfig_codeSigningConfigId,
    codeSigningConfig_codeSigningConfigArn,
    codeSigningConfig_allowedPublishers,
    codeSigningConfig_codeSigningPolicies,
    codeSigningConfig_lastModified,

    -- * CodeSigningPolicies
    CodeSigningPolicies (..),
    newCodeSigningPolicies,
    codeSigningPolicies_untrustedArtifactOnDeployment,

    -- * Concurrency
    Concurrency (..),
    newConcurrency,
    concurrency_reservedConcurrentExecutions,

    -- * DeadLetterConfig
    DeadLetterConfig (..),
    newDeadLetterConfig,
    deadLetterConfig_targetArn,

    -- * DestinationConfig
    DestinationConfig (..),
    newDestinationConfig,
    destinationConfig_onFailure,
    destinationConfig_onSuccess,

    -- * Environment
    Environment (..),
    newEnvironment,
    environment_variables,

    -- * EnvironmentError
    EnvironmentError (..),
    newEnvironmentError,
    environmentError_message,
    environmentError_errorCode,

    -- * EnvironmentResponse
    EnvironmentResponse (..),
    newEnvironmentResponse,
    environmentResponse_variables,
    environmentResponse_error,

    -- * EventSourceMappingConfiguration
    EventSourceMappingConfiguration (..),
    newEventSourceMappingConfiguration,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_sourceAccessConfigurations,

    -- * FileSystemConfig
    FileSystemConfig (..),
    newFileSystemConfig,
    fileSystemConfig_arn,
    fileSystemConfig_localMountPath,

    -- * FunctionCode
    FunctionCode (..),
    newFunctionCode,
    functionCode_imageUri,
    functionCode_s3Bucket,
    functionCode_zipFile,
    functionCode_s3ObjectVersion,
    functionCode_s3Key,

    -- * FunctionCodeLocation
    FunctionCodeLocation (..),
    newFunctionCodeLocation,
    functionCodeLocation_imageUri,
    functionCodeLocation_resolvedImageUri,
    functionCodeLocation_location,
    functionCodeLocation_repositoryType,

    -- * FunctionConfiguration
    FunctionConfiguration (..),
    newFunctionConfiguration,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_vpcConfig,
    functionConfiguration_memorySize,
    functionConfiguration_masterArn,
    functionConfiguration_revisionId,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_codeSha256,
    functionConfiguration_stateReason,
    functionConfiguration_timeout,
    functionConfiguration_handler,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_functionName,
    functionConfiguration_environment,
    functionConfiguration_version,
    functionConfiguration_functionArn,
    functionConfiguration_state,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_runtime,
    functionConfiguration_role,
    functionConfiguration_signingJobArn,
    functionConfiguration_stateReasonCode,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_tracingConfig,
    functionConfiguration_description,
    functionConfiguration_lastModified,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_layers,
    functionConfiguration_codeSize,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_packageType,

    -- * FunctionEventInvokeConfig
    FunctionEventInvokeConfig (..),
    newFunctionEventInvokeConfig,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,

    -- * GetLayerVersionResponse
    GetLayerVersionResponse (..),
    newGetLayerVersionResponse,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_version,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_content,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_description,
    getLayerVersionResponse_licenseInfo,

    -- * ImageConfig
    ImageConfig (..),
    newImageConfig,
    imageConfig_workingDirectory,
    imageConfig_entryPoint,
    imageConfig_command,

    -- * ImageConfigError
    ImageConfigError (..),
    newImageConfigError,
    imageConfigError_message,
    imageConfigError_errorCode,

    -- * ImageConfigResponse
    ImageConfigResponse (..),
    newImageConfigResponse,
    imageConfigResponse_imageConfig,
    imageConfigResponse_error,

    -- * Layer
    Layer (..),
    newLayer,
    layer_signingProfileVersionArn,
    layer_arn,
    layer_signingJobArn,
    layer_codeSize,

    -- * LayerVersionContentInput
    LayerVersionContentInput (..),
    newLayerVersionContentInput,
    layerVersionContentInput_s3Bucket,
    layerVersionContentInput_zipFile,
    layerVersionContentInput_s3ObjectVersion,
    layerVersionContentInput_s3Key,

    -- * LayerVersionContentOutput
    LayerVersionContentOutput (..),
    newLayerVersionContentOutput,
    layerVersionContentOutput_signingProfileVersionArn,
    layerVersionContentOutput_codeSha256,
    layerVersionContentOutput_signingJobArn,
    layerVersionContentOutput_codeSize,
    layerVersionContentOutput_location,

    -- * LayerVersionsListItem
    LayerVersionsListItem (..),
    newLayerVersionsListItem,
    layerVersionsListItem_createdDate,
    layerVersionsListItem_version,
    layerVersionsListItem_layerVersionArn,
    layerVersionsListItem_compatibleRuntimes,
    layerVersionsListItem_description,
    layerVersionsListItem_licenseInfo,

    -- * LayersListItem
    LayersListItem (..),
    newLayersListItem,
    layersListItem_layerArn,
    layersListItem_layerName,
    layersListItem_latestMatchingVersion,

    -- * OnFailure
    OnFailure (..),
    newOnFailure,
    onFailure_destination,

    -- * OnSuccess
    OnSuccess (..),
    newOnSuccess,
    onSuccess_destination,

    -- * ProvisionedConcurrencyConfigListItem
    ProvisionedConcurrencyConfigListItem (..),
    newProvisionedConcurrencyConfigListItem,
    provisionedConcurrencyConfigListItem_status,
    provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_functionArn,
    provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_lastModified,
    provisionedConcurrencyConfigListItem_statusReason,

    -- * SelfManagedEventSource
    SelfManagedEventSource (..),
    newSelfManagedEventSource,
    selfManagedEventSource_endpoints,

    -- * SourceAccessConfiguration
    SourceAccessConfiguration (..),
    newSourceAccessConfiguration,
    sourceAccessConfiguration_uri,
    sourceAccessConfiguration_type,

    -- * TracingConfig
    TracingConfig (..),
    newTracingConfig,
    tracingConfig_mode,

    -- * TracingConfigResponse
    TracingConfigResponse (..),
    newTracingConfigResponse,
    tracingConfigResponse_mode,

    -- * VpcConfig
    VpcConfig (..),
    newVpcConfig,
    vpcConfig_securityGroupIds,
    vpcConfig_subnetIds,

    -- * VpcConfigResponse
    VpcConfigResponse (..),
    newVpcConfigResponse,
    vpcConfigResponse_securityGroupIds,
    vpcConfigResponse_subnetIds,
    vpcConfigResponse_vpcId,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.AccountLimit
import Network.AWS.Lambda.Types.AccountUsage
import Network.AWS.Lambda.Types.AliasConfiguration
import Network.AWS.Lambda.Types.AliasRoutingConfiguration
import Network.AWS.Lambda.Types.AllowedPublishers
import Network.AWS.Lambda.Types.CodeSigningConfig
import Network.AWS.Lambda.Types.CodeSigningPolicies
import Network.AWS.Lambda.Types.CodeSigningPolicy
import Network.AWS.Lambda.Types.Concurrency
import Network.AWS.Lambda.Types.DeadLetterConfig
import Network.AWS.Lambda.Types.DestinationConfig
import Network.AWS.Lambda.Types.EndPointType
import Network.AWS.Lambda.Types.Environment
import Network.AWS.Lambda.Types.EnvironmentError
import Network.AWS.Lambda.Types.EnvironmentResponse
import Network.AWS.Lambda.Types.EventSourceMappingConfiguration
import Network.AWS.Lambda.Types.EventSourcePosition
import Network.AWS.Lambda.Types.FileSystemConfig
import Network.AWS.Lambda.Types.FunctionCode
import Network.AWS.Lambda.Types.FunctionCodeLocation
import Network.AWS.Lambda.Types.FunctionConfiguration
import Network.AWS.Lambda.Types.FunctionEventInvokeConfig
import Network.AWS.Lambda.Types.FunctionResponseType
import Network.AWS.Lambda.Types.FunctionVersion
import Network.AWS.Lambda.Types.GetLayerVersionResponse
import Network.AWS.Lambda.Types.ImageConfig
import Network.AWS.Lambda.Types.ImageConfigError
import Network.AWS.Lambda.Types.ImageConfigResponse
import Network.AWS.Lambda.Types.InvocationType
import Network.AWS.Lambda.Types.LastUpdateStatus
import Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
import Network.AWS.Lambda.Types.Layer
import Network.AWS.Lambda.Types.LayerVersionContentInput
import Network.AWS.Lambda.Types.LayerVersionContentOutput
import Network.AWS.Lambda.Types.LayerVersionsListItem
import Network.AWS.Lambda.Types.LayersListItem
import Network.AWS.Lambda.Types.LogType
import Network.AWS.Lambda.Types.OnFailure
import Network.AWS.Lambda.Types.OnSuccess
import Network.AWS.Lambda.Types.PackageType
import Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem
import Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
import Network.AWS.Lambda.Types.Runtime
import Network.AWS.Lambda.Types.SelfManagedEventSource
import Network.AWS.Lambda.Types.SourceAccessConfiguration
import Network.AWS.Lambda.Types.SourceAccessType
import Network.AWS.Lambda.Types.State
import Network.AWS.Lambda.Types.StateReasonCode
import Network.AWS.Lambda.Types.TracingConfig
import Network.AWS.Lambda.Types.TracingConfigResponse
import Network.AWS.Lambda.Types.TracingMode
import Network.AWS.Lambda.Types.VpcConfig
import Network.AWS.Lambda.Types.VpcConfigResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-03-31@ of the Amazon Lambda SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Lambda",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "lambda",
      Core._serviceSigningName = "lambda",
      Core._serviceVersion = "2015-03-31",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Lambda",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The code signature failed the integrity check. Lambda always blocks
-- deployment if the integrity check fails, even if code signing policy is
-- set to WARN.
_InvalidCodeSignatureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCodeSignatureException =
  Core._MatchServiceError
    defaultService
    "InvalidCodeSignatureException"
    Core.. Core.hasStatus 400

-- | Lambda was unable to decrypt the environment variables because the KMS
-- key used is in an invalid state for Decrypt. Check the function\'s KMS
-- key settings.
_KMSInvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateException =
  Core._MatchServiceError
    defaultService
    "KMSInvalidStateException"
    Core.. Core.hasStatus 502

-- | Lambda was unable to decrypt the environment variables because the KMS
-- key was not found. Check the function\'s KMS key settings.
_KMSNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSNotFoundException =
  Core._MatchServiceError
    defaultService
    "KMSNotFoundException"
    Core.. Core.hasStatus 502

-- | AWS Lambda received an unexpected EC2 client exception while setting up
-- for the Lambda function.
_EC2UnexpectedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EC2UnexpectedException =
  Core._MatchServiceError
    defaultService
    "EC2UnexpectedException"
    Core.. Core.hasStatus 502

-- | The content type of the @Invoke@ request body is not JSON.
_UnsupportedMediaTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedMediaTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedMediaTypeException"
    Core.. Core.hasStatus 415

-- | AWS Lambda could not unzip the deployment package.
_InvalidZipFileException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidZipFileException =
  Core._MatchServiceError
    defaultService
    "InvalidZipFileException"
    Core.. Core.hasStatus 502

-- | The function is inactive and its VPC connection is no longer available.
-- Wait for the VPC connection to reestablish and try again.
_ResourceNotReadyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotReadyException =
  Core._MatchServiceError
    defaultService
    "ResourceNotReadyException"
    Core.. Core.hasStatus 502

-- | The resource already exists, or another operation is in progress.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Core.. Core.hasStatus 409

-- | The specified configuration does not exist.
_ProvisionedConcurrencyConfigNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProvisionedConcurrencyConfigNotFoundException =
  Core._MatchServiceError
    defaultService
    "ProvisionedConcurrencyConfigNotFoundException"
    Core.. Core.hasStatus 404

-- | Need additional permissions to configure VPC settings.
_EC2AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EC2AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "EC2AccessDeniedException"
    Core.. Core.hasStatus 502

-- | The Subnet ID provided in the Lambda function VPC configuration is
-- invalid.
_InvalidSubnetIDException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSubnetIDException =
  Core._MatchServiceError
    defaultService
    "InvalidSubnetIDException"
    Core.. Core.hasStatus 502

-- | The function couldn\'t mount the configured file system due to a
-- permission or configuration issue.
_EFSMountFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EFSMountFailureException =
  Core._MatchServiceError
    defaultService
    "EFSMountFailureException"
    Core.. Core.hasStatus 403

-- | The Security Group ID provided in the Lambda function VPC configuration
-- is invalid.
_InvalidSecurityGroupIDException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSecurityGroupIDException =
  Core._MatchServiceError
    defaultService
    "InvalidSecurityGroupIDException"
    Core.. Core.hasStatus 502

-- | AWS Lambda was throttled by Amazon EC2 during Lambda function
-- initialization using the execution role provided for the Lambda
-- function.
_EC2ThrottledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EC2ThrottledException =
  Core._MatchServiceError
    defaultService
    "EC2ThrottledException"
    Core.. Core.hasStatus 502

-- | The request payload exceeded the @Invoke@ request body JSON input limit.
-- For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Limits>.
_RequestTooLargeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestTooLargeException =
  Core._MatchServiceError
    defaultService
    "RequestTooLargeException"
    Core.. Core.hasStatus 413

-- | The function couldn\'t make a network connection to the configured file
-- system.
_EFSMountConnectivityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EFSMountConnectivityException =
  Core._MatchServiceError
    defaultService
    "EFSMountConnectivityException"
    Core.. Core.hasStatus 408

-- | One of the parameters in the request is invalid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
    Core.. Core.hasStatus 400

-- | AWS Lambda was not able to create an elastic network interface in the
-- VPC, specified as part of Lambda function configuration, because the
-- limit for network interfaces has been reached.
_ENILimitReachedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ENILimitReachedException =
  Core._MatchServiceError
    defaultService
    "ENILimitReachedException"
    Core.. Core.hasStatus 502

-- | The operation conflicts with the resource\'s availability. For example,
-- you attempted to update an EventSource Mapping in CREATING, or tried to
-- delete a EventSource mapping currently in the UPDATING state.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Core.. Core.hasStatus 400

-- | The function was able to make a network connection to the configured
-- file system, but the mount operation timed out.
_EFSMountTimeoutException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EFSMountTimeoutException =
  Core._MatchServiceError
    defaultService
    "EFSMountTimeoutException"
    Core.. Core.hasStatus 408

-- | An error occured when reading from or writing to a connected file
-- system.
_EFSIOException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EFSIOException =
  Core._MatchServiceError
    defaultService
    "EFSIOException"
    Core.. Core.hasStatus 410

-- | Lambda was unable to decrypt the environment variables because KMS
-- access was denied. Check the Lambda function\'s KMS permissions.
_KMSAccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "KMSAccessDeniedException"
    Core.. Core.hasStatus 502

-- | The request body could not be parsed as JSON.
_InvalidRequestContentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestContentException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestContentException"
    Core.. Core.hasStatus 400

-- | The resource specified in the request does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Core.. Core.hasStatus 404

-- | The permissions policy for the resource is too large.
-- <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Learn more>
_PolicyLengthExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyLengthExceededException =
  Core._MatchServiceError
    defaultService
    "PolicyLengthExceededException"
    Core.. Core.hasStatus 400

-- | The specified code signing configuration does not exist.
_CodeSigningConfigNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CodeSigningConfigNotFoundException =
  Core._MatchServiceError
    defaultService
    "CodeSigningConfigNotFoundException"
    Core.. Core.hasStatus 404

-- | The code signature failed one or more of the validation checks for
-- signature mismatch or expiry, and the code signing policy is set to
-- ENFORCE. Lambda blocks the deployment.
_CodeVerificationFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CodeVerificationFailedException =
  Core._MatchServiceError
    defaultService
    "CodeVerificationFailedException"
    Core.. Core.hasStatus 400

-- | You have exceeded your maximum total code size per account.
-- <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Learn more>
_CodeStorageExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CodeStorageExceededException =
  Core._MatchServiceError
    defaultService
    "CodeStorageExceededException"
    Core.. Core.hasStatus 400

-- | The RevisionId provided does not match the latest RevisionId for the
-- Lambda function or alias. Call the @GetFunction@ or the @GetAlias@ API
-- to retrieve the latest RevisionId for your resource.
_PreconditionFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PreconditionFailedException =
  Core._MatchServiceError
    defaultService
    "PreconditionFailedException"
    Core.. Core.hasStatus 412

-- | Lambda was unable to decrypt the environment variables because the KMS
-- key used is disabled. Check the Lambda function\'s KMS key settings.
_KMSDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSDisabledException =
  Core._MatchServiceError
    defaultService
    "KMSDisabledException"
    Core.. Core.hasStatus 502

-- | AWS Lambda was not able to set up VPC access for the Lambda function
-- because one or more configured subnets has no available IP addresses.
_SubnetIPAddressLimitReachedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetIPAddressLimitReachedException =
  Core._MatchServiceError
    defaultService
    "SubnetIPAddressLimitReachedException"
    Core.. Core.hasStatus 502

-- | The request throughput limit was exceeded.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Core.. Core.hasStatus 429

-- | The AWS Lambda service encountered an internal error.
_ServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"
    Core.. Core.hasStatus 500

-- | The runtime or runtime version specified is not supported.
_InvalidRuntimeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRuntimeException =
  Core._MatchServiceError
    defaultService
    "InvalidRuntimeException"
    Core.. Core.hasStatus 502
