{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lambda.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _KMSInvalidStateException,
    _EC2ThrottledException,
    _EFSMountConnectivityException,
    _InvalidRuntimeException,
    _EFSMountFailureException,
    _PolicyLengthExceededException,
    _PreconditionFailedException,
    _EC2AccessDeniedException,
    _InvalidSubnetIDException,
    _CodeVerificationFailedException,
    _UnsupportedMediaTypeException,
    _InvalidRequestContentException,
    _KMSNotFoundException,
    _ENILimitReachedException,
    _InvalidParameterValueException,
    _RequestTooLargeException,
    _InvalidCodeSignatureException,
    _TooManyRequestsException,
    _InvalidSecurityGroupIDException,
    _KMSDisabledException,
    _SubnetIPAddressLimitReachedException,
    _ServiceException,
    _CodeStorageExceededException,
    _CodeSigningConfigNotFoundException,
    _InvalidZipFileException,
    _ProvisionedConcurrencyConfigNotFoundException,
    _ResourceConflictException,
    _ResourceNotReadyException,
    _EC2UnexpectedException,
    _ResourceNotFoundException,
    _EFSIOException,
    _EFSMountTimeoutException,
    _KMSAccessDeniedException,
    _ResourceInUseException,

    -- * Architecture
    Architecture (..),

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
    accountLimit_concurrentExecutions,
    accountLimit_totalCodeSize,
    accountLimit_unreservedConcurrentExecutions,
    accountLimit_codeSizeUnzipped,
    accountLimit_codeSizeZipped,

    -- * AccountUsage
    AccountUsage (..),
    newAccountUsage,
    accountUsage_totalCodeSize,
    accountUsage_functionCount,

    -- * AliasConfiguration
    AliasConfiguration (..),
    newAliasConfiguration,
    aliasConfiguration_routingConfig,
    aliasConfiguration_name,
    aliasConfiguration_functionVersion,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_revisionId,

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
    destinationConfig_onSuccess,
    destinationConfig_onFailure,

    -- * Environment
    Environment (..),
    newEnvironment,
    environment_variables,

    -- * EnvironmentError
    EnvironmentError (..),
    newEnvironmentError,
    environmentError_errorCode,
    environmentError_message,

    -- * EnvironmentResponse
    EnvironmentResponse (..),
    newEnvironmentResponse,
    environmentResponse_variables,
    environmentResponse_error,

    -- * EventSourceMappingConfiguration
    EventSourceMappingConfiguration (..),
    newEventSourceMappingConfiguration,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_startingPosition,

    -- * FileSystemConfig
    FileSystemConfig (..),
    newFileSystemConfig,
    fileSystemConfig_arn,
    fileSystemConfig_localMountPath,

    -- * FunctionCode
    FunctionCode (..),
    newFunctionCode,
    functionCode_s3ObjectVersion,
    functionCode_s3Key,
    functionCode_zipFile,
    functionCode_imageUri,
    functionCode_s3Bucket,

    -- * FunctionCodeLocation
    FunctionCodeLocation (..),
    newFunctionCodeLocation,
    functionCodeLocation_location,
    functionCodeLocation_resolvedImageUri,
    functionCodeLocation_imageUri,
    functionCodeLocation_repositoryType,

    -- * FunctionConfiguration
    FunctionConfiguration (..),
    newFunctionConfiguration,
    functionConfiguration_memorySize,
    functionConfiguration_runtime,
    functionConfiguration_state,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_functionArn,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_packageType,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_environment,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_architectures,
    functionConfiguration_signingJobArn,
    functionConfiguration_role,
    functionConfiguration_vpcConfig,
    functionConfiguration_version,
    functionConfiguration_functionName,
    functionConfiguration_layers,
    functionConfiguration_codeSize,
    functionConfiguration_handler,
    functionConfiguration_timeout,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_stateReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSha256,
    functionConfiguration_tracingConfig,
    functionConfiguration_stateReasonCode,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_description,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_masterArn,

    -- * FunctionEventInvokeConfig
    FunctionEventInvokeConfig (..),
    newFunctionEventInvokeConfig,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,
    functionEventInvokeConfig_destinationConfig,

    -- * GetLayerVersionResponse
    GetLayerVersionResponse (..),
    newGetLayerVersionResponse,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_content,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_version,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_description,
    getLayerVersionResponse_compatibleRuntimes,

    -- * ImageConfig
    ImageConfig (..),
    newImageConfig,
    imageConfig_command,
    imageConfig_entryPoint,
    imageConfig_workingDirectory,

    -- * ImageConfigError
    ImageConfigError (..),
    newImageConfigError,
    imageConfigError_errorCode,
    imageConfigError_message,

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
    layerVersionContentInput_s3ObjectVersion,
    layerVersionContentInput_s3Key,
    layerVersionContentInput_zipFile,
    layerVersionContentInput_s3Bucket,

    -- * LayerVersionContentOutput
    LayerVersionContentOutput (..),
    newLayerVersionContentOutput,
    layerVersionContentOutput_signingProfileVersionArn,
    layerVersionContentOutput_location,
    layerVersionContentOutput_signingJobArn,
    layerVersionContentOutput_codeSize,
    layerVersionContentOutput_codeSha256,

    -- * LayerVersionsListItem
    LayerVersionsListItem (..),
    newLayerVersionsListItem,
    layerVersionsListItem_layerVersionArn,
    layerVersionsListItem_createdDate,
    layerVersionsListItem_version,
    layerVersionsListItem_licenseInfo,
    layerVersionsListItem_compatibleArchitectures,
    layerVersionsListItem_description,
    layerVersionsListItem_compatibleRuntimes,

    -- * LayersListItem
    LayersListItem (..),
    newLayersListItem,
    layersListItem_layerName,
    layersListItem_latestMatchingVersion,
    layersListItem_layerArn,

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
    provisionedConcurrencyConfigListItem_functionArn,
    provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_statusReason,
    provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_lastModified,

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

import qualified Amazonka.Core as Core
import Amazonka.Lambda.Types.AccountLimit
import Amazonka.Lambda.Types.AccountUsage
import Amazonka.Lambda.Types.AliasConfiguration
import Amazonka.Lambda.Types.AliasRoutingConfiguration
import Amazonka.Lambda.Types.AllowedPublishers
import Amazonka.Lambda.Types.Architecture
import Amazonka.Lambda.Types.CodeSigningConfig
import Amazonka.Lambda.Types.CodeSigningPolicies
import Amazonka.Lambda.Types.CodeSigningPolicy
import Amazonka.Lambda.Types.Concurrency
import Amazonka.Lambda.Types.DeadLetterConfig
import Amazonka.Lambda.Types.DestinationConfig
import Amazonka.Lambda.Types.EndPointType
import Amazonka.Lambda.Types.Environment
import Amazonka.Lambda.Types.EnvironmentError
import Amazonka.Lambda.Types.EnvironmentResponse
import Amazonka.Lambda.Types.EventSourceMappingConfiguration
import Amazonka.Lambda.Types.EventSourcePosition
import Amazonka.Lambda.Types.FileSystemConfig
import Amazonka.Lambda.Types.FunctionCode
import Amazonka.Lambda.Types.FunctionCodeLocation
import Amazonka.Lambda.Types.FunctionConfiguration
import Amazonka.Lambda.Types.FunctionEventInvokeConfig
import Amazonka.Lambda.Types.FunctionResponseType
import Amazonka.Lambda.Types.FunctionVersion
import Amazonka.Lambda.Types.GetLayerVersionResponse
import Amazonka.Lambda.Types.ImageConfig
import Amazonka.Lambda.Types.ImageConfigError
import Amazonka.Lambda.Types.ImageConfigResponse
import Amazonka.Lambda.Types.InvocationType
import Amazonka.Lambda.Types.LastUpdateStatus
import Amazonka.Lambda.Types.LastUpdateStatusReasonCode
import Amazonka.Lambda.Types.Layer
import Amazonka.Lambda.Types.LayerVersionContentInput
import Amazonka.Lambda.Types.LayerVersionContentOutput
import Amazonka.Lambda.Types.LayerVersionsListItem
import Amazonka.Lambda.Types.LayersListItem
import Amazonka.Lambda.Types.LogType
import Amazonka.Lambda.Types.OnFailure
import Amazonka.Lambda.Types.OnSuccess
import Amazonka.Lambda.Types.PackageType
import Amazonka.Lambda.Types.ProvisionedConcurrencyConfigListItem
import Amazonka.Lambda.Types.ProvisionedConcurrencyStatusEnum
import Amazonka.Lambda.Types.Runtime
import Amazonka.Lambda.Types.SelfManagedEventSource
import Amazonka.Lambda.Types.SourceAccessConfiguration
import Amazonka.Lambda.Types.SourceAccessType
import Amazonka.Lambda.Types.State
import Amazonka.Lambda.Types.StateReasonCode
import Amazonka.Lambda.Types.TracingConfig
import Amazonka.Lambda.Types.TracingConfigResponse
import Amazonka.Lambda.Types.TracingMode
import Amazonka.Lambda.Types.VpcConfig
import Amazonka.Lambda.Types.VpcConfigResponse
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

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
      Core._serviceTimeout = Prelude.Just 70,
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Lambda was unable to decrypt the environment variables because the KMS
-- key used is in an invalid state for Decrypt. Check the function\'s KMS
-- key settings.
_KMSInvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateException =
  Core._MatchServiceError
    defaultService
    "KMSInvalidStateException"
    Prelude.. Core.hasStatus 502

-- | Lambda was throttled by Amazon EC2 during Lambda function initialization
-- using the execution role provided for the Lambda function.
_EC2ThrottledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EC2ThrottledException =
  Core._MatchServiceError
    defaultService
    "EC2ThrottledException"
    Prelude.. Core.hasStatus 502

-- | The function couldn\'t make a network connection to the configured file
-- system.
_EFSMountConnectivityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EFSMountConnectivityException =
  Core._MatchServiceError
    defaultService
    "EFSMountConnectivityException"
    Prelude.. Core.hasStatus 408

-- | The runtime or runtime version specified is not supported.
_InvalidRuntimeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRuntimeException =
  Core._MatchServiceError
    defaultService
    "InvalidRuntimeException"
    Prelude.. Core.hasStatus 502

-- | The function couldn\'t mount the configured file system due to a
-- permission or configuration issue.
_EFSMountFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EFSMountFailureException =
  Core._MatchServiceError
    defaultService
    "EFSMountFailureException"
    Prelude.. Core.hasStatus 403

-- | The permissions policy for the resource is too large.
-- <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Learn more>
_PolicyLengthExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PolicyLengthExceededException =
  Core._MatchServiceError
    defaultService
    "PolicyLengthExceededException"
    Prelude.. Core.hasStatus 400

-- | The RevisionId provided does not match the latest RevisionId for the
-- Lambda function or alias. Call the @GetFunction@ or the @GetAlias@ API
-- to retrieve the latest RevisionId for your resource.
_PreconditionFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PreconditionFailedException =
  Core._MatchServiceError
    defaultService
    "PreconditionFailedException"
    Prelude.. Core.hasStatus 412

-- | Need additional permissions to configure VPC settings.
_EC2AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EC2AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "EC2AccessDeniedException"
    Prelude.. Core.hasStatus 502

-- | The Subnet ID provided in the Lambda function VPC configuration is
-- invalid.
_InvalidSubnetIDException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnetIDException =
  Core._MatchServiceError
    defaultService
    "InvalidSubnetIDException"
    Prelude.. Core.hasStatus 502

-- | The code signature failed one or more of the validation checks for
-- signature mismatch or expiry, and the code signing policy is set to
-- ENFORCE. Lambda blocks the deployment.
_CodeVerificationFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeVerificationFailedException =
  Core._MatchServiceError
    defaultService
    "CodeVerificationFailedException"
    Prelude.. Core.hasStatus 400

-- | The content type of the @Invoke@ request body is not JSON.
_UnsupportedMediaTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedMediaTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedMediaTypeException"
    Prelude.. Core.hasStatus 415

-- | The request body could not be parsed as JSON.
_InvalidRequestContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestContentException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestContentException"
    Prelude.. Core.hasStatus 400

-- | Lambda was unable to decrypt the environment variables because the KMS
-- key was not found. Check the function\'s KMS key settings.
_KMSNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSNotFoundException =
  Core._MatchServiceError
    defaultService
    "KMSNotFoundException"
    Prelude.. Core.hasStatus 502

-- | Lambda was not able to create an elastic network interface in the VPC,
-- specified as part of Lambda function configuration, because the limit
-- for network interfaces has been reached.
_ENILimitReachedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ENILimitReachedException =
  Core._MatchServiceError
    defaultService
    "ENILimitReachedException"
    Prelude.. Core.hasStatus 502

-- | One of the parameters in the request is invalid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
    Prelude.. Core.hasStatus 400

-- | The request payload exceeded the @Invoke@ request body JSON input limit.
-- For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Limits>.
_RequestTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestTooLargeException =
  Core._MatchServiceError
    defaultService
    "RequestTooLargeException"
    Prelude.. Core.hasStatus 413

-- | The code signature failed the integrity check. Lambda always blocks
-- deployment if the integrity check fails, even if code signing policy is
-- set to WARN.
_InvalidCodeSignatureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCodeSignatureException =
  Core._MatchServiceError
    defaultService
    "InvalidCodeSignatureException"
    Prelude.. Core.hasStatus 400

-- | The request throughput limit was exceeded.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | The Security Group ID provided in the Lambda function VPC configuration
-- is invalid.
_InvalidSecurityGroupIDException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSecurityGroupIDException =
  Core._MatchServiceError
    defaultService
    "InvalidSecurityGroupIDException"
    Prelude.. Core.hasStatus 502

-- | Lambda was unable to decrypt the environment variables because the KMS
-- key used is disabled. Check the Lambda function\'s KMS key settings.
_KMSDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSDisabledException =
  Core._MatchServiceError
    defaultService
    "KMSDisabledException"
    Prelude.. Core.hasStatus 502

-- | Lambda was not able to set up VPC access for the Lambda function because
-- one or more configured subnets has no available IP addresses.
_SubnetIPAddressLimitReachedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetIPAddressLimitReachedException =
  Core._MatchServiceError
    defaultService
    "SubnetIPAddressLimitReachedException"
    Prelude.. Core.hasStatus 502

-- | The Lambda service encountered an internal error.
_ServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"
    Prelude.. Core.hasStatus 500

-- | You have exceeded your maximum total code size per account.
-- <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Learn more>
_CodeStorageExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeStorageExceededException =
  Core._MatchServiceError
    defaultService
    "CodeStorageExceededException"
    Prelude.. Core.hasStatus 400

-- | The specified code signing configuration does not exist.
_CodeSigningConfigNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeSigningConfigNotFoundException =
  Core._MatchServiceError
    defaultService
    "CodeSigningConfigNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Lambda could not unzip the deployment package.
_InvalidZipFileException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidZipFileException =
  Core._MatchServiceError
    defaultService
    "InvalidZipFileException"
    Prelude.. Core.hasStatus 502

-- | The specified configuration does not exist.
_ProvisionedConcurrencyConfigNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ProvisionedConcurrencyConfigNotFoundException =
  Core._MatchServiceError
    defaultService
    "ProvisionedConcurrencyConfigNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The resource already exists, or another operation is in progress.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Prelude.. Core.hasStatus 409

-- | The function is inactive and its VPC connection is no longer available.
-- Wait for the VPC connection to reestablish and try again.
_ResourceNotReadyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotReadyException =
  Core._MatchServiceError
    defaultService
    "ResourceNotReadyException"
    Prelude.. Core.hasStatus 502

-- | Lambda received an unexpected EC2 client exception while setting up for
-- the Lambda function.
_EC2UnexpectedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EC2UnexpectedException =
  Core._MatchServiceError
    defaultService
    "EC2UnexpectedException"
    Prelude.. Core.hasStatus 502

-- | The resource specified in the request does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | An error occurred when reading from or writing to a connected file
-- system.
_EFSIOException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EFSIOException =
  Core._MatchServiceError
    defaultService
    "EFSIOException"
    Prelude.. Core.hasStatus 410

-- | The function was able to make a network connection to the configured
-- file system, but the mount operation timed out.
_EFSMountTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EFSMountTimeoutException =
  Core._MatchServiceError
    defaultService
    "EFSMountTimeoutException"
    Prelude.. Core.hasStatus 408

-- | Lambda was unable to decrypt the environment variables because KMS
-- access was denied. Check the Lambda function\'s KMS permissions.
_KMSAccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "KMSAccessDeniedException"
    Prelude.. Core.hasStatus 502

-- | The operation conflicts with the resource\'s availability. For example,
-- you attempted to update an EventSource Mapping in CREATING, or tried to
-- delete a EventSource mapping currently in the UPDATING state.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 400
