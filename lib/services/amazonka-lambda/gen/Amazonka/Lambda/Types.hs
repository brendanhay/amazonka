{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lambda.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidZipFileException,
    _SubnetIPAddressLimitReachedException,
    _PolicyLengthExceededException,
    _ResourceNotReadyException,
    _EFSMountConnectivityException,
    _PreconditionFailedException,
    _EFSMountFailureException,
    _CodeSigningConfigNotFoundException,
    _InvalidRuntimeException,
    _InvalidSubnetIDException,
    _EC2AccessDeniedException,
    _UnsupportedMediaTypeException,
    _InvalidCodeSignatureException,
    _CodeStorageExceededException,
    _EC2ThrottledException,
    _EFSMountTimeoutException,
    _ResourceNotFoundException,
    _KMSAccessDeniedException,
    _ResourceInUseException,
    _EC2UnexpectedException,
    _ProvisionedConcurrencyConfigNotFoundException,
    _CodeVerificationFailedException,
    _KMSDisabledException,
    _ResourceConflictException,
    _KMSInvalidStateException,
    _ServiceException,
    _InvalidSecurityGroupIDException,
    _EFSIOException,
    _RequestTooLargeException,
    _KMSNotFoundException,
    _ENILimitReachedException,
    _TooManyRequestsException,
    _InvalidParameterValueException,
    _InvalidRequestContentException,

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

    -- * FunctionUrlAuthType
    FunctionUrlAuthType (..),

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
    accountLimit_unreservedConcurrentExecutions,
    accountLimit_totalCodeSize,
    accountLimit_concurrentExecutions,
    accountLimit_codeSizeZipped,
    accountLimit_codeSizeUnzipped,

    -- * AccountUsage
    AccountUsage (..),
    newAccountUsage,
    accountUsage_totalCodeSize,
    accountUsage_functionCount,

    -- * AliasConfiguration
    AliasConfiguration (..),
    newAliasConfiguration,
    aliasConfiguration_name,
    aliasConfiguration_routingConfig,
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

    -- * AmazonManagedKafkaEventSourceConfig
    AmazonManagedKafkaEventSourceConfig (..),
    newAmazonManagedKafkaEventSourceConfig,
    amazonManagedKafkaEventSourceConfig_consumerGroupId,

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

    -- * Cors
    Cors (..),
    newCors,
    cors_allowHeaders,
    cors_exposeHeaders,
    cors_allowCredentials,
    cors_allowMethods,
    cors_allowOrigins,
    cors_maxAge,

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
    environmentResponse_error,
    environmentResponse_variables,

    -- * EphemeralStorage
    EphemeralStorage (..),
    newEphemeralStorage,
    ephemeralStorage_size,

    -- * EventSourceMappingConfiguration
    EventSourceMappingConfiguration (..),
    newEventSourceMappingConfiguration,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_sourceAccessConfigurations,

    -- * FileSystemConfig
    FileSystemConfig (..),
    newFileSystemConfig,
    fileSystemConfig_arn,
    fileSystemConfig_localMountPath,

    -- * Filter
    Filter (..),
    newFilter,
    filter_pattern,

    -- * FilterCriteria
    FilterCriteria (..),
    newFilterCriteria,
    filterCriteria_filters,

    -- * FunctionCode
    FunctionCode (..),
    newFunctionCode,
    functionCode_s3Bucket,
    functionCode_imageUri,
    functionCode_s3Key,
    functionCode_zipFile,
    functionCode_s3ObjectVersion,

    -- * FunctionCodeLocation
    FunctionCodeLocation (..),
    newFunctionCodeLocation,
    functionCodeLocation_imageUri,
    functionCodeLocation_location,
    functionCodeLocation_repositoryType,
    functionCodeLocation_resolvedImageUri,

    -- * FunctionConfiguration
    FunctionConfiguration (..),
    newFunctionConfiguration,
    functionConfiguration_tracingConfig,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_masterArn,
    functionConfiguration_functionArn,
    functionConfiguration_timeout,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_memorySize,
    functionConfiguration_codeSha256,
    functionConfiguration_environment,
    functionConfiguration_vpcConfig,
    functionConfiguration_state,
    functionConfiguration_functionName,
    functionConfiguration_runtime,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_description,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_handler,
    functionConfiguration_layers,
    functionConfiguration_stateReasonCode,
    functionConfiguration_packageType,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_signingJobArn,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastModified,
    functionConfiguration_role,
    functionConfiguration_architectures,
    functionConfiguration_stateReason,
    functionConfiguration_version,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_codeSize,

    -- * FunctionEventInvokeConfig
    FunctionEventInvokeConfig (..),
    newFunctionEventInvokeConfig,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,

    -- * FunctionUrlConfig
    FunctionUrlConfig (..),
    newFunctionUrlConfig,
    functionUrlConfig_cors,
    functionUrlConfig_functionUrl,
    functionUrlConfig_functionArn,
    functionUrlConfig_creationTime,
    functionUrlConfig_lastModifiedTime,
    functionUrlConfig_authType,

    -- * GetLayerVersionResponse
    GetLayerVersionResponse (..),
    newGetLayerVersionResponse,
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_description,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_content,
    getLayerVersionResponse_version,

    -- * ImageConfig
    ImageConfig (..),
    newImageConfig,
    imageConfig_command,
    imageConfig_entryPoint,
    imageConfig_workingDirectory,

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
    layer_arn,
    layer_signingProfileVersionArn,
    layer_signingJobArn,
    layer_codeSize,

    -- * LayerVersionContentInput
    LayerVersionContentInput (..),
    newLayerVersionContentInput,
    layerVersionContentInput_s3Bucket,
    layerVersionContentInput_s3Key,
    layerVersionContentInput_zipFile,
    layerVersionContentInput_s3ObjectVersion,

    -- * LayerVersionContentOutput
    LayerVersionContentOutput (..),
    newLayerVersionContentOutput,
    layerVersionContentOutput_codeSha256,
    layerVersionContentOutput_signingProfileVersionArn,
    layerVersionContentOutput_location,
    layerVersionContentOutput_signingJobArn,
    layerVersionContentOutput_codeSize,

    -- * LayerVersionsListItem
    LayerVersionsListItem (..),
    newLayerVersionsListItem,
    layerVersionsListItem_compatibleArchitectures,
    layerVersionsListItem_layerVersionArn,
    layerVersionsListItem_licenseInfo,
    layerVersionsListItem_description,
    layerVersionsListItem_compatibleRuntimes,
    layerVersionsListItem_createdDate,
    layerVersionsListItem_version,

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
    provisionedConcurrencyConfigListItem_functionArn,
    provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_statusReason,
    provisionedConcurrencyConfigListItem_status,
    provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_lastModified,
    provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions,

    -- * SelfManagedEventSource
    SelfManagedEventSource (..),
    newSelfManagedEventSource,
    selfManagedEventSource_endpoints,

    -- * SelfManagedKafkaEventSourceConfig
    SelfManagedKafkaEventSourceConfig (..),
    newSelfManagedKafkaEventSourceConfig,
    selfManagedKafkaEventSourceConfig_consumerGroupId,

    -- * SourceAccessConfiguration
    SourceAccessConfiguration (..),
    newSourceAccessConfiguration,
    sourceAccessConfiguration_type,
    sourceAccessConfiguration_uri,

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
    vpcConfigResponse_vpcId,
    vpcConfigResponse_subnetIds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lambda.Types.AccountLimit
import Amazonka.Lambda.Types.AccountUsage
import Amazonka.Lambda.Types.AliasConfiguration
import Amazonka.Lambda.Types.AliasRoutingConfiguration
import Amazonka.Lambda.Types.AllowedPublishers
import Amazonka.Lambda.Types.AmazonManagedKafkaEventSourceConfig
import Amazonka.Lambda.Types.Architecture
import Amazonka.Lambda.Types.CodeSigningConfig
import Amazonka.Lambda.Types.CodeSigningPolicies
import Amazonka.Lambda.Types.CodeSigningPolicy
import Amazonka.Lambda.Types.Concurrency
import Amazonka.Lambda.Types.Cors
import Amazonka.Lambda.Types.DeadLetterConfig
import Amazonka.Lambda.Types.DestinationConfig
import Amazonka.Lambda.Types.EndPointType
import Amazonka.Lambda.Types.Environment
import Amazonka.Lambda.Types.EnvironmentError
import Amazonka.Lambda.Types.EnvironmentResponse
import Amazonka.Lambda.Types.EphemeralStorage
import Amazonka.Lambda.Types.EventSourceMappingConfiguration
import Amazonka.Lambda.Types.EventSourcePosition
import Amazonka.Lambda.Types.FileSystemConfig
import Amazonka.Lambda.Types.Filter
import Amazonka.Lambda.Types.FilterCriteria
import Amazonka.Lambda.Types.FunctionCode
import Amazonka.Lambda.Types.FunctionCodeLocation
import Amazonka.Lambda.Types.FunctionConfiguration
import Amazonka.Lambda.Types.FunctionEventInvokeConfig
import Amazonka.Lambda.Types.FunctionResponseType
import Amazonka.Lambda.Types.FunctionUrlAuthType
import Amazonka.Lambda.Types.FunctionUrlConfig
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
import Amazonka.Lambda.Types.SelfManagedKafkaEventSourceConfig
import Amazonka.Lambda.Types.SourceAccessConfiguration
import Amazonka.Lambda.Types.SourceAccessType
import Amazonka.Lambda.Types.State
import Amazonka.Lambda.Types.StateReasonCode
import Amazonka.Lambda.Types.TracingConfig
import Amazonka.Lambda.Types.TracingConfigResponse
import Amazonka.Lambda.Types.TracingMode
import Amazonka.Lambda.Types.VpcConfig
import Amazonka.Lambda.Types.VpcConfigResponse
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-03-31@ of the Amazon Lambda SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Lambda",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "lambda",
      Core.signingName = "lambda",
      Core.version = "2015-03-31",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Lambda",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Lambda could not unzip the deployment package.
_InvalidZipFileException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidZipFileException =
  Core._MatchServiceError
    defaultService
    "InvalidZipFileException"
    Prelude.. Core.hasStatus 502

-- | Lambda was not able to set up VPC access for the Lambda function because
-- one or more configured subnets has no available IP addresses.
_SubnetIPAddressLimitReachedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetIPAddressLimitReachedException =
  Core._MatchServiceError
    defaultService
    "SubnetIPAddressLimitReachedException"
    Prelude.. Core.hasStatus 502

-- | The permissions policy for the resource is too large.
-- <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Learn more>
_PolicyLengthExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PolicyLengthExceededException =
  Core._MatchServiceError
    defaultService
    "PolicyLengthExceededException"
    Prelude.. Core.hasStatus 400

-- | The function is inactive and its VPC connection is no longer available.
-- Wait for the VPC connection to reestablish and try again.
_ResourceNotReadyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotReadyException =
  Core._MatchServiceError
    defaultService
    "ResourceNotReadyException"
    Prelude.. Core.hasStatus 502

-- | The function couldn\'t make a network connection to the configured file
-- system.
_EFSMountConnectivityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EFSMountConnectivityException =
  Core._MatchServiceError
    defaultService
    "EFSMountConnectivityException"
    Prelude.. Core.hasStatus 408

-- | The RevisionId provided does not match the latest RevisionId for the
-- Lambda function or alias. Call the @GetFunction@ or the @GetAlias@ API
-- to retrieve the latest RevisionId for your resource.
_PreconditionFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PreconditionFailedException =
  Core._MatchServiceError
    defaultService
    "PreconditionFailedException"
    Prelude.. Core.hasStatus 412

-- | The function couldn\'t mount the configured file system due to a
-- permission or configuration issue.
_EFSMountFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EFSMountFailureException =
  Core._MatchServiceError
    defaultService
    "EFSMountFailureException"
    Prelude.. Core.hasStatus 403

-- | The specified code signing configuration does not exist.
_CodeSigningConfigNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeSigningConfigNotFoundException =
  Core._MatchServiceError
    defaultService
    "CodeSigningConfigNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The runtime or runtime version specified is not supported.
_InvalidRuntimeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRuntimeException =
  Core._MatchServiceError
    defaultService
    "InvalidRuntimeException"
    Prelude.. Core.hasStatus 502

-- | The Subnet ID provided in the Lambda function VPC configuration is
-- invalid.
_InvalidSubnetIDException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnetIDException =
  Core._MatchServiceError
    defaultService
    "InvalidSubnetIDException"
    Prelude.. Core.hasStatus 502

-- | Need additional permissions to configure VPC settings.
_EC2AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EC2AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "EC2AccessDeniedException"
    Prelude.. Core.hasStatus 502

-- | The content type of the @Invoke@ request body is not JSON.
_UnsupportedMediaTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedMediaTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedMediaTypeException"
    Prelude.. Core.hasStatus 415

-- | The code signature failed the integrity check. Lambda always blocks
-- deployment if the integrity check fails, even if code signing policy is
-- set to WARN.
_InvalidCodeSignatureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCodeSignatureException =
  Core._MatchServiceError
    defaultService
    "InvalidCodeSignatureException"
    Prelude.. Core.hasStatus 400

-- | You have exceeded your maximum total code size per account.
-- <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Learn more>
_CodeStorageExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeStorageExceededException =
  Core._MatchServiceError
    defaultService
    "CodeStorageExceededException"
    Prelude.. Core.hasStatus 400

-- | Lambda was throttled by Amazon EC2 during Lambda function initialization
-- using the execution role provided for the Lambda function.
_EC2ThrottledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EC2ThrottledException =
  Core._MatchServiceError
    defaultService
    "EC2ThrottledException"
    Prelude.. Core.hasStatus 502

-- | The function was able to make a network connection to the configured
-- file system, but the mount operation timed out.
_EFSMountTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EFSMountTimeoutException =
  Core._MatchServiceError
    defaultService
    "EFSMountTimeoutException"
    Prelude.. Core.hasStatus 408

-- | The resource specified in the request does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

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

-- | Lambda received an unexpected EC2 client exception while setting up for
-- the Lambda function.
_EC2UnexpectedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EC2UnexpectedException =
  Core._MatchServiceError
    defaultService
    "EC2UnexpectedException"
    Prelude.. Core.hasStatus 502

-- | The specified configuration does not exist.
_ProvisionedConcurrencyConfigNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ProvisionedConcurrencyConfigNotFoundException =
  Core._MatchServiceError
    defaultService
    "ProvisionedConcurrencyConfigNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The code signature failed one or more of the validation checks for
-- signature mismatch or expiry, and the code signing policy is set to
-- ENFORCE. Lambda blocks the deployment.
_CodeVerificationFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeVerificationFailedException =
  Core._MatchServiceError
    defaultService
    "CodeVerificationFailedException"
    Prelude.. Core.hasStatus 400

-- | Lambda was unable to decrypt the environment variables because the KMS
-- key used is disabled. Check the Lambda function\'s KMS key settings.
_KMSDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSDisabledException =
  Core._MatchServiceError
    defaultService
    "KMSDisabledException"
    Prelude.. Core.hasStatus 502

-- | The resource already exists, or another operation is in progress.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Prelude.. Core.hasStatus 409

-- | Lambda was unable to decrypt the environment variables because the KMS
-- key used is in an invalid state for Decrypt. Check the function\'s KMS
-- key settings.
_KMSInvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateException =
  Core._MatchServiceError
    defaultService
    "KMSInvalidStateException"
    Prelude.. Core.hasStatus 502

-- | The Lambda service encountered an internal error.
_ServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"
    Prelude.. Core.hasStatus 500

-- | The Security Group ID provided in the Lambda function VPC configuration
-- is invalid.
_InvalidSecurityGroupIDException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSecurityGroupIDException =
  Core._MatchServiceError
    defaultService
    "InvalidSecurityGroupIDException"
    Prelude.. Core.hasStatus 502

-- | An error occurred when reading from or writing to a connected file
-- system.
_EFSIOException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EFSIOException =
  Core._MatchServiceError
    defaultService
    "EFSIOException"
    Prelude.. Core.hasStatus 410

-- | The request payload exceeded the @Invoke@ request body JSON input limit.
-- For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Limits>.
_RequestTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestTooLargeException =
  Core._MatchServiceError
    defaultService
    "RequestTooLargeException"
    Prelude.. Core.hasStatus 413

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

-- | The request throughput limit was exceeded.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | One of the parameters in the request is invalid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
    Prelude.. Core.hasStatus 400

-- | The request body could not be parsed as JSON.
_InvalidRequestContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestContentException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestContentException"
    Prelude.. Core.hasStatus 400
