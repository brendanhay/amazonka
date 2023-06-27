{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lambda.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CodeSigningConfigNotFoundException,
    _CodeStorageExceededException,
    _CodeVerificationFailedException,
    _EC2AccessDeniedException,
    _EC2ThrottledException,
    _EC2UnexpectedException,
    _EFSIOException,
    _EFSMountConnectivityException,
    _EFSMountFailureException,
    _EFSMountTimeoutException,
    _ENILimitReachedException,
    _InvalidCodeSignatureException,
    _InvalidParameterValueException,
    _InvalidRequestContentException,
    _InvalidRuntimeException,
    _InvalidSecurityGroupIDException,
    _InvalidSubnetIDException,
    _InvalidZipFileException,
    _KMSAccessDeniedException,
    _KMSDisabledException,
    _KMSInvalidStateException,
    _KMSNotFoundException,
    _PolicyLengthExceededException,
    _PreconditionFailedException,
    _ProvisionedConcurrencyConfigNotFoundException,
    _RecursiveInvocationException,
    _RequestTooLargeException,
    _ResourceConflictException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ResourceNotReadyException,
    _ServiceException,
    _SnapStartException,
    _SnapStartNotReadyException,
    _SnapStartTimeoutException,
    _SubnetIPAddressLimitReachedException,
    _TooManyRequestsException,
    _UnsupportedMediaTypeException,

    -- * Architecture
    Architecture (..),

    -- * CodeSigningPolicy
    CodeSigningPolicy (..),

    -- * EndPointType
    EndPointType (..),

    -- * EventSourcePosition
    EventSourcePosition (..),

    -- * FullDocument
    FullDocument (..),

    -- * FunctionResponseType
    FunctionResponseType (..),

    -- * FunctionUrlAuthType
    FunctionUrlAuthType (..),

    -- * FunctionVersion
    FunctionVersion (..),

    -- * InvocationType
    InvocationType (..),

    -- * InvokeMode
    InvokeMode (..),

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

    -- * ResponseStreamingInvocationType
    ResponseStreamingInvocationType (..),

    -- * Runtime
    Runtime (..),

    -- * SnapStartApplyOn
    SnapStartApplyOn (..),

    -- * SnapStartOptimizationStatus
    SnapStartOptimizationStatus (..),

    -- * SourceAccessType
    SourceAccessType (..),

    -- * State
    State (..),

    -- * StateReasonCode
    StateReasonCode (..),

    -- * TracingMode
    TracingMode (..),

    -- * UpdateRuntimeOn
    UpdateRuntimeOn (..),

    -- * AccountLimit
    AccountLimit (..),
    newAccountLimit,
    accountLimit_codeSizeUnzipped,
    accountLimit_codeSizeZipped,
    accountLimit_concurrentExecutions,
    accountLimit_totalCodeSize,
    accountLimit_unreservedConcurrentExecutions,

    -- * AccountUsage
    AccountUsage (..),
    newAccountUsage,
    accountUsage_functionCount,
    accountUsage_totalCodeSize,

    -- * AliasConfiguration
    AliasConfiguration (..),
    newAliasConfiguration,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_revisionId,
    aliasConfiguration_routingConfig,

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
    cors_allowCredentials,
    cors_allowHeaders,
    cors_allowMethods,
    cors_allowOrigins,
    cors_exposeHeaders,
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

    -- * DocumentDBEventSourceConfig
    DocumentDBEventSourceConfig (..),
    newDocumentDBEventSourceConfig,
    documentDBEventSourceConfig_collectionName,
    documentDBEventSourceConfig_databaseName,
    documentDBEventSourceConfig_fullDocument,

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
    environmentResponse_error,
    environmentResponse_variables,

    -- * EphemeralStorage
    EphemeralStorage (..),
    newEphemeralStorage,
    ephemeralStorage_size,

    -- * EventSourceMappingConfiguration
    EventSourceMappingConfiguration (..),
    newEventSourceMappingConfiguration,
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_documentDBEventSourceConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_scalingConfig,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_uuid,

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
    functionCode_imageUri,
    functionCode_s3Bucket,
    functionCode_s3Key,
    functionCode_s3ObjectVersion,
    functionCode_zipFile,

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
    functionConfiguration_architectures,
    functionConfiguration_codeSha256,
    functionConfiguration_codeSize,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_description,
    functionConfiguration_environment,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_functionArn,
    functionConfiguration_functionName,
    functionConfiguration_handler,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_lastModified,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_layers,
    functionConfiguration_masterArn,
    functionConfiguration_memorySize,
    functionConfiguration_packageType,
    functionConfiguration_revisionId,
    functionConfiguration_role,
    functionConfiguration_runtime,
    functionConfiguration_runtimeVersionConfig,
    functionConfiguration_signingJobArn,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_snapStart,
    functionConfiguration_state,
    functionConfiguration_stateReason,
    functionConfiguration_stateReasonCode,
    functionConfiguration_timeout,
    functionConfiguration_tracingConfig,
    functionConfiguration_version,
    functionConfiguration_vpcConfig,

    -- * FunctionEventInvokeConfig
    FunctionEventInvokeConfig (..),
    newFunctionEventInvokeConfig,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_lastModified,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_maximumRetryAttempts,

    -- * FunctionUrlConfig
    FunctionUrlConfig (..),
    newFunctionUrlConfig,
    functionUrlConfig_cors,
    functionUrlConfig_invokeMode,
    functionUrlConfig_functionUrl,
    functionUrlConfig_functionArn,
    functionUrlConfig_creationTime,
    functionUrlConfig_lastModifiedTime,
    functionUrlConfig_authType,

    -- * GetLayerVersionResponse
    GetLayerVersionResponse (..),
    newGetLayerVersionResponse,
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_content,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_description,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_licenseInfo,
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
    imageConfigError_errorCode,
    imageConfigError_message,

    -- * ImageConfigResponse
    ImageConfigResponse (..),
    newImageConfigResponse,
    imageConfigResponse_error,
    imageConfigResponse_imageConfig,

    -- * InvokeResponseStreamUpdate
    InvokeResponseStreamUpdate (..),
    newInvokeResponseStreamUpdate,
    invokeResponseStreamUpdate_payload,

    -- * InvokeWithResponseStreamCompleteEvent
    InvokeWithResponseStreamCompleteEvent (..),
    newInvokeWithResponseStreamCompleteEvent,
    invokeWithResponseStreamCompleteEvent_errorCode,
    invokeWithResponseStreamCompleteEvent_errorDetails,
    invokeWithResponseStreamCompleteEvent_logResult,

    -- * InvokeWithResponseStreamResponseEvent
    InvokeWithResponseStreamResponseEvent (..),
    newInvokeWithResponseStreamResponseEvent,
    invokeWithResponseStreamResponseEvent_invokeComplete,
    invokeWithResponseStreamResponseEvent_payloadChunk,

    -- * Layer
    Layer (..),
    newLayer,
    layer_arn,
    layer_codeSize,
    layer_signingJobArn,
    layer_signingProfileVersionArn,

    -- * LayerVersionContentInput
    LayerVersionContentInput (..),
    newLayerVersionContentInput,
    layerVersionContentInput_s3Bucket,
    layerVersionContentInput_s3Key,
    layerVersionContentInput_s3ObjectVersion,
    layerVersionContentInput_zipFile,

    -- * LayerVersionContentOutput
    LayerVersionContentOutput (..),
    newLayerVersionContentOutput,
    layerVersionContentOutput_codeSha256,
    layerVersionContentOutput_codeSize,
    layerVersionContentOutput_location,
    layerVersionContentOutput_signingJobArn,
    layerVersionContentOutput_signingProfileVersionArn,

    -- * LayerVersionsListItem
    LayerVersionsListItem (..),
    newLayerVersionsListItem,
    layerVersionsListItem_compatibleArchitectures,
    layerVersionsListItem_compatibleRuntimes,
    layerVersionsListItem_createdDate,
    layerVersionsListItem_description,
    layerVersionsListItem_layerVersionArn,
    layerVersionsListItem_licenseInfo,
    layerVersionsListItem_version,

    -- * LayersListItem
    LayersListItem (..),
    newLayersListItem,
    layersListItem_latestMatchingVersion,
    layersListItem_layerArn,
    layersListItem_layerName,

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
    provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_functionArn,
    provisionedConcurrencyConfigListItem_lastModified,
    provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_status,
    provisionedConcurrencyConfigListItem_statusReason,

    -- * RuntimeVersionConfig
    RuntimeVersionConfig (..),
    newRuntimeVersionConfig,
    runtimeVersionConfig_error,
    runtimeVersionConfig_runtimeVersionArn,

    -- * RuntimeVersionError
    RuntimeVersionError (..),
    newRuntimeVersionError,
    runtimeVersionError_errorCode,
    runtimeVersionError_message,

    -- * ScalingConfig
    ScalingConfig (..),
    newScalingConfig,
    scalingConfig_maximumConcurrency,

    -- * SelfManagedEventSource
    SelfManagedEventSource (..),
    newSelfManagedEventSource,
    selfManagedEventSource_endpoints,

    -- * SelfManagedKafkaEventSourceConfig
    SelfManagedKafkaEventSourceConfig (..),
    newSelfManagedKafkaEventSourceConfig,
    selfManagedKafkaEventSourceConfig_consumerGroupId,

    -- * SnapStart
    SnapStart (..),
    newSnapStart,
    snapStart_applyOn,

    -- * SnapStartResponse
    SnapStartResponse (..),
    newSnapStartResponse,
    snapStartResponse_applyOn,
    snapStartResponse_optimizationStatus,

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
    vpcConfigResponse_subnetIds,
    vpcConfigResponse_vpcId,
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
import Amazonka.Lambda.Types.DocumentDBEventSourceConfig
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
import Amazonka.Lambda.Types.FullDocument
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
import Amazonka.Lambda.Types.InvokeMode
import Amazonka.Lambda.Types.InvokeResponseStreamUpdate
import Amazonka.Lambda.Types.InvokeWithResponseStreamCompleteEvent
import Amazonka.Lambda.Types.InvokeWithResponseStreamResponseEvent
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
import Amazonka.Lambda.Types.ResponseStreamingInvocationType
import Amazonka.Lambda.Types.Runtime
import Amazonka.Lambda.Types.RuntimeVersionConfig
import Amazonka.Lambda.Types.RuntimeVersionError
import Amazonka.Lambda.Types.ScalingConfig
import Amazonka.Lambda.Types.SelfManagedEventSource
import Amazonka.Lambda.Types.SelfManagedKafkaEventSourceConfig
import Amazonka.Lambda.Types.SnapStart
import Amazonka.Lambda.Types.SnapStartApplyOn
import Amazonka.Lambda.Types.SnapStartOptimizationStatus
import Amazonka.Lambda.Types.SnapStartResponse
import Amazonka.Lambda.Types.SourceAccessConfiguration
import Amazonka.Lambda.Types.SourceAccessType
import Amazonka.Lambda.Types.State
import Amazonka.Lambda.Types.StateReasonCode
import Amazonka.Lambda.Types.TracingConfig
import Amazonka.Lambda.Types.TracingConfigResponse
import Amazonka.Lambda.Types.TracingMode
import Amazonka.Lambda.Types.UpdateRuntimeOn
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified code signing configuration does not exist.
_CodeSigningConfigNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CodeSigningConfigNotFoundException =
  Core._MatchServiceError
    defaultService
    "CodeSigningConfigNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Your Amazon Web Services account has exceeded its maximum total code
-- size. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/gettingstarted-limits.html Lambda quotas>.
_CodeStorageExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CodeStorageExceededException =
  Core._MatchServiceError
    defaultService
    "CodeStorageExceededException"
    Prelude.. Core.hasStatus 400

-- | The code signature failed one or more of the validation checks for
-- signature mismatch or expiry, and the code signing policy is set to
-- ENFORCE. Lambda blocks the deployment.
_CodeVerificationFailedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CodeVerificationFailedException =
  Core._MatchServiceError
    defaultService
    "CodeVerificationFailedException"
    Prelude.. Core.hasStatus 400

-- | Need additional permissions to configure VPC settings.
_EC2AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EC2AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "EC2AccessDeniedException"
    Prelude.. Core.hasStatus 502

-- | Amazon EC2 throttled Lambda during Lambda function initialization using
-- the execution role provided for the function.
_EC2ThrottledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EC2ThrottledException =
  Core._MatchServiceError
    defaultService
    "EC2ThrottledException"
    Prelude.. Core.hasStatus 502

-- | Lambda received an unexpected Amazon EC2 client exception while setting
-- up for the Lambda function.
_EC2UnexpectedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EC2UnexpectedException =
  Core._MatchServiceError
    defaultService
    "EC2UnexpectedException"
    Prelude.. Core.hasStatus 502

-- | An error occurred when reading from or writing to a connected file
-- system.
_EFSIOException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EFSIOException =
  Core._MatchServiceError
    defaultService
    "EFSIOException"
    Prelude.. Core.hasStatus 410

-- | The Lambda function couldn\'t make a network connection to the
-- configured file system.
_EFSMountConnectivityException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EFSMountConnectivityException =
  Core._MatchServiceError
    defaultService
    "EFSMountConnectivityException"
    Prelude.. Core.hasStatus 408

-- | The Lambda function couldn\'t mount the configured file system due to a
-- permission or configuration issue.
_EFSMountFailureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EFSMountFailureException =
  Core._MatchServiceError
    defaultService
    "EFSMountFailureException"
    Prelude.. Core.hasStatus 403

-- | The Lambda function made a network connection to the configured file
-- system, but the mount operation timed out.
_EFSMountTimeoutException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EFSMountTimeoutException =
  Core._MatchServiceError
    defaultService
    "EFSMountTimeoutException"
    Prelude.. Core.hasStatus 408

-- | Lambda couldn\'t create an elastic network interface in the VPC,
-- specified as part of Lambda function configuration, because the limit
-- for network interfaces has been reached. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/gettingstarted-limits.html Lambda quotas>.
_ENILimitReachedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ENILimitReachedException =
  Core._MatchServiceError
    defaultService
    "ENILimitReachedException"
    Prelude.. Core.hasStatus 502

-- | The code signature failed the integrity check. If the integrity check
-- fails, then Lambda blocks deployment, even if the code signing policy is
-- set to WARN.
_InvalidCodeSignatureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCodeSignatureException =
  Core._MatchServiceError
    defaultService
    "InvalidCodeSignatureException"
    Prelude.. Core.hasStatus 400

-- | One of the parameters in the request is not valid.
_InvalidParameterValueException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
    Prelude.. Core.hasStatus 400

-- | The request body could not be parsed as JSON.
_InvalidRequestContentException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestContentException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestContentException"
    Prelude.. Core.hasStatus 400

-- | The runtime or runtime version specified is not supported.
_InvalidRuntimeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRuntimeException =
  Core._MatchServiceError
    defaultService
    "InvalidRuntimeException"
    Prelude.. Core.hasStatus 502

-- | The security group ID provided in the Lambda function VPC configuration
-- is not valid.
_InvalidSecurityGroupIDException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSecurityGroupIDException =
  Core._MatchServiceError
    defaultService
    "InvalidSecurityGroupIDException"
    Prelude.. Core.hasStatus 502

-- | The subnet ID provided in the Lambda function VPC configuration is not
-- valid.
_InvalidSubnetIDException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSubnetIDException =
  Core._MatchServiceError
    defaultService
    "InvalidSubnetIDException"
    Prelude.. Core.hasStatus 502

-- | Lambda could not unzip the deployment package.
_InvalidZipFileException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidZipFileException =
  Core._MatchServiceError
    defaultService
    "InvalidZipFileException"
    Prelude.. Core.hasStatus 502

-- | Lambda couldn\'t decrypt the environment variables because KMS access
-- was denied. Check the Lambda function\'s KMS permissions.
_KMSAccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "KMSAccessDeniedException"
    Prelude.. Core.hasStatus 502

-- | Lambda couldn\'t decrypt the environment variables because the KMS key
-- used is disabled. Check the Lambda function\'s KMS key settings.
_KMSDisabledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSDisabledException =
  Core._MatchServiceError
    defaultService
    "KMSDisabledException"
    Prelude.. Core.hasStatus 502

-- | Lambda couldn\'t decrypt the environment variables because the state of
-- the KMS key used is not valid for Decrypt. Check the function\'s KMS key
-- settings.
_KMSInvalidStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSInvalidStateException =
  Core._MatchServiceError
    defaultService
    "KMSInvalidStateException"
    Prelude.. Core.hasStatus 502

-- | Lambda couldn\'t decrypt the environment variables because the KMS key
-- was not found. Check the function\'s KMS key settings.
_KMSNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSNotFoundException =
  Core._MatchServiceError
    defaultService
    "KMSNotFoundException"
    Prelude.. Core.hasStatus 502

-- | The permissions policy for the resource is too large. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/gettingstarted-limits.html Lambda quotas>.
_PolicyLengthExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PolicyLengthExceededException =
  Core._MatchServiceError
    defaultService
    "PolicyLengthExceededException"
    Prelude.. Core.hasStatus 400

-- | The RevisionId provided does not match the latest RevisionId for the
-- Lambda function or alias. Call the @GetFunction@ or the @GetAlias@ API
-- operation to retrieve the latest RevisionId for your resource.
_PreconditionFailedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PreconditionFailedException =
  Core._MatchServiceError
    defaultService
    "PreconditionFailedException"
    Prelude.. Core.hasStatus 412

-- | The specified configuration does not exist.
_ProvisionedConcurrencyConfigNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ProvisionedConcurrencyConfigNotFoundException =
  Core._MatchServiceError
    defaultService
    "ProvisionedConcurrencyConfigNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Lambda has detected your function being invoked in a recursive loop with
-- other Amazon Web Services resources and stopped your function\'s
-- invocation.
_RecursiveInvocationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RecursiveInvocationException =
  Core._MatchServiceError
    defaultService
    "RecursiveInvocationException"
    Prelude.. Core.hasStatus 400

-- | The request payload exceeded the @Invoke@ request body JSON input quota.
-- For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/gettingstarted-limits.html Lambda quotas>.
_RequestTooLargeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RequestTooLargeException =
  Core._MatchServiceError
    defaultService
    "RequestTooLargeException"
    Prelude.. Core.hasStatus 413

-- | The resource already exists, or another operation is in progress.
_ResourceConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Prelude.. Core.hasStatus 409

-- | The operation conflicts with the resource\'s availability. For example,
-- you tried to update an event source mapping in the CREATING state, or
-- you tried to delete an event source mapping currently UPDATING.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 400

-- | The resource specified in the request does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The function is inactive and its VPC connection is no longer available.
-- Wait for the VPC connection to reestablish and try again.
_ResourceNotReadyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotReadyException =
  Core._MatchServiceError
    defaultService
    "ResourceNotReadyException"
    Prelude.. Core.hasStatus 502

-- | The Lambda service encountered an internal error.
_ServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"
    Prelude.. Core.hasStatus 500

-- | The @afterRestore()@
-- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart-runtime-hooks.html runtime hook>
-- encountered an error. For more information, check the Amazon CloudWatch
-- logs.
_SnapStartException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapStartException =
  Core._MatchServiceError
    defaultService
    "SnapStartException"
    Prelude.. Core.hasStatus 400

-- | Lambda is initializing your function. You can invoke the function when
-- the
-- <https://docs.aws.amazon.com/lambda/latest/dg/functions-states.html function state>
-- becomes @Active@.
_SnapStartNotReadyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapStartNotReadyException =
  Core._MatchServiceError
    defaultService
    "SnapStartNotReadyException"
    Prelude.. Core.hasStatus 409

-- | Lambda couldn\'t restore the snapshot within the timeout limit.
_SnapStartTimeoutException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapStartTimeoutException =
  Core._MatchServiceError
    defaultService
    "SnapStartTimeoutException"
    Prelude.. Core.hasStatus 408

-- | Lambda couldn\'t set up VPC access for the Lambda function because one
-- or more configured subnets has no available IP addresses.
_SubnetIPAddressLimitReachedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetIPAddressLimitReachedException =
  Core._MatchServiceError
    defaultService
    "SubnetIPAddressLimitReachedException"
    Prelude.. Core.hasStatus 502

-- | The request throughput limit was exceeded. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/gettingstarted-limits.html#api-requests Lambda quotas>.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | The content type of the @Invoke@ request body is not JSON.
_UnsupportedMediaTypeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedMediaTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedMediaTypeException"
    Prelude.. Core.hasStatus 415
