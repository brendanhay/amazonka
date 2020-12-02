{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types
  ( -- * Service Configuration
    lambda,

    -- * Errors

    -- * CodeSigningPolicy
    CodeSigningPolicy (..),

    -- * EventSourcePosition
    EventSourcePosition (..),

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
    AccountLimit,
    accountLimit,
    alConcurrentExecutions,
    alTotalCodeSize,
    alUnreservedConcurrentExecutions,
    alCodeSizeUnzipped,
    alCodeSizeZipped,

    -- * AccountUsage
    AccountUsage,
    accountUsage,
    auTotalCodeSize,
    auFunctionCount,

    -- * AliasConfiguration
    AliasConfiguration,
    aliasConfiguration,
    acRoutingConfig,
    acName,
    acFunctionVersion,
    acAliasARN,
    acDescription,
    acRevisionId,

    -- * AliasRoutingConfiguration
    AliasRoutingConfiguration,
    aliasRoutingConfiguration,
    arcAdditionalVersionWeights,

    -- * AllowedPublishers
    AllowedPublishers,
    allowedPublishers,
    apSigningProfileVersionARNs,

    -- * CodeSigningConfig
    CodeSigningConfig,
    codeSigningConfig,
    cscDescription,
    cscCodeSigningConfigId,
    cscCodeSigningConfigARN,
    cscAllowedPublishers,
    cscCodeSigningPolicies,
    cscLastModified,

    -- * CodeSigningPolicies
    CodeSigningPolicies,
    codeSigningPolicies,
    cspUntrustedArtifactOnDeployment,

    -- * Concurrency
    Concurrency,
    concurrency,
    cReservedConcurrentExecutions,

    -- * DeadLetterConfig
    DeadLetterConfig,
    deadLetterConfig,
    dlcTargetARN,

    -- * DestinationConfig
    DestinationConfig,
    destinationConfig,
    dcOnSuccess,
    dcOnFailure,

    -- * Environment
    Environment,
    environment,
    eVariables,

    -- * EnvironmentError
    EnvironmentError,
    environmentError,
    eeErrorCode,
    eeMessage,

    -- * EnvironmentResponse
    EnvironmentResponse,
    environmentResponse,
    envVariables,
    envError,

    -- * EventSourceMappingConfiguration
    EventSourceMappingConfiguration,
    eventSourceMappingConfiguration,
    esmcEventSourceARN,
    esmcState,
    esmcStartingPositionTimestamp,
    esmcFunctionARN,
    esmcTopics,
    esmcQueues,
    esmcBisectBatchOnFunctionError,
    esmcUUId,
    esmcParallelizationFactor,
    esmcLastProcessingResult,
    esmcMaximumRetryAttempts,
    esmcBatchSize,
    esmcStateTransitionReason,
    esmcMaximumBatchingWindowInSeconds,
    esmcSourceAccessConfigurations,
    esmcMaximumRecordAgeInSeconds,
    esmcLastModified,
    esmcDestinationConfig,
    esmcStartingPosition,

    -- * FileSystemConfig
    FileSystemConfig,
    fileSystemConfig,
    fscARN,
    fscLocalMountPath,

    -- * FunctionCode
    FunctionCode,
    functionCode,
    fcS3ObjectVersion,
    fcS3Key,
    fcZipFile,
    fcS3Bucket,

    -- * FunctionCodeLocation
    FunctionCodeLocation,
    functionCodeLocation,
    fclLocation,
    fclRepositoryType,

    -- * FunctionConfiguration
    FunctionConfiguration,
    functionConfiguration,
    fcMemorySize,
    fcRuntime,
    fcState,
    fcSigningProfileVersionARN,
    fcLastUpdateStatus,
    fcFunctionARN,
    fcKMSKeyARN,
    fcFileSystemConfigs,
    fcEnvironment,
    fcDeadLetterConfig,
    fcSigningJobARN,
    fcRole,
    fcVPCConfig,
    fcVersion,
    fcFunctionName,
    fcLayers,
    fcCodeSize,
    fcHandler,
    fcTimeout,
    fcLastUpdateStatusReason,
    fcStateReason,
    fcLastModified,
    fcCodeSha256,
    fcTracingConfig,
    fcStateReasonCode,
    fcDescription,
    fcLastUpdateStatusReasonCode,
    fcRevisionId,
    fcMasterARN,

    -- * FunctionEventInvokeConfig
    FunctionEventInvokeConfig,
    functionEventInvokeConfig,
    feicFunctionARN,
    feicMaximumEventAgeInSeconds,
    feicMaximumRetryAttempts,
    feicLastModified,
    feicDestinationConfig,

    -- * GetLayerVersionResponse
    GetLayerVersionResponse,
    getLayerVersionResponse,
    glvLayerVersionARN,
    glvContent,
    glvCreatedDate,
    glvVersion,
    glvLicenseInfo,
    glvLayerARN,
    glvDescription,
    glvCompatibleRuntimes,

    -- * Layer
    Layer,
    layer,
    lSigningProfileVersionARN,
    lARN,
    lSigningJobARN,
    lCodeSize,

    -- * LayerVersionContentInput
    LayerVersionContentInput,
    layerVersionContentInput,
    lvciS3ObjectVersion,
    lvciS3Key,
    lvciZipFile,
    lvciS3Bucket,

    -- * LayerVersionContentOutput
    LayerVersionContentOutput,
    layerVersionContentOutput,
    lvcoSigningProfileVersionARN,
    lvcoLocation,
    lvcoSigningJobARN,
    lvcoCodeSize,
    lvcoCodeSha256,

    -- * LayerVersionsListItem
    LayerVersionsListItem,
    layerVersionsListItem,
    lvliLayerVersionARN,
    lvliCreatedDate,
    lvliVersion,
    lvliLicenseInfo,
    lvliDescription,
    lvliCompatibleRuntimes,

    -- * LayersListItem
    LayersListItem,
    layersListItem,
    lliLayerName,
    lliLatestMatchingVersion,
    lliLayerARN,

    -- * OnFailure
    OnFailure,
    onFailure,
    ofDestination,

    -- * OnSuccess
    OnSuccess,
    onSuccess,
    osDestination,

    -- * ProvisionedConcurrencyConfigListItem
    ProvisionedConcurrencyConfigListItem,
    provisionedConcurrencyConfigListItem,
    pccliStatus,
    pccliFunctionARN,
    pccliRequestedProvisionedConcurrentExecutions,
    pccliAvailableProvisionedConcurrentExecutions,
    pccliStatusReason,
    pccliAllocatedProvisionedConcurrentExecutions,
    pccliLastModified,

    -- * SourceAccessConfiguration
    SourceAccessConfiguration,
    sourceAccessConfiguration,
    sacURI,
    sacType,

    -- * TracingConfig
    TracingConfig,
    tracingConfig,
    tMode,

    -- * TracingConfigResponse
    TracingConfigResponse,
    tracingConfigResponse,
    tcMode,

    -- * VPCConfig
    VPCConfig,
    vpcConfig,
    vpccSecurityGroupIds,
    vpccSubnetIds,

    -- * VPCConfigResponse
    VPCConfigResponse,
    vpcConfigResponse,
    vcSecurityGroupIds,
    vcSubnetIds,
    vcVPCId,
  )
where

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
import Network.AWS.Lambda.Types.FunctionVersion
import Network.AWS.Lambda.Types.GetLayerVersionResponse
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
import Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem
import Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
import Network.AWS.Lambda.Types.Runtime
import Network.AWS.Lambda.Types.SourceAccessConfiguration
import Network.AWS.Lambda.Types.SourceAccessType
import Network.AWS.Lambda.Types.State
import Network.AWS.Lambda.Types.StateReasonCode
import Network.AWS.Lambda.Types.TracingConfig
import Network.AWS.Lambda.Types.TracingConfigResponse
import Network.AWS.Lambda.Types.TracingMode
import Network.AWS.Lambda.Types.VPCConfig
import Network.AWS.Lambda.Types.VPCConfigResponse
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-03-31@ of the Amazon Lambda SDK configuration.
lambda :: Service
lambda =
  Service
    { _svcAbbrev = "Lambda",
      _svcSigner = v4,
      _svcPrefix = "lambda",
      _svcVersion = "2015-03-31",
      _svcEndpoint = defaultEndpoint lambda,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Lambda",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
