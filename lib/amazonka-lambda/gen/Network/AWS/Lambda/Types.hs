-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types
  ( -- * Service configuration
    lambdaService,

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
    AccountLimit (..),
    mkAccountLimit,
    alConcurrentExecutions,
    alTotalCodeSize,
    alUnreservedConcurrentExecutions,
    alCodeSizeUnzipped,
    alCodeSizeZipped,

    -- * AccountUsage
    AccountUsage (..),
    mkAccountUsage,
    auTotalCodeSize,
    auFunctionCount,

    -- * AliasConfiguration
    AliasConfiguration (..),
    mkAliasConfiguration,
    acRoutingConfig,
    acName,
    acFunctionVersion,
    acAliasARN,
    acDescription,
    acRevisionId,

    -- * AliasRoutingConfiguration
    AliasRoutingConfiguration (..),
    mkAliasRoutingConfiguration,
    arcAdditionalVersionWeights,

    -- * AllowedPublishers
    AllowedPublishers (..),
    mkAllowedPublishers,
    apSigningProfileVersionARNs,

    -- * CodeSigningConfig
    CodeSigningConfig (..),
    mkCodeSigningConfig,
    cscDescription,
    cscCodeSigningConfigId,
    cscCodeSigningConfigARN,
    cscAllowedPublishers,
    cscCodeSigningPolicies,
    cscLastModified,

    -- * CodeSigningPolicies
    CodeSigningPolicies (..),
    mkCodeSigningPolicies,
    cspUntrustedArtifactOnDeployment,

    -- * Concurrency
    Concurrency (..),
    mkConcurrency,
    cReservedConcurrentExecutions,

    -- * DeadLetterConfig
    DeadLetterConfig (..),
    mkDeadLetterConfig,
    dlcTargetARN,

    -- * DestinationConfig
    DestinationConfig (..),
    mkDestinationConfig,
    dcOnSuccess,
    dcOnFailure,

    -- * Environment
    Environment (..),
    mkEnvironment,
    eVariables,

    -- * EnvironmentError
    EnvironmentError (..),
    mkEnvironmentError,
    eeErrorCode,
    eeMessage,

    -- * EnvironmentResponse
    EnvironmentResponse (..),
    mkEnvironmentResponse,
    envVariables,
    envError,

    -- * EventSourceMappingConfiguration
    EventSourceMappingConfiguration (..),
    mkEventSourceMappingConfiguration,
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
    FileSystemConfig (..),
    mkFileSystemConfig,
    fscARN,
    fscLocalMountPath,

    -- * FunctionCode
    FunctionCode (..),
    mkFunctionCode,
    fcS3ObjectVersion,
    fcS3Key,
    fcZipFile,
    fcS3Bucket,

    -- * FunctionCodeLocation
    FunctionCodeLocation (..),
    mkFunctionCodeLocation,
    fclLocation,
    fclRepositoryType,

    -- * FunctionConfiguration
    FunctionConfiguration (..),
    mkFunctionConfiguration,
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
    FunctionEventInvokeConfig (..),
    mkFunctionEventInvokeConfig,
    feicFunctionARN,
    feicMaximumEventAgeInSeconds,
    feicMaximumRetryAttempts,
    feicLastModified,
    feicDestinationConfig,

    -- * GetLayerVersionResponse
    GetLayerVersionResponse (..),
    mkGetLayerVersionResponse,
    glvLayerVersionARN,
    glvContent,
    glvCreatedDate,
    glvVersion,
    glvLicenseInfo,
    glvLayerARN,
    glvDescription,
    glvCompatibleRuntimes,

    -- * Layer
    Layer (..),
    mkLayer,
    lSigningProfileVersionARN,
    lARN,
    lSigningJobARN,
    lCodeSize,

    -- * LayerVersionContentInput
    LayerVersionContentInput (..),
    mkLayerVersionContentInput,
    lvciS3ObjectVersion,
    lvciS3Key,
    lvciZipFile,
    lvciS3Bucket,

    -- * LayerVersionContentOutput
    LayerVersionContentOutput (..),
    mkLayerVersionContentOutput,
    lvcoSigningProfileVersionARN,
    lvcoLocation,
    lvcoSigningJobARN,
    lvcoCodeSize,
    lvcoCodeSha256,

    -- * LayerVersionsListItem
    LayerVersionsListItem (..),
    mkLayerVersionsListItem,
    lvliLayerVersionARN,
    lvliCreatedDate,
    lvliVersion,
    lvliLicenseInfo,
    lvliDescription,
    lvliCompatibleRuntimes,

    -- * LayersListItem
    LayersListItem (..),
    mkLayersListItem,
    lliLayerName,
    lliLatestMatchingVersion,
    lliLayerARN,

    -- * OnFailure
    OnFailure (..),
    mkOnFailure,
    ofDestination,

    -- * OnSuccess
    OnSuccess (..),
    mkOnSuccess,
    osDestination,

    -- * ProvisionedConcurrencyConfigListItem
    ProvisionedConcurrencyConfigListItem (..),
    mkProvisionedConcurrencyConfigListItem,
    pccliStatus,
    pccliFunctionARN,
    pccliRequestedProvisionedConcurrentExecutions,
    pccliAvailableProvisionedConcurrentExecutions,
    pccliStatusReason,
    pccliAllocatedProvisionedConcurrentExecutions,
    pccliLastModified,

    -- * SourceAccessConfiguration
    SourceAccessConfiguration (..),
    mkSourceAccessConfiguration,
    sacURI,
    sacType,

    -- * TracingConfig
    TracingConfig (..),
    mkTracingConfig,
    tMode,

    -- * TracingConfigResponse
    TracingConfigResponse (..),
    mkTracingConfigResponse,
    tcMode,

    -- * VPCConfig
    VPCConfig (..),
    mkVPCConfig,
    vpccSecurityGroupIds,
    vpccSubnetIds,

    -- * VPCConfigResponse
    VPCConfigResponse (..),
    mkVPCConfigResponse,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-03-31@ of the Amazon Lambda SDK configuration.
lambdaService :: Lude.Service
lambdaService =
  Lude.Service
    { Lude._svcAbbrev = "Lambda",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "lambda",
      Lude._svcVersion = "2015-03-31",
      Lude._svcEndpoint = Lude.defaultEndpoint lambdaService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Lambda",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
