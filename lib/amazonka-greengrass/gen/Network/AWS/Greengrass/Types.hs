-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InternalServerErrorException,
    _BadRequestException,

    -- * CoreDefinitionVersion
    CoreDefinitionVersion (..),
    mkCoreDefinitionVersion,
    cdvCores,

    -- * SubscriptionDefinitionVersion
    SubscriptionDefinitionVersion (..),
    mkSubscriptionDefinitionVersion,
    sdvSubscriptions,

    -- * LoggerType
    LoggerType (..),

    -- * S3MachineLearningModelResourceData
    S3MachineLearningModelResourceData (..),
    mkS3MachineLearningModelResourceData,
    smlmrdDestinationPath,
    smlmrdOwnerSetting,
    smlmrdS3Uri,

    -- * TelemetryConfiguration
    TelemetryConfiguration (..),
    mkTelemetryConfiguration,
    tcTelemetry,
    tcConfigurationSyncStatus,

    -- * Telemetry
    Telemetry (..),

    -- * DeviceDefinitionVersion
    DeviceDefinitionVersion (..),
    mkDeviceDefinitionVersion,
    ddvDevices,

    -- * UpdateAgentLogLevel
    UpdateAgentLogLevel (..),

    -- * Function
    Function (..),
    mkFunction,
    fId,
    fFunctionArn,
    fFunctionConfiguration,

    -- * GroupCertificateAuthorityProperties
    GroupCertificateAuthorityProperties (..),
    mkGroupCertificateAuthorityProperties,
    gcapGroupCertificateAuthorityArn,
    gcapGroupCertificateAuthorityId,

    -- * SoftwareToUpdate
    SoftwareToUpdate (..),

    -- * FunctionDefinitionVersion
    FunctionDefinitionVersion (..),
    mkFunctionDefinitionVersion,
    fdvDefaultConfig,
    fdvFunctions,

    -- * GroupVersion
    GroupVersion (..),
    mkGroupVersion,
    gvConnectorDefinitionVersionArn,
    gvCoreDefinitionVersionArn,
    gvDeviceDefinitionVersionArn,
    gvFunctionDefinitionVersionArn,
    gvLoggerDefinitionVersionArn,
    gvResourceDefinitionVersionArn,
    gvSubscriptionDefinitionVersionArn,

    -- * ResourceDataContainer
    ResourceDataContainer (..),
    mkResourceDataContainer,
    rdcLocalDeviceResourceData,
    rdcLocalVolumeResourceData,
    rdcS3MachineLearningModelResourceData,
    rdcSageMakerMachineLearningModelResourceData,
    rdcSecretsManagerSecretResourceData,

    -- * GroupOwnerSetting
    GroupOwnerSetting (..),
    mkGroupOwnerSetting,
    gosAutoAddGroupOwner,
    gosGroupOwner,

    -- * Device
    Device (..),
    mkDevice,
    dThingArn,
    dId,
    dCertificateArn,
    dSyncShadow,

    -- * FunctionDefaultConfig
    FunctionDefaultConfig (..),
    mkFunctionDefaultConfig,
    fdcExecution,

    -- * UpdateTargetsOperatingSystem
    UpdateTargetsOperatingSystem (..),

    -- * S3UrlSignerRole
    S3UrlSignerRole (..),

    -- * FunctionRunAsConfig
    FunctionRunAsConfig (..),
    mkFunctionRunAsConfig,
    fracGid,
    fracUid,

    -- * EncodingType
    EncodingType (..),

    -- * GroupInformation
    GroupInformation (..),
    mkGroupInformation,
    giArn,
    giCreationTimestamp,
    giId,
    giLastUpdatedTimestamp,
    giLatestVersion,
    giLatestVersionArn,
    giName,

    -- * ConfigurationSyncStatus
    ConfigurationSyncStatus (..),

    -- * Connector
    Connector (..),
    mkConnector,
    cfConnectorArn,
    cfId,
    cfParameters,

    -- * TelemetryConfigurationUpdate
    TelemetryConfigurationUpdate (..),
    mkTelemetryConfigurationUpdate,
    tcuTelemetry,

    -- * DeploymentType
    DeploymentType (..),

    -- * FunctionDefaultExecutionConfig
    FunctionDefaultExecutionConfig (..),
    mkFunctionDefaultExecutionConfig,
    fdecIsolationMode,
    fdecRunAs,

    -- * DefinitionInformation
    DefinitionInformation (..),
    mkDefinitionInformation,
    diArn,
    diCreationTimestamp,
    diId,
    diLastUpdatedTimestamp,
    diLatestVersion,
    diLatestVersionArn,
    diName,
    diTags,

    -- * ResourceDownloadOwnerSetting
    ResourceDownloadOwnerSetting (..),
    mkResourceDownloadOwnerSetting,
    rdosGroupOwner,
    rdosGroupPermission,

    -- * RuntimeConfiguration
    RuntimeConfiguration (..),
    mkRuntimeConfiguration,
    rcTelemetryConfiguration,

    -- * GreengrassLogger
    GreengrassLogger (..),
    mkGreengrassLogger,
    glType,
    glLevel,
    glId,
    glComponent,
    glSpace,

    -- * ConnectorDefinitionVersion
    ConnectorDefinitionVersion (..),
    mkConnectorDefinitionVersion,
    cdvConnectors,

    -- * SageMakerMachineLearningModelResourceData
    SageMakerMachineLearningModelResourceData (..),
    mkSageMakerMachineLearningModelResourceData,
    smmlmrdDestinationPath,
    smmlmrdOwnerSetting,
    smmlmrdSageMakerJobArn,

    -- * FunctionConfigurationEnvironment
    FunctionConfigurationEnvironment (..),
    mkFunctionConfigurationEnvironment,
    fceAccessSysfs,
    fceExecution,
    fceResourceAccessPolicies,
    fceVariables,

    -- * LocalVolumeResourceData
    LocalVolumeResourceData (..),
    mkLocalVolumeResourceData,
    lvrdDestinationPath,
    lvrdGroupOwnerSetting,
    lvrdSourcePath,

    -- * LocalDeviceResourceData
    LocalDeviceResourceData (..),
    mkLocalDeviceResourceData,
    ldrdGroupOwnerSetting,
    ldrdSourcePath,

    -- * FunctionExecutionConfig
    FunctionExecutionConfig (..),
    mkFunctionExecutionConfig,
    fecIsolationMode,
    fecRunAs,

    -- * FunctionConfiguration
    FunctionConfiguration (..),
    mkFunctionConfiguration,
    fcEncodingType,
    fcEnvironment,
    fcExecArgs,
    fcExecutable,
    fcMemorySize,
    fcPinned,
    fcTimeout,

    -- * Resource
    Resource (..),
    mkResource,
    rResourceDataContainer,
    rId,
    rName,

    -- * SecretsManagerSecretResourceData
    SecretsManagerSecretResourceData (..),
    mkSecretsManagerSecretResourceData,
    smsrdARN,
    smsrdAdditionalStagingLabelsToDownload,

    -- * LoggerLevel
    LoggerLevel (..),

    -- * BulkDeploymentStatus
    BulkDeploymentStatus (..),

    -- * ResourceDefinitionVersion
    ResourceDefinitionVersion (..),
    mkResourceDefinitionVersion,
    rdvResources,

    -- * FunctionIsolationMode
    FunctionIsolationMode (..),

    -- * BulkDeploymentResult
    BulkDeploymentResult (..),
    mkBulkDeploymentResult,
    bdrCreatedAt,
    bdrDeploymentArn,
    bdrDeploymentId,
    bdrDeploymentStatus,
    bdrDeploymentType,
    bdrErrorDetails,
    bdrErrorMessage,
    bdrGroupArn,

    -- * Core
    Core (..),
    mkCore,
    cThingArn,
    cId,
    cCertificateArn,
    cSyncShadow,

    -- * Permission
    Permission (..),

    -- * LoggerComponent
    LoggerComponent (..),

    -- * Subscription
    Subscription (..),
    mkSubscription,
    sTarget,
    sId,
    sSubject,
    sSource,

    -- * LoggerDefinitionVersion
    LoggerDefinitionVersion (..),
    mkLoggerDefinitionVersion,
    ldvLoggers,

    -- * ConnectivityInfo
    ConnectivityInfo (..),
    mkConnectivityInfo,
    ciHostAddress,
    ciId,
    ciMetadata,
    ciPortNumber,

    -- * UpdateTargetsArchitecture
    UpdateTargetsArchitecture (..),

    -- * VersionInformation
    VersionInformation (..),
    mkVersionInformation,
    viArn,
    viCreationTimestamp,
    viId,
    viVersion,

    -- * BulkDeploymentMetrics
    BulkDeploymentMetrics (..),
    mkBulkDeploymentMetrics,
    bdmInvalidInputRecords,
    bdmRecordsProcessed,
    bdmRetryAttempts,

    -- * ResourceAccessPolicy
    ResourceAccessPolicy (..),
    mkResourceAccessPolicy,
    rapResourceId,
    rapPermission,

    -- * BulkDeployment
    BulkDeployment (..),
    mkBulkDeployment,
    bdBulkDeploymentArn,
    bdBulkDeploymentId,
    bdCreatedAt,

    -- * Deployment
    Deployment (..),
    mkDeployment,
    dCreatedAt,
    dDeploymentArn,
    dDeploymentId,
    dDeploymentType,
    dGroupArn,

    -- * ErrorDetail
    ErrorDetail (..),
    mkErrorDetail,
    edDetailedErrorCode,
    edDetailedErrorMessage,
  )
where

import Network.AWS.Greengrass.Types.BulkDeployment
import Network.AWS.Greengrass.Types.BulkDeploymentMetrics
import Network.AWS.Greengrass.Types.BulkDeploymentResult
import Network.AWS.Greengrass.Types.BulkDeploymentStatus
import Network.AWS.Greengrass.Types.ConfigurationSyncStatus
import Network.AWS.Greengrass.Types.ConnectivityInfo
import Network.AWS.Greengrass.Types.Connector
import Network.AWS.Greengrass.Types.ConnectorDefinitionVersion
import Network.AWS.Greengrass.Types.Core
import Network.AWS.Greengrass.Types.CoreDefinitionVersion
import Network.AWS.Greengrass.Types.DefinitionInformation
import Network.AWS.Greengrass.Types.Deployment
import Network.AWS.Greengrass.Types.DeploymentType
import Network.AWS.Greengrass.Types.Device
import Network.AWS.Greengrass.Types.DeviceDefinitionVersion
import Network.AWS.Greengrass.Types.EncodingType
import Network.AWS.Greengrass.Types.ErrorDetail
import Network.AWS.Greengrass.Types.Function
import Network.AWS.Greengrass.Types.FunctionConfiguration
import Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment
import Network.AWS.Greengrass.Types.FunctionDefaultConfig
import Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
import Network.AWS.Greengrass.Types.FunctionDefinitionVersion
import Network.AWS.Greengrass.Types.FunctionExecutionConfig
import Network.AWS.Greengrass.Types.FunctionIsolationMode
import Network.AWS.Greengrass.Types.FunctionRunAsConfig
import Network.AWS.Greengrass.Types.GreengrassLogger
import Network.AWS.Greengrass.Types.GroupCertificateAuthorityProperties
import Network.AWS.Greengrass.Types.GroupInformation
import Network.AWS.Greengrass.Types.GroupOwnerSetting
import Network.AWS.Greengrass.Types.GroupVersion
import Network.AWS.Greengrass.Types.LocalDeviceResourceData
import Network.AWS.Greengrass.Types.LocalVolumeResourceData
import Network.AWS.Greengrass.Types.LoggerComponent
import Network.AWS.Greengrass.Types.LoggerDefinitionVersion
import Network.AWS.Greengrass.Types.LoggerLevel
import Network.AWS.Greengrass.Types.LoggerType
import Network.AWS.Greengrass.Types.Permission
import Network.AWS.Greengrass.Types.Resource
import Network.AWS.Greengrass.Types.ResourceAccessPolicy
import Network.AWS.Greengrass.Types.ResourceDataContainer
import Network.AWS.Greengrass.Types.ResourceDefinitionVersion
import Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
import Network.AWS.Greengrass.Types.RuntimeConfiguration
import Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
import Network.AWS.Greengrass.Types.S3UrlSignerRole
import Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData
import Network.AWS.Greengrass.Types.SecretsManagerSecretResourceData
import Network.AWS.Greengrass.Types.SoftwareToUpdate
import Network.AWS.Greengrass.Types.Subscription
import Network.AWS.Greengrass.Types.SubscriptionDefinitionVersion
import Network.AWS.Greengrass.Types.Telemetry
import Network.AWS.Greengrass.Types.TelemetryConfiguration
import Network.AWS.Greengrass.Types.TelemetryConfigurationUpdate
import Network.AWS.Greengrass.Types.UpdateAgentLogLevel
import Network.AWS.Greengrass.Types.UpdateTargetsArchitecture
import Network.AWS.Greengrass.Types.UpdateTargetsOperatingSystem
import Network.AWS.Greengrass.Types.VersionInformation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-06-07@ of the Amazon Greengrass SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Greengrass",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "greengrass",
      Core._svcVersion = "2017-06-07",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Greengrass",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | General error information.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalServerErrorException"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalServerErrorException "Use generic-lens or generic-optics instead." #-}

-- | General error information.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError mkServiceConfig "BadRequestException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead." #-}
