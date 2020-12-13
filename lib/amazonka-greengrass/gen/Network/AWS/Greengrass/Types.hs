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
    greengrassService,

    -- * Errors

    -- * BulkDeploymentStatus
    BulkDeploymentStatus (..),

    -- * ConfigurationSyncStatus
    ConfigurationSyncStatus (..),

    -- * DeploymentType
    DeploymentType (..),

    -- * EncodingType
    EncodingType (..),

    -- * FunctionIsolationMode
    FunctionIsolationMode (..),

    -- * LoggerComponent
    LoggerComponent (..),

    -- * LoggerLevel
    LoggerLevel (..),

    -- * LoggerType
    LoggerType (..),

    -- * Permission
    Permission (..),

    -- * SoftwareToUpdate
    SoftwareToUpdate (..),

    -- * Telemetry
    Telemetry (..),

    -- * UpdateAgentLogLevel
    UpdateAgentLogLevel (..),

    -- * UpdateTargetsArchitecture
    UpdateTargetsArchitecture (..),

    -- * UpdateTargetsOperatingSystem
    UpdateTargetsOperatingSystem (..),

    -- * BulkDeployment
    BulkDeployment (..),
    mkBulkDeployment,
    bdBulkDeploymentARN,
    bdBulkDeploymentId,
    bdCreatedAt,

    -- * BulkDeploymentMetrics
    BulkDeploymentMetrics (..),
    mkBulkDeploymentMetrics,
    bdmRecordsProcessed,
    bdmRetryAttempts,
    bdmInvalidInputRecords,

    -- * BulkDeploymentResult
    BulkDeploymentResult (..),
    mkBulkDeploymentResult,
    bdrDeploymentId,
    bdrDeploymentARN,
    bdrCreatedAt,
    bdrDeploymentType,
    bdrErrorDetails,
    bdrGroupARN,
    bdrDeploymentStatus,
    bdrErrorMessage,

    -- * ConnectivityInfo
    ConnectivityInfo (..),
    mkConnectivityInfo,
    ciPortNumber,
    ciId,
    ciMetadata,
    ciHostAddress,

    -- * Connector
    Connector (..),
    mkConnector,
    cfConnectorARN,
    cfParameters,
    cfId,

    -- * ConnectorDefinitionVersion
    ConnectorDefinitionVersion (..),
    mkConnectorDefinitionVersion,
    cdvConnectors,

    -- * Core
    Core (..),
    mkCore,
    cCertificateARN,
    cThingARN,
    cSyncShadow,
    cId,

    -- * CoreDefinitionVersion
    CoreDefinitionVersion (..),
    mkCoreDefinitionVersion,
    cdvCores,

    -- * DefinitionInformation
    DefinitionInformation (..),
    mkDefinitionInformation,
    diLatestVersionARN,
    diARN,
    diName,
    diCreationTimestamp,
    diId,
    diTags,
    diLatestVersion,
    diLastUpdatedTimestamp,

    -- * Deployment
    Deployment (..),
    mkDeployment,
    dDeploymentId,
    dDeploymentARN,
    dCreatedAt,
    dDeploymentType,
    dGroupARN,

    -- * Device
    Device (..),
    mkDevice,
    dCertificateARN,
    dThingARN,
    dSyncShadow,
    dId,

    -- * DeviceDefinitionVersion
    DeviceDefinitionVersion (..),
    mkDeviceDefinitionVersion,
    ddvDevices,

    -- * ErrorDetail
    ErrorDetail (..),
    mkErrorDetail,
    edDetailedErrorCode,
    edDetailedErrorMessage,

    -- * Function
    Function (..),
    mkFunction,
    fFunctionARN,
    fFunctionConfiguration,
    fId,

    -- * FunctionConfiguration
    FunctionConfiguration (..),
    mkFunctionConfiguration,
    fcMemorySize,
    fcExecArgs,
    fcEnvironment,
    fcExecutable,
    fcPinned,
    fcEncodingType,
    fcTimeout,

    -- * FunctionConfigurationEnvironment
    FunctionConfigurationEnvironment (..),
    mkFunctionConfigurationEnvironment,
    fceVariables,
    fceExecution,
    fceResourceAccessPolicies,
    fceAccessSysfs,

    -- * FunctionDefaultConfig
    FunctionDefaultConfig (..),
    mkFunctionDefaultConfig,
    fdcExecution,

    -- * FunctionDefaultExecutionConfig
    FunctionDefaultExecutionConfig (..),
    mkFunctionDefaultExecutionConfig,
    fdecRunAs,
    fdecIsolationMode,

    -- * FunctionDefinitionVersion
    FunctionDefinitionVersion (..),
    mkFunctionDefinitionVersion,
    fdvDefaultConfig,
    fdvFunctions,

    -- * FunctionExecutionConfig
    FunctionExecutionConfig (..),
    mkFunctionExecutionConfig,
    fecRunAs,
    fecIsolationMode,

    -- * FunctionRunAsConfig
    FunctionRunAsConfig (..),
    mkFunctionRunAsConfig,
    fracUid,
    fracGid,

    -- * GreengrassLogger
    GreengrassLogger (..),
    mkGreengrassLogger,
    glSpace,
    glComponent,
    glId,
    glType,
    glLevel,

    -- * GroupCertificateAuthorityProperties
    GroupCertificateAuthorityProperties (..),
    mkGroupCertificateAuthorityProperties,
    gcapGroupCertificateAuthorityARN,
    gcapGroupCertificateAuthorityId,

    -- * GroupInformation
    GroupInformation (..),
    mkGroupInformation,
    giLatestVersionARN,
    giARN,
    giName,
    giCreationTimestamp,
    giId,
    giLatestVersion,
    giLastUpdatedTimestamp,

    -- * GroupOwnerSetting
    GroupOwnerSetting (..),
    mkGroupOwnerSetting,
    gosAutoAddGroupOwner,
    gosGroupOwner,

    -- * GroupVersion
    GroupVersion (..),
    mkGroupVersion,
    gvResourceDefinitionVersionARN,
    gvSubscriptionDefinitionVersionARN,
    gvCoreDefinitionVersionARN,
    gvDeviceDefinitionVersionARN,
    gvFunctionDefinitionVersionARN,
    gvLoggerDefinitionVersionARN,
    gvConnectorDefinitionVersionARN,

    -- * LocalDeviceResourceData
    LocalDeviceResourceData (..),
    mkLocalDeviceResourceData,
    ldrdGroupOwnerSetting,
    ldrdSourcePath,

    -- * LocalVolumeResourceData
    LocalVolumeResourceData (..),
    mkLocalVolumeResourceData,
    lvrdGroupOwnerSetting,
    lvrdDestinationPath,
    lvrdSourcePath,

    -- * LoggerDefinitionVersion
    LoggerDefinitionVersion (..),
    mkLoggerDefinitionVersion,
    ldvLoggers,

    -- * Resource
    Resource (..),
    mkResource,
    rResourceDataContainer,
    rName,
    rId,

    -- * ResourceAccessPolicy
    ResourceAccessPolicy (..),
    mkResourceAccessPolicy,
    rapResourceId,
    rapPermission,

    -- * ResourceDataContainer
    ResourceDataContainer (..),
    mkResourceDataContainer,
    rdcS3MachineLearningModelResourceData,
    rdcSageMakerMachineLearningModelResourceData,
    rdcLocalVolumeResourceData,
    rdcLocalDeviceResourceData,
    rdcSecretsManagerSecretResourceData,

    -- * ResourceDefinitionVersion
    ResourceDefinitionVersion (..),
    mkResourceDefinitionVersion,
    rdvResources,

    -- * ResourceDownloadOwnerSetting
    ResourceDownloadOwnerSetting (..),
    mkResourceDownloadOwnerSetting,
    rdosGroupOwner,
    rdosGroupPermission,

    -- * RuntimeConfiguration
    RuntimeConfiguration (..),
    mkRuntimeConfiguration,
    rcTelemetryConfiguration,

    -- * S3MachineLearningModelResourceData
    S3MachineLearningModelResourceData (..),
    mkS3MachineLearningModelResourceData,
    smlmrdOwnerSetting,
    smlmrdDestinationPath,
    smlmrdS3URI,

    -- * SageMakerMachineLearningModelResourceData
    SageMakerMachineLearningModelResourceData (..),
    mkSageMakerMachineLearningModelResourceData,
    smmlmrdOwnerSetting,
    smmlmrdSageMakerJobARN,
    smmlmrdDestinationPath,

    -- * SecretsManagerSecretResourceData
    SecretsManagerSecretResourceData (..),
    mkSecretsManagerSecretResourceData,
    smsrdAdditionalStagingLabelsToDownload,
    smsrdARN,

    -- * Subscription
    Subscription (..),
    mkSubscription,
    sSubject,
    sSource,
    sId,
    sTarget,

    -- * SubscriptionDefinitionVersion
    SubscriptionDefinitionVersion (..),
    mkSubscriptionDefinitionVersion,
    sdvSubscriptions,

    -- * TelemetryConfiguration
    TelemetryConfiguration (..),
    mkTelemetryConfiguration,
    tcTelemetry,
    tcConfigurationSyncStatus,

    -- * TelemetryConfigurationUpdate
    TelemetryConfigurationUpdate (..),
    mkTelemetryConfigurationUpdate,
    tcuTelemetry,

    -- * VersionInformation
    VersionInformation (..),
    mkVersionInformation,
    viARN,
    viCreationTimestamp,
    viVersion,
    viId,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-06-07@ of the Amazon Greengrass SDK configuration.
greengrassService :: Lude.Service
greengrassService =
  Lude.Service
    { Lude._svcAbbrev = "Greengrass",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "greengrass",
      Lude._svcVersion = "2017-06-07",
      Lude._svcEndpoint = Lude.defaultEndpoint greengrassService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Greengrass",
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
