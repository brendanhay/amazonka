{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types
  ( -- * Service Configuration
    greengrass,

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
    BulkDeployment,
    bulkDeployment,
    bdBulkDeploymentARN,
    bdBulkDeploymentId,
    bdCreatedAt,

    -- * BulkDeploymentMetrics
    BulkDeploymentMetrics,
    bulkDeploymentMetrics,
    bdmRecordsProcessed,
    bdmRetryAttempts,
    bdmInvalidInputRecords,

    -- * BulkDeploymentResult
    BulkDeploymentResult,
    bulkDeploymentResult,
    bdrDeploymentId,
    bdrDeploymentARN,
    bdrCreatedAt,
    bdrDeploymentType,
    bdrErrorDetails,
    bdrGroupARN,
    bdrDeploymentStatus,
    bdrErrorMessage,

    -- * ConnectivityInfo
    ConnectivityInfo,
    connectivityInfo,
    ciPortNumber,
    ciId,
    ciMetadata,
    ciHostAddress,

    -- * Connector
    Connector,
    connector,
    conParameters,
    conConnectorARN,
    conId,

    -- * ConnectorDefinitionVersion
    ConnectorDefinitionVersion,
    connectorDefinitionVersion,
    cdvConnectors,

    -- * Core
    Core,
    core,
    cSyncShadow,
    cThingARN,
    cId,
    cCertificateARN,

    -- * CoreDefinitionVersion
    CoreDefinitionVersion,
    coreDefinitionVersion,
    cdvCores,

    -- * DefinitionInformation
    DefinitionInformation,
    definitionInformation,
    diLatestVersionARN,
    diARN,
    diName,
    diCreationTimestamp,
    diId,
    diTags,
    diLatestVersion,
    diLastUpdatedTimestamp,

    -- * Deployment
    Deployment,
    deployment,
    dDeploymentId,
    dDeploymentARN,
    dCreatedAt,
    dDeploymentType,
    dGroupARN,

    -- * Device
    Device,
    device,
    dSyncShadow,
    dThingARN,
    dId,
    dCertificateARN,

    -- * DeviceDefinitionVersion
    DeviceDefinitionVersion,
    deviceDefinitionVersion,
    ddvDevices,

    -- * ErrorDetail
    ErrorDetail,
    errorDetail,
    edDetailedErrorCode,
    edDetailedErrorMessage,

    -- * Function
    Function,
    function,
    fFunctionARN,
    fFunctionConfiguration,
    fId,

    -- * FunctionConfiguration
    FunctionConfiguration,
    functionConfiguration,
    fcMemorySize,
    fcExecArgs,
    fcEnvironment,
    fcExecutable,
    fcPinned,
    fcEncodingType,
    fcTimeout,

    -- * FunctionConfigurationEnvironment
    FunctionConfigurationEnvironment,
    functionConfigurationEnvironment,
    fceVariables,
    fceExecution,
    fceResourceAccessPolicies,
    fceAccessSysfs,

    -- * FunctionDefaultConfig
    FunctionDefaultConfig,
    functionDefaultConfig,
    fdcExecution,

    -- * FunctionDefaultExecutionConfig
    FunctionDefaultExecutionConfig,
    functionDefaultExecutionConfig,
    fdecRunAs,
    fdecIsolationMode,

    -- * FunctionDefinitionVersion
    FunctionDefinitionVersion,
    functionDefinitionVersion,
    fdvDefaultConfig,
    fdvFunctions,

    -- * FunctionExecutionConfig
    FunctionExecutionConfig,
    functionExecutionConfig,
    fecRunAs,
    fecIsolationMode,

    -- * FunctionRunAsConfig
    FunctionRunAsConfig,
    functionRunAsConfig,
    fracUid,
    fracGid,

    -- * GreengrassLogger
    GreengrassLogger,
    greengrassLogger,
    glSpace,
    glType,
    glLevel,
    glId,
    glComponent,

    -- * GroupCertificateAuthorityProperties
    GroupCertificateAuthorityProperties,
    groupCertificateAuthorityProperties,
    gcapGroupCertificateAuthorityARN,
    gcapGroupCertificateAuthorityId,

    -- * GroupInformation
    GroupInformation,
    groupInformation,
    giLatestVersionARN,
    giARN,
    giName,
    giCreationTimestamp,
    giId,
    giLatestVersion,
    giLastUpdatedTimestamp,

    -- * GroupOwnerSetting
    GroupOwnerSetting,
    groupOwnerSetting,
    gosAutoAddGroupOwner,
    gosGroupOwner,

    -- * GroupVersion
    GroupVersion,
    groupVersion,
    gvResourceDefinitionVersionARN,
    gvSubscriptionDefinitionVersionARN,
    gvCoreDefinitionVersionARN,
    gvDeviceDefinitionVersionARN,
    gvFunctionDefinitionVersionARN,
    gvLoggerDefinitionVersionARN,
    gvConnectorDefinitionVersionARN,

    -- * LocalDeviceResourceData
    LocalDeviceResourceData,
    localDeviceResourceData,
    ldrdGroupOwnerSetting,
    ldrdSourcePath,

    -- * LocalVolumeResourceData
    LocalVolumeResourceData,
    localVolumeResourceData,
    lvrdGroupOwnerSetting,
    lvrdDestinationPath,
    lvrdSourcePath,

    -- * LoggerDefinitionVersion
    LoggerDefinitionVersion,
    loggerDefinitionVersion,
    ldvLoggers,

    -- * Resource
    Resource,
    resource,
    rResourceDataContainer,
    rId,
    rName,

    -- * ResourceAccessPolicy
    ResourceAccessPolicy,
    resourceAccessPolicy,
    rapPermission,
    rapResourceId,

    -- * ResourceDataContainer
    ResourceDataContainer,
    resourceDataContainer,
    rdcS3MachineLearningModelResourceData,
    rdcSageMakerMachineLearningModelResourceData,
    rdcLocalVolumeResourceData,
    rdcLocalDeviceResourceData,
    rdcSecretsManagerSecretResourceData,

    -- * ResourceDefinitionVersion
    ResourceDefinitionVersion,
    resourceDefinitionVersion,
    rdvResources,

    -- * ResourceDownloadOwnerSetting
    ResourceDownloadOwnerSetting,
    resourceDownloadOwnerSetting,
    rdosGroupOwner,
    rdosGroupPermission,

    -- * RuntimeConfiguration
    RuntimeConfiguration,
    runtimeConfiguration,
    rcTelemetryConfiguration,

    -- * S3MachineLearningModelResourceData
    S3MachineLearningModelResourceData,
    s3MachineLearningModelResourceData,
    smlmrdOwnerSetting,
    smlmrdDestinationPath,
    smlmrdS3URI,

    -- * SageMakerMachineLearningModelResourceData
    SageMakerMachineLearningModelResourceData,
    sageMakerMachineLearningModelResourceData,
    smmlmrdOwnerSetting,
    smmlmrdSageMakerJobARN,
    smmlmrdDestinationPath,

    -- * SecretsManagerSecretResourceData
    SecretsManagerSecretResourceData,
    secretsManagerSecretResourceData,
    smsrdAdditionalStagingLabelsToDownload,
    smsrdARN,

    -- * Subscription
    Subscription,
    subscription,
    sTarget,
    sId,
    sSubject,
    sSource,

    -- * SubscriptionDefinitionVersion
    SubscriptionDefinitionVersion,
    subscriptionDefinitionVersion,
    sdvSubscriptions,

    -- * TelemetryConfiguration
    TelemetryConfiguration,
    telemetryConfiguration,
    tcConfigurationSyncStatus,
    tcTelemetry,

    -- * TelemetryConfigurationUpdate
    TelemetryConfigurationUpdate,
    telemetryConfigurationUpdate,
    tcuTelemetry,

    -- * VersionInformation
    VersionInformation,
    versionInformation,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-06-07@ of the Amazon Greengrass SDK configuration.
greengrass :: Service
greengrass =
  Service
    { _svcAbbrev = "Greengrass",
      _svcSigner = v4,
      _svcPrefix = "greengrass",
      _svcVersion = "2017-06-07",
      _svcEndpoint = defaultEndpoint greengrass,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Greengrass",
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
