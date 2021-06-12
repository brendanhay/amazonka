{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,
    _InternalServerErrorException,

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
    newBulkDeployment,
    bulkDeployment_bulkDeploymentId,
    bulkDeployment_createdAt,
    bulkDeployment_bulkDeploymentArn,

    -- * BulkDeploymentMetrics
    BulkDeploymentMetrics (..),
    newBulkDeploymentMetrics,
    bulkDeploymentMetrics_recordsProcessed,
    bulkDeploymentMetrics_retryAttempts,
    bulkDeploymentMetrics_invalidInputRecords,

    -- * BulkDeploymentResult
    BulkDeploymentResult (..),
    newBulkDeploymentResult,
    bulkDeploymentResult_deploymentId,
    bulkDeploymentResult_deploymentType,
    bulkDeploymentResult_deploymentStatus,
    bulkDeploymentResult_createdAt,
    bulkDeploymentResult_deploymentArn,
    bulkDeploymentResult_errorMessage,
    bulkDeploymentResult_groupArn,
    bulkDeploymentResult_errorDetails,

    -- * ConnectivityInfo
    ConnectivityInfo (..),
    newConnectivityInfo,
    connectivityInfo_id,
    connectivityInfo_metadata,
    connectivityInfo_portNumber,
    connectivityInfo_hostAddress,

    -- * Connector
    Connector (..),
    newConnector,
    connector_parameters,
    connector_connectorArn,
    connector_id,

    -- * ConnectorDefinitionVersion
    ConnectorDefinitionVersion (..),
    newConnectorDefinitionVersion,
    connectorDefinitionVersion_connectors,

    -- * Core
    Core (..),
    newCore,
    core_syncShadow,
    core_thingArn,
    core_id,
    core_certificateArn,

    -- * CoreDefinitionVersion
    CoreDefinitionVersion (..),
    newCoreDefinitionVersion,
    coreDefinitionVersion_cores,

    -- * DefinitionInformation
    DefinitionInformation (..),
    newDefinitionInformation,
    definitionInformation_creationTimestamp,
    definitionInformation_latestVersionArn,
    definitionInformation_latestVersion,
    definitionInformation_arn,
    definitionInformation_id,
    definitionInformation_name,
    definitionInformation_tags,
    definitionInformation_lastUpdatedTimestamp,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_deploymentId,
    deployment_deploymentType,
    deployment_createdAt,
    deployment_deploymentArn,
    deployment_groupArn,

    -- * Device
    Device (..),
    newDevice,
    device_syncShadow,
    device_thingArn,
    device_id,
    device_certificateArn,

    -- * DeviceDefinitionVersion
    DeviceDefinitionVersion (..),
    newDeviceDefinitionVersion,
    deviceDefinitionVersion_devices,

    -- * ErrorDetail
    ErrorDetail (..),
    newErrorDetail,
    errorDetail_detailedErrorMessage,
    errorDetail_detailedErrorCode,

    -- * Function
    Function (..),
    newFunction,
    function_functionConfiguration,
    function_functionArn,
    function_id,

    -- * FunctionConfiguration
    FunctionConfiguration (..),
    newFunctionConfiguration,
    functionConfiguration_execArgs,
    functionConfiguration_memorySize,
    functionConfiguration_timeout,
    functionConfiguration_encodingType,
    functionConfiguration_pinned,
    functionConfiguration_executable,
    functionConfiguration_environment,

    -- * FunctionConfigurationEnvironment
    FunctionConfigurationEnvironment (..),
    newFunctionConfigurationEnvironment,
    functionConfigurationEnvironment_accessSysfs,
    functionConfigurationEnvironment_variables,
    functionConfigurationEnvironment_execution,
    functionConfigurationEnvironment_resourceAccessPolicies,

    -- * FunctionDefaultConfig
    FunctionDefaultConfig (..),
    newFunctionDefaultConfig,
    functionDefaultConfig_execution,

    -- * FunctionDefaultExecutionConfig
    FunctionDefaultExecutionConfig (..),
    newFunctionDefaultExecutionConfig,
    functionDefaultExecutionConfig_isolationMode,
    functionDefaultExecutionConfig_runAs,

    -- * FunctionDefinitionVersion
    FunctionDefinitionVersion (..),
    newFunctionDefinitionVersion,
    functionDefinitionVersion_functions,
    functionDefinitionVersion_defaultConfig,

    -- * FunctionExecutionConfig
    FunctionExecutionConfig (..),
    newFunctionExecutionConfig,
    functionExecutionConfig_isolationMode,
    functionExecutionConfig_runAs,

    -- * FunctionRunAsConfig
    FunctionRunAsConfig (..),
    newFunctionRunAsConfig,
    functionRunAsConfig_gid,
    functionRunAsConfig_uid,

    -- * GreengrassLogger
    GreengrassLogger (..),
    newGreengrassLogger,
    greengrassLogger_space,
    greengrassLogger_type,
    greengrassLogger_level,
    greengrassLogger_id,
    greengrassLogger_component,

    -- * GroupCertificateAuthorityProperties
    GroupCertificateAuthorityProperties (..),
    newGroupCertificateAuthorityProperties,
    groupCertificateAuthorityProperties_groupCertificateAuthorityArn,
    groupCertificateAuthorityProperties_groupCertificateAuthorityId,

    -- * GroupInformation
    GroupInformation (..),
    newGroupInformation,
    groupInformation_creationTimestamp,
    groupInformation_latestVersionArn,
    groupInformation_latestVersion,
    groupInformation_arn,
    groupInformation_id,
    groupInformation_name,
    groupInformation_lastUpdatedTimestamp,

    -- * GroupOwnerSetting
    GroupOwnerSetting (..),
    newGroupOwnerSetting,
    groupOwnerSetting_groupOwner,
    groupOwnerSetting_autoAddGroupOwner,

    -- * GroupVersion
    GroupVersion (..),
    newGroupVersion,
    groupVersion_coreDefinitionVersionArn,
    groupVersion_connectorDefinitionVersionArn,
    groupVersion_subscriptionDefinitionVersionArn,
    groupVersion_loggerDefinitionVersionArn,
    groupVersion_resourceDefinitionVersionArn,
    groupVersion_functionDefinitionVersionArn,
    groupVersion_deviceDefinitionVersionArn,

    -- * LocalDeviceResourceData
    LocalDeviceResourceData (..),
    newLocalDeviceResourceData,
    localDeviceResourceData_sourcePath,
    localDeviceResourceData_groupOwnerSetting,

    -- * LocalVolumeResourceData
    LocalVolumeResourceData (..),
    newLocalVolumeResourceData,
    localVolumeResourceData_destinationPath,
    localVolumeResourceData_sourcePath,
    localVolumeResourceData_groupOwnerSetting,

    -- * LoggerDefinitionVersion
    LoggerDefinitionVersion (..),
    newLoggerDefinitionVersion,
    loggerDefinitionVersion_loggers,

    -- * Resource
    Resource (..),
    newResource,
    resource_resourceDataContainer,
    resource_id,
    resource_name,

    -- * ResourceAccessPolicy
    ResourceAccessPolicy (..),
    newResourceAccessPolicy,
    resourceAccessPolicy_permission,
    resourceAccessPolicy_resourceId,

    -- * ResourceDataContainer
    ResourceDataContainer (..),
    newResourceDataContainer,
    resourceDataContainer_localVolumeResourceData,
    resourceDataContainer_localDeviceResourceData,
    resourceDataContainer_sageMakerMachineLearningModelResourceData,
    resourceDataContainer_s3MachineLearningModelResourceData,
    resourceDataContainer_secretsManagerSecretResourceData,

    -- * ResourceDefinitionVersion
    ResourceDefinitionVersion (..),
    newResourceDefinitionVersion,
    resourceDefinitionVersion_resources,

    -- * ResourceDownloadOwnerSetting
    ResourceDownloadOwnerSetting (..),
    newResourceDownloadOwnerSetting,
    resourceDownloadOwnerSetting_groupOwner,
    resourceDownloadOwnerSetting_groupPermission,

    -- * RuntimeConfiguration
    RuntimeConfiguration (..),
    newRuntimeConfiguration,
    runtimeConfiguration_telemetryConfiguration,

    -- * S3MachineLearningModelResourceData
    S3MachineLearningModelResourceData (..),
    newS3MachineLearningModelResourceData,
    s3MachineLearningModelResourceData_ownerSetting,
    s3MachineLearningModelResourceData_destinationPath,
    s3MachineLearningModelResourceData_s3Uri,

    -- * SageMakerMachineLearningModelResourceData
    SageMakerMachineLearningModelResourceData (..),
    newSageMakerMachineLearningModelResourceData,
    sageMakerMachineLearningModelResourceData_ownerSetting,
    sageMakerMachineLearningModelResourceData_destinationPath,
    sageMakerMachineLearningModelResourceData_sageMakerJobArn,

    -- * SecretsManagerSecretResourceData
    SecretsManagerSecretResourceData (..),
    newSecretsManagerSecretResourceData,
    secretsManagerSecretResourceData_arn,
    secretsManagerSecretResourceData_additionalStagingLabelsToDownload,

    -- * Subscription
    Subscription (..),
    newSubscription,
    subscription_target,
    subscription_id,
    subscription_subject,
    subscription_source,

    -- * SubscriptionDefinitionVersion
    SubscriptionDefinitionVersion (..),
    newSubscriptionDefinitionVersion,
    subscriptionDefinitionVersion_subscriptions,

    -- * TelemetryConfiguration
    TelemetryConfiguration (..),
    newTelemetryConfiguration,
    telemetryConfiguration_configurationSyncStatus,
    telemetryConfiguration_telemetry,

    -- * TelemetryConfigurationUpdate
    TelemetryConfigurationUpdate (..),
    newTelemetryConfigurationUpdate,
    telemetryConfigurationUpdate_telemetry,

    -- * VersionInformation
    VersionInformation (..),
    newVersionInformation,
    versionInformation_creationTimestamp,
    versionInformation_arn,
    versionInformation_id,
    versionInformation_version,
  )
where

import qualified Network.AWS.Core as Core
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
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-06-07@ of the Amazon Greengrass SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Greengrass",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "greengrass",
      Core._serviceSigningName = "greengrass",
      Core._serviceVersion = "2017-06-07",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "Greengrass",
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

-- | General error information.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Core.. Core.hasStatus 400

-- | General error information.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Core.. Core.hasStatus 500
