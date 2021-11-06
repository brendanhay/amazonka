{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorks.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _ResourceNotFoundException,

    -- * AppAttributesKeys
    AppAttributesKeys (..),

    -- * AppType
    AppType (..),

    -- * Architecture
    Architecture (..),

    -- * AutoScalingType
    AutoScalingType (..),

    -- * CloudWatchLogsEncoding
    CloudWatchLogsEncoding (..),

    -- * CloudWatchLogsInitialPosition
    CloudWatchLogsInitialPosition (..),

    -- * CloudWatchLogsTimeZone
    CloudWatchLogsTimeZone (..),

    -- * DeploymentCommandName
    DeploymentCommandName (..),

    -- * LayerAttributesKeys
    LayerAttributesKeys (..),

    -- * LayerType
    LayerType (..),

    -- * RootDeviceType
    RootDeviceType (..),

    -- * SourceType
    SourceType (..),

    -- * StackAttributesKeys
    StackAttributesKeys (..),

    -- * VirtualizationType
    VirtualizationType (..),

    -- * VolumeType
    VolumeType (..),

    -- * AgentVersion
    AgentVersion (..),
    newAgentVersion,
    agentVersion_version,
    agentVersion_configurationManager,

    -- * App
    App (..),
    newApp,
    app_sslConfiguration,
    app_environment,
    app_enableSsl,
    app_createdAt,
    app_shortname,
    app_dataSources,
    app_appSource,
    app_appId,
    app_attributes,
    app_name,
    app_type,
    app_stackId,
    app_domains,
    app_description,

    -- * AutoScalingThresholds
    AutoScalingThresholds (..),
    newAutoScalingThresholds,
    autoScalingThresholds_instanceCount,
    autoScalingThresholds_ignoreMetricsTime,
    autoScalingThresholds_loadThreshold,
    autoScalingThresholds_thresholdsWaitTime,
    autoScalingThresholds_alarms,
    autoScalingThresholds_memoryThreshold,
    autoScalingThresholds_cpuThreshold,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    newBlockDeviceMapping,
    blockDeviceMapping_virtualName,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_ebs,
    blockDeviceMapping_deviceName,

    -- * ChefConfiguration
    ChefConfiguration (..),
    newChefConfiguration,
    chefConfiguration_berkshelfVersion,
    chefConfiguration_manageBerkshelf,

    -- * CloudWatchLogsConfiguration
    CloudWatchLogsConfiguration (..),
    newCloudWatchLogsConfiguration,
    cloudWatchLogsConfiguration_enabled,
    cloudWatchLogsConfiguration_logStreams,

    -- * CloudWatchLogsLogStream
    CloudWatchLogsLogStream (..),
    newCloudWatchLogsLogStream,
    cloudWatchLogsLogStream_batchCount,
    cloudWatchLogsLogStream_fileFingerprintLines,
    cloudWatchLogsLogStream_bufferDuration,
    cloudWatchLogsLogStream_batchSize,
    cloudWatchLogsLogStream_logGroupName,
    cloudWatchLogsLogStream_multiLineStartPattern,
    cloudWatchLogsLogStream_initialPosition,
    cloudWatchLogsLogStream_datetimeFormat,
    cloudWatchLogsLogStream_encoding,
    cloudWatchLogsLogStream_timeZone,
    cloudWatchLogsLogStream_file,

    -- * Command
    Command (..),
    newCommand,
    command_deploymentId,
    command_instanceId,
    command_status,
    command_logUrl,
    command_createdAt,
    command_commandId,
    command_exitCode,
    command_type,
    command_completedAt,
    command_acknowledgedAt,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_arn,
    dataSource_databaseName,
    dataSource_type,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_deploymentId,
    deployment_status,
    deployment_command,
    deployment_createdAt,
    deployment_customJson,
    deployment_iamUserArn,
    deployment_appId,
    deployment_instanceIds,
    deployment_completedAt,
    deployment_stackId,
    deployment_comment,
    deployment_duration,

    -- * DeploymentCommand
    DeploymentCommand (..),
    newDeploymentCommand,
    deploymentCommand_args,
    deploymentCommand_name,

    -- * EbsBlockDevice
    EbsBlockDevice (..),
    newEbsBlockDevice,
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_iops,
    ebsBlockDevice_volumeType,
    ebsBlockDevice_snapshotId,

    -- * EcsCluster
    EcsCluster (..),
    newEcsCluster,
    ecsCluster_ecsClusterArn,
    ecsCluster_ecsClusterName,
    ecsCluster_registeredAt,
    ecsCluster_stackId,

    -- * ElasticIp
    ElasticIp (..),
    newElasticIp,
    elasticIp_instanceId,
    elasticIp_domain,
    elasticIp_ip,
    elasticIp_name,
    elasticIp_region,

    -- * ElasticLoadBalancer
    ElasticLoadBalancer (..),
    newElasticLoadBalancer,
    elasticLoadBalancer_subnetIds,
    elasticLoadBalancer_vpcId,
    elasticLoadBalancer_availabilityZones,
    elasticLoadBalancer_region,
    elasticLoadBalancer_elasticLoadBalancerName,
    elasticLoadBalancer_stackId,
    elasticLoadBalancer_ec2InstanceIds,
    elasticLoadBalancer_layerId,
    elasticLoadBalancer_dnsName,

    -- * EnvironmentVariable
    EnvironmentVariable (..),
    newEnvironmentVariable,
    environmentVariable_secure,
    environmentVariable_key,
    environmentVariable_value,

    -- * Instance
    Instance (..),
    newInstance,
    instance_privateDns,
    instance_reportedAgentVersion,
    instance_instanceId,
    instance_status,
    instance_privateIp,
    instance_installUpdatesOnBoot,
    instance_virtualizationType,
    instance_instanceProfileArn,
    instance_platform,
    instance_hostname,
    instance_sshHostRsaKeyFingerprint,
    instance_securityGroupIds,
    instance_ecsClusterArn,
    instance_arn,
    instance_createdAt,
    instance_ec2InstanceId,
    instance_sshKeyName,
    instance_agentVersion,
    instance_rootDeviceVolumeId,
    instance_subnetId,
    instance_infrastructureClass,
    instance_sshHostDsaKeyFingerprint,
    instance_instanceType,
    instance_ebsOptimized,
    instance_elasticIp,
    instance_os,
    instance_availabilityZone,
    instance_lastServiceErrorId,
    instance_tenancy,
    instance_autoScalingType,
    instance_layerIds,
    instance_architecture,
    instance_publicDns,
    instance_amiId,
    instance_publicIp,
    instance_reportedOs,
    instance_registeredBy,
    instance_stackId,
    instance_rootDeviceType,
    instance_ecsContainerInstanceArn,
    instance_blockDeviceMappings,

    -- * InstanceIdentity
    InstanceIdentity (..),
    newInstanceIdentity,
    instanceIdentity_signature,
    instanceIdentity_document,

    -- * InstancesCount
    InstancesCount (..),
    newInstancesCount,
    instancesCount_terminating,
    instancesCount_pending,
    instancesCount_online,
    instancesCount_unassigning,
    instancesCount_deregistering,
    instancesCount_runningSetup,
    instancesCount_requested,
    instancesCount_stopFailed,
    instancesCount_booting,
    instancesCount_stopped,
    instancesCount_rebooting,
    instancesCount_assigning,
    instancesCount_shuttingDown,
    instancesCount_setupFailed,
    instancesCount_connectionLost,
    instancesCount_terminated,
    instancesCount_stopping,
    instancesCount_registered,
    instancesCount_startFailed,
    instancesCount_registering,

    -- * Layer
    Layer (..),
    newLayer,
    layer_customInstanceProfileArn,
    layer_customSecurityGroupIds,
    layer_installUpdatesOnBoot,
    layer_cloudWatchLogsConfiguration,
    layer_lifecycleEventConfiguration,
    layer_arn,
    layer_createdAt,
    layer_shortname,
    layer_defaultRecipes,
    layer_customRecipes,
    layer_customJson,
    layer_volumeConfigurations,
    layer_enableAutoHealing,
    layer_packages,
    layer_attributes,
    layer_name,
    layer_autoAssignPublicIps,
    layer_type,
    layer_useEbsOptimizedInstances,
    layer_stackId,
    layer_layerId,
    layer_defaultSecurityGroupNames,
    layer_autoAssignElasticIps,

    -- * LifecycleEventConfiguration
    LifecycleEventConfiguration (..),
    newLifecycleEventConfiguration,
    lifecycleEventConfiguration_shutdown,

    -- * LoadBasedAutoScalingConfiguration
    LoadBasedAutoScalingConfiguration (..),
    newLoadBasedAutoScalingConfiguration,
    loadBasedAutoScalingConfiguration_upScaling,
    loadBasedAutoScalingConfiguration_enable,
    loadBasedAutoScalingConfiguration_downScaling,
    loadBasedAutoScalingConfiguration_layerId,

    -- * OperatingSystem
    OperatingSystem (..),
    newOperatingSystem,
    operatingSystem_reportedVersion,
    operatingSystem_supported,
    operatingSystem_name,
    operatingSystem_id,
    operatingSystem_configurationManagers,
    operatingSystem_type,
    operatingSystem_reportedName,

    -- * OperatingSystemConfigurationManager
    OperatingSystemConfigurationManager (..),
    newOperatingSystemConfigurationManager,
    operatingSystemConfigurationManager_name,
    operatingSystemConfigurationManager_version,

    -- * Permission
    Permission (..),
    newPermission,
    permission_iamUserArn,
    permission_allowSudo,
    permission_stackId,
    permission_level,
    permission_allowSsh,

    -- * RaidArray
    RaidArray (..),
    newRaidArray,
    raidArray_instanceId,
    raidArray_size,
    raidArray_iops,
    raidArray_createdAt,
    raidArray_raidLevel,
    raidArray_device,
    raidArray_numberOfDisks,
    raidArray_availabilityZone,
    raidArray_name,
    raidArray_raidArrayId,
    raidArray_volumeType,
    raidArray_stackId,
    raidArray_mountPoint,

    -- * RdsDbInstance
    RdsDbInstance (..),
    newRdsDbInstance,
    rdsDbInstance_rdsDbInstanceArn,
    rdsDbInstance_dbUser,
    rdsDbInstance_missingOnRds,
    rdsDbInstance_engine,
    rdsDbInstance_address,
    rdsDbInstance_dbInstanceIdentifier,
    rdsDbInstance_region,
    rdsDbInstance_stackId,
    rdsDbInstance_dbPassword,

    -- * Recipes
    Recipes (..),
    newRecipes,
    recipes_setup,
    recipes_shutdown,
    recipes_undeploy,
    recipes_configure,
    recipes_deploy,

    -- * ReportedOs
    ReportedOs (..),
    newReportedOs,
    reportedOs_family,
    reportedOs_name,
    reportedOs_version,

    -- * SelfUserProfile
    SelfUserProfile (..),
    newSelfUserProfile,
    selfUserProfile_sshPublicKey,
    selfUserProfile_sshUsername,
    selfUserProfile_iamUserArn,
    selfUserProfile_name,

    -- * ServiceError
    ServiceError (..),
    newServiceError,
    serviceError_instanceId,
    serviceError_createdAt,
    serviceError_serviceErrorId,
    serviceError_type,
    serviceError_stackId,
    serviceError_message,

    -- * ShutdownEventConfiguration
    ShutdownEventConfiguration (..),
    newShutdownEventConfiguration,
    shutdownEventConfiguration_executionTimeout,
    shutdownEventConfiguration_delayUntilElbConnectionsDrained,

    -- * Source
    Source (..),
    newSource,
    source_url,
    source_username,
    source_sshKey,
    source_password,
    source_type,
    source_revision,

    -- * SslConfiguration
    SslConfiguration (..),
    newSslConfiguration,
    sslConfiguration_privateKey,
    sslConfiguration_certificate,
    sslConfiguration_chain,

    -- * Stack
    Stack (..),
    newStack,
    stack_defaultInstanceProfileArn,
    stack_serviceRoleArn,
    stack_defaultRootDeviceType,
    stack_arn,
    stack_createdAt,
    stack_vpcId,
    stack_chefConfiguration,
    stack_agentVersion,
    stack_defaultSshKeyName,
    stack_customJson,
    stack_customCookbooksSource,
    stack_defaultAvailabilityZone,
    stack_attributes,
    stack_name,
    stack_defaultOs,
    stack_useOpsworksSecurityGroups,
    stack_useCustomCookbooks,
    stack_defaultSubnetId,
    stack_region,
    stack_configurationManager,
    stack_stackId,
    stack_hostnameTheme,

    -- * StackConfigurationManager
    StackConfigurationManager (..),
    newStackConfigurationManager,
    stackConfigurationManager_name,
    stackConfigurationManager_version,

    -- * StackSummary
    StackSummary (..),
    newStackSummary,
    stackSummary_arn,
    stackSummary_appsCount,
    stackSummary_name,
    stackSummary_stackId,
    stackSummary_layersCount,
    stackSummary_instancesCount,

    -- * TemporaryCredential
    TemporaryCredential (..),
    newTemporaryCredential,
    temporaryCredential_instanceId,
    temporaryCredential_username,
    temporaryCredential_password,
    temporaryCredential_validForInMinutes,

    -- * TimeBasedAutoScalingConfiguration
    TimeBasedAutoScalingConfiguration (..),
    newTimeBasedAutoScalingConfiguration,
    timeBasedAutoScalingConfiguration_instanceId,
    timeBasedAutoScalingConfiguration_autoScalingSchedule,

    -- * UserProfile
    UserProfile (..),
    newUserProfile,
    userProfile_allowSelfManagement,
    userProfile_sshPublicKey,
    userProfile_sshUsername,
    userProfile_iamUserArn,
    userProfile_name,

    -- * Volume
    Volume (..),
    newVolume,
    volume_instanceId,
    volume_status,
    volume_size,
    volume_iops,
    volume_device,
    volume_encrypted,
    volume_availabilityZone,
    volume_name,
    volume_raidArrayId,
    volume_volumeId,
    volume_region,
    volume_volumeType,
    volume_ec2VolumeId,
    volume_mountPoint,

    -- * VolumeConfiguration
    VolumeConfiguration (..),
    newVolumeConfiguration,
    volumeConfiguration_iops,
    volumeConfiguration_raidLevel,
    volumeConfiguration_encrypted,
    volumeConfiguration_volumeType,
    volumeConfiguration_mountPoint,
    volumeConfiguration_numberOfDisks,
    volumeConfiguration_size,

    -- * WeeklyAutoScalingSchedule
    WeeklyAutoScalingSchedule (..),
    newWeeklyAutoScalingSchedule,
    weeklyAutoScalingSchedule_thursday,
    weeklyAutoScalingSchedule_wednesday,
    weeklyAutoScalingSchedule_saturday,
    weeklyAutoScalingSchedule_monday,
    weeklyAutoScalingSchedule_friday,
    weeklyAutoScalingSchedule_sunday,
    weeklyAutoScalingSchedule_tuesday,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpsWorks.Types.AgentVersion
import Amazonka.OpsWorks.Types.App
import Amazonka.OpsWorks.Types.AppAttributesKeys
import Amazonka.OpsWorks.Types.AppType
import Amazonka.OpsWorks.Types.Architecture
import Amazonka.OpsWorks.Types.AutoScalingThresholds
import Amazonka.OpsWorks.Types.AutoScalingType
import Amazonka.OpsWorks.Types.BlockDeviceMapping
import Amazonka.OpsWorks.Types.ChefConfiguration
import Amazonka.OpsWorks.Types.CloudWatchLogsConfiguration
import Amazonka.OpsWorks.Types.CloudWatchLogsEncoding
import Amazonka.OpsWorks.Types.CloudWatchLogsInitialPosition
import Amazonka.OpsWorks.Types.CloudWatchLogsLogStream
import Amazonka.OpsWorks.Types.CloudWatchLogsTimeZone
import Amazonka.OpsWorks.Types.Command
import Amazonka.OpsWorks.Types.DataSource
import Amazonka.OpsWorks.Types.Deployment
import Amazonka.OpsWorks.Types.DeploymentCommand
import Amazonka.OpsWorks.Types.DeploymentCommandName
import Amazonka.OpsWorks.Types.EbsBlockDevice
import Amazonka.OpsWorks.Types.EcsCluster
import Amazonka.OpsWorks.Types.ElasticIp
import Amazonka.OpsWorks.Types.ElasticLoadBalancer
import Amazonka.OpsWorks.Types.EnvironmentVariable
import Amazonka.OpsWorks.Types.Instance
import Amazonka.OpsWorks.Types.InstanceIdentity
import Amazonka.OpsWorks.Types.InstancesCount
import Amazonka.OpsWorks.Types.Layer
import Amazonka.OpsWorks.Types.LayerAttributesKeys
import Amazonka.OpsWorks.Types.LayerType
import Amazonka.OpsWorks.Types.LifecycleEventConfiguration
import Amazonka.OpsWorks.Types.LoadBasedAutoScalingConfiguration
import Amazonka.OpsWorks.Types.OperatingSystem
import Amazonka.OpsWorks.Types.OperatingSystemConfigurationManager
import Amazonka.OpsWorks.Types.Permission
import Amazonka.OpsWorks.Types.RaidArray
import Amazonka.OpsWorks.Types.RdsDbInstance
import Amazonka.OpsWorks.Types.Recipes
import Amazonka.OpsWorks.Types.ReportedOs
import Amazonka.OpsWorks.Types.RootDeviceType
import Amazonka.OpsWorks.Types.SelfUserProfile
import Amazonka.OpsWorks.Types.ServiceError
import Amazonka.OpsWorks.Types.ShutdownEventConfiguration
import Amazonka.OpsWorks.Types.Source
import Amazonka.OpsWorks.Types.SourceType
import Amazonka.OpsWorks.Types.SslConfiguration
import Amazonka.OpsWorks.Types.Stack
import Amazonka.OpsWorks.Types.StackAttributesKeys
import Amazonka.OpsWorks.Types.StackConfigurationManager
import Amazonka.OpsWorks.Types.StackSummary
import Amazonka.OpsWorks.Types.TemporaryCredential
import Amazonka.OpsWorks.Types.TimeBasedAutoScalingConfiguration
import Amazonka.OpsWorks.Types.UserProfile
import Amazonka.OpsWorks.Types.VirtualizationType
import Amazonka.OpsWorks.Types.Volume
import Amazonka.OpsWorks.Types.VolumeConfiguration
import Amazonka.OpsWorks.Types.VolumeType
import Amazonka.OpsWorks.Types.WeeklyAutoScalingSchedule
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2013-02-18@ of the Amazon OpsWorks SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "OpsWorks",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "opsworks",
      Core._serviceSigningName = "opsworks",
      Core._serviceVersion = "2013-02-18",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "OpsWorks",
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

-- | Indicates that a request was not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | Indicates that a resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
