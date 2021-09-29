{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types
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
    app_appId,
    app_sslConfiguration,
    app_appSource,
    app_dataSources,
    app_domains,
    app_stackId,
    app_createdAt,
    app_enableSsl,
    app_environment,
    app_shortname,
    app_name,
    app_attributes,
    app_description,
    app_type,

    -- * AutoScalingThresholds
    AutoScalingThresholds (..),
    newAutoScalingThresholds,
    autoScalingThresholds_loadThreshold,
    autoScalingThresholds_cpuThreshold,
    autoScalingThresholds_memoryThreshold,
    autoScalingThresholds_alarms,
    autoScalingThresholds_thresholdsWaitTime,
    autoScalingThresholds_ignoreMetricsTime,
    autoScalingThresholds_instanceCount,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    newBlockDeviceMapping,
    blockDeviceMapping_ebs,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_virtualName,
    blockDeviceMapping_deviceName,

    -- * ChefConfiguration
    ChefConfiguration (..),
    newChefConfiguration,
    chefConfiguration_manageBerkshelf,
    chefConfiguration_berkshelfVersion,

    -- * CloudWatchLogsConfiguration
    CloudWatchLogsConfiguration (..),
    newCloudWatchLogsConfiguration,
    cloudWatchLogsConfiguration_enabled,
    cloudWatchLogsConfiguration_logStreams,

    -- * CloudWatchLogsLogStream
    CloudWatchLogsLogStream (..),
    newCloudWatchLogsLogStream,
    cloudWatchLogsLogStream_initialPosition,
    cloudWatchLogsLogStream_batchCount,
    cloudWatchLogsLogStream_multiLineStartPattern,
    cloudWatchLogsLogStream_file,
    cloudWatchLogsLogStream_fileFingerprintLines,
    cloudWatchLogsLogStream_logGroupName,
    cloudWatchLogsLogStream_batchSize,
    cloudWatchLogsLogStream_bufferDuration,
    cloudWatchLogsLogStream_encoding,
    cloudWatchLogsLogStream_timeZone,
    cloudWatchLogsLogStream_datetimeFormat,

    -- * Command
    Command (..),
    newCommand,
    command_logUrl,
    command_status,
    command_deploymentId,
    command_instanceId,
    command_completedAt,
    command_createdAt,
    command_exitCode,
    command_commandId,
    command_acknowledgedAt,
    command_type,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_arn,
    dataSource_type,
    dataSource_databaseName,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_instanceIds,
    deployment_appId,
    deployment_status,
    deployment_deploymentId,
    deployment_iamUserArn,
    deployment_duration,
    deployment_customJson,
    deployment_stackId,
    deployment_comment,
    deployment_completedAt,
    deployment_createdAt,
    deployment_command,

    -- * DeploymentCommand
    DeploymentCommand (..),
    newDeploymentCommand,
    deploymentCommand_args,
    deploymentCommand_name,

    -- * EbsBlockDevice
    EbsBlockDevice (..),
    newEbsBlockDevice,
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_snapshotId,
    ebsBlockDevice_volumeType,
    ebsBlockDevice_iops,
    ebsBlockDevice_volumeSize,

    -- * EcsCluster
    EcsCluster (..),
    newEcsCluster,
    ecsCluster_stackId,
    ecsCluster_ecsClusterName,
    ecsCluster_registeredAt,
    ecsCluster_ecsClusterArn,

    -- * ElasticIp
    ElasticIp (..),
    newElasticIp,
    elasticIp_instanceId,
    elasticIp_ip,
    elasticIp_domain,
    elasticIp_name,
    elasticIp_region,

    -- * ElasticLoadBalancer
    ElasticLoadBalancer (..),
    newElasticLoadBalancer,
    elasticLoadBalancer_availabilityZones,
    elasticLoadBalancer_stackId,
    elasticLoadBalancer_elasticLoadBalancerName,
    elasticLoadBalancer_subnetIds,
    elasticLoadBalancer_layerId,
    elasticLoadBalancer_dnsName,
    elasticLoadBalancer_ec2InstanceIds,
    elasticLoadBalancer_region,
    elasticLoadBalancer_vpcId,

    -- * EnvironmentVariable
    EnvironmentVariable (..),
    newEnvironmentVariable,
    environmentVariable_secure,
    environmentVariable_key,
    environmentVariable_value,

    -- * Instance
    Instance (..),
    newInstance,
    instance_virtualizationType,
    instance_instanceProfileArn,
    instance_platform,
    instance_hostname,
    instance_sshHostRsaKeyFingerprint,
    instance_securityGroupIds,
    instance_installUpdatesOnBoot,
    instance_status,
    instance_instanceId,
    instance_privateDns,
    instance_reportedAgentVersion,
    instance_elasticIp,
    instance_ebsOptimized,
    instance_sshHostDsaKeyFingerprint,
    instance_instanceType,
    instance_rootDeviceType,
    instance_agentVersion,
    instance_stackId,
    instance_rootDeviceVolumeId,
    instance_publicDns,
    instance_amiId,
    instance_sshKeyName,
    instance_architecture,
    instance_createdAt,
    instance_layerIds,
    instance_arn,
    instance_autoScalingType,
    instance_tenancy,
    instance_availabilityZone,
    instance_os,
    instance_privateIp,
    instance_infrastructureClass,
    instance_ecsContainerInstanceArn,
    instance_blockDeviceMappings,
    instance_subnetId,
    instance_registeredBy,
    instance_reportedOs,
    instance_ec2InstanceId,
    instance_publicIp,
    instance_lastServiceErrorId,
    instance_ecsClusterArn,

    -- * InstanceIdentity
    InstanceIdentity (..),
    newInstanceIdentity,
    instanceIdentity_document,
    instanceIdentity_signature,

    -- * InstancesCount
    InstancesCount (..),
    newInstancesCount,
    instancesCount_online,
    instancesCount_setupFailed,
    instancesCount_registering,
    instancesCount_booting,
    instancesCount_startFailed,
    instancesCount_stopFailed,
    instancesCount_runningSetup,
    instancesCount_terminated,
    instancesCount_pending,
    instancesCount_terminating,
    instancesCount_shuttingDown,
    instancesCount_assigning,
    instancesCount_stopped,
    instancesCount_rebooting,
    instancesCount_stopping,
    instancesCount_requested,
    instancesCount_registered,
    instancesCount_deregistering,
    instancesCount_connectionLost,
    instancesCount_unassigning,

    -- * Layer
    Layer (..),
    newLayer,
    layer_installUpdatesOnBoot,
    layer_customSecurityGroupIds,
    layer_customInstanceProfileArn,
    layer_packages,
    layer_enableAutoHealing,
    layer_customJson,
    layer_stackId,
    layer_volumeConfigurations,
    layer_defaultRecipes,
    layer_createdAt,
    layer_arn,
    layer_shortname,
    layer_name,
    layer_attributes,
    layer_cloudWatchLogsConfiguration,
    layer_autoAssignElasticIps,
    layer_defaultSecurityGroupNames,
    layer_layerId,
    layer_useEbsOptimizedInstances,
    layer_customRecipes,
    layer_type,
    layer_autoAssignPublicIps,
    layer_lifecycleEventConfiguration,

    -- * LifecycleEventConfiguration
    LifecycleEventConfiguration (..),
    newLifecycleEventConfiguration,
    lifecycleEventConfiguration_shutdown,

    -- * LoadBasedAutoScalingConfiguration
    LoadBasedAutoScalingConfiguration (..),
    newLoadBasedAutoScalingConfiguration,
    loadBasedAutoScalingConfiguration_downScaling,
    loadBasedAutoScalingConfiguration_enable,
    loadBasedAutoScalingConfiguration_layerId,
    loadBasedAutoScalingConfiguration_upScaling,

    -- * OperatingSystem
    OperatingSystem (..),
    newOperatingSystem,
    operatingSystem_supported,
    operatingSystem_configurationManagers,
    operatingSystem_reportedVersion,
    operatingSystem_id,
    operatingSystem_name,
    operatingSystem_reportedName,
    operatingSystem_type,

    -- * OperatingSystemConfigurationManager
    OperatingSystemConfigurationManager (..),
    newOperatingSystemConfigurationManager,
    operatingSystemConfigurationManager_version,
    operatingSystemConfigurationManager_name,

    -- * Permission
    Permission (..),
    newPermission,
    permission_allowSudo,
    permission_iamUserArn,
    permission_stackId,
    permission_allowSsh,
    permission_level,

    -- * RaidArray
    RaidArray (..),
    newRaidArray,
    raidArray_instanceId,
    raidArray_numberOfDisks,
    raidArray_stackId,
    raidArray_device,
    raidArray_createdAt,
    raidArray_raidArrayId,
    raidArray_name,
    raidArray_availabilityZone,
    raidArray_mountPoint,
    raidArray_volumeType,
    raidArray_raidLevel,
    raidArray_iops,
    raidArray_size,

    -- * RdsDbInstance
    RdsDbInstance (..),
    newRdsDbInstance,
    rdsDbInstance_rdsDbInstanceArn,
    rdsDbInstance_dbUser,
    rdsDbInstance_address,
    rdsDbInstance_stackId,
    rdsDbInstance_missingOnRds,
    rdsDbInstance_dbInstanceIdentifier,
    rdsDbInstance_dbPassword,
    rdsDbInstance_engine,
    rdsDbInstance_region,

    -- * Recipes
    Recipes (..),
    newRecipes,
    recipes_configure,
    recipes_undeploy,
    recipes_shutdown,
    recipes_setup,
    recipes_deploy,

    -- * ReportedOs
    ReportedOs (..),
    newReportedOs,
    reportedOs_version,
    reportedOs_name,
    reportedOs_family,

    -- * SelfUserProfile
    SelfUserProfile (..),
    newSelfUserProfile,
    selfUserProfile_iamUserArn,
    selfUserProfile_sshUsername,
    selfUserProfile_name,
    selfUserProfile_sshPublicKey,

    -- * ServiceError'
    ServiceError' (..),
    newServiceError',
    serviceError'_instanceId,
    serviceError'_stackId,
    serviceError'_message,
    serviceError'_serviceErrorId,
    serviceError'_createdAt,
    serviceError'_type,

    -- * ShutdownEventConfiguration
    ShutdownEventConfiguration (..),
    newShutdownEventConfiguration,
    shutdownEventConfiguration_executionTimeout,
    shutdownEventConfiguration_delayUntilElbConnectionsDrained,

    -- * Source
    Source (..),
    newSource,
    source_sshKey,
    source_password,
    source_username,
    source_url,
    source_revision,
    source_type,

    -- * SslConfiguration
    SslConfiguration (..),
    newSslConfiguration,
    sslConfiguration_privateKey,
    sslConfiguration_certificate,
    sslConfiguration_chain,

    -- * Stack
    Stack (..),
    newStack,
    stack_useOpsworksSecurityGroups,
    stack_defaultOs,
    stack_customCookbooksSource,
    stack_serviceRoleArn,
    stack_defaultAvailabilityZone,
    stack_customJson,
    stack_agentVersion,
    stack_stackId,
    stack_createdAt,
    stack_arn,
    stack_defaultRootDeviceType,
    stack_name,
    stack_attributes,
    stack_defaultInstanceProfileArn,
    stack_hostnameTheme,
    stack_configurationManager,
    stack_defaultSshKeyName,
    stack_chefConfiguration,
    stack_region,
    stack_vpcId,
    stack_defaultSubnetId,
    stack_useCustomCookbooks,

    -- * StackConfigurationManager
    StackConfigurationManager (..),
    newStackConfigurationManager,
    stackConfigurationManager_version,
    stackConfigurationManager_name,

    -- * StackSummary
    StackSummary (..),
    newStackSummary,
    stackSummary_stackId,
    stackSummary_layersCount,
    stackSummary_arn,
    stackSummary_name,
    stackSummary_appsCount,
    stackSummary_instancesCount,

    -- * TemporaryCredential
    TemporaryCredential (..),
    newTemporaryCredential,
    temporaryCredential_validForInMinutes,
    temporaryCredential_instanceId,
    temporaryCredential_password,
    temporaryCredential_username,

    -- * TimeBasedAutoScalingConfiguration
    TimeBasedAutoScalingConfiguration (..),
    newTimeBasedAutoScalingConfiguration,
    timeBasedAutoScalingConfiguration_instanceId,
    timeBasedAutoScalingConfiguration_autoScalingSchedule,

    -- * UserProfile
    UserProfile (..),
    newUserProfile,
    userProfile_iamUserArn,
    userProfile_sshUsername,
    userProfile_allowSelfManagement,
    userProfile_name,
    userProfile_sshPublicKey,

    -- * Volume
    Volume (..),
    newVolume,
    volume_status,
    volume_instanceId,
    volume_ec2VolumeId,
    volume_encrypted,
    volume_device,
    volume_volumeId,
    volume_raidArrayId,
    volume_name,
    volume_availabilityZone,
    volume_mountPoint,
    volume_volumeType,
    volume_region,
    volume_iops,
    volume_size,

    -- * VolumeConfiguration
    VolumeConfiguration (..),
    newVolumeConfiguration,
    volumeConfiguration_encrypted,
    volumeConfiguration_volumeType,
    volumeConfiguration_raidLevel,
    volumeConfiguration_iops,
    volumeConfiguration_mountPoint,
    volumeConfiguration_numberOfDisks,
    volumeConfiguration_size,

    -- * WeeklyAutoScalingSchedule
    WeeklyAutoScalingSchedule (..),
    newWeeklyAutoScalingSchedule,
    weeklyAutoScalingSchedule_thursday,
    weeklyAutoScalingSchedule_friday,
    weeklyAutoScalingSchedule_tuesday,
    weeklyAutoScalingSchedule_monday,
    weeklyAutoScalingSchedule_sunday,
    weeklyAutoScalingSchedule_saturday,
    weeklyAutoScalingSchedule_wednesday,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.AgentVersion
import Network.AWS.OpsWorks.Types.App
import Network.AWS.OpsWorks.Types.AppAttributesKeys
import Network.AWS.OpsWorks.Types.AppType
import Network.AWS.OpsWorks.Types.Architecture
import Network.AWS.OpsWorks.Types.AutoScalingThresholds
import Network.AWS.OpsWorks.Types.AutoScalingType
import Network.AWS.OpsWorks.Types.BlockDeviceMapping
import Network.AWS.OpsWorks.Types.ChefConfiguration
import Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
import Network.AWS.OpsWorks.Types.CloudWatchLogsEncoding
import Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition
import Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream
import Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone
import Network.AWS.OpsWorks.Types.Command
import Network.AWS.OpsWorks.Types.DataSource
import Network.AWS.OpsWorks.Types.Deployment
import Network.AWS.OpsWorks.Types.DeploymentCommand
import Network.AWS.OpsWorks.Types.DeploymentCommandName
import Network.AWS.OpsWorks.Types.EbsBlockDevice
import Network.AWS.OpsWorks.Types.EcsCluster
import Network.AWS.OpsWorks.Types.ElasticIp
import Network.AWS.OpsWorks.Types.ElasticLoadBalancer
import Network.AWS.OpsWorks.Types.EnvironmentVariable
import Network.AWS.OpsWorks.Types.Instance
import Network.AWS.OpsWorks.Types.InstanceIdentity
import Network.AWS.OpsWorks.Types.InstancesCount
import Network.AWS.OpsWorks.Types.Layer
import Network.AWS.OpsWorks.Types.LayerAttributesKeys
import Network.AWS.OpsWorks.Types.LayerType
import Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
import Network.AWS.OpsWorks.Types.LoadBasedAutoScalingConfiguration
import Network.AWS.OpsWorks.Types.OperatingSystem
import Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager
import Network.AWS.OpsWorks.Types.Permission
import Network.AWS.OpsWorks.Types.RaidArray
import Network.AWS.OpsWorks.Types.RdsDbInstance
import Network.AWS.OpsWorks.Types.Recipes
import Network.AWS.OpsWorks.Types.ReportedOs
import Network.AWS.OpsWorks.Types.RootDeviceType
import Network.AWS.OpsWorks.Types.SelfUserProfile
import Network.AWS.OpsWorks.Types.ServiceError'
import Network.AWS.OpsWorks.Types.ShutdownEventConfiguration
import Network.AWS.OpsWorks.Types.Source
import Network.AWS.OpsWorks.Types.SourceType
import Network.AWS.OpsWorks.Types.SslConfiguration
import Network.AWS.OpsWorks.Types.Stack
import Network.AWS.OpsWorks.Types.StackAttributesKeys
import Network.AWS.OpsWorks.Types.StackConfigurationManager
import Network.AWS.OpsWorks.Types.StackSummary
import Network.AWS.OpsWorks.Types.TemporaryCredential
import Network.AWS.OpsWorks.Types.TimeBasedAutoScalingConfiguration
import Network.AWS.OpsWorks.Types.UserProfile
import Network.AWS.OpsWorks.Types.VirtualizationType
import Network.AWS.OpsWorks.Types.Volume
import Network.AWS.OpsWorks.Types.VolumeConfiguration
import Network.AWS.OpsWorks.Types.VolumeType
import Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
