{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorks.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceNotFoundException,
    _ValidationException,

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
    agentVersion_configurationManager,
    agentVersion_version,

    -- * App
    App (..),
    newApp,
    app_domains,
    app_stackId,
    app_appSource,
    app_name,
    app_type,
    app_environment,
    app_dataSources,
    app_sslConfiguration,
    app_description,
    app_attributes,
    app_enableSsl,
    app_shortname,
    app_createdAt,
    app_appId,

    -- * AutoScalingThresholds
    AutoScalingThresholds (..),
    newAutoScalingThresholds,
    autoScalingThresholds_ignoreMetricsTime,
    autoScalingThresholds_alarms,
    autoScalingThresholds_memoryThreshold,
    autoScalingThresholds_instanceCount,
    autoScalingThresholds_loadThreshold,
    autoScalingThresholds_thresholdsWaitTime,
    autoScalingThresholds_cpuThreshold,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    newBlockDeviceMapping,
    blockDeviceMapping_ebs,
    blockDeviceMapping_deviceName,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_virtualName,

    -- * ChefConfiguration
    ChefConfiguration (..),
    newChefConfiguration,
    chefConfiguration_berkshelfVersion,
    chefConfiguration_manageBerkshelf,

    -- * CloudWatchLogsConfiguration
    CloudWatchLogsConfiguration (..),
    newCloudWatchLogsConfiguration,
    cloudWatchLogsConfiguration_logStreams,
    cloudWatchLogsConfiguration_enabled,

    -- * CloudWatchLogsLogStream
    CloudWatchLogsLogStream (..),
    newCloudWatchLogsLogStream,
    cloudWatchLogsLogStream_batchCount,
    cloudWatchLogsLogStream_multiLineStartPattern,
    cloudWatchLogsLogStream_fileFingerprintLines,
    cloudWatchLogsLogStream_encoding,
    cloudWatchLogsLogStream_timeZone,
    cloudWatchLogsLogStream_datetimeFormat,
    cloudWatchLogsLogStream_initialPosition,
    cloudWatchLogsLogStream_file,
    cloudWatchLogsLogStream_bufferDuration,
    cloudWatchLogsLogStream_batchSize,
    cloudWatchLogsLogStream_logGroupName,

    -- * Command
    Command (..),
    newCommand,
    command_type,
    command_deploymentId,
    command_logUrl,
    command_status,
    command_commandId,
    command_instanceId,
    command_acknowledgedAt,
    command_exitCode,
    command_completedAt,
    command_createdAt,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_type,
    dataSource_databaseName,
    dataSource_arn,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_stackId,
    deployment_iamUserArn,
    deployment_customJson,
    deployment_deploymentId,
    deployment_command,
    deployment_status,
    deployment_duration,
    deployment_comment,
    deployment_instanceIds,
    deployment_completedAt,
    deployment_createdAt,
    deployment_appId,

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
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_iops,

    -- * EcsCluster
    EcsCluster (..),
    newEcsCluster,
    ecsCluster_ecsClusterName,
    ecsCluster_stackId,
    ecsCluster_ecsClusterArn,
    ecsCluster_registeredAt,

    -- * ElasticIp
    ElasticIp (..),
    newElasticIp,
    elasticIp_name,
    elasticIp_domain,
    elasticIp_ip,
    elasticIp_region,
    elasticIp_instanceId,

    -- * ElasticLoadBalancer
    ElasticLoadBalancer (..),
    newElasticLoadBalancer,
    elasticLoadBalancer_elasticLoadBalancerName,
    elasticLoadBalancer_stackId,
    elasticLoadBalancer_availabilityZones,
    elasticLoadBalancer_region,
    elasticLoadBalancer_ec2InstanceIds,
    elasticLoadBalancer_vpcId,
    elasticLoadBalancer_layerId,
    elasticLoadBalancer_dnsName,
    elasticLoadBalancer_subnetIds,

    -- * EnvironmentVariable
    EnvironmentVariable (..),
    newEnvironmentVariable,
    environmentVariable_secure,
    environmentVariable_key,
    environmentVariable_value,

    -- * Instance
    Instance (..),
    newInstance,
    instance_ebsOptimized,
    instance_os,
    instance_privateDns,
    instance_stackId,
    instance_amiId,
    instance_ec2InstanceId,
    instance_elasticIp,
    instance_ecsClusterArn,
    instance_blockDeviceMappings,
    instance_securityGroupIds,
    instance_autoScalingType,
    instance_registeredBy,
    instance_rootDeviceType,
    instance_virtualizationType,
    instance_instanceProfileArn,
    instance_subnetId,
    instance_ecsContainerInstanceArn,
    instance_arn,
    instance_rootDeviceVolumeId,
    instance_status,
    instance_platform,
    instance_availabilityZone,
    instance_hostname,
    instance_publicIp,
    instance_infrastructureClass,
    instance_reportedAgentVersion,
    instance_instanceType,
    instance_instanceId,
    instance_sshHostDsaKeyFingerprint,
    instance_sshKeyName,
    instance_lastServiceErrorId,
    instance_layerIds,
    instance_privateIp,
    instance_reportedOs,
    instance_architecture,
    instance_createdAt,
    instance_publicDns,
    instance_tenancy,
    instance_agentVersion,
    instance_sshHostRsaKeyFingerprint,
    instance_installUpdatesOnBoot,

    -- * InstanceIdentity
    InstanceIdentity (..),
    newInstanceIdentity,
    instanceIdentity_document,
    instanceIdentity_signature,

    -- * InstancesCount
    InstancesCount (..),
    newInstancesCount,
    instancesCount_booting,
    instancesCount_deregistering,
    instancesCount_runningSetup,
    instancesCount_stopFailed,
    instancesCount_terminated,
    instancesCount_requested,
    instancesCount_connectionLost,
    instancesCount_online,
    instancesCount_startFailed,
    instancesCount_terminating,
    instancesCount_unassigning,
    instancesCount_setupFailed,
    instancesCount_rebooting,
    instancesCount_assigning,
    instancesCount_stopped,
    instancesCount_registered,
    instancesCount_pending,
    instancesCount_stopping,
    instancesCount_shuttingDown,
    instancesCount_registering,

    -- * Layer
    Layer (..),
    newLayer,
    layer_stackId,
    layer_name,
    layer_customRecipes,
    layer_type,
    layer_autoAssignPublicIps,
    layer_customJson,
    layer_packages,
    layer_volumeConfigurations,
    layer_arn,
    layer_defaultSecurityGroupNames,
    layer_enableAutoHealing,
    layer_customSecurityGroupIds,
    layer_autoAssignElasticIps,
    layer_defaultRecipes,
    layer_cloudWatchLogsConfiguration,
    layer_lifecycleEventConfiguration,
    layer_customInstanceProfileArn,
    layer_attributes,
    layer_layerId,
    layer_shortname,
    layer_createdAt,
    layer_useEbsOptimizedInstances,
    layer_installUpdatesOnBoot,

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
    operatingSystem_configurationManagers,
    operatingSystem_name,
    operatingSystem_type,
    operatingSystem_supported,
    operatingSystem_reportedVersion,
    operatingSystem_id,
    operatingSystem_reportedName,

    -- * OperatingSystemConfigurationManager
    OperatingSystemConfigurationManager (..),
    newOperatingSystemConfigurationManager,
    operatingSystemConfigurationManager_name,
    operatingSystemConfigurationManager_version,

    -- * Permission
    Permission (..),
    newPermission,
    permission_stackId,
    permission_iamUserArn,
    permission_allowSudo,
    permission_level,
    permission_allowSsh,

    -- * RaidArray
    RaidArray (..),
    newRaidArray,
    raidArray_stackId,
    raidArray_name,
    raidArray_device,
    raidArray_raidArrayId,
    raidArray_mountPoint,
    raidArray_size,
    raidArray_volumeType,
    raidArray_availabilityZone,
    raidArray_instanceId,
    raidArray_numberOfDisks,
    raidArray_raidLevel,
    raidArray_iops,
    raidArray_createdAt,

    -- * RdsDbInstance
    RdsDbInstance (..),
    newRdsDbInstance,
    rdsDbInstance_stackId,
    rdsDbInstance_missingOnRds,
    rdsDbInstance_dbInstanceIdentifier,
    rdsDbInstance_dbPassword,
    rdsDbInstance_region,
    rdsDbInstance_address,
    rdsDbInstance_rdsDbInstanceArn,
    rdsDbInstance_engine,
    rdsDbInstance_dbUser,

    -- * Recipes
    Recipes (..),
    newRecipes,
    recipes_deploy,
    recipes_configure,
    recipes_undeploy,
    recipes_shutdown,
    recipes_setup,

    -- * ReportedOs
    ReportedOs (..),
    newReportedOs,
    reportedOs_name,
    reportedOs_family,
    reportedOs_version,

    -- * SelfUserProfile
    SelfUserProfile (..),
    newSelfUserProfile,
    selfUserProfile_name,
    selfUserProfile_iamUserArn,
    selfUserProfile_sshPublicKey,
    selfUserProfile_sshUsername,

    -- * ServiceError
    ServiceError (..),
    newServiceError,
    serviceError_stackId,
    serviceError_message,
    serviceError_type,
    serviceError_serviceErrorId,
    serviceError_instanceId,
    serviceError_createdAt,

    -- * ShutdownEventConfiguration
    ShutdownEventConfiguration (..),
    newShutdownEventConfiguration,
    shutdownEventConfiguration_delayUntilElbConnectionsDrained,
    shutdownEventConfiguration_executionTimeout,

    -- * Source
    Source (..),
    newSource,
    source_type,
    source_password,
    source_username,
    source_revision,
    source_url,
    source_sshKey,

    -- * SslConfiguration
    SslConfiguration (..),
    newSslConfiguration,
    sslConfiguration_chain,
    sslConfiguration_privateKey,
    sslConfiguration_certificate,

    -- * Stack
    Stack (..),
    newStack,
    stack_hostnameTheme,
    stack_stackId,
    stack_name,
    stack_defaultSshKeyName,
    stack_customJson,
    stack_defaultAvailabilityZone,
    stack_serviceRoleArn,
    stack_defaultRootDeviceType,
    stack_defaultInstanceProfileArn,
    stack_arn,
    stack_configurationManager,
    stack_defaultSubnetId,
    stack_region,
    stack_useCustomCookbooks,
    stack_defaultOs,
    stack_useOpsworksSecurityGroups,
    stack_attributes,
    stack_vpcId,
    stack_createdAt,
    stack_chefConfiguration,
    stack_customCookbooksSource,
    stack_agentVersion,

    -- * StackConfigurationManager
    StackConfigurationManager (..),
    newStackConfigurationManager,
    stackConfigurationManager_name,
    stackConfigurationManager_version,

    -- * StackSummary
    StackSummary (..),
    newStackSummary,
    stackSummary_stackId,
    stackSummary_name,
    stackSummary_arn,
    stackSummary_instancesCount,
    stackSummary_appsCount,
    stackSummary_layersCount,

    -- * TemporaryCredential
    TemporaryCredential (..),
    newTemporaryCredential,
    temporaryCredential_password,
    temporaryCredential_username,
    temporaryCredential_validForInMinutes,
    temporaryCredential_instanceId,

    -- * TimeBasedAutoScalingConfiguration
    TimeBasedAutoScalingConfiguration (..),
    newTimeBasedAutoScalingConfiguration,
    timeBasedAutoScalingConfiguration_instanceId,
    timeBasedAutoScalingConfiguration_autoScalingSchedule,

    -- * UserProfile
    UserProfile (..),
    newUserProfile,
    userProfile_name,
    userProfile_iamUserArn,
    userProfile_sshPublicKey,
    userProfile_sshUsername,
    userProfile_allowSelfManagement,

    -- * Volume
    Volume (..),
    newVolume,
    volume_name,
    volume_device,
    volume_raidArrayId,
    volume_mountPoint,
    volume_size,
    volume_volumeType,
    volume_status,
    volume_availabilityZone,
    volume_ec2VolumeId,
    volume_region,
    volume_instanceId,
    volume_encrypted,
    volume_volumeId,
    volume_iops,

    -- * VolumeConfiguration
    VolumeConfiguration (..),
    newVolumeConfiguration,
    volumeConfiguration_volumeType,
    volumeConfiguration_encrypted,
    volumeConfiguration_raidLevel,
    volumeConfiguration_iops,
    volumeConfiguration_mountPoint,
    volumeConfiguration_numberOfDisks,
    volumeConfiguration_size,

    -- * WeeklyAutoScalingSchedule
    WeeklyAutoScalingSchedule (..),
    newWeeklyAutoScalingSchedule,
    weeklyAutoScalingSchedule_tuesday,
    weeklyAutoScalingSchedule_friday,
    weeklyAutoScalingSchedule_saturday,
    weeklyAutoScalingSchedule_thursday,
    weeklyAutoScalingSchedule_sunday,
    weeklyAutoScalingSchedule_wednesday,
    weeklyAutoScalingSchedule_monday,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
    { Core.abbrev = "OpsWorks",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "opsworks",
      Core.signingName = "opsworks",
      Core.version = "2013-02-18",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "OpsWorks",
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

-- | Indicates that a resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Indicates that a request was not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
