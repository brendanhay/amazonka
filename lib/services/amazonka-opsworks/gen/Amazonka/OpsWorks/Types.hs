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
    app_appId,
    app_appSource,
    app_attributes,
    app_createdAt,
    app_dataSources,
    app_description,
    app_domains,
    app_enableSsl,
    app_environment,
    app_name,
    app_shortname,
    app_sslConfiguration,
    app_stackId,
    app_type,

    -- * AutoScalingThresholds
    AutoScalingThresholds (..),
    newAutoScalingThresholds,
    autoScalingThresholds_alarms,
    autoScalingThresholds_cpuThreshold,
    autoScalingThresholds_ignoreMetricsTime,
    autoScalingThresholds_instanceCount,
    autoScalingThresholds_loadThreshold,
    autoScalingThresholds_memoryThreshold,
    autoScalingThresholds_thresholdsWaitTime,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    newBlockDeviceMapping,
    blockDeviceMapping_deviceName,
    blockDeviceMapping_ebs,
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
    cloudWatchLogsConfiguration_enabled,
    cloudWatchLogsConfiguration_logStreams,

    -- * CloudWatchLogsLogStream
    CloudWatchLogsLogStream (..),
    newCloudWatchLogsLogStream,
    cloudWatchLogsLogStream_batchCount,
    cloudWatchLogsLogStream_batchSize,
    cloudWatchLogsLogStream_bufferDuration,
    cloudWatchLogsLogStream_datetimeFormat,
    cloudWatchLogsLogStream_encoding,
    cloudWatchLogsLogStream_file,
    cloudWatchLogsLogStream_fileFingerprintLines,
    cloudWatchLogsLogStream_initialPosition,
    cloudWatchLogsLogStream_logGroupName,
    cloudWatchLogsLogStream_multiLineStartPattern,
    cloudWatchLogsLogStream_timeZone,

    -- * Command
    Command (..),
    newCommand,
    command_acknowledgedAt,
    command_commandId,
    command_completedAt,
    command_createdAt,
    command_deploymentId,
    command_exitCode,
    command_instanceId,
    command_logUrl,
    command_status,
    command_type,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_arn,
    dataSource_databaseName,
    dataSource_type,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_appId,
    deployment_command,
    deployment_comment,
    deployment_completedAt,
    deployment_createdAt,
    deployment_customJson,
    deployment_deploymentId,
    deployment_duration,
    deployment_iamUserArn,
    deployment_instanceIds,
    deployment_stackId,
    deployment_status,

    -- * DeploymentCommand
    DeploymentCommand (..),
    newDeploymentCommand,
    deploymentCommand_args,
    deploymentCommand_name,

    -- * EbsBlockDevice
    EbsBlockDevice (..),
    newEbsBlockDevice,
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_iops,
    ebsBlockDevice_snapshotId,
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_volumeType,

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
    elasticIp_domain,
    elasticIp_instanceId,
    elasticIp_ip,
    elasticIp_name,
    elasticIp_region,

    -- * ElasticLoadBalancer
    ElasticLoadBalancer (..),
    newElasticLoadBalancer,
    elasticLoadBalancer_availabilityZones,
    elasticLoadBalancer_dnsName,
    elasticLoadBalancer_ec2InstanceIds,
    elasticLoadBalancer_elasticLoadBalancerName,
    elasticLoadBalancer_layerId,
    elasticLoadBalancer_region,
    elasticLoadBalancer_stackId,
    elasticLoadBalancer_subnetIds,
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
    instance_agentVersion,
    instance_amiId,
    instance_architecture,
    instance_arn,
    instance_autoScalingType,
    instance_availabilityZone,
    instance_blockDeviceMappings,
    instance_createdAt,
    instance_ebsOptimized,
    instance_ec2InstanceId,
    instance_ecsClusterArn,
    instance_ecsContainerInstanceArn,
    instance_elasticIp,
    instance_hostname,
    instance_infrastructureClass,
    instance_installUpdatesOnBoot,
    instance_instanceId,
    instance_instanceProfileArn,
    instance_instanceType,
    instance_lastServiceErrorId,
    instance_layerIds,
    instance_os,
    instance_platform,
    instance_privateDns,
    instance_privateIp,
    instance_publicDns,
    instance_publicIp,
    instance_registeredBy,
    instance_reportedAgentVersion,
    instance_reportedOs,
    instance_rootDeviceType,
    instance_rootDeviceVolumeId,
    instance_securityGroupIds,
    instance_sshHostDsaKeyFingerprint,
    instance_sshHostRsaKeyFingerprint,
    instance_sshKeyName,
    instance_stackId,
    instance_status,
    instance_subnetId,
    instance_tenancy,
    instance_virtualizationType,

    -- * InstanceIdentity
    InstanceIdentity (..),
    newInstanceIdentity,
    instanceIdentity_document,
    instanceIdentity_signature,

    -- * InstancesCount
    InstancesCount (..),
    newInstancesCount,
    instancesCount_assigning,
    instancesCount_booting,
    instancesCount_connectionLost,
    instancesCount_deregistering,
    instancesCount_online,
    instancesCount_pending,
    instancesCount_rebooting,
    instancesCount_registered,
    instancesCount_registering,
    instancesCount_requested,
    instancesCount_runningSetup,
    instancesCount_setupFailed,
    instancesCount_shuttingDown,
    instancesCount_startFailed,
    instancesCount_stopFailed,
    instancesCount_stopped,
    instancesCount_stopping,
    instancesCount_terminated,
    instancesCount_terminating,
    instancesCount_unassigning,

    -- * Layer
    Layer (..),
    newLayer,
    layer_arn,
    layer_attributes,
    layer_autoAssignElasticIps,
    layer_autoAssignPublicIps,
    layer_cloudWatchLogsConfiguration,
    layer_createdAt,
    layer_customInstanceProfileArn,
    layer_customJson,
    layer_customRecipes,
    layer_customSecurityGroupIds,
    layer_defaultRecipes,
    layer_defaultSecurityGroupNames,
    layer_enableAutoHealing,
    layer_installUpdatesOnBoot,
    layer_layerId,
    layer_lifecycleEventConfiguration,
    layer_name,
    layer_packages,
    layer_shortname,
    layer_stackId,
    layer_type,
    layer_useEbsOptimizedInstances,
    layer_volumeConfigurations,

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
    operatingSystem_configurationManagers,
    operatingSystem_id,
    operatingSystem_name,
    operatingSystem_reportedName,
    operatingSystem_reportedVersion,
    operatingSystem_supported,
    operatingSystem_type,

    -- * OperatingSystemConfigurationManager
    OperatingSystemConfigurationManager (..),
    newOperatingSystemConfigurationManager,
    operatingSystemConfigurationManager_name,
    operatingSystemConfigurationManager_version,

    -- * Permission
    Permission (..),
    newPermission,
    permission_allowSsh,
    permission_allowSudo,
    permission_iamUserArn,
    permission_level,
    permission_stackId,

    -- * RaidArray
    RaidArray (..),
    newRaidArray,
    raidArray_availabilityZone,
    raidArray_createdAt,
    raidArray_device,
    raidArray_instanceId,
    raidArray_iops,
    raidArray_mountPoint,
    raidArray_name,
    raidArray_numberOfDisks,
    raidArray_raidArrayId,
    raidArray_raidLevel,
    raidArray_size,
    raidArray_stackId,
    raidArray_volumeType,

    -- * RdsDbInstance
    RdsDbInstance (..),
    newRdsDbInstance,
    rdsDbInstance_address,
    rdsDbInstance_dbInstanceIdentifier,
    rdsDbInstance_dbPassword,
    rdsDbInstance_dbUser,
    rdsDbInstance_engine,
    rdsDbInstance_missingOnRds,
    rdsDbInstance_rdsDbInstanceArn,
    rdsDbInstance_region,
    rdsDbInstance_stackId,

    -- * Recipes
    Recipes (..),
    newRecipes,
    recipes_configure,
    recipes_deploy,
    recipes_setup,
    recipes_shutdown,
    recipes_undeploy,

    -- * ReportedOs
    ReportedOs (..),
    newReportedOs,
    reportedOs_family,
    reportedOs_name,
    reportedOs_version,

    -- * SelfUserProfile
    SelfUserProfile (..),
    newSelfUserProfile,
    selfUserProfile_iamUserArn,
    selfUserProfile_name,
    selfUserProfile_sshPublicKey,
    selfUserProfile_sshUsername,

    -- * ServiceError
    ServiceError (..),
    newServiceError,
    serviceError_createdAt,
    serviceError_instanceId,
    serviceError_message,
    serviceError_serviceErrorId,
    serviceError_stackId,
    serviceError_type,

    -- * ShutdownEventConfiguration
    ShutdownEventConfiguration (..),
    newShutdownEventConfiguration,
    shutdownEventConfiguration_delayUntilElbConnectionsDrained,
    shutdownEventConfiguration_executionTimeout,

    -- * Source
    Source (..),
    newSource,
    source_password,
    source_revision,
    source_sshKey,
    source_type,
    source_url,
    source_username,

    -- * SslConfiguration
    SslConfiguration (..),
    newSslConfiguration,
    sslConfiguration_certificate,
    sslConfiguration_chain,
    sslConfiguration_privateKey,

    -- * Stack
    Stack (..),
    newStack,
    stack_agentVersion,
    stack_arn,
    stack_attributes,
    stack_chefConfiguration,
    stack_configurationManager,
    stack_createdAt,
    stack_customCookbooksSource,
    stack_customJson,
    stack_defaultAvailabilityZone,
    stack_defaultInstanceProfileArn,
    stack_defaultOs,
    stack_defaultRootDeviceType,
    stack_defaultSshKeyName,
    stack_defaultSubnetId,
    stack_hostnameTheme,
    stack_name,
    stack_region,
    stack_serviceRoleArn,
    stack_stackId,
    stack_useCustomCookbooks,
    stack_useOpsworksSecurityGroups,
    stack_vpcId,

    -- * StackConfigurationManager
    StackConfigurationManager (..),
    newStackConfigurationManager,
    stackConfigurationManager_name,
    stackConfigurationManager_version,

    -- * StackSummary
    StackSummary (..),
    newStackSummary,
    stackSummary_appsCount,
    stackSummary_arn,
    stackSummary_instancesCount,
    stackSummary_layersCount,
    stackSummary_name,
    stackSummary_stackId,

    -- * TemporaryCredential
    TemporaryCredential (..),
    newTemporaryCredential,
    temporaryCredential_instanceId,
    temporaryCredential_password,
    temporaryCredential_username,
    temporaryCredential_validForInMinutes,

    -- * TimeBasedAutoScalingConfiguration
    TimeBasedAutoScalingConfiguration (..),
    newTimeBasedAutoScalingConfiguration,
    timeBasedAutoScalingConfiguration_autoScalingSchedule,
    timeBasedAutoScalingConfiguration_instanceId,

    -- * UserProfile
    UserProfile (..),
    newUserProfile,
    userProfile_allowSelfManagement,
    userProfile_iamUserArn,
    userProfile_name,
    userProfile_sshPublicKey,
    userProfile_sshUsername,

    -- * Volume
    Volume (..),
    newVolume,
    volume_availabilityZone,
    volume_device,
    volume_ec2VolumeId,
    volume_encrypted,
    volume_instanceId,
    volume_iops,
    volume_mountPoint,
    volume_name,
    volume_raidArrayId,
    volume_region,
    volume_size,
    volume_status,
    volume_volumeId,
    volume_volumeType,

    -- * VolumeConfiguration
    VolumeConfiguration (..),
    newVolumeConfiguration,
    volumeConfiguration_encrypted,
    volumeConfiguration_iops,
    volumeConfiguration_raidLevel,
    volumeConfiguration_volumeType,
    volumeConfiguration_mountPoint,
    volumeConfiguration_numberOfDisks,
    volumeConfiguration_size,

    -- * WeeklyAutoScalingSchedule
    WeeklyAutoScalingSchedule (..),
    newWeeklyAutoScalingSchedule,
    weeklyAutoScalingSchedule_friday,
    weeklyAutoScalingSchedule_monday,
    weeklyAutoScalingSchedule_saturday,
    weeklyAutoScalingSchedule_sunday,
    weeklyAutoScalingSchedule_thursday,
    weeklyAutoScalingSchedule_tuesday,
    weeklyAutoScalingSchedule_wednesday,
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
