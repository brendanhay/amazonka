{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _UpdateInProgressException,
    _TargetNotConnectedException,
    _PlatformTaskDefinitionIncompatibilityException,
    _ServiceNotFoundException,
    _UnsupportedFeatureException,
    _TaskSetNotFoundException,
    _ClusterContainsContainerInstancesException,
    _ClusterContainsServicesException,
    _PlatformUnknownException,
    _InvalidParameterException,
    _BlockedException,
    _AccessDeniedException,
    _MissingVersionException,
    _LimitExceededException,
    _ResourceInUseException,
    _ClusterNotFoundException,
    _ResourceNotFoundException,
    _ClientException,
    _NoUpdateAvailableException,
    _ServiceNotActiveException,
    _ClusterContainsTasksException,
    _AttributeLimitExceededException,
    _TargetNotFoundException,
    _ServerException,

    -- * AgentUpdateStatus
    AgentUpdateStatus (..),

    -- * AssignPublicIp
    AssignPublicIp (..),

    -- * CapacityProviderField
    CapacityProviderField (..),

    -- * CapacityProviderStatus
    CapacityProviderStatus (..),

    -- * CapacityProviderUpdateStatus
    CapacityProviderUpdateStatus (..),

    -- * ClusterField
    ClusterField (..),

    -- * ClusterSettingName
    ClusterSettingName (..),

    -- * Compatibility
    Compatibility (..),

    -- * Connectivity
    Connectivity (..),

    -- * ContainerCondition
    ContainerCondition (..),

    -- * ContainerInstanceField
    ContainerInstanceField (..),

    -- * ContainerInstanceStatus
    ContainerInstanceStatus (..),

    -- * DeploymentControllerType
    DeploymentControllerType (..),

    -- * DeploymentRolloutState
    DeploymentRolloutState (..),

    -- * DesiredStatus
    DesiredStatus (..),

    -- * DeviceCgroupPermission
    DeviceCgroupPermission (..),

    -- * EFSAuthorizationConfigIAM
    EFSAuthorizationConfigIAM (..),

    -- * EFSTransitEncryption
    EFSTransitEncryption (..),

    -- * EnvironmentFileType
    EnvironmentFileType (..),

    -- * ExecuteCommandLogging
    ExecuteCommandLogging (..),

    -- * FirelensConfigurationType
    FirelensConfigurationType (..),

    -- * HealthStatus
    HealthStatus (..),

    -- * IpcMode
    IpcMode (..),

    -- * LaunchType
    LaunchType (..),

    -- * LogDriver
    LogDriver (..),

    -- * ManagedAgentName
    ManagedAgentName (..),

    -- * ManagedScalingStatus
    ManagedScalingStatus (..),

    -- * ManagedTerminationProtection
    ManagedTerminationProtection (..),

    -- * NetworkMode
    NetworkMode (..),

    -- * PidMode
    PidMode (..),

    -- * PlacementConstraintType
    PlacementConstraintType (..),

    -- * PlacementStrategyType
    PlacementStrategyType (..),

    -- * PlatformDeviceType
    PlatformDeviceType (..),

    -- * PropagateTags
    PropagateTags (..),

    -- * ProxyConfigurationType
    ProxyConfigurationType (..),

    -- * ResourceType
    ResourceType (..),

    -- * ScaleUnit
    ScaleUnit (..),

    -- * SchedulingStrategy
    SchedulingStrategy (..),

    -- * Scope
    Scope (..),

    -- * ServiceField
    ServiceField (..),

    -- * SettingName
    SettingName (..),

    -- * SortOrder
    SortOrder (..),

    -- * StabilityStatus
    StabilityStatus (..),

    -- * TargetType
    TargetType (..),

    -- * TaskDefinitionFamilyStatus
    TaskDefinitionFamilyStatus (..),

    -- * TaskDefinitionField
    TaskDefinitionField (..),

    -- * TaskDefinitionPlacementConstraintType
    TaskDefinitionPlacementConstraintType (..),

    -- * TaskDefinitionStatus
    TaskDefinitionStatus (..),

    -- * TaskField
    TaskField (..),

    -- * TaskSetField
    TaskSetField (..),

    -- * TaskStopCode
    TaskStopCode (..),

    -- * TransportProtocol
    TransportProtocol (..),

    -- * UlimitName
    UlimitName (..),

    -- * Attachment
    Attachment (..),
    newAttachment,
    attachment_status,
    attachment_id,
    attachment_details,
    attachment_type,

    -- * AttachmentStateChange
    AttachmentStateChange (..),
    newAttachmentStateChange,
    attachmentStateChange_attachmentArn,
    attachmentStateChange_status,

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_targetId,
    attribute_targetType,
    attribute_value,
    attribute_name,

    -- * AutoScalingGroupProvider
    AutoScalingGroupProvider (..),
    newAutoScalingGroupProvider,
    autoScalingGroupProvider_managedScaling,
    autoScalingGroupProvider_managedTerminationProtection,
    autoScalingGroupProvider_autoScalingGroupArn,

    -- * AutoScalingGroupProviderUpdate
    AutoScalingGroupProviderUpdate (..),
    newAutoScalingGroupProviderUpdate,
    autoScalingGroupProviderUpdate_managedScaling,
    autoScalingGroupProviderUpdate_managedTerminationProtection,

    -- * AwsVpcConfiguration
    AwsVpcConfiguration (..),
    newAwsVpcConfiguration,
    awsVpcConfiguration_assignPublicIp,
    awsVpcConfiguration_securityGroups,
    awsVpcConfiguration_subnets,

    -- * CapacityProvider
    CapacityProvider (..),
    newCapacityProvider,
    capacityProvider_status,
    capacityProvider_updateStatusReason,
    capacityProvider_capacityProviderArn,
    capacityProvider_updateStatus,
    capacityProvider_name,
    capacityProvider_autoScalingGroupProvider,
    capacityProvider_tags,

    -- * CapacityProviderStrategyItem
    CapacityProviderStrategyItem (..),
    newCapacityProviderStrategyItem,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_capacityProvider,

    -- * Cluster
    Cluster (..),
    newCluster,
    cluster_clusterArn,
    cluster_status,
    cluster_activeServicesCount,
    cluster_registeredContainerInstancesCount,
    cluster_configuration,
    cluster_statistics,
    cluster_defaultCapacityProviderStrategy,
    cluster_pendingTasksCount,
    cluster_tags,
    cluster_capacityProviders,
    cluster_attachmentsStatus,
    cluster_clusterName,
    cluster_settings,
    cluster_runningTasksCount,
    cluster_attachments,

    -- * ClusterConfiguration
    ClusterConfiguration (..),
    newClusterConfiguration,
    clusterConfiguration_executeCommandConfiguration,

    -- * ClusterSetting
    ClusterSetting (..),
    newClusterSetting,
    clusterSetting_name,
    clusterSetting_value,

    -- * Container
    Container (..),
    newContainer,
    container_imageDigest,
    container_gpuIds,
    container_memoryReservation,
    container_memory,
    container_runtimeId,
    container_exitCode,
    container_containerArn,
    container_name,
    container_image,
    container_managedAgents,
    container_networkBindings,
    container_reason,
    container_lastStatus,
    container_cpu,
    container_networkInterfaces,
    container_healthStatus,
    container_taskArn,

    -- * ContainerDefinition
    ContainerDefinition (..),
    newContainerDefinition,
    containerDefinition_hostname,
    containerDefinition_linuxParameters,
    containerDefinition_firelensConfiguration,
    containerDefinition_dependsOn,
    containerDefinition_memoryReservation,
    containerDefinition_dockerLabels,
    containerDefinition_memory,
    containerDefinition_extraHosts,
    containerDefinition_user,
    containerDefinition_systemControls,
    containerDefinition_privileged,
    containerDefinition_links,
    containerDefinition_interactive,
    containerDefinition_environmentFiles,
    containerDefinition_entryPoint,
    containerDefinition_workingDirectory,
    containerDefinition_environment,
    containerDefinition_secrets,
    containerDefinition_volumesFrom,
    containerDefinition_mountPoints,
    containerDefinition_command,
    containerDefinition_dnsServers,
    containerDefinition_name,
    containerDefinition_image,
    containerDefinition_dnsSearchDomains,
    containerDefinition_pseudoTerminal,
    containerDefinition_logConfiguration,
    containerDefinition_portMappings,
    containerDefinition_essential,
    containerDefinition_cpu,
    containerDefinition_resourceRequirements,
    containerDefinition_ulimits,
    containerDefinition_startTimeout,
    containerDefinition_readonlyRootFilesystem,
    containerDefinition_stopTimeout,
    containerDefinition_healthCheck,
    containerDefinition_dockerSecurityOptions,
    containerDefinition_disableNetworking,
    containerDefinition_repositoryCredentials,

    -- * ContainerDependency
    ContainerDependency (..),
    newContainerDependency,
    containerDependency_containerName,
    containerDependency_condition,

    -- * ContainerInstance
    ContainerInstance (..),
    newContainerInstance,
    containerInstance_agentUpdateStatus,
    containerInstance_versionInfo,
    containerInstance_status,
    containerInstance_registeredResources,
    containerInstance_containerInstanceArn,
    containerInstance_registeredAt,
    containerInstance_pendingTasksCount,
    containerInstance_version,
    containerInstance_attributes,
    containerInstance_tags,
    containerInstance_agentConnected,
    containerInstance_ec2InstanceId,
    containerInstance_statusReason,
    containerInstance_remainingResources,
    containerInstance_runningTasksCount,
    containerInstance_capacityProviderName,
    containerInstance_attachments,

    -- * ContainerOverride
    ContainerOverride (..),
    newContainerOverride,
    containerOverride_memoryReservation,
    containerOverride_memory,
    containerOverride_environmentFiles,
    containerOverride_environment,
    containerOverride_command,
    containerOverride_name,
    containerOverride_cpu,
    containerOverride_resourceRequirements,

    -- * ContainerService
    ContainerService (..),
    newContainerService,
    containerService_clusterArn,
    containerService_taskSets,
    containerService_runningCount,
    containerService_status,
    containerService_roleArn,
    containerService_deploymentConfiguration,
    containerService_capacityProviderStrategy,
    containerService_networkConfiguration,
    containerService_desiredCount,
    containerService_enableECSManagedTags,
    containerService_deploymentController,
    containerService_launchType,
    containerService_createdAt,
    containerService_platformVersion,
    containerService_deployments,
    containerService_placementStrategy,
    containerService_serviceName,
    containerService_placementConstraints,
    containerService_events,
    containerService_pendingCount,
    containerService_enableExecuteCommand,
    containerService_loadBalancers,
    containerService_tags,
    containerService_healthCheckGracePeriodSeconds,
    containerService_serviceRegistries,
    containerService_createdBy,
    containerService_schedulingStrategy,
    containerService_taskDefinition,
    containerService_serviceArn,
    containerService_propagateTags,

    -- * ContainerStateChange
    ContainerStateChange (..),
    newContainerStateChange,
    containerStateChange_imageDigest,
    containerStateChange_status,
    containerStateChange_runtimeId,
    containerStateChange_exitCode,
    containerStateChange_networkBindings,
    containerStateChange_reason,
    containerStateChange_containerName,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_rolloutState,
    deployment_runningCount,
    deployment_status,
    deployment_capacityProviderStrategy,
    deployment_networkConfiguration,
    deployment_desiredCount,
    deployment_updatedAt,
    deployment_launchType,
    deployment_id,
    deployment_createdAt,
    deployment_platformVersion,
    deployment_pendingCount,
    deployment_taskDefinition,
    deployment_rolloutStateReason,
    deployment_failedTasks,

    -- * DeploymentCircuitBreaker
    DeploymentCircuitBreaker (..),
    newDeploymentCircuitBreaker,
    deploymentCircuitBreaker_enable,
    deploymentCircuitBreaker_rollback,

    -- * DeploymentConfiguration
    DeploymentConfiguration (..),
    newDeploymentConfiguration,
    deploymentConfiguration_maximumPercent,
    deploymentConfiguration_minimumHealthyPercent,
    deploymentConfiguration_deploymentCircuitBreaker,

    -- * DeploymentController
    DeploymentController (..),
    newDeploymentController,
    deploymentController_type,

    -- * Device
    Device (..),
    newDevice,
    device_permissions,
    device_containerPath,
    device_hostPath,

    -- * DockerVolumeConfiguration
    DockerVolumeConfiguration (..),
    newDockerVolumeConfiguration,
    dockerVolumeConfiguration_labels,
    dockerVolumeConfiguration_scope,
    dockerVolumeConfiguration_driverOpts,
    dockerVolumeConfiguration_autoprovision,
    dockerVolumeConfiguration_driver,

    -- * EFSAuthorizationConfig
    EFSAuthorizationConfig (..),
    newEFSAuthorizationConfig,
    eFSAuthorizationConfig_accessPointId,
    eFSAuthorizationConfig_iam,

    -- * EFSVolumeConfiguration
    EFSVolumeConfiguration (..),
    newEFSVolumeConfiguration,
    eFSVolumeConfiguration_transitEncryptionPort,
    eFSVolumeConfiguration_rootDirectory,
    eFSVolumeConfiguration_authorizationConfig,
    eFSVolumeConfiguration_transitEncryption,
    eFSVolumeConfiguration_fileSystemId,

    -- * EnvironmentFile
    EnvironmentFile (..),
    newEnvironmentFile,
    environmentFile_value,
    environmentFile_type,

    -- * EphemeralStorage
    EphemeralStorage (..),
    newEphemeralStorage,
    ephemeralStorage_sizeInGiB,

    -- * ExecuteCommandConfiguration
    ExecuteCommandConfiguration (..),
    newExecuteCommandConfiguration,
    executeCommandConfiguration_logging,
    executeCommandConfiguration_kmsKeyId,
    executeCommandConfiguration_logConfiguration,

    -- * ExecuteCommandLogConfiguration
    ExecuteCommandLogConfiguration (..),
    newExecuteCommandLogConfiguration,
    executeCommandLogConfiguration_cloudWatchLogGroupName,
    executeCommandLogConfiguration_cloudWatchEncryptionEnabled,
    executeCommandLogConfiguration_s3EncryptionEnabled,
    executeCommandLogConfiguration_s3KeyPrefix,
    executeCommandLogConfiguration_s3BucketName,

    -- * FSxWindowsFileServerAuthorizationConfig
    FSxWindowsFileServerAuthorizationConfig (..),
    newFSxWindowsFileServerAuthorizationConfig,
    fSxWindowsFileServerAuthorizationConfig_credentialsParameter,
    fSxWindowsFileServerAuthorizationConfig_domain,

    -- * FSxWindowsFileServerVolumeConfiguration
    FSxWindowsFileServerVolumeConfiguration (..),
    newFSxWindowsFileServerVolumeConfiguration,
    fSxWindowsFileServerVolumeConfiguration_fileSystemId,
    fSxWindowsFileServerVolumeConfiguration_rootDirectory,
    fSxWindowsFileServerVolumeConfiguration_authorizationConfig,

    -- * Failure
    Failure (..),
    newFailure,
    failure_arn,
    failure_reason,
    failure_detail,

    -- * FirelensConfiguration
    FirelensConfiguration (..),
    newFirelensConfiguration,
    firelensConfiguration_options,
    firelensConfiguration_type,

    -- * HealthCheck
    HealthCheck (..),
    newHealthCheck,
    healthCheck_retries,
    healthCheck_timeout,
    healthCheck_startPeriod,
    healthCheck_interval,
    healthCheck_command,

    -- * HostEntry
    HostEntry (..),
    newHostEntry,
    hostEntry_hostname,
    hostEntry_ipAddress,

    -- * HostVolumeProperties
    HostVolumeProperties (..),
    newHostVolumeProperties,
    hostVolumeProperties_sourcePath,

    -- * InferenceAccelerator
    InferenceAccelerator (..),
    newInferenceAccelerator,
    inferenceAccelerator_deviceName,
    inferenceAccelerator_deviceType,

    -- * InferenceAcceleratorOverride
    InferenceAcceleratorOverride (..),
    newInferenceAcceleratorOverride,
    inferenceAcceleratorOverride_deviceName,
    inferenceAcceleratorOverride_deviceType,

    -- * KernelCapabilities
    KernelCapabilities (..),
    newKernelCapabilities,
    kernelCapabilities_drop,
    kernelCapabilities_add,

    -- * KeyValuePair
    KeyValuePair (..),
    newKeyValuePair,
    keyValuePair_name,
    keyValuePair_value,

    -- * LinuxParameters
    LinuxParameters (..),
    newLinuxParameters,
    linuxParameters_tmpfs,
    linuxParameters_maxSwap,
    linuxParameters_capabilities,
    linuxParameters_devices,
    linuxParameters_swappiness,
    linuxParameters_initProcessEnabled,
    linuxParameters_sharedMemorySize,

    -- * LoadBalancer
    LoadBalancer (..),
    newLoadBalancer,
    loadBalancer_targetGroupArn,
    loadBalancer_containerPort,
    loadBalancer_containerName,
    loadBalancer_loadBalancerName,

    -- * LogConfiguration
    LogConfiguration (..),
    newLogConfiguration,
    logConfiguration_options,
    logConfiguration_secretOptions,
    logConfiguration_logDriver,

    -- * ManagedAgent
    ManagedAgent (..),
    newManagedAgent,
    managedAgent_name,
    managedAgent_reason,
    managedAgent_lastStatus,
    managedAgent_lastStartedAt,

    -- * ManagedAgentStateChange
    ManagedAgentStateChange (..),
    newManagedAgentStateChange,
    managedAgentStateChange_reason,
    managedAgentStateChange_containerName,
    managedAgentStateChange_managedAgentName,
    managedAgentStateChange_status,

    -- * ManagedScaling
    ManagedScaling (..),
    newManagedScaling,
    managedScaling_status,
    managedScaling_maximumScalingStepSize,
    managedScaling_minimumScalingStepSize,
    managedScaling_instanceWarmupPeriod,
    managedScaling_targetCapacity,

    -- * MountPoint
    MountPoint (..),
    newMountPoint,
    mountPoint_readOnly,
    mountPoint_sourceVolume,
    mountPoint_containerPath,

    -- * NetworkBinding
    NetworkBinding (..),
    newNetworkBinding,
    networkBinding_hostPort,
    networkBinding_bindIP,
    networkBinding_protocol,
    networkBinding_containerPort,

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    newNetworkConfiguration,
    networkConfiguration_awsvpcConfiguration,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_privateIpv4Address,
    networkInterface_ipv6Address,
    networkInterface_attachmentId,

    -- * PlacementConstraint
    PlacementConstraint (..),
    newPlacementConstraint,
    placementConstraint_type,
    placementConstraint_expression,

    -- * PlacementStrategy
    PlacementStrategy (..),
    newPlacementStrategy,
    placementStrategy_type,
    placementStrategy_field,

    -- * PlatformDevice
    PlatformDevice (..),
    newPlatformDevice,
    platformDevice_id,
    platformDevice_type,

    -- * PortMapping
    PortMapping (..),
    newPortMapping,
    portMapping_hostPort,
    portMapping_protocol,
    portMapping_containerPort,

    -- * ProxyConfiguration
    ProxyConfiguration (..),
    newProxyConfiguration,
    proxyConfiguration_properties,
    proxyConfiguration_type,
    proxyConfiguration_containerName,

    -- * RepositoryCredentials
    RepositoryCredentials (..),
    newRepositoryCredentials,
    repositoryCredentials_credentialsParameter,

    -- * Resource
    Resource (..),
    newResource,
    resource_stringSetValue,
    resource_doubleValue,
    resource_name,
    resource_longValue,
    resource_type,
    resource_integerValue,

    -- * ResourceRequirement
    ResourceRequirement (..),
    newResourceRequirement,
    resourceRequirement_value,
    resourceRequirement_type,

    -- * Scale
    Scale (..),
    newScale,
    scale_unit,
    scale_value,

    -- * Secret
    Secret (..),
    newSecret,
    secret_name,
    secret_valueFrom,

    -- * ServiceEvent
    ServiceEvent (..),
    newServiceEvent,
    serviceEvent_message,
    serviceEvent_id,
    serviceEvent_createdAt,

    -- * ServiceRegistry
    ServiceRegistry (..),
    newServiceRegistry,
    serviceRegistry_port,
    serviceRegistry_containerPort,
    serviceRegistry_containerName,
    serviceRegistry_registryArn,

    -- * Session
    Session (..),
    newSession,
    session_sessionId,
    session_streamUrl,
    session_tokenValue,

    -- * Setting
    Setting (..),
    newSetting,
    setting_name,
    setting_principalArn,
    setting_value,

    -- * SystemControl
    SystemControl (..),
    newSystemControl,
    systemControl_value,
    systemControl_namespace,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Task
    Task (..),
    newTask,
    task_clusterArn,
    task_startedAt,
    task_memory,
    task_pullStartedAt,
    task_containerInstanceArn,
    task_launchType,
    task_createdAt,
    task_platformVersion,
    task_connectivity,
    task_stoppingAt,
    task_startedBy,
    task_version,
    task_group,
    task_availabilityZone,
    task_inferenceAccelerators,
    task_attributes,
    task_overrides,
    task_desiredStatus,
    task_stoppedAt,
    task_containers,
    task_enableExecuteCommand,
    task_tags,
    task_pullStoppedAt,
    task_ephemeralStorage,
    task_lastStatus,
    task_executionStoppedAt,
    task_cpu,
    task_healthStatus,
    task_connectivityAt,
    task_taskArn,
    task_taskDefinitionArn,
    task_stopCode,
    task_stoppedReason,
    task_capacityProviderName,
    task_attachments,

    -- * TaskDefinition
    TaskDefinition (..),
    newTaskDefinition,
    taskDefinition_taskRoleArn,
    taskDefinition_status,
    taskDefinition_memory,
    taskDefinition_containerDefinitions,
    taskDefinition_requiresCompatibilities,
    taskDefinition_pidMode,
    taskDefinition_volumes,
    taskDefinition_executionRoleArn,
    taskDefinition_compatibilities,
    taskDefinition_registeredAt,
    taskDefinition_placementConstraints,
    taskDefinition_inferenceAccelerators,
    taskDefinition_deregisteredAt,
    taskDefinition_proxyConfiguration,
    taskDefinition_requiresAttributes,
    taskDefinition_ipcMode,
    taskDefinition_family,
    taskDefinition_ephemeralStorage,
    taskDefinition_cpu,
    taskDefinition_registeredBy,
    taskDefinition_revision,
    taskDefinition_networkMode,
    taskDefinition_taskDefinitionArn,

    -- * TaskDefinitionPlacementConstraint
    TaskDefinitionPlacementConstraint (..),
    newTaskDefinitionPlacementConstraint,
    taskDefinitionPlacementConstraint_type,
    taskDefinitionPlacementConstraint_expression,

    -- * TaskOverride
    TaskOverride (..),
    newTaskOverride,
    taskOverride_taskRoleArn,
    taskOverride_memory,
    taskOverride_inferenceAcceleratorOverrides,
    taskOverride_executionRoleArn,
    taskOverride_containerOverrides,
    taskOverride_ephemeralStorage,
    taskOverride_cpu,

    -- * TaskSet
    TaskSet (..),
    newTaskSet,
    taskSet_clusterArn,
    taskSet_stabilityStatusAt,
    taskSet_runningCount,
    taskSet_status,
    taskSet_stabilityStatus,
    taskSet_capacityProviderStrategy,
    taskSet_networkConfiguration,
    taskSet_updatedAt,
    taskSet_launchType,
    taskSet_id,
    taskSet_createdAt,
    taskSet_platformVersion,
    taskSet_startedBy,
    taskSet_computedDesiredCount,
    taskSet_pendingCount,
    taskSet_loadBalancers,
    taskSet_tags,
    taskSet_serviceRegistries,
    taskSet_scale,
    taskSet_taskDefinition,
    taskSet_serviceArn,
    taskSet_externalId,
    taskSet_taskSetArn,

    -- * Tmpfs
    Tmpfs (..),
    newTmpfs,
    tmpfs_mountOptions,
    tmpfs_containerPath,
    tmpfs_size,

    -- * Ulimit
    Ulimit (..),
    newUlimit,
    ulimit_name,
    ulimit_softLimit,
    ulimit_hardLimit,

    -- * VersionInfo
    VersionInfo (..),
    newVersionInfo,
    versionInfo_agentVersion,
    versionInfo_dockerVersion,
    versionInfo_agentHash,

    -- * Volume
    Volume (..),
    newVolume,
    volume_name,
    volume_dockerVolumeConfiguration,
    volume_fsxWindowsFileServerVolumeConfiguration,
    volume_host,
    volume_efsVolumeConfiguration,

    -- * VolumeFrom
    VolumeFrom (..),
    newVolumeFrom,
    volumeFrom_readOnly,
    volumeFrom_sourceContainer,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.AgentUpdateStatus
import Network.AWS.ECS.Types.AssignPublicIp
import Network.AWS.ECS.Types.Attachment
import Network.AWS.ECS.Types.AttachmentStateChange
import Network.AWS.ECS.Types.Attribute
import Network.AWS.ECS.Types.AutoScalingGroupProvider
import Network.AWS.ECS.Types.AutoScalingGroupProviderUpdate
import Network.AWS.ECS.Types.AwsVpcConfiguration
import Network.AWS.ECS.Types.CapacityProvider
import Network.AWS.ECS.Types.CapacityProviderField
import Network.AWS.ECS.Types.CapacityProviderStatus
import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.CapacityProviderUpdateStatus
import Network.AWS.ECS.Types.Cluster
import Network.AWS.ECS.Types.ClusterConfiguration
import Network.AWS.ECS.Types.ClusterField
import Network.AWS.ECS.Types.ClusterSetting
import Network.AWS.ECS.Types.ClusterSettingName
import Network.AWS.ECS.Types.Compatibility
import Network.AWS.ECS.Types.Connectivity
import Network.AWS.ECS.Types.Container
import Network.AWS.ECS.Types.ContainerCondition
import Network.AWS.ECS.Types.ContainerDefinition
import Network.AWS.ECS.Types.ContainerDependency
import Network.AWS.ECS.Types.ContainerInstance
import Network.AWS.ECS.Types.ContainerInstanceField
import Network.AWS.ECS.Types.ContainerInstanceStatus
import Network.AWS.ECS.Types.ContainerOverride
import Network.AWS.ECS.Types.ContainerService
import Network.AWS.ECS.Types.ContainerStateChange
import Network.AWS.ECS.Types.Deployment
import Network.AWS.ECS.Types.DeploymentCircuitBreaker
import Network.AWS.ECS.Types.DeploymentConfiguration
import Network.AWS.ECS.Types.DeploymentController
import Network.AWS.ECS.Types.DeploymentControllerType
import Network.AWS.ECS.Types.DeploymentRolloutState
import Network.AWS.ECS.Types.DesiredStatus
import Network.AWS.ECS.Types.Device
import Network.AWS.ECS.Types.DeviceCgroupPermission
import Network.AWS.ECS.Types.DockerVolumeConfiguration
import Network.AWS.ECS.Types.EFSAuthorizationConfig
import Network.AWS.ECS.Types.EFSAuthorizationConfigIAM
import Network.AWS.ECS.Types.EFSTransitEncryption
import Network.AWS.ECS.Types.EFSVolumeConfiguration
import Network.AWS.ECS.Types.EnvironmentFile
import Network.AWS.ECS.Types.EnvironmentFileType
import Network.AWS.ECS.Types.EphemeralStorage
import Network.AWS.ECS.Types.ExecuteCommandConfiguration
import Network.AWS.ECS.Types.ExecuteCommandLogConfiguration
import Network.AWS.ECS.Types.ExecuteCommandLogging
import Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig
import Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration
import Network.AWS.ECS.Types.Failure
import Network.AWS.ECS.Types.FirelensConfiguration
import Network.AWS.ECS.Types.FirelensConfigurationType
import Network.AWS.ECS.Types.HealthCheck
import Network.AWS.ECS.Types.HealthStatus
import Network.AWS.ECS.Types.HostEntry
import Network.AWS.ECS.Types.HostVolumeProperties
import Network.AWS.ECS.Types.InferenceAccelerator
import Network.AWS.ECS.Types.InferenceAcceleratorOverride
import Network.AWS.ECS.Types.IpcMode
import Network.AWS.ECS.Types.KernelCapabilities
import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.LinuxParameters
import Network.AWS.ECS.Types.LoadBalancer
import Network.AWS.ECS.Types.LogConfiguration
import Network.AWS.ECS.Types.LogDriver
import Network.AWS.ECS.Types.ManagedAgent
import Network.AWS.ECS.Types.ManagedAgentName
import Network.AWS.ECS.Types.ManagedAgentStateChange
import Network.AWS.ECS.Types.ManagedScaling
import Network.AWS.ECS.Types.ManagedScalingStatus
import Network.AWS.ECS.Types.ManagedTerminationProtection
import Network.AWS.ECS.Types.MountPoint
import Network.AWS.ECS.Types.NetworkBinding
import Network.AWS.ECS.Types.NetworkConfiguration
import Network.AWS.ECS.Types.NetworkInterface
import Network.AWS.ECS.Types.NetworkMode
import Network.AWS.ECS.Types.PidMode
import Network.AWS.ECS.Types.PlacementConstraint
import Network.AWS.ECS.Types.PlacementConstraintType
import Network.AWS.ECS.Types.PlacementStrategy
import Network.AWS.ECS.Types.PlacementStrategyType
import Network.AWS.ECS.Types.PlatformDevice
import Network.AWS.ECS.Types.PlatformDeviceType
import Network.AWS.ECS.Types.PortMapping
import Network.AWS.ECS.Types.PropagateTags
import Network.AWS.ECS.Types.ProxyConfiguration
import Network.AWS.ECS.Types.ProxyConfigurationType
import Network.AWS.ECS.Types.RepositoryCredentials
import Network.AWS.ECS.Types.Resource
import Network.AWS.ECS.Types.ResourceRequirement
import Network.AWS.ECS.Types.ResourceType
import Network.AWS.ECS.Types.Scale
import Network.AWS.ECS.Types.ScaleUnit
import Network.AWS.ECS.Types.SchedulingStrategy
import Network.AWS.ECS.Types.Scope
import Network.AWS.ECS.Types.Secret
import Network.AWS.ECS.Types.ServiceEvent
import Network.AWS.ECS.Types.ServiceField
import Network.AWS.ECS.Types.ServiceRegistry
import Network.AWS.ECS.Types.Session
import Network.AWS.ECS.Types.Setting
import Network.AWS.ECS.Types.SettingName
import Network.AWS.ECS.Types.SortOrder
import Network.AWS.ECS.Types.StabilityStatus
import Network.AWS.ECS.Types.SystemControl
import Network.AWS.ECS.Types.Tag
import Network.AWS.ECS.Types.TargetType
import Network.AWS.ECS.Types.Task
import Network.AWS.ECS.Types.TaskDefinition
import Network.AWS.ECS.Types.TaskDefinitionFamilyStatus
import Network.AWS.ECS.Types.TaskDefinitionField
import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraintType
import Network.AWS.ECS.Types.TaskDefinitionStatus
import Network.AWS.ECS.Types.TaskField
import Network.AWS.ECS.Types.TaskOverride
import Network.AWS.ECS.Types.TaskSet
import Network.AWS.ECS.Types.TaskSetField
import Network.AWS.ECS.Types.TaskStopCode
import Network.AWS.ECS.Types.Tmpfs
import Network.AWS.ECS.Types.TransportProtocol
import Network.AWS.ECS.Types.Ulimit
import Network.AWS.ECS.Types.UlimitName
import Network.AWS.ECS.Types.VersionInfo
import Network.AWS.ECS.Types.Volume
import Network.AWS.ECS.Types.VolumeFrom
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-11-13@ of the Amazon EC2 Container Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "ECS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "ecs",
      Core._serviceSigningName = "ecs",
      Core._serviceVersion = "2014-11-13",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "ECS",
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

-- | There is already a current Amazon ECS container agent update in progress
-- on the specified container instance. If the container agent becomes
-- disconnected while it is in a transitional stage, such as @PENDING@ or
-- @STAGING@, the update process can get stuck in that state. However, when
-- the agent reconnects, it resumes where it stopped previously.
_UpdateInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UpdateInProgressException =
  Core._MatchServiceError
    defaultService
    "UpdateInProgressException"

-- | The target container is not properly configured with the execute command
-- agent or the container is no longer active or running.
_TargetNotConnectedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetNotConnectedException =
  Core._MatchServiceError
    defaultService
    "TargetNotConnectedException"

-- | The specified platform version does not satisfy the task definition\'s
-- required capabilities.
_PlatformTaskDefinitionIncompatibilityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PlatformTaskDefinitionIncompatibilityException =
  Core._MatchServiceError
    defaultService
    "PlatformTaskDefinitionIncompatibilityException"

-- | The specified service could not be found. You can view your available
-- services with ListServices. Amazon ECS services are cluster-specific and
-- Region-specific.
_ServiceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ServiceNotFoundException"

-- | The specified task is not supported in this Region.
_UnsupportedFeatureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedFeatureException =
  Core._MatchServiceError
    defaultService
    "UnsupportedFeatureException"

-- | The specified task set could not be found. You can view your available
-- task sets with DescribeTaskSets. Task sets are specific to each cluster,
-- service and Region.
_TaskSetNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TaskSetNotFoundException =
  Core._MatchServiceError
    defaultService
    "TaskSetNotFoundException"

-- | You cannot delete a cluster that has registered container instances.
-- First, deregister the container instances before you can delete the
-- cluster. For more information, see DeregisterContainerInstance.
_ClusterContainsContainerInstancesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterContainsContainerInstancesException =
  Core._MatchServiceError
    defaultService
    "ClusterContainsContainerInstancesException"

-- | You cannot delete a cluster that contains services. First, update the
-- service to reduce its desired task count to 0 and then delete the
-- service. For more information, see UpdateService and DeleteService.
_ClusterContainsServicesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterContainsServicesException =
  Core._MatchServiceError
    defaultService
    "ClusterContainsServicesException"

-- | The specified platform version does not exist.
_PlatformUnknownException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PlatformUnknownException =
  Core._MatchServiceError
    defaultService
    "PlatformUnknownException"

-- | The specified parameter is invalid. Review the available parameters for
-- the API request.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | Your Amazon Web Services account has been blocked. For more information,
-- contact <http://aws.amazon.com/contact-us/ Amazon Web Services Support>.
_BlockedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BlockedException =
  Core._MatchServiceError
    defaultService
    "BlockedException"

-- | You do not have authorization to perform the requested action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Amazon ECS is unable to determine the current version of the Amazon ECS
-- container agent on the container instance and does not have enough
-- information to proceed with an update. This could be because the agent
-- running on the container instance is an older or custom version that
-- does not use our version information.
_MissingVersionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingVersionException =
  Core._MatchServiceError
    defaultService
    "MissingVersionException"

-- | The limit for the resource has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified resource is in-use and cannot be removed.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The specified cluster could not be found. You can view your available
-- clusters with ListClusters. Amazon ECS clusters are Region-specific.
_ClusterNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterNotFoundException =
  Core._MatchServiceError
    defaultService
    "ClusterNotFoundException"

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | These errors are usually caused by a client action, such as using an
-- action or resource on behalf of a user that doesn\'t have permissions to
-- use the action or resource, or specifying an identifier that is not
-- valid.
_ClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClientException =
  Core._MatchServiceError
    defaultService
    "ClientException"

-- | There is no update available for this Amazon ECS container agent. This
-- could be because the agent is already running the latest version, or it
-- is so old that there is no update path to the current version.
_NoUpdateAvailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoUpdateAvailableException =
  Core._MatchServiceError
    defaultService
    "NoUpdateAvailableException"

-- | The specified service is not active. You can\'t update a service that is
-- inactive. If you have previously deleted a service, you can re-create it
-- with CreateService.
_ServiceNotActiveException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceNotActiveException =
  Core._MatchServiceError
    defaultService
    "ServiceNotActiveException"

-- | You cannot delete a cluster that has active tasks.
_ClusterContainsTasksException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterContainsTasksException =
  Core._MatchServiceError
    defaultService
    "ClusterContainsTasksException"

-- | You can apply up to 10 custom attributes per resource. You can view the
-- attributes of a resource with ListAttributes. You can remove existing
-- attributes on a resource with DeleteAttributes.
_AttributeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AttributeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AttributeLimitExceededException"

-- | The specified target could not be found. You can view your available
-- container instances with ListContainerInstances. Amazon ECS container
-- instances are cluster-specific and Region-specific.
_TargetNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetNotFoundException =
  Core._MatchServiceError
    defaultService
    "TargetNotFoundException"

-- | These errors are usually caused by a server issue.
_ServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServerException =
  Core._MatchServiceError
    defaultService
    "ServerException"
