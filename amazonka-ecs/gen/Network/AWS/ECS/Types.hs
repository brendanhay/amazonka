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
    _ServiceNotFoundException,
    _PlatformTaskDefinitionIncompatibilityException,
    _UnsupportedFeatureException,
    _ClusterContainsContainerInstancesException,
    _TaskSetNotFoundException,
    _ClusterContainsServicesException,
    _PlatformUnknownException,
    _BlockedException,
    _InvalidParameterException,
    _AccessDeniedException,
    _ResourceInUseException,
    _LimitExceededException,
    _MissingVersionException,
    _ClientException,
    _ClusterNotFoundException,
    _NoUpdateAvailableException,
    _ResourceNotFoundException,
    _ServiceNotActiveException,
    _ClusterContainsTasksException,
    _TargetNotFoundException,
    _AttributeLimitExceededException,
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
    cluster_statistics,
    cluster_defaultCapacityProviderStrategy,
    cluster_pendingTasksCount,
    cluster_tags,
    cluster_attachmentsStatus,
    cluster_capacityProviders,
    cluster_clusterName,
    cluster_settings,
    cluster_attachments,
    cluster_runningTasksCount,

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
    container_memory,
    container_memoryReservation,
    container_runtimeId,
    container_exitCode,
    container_containerArn,
    container_name,
    container_image,
    container_networkBindings,
    container_reason,
    container_cpu,
    container_lastStatus,
    container_taskArn,
    container_healthStatus,
    container_networkInterfaces,

    -- * ContainerDefinition
    ContainerDefinition (..),
    newContainerDefinition,
    containerDefinition_hostname,
    containerDefinition_dependsOn,
    containerDefinition_linuxParameters,
    containerDefinition_firelensConfiguration,
    containerDefinition_memory,
    containerDefinition_user,
    containerDefinition_memoryReservation,
    containerDefinition_dockerLabels,
    containerDefinition_extraHosts,
    containerDefinition_systemControls,
    containerDefinition_privileged,
    containerDefinition_links,
    containerDefinition_environmentFiles,
    containerDefinition_interactive,
    containerDefinition_workingDirectory,
    containerDefinition_entryPoint,
    containerDefinition_environment,
    containerDefinition_volumesFrom,
    containerDefinition_secrets,
    containerDefinition_mountPoints,
    containerDefinition_dnsServers,
    containerDefinition_name,
    containerDefinition_image,
    containerDefinition_command,
    containerDefinition_logConfiguration,
    containerDefinition_pseudoTerminal,
    containerDefinition_dnsSearchDomains,
    containerDefinition_essential,
    containerDefinition_portMappings,
    containerDefinition_cpu,
    containerDefinition_resourceRequirements,
    containerDefinition_startTimeout,
    containerDefinition_readonlyRootFilesystem,
    containerDefinition_ulimits,
    containerDefinition_stopTimeout,
    containerDefinition_dockerSecurityOptions,
    containerDefinition_disableNetworking,
    containerDefinition_healthCheck,
    containerDefinition_repositoryCredentials,

    -- * ContainerDependency
    ContainerDependency (..),
    newContainerDependency,
    containerDependency_containerName,
    containerDependency_condition,

    -- * ContainerInstance
    ContainerInstance (..),
    newContainerInstance,
    containerInstance_versionInfo,
    containerInstance_status,
    containerInstance_agentUpdateStatus,
    containerInstance_registeredResources,
    containerInstance_containerInstanceArn,
    containerInstance_registeredAt,
    containerInstance_version,
    containerInstance_pendingTasksCount,
    containerInstance_attributes,
    containerInstance_tags,
    containerInstance_agentConnected,
    containerInstance_statusReason,
    containerInstance_ec2InstanceId,
    containerInstance_remainingResources,
    containerInstance_capacityProviderName,
    containerInstance_attachments,
    containerInstance_runningTasksCount,

    -- * ContainerOverride
    ContainerOverride (..),
    newContainerOverride,
    containerOverride_memory,
    containerOverride_memoryReservation,
    containerOverride_environmentFiles,
    containerOverride_environment,
    containerOverride_name,
    containerOverride_command,
    containerOverride_cpu,
    containerOverride_resourceRequirements,

    -- * ContainerService
    ContainerService (..),
    newContainerService,
    containerService_clusterArn,
    containerService_taskSets,
    containerService_status,
    containerService_runningCount,
    containerService_roleArn,
    containerService_deploymentConfiguration,
    containerService_networkConfiguration,
    containerService_capacityProviderStrategy,
    containerService_desiredCount,
    containerService_enableECSManagedTags,
    containerService_launchType,
    containerService_createdAt,
    containerService_platformVersion,
    containerService_deploymentController,
    containerService_serviceName,
    containerService_placementStrategy,
    containerService_deployments,
    containerService_placementConstraints,
    containerService_events,
    containerService_pendingCount,
    containerService_loadBalancers,
    containerService_tags,
    containerService_serviceRegistries,
    containerService_healthCheckGracePeriodSeconds,
    containerService_schedulingStrategy,
    containerService_createdBy,
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
    deployment_status,
    deployment_runningCount,
    deployment_networkConfiguration,
    deployment_capacityProviderStrategy,
    deployment_desiredCount,
    deployment_updatedAt,
    deployment_launchType,
    deployment_createdAt,
    deployment_platformVersion,
    deployment_id,
    deployment_pendingCount,
    deployment_rolloutStateReason,
    deployment_taskDefinition,
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
    dockerVolumeConfiguration_driver,
    dockerVolumeConfiguration_driverOpts,
    dockerVolumeConfiguration_autoprovision,

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
    serviceEvent_createdAt,
    serviceEvent_id,

    -- * ServiceRegistry
    ServiceRegistry (..),
    newServiceRegistry,
    serviceRegistry_port,
    serviceRegistry_containerPort,
    serviceRegistry_containerName,
    serviceRegistry_registryArn,

    -- * Setting
    Setting (..),
    newSetting,
    setting_name,
    setting_principalArn,
    setting_value,

    -- * SystemControl
    SystemControl (..),
    newSystemControl,
    systemControl_namespace,
    systemControl_value,

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
    task_connectivity,
    task_createdAt,
    task_platformVersion,
    task_stoppingAt,
    task_version,
    task_startedBy,
    task_inferenceAccelerators,
    task_group,
    task_attributes,
    task_availabilityZone,
    task_overrides,
    task_desiredStatus,
    task_stoppedAt,
    task_containers,
    task_tags,
    task_pullStoppedAt,
    task_executionStoppedAt,
    task_cpu,
    task_lastStatus,
    task_taskArn,
    task_connectivityAt,
    task_healthStatus,
    task_stopCode,
    task_taskDefinitionArn,
    task_stoppedReason,
    task_capacityProviderName,
    task_attachments,

    -- * TaskDefinition
    TaskDefinition (..),
    newTaskDefinition,
    taskDefinition_status,
    taskDefinition_taskRoleArn,
    taskDefinition_memory,
    taskDefinition_containerDefinitions,
    taskDefinition_pidMode,
    taskDefinition_requiresCompatibilities,
    taskDefinition_executionRoleArn,
    taskDefinition_volumes,
    taskDefinition_compatibilities,
    taskDefinition_registeredAt,
    taskDefinition_inferenceAccelerators,
    taskDefinition_placementConstraints,
    taskDefinition_proxyConfiguration,
    taskDefinition_deregisteredAt,
    taskDefinition_requiresAttributes,
    taskDefinition_ipcMode,
    taskDefinition_cpu,
    taskDefinition_family,
    taskDefinition_revision,
    taskDefinition_registeredBy,
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
    taskOverride_executionRoleArn,
    taskOverride_inferenceAcceleratorOverrides,
    taskOverride_containerOverrides,
    taskOverride_cpu,

    -- * TaskSet
    TaskSet (..),
    newTaskSet,
    taskSet_clusterArn,
    taskSet_status,
    taskSet_stabilityStatusAt,
    taskSet_runningCount,
    taskSet_stabilityStatus,
    taskSet_networkConfiguration,
    taskSet_capacityProviderStrategy,
    taskSet_updatedAt,
    taskSet_launchType,
    taskSet_createdAt,
    taskSet_platformVersion,
    taskSet_id,
    taskSet_startedBy,
    taskSet_computedDesiredCount,
    taskSet_pendingCount,
    taskSet_loadBalancers,
    taskSet_tags,
    taskSet_serviceRegistries,
    taskSet_taskDefinition,
    taskSet_serviceArn,
    taskSet_externalId,
    taskSet_scale,
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
    volume_efsVolumeConfiguration,
    volume_host,

    -- * VolumeFrom
    VolumeFrom (..),
    newVolumeFrom,
    volumeFrom_readOnly,
    volumeFrom_sourceContainer,
  )
where

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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "ECS",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "ecs",
      Prelude._svcVersion = "2014-11-13",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "ECS",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | There is already a current Amazon ECS container agent update in progress
-- on the specified container instance. If the container agent becomes
-- disconnected while it is in a transitional stage, such as @PENDING@ or
-- @STAGING@, the update process can get stuck in that state. However, when
-- the agent reconnects, it resumes where it stopped previously.
_UpdateInProgressException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UpdateInProgressException =
  Prelude._MatchServiceError
    defaultService
    "UpdateInProgressException"

-- | The specified service could not be found. You can view your available
-- services with ListServices. Amazon ECS services are cluster-specific and
-- Region-specific.
_ServiceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ServiceNotFoundException"

-- | The specified platform version does not satisfy the task definition\'s
-- required capabilities.
_PlatformTaskDefinitionIncompatibilityException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PlatformTaskDefinitionIncompatibilityException =
  Prelude._MatchServiceError
    defaultService
    "PlatformTaskDefinitionIncompatibilityException"

-- | The specified task is not supported in this Region.
_UnsupportedFeatureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedFeatureException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedFeatureException"

-- | You cannot delete a cluster that has registered container instances.
-- First, deregister the container instances before you can delete the
-- cluster. For more information, see DeregisterContainerInstance.
_ClusterContainsContainerInstancesException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterContainsContainerInstancesException =
  Prelude._MatchServiceError
    defaultService
    "ClusterContainsContainerInstancesException"

-- | The specified task set could not be found. You can view your available
-- task sets with DescribeTaskSets. Task sets are specific to each cluster,
-- service and Region.
_TaskSetNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TaskSetNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "TaskSetNotFoundException"

-- | You cannot delete a cluster that contains services. First, update the
-- service to reduce its desired task count to 0 and then delete the
-- service. For more information, see UpdateService and DeleteService.
_ClusterContainsServicesException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterContainsServicesException =
  Prelude._MatchServiceError
    defaultService
    "ClusterContainsServicesException"

-- | The specified platform version does not exist.
_PlatformUnknownException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PlatformUnknownException =
  Prelude._MatchServiceError
    defaultService
    "PlatformUnknownException"

-- | Your AWS account has been blocked. For more information, contact
-- <http://aws.amazon.com/contact-us/ AWS Support>.
_BlockedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BlockedException =
  Prelude._MatchServiceError
    defaultService
    "BlockedException"

-- | The specified parameter is invalid. Review the available parameters for
-- the API request.
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | You do not have authorization to perform the requested action.
_AccessDeniedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessDeniedException =
  Prelude._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The specified resource is in-use and cannot be removed.
_ResourceInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceInUseException =
  Prelude._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The limit for the resource has been exceeded.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Amazon ECS is unable to determine the current version of the Amazon ECS
-- container agent on the container instance and does not have enough
-- information to proceed with an update. This could be because the agent
-- running on the container instance is an older or custom version that
-- does not use our version information.
_MissingVersionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MissingVersionException =
  Prelude._MatchServiceError
    defaultService
    "MissingVersionException"

-- | These errors are usually caused by a client action, such as using an
-- action or resource on behalf of a user that doesn\'t have permissions to
-- use the action or resource, or specifying an identifier that is not
-- valid.
_ClientException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClientException =
  Prelude._MatchServiceError
    defaultService
    "ClientException"

-- | The specified cluster could not be found. You can view your available
-- clusters with ListClusters. Amazon ECS clusters are Region-specific.
_ClusterNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ClusterNotFoundException"

-- | There is no update available for this Amazon ECS container agent. This
-- could be because the agent is already running the latest version, or it
-- is so old that there is no update path to the current version.
_NoUpdateAvailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoUpdateAvailableException =
  Prelude._MatchServiceError
    defaultService
    "NoUpdateAvailableException"

-- | The specified resource could not be found.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The specified service is not active. You can\'t update a service that is
-- inactive. If you have previously deleted a service, you can re-create it
-- with CreateService.
_ServiceNotActiveException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceNotActiveException =
  Prelude._MatchServiceError
    defaultService
    "ServiceNotActiveException"

-- | You cannot delete a cluster that has active tasks.
_ClusterContainsTasksException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterContainsTasksException =
  Prelude._MatchServiceError
    defaultService
    "ClusterContainsTasksException"

-- | The specified target could not be found. You can view your available
-- container instances with ListContainerInstances. Amazon ECS container
-- instances are cluster-specific and Region-specific.
_TargetNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TargetNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "TargetNotFoundException"

-- | You can apply up to 10 custom attributes per resource. You can view the
-- attributes of a resource with ListAttributes. You can remove existing
-- attributes on a resource with DeleteAttributes.
_AttributeLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AttributeLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "AttributeLimitExceededException"

-- | These errors are usually caused by a server issue.
_ServerException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServerException =
  Prelude._MatchServiceError
    defaultService
    "ServerException"
