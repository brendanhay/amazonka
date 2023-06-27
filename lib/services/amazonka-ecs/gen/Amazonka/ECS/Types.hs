{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECS.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _AttributeLimitExceededException,
    _BlockedException,
    _ClientException,
    _ClusterContainsContainerInstancesException,
    _ClusterContainsServicesException,
    _ClusterContainsTasksException,
    _ClusterNotFoundException,
    _InvalidParameterException,
    _LimitExceededException,
    _MissingVersionException,
    _NamespaceNotFoundException,
    _NoUpdateAvailableException,
    _PlatformTaskDefinitionIncompatibilityException,
    _PlatformUnknownException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ServerException,
    _ServiceNotActiveException,
    _ServiceNotFoundException,
    _TargetNotConnectedException,
    _TargetNotFoundException,
    _TaskSetNotFoundException,
    _UnsupportedFeatureException,
    _UpdateInProgressException,

    -- * AgentUpdateStatus
    AgentUpdateStatus (..),

    -- * ApplicationProtocol
    ApplicationProtocol (..),

    -- * AssignPublicIp
    AssignPublicIp (..),

    -- * CPUArchitecture
    CPUArchitecture (..),

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

    -- * InstanceHealthCheckState
    InstanceHealthCheckState (..),

    -- * InstanceHealthCheckType
    InstanceHealthCheckType (..),

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

    -- * OSFamily
    OSFamily (..),

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
    attachment_details,
    attachment_id,
    attachment_status,
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
    capacityProvider_autoScalingGroupProvider,
    capacityProvider_capacityProviderArn,
    capacityProvider_name,
    capacityProvider_status,
    capacityProvider_tags,
    capacityProvider_updateStatus,
    capacityProvider_updateStatusReason,

    -- * CapacityProviderStrategyItem
    CapacityProviderStrategyItem (..),
    newCapacityProviderStrategyItem,
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_capacityProvider,

    -- * Cluster
    Cluster (..),
    newCluster,
    cluster_activeServicesCount,
    cluster_attachments,
    cluster_attachmentsStatus,
    cluster_capacityProviders,
    cluster_clusterArn,
    cluster_clusterName,
    cluster_configuration,
    cluster_defaultCapacityProviderStrategy,
    cluster_pendingTasksCount,
    cluster_registeredContainerInstancesCount,
    cluster_runningTasksCount,
    cluster_serviceConnectDefaults,
    cluster_settings,
    cluster_statistics,
    cluster_status,
    cluster_tags,

    -- * ClusterConfiguration
    ClusterConfiguration (..),
    newClusterConfiguration,
    clusterConfiguration_executeCommandConfiguration,

    -- * ClusterServiceConnectDefaults
    ClusterServiceConnectDefaults (..),
    newClusterServiceConnectDefaults,
    clusterServiceConnectDefaults_namespace,

    -- * ClusterServiceConnectDefaultsRequest
    ClusterServiceConnectDefaultsRequest (..),
    newClusterServiceConnectDefaultsRequest,
    clusterServiceConnectDefaultsRequest_namespace,

    -- * ClusterSetting
    ClusterSetting (..),
    newClusterSetting,
    clusterSetting_name,
    clusterSetting_value,

    -- * Container
    Container (..),
    newContainer,
    container_containerArn,
    container_cpu,
    container_exitCode,
    container_gpuIds,
    container_healthStatus,
    container_image,
    container_imageDigest,
    container_lastStatus,
    container_managedAgents,
    container_memory,
    container_memoryReservation,
    container_name,
    container_networkBindings,
    container_networkInterfaces,
    container_reason,
    container_runtimeId,
    container_taskArn,

    -- * ContainerDefinition
    ContainerDefinition (..),
    newContainerDefinition,
    containerDefinition_command,
    containerDefinition_cpu,
    containerDefinition_dependsOn,
    containerDefinition_disableNetworking,
    containerDefinition_dnsSearchDomains,
    containerDefinition_dnsServers,
    containerDefinition_dockerLabels,
    containerDefinition_dockerSecurityOptions,
    containerDefinition_entryPoint,
    containerDefinition_environment,
    containerDefinition_environmentFiles,
    containerDefinition_essential,
    containerDefinition_extraHosts,
    containerDefinition_firelensConfiguration,
    containerDefinition_healthCheck,
    containerDefinition_hostname,
    containerDefinition_image,
    containerDefinition_interactive,
    containerDefinition_links,
    containerDefinition_linuxParameters,
    containerDefinition_logConfiguration,
    containerDefinition_memory,
    containerDefinition_memoryReservation,
    containerDefinition_mountPoints,
    containerDefinition_name,
    containerDefinition_portMappings,
    containerDefinition_privileged,
    containerDefinition_pseudoTerminal,
    containerDefinition_readonlyRootFilesystem,
    containerDefinition_repositoryCredentials,
    containerDefinition_resourceRequirements,
    containerDefinition_secrets,
    containerDefinition_startTimeout,
    containerDefinition_stopTimeout,
    containerDefinition_systemControls,
    containerDefinition_ulimits,
    containerDefinition_user,
    containerDefinition_volumesFrom,
    containerDefinition_workingDirectory,

    -- * ContainerDependency
    ContainerDependency (..),
    newContainerDependency,
    containerDependency_containerName,
    containerDependency_condition,

    -- * ContainerInstance
    ContainerInstance (..),
    newContainerInstance,
    containerInstance_agentConnected,
    containerInstance_agentUpdateStatus,
    containerInstance_attachments,
    containerInstance_attributes,
    containerInstance_capacityProviderName,
    containerInstance_containerInstanceArn,
    containerInstance_ec2InstanceId,
    containerInstance_healthStatus,
    containerInstance_pendingTasksCount,
    containerInstance_registeredAt,
    containerInstance_registeredResources,
    containerInstance_remainingResources,
    containerInstance_runningTasksCount,
    containerInstance_status,
    containerInstance_statusReason,
    containerInstance_tags,
    containerInstance_version,
    containerInstance_versionInfo,

    -- * ContainerInstanceHealthStatus
    ContainerInstanceHealthStatus (..),
    newContainerInstanceHealthStatus,
    containerInstanceHealthStatus_details,
    containerInstanceHealthStatus_overallStatus,

    -- * ContainerOverride
    ContainerOverride (..),
    newContainerOverride,
    containerOverride_command,
    containerOverride_cpu,
    containerOverride_environment,
    containerOverride_environmentFiles,
    containerOverride_memory,
    containerOverride_memoryReservation,
    containerOverride_name,
    containerOverride_resourceRequirements,

    -- * ContainerService
    ContainerService (..),
    newContainerService,
    containerService_capacityProviderStrategy,
    containerService_clusterArn,
    containerService_createdAt,
    containerService_createdBy,
    containerService_deploymentConfiguration,
    containerService_deploymentController,
    containerService_deployments,
    containerService_desiredCount,
    containerService_enableECSManagedTags,
    containerService_enableExecuteCommand,
    containerService_events,
    containerService_healthCheckGracePeriodSeconds,
    containerService_launchType,
    containerService_loadBalancers,
    containerService_networkConfiguration,
    containerService_pendingCount,
    containerService_placementConstraints,
    containerService_placementStrategy,
    containerService_platformFamily,
    containerService_platformVersion,
    containerService_propagateTags,
    containerService_roleArn,
    containerService_runningCount,
    containerService_schedulingStrategy,
    containerService_serviceArn,
    containerService_serviceName,
    containerService_serviceRegistries,
    containerService_status,
    containerService_tags,
    containerService_taskDefinition,
    containerService_taskSets,

    -- * ContainerStateChange
    ContainerStateChange (..),
    newContainerStateChange,
    containerStateChange_containerName,
    containerStateChange_exitCode,
    containerStateChange_imageDigest,
    containerStateChange_networkBindings,
    containerStateChange_reason,
    containerStateChange_runtimeId,
    containerStateChange_status,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_capacityProviderStrategy,
    deployment_createdAt,
    deployment_desiredCount,
    deployment_failedTasks,
    deployment_id,
    deployment_launchType,
    deployment_networkConfiguration,
    deployment_pendingCount,
    deployment_platformFamily,
    deployment_platformVersion,
    deployment_rolloutState,
    deployment_rolloutStateReason,
    deployment_runningCount,
    deployment_serviceConnectConfiguration,
    deployment_serviceConnectResources,
    deployment_status,
    deployment_taskDefinition,
    deployment_updatedAt,

    -- * DeploymentAlarms
    DeploymentAlarms (..),
    newDeploymentAlarms,
    deploymentAlarms_alarmNames,
    deploymentAlarms_enable,
    deploymentAlarms_rollback,

    -- * DeploymentCircuitBreaker
    DeploymentCircuitBreaker (..),
    newDeploymentCircuitBreaker,
    deploymentCircuitBreaker_enable,
    deploymentCircuitBreaker_rollback,

    -- * DeploymentConfiguration
    DeploymentConfiguration (..),
    newDeploymentConfiguration,
    deploymentConfiguration_alarms,
    deploymentConfiguration_deploymentCircuitBreaker,
    deploymentConfiguration_maximumPercent,
    deploymentConfiguration_minimumHealthyPercent,

    -- * DeploymentController
    DeploymentController (..),
    newDeploymentController,
    deploymentController_type,

    -- * Device
    Device (..),
    newDevice,
    device_containerPath,
    device_permissions,
    device_hostPath,

    -- * DockerVolumeConfiguration
    DockerVolumeConfiguration (..),
    newDockerVolumeConfiguration,
    dockerVolumeConfiguration_autoprovision,
    dockerVolumeConfiguration_driver,
    dockerVolumeConfiguration_driverOpts,
    dockerVolumeConfiguration_labels,
    dockerVolumeConfiguration_scope,

    -- * EFSAuthorizationConfig
    EFSAuthorizationConfig (..),
    newEFSAuthorizationConfig,
    eFSAuthorizationConfig_accessPointId,
    eFSAuthorizationConfig_iam,

    -- * EFSVolumeConfiguration
    EFSVolumeConfiguration (..),
    newEFSVolumeConfiguration,
    eFSVolumeConfiguration_authorizationConfig,
    eFSVolumeConfiguration_rootDirectory,
    eFSVolumeConfiguration_transitEncryption,
    eFSVolumeConfiguration_transitEncryptionPort,
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
    executeCommandConfiguration_kmsKeyId,
    executeCommandConfiguration_logConfiguration,
    executeCommandConfiguration_logging,

    -- * ExecuteCommandLogConfiguration
    ExecuteCommandLogConfiguration (..),
    newExecuteCommandLogConfiguration,
    executeCommandLogConfiguration_cloudWatchEncryptionEnabled,
    executeCommandLogConfiguration_cloudWatchLogGroupName,
    executeCommandLogConfiguration_s3BucketName,
    executeCommandLogConfiguration_s3EncryptionEnabled,
    executeCommandLogConfiguration_s3KeyPrefix,

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
    failure_detail,
    failure_reason,

    -- * FirelensConfiguration
    FirelensConfiguration (..),
    newFirelensConfiguration,
    firelensConfiguration_options,
    firelensConfiguration_type,

    -- * HealthCheck
    HealthCheck (..),
    newHealthCheck,
    healthCheck_interval,
    healthCheck_retries,
    healthCheck_startPeriod,
    healthCheck_timeout,
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

    -- * InstanceHealthCheckResult
    InstanceHealthCheckResult (..),
    newInstanceHealthCheckResult,
    instanceHealthCheckResult_lastStatusChange,
    instanceHealthCheckResult_lastUpdated,
    instanceHealthCheckResult_status,
    instanceHealthCheckResult_type,

    -- * KernelCapabilities
    KernelCapabilities (..),
    newKernelCapabilities,
    kernelCapabilities_add,
    kernelCapabilities_drop,

    -- * KeyValuePair
    KeyValuePair (..),
    newKeyValuePair,
    keyValuePair_name,
    keyValuePair_value,

    -- * LinuxParameters
    LinuxParameters (..),
    newLinuxParameters,
    linuxParameters_capabilities,
    linuxParameters_devices,
    linuxParameters_initProcessEnabled,
    linuxParameters_maxSwap,
    linuxParameters_sharedMemorySize,
    linuxParameters_swappiness,
    linuxParameters_tmpfs,

    -- * LoadBalancer
    LoadBalancer (..),
    newLoadBalancer,
    loadBalancer_containerName,
    loadBalancer_containerPort,
    loadBalancer_loadBalancerName,
    loadBalancer_targetGroupArn,

    -- * LogConfiguration
    LogConfiguration (..),
    newLogConfiguration,
    logConfiguration_options,
    logConfiguration_secretOptions,
    logConfiguration_logDriver,

    -- * ManagedAgent
    ManagedAgent (..),
    newManagedAgent,
    managedAgent_lastStartedAt,
    managedAgent_lastStatus,
    managedAgent_name,
    managedAgent_reason,

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
    managedScaling_instanceWarmupPeriod,
    managedScaling_maximumScalingStepSize,
    managedScaling_minimumScalingStepSize,
    managedScaling_status,
    managedScaling_targetCapacity,

    -- * MountPoint
    MountPoint (..),
    newMountPoint,
    mountPoint_containerPath,
    mountPoint_readOnly,
    mountPoint_sourceVolume,

    -- * NetworkBinding
    NetworkBinding (..),
    newNetworkBinding,
    networkBinding_bindIP,
    networkBinding_containerPort,
    networkBinding_containerPortRange,
    networkBinding_hostPort,
    networkBinding_hostPortRange,
    networkBinding_protocol,

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    newNetworkConfiguration,
    networkConfiguration_awsvpcConfiguration,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_attachmentId,
    networkInterface_ipv6Address,
    networkInterface_privateIpv4Address,

    -- * PlacementConstraint
    PlacementConstraint (..),
    newPlacementConstraint,
    placementConstraint_expression,
    placementConstraint_type,

    -- * PlacementStrategy
    PlacementStrategy (..),
    newPlacementStrategy,
    placementStrategy_field,
    placementStrategy_type,

    -- * PlatformDevice
    PlatformDevice (..),
    newPlatformDevice,
    platformDevice_id,
    platformDevice_type,

    -- * PortMapping
    PortMapping (..),
    newPortMapping,
    portMapping_appProtocol,
    portMapping_containerPort,
    portMapping_containerPortRange,
    portMapping_hostPort,
    portMapping_name,
    portMapping_protocol,

    -- * ProtectedTask
    ProtectedTask (..),
    newProtectedTask,
    protectedTask_expirationDate,
    protectedTask_protectionEnabled,
    protectedTask_taskArn,

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
    resource_doubleValue,
    resource_integerValue,
    resource_longValue,
    resource_name,
    resource_stringSetValue,
    resource_type,

    -- * ResourceRequirement
    ResourceRequirement (..),
    newResourceRequirement,
    resourceRequirement_value,
    resourceRequirement_type,

    -- * RuntimePlatform
    RuntimePlatform (..),
    newRuntimePlatform,
    runtimePlatform_cpuArchitecture,
    runtimePlatform_operatingSystemFamily,

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

    -- * ServiceConnectClientAlias
    ServiceConnectClientAlias (..),
    newServiceConnectClientAlias,
    serviceConnectClientAlias_dnsName,
    serviceConnectClientAlias_port,

    -- * ServiceConnectConfiguration
    ServiceConnectConfiguration (..),
    newServiceConnectConfiguration,
    serviceConnectConfiguration_logConfiguration,
    serviceConnectConfiguration_namespace,
    serviceConnectConfiguration_services,
    serviceConnectConfiguration_enabled,

    -- * ServiceConnectService
    ServiceConnectService (..),
    newServiceConnectService,
    serviceConnectService_clientAliases,
    serviceConnectService_discoveryName,
    serviceConnectService_ingressPortOverride,
    serviceConnectService_portName,

    -- * ServiceConnectServiceResource
    ServiceConnectServiceResource (..),
    newServiceConnectServiceResource,
    serviceConnectServiceResource_discoveryArn,
    serviceConnectServiceResource_discoveryName,

    -- * ServiceEvent
    ServiceEvent (..),
    newServiceEvent,
    serviceEvent_createdAt,
    serviceEvent_id,
    serviceEvent_message,

    -- * ServiceRegistry
    ServiceRegistry (..),
    newServiceRegistry,
    serviceRegistry_containerName,
    serviceRegistry_containerPort,
    serviceRegistry_port,
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
    task_attachments,
    task_attributes,
    task_availabilityZone,
    task_capacityProviderName,
    task_clusterArn,
    task_connectivity,
    task_connectivityAt,
    task_containerInstanceArn,
    task_containers,
    task_cpu,
    task_createdAt,
    task_desiredStatus,
    task_enableExecuteCommand,
    task_ephemeralStorage,
    task_executionStoppedAt,
    task_group,
    task_healthStatus,
    task_inferenceAccelerators,
    task_lastStatus,
    task_launchType,
    task_memory,
    task_overrides,
    task_platformFamily,
    task_platformVersion,
    task_pullStartedAt,
    task_pullStoppedAt,
    task_startedAt,
    task_startedBy,
    task_stopCode,
    task_stoppedAt,
    task_stoppedReason,
    task_stoppingAt,
    task_tags,
    task_taskArn,
    task_taskDefinitionArn,
    task_version,

    -- * TaskDefinition
    TaskDefinition (..),
    newTaskDefinition,
    taskDefinition_compatibilities,
    taskDefinition_containerDefinitions,
    taskDefinition_cpu,
    taskDefinition_deregisteredAt,
    taskDefinition_ephemeralStorage,
    taskDefinition_executionRoleArn,
    taskDefinition_family,
    taskDefinition_inferenceAccelerators,
    taskDefinition_ipcMode,
    taskDefinition_memory,
    taskDefinition_networkMode,
    taskDefinition_pidMode,
    taskDefinition_placementConstraints,
    taskDefinition_proxyConfiguration,
    taskDefinition_registeredAt,
    taskDefinition_registeredBy,
    taskDefinition_requiresAttributes,
    taskDefinition_requiresCompatibilities,
    taskDefinition_revision,
    taskDefinition_runtimePlatform,
    taskDefinition_status,
    taskDefinition_taskDefinitionArn,
    taskDefinition_taskRoleArn,
    taskDefinition_volumes,

    -- * TaskDefinitionPlacementConstraint
    TaskDefinitionPlacementConstraint (..),
    newTaskDefinitionPlacementConstraint,
    taskDefinitionPlacementConstraint_expression,
    taskDefinitionPlacementConstraint_type,

    -- * TaskOverride
    TaskOverride (..),
    newTaskOverride,
    taskOverride_containerOverrides,
    taskOverride_cpu,
    taskOverride_ephemeralStorage,
    taskOverride_executionRoleArn,
    taskOverride_inferenceAcceleratorOverrides,
    taskOverride_memory,
    taskOverride_taskRoleArn,

    -- * TaskSet
    TaskSet (..),
    newTaskSet,
    taskSet_capacityProviderStrategy,
    taskSet_clusterArn,
    taskSet_computedDesiredCount,
    taskSet_createdAt,
    taskSet_externalId,
    taskSet_id,
    taskSet_launchType,
    taskSet_loadBalancers,
    taskSet_networkConfiguration,
    taskSet_pendingCount,
    taskSet_platformFamily,
    taskSet_platformVersion,
    taskSet_runningCount,
    taskSet_scale,
    taskSet_serviceArn,
    taskSet_serviceRegistries,
    taskSet_stabilityStatus,
    taskSet_stabilityStatusAt,
    taskSet_startedBy,
    taskSet_status,
    taskSet_tags,
    taskSet_taskDefinition,
    taskSet_taskSetArn,
    taskSet_updatedAt,

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
    versionInfo_agentHash,
    versionInfo_agentVersion,
    versionInfo_dockerVersion,

    -- * Volume
    Volume (..),
    newVolume,
    volume_dockerVolumeConfiguration,
    volume_efsVolumeConfiguration,
    volume_fsxWindowsFileServerVolumeConfiguration,
    volume_host,
    volume_name,

    -- * VolumeFrom
    VolumeFrom (..),
    newVolumeFrom,
    volumeFrom_readOnly,
    volumeFrom_sourceContainer,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECS.Types.AgentUpdateStatus
import Amazonka.ECS.Types.ApplicationProtocol
import Amazonka.ECS.Types.AssignPublicIp
import Amazonka.ECS.Types.Attachment
import Amazonka.ECS.Types.AttachmentStateChange
import Amazonka.ECS.Types.Attribute
import Amazonka.ECS.Types.AutoScalingGroupProvider
import Amazonka.ECS.Types.AutoScalingGroupProviderUpdate
import Amazonka.ECS.Types.AwsVpcConfiguration
import Amazonka.ECS.Types.CPUArchitecture
import Amazonka.ECS.Types.CapacityProvider
import Amazonka.ECS.Types.CapacityProviderField
import Amazonka.ECS.Types.CapacityProviderStatus
import Amazonka.ECS.Types.CapacityProviderStrategyItem
import Amazonka.ECS.Types.CapacityProviderUpdateStatus
import Amazonka.ECS.Types.Cluster
import Amazonka.ECS.Types.ClusterConfiguration
import Amazonka.ECS.Types.ClusterField
import Amazonka.ECS.Types.ClusterServiceConnectDefaults
import Amazonka.ECS.Types.ClusterServiceConnectDefaultsRequest
import Amazonka.ECS.Types.ClusterSetting
import Amazonka.ECS.Types.ClusterSettingName
import Amazonka.ECS.Types.Compatibility
import Amazonka.ECS.Types.Connectivity
import Amazonka.ECS.Types.Container
import Amazonka.ECS.Types.ContainerCondition
import Amazonka.ECS.Types.ContainerDefinition
import Amazonka.ECS.Types.ContainerDependency
import Amazonka.ECS.Types.ContainerInstance
import Amazonka.ECS.Types.ContainerInstanceField
import Amazonka.ECS.Types.ContainerInstanceHealthStatus
import Amazonka.ECS.Types.ContainerInstanceStatus
import Amazonka.ECS.Types.ContainerOverride
import Amazonka.ECS.Types.ContainerService
import Amazonka.ECS.Types.ContainerStateChange
import Amazonka.ECS.Types.Deployment
import Amazonka.ECS.Types.DeploymentAlarms
import Amazonka.ECS.Types.DeploymentCircuitBreaker
import Amazonka.ECS.Types.DeploymentConfiguration
import Amazonka.ECS.Types.DeploymentController
import Amazonka.ECS.Types.DeploymentControllerType
import Amazonka.ECS.Types.DeploymentRolloutState
import Amazonka.ECS.Types.DesiredStatus
import Amazonka.ECS.Types.Device
import Amazonka.ECS.Types.DeviceCgroupPermission
import Amazonka.ECS.Types.DockerVolumeConfiguration
import Amazonka.ECS.Types.EFSAuthorizationConfig
import Amazonka.ECS.Types.EFSAuthorizationConfigIAM
import Amazonka.ECS.Types.EFSTransitEncryption
import Amazonka.ECS.Types.EFSVolumeConfiguration
import Amazonka.ECS.Types.EnvironmentFile
import Amazonka.ECS.Types.EnvironmentFileType
import Amazonka.ECS.Types.EphemeralStorage
import Amazonka.ECS.Types.ExecuteCommandConfiguration
import Amazonka.ECS.Types.ExecuteCommandLogConfiguration
import Amazonka.ECS.Types.ExecuteCommandLogging
import Amazonka.ECS.Types.FSxWindowsFileServerAuthorizationConfig
import Amazonka.ECS.Types.FSxWindowsFileServerVolumeConfiguration
import Amazonka.ECS.Types.Failure
import Amazonka.ECS.Types.FirelensConfiguration
import Amazonka.ECS.Types.FirelensConfigurationType
import Amazonka.ECS.Types.HealthCheck
import Amazonka.ECS.Types.HealthStatus
import Amazonka.ECS.Types.HostEntry
import Amazonka.ECS.Types.HostVolumeProperties
import Amazonka.ECS.Types.InferenceAccelerator
import Amazonka.ECS.Types.InferenceAcceleratorOverride
import Amazonka.ECS.Types.InstanceHealthCheckResult
import Amazonka.ECS.Types.InstanceHealthCheckState
import Amazonka.ECS.Types.InstanceHealthCheckType
import Amazonka.ECS.Types.IpcMode
import Amazonka.ECS.Types.KernelCapabilities
import Amazonka.ECS.Types.KeyValuePair
import Amazonka.ECS.Types.LaunchType
import Amazonka.ECS.Types.LinuxParameters
import Amazonka.ECS.Types.LoadBalancer
import Amazonka.ECS.Types.LogConfiguration
import Amazonka.ECS.Types.LogDriver
import Amazonka.ECS.Types.ManagedAgent
import Amazonka.ECS.Types.ManagedAgentName
import Amazonka.ECS.Types.ManagedAgentStateChange
import Amazonka.ECS.Types.ManagedScaling
import Amazonka.ECS.Types.ManagedScalingStatus
import Amazonka.ECS.Types.ManagedTerminationProtection
import Amazonka.ECS.Types.MountPoint
import Amazonka.ECS.Types.NetworkBinding
import Amazonka.ECS.Types.NetworkConfiguration
import Amazonka.ECS.Types.NetworkInterface
import Amazonka.ECS.Types.NetworkMode
import Amazonka.ECS.Types.OSFamily
import Amazonka.ECS.Types.PidMode
import Amazonka.ECS.Types.PlacementConstraint
import Amazonka.ECS.Types.PlacementConstraintType
import Amazonka.ECS.Types.PlacementStrategy
import Amazonka.ECS.Types.PlacementStrategyType
import Amazonka.ECS.Types.PlatformDevice
import Amazonka.ECS.Types.PlatformDeviceType
import Amazonka.ECS.Types.PortMapping
import Amazonka.ECS.Types.PropagateTags
import Amazonka.ECS.Types.ProtectedTask
import Amazonka.ECS.Types.ProxyConfiguration
import Amazonka.ECS.Types.ProxyConfigurationType
import Amazonka.ECS.Types.RepositoryCredentials
import Amazonka.ECS.Types.Resource
import Amazonka.ECS.Types.ResourceRequirement
import Amazonka.ECS.Types.ResourceType
import Amazonka.ECS.Types.RuntimePlatform
import Amazonka.ECS.Types.Scale
import Amazonka.ECS.Types.ScaleUnit
import Amazonka.ECS.Types.SchedulingStrategy
import Amazonka.ECS.Types.Scope
import Amazonka.ECS.Types.Secret
import Amazonka.ECS.Types.ServiceConnectClientAlias
import Amazonka.ECS.Types.ServiceConnectConfiguration
import Amazonka.ECS.Types.ServiceConnectService
import Amazonka.ECS.Types.ServiceConnectServiceResource
import Amazonka.ECS.Types.ServiceEvent
import Amazonka.ECS.Types.ServiceField
import Amazonka.ECS.Types.ServiceRegistry
import Amazonka.ECS.Types.Session
import Amazonka.ECS.Types.Setting
import Amazonka.ECS.Types.SettingName
import Amazonka.ECS.Types.SortOrder
import Amazonka.ECS.Types.StabilityStatus
import Amazonka.ECS.Types.SystemControl
import Amazonka.ECS.Types.Tag
import Amazonka.ECS.Types.TargetType
import Amazonka.ECS.Types.Task
import Amazonka.ECS.Types.TaskDefinition
import Amazonka.ECS.Types.TaskDefinitionFamilyStatus
import Amazonka.ECS.Types.TaskDefinitionField
import Amazonka.ECS.Types.TaskDefinitionPlacementConstraint
import Amazonka.ECS.Types.TaskDefinitionPlacementConstraintType
import Amazonka.ECS.Types.TaskDefinitionStatus
import Amazonka.ECS.Types.TaskField
import Amazonka.ECS.Types.TaskOverride
import Amazonka.ECS.Types.TaskSet
import Amazonka.ECS.Types.TaskSetField
import Amazonka.ECS.Types.TaskStopCode
import Amazonka.ECS.Types.Tmpfs
import Amazonka.ECS.Types.TransportProtocol
import Amazonka.ECS.Types.Ulimit
import Amazonka.ECS.Types.UlimitName
import Amazonka.ECS.Types.VersionInfo
import Amazonka.ECS.Types.Volume
import Amazonka.ECS.Types.VolumeFrom
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2014-11-13@ of the Amazon EC2 Container Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ECS",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ecs",
      Core.signingName = "ecs",
      Core.version = "2014-11-13",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ECS",
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

-- | You don\'t have authorization to perform the requested action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | You can apply up to 10 custom attributes for each resource. You can view
-- the attributes of a resource with ListAttributes. You can remove
-- existing attributes on a resource with DeleteAttributes.
_AttributeLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AttributeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AttributeLimitExceededException"

-- | Your Amazon Web Services account was blocked. For more information,
-- contact <http://aws.amazon.com/contact-us/ Amazon Web Services Support>.
_BlockedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BlockedException =
  Core._MatchServiceError
    defaultService
    "BlockedException"

-- | These errors are usually caused by a client action. This client action
-- might be using an action or resource on behalf of a user that doesn\'t
-- have permissions to use the action or resource,. Or, it might be
-- specifying an identifier that isn\'t valid.
_ClientException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClientException =
  Core._MatchServiceError
    defaultService
    "ClientException"

-- | You can\'t delete a cluster that has registered container instances.
-- First, deregister the container instances before you can delete the
-- cluster. For more information, see DeregisterContainerInstance.
_ClusterContainsContainerInstancesException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterContainsContainerInstancesException =
  Core._MatchServiceError
    defaultService
    "ClusterContainsContainerInstancesException"

-- | You can\'t delete a cluster that contains services. First, update the
-- service to reduce its desired task count to 0, and then delete the
-- service. For more information, see UpdateService and DeleteService.
_ClusterContainsServicesException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterContainsServicesException =
  Core._MatchServiceError
    defaultService
    "ClusterContainsServicesException"

-- | You can\'t delete a cluster that has active tasks.
_ClusterContainsTasksException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterContainsTasksException =
  Core._MatchServiceError
    defaultService
    "ClusterContainsTasksException"

-- | The specified cluster wasn\'t found. You can view your available
-- clusters with ListClusters. Amazon ECS clusters are Region specific.
_ClusterNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterNotFoundException =
  Core._MatchServiceError
    defaultService
    "ClusterNotFoundException"

-- | The specified parameter isn\'t valid. Review the available parameters
-- for the API request.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The limit for the resource was exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Amazon ECS can\'t determine the current version of the Amazon ECS
-- container agent on the container instance and doesn\'t have enough
-- information to proceed with an update. This could be because the agent
-- running on the container instance is a previous or custom version that
-- doesn\'t use our version information.
_MissingVersionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MissingVersionException =
  Core._MatchServiceError
    defaultService
    "MissingVersionException"

-- | The specified namespace wasn\'t found.
_NamespaceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NamespaceNotFoundException =
  Core._MatchServiceError
    defaultService
    "NamespaceNotFoundException"

-- | There\'s no update available for this Amazon ECS container agent. This
-- might be because the agent is already running the latest version or
-- because it\'s so old that there\'s no update path to the current
-- version.
_NoUpdateAvailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoUpdateAvailableException =
  Core._MatchServiceError
    defaultService
    "NoUpdateAvailableException"

-- | The specified platform version doesn\'t satisfy the required
-- capabilities of the task definition.
_PlatformTaskDefinitionIncompatibilityException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PlatformTaskDefinitionIncompatibilityException =
  Core._MatchServiceError
    defaultService
    "PlatformTaskDefinitionIncompatibilityException"

-- | The specified platform version doesn\'t exist.
_PlatformUnknownException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PlatformUnknownException =
  Core._MatchServiceError
    defaultService
    "PlatformUnknownException"

-- | The specified resource is in-use and can\'t be removed.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The specified resource wasn\'t found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | These errors are usually caused by a server issue.
_ServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServerException =
  Core._MatchServiceError
    defaultService
    "ServerException"

-- | The specified service isn\'t active. You can\'t update a service that\'s
-- inactive. If you have previously deleted a service, you can re-create it
-- with CreateService.
_ServiceNotActiveException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceNotActiveException =
  Core._MatchServiceError
    defaultService
    "ServiceNotActiveException"

-- | The specified service wasn\'t found. You can view your available
-- services with ListServices. Amazon ECS services are cluster specific and
-- Region specific.
_ServiceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ServiceNotFoundException"

-- | The execute command cannot run. This error can be caused by any of the
-- following configuration issues:
--
-- -   Incorrect IAM permissions
--
-- -   The SSM agent is not installed or is not running
--
-- -   There is an interface Amazon VPC endpoint for Amazon ECS, but there
--     is not one for Systems Manager Session Manager
--
-- For information about how to troubleshoot the issues, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-exec.html Troubleshooting issues with ECS Exec>
-- in the /Amazon Elastic Container Service Developer Guide/.
_TargetNotConnectedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TargetNotConnectedException =
  Core._MatchServiceError
    defaultService
    "TargetNotConnectedException"

-- | The specified target wasn\'t found. You can view your available
-- container instances with ListContainerInstances. Amazon ECS container
-- instances are cluster-specific and Region-specific.
_TargetNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TargetNotFoundException =
  Core._MatchServiceError
    defaultService
    "TargetNotFoundException"

-- | The specified task set wasn\'t found. You can view your available task
-- sets with DescribeTaskSets. Task sets are specific to each cluster,
-- service and Region.
_TaskSetNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TaskSetNotFoundException =
  Core._MatchServiceError
    defaultService
    "TaskSetNotFoundException"

-- | The specified task isn\'t supported in this Region.
_UnsupportedFeatureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedFeatureException =
  Core._MatchServiceError
    defaultService
    "UnsupportedFeatureException"

-- | There\'s already a current Amazon ECS container agent update in progress
-- on the container instance that\'s specified. If the container agent
-- becomes disconnected while it\'s in a transitional stage, such as
-- @PENDING@ or @STAGING@, the update process can get stuck in that state.
-- However, when the agent reconnects, it resumes where it stopped
-- previously.
_UpdateInProgressException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UpdateInProgressException =
  Core._MatchServiceError
    defaultService
    "UpdateInProgressException"
