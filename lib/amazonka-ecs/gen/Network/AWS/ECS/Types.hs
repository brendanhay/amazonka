-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _AccessDeniedException
    , _InvalidParameterException
    , _ServerException
    , _ClusterContainsTasksException
    , _PlatformUnknownException
    , _ClusterContainsServicesException
    , _TaskSetNotFoundException
    , _ClusterContainsContainerInstancesException
    , _ServiceNotActiveException
    , _ClusterNotFoundException
    , _NoUpdateAvailableException
    , _UnsupportedFeatureException
    , _ServiceNotFoundException
    , _PlatformTaskDefinitionIncompatibilityException
    , _MissingVersionException
    , _UpdateInProgressException
    , _BlockedException
    , _TargetNotFoundException
    , _AttributeLimitExceededException
    , _ClientException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _ResourceInUseException

    -- * DesiredStatus
    , DesiredStatus (..)

    -- * TaskDefinitionPlacementConstraintType
    , TaskDefinitionPlacementConstraintType (..)

    -- * DockerVolumeConfiguration
    , DockerVolumeConfiguration (..)
    , mkDockerVolumeConfiguration
    , dvcAutoprovision
    , dvcDriver
    , dvcDriverOpts
    , dvcLabels
    , dvcScope

    -- * ProxyConfigurationType
    , ProxyConfigurationType (..)

    -- * Attribute
    , Attribute (..)
    , mkAttribute
    , aName
    , aTargetId
    , aTargetType
    , aValue

    -- * HostEntry
    , HostEntry (..)
    , mkHostEntry
    , heHostname
    , heIpAddress

    -- * ManagedScalingStatus
    , ManagedScalingStatus (..)

    -- * ContainerInstanceField
    , ContainerInstanceField (..)

    -- * PlacementConstraint
    , PlacementConstraint (..)
    , mkPlacementConstraint
    , pcExpression
    , pcType

    -- * FirelensConfigurationType
    , FirelensConfigurationType (..)

    -- * TaskField
    , TaskField (..)

    -- * UlimitName
    , UlimitName (..)

    -- * TaskSet
    , TaskSet (..)
    , mkTaskSet
    , tsCapacityProviderStrategy
    , tsClusterArn
    , tsComputedDesiredCount
    , tsCreatedAt
    , tsExternalId
    , tsId
    , tsLaunchType
    , tsLoadBalancers
    , tsNetworkConfiguration
    , tsPendingCount
    , tsPlatformVersion
    , tsRunningCount
    , tsScale
    , tsServiceArn
    , tsServiceRegistries
    , tsStabilityStatus
    , tsStabilityStatusAt
    , tsStartedBy
    , tsStatus
    , tsTags
    , tsTaskDefinition
    , tsTaskSetArn
    , tsUpdatedAt

    -- * NetworkBinding
    , NetworkBinding (..)
    , mkNetworkBinding
    , nbBindIP
    , nbContainerPort
    , nbHostPort
    , nbProtocol

    -- * PlatformDevice
    , PlatformDevice (..)
    , mkPlatformDevice
    , pdId
    , pdType

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * RepositoryCredentials
    , RepositoryCredentials (..)
    , mkRepositoryCredentials
    , rcCredentialsParameter

    -- * Cluster
    , Cluster (..)
    , mkCluster
    , cActiveServicesCount
    , cAttachments
    , cAttachmentsStatus
    , cCapacityProviders
    , cClusterArn
    , cClusterName
    , cDefaultCapacityProviderStrategy
    , cPendingTasksCount
    , cRegisteredContainerInstancesCount
    , cRunningTasksCount
    , cSettings
    , cStatistics
    , cStatus
    , cTags

    -- * PlacementStrategyType
    , PlacementStrategyType (..)

    -- * ResourceType
    , ResourceType (..)

    -- * ContainerInstanceStatus
    , ContainerInstanceStatus (..)

    -- * HealthCheck
    , HealthCheck (..)
    , mkHealthCheck
    , hcCommand
    , hcInterval
    , hcRetries
    , hcStartPeriod
    , hcTimeout

    -- * PropagateTags
    , PropagateTags (..)

    -- * TaskDefinitionFamilyStatus
    , TaskDefinitionFamilyStatus (..)

    -- * KernelCapabilities
    , KernelCapabilities (..)
    , mkKernelCapabilities
    , kcAdd
    , kcDrop

    -- * TransportProtocol
    , TransportProtocol (..)

    -- * ContainerService
    , ContainerService (..)
    , mkContainerService
    , csCapacityProviderStrategy
    , csClusterArn
    , csCreatedAt
    , csCreatedBy
    , csDeploymentConfiguration
    , csDeploymentController
    , csDeployments
    , csDesiredCount
    , csEnableECSManagedTags
    , csEvents
    , csHealthCheckGracePeriodSeconds
    , csLaunchType
    , csLoadBalancers
    , csNetworkConfiguration
    , csPendingCount
    , csPlacementConstraints
    , csPlacementStrategy
    , csPlatformVersion
    , csPropagateTags
    , csRoleArn
    , csRunningCount
    , csSchedulingStrategy
    , csServiceArn
    , csServiceName
    , csServiceRegistries
    , csStatus
    , csTags
    , csTaskDefinition
    , csTaskSets

    -- * DeploymentControllerType
    , DeploymentControllerType (..)

    -- * AutoScalingGroupProviderUpdate
    , AutoScalingGroupProviderUpdate (..)
    , mkAutoScalingGroupProviderUpdate
    , asgpuManagedScaling
    , asgpuManagedTerminationProtection

    -- * CapacityProvider
    , CapacityProvider (..)
    , mkCapacityProvider
    , cpAutoScalingGroupProvider
    , cpCapacityProviderArn
    , cpName
    , cpStatus
    , cpTags
    , cpUpdateStatus
    , cpUpdateStatusReason

    -- * EFSTransitEncryption
    , EFSTransitEncryption (..)

    -- * ClusterField
    , ClusterField (..)

    -- * ContainerDependency
    , ContainerDependency (..)
    , mkContainerDependency
    , cdContainerName
    , cdCondition

    -- * Device
    , Device (..)
    , mkDevice
    , dHostPath
    , dContainerPath
    , dPermissions

    -- * Scale
    , Scale (..)
    , mkScale
    , sUnit
    , sValue

    -- * Volume
    , Volume (..)
    , mkVolume
    , vDockerVolumeConfiguration
    , vEfsVolumeConfiguration
    , vFsxWindowsFileServerVolumeConfiguration
    , vHost
    , vName

    -- * AttachmentStateChange
    , AttachmentStateChange (..)
    , mkAttachmentStateChange
    , ascAttachmentArn
    , ascStatus

    -- * InferenceAcceleratorOverride
    , InferenceAcceleratorOverride (..)
    , mkInferenceAcceleratorOverride
    , iaoDeviceName
    , iaoDeviceType

    -- * ManagedScaling
    , ManagedScaling (..)
    , mkManagedScaling
    , msInstanceWarmupPeriod
    , msMaximumScalingStepSize
    , msMinimumScalingStepSize
    , msStatus
    , msTargetCapacity

    -- * ContainerOverride
    , ContainerOverride (..)
    , mkContainerOverride
    , coCommand
    , coCpu
    , coEnvironment
    , coEnvironmentFiles
    , coMemory
    , coMemoryReservation
    , coName
    , coResourceRequirements

    -- * NetworkInterface
    , NetworkInterface (..)
    , mkNetworkInterface
    , niAttachmentId
    , niIpv6Address
    , niPrivateIpv4Address

    -- * EnvironmentFile
    , EnvironmentFile (..)
    , mkEnvironmentFile
    , efValue
    , efType

    -- * ClusterSettingName
    , ClusterSettingName (..)

    -- * PidMode
    , PidMode (..)

    -- * KeyValuePair
    , KeyValuePair (..)
    , mkKeyValuePair
    , kvpName
    , kvpValue

    -- * Secret
    , Secret (..)
    , mkSecret
    , sName
    , sValueFrom

    -- * ContainerStateChange
    , ContainerStateChange (..)
    , mkContainerStateChange
    , cscContainerName
    , cscExitCode
    , cscImageDigest
    , cscNetworkBindings
    , cscReason
    , cscRuntimeId
    , cscStatus

    -- * VolumeFrom
    , VolumeFrom (..)
    , mkVolumeFrom
    , vfReadOnly
    , vfSourceContainer

    -- * Setting
    , Setting (..)
    , mkSetting
    , sfName
    , sfPrincipalArn
    , sfValue

    -- * FSxWindowsFileServerVolumeConfiguration
    , FSxWindowsFileServerVolumeConfiguration (..)
    , mkFSxWindowsFileServerVolumeConfiguration
    , fswfsvcFileSystemId
    , fswfsvcRootDirectory
    , fswfsvcAuthorizationConfig

    -- * EFSAuthorizationConfig
    , EFSAuthorizationConfig (..)
    , mkEFSAuthorizationConfig
    , efsacAccessPointId
    , efsacIam

    -- * AssignPublicIp
    , AssignPublicIp (..)

    -- * TaskOverride
    , TaskOverride (..)
    , mkTaskOverride
    , toContainerOverrides
    , toCpu
    , toExecutionRoleArn
    , toInferenceAcceleratorOverrides
    , toMemory
    , toTaskRoleArn

    -- * HostVolumeProperties
    , HostVolumeProperties (..)
    , mkHostVolumeProperties
    , hvpSourcePath

    -- * CapacityProviderStatus
    , CapacityProviderStatus (..)

    -- * TargetType
    , TargetType (..)

    -- * IpcMode
    , IpcMode (..)

    -- * SettingName
    , SettingName (..)

    -- * Attachment
    , Attachment (..)
    , mkAttachment
    , aDetails
    , aId
    , aStatus
    , aType

    -- * TaskDefinitionPlacementConstraint
    , TaskDefinitionPlacementConstraint (..)
    , mkTaskDefinitionPlacementConstraint
    , tdpcExpression
    , tdpcType

    -- * LinuxParameters
    , LinuxParameters (..)
    , mkLinuxParameters
    , lpCapabilities
    , lpDevices
    , lpInitProcessEnabled
    , lpMaxSwap
    , lpSharedMemorySize
    , lpSwappiness
    , lpTmpfs

    -- * Tmpfs
    , Tmpfs (..)
    , mkTmpfs
    , tContainerPath
    , tSize
    , tMountOptions

    -- * InferenceAccelerator
    , InferenceAccelerator (..)
    , mkInferenceAccelerator
    , iaDeviceName
    , iaDeviceType

    -- * LogConfiguration
    , LogConfiguration (..)
    , mkLogConfiguration
    , lcLogDriver
    , lcOptions
    , lcSecretOptions

    -- * EFSAuthorizationConfigIAM
    , EFSAuthorizationConfigIAM (..)

    -- * FirelensConfiguration
    , FirelensConfiguration (..)
    , mkFirelensConfiguration
    , fcType
    , fcOptions

    -- * ProxyConfiguration
    , ProxyConfiguration (..)
    , mkProxyConfiguration
    , pContainerName
    , pProperties
    , pType

    -- * PlacementConstraintType
    , PlacementConstraintType (..)

    -- * TaskDefinitionStatus
    , TaskDefinitionStatus (..)

    -- * ContainerCondition
    , ContainerCondition (..)

    -- * ClusterSetting
    , ClusterSetting (..)
    , mkClusterSetting
    , csName
    , csValue

    -- * SortOrder
    , SortOrder (..)

    -- * AutoScalingGroupProvider
    , AutoScalingGroupProvider (..)
    , mkAutoScalingGroupProvider
    , asgpAutoScalingGroupArn
    , asgpManagedScaling
    , asgpManagedTerminationProtection

    -- * AgentUpdateStatus
    , AgentUpdateStatus (..)

    -- * VersionInfo
    , VersionInfo (..)
    , mkVersionInfo
    , viAgentHash
    , viAgentVersion
    , viDockerVersion

    -- * ServiceField
    , ServiceField (..)

    -- * CapacityProviderField
    , CapacityProviderField (..)

    -- * Container
    , Container (..)
    , mkContainer
    , cContainerArn
    , cCpu
    , cExitCode
    , cGpuIds
    , cHealthStatus
    , cImage
    , cImageDigest
    , cLastStatus
    , cMemory
    , cMemoryReservation
    , cName
    , cNetworkBindings
    , cNetworkInterfaces
    , cReason
    , cRuntimeId
    , cTaskArn

    -- * PlatformDeviceType
    , PlatformDeviceType (..)

    -- * DeviceCgroupPermission
    , DeviceCgroupPermission (..)

    -- * LoadBalancer
    , LoadBalancer (..)
    , mkLoadBalancer
    , lbContainerName
    , lbContainerPort
    , lbLoadBalancerName
    , lbTargetGroupArn

    -- * TaskDefinitionField
    , TaskDefinitionField (..)

    -- * LogDriver
    , LogDriver (..)

    -- * ManagedTerminationProtection
    , ManagedTerminationProtection (..)

    -- * DeploymentCircuitBreaker
    , DeploymentCircuitBreaker (..)
    , mkDeploymentCircuitBreaker
    , dcbEnable
    , dcbRollback

    -- * SystemControl
    , SystemControl (..)
    , mkSystemControl
    , scNamespace
    , scValue

    -- * Scope
    , Scope (..)

    -- * ContainerDefinition
    , ContainerDefinition (..)
    , mkContainerDefinition
    , cdCommand
    , cdCpu
    , cdDependsOn
    , cdDisableNetworking
    , cdDnsSearchDomains
    , cdDnsServers
    , cdDockerLabels
    , cdDockerSecurityOptions
    , cdEntryPoint
    , cdEnvironment
    , cdEnvironmentFiles
    , cdEssential
    , cdExtraHosts
    , cdFirelensConfiguration
    , cdHealthCheck
    , cdHostname
    , cdImage
    , cdInteractive
    , cdLinks
    , cdLinuxParameters
    , cdLogConfiguration
    , cdMemory
    , cdMemoryReservation
    , cdMountPoints
    , cdName
    , cdPortMappings
    , cdPrivileged
    , cdPseudoTerminal
    , cdReadonlyRootFilesystem
    , cdRepositoryCredentials
    , cdResourceRequirements
    , cdSecrets
    , cdStartTimeout
    , cdStopTimeout
    , cdSystemControls
    , cdUlimits
    , cdUser
    , cdVolumesFrom
    , cdWorkingDirectory

    -- * PlacementStrategy
    , PlacementStrategy (..)
    , mkPlacementStrategy
    , psField
    , psType

    -- * Resource
    , Resource (..)
    , mkResource
    , rDoubleValue
    , rIntegerValue
    , rLongValue
    , rName
    , rStringSetValue
    , rType

    -- * DeploymentController
    , DeploymentController (..)
    , mkDeploymentController
    , dcType

    -- * Connectivity
    , Connectivity (..)

    -- * DeploymentRolloutState
    , DeploymentRolloutState (..)

    -- * TaskSetField
    , TaskSetField (..)

    -- * Task
    , Task (..)
    , mkTask
    , tAttachments
    , tAttributes
    , tAvailabilityZone
    , tCapacityProviderName
    , tClusterArn
    , tConnectivity
    , tConnectivityAt
    , tContainerInstanceArn
    , tContainers
    , tCpu
    , tCreatedAt
    , tDesiredStatus
    , tExecutionStoppedAt
    , tGroup
    , tHealthStatus
    , tInferenceAccelerators
    , tLastStatus
    , tLaunchType
    , tMemory
    , tOverrides
    , tPlatformVersion
    , tPullStartedAt
    , tPullStoppedAt
    , tStartedAt
    , tStartedBy
    , tStopCode
    , tStoppedAt
    , tStoppedReason
    , tStoppingAt
    , tTags
    , tTaskArn
    , tTaskDefinitionArn
    , tVersion

    -- * ResourceRequirement
    , ResourceRequirement (..)
    , mkResourceRequirement
    , rrValue
    , rrType

    -- * PortMapping
    , PortMapping (..)
    , mkPortMapping
    , pmContainerPort
    , pmHostPort
    , pmProtocol

    -- * LaunchType
    , LaunchType (..)

    -- * NetworkMode
    , NetworkMode (..)

    -- * TagKey
    , TagKey (..)

    -- * TaskDefinition
    , TaskDefinition (..)
    , mkTaskDefinition
    , tdCompatibilities
    , tdContainerDefinitions
    , tdCpu
    , tdExecutionRoleArn
    , tdFamily
    , tdInferenceAccelerators
    , tdIpcMode
    , tdMemory
    , tdNetworkMode
    , tdPidMode
    , tdPlacementConstraints
    , tdProxyConfiguration
    , tdRequiresAttributes
    , tdRequiresCompatibilities
    , tdRevision
    , tdStatus
    , tdTaskDefinitionArn
    , tdTaskRoleArn
    , tdVolumes

    -- * Ulimit
    , Ulimit (..)
    , mkUlimit
    , uName
    , uSoftLimit
    , uHardLimit

    -- * ServiceRegistry
    , ServiceRegistry (..)
    , mkServiceRegistry
    , srContainerName
    , srContainerPort
    , srPort
    , srRegistryArn

    -- * CapacityProviderUpdateStatus
    , CapacityProviderUpdateStatus (..)

    -- * EFSVolumeConfiguration
    , EFSVolumeConfiguration (..)
    , mkEFSVolumeConfiguration
    , efsvcFileSystemId
    , efsvcAuthorizationConfig
    , efsvcRootDirectory
    , efsvcTransitEncryption
    , efsvcTransitEncryptionPort

    -- * HealthStatus
    , HealthStatus (..)

    -- * FSxWindowsFileServerAuthorizationConfig
    , FSxWindowsFileServerAuthorizationConfig (..)
    , mkFSxWindowsFileServerAuthorizationConfig
    , fswfsacCredentialsParameter
    , fswfsacDomain

    -- * TaskStopCode
    , TaskStopCode (..)

    -- * AwsVpcConfiguration
    , AwsVpcConfiguration (..)
    , mkAwsVpcConfiguration
    , avcSubnets
    , avcAssignPublicIp
    , avcSecurityGroups

    -- * Failure
    , Failure (..)
    , mkFailure
    , fArn
    , fDetail
    , fReason

    -- * CapacityProviderStrategyItem
    , CapacityProviderStrategyItem (..)
    , mkCapacityProviderStrategyItem
    , cpsiCapacityProvider
    , cpsiBase
    , cpsiWeight

    -- * EnvironmentFileType
    , EnvironmentFileType (..)

    -- * SchedulingStrategy
    , SchedulingStrategy (..)

    -- * ContainerInstance
    , ContainerInstance (..)
    , mkContainerInstance
    , ciAgentConnected
    , ciAgentUpdateStatus
    , ciAttachments
    , ciAttributes
    , ciCapacityProviderName
    , ciContainerInstanceArn
    , ciEc2InstanceId
    , ciPendingTasksCount
    , ciRegisteredAt
    , ciRegisteredResources
    , ciRemainingResources
    , ciRunningTasksCount
    , ciStatus
    , ciStatusReason
    , ciTags
    , ciVersion
    , ciVersionInfo

    -- * ScaleUnit
    , ScaleUnit (..)

    -- * NetworkConfiguration
    , NetworkConfiguration (..)
    , mkNetworkConfiguration
    , ncAwsvpcConfiguration

    -- * Compatibility
    , Compatibility (..)

    -- * StabilityStatus
    , StabilityStatus (..)

    -- * ServiceEvent
    , ServiceEvent (..)
    , mkServiceEvent
    , seCreatedAt
    , seId
    , seMessage

    -- * DeploymentConfiguration
    , DeploymentConfiguration (..)
    , mkDeploymentConfiguration
    , dcDeploymentCircuitBreaker
    , dcMaximumPercent
    , dcMinimumHealthyPercent

    -- * Deployment
    , Deployment (..)
    , mkDeployment
    , dCapacityProviderStrategy
    , dCreatedAt
    , dDesiredCount
    , dFailedTasks
    , dId
    , dLaunchType
    , dNetworkConfiguration
    , dPendingCount
    , dPlatformVersion
    , dRolloutState
    , dRolloutStateReason
    , dRunningCount
    , dStatus
    , dTaskDefinition
    , dUpdatedAt

    -- * MountPoint
    , MountPoint (..)
    , mkMountPoint
    , mpContainerPath
    , mpReadOnly
    , mpSourceVolume

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.ECS.Types.DesiredStatus
  
import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraintType
  
import Network.AWS.ECS.Types.DockerVolumeConfiguration
  
import Network.AWS.ECS.Types.ProxyConfigurationType
  
import Network.AWS.ECS.Types.Attribute
  
import Network.AWS.ECS.Types.HostEntry
  
import Network.AWS.ECS.Types.ManagedScalingStatus
  
import Network.AWS.ECS.Types.ContainerInstanceField
  
import Network.AWS.ECS.Types.PlacementConstraint
  
import Network.AWS.ECS.Types.FirelensConfigurationType
  
  
import Network.AWS.ECS.Types.TaskField
  
import Network.AWS.ECS.Types.UlimitName
  
import Network.AWS.ECS.Types.TaskSet
  
import Network.AWS.ECS.Types.NetworkBinding
  
  
import Network.AWS.ECS.Types.PlatformDevice
  
import Network.AWS.ECS.Types.Tag
  
import Network.AWS.ECS.Types.RepositoryCredentials
  
import Network.AWS.ECS.Types.Cluster
  
import Network.AWS.ECS.Types.PlacementStrategyType
  
import Network.AWS.ECS.Types.ResourceType
  
import Network.AWS.ECS.Types.ContainerInstanceStatus
  
import Network.AWS.ECS.Types.HealthCheck
  
import Network.AWS.ECS.Types.PropagateTags
  
import Network.AWS.ECS.Types.TaskDefinitionFamilyStatus
  
  
import Network.AWS.ECS.Types.KernelCapabilities
  
import Network.AWS.ECS.Types.TransportProtocol
  
import Network.AWS.ECS.Types.ContainerService
  
import Network.AWS.ECS.Types.DeploymentControllerType
  
import Network.AWS.ECS.Types.AutoScalingGroupProviderUpdate
  
import Network.AWS.ECS.Types.CapacityProvider
  
import Network.AWS.ECS.Types.EFSTransitEncryption
  
import Network.AWS.ECS.Types.ClusterField
  
import Network.AWS.ECS.Types.ContainerDependency
  
  
import Network.AWS.ECS.Types.Device
  
  
import Network.AWS.ECS.Types.Scale
  
import Network.AWS.ECS.Types.Volume
  
import Network.AWS.ECS.Types.AttachmentStateChange
  
  
import Network.AWS.ECS.Types.InferenceAcceleratorOverride
  
import Network.AWS.ECS.Types.ManagedScaling
  
import Network.AWS.ECS.Types.ContainerOverride
  
import Network.AWS.ECS.Types.NetworkInterface
  
  
  
import Network.AWS.ECS.Types.EnvironmentFile
  
  
import Network.AWS.ECS.Types.ClusterSettingName
  
import Network.AWS.ECS.Types.PidMode
  
import Network.AWS.ECS.Types.KeyValuePair
  
  
import Network.AWS.ECS.Types.Secret
  
  
import Network.AWS.ECS.Types.ContainerStateChange
  
import Network.AWS.ECS.Types.VolumeFrom
  
import Network.AWS.ECS.Types.Setting
  
  
import Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration
  
import Network.AWS.ECS.Types.EFSAuthorizationConfig
  
import Network.AWS.ECS.Types.AssignPublicIp
  
import Network.AWS.ECS.Types.TaskOverride
  
import Network.AWS.ECS.Types.HostVolumeProperties
  
  
  
import Network.AWS.ECS.Types.CapacityProviderStatus
  
import Network.AWS.ECS.Types.TargetType
  
import Network.AWS.ECS.Types.IpcMode
  
import Network.AWS.ECS.Types.SettingName
  
  
import Network.AWS.ECS.Types.Attachment
  
import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
  
import Network.AWS.ECS.Types.LinuxParameters
  
  
import Network.AWS.ECS.Types.Tmpfs
  
import Network.AWS.ECS.Types.InferenceAccelerator
  
import Network.AWS.ECS.Types.LogConfiguration
  
import Network.AWS.ECS.Types.EFSAuthorizationConfigIAM
  
import Network.AWS.ECS.Types.FirelensConfiguration
  
import Network.AWS.ECS.Types.ProxyConfiguration
  
import Network.AWS.ECS.Types.PlacementConstraintType
  
import Network.AWS.ECS.Types.TaskDefinitionStatus
  
import Network.AWS.ECS.Types.ContainerCondition
  
import Network.AWS.ECS.Types.ClusterSetting
  
import Network.AWS.ECS.Types.SortOrder
  
import Network.AWS.ECS.Types.AutoScalingGroupProvider
  
import Network.AWS.ECS.Types.AgentUpdateStatus
  
import Network.AWS.ECS.Types.VersionInfo
  
import Network.AWS.ECS.Types.ServiceField
  
  
import Network.AWS.ECS.Types.CapacityProviderField
  
import Network.AWS.ECS.Types.Container
  
import Network.AWS.ECS.Types.PlatformDeviceType
  
import Network.AWS.ECS.Types.DeviceCgroupPermission
  
import Network.AWS.ECS.Types.LoadBalancer
  
import Network.AWS.ECS.Types.TaskDefinitionField
  
import Network.AWS.ECS.Types.LogDriver
  
import Network.AWS.ECS.Types.ManagedTerminationProtection
  
import Network.AWS.ECS.Types.DeploymentCircuitBreaker
  
import Network.AWS.ECS.Types.SystemControl
  
import Network.AWS.ECS.Types.Scope
  
import Network.AWS.ECS.Types.ContainerDefinition
  
import Network.AWS.ECS.Types.PlacementStrategy
  
import Network.AWS.ECS.Types.Resource
  
import Network.AWS.ECS.Types.DeploymentController
  
import Network.AWS.ECS.Types.Connectivity
  
  
import Network.AWS.ECS.Types.DeploymentRolloutState
  
import Network.AWS.ECS.Types.TaskSetField
  
import Network.AWS.ECS.Types.Task
  
import Network.AWS.ECS.Types.ResourceRequirement
  
import Network.AWS.ECS.Types.PortMapping
  
  
import Network.AWS.ECS.Types.LaunchType
  
import Network.AWS.ECS.Types.NetworkMode
  
import Network.AWS.ECS.Types.TagKey
  
import Network.AWS.ECS.Types.TaskDefinition
  
import Network.AWS.ECS.Types.Ulimit
  
import Network.AWS.ECS.Types.ServiceRegistry
  
import Network.AWS.ECS.Types.CapacityProviderUpdateStatus
  
import Network.AWS.ECS.Types.EFSVolumeConfiguration
  
import Network.AWS.ECS.Types.HealthStatus
  
import Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig
  
import Network.AWS.ECS.Types.TaskStopCode
  
import Network.AWS.ECS.Types.AwsVpcConfiguration
  
import Network.AWS.ECS.Types.Failure
  
import Network.AWS.ECS.Types.CapacityProviderStrategyItem
  
import Network.AWS.ECS.Types.EnvironmentFileType
  
import Network.AWS.ECS.Types.SchedulingStrategy
  
import Network.AWS.ECS.Types.ContainerInstance
  
import Network.AWS.ECS.Types.ScaleUnit
  
  
import Network.AWS.ECS.Types.NetworkConfiguration
  
import Network.AWS.ECS.Types.Compatibility
  
  
import Network.AWS.ECS.Types.StabilityStatus
  
import Network.AWS.ECS.Types.ServiceEvent
  
import Network.AWS.ECS.Types.DeploymentConfiguration
  
import Network.AWS.ECS.Types.Deployment
  
import Network.AWS.ECS.Types.MountPoint
  
  
  
import Network.AWS.ECS.Types.Key
  
import Network.AWS.ECS.Types.Value
  

-- | API version @2014-11-13@ of the Amazon EC2 Container Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ECS", Core._svcSigner = Sign.v4,
                 Core._svcPrefix = "ecs", Core._svcVersion = "2014-11-13",
                 Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "ECS",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | You do not have authorization to perform the requested action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException
  = Core._MatchServiceError mkServiceConfig "AccessDeniedException"
{-# INLINEABLE _AccessDeniedException #-}
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | The specified parameter is invalid. Review the available parameters for the API request.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterException"
{-# INLINEABLE _InvalidParameterException #-}
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead"  #-}

-- | These errors are usually caused by a server issue.
_ServerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServerException
  = Core._MatchServiceError mkServiceConfig "ServerException"
{-# INLINEABLE _ServerException #-}
{-# DEPRECATED _ServerException "Use generic-lens or generic-optics instead"  #-}

-- | You cannot delete a cluster that has active tasks.
_ClusterContainsTasksException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterContainsTasksException
  = Core._MatchServiceError mkServiceConfig
      "ClusterContainsTasksException"
{-# INLINEABLE _ClusterContainsTasksException #-}
{-# DEPRECATED _ClusterContainsTasksException "Use generic-lens or generic-optics instead"  #-}

-- | The specified platform version does not exist.
_PlatformUnknownException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PlatformUnknownException
  = Core._MatchServiceError mkServiceConfig
      "PlatformUnknownException"
{-# INLINEABLE _PlatformUnknownException #-}
{-# DEPRECATED _PlatformUnknownException "Use generic-lens or generic-optics instead"  #-}

-- | You cannot delete a cluster that contains services. First, update the service to reduce its desired task count to 0 and then delete the service. For more information, see 'UpdateService' and 'DeleteService' .
_ClusterContainsServicesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterContainsServicesException
  = Core._MatchServiceError mkServiceConfig
      "ClusterContainsServicesException"
{-# INLINEABLE _ClusterContainsServicesException #-}
{-# DEPRECATED _ClusterContainsServicesException "Use generic-lens or generic-optics instead"  #-}

-- | The specified task set could not be found. You can view your available task sets with 'DescribeTaskSets' . Task sets are specific to each cluster, service and Region.
_TaskSetNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TaskSetNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "TaskSetNotFoundException"
{-# INLINEABLE _TaskSetNotFoundException #-}
{-# DEPRECATED _TaskSetNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | You cannot delete a cluster that has registered container instances. First, deregister the container instances before you can delete the cluster. For more information, see 'DeregisterContainerInstance' .
_ClusterContainsContainerInstancesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterContainsContainerInstancesException
  = Core._MatchServiceError mkServiceConfig
      "ClusterContainsContainerInstancesException"
{-# INLINEABLE _ClusterContainsContainerInstancesException #-}
{-# DEPRECATED _ClusterContainsContainerInstancesException "Use generic-lens or generic-optics instead"  #-}

-- | The specified service is not active. You can't update a service that is inactive. If you have previously deleted a service, you can re-create it with 'CreateService' .
_ServiceNotActiveException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceNotActiveException
  = Core._MatchServiceError mkServiceConfig
      "ServiceNotActiveException"
{-# INLINEABLE _ServiceNotActiveException #-}
{-# DEPRECATED _ServiceNotActiveException "Use generic-lens or generic-optics instead"  #-}

-- | The specified cluster could not be found. You can view your available clusters with 'ListClusters' . Amazon ECS clusters are Region-specific.
_ClusterNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ClusterNotFoundException"
{-# INLINEABLE _ClusterNotFoundException #-}
{-# DEPRECATED _ClusterNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | There is no update available for this Amazon ECS container agent. This could be because the agent is already running the latest version, or it is so old that there is no update path to the current version.
_NoUpdateAvailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoUpdateAvailableException
  = Core._MatchServiceError mkServiceConfig
      "NoUpdateAvailableException"
{-# INLINEABLE _NoUpdateAvailableException #-}
{-# DEPRECATED _NoUpdateAvailableException "Use generic-lens or generic-optics instead"  #-}

-- | The specified task is not supported in this Region.
_UnsupportedFeatureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedFeatureException
  = Core._MatchServiceError mkServiceConfig
      "UnsupportedFeatureException"
{-# INLINEABLE _UnsupportedFeatureException #-}
{-# DEPRECATED _UnsupportedFeatureException "Use generic-lens or generic-optics instead"  #-}

-- | The specified service could not be found. You can view your available services with 'ListServices' . Amazon ECS services are cluster-specific and Region-specific.
_ServiceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ServiceNotFoundException"
{-# INLINEABLE _ServiceNotFoundException #-}
{-# DEPRECATED _ServiceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified platform version does not satisfy the task definition's required capabilities.
_PlatformTaskDefinitionIncompatibilityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PlatformTaskDefinitionIncompatibilityException
  = Core._MatchServiceError mkServiceConfig
      "PlatformTaskDefinitionIncompatibilityException"
{-# INLINEABLE _PlatformTaskDefinitionIncompatibilityException #-}
{-# DEPRECATED _PlatformTaskDefinitionIncompatibilityException "Use generic-lens or generic-optics instead"  #-}

-- | Amazon ECS is unable to determine the current version of the Amazon ECS container agent on the container instance and does not have enough information to proceed with an update. This could be because the agent running on the container instance is an older or custom version that does not use our version information.
_MissingVersionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingVersionException
  = Core._MatchServiceError mkServiceConfig "MissingVersionException"
{-# INLINEABLE _MissingVersionException #-}
{-# DEPRECATED _MissingVersionException "Use generic-lens or generic-optics instead"  #-}

-- | There is already a current Amazon ECS container agent update in progress on the specified container instance. If the container agent becomes disconnected while it is in a transitional stage, such as @PENDING@ or @STAGING@ , the update process can get stuck in that state. However, when the agent reconnects, it resumes where it stopped previously.
_UpdateInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UpdateInProgressException
  = Core._MatchServiceError mkServiceConfig
      "UpdateInProgressException"
{-# INLINEABLE _UpdateInProgressException #-}
{-# DEPRECATED _UpdateInProgressException "Use generic-lens or generic-optics instead"  #-}

-- | Your AWS account has been blocked. For more information, contact <http://aws.amazon.com/contact-us/ AWS Support> .
_BlockedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BlockedException
  = Core._MatchServiceError mkServiceConfig "BlockedException"
{-# INLINEABLE _BlockedException #-}
{-# DEPRECATED _BlockedException "Use generic-lens or generic-optics instead"  #-}

-- | The specified target could not be found. You can view your available container instances with 'ListContainerInstances' . Amazon ECS container instances are cluster-specific and Region-specific.
_TargetNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TargetNotFoundException
  = Core._MatchServiceError mkServiceConfig "TargetNotFoundException"
{-# INLINEABLE _TargetNotFoundException #-}
{-# DEPRECATED _TargetNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | You can apply up to 10 custom attributes per resource. You can view the attributes of a resource with 'ListAttributes' . You can remove existing attributes on a resource with 'DeleteAttributes' .
_AttributeLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AttributeLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "AttributeLimitExceededException"
{-# INLINEABLE _AttributeLimitExceededException #-}
{-# DEPRECATED _AttributeLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | These errors are usually caused by a client action, such as using an action or resource on behalf of a user that doesn't have permissions to use the action or resource, or specifying an identifier that is not valid.
_ClientException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClientException
  = Core._MatchServiceError mkServiceConfig "ClientException"
{-# INLINEABLE _ClientException #-}
{-# DEPRECATED _ClientException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The limit for the resource has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource is in-use and cannot be removed.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}
