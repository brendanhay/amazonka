{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types
    (
    -- * Service Configuration
      ecs

    -- * Errors
    , _InvalidParameterException
    , _ServerException
    , _ClusterContainsServicesException
    , _ClusterContainsContainerInstancesException
    , _ServiceNotActiveException
    , _ClusterNotFoundException
    , _NoUpdateAvailableException
    , _ServiceNotFoundException
    , _MissingVersionException
    , _UpdateInProgressException
    , _TargetNotFoundException
    , _AttributeLimitExceededException
    , _ClientException

    -- * AgentUpdateStatus
    , AgentUpdateStatus (..)

    -- * ContainerInstanceStatus
    , ContainerInstanceStatus (..)

    -- * DesiredStatus
    , DesiredStatus (..)

    -- * LogDriver
    , LogDriver (..)

    -- * NetworkMode
    , NetworkMode (..)

    -- * PlacementConstraintType
    , PlacementConstraintType (..)

    -- * PlacementStrategyType
    , PlacementStrategyType (..)

    -- * SortOrder
    , SortOrder (..)

    -- * TargetType
    , TargetType (..)

    -- * TaskDefinitionFamilyStatus
    , TaskDefinitionFamilyStatus (..)

    -- * TaskDefinitionPlacementConstraintType
    , TaskDefinitionPlacementConstraintType (..)

    -- * TaskDefinitionStatus
    , TaskDefinitionStatus (..)

    -- * TransportProtocol
    , TransportProtocol (..)

    -- * UlimitName
    , UlimitName (..)

    -- * Attribute
    , Attribute
    , attribute
    , aTargetId
    , aValue
    , aTargetType
    , aName

    -- * Cluster
    , Cluster
    , cluster
    , cStatus
    , cClusterARN
    , cRunningTasksCount
    , cRegisteredContainerInstancesCount
    , cPendingTasksCount
    , cClusterName
    , cActiveServicesCount

    -- * Container
    , Container
    , container
    , cNetworkBindings
    , cContainerARN
    , cTaskARN
    , cLastStatus
    , cReason
    , cName
    , cExitCode

    -- * ContainerDefinition
    , ContainerDefinition
    , containerDefinition
    , cdImage
    , cdCommand
    , cdHostname
    , cdDockerSecurityOptions
    , cdDisableNetworking
    , cdVolumesFrom
    , cdEnvironment
    , cdEntryPoint
    , cdWorkingDirectory
    , cdUlimits
    , cdPrivileged
    , cdPortMappings
    , cdDockerLabels
    , cdExtraHosts
    , cdMemory
    , cdUser
    , cdDnsSearchDomains
    , cdLogConfiguration
    , cdName
    , cdDnsServers
    , cdMountPoints
    , cdLinks
    , cdReadonlyRootFilesystem
    , cdEssential
    , cdCpu
    , cdMemoryReservation

    -- * ContainerInstance
    , ContainerInstance
    , containerInstance
    , ciStatus
    , ciRunningTasksCount
    , ciRemainingResources
    , ciEc2InstanceId
    , ciContainerInstanceARN
    , ciAgentConnected
    , ciVersionInfo
    , ciAgentUpdateStatus
    , ciAttributes
    , ciVersion
    , ciPendingTasksCount
    , ciRegisteredAt
    , ciRegisteredResources

    -- * ContainerOverride
    , ContainerOverride
    , containerOverride
    , coCommand
    , coEnvironment
    , coMemory
    , coName
    , coCpu
    , coMemoryReservation

    -- * ContainerService
    , ContainerService
    , containerService
    , csRunningCount
    , csStatus
    , csClusterARN
    , csCreatedAt
    , csDesiredCount
    , csLoadBalancers
    , csPendingCount
    , csPlacementConstraints
    , csEvents
    , csPlacementStrategy
    , csDeployments
    , csServiceName
    , csServiceARN
    , csTaskDefinition
    , csRoleARN
    , csDeploymentConfiguration

    -- * Deployment
    , Deployment
    , deployment
    , dRunningCount
    , dStatus
    , dCreatedAt
    , dDesiredCount
    , dPendingCount
    , dId
    , dUpdatedAt
    , dTaskDefinition

    -- * DeploymentConfiguration
    , DeploymentConfiguration
    , deploymentConfiguration
    , dcMinimumHealthyPercent
    , dcMaximumPercent

    -- * Failure
    , Failure
    , failure
    , fArn
    , fReason

    -- * HostEntry
    , HostEntry
    , hostEntry
    , heHostname
    , heIpAddress

    -- * HostVolumeProperties
    , HostVolumeProperties
    , hostVolumeProperties
    , hvpSourcePath

    -- * KeyValuePair
    , KeyValuePair
    , keyValuePair
    , kvpValue
    , kvpName

    -- * LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbLoadBalancerName
    , lbContainerName
    , lbTargetGroupARN
    , lbContainerPort

    -- * LogConfiguration
    , LogConfiguration
    , logConfiguration
    , lcOptions
    , lcLogDriver

    -- * MountPoint
    , MountPoint
    , mountPoint
    , mpContainerPath
    , mpSourceVolume
    , mpReadOnly

    -- * NetworkBinding
    , NetworkBinding
    , networkBinding
    , nbBindIP
    , nbProtocol
    , nbHostPort
    , nbContainerPort

    -- * PlacementConstraint
    , PlacementConstraint
    , placementConstraint
    , pcExpression
    , pcType

    -- * PlacementStrategy
    , PlacementStrategy
    , placementStrategy
    , psField
    , psType

    -- * PortMapping
    , PortMapping
    , portMapping
    , pmProtocol
    , pmHostPort
    , pmContainerPort

    -- * Resource
    , Resource
    , resource
    , rStringSetValue
    , rIntegerValue
    , rDoubleValue
    , rLongValue
    , rName
    , rType

    -- * ServiceEvent
    , ServiceEvent
    , serviceEvent
    , seCreatedAt
    , seId
    , seMessage

    -- * Task
    , Task
    , task
    , tStoppedAt
    , tDesiredStatus
    , tOverrides
    , tClusterARN
    , tGroup
    , tCreatedAt
    , tTaskARN
    , tContainerInstanceARN
    , tLastStatus
    , tContainers
    , tStartedAt
    , tVersion
    , tStartedBy
    , tStoppedReason
    , tTaskDefinitionARN

    -- * TaskDefinition
    , TaskDefinition
    , taskDefinition
    , tdStatus
    , tdFamily
    , tdContainerDefinitions
    , tdTaskRoleARN
    , tdPlacementConstraints
    , tdNetworkMode
    , tdTaskDefinitionARN
    , tdRevision
    , tdVolumes
    , tdRequiresAttributes

    -- * TaskDefinitionPlacementConstraint
    , TaskDefinitionPlacementConstraint
    , taskDefinitionPlacementConstraint
    , tdpcExpression
    , tdpcType

    -- * TaskOverride
    , TaskOverride
    , taskOverride
    , toContainerOverrides
    , toTaskRoleARN

    -- * Ulimit
    , Ulimit
    , ulimit
    , uName
    , uSoftLimit
    , uHardLimit

    -- * VersionInfo
    , VersionInfo
    , versionInfo
    , viAgentHash
    , viAgentVersion
    , viDockerVersion

    -- * Volume
    , Volume
    , volume
    , vName
    , vHost

    -- * VolumeFrom
    , VolumeFrom
    , volumeFrom
    , vfSourceContainer
    , vfReadOnly
    ) where

import           Network.AWS.ECS.Types.Product
import           Network.AWS.ECS.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version @2014-11-13@ of the Amazon EC2 Container Service SDK configuration.
ecs :: Service
ecs =
    Service
    { _svcAbbrev = "ECS"
    , _svcSigner = v4
    , _svcPrefix = "ecs"
    , _svcVersion = "2014-11-13"
    , _svcEndpoint = defaultEndpoint ecs
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "ECS"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
          Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The specified parameter is invalid. Review the available parameters for the API request.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException = _MatchServiceError ecs "InvalidParameterException"

-- | These errors are usually caused by a server issue.
--
--
_ServerException :: AsError a => Getting (First ServiceError) a ServiceError
_ServerException = _MatchServiceError ecs "ServerException"

-- | You cannot delete a cluster that contains services. You must first update the service to reduce its desired task count to 0 and then delete the service. For more information, see 'UpdateService' and 'DeleteService' .
--
--
_ClusterContainsServicesException :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterContainsServicesException =
    _MatchServiceError ecs "ClusterContainsServicesException"

-- | You cannot delete a cluster that has registered container instances. You must first deregister the container instances before you can delete the cluster. For more information, see 'DeregisterContainerInstance' .
--
--
_ClusterContainsContainerInstancesException :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterContainsContainerInstancesException =
    _MatchServiceError ecs "ClusterContainsContainerInstancesException"

-- | The specified service is not active. You cannot update a service that is not active. If you have previously deleted a service, you can re-create it with 'CreateService' .
--
--
_ServiceNotActiveException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceNotActiveException = _MatchServiceError ecs "ServiceNotActiveException"

-- | The specified cluster could not be found. You can view your available clusters with 'ListClusters' . Amazon ECS clusters are region-specific.
--
--
_ClusterNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterNotFoundException = _MatchServiceError ecs "ClusterNotFoundException"

-- | There is no update available for this Amazon ECS container agent. This could be because the agent is already running the latest version, or it is so old that there is no update path to the current version.
--
--
_NoUpdateAvailableException :: AsError a => Getting (First ServiceError) a ServiceError
_NoUpdateAvailableException =
    _MatchServiceError ecs "NoUpdateAvailableException"

-- | The specified service could not be found. You can view your available services with 'ListServices' . Amazon ECS services are cluster-specific and region-specific.
--
--
_ServiceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceNotFoundException = _MatchServiceError ecs "ServiceNotFoundException"

-- | Amazon ECS is unable to determine the current version of the Amazon ECS container agent on the container instance and does not have enough information to proceed with an update. This could be because the agent running on the container instance is an older or custom version that does not use our version information.
--
--
_MissingVersionException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingVersionException = _MatchServiceError ecs "MissingVersionException"

-- | There is already a current Amazon ECS container agent update in progress on the specified container instance. If the container agent becomes disconnected while it is in a transitional stage, such as @PENDING@ or @STAGING@ , the update process can get stuck in that state. However, when the agent reconnects, it resumes where it stopped previously.
--
--
_UpdateInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_UpdateInProgressException = _MatchServiceError ecs "UpdateInProgressException"

-- | The specified target could not be found. You can view your available container instances with 'ListContainerInstances' . Amazon ECS container instances are cluster-specific and region-specific.
--
--
_TargetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_TargetNotFoundException = _MatchServiceError ecs "TargetNotFoundException"

-- | You can apply up to 10 custom attributes per resource. You can view the attributes of a resource with 'ListAttributes' . You can remove existing attributes on a resource with 'DeleteAttributes' .
--
--
_AttributeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_AttributeLimitExceededException =
    _MatchServiceError ecs "AttributeLimitExceededException"

-- | These errors are usually caused by a client action, such as using an action or resource on behalf of a user that doesn't have permission to use the action or resource, or specifying an identifier that is not valid.
--
--
_ClientException :: AsError a => Getting (First ServiceError) a ServiceError
_ClientException = _MatchServiceError ecs "ClientException"
