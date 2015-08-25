{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon EC2 Container Service (Amazon ECS) is a highly scalable, fast,
-- container management service that makes it easy to run, stop, and manage
-- Docker containers on a cluster of Amazon EC2 instances. Amazon ECS lets
-- you launch and stop container-enabled applications with simple API
-- calls, allows you to get the state of your cluster from a centralized
-- service, and gives you access to many familiar Amazon EC2 features like
-- security groups, Amazon EBS volumes, and IAM roles.
--
-- You can use Amazon ECS to schedule the placement of containers across
-- your cluster based on your resource needs, isolation policies, and
-- availability requirements. Amazon EC2 Container Service eliminates the
-- need for you to operate your own cluster management and configuration
-- management systems or worry about scaling your management
-- infrastructure.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.ECS
    (
    -- * Service Configuration
      eCS

    -- * Errors
    -- $errors

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** ServerException
    , _ServerException

    -- ** ClusterContainsServicesException
    , _ClusterContainsServicesException

    -- ** ClusterContainsContainerInstancesException
    , _ClusterContainsContainerInstancesException

    -- ** ServiceNotActiveException
    , _ServiceNotActiveException

    -- ** NoUpdateAvailableException
    , _NoUpdateAvailableException

    -- ** ClusterNotFoundException
    , _ClusterNotFoundException

    -- ** ServiceNotFoundException
    , _ServiceNotFoundException

    -- ** MissingVersionException
    , _MissingVersionException

    -- ** UpdateInProgressException
    , _UpdateInProgressException

    -- ** ClientException
    , _ClientException

    -- * Waiters
    -- $waiters

    -- ** ServicesInactive
    , servicesInactive

    -- ** TasksRunning
    , tasksRunning

    -- ** TasksStopped
    , tasksStopped

    -- * Operations
    -- $operations

    -- ** ListServices (Paginated)
    , module Network.AWS.ECS.ListServices

    -- ** DescribeClusters
    , module Network.AWS.ECS.DescribeClusters

    -- ** DeleteService
    , module Network.AWS.ECS.DeleteService

    -- ** UpdateService
    , module Network.AWS.ECS.UpdateService

    -- ** DiscoverPollEndpoint
    , module Network.AWS.ECS.DiscoverPollEndpoint

    -- ** SubmitContainerStateChange
    , module Network.AWS.ECS.SubmitContainerStateChange

    -- ** StopTask
    , module Network.AWS.ECS.StopTask

    -- ** DescribeTaskDefinition
    , module Network.AWS.ECS.DescribeTaskDefinition

    -- ** SubmitTaskStateChange
    , module Network.AWS.ECS.SubmitTaskStateChange

    -- ** DescribeContainerInstances
    , module Network.AWS.ECS.DescribeContainerInstances

    -- ** DeleteCluster
    , module Network.AWS.ECS.DeleteCluster

    -- ** CreateCluster
    , module Network.AWS.ECS.CreateCluster

    -- ** ListTaskDefinitions (Paginated)
    , module Network.AWS.ECS.ListTaskDefinitions

    -- ** ListTasks (Paginated)
    , module Network.AWS.ECS.ListTasks

    -- ** RunTask
    , module Network.AWS.ECS.RunTask

    -- ** ListContainerInstances (Paginated)
    , module Network.AWS.ECS.ListContainerInstances

    -- ** RegisterContainerInstance
    , module Network.AWS.ECS.RegisterContainerInstance

    -- ** UpdateContainerAgent
    , module Network.AWS.ECS.UpdateContainerAgent

    -- ** ListTaskDefinitionFamilies (Paginated)
    , module Network.AWS.ECS.ListTaskDefinitionFamilies

    -- ** StartTask
    , module Network.AWS.ECS.StartTask

    -- ** DeregisterTaskDefinition
    , module Network.AWS.ECS.DeregisterTaskDefinition

    -- ** DescribeTasks
    , module Network.AWS.ECS.DescribeTasks

    -- ** ListClusters (Paginated)
    , module Network.AWS.ECS.ListClusters

    -- ** DescribeServices
    , module Network.AWS.ECS.DescribeServices

    -- ** DeregisterContainerInstance
    , module Network.AWS.ECS.DeregisterContainerInstance

    -- ** RegisterTaskDefinition
    , module Network.AWS.ECS.RegisterTaskDefinition

    -- ** CreateService
    , module Network.AWS.ECS.CreateService

    -- * Types

    -- ** AgentUpdateStatus
    , AgentUpdateStatus (..)

    -- ** DesiredStatus
    , DesiredStatus (..)

    -- ** SortOrder
    , SortOrder (..)

    -- ** TaskDefinitionStatus
    , TaskDefinitionStatus (..)

    -- ** TransportProtocol
    , TransportProtocol (..)

    -- ** Cluster
    , Cluster
    , cluster
    , cStatus
    , cClusterARN
    , cRunningTasksCount
    , cRegisteredContainerInstancesCount
    , cPendingTasksCount
    , cClusterName
    , cActiveServicesCount

    -- ** Container
    , Container
    , container
    , cNetworkBindings
    , cContainerARN
    , cTaskARN
    , cLastStatus
    , cReason
    , cName
    , cExitCode

    -- ** ContainerDefinition
    , ContainerDefinition
    , containerDefinition
    , cdImage
    , cdCommand
    , cdVolumesFrom
    , cdEnvironment
    , cdEntryPoint
    , cdPortMappings
    , cdMemory
    , cdName
    , cdMountPoints
    , cdLinks
    , cdEssential
    , cdCpu

    -- ** ContainerInstance
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
    , ciPendingTasksCount
    , ciRegisteredResources

    -- ** ContainerOverride
    , ContainerOverride
    , containerOverride
    , coCommand
    , coEnvironment
    , coName

    -- ** ContainerService
    , ContainerService
    , containerService
    , csStatus
    , csRunningCount
    , csClusterARN
    , csDesiredCount
    , csLoadBalancers
    , csPendingCount
    , csEvents
    , csServiceName
    , csDeployments
    , csTaskDefinition
    , csServiceARN
    , csRoleARN

    -- ** Deployment
    , Deployment
    , deployment
    , dStatus
    , dRunningCount
    , dCreatedAt
    , dDesiredCount
    , dPendingCount
    , dId
    , dTaskDefinition
    , dUpdatedAt

    -- ** Failure
    , Failure
    , failure
    , fArn
    , fReason

    -- ** HostVolumeProperties
    , HostVolumeProperties
    , hostVolumeProperties
    , hvpSourcePath

    -- ** KeyValuePair
    , KeyValuePair
    , keyValuePair
    , kvpValue
    , kvpName

    -- ** LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbLoadBalancerName
    , lbContainerName
    , lbContainerPort

    -- ** MountPoint
    , MountPoint
    , mountPoint
    , mpContainerPath
    , mpSourceVolume
    , mpReadOnly

    -- ** NetworkBinding
    , NetworkBinding
    , networkBinding
    , nbBindIP
    , nbProtocol
    , nbHostPort
    , nbContainerPort

    -- ** PortMapping
    , PortMapping
    , portMapping
    , pmProtocol
    , pmHostPort
    , pmContainerPort

    -- ** Resource
    , Resource
    , resource
    , rStringSetValue
    , rIntegerValue
    , rDoubleValue
    , rLongValue
    , rName
    , rType

    -- ** ServiceEvent
    , ServiceEvent
    , serviceEvent
    , seCreatedAt
    , seId
    , seMessage

    -- ** Task
    , Task
    , task
    , tDesiredStatus
    , tClusterARN
    , tOverrides
    , tTaskARN
    , tContainerInstanceARN
    , tLastStatus
    , tContainers
    , tStartedBy
    , tTaskDefinitionARN

    -- ** TaskDefinition
    , TaskDefinition
    , taskDefinition
    , tdStatus
    , tdFamily
    , tdContainerDefinitions
    , tdTaskDefinitionARN
    , tdRevision
    , tdVolumes

    -- ** TaskOverride
    , TaskOverride
    , taskOverride
    , toContainerOverrides

    -- ** VersionInfo
    , VersionInfo
    , versionInfo
    , viAgentVersion
    , viAgentHash
    , viDockerVersion

    -- ** Volume
    , Volume
    , volume
    , vName
    , vHost

    -- ** VolumeFrom
    , VolumeFrom
    , volumeFrom
    , vfSourceContainer
    , vfReadOnly
    ) where

import           Network.AWS.ECS.CreateCluster
import           Network.AWS.ECS.CreateService
import           Network.AWS.ECS.DeleteCluster
import           Network.AWS.ECS.DeleteService
import           Network.AWS.ECS.DeregisterContainerInstance
import           Network.AWS.ECS.DeregisterTaskDefinition
import           Network.AWS.ECS.DescribeClusters
import           Network.AWS.ECS.DescribeContainerInstances
import           Network.AWS.ECS.DescribeServices
import           Network.AWS.ECS.DescribeTaskDefinition
import           Network.AWS.ECS.DescribeTasks
import           Network.AWS.ECS.DiscoverPollEndpoint
import           Network.AWS.ECS.ListClusters
import           Network.AWS.ECS.ListContainerInstances
import           Network.AWS.ECS.ListServices
import           Network.AWS.ECS.ListTaskDefinitionFamilies
import           Network.AWS.ECS.ListTaskDefinitions
import           Network.AWS.ECS.ListTasks
import           Network.AWS.ECS.RegisterContainerInstance
import           Network.AWS.ECS.RegisterTaskDefinition
import           Network.AWS.ECS.RunTask
import           Network.AWS.ECS.StartTask
import           Network.AWS.ECS.StopTask
import           Network.AWS.ECS.SubmitContainerStateChange
import           Network.AWS.ECS.SubmitTaskStateChange
import           Network.AWS.ECS.Types
import           Network.AWS.ECS.UpdateContainerAgent
import           Network.AWS.ECS.UpdateService
import           Network.AWS.ECS.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ECS'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
