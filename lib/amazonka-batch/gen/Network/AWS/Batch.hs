{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Batch enables you to run batch computing workloads on the AWS Cloud. Batch computing is a common way for developers, scientists, and engineers to access large amounts of compute resources, and AWS Batch removes the undifferentiated heavy lifting of configuring and managing the required infrastructure. AWS Batch will be familiar to users of traditional batch computing software. This service can efficiently provision resources in response to jobs submitted in order to eliminate capacity constraints, reduce compute costs, and deliver results quickly.
--
--
-- As a fully managed service, AWS Batch enables developers, scientists, and engineers to run batch computing workloads of any scale. AWS Batch automatically provisions compute resources and optimizes the workload distribution based on the quantity and scale of the workloads. With AWS Batch, there is no need to install or manage batch computing software, which allows you to focus on analyzing results and solving problems. AWS Batch reduces operational complexities, saves time, and reduces costs, which makes it easy for developers, scientists, and engineers to run their batch jobs in the AWS Cloud.
--
module Network.AWS.Batch
    (
    -- * Service Configuration
      batch

    -- * Errors
    -- $errors

    -- ** ServerException
    , _ServerException

    -- ** ClientException
    , _ClientException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateComputeEnvironment
    , module Network.AWS.Batch.CreateComputeEnvironment

    -- ** RegisterJobDefinition
    , module Network.AWS.Batch.RegisterJobDefinition

    -- ** SubmitJob
    , module Network.AWS.Batch.SubmitJob

    -- ** ListJobs
    , module Network.AWS.Batch.ListJobs

    -- ** TerminateJob
    , module Network.AWS.Batch.TerminateJob

    -- ** DescribeJobs
    , module Network.AWS.Batch.DescribeJobs

    -- ** DeleteComputeEnvironment
    , module Network.AWS.Batch.DeleteComputeEnvironment

    -- ** UpdateComputeEnvironment
    , module Network.AWS.Batch.UpdateComputeEnvironment

    -- ** DescribeJobDefinitions
    , module Network.AWS.Batch.DescribeJobDefinitions

    -- ** UpdateJobQueue
    , module Network.AWS.Batch.UpdateJobQueue

    -- ** DeleteJobQueue
    , module Network.AWS.Batch.DeleteJobQueue

    -- ** CreateJobQueue
    , module Network.AWS.Batch.CreateJobQueue

    -- ** DeregisterJobDefinition
    , module Network.AWS.Batch.DeregisterJobDefinition

    -- ** DescribeJobQueues
    , module Network.AWS.Batch.DescribeJobQueues

    -- ** DescribeComputeEnvironments
    , module Network.AWS.Batch.DescribeComputeEnvironments

    -- ** CancelJob
    , module Network.AWS.Batch.CancelJob

    -- * Types

    -- ** ArrayJobDependency
    , ArrayJobDependency (..)

    -- ** CEState
    , CEState (..)

    -- ** CEStatus
    , CEStatus (..)

    -- ** CEType
    , CEType (..)

    -- ** CRType
    , CRType (..)

    -- ** JQState
    , JQState (..)

    -- ** JQStatus
    , JQStatus (..)

    -- ** JobDefinitionType
    , JobDefinitionType (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** ArrayProperties
    , ArrayProperties
    , arrayProperties
    , apSize

    -- ** ArrayPropertiesDetail
    , ArrayPropertiesDetail
    , arrayPropertiesDetail
    , apdSize
    , apdStatusSummary
    , apdIndex

    -- ** ArrayPropertiesSummary
    , ArrayPropertiesSummary
    , arrayPropertiesSummary
    , apsSize
    , apsIndex

    -- ** AttemptContainerDetail
    , AttemptContainerDetail
    , attemptContainerDetail
    , acdTaskARN
    , acdContainerInstanceARN
    , acdReason
    , acdLogStreamName
    , acdExitCode

    -- ** AttemptDetail
    , AttemptDetail
    , attemptDetail
    , adStoppedAt
    , adStartedAt
    , adContainer
    , adStatusReason

    -- ** ComputeEnvironmentDetail
    , ComputeEnvironmentDetail
    , computeEnvironmentDetail
    , cedStatus
    , cedState
    , cedComputeResources
    , cedStatusReason
    , cedType
    , cedServiceRole
    , cedComputeEnvironmentName
    , cedComputeEnvironmentARN
    , cedEcsClusterARN

    -- ** ComputeEnvironmentOrder
    , ComputeEnvironmentOrder
    , computeEnvironmentOrder
    , ceoOrder
    , ceoComputeEnvironment

    -- ** ComputeResource
    , ComputeResource
    , computeResource
    , crEc2KeyPair
    , crBidPercentage
    , crSpotIAMFleetRole
    , crImageId
    , crDesiredvCPUs
    , crTags
    , crType
    , crMinvCPUs
    , crMaxvCPUs
    , crInstanceTypes
    , crSubnets
    , crSecurityGroupIds
    , crInstanceRole

    -- ** ComputeResourceUpdate
    , ComputeResourceUpdate
    , computeResourceUpdate
    , cruMinvCPUs
    , cruMaxvCPUs
    , cruDesiredvCPUs

    -- ** ContainerDetail
    , ContainerDetail
    , containerDetail
    , cdImage
    , cdCommand
    , cdEnvironment
    , cdTaskARN
    , cdUlimits
    , cdContainerInstanceARN
    , cdPrivileged
    , cdJobRoleARN
    , cdMemory
    , cdUser
    , cdReason
    , cdLogStreamName
    , cdMountPoints
    , cdExitCode
    , cdVcpus
    , cdReadonlyRootFilesystem
    , cdVolumes

    -- ** ContainerOverrides
    , ContainerOverrides
    , containerOverrides
    , coCommand
    , coEnvironment
    , coMemory
    , coVcpus

    -- ** ContainerProperties
    , ContainerProperties
    , containerProperties
    , cpCommand
    , cpEnvironment
    , cpUlimits
    , cpPrivileged
    , cpJobRoleARN
    , cpUser
    , cpMountPoints
    , cpReadonlyRootFilesystem
    , cpVolumes
    , cpImage
    , cpVcpus
    , cpMemory

    -- ** ContainerSummary
    , ContainerSummary
    , containerSummary
    , csReason
    , csExitCode

    -- ** Host
    , Host
    , host
    , hSourcePath

    -- ** JobDefinition
    , JobDefinition
    , jobDefinition
    , jddStatus
    , jddRetryStrategy
    , jddParameters
    , jddTimeout
    , jddContainerProperties
    , jddJobDefinitionName
    , jddJobDefinitionARN
    , jddRevision
    , jddType

    -- ** JobDependency
    , JobDependency
    , jobDependency
    , jJobId
    , jType

    -- ** JobDetail
    , JobDetail
    , jobDetail
    , jdStoppedAt
    , jdCreatedAt
    , jdRetryStrategy
    , jdAttempts
    , jdDependsOn
    , jdContainer
    , jdParameters
    , jdStatusReason
    , jdArrayProperties
    , jdTimeout
    , jdJobName
    , jdJobId
    , jdJobQueue
    , jdStatus
    , jdStartedAt
    , jdJobDefinition

    -- ** JobQueueDetail
    , JobQueueDetail
    , jobQueueDetail
    , jqdStatus
    , jqdStatusReason
    , jqdJobQueueName
    , jqdJobQueueARN
    , jqdState
    , jqdPriority
    , jqdComputeEnvironmentOrder

    -- ** JobSummary
    , JobSummary
    , jobSummary
    , jsStoppedAt
    , jsStatus
    , jsCreatedAt
    , jsStartedAt
    , jsContainer
    , jsStatusReason
    , jsArrayProperties
    , jsJobId
    , jsJobName

    -- ** JobTimeout
    , JobTimeout
    , jobTimeout
    , jtAttemptDurationSeconds

    -- ** KeyValuePair
    , KeyValuePair
    , keyValuePair
    , kvpValue
    , kvpName

    -- ** MountPoint
    , MountPoint
    , mountPoint
    , mpContainerPath
    , mpSourceVolume
    , mpReadOnly

    -- ** RetryStrategy
    , RetryStrategy
    , retryStrategy
    , rsAttempts

    -- ** Ulimit
    , Ulimit
    , ulimit
    , uHardLimit
    , uName
    , uSoftLimit

    -- ** Volume
    , Volume
    , volume
    , vName
    , vHost
    ) where

import Network.AWS.Batch.CancelJob
import Network.AWS.Batch.CreateComputeEnvironment
import Network.AWS.Batch.CreateJobQueue
import Network.AWS.Batch.DeleteComputeEnvironment
import Network.AWS.Batch.DeleteJobQueue
import Network.AWS.Batch.DeregisterJobDefinition
import Network.AWS.Batch.DescribeComputeEnvironments
import Network.AWS.Batch.DescribeJobDefinitions
import Network.AWS.Batch.DescribeJobQueues
import Network.AWS.Batch.DescribeJobs
import Network.AWS.Batch.ListJobs
import Network.AWS.Batch.RegisterJobDefinition
import Network.AWS.Batch.SubmitJob
import Network.AWS.Batch.TerminateJob
import Network.AWS.Batch.Types
import Network.AWS.Batch.UpdateComputeEnvironment
import Network.AWS.Batch.UpdateJobQueue
import Network.AWS.Batch.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Batch'.
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
