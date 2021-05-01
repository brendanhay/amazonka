{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Using AWS Batch, you can run batch computing workloads on the AWS Cloud.
-- Batch computing is a common means for developers, scientists, and
-- engineers to access large amounts of compute resources. AWS Batch
-- utilizes the advantages of this computing workload to remove the
-- undifferentiated heavy lifting of configuring and managing required
-- infrastructure, while also adopting a familiar batch computing software
-- approach. Given these advantages, AWS Batch can help you to efficiently
-- provision resources in response to jobs submitted, thus effectively
-- helping to eliminate capacity constraints, reduce compute costs, and
-- deliver your results more quickly.
--
-- As a fully managed service, AWS Batch can run batch computing workloads
-- of any scale. AWS Batch automatically provisions compute resources and
-- optimizes workload distribution based on the quantity and scale of your
-- specific workloads. With AWS Batch, there\'s no need to install or
-- manage batch computing software. This means that you can focus your time
-- and energy on analyzing results and solving your specific problems.
module Network.AWS.Batch
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ClientException
    _ClientException,

    -- ** ServerException
    _ServerException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateComputeEnvironment
    CreateComputeEnvironment (CreateComputeEnvironment'),
    newCreateComputeEnvironment,
    CreateComputeEnvironmentResponse (CreateComputeEnvironmentResponse'),
    newCreateComputeEnvironmentResponse,

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- ** DescribeJobs
    DescribeJobs (DescribeJobs'),
    newDescribeJobs,
    DescribeJobsResponse (DescribeJobsResponse'),
    newDescribeJobsResponse,

    -- ** DeleteComputeEnvironment
    DeleteComputeEnvironment (DeleteComputeEnvironment'),
    newDeleteComputeEnvironment,
    DeleteComputeEnvironmentResponse (DeleteComputeEnvironmentResponse'),
    newDeleteComputeEnvironmentResponse,

    -- ** UpdateComputeEnvironment
    UpdateComputeEnvironment (UpdateComputeEnvironment'),
    newUpdateComputeEnvironment,
    UpdateComputeEnvironmentResponse (UpdateComputeEnvironmentResponse'),
    newUpdateComputeEnvironmentResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeJobQueues (Paginated)
    DescribeJobQueues (DescribeJobQueues'),
    newDescribeJobQueues,
    DescribeJobQueuesResponse (DescribeJobQueuesResponse'),
    newDescribeJobQueuesResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DeregisterJobDefinition
    DeregisterJobDefinition (DeregisterJobDefinition'),
    newDeregisterJobDefinition,
    DeregisterJobDefinitionResponse (DeregisterJobDefinitionResponse'),
    newDeregisterJobDefinitionResponse,

    -- ** DeleteJobQueue
    DeleteJobQueue (DeleteJobQueue'),
    newDeleteJobQueue,
    DeleteJobQueueResponse (DeleteJobQueueResponse'),
    newDeleteJobQueueResponse,

    -- ** UpdateJobQueue
    UpdateJobQueue (UpdateJobQueue'),
    newUpdateJobQueue,
    UpdateJobQueueResponse (UpdateJobQueueResponse'),
    newUpdateJobQueueResponse,

    -- ** DescribeJobDefinitions (Paginated)
    DescribeJobDefinitions (DescribeJobDefinitions'),
    newDescribeJobDefinitions,
    DescribeJobDefinitionsResponse (DescribeJobDefinitionsResponse'),
    newDescribeJobDefinitionsResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** DescribeComputeEnvironments (Paginated)
    DescribeComputeEnvironments (DescribeComputeEnvironments'),
    newDescribeComputeEnvironments,
    DescribeComputeEnvironmentsResponse (DescribeComputeEnvironmentsResponse'),
    newDescribeComputeEnvironmentsResponse,

    -- ** TerminateJob
    TerminateJob (TerminateJob'),
    newTerminateJob,
    TerminateJobResponse (TerminateJobResponse'),
    newTerminateJobResponse,

    -- ** RegisterJobDefinition
    RegisterJobDefinition (RegisterJobDefinition'),
    newRegisterJobDefinition,
    RegisterJobDefinitionResponse (RegisterJobDefinitionResponse'),
    newRegisterJobDefinitionResponse,

    -- ** SubmitJob
    SubmitJob (SubmitJob'),
    newSubmitJob,
    SubmitJobResponse (SubmitJobResponse'),
    newSubmitJobResponse,

    -- ** CreateJobQueue
    CreateJobQueue (CreateJobQueue'),
    newCreateJobQueue,
    CreateJobQueueResponse (CreateJobQueueResponse'),
    newCreateJobQueueResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- * Types

    -- ** ArrayJobDependency
    ArrayJobDependency (..),

    -- ** AssignPublicIp
    AssignPublicIp (..),

    -- ** CEState
    CEState (..),

    -- ** CEStatus
    CEStatus (..),

    -- ** CEType
    CEType (..),

    -- ** CRAllocationStrategy
    CRAllocationStrategy (..),

    -- ** CRType
    CRType (..),

    -- ** DeviceCgroupPermission
    DeviceCgroupPermission (..),

    -- ** JQState
    JQState (..),

    -- ** JQStatus
    JQStatus (..),

    -- ** JobDefinitionType
    JobDefinitionType (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** LogDriver
    LogDriver (..),

    -- ** PlatformCapability
    PlatformCapability (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** RetryAction
    RetryAction (..),

    -- ** ArrayProperties
    ArrayProperties (ArrayProperties'),
    newArrayProperties,

    -- ** ArrayPropertiesDetail
    ArrayPropertiesDetail (ArrayPropertiesDetail'),
    newArrayPropertiesDetail,

    -- ** ArrayPropertiesSummary
    ArrayPropertiesSummary (ArrayPropertiesSummary'),
    newArrayPropertiesSummary,

    -- ** AttemptContainerDetail
    AttemptContainerDetail (AttemptContainerDetail'),
    newAttemptContainerDetail,

    -- ** AttemptDetail
    AttemptDetail (AttemptDetail'),
    newAttemptDetail,

    -- ** ComputeEnvironmentDetail
    ComputeEnvironmentDetail (ComputeEnvironmentDetail'),
    newComputeEnvironmentDetail,

    -- ** ComputeEnvironmentOrder
    ComputeEnvironmentOrder (ComputeEnvironmentOrder'),
    newComputeEnvironmentOrder,

    -- ** ComputeResource
    ComputeResource (ComputeResource'),
    newComputeResource,

    -- ** ComputeResourceUpdate
    ComputeResourceUpdate (ComputeResourceUpdate'),
    newComputeResourceUpdate,

    -- ** ContainerDetail
    ContainerDetail (ContainerDetail'),
    newContainerDetail,

    -- ** ContainerOverrides
    ContainerOverrides (ContainerOverrides'),
    newContainerOverrides,

    -- ** ContainerProperties
    ContainerProperties (ContainerProperties'),
    newContainerProperties,

    -- ** ContainerSummary
    ContainerSummary (ContainerSummary'),
    newContainerSummary,

    -- ** Device
    Device (Device'),
    newDevice,

    -- ** Ec2Configuration
    Ec2Configuration (Ec2Configuration'),
    newEc2Configuration,

    -- ** EvaluateOnExit
    EvaluateOnExit (EvaluateOnExit'),
    newEvaluateOnExit,

    -- ** FargatePlatformConfiguration
    FargatePlatformConfiguration (FargatePlatformConfiguration'),
    newFargatePlatformConfiguration,

    -- ** Host
    Host (Host'),
    newHost,

    -- ** JobDefinition
    JobDefinition (JobDefinition'),
    newJobDefinition,

    -- ** JobDependency
    JobDependency (JobDependency'),
    newJobDependency,

    -- ** JobDetail
    JobDetail (JobDetail'),
    newJobDetail,

    -- ** JobQueueDetail
    JobQueueDetail (JobQueueDetail'),
    newJobQueueDetail,

    -- ** JobSummary
    JobSummary (JobSummary'),
    newJobSummary,

    -- ** JobTimeout
    JobTimeout (JobTimeout'),
    newJobTimeout,

    -- ** KeyValuePair
    KeyValuePair (KeyValuePair'),
    newKeyValuePair,

    -- ** LaunchTemplateSpecification
    LaunchTemplateSpecification (LaunchTemplateSpecification'),
    newLaunchTemplateSpecification,

    -- ** LinuxParameters
    LinuxParameters (LinuxParameters'),
    newLinuxParameters,

    -- ** LogConfiguration
    LogConfiguration (LogConfiguration'),
    newLogConfiguration,

    -- ** MountPoint
    MountPoint (MountPoint'),
    newMountPoint,

    -- ** NetworkConfiguration
    NetworkConfiguration (NetworkConfiguration'),
    newNetworkConfiguration,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** NodeDetails
    NodeDetails (NodeDetails'),
    newNodeDetails,

    -- ** NodeOverrides
    NodeOverrides (NodeOverrides'),
    newNodeOverrides,

    -- ** NodeProperties
    NodeProperties (NodeProperties'),
    newNodeProperties,

    -- ** NodePropertiesSummary
    NodePropertiesSummary (NodePropertiesSummary'),
    newNodePropertiesSummary,

    -- ** NodePropertyOverride
    NodePropertyOverride (NodePropertyOverride'),
    newNodePropertyOverride,

    -- ** NodeRangeProperty
    NodeRangeProperty (NodeRangeProperty'),
    newNodeRangeProperty,

    -- ** ResourceRequirement
    ResourceRequirement (ResourceRequirement'),
    newResourceRequirement,

    -- ** RetryStrategy
    RetryStrategy (RetryStrategy'),
    newRetryStrategy,

    -- ** Secret
    Secret (Secret'),
    newSecret,

    -- ** Tmpfs
    Tmpfs (Tmpfs'),
    newTmpfs,

    -- ** Ulimit
    Ulimit (Ulimit'),
    newUlimit,

    -- ** Volume
    Volume (Volume'),
    newVolume,
  )
where

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
import Network.AWS.Batch.Lens
import Network.AWS.Batch.ListJobs
import Network.AWS.Batch.ListTagsForResource
import Network.AWS.Batch.RegisterJobDefinition
import Network.AWS.Batch.SubmitJob
import Network.AWS.Batch.TagResource
import Network.AWS.Batch.TerminateJob
import Network.AWS.Batch.Types
import Network.AWS.Batch.UntagResource
import Network.AWS.Batch.UpdateComputeEnvironment
import Network.AWS.Batch.UpdateJobQueue
import Network.AWS.Batch.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Batch'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
