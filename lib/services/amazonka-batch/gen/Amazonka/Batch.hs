{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Batch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-08-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Batch
--
-- Using Batch, you can run batch computing workloads on the Amazon Web
-- Services Cloud. Batch computing is a common means for developers,
-- scientists, and engineers to access large amounts of compute resources.
-- Batch uses the advantages of the batch computing to remove the
-- undifferentiated heavy lifting of configuring and managing required
-- infrastructure. At the same time, it also adopts a familiar batch
-- computing software approach. You can use Batch to efficiently provision
-- resources d, and work toward eliminating capacity constraints, reducing
-- your overall compute costs, and delivering results more quickly.
--
-- As a fully managed service, Batch can run batch computing workloads of
-- any scale. Batch automatically provisions compute resources and
-- optimizes workload distribution based on the quantity and scale of your
-- specific workloads. With Batch, there\'s no need to install or manage
-- batch computing software. This means that you can focus on analyzing
-- results and solving your specific problems instead.
module Amazonka.Batch
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

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- ** CreateComputeEnvironment
    CreateComputeEnvironment (CreateComputeEnvironment'),
    newCreateComputeEnvironment,
    CreateComputeEnvironmentResponse (CreateComputeEnvironmentResponse'),
    newCreateComputeEnvironmentResponse,

    -- ** CreateJobQueue
    CreateJobQueue (CreateJobQueue'),
    newCreateJobQueue,
    CreateJobQueueResponse (CreateJobQueueResponse'),
    newCreateJobQueueResponse,

    -- ** CreateSchedulingPolicy
    CreateSchedulingPolicy (CreateSchedulingPolicy'),
    newCreateSchedulingPolicy,
    CreateSchedulingPolicyResponse (CreateSchedulingPolicyResponse'),
    newCreateSchedulingPolicyResponse,

    -- ** DeleteComputeEnvironment
    DeleteComputeEnvironment (DeleteComputeEnvironment'),
    newDeleteComputeEnvironment,
    DeleteComputeEnvironmentResponse (DeleteComputeEnvironmentResponse'),
    newDeleteComputeEnvironmentResponse,

    -- ** DeleteJobQueue
    DeleteJobQueue (DeleteJobQueue'),
    newDeleteJobQueue,
    DeleteJobQueueResponse (DeleteJobQueueResponse'),
    newDeleteJobQueueResponse,

    -- ** DeleteSchedulingPolicy
    DeleteSchedulingPolicy (DeleteSchedulingPolicy'),
    newDeleteSchedulingPolicy,
    DeleteSchedulingPolicyResponse (DeleteSchedulingPolicyResponse'),
    newDeleteSchedulingPolicyResponse,

    -- ** DeregisterJobDefinition
    DeregisterJobDefinition (DeregisterJobDefinition'),
    newDeregisterJobDefinition,
    DeregisterJobDefinitionResponse (DeregisterJobDefinitionResponse'),
    newDeregisterJobDefinitionResponse,

    -- ** DescribeComputeEnvironments (Paginated)
    DescribeComputeEnvironments (DescribeComputeEnvironments'),
    newDescribeComputeEnvironments,
    DescribeComputeEnvironmentsResponse (DescribeComputeEnvironmentsResponse'),
    newDescribeComputeEnvironmentsResponse,

    -- ** DescribeJobDefinitions (Paginated)
    DescribeJobDefinitions (DescribeJobDefinitions'),
    newDescribeJobDefinitions,
    DescribeJobDefinitionsResponse (DescribeJobDefinitionsResponse'),
    newDescribeJobDefinitionsResponse,

    -- ** DescribeJobQueues (Paginated)
    DescribeJobQueues (DescribeJobQueues'),
    newDescribeJobQueues,
    DescribeJobQueuesResponse (DescribeJobQueuesResponse'),
    newDescribeJobQueuesResponse,

    -- ** DescribeJobs
    DescribeJobs (DescribeJobs'),
    newDescribeJobs,
    DescribeJobsResponse (DescribeJobsResponse'),
    newDescribeJobsResponse,

    -- ** DescribeSchedulingPolicies
    DescribeSchedulingPolicies (DescribeSchedulingPolicies'),
    newDescribeSchedulingPolicies,
    DescribeSchedulingPoliciesResponse (DescribeSchedulingPoliciesResponse'),
    newDescribeSchedulingPoliciesResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** ListSchedulingPolicies (Paginated)
    ListSchedulingPolicies (ListSchedulingPolicies'),
    newListSchedulingPolicies,
    ListSchedulingPoliciesResponse (ListSchedulingPoliciesResponse'),
    newListSchedulingPoliciesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TerminateJob
    TerminateJob (TerminateJob'),
    newTerminateJob,
    TerminateJobResponse (TerminateJobResponse'),
    newTerminateJobResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateComputeEnvironment
    UpdateComputeEnvironment (UpdateComputeEnvironment'),
    newUpdateComputeEnvironment,
    UpdateComputeEnvironmentResponse (UpdateComputeEnvironmentResponse'),
    newUpdateComputeEnvironmentResponse,

    -- ** UpdateJobQueue
    UpdateJobQueue (UpdateJobQueue'),
    newUpdateJobQueue,
    UpdateJobQueueResponse (UpdateJobQueueResponse'),
    newUpdateJobQueueResponse,

    -- ** UpdateSchedulingPolicy
    UpdateSchedulingPolicy (UpdateSchedulingPolicy'),
    newUpdateSchedulingPolicy,
    UpdateSchedulingPolicyResponse (UpdateSchedulingPolicyResponse'),
    newUpdateSchedulingPolicyResponse,

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

    -- ** CRUpdateAllocationStrategy
    CRUpdateAllocationStrategy (..),

    -- ** DeviceCgroupPermission
    DeviceCgroupPermission (..),

    -- ** EFSAuthorizationConfigIAM
    EFSAuthorizationConfigIAM (..),

    -- ** EFSTransitEncryption
    EFSTransitEncryption (..),

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

    -- ** OrchestrationType
    OrchestrationType (..),

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

    -- ** EFSAuthorizationConfig
    EFSAuthorizationConfig (EFSAuthorizationConfig'),
    newEFSAuthorizationConfig,

    -- ** EFSVolumeConfiguration
    EFSVolumeConfiguration (EFSVolumeConfiguration'),
    newEFSVolumeConfiguration,

    -- ** Ec2Configuration
    Ec2Configuration (Ec2Configuration'),
    newEc2Configuration,

    -- ** EksAttemptContainerDetail
    EksAttemptContainerDetail (EksAttemptContainerDetail'),
    newEksAttemptContainerDetail,

    -- ** EksAttemptDetail
    EksAttemptDetail (EksAttemptDetail'),
    newEksAttemptDetail,

    -- ** EksConfiguration
    EksConfiguration (EksConfiguration'),
    newEksConfiguration,

    -- ** EksContainer
    EksContainer (EksContainer'),
    newEksContainer,

    -- ** EksContainerDetail
    EksContainerDetail (EksContainerDetail'),
    newEksContainerDetail,

    -- ** EksContainerEnvironmentVariable
    EksContainerEnvironmentVariable (EksContainerEnvironmentVariable'),
    newEksContainerEnvironmentVariable,

    -- ** EksContainerOverride
    EksContainerOverride (EksContainerOverride'),
    newEksContainerOverride,

    -- ** EksContainerResourceRequirements
    EksContainerResourceRequirements (EksContainerResourceRequirements'),
    newEksContainerResourceRequirements,

    -- ** EksContainerSecurityContext
    EksContainerSecurityContext (EksContainerSecurityContext'),
    newEksContainerSecurityContext,

    -- ** EksContainerVolumeMount
    EksContainerVolumeMount (EksContainerVolumeMount'),
    newEksContainerVolumeMount,

    -- ** EksEmptyDir
    EksEmptyDir (EksEmptyDir'),
    newEksEmptyDir,

    -- ** EksHostPath
    EksHostPath (EksHostPath'),
    newEksHostPath,

    -- ** EksMetadata
    EksMetadata (EksMetadata'),
    newEksMetadata,

    -- ** EksPodProperties
    EksPodProperties (EksPodProperties'),
    newEksPodProperties,

    -- ** EksPodPropertiesDetail
    EksPodPropertiesDetail (EksPodPropertiesDetail'),
    newEksPodPropertiesDetail,

    -- ** EksPodPropertiesOverride
    EksPodPropertiesOverride (EksPodPropertiesOverride'),
    newEksPodPropertiesOverride,

    -- ** EksProperties
    EksProperties (EksProperties'),
    newEksProperties,

    -- ** EksPropertiesDetail
    EksPropertiesDetail (EksPropertiesDetail'),
    newEksPropertiesDetail,

    -- ** EksPropertiesOverride
    EksPropertiesOverride (EksPropertiesOverride'),
    newEksPropertiesOverride,

    -- ** EksSecret
    EksSecret (EksSecret'),
    newEksSecret,

    -- ** EksVolume
    EksVolume (EksVolume'),
    newEksVolume,

    -- ** EphemeralStorage
    EphemeralStorage (EphemeralStorage'),
    newEphemeralStorage,

    -- ** EvaluateOnExit
    EvaluateOnExit (EvaluateOnExit'),
    newEvaluateOnExit,

    -- ** FairsharePolicy
    FairsharePolicy (FairsharePolicy'),
    newFairsharePolicy,

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

    -- ** KeyValuesPair
    KeyValuesPair (KeyValuesPair'),
    newKeyValuesPair,

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

    -- ** SchedulingPolicyDetail
    SchedulingPolicyDetail (SchedulingPolicyDetail'),
    newSchedulingPolicyDetail,

    -- ** SchedulingPolicyListingDetail
    SchedulingPolicyListingDetail (SchedulingPolicyListingDetail'),
    newSchedulingPolicyListingDetail,

    -- ** Secret
    Secret (Secret'),
    newSecret,

    -- ** ShareAttributes
    ShareAttributes (ShareAttributes'),
    newShareAttributes,

    -- ** Tmpfs
    Tmpfs (Tmpfs'),
    newTmpfs,

    -- ** Ulimit
    Ulimit (Ulimit'),
    newUlimit,

    -- ** UpdatePolicy
    UpdatePolicy (UpdatePolicy'),
    newUpdatePolicy,

    -- ** Volume
    Volume (Volume'),
    newVolume,
  )
where

import Amazonka.Batch.CancelJob
import Amazonka.Batch.CreateComputeEnvironment
import Amazonka.Batch.CreateJobQueue
import Amazonka.Batch.CreateSchedulingPolicy
import Amazonka.Batch.DeleteComputeEnvironment
import Amazonka.Batch.DeleteJobQueue
import Amazonka.Batch.DeleteSchedulingPolicy
import Amazonka.Batch.DeregisterJobDefinition
import Amazonka.Batch.DescribeComputeEnvironments
import Amazonka.Batch.DescribeJobDefinitions
import Amazonka.Batch.DescribeJobQueues
import Amazonka.Batch.DescribeJobs
import Amazonka.Batch.DescribeSchedulingPolicies
import Amazonka.Batch.Lens
import Amazonka.Batch.ListJobs
import Amazonka.Batch.ListSchedulingPolicies
import Amazonka.Batch.ListTagsForResource
import Amazonka.Batch.RegisterJobDefinition
import Amazonka.Batch.SubmitJob
import Amazonka.Batch.TagResource
import Amazonka.Batch.TerminateJob
import Amazonka.Batch.Types
import Amazonka.Batch.UntagResource
import Amazonka.Batch.UpdateComputeEnvironment
import Amazonka.Batch.UpdateJobQueue
import Amazonka.Batch.UpdateSchedulingPolicy
import Amazonka.Batch.Waiters

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
