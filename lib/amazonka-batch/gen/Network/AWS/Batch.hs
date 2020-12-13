{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Batch enables you to run batch computing workloads on the AWS Cloud. Batch computing is a common way for developers, scientists, and engineers to access large amounts of compute resources, and AWS Batch removes the undifferentiated heavy lifting of configuring and managing the required infrastructure. AWS Batch will be familiar to users of traditional batch computing software. This service can efficiently provision resources in response to jobs submitted in order to eliminate capacity constraints, reduce compute costs, and deliver results quickly.
--
-- As a fully managed service, AWS Batch enables developers, scientists, and engineers to run batch computing workloads of any scale. AWS Batch automatically provisions compute resources and optimizes the workload distribution based on the quantity and scale of the workloads. With AWS Batch, there is no need to install or manage batch computing software, which allows you to focus on analyzing results and solving problems. AWS Batch reduces operational complexities, saves time, and reduces costs, which makes it easy for developers, scientists, and engineers to run their batch jobs in the AWS Cloud.
module Network.AWS.Batch
  ( -- * Service configuration
    batchService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateComputeEnvironment
    module Network.AWS.Batch.CreateComputeEnvironment,

    -- ** ListTagsForResource
    module Network.AWS.Batch.ListTagsForResource,

    -- ** RegisterJobDefinition
    module Network.AWS.Batch.RegisterJobDefinition,

    -- ** SubmitJob
    module Network.AWS.Batch.SubmitJob,

    -- ** ListJobs (Paginated)
    module Network.AWS.Batch.ListJobs,

    -- ** TerminateJob
    module Network.AWS.Batch.TerminateJob,

    -- ** DescribeJobs
    module Network.AWS.Batch.DescribeJobs,

    -- ** DeleteComputeEnvironment
    module Network.AWS.Batch.DeleteComputeEnvironment,

    -- ** UpdateComputeEnvironment
    module Network.AWS.Batch.UpdateComputeEnvironment,

    -- ** DescribeJobDefinitions (Paginated)
    module Network.AWS.Batch.DescribeJobDefinitions,

    -- ** UpdateJobQueue
    module Network.AWS.Batch.UpdateJobQueue,

    -- ** DeleteJobQueue
    module Network.AWS.Batch.DeleteJobQueue,

    -- ** CreateJobQueue
    module Network.AWS.Batch.CreateJobQueue,

    -- ** DeregisterJobDefinition
    module Network.AWS.Batch.DeregisterJobDefinition,

    -- ** DescribeJobQueues (Paginated)
    module Network.AWS.Batch.DescribeJobQueues,

    -- ** TagResource
    module Network.AWS.Batch.TagResource,

    -- ** UntagResource
    module Network.AWS.Batch.UntagResource,

    -- ** DescribeComputeEnvironments (Paginated)
    module Network.AWS.Batch.DescribeComputeEnvironments,

    -- ** CancelJob
    module Network.AWS.Batch.CancelJob,

    -- * Types

    -- ** ArrayJobDependency
    ArrayJobDependency (..),

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

    -- ** ResourceType
    ResourceType (..),

    -- ** RetryAction
    RetryAction (..),

    -- ** ArrayProperties
    ArrayProperties (..),
    mkArrayProperties,
    apSize,

    -- ** ArrayPropertiesDetail
    ArrayPropertiesDetail (..),
    mkArrayPropertiesDetail,
    apdSize,
    apdStatusSummary,
    apdIndex,

    -- ** ArrayPropertiesSummary
    ArrayPropertiesSummary (..),
    mkArrayPropertiesSummary,
    apsSize,
    apsIndex,

    -- ** AttemptContainerDetail
    AttemptContainerDetail (..),
    mkAttemptContainerDetail,
    acdNetworkInterfaces,
    acdTaskARN,
    acdContainerInstanceARN,
    acdReason,
    acdLogStreamName,
    acdExitCode,

    -- ** AttemptDetail
    AttemptDetail (..),
    mkAttemptDetail,
    adStoppedAt,
    adStartedAt,
    adContainer,
    adStatusReason,

    -- ** ComputeEnvironmentDetail
    ComputeEnvironmentDetail (..),
    mkComputeEnvironmentDetail,
    cedStatus,
    cedComputeEnvironmentName,
    cedState,
    cedEcsClusterARN,
    cedComputeResources,
    cedComputeEnvironmentARN,
    cedStatusReason,
    cedType,
    cedServiceRole,
    cedTags,

    -- ** ComputeEnvironmentOrder
    ComputeEnvironmentOrder (..),
    mkComputeEnvironmentOrder,
    ceoComputeEnvironment,
    ceoOrder,

    -- ** ComputeResource
    ComputeResource (..),
    mkComputeResource,
    crSecurityGroupIds,
    crInstanceTypes,
    crInstanceRole,
    crSubnets,
    crEc2KeyPair,
    crMinvCPUs,
    crEc2Configuration,
    crMaxvCPUs,
    crBidPercentage,
    crSpotIAMFleetRole,
    crImageId,
    crLaunchTemplate,
    crType,
    crDesiredvCPUs,
    crAllocationStrategy,
    crPlacementGroup,
    crTags,

    -- ** ComputeResourceUpdate
    ComputeResourceUpdate (..),
    mkComputeResourceUpdate,
    cruMinvCPUs,
    cruMaxvCPUs,
    cruDesiredvCPUs,

    -- ** ContainerDetail
    ContainerDetail (..),
    mkContainerDetail,
    cdImage,
    cdCommand,
    cdSecrets,
    cdEnvironment,
    cdNetworkInterfaces,
    cdTaskARN,
    cdUlimits,
    cdContainerInstanceARN,
    cdExecutionRoleARN,
    cdPrivileged,
    cdJobRoleARN,
    cdResourceRequirements,
    cdInstanceType,
    cdMemory,
    cdUser,
    cdLogConfiguration,
    cdLinuxParameters,
    cdReason,
    cdLogStreamName,
    cdMountPoints,
    cdExitCode,
    cdVcpus,
    cdReadonlyRootFilesystem,
    cdVolumes,

    -- ** ContainerOverrides
    ContainerOverrides (..),
    mkContainerOverrides,
    coCommand,
    coEnvironment,
    coResourceRequirements,
    coInstanceType,
    coMemory,
    coVcpus,

    -- ** ContainerProperties
    ContainerProperties (..),
    mkContainerProperties,
    cpImage,
    cpCommand,
    cpSecrets,
    cpEnvironment,
    cpUlimits,
    cpExecutionRoleARN,
    cpPrivileged,
    cpJobRoleARN,
    cpResourceRequirements,
    cpInstanceType,
    cpMemory,
    cpUser,
    cpLogConfiguration,
    cpLinuxParameters,
    cpMountPoints,
    cpVcpus,
    cpReadonlyRootFilesystem,
    cpVolumes,

    -- ** ContainerSummary
    ContainerSummary (..),
    mkContainerSummary,
    csReason,
    csExitCode,

    -- ** Device
    Device (..),
    mkDevice,
    dContainerPath,
    dHostPath,
    dPermissions,

    -- ** EC2Configuration
    EC2Configuration (..),
    mkEC2Configuration,
    ecImageType,
    ecImageIdOverride,

    -- ** EvaluateOnExit
    EvaluateOnExit (..),
    mkEvaluateOnExit,
    eoeOnExitCode,
    eoeOnReason,
    eoeAction,
    eoeOnStatusReason,

    -- ** Host
    Host (..),
    mkHost,
    hSourcePath,

    -- ** JobDefinition
    JobDefinition (..),
    mkJobDefinition,
    jdfStatus,
    jdfJobDefinitionName,
    jdfRetryStrategy,
    jdfJobDefinitionARN,
    jdfParameters,
    jdfType,
    jdfTimeout,
    jdfRevision,
    jdfContainerProperties,
    jdfNodeProperties,
    jdfTags,

    -- ** JobDependency
    JobDependency (..),
    mkJobDependency,
    jJobId,
    jType,

    -- ** JobDetail
    JobDetail (..),
    mkJobDetail,
    jdStoppedAt,
    jdStatus,
    jdJobId,
    jdJobARN,
    jdCreatedAt,
    jdJobName,
    jdRetryStrategy,
    jdAttempts,
    jdStartedAt,
    jdDependsOn,
    jdContainer,
    jdJobDefinition,
    jdNodeDetails,
    jdParameters,
    jdStatusReason,
    jdArrayProperties,
    jdTimeout,
    jdJobQueue,
    jdNodeProperties,
    jdTags,

    -- ** JobQueueDetail
    JobQueueDetail (..),
    mkJobQueueDetail,
    jqdStatus,
    jqdState,
    jqdPriority,
    jqdJobQueueARN,
    jqdComputeEnvironmentOrder,
    jqdStatusReason,
    jqdJobQueueName,
    jqdTags,

    -- ** JobSummary
    JobSummary (..),
    mkJobSummary,
    jsStoppedAt,
    jsStatus,
    jsJobId,
    jsJobARN,
    jsCreatedAt,
    jsJobName,
    jsStartedAt,
    jsContainer,
    jsStatusReason,
    jsArrayProperties,
    jsNodeProperties,

    -- ** JobTimeout
    JobTimeout (..),
    mkJobTimeout,
    jtAttemptDurationSeconds,

    -- ** KeyValuePair
    KeyValuePair (..),
    mkKeyValuePair,
    kvpValue,
    kvpName,

    -- ** LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    mkLaunchTemplateSpecification,
    ltsLaunchTemplateName,
    ltsLaunchTemplateId,
    ltsVersion,

    -- ** LinuxParameters
    LinuxParameters (..),
    mkLinuxParameters,
    lpSharedMemorySize,
    lpInitProcessEnabled,
    lpTmpfs,
    lpSwappiness,
    lpDevices,
    lpMaxSwap,

    -- ** LogConfiguration
    LogConfiguration (..),
    mkLogConfiguration,
    lcLogDriver,
    lcOptions,
    lcSecretOptions,

    -- ** MountPoint
    MountPoint (..),
    mkMountPoint,
    mpContainerPath,
    mpSourceVolume,
    mpReadOnly,

    -- ** NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niIpv6Address,
    niPrivateIPv4Address,
    niAttachmentId,

    -- ** NodeDetails
    NodeDetails (..),
    mkNodeDetails,
    ndNodeIndex,
    ndIsMainNode,

    -- ** NodeOverrides
    NodeOverrides (..),
    mkNodeOverrides,
    noNumNodes,
    noNodePropertyOverrides,

    -- ** NodeProperties
    NodeProperties (..),
    mkNodeProperties,
    npNumNodes,
    npMainNode,
    npNodeRangeProperties,

    -- ** NodePropertiesSummary
    NodePropertiesSummary (..),
    mkNodePropertiesSummary,
    npsNumNodes,
    npsNodeIndex,
    npsIsMainNode,

    -- ** NodePropertyOverride
    NodePropertyOverride (..),
    mkNodePropertyOverride,
    npoContainerOverrides,
    npoTargetNodes,

    -- ** NodeRangeProperty
    NodeRangeProperty (..),
    mkNodeRangeProperty,
    nrpContainer,
    nrpTargetNodes,

    -- ** ResourceRequirement
    ResourceRequirement (..),
    mkResourceRequirement,
    rrValue,
    rrType,

    -- ** RetryStrategy
    RetryStrategy (..),
    mkRetryStrategy,
    rsEvaluateOnExit,
    rsAttempts,

    -- ** Secret
    Secret (..),
    mkSecret,
    sName,
    sValueFrom,

    -- ** Tmpfs
    Tmpfs (..),
    mkTmpfs,
    tSize,
    tContainerPath,
    tMountOptions,

    -- ** Ulimit
    Ulimit (..),
    mkUlimit,
    uName,
    uHardLimit,
    uSoftLimit,

    -- ** Volume
    Volume (..),
    mkVolume,
    vName,
    vHost,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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
import qualified Network.AWS.Prelude as Lude

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
