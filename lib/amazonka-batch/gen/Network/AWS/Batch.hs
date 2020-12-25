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
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** ServerException
    _ServerException,

    -- ** ClientException
    _ClientException,

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

    -- ** AttemptDetail
    AttemptDetail (..),
    mkAttemptDetail,
    adContainer,
    adStartedAt,
    adStatusReason,
    adStoppedAt,

    -- ** NodeOverrides
    NodeOverrides (..),
    mkNodeOverrides,
    noNodePropertyOverrides,
    noNumNodes,

    -- ** JQStatus
    JQStatus (..),

    -- ** CRAllocationStrategy
    CRAllocationStrategy (..),

    -- ** JobDefinitionType
    JobDefinitionType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** EvaluateOnExit
    EvaluateOnExit (..),
    mkEvaluateOnExit,
    eoeAction,
    eoeOnExitCode,
    eoeOnReason,
    eoeOnStatusReason,

    -- ** NodePropertiesSummary
    NodePropertiesSummary (..),
    mkNodePropertiesSummary,
    npsIsMainNode,
    npsNodeIndex,
    npsNumNodes,

    -- ** String
    String (..),

    -- ** JobDependency
    JobDependency (..),
    mkJobDependency,
    jJobId,
    jType,

    -- ** ContainerOverrides
    ContainerOverrides (..),
    mkContainerOverrides,
    coCommand,
    coEnvironment,
    coInstanceType,
    coMemory,
    coResourceRequirements,
    coVcpus,

    -- ** Device
    Device (..),
    mkDevice,
    dHostPath,
    dContainerPath,
    dPermissions,

    -- ** Volume
    Volume (..),
    mkVolume,
    vHost,
    vName,

    -- ** ContainerSummary
    ContainerSummary (..),
    mkContainerSummary,
    csExitCode,
    csReason,

    -- ** NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niAttachmentId,
    niIpv6Address,
    niPrivateIpv4Address,

    -- ** RetryStrategy
    RetryStrategy (..),
    mkRetryStrategy,
    rsAttempts,
    rsEvaluateOnExit,

    -- ** AttemptContainerDetail
    AttemptContainerDetail (..),
    mkAttemptContainerDetail,
    acdContainerInstanceArn,
    acdExitCode,
    acdLogStreamName,
    acdNetworkInterfaces,
    acdReason,
    acdTaskArn,

    -- ** JobSummary
    JobSummary (..),
    mkJobSummary,
    jsJobId,
    jsJobName,
    jsArrayProperties,
    jsContainer,
    jsCreatedAt,
    jsJobArn,
    jsNodeProperties,
    jsStartedAt,
    jsStatus,
    jsStatusReason,
    jsStoppedAt,

    -- ** KeyValuePair
    KeyValuePair (..),
    mkKeyValuePair,
    kvpName,
    kvpValue,

    -- ** ComputeEnvironmentOrder
    ComputeEnvironmentOrder (..),
    mkComputeEnvironmentOrder,
    ceoOrder,
    ceoComputeEnvironment,

    -- ** Secret
    Secret (..),
    mkSecret,
    sName,
    sValueFrom,

    -- ** RetryAction
    RetryAction (..),

    -- ** TagValue
    TagValue (..),

    -- ** ComputeEnvironmentDetail
    ComputeEnvironmentDetail (..),
    mkComputeEnvironmentDetail,
    cedComputeEnvironmentName,
    cedComputeEnvironmentArn,
    cedEcsClusterArn,
    cedComputeResources,
    cedServiceRole,
    cedState,
    cedStatus,
    cedStatusReason,
    cedTags,
    cedType,

    -- ** CRType
    CRType (..),

    -- ** ArrayPropertiesDetail
    ArrayPropertiesDetail (..),
    mkArrayPropertiesDetail,
    apdIndex,
    apdSize,
    apdStatusSummary,

    -- ** LinuxParameters
    LinuxParameters (..),
    mkLinuxParameters,
    lpDevices,
    lpInitProcessEnabled,
    lpMaxSwap,
    lpSharedMemorySize,
    lpSwappiness,
    lpTmpfs,

    -- ** Tmpfs
    Tmpfs (..),
    mkTmpfs,
    tContainerPath,
    tSize,
    tMountOptions,

    -- ** LogConfiguration
    LogConfiguration (..),
    mkLogConfiguration,
    lcLogDriver,
    lcOptions,
    lcSecretOptions,

    -- ** JobTimeout
    JobTimeout (..),
    mkJobTimeout,
    jtAttemptDurationSeconds,

    -- ** JQState
    JQState (..),

    -- ** Ec2Configuration
    Ec2Configuration (..),
    mkEc2Configuration,
    ecImageType,
    ecImageIdOverride,

    -- ** ArrayPropertiesSummary
    ArrayPropertiesSummary (..),
    mkArrayPropertiesSummary,
    apsIndex,
    apsSize,

    -- ** JobQueueDetail
    JobQueueDetail (..),
    mkJobQueueDetail,
    jqdJobQueueName,
    jqdJobQueueArn,
    jqdState,
    jqdPriority,
    jqdComputeEnvironmentOrder,
    jqdStatus,
    jqdStatusReason,
    jqdTags,

    -- ** ComputeResource
    ComputeResource (..),
    mkComputeResource,
    crType,
    crMinvCpus,
    crMaxvCpus,
    crInstanceTypes,
    crSubnets,
    crInstanceRole,
    crAllocationStrategy,
    crBidPercentage,
    crDesiredvCpus,
    crEc2Configuration,
    crEc2KeyPair,
    crImageId,
    crLaunchTemplate,
    crPlacementGroup,
    crSecurityGroupIds,
    crSpotIamFleetRole,
    crTags,

    -- ** ImageType
    ImageType (..),

    -- ** DeviceCgroupPermission
    DeviceCgroupPermission (..),

    -- ** CEStatus
    CEStatus (..),

    -- ** NodeDetails
    NodeDetails (..),
    mkNodeDetails,
    ndIsMainNode,
    ndNodeIndex,

    -- ** LogDriver
    LogDriver (..),

    -- ** JobDefinition
    JobDefinition (..),
    mkJobDefinition,
    jdfJobDefinitionName,
    jdfJobDefinitionArn,
    jdfRevision,
    jdfType,
    jdfContainerProperties,
    jdfNodeProperties,
    jdfParameters,
    jdfRetryStrategy,
    jdfStatus,
    jdfTags,
    jdfTimeout,

    -- ** ResourceRequirement
    ResourceRequirement (..),
    mkResourceRequirement,
    rrValue,
    rrType,

    -- ** TagKey
    TagKey (..),

    -- ** Ulimit
    Ulimit (..),
    mkUlimit,
    uHardLimit,
    uName,
    uSoftLimit,

    -- ** LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    mkLaunchTemplateSpecification,
    ltsLaunchTemplateId,
    ltsLaunchTemplateName,
    ltsVersion,

    -- ** Host
    Host (..),
    mkHost,
    hSourcePath,

    -- ** NodeRangeProperty
    NodeRangeProperty (..),
    mkNodeRangeProperty,
    nrpTargetNodes,
    nrpContainer,

    -- ** ArrayProperties
    ArrayProperties (..),
    mkArrayProperties,
    apSize,

    -- ** JobStatus
    JobStatus (..),

    -- ** NodePropertyOverride
    NodePropertyOverride (..),
    mkNodePropertyOverride,
    npoTargetNodes,
    npoContainerOverrides,

    -- ** ContainerProperties
    ContainerProperties (..),
    mkContainerProperties,
    cpCommand,
    cpEnvironment,
    cpExecutionRoleArn,
    cpImage,
    cpInstanceType,
    cpJobRoleArn,
    cpLinuxParameters,
    cpLogConfiguration,
    cpMemory,
    cpMountPoints,
    cpPrivileged,
    cpReadonlyRootFilesystem,
    cpResourceRequirements,
    cpSecrets,
    cpUlimits,
    cpUser,
    cpVcpus,
    cpVolumes,

    -- ** ContainerDetail
    ContainerDetail (..),
    mkContainerDetail,
    cdCommand,
    cdContainerInstanceArn,
    cdEnvironment,
    cdExecutionRoleArn,
    cdExitCode,
    cdImage,
    cdInstanceType,
    cdJobRoleArn,
    cdLinuxParameters,
    cdLogConfiguration,
    cdLogStreamName,
    cdMemory,
    cdMountPoints,
    cdNetworkInterfaces,
    cdPrivileged,
    cdReadonlyRootFilesystem,
    cdReason,
    cdResourceRequirements,
    cdSecrets,
    cdTaskArn,
    cdUlimits,
    cdUser,
    cdVcpus,
    cdVolumes,

    -- ** CEType
    CEType (..),

    -- ** JobDetail
    JobDetail (..),
    mkJobDetail,
    jdJobName,
    jdJobId,
    jdJobQueue,
    jdStatus,
    jdStartedAt,
    jdJobDefinition,
    jdArrayProperties,
    jdAttempts,
    jdContainer,
    jdCreatedAt,
    jdDependsOn,
    jdJobArn,
    jdNodeDetails,
    jdNodeProperties,
    jdParameters,
    jdRetryStrategy,
    jdStatusReason,
    jdStoppedAt,
    jdTags,
    jdTimeout,

    -- ** ComputeResourceUpdate
    ComputeResourceUpdate (..),
    mkComputeResourceUpdate,
    cruDesiredvCpus,
    cruMaxvCpus,
    cruMinvCpus,

    -- ** ImageIdOverride
    ImageIdOverride (..),

    -- ** NodeProperties
    NodeProperties (..),
    mkNodeProperties,
    npNumNodes,
    npMainNode,
    npNodeRangeProperties,

    -- ** ArrayJobDependency
    ArrayJobDependency (..),

    -- ** CEState
    CEState (..),

    -- ** MountPoint
    MountPoint (..),
    mkMountPoint,
    mpContainerPath,
    mpReadOnly,
    mpSourceVolume,

    -- ** StatusReason
    StatusReason (..),

    -- ** ComputeEnvironmentArn
    ComputeEnvironmentArn (..),

    -- ** ComputeEnvironmentName
    ComputeEnvironmentName (..),

    -- ** JobId
    JobId (..),

    -- ** Reason
    Reason (..),

    -- ** ArrayJobId
    ArrayJobId (..),

    -- ** JobQueue
    JobQueue (..),

    -- ** MultiNodeJobId
    MultiNodeJobId (..),

    -- ** NextToken
    NextToken (..),

    -- ** JobName
    JobName (..),

    -- ** JobDefinitionName
    JobDefinitionName (..),

    -- ** OnExitCode
    OnExitCode (..),

    -- ** OnReason
    OnReason (..),

    -- ** OnStatusReason
    OnStatusReason (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
