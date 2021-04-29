{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Lens
  ( -- * Operations

    -- ** CreateComputeEnvironment
    createComputeEnvironment_state,
    createComputeEnvironment_computeResources,
    createComputeEnvironment_tags,
    createComputeEnvironment_computeEnvironmentName,
    createComputeEnvironment_type,
    createComputeEnvironment_serviceRole,
    createComputeEnvironmentResponse_computeEnvironmentName,
    createComputeEnvironmentResponse_computeEnvironmentArn,
    createComputeEnvironmentResponse_httpStatus,

    -- ** CancelJob
    cancelJob_jobId,
    cancelJob_reason,
    cancelJobResponse_httpStatus,

    -- ** DescribeJobs
    describeJobs_jobs,
    describeJobsResponse_jobs,
    describeJobsResponse_httpStatus,

    -- ** DeleteComputeEnvironment
    deleteComputeEnvironment_computeEnvironment,
    deleteComputeEnvironmentResponse_httpStatus,

    -- ** UpdateComputeEnvironment
    updateComputeEnvironment_serviceRole,
    updateComputeEnvironment_state,
    updateComputeEnvironment_computeResources,
    updateComputeEnvironment_computeEnvironment,
    updateComputeEnvironmentResponse_computeEnvironmentName,
    updateComputeEnvironmentResponse_computeEnvironmentArn,
    updateComputeEnvironmentResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeJobQueues
    describeJobQueues_nextToken,
    describeJobQueues_maxResults,
    describeJobQueues_jobQueues,
    describeJobQueuesResponse_nextToken,
    describeJobQueuesResponse_jobQueues,
    describeJobQueuesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DeregisterJobDefinition
    deregisterJobDefinition_jobDefinition,
    deregisterJobDefinitionResponse_httpStatus,

    -- ** DeleteJobQueue
    deleteJobQueue_jobQueue,
    deleteJobQueueResponse_httpStatus,

    -- ** UpdateJobQueue
    updateJobQueue_computeEnvironmentOrder,
    updateJobQueue_priority,
    updateJobQueue_state,
    updateJobQueue_jobQueue,
    updateJobQueueResponse_jobQueueName,
    updateJobQueueResponse_jobQueueArn,
    updateJobQueueResponse_httpStatus,

    -- ** DescribeJobDefinitions
    describeJobDefinitions_nextToken,
    describeJobDefinitions_status,
    describeJobDefinitions_jobDefinitions,
    describeJobDefinitions_maxResults,
    describeJobDefinitions_jobDefinitionName,
    describeJobDefinitionsResponse_nextToken,
    describeJobDefinitionsResponse_jobDefinitions,
    describeJobDefinitionsResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_maxResults,
    listJobs_jobQueue,
    listJobs_jobStatus,
    listJobs_arrayJobId,
    listJobs_multiNodeJobId,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,
    listJobsResponse_jobSummaryList,

    -- ** DescribeComputeEnvironments
    describeComputeEnvironments_nextToken,
    describeComputeEnvironments_maxResults,
    describeComputeEnvironments_computeEnvironments,
    describeComputeEnvironmentsResponse_nextToken,
    describeComputeEnvironmentsResponse_computeEnvironments,
    describeComputeEnvironmentsResponse_httpStatus,

    -- ** TerminateJob
    terminateJob_jobId,
    terminateJob_reason,
    terminateJobResponse_httpStatus,

    -- ** RegisterJobDefinition
    registerJobDefinition_platformCapabilities,
    registerJobDefinition_timeout,
    registerJobDefinition_nodeProperties,
    registerJobDefinition_tags,
    registerJobDefinition_containerProperties,
    registerJobDefinition_retryStrategy,
    registerJobDefinition_parameters,
    registerJobDefinition_propagateTags,
    registerJobDefinition_jobDefinitionName,
    registerJobDefinition_type,
    registerJobDefinitionResponse_httpStatus,
    registerJobDefinitionResponse_jobDefinitionName,
    registerJobDefinitionResponse_jobDefinitionArn,
    registerJobDefinitionResponse_revision,

    -- ** SubmitJob
    submitJob_dependsOn,
    submitJob_timeout,
    submitJob_arrayProperties,
    submitJob_containerOverrides,
    submitJob_nodeOverrides,
    submitJob_tags,
    submitJob_retryStrategy,
    submitJob_parameters,
    submitJob_propagateTags,
    submitJob_jobName,
    submitJob_jobQueue,
    submitJob_jobDefinition,
    submitJobResponse_jobArn,
    submitJobResponse_httpStatus,
    submitJobResponse_jobName,
    submitJobResponse_jobId,

    -- ** CreateJobQueue
    createJobQueue_state,
    createJobQueue_tags,
    createJobQueue_jobQueueName,
    createJobQueue_priority,
    createJobQueue_computeEnvironmentOrder,
    createJobQueueResponse_httpStatus,
    createJobQueueResponse_jobQueueName,
    createJobQueueResponse_jobQueueArn,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** ArrayProperties
    arrayProperties_size,

    -- ** ArrayPropertiesDetail
    arrayPropertiesDetail_index,
    arrayPropertiesDetail_statusSummary,
    arrayPropertiesDetail_size,

    -- ** ArrayPropertiesSummary
    arrayPropertiesSummary_index,
    arrayPropertiesSummary_size,

    -- ** AttemptContainerDetail
    attemptContainerDetail_logStreamName,
    attemptContainerDetail_containerInstanceArn,
    attemptContainerDetail_exitCode,
    attemptContainerDetail_reason,
    attemptContainerDetail_taskArn,
    attemptContainerDetail_networkInterfaces,

    -- ** AttemptDetail
    attemptDetail_container,
    attemptDetail_startedAt,
    attemptDetail_stoppedAt,
    attemptDetail_statusReason,

    -- ** ComputeEnvironmentDetail
    computeEnvironmentDetail_status,
    computeEnvironmentDetail_serviceRole,
    computeEnvironmentDetail_state,
    computeEnvironmentDetail_computeResources,
    computeEnvironmentDetail_tags,
    computeEnvironmentDetail_statusReason,
    computeEnvironmentDetail_type,
    computeEnvironmentDetail_computeEnvironmentName,
    computeEnvironmentDetail_computeEnvironmentArn,
    computeEnvironmentDetail_ecsClusterArn,

    -- ** ComputeEnvironmentOrder
    computeEnvironmentOrder_order,
    computeEnvironmentOrder_computeEnvironment,

    -- ** ComputeResource
    computeResource_securityGroupIds,
    computeResource_bidPercentage,
    computeResource_minvCpus,
    computeResource_ec2KeyPair,
    computeResource_placementGroup,
    computeResource_launchTemplate,
    computeResource_imageId,
    computeResource_spotIamFleetRole,
    computeResource_ec2Configuration,
    computeResource_tags,
    computeResource_desiredvCpus,
    computeResource_allocationStrategy,
    computeResource_instanceRole,
    computeResource_instanceTypes,
    computeResource_type,
    computeResource_maxvCpus,
    computeResource_subnets,

    -- ** ComputeResourceUpdate
    computeResourceUpdate_securityGroupIds,
    computeResourceUpdate_minvCpus,
    computeResourceUpdate_maxvCpus,
    computeResourceUpdate_desiredvCpus,
    computeResourceUpdate_subnets,

    -- ** ContainerDetail
    containerDetail_logStreamName,
    containerDetail_linuxParameters,
    containerDetail_memory,
    containerDetail_user,
    containerDetail_instanceType,
    containerDetail_networkConfiguration,
    containerDetail_executionRoleArn,
    containerDetail_privileged,
    containerDetail_vcpus,
    containerDetail_containerInstanceArn,
    containerDetail_volumes,
    containerDetail_environment,
    containerDetail_fargatePlatformConfiguration,
    containerDetail_exitCode,
    containerDetail_secrets,
    containerDetail_mountPoints,
    containerDetail_image,
    containerDetail_command,
    containerDetail_logConfiguration,
    containerDetail_reason,
    containerDetail_resourceRequirements,
    containerDetail_jobRoleArn,
    containerDetail_readonlyRootFilesystem,
    containerDetail_ulimits,
    containerDetail_taskArn,
    containerDetail_networkInterfaces,

    -- ** ContainerOverrides
    containerOverrides_memory,
    containerOverrides_instanceType,
    containerOverrides_vcpus,
    containerOverrides_environment,
    containerOverrides_command,
    containerOverrides_resourceRequirements,

    -- ** ContainerProperties
    containerProperties_linuxParameters,
    containerProperties_memory,
    containerProperties_user,
    containerProperties_instanceType,
    containerProperties_networkConfiguration,
    containerProperties_executionRoleArn,
    containerProperties_privileged,
    containerProperties_vcpus,
    containerProperties_volumes,
    containerProperties_environment,
    containerProperties_fargatePlatformConfiguration,
    containerProperties_secrets,
    containerProperties_mountPoints,
    containerProperties_image,
    containerProperties_command,
    containerProperties_logConfiguration,
    containerProperties_resourceRequirements,
    containerProperties_jobRoleArn,
    containerProperties_readonlyRootFilesystem,
    containerProperties_ulimits,

    -- ** ContainerSummary
    containerSummary_exitCode,
    containerSummary_reason,

    -- ** Device
    device_permissions,
    device_containerPath,
    device_hostPath,

    -- ** Ec2Configuration
    ec2Configuration_imageIdOverride,
    ec2Configuration_imageType,

    -- ** EvaluateOnExit
    evaluateOnExit_onExitCode,
    evaluateOnExit_onStatusReason,
    evaluateOnExit_onReason,
    evaluateOnExit_action,

    -- ** FargatePlatformConfiguration
    fargatePlatformConfiguration_platformVersion,

    -- ** Host
    host_sourcePath,

    -- ** JobDefinition
    jobDefinition_status,
    jobDefinition_platformCapabilities,
    jobDefinition_timeout,
    jobDefinition_nodeProperties,
    jobDefinition_tags,
    jobDefinition_containerProperties,
    jobDefinition_retryStrategy,
    jobDefinition_parameters,
    jobDefinition_propagateTags,
    jobDefinition_jobDefinitionName,
    jobDefinition_jobDefinitionArn,
    jobDefinition_revision,
    jobDefinition_type,

    -- ** JobDependency
    jobDependency_type,
    jobDependency_jobId,

    -- ** JobDetail
    jobDetail_container,
    jobDetail_startedAt,
    jobDetail_dependsOn,
    jobDetail_platformCapabilities,
    jobDetail_timeout,
    jobDetail_arrayProperties,
    jobDetail_createdAt,
    jobDetail_jobArn,
    jobDetail_nodeDetails,
    jobDetail_stoppedAt,
    jobDetail_nodeProperties,
    jobDetail_tags,
    jobDetail_attempts,
    jobDetail_retryStrategy,
    jobDetail_statusReason,
    jobDetail_parameters,
    jobDetail_propagateTags,
    jobDetail_jobName,
    jobDetail_jobId,
    jobDetail_jobQueue,
    jobDetail_status,
    jobDetail_jobDefinition,

    -- ** JobQueueDetail
    jobQueueDetail_status,
    jobQueueDetail_tags,
    jobQueueDetail_statusReason,
    jobQueueDetail_jobQueueName,
    jobQueueDetail_jobQueueArn,
    jobQueueDetail_state,
    jobQueueDetail_priority,
    jobQueueDetail_computeEnvironmentOrder,

    -- ** JobSummary
    jobSummary_container,
    jobSummary_startedAt,
    jobSummary_status,
    jobSummary_arrayProperties,
    jobSummary_createdAt,
    jobSummary_jobArn,
    jobSummary_stoppedAt,
    jobSummary_nodeProperties,
    jobSummary_statusReason,
    jobSummary_jobId,
    jobSummary_jobName,

    -- ** JobTimeout
    jobTimeout_attemptDurationSeconds,

    -- ** KeyValuePair
    keyValuePair_name,
    keyValuePair_value,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_version,

    -- ** LinuxParameters
    linuxParameters_tmpfs,
    linuxParameters_maxSwap,
    linuxParameters_devices,
    linuxParameters_swappiness,
    linuxParameters_initProcessEnabled,
    linuxParameters_sharedMemorySize,

    -- ** LogConfiguration
    logConfiguration_options,
    logConfiguration_secretOptions,
    logConfiguration_logDriver,

    -- ** MountPoint
    mountPoint_readOnly,
    mountPoint_sourceVolume,
    mountPoint_containerPath,

    -- ** NetworkConfiguration
    networkConfiguration_assignPublicIp,

    -- ** NetworkInterface
    networkInterface_privateIpv4Address,
    networkInterface_ipv6Address,
    networkInterface_attachmentId,

    -- ** NodeDetails
    nodeDetails_isMainNode,
    nodeDetails_nodeIndex,

    -- ** NodeOverrides
    nodeOverrides_nodePropertyOverrides,
    nodeOverrides_numNodes,

    -- ** NodeProperties
    nodeProperties_numNodes,
    nodeProperties_mainNode,
    nodeProperties_nodeRangeProperties,

    -- ** NodePropertiesSummary
    nodePropertiesSummary_isMainNode,
    nodePropertiesSummary_nodeIndex,
    nodePropertiesSummary_numNodes,

    -- ** NodePropertyOverride
    nodePropertyOverride_containerOverrides,
    nodePropertyOverride_targetNodes,

    -- ** NodeRangeProperty
    nodeRangeProperty_container,
    nodeRangeProperty_targetNodes,

    -- ** ResourceRequirement
    resourceRequirement_value,
    resourceRequirement_type,

    -- ** RetryStrategy
    retryStrategy_evaluateOnExit,
    retryStrategy_attempts,

    -- ** Secret
    secret_name,
    secret_valueFrom,

    -- ** Tmpfs
    tmpfs_mountOptions,
    tmpfs_containerPath,
    tmpfs_size,

    -- ** Ulimit
    ulimit_hardLimit,
    ulimit_name,
    ulimit_softLimit,

    -- ** Volume
    volume_name,
    volume_host,
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
import Network.AWS.Batch.Types.ArrayProperties
import Network.AWS.Batch.Types.ArrayPropertiesDetail
import Network.AWS.Batch.Types.ArrayPropertiesSummary
import Network.AWS.Batch.Types.AttemptContainerDetail
import Network.AWS.Batch.Types.AttemptDetail
import Network.AWS.Batch.Types.ComputeEnvironmentDetail
import Network.AWS.Batch.Types.ComputeEnvironmentOrder
import Network.AWS.Batch.Types.ComputeResource
import Network.AWS.Batch.Types.ComputeResourceUpdate
import Network.AWS.Batch.Types.ContainerDetail
import Network.AWS.Batch.Types.ContainerOverrides
import Network.AWS.Batch.Types.ContainerProperties
import Network.AWS.Batch.Types.ContainerSummary
import Network.AWS.Batch.Types.Device
import Network.AWS.Batch.Types.Ec2Configuration
import Network.AWS.Batch.Types.EvaluateOnExit
import Network.AWS.Batch.Types.FargatePlatformConfiguration
import Network.AWS.Batch.Types.Host
import Network.AWS.Batch.Types.JobDefinition
import Network.AWS.Batch.Types.JobDependency
import Network.AWS.Batch.Types.JobDetail
import Network.AWS.Batch.Types.JobQueueDetail
import Network.AWS.Batch.Types.JobSummary
import Network.AWS.Batch.Types.JobTimeout
import Network.AWS.Batch.Types.KeyValuePair
import Network.AWS.Batch.Types.LaunchTemplateSpecification
import Network.AWS.Batch.Types.LinuxParameters
import Network.AWS.Batch.Types.LogConfiguration
import Network.AWS.Batch.Types.MountPoint
import Network.AWS.Batch.Types.NetworkConfiguration
import Network.AWS.Batch.Types.NetworkInterface
import Network.AWS.Batch.Types.NodeDetails
import Network.AWS.Batch.Types.NodeOverrides
import Network.AWS.Batch.Types.NodeProperties
import Network.AWS.Batch.Types.NodePropertiesSummary
import Network.AWS.Batch.Types.NodePropertyOverride
import Network.AWS.Batch.Types.NodeRangeProperty
import Network.AWS.Batch.Types.ResourceRequirement
import Network.AWS.Batch.Types.RetryStrategy
import Network.AWS.Batch.Types.Secret
import Network.AWS.Batch.Types.Tmpfs
import Network.AWS.Batch.Types.Ulimit
import Network.AWS.Batch.Types.Volume
import Network.AWS.Batch.UntagResource
import Network.AWS.Batch.UpdateComputeEnvironment
import Network.AWS.Batch.UpdateJobQueue
