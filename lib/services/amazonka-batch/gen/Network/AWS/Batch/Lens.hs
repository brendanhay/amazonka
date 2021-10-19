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
    createComputeEnvironment_serviceRole,
    createComputeEnvironment_tags,
    createComputeEnvironment_computeEnvironmentName,
    createComputeEnvironment_type,
    createComputeEnvironmentResponse_computeEnvironmentName,
    createComputeEnvironmentResponse_computeEnvironmentArn,
    createComputeEnvironmentResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RegisterJobDefinition
    registerJobDefinition_propagateTags,
    registerJobDefinition_retryStrategy,
    registerJobDefinition_platformCapabilities,
    registerJobDefinition_parameters,
    registerJobDefinition_timeout,
    registerJobDefinition_containerProperties,
    registerJobDefinition_nodeProperties,
    registerJobDefinition_tags,
    registerJobDefinition_jobDefinitionName,
    registerJobDefinition_type,
    registerJobDefinitionResponse_httpStatus,
    registerJobDefinitionResponse_jobDefinitionName,
    registerJobDefinitionResponse_jobDefinitionArn,
    registerJobDefinitionResponse_revision,

    -- ** SubmitJob
    submitJob_nodeOverrides,
    submitJob_propagateTags,
    submitJob_containerOverrides,
    submitJob_retryStrategy,
    submitJob_dependsOn,
    submitJob_parameters,
    submitJob_arrayProperties,
    submitJob_timeout,
    submitJob_tags,
    submitJob_jobName,
    submitJob_jobQueue,
    submitJob_jobDefinition,
    submitJobResponse_jobArn,
    submitJobResponse_httpStatus,
    submitJobResponse_jobName,
    submitJobResponse_jobId,

    -- ** ListJobs
    listJobs_filters,
    listJobs_nextToken,
    listJobs_multiNodeJobId,
    listJobs_jobStatus,
    listJobs_arrayJobId,
    listJobs_jobQueue,
    listJobs_maxResults,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,
    listJobsResponse_jobSummaryList,

    -- ** TerminateJob
    terminateJob_jobId,
    terminateJob_reason,
    terminateJobResponse_httpStatus,

    -- ** DescribeJobs
    describeJobs_jobs,
    describeJobsResponse_jobs,
    describeJobsResponse_httpStatus,

    -- ** DeleteComputeEnvironment
    deleteComputeEnvironment_computeEnvironment,
    deleteComputeEnvironmentResponse_httpStatus,

    -- ** UpdateComputeEnvironment
    updateComputeEnvironment_state,
    updateComputeEnvironment_computeResources,
    updateComputeEnvironment_serviceRole,
    updateComputeEnvironment_computeEnvironment,
    updateComputeEnvironmentResponse_computeEnvironmentName,
    updateComputeEnvironmentResponse_computeEnvironmentArn,
    updateComputeEnvironmentResponse_httpStatus,

    -- ** DescribeJobDefinitions
    describeJobDefinitions_status,
    describeJobDefinitions_jobDefinitionName,
    describeJobDefinitions_jobDefinitions,
    describeJobDefinitions_nextToken,
    describeJobDefinitions_maxResults,
    describeJobDefinitionsResponse_jobDefinitions,
    describeJobDefinitionsResponse_nextToken,
    describeJobDefinitionsResponse_httpStatus,

    -- ** UpdateJobQueue
    updateJobQueue_state,
    updateJobQueue_priority,
    updateJobQueue_computeEnvironmentOrder,
    updateJobQueue_jobQueue,
    updateJobQueueResponse_jobQueueArn,
    updateJobQueueResponse_jobQueueName,
    updateJobQueueResponse_httpStatus,

    -- ** DeleteJobQueue
    deleteJobQueue_jobQueue,
    deleteJobQueueResponse_httpStatus,

    -- ** CreateJobQueue
    createJobQueue_state,
    createJobQueue_tags,
    createJobQueue_jobQueueName,
    createJobQueue_priority,
    createJobQueue_computeEnvironmentOrder,
    createJobQueueResponse_httpStatus,
    createJobQueueResponse_jobQueueName,
    createJobQueueResponse_jobQueueArn,

    -- ** DeregisterJobDefinition
    deregisterJobDefinition_jobDefinition,
    deregisterJobDefinitionResponse_httpStatus,

    -- ** DescribeJobQueues
    describeJobQueues_nextToken,
    describeJobQueues_jobQueues,
    describeJobQueues_maxResults,
    describeJobQueuesResponse_nextToken,
    describeJobQueuesResponse_jobQueues,
    describeJobQueuesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeComputeEnvironments
    describeComputeEnvironments_computeEnvironments,
    describeComputeEnvironments_nextToken,
    describeComputeEnvironments_maxResults,
    describeComputeEnvironmentsResponse_computeEnvironments,
    describeComputeEnvironmentsResponse_nextToken,
    describeComputeEnvironmentsResponse_httpStatus,

    -- ** CancelJob
    cancelJob_jobId,
    cancelJob_reason,
    cancelJobResponse_httpStatus,

    -- * Types

    -- ** ArrayProperties
    arrayProperties_size,

    -- ** ArrayPropertiesDetail
    arrayPropertiesDetail_size,
    arrayPropertiesDetail_statusSummary,
    arrayPropertiesDetail_index,

    -- ** ArrayPropertiesSummary
    arrayPropertiesSummary_size,
    arrayPropertiesSummary_index,

    -- ** AttemptContainerDetail
    attemptContainerDetail_networkInterfaces,
    attemptContainerDetail_taskArn,
    attemptContainerDetail_containerInstanceArn,
    attemptContainerDetail_reason,
    attemptContainerDetail_logStreamName,
    attemptContainerDetail_exitCode,

    -- ** AttemptDetail
    attemptDetail_stoppedAt,
    attemptDetail_startedAt,
    attemptDetail_container,
    attemptDetail_statusReason,

    -- ** ComputeEnvironmentDetail
    computeEnvironmentDetail_status,
    computeEnvironmentDetail_state,
    computeEnvironmentDetail_computeResources,
    computeEnvironmentDetail_statusReason,
    computeEnvironmentDetail_type,
    computeEnvironmentDetail_serviceRole,
    computeEnvironmentDetail_tags,
    computeEnvironmentDetail_computeEnvironmentName,
    computeEnvironmentDetail_computeEnvironmentArn,
    computeEnvironmentDetail_ecsClusterArn,

    -- ** ComputeEnvironmentOrder
    computeEnvironmentOrder_order,
    computeEnvironmentOrder_computeEnvironment,

    -- ** ComputeResource
    computeResource_securityGroupIds,
    computeResource_instanceTypes,
    computeResource_instanceRole,
    computeResource_ec2KeyPair,
    computeResource_minvCpus,
    computeResource_ec2Configuration,
    computeResource_bidPercentage,
    computeResource_spotIamFleetRole,
    computeResource_imageId,
    computeResource_launchTemplate,
    computeResource_desiredvCpus,
    computeResource_allocationStrategy,
    computeResource_placementGroup,
    computeResource_tags,
    computeResource_type,
    computeResource_maxvCpus,
    computeResource_subnets,

    -- ** ComputeResourceUpdate
    computeResourceUpdate_securityGroupIds,
    computeResourceUpdate_subnets,
    computeResourceUpdate_minvCpus,
    computeResourceUpdate_maxvCpus,
    computeResourceUpdate_desiredvCpus,

    -- ** ContainerDetail
    containerDetail_image,
    containerDetail_command,
    containerDetail_secrets,
    containerDetail_environment,
    containerDetail_networkInterfaces,
    containerDetail_taskArn,
    containerDetail_ulimits,
    containerDetail_containerInstanceArn,
    containerDetail_executionRoleArn,
    containerDetail_privileged,
    containerDetail_jobRoleArn,
    containerDetail_resourceRequirements,
    containerDetail_instanceType,
    containerDetail_memory,
    containerDetail_user,
    containerDetail_logConfiguration,
    containerDetail_linuxParameters,
    containerDetail_reason,
    containerDetail_logStreamName,
    containerDetail_mountPoints,
    containerDetail_exitCode,
    containerDetail_fargatePlatformConfiguration,
    containerDetail_vcpus,
    containerDetail_readonlyRootFilesystem,
    containerDetail_volumes,
    containerDetail_networkConfiguration,

    -- ** ContainerOverrides
    containerOverrides_command,
    containerOverrides_environment,
    containerOverrides_resourceRequirements,
    containerOverrides_instanceType,
    containerOverrides_memory,
    containerOverrides_vcpus,

    -- ** ContainerProperties
    containerProperties_image,
    containerProperties_command,
    containerProperties_secrets,
    containerProperties_environment,
    containerProperties_ulimits,
    containerProperties_executionRoleArn,
    containerProperties_privileged,
    containerProperties_jobRoleArn,
    containerProperties_resourceRequirements,
    containerProperties_instanceType,
    containerProperties_memory,
    containerProperties_user,
    containerProperties_logConfiguration,
    containerProperties_linuxParameters,
    containerProperties_mountPoints,
    containerProperties_fargatePlatformConfiguration,
    containerProperties_vcpus,
    containerProperties_readonlyRootFilesystem,
    containerProperties_volumes,
    containerProperties_networkConfiguration,

    -- ** ContainerSummary
    containerSummary_reason,
    containerSummary_exitCode,

    -- ** Device
    device_containerPath,
    device_permissions,
    device_hostPath,

    -- ** EFSAuthorizationConfig
    eFSAuthorizationConfig_accessPointId,
    eFSAuthorizationConfig_iam,

    -- ** EFSVolumeConfiguration
    eFSVolumeConfiguration_rootDirectory,
    eFSVolumeConfiguration_transitEncryption,
    eFSVolumeConfiguration_authorizationConfig,
    eFSVolumeConfiguration_transitEncryptionPort,
    eFSVolumeConfiguration_fileSystemId,

    -- ** Ec2Configuration
    ec2Configuration_imageIdOverride,
    ec2Configuration_imageType,

    -- ** EvaluateOnExit
    evaluateOnExit_onExitCode,
    evaluateOnExit_onReason,
    evaluateOnExit_onStatusReason,
    evaluateOnExit_action,

    -- ** FargatePlatformConfiguration
    fargatePlatformConfiguration_platformVersion,

    -- ** Host
    host_sourcePath,

    -- ** JobDefinition
    jobDefinition_status,
    jobDefinition_propagateTags,
    jobDefinition_retryStrategy,
    jobDefinition_platformCapabilities,
    jobDefinition_parameters,
    jobDefinition_timeout,
    jobDefinition_containerProperties,
    jobDefinition_nodeProperties,
    jobDefinition_tags,
    jobDefinition_jobDefinitionName,
    jobDefinition_jobDefinitionArn,
    jobDefinition_revision,
    jobDefinition_type,

    -- ** JobDependency
    jobDependency_jobId,
    jobDependency_type,

    -- ** JobDetail
    jobDetail_stoppedAt,
    jobDetail_jobArn,
    jobDetail_propagateTags,
    jobDetail_createdAt,
    jobDetail_retryStrategy,
    jobDetail_attempts,
    jobDetail_platformCapabilities,
    jobDetail_startedAt,
    jobDetail_dependsOn,
    jobDetail_container,
    jobDetail_nodeDetails,
    jobDetail_parameters,
    jobDetail_statusReason,
    jobDetail_arrayProperties,
    jobDetail_timeout,
    jobDetail_nodeProperties,
    jobDetail_tags,
    jobDetail_jobName,
    jobDetail_jobId,
    jobDetail_jobQueue,
    jobDetail_status,
    jobDetail_jobDefinition,

    -- ** JobQueueDetail
    jobQueueDetail_status,
    jobQueueDetail_statusReason,
    jobQueueDetail_tags,
    jobQueueDetail_jobQueueName,
    jobQueueDetail_jobQueueArn,
    jobQueueDetail_state,
    jobQueueDetail_priority,
    jobQueueDetail_computeEnvironmentOrder,

    -- ** JobSummary
    jobSummary_stoppedAt,
    jobSummary_status,
    jobSummary_jobArn,
    jobSummary_createdAt,
    jobSummary_startedAt,
    jobSummary_container,
    jobSummary_jobDefinition,
    jobSummary_statusReason,
    jobSummary_arrayProperties,
    jobSummary_nodeProperties,
    jobSummary_jobId,
    jobSummary_jobName,

    -- ** JobTimeout
    jobTimeout_attemptDurationSeconds,

    -- ** KeyValuePair
    keyValuePair_value,
    keyValuePair_name,

    -- ** KeyValuesPair
    keyValuesPair_values,
    keyValuesPair_name,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,

    -- ** LinuxParameters
    linuxParameters_sharedMemorySize,
    linuxParameters_initProcessEnabled,
    linuxParameters_tmpfs,
    linuxParameters_swappiness,
    linuxParameters_devices,
    linuxParameters_maxSwap,

    -- ** LogConfiguration
    logConfiguration_options,
    logConfiguration_secretOptions,
    logConfiguration_logDriver,

    -- ** MountPoint
    mountPoint_containerPath,
    mountPoint_sourceVolume,
    mountPoint_readOnly,

    -- ** NetworkConfiguration
    networkConfiguration_assignPublicIp,

    -- ** NetworkInterface
    networkInterface_ipv6Address,
    networkInterface_privateIpv4Address,
    networkInterface_attachmentId,

    -- ** NodeDetails
    nodeDetails_nodeIndex,
    nodeDetails_isMainNode,

    -- ** NodeOverrides
    nodeOverrides_numNodes,
    nodeOverrides_nodePropertyOverrides,

    -- ** NodeProperties
    nodeProperties_numNodes,
    nodeProperties_mainNode,
    nodeProperties_nodeRangeProperties,

    -- ** NodePropertiesSummary
    nodePropertiesSummary_numNodes,
    nodePropertiesSummary_nodeIndex,
    nodePropertiesSummary_isMainNode,

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
    volume_efsVolumeConfiguration,
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
import Network.AWS.Batch.Types.EFSAuthorizationConfig
import Network.AWS.Batch.Types.EFSVolumeConfiguration
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
import Network.AWS.Batch.Types.KeyValuesPair
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
