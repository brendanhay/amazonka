{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Batch.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Lens
  ( -- * Operations

    -- ** CancelJob
    cancelJob_jobId,
    cancelJob_reason,
    cancelJobResponse_httpStatus,

    -- ** CreateComputeEnvironment
    createComputeEnvironment_computeResources,
    createComputeEnvironment_eksConfiguration,
    createComputeEnvironment_serviceRole,
    createComputeEnvironment_state,
    createComputeEnvironment_tags,
    createComputeEnvironment_unmanagedvCpus,
    createComputeEnvironment_computeEnvironmentName,
    createComputeEnvironment_type,
    createComputeEnvironmentResponse_computeEnvironmentArn,
    createComputeEnvironmentResponse_computeEnvironmentName,
    createComputeEnvironmentResponse_httpStatus,

    -- ** CreateJobQueue
    createJobQueue_schedulingPolicyArn,
    createJobQueue_state,
    createJobQueue_tags,
    createJobQueue_jobQueueName,
    createJobQueue_priority,
    createJobQueue_computeEnvironmentOrder,
    createJobQueueResponse_httpStatus,
    createJobQueueResponse_jobQueueName,
    createJobQueueResponse_jobQueueArn,

    -- ** CreateSchedulingPolicy
    createSchedulingPolicy_fairsharePolicy,
    createSchedulingPolicy_tags,
    createSchedulingPolicy_name,
    createSchedulingPolicyResponse_httpStatus,
    createSchedulingPolicyResponse_name,
    createSchedulingPolicyResponse_arn,

    -- ** DeleteComputeEnvironment
    deleteComputeEnvironment_computeEnvironment,
    deleteComputeEnvironmentResponse_httpStatus,

    -- ** DeleteJobQueue
    deleteJobQueue_jobQueue,
    deleteJobQueueResponse_httpStatus,

    -- ** DeleteSchedulingPolicy
    deleteSchedulingPolicy_arn,
    deleteSchedulingPolicyResponse_httpStatus,

    -- ** DeregisterJobDefinition
    deregisterJobDefinition_jobDefinition,
    deregisterJobDefinitionResponse_httpStatus,

    -- ** DescribeComputeEnvironments
    describeComputeEnvironments_computeEnvironments,
    describeComputeEnvironments_maxResults,
    describeComputeEnvironments_nextToken,
    describeComputeEnvironmentsResponse_computeEnvironments,
    describeComputeEnvironmentsResponse_nextToken,
    describeComputeEnvironmentsResponse_httpStatus,

    -- ** DescribeJobDefinitions
    describeJobDefinitions_jobDefinitionName,
    describeJobDefinitions_jobDefinitions,
    describeJobDefinitions_maxResults,
    describeJobDefinitions_nextToken,
    describeJobDefinitions_status,
    describeJobDefinitionsResponse_jobDefinitions,
    describeJobDefinitionsResponse_nextToken,
    describeJobDefinitionsResponse_httpStatus,

    -- ** DescribeJobQueues
    describeJobQueues_jobQueues,
    describeJobQueues_maxResults,
    describeJobQueues_nextToken,
    describeJobQueuesResponse_jobQueues,
    describeJobQueuesResponse_nextToken,
    describeJobQueuesResponse_httpStatus,

    -- ** DescribeJobs
    describeJobs_jobs,
    describeJobsResponse_jobs,
    describeJobsResponse_httpStatus,

    -- ** DescribeSchedulingPolicies
    describeSchedulingPolicies_arns,
    describeSchedulingPoliciesResponse_schedulingPolicies,
    describeSchedulingPoliciesResponse_httpStatus,

    -- ** ListJobs
    listJobs_arrayJobId,
    listJobs_filters,
    listJobs_jobQueue,
    listJobs_jobStatus,
    listJobs_maxResults,
    listJobs_multiNodeJobId,
    listJobs_nextToken,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,
    listJobsResponse_jobSummaryList,

    -- ** ListSchedulingPolicies
    listSchedulingPolicies_maxResults,
    listSchedulingPolicies_nextToken,
    listSchedulingPoliciesResponse_nextToken,
    listSchedulingPoliciesResponse_schedulingPolicies,
    listSchedulingPoliciesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RegisterJobDefinition
    registerJobDefinition_containerProperties,
    registerJobDefinition_eksProperties,
    registerJobDefinition_nodeProperties,
    registerJobDefinition_parameters,
    registerJobDefinition_platformCapabilities,
    registerJobDefinition_propagateTags,
    registerJobDefinition_retryStrategy,
    registerJobDefinition_schedulingPriority,
    registerJobDefinition_tags,
    registerJobDefinition_timeout,
    registerJobDefinition_jobDefinitionName,
    registerJobDefinition_type,
    registerJobDefinitionResponse_httpStatus,
    registerJobDefinitionResponse_jobDefinitionName,
    registerJobDefinitionResponse_jobDefinitionArn,
    registerJobDefinitionResponse_revision,

    -- ** SubmitJob
    submitJob_arrayProperties,
    submitJob_containerOverrides,
    submitJob_dependsOn,
    submitJob_eksPropertiesOverride,
    submitJob_nodeOverrides,
    submitJob_parameters,
    submitJob_propagateTags,
    submitJob_retryStrategy,
    submitJob_schedulingPriorityOverride,
    submitJob_shareIdentifier,
    submitJob_tags,
    submitJob_timeout,
    submitJob_jobName,
    submitJob_jobQueue,
    submitJob_jobDefinition,
    submitJobResponse_jobArn,
    submitJobResponse_httpStatus,
    submitJobResponse_jobName,
    submitJobResponse_jobId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TerminateJob
    terminateJob_jobId,
    terminateJob_reason,
    terminateJobResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateComputeEnvironment
    updateComputeEnvironment_computeResources,
    updateComputeEnvironment_serviceRole,
    updateComputeEnvironment_state,
    updateComputeEnvironment_unmanagedvCpus,
    updateComputeEnvironment_updatePolicy,
    updateComputeEnvironment_computeEnvironment,
    updateComputeEnvironmentResponse_computeEnvironmentArn,
    updateComputeEnvironmentResponse_computeEnvironmentName,
    updateComputeEnvironmentResponse_httpStatus,

    -- ** UpdateJobQueue
    updateJobQueue_computeEnvironmentOrder,
    updateJobQueue_priority,
    updateJobQueue_schedulingPolicyArn,
    updateJobQueue_state,
    updateJobQueue_jobQueue,
    updateJobQueueResponse_jobQueueArn,
    updateJobQueueResponse_jobQueueName,
    updateJobQueueResponse_httpStatus,

    -- ** UpdateSchedulingPolicy
    updateSchedulingPolicy_fairsharePolicy,
    updateSchedulingPolicy_arn,
    updateSchedulingPolicyResponse_httpStatus,

    -- * Types

    -- ** ArrayProperties
    arrayProperties_size,

    -- ** ArrayPropertiesDetail
    arrayPropertiesDetail_index,
    arrayPropertiesDetail_size,
    arrayPropertiesDetail_statusSummary,

    -- ** ArrayPropertiesSummary
    arrayPropertiesSummary_index,
    arrayPropertiesSummary_size,

    -- ** AttemptContainerDetail
    attemptContainerDetail_containerInstanceArn,
    attemptContainerDetail_exitCode,
    attemptContainerDetail_logStreamName,
    attemptContainerDetail_networkInterfaces,
    attemptContainerDetail_reason,
    attemptContainerDetail_taskArn,

    -- ** AttemptDetail
    attemptDetail_container,
    attemptDetail_startedAt,
    attemptDetail_statusReason,
    attemptDetail_stoppedAt,

    -- ** ComputeEnvironmentDetail
    computeEnvironmentDetail_computeResources,
    computeEnvironmentDetail_containerOrchestrationType,
    computeEnvironmentDetail_ecsClusterArn,
    computeEnvironmentDetail_eksConfiguration,
    computeEnvironmentDetail_serviceRole,
    computeEnvironmentDetail_state,
    computeEnvironmentDetail_status,
    computeEnvironmentDetail_statusReason,
    computeEnvironmentDetail_tags,
    computeEnvironmentDetail_type,
    computeEnvironmentDetail_unmanagedvCpus,
    computeEnvironmentDetail_updatePolicy,
    computeEnvironmentDetail_uuid,
    computeEnvironmentDetail_computeEnvironmentName,
    computeEnvironmentDetail_computeEnvironmentArn,

    -- ** ComputeEnvironmentOrder
    computeEnvironmentOrder_order,
    computeEnvironmentOrder_computeEnvironment,

    -- ** ComputeResource
    computeResource_allocationStrategy,
    computeResource_bidPercentage,
    computeResource_desiredvCpus,
    computeResource_ec2Configuration,
    computeResource_ec2KeyPair,
    computeResource_imageId,
    computeResource_instanceRole,
    computeResource_instanceTypes,
    computeResource_launchTemplate,
    computeResource_minvCpus,
    computeResource_placementGroup,
    computeResource_securityGroupIds,
    computeResource_spotIamFleetRole,
    computeResource_tags,
    computeResource_type,
    computeResource_maxvCpus,
    computeResource_subnets,

    -- ** ComputeResourceUpdate
    computeResourceUpdate_allocationStrategy,
    computeResourceUpdate_bidPercentage,
    computeResourceUpdate_desiredvCpus,
    computeResourceUpdate_ec2Configuration,
    computeResourceUpdate_ec2KeyPair,
    computeResourceUpdate_imageId,
    computeResourceUpdate_instanceRole,
    computeResourceUpdate_instanceTypes,
    computeResourceUpdate_launchTemplate,
    computeResourceUpdate_maxvCpus,
    computeResourceUpdate_minvCpus,
    computeResourceUpdate_placementGroup,
    computeResourceUpdate_securityGroupIds,
    computeResourceUpdate_subnets,
    computeResourceUpdate_tags,
    computeResourceUpdate_type,
    computeResourceUpdate_updateToLatestImageVersion,

    -- ** ContainerDetail
    containerDetail_command,
    containerDetail_containerInstanceArn,
    containerDetail_environment,
    containerDetail_ephemeralStorage,
    containerDetail_executionRoleArn,
    containerDetail_exitCode,
    containerDetail_fargatePlatformConfiguration,
    containerDetail_image,
    containerDetail_instanceType,
    containerDetail_jobRoleArn,
    containerDetail_linuxParameters,
    containerDetail_logConfiguration,
    containerDetail_logStreamName,
    containerDetail_memory,
    containerDetail_mountPoints,
    containerDetail_networkConfiguration,
    containerDetail_networkInterfaces,
    containerDetail_privileged,
    containerDetail_readonlyRootFilesystem,
    containerDetail_reason,
    containerDetail_resourceRequirements,
    containerDetail_secrets,
    containerDetail_taskArn,
    containerDetail_ulimits,
    containerDetail_user,
    containerDetail_vcpus,
    containerDetail_volumes,

    -- ** ContainerOverrides
    containerOverrides_command,
    containerOverrides_environment,
    containerOverrides_instanceType,
    containerOverrides_memory,
    containerOverrides_resourceRequirements,
    containerOverrides_vcpus,

    -- ** ContainerProperties
    containerProperties_command,
    containerProperties_environment,
    containerProperties_ephemeralStorage,
    containerProperties_executionRoleArn,
    containerProperties_fargatePlatformConfiguration,
    containerProperties_image,
    containerProperties_instanceType,
    containerProperties_jobRoleArn,
    containerProperties_linuxParameters,
    containerProperties_logConfiguration,
    containerProperties_memory,
    containerProperties_mountPoints,
    containerProperties_networkConfiguration,
    containerProperties_privileged,
    containerProperties_readonlyRootFilesystem,
    containerProperties_resourceRequirements,
    containerProperties_secrets,
    containerProperties_ulimits,
    containerProperties_user,
    containerProperties_vcpus,
    containerProperties_volumes,

    -- ** ContainerSummary
    containerSummary_exitCode,
    containerSummary_reason,

    -- ** Device
    device_containerPath,
    device_permissions,
    device_hostPath,

    -- ** EFSAuthorizationConfig
    eFSAuthorizationConfig_accessPointId,
    eFSAuthorizationConfig_iam,

    -- ** EFSVolumeConfiguration
    eFSVolumeConfiguration_authorizationConfig,
    eFSVolumeConfiguration_rootDirectory,
    eFSVolumeConfiguration_transitEncryption,
    eFSVolumeConfiguration_transitEncryptionPort,
    eFSVolumeConfiguration_fileSystemId,

    -- ** Ec2Configuration
    ec2Configuration_imageIdOverride,
    ec2Configuration_imageKubernetesVersion,
    ec2Configuration_imageType,

    -- ** EksAttemptContainerDetail
    eksAttemptContainerDetail_exitCode,
    eksAttemptContainerDetail_reason,

    -- ** EksAttemptDetail
    eksAttemptDetail_containers,
    eksAttemptDetail_nodeName,
    eksAttemptDetail_podName,
    eksAttemptDetail_startedAt,
    eksAttemptDetail_statusReason,
    eksAttemptDetail_stoppedAt,

    -- ** EksConfiguration
    eksConfiguration_eksClusterArn,
    eksConfiguration_kubernetesNamespace,

    -- ** EksContainer
    eksContainer_args,
    eksContainer_command,
    eksContainer_env,
    eksContainer_imagePullPolicy,
    eksContainer_name,
    eksContainer_resources,
    eksContainer_securityContext,
    eksContainer_volumeMounts,
    eksContainer_image,

    -- ** EksContainerDetail
    eksContainerDetail_args,
    eksContainerDetail_command,
    eksContainerDetail_env,
    eksContainerDetail_exitCode,
    eksContainerDetail_image,
    eksContainerDetail_imagePullPolicy,
    eksContainerDetail_name,
    eksContainerDetail_reason,
    eksContainerDetail_resources,
    eksContainerDetail_securityContext,
    eksContainerDetail_volumeMounts,

    -- ** EksContainerEnvironmentVariable
    eksContainerEnvironmentVariable_value,
    eksContainerEnvironmentVariable_name,

    -- ** EksContainerOverride
    eksContainerOverride_args,
    eksContainerOverride_command,
    eksContainerOverride_env,
    eksContainerOverride_image,
    eksContainerOverride_resources,

    -- ** EksContainerResourceRequirements
    eksContainerResourceRequirements_limits,
    eksContainerResourceRequirements_requests,

    -- ** EksContainerSecurityContext
    eksContainerSecurityContext_privileged,
    eksContainerSecurityContext_readOnlyRootFilesystem,
    eksContainerSecurityContext_runAsGroup,
    eksContainerSecurityContext_runAsNonRoot,
    eksContainerSecurityContext_runAsUser,

    -- ** EksContainerVolumeMount
    eksContainerVolumeMount_mountPath,
    eksContainerVolumeMount_name,
    eksContainerVolumeMount_readOnly,

    -- ** EksEmptyDir
    eksEmptyDir_medium,
    eksEmptyDir_sizeLimit,

    -- ** EksHostPath
    eksHostPath_path,

    -- ** EksMetadata
    eksMetadata_labels,

    -- ** EksPodProperties
    eksPodProperties_containers,
    eksPodProperties_dnsPolicy,
    eksPodProperties_hostNetwork,
    eksPodProperties_metadata,
    eksPodProperties_serviceAccountName,
    eksPodProperties_volumes,

    -- ** EksPodPropertiesDetail
    eksPodPropertiesDetail_containers,
    eksPodPropertiesDetail_dnsPolicy,
    eksPodPropertiesDetail_hostNetwork,
    eksPodPropertiesDetail_metadata,
    eksPodPropertiesDetail_nodeName,
    eksPodPropertiesDetail_podName,
    eksPodPropertiesDetail_serviceAccountName,
    eksPodPropertiesDetail_volumes,

    -- ** EksPodPropertiesOverride
    eksPodPropertiesOverride_containers,
    eksPodPropertiesOverride_metadata,

    -- ** EksProperties
    eksProperties_podProperties,

    -- ** EksPropertiesDetail
    eksPropertiesDetail_podProperties,

    -- ** EksPropertiesOverride
    eksPropertiesOverride_podProperties,

    -- ** EksSecret
    eksSecret_optional,
    eksSecret_secretName,

    -- ** EksVolume
    eksVolume_emptyDir,
    eksVolume_hostPath,
    eksVolume_secret,
    eksVolume_name,

    -- ** EphemeralStorage
    ephemeralStorage_sizeInGiB,

    -- ** EvaluateOnExit
    evaluateOnExit_onExitCode,
    evaluateOnExit_onReason,
    evaluateOnExit_onStatusReason,
    evaluateOnExit_action,

    -- ** FairsharePolicy
    fairsharePolicy_computeReservation,
    fairsharePolicy_shareDecaySeconds,
    fairsharePolicy_shareDistribution,

    -- ** FargatePlatformConfiguration
    fargatePlatformConfiguration_platformVersion,

    -- ** Host
    host_sourcePath,

    -- ** JobDefinition
    jobDefinition_containerOrchestrationType,
    jobDefinition_containerProperties,
    jobDefinition_eksProperties,
    jobDefinition_nodeProperties,
    jobDefinition_parameters,
    jobDefinition_platformCapabilities,
    jobDefinition_propagateTags,
    jobDefinition_retryStrategy,
    jobDefinition_schedulingPriority,
    jobDefinition_status,
    jobDefinition_tags,
    jobDefinition_timeout,
    jobDefinition_jobDefinitionName,
    jobDefinition_jobDefinitionArn,
    jobDefinition_revision,
    jobDefinition_type,

    -- ** JobDependency
    jobDependency_jobId,
    jobDependency_type,

    -- ** JobDetail
    jobDetail_arrayProperties,
    jobDetail_attempts,
    jobDetail_container,
    jobDetail_createdAt,
    jobDetail_dependsOn,
    jobDetail_eksAttempts,
    jobDetail_eksProperties,
    jobDetail_isCancelled,
    jobDetail_isTerminated,
    jobDetail_jobArn,
    jobDetail_nodeDetails,
    jobDetail_nodeProperties,
    jobDetail_parameters,
    jobDetail_platformCapabilities,
    jobDetail_propagateTags,
    jobDetail_retryStrategy,
    jobDetail_schedulingPriority,
    jobDetail_shareIdentifier,
    jobDetail_startedAt,
    jobDetail_statusReason,
    jobDetail_stoppedAt,
    jobDetail_tags,
    jobDetail_timeout,
    jobDetail_jobName,
    jobDetail_jobId,
    jobDetail_jobQueue,
    jobDetail_status,
    jobDetail_jobDefinition,

    -- ** JobQueueDetail
    jobQueueDetail_schedulingPolicyArn,
    jobQueueDetail_status,
    jobQueueDetail_statusReason,
    jobQueueDetail_tags,
    jobQueueDetail_jobQueueName,
    jobQueueDetail_jobQueueArn,
    jobQueueDetail_state,
    jobQueueDetail_priority,
    jobQueueDetail_computeEnvironmentOrder,

    -- ** JobSummary
    jobSummary_arrayProperties,
    jobSummary_container,
    jobSummary_createdAt,
    jobSummary_jobArn,
    jobSummary_jobDefinition,
    jobSummary_nodeProperties,
    jobSummary_startedAt,
    jobSummary_status,
    jobSummary_statusReason,
    jobSummary_stoppedAt,
    jobSummary_jobId,
    jobSummary_jobName,

    -- ** JobTimeout
    jobTimeout_attemptDurationSeconds,

    -- ** KeyValuePair
    keyValuePair_name,
    keyValuePair_value,

    -- ** KeyValuesPair
    keyValuesPair_name,
    keyValuesPair_values,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_version,

    -- ** LinuxParameters
    linuxParameters_devices,
    linuxParameters_initProcessEnabled,
    linuxParameters_maxSwap,
    linuxParameters_sharedMemorySize,
    linuxParameters_swappiness,
    linuxParameters_tmpfs,

    -- ** LogConfiguration
    logConfiguration_options,
    logConfiguration_secretOptions,
    logConfiguration_logDriver,

    -- ** MountPoint
    mountPoint_containerPath,
    mountPoint_readOnly,
    mountPoint_sourceVolume,

    -- ** NetworkConfiguration
    networkConfiguration_assignPublicIp,

    -- ** NetworkInterface
    networkInterface_attachmentId,
    networkInterface_ipv6Address,
    networkInterface_privateIpv4Address,

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
    retryStrategy_attempts,
    retryStrategy_evaluateOnExit,

    -- ** SchedulingPolicyDetail
    schedulingPolicyDetail_fairsharePolicy,
    schedulingPolicyDetail_tags,
    schedulingPolicyDetail_name,
    schedulingPolicyDetail_arn,

    -- ** SchedulingPolicyListingDetail
    schedulingPolicyListingDetail_arn,

    -- ** Secret
    secret_name,
    secret_valueFrom,

    -- ** ShareAttributes
    shareAttributes_weightFactor,
    shareAttributes_shareIdentifier,

    -- ** Tmpfs
    tmpfs_mountOptions,
    tmpfs_containerPath,
    tmpfs_size,

    -- ** Ulimit
    ulimit_hardLimit,
    ulimit_name,
    ulimit_softLimit,

    -- ** UpdatePolicy
    updatePolicy_jobExecutionTimeoutMinutes,
    updatePolicy_terminateJobsOnUpdate,

    -- ** Volume
    volume_efsVolumeConfiguration,
    volume_host,
    volume_name,
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
import Amazonka.Batch.ListJobs
import Amazonka.Batch.ListSchedulingPolicies
import Amazonka.Batch.ListTagsForResource
import Amazonka.Batch.RegisterJobDefinition
import Amazonka.Batch.SubmitJob
import Amazonka.Batch.TagResource
import Amazonka.Batch.TerminateJob
import Amazonka.Batch.Types.ArrayProperties
import Amazonka.Batch.Types.ArrayPropertiesDetail
import Amazonka.Batch.Types.ArrayPropertiesSummary
import Amazonka.Batch.Types.AttemptContainerDetail
import Amazonka.Batch.Types.AttemptDetail
import Amazonka.Batch.Types.ComputeEnvironmentDetail
import Amazonka.Batch.Types.ComputeEnvironmentOrder
import Amazonka.Batch.Types.ComputeResource
import Amazonka.Batch.Types.ComputeResourceUpdate
import Amazonka.Batch.Types.ContainerDetail
import Amazonka.Batch.Types.ContainerOverrides
import Amazonka.Batch.Types.ContainerProperties
import Amazonka.Batch.Types.ContainerSummary
import Amazonka.Batch.Types.Device
import Amazonka.Batch.Types.EFSAuthorizationConfig
import Amazonka.Batch.Types.EFSVolumeConfiguration
import Amazonka.Batch.Types.Ec2Configuration
import Amazonka.Batch.Types.EksAttemptContainerDetail
import Amazonka.Batch.Types.EksAttemptDetail
import Amazonka.Batch.Types.EksConfiguration
import Amazonka.Batch.Types.EksContainer
import Amazonka.Batch.Types.EksContainerDetail
import Amazonka.Batch.Types.EksContainerEnvironmentVariable
import Amazonka.Batch.Types.EksContainerOverride
import Amazonka.Batch.Types.EksContainerResourceRequirements
import Amazonka.Batch.Types.EksContainerSecurityContext
import Amazonka.Batch.Types.EksContainerVolumeMount
import Amazonka.Batch.Types.EksEmptyDir
import Amazonka.Batch.Types.EksHostPath
import Amazonka.Batch.Types.EksMetadata
import Amazonka.Batch.Types.EksPodProperties
import Amazonka.Batch.Types.EksPodPropertiesDetail
import Amazonka.Batch.Types.EksPodPropertiesOverride
import Amazonka.Batch.Types.EksProperties
import Amazonka.Batch.Types.EksPropertiesDetail
import Amazonka.Batch.Types.EksPropertiesOverride
import Amazonka.Batch.Types.EksSecret
import Amazonka.Batch.Types.EksVolume
import Amazonka.Batch.Types.EphemeralStorage
import Amazonka.Batch.Types.EvaluateOnExit
import Amazonka.Batch.Types.FairsharePolicy
import Amazonka.Batch.Types.FargatePlatformConfiguration
import Amazonka.Batch.Types.Host
import Amazonka.Batch.Types.JobDefinition
import Amazonka.Batch.Types.JobDependency
import Amazonka.Batch.Types.JobDetail
import Amazonka.Batch.Types.JobQueueDetail
import Amazonka.Batch.Types.JobSummary
import Amazonka.Batch.Types.JobTimeout
import Amazonka.Batch.Types.KeyValuePair
import Amazonka.Batch.Types.KeyValuesPair
import Amazonka.Batch.Types.LaunchTemplateSpecification
import Amazonka.Batch.Types.LinuxParameters
import Amazonka.Batch.Types.LogConfiguration
import Amazonka.Batch.Types.MountPoint
import Amazonka.Batch.Types.NetworkConfiguration
import Amazonka.Batch.Types.NetworkInterface
import Amazonka.Batch.Types.NodeDetails
import Amazonka.Batch.Types.NodeOverrides
import Amazonka.Batch.Types.NodeProperties
import Amazonka.Batch.Types.NodePropertiesSummary
import Amazonka.Batch.Types.NodePropertyOverride
import Amazonka.Batch.Types.NodeRangeProperty
import Amazonka.Batch.Types.ResourceRequirement
import Amazonka.Batch.Types.RetryStrategy
import Amazonka.Batch.Types.SchedulingPolicyDetail
import Amazonka.Batch.Types.SchedulingPolicyListingDetail
import Amazonka.Batch.Types.Secret
import Amazonka.Batch.Types.ShareAttributes
import Amazonka.Batch.Types.Tmpfs
import Amazonka.Batch.Types.Ulimit
import Amazonka.Batch.Types.UpdatePolicy
import Amazonka.Batch.Types.Volume
import Amazonka.Batch.UntagResource
import Amazonka.Batch.UpdateComputeEnvironment
import Amazonka.Batch.UpdateJobQueue
import Amazonka.Batch.UpdateSchedulingPolicy
