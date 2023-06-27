{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Batch.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ClientException,
    _ServerException,

    -- * ArrayJobDependency
    ArrayJobDependency (..),

    -- * AssignPublicIp
    AssignPublicIp (..),

    -- * CEState
    CEState (..),

    -- * CEStatus
    CEStatus (..),

    -- * CEType
    CEType (..),

    -- * CRAllocationStrategy
    CRAllocationStrategy (..),

    -- * CRType
    CRType (..),

    -- * CRUpdateAllocationStrategy
    CRUpdateAllocationStrategy (..),

    -- * DeviceCgroupPermission
    DeviceCgroupPermission (..),

    -- * EFSAuthorizationConfigIAM
    EFSAuthorizationConfigIAM (..),

    -- * EFSTransitEncryption
    EFSTransitEncryption (..),

    -- * JQState
    JQState (..),

    -- * JQStatus
    JQStatus (..),

    -- * JobDefinitionType
    JobDefinitionType (..),

    -- * JobStatus
    JobStatus (..),

    -- * LogDriver
    LogDriver (..),

    -- * OrchestrationType
    OrchestrationType (..),

    -- * PlatformCapability
    PlatformCapability (..),

    -- * ResourceType
    ResourceType (..),

    -- * RetryAction
    RetryAction (..),

    -- * ArrayProperties
    ArrayProperties (..),
    newArrayProperties,
    arrayProperties_size,

    -- * ArrayPropertiesDetail
    ArrayPropertiesDetail (..),
    newArrayPropertiesDetail,
    arrayPropertiesDetail_index,
    arrayPropertiesDetail_size,
    arrayPropertiesDetail_statusSummary,

    -- * ArrayPropertiesSummary
    ArrayPropertiesSummary (..),
    newArrayPropertiesSummary,
    arrayPropertiesSummary_index,
    arrayPropertiesSummary_size,

    -- * AttemptContainerDetail
    AttemptContainerDetail (..),
    newAttemptContainerDetail,
    attemptContainerDetail_containerInstanceArn,
    attemptContainerDetail_exitCode,
    attemptContainerDetail_logStreamName,
    attemptContainerDetail_networkInterfaces,
    attemptContainerDetail_reason,
    attemptContainerDetail_taskArn,

    -- * AttemptDetail
    AttemptDetail (..),
    newAttemptDetail,
    attemptDetail_container,
    attemptDetail_startedAt,
    attemptDetail_statusReason,
    attemptDetail_stoppedAt,

    -- * ComputeEnvironmentDetail
    ComputeEnvironmentDetail (..),
    newComputeEnvironmentDetail,
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

    -- * ComputeEnvironmentOrder
    ComputeEnvironmentOrder (..),
    newComputeEnvironmentOrder,
    computeEnvironmentOrder_order,
    computeEnvironmentOrder_computeEnvironment,

    -- * ComputeResource
    ComputeResource (..),
    newComputeResource,
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

    -- * ComputeResourceUpdate
    ComputeResourceUpdate (..),
    newComputeResourceUpdate,
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

    -- * ContainerDetail
    ContainerDetail (..),
    newContainerDetail,
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

    -- * ContainerOverrides
    ContainerOverrides (..),
    newContainerOverrides,
    containerOverrides_command,
    containerOverrides_environment,
    containerOverrides_instanceType,
    containerOverrides_memory,
    containerOverrides_resourceRequirements,
    containerOverrides_vcpus,

    -- * ContainerProperties
    ContainerProperties (..),
    newContainerProperties,
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

    -- * ContainerSummary
    ContainerSummary (..),
    newContainerSummary,
    containerSummary_exitCode,
    containerSummary_reason,

    -- * Device
    Device (..),
    newDevice,
    device_containerPath,
    device_permissions,
    device_hostPath,

    -- * EFSAuthorizationConfig
    EFSAuthorizationConfig (..),
    newEFSAuthorizationConfig,
    eFSAuthorizationConfig_accessPointId,
    eFSAuthorizationConfig_iam,

    -- * EFSVolumeConfiguration
    EFSVolumeConfiguration (..),
    newEFSVolumeConfiguration,
    eFSVolumeConfiguration_authorizationConfig,
    eFSVolumeConfiguration_rootDirectory,
    eFSVolumeConfiguration_transitEncryption,
    eFSVolumeConfiguration_transitEncryptionPort,
    eFSVolumeConfiguration_fileSystemId,

    -- * Ec2Configuration
    Ec2Configuration (..),
    newEc2Configuration,
    ec2Configuration_imageIdOverride,
    ec2Configuration_imageKubernetesVersion,
    ec2Configuration_imageType,

    -- * EksAttemptContainerDetail
    EksAttemptContainerDetail (..),
    newEksAttemptContainerDetail,
    eksAttemptContainerDetail_exitCode,
    eksAttemptContainerDetail_reason,

    -- * EksAttemptDetail
    EksAttemptDetail (..),
    newEksAttemptDetail,
    eksAttemptDetail_containers,
    eksAttemptDetail_nodeName,
    eksAttemptDetail_podName,
    eksAttemptDetail_startedAt,
    eksAttemptDetail_statusReason,
    eksAttemptDetail_stoppedAt,

    -- * EksConfiguration
    EksConfiguration (..),
    newEksConfiguration,
    eksConfiguration_eksClusterArn,
    eksConfiguration_kubernetesNamespace,

    -- * EksContainer
    EksContainer (..),
    newEksContainer,
    eksContainer_args,
    eksContainer_command,
    eksContainer_env,
    eksContainer_imagePullPolicy,
    eksContainer_name,
    eksContainer_resources,
    eksContainer_securityContext,
    eksContainer_volumeMounts,
    eksContainer_image,

    -- * EksContainerDetail
    EksContainerDetail (..),
    newEksContainerDetail,
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

    -- * EksContainerEnvironmentVariable
    EksContainerEnvironmentVariable (..),
    newEksContainerEnvironmentVariable,
    eksContainerEnvironmentVariable_value,
    eksContainerEnvironmentVariable_name,

    -- * EksContainerOverride
    EksContainerOverride (..),
    newEksContainerOverride,
    eksContainerOverride_args,
    eksContainerOverride_command,
    eksContainerOverride_env,
    eksContainerOverride_image,
    eksContainerOverride_resources,

    -- * EksContainerResourceRequirements
    EksContainerResourceRequirements (..),
    newEksContainerResourceRequirements,
    eksContainerResourceRequirements_limits,
    eksContainerResourceRequirements_requests,

    -- * EksContainerSecurityContext
    EksContainerSecurityContext (..),
    newEksContainerSecurityContext,
    eksContainerSecurityContext_privileged,
    eksContainerSecurityContext_readOnlyRootFilesystem,
    eksContainerSecurityContext_runAsGroup,
    eksContainerSecurityContext_runAsNonRoot,
    eksContainerSecurityContext_runAsUser,

    -- * EksContainerVolumeMount
    EksContainerVolumeMount (..),
    newEksContainerVolumeMount,
    eksContainerVolumeMount_mountPath,
    eksContainerVolumeMount_name,
    eksContainerVolumeMount_readOnly,

    -- * EksEmptyDir
    EksEmptyDir (..),
    newEksEmptyDir,
    eksEmptyDir_medium,
    eksEmptyDir_sizeLimit,

    -- * EksHostPath
    EksHostPath (..),
    newEksHostPath,
    eksHostPath_path,

    -- * EksMetadata
    EksMetadata (..),
    newEksMetadata,
    eksMetadata_labels,

    -- * EksPodProperties
    EksPodProperties (..),
    newEksPodProperties,
    eksPodProperties_containers,
    eksPodProperties_dnsPolicy,
    eksPodProperties_hostNetwork,
    eksPodProperties_metadata,
    eksPodProperties_serviceAccountName,
    eksPodProperties_volumes,

    -- * EksPodPropertiesDetail
    EksPodPropertiesDetail (..),
    newEksPodPropertiesDetail,
    eksPodPropertiesDetail_containers,
    eksPodPropertiesDetail_dnsPolicy,
    eksPodPropertiesDetail_hostNetwork,
    eksPodPropertiesDetail_metadata,
    eksPodPropertiesDetail_nodeName,
    eksPodPropertiesDetail_podName,
    eksPodPropertiesDetail_serviceAccountName,
    eksPodPropertiesDetail_volumes,

    -- * EksPodPropertiesOverride
    EksPodPropertiesOverride (..),
    newEksPodPropertiesOverride,
    eksPodPropertiesOverride_containers,
    eksPodPropertiesOverride_metadata,

    -- * EksProperties
    EksProperties (..),
    newEksProperties,
    eksProperties_podProperties,

    -- * EksPropertiesDetail
    EksPropertiesDetail (..),
    newEksPropertiesDetail,
    eksPropertiesDetail_podProperties,

    -- * EksPropertiesOverride
    EksPropertiesOverride (..),
    newEksPropertiesOverride,
    eksPropertiesOverride_podProperties,

    -- * EksSecret
    EksSecret (..),
    newEksSecret,
    eksSecret_optional,
    eksSecret_secretName,

    -- * EksVolume
    EksVolume (..),
    newEksVolume,
    eksVolume_emptyDir,
    eksVolume_hostPath,
    eksVolume_secret,
    eksVolume_name,

    -- * EphemeralStorage
    EphemeralStorage (..),
    newEphemeralStorage,
    ephemeralStorage_sizeInGiB,

    -- * EvaluateOnExit
    EvaluateOnExit (..),
    newEvaluateOnExit,
    evaluateOnExit_onExitCode,
    evaluateOnExit_onReason,
    evaluateOnExit_onStatusReason,
    evaluateOnExit_action,

    -- * FairsharePolicy
    FairsharePolicy (..),
    newFairsharePolicy,
    fairsharePolicy_computeReservation,
    fairsharePolicy_shareDecaySeconds,
    fairsharePolicy_shareDistribution,

    -- * FargatePlatformConfiguration
    FargatePlatformConfiguration (..),
    newFargatePlatformConfiguration,
    fargatePlatformConfiguration_platformVersion,

    -- * Host
    Host (..),
    newHost,
    host_sourcePath,

    -- * JobDefinition
    JobDefinition (..),
    newJobDefinition,
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

    -- * JobDependency
    JobDependency (..),
    newJobDependency,
    jobDependency_jobId,
    jobDependency_type,

    -- * JobDetail
    JobDetail (..),
    newJobDetail,
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

    -- * JobQueueDetail
    JobQueueDetail (..),
    newJobQueueDetail,
    jobQueueDetail_schedulingPolicyArn,
    jobQueueDetail_status,
    jobQueueDetail_statusReason,
    jobQueueDetail_tags,
    jobQueueDetail_jobQueueName,
    jobQueueDetail_jobQueueArn,
    jobQueueDetail_state,
    jobQueueDetail_priority,
    jobQueueDetail_computeEnvironmentOrder,

    -- * JobSummary
    JobSummary (..),
    newJobSummary,
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

    -- * JobTimeout
    JobTimeout (..),
    newJobTimeout,
    jobTimeout_attemptDurationSeconds,

    -- * KeyValuePair
    KeyValuePair (..),
    newKeyValuePair,
    keyValuePair_name,
    keyValuePair_value,

    -- * KeyValuesPair
    KeyValuesPair (..),
    newKeyValuesPair,
    keyValuesPair_name,
    keyValuesPair_values,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    newLaunchTemplateSpecification,
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_version,

    -- * LinuxParameters
    LinuxParameters (..),
    newLinuxParameters,
    linuxParameters_devices,
    linuxParameters_initProcessEnabled,
    linuxParameters_maxSwap,
    linuxParameters_sharedMemorySize,
    linuxParameters_swappiness,
    linuxParameters_tmpfs,

    -- * LogConfiguration
    LogConfiguration (..),
    newLogConfiguration,
    logConfiguration_options,
    logConfiguration_secretOptions,
    logConfiguration_logDriver,

    -- * MountPoint
    MountPoint (..),
    newMountPoint,
    mountPoint_containerPath,
    mountPoint_readOnly,
    mountPoint_sourceVolume,

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    newNetworkConfiguration,
    networkConfiguration_assignPublicIp,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_attachmentId,
    networkInterface_ipv6Address,
    networkInterface_privateIpv4Address,

    -- * NodeDetails
    NodeDetails (..),
    newNodeDetails,
    nodeDetails_isMainNode,
    nodeDetails_nodeIndex,

    -- * NodeOverrides
    NodeOverrides (..),
    newNodeOverrides,
    nodeOverrides_nodePropertyOverrides,
    nodeOverrides_numNodes,

    -- * NodeProperties
    NodeProperties (..),
    newNodeProperties,
    nodeProperties_numNodes,
    nodeProperties_mainNode,
    nodeProperties_nodeRangeProperties,

    -- * NodePropertiesSummary
    NodePropertiesSummary (..),
    newNodePropertiesSummary,
    nodePropertiesSummary_isMainNode,
    nodePropertiesSummary_nodeIndex,
    nodePropertiesSummary_numNodes,

    -- * NodePropertyOverride
    NodePropertyOverride (..),
    newNodePropertyOverride,
    nodePropertyOverride_containerOverrides,
    nodePropertyOverride_targetNodes,

    -- * NodeRangeProperty
    NodeRangeProperty (..),
    newNodeRangeProperty,
    nodeRangeProperty_container,
    nodeRangeProperty_targetNodes,

    -- * ResourceRequirement
    ResourceRequirement (..),
    newResourceRequirement,
    resourceRequirement_value,
    resourceRequirement_type,

    -- * RetryStrategy
    RetryStrategy (..),
    newRetryStrategy,
    retryStrategy_attempts,
    retryStrategy_evaluateOnExit,

    -- * SchedulingPolicyDetail
    SchedulingPolicyDetail (..),
    newSchedulingPolicyDetail,
    schedulingPolicyDetail_fairsharePolicy,
    schedulingPolicyDetail_tags,
    schedulingPolicyDetail_name,
    schedulingPolicyDetail_arn,

    -- * SchedulingPolicyListingDetail
    SchedulingPolicyListingDetail (..),
    newSchedulingPolicyListingDetail,
    schedulingPolicyListingDetail_arn,

    -- * Secret
    Secret (..),
    newSecret,
    secret_name,
    secret_valueFrom,

    -- * ShareAttributes
    ShareAttributes (..),
    newShareAttributes,
    shareAttributes_weightFactor,
    shareAttributes_shareIdentifier,

    -- * Tmpfs
    Tmpfs (..),
    newTmpfs,
    tmpfs_mountOptions,
    tmpfs_containerPath,
    tmpfs_size,

    -- * Ulimit
    Ulimit (..),
    newUlimit,
    ulimit_hardLimit,
    ulimit_name,
    ulimit_softLimit,

    -- * UpdatePolicy
    UpdatePolicy (..),
    newUpdatePolicy,
    updatePolicy_jobExecutionTimeoutMinutes,
    updatePolicy_terminateJobsOnUpdate,

    -- * Volume
    Volume (..),
    newVolume,
    volume_efsVolumeConfiguration,
    volume_host,
    volume_name,
  )
where

import Amazonka.Batch.Types.ArrayJobDependency
import Amazonka.Batch.Types.ArrayProperties
import Amazonka.Batch.Types.ArrayPropertiesDetail
import Amazonka.Batch.Types.ArrayPropertiesSummary
import Amazonka.Batch.Types.AssignPublicIp
import Amazonka.Batch.Types.AttemptContainerDetail
import Amazonka.Batch.Types.AttemptDetail
import Amazonka.Batch.Types.CEState
import Amazonka.Batch.Types.CEStatus
import Amazonka.Batch.Types.CEType
import Amazonka.Batch.Types.CRAllocationStrategy
import Amazonka.Batch.Types.CRType
import Amazonka.Batch.Types.CRUpdateAllocationStrategy
import Amazonka.Batch.Types.ComputeEnvironmentDetail
import Amazonka.Batch.Types.ComputeEnvironmentOrder
import Amazonka.Batch.Types.ComputeResource
import Amazonka.Batch.Types.ComputeResourceUpdate
import Amazonka.Batch.Types.ContainerDetail
import Amazonka.Batch.Types.ContainerOverrides
import Amazonka.Batch.Types.ContainerProperties
import Amazonka.Batch.Types.ContainerSummary
import Amazonka.Batch.Types.Device
import Amazonka.Batch.Types.DeviceCgroupPermission
import Amazonka.Batch.Types.EFSAuthorizationConfig
import Amazonka.Batch.Types.EFSAuthorizationConfigIAM
import Amazonka.Batch.Types.EFSTransitEncryption
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
import Amazonka.Batch.Types.JQState
import Amazonka.Batch.Types.JQStatus
import Amazonka.Batch.Types.JobDefinition
import Amazonka.Batch.Types.JobDefinitionType
import Amazonka.Batch.Types.JobDependency
import Amazonka.Batch.Types.JobDetail
import Amazonka.Batch.Types.JobQueueDetail
import Amazonka.Batch.Types.JobStatus
import Amazonka.Batch.Types.JobSummary
import Amazonka.Batch.Types.JobTimeout
import Amazonka.Batch.Types.KeyValuePair
import Amazonka.Batch.Types.KeyValuesPair
import Amazonka.Batch.Types.LaunchTemplateSpecification
import Amazonka.Batch.Types.LinuxParameters
import Amazonka.Batch.Types.LogConfiguration
import Amazonka.Batch.Types.LogDriver
import Amazonka.Batch.Types.MountPoint
import Amazonka.Batch.Types.NetworkConfiguration
import Amazonka.Batch.Types.NetworkInterface
import Amazonka.Batch.Types.NodeDetails
import Amazonka.Batch.Types.NodeOverrides
import Amazonka.Batch.Types.NodeProperties
import Amazonka.Batch.Types.NodePropertiesSummary
import Amazonka.Batch.Types.NodePropertyOverride
import Amazonka.Batch.Types.NodeRangeProperty
import Amazonka.Batch.Types.OrchestrationType
import Amazonka.Batch.Types.PlatformCapability
import Amazonka.Batch.Types.ResourceRequirement
import Amazonka.Batch.Types.ResourceType
import Amazonka.Batch.Types.RetryAction
import Amazonka.Batch.Types.RetryStrategy
import Amazonka.Batch.Types.SchedulingPolicyDetail
import Amazonka.Batch.Types.SchedulingPolicyListingDetail
import Amazonka.Batch.Types.Secret
import Amazonka.Batch.Types.ShareAttributes
import Amazonka.Batch.Types.Tmpfs
import Amazonka.Batch.Types.Ulimit
import Amazonka.Batch.Types.UpdatePolicy
import Amazonka.Batch.Types.Volume
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-08-10@ of the Amazon Batch SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Batch",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "batch",
      Core.signingName = "batch",
      Core.version = "2016-08-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Batch",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | These errors are usually caused by a client action. One example cause is
-- using an action or resource on behalf of a user that doesn\'t have
-- permissions to use the action or resource. Another cause is specifying
-- an identifier that\'s not valid.
_ClientException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClientException =
  Core._MatchServiceError
    defaultService
    "ClientException"
    Prelude.. Core.hasStatus 400

-- | These errors are usually caused by a server issue.
_ServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServerException =
  Core._MatchServiceError
    defaultService
    "ServerException"
    Prelude.. Core.hasStatus 500
