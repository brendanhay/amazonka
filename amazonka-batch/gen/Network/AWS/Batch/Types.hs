{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types
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
    arrayPropertiesDetail_statusSummary,
    arrayPropertiesDetail_size,

    -- * ArrayPropertiesSummary
    ArrayPropertiesSummary (..),
    newArrayPropertiesSummary,
    arrayPropertiesSummary_index,
    arrayPropertiesSummary_size,

    -- * AttemptContainerDetail
    AttemptContainerDetail (..),
    newAttemptContainerDetail,
    attemptContainerDetail_logStreamName,
    attemptContainerDetail_containerInstanceArn,
    attemptContainerDetail_exitCode,
    attemptContainerDetail_reason,
    attemptContainerDetail_networkInterfaces,
    attemptContainerDetail_taskArn,

    -- * AttemptDetail
    AttemptDetail (..),
    newAttemptDetail,
    attemptDetail_container,
    attemptDetail_startedAt,
    attemptDetail_stoppedAt,
    attemptDetail_statusReason,

    -- * ComputeEnvironmentDetail
    ComputeEnvironmentDetail (..),
    newComputeEnvironmentDetail,
    computeEnvironmentDetail_status,
    computeEnvironmentDetail_serviceRole,
    computeEnvironmentDetail_state,
    computeEnvironmentDetail_computeResources,
    computeEnvironmentDetail_tags,
    computeEnvironmentDetail_type,
    computeEnvironmentDetail_statusReason,
    computeEnvironmentDetail_computeEnvironmentName,
    computeEnvironmentDetail_computeEnvironmentArn,
    computeEnvironmentDetail_ecsClusterArn,

    -- * ComputeEnvironmentOrder
    ComputeEnvironmentOrder (..),
    newComputeEnvironmentOrder,
    computeEnvironmentOrder_order,
    computeEnvironmentOrder_computeEnvironment,

    -- * ComputeResource
    ComputeResource (..),
    newComputeResource,
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

    -- * ComputeResourceUpdate
    ComputeResourceUpdate (..),
    newComputeResourceUpdate,
    computeResourceUpdate_securityGroupIds,
    computeResourceUpdate_minvCpus,
    computeResourceUpdate_maxvCpus,
    computeResourceUpdate_desiredvCpus,
    computeResourceUpdate_subnets,

    -- * ContainerDetail
    ContainerDetail (..),
    newContainerDetail,
    containerDetail_logStreamName,
    containerDetail_linuxParameters,
    containerDetail_memory,
    containerDetail_instanceType,
    containerDetail_user,
    containerDetail_networkConfiguration,
    containerDetail_containerInstanceArn,
    containerDetail_privileged,
    containerDetail_volumes,
    containerDetail_vcpus,
    containerDetail_executionRoleArn,
    containerDetail_environment,
    containerDetail_fargatePlatformConfiguration,
    containerDetail_exitCode,
    containerDetail_secrets,
    containerDetail_mountPoints,
    containerDetail_command,
    containerDetail_image,
    containerDetail_logConfiguration,
    containerDetail_reason,
    containerDetail_resourceRequirements,
    containerDetail_jobRoleArn,
    containerDetail_ulimits,
    containerDetail_readonlyRootFilesystem,
    containerDetail_networkInterfaces,
    containerDetail_taskArn,

    -- * ContainerOverrides
    ContainerOverrides (..),
    newContainerOverrides,
    containerOverrides_memory,
    containerOverrides_instanceType,
    containerOverrides_vcpus,
    containerOverrides_environment,
    containerOverrides_command,
    containerOverrides_resourceRequirements,

    -- * ContainerProperties
    ContainerProperties (..),
    newContainerProperties,
    containerProperties_linuxParameters,
    containerProperties_memory,
    containerProperties_instanceType,
    containerProperties_user,
    containerProperties_networkConfiguration,
    containerProperties_privileged,
    containerProperties_volumes,
    containerProperties_vcpus,
    containerProperties_executionRoleArn,
    containerProperties_environment,
    containerProperties_fargatePlatformConfiguration,
    containerProperties_secrets,
    containerProperties_mountPoints,
    containerProperties_command,
    containerProperties_image,
    containerProperties_logConfiguration,
    containerProperties_resourceRequirements,
    containerProperties_jobRoleArn,
    containerProperties_ulimits,
    containerProperties_readonlyRootFilesystem,

    -- * ContainerSummary
    ContainerSummary (..),
    newContainerSummary,
    containerSummary_exitCode,
    containerSummary_reason,

    -- * Device
    Device (..),
    newDevice,
    device_permissions,
    device_containerPath,
    device_hostPath,

    -- * EFSAuthorizationConfig
    EFSAuthorizationConfig (..),
    newEFSAuthorizationConfig,
    eFSAuthorizationConfig_accessPointId,
    eFSAuthorizationConfig_iam,

    -- * EFSVolumeConfiguration
    EFSVolumeConfiguration (..),
    newEFSVolumeConfiguration,
    eFSVolumeConfiguration_transitEncryptionPort,
    eFSVolumeConfiguration_rootDirectory,
    eFSVolumeConfiguration_authorizationConfig,
    eFSVolumeConfiguration_transitEncryption,
    eFSVolumeConfiguration_fileSystemId,

    -- * Ec2Configuration
    Ec2Configuration (..),
    newEc2Configuration,
    ec2Configuration_imageIdOverride,
    ec2Configuration_imageType,

    -- * EvaluateOnExit
    EvaluateOnExit (..),
    newEvaluateOnExit,
    evaluateOnExit_onExitCode,
    evaluateOnExit_onStatusReason,
    evaluateOnExit_onReason,
    evaluateOnExit_action,

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

    -- * JobDependency
    JobDependency (..),
    newJobDependency,
    jobDependency_type,
    jobDependency_jobId,

    -- * JobDetail
    JobDetail (..),
    newJobDetail,
    jobDetail_container,
    jobDetail_dependsOn,
    jobDetail_startedAt,
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

    -- * JobQueueDetail
    JobQueueDetail (..),
    newJobQueueDetail,
    jobQueueDetail_status,
    jobQueueDetail_tags,
    jobQueueDetail_statusReason,
    jobQueueDetail_jobQueueName,
    jobQueueDetail_jobQueueArn,
    jobQueueDetail_state,
    jobQueueDetail_priority,
    jobQueueDetail_computeEnvironmentOrder,

    -- * JobSummary
    JobSummary (..),
    newJobSummary,
    jobSummary_container,
    jobSummary_startedAt,
    jobSummary_status,
    jobSummary_arrayProperties,
    jobSummary_createdAt,
    jobSummary_jobArn,
    jobSummary_jobDefinition,
    jobSummary_stoppedAt,
    jobSummary_nodeProperties,
    jobSummary_statusReason,
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
    keyValuesPair_values,
    keyValuesPair_name,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    newLaunchTemplateSpecification,
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,
    launchTemplateSpecification_launchTemplateName,

    -- * LinuxParameters
    LinuxParameters (..),
    newLinuxParameters,
    linuxParameters_tmpfs,
    linuxParameters_maxSwap,
    linuxParameters_devices,
    linuxParameters_swappiness,
    linuxParameters_initProcessEnabled,
    linuxParameters_sharedMemorySize,

    -- * LogConfiguration
    LogConfiguration (..),
    newLogConfiguration,
    logConfiguration_options,
    logConfiguration_secretOptions,
    logConfiguration_logDriver,

    -- * MountPoint
    MountPoint (..),
    newMountPoint,
    mountPoint_readOnly,
    mountPoint_sourceVolume,
    mountPoint_containerPath,

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    newNetworkConfiguration,
    networkConfiguration_assignPublicIp,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_privateIpv4Address,
    networkInterface_ipv6Address,
    networkInterface_attachmentId,

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
    retryStrategy_evaluateOnExit,
    retryStrategy_attempts,

    -- * Secret
    Secret (..),
    newSecret,
    secret_name,
    secret_valueFrom,

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

    -- * Volume
    Volume (..),
    newVolume,
    volume_name,
    volume_host,
    volume_efsVolumeConfiguration,
  )
where

import Network.AWS.Batch.Types.ArrayJobDependency
import Network.AWS.Batch.Types.ArrayProperties
import Network.AWS.Batch.Types.ArrayPropertiesDetail
import Network.AWS.Batch.Types.ArrayPropertiesSummary
import Network.AWS.Batch.Types.AssignPublicIp
import Network.AWS.Batch.Types.AttemptContainerDetail
import Network.AWS.Batch.Types.AttemptDetail
import Network.AWS.Batch.Types.CEState
import Network.AWS.Batch.Types.CEStatus
import Network.AWS.Batch.Types.CEType
import Network.AWS.Batch.Types.CRAllocationStrategy
import Network.AWS.Batch.Types.CRType
import Network.AWS.Batch.Types.ComputeEnvironmentDetail
import Network.AWS.Batch.Types.ComputeEnvironmentOrder
import Network.AWS.Batch.Types.ComputeResource
import Network.AWS.Batch.Types.ComputeResourceUpdate
import Network.AWS.Batch.Types.ContainerDetail
import Network.AWS.Batch.Types.ContainerOverrides
import Network.AWS.Batch.Types.ContainerProperties
import Network.AWS.Batch.Types.ContainerSummary
import Network.AWS.Batch.Types.Device
import Network.AWS.Batch.Types.DeviceCgroupPermission
import Network.AWS.Batch.Types.EFSAuthorizationConfig
import Network.AWS.Batch.Types.EFSAuthorizationConfigIAM
import Network.AWS.Batch.Types.EFSTransitEncryption
import Network.AWS.Batch.Types.EFSVolumeConfiguration
import Network.AWS.Batch.Types.Ec2Configuration
import Network.AWS.Batch.Types.EvaluateOnExit
import Network.AWS.Batch.Types.FargatePlatformConfiguration
import Network.AWS.Batch.Types.Host
import Network.AWS.Batch.Types.JQState
import Network.AWS.Batch.Types.JQStatus
import Network.AWS.Batch.Types.JobDefinition
import Network.AWS.Batch.Types.JobDefinitionType
import Network.AWS.Batch.Types.JobDependency
import Network.AWS.Batch.Types.JobDetail
import Network.AWS.Batch.Types.JobQueueDetail
import Network.AWS.Batch.Types.JobStatus
import Network.AWS.Batch.Types.JobSummary
import Network.AWS.Batch.Types.JobTimeout
import Network.AWS.Batch.Types.KeyValuePair
import Network.AWS.Batch.Types.KeyValuesPair
import Network.AWS.Batch.Types.LaunchTemplateSpecification
import Network.AWS.Batch.Types.LinuxParameters
import Network.AWS.Batch.Types.LogConfiguration
import Network.AWS.Batch.Types.LogDriver
import Network.AWS.Batch.Types.MountPoint
import Network.AWS.Batch.Types.NetworkConfiguration
import Network.AWS.Batch.Types.NetworkInterface
import Network.AWS.Batch.Types.NodeDetails
import Network.AWS.Batch.Types.NodeOverrides
import Network.AWS.Batch.Types.NodeProperties
import Network.AWS.Batch.Types.NodePropertiesSummary
import Network.AWS.Batch.Types.NodePropertyOverride
import Network.AWS.Batch.Types.NodeRangeProperty
import Network.AWS.Batch.Types.PlatformCapability
import Network.AWS.Batch.Types.ResourceRequirement
import Network.AWS.Batch.Types.ResourceType
import Network.AWS.Batch.Types.RetryAction
import Network.AWS.Batch.Types.RetryStrategy
import Network.AWS.Batch.Types.Secret
import Network.AWS.Batch.Types.Tmpfs
import Network.AWS.Batch.Types.Ulimit
import Network.AWS.Batch.Types.Volume
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-08-10@ of the Amazon Batch SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Batch",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "batch",
      Core._serviceSigningName = "batch",
      Core._serviceVersion = "2016-08-10",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Batch",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | These errors are usually caused by a client action, such as using an
-- action or resource on behalf of a user that doesn\'t have permissions to
-- use the action or resource, or specifying an identifier that\'s not
-- valid.
_ClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClientException =
  Core._MatchServiceError
    defaultService
    "ClientException"
    Prelude.. Core.hasStatus 400

-- | These errors are usually caused by a server issue.
_ServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServerException =
  Core._MatchServiceError
    defaultService
    "ServerException"
    Prelude.. Core.hasStatus 500
