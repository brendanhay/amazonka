{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Batch.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ServerException,
    _ClientException,

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
    arrayPropertiesDetail_size,
    arrayPropertiesDetail_statusSummary,
    arrayPropertiesDetail_index,

    -- * ArrayPropertiesSummary
    ArrayPropertiesSummary (..),
    newArrayPropertiesSummary,
    arrayPropertiesSummary_size,
    arrayPropertiesSummary_index,

    -- * AttemptContainerDetail
    AttemptContainerDetail (..),
    newAttemptContainerDetail,
    attemptContainerDetail_networkInterfaces,
    attemptContainerDetail_taskArn,
    attemptContainerDetail_containerInstanceArn,
    attemptContainerDetail_reason,
    attemptContainerDetail_logStreamName,
    attemptContainerDetail_exitCode,

    -- * AttemptDetail
    AttemptDetail (..),
    newAttemptDetail,
    attemptDetail_stoppedAt,
    attemptDetail_startedAt,
    attemptDetail_container,
    attemptDetail_statusReason,

    -- * ComputeEnvironmentDetail
    ComputeEnvironmentDetail (..),
    newComputeEnvironmentDetail,
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

    -- * ComputeEnvironmentOrder
    ComputeEnvironmentOrder (..),
    newComputeEnvironmentOrder,
    computeEnvironmentOrder_order,
    computeEnvironmentOrder_computeEnvironment,

    -- * ComputeResource
    ComputeResource (..),
    newComputeResource,
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

    -- * ComputeResourceUpdate
    ComputeResourceUpdate (..),
    newComputeResourceUpdate,
    computeResourceUpdate_securityGroupIds,
    computeResourceUpdate_subnets,
    computeResourceUpdate_minvCpus,
    computeResourceUpdate_maxvCpus,
    computeResourceUpdate_desiredvCpus,

    -- * ContainerDetail
    ContainerDetail (..),
    newContainerDetail,
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

    -- * ContainerOverrides
    ContainerOverrides (..),
    newContainerOverrides,
    containerOverrides_command,
    containerOverrides_environment,
    containerOverrides_resourceRequirements,
    containerOverrides_instanceType,
    containerOverrides_memory,
    containerOverrides_vcpus,

    -- * ContainerProperties
    ContainerProperties (..),
    newContainerProperties,
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

    -- * ContainerSummary
    ContainerSummary (..),
    newContainerSummary,
    containerSummary_reason,
    containerSummary_exitCode,

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
    eFSVolumeConfiguration_rootDirectory,
    eFSVolumeConfiguration_transitEncryption,
    eFSVolumeConfiguration_authorizationConfig,
    eFSVolumeConfiguration_transitEncryptionPort,
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
    evaluateOnExit_onReason,
    evaluateOnExit_onStatusReason,
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

    -- * JobDependency
    JobDependency (..),
    newJobDependency,
    jobDependency_jobId,
    jobDependency_type,

    -- * JobDetail
    JobDetail (..),
    newJobDetail,
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

    -- * JobQueueDetail
    JobQueueDetail (..),
    newJobQueueDetail,
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

    -- * JobTimeout
    JobTimeout (..),
    newJobTimeout,
    jobTimeout_attemptDurationSeconds,

    -- * KeyValuePair
    KeyValuePair (..),
    newKeyValuePair,
    keyValuePair_value,
    keyValuePair_name,

    -- * KeyValuesPair
    KeyValuesPair (..),
    newKeyValuesPair,
    keyValuesPair_values,
    keyValuesPair_name,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    newLaunchTemplateSpecification,
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,

    -- * LinuxParameters
    LinuxParameters (..),
    newLinuxParameters,
    linuxParameters_sharedMemorySize,
    linuxParameters_initProcessEnabled,
    linuxParameters_tmpfs,
    linuxParameters_swappiness,
    linuxParameters_devices,
    linuxParameters_maxSwap,

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
    mountPoint_sourceVolume,
    mountPoint_readOnly,

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    newNetworkConfiguration,
    networkConfiguration_assignPublicIp,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_ipv6Address,
    networkInterface_privateIpv4Address,
    networkInterface_attachmentId,

    -- * NodeDetails
    NodeDetails (..),
    newNodeDetails,
    nodeDetails_nodeIndex,
    nodeDetails_isMainNode,

    -- * NodeOverrides
    NodeOverrides (..),
    newNodeOverrides,
    nodeOverrides_numNodes,
    nodeOverrides_nodePropertyOverrides,

    -- * NodeProperties
    NodeProperties (..),
    newNodeProperties,
    nodeProperties_numNodes,
    nodeProperties_mainNode,
    nodeProperties_nodeRangeProperties,

    -- * NodePropertiesSummary
    NodePropertiesSummary (..),
    newNodePropertiesSummary,
    nodePropertiesSummary_numNodes,
    nodePropertiesSummary_nodeIndex,
    nodePropertiesSummary_isMainNode,

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
    volume_efsVolumeConfiguration,
    volume_host,
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
import Amazonka.Batch.Types.EvaluateOnExit
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
import Amazonka.Batch.Types.PlatformCapability
import Amazonka.Batch.Types.ResourceRequirement
import Amazonka.Batch.Types.ResourceType
import Amazonka.Batch.Types.RetryAction
import Amazonka.Batch.Types.RetryStrategy
import Amazonka.Batch.Types.Secret
import Amazonka.Batch.Types.Tmpfs
import Amazonka.Batch.Types.Ulimit
import Amazonka.Batch.Types.Volume
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | These errors are usually caused by a server issue.
_ServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServerException =
  Core._MatchServiceError
    defaultService
    "ServerException"
    Prelude.. Core.hasStatus 500

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
