-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ServerException,
    _ClientException,

    -- * AttemptDetail
    AttemptDetail (..),
    mkAttemptDetail,
    adContainer,
    adStartedAt,
    adStatusReason,
    adStoppedAt,

    -- * NodeOverrides
    NodeOverrides (..),
    mkNodeOverrides,
    noNodePropertyOverrides,
    noNumNodes,

    -- * JQStatus
    JQStatus (..),

    -- * CRAllocationStrategy
    CRAllocationStrategy (..),

    -- * JobDefinitionType
    JobDefinitionType (..),

    -- * ResourceType
    ResourceType (..),

    -- * EvaluateOnExit
    EvaluateOnExit (..),
    mkEvaluateOnExit,
    eoeAction,
    eoeOnExitCode,
    eoeOnReason,
    eoeOnStatusReason,

    -- * NodePropertiesSummary
    NodePropertiesSummary (..),
    mkNodePropertiesSummary,
    npsIsMainNode,
    npsNodeIndex,
    npsNumNodes,

    -- * String
    String (..),

    -- * JobDependency
    JobDependency (..),
    mkJobDependency,
    jJobId,
    jType,

    -- * ContainerOverrides
    ContainerOverrides (..),
    mkContainerOverrides,
    coCommand,
    coEnvironment,
    coInstanceType,
    coMemory,
    coResourceRequirements,
    coVcpus,

    -- * Device
    Device (..),
    mkDevice,
    dHostPath,
    dContainerPath,
    dPermissions,

    -- * Volume
    Volume (..),
    mkVolume,
    vHost,
    vName,

    -- * ContainerSummary
    ContainerSummary (..),
    mkContainerSummary,
    csExitCode,
    csReason,

    -- * NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niAttachmentId,
    niIpv6Address,
    niPrivateIpv4Address,

    -- * RetryStrategy
    RetryStrategy (..),
    mkRetryStrategy,
    rsAttempts,
    rsEvaluateOnExit,

    -- * AttemptContainerDetail
    AttemptContainerDetail (..),
    mkAttemptContainerDetail,
    acdContainerInstanceArn,
    acdExitCode,
    acdLogStreamName,
    acdNetworkInterfaces,
    acdReason,
    acdTaskArn,

    -- * JobSummary
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

    -- * KeyValuePair
    KeyValuePair (..),
    mkKeyValuePair,
    kvpName,
    kvpValue,

    -- * ComputeEnvironmentOrder
    ComputeEnvironmentOrder (..),
    mkComputeEnvironmentOrder,
    ceoOrder,
    ceoComputeEnvironment,

    -- * Secret
    Secret (..),
    mkSecret,
    sName,
    sValueFrom,

    -- * RetryAction
    RetryAction (..),

    -- * TagValue
    TagValue (..),

    -- * ComputeEnvironmentDetail
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

    -- * CRType
    CRType (..),

    -- * ArrayPropertiesDetail
    ArrayPropertiesDetail (..),
    mkArrayPropertiesDetail,
    apdIndex,
    apdSize,
    apdStatusSummary,

    -- * LinuxParameters
    LinuxParameters (..),
    mkLinuxParameters,
    lpDevices,
    lpInitProcessEnabled,
    lpMaxSwap,
    lpSharedMemorySize,
    lpSwappiness,
    lpTmpfs,

    -- * Tmpfs
    Tmpfs (..),
    mkTmpfs,
    tContainerPath,
    tSize,
    tMountOptions,

    -- * LogConfiguration
    LogConfiguration (..),
    mkLogConfiguration,
    lcLogDriver,
    lcOptions,
    lcSecretOptions,

    -- * JobTimeout
    JobTimeout (..),
    mkJobTimeout,
    jtAttemptDurationSeconds,

    -- * JQState
    JQState (..),

    -- * Ec2Configuration
    Ec2Configuration (..),
    mkEc2Configuration,
    ecImageType,
    ecImageIdOverride,

    -- * ArrayPropertiesSummary
    ArrayPropertiesSummary (..),
    mkArrayPropertiesSummary,
    apsIndex,
    apsSize,

    -- * JobQueueDetail
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

    -- * ComputeResource
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

    -- * ImageType
    ImageType (..),

    -- * DeviceCgroupPermission
    DeviceCgroupPermission (..),

    -- * CEStatus
    CEStatus (..),

    -- * NodeDetails
    NodeDetails (..),
    mkNodeDetails,
    ndIsMainNode,
    ndNodeIndex,

    -- * LogDriver
    LogDriver (..),

    -- * JobDefinition
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

    -- * ResourceRequirement
    ResourceRequirement (..),
    mkResourceRequirement,
    rrValue,
    rrType,

    -- * TagKey
    TagKey (..),

    -- * Ulimit
    Ulimit (..),
    mkUlimit,
    uHardLimit,
    uName,
    uSoftLimit,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    mkLaunchTemplateSpecification,
    ltsLaunchTemplateId,
    ltsLaunchTemplateName,
    ltsVersion,

    -- * Host
    Host (..),
    mkHost,
    hSourcePath,

    -- * NodeRangeProperty
    NodeRangeProperty (..),
    mkNodeRangeProperty,
    nrpTargetNodes,
    nrpContainer,

    -- * ArrayProperties
    ArrayProperties (..),
    mkArrayProperties,
    apSize,

    -- * JobStatus
    JobStatus (..),

    -- * NodePropertyOverride
    NodePropertyOverride (..),
    mkNodePropertyOverride,
    npoTargetNodes,
    npoContainerOverrides,

    -- * ContainerProperties
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

    -- * ContainerDetail
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

    -- * CEType
    CEType (..),

    -- * JobDetail
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

    -- * ComputeResourceUpdate
    ComputeResourceUpdate (..),
    mkComputeResourceUpdate,
    cruDesiredvCpus,
    cruMaxvCpus,
    cruMinvCpus,

    -- * ImageIdOverride
    ImageIdOverride (..),

    -- * NodeProperties
    NodeProperties (..),
    mkNodeProperties,
    npNumNodes,
    npMainNode,
    npNodeRangeProperties,

    -- * ArrayJobDependency
    ArrayJobDependency (..),

    -- * CEState
    CEState (..),

    -- * MountPoint
    MountPoint (..),
    mkMountPoint,
    mpContainerPath,
    mpReadOnly,
    mpSourceVolume,

    -- * StatusReason
    StatusReason (..),

    -- * ComputeEnvironmentArn
    ComputeEnvironmentArn (..),

    -- * ComputeEnvironmentName
    ComputeEnvironmentName (..),

    -- * JobId
    JobId (..),

    -- * Reason
    Reason (..),

    -- * ArrayJobId
    ArrayJobId (..),

    -- * JobQueue
    JobQueue (..),

    -- * MultiNodeJobId
    MultiNodeJobId (..),

    -- * NextToken
    NextToken (..),

    -- * JobName
    JobName (..),

    -- * JobDefinitionName
    JobDefinitionName (..),

    -- * OnExitCode
    OnExitCode (..),

    -- * OnReason
    OnReason (..),

    -- * OnStatusReason
    OnStatusReason (..),
  )
where

import Network.AWS.Batch.Types.ArrayJobDependency
import Network.AWS.Batch.Types.ArrayJobId
import Network.AWS.Batch.Types.ArrayProperties
import Network.AWS.Batch.Types.ArrayPropertiesDetail
import Network.AWS.Batch.Types.ArrayPropertiesSummary
import Network.AWS.Batch.Types.AttemptContainerDetail
import Network.AWS.Batch.Types.AttemptDetail
import Network.AWS.Batch.Types.CEState
import Network.AWS.Batch.Types.CEStatus
import Network.AWS.Batch.Types.CEType
import Network.AWS.Batch.Types.CRAllocationStrategy
import Network.AWS.Batch.Types.CRType
import Network.AWS.Batch.Types.ComputeEnvironmentArn
import Network.AWS.Batch.Types.ComputeEnvironmentDetail
import Network.AWS.Batch.Types.ComputeEnvironmentName
import Network.AWS.Batch.Types.ComputeEnvironmentOrder
import Network.AWS.Batch.Types.ComputeResource
import Network.AWS.Batch.Types.ComputeResourceUpdate
import Network.AWS.Batch.Types.ContainerDetail
import Network.AWS.Batch.Types.ContainerOverrides
import Network.AWS.Batch.Types.ContainerProperties
import Network.AWS.Batch.Types.ContainerSummary
import Network.AWS.Batch.Types.Device
import Network.AWS.Batch.Types.DeviceCgroupPermission
import Network.AWS.Batch.Types.Ec2Configuration
import Network.AWS.Batch.Types.EvaluateOnExit
import Network.AWS.Batch.Types.Host
import Network.AWS.Batch.Types.ImageIdOverride
import Network.AWS.Batch.Types.ImageType
import Network.AWS.Batch.Types.JQState
import Network.AWS.Batch.Types.JQStatus
import Network.AWS.Batch.Types.JobDefinition
import Network.AWS.Batch.Types.JobDefinitionName
import Network.AWS.Batch.Types.JobDefinitionType
import Network.AWS.Batch.Types.JobDependency
import Network.AWS.Batch.Types.JobDetail
import Network.AWS.Batch.Types.JobId
import Network.AWS.Batch.Types.JobName
import Network.AWS.Batch.Types.JobQueue
import Network.AWS.Batch.Types.JobQueueDetail
import Network.AWS.Batch.Types.JobStatus
import Network.AWS.Batch.Types.JobSummary
import Network.AWS.Batch.Types.JobTimeout
import Network.AWS.Batch.Types.KeyValuePair
import Network.AWS.Batch.Types.LaunchTemplateSpecification
import Network.AWS.Batch.Types.LinuxParameters
import Network.AWS.Batch.Types.LogConfiguration
import Network.AWS.Batch.Types.LogDriver
import Network.AWS.Batch.Types.MountPoint
import Network.AWS.Batch.Types.MultiNodeJobId
import Network.AWS.Batch.Types.NetworkInterface
import Network.AWS.Batch.Types.NextToken
import Network.AWS.Batch.Types.NodeDetails
import Network.AWS.Batch.Types.NodeOverrides
import Network.AWS.Batch.Types.NodeProperties
import Network.AWS.Batch.Types.NodePropertiesSummary
import Network.AWS.Batch.Types.NodePropertyOverride
import Network.AWS.Batch.Types.NodeRangeProperty
import Network.AWS.Batch.Types.OnExitCode
import Network.AWS.Batch.Types.OnReason
import Network.AWS.Batch.Types.OnStatusReason
import Network.AWS.Batch.Types.Reason
import Network.AWS.Batch.Types.ResourceRequirement
import Network.AWS.Batch.Types.ResourceType
import Network.AWS.Batch.Types.RetryAction
import Network.AWS.Batch.Types.RetryStrategy
import Network.AWS.Batch.Types.Secret
import Network.AWS.Batch.Types.StatusReason
import Network.AWS.Batch.Types.String
import Network.AWS.Batch.Types.TagKey
import Network.AWS.Batch.Types.TagValue
import Network.AWS.Batch.Types.Tmpfs
import Network.AWS.Batch.Types.Ulimit
import Network.AWS.Batch.Types.Volume
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-08-10@ of the Amazon Batch SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Batch",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "batch",
      Core._svcVersion = "2016-08-10",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Batch",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | These errors are usually caused by a server issue.
_ServerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServerException =
  Core._MatchServiceError mkServiceConfig "ServerException"
    Core.. Core.hasStatues 500
{-# DEPRECATED _ServerException "Use generic-lens or generic-optics instead." #-}

-- | These errors are usually caused by a client action, such as using an action or resource on behalf of a user that doesn't have permissions to use the action or resource, or specifying an identifier that is not valid.
_ClientException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClientException =
  Core._MatchServiceError mkServiceConfig "ClientException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClientException "Use generic-lens or generic-optics instead." #-}
